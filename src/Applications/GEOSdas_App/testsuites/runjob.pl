#!/usr/bin/env perl
#=======================================================================
# name - runjob
# purpose - setup and submit jobs using pre-defined sets of inputs
#           to the fvsetup script
# Notes:
# 1. Each job should have an input file with the name format: expid.input
# 2. This script finds all possible input files, and queries the user to
#    identify which job to run.
#
# global hashes (keys are integers associated with each input file):
# - %inFile ...... names of *.input files
# - %rawInFile ... names of files containing raw input stripped from input files
# - %descript .... job descriptions
# - %edits ....... additional setup edits
# - %expid ....... job experiment IDs
# - %flags ....... fvsetup runtime flags for each job
# - %fvhome ...... experiment home directories
#
# Input file header information
# => description: [experiment description]
# => fvid: [fvsetup ID]
# => def: [variable];nas = [value at NAS]
# => def: [variable];nccs = [value at NCCS]
# => fvsetupflags: [fvsetup option]
#    where fvsetup options
#    -agcm                     include agcm_import_rst when copying restart files
#    -sensdeg=s                =1 for first-deg sensitivity test
#                              =3 for third-deg sensitivity test (default)
#    -setupfile fname=subdir   copy file from FVICS dir to expid subdirectory
#
# revision history
# 09Jun2010  Stassi   Initial version
#=======================================================================
use strict;
use warnings;

# global variables
#-----------------
my ($ESMABIN, $ESMATST, $TRYAGAIN);
my ($auto, $autox, $codeID, $dbqueue, $debug);
my ($fvroot, $fvsetupID, $fvsetupScript);
my ($ignoreOSdiff, $inputDir, $jobn, $nocheck, $nofilter, $noloop);
my ($sel, $siteID, $specified, $stage, $verbose);
my (%descript, %edits, %expid, %flags, %fvhome, %fvics, %fvid);
my (%inFile, %rawInFile, %rem_acct);
my (@default, @inputFiles, @nondefault);

# main program
#-------------
{
    init();
    intro();
    checkOS();
    getInputDir();
    getInputs();
    runjobs();
}

#=======================================================================
# name - init
# purpose - initialize global hashes
#
# Notes on where to find fvsetup -
# 1. The variable, $ESMABIN, contains the directory location for the
#    fvsetup script. This value is hard-coded during the build.
# 2. This script is fvsetupID-dependent; it may not work properly for different
#    versions of fvsetup which have differences in the prompts.
# 3. $ESMABIN can be overwritten by using the -D flag to supply an alternate
#    location for fvsetup. This should be done with great care.
#=======================================================================
sub init {
    use FindBin;
    use lib ("$FindBin::Bin");
    use Cwd ("cwd");
    use Getopt::Long;
    use GMAO_utils ("get_siteID");

    my ($BINDIR, $localdir, $help);

    $TRYAGAIN = 9999;
    $siteID = get_siteID();
    #--$fvsetupID = `git hash-object $fvsetupScript | cut -c1-10`;

    # these values are set with sed substitution during build
    #--------------------------------------------------------
    $codeID = "@GIT_TAG_OR_REV@";
    $fvsetupID = "@fvID@";
    $ESMABIN = "@ESMABIN@";
    $ESMATST = "@ESMATST@";
    die ">> Error << $ESMABIN is not a directory;" unless -d $ESMABIN;

    # get runtime options
    #--------------------
    Getopt::Long::Configure("no_ignore_case");
    GetOptions( "a|auto"      => \$auto,
                "ax|autox"    => \$autox,
                "nc|nocheck"  => \$nocheck,
                "nf|nofilter" => \$nofilter,
                "d=s"         => \$inputDir,
                "l|local"     => \$localdir,
                "OSx"         => \$ignoreOSdiff,
                "db|debug"    => \$debug,
                "dbq|dbqueue" => \$dbqueue,
                "f=s"         => \$fvsetupScript,
                "h|help"      => \$help,
                "stage"       => \$stage,
                "v"           => \$verbose );
    usage() if $help;
    $auto = 1 if $autox;
    $verbose = 0 unless $verbose;

    @inputFiles = @ARGV;
    if (@inputFiles) { $specified = 1; $nofilter = 1 }
    else             { $specified = 0 }

    if ($localdir) { $inputDir = cwd() unless $inputDir }

    # find $fvroot and $fvsetupScript
    #--------------------------------
    $fvroot = dirname($ESMABIN);
    $fvsetupScript = "$ESMABIN/fvsetup" unless $fvsetupScript;
    die ">> Error << cannot find $fvsetupScript;\n" unless -e $fvsetupScript;
}

#=======================================================================
# name - intro
# purpose - print introduction
#=======================================================================
sub intro {
    use File::Basename;
    system("clear");

    print "\n=============================\n"
        . "\nRun fvsetup from Saved Inputs\n"
        . "\n=============================\n";
    print "\nfvsetup: $fvsetupScript\n";
}

#=======================================================================
# name - checkOS
# purpose - compare system OS file file to file in $fvetc directory
#=======================================================================
sub checkOS {
    my ($sysfile, $status, $ans, $dflt);

    chomp($sysfile = `$fvroot/bin/OScheck.pl -sysfile`);

    if ($ignoreOSdiff) { $dflt = "y" }
    else               { $dflt = "n" }

    if (-f $sysfile) {
        $status = system("$fvroot/bin/OScheck.pl -cmp $fvroot/etc");
        if ($status) {
            $ans = query("\nContinue (y/n)?", $dflt);
            exit unless yes($ans);
        }
    }
}

#=======================================================================
# name - getInputDir
# purpose - get directory location of *.input files
#=======================================================================
sub getInputDir {
    my ($success, $cmd, $ans);

    # naked block to allow redo's
    #----------------------------
    {
        # query for $inputDir if not already supplied
        #--------------------------------------------
        $inputDir = queryInputDir() unless $inputDir;

        # sorry, but we don't want you writing to fvroot subdirectories
        #--------------------------------------------------------------
        if ($inputDir =~ m[$fvroot]) {
            print "fvroot: $fvroot\n";
            print "input directory: $inputDir\n";
            warn ">> Error << this input directory is not allowed;";
            $inputDir = "";
            $specified = 0;
            exit if $auto;
            redo;
        }

        # make $inputDir if it does not already exist
        #--------------------------------------------
        unless (-e $inputDir) {
            $success = mkdir $inputDir;
            unless ($success) {
                warn ">> Error << making directory, $inputDir";
                $inputDir = "";
                exit if $auto;
                redo;
            }
            print "\nInput directory created: $inputDir\n";
            $ans = query("Stage test inputs (y/n):", "y");
            stageInputs() if yes($ans);
        }

        # check that $inputDir is a directory
        #------------------------------------
        unless (-d $inputDir) {
            warn ">> Error << not a directory: $inputDir";
            $inputDir = "";
            exit if $auto;
            redo;
        }
            
        # check that $inputDir is writeable
        #----------------------------------
        unless (-w $inputDir) {
            warn ">> Error << cannot write to directory: $inputDir";
            $inputDir = "";
            exit if $auto;
            redo;
        }
    }

    # chdir to $inputDir
    #-------------------
    chdir $inputDir;

    # stage inputs from testsuites directory, if requested
    #-----------------------------------------------------
    stageInputs() if $stage;
}

#=======================================================================
# name - queryInputDir
# purpose - query for the directory location of *.input files
#
# Note: (jcs, 1Feb2013) Commented out query; just take default instead
#=======================================================================
sub queryInputDir {
    use File::Basename;
    my ($home, $user);

    # get environment variables
    #--------------------------
    $home = $ENV{"HOME"};
    $user = $ENV{"USER"};

    # get default $inputDir from environment variable, if set
    #--------------------------------------------------------
    $inputDir = $ENV{"SAVEDINPUTS"};

    # default inputDir locations
    #---------------------------
    unless ($inputDir) {
        if ($siteID eq "nccs") {
            die ">> Error << cannot find USER name;" unless $user;
            $inputDir = "/discover/nobackup/$user/SavedInputs";
        }
        elsif ($siteID eq "nas") {
            die ">> Error << cannot find USER name;" unless $user;
            $inputDir = "/nobackup/$user/SavedInputs";
        }
        else {
            $home = "" unless $home;
            $inputDir = "$home/SavedInputs";
        }
    }

    # query for $inputDir
    #--------------------
    #--$inputDir = query("\n> Input directory:", $inputDir) unless $specified;

    return $inputDir;
}

#=======================================================================
# name - getInputs
# purpose - get list of *.input files in $inputDir
#=======================================================================
sub getInputs {
    my ($key, $label, $index, $input, $fvID, $expid, $ext);
    my ($file, $file1, $size);

    # check that user-specified input files exist
    #--------------------------------------------
    $size = scalar @inputFiles;
    foreach (1..$size) {
        $file = shift @inputFiles;
        $file =~ s/\/+$//;
        $file .= ".input" unless $file =~ /\.input$/;

        $file1 = $file;
        $file1 = "$inputDir/$file" unless -e $file1;
        $file1 = "$inputDir/$file.input" unless -e $file1;

        die ">> Error << cannot find input file: $file\n" unless -e $file1;
        push @inputFiles, $file1;
    }

    # get list of setup input file, if not specified by user
    #-------------------------------------------------------
    @inputFiles = sort (<$inputDir/*.input>) unless @inputFiles;

    # extract inputs
    #---------------
    $label = "Extracting inputs";
    if ($specified) {
        $label .= " for specified file(s)\n" }
    elsif ($nofilter) {
        $label .= "\n" }
    else {
        $label .= " for fvsetupID: $fvsetupID\n" }

    underline($label);

    $key = 0;
    foreach $index (0..$#inputFiles) {
        $input = $inputFiles[$index];
        $fvID = extract($input, "fvsetupID", 1);
        ($expid, $ext) = split/[\.]/, basename $input;
        next unless $nofilter or ($fvsetupID eq $fvID);

        # continue with input file
        #-------------------------
        $inFile{++$key} = $input;
        $fvid{$key} = $fvID;
        $expid{$key} = $expid;
        getRawInputs($key);
        extractAdditionalInfo($key);
    }

    unless (@inputFiles) {
        print "\n  No input files found in directory: $inputDir"
            . "\n    Type \"runjob -stage\" to stage test input files."
            . "\n    Type \"runjob -h\" for complete usage information.\n\n";
        quit();
    }
}

#=======================================================================
# name - getRawInputs
# purpose - convert setup input data file (x.input) to a format
#           which can be inputted to fvsetup through redirection
# Notes:
# 1. Heading data is extracted from input file as it is being read.
# 2. The expid value extracted from the heading will be validated in the
#    extractAdditionalInfo() subroutine.
#=======================================================================
sub getRawInputs {
    use File::Basename;
    my ($key, $input, @dummy);
    my ($rawinput, $inputval, @rawVALUE);
    my ($readhead, $previous, $packed_inputline);
    my (%def, $var, $val);

    # input parameters
    #-----------------
    $key = shift @_;

    # input file name
    #----------------
    $input = $inFile{$key};

    # initialize heading info
    #------------------------
    $descript{$key} = "No description found";
    $edits{$key} = "";
    $flags{$key} = "";
    %def = ();

    print "extracting fvsetup inputs: $input\n";

    # use expid from file name
    #-------------------------
    $def{"expid"} = $expid{$key};

    # filter setup data from input file
    #----------------------------------
    $readhead = 1;
    open INPUT, "< $input" or die ">> Error << opening $input: $!";
    while (<INPUT>) {
        chomp; s/^\s+|\s+$//g;
        $_ = envsubst($_);

        # get heading info
        #-----------------
        if ($readhead) {
            if ( /^description\s*:\s*(.*)$/ )  { $descript{$key} = $1; next }
            if ( /^fvsetupflags\s*:\s*(.*)$/ ) { $flags{$key}    = $1; next }
            if ( /^edits\s*:\s*(.*)$/ )        { $edits{$key}    = $1; next }

            # variable definitions to expand responses
            #-----------------------------------------
            if ( /^def\s*:\s*(\S+)\s*=\s*(.*)$/ ) {
                $var = $1;
                $val = $2;

                # site-specific values take precedent
                #------------------------------------
                if ($var =~ /;$siteID$/) {
                    $var =~ s/;$siteID$//;
                    $def{$var} = $val;
                }

                # otherwise previously defined takes precedent
                #---------------------------------------------
                else {
                    $def{$var} = $val unless $def{$var};
                }
            }                
            $readhead = 0 if /---ENDHEADERS---/;   # stop looking for heading
            next;
        }

        # store responses in @rawVALUE array
        #-----------------------------------
        unless (/^>/) { $previous = $_; next }
        $inputval = $1 if /^>\s*(.*)$/;
        $inputval = "" unless $inputval =~ /\S+/;
        $inputval = expand_string($inputval, %def);
        push @rawVALUE, $inputval;

        # save non-default responses
        #---------------------------
        if ($inputval =~ /\S+/) {
            $packed_inputline = "${key}:::${previous}:::${inputval}";
            push @nondefault, $packed_inputline;
        }
        else {
            $packed_inputline = "${key}:::${previous}";
            push @default, $packed_inputline;
        }
    }
    close INPUT;

    # write stripped-out input values to a new file
    #----------------------------------------------
    $rawinput = "$inputDir/.rawInFile.$expid{$key}";
    my_unlink($rawinput) if -e $rawinput;

    print "-> writing $rawinput\n" if $verbose;
    open RAW, "> $rawinput" or die ">> Error << opening $rawinput: $!";
    foreach (@rawVALUE) { print RAW "$_\n"; print " > $_\n" if $verbose and $_ };
    close RAW;

    # save output filename in global hash
    #------------------------------------
    $rawInFile{$key} = $rawinput;
}

#=======================================================================
# name - envsubst
# purpose - replace environment variables in a string with their values
#=======================================================================
sub envsubst {
    my ($string, @envVarList, $var);
    $string = shift @_;

    # list of environment variables to substitute
    #--------------------------------------------
    @envVarList = (qw/USER HOME ARCHIVE/);

    foreach $var (@envVarList) {

        # substitute for format $variable
        #--------------------------------
        if ($string =~ m/(\$$var)/) {
            $string =~ s/\$$var/$ENV{$var}/g if $ENV{$var};
        }

        # substitute for format ${variable}
        #----------------------------------
        if ($string =~ m/(\${$var})/) {
            $string =~ s/\${$var}/$ENV{$var}/g if $ENV{$var};
        }
    }
    return $string;
}

#=======================================================================
# name - extractAdditionalInfo
# purpose - run short-circuited fvsetup to get expid and FVHOME values.
#
# notes
# 1. The -stop flag in fvsetup will stop the setup prior to creating the
#    $FVHOME directory.
# 2. The FVHOME value is stored in a global hash.
# 3. The expid value is validated against the value in global hash if
#    global hash has a value; Otherwise it is stored in global hash.
#=======================================================================
sub extractAdditionalInfo {
    my $key;
    my ($rawinput, $tempLOG, $fvsetup, $cmd, $status);
    my ($FVHOME, $expid, $rem);

    # input parameter
    #----------------
    $key = shift @_;

    # file names
    #-----------
    $rawinput = $rawInFile{$key};
    $tempLOG = ".fvsetup.tempLOG$key";
    my_unlink($tempLOG) if -e $tempLOG;

    # run short-circuited fvsetup
    #----------------------------
    $fvsetup  = "$fvsetupScript -runjob";
    $fvsetup .= " $flags{$key}" if $flags{$key};
    $cmd = "$fvsetup -stop < $rawinput > $tempLOG";
    $cmd = "($cmd) 2> /dev/null" unless $verbose;
    print "\n$cmd\n\n" if $verbose;
    $status = system $cmd;
    if ($status) {
        print "\n>> Error << running abbreviated fvsetup for input file\n";
        checklist($key);
        die;
    }

    # extract FVHOME value
    #---------------------
    $FVHOME = extract($tempLOG, "FVHOME", 1);
    $fvhome{$key} = $FVHOME;

    # extract remote account info
    #----------------------------
    $rem = extract($tempLOG, "Remote account for Intranet plots", 1);
    $rem_acct{$key} = $rem;

    # verify that fvsetup used correct expid value
    #---------------------------------------------
    $expid = extract($tempLOG, "EXPID", 1);
    if ($expid ne $expid{$key}) {
        print "\n>> Error << Expid inconsistency in input file\n"
            . "  input file:   $inFile{$key}\n"
            . "  from fvsetup: $expid\n";
        checklist($key);
        die;
    }
    my_unlink($tempLOG);
}

#=======================================================================
# name - runjobs
# purpose - display options for setting up and submitting jobs
#=======================================================================
sub runjobs {
    my ($key, @keyArr);

    unless (%inFile) {
        print "\nNo infiles were found for fvsetupID: $fvsetupID\n\n";
        return;
    }

    while (1) {
        $key = "";
        @keyArr = ( keys %inFile );

        if ( $#keyArr == 0 ) {
            $key = $keyArr[0];
            $noloop = 1;
        }

        $key = choose_job() unless $key;
        next if $key eq $TRYAGAIN;

        if ($key < 0) {
            show_nondefault_inputs($key);
            next;
        }
        if ($key =~ /^\+/) {
            show_default_inputs($key);
            next;
        }
        unless ( run_fvsetup($key) ) {
            last if $noloop;
            next;
        }
        submit_job($key);
        last if $noloop;
    }
}

#=======================================================================
# name - choose_job
# purpose - query user as to which job to run
#=======================================================================
sub choose_job {
    use File::Basename;
    my ($max1, $max2, $max3, $fmt0, $fmt1, $fmt2, $fmt3, $fmt4);
    my ($dflt, $key);

    # increment $dflt
    #----------------
    $dflt = ++$sel;
    $dflt = 0 unless $inFile{$dflt};

    $max1 = maxlen(values %expid);
    $max2 = maxlen(values %inFile);
    $max3 = maxlen(values %fvid);
    $fmt0 = "%3s. %s\n";
    $fmt1 = "     %-${max1}s  %-${max2}s  %-${max3}s   %s\n";
    $fmt2 = "%3s. %-${max1}s  %-${max2}s  %-${max3}s : %s\n";
    $fmt3 = "     %-${max1}s  %-${max2}s    %s\n";
    $fmt4 = "%3s. %-${max1}s  %-${max2}s  : %s\n";

    # choose from list of available jobs
    #-----------------------------------
    print "\n--------------\n"
        .   "Available Jobs\n"
        .   "--------------\n";
    print  "fvsetupID: $fvsetupID\n" unless $nofilter;
    print  "directory: $inputDir\n\n";
    if (%expid) {
        if ($nofilter) {
            printf $fmt1, "expid", "file", "fvID", "description";
            printf $fmt1, "-----", "----", "----", "-----------";
        }
        else {
            printf $fmt3, "expid", "file", "description";
            printf $fmt3, "-----", "----", "-----------";
        }
    }

    foreach (sort numeric keys %inFile) {
        if ($nofilter) {
            printf $fmt2, $_, $expid{$_}, basename($inFile{$_}),
            $fvid{$_}, $descript{$_};
        }
        else {
            printf $fmt4, $_, $expid{$_}, basename($inFile{$_}),
            $descript{$_};
        }
    }
    print "\n";
    printf $fmt0, "-n", "show fvsetup flags and non-default inputs for job #n";
    printf $fmt0, "+n", "show default inputs for job #n\n";
    printf $fmt0, 0, "quit\n";
    $sel = query("Select:", $dflt);

    # get selection
    #--------------
    quit() if $sel eq "0";

    # check for valid selection
    #--------------------------
    $key = $sel;
    $key = substr($sel,1) if $sel =~/^-/ or $sel =~ /^\+/;
    unless ( $inFile{$key} ) {
        print "\nInvalid selection: $sel\n";
        pause();
        $sel = $TRYAGAIN;
    }
    return $sel;
}

#=======================================================================
# name - show_nondefault_inputs
# purpose - display the non-default responses and fvsetup flags for
#           specified job
#=======================================================================
sub show_nondefault_inputs {
    my ($negkey, $key);
    my ($first, $kk, $prompt, $response);

    $negkey = shift @_;
    $key = substr($negkey,1);

    if ($flags{$key}) { print "\n\nfvsetup flags: $flags{$key}\n"     }
    else              { print "\n\nNo fvsetup flags for Job \#$key\n" }

    $first = 1;
    foreach (@nondefault) {
        ($kk, $prompt, $response) = split /:{3}/;
        next unless $kk == $key;
        if ($first) {
            underline("Non-default responses for Job \#$key",2);
            $first = 0;
        }
        print "$prompt\n";
        print "> $response\n";
        print "-------\n";
    }
    print "\n";
    if ($first) { print "No non-default responses for Job \#$key" }
    else        { print "Note: This assumes that responses in input file "
                      . "correspond correctly to prompts in fvsetup,\n" }
    pause();
    return;
}

#=======================================================================
# name - show_default_inputs
# purpose - display the default responses for specified job
#=======================================================================
sub show_default_inputs {
    my ($key, $first, $kk, $prompt);

    $key = shift @_;
    $key = substr($key,1);

    $first = 1;
    foreach (@default) {
        ($kk, $prompt) = split /:{3}/;
        next unless $kk == $key;
        if ($first) {
            underline("These prompts take default response "
                      . " for Job \#$key",2);
            $first = 0;
        }
        print "- $prompt\n";
    }
    print "\n";
    if ($first) { print "No default responses for Job \#$key" }
    else        { print "Note: This assumes that responses in input file "
                      . "correspond correctly to prompts in fvsetup,\n" }
    pause();
    return;
}

#=======================================================================
# name - run_fvsetup
# purpose - run fvsetup for the selected job
#
# input parameter
# => $key: key to global hashes; this is an integer associated with
#          with a particular job
# return value
# => $continueflag: do not continue if equal to zero
#
# Note
# 1. In this routine, the information in the setup input file is
#    converted to a format that can fed to fvsetup through redirection.
#=======================================================================
sub run_fvsetup {
    use File::Path;
    my $key;
    my ($rawinput, $fvsuLOG, $fvsuLOG1, $fvsuERR, $fvsuERR1, $output, $logdir);
    my ($fvsetup, $expid_arc, $clean, $verify, $ans);
    my ($FVHOME, $dflt, $cmd, @bootstrap, $status, $continueflag);

    # input parameter
    #----------------
    $key = shift @_;

    # file names
    #-----------
    $rawinput = $rawInFile{$key};
    $fvsuLOG = "$inputDir/fvsetup.$expid{$key}.LOG";
    $fvsuERR = "$inputDir/fvsetup.$expid{$key}.ERR";

    my_unlink($fvsuLOG) if -e $fvsuLOG;
    my_unlink($fvsuERR) if -e $fvsuERR;

    # display information about job being setup
    #------------------------------------------
    print "\n -----------------\n";
    print   "  job information \n";
    print   " -----------------\n";
    print "  expid:         $expid{$key}\n";
    print "  description:   $descript{$key}\n";
    print "  input file:    $inFile{$key}\n";
    print "  fvsetup ID:    $fvid{$key}\n"  if $fvid{$key};;
    print "  fvsetup flags: $flags{$key}\n" if $flags{$key};
    print "  other edits:   $edits{$key}\n" if $edits{$key};
    print "  FVHOME:        $fvhome{$key}\n";
    print "  fvsetup:       $fvsetupScript\n";

    # check fvsetupid consistency
    #----------------------------
    $verify = 0;
    if ( $fvid{$key} ) {
        if ( $fvsetupID ne $fvid{$key} ) {
            $verify = 1;
            print "\n  !!!---------!!!\n"
                .   "  !!! WARNING !!!\n"
                .   "  !!!---------!!!\n"
                .   "  fvsetupID inconsistency found.\n"
                .   "  input file: $inFile{$key}\n\n"
                .   "    fvsetupID (input file):    $fvid{$key}\n"
                .   "    fvsetupID (runjob script): $fvsetupID\n";
        }
    }
    else {
        $verify = 1;
        print "\n  !!!!!!!!!!!!!!!\n"
            .   "  !!! WARNING !!!\n"
            .   "  !!!!!!!!!!!!!!!\n"
            .   "  fvsetupID not found in input file.\n"
            .   "  input file: $inFile{$key}\n\n"
            .   "    fvsetupID (input file):    not found\n"
            .   "    fvsetupID (runjob script): $fvsetupID\n";
    }
    if ($verify) {
        print "\nUnless you are sure the input file is okay, you should quit"
            . " and run the checkinput utility before continuing.\n";
        $ans = query("Continue (y/n)?", "n");
        quit("Quitting.") if neg($ans);
    }

    # query user whether to run fvsetup
    #----------------------------------
    $ans = query("\nRun fvsetup?", "y");
    unless ( yes($ans) ) { pause() unless $noloop; return 0 }

    # check for pre-existing ARCHIVE
    #-------------------------------
    if ( defined($ENV{"ARCHIVE"}) ) {
        $expid_arc = "$ENV{ARCHIVE}/$expid{$key}";
        if (-d $expid_arc ) {
            print "\nARCHIVE directory $expid_arc already exists. ";
            $ans = query("Continue (y/n)", "n");
            quit("Quitting prior to setting up job.") unless yes($ans);
            pause() if $verbose;
        }
    }

    # check for pre-existing FVHOME
    #------------------------------
    $FVHOME = $fvhome{$key};
    if (-d $FVHOME) {
        print "\nThe directory $FVHOME already exists. ";
        $clean = query("Clean it (y/n)", "y");
        quit("No clean. Quitting prior to setting up job.") unless yes($clean);

        rmtree($FVHOME,$verbose);
        pause() if $verbose;
    }

    # check for previous use of expid
    #--------------------------------
    if ($nocheck) { $dflt = "n" }
    else          { $dflt = "y" }
    $ans = query("\nCheck for previous use of expid (y/n)?", $dflt);
    unless (neg($ans)) {
        print "\n" ."="x65 ."\n";
        $cmd = "$ESMABIN/idcheck.pl -rem $rem_acct{$key} $expid{$key}";
        print "$cmd\n";
        $status = system $cmd;
        $status = $status >> 8;
        print "="x65 ."\n\n";

        if ($status == 1) {
            print "Previous use of \"$expid{$key}\" was found.";
            $ans = query("\nContinue (y/n)?", "n");
            return unless yes($ans);
        }
}                              
 
    # fvsetup command
    #----------------
    $fvsetup = "$fvsetupScript -runjob";
    $fvsetup .= " $flags{$key}" if $flags{$key};
    $fvsetup .= " -dbqueue" if $dbqueue;

    if ($verbose) { $output = "| tee $fvsuLOG" }
    else          { $output = ">     $fvsuLOG" }

    $cmd = "$fvsetup < $rawinput $output";
    $cmd = "($cmd) 2> $fvsuERR" unless $debug or $verbose;
    print "\n$cmd\n\n" if $verbose;

    # run fvsetup
    #------------
    print "\n  Running fvsetup."
        . "\n  View fvsetup log from other window to check progress."
        . "\n  fvsetup log: $fvsuLOG\n"
        . "\n  Please wait ... ";
    $status = system "$cmd"; print "\n";

    # check for fvsetup errors
    #-------------------------
    if ($status or -s $fvsuERR) {
        print "\n        input file: $inFile{$key}\n"
            .   "        raw inputs: $rawinput\n"
            .   "            errlog: $fvsuERR\n"
            .   "            runlog: $fvsuLOG\n";
        quit("Quitting.") if checkERRS($fvsuERR);
    }
    unlink $fvsuERR if -z $fvsuERR;

    # verify that FVHOME directory was created
    #-----------------------------------------
    unless (-d $FVHOME) {
        print "FVHOME directory was not created: $FVHOME\n"
            . "Check for problems in fvsetup log: $fvsuLOG\n";
        quit("Quitting.");
    }

    # extract job name from fvsetup log file
    #---------------------------------------
    $jobn = extract($fvsuLOG, "Job Script", 1);

    # were any restarts Bootstrapped?
    #--------------------------------
    @bootstrap = extract($fvsuLOG, "BOOTSTRAP");
    $continueflag = 1;
    if (@bootstrap) {
        underline("The following file(s) will be BOOTSTRAP'ed");
        foreach (@bootstrap) { print " + $_\n" }
        $ans = query("\nDo you wish to continue (y/n)?", "y");
        $continueflag = 0 unless yes($ans);
    }

    # move logfile to LOG directory
    #------------------------------
    if (-e $fvsuLOG) {
        $logdir = "LOGs";
        unless (-d $logdir) { mkdir $logdir or warn "Error making $logdir" }
        if (-d $logdir) {
            print "\n";
            if (-e $fvsuERR) {
                $fvsuERR1 = dirname($fvsuERR) ."/$logdir" ."/" .basename($fvsuERR);
                my_move($fvsuERR, $fvsuERR1);
                print "  Errfile moved to $logdir directory: $fvsuERR1\n";
            }
            $fvsuLOG1 = dirname($fvsuLOG) ."/$logdir" ."/" .basename($fvsuLOG);
            my_move($fvsuLOG, $fvsuLOG1);
            print "  Logfile moved to $logdir directory: $fvsuLOG1\n";

        } else {
            print "\n  Errfile: $fvsuERR\n"
                .   "  Logfile: $fvsuLOG\n";
        }
    }

    return $continueflag;
}

#=======================================================================
# name - submit_job
# purpose - submit job to queue with system sbatch command
#=======================================================================
sub submit_job {
    use Cwd;
    my ($key, $here, $dflt, $ans);

    $key = shift @_;

    $here = cwd;
    $dflt = "n";
    if ($auto) { $dflt = "y" unless $autox }

    $ans = query("\nSubmit DAS batch (SLURM) job now (y/n)?", $dflt);
    return unless yes($ans);

    my_chdir("$fvhome{$key}/run");
    my_system("sbatch $jobn");
    my_chdir($here);
    pause() unless $noloop;
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#               UTILITY SUBROUTINES
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#=======================================================================
# name - blank
# purpose - test whether input string is blank
#=======================================================================
sub blank {
    my $str;
    $str = shift @_;
    return 1 if $str =~ /^\s*$/;
}

#=======================================================================
# name - checkERRS
# purpose - check the fvsetup error log for errors
#
# input parameter
# => $errlog: fvsetup error log
#
# return value
# => $status:  =0 continue
#              =1 (or non-zero) stop processing
#=======================================================================
sub checkERRS {
    my ($errlog, @lines, $status);

    $errlog = shift @_;
    $status = 0;

    if (-s $errlog) {

        # display contents if log is non-zero
        #------------------------------------
        print "\nerrlog contents:\n";
        print   "================\n";
        open ERR, "< $errlog" or die ">> ERROR << opening $errlog: $!";
        while (<ERR>) { ++$status unless /^\s*$/ or /^WARNING/i; print $_ }
        print   "================\n";
        close ERR;
    }
    return $status;
}

#=======================================================================
# name - checklist
# purpose - give user a checklist of things to check if error occurs
#           during fvsetup
#=======================================================================
sub checklist {
    my $key = shift @_;
    print "\ninput file: $inFile{$key}\n" if $key;
    print "- Check fvsetupflags in input file\n"
        . "- Be sure that responses in input file correspond to prompts "
        . "from fvsetup using fvsetupflags.\n";
}

#=======================================================================
# name - expand_string
# purpose - expand variables in string using values defined in input 
#           file header
#=======================================================================
sub expand_string {
    my ($string, %def, $var);
    $string= shift @_;
    %def = @_;

    return $string unless $string =~ /\$/;

    foreach $var (keys %def) {
        $string =~ s/\$\{$var\}/$def{$var}/g;
        $string =~ s/\$$var/$def{$var}/g;
    }
    return $string;
}

#=======================================================================
# name - extract
# purpose - extract values for a variable from a file, where the variables
#           are defined within the file with one of the following formats:
#                variable : value
#                variable = value
#           where blanks before or after ":" or "=" or ignored.
#
# input parameters
# => $fname: log from fvsetup
# => $var: variable name for which to extract value
# => $flg: 1 = quit after finding one value
#          2 = quit after finding two values, etc
#          no response = search to end of fname
#=======================================================================
sub extract {
    my ($fname, $var, $flg);
    my (@vals, $val, $cnt);

    # input parameter
    #----------------
    $fname = shift @_;
    $var = shift @_;
    $flg = shift @_;
    $flg = 0 unless $flg;

    # look for line containing $var
    #------------------------------
    $cnt  = 0;
    @vals = ();
    open INFIL, "< $fname" or die ">> Error << unable to open $fname: $!";
    while (<INFIL>) {
        chomp; s/^\s+|\s+$//g;

        if ( /\b$var\s*[:|=]\s*(\S+)$/ ) {
            $val = $1;
            if ($flg == 1) { return $val      }
            else           { push @vals, $val }
            last if $flg == ++$cnt;
        }
    }
    close INFIL;
    return @vals;
}

#=======================================================================
# name - maxlen
# purpose - find maximum length of an array of strings
#=======================================================================
sub maxlen {
    my (@strArr, $max, $len);
    @strArr = @_;
    $max = 0;
    foreach (@strArr) {
        $len = length( basename($_) );
        $max = $len if $len > $max;
    }
    return $max;
}

#=======================================================================
# name - my_chdir
# purpose - change directory and print message to STDOUT
#=======================================================================
sub my_chdir {
    my $dir = shift @_;
    print "chdir $dir\n" if $verbose;
    chdir $dir or die ">> Error << chdir $dir: $!";;
}

#=======================================================================
# name - my_copy
# purpose - wrapper for copy command with verbose option
#
# input parameters
# => $name1: name of file to copy
# => $name2: target name of file copy
# => $flag: (optional) print message to STDOUT if set
#=======================================================================
sub my_copy {
    use File::Copy "copy";
    my ($name1, $name2, $flag);

    $name1 = shift @_;
    $name2 = shift @_;
    $flag  = shift @_;

    $flag = 1 if $verbose;
    print "-> copy $name1, $name2\n" if $flag;
    copy $name1, $name2 or die ">> Error << copy $name1, $name2: $!"
}

#=======================================================================
# name - my_move
# purpose - wrapper for move command with verbose option
#
# input parameters
# => $name1: name of file to move
# => $name2: target name of file move
# => $flag: (optional) print message to STDOUT if set
#=======================================================================
sub my_move {
    use File::Copy "move";
    my ($name1, $name2, $flag);

    $name1 = shift @_;
    $name2 = shift @_;
    $flag  = shift @_;

    $flag = 1 if $verbose;
    print "-> move $name1, $name2\n" if $flag;
    move $name1, $name2 or die ">> Error << move $name1, $name2: $!"
}

#=======================================================================
# name - my_system
# purpose - issue system command and print message to STDOUT
#=======================================================================
sub my_system {
    my $cmd = shift @_;
    print "$cmd\n";
    system($cmd);
}

#=======================================================================
# name - my_unlink
# purpose - wrapper for unlink command with verbose option
#
# input parameters
# => @_: name of files to unlink
#=======================================================================
sub my_unlink {
    my $flag;

    $flag = 1 if $verbose;
    foreach (@_) {
        print "-> unlink $_\n" if $flag;
        unlink $_;
    }
}

#=======================================================================
# name - neg
# purpose - test whether input string is a "no" response
#=======================================================================
sub neg {
    my $str;
    $str = shift @_;
    $str = lc $str;           # make lowercase
    $str =~ s/^\s+|\s+$//g;   # remove leading/trailing blanks
    return 1 if $str eq "n" or $str eq "no"
}

#=======================================================================
# name - numeric
# purpose - this is a sort subroutine used in a sort command to perform
#           a numeric sort
#=======================================================================
sub numeric {
    return -1 if $a < $b;
    return  1 if $a > $b;
}

#=======================================================================
# name - pause
# purpose - pause processing until user input is detected
#=======================================================================
sub pause {
    my $dummy;
    unless ($auto) {
        print "\nHit <cr> to continue ... ";
        $dummy = <STDIN>;
    }
}

#=======================================================================
# name - query
# purpose - query user for a response; return the response
#
# input parameters
# => prompt: use this line to prompt for a response
# => dflt: (optional) default value to use for <cr> response
#=======================================================================
sub query {
    my ($prompt, $dflt, $ans);

    $prompt = shift @_;
    $dflt = shift @_;
    $dflt = "" if blank($dflt);

    # prepare prompt
    #---------------
    $prompt .= " ";
    $prompt .= "[$dflt] " unless blank($dflt);

    # get user response
    #------------------
    print $prompt;
    if ($auto) { $ans = $dflt; print "$ans\n" }
    else {
        chomp($ans = <STDIN>);
        if ( blank($ans) ) { $ans = $dflt unless blank($dflt) }
    }
    return $ans;
}

#=======================================================================
# name - yes
# purpose - test whether input string is a "yes" response
#=======================================================================
sub yes {
    my $str;
    $str = shift @_;
    $str = lc $str;           # make lowercase
    $str =~ s/^\s+|\s+$//g;   # remove leading/trailing blanks
    return 1 if $str eq "y" or $str eq "yes"
}

#=======================================================================
# name - quit
# purpose - clean up and quit
#=======================================================================
sub quit {
    my $str;

    $str = shift @_;

    # unlink rawInFiles
    #------------------
    unless ($debug) {
        foreach (sort numeric keys %rawInFile) {
            my_unlink($rawInFile{$_});
        }
    }
    print "\n$str\n\n" if $str;
    exit;
}

#=======================================================================
# name - stageInputs
# purpose - copy *.input files from the testsuites directory to the
#           $inputDir directory
#=======================================================================
sub stageInputs {
    use File::Basename qw(basename);
    my (@testInputs, $tst, $skip, $inFile, $cnt);

    @testInputs = (<$ESMATST/*.input>);
    $cnt = 0;

    if (@testInputs) {
        print "\nStaging testsuites input files\n";
        foreach (@testInputs) {
            $tst = basename $_;

            # only stage specified inputs, if inputs are specified
            #-----------------------------------------------------
            if ($specified) {
                $skip = 1;
                foreach $inFile (@inputFiles) {
                    if ($tst =~ m/^$inFile$/ or $tst =~ m/^$inFile\.input$/) {
                        $skip = 0;
                        last;
                    }
                }
                next if $skip;
            }
            my_copy($_, $inputDir, 1);
            $cnt++;
        }
        print "$cnt file(s) copied to $inputDir\n";
        print "\n";
    }
    else {
        print "\nNo input files found in directory:"
            . "\ndir: $ESMATST\n\n";
        quit();
    }
}

#=======================================================================
# name - underline
# purpose - prints a string to stdout and underlines it
#
# input parameters
# => string: the string to underline
# => flag: (optional); defaults to =1
#           =1: underline only with '-'
#           =2: underline and overline with '='
#=======================================================================
sub underline {
    my ($string, $flag);
    my (%pattern, $cnt1, $cnt2);
    my ($leadingBlanks, $remainder);

    $string = shift @_;
    $flag = shift @_;

    $pattern{1} = "-";
    $pattern{2} = "=";

    $flag = 1 unless $flag;
    $flag = 1 unless $flag == 2;

    if ($string =~ /^(\s*)(\S.*)/) {
        $leadingBlanks = $1;
        $remainder = $2;
    }
    $cnt1 = length($leadingBlanks);
    $cnt2 = length($remainder);
    print "\n";
    print " "x$cnt1.$pattern{$flag}x$cnt2."\n" if $flag == 2;
    print $leadingBlanks.$remainder."\n";
    print " "x$cnt1.$pattern{$flag}x$cnt2."\n";
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    use File::Basename;
    my $script = basename $0;
    print <<"EOF";

usage: $script [options] [file1 [file2 [..]]]
options
   -auto/-a           use dflt responses for queries; automatically submit job(s)
   -autox/-ax         use dflt responses for queries; do not submit job(s)
   -nocheck/-nc       do not check for previous use of expid
   -nofilter/-nf      do not exclude *.input files if fvsetupID does not match
   -d inputDir        directory location of saved *.input files
   -debug/-db         runjob debug mode; do not remove .rawInFile and ERR files
   -dbqueue/-dbq      send job to debug queue for faster processing
   -f fvsetup         fvsetup script to use; defaults to \$ESMABIN version
   -help/-h           print usage information
   -l                 get *.input files from local directory
   -OSx               proceed even if OS difference found
   -stage             copy testsuites *.input files to input directory
   -v                 verbose mode; turns on more verbage

[file1 [file2 [..]]]  Specified *.input files (see notes #2 and #3)

Notes
1. All input files must have the ".input" extension.
2. However, input files may be specified by name only in the command line,
   without including the ".input" extension.
3. If no file is specified as an input parameter, then the script will look
   in the input directory for input files which match the fvsetupID of this script
   (if -nofilter flag is set, then all inputs will be available).
4. The precedence for determining the input directory (location of *.input files)
   is as follows:
     i) location specified with the -d flag
    ii) local directory if -l flag is included
   iii) value of environment variable, \$SAVEDINPUTS, if set
    iV) system default, if none of the above

EOF
quit();
}
