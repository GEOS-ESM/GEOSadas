#!/usr/bin/env perl
#=======================================================================
# name - checkinput.pl
# purpose - make a .input file for fvsetup which is readable by runjob
#
# revision history
# 18Jun2010  Stassi   Initial version
# 21Oct2010  Stassi   Added auto option
#=======================================================================
use strict;
use warnings;

# global variables
#-----------------
my ($auto, $codeID, $debug, $ESMATST, $expid, $full, $fvroot);
my ($fvsetupID, $fvsetupScript, $ignoreOSdiff);
my ($inFile, $inFileChk, $inputDir, $newinput, $noloop, $replaceALL);
my ($sel, $sel_dflt, $siteID, $stage, $VERBOSE, $verbose);
my (@badFiles, @errFiles, @goodFiles, @inputFiles, @rawVALUE);
my (%def0, %def1, %head);

# main program
#-------------
{
    my ($status);
    init();
    intro();
    checkOS();
    getInputDir();
    getInputFiles();

    while (1) {
        chooseInput2check();
        last if $sel eq "0";

        checkInFile();
        getInputs();
        runSetup(\$status);
        compareFiles() unless $status;
        last if $noloop;
    }
    summary();
}

#=======================================================================
# name - init
# purpose - get runtime options and fvsetup command
#=======================================================================
sub init {
    use FindBin;
    use lib ("$FindBin::Bin");
    use Cwd ("cwd");
    use Getopt::Long;
    use GMAO_utils ("get_siteID");

    my ($ESMADIR, $ESMABIN, $BINDIR, $localdir, $help);

    $siteID = get_siteID();
    $sel_dflt = 1;
    $noloop = 0;

    # these values are set during build
    #----------------------------------
    $codeID = "@GIT_TAG_OR_REV@";
    $fvsetupID = "@fvID@";
    $ESMABIN = "@ESMABIN@";
    $ESMATST = "@ESMATST@";
    die ">> Error << $ESMABIN is not a directory;" unless -d $ESMABIN;

    # get runtime options
    #--------------------
    Getopt::Long::Configure("no_ignore_case");
    GetOptions( "a|auto"   => \$auto,
                "d=s"      => \$inputDir,
                "full"     => \$full,
                "l|local"  => \$localdir,
                "OSx"      => \$ignoreOSdiff,
                "db|debug" => \$debug,
                "h|help"   => \$help,
                "stage"    => \$stage,
                "v"        => \$verbose,
                "V"        => \$VERBOSE );
    usage() if $help;
    $verbose = 0 unless $verbose;
    $VERBOSE = 0 unless $VERBOSE;
    $verbose = 1 if $VERBOSE;

    @inputFiles = @ARGV;
    if ($localdir) { $inputDir = cwd() unless $inputDir }

    # find $fvroot and $fvsetupScript
    #--------------------------------
    $fvroot = dirname($ESMABIN);
    $fvsetupScript = "$ESMABIN/fvsetup";
    die ">> Error << cannot find $fvsetupScript;\n" unless -e $fvsetupScript;
}

#=======================================================================
# name - intro
# purpose - print introduction
#=======================================================================
sub intro {
    use File::Basename;
    system("clear");

    print "\n====================\n"
        . "\nCheck Fvsetup Inputs\n"
        . "\n====================\n";
    print "fvsetupID: $fvsetupID\n";
    print "fvsetup: " . dirname($fvsetupScript) ."\n";
}

#=======================================================================
# name - checkOS
# purpose - compare system OS file to file in $fvetc directory
#=======================================================================
sub checkOS {
    my ($sysfile, $dflt, $status, $ans);

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
    my ($success, $ans);

    # naked block to allow redo's
    #----------------------------
    {
        # query for $inputDir if not already supplied
        #--------------------------------------------
        $inputDir = queryInputDir() unless $inputDir;

        # sorry, but we don't want you writing to fvroot subdirectories
        #--------------------------------------------------------------
        if ($inputDir =~ m[$fvroot]) {
            warn ">> Error << this directory is not allowed: $inputDir";
            $inputDir = "";
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
    #--$inputDir = query("\n> Input directory:", $inputDir);

    return $inputDir;
}

#=======================================================================
# name - getInputFiles
# purpose - get list of input files
#=======================================================================
sub getInputFiles {
    my ($size, $file, $file1);

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

        die ">> Error << cannot find input file: $file" unless -e $file1;
        push @inputFiles, $file1;
    }

    # get list of setup input files, if not specified by user
    #--------------------------------------------------------
    @inputFiles = (<*.input>) unless @inputFiles;
}

#=======================================================================
# name - chooseInput2check
# purpose - query user for name of input file to check
#=======================================================================
sub chooseInput2check {
    use File::Basename;
    my ($size, $fmt, $cnt, $index);

    # short-circuit if only one file is available
    #--------------------------------------------
    if (scalar(@inputFiles) == 1) {
        $inFile = $inputFiles[0];
        $noloop = 1;
        $sel = 1;
        return;
    }

    # initialize variables
    #---------------------
    $inFile = "";
    $inFileChk = "";
    $newinput = 0;

    # short-circuit for auto mode
    #----------------------------
    if ($auto) {
        if ($sel) { $sel++   }
        else      { $sel = 1 }
        if ($sel > scalar(@inputFiles)) { $sel = 0; return }

        $index = $sel - 1;
        $inFile = $inputFiles[$index];
        return;
    }

    # initialize format
    #------------------
    $fmt = "%3i. %s\n";

    {
        # choose input file to check
        #---------------------------
        print "\n---------\n"
            .   "Main Menu\n"
            .   "---------\n"
            .   "directory: $inputDir\n"
            .   "fvsetup: $fvsetupScript\n\n";

        $size = scalar(@inputFiles);
        if ($size > 0) {
            print "  -------------------\n"
                . "  Input file to check\n"
                . "  -------------------\n";
            foreach $cnt (1..$size) {
                $index = $cnt - 1;
                printf $fmt, $cnt, basename($inputFiles[$index]);
            }
            print "\n";
        }
        else {
            $sel_dflt = -1 if $size < 1;
        }
        printf $fmt, -1, "create new expid using all default responses";
        printf $fmt,  0, "quit\n";
        $sel = query("Select:", $sel_dflt);

        # check selection validity
        #-------------------------
        unless ( found(\$sel, (-1..$size)) ) {
            print "\nUnrecognizable response.  Try again.\n";
            pause();
            exit if $auto;
            redo;
        }
        $sel_dflt = $sel;
    }

    # process user response
    #----------------------
    quit("Quitting.") if $sel eq  "0";

    if ($sel eq "-1") {
        $newinput = 1;
        while (! $expid) {
            $expid = query("\nEnter new Experiment ID:");
            if ($expid =~ /\W+/) {
                print "Illegal characters: $expid\n"
                    . "Try again\n";
                $expid = "";
            }
        }
        $inFile = "$inputDir/$expid.input";
        $sel_dflt = 0;
    }
    else {
        $index = $sel - 1;
        $inFile = $inputFiles[$index];
    }
}

#=======================================================================
# name - checkInFile
# purpose - check that $inFile exists and is named properly
#=======================================================================
sub checkInFile {
    use File::Basename;
    my $ext;

    # check existence of $inFile name
    #--------------------------------
    $inFile = "$inputDir/$inFile" if basename($inFile) eq $inFile;
    die ">> Error << cannot find input file: $inFile;" unless -e $inFile;

    # extract expid from input file name
    #-----------------------------------
    ($expid, $ext) = split /[.]/, basename($inFile);

    # check validity of $inFile name
    #-------------------------------
    die ">> Error << input filename does not have .input extension: $inFile;"
        unless $ext eq "input";
    die ">> Error << cannot extract expid from input filename: $inFile;"
        unless $expid;
}

#=======================================================================
# name - getInputs
# purpose - strip headers and responses from .input file if it exists;
#           otherwise, initialize headers and responses
#
# hash variables:
# => %def0: variable definitions exactly as read from .input file
# => %def1: variable definitions to use when expanding .input lines
#      where hash keys = variable name; hash values = variable values
#
# notes about differences between %def0 and %def1:
# 1. The %def0 hash may contain site-specific definitions such as "fvics;nccs"
#    which is the definition for variable "fvics" on the nccs machines. In this
#    case, the %def1 hash will include "fvics" only.
# 2. The %def0 hash is preserved so that the original definitions can be
#    rewritten to the new .input file.
#=======================================================================
sub getInputs {
    my ($readhead, $var, $val, $inputval);

    # initialize hashes
    #------------------
    %head = ();
    %def0 = ();
    %def1 = ();

    # read input file
    #----------------
    @rawVALUE = ();
    $def1{"expid"} = $expid;
    if (-e $inFile) {
        $readhead = 1;
        open INPUT, "< $inFile" or die ">> Error << opening $inFile: $!";
        while (<INPUT>) {

            # remove leading and trailing blanks
            #-----------------------------------
            chomp; s/^\s+|\s+$//g;
            $_ = envsubst($_);

            # get heading info
            #-----------------
            if ($readhead) {

                # look for description
                #---------------------
                $head{"description"}  = $1 if /^description\s*:\s*(.*)\s*$/;
                $head{"fvsetupflags"} = $1 if /^fvsetupflags\s*:\s*(.*)$/;

                # header variables
                #-----------------
                if ( /^def\s*:\s*(\S+)\s*=\s*(.*)$/ ) {
                    $var = $1;
                    $val = $2;
                    $def0{$var} = $val unless $def0{$var};

                    # site-specific values take precedent
                    #------------------------------------
                    if ($var =~ /;$siteID$/) {
                        $var =~ s/;$siteID$//;
                        $def1{$var} = $val;
                    }

                    # otherwise previously defined takes precedent
                    #---------------------------------------------
                    else {
                        $def1{$var} = $val unless $def1{$var};
                    }
                }                

                # included to handle obsolete fvics_{siteID} headers
                #---------------------------------------------------
                if ( /^fvics_(\S+):\s*(.+)\s*$/ ) {
                    $def0{"fvics;$1"} = $2;
                    if ($1 eq $siteID) {
                        $def1{"fvics"} = $2 unless $def1{"fvics"}
                    }
                }

                # end of headers
                #---------------
                $readhead = 0 if /---ENDHEADERS---/;
                $readhead = 0 if /---ENDHEADINGS---/;
                $readhead = 0 if /^>/;
                next if $readhead;
            }

            # strip out input values
            #-----------------------
            next unless /^>/;
            $inputval = $1 if /^>\s*(.*)$/;
            $inputval = "" unless $inputval =~ /\S+/;
            $inputval = expand_string($inputval);
            push @rawVALUE, $inputval;
        }
        close INPUT;
    }

    # initialize fvsetupflags if not set
    #-----------------------------------
    $head{"fvsetupflags"} = "" unless $head{"fvsetupflags"};

    # add plenty of cushion to @rawVALUE array for good measure
    #----------------------------------------------------------
    for (1..300) { push @rawVALUE, "" }
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
# name - runSetup
# purpose - run fvsetup to create another .input file
#
# out parameter
# => $return_status: address of return status variable
#=======================================================================
sub runSetup {
    use File::Path;
    my ($return_status, $status, $errlog_status);
    my ($fvsuLOG, $fvsuLOG1, $fvsuERR, $logdir, $rawInFile, $savefile);
    my ($FVHOME, $output, $flags, $var, $cmd);
    my ($modFile, $line, $modline);

    # initial return status as zero
    #------------------------------
    $return_status = shift @_;
    $$return_status = 0;

    # file names
    #-----------
    $fvsuLOG = "$inputDir/fvsetup.chk.$expid.LOG";
    $fvsuERR = "$inputDir/fvsetup.chk.$expid.ERR";
    $rawInFile = "$inputDir/.rawInFile.chk.$expid";

    unlink $fvsuLOG if -e $fvsuLOG;
    unlink $fvsuERR if -e $fvsuERR;
    unlink $rawInFile if -e $rawInFile;

    # fvsetup command
    #----------------
    if ($VERBOSE) { $output = "| tee " .$fvsuLOG }
    else          { $output = "> "     .$fvsuLOG }

    # write raw values to $rawInFile
    #-------------------------------
    open RAW, "> $rawInFile" or die ">> Error << opening $rawInFile: $!";
    if (@rawVALUE) {
        foreach (@rawVALUE) { print RAW "$_\n" }
    }
    close RAW;

    if ($newinput) {
        print "\n  Creating new input file: ". basename($inFile) ."\n";
    }
    else {
        print "\n  Checking input file: ". basename($inFile)
            . "\n  Experiment directory $expid will not be touched.\n";
    }
    print "\n  View fvsetup log from other window to check progress."
        . "\n  fvsetup log: $fvsuLOG\n"
        . "\n  Please wait ... ";

    # run fvsetup
    #------------
    $flags = "-check";
    $flags .= " $head{fvsetupflags}" if $head{"fvsetupflags"};
    $flags .= " -save 2" if $full;
    foreach $var (sort keys %def0) {
        next if $var eq "expid";
        $flags .=  " -header 'def: $var = $def0{$var}'";
    }

    $cmd = "$fvsetupScript $flags < $rawInFile $output";
    $cmd = "($cmd) 2> $fvsuERR" unless $debug;
    print "\n  $cmd\n" if $verbose;

    $status = system $cmd; print "\n\n";

    # check for fvsetup errors
    #-------------------------
    $FVHOME = extract($fvsuLOG, "FVHOME", 1);
    $errlog_status = checkERRS($fvsuERR);

    if ($status or $errlog_status) {
        print "\n>> Error << running fvsetup:\n"
            .   "    fvsetup status: $status\n"
            .   "     errlog status: $errlog_status\n"
            .   "        input file: $inFile\n"
            .   "        raw inputs: $rawInFile\n"
            .   "            runlog: $fvsuLOG\n"
            .   "            errlog: $fvsuERR\n\n";
        rmtree($FVHOME, $verbose) unless $debug;
        print ">> Errors found during setup <<\n";

        push @errFiles, $inFile;
        $$return_status = $status + $errlog_status;
        return;
    }
    unlink $fvsuERR if -z $fvsuERR;

    # extract savefile value from LOG
    #--------------------------------
    $savefile = extract($fvsuLOG, "SavedInputsFile", 1);
    die ">> Error << check file not found: $savefile;" unless -e $savefile;
    die ">> Error << check file is empty: $savefile;" if -z $savefile;

    $inFileChk = "$inputDir/".basename($inFile).".check";
    my_copy($savefile, $inFileChk);

    # modify the *.input file written by fvsetup
    #-------------------------------------------
    ($modFile = $inFileChk) =~ s/$expid.input/${expid}M.input/;
    print "  Output from fvsetup: $inFileChk\n";
    print "- transfer $inFileChk -> $modFile\n" if $verbose;

    open NEW, "< $inFileChk" or die ">> Error << opening $inFileChk: $!";
    open MOD, "> $modFile"   or die ">> Error << opening $modFile: $!";

    foreach $line (<NEW>) {
        chomp $line;

        # transfer previous description if one existed
        #---------------------------------------------
        if ($line =~ /^description:\s*$/ and $head{"description"}) {
            $modline = $line .$head{"description"};
            print "  mod: [$line] => [$modline]\n";
            $line = $modline;
        }
        print MOD $line."\n";
    }
    close NEW;
    close MOD;

    my_unlink($inFileChk);
    my_move($modFile, $inFileChk, $verbose);

    # move logfile to LOG directory
    #------------------------------
    if (-e $fvsuLOG) {
        $logdir = "LOGs";
        unless (-d $logdir) { mkdir $logdir or warn "Error making $logdir" }
        if (-d $logdir) {
            $fvsuLOG1 = dirname($fvsuLOG) ."/$logdir" ."/" .basename($fvsuLOG);
            my_move($fvsuLOG, $fvsuLOG1, $verbose);
            print "\n  Logfile moved to $logdir directory: $fvsuLOG1\n";
        } else {
            print "\n  Logfile: $fvsuLOG\n";
        }
    }

    # clean FVHOME; remove error and redir files
    #-------------------------------------------------
    unless ($debug) {
        pause() if $verbose;
        rmtree($FVHOME, $verbose);
        pause() if $verbose;
        my_unlink($fvsuERR);
        my_unlink($rawInFile);
    }
}

#=======================================================================
# name - compareFiles
# purpose - compare newly created .input with original
#=======================================================================
sub compareFiles {
    my $status;

    # if new file, then rename and return
    #------------------------------------
    if ($newinput) { replaceInFile(); return }

    # check for differences
    #----------------------
    $status = system("cmp -s $inFile $inFileChk");

    # if differences found
    #---------------------
    if ($status) {
        push @badFiles, $inFile;
        print "\n"
            . "  *** Differences were found ***\n"
            . "  old: $inFile\n"
            . "  new: $inFileChk\n";

        if ($auto) { pause() }
        else       { show_differences() }
    }

    # if no differences found
    #------------------------
    else {
        push @goodFiles, $inFile;
        print "  input file: ", basename($inFile). "\n"
            . "  No problem found\n";
        unlink $inFileChk;

        if ($noloop) { print "\n" }
        else         { pause()    }
    }

    $sel_dflt++;
    $sel_dflt = 0 if $sel_dflt > scalar(@inputFiles);
    return;
}

#=======================================================================
# name - show_differences
# purpose - show differences between original and newly created input file
#
# input parameters
# => $flag: if =1, go directly into diffs during first loop
#           if =0, give options
#=======================================================================
sub show_differences {
    use File::Basename;
    my ($flag, $len, $ans, $file, $rFLG);

    $flag = shift @_;
    $len = length(basename($inFile));
    $rFLG = 0;

    while (1) {

        # use $flag for response if provided
        #-----------------------------------
        if (defined($flag)) { $ans = $flag; $flag = undef }

        # or give menu options
        #---------------------
        else {
            if ( $ans ) { $ans = 0 } else { $ans = 1 }

            print "\n"
                . "  ". "-"x$len ."\n"
                . "  ". basename($inFile) ."\n"
                . "  ". "-"x$len ."\n"
                . "  1. show differences\n"
                . "  2. replace old with new\n"
                . "  ". "-"x$len ."\n"
                . "  0. return\n\n";

            $ans = query("  option:", $ans);
        }

        # 0. return to main menu
        #------------------------
        $ans = 2 if $ans eq "r";
        $ans = 0 if $ans =~ m/\D/;
        if ($ans == 0) {
            print "\noriginal inputs file left unchanged: $inFile";
            print "\nnew input file: $inFileChk\n\n";
            last;
        }

        # 1. show differences
        #--------------------
        elsif ($ans == 1) {
            system "xxdiff $inFile $inFileChk";
        }

        # 2. replace old with new
        #------------------------
        elsif ($ans == 2) {
            replaceInFile();
            $rFLG++;
            for (0..$#badFiles) {
                $file = shift @badFiles;
                push @badFiles, $file unless $file eq $inFile;
            }
            push @goodFiles, $inFile;
            last;
        }

        # ???
        #----
        else {
            print "Unrecognizable response: \#$ans\#\n"
                . "Try again.";
            pause();
        }
    }
    return $rFLG;
}

#=======================================================================
# name - summary
# purpose - show which files exactly reproduced themselves when inputted
#           into fvsetup (@goodFiles) and which files did not (@badFiles)
#=======================================================================
sub summary {
    $auto = 0;   # turn off auto mode for summary

    if (@errFiles) {
        print "\nThe following input files had errors during setup:\n";
        foreach (@errFiles) { print "=> $_\n" }
        print "\n";
    }

    if (@goodFiles) {
        print "The following input files are okay:\n";
        foreach (@goodFiles) { print "=> $_\n" }
        print "\n";
    }

    if (@badFiles) {
        print "The following input files have mismatches:\n";
        foreach (@badFiles) { print "=> $_\n" }
        print "\n";
        postsummary();
    }
}

#=======================================================================
# name - postsummary
# purpose - give option to show differences in @badFiles
#=======================================================================
sub postsummary {
    use File::Basename;
    my ($ans, $numbf, $cnt, $nnn, $index);

    $ans = 0;
    $auto = 0;
    $replaceALL = 0;

    {
        die ">> Infinite Loop << ($nnn);" if ++$nnn > 99;
        last unless ($numbf = scalar @badFiles);

        if ($replaceALL) {
            $inFile = $badFiles[0];
            $inFileChk = "$inFile.check";
            show_differences("2");
            redo;
        }

        # show view/replace menu
        #-----------------------
        $ans++;
        $ans = 0 if $ans > $numbf;

        print "  ----------------\n"
            . "  View differences\n"
            . "  ----------------\n";

        $cnt = 0;
        foreach (@badFiles) { printf "%3i. %s\n", ++$cnt, basename($_) }
        print "  -------\n"
            . "  r. replace all\n"
            . "  0. quit\n";

        # get user response
        #------------------
        $ans = query("\n  select:", $ans);
        last unless $ans;
        if ($ans eq "r") { $replaceALL = 1; redo }

        if ($ans < 1 or $ans > $cnt) {
            print "Unrecognizable response; $ans\n"
                . "Try again.";
            redo;
        }

        $index = $ans - 1;
        $inFile = $badFiles[$index];
        $inFileChk = "$inFile.check";
        $ans-- if show_differences("1");
        
        redo;
    }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                         UTILITY subroutines
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#=======================================================================
# name - checkERRS
# purpose - check the fvsetup error log for errors
#
# input parameter
# => $errlog: fvsetup error log
#
# return value
# => $status: =0 continue
#             =1 (or non-zero) stop processing
# notes:
# 1. $status equals the number of lines in the error log, minus blank lines
#    and lines which begin with the word "WARNING" (case insensitive).
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
        open ERRLOG, "< $errlog" or die ">> ERROR << opening $errlog: $!";
        while (<ERRLOG>) { ++$status unless /^\s*$/ or /^WARNING/i; print $_ }
        print   "================\n";
        close ERRLOG;
    }
    return $status;
}

#=======================================================================
# name - expand_string
# purpose - expand variables in string using values defined in input 
#           file header
#=======================================================================
sub expand_string {
    my ($string, $var);
    $string= shift @_;

    return $string unless $string =~ /\$/;

    foreach $var (keys %def1) {
        $string =~ s/\${$var}/$def1{$var}/g;
        $string =~ s/\$$var/$def1{$var}/g;
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
# name - found
# purpose - determine whether a value is found in an array
#=======================================================================
sub found {
    my ($valAddr, @arr);
    my ($val, $found);

    $valAddr = shift @_;
    @arr = @_;

    $val = $$valAddr;
    $val =~ s/^\s+|\s+$//g;

    $found = 0;
    foreach (@arr) { $found = 1 if $val eq $_ }
    return $found;
}

#=======================================================================
# name - pause
# purpose - pause processing until user input is detected
#=======================================================================
sub pause {
    my $dummy;

    if ($auto) {
        print "\n" ."~"x33 ."\n";
        return;
    }
    print "\nHit <cr> to continue ... ";
    $dummy = <STDIN>;
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
# name - blank
# purpose - test whether input string is blank
#=======================================================================
sub blank {
    my $str;
    $str = shift @_;
    $str = "" unless defined($str);
    return 1 if $str =~ /^\s*$/;
}

#=======================================================================
# name - yes
# purpose - test whether input string is a "yes" response
#=======================================================================
sub yes {
    my $str;
    $str = shift @_;
    $str = lc $str;           # make lowercase
    $str =~ s/^\s+|\s+$//g;
    return 1 if $str eq "y" or $str eq "yes"
}

#=======================================================================
# name - neg
# purpose - test whether input string is a "no" response
#=======================================================================
sub neg {
    my $str;
    $str = shift @_;
    $str = lc $str;           # make lowercase
    $str =~ s/^\s+|\s+$//g;
    return 1 if $str eq "n" or $str eq "no"
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
    print "- copy $name1, $name2\n" if $flag;
    copy $name1, $name2 or die ">> Error << copy $name1, $name2: $!"
}

#=======================================================================
# name - my_move
# purpose - wrapper for move command with verbose option
#
# input parameters
# => $name1: name of file to copy
# => $name2: target name of file copy
# => $flag: (optional) print message to STDOUT if set
#=======================================================================
sub my_move {
    use File::Copy "move";
    my ($name1, $name2, $flag);

    $name1 = shift @_;
    $name2 = shift @_;
    $flag  = shift @_;

    $flag = 1 if $verbose;
    print "- move $name1, $name2\n" if $flag;
    move $name1, $name2 or die ">> Error << move $name1, $name2: $!"
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
        print "- unlink $_\n" if $flag;
        unlink $_;
    }
}

#=======================================================================
# name - quit
# purpose - clean up and quit
#=======================================================================
sub quit {
    my $str;

    $str = shift @_;
    print "$str\n\n" if $str;
    exit;
}

#=======================================================================
# name - replaceInFile
# purpose - replace $inFile with $inFileChk
#=======================================================================
sub replaceInFile {

    # check that new file exists
    #---------------------------
    if (! -e $inFileChk) {
        warn ">> Error << cannot find new input file: $inFileChk";
        return;
    }

    # check that new file is not zero length
    #---------------------------------------
    if (-z $inFileChk) {
        warn ">> Error << new input file has zero length: $inFileChk";
        return;
    }

    # rename old file with tilde (if it exists)
    #------------------------------------------
    my_move($inFile, $inFile."~", $verbose) if -e $inFile;

    # rename new file
    #----------------
    my_move($inFileChk, $inFile, $verbose);
    print "\nold input file replaced with new: $inFile\n";

    if ($noloop) { print "\n" }
    else         { pause() unless $replaceALL }
}

#=======================================================================
# name - stageInputs
# purpose - copy *.input files from the testsuites directory to the
#           $inputDir directory
#=======================================================================
sub stageInputs {
    my @testInputs;

    @testInputs = (<$ESMATST/*.input>);

    if (@testInputs) {
        print "\nStaging testsuites input files\n";
        foreach (@testInputs) { my_copy($_, $inputDir, 1) }
        print "\n";
    }
    else {
        print "\nNo input files found in directory:"
            . "\ndir: $ESMATST\n\n";
        quit();
    }
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    use File::Basename;
    my $script = basename $0;
    print <<"EOF";

This utility attempts to evaluate whether input files are in the proper format
to be used by the runjob utility and fvsetup script.

filename format: [name].input,
   where [name] defaults to expid

usage: $script [options]
options
   -auto/-a           use defaults rather than prompting for responses
   -d inputDir        directory location of saved *.input files
   -full              write full responses in *.input files
   -local/-l          use local directory to look for saved *.input files
   -OSx               proceed even if OS difference found
   -debug/-db         do not remove .rawInFile and error logfile
   -help/-h           print usage information
   -noRL              do not remove the following labels from tag name:
                      "_UNSTABLE" "_INTERIM", "_OPS", "_rejected", "_retired"
                      by default these labels are removed
   -stage             copy testsuites *.input files to input directory
   -v                 verbose mode; turns on more verbage
   -V                 superverbose mode; verbose + fvsetup session

Notes
1. All input files must have the ".input" extension.
2. However, input files may be specified by name only in the command line,
   without including the ".input" extension.
3. If no file is specified as an input parameter, then the script will look
   at all input files in the local directory.
4. The precedence for determining the input directory (location of *.input files)
   is as follows:
     i) location specified with the -d flag
    ii) local directory if -l flag is included
   iii) value of environment variable, \$SAVEDINPUTS, if set
    iV) system default, if none of the above

EOF
quit();
}
