#!/usr/bin/env perl
#=======================================================================
# name - monthly_means.pl
# purpose - script for calculating monthly means of fvDAS products
#
# NOTES:
# 1. Typical monthly means processing will pass through this program twice
#    - 1st pass: prefetch inputs
#    - 2nd pass: check inputs and perform monthly means
# 2. The $IN_TAR_INFO and/or $OUT_TAR_INFO files are written in the $workdir
#    during the 1st pass when inputs are fetched. They are text files which
#    are used for passing information to the clean inputs job, where tarring
#    occurs.
# 3. The $IN_TAR_INFO file is written if inputs are extracted from an archived
#    tarfile.
#    $IN_TAR_INFO contains one record:
#    == "full" if inputs are retrieved from a full tarfile
#    == "partial" if inputs are retrieved from a partial tarfile
# 4. The $OUT_TAR_INFO file is written if some or all inputs are fetched
#    individually from the archives.
#    $OUT_TAR_INFO contains three records:
#    1st: $local_dir = location to write tarfile when tarring inputs
#    2nd: $remote_dir = location of individual archived inputs to be deleted
#                       after successful completion of tar operation
#    3rd: == "full" if all inputs are available
#         == "partial" if some inputs are missing
#         == "empty" if no inputs are found (i.e. no need to tar)
# 5. The $IN_TAR_INFO and/or $OUT_TAR_INFO files are read in the
#    clean_inputs.$ftype.$yyyymm.j job scripts, if tarring is requested.
#    > If the $IN_TAR_INFO file is found, then no tarring will occur if the
#      input archived tarfile is "full", i.e. it already contains all inputs.
#      Tarring again would just re-tar all the inputs extracted from the
#      archived tarfile.
#    > If the input archived tarfile is "partial", then the inputs will be
#      tarred again, creating either a new partial tarfile to overwrite the
#      one in the archive or a new full tarfile to add to the archive.
#    > The $OUT_TAR_INFO indicates
#      - where to write the tarfile locally, prior to archival ($local_dir)
#      - where to delete individual archived inputs ($remote_dir)
#      - whether to write a full tarfile, a partial tarfile, or to not write
#        a tarfile at all.
# 
# !REVISION HISTORY:
# 20160123   Stassi    Modified version of monthly_means_mpi.pl
#=======================================================================
use strict;
use warnings;
my (@SEARCH_PATH, $fvhome, $fvroot);

# pre-process to define @SEARCH_PATH
#-----------------------------------
BEGIN {
    use FindBin qw($Bin);
    use Getopt::Long (":config", "pass_through");

    # get fvhome and fvroot directories
    #----------------------------------
    GetOptions( "H=s" => \$fvhome,
                "R=s" => \$fvroot );
    $fvhome = $ENV{"FVHOME"} unless $fvhome;
    $fvroot = $ENV{"FVROOT"} unless $fvroot;

    die "ERROR: No FVHOME is set. Use -H;" unless $fvhome;
    die "ERROR: No FVROOT is set. Use -R;" unless $fvroot;
    die "ERROR: Cannot find FVHOME, $fvhome;" unless -e $fvhome;
    die "ERROR: Cannot find FVROOT, $fvroot;" unless -e $fvroot;

    @SEARCH_PATH = ("$fvhome/run", $Bin, "$fvroot/bin");
}
use lib @SEARCH_PATH;

# global variables
#-----------------
my ($archive, $date, $delete, $dmget_bin, $do_dmput, $do_tar, $endday, $expid);
my ($fileToken, $ftype, $hm, $htype, $ignore, $links, $localflg, $local_dir);
my ($monitorloc, $monthly_means_x, $nodiurnal, $num_days, $prefetch, $rcfile);
my ($remote_dir, $remote_machine, $remote_user, $run_config, $silo_dir);
my ($silo_work, $script, $startday, @times, $verbose, $workdir, $yyyymm);
my (@mean_files, @remote_file_list, @remove);

# main program
#-------------
{
    my ($op);

    print "\nDATE at start\n";
    system("date"); print "\n";

    init();
    fetch_inputs();
    calculate_means() unless $prefetch;

    if ($prefetch) { $op = "PREFETCH"    }
    else           { $op = "CALCULATION" }
    wrapup(0, $delete, "Monthly Means $op successful for $ftype");
}

#=======================================================================
# name - init
# purpose - get runtime parameters
#=======================================================================
sub init {
    use File::Basename qw(basename dirname);
    use File::Path qw(mkpath);
    use Getopt::Long (":config", "no_ignore_case");
    use Manipulate_time qw(get_htype get_hours num_days_in_month token_resolve);
    use Remote_utils qw(splitrfile);
    use Perl_Config qw(perl_config);
    my ($filestring, $help, $null, $SILO_DIR);
    my (%opts, $fvarch, $fpathToken, @parts);
    my (@remote_info, $remote_file_path);
    my ($year, $month);

    $script = basename($0);

    # get run options
    #----------------
    # NOTE: -H and -R flags are retrieved in BEGIN block
    #---------------------------------------------------
    GetOptions( "C"        => \$delete,
                "c=s"      => \$rcfile,
                "D=i"      => \$date,
                "f=s"      => \$filestring,
                "h"        => \$help,
                "i"        => \$ignore,
                "L"        => \$localflg,
                "l"        => \$links,
                "M=s"      => \$monthly_means_x,
                "m=s"      => \$monitorloc,
                "nd"       => \$nodiurnal,
                "prefetch" => \$prefetch,
                "p"        => \$do_dmput,
                "r=s"      => \$run_config,
                "S=s"      => \$SILO_DIR,
                "T=s"      => \$htype,
                "t"        => \$do_tar,
                "v"        => \$verbose,
                "w=s"      => \$workdir );
    usage() if $help;

    # check for required inputs
    #--------------------------
    die "Error: Date (yyyymm) is required; Use -D;"        unless $date;
    die "Error: Filestring is required; Use -f;"           unless $filestring;

    $rcfile = "$fvroot/etc/time_ave.rc" unless $rcfile;
    unless ($prefetch) {
        die "Error: time_ave.rc file is required. Use -c;" unless $rcfile;
        die "Error: time_ave.rc file not found;"           unless -e $rcfile;

        # check monthly means program
        #----------------------------
        $monthly_means_x = "$fvroot/bin/time_ave.x"        unless $monthly_means_x;
        die "Error: monthly_means_x is required. Use -M;"  unless -e $monthly_means_x;
        die "Error: $monthly_means_x not executable;"      unless -x $monthly_means_x;
    }

    # get environment variables
    #--------------------------
    $opts{"config_file"} = "$fvhome/run/FVDAS_Run_Config";
    perl_config(%opts);

    $expid     = $ENV{"EXPID"};
    $dmget_bin = $ENV{"FVDMGET"};
    $fvarch    = $ENV{"FVARCH"};

    # extract values from $filestring
    #--------------------------------
    $fileToken = basename($filestring);
    $fpathToken = dirname($filestring);
    if ($fileToken =~ /%h2%n2/) { $hm = "m" }
    else                        { $hm = "h" }

    # get $ftype by removing expid, datetime, and ext segments
    #---------------------------------------------------------
    @parts = split /[\.]/, $fileToken;
    shift(@parts); pop(@parts); pop(@parts);
    $ftype = join ".", @parts;

    # get $htype from $ftype, unless alternate htype value given
    #-----------------------------------------------------------
    $htype = get_htype($ftype) unless $htype;

    $archive = "$fvarch/$expid/$fpathToken";
    $silo_dir = "$fvhome/$fpathToken";

    # determine dates and times
    #--------------------------
    $yyyymm = substr($date,0,6);
    $year  = substr($yyyymm,0,4);
    $month = substr($yyyymm,4,2);

    $num_days = num_days_in_month($year, $month);
    $startday = $yyyymm."01";
    $endday   = $yyyymm.$num_days;

    @times = ();
    @times = get_hours($htype, "0", "all", $hm);
    @times = get_hours_HIST($ftype) unless @times;
    die "Error. Unrecognized hour type $htype;" unless @times;

    # defaults
    #---------
    $localflg = 0 if $prefetch;
    $delete = 0 unless $delete;
    $dmget_bin = "" unless $dmget_bin;
    $run_config = "DEFAULT" unless $run_config;

    if ($SILO_DIR) {
        $delete = 0;
        $links = 0;
        $silo_work = 1;
        $silo_dir = $SILO_DIR;
        $workdir = token_resolve($SILO_DIR, $startday);
    }
    ($remote_user, $remote_machine, $remote_file_path) = splitrfile($archive);
    $remote_dir = token_resolve($remote_file_path, $startday);
    print "\$remote_dir = $remote_dir\n" if $verbose;

    # define working directory, if not already defined
    #-------------------------------------------------
    unless ($workdir) {
        if ($ENV{"SCRATCH"}) { $workdir = "$ENV{SCRATCH}/monthly/$$" }
        else                 { $workdir = "$ENV{TMPDIR}/monthly/$$"  }
    }

    # create work directory, if necessary
    #------------------------------------
    unless (-d $workdir) {
        print "$workdir does not exist - creating...\n" if $verbose;
        mkpath($workdir, 1, 0755) or die "Error. Could not create $workdir;";
    }

    # environment variables
    #----------------------
    $ENV{"PATH"} = join ":", @SEARCH_PATH, $ENV{"PATH"};
    $ENV{"PATH"} = join ":", $ENV{"PATH"}, $ENV{"PBS_O_PATH"} if $ENV{"PBS_O_PATH"};
    $ENV{"SHELL"} = "/bin/csh";

    # parse location data
    #--------------------
    $local_dir = token_resolve($silo_dir, $startday);
    die "Error. Unable to define local_dir variable;" unless $local_dir;
    print "\$local_dir = $local_dir\n" if $verbose;

    # create local directory, if necessary
    #-------------------------------------
    unless (-d $local_dir ) {
        print "local_dir, $local_dir, does not exist - creating...\n" if $verbose;
        my $status = mkpath($local_dir, 1, 0755);
        die "Error. Could not create $local_dir;" unless -d $local_dir;
    }
}

#=======================================================================
# name - get_hours_HIST
# purpose - get ftype hours based on its frequency and duration
#           in the HISTORY.rc file.
#=======================================================================
sub get_hours_HIST {
    my ($ftype, $HISTORY, $freq, $dur, @hours);
    $ftype = shift;

    $HISTORY = "$fvhome/run/HISTORY.rc.tmpl";
    $freq = extract_value($HISTORY, "$ftype.frequency");
    $dur  = extract_value($HISTORY, "$ftype.duration");
    $dur = $freq unless $dur;

    @hours = ();
    if ($dur eq "010000") { @hours = qw(00 01 02 03 04 05 06 07 08 09 10 11
                                        12 13 14 15 16 17 18 19 20 21 22 23 ) }
    if ($dur eq "030000") { @hours = qw(00 03 06 09 12 15 18 21) }
    if ($dur eq "060000") { @hours = qw(00 06 12 18) }
    if ($dur eq "240000") { @hours = qw(00 03 06 09 12 15 18 21) }

    return @hours;
}

#=======================================================================
# name - fetch_inputs
# purpose - make input files local prior to calculating monthly means
#           and/or tarring
#=======================================================================
sub fetch_inputs {
    use File::Basename qw(basename);
    use File::Compare qw(compare);
    use Remote_utils qw(rm_remote_file);
    use Sys::Hostname ("hostname");

    my ($stem, $full_tarfile, $partial_tarfile, $check_inputs);
    my ($IN_TAR_INFO, $OUT_TAR_INFO);

    $stem = "$expid.$ftype.$yyyymm";
    $full_tarfile = "$stem.tar";
    $partial_tarfile = "$stem.partial.tar";
    $IN_TAR_INFO = "$workdir/IN_TAR.$ftype";
    $OUT_TAR_INFO = "$workdir/OUT_TAR.$ftype";

    # change directory to work directory
    #-----------------------------------
    chdir $workdir or die "Error. Cannot change directory to $workdir;\n";
    print "Current work directory is $workdir\n" if $verbose;
    print "Fetching $ftype files for $num_days days of $yyyymm\n";

    # check for archived inputs tarfile
    #----------------------------------
    $check_inputs = 1;
    unless ($localflg) {

        # look for full month tarfile
        #----------------------------
        if (get_inputs_from_tarfile($full_tarfile)) {
            print "Full tarfile found in archives\n\n" if $verbose;
            open IFO, "> $IN_TAR_INFO" or die "Error opening $IN_TAR_INFO";
            print IFO "full\n";
            close IFO;
            $check_inputs = 0;
        }

        # look for partial month tarfile
        #-------------------------------
        elsif (get_inputs_from_tarfile($partial_tarfile)) {
            print "Partial tarfile found in archives\n\n" if $verbose;
            open IFO, "> $IN_TAR_INFO" or die "Error opening $IN_TAR_INFO";
            print IFO "partial\n";
            close IFO;
        }
        else { print "No tarfile found in archives\n\n" if $verbose }
    }

    # check for individual inputs
    #----------------------------
    if ($check_inputs) {
        fetch_individual_inputs($OUT_TAR_INFO);
    }
}

#=======================================================================
# name - get_files_from_tarfile
# purpose - look for archived tarfile and if found, then retrieve it,
#           untar it, and then remove it
#
# input parameter
# => $tarfile: name of tarfile from which to get files
#
# return values
# => success   if ==1, then tarfile was retrieved and untarred
#              if ==0, then tarfile not found or not successfully untarred
#=======================================================================
sub get_inputs_from_tarfile {
    use Remote_utils qw(rget);
    my ($tarfile, $remote_ref, %opts, $rc, $success);
    $tarfile = shift;
    $success = 0;

    $remote_ref = "$remote_user\@$remote_machine:$remote_dir/$tarfile";
    print "\nLooking for tarfile in archive: $remote_ref\n" if $verbose;

    %opts = ();
    $opts{"debug"} = $verbose if $verbose;
    $opts{"run_config"} = $run_config;
    rget($remote_ref, $tarfile, \%opts);

    # if found, then extract inputs from tarfile
    #-------------------------------------------
    if (-f $tarfile and -s $tarfile) {
        print "Tarfile acquired from archive: $tarfile\n" if $verbose;

        $rc = system_("tar xf $tarfile");
        die "Unable to extract inputs from tarfile: $tarfile;" if $rc;

        print "Inputs extracted from tarfile: $tarfile\n" if $verbose;
        print "removing $tarfile from workdir\n" if $verbose;
        unlink($tarfile);
        $success = 1;
    }
    return $success;
}

#=======================================================================
# name - fetch_individual_inputs
# purpose - 
#=======================================================================
sub fetch_individual_inputs {
    use Manipulate_time qw(token_resolve);
    use Remote_utils qw(rdmget rget);
    my ($currday, $filename, $full_local_host_name, $host, $local_ref);
    my ($local_tarfile, $missing_inputs, $rc, $rget_rc, $rdmget_rc);
    my ($remote_file, $remote_ref, $time, $times_addr, $try);
    my (%opts, $OUT_TAR_INFO);

    $OUT_TAR_INFO = shift;

    # look for individual file inputs
    #--------------------------------
    print "Looking for individual $ftype inputs\n" if $verbose;
    foreach $currday ($startday..$endday) {
        foreach $time (@times) {

            $filename = token_resolve($fileToken, $currday, $time);
            print "\$filename = $filename\n" if $verbose;
            push @remove, $filename if $delete;

            # look for file in work directory
            #--------------------------------
            unless (-f $filename and -s $filename) {
                print "$filename not found in work directory\n" if $verbose;
                wrapup(1, $delete, "Running $script failed missing $filename.") if $silo_work;

                # look for file in stage directory
                #---------------------------------
                print "Checking for $local_dir/$filename\n" if $verbose;
                if (-f "$local_dir/$filename") {

                    # create symlink
                    #---------------
                    if ($links) {
                        print "linking $local_dir/$filename to $filename\n" if $verbose;
                        $rc = symlink "$local_dir/$filename", $filename;
                        wrapup(1, $delete, "Cannot create symbolic link to $filename") unless $rc;
                    }

                    # or rget to work dir
                    #--------------------
                    else {
                        %opts = ();
                        $opts{"debug"} = $verbose if $verbose;
                        $opts{"run_config"} = $run_config;
                        $opts{"preserve"} = 1;

                        $host = hostname();
                        $full_local_host_name = (gethostbyname($host))[0];

                        $local_ref = "$ENV{USER}\@$full_local_host_name:$local_dir/$filename";
                        print "rget($local_ref, $filename, \%opts)\n" if $verbose;

                        $rget_rc = rget($local_ref, $filename, \%opts);
                        print "\$rget_rc = $rget_rc\n" if $verbose;
                    }

                } elsif ($localflg) {
                    wrapup(1, $delete,"Running $script failed missing $filename.");
                }
            }

            # if file still not found, then add it to list to pull from archive
            #------------------------------------------------------------------
            unless (-f $filename and -s $filename) {
                if ($archive) {
                    print "Adding $remote_dir/$filename to list to be retrieved.\n" if $verbose;
                    push @remote_file_list, "$remote_dir/$filename";
                }
            }
        }
    }

    # get files from archive
    #-----------------------
    if (@remote_file_list) {

        # dmget files
        #------------
        %opts = ();
        $opts{"dmget_bin"}  = $dmget_bin if $dmget_bin;
        $opts{"run_config"} = $run_config;
        $opts{"verbose"}    = $verbose if $verbose;

        print "rdmget($remote_user, $remote_machine, \@remote_file_list, \%opts)\n" if $verbose;
        $rdmget_rc = rdmget($remote_user, $remote_machine, \@remote_file_list, \%opts);
        print "ERROR in DMGET on $remote_machine\n" unless $rdmget_rc;

        # retrieve files from archive
        #----------------------------
        unless ($silo_work) {
            %opts = ();
            $opts{"debug"} = $verbose if $verbose;
            $opts{"run_config"} = $run_config;
            $opts{"preserve"} = 1;

            foreach $remote_file (@remote_file_list) {
                $filename = basename $remote_file;
                $remote_ref = "$remote_user\@$remote_machine:$remote_file";

                foreach $try (1..2) {
                    $rget_rc = rget($remote_ref, $filename, \%opts);
                    last if $rget_rc and -f $filename and -s $filename;

                    if ($ignore) {
                        warn "WARNING: Cannot acquire $remote_file\n";
                        $missing_inputs++;
                        last;
                    }

                    if ($try < 2) {
                        sleep 10;
                        print "RETRYING $remote_file\n";
                        next;
                    }
                    wrapup(1, $delete, "FATAL ERROR cannot acquire $remote_file");
                }
            }
        }
    }

    # final check for files
    #----------------------
    foreach $currday ($startday..$endday) {
        foreach $time (@times) {
            $filename = token_resolve($fileToken, $currday, $time);
            if (-f $filename and -s $filename) {
                push @mean_files, $filename;
            }
            elsif ($ignore) {
                warn "WARNING: $filename cannot be found\n";
                $missing_inputs++;
            }
            else {
                wrapup(1, $delete, "FATAL ERROR: $filename cannot be found");
            }
        }
    }

    # after prefetch, store info in workdir files for tar job
    #--------------------------------------------------------
    if ($prefetch) {

        # write info for output tarfile
        #------------------------------
        if ($do_tar) {
            open OFO, "> $OUT_TAR_INFO" or die "Error opening $OUT_TAR_INFO: $!";
            print OFO "$local_dir\n";
            print OFO "$remote_dir\n";
        }
        if ($missing_inputs) {
            if (@mean_files) {
                print OFO "partial\n" if $do_tar;
                print "\n$missing_inputs $ftype $yyyymm inputs are missing.\n";
                print "Will create partial tarfile for $ftype inputs.\n" if $do_tar;
            } else {
                print OFO "empty\n" if $do_tar;
                print "\nNo $ftype $yyyymm inputs were found.\n";
                print "No tarfile will be created for $ftype inputs.\n" if $do_tar;
                wrapup(1, $delete, "FATAL ERROR: no $ftype inputs were found");
            }
        }
        else {
            print OFO "full\n" if $do_tar;
            print "\nAll $ftype $yyyymm inputs were found.\n";
            print "Will create full tarfile for $ftype inputs.\n" if $do_tar;
        }
        close OFO if $do_tar;
    }
}

#=======================================================================
# name - calculate_means
# purpose - calculate monthly means
#=======================================================================
sub calculate_means {
    use File::Copy ("copy");
    use lib @SEARCH_PATH;
    use Manipulate_time qw(token_resolve);
    use Remote_utils qw(rput splitrfile);

    my ($mtag, $dtag, $atag, $mfile, $dfile, $afile);
    my ($local_afile, $local_mfile, $dflag, $strict);
    my ($mpirun_mm, $cmd, $rc, @outfiles, $outfile);
    my (%opts, $remote_ref, $local_ref);
    my ($monitor_dir, $monitor_machine, $monitor_path);
    my ($monitor_ref, $monitor_user);
    my $notfound = 0;

    # set output filename tokens
    #---------------------------
    $mtag  = "$expid.$ftype.monthly";
    $dtag  = "$expid.$ftype.diurnal";
    $atag  = "$expid.$ftype.ave";

    $mfile = "$mtag.$yyyymm.nc4";
    $dfile = "$dtag.$yyyymm.nc4";
    $afile = "$atag.$yyyymm.nc4";

    if ($nodiurnal) { @outfiles = ($mfile)         }
    else            { @outfiles = ($mfile, $dfile) }

    foreach $outfile (@outfiles) {
        if (-e $outfile) { unlink($outfile) or warn "unable to delete $outfile\n" }
    }

    # get mpirun command from environment
    #------------------------------------
    $mpirun_mm = $ENV{"MPIRUN_MM"};

    delete $ENV{"SLURM_MEM_PER_GPU"} if $ENV{"SLURM_MEM_PER_GPU"};
    delete $ENV{"SLURM_MEM_PER_NODE"} if $ENV{"SLURM_MEM_PER_NODE"};

    if ($ignore) { $strict = "-strict .false." }
    else         { $strict = ""                }

    if ($nodiurnal) { $dflag = ""         }
    else            { $dflag = "-d $dtag" }

    #===============
    # RUN EXECUTABLE
    #===============
    $cmd = "$mpirun_mm $monthly_means_x -rc $rcfile $strict -ops -tag $mtag $dflag -hdf";
    foreach (@mean_files) { $cmd .= " $_" }
    $rc = system_($cmd);
    #===============

    # push files to local directory
    #------------------------------
    foreach $outfile (@outfiles) { $notfound = 1 unless -e $outfile }
    if ($notfound or $rc) {

        # report error and wrap up
        #-------------------------
        print "rc = $rc\n";
        foreach $outfile (@outfiles) {
            unless (-e $outfile) {
                print "$outfile not found\n";
                wrapup(1, $delete, "Running $monthly_means_x failed for $outfile.");
            }
        }

    } else {

        %opts = ();
        $opts{"debug"}      = $verbose if $verbose;
        $opts{"direct"}     = 1;
        $opts{"run_config"} = $run_config;

        foreach $outfile ( @outfiles ) {

            # copy to local directory
            #------------------------
            unless ($silo_work) {
                $local_ref = "$local_dir/$outfile";
                copy $outfile, $local_ref;
            }

            # rput to remote location
            #------------------------
            unless ($localflg) {
                $remote_ref = "$remote_user\@$remote_machine:$remote_dir/$outfile";
                rput($outfile, $remote_ref, \%opts)
            }

            # rput to monitor location
            #-------------------------
            if ($monitorloc) {
                ($monitor_user, $monitor_machine, $monitor_path) = splitrfile($monitorloc);
                $monitor_dir = token_resolve($monitor_path, $startday);
                $monitor_ref = "$monitor_user\@$monitor_machine:$monitor_dir/$outfile";
                rput($outfile, $monitor_ref, \%opts);
            }

            # add to list of files to be deleted on wrapup
            #----------------------------------------------
            push @remove, $outfile if $delete;
        }

        # link monthly file to ave to maintain compatibility with plotting 
        #-----------------------------------------------------------------
        $local_afile = "$local_dir/$afile";
        $local_mfile = "$local_dir/$mfile";
        symlink $local_mfile, $local_afile unless -e $local_afile;
    }
}

#=======================================================================
# name - wrapup
# purpose - Wrap up by doing dmput on archive; delete files, if requested
#=======================================================================
sub wrapup{
    use Remote_utils qw(rdmget);
    my ($die_away, $delete, $message);
    my (%opts, $rc, $rdmget_rc);

    ($die_away, $delete, $message) = @_;

    if ($delete and @remove) {
        $rc = unlink (@remove) or warn "unable to delete working files\n" ;
        print "$rc files removed \n" if $verbose;
        rmdir $workdir or warn "unable to rmdir $workdir\n";
    }
    die "FATAL ERROR: $message \n" if $die_away;

    if ($do_dmput and @remote_file_list) {
        %opts = ();
        $opts{"dmget_bin"}  = $dmget_bin if $dmget_bin;
        $opts{"do_dmput"}   = 1,
        $opts{"run_config"} = $run_config;
        $opts{"verbose"}    = $verbose if $verbose;

        print "rdmget($remote_user, $remote_machine, \@remote_file_list, \%opts)\n" if $verbose;
        $rdmget_rc = rdmget($remote_user, $remote_machine, \@remote_file_list, \%opts);
        print "ERROR in DMPUT on $remote_machine\n" unless $rdmget_rc;
    }
    print "$message\n";
    exit(0);
}

#=======================================================================
# name - extract_value
# purpose - extract value for variable from a file where the record
#           has format, $variable: $value
#
# input parameters
# => $fname: name of file from which to extract value
# => $var: name of variable whose value to extract
#=======================================================================
sub extract_value {
    my ($fname, $var, $val);

    $fname = shift;
    $var = shift;
    $val = "";

    open(FNAME, "< $fname") or die "Error opening $fname: $!";
    while (<FNAME>) {
        $val = $1 if /\s*$var\s*:\s*(\S+)\s*/;
        last if $val;
    }
    return $val;
}    

#=======================================================================
# name - system_
# purpose - print $cmd and then execute it
#=======================================================================
sub system_ {
    my ($cmd, $rc);
    $cmd = shift;
    print "$cmd\n";
    $rc = system($cmd);
    return $rc;
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {

   print <<"EOF";

USAGE

     $script -D YYYYMMDD -f fvdas_file [ -C -H FVHOME -i -l -m mon_loc -R FVROOT -r Run_Config -S SILO_DIR -v -w workdir ]

DESCRIPTION

     $script - calculate monthly mean and quadratics for a GEOS-5 DAS output file

OPTIONS
 -C            clean flag - Remove local working copy of file after means calculation
 -c            configuration file for time_ave.x Default is FVROOT/etc/time_ave.rc
 -D date       Date of data to convert, in YYYYMMDD format.
 -f filestring filestring entry from monthly.rc
 -H FVHOME     The home directory for this fvDAS run.  monthly_means will
               place  FVHOME/run in the executable search path. If not specified
               script will attempt to get value from FVHOME environment variable.
 -h            Prints this usage notice
 -i            (non-strict mode) ignore missing files
 -L            Flag to toggle local processing without pulling from or pushing to archive
 -l            Flag to use links to local data files rather than copying them
 -M time_ave.x Optional binary to compute monthly means. Default is FVROOT/bin/time_ave.x
 -m mon_loc    Optional remote location to distribute means files for operational
 -nd           Do not produce diurnal means.
 -prefetch     Fetch files but do not calculate monthly means
 -R FVROOT     The installation directory of the fvDAS software. If not specified,
               then script will attempt to get the value from FVROOT environment variable.
 -r run_config Run_Config file for file transfer options if not DEFAULT
 -S SILO_DIR   If silo is supplied with -S all work is done on files in situ
 -v            Flag to enable verbose output to STDOUT
 -w workdir    User defined work space for monthly processing.

REQUIRED FILES
  Manipulate_time.pm
  Remote_utils.pm
  time_ave.x
EOF

exit(1)

}
