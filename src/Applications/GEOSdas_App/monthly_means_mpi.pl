#!/usr/bin/env perl
#=======================================================================
# name - monthly_means_mpi.pl
# purpose - script for calculating monthly means of fvDAS products
#
# !REVISION HISTORY:
#
# 20020501   Owens      Initial code (adapted from fcst_convert)
# 20021108   Owens      Modified to run from FVROOT/bin and read files
#                       from locally mounted /silo directories
# 20030511   Owens      Fixed bug in fetch from archive when only
#                       one file needed
# 20030811   Owens      Added strict mode
# 20030916   Todling    Added DASPERL (had been added before and got lost)
# 20061220   Owens      Modified to use Larry's monthly_hdf.x binary and renamed with .pl
# 20070726   Owens      Modified to use Larry's time_ave.x binary with quadratic support
#                       Added type option to support ( inst1 inst3 inst6 tavg3 )
# 20071010   Owens      Modified to support local processing w/o /archive access
# 20071225   Owens      Modified to support daily files 
# 20120905   Stassi     Cleaned format; added option to do prefetch of files
#=======================================================================
use strict;
use warnings;
my (@SEARCH_PATH, $fvroot);

# pre-process to define @SEARCH_PATH
#-----------------------------------
BEGIN {
    use FindBin;
    use Getopt::Long (":config", "pass_through");
    my $fvhome;

    GetOptions( "H=s" => \$fvhome,
                "R=s" => \$fvroot );

    $fvhome = $ENV{"FVHOME"} unless $fvhome;
    $fvroot = $ENV{"FVROOT"} unless $fvroot;

    die "ERROR: No FVHOME is set. Use -H;" unless $fvhome;
    die "ERROR: No FVROOT is set. Use -R;" unless $fvroot;

    @SEARCH_PATH = ("$fvhome/run", $FindBin::Bin, "$fvroot/bin");
}

# global variables
#-----------------
my ($archive, $date, $delete, $dmget_bin, $do_dmput, $expid, $fileToken);
my ($hType, $ignore, $links, $localflg, $local_dir, $mdate);
my ($monitor_dir, $monitor_machine, $monitor_user, $monitorloc);
my ($monthly_means_x, $nodiurnal, $prefetch, $rcfile, $remote_dir);
my ($remote_machine, $remote_user, $run_config, $silo_dir, $silo_work);
my ($script, $strict, $type, $verbose, $work_dir);
my (@mean_files, @remote_file_list, @remove);

# main program
#-------------
{
    print "\nDATE at start\n";
    system ("date"); print "\n";

    init();
    fetch_files();
    calculate_means() unless $prefetch;
}

#=======================================================================
# name - init
# purpose - get runtime parameters
#=======================================================================
sub init {
    use File::Basename ("basename");
    use Getopt::Long (":config", "no_ignore_case");
    my ($help, $null, $SILO_DIR);

    $script = basename($0);

    # NOTE: -H and -R flags are retrieved in BEGIN block above
    #---------------------------------------------------------
    GetOptions( "a=s"      => \$archive,
                "C"        => \$delete,
                "c=s"      => \$rcfile,
                "D=i"      => \$date,
                "d=s"      => \$dmget_bin,
                "E=s"      => \$expid,
                "F=s"      => \$null,
                "f=s"      => \$fileToken,
                "h"        => \$help,
                "i"        => \$ignore,
                "L"        => \$localflg,
                "l"        => \$links,
                "M=s"      => \$monthly_means_x,
                "m=s"      => \$monitorloc,
                "nd"       => \$nodiurnal,
                "prefetch" => \$prefetch,
                "r=s"      => \$run_config,
                "S=s"      => \$SILO_DIR,
                "s=s"      => \$silo_dir,
                "T=s"      => \$hType,
                "t=s"      => \$type,
                "v"        => \$verbose,
                "w=s"      => \$work_dir );
    

    usage() if $help;

    # defaults
    #---------
    if ($prefetch) { $ignore = 0; $localflg = 0 }
    $delete = 0 unless $delete;
    $dmget_bin = "" unless $dmget_bin;
    $expid = ENV{"EXPID"} unless $expid;
    $rcfile = "$fvroot/etc/time_ave.rc" unless $rcfile;
    $run_config = "DEFAULT" unless $run_config;

    if ($ignore) { $strict = "-strict .false." }
    else         { $strict = ""                }

    if ($SILO_DIR) {
        $delete = 0;
        $links = 0;
        $silo_work = 1;
        $silo_dir = $SILO_DIR;
        $work_dir = $SILO_DIR;
    }

    # check for required inputs
    #--------------------------
    die "Error: Date (yyyymm) is required; Use -D;"        unless $date;
    die "Error: Tokenized file is required; Use -f;"       unless $fileToken;
    die "Error: Time increment type is required; Use -T;"  unless $hType;
    die "Error: Silo directory is required. Use -s or -S;" unless $silo_dir;
    print "\$fvroot = $fvroot\n";

    unless ($prefetch) {
        die "Error: Type is required. Use -t;"             unless $type;
        die "Error: time_ave.rc file is required. Use -c;" unless $rcfile;
        die "Error: time_ave.rc file not found;"           unless -e $rcfile;
        die "Error: expid is required. Use -E;"            unless $expid;
        #--die "Error: Monitor Location is required. Use -m;" unless $monitorloc;

        # check monthly means program
        #----------------------------
        $monthly_means_x = "$fvroot/bin/time_ave.x"        unless $monthly_means_x;
        die "Error: monthly_means_x is required. Use -M;"  unless -e $monthly_means_x;
        die "Error: $monthly_means_x not executable;"      unless -x $monthly_means_x;
    }

    # define working directory, if not already defined
    #-------------------------------------------------
    unless ($work_dir) {
        if ($ENV{"SCRATCH"}) { $work_dir = "$ENV{SCRATCH}/monthly/$$" }
        else                 { $work_dir = "$ENV{TMPDIR}/monthly/$$"  }
    }

    # environment variables
    #----------------------
    $ENV{"PATH"} = join ":", @SEARCH_PATH, $ENV{"PATH"};
    $ENV{"PATH"} = join ":", $ENV{"PATH"}, $ENV{"PBS_O_PATH"} if $ENV{"PBS_O_PATH"};
    $ENV{"SHELL"} = "/bin/csh";
}

#=======================================================================
# name - fetch_files
# purpose - make input files local prior to calculating monthly means
#=======================================================================
sub fetch_files {
    use File::Basename ("basename");
    use File::Path ("mkpath");
    use lib @SEARCH_PATH;
    use Manipulate_time qw(get_hours num_days_in_month token_resolve);
    use Remote_utils qw(rdmget rget splitrfile);
    use Sys::Hostname ("hostname");

    my ($currday, $current_dir, $days, $do_dmget, $endday, $filename);
    my ($full_local_host_name, $host, $local_file, $local_ref, $monitor_path);
    my ($month, $rc, $rdmget_rc, $remote_file, $remote_file_path, $remote_ref);
    my ($rget_rc, $startday, $time, $try, $year);
    my (%options, @remote_info, @times);

    # determine dates and times
    #--------------------------
    $mdate = substr($date,0,6);
    $year  = substr($mdate,0,4);
    $month = substr($mdate,4,2);

    $days = num_days_in_month($year, $month);
    $startday = $mdate."01";
    $endday   = $mdate.$days;

    @times = ();
    @times = get_hours($hType, "0", "all", "m");
    die "Error. Unrecognized file type $hType;" unless @times;

    # parse location data
    #--------------------
    $do_dmput = 0;
    $do_dmget = 0;
    if ($archive) {
        ($remote_user, $remote_machine, $remote_file_path) = splitrfile($archive);
        $remote_dir = token_resolve($remote_file_path, $startday);
        print "\$remote_dir = $remote_dir\n" if $verbose;
        $do_dmget = 1;
    }

    if ($monitorloc) {
        ($monitor_user, $monitor_machine, $monitor_path) = splitrfile($monitorloc);
        $monitor_dir = token_resolve($monitor_path, $startday);
    }

    # create local directory, if necessary
    #-------------------------------------
    $local_dir = token_resolve($silo_dir, $startday);
    die "Error. Unable to define local_dir variable;" unless $local_dir;
    print "\$local_dir = $local_dir\n" if $verbose;

    unless (-d $local_dir ) {
        print "local_dir, $local_dir, does not exist - creating...\n" if $verbose;
        mkpath($local_dir, 1, 0755) or die "Error. Could not create $local_dir;";
    }

    # create work directory, if necessary
    #------------------------------------
    if ($silo_work) {
        $work_dir = token_resolve($work_dir, $startday);
        if ((! $archive) and ($dmget_bin =~ /:/)) {
            @remote_info = splitrfile($dmget_bin);
            ($remote_user, $remote_machine, $remote_file_path) = @remote_info;
            $remote_dir = $work_dir;
            $do_dmget = 1;
        }
    }

    unless (-d $work_dir) {
        print "$work_dir does not exist - creating...\n" if $verbose;
        mkpath($work_dir, 1, 0755) or  die "Error. Could not create $work_dir;";
    }

    # change directory to work directory
    #-----------------------------------
    chdir $work_dir or die "Error. Cannot change directory to $work_dir;\n";
    print "Current work directory is $work_dir\n" if $verbose;
    print "Processing Monthly Means for $days days of $year$month\n";

    # get hostname info
    #------------------
    $host = hostname();
    $full_local_host_name = (gethostbyname($host))[0];

    # initialize loop variables
    #--------------------------
    foreach $currday ($startday..$endday) {
        foreach $time (@times) {

            $filename = token_resolve($fileToken, $currday, $time);
            print "\$filename = $filename\n" if $verbose;
            push @remove, $filename if $delete;

            # look for file in work directory
            #--------------------------------
            unless (-f $filename and -s $filename) {
                print "$filename not found in work directory\n" if $verbose;
                cleanup(1, $delete, "Running $script failed missing $filename.") if $silo_work;

                # look for file in stage directory
                #---------------------------------
                print "Checking for $local_dir/$filename\n" if $verbose;
                if (-f "$local_dir/$filename") {

                    # create symlink
                    #---------------
                    if ($links) {
                        print "linking $local_dir/$filename to $filename\n" if $verbose;
                        $rc = symlink "$local_dir/$filename", $filename;
                        cleanup(1, $delete, "Cannot create symbolic link to $filename") unless $rc;
                    }

                    # or rget to work dir
                    #--------------------
                    else {
                        %options = ();
                        $options{"debug"} = $verbose if $verbose;
                        $options{"run_config"} = $run_config;

                        $local_ref = "$ENV{USER}\@$full_local_host_name:$local_dir/$filename";
                        print "rget($local_ref, $filename, \%options)\n" if $verbose;

                        $rget_rc = rget($local_ref, $filename, \%options);
                        print "\$rget_rc = $rget_rc\n" if $verbose;
                    }

                } elsif ($localflg) {
                    cleanup(1, $delete,"Running $script failed missing $filename.");
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

        # do dmget
        #---------
        if ($do_dmget) {
            $do_dmput = 1;

            # dmget files
            #------------
            %options = ();
            $options{"dmget_bin"}  = $dmget_bin if $dmget_bin;
            $options{"run_config"} = $run_config;
            $options{"verbose"}    = $verbose if $verbose;

            print "rdmget($remote_user, $remote_machine, \@remote_file_list, \%options)\n" if $verbose;
            $rdmget_rc = rdmget($remote_user, $remote_machine, \@remote_file_list, \%options);
            print "ERROR in DMGET on $remote_machine\n" unless $rdmget_rc;
        }

        # retrieve files from archive
        #----------------------------
        unless ($silo_work) {
            %options = ();
            $options{"debug"}      = $verbose if $verbose;
            $options{"run_config"} = $run_config;

            foreach $remote_file (@remote_file_list) {
                $local_file = basename $remote_file;
                $remote_ref = "$remote_user\@$remote_machine:$remote_file";

                foreach $try (1..2) {
                    $rget_rc = rget($remote_ref, $local_file, \%options);
                    last if $rget_rc and -f $local_file and -s $local_file;
                    if ($try < 2) {
                        sleep 10;
                        print "RETRYING $remote_file\n";
                        next;
                    }
                    cleanup(1, $delete, "FATAL ERROR cannot acquire $remote_file");
                }
            }
        }
    }

    # final check for files
    #----------------------
    foreach $currday ($startday..$endday) {
        foreach $time (@times) {
            $filename = token_resolve($fileToken, $currday, $time);
            unless (-f $filename and -s $filename) {
                sleep 5;
                unless (-f $filename and -s $filename) {
                    cleanup(1, $delete, "FATAL ERROR: $filename cannot be found") unless $ignore;
                    warn "WARNING : $filename cannot be found";
                }
            }
            push @mean_files, $filename;
        }
    }
}

#=======================================================================
# name - calculate_means
# purpose - calculate monthly means
#=======================================================================
sub calculate_means {
    use File::Copy ("copy");
    use lib @SEARCH_PATH;
    use Remote_utils ("rput");

    my ($mtag, $dtag, $atag, $mfile, $dfile, $afile);
    my ($local_afile, $local_mfile, $dflag);
    my ($mpirun_mm, $cmd, $rc, @outfiles, $outfile);
    my (%options, $remote_ref, $local_ref, $monitor_ref);
    my $notfound = 0;

    # set output filename tokens
    #---------------------------
    $mtag  = "$expid.$type.monthly";
    $dtag  = "$expid.$type.diurnal";
    $atag  = "$expid.$type.ave";

    $mfile = "$mtag.$mdate.nc4";
    $dfile = "$dtag.$mdate.nc4";
    $afile = "$atag.$mdate.nc4";

    if ($nodiurnal) { @outfiles = ($mfile)         }
    else            { @outfiles = ($mfile, $dfile) }

    foreach $outfile (@outfiles) {
        if (-e $outfile) { unlink($outfile) or warn "unable to delete $outfile\n" }
    }

    # get mpirun command from environment
    #------------------------------------
    $mpirun_mm = $ENV{"MPIRUN_MM"};

    # run the executable
    #-------------------
    if ($nodiurnal) { $dflag = ""         }
    else            { $dflag = "-d $dtag" }
    $cmd = "$mpirun_mm $monthly_means_x -rc $rcfile $strict -ops -tag $mtag $dflag -hdf";
    foreach (@mean_files) { $cmd .= " $_" }

    print "Running: $cmd\n";
    $rc = system($cmd);

    # push files to local directory
    #------------------------------
    foreach $outfile (@outfiles) { $notfound = 1 unless -e $outfile }
    if ($notfound or $rc) {

        # report error and clean up
        #--------------------------
        print "rc = $rc\n";
        foreach $outfile (@outfiles) {
            unless (-e $outfile) {
                print "$outfile not found\n";
                cleanup(1, $delete, "Running $monthly_means_x failed for $outfile.");
            }
        }

    } else {

        %options = ();
        $options{"debug"}      = $verbose if $verbose;
        $options{"direct"}     = 1;
        $options{"run_config"} = $run_config;

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
                rput($outfile, $remote_ref, \%options)
            }

            # rput to monitor location
            #-------------------------
            if ($monitorloc) {
                $monitor_ref = "$monitor_user\@$monitor_machine:$monitor_dir/$outfile";
                rput($outfile, $monitor_ref, \%options);
            }

            # add to list of files to be deleted on cleanup
            #----------------------------------------------
            push @remove, $outfile;
        }

        # link monthly file to ave to maintain compatibility with plotting 
        #-----------------------------------------------------------------
        $local_afile = "$local_dir/$afile";
        $local_mfile = "$local_dir/$mfile";
        symlink $local_mfile, $local_afile unless -e $local_afile;

        # report success and clean up
        #----------------------------
        cleanup(0, $delete, "Monthly Means successfully processed for $type");
    }
}

#=======================================================================
# name - cleanup
# purpose - Clean up by deleting files and doing dmput on archive
#=======================================================================
sub cleanup{
    use Remote_utils ("rdmget");
    my ($die_away, $delete, $message);
    my (%options, $rc, $rdmget_rc);

    ($die_away, $delete, $message) = @_;
    print "$message\n";

    if ($delete and @remove) {
        $rc = unlink (@remove) or warn "unable to delete working files\n" ;
        print "$rc files removed \n" if $verbose;
        rmdir $work_dir or warn "unable to rmdir $work_dir\n";
    }
    die "FATAL ERROR: $message \n" if $die_away;

    if ($do_dmput) {
        %options = ();
        $options{"dmget_bin"}  = $dmget_bin if $dmget_bin;
        $options{"do_dmput"}   = 1,
        $options{"run_config"} = $run_config;
        $options{"verbose"}    = $verbose if $verbose;

        print "rdmget($remote_user, $remote_machine, \@remote_file_list, \%options)\n" if $verbose;
        $rdmget_rc = rdmget($remote_user, $remote_machine, \@remote_file_list, \%options);
        print "ERROR in DMPUT on $remote_machine\n" unless $rdmget_rc;
    }
    print "$message\n";
    exit(0);
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {

   print <<"EOF";

USAGE

     monthly_means_mpi.pl -D YYYYMMDD -f fvdas_file -t type [ -C -d dmget_bin  -E EXPID -g -H FVHOME -i 
                      -l -m mon_loc -R FVROOT -r Run_Config -s silo_dir -S silo_work -v  -w workdir]

DESCRIPTION

     monthly_means_mpi.pl - calculate monthly mean and quadratics for a GEOS-5 DAS output file

OPTIONS
 -a archive    Mass Storage location for input data files and output means files
 -c            configuration file for time_ave.x Default is FVROOT/etc/time_ave.rc
 -C            clean flag - Remove local working copy of file after means calculation
 -d dmget_bin  Location and name of dmget binary - can include user\@machine:
 -D date       Date of data to convert, in YYYYMMDD format.
 -E EXPID      Exeriment ID of the data files.  If -E is not specified,
               monthly_means probes the EXPID environment variable.
 -f file       Tokenized name of file to be processed
 -h            Prints this usage notice
 -H FVHOME     The home directory for this fvDAS run.  monthly_means will
               place  FVHOME/run in the executable search path. If -H is not
               specified, monthly_means probes the FVHOME environment variable.
 -i            ignore missing files
 -L            Flag to toggle local processing without pulling from or pushing to archive
 -l            Flag to use links to local data files rather than copying them
 -m mon_loc    Optional remote location to distribute means files for operational
 -M time_ave.x  Optional binary to compute monthly means. Default is FVROOT/bin/time_ave.x
 -nd           Do not produce diurnal means.
 -R FVROOT     The installation directory of the fvDAS software.  If
               -R is not specified, monthly_means probes the FVROOT environment
               variable.
 -r Run_Config Run_Config file for file transfer options if not DEFAULT
 -s silo_dir   Local directory to check first before polling archive usually FVSILO
 -S silo_work  If silo is supplied with -S all work is done on files in situ
 -v            Flag to enable verbose output to STDOUT
 -w workdir    User defined work space for means calculations. Default is \$SCRATCH

REQUIRED FILES
  Manipulate_time.pm
  Remote_utils.pm
  time_ave.x
EOF

exit(1)

}
