#!/usr/bin/env perl
# 
# jobgen
#
#  04Nov2011 Todling  Initial code
#  11Feb2013 Todling  Add machfile opt
#  04Dec2014 Todling  Adapt to incorporate basic (needed) slurm directivies
#  08Jun2016 Thompson Adaptations for MPT to allow Intel MPI to still work. The
#                     substr call below on mpirun *always* does something so if using
#                     MPT we must prevent this.
#  30Mar2017 Todling  Hack to fix env that looks for missing lib under messed up NCCS batch system
#  21Feb2020 Todling  Allow for high freq bkg (up to 1mn)
#-----------------------------------------------------------------------------------------------------

use Env;                 # make env vars readily available
use File::Basename;      # for basename(), dirname()
use File::Copy "cp";     # for cp()
use Getopt::Long;        # load module with GetOptions function
use Time::Local;         # time functions
use FindBin;             # so we can find where this script resides

# look for perl packages in the following locations
#--------------------------------------------------
use lib ( "$FindBin::Bin", "$FVROOT/bin", "$ESMADIR/$ARCH/bin" );


my $scriptname = basename($0);

# Command line options

  GetOptions ( "egress=s",
               "expid=s",
               "q=s",
               "proc=s",
               "mpiprocs=s",
               "machfile=s",
               "xc=s",
               "h" );

  usage() if $opt_h;

# Parse command line, etc

  init();

# Generate pbs job script

  gen();

# All done

# print "jobgen: resulting files \n";
# $rc_ignore = system('ls -lrt');
  if ($rc==0) {
     print "$0: sucessfully completed.\n\n";
     exit(0);
  } else {
     print "$0: failed to generate PBS job script\n\n";
     exit(1);
  }


#......................................................................

sub init {

   if ( $#ARGV  <  7 ) {
     print STDERR " Missing arguments; see usage:\n";
     usage();
   } else {              # required command line args
     $jobname     = $ARGV[0];
     $gid         = $ARGV[1];
     $pbs_wallclk = $ARGV[2];
     $command     = $ARGV[3];
     $gotodir     = $ARGV[4];
     $whocalled   = $ARGV[5];
     $file2touch  = $ARGV[6];
     $failedmsg   = $ARGV[7];
   }

# process options

   $rc    = 0;

# allow for extra command line
   if ( $opt_xc ) {
        $xcommand = $opt_xc;
   } else {
        $xcommand = "";
   }
   if ( $opt_proc ) {
      $proc = ":proc=$opt_proc";
   } else {
      $proc = "";
   }


# FVROOT is where the binaries have been installed
# ------------------------------------------------
   $fvhome  = $ENV{FVHOME};
   $fvroot  = $ENV{FVROOT};
   $fvwork  = $ENV{FVWORK}; if ( "$fvwork" eq "" ) { $fvwork = "./" };
   
   if ( $opt_q eq "datamove" ) {
        $ncpus          = 1;
        $ncpus_per_node = 1;
   } else {
        $ncpus          = $ENV{JOBGEN_NCPUS};
        $ncpus_per_node = $ENV{JOBGEN_NCPUS_PER_NODE};
   }
   if ( ! $ncpus_per_node ) {
      print "$0: failed due undefined or zero ncpus_per_node \n\n";
      exit(1);
   }
   $nodes = $ncpus / $ncpus_per_node;

   if ( $opt_mpiprocs ) {
      $mpiprocs = $opt_mpiprocs;
      if ( $mpiprocs > $ncpus_per_node ) {
          print "$0: failed due to invalid mpiprocs \n\n";
          exit(1);
      }
   } else {
      $mpiprocs = $ncpus_per_node;
   }

# allow overwrite of job name
  $newjobname = $jobname;
  if ( $ENV{JOBGEN_PFXNAME} ) {
     $newjobname = $ENV{JOBGEN_PFXNAME} . "_" . $newjobname;
  }
  if ( $ENV{JOBGEN_SFXNAME} ) {
     $newjobname = $newjobname . "_" . $ENV{JOBGEN_SFXNAME};
  }

#  The following is a tricky one: replace mpirun w/ mpiexec
   if ( $opt_machfile ) {

      # This is for Intel MPI
      # ---------------------

      #  The following is a tricky one: replace mpirun w/ mpiexec
      if ($command =~ /mpirun/ ) {
         my $fragment =  substr $command, index($command, 'mpirun'), 6, "mpiexec -machinefile $opt_machfile";
      }
      
      # Note: Due to perl weirdness. The above command will *always* do something. If it can't find an
      #       mpirun to replace, it replaces the last character of the string! Thus we must protect.

   }

}

#......................................................................

sub gen {

 open(SCRIPT,">${jobname}.j");

 print  SCRIPT <<"EOF";
#\!/bin/csh -xvf
#SBATCH --job-name=$jobname
#SBATCH --output=batch_${jobname}.log
#SBATCH --time=$pbs_wallclk
#PBS -N $jobname
#PBS -o $jobname.log
#PBS -l walltime=$pbs_wallclk
#PBS -S /bin/csh
#PBS -V
#PBS -j eo
EOF

 print  SCRIPT <<"EOF";
#SBATCH -A $gid
EOF

 if ( $ENV{JOBGEN_QOS} ) {
   if ( $opt_q ne "datamove" ) {
 print  SCRIPT <<"EOF";
#SBATCH --qos=$ENV{JOBGEN_QOS}
EOF
   }
 }
 if ( $ENV{JOBGEN_PARTITION} ) {
   if ( $opt_q ne "datamove" ) {
 print  SCRIPT <<"EOF";
#SBATCH --partition=$ENV{JOBGEN_PARTITION}
EOF
   }
 }
 if ( $ENV{JOBGEN_RESERVATION} ) {
   if ( $opt_q ne "datamove" ) {
 print  SCRIPT <<"EOF";
#SBATCH --reservation=$ENV{JOBGEN_RESERVATION}
EOF
   }
 }
 if ( $opt_q ne "datamove" ) {
   if ( $ENV{JOBGEN_CONSTRAINT} ) {
 print  SCRIPT <<"EOF";
#SBATCH --constraint=$ENV{JOBGEN_CONSTRAINT}
EOF
   }
   print  SCRIPT <<"EOF";
#SBATCH --ntasks=${ncpus}
#_SBATCH --ntasks-per-node=${ncpus_per_node}
EOF
 }

 if ( $opt_q ne "datamove" ) {
   if ( $ENV{JOBGEN_STREAM} ) {
 print  SCRIPT <<"EOF";
#SBATCH --constraint=$ENV{JOBGEN_STREAM}
EOF
   }
 }

 if ( $opt_q ne "NULL" ) {
    if ( $opt_q eq "datamove" ) {
    print  SCRIPT <<"EOF";
#PBS -q $opt_q
#SBATCH --partition=datamove
EOF
      if ( $ENV{JOBGEN_ARCH_CONSTRAINT} ) {
    print  SCRIPT <<"EOF";
#$ENV{JOBGEN_ARCH_CONSTRAINT}
EOF
      }
    }
 }

 if ( "$newjobname" ne "$jobname" ) {
 print  SCRIPT <<"EOF";
 if( \$?PBS_JOBID ) then
   qalter -N $newjobname \$PBS_JOBID
 else
   if( \$?SLURM_JOBID ) then
     qalter -N $newjobname \$SLURM_JOBID
   endif
 endif
EOF
 }

 if ( $fvhome ) {
 print  SCRIPT <<"EOF";
 setenv FVHOME $fvhome
EOF
 }

 if ( $fvroot ) {
 print  SCRIPT <<"EOF";
 setenv FVROOT $fvroot
 source \$FVROOT/bin/g5_modules
 set path = ( . \$FVHOME/run \$FVROOT/bin \$path )
EOF
 }

 if ( $opt_expid ) {
 print  SCRIPT <<"EOF";
 setenv EXPID $opt_expid
EOF
 }

 print  SCRIPT <<"EOF";

# These env vars are here because the batch system is messed up
# and at times the system cannot find proper run-time libraries.
 unsetenv PMI_RANK
 unsetenv PMI_FD
 unsetenv PMI_JOBID
 unsetenv PMI_SIZE

 cd $gotodir
 /bin/rm .SUBMITTED
 touch .RUNNING
EOF
 if( $opt_egress ) {
 print  SCRIPT <<"EOF";
 if ( -e $opt_egress ) then
    /bin/rm $opt_egress 
 endif
EOF
 }
 print  SCRIPT <<"EOF";
 $command
EOF
 if( $opt_egress ) {
 print  SCRIPT <<"EOF";
 if ( ! -e $opt_egress ) then 
      echo " ${whocalled}: ${failedmsg}, Aborting ... "
      exit(1)
 endif
EOF
 } else {
 print  SCRIPT <<"EOF";
 if ( \$status ) then 
      echo " ${whocalled}: ${failedmsg}, Aborting ... "
      exit(1)
 endif
EOF
 }
 print  SCRIPT <<"EOF";
 $xcommand
 /bin/rm .RUNNING
 touch $file2touch
EOF

}
#......................................................................
#
# Tick - advance date/time by nsecs seconds
#
#

sub tick {
    my ( $nymd, $nhms, $nsecs ) = @_;

    if("$nsecs" == "0" ) {
        return ($nymd, $nhms);
    }

    $yyyy1  = substr($nymd,0,4);
    $mm1    = substr($nymd,4,2);
    $dd1    = substr($nymd,6,2);

    $hh1 = 0 unless ( $hh1 = substr($nhms,0,2));
    $mn1 = 0 unless ( $mn1 = substr($nhms,2,2));
    $ss1 = 0 unless ( $ss1 = substr($nhms,4,2));
    $time1 = timegm($ss1,$mn1,$hh1,$dd1,$mm1-1,$yyyy1) + $nsecs;
    ($ss1,$mn1,$hh1,$dd1,$mm1,$yyyy1,undef,undef,undef) = gmtime($time1);

    $nymd = (1900+$yyyy1)*10000 + ($mm1+1)*100 + $dd1;
    $nhms = sprintf("%6.6d",$hh1*10000 + $mn1*100 + $ss1);
    return ($nymd, $nhms);

}

#....................................................................................
sub System {

    my ( $cmd, $logfile, $xname ) = @_;
    my ( @zname );

    open SAVEOUT, ">&STDOUT";  # save stdout
    open SAVEERR, ">&STDERR";  # save stderr

    open STDOUT, ">>$logfile" or die "can't redirect stdout";
    open STDERR, ">>$logfile" or die "can't redirect stderr";

    select STDERR; $| = 1;     # make it unbuffered
    select STDOUT; $| = 1;     # make it unbuffered

    @zname = split(" ", $cmd);
    if ( "$zname[0]" eq "mpirun" || "$zname[0]" eq "prun" ) {
      $rc1 = system( "zeit_ci.x -r $fvwork/.zeit $xname");
    } else {
      $rc1 = system( "zeit_ci.x -r $fvwork/.zeit $zname[0]");
    }

    $rc = system ( $cmd );     # run the shell command

    if ( "$zname[0]" eq "mpirun" || "$zname[0]" eq "prun" ) {
      $rc2 = system( "zeit_co.x -r $fvwork/.zeit $xname");
    } else {
      $rc2 = system( "zeit_co.x -r $fvwork/.zeit $zname[0]");
    }

    # Bitwise shift returns actual UNIX return code
    $exit_code = $rc >> 8;

    close STDOUT;
    close STDERR;

    open STDOUT, ">&SAVEOUT" ;  # restore stdout
    open STDERR, ">&SAVEERR" ;  # restore stdout

    return $exit_code;

  }

#......................................................................

sub Assign {

  my ( $fname, $lu ) = @_;

  $f77name = "fort.$lu";
  unlink($f77name) if ( -e $f77name ) ;
  symlink("$fname","$f77name");

}

sub Assignfn {

# Assignfn - assigns fn to given file name fname.
# fname = old file
# fn = new file (links to old)
  my ( $fname, $fn ) = @_;
  unlink($fn) if ( -e $fn ) ;
  symlink("$fname","$fn");

}

sub fullpath {

    my ( $fname ) = @_;

    $dirn = dirname("$fname");
    chomp($dirn = `pwd`) if ( "$dirn" eq "." ) ;
    $name = basename("$fname");
    $full = "$dirn/$name";
    return ($full);

  }

#......................................................................

sub usage {

   print <<"EOF";

NAME
     jobgen - Generate PBS job script
          
SYNOPSIS

     jobgen [...options...] jobname
                            gid
                            pbs_wallclk
                            command
                            gotodir
                            whocalled
                            file2touch
                            failedmsg
          
DESCRIPTION


     The following parameters are required 

     jobname      name of job script to be created (will be appended with j extension)
     gid          group ID job will run under
     pbs_wallclk  wall clock time for job
     command      e.g., mpirun -np \$ENSGSI_NCPUS GSIsa.x
     gotodir      location to cd to (where all input files reside)
     whocalled   name of calling script (e.g., obsvr_ensemble)
     file2touch  name of file to be touched indicating a successful execution
     failedmsg   message to be issued in case of failed execution, between quote marks

OPTIONS

     -egress       specify file to watch for completion of job (e.g., EGRESS for GCM)
     -expid        experiment name
     -q            specify pbs queue (e.g., datamove when archiving)
     -h            prints this usage notice

NECESSARY ENVIRONMENT

  JOBGEN_CONSTRAINT      use for SLURM constraint option
  JOBGEN_NCPUS           number of CPUS
  JOBGEN_NCPUS_PER_NODE  number of CPUS per node
  JOBGEN_PFXNAME         provide prefix for job name
  JOBGEN_QOS             use for SLURM priority option
  JOBGEN_SFXNAME         provide suffix for job name

OPTIONAL ENVIRONMENT

  ARCH            machine architecture, such as, Linux, AIX, etc
  FVHOME          location of alternative binaries
  FVROOT          location of build's bin
  JOBGEN_QOS      allows user to specify own priori in batch system

AUTHOR

     Ricardo Todling (Ricardo.Todling\@nasa.gov), NASA/GSFC/GMAO
     Last modified: 04Dec2014      by: R. Todling


EOF

  exit(1)

}
