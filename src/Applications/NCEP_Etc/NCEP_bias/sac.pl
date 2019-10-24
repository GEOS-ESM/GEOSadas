#!/usr/bin/perl
#--------------------------------------------------
#
# Purpose: run satellite bias correction after GSI
#
# Usage:
#
#  sac.pl [options] RUNDIR EXPID NYMD NHMS
#
# !REVISION HISTORY:
#
#   13Nov2009 Todling  Initial code
#   20Sep2013 Todling  Change to fit sac.nl.tmpl into correct conventional 
#                      of template file from @VAR to >>>VAR<<<
#
#--------------------------------------------------
use Env;                 # make env vars readily available
use File::Basename;      # for basename(), dirname()
use File::Copy "cp";     # for cp()
use Getopt::Long;        # load module with GetOptions function
use Time::Local;         # time functions
use FindBin;             # so we can find where this script resides

# look for perl packages in the following locations
#--------------------------------------------------
use lib ( "$FindBin::Bin", "$FVROOT/bin", "$ESMADIR/$ARCH/bin" );

  GetOptions ( "skipSATBIAS",
               "ana",
               "observer",
               "log",
               "debug",
               "h" );

# Initialize variables
# --------------------
  init();

# Set namelist
# ------------
  sac_namelist();

# Run program
# -----------
  sac();

  exit(0);

#....................................................................................
sub sac_namelist {                        

 my($ft, $frun);

  $rc_sac = "$rundir/sac.nl";
  $frun   = "$fvwork/sac.nl.tmpl";

  return if ( -e $rc_sac );

  open(LUN,"$frun")     || die "Fail to open sac.nl.tmpl: $!\n";
  open(LUN2,">$rc_sac") || die "Fail to open sac.nl: $!\n";

# Change variables to the correct inputs
#---------------------------------------
  while( defined($rcd = <LUN>) ) {
    chomp($rcd);
    if($rcd =~ />>>YYYY<<</) {$rcd=~ s/>>>YYYY<<</$yyyy/; }
    if($rcd =~ />>>MM<<</)   {$rcd=~ s/>>>MM<<</$mm/;     }
    if($rcd =~ />>>DD<<</)   {$rcd=~ s/>>>DD<<</$dd/;     }
    if($rcd =~ />>>HH<<</)   {$rcd=~ s/>>>HH<<</$hh/;     }
    print(LUN2 "$rcd\n");
  }
  close(LUN);
  close(LUN2);

}
#....................................................................................
sub sac {                        

  return if ( $rc!=0 );
  return 0 unless ( $doSAC );

  print " Running sac.x, please wait...\n";

# Fix date/time in namelist
# -------------------------
  sac_namelist();

# Now run SAC: satellite angular correction
# -----------
  $mpirun_sac = $ENV{MPIRUN_SAC};
  if ( $mpirun_sac =~ "mpirun" || $mpirun_sac =~ "prun" || $mpirun_sac =~ "poe" ) {
       $cmd = "${mpirun_sac} ";
  } else {
       $cmd = "mpirun -np 1 sac.x ";
  }
  print " $cmd\n";
  if ( ! $opt_debug ) {
     $rc = System($cmd, "$log","sac.x");
     die ">>>> ERROR <<< running sac.x" if ( $rc );
     print " $0: sac.x \$rc =  $rc\n";

     cp("satbias_ang.out","$fvwork/$expid.ana.satbang.${nymd}_${hh}z.txt");  # satbang for storage
     cp("satbias_ang.out","$fvwork/satbang");                                # satangbias to recycle
     if ( $doSPINBIAS ) {
          cp("satbias_ang.out","satbias_ang.in");                            # cp sat bias ang. correction for 2nd analysis pass
     }

#    Get rid of diag files links
#    ---------------------------
#    lndiag("unset");

  }
}

#....................................................................................
sub init {

   if ( $#ARGV  <  3 ) {
     print STDERR " Improper input parameters ; see usage:\n";
     usage();
   } else {              # required command line args
     $rundir = $ARGV[0];
     $expid  = $ARGV[1];
     $nymd   = $ARGV[2];
     $nhms   = sprintf("%6.6d",$ARGV[3]);
   }
   $yyyy = substr($nymd,0,4);
   $mm   = substr($nymd,4,2);
   $dd   = substr($nymd,6,2);
   $hh   = substr($nhms,0,2);

   
   $fvwork = $ENV{FVWORK};
   if ( ! $fvwork ) {
       print "env var FVWORK must be defined. Aborting ...\n";
       exit(1) unless ($opt_debug);
   }
   $mpirun_sac = $ENV{MPIRUN_SAC};
   if ( ! $mpirun_sac ) {
       print "env var MPIRUN_SAC must be defined. Aborting ...\n";
       exit(1) unless ($opt_debug);
   }
   $doSAC    = 1  if ( ! $opt_skipSATBIAS );

   $doSPINBIAS = 0;
   if ( $opt_ana || $opt_observer ) {
       if ( -e "$fvwork/satbias" ) {
            Assignfn ( "$fvwork/satbias", "satbias_in"   );
       } else {
            print " Generating satbias_in ... \n";
            $doSPINBIAS = 1 unless ( $opt_skipSATBIAS );
       }
       if ( -e "$fvwork/satbang" ) {
            Assignfn ( "$fvwork/satbang", "satbias_ang.in");  # need for sac.x
            Assignfn ( "$fvwork/satbang", "satbias_angle" );  # need for gsi.x
       } else {
            die ">>>> Unable to find satbang file ... \n <<<";
       }
       if ( -e "$fvwork/pcpbias" ) {
            Assignfn ( "$fvwork/pcpbias", "pcpbias_in");
       }
   }

   $log = "$expid.sac.log.${nymd}_${hh}z.txt";
   if ($opt_log) {
       $log = $opt_log;
   }

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

#....................................................................................
#
# Tick - advance date/time by nsecs seconds
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
sub usage {

   print <<"EOF";

NAME
     sac.pl - runs satellite bias correction (after GSI)

SYNOPSIS

     sac.pl [options] RUNDIR EXPID NYMD NHMS

DESCRIPTION

     This script involves the satellite bias corection program

     rundir  name of directory where run takes place, i.e.,
             location of diag files
     expid   experiment id
     nymd    date, as in YYYYMMDD
     nhms    time, as in HHMMSS

    Optinal Arguents:

    -h      help (echoes this usage)
    -log    name of log files
             (default: expid.sac.log.nymd_hhz.txt)
    -debug  going through it bug do not actually run it

EOF

  exit(1);
}

