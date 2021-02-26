#!/usr/bin/env perl
#
# Wrapper driver script for PREPQC
#
# !REVISION HISTORY:
#
#  20Mar2009  Todling   Created from stripped off version of fvana
#
#------------------------------------------------------------------

use Env;                 # make env vars readily available
use File::Basename;      # for basename(), dirname()
use File::Copy "cp";     # for cp()
use Getopt::Long;        # command line options

# Command line options
# --------------------
  GetOptions( "h", "n", "q", "radcor=i", "pb=s" );
  usage() if $opt_h;

# Figure out expid, directories, etc
# ----------------------------------
  init();

# NCEP pre-processing QC
# ----------------------
  prepqc() if ( $doPREPQC );

# All done
# --------
  exit(0);

#......................................................................

sub init {


  if ( $#ARGV < 2 ) {
       print STDERR "missing nymd, nhms or dyn_f; see usage";
       usage();
     } else {              # required command lile args
       $nymd = $ARGV[0];
       $nhms = sprintf("%6.6d",$ARGV[1]);
       $dynf = $ARGV[2];
       $yyyy = substr($nymd,0,4);
       $mm   = substr($nymd,4,2);
       $dd   = substr($nymd,6,2);
       $hh   = substr($nhms,0,2);

     }
     
    $doPREPQC = 0;
    if ( $hh== 00 || $hh == 06 || $hh== 12 || $hh== 18 ) {
	 $doPREPQC  = 1  if ( $ENV{PREPQC}  );
    }

    $strict    = "-strict" if ( $ENV{STRICT} || $opt_strict );

  if ( ! $opt_q ) {

    print "$0: skipping PREPQC\n"     if ( ! $doPREPQC  );

  }

  $fvhome = dirname($FindBin::Bin) unless ( $fvhome = $FVHOME );
  $expid  = basename($fvhome);

  $radcor = 4;   #set 4 as the default
  if($RADCOR ne "") { $radcor = $RADCOR; }
  if($opt_radcor ne "") { $radcor = $opt_radcor; }
}

#......................................................................
sub prepqc {                        # runs NCEP preprocessing QC subsystem

#   Check for prepqc daemon. If not running, then this is the first time through
#   If running, don't return util we are positive that the daemon completed
#   Timeout after 15 minutes

    if (-e "prepqc.daemon.running") { 
       if (-e "prepqc.$nymd.$nhms.done") {
         return 0;
       }
       else {
         $count = 0;
         while (! -e "prepqc.$nymd.$nhms.done") {
            sleep(1);
            $count++;
            die ">>>> ERROR <<< timeout in prepqc" if ($count > 900 );
         }
         return 0;
       }
    }

    $prepqc_new   = ( $opt_pb   or $prepb  = "$expid.prepbufr.${nymd}.t${hh}z.blk" );

# Only execute if the input PREPBUFR file exists and is non-empty
# ---------------------------------------------------------------
    if ( -e $prepqc_new && -s $prepqc_new ) {

       $prepqc_old = "prepbufr.pre-qc.${nymd}.t${hh}z.blk" ;

# Rename original prepqc file to serve as input to PREPQC
# -------------------------------------------------------
       cp("$prepqc_new","$prepqc_old");
       unlink("$prepqc_new");

       $cmd = "gmao_prepqc -r $fvhome/run -o $prepqc_new $nymd $nhms $prepqc_old $dynf";
       print "$0: $cmd\n" unless ( $opt_q );

       $rc = System ( $cmd, "prepqc.log" );
       print "$0: prepqc \$rc =  $rc\n" unless ( $opt_q );
       die ">>>> ERROR <<< running prepqc" if ( $rc );
    }
    else {
       print "$0: missing/empty PREPQC file $prepqc_new\n" unless ( $opt_q );
       die ">>>> ERROR <<< running PREPQC in strict mode" if ( $strict );
    }
# Launch prepqc.daemon in the background now
#-------------------------------------------
    if ($BKG_PREPQC) {
	$SIG{CHLD} = 'IGNORE';
	defined($newpid=fork)
	    or die ">>> ERROR <<< unable to fork: $!";
	unless ($newpid) {
	    system("prepqc_daemon.pl");
	    exit(0);
	}
	$SIG{CHLD} = 'DEFAULT'; 
    }
}

#......................................................................
# System: This routine saves stdout/stderr, redirects it to a specified file, 
#         runs a shell command using this new stdout/stderr, and finally 
#         restores the original stdout/stderr.
#

sub System {

    my ( $cmd, $logfile ) = @_;
    my ( @zname );

    open SAVEOUT, ">&STDOUT";  # save stdout
    open SAVEERR, ">&STDERR";  # save stderr

    open STDOUT, ">>$logfile" or die "can't redirect stdout";
    open STDERR, ">>$logfile" or die "can't redirect stderr";

    select STDERR; $| = 1;     # make it unbuffered
    select STDOUT; $| = 1;     # make it unbuffered

    @zname = split(" ", $cmd);
    $rc1 = system( "zeit_ci.x $zname[0]");

    $rc = system ( $cmd );     # run the shell command

    $rc2 = system( "zeit_co.x $zname[0]");

    # Bitwise shift returns actual UNIX return code
    $exit_code = $rc >> 8; 

    close STDOUT;
    close STDERR;

    open STDOUT, ">&SAVEOUT" ;  # restore stdout
    open STDERR, ">&SAVEERR" ;  # restore stdout

    return $exit_code;

  }

#......................................................................

sub usage {

   print <<"EOF";

NAME
     prepqc.pl - Wrapper for PREPQC driver
          
SYNOPSIS

     prepqc.pl [...options...]  nymd  nhms  dyn_f  
          
DESCRIPTION

     This script drives GMAO version of NCEP PREPQC.

     The following parameter are required 

     nymd     Year-month-day, e.g., 19990901  for 01 Sept 1999 
     nhms     Hour-month-sec, e.g., 120000    for 12Z
     dyn_f    dynamics state vector file name (first guess)

OPTIONS
 
 -h            prints this usage notice

 -b upa_bfr    input UPA BFR file for Complex CQC; if specified, the
               flag "-cqc" (see below) is automatically set. The
               default UPA BUFR file name is upa\$nymd.\${hh}z.bufr_d 

 -pb prep_bfr  input prepbufr format file for PREPQC; default is
                   prepbufr.\$nymd.t\${hh}z.blk

 -strict       analysis (ana.x) will abort if any of the input ODS files
               are missing

  -n           dry-run mode: just print what it would do

  -q           quiet mode

ENVIRONMENT

 PREPQC        if set to 1, same as option "-pqc"
 RADCOR        control radiosonde radiation correction
 FVHOME        experiment home directory 
 STRICT        if set to 1, same as option "-strict"

SEE ALSO
      gmao_prepqc  - real driver of PREPQC 

AUTHOR
      Ricardo Todling (Ricardo.Todling\@nasa.gov), NASA/GSFC/GMAO
      Latest update: R. Todling, 19Nov2003

EOF

  exit(1)

	   }
