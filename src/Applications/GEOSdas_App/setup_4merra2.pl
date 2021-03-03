#!/usr/bin/env perl
# 
# setup_4merra2 - overwrites setting from fvsetup based on MERRA-2
#                 this is to be called from within fvsetup
#
#  04Nov2013 Todling  Initial code (mimic from what I have for Atmos-EnKF)
#  31Jan2014 Todling  Handle gsi_sens to allow merra-2 exp to run obs-impact
#
#-----------------------------------------------------------------------------------------------------

use Env;                 # make env vars readily available
use File::Basename;      # for basename(), dirname()
use File::Path;          # for mkpath()
use File::Copy "cp";     # for cp()
use Getopt::Long;        # load module with GetOptions function
use Time::Local;         # time functions
use FindBin;             # so we can find where this script resides

# look for perl packages in the following locations
#--------------------------------------------------

$FVROOT = dirname($FindBin::Bin);
$FVROOT =~ s|/u/.realmounts/share|/share|;   # for portability across
                                             # NAS machines
                                             #   $fvbin = "$fvroot/bin";
                                             #     $user = getpwuid($<) unless ($user = $ENV{"USER"});
$FVETC = "$FVROOT/etc";

$user = getpwuid($<) unless ($user = $ENV{"USER"});

use lib ( "$FindBin::Bin", "$FVROOT/bin" );

my $scriptname = basename($0);

# Command line options

  GetOptions ( "exploc=s",
               "h" );

  usage() if $opt_h;

# Parse command line, etc

  init();

# Generate pbs job script

  install();

# All done

# print "jobgen: resulting files \n";
# $rc_ignore = system('ls -lrt');
  if ($rc==0) {
     print "$0: sucessfully completed.\n\n";
     exit(0);
  } else {
     print "$0: failed to setup resources for atmos-ensemble\n\n";
     exit(1);
  }


#......................................................................

sub init {

   if ( $#ARGV  <  5 ) {
     print STDERR " Missing arguments; see usage:\n";
     usage();
   } else {              # required command line args
     $fvhome      = $ARGV[0];
     $nymd        = $ARGV[1];
     $hh          = $ARGV[2];
     $ana_im      = $ARGV[3];
     $ana_jm      = $ARGV[4];
     $ana_km      = $ARGV[5];
   }

# process options

   $rc    = 0;

   $yyyymmdd_hhz = "${nymd}_${hh}z";

# allow for extra command line

# build internal variables


  @rcana = qw ( gmao_global_satinfo.rc
                gmao_global_scaninfo.rc
                gsi.rc.tmpl
                gsi_sens.rc.tmpl
                sac.nl.tmpl );

  @rcgocart = qw ( BC_GridComp.rc
                   CO2_GridComp.rc
                   CO_GridComp---bbae.rc
                   CO_GridComp---bbaf.rc
                   CO_GridComp---bbbo.rc
                   CO_GridComp---bbgl.rc
                   CO_GridComp---bbla.rc
                   CO_GridComp---bbna.rc
                   CO_GridComp---bbnb.rc
                   CO_GridComp---ffas.rc
                   CO_GridComp---ffeu.rc
                   CO_GridComp---ffna.rc
                   CO_GridComp---ffru.rc
                   CO_GridComp---full.rc
                   CO_GridComp---nbas.rc
                   CO_GridComp---nbeu.rc
                   CO_GridComp---nbgl.rc
                   CO_GridComp---nbna.rc
                   CO_GridComp.rc
                   OC_GridComp---full.rc
                   OC_GridComp.rc
                   SU_GridComp---full.rc
                   SU_GridComp.rc
                   SU_GridComp---volc.rc );

  $MYRUN = "$fvhome/run";
}
#......................................................................

sub install {

# transfer resource files to proper location
# TBD: at this time, no editing is done of the resource
#      user must edit files as needed
foreach $fn ( @rcana ) {
  cp("$FVETC/gsi/MERRA2/$fn","$MYRUN/$fn");
  print "copying $FVETC/gsi/MERRA2/$fn over to $MYRUN/$fn \n";
  if($fn =~ "gsi.rc.tmpl" ) {ed_gsi_rc($MYRUN,"gsi.rc.tmpl")};
  if($fn =~ "gsi_sens.rc.tmpl" ) {ed_gsi_rc($MYRUN,"gsi_sens.rc.tmpl")};
}

# handle GOCART resource files
if ( $nymd < 20000331 ) {
   if ( $hh < 23 ) {
      $thisdir = `pwd`;
      foreach $fn ( @rcgocart ) {
         cp("$FVETC/MERRA2/19600101-20000331/$fn","$MYRUN/gocart/$fn");
      }
   }
}

}

#......................................................................
sub gocart4merra2 {
  my($filename,$token) = @_;

  return 0 unless (! -e "${filename}.original");

  rename("$filename","$filename.original");

  if ( -e "xtmp"   ) {unlink("xtmp")};
  if ( -e "ytmp"   ) {unlink("ytmp")};
  if ( -e "xmerra" ) {unlink("xmerra")};

  $merra = (`grep $token ${filename}.original > xmerra`);
  $merra = (`grep MERRA2 xmerra > ytmp`); unlink("xmerra");
  if ( -z "ytmp" ) {
     cp("$filename.original","$filename");
     print "\n nothing done with file $filename \n";
     return 0;
  }
  $merra = (`cut -c2- ytmp > xmerra`);

  $cmd = `grep -v $token ${filename}.original > xtmp`;
  system($cmd);

  $cmd = `cat xtmp xmerra > $filename`;
  system($cmd);
  print "\n done with file $filename \n";

  if ( -e "xtmp"   ) {unlink("xtmp")};
  if ( -e "ytmp"   ) {unlink("ytmp")};
  if ( -e "xmerra" ) {unlink("xmerra")};
}
#......................................................................
sub ed_gsi_rc {

  my($mydir,$gsirc) = @_;

  my($frun, $ft, $rcd, $jcap);
  print "inside ... $mydir $gsirc \n";

  $ftrun = "$mydir/$gsirc";
  $ft    = "$mydir/tmp.rc";

  open(LUN,"$ftrun") || die "Fail to open $ftrun $!\n";
  open(LUN2,">$ft")  || die "Fail to open $mydir/tmp.rc $!\n";

  # Change variables to the correct inputs
  #---------------------------------------
  if($ana_jm ==  91) {$jcap =  62};
  if($ana_jm == 181) {$jcap = 126};
  if($ana_jm == 361) {$jcap = 254};
  if($ana_jm == 721) {$jcap = 512};

  while( defined($rcd = <LUN>) ) {
    chomp($rcd);
    if($rcd =~ /\@JCAP/)    {$rcd=~ s/\@JCAP/$jcap/; }
    if($rcd =~ /\@GSINLAT/) {$rcd=~ s/\@GSINLAT/$ana_jm/; }   # this is true in split observer mode
    if($rcd =~ /\@GSINLON/) {$rcd=~ s/\@GSINLON/$ana_im/; }   # this is true in split observer mode
    if($rcd =~ /\@NSIG/)    {$rcd=~ s/\@NSIG/$ana_km/g; }
    print(LUN2 "$rcd\n");
  }

  close(LUN);
  close(LUN2);
  cp($ft, $ftrun);
  unlink $ft;
}
#......................................................................

sub usage {

   print <<"EOF";

NAME
     setup_atmens.pl - setup resources to allow running Hybrid ADAS
          
SYNOPSIS

     setup_4merra2.pl [...options...] fvhome
                                      nymd
                                      hh
                                      a_im
                                      a_jm
                                      a_km
          
DESCRIPTION


     The following parameters are required 

     fvhome   location of experiment
     nymd     date as in YYYYMMDD
     hh       hour as in HH
     a_im     number of longitude points for GSI
     a_jm     number of latitude  points for GSI
     a_km     number of levels in GSI


OPTIONS

     -exploc       experiment location, e.g., /discover/nobackup/user/
     -h            prints this usage notice

EXAMPLE COMMAND LINE

     setup_4merra2.pl u000_c72 19981231 21 576 361

NECESSARY ENVIRONMENT

OPTIONAL ENVIRONMENT

AUTHOR

     Ricardo Todling (Ricardo.Todling\@nasa.gov), NASA/GSFC/GMAO
     Last modified: 13Nov2013      by: R. Todling


EOF

  exit(1)

}
