#!/usr/bin/perl
# 
# jobgen
#
#  25Nov2011 Todling  Extracted from fvsetup
#  10Sep2013 Todling  Update to Ganymed-4_0
#  20Sep2013 Todling  Update to handle MERRA-2 SST/ICE location
#  02Mar2017 Todling  Update to cubed-SST BCs
#  18Apr2017 Todling  Update to Icarus-1_0 BCs (revised topo)
#  28Jul2017 Todling  Update to fixed(lakes) version of Icarus-cubed BCs
#  23Oct2019 Todling  Add arg to hand Land BCs
#
#-----------------------------------------------------------------------------------------------------

use Env;                 # make env vars readily available
use File::Basename;      # for basename(), dirname()
use Getopt::Long;        # load module with GetOptions function
use Time::Local;         # time functions
use FindBin;             # so we can find where this script resides

# look for perl packages in the following locations
#--------------------------------------------------
use lib ( "$FindBin::Bin", "$FVROOT/bin", "$ESMADIR/$ARCH/bin" );


my $scriptname = basename($0);

# Command line options

  GetOptions ( "o=s",
               "fvhome=s",
               "sstdir=s",
               "cubed",
               "merra2",
               "h" );

  usage() if $opt_h;

# Parse command line, etc

  init();

# Create script for BCs

  create_g5bcs_script();

# All done

# print "jobgen: resulting files \n";
# $rc_ignore = system('ls -lrt');
  if ($rc==0) {
     print "$0: sucessfully completed.\n\n";
     exit(0);
  } else {
     print "$0: failed to collect diag files\n\n";
     exit(1);
  }

#......................................................................
sub init {

   if ( $#ARGV  <  3 ) {
     print STDERR " Missing arguments; see usage:\n";
     usage();
   } else {              # required command line args
     $agcm_im     = $ARGV[0];
     $agcm_jm     = $ARGV[1];
     $ogcm        = $ARGV[2];
     $lndbcs      = $ARGV[3];
   }

$cubed = 0;
if ( $opt_cubed ) {
    $cubed = 1;
}

if ( $opt_sstdir ) {
     $sstdir = $opt_sstdir;
}

if ( $opt_merra2 ) {
  $pcp_loc = "/discover/nobackup/projects/gmao/share/dao_ops/fvInput/merra_land/precip_CPCUexcludeAfrica-CMAP_corrected_MERRA/GEOSdas-2_1_4";
} else {
  $pcp_loc = "/gpfsm/dnb51/projects/p15/iau/merra_land/precip_CPCU-CMAP_corrected_MERRA/GEOSdas-2_1_4";
}

$outfile = "lnbcs";
if ( $opt_o ) {
     $outfile = $opt_o;
}
if ( $opt_fvhome ) {
     $outfile = "$opt_fvhome/run/$outfile";
}

if ($ogcm eq "c") {
    $ogcm_im  = 360;
    $ogcm_jm  = 180;
    $ogrid    = "${ogcm_im}x${ogcm_jm}";
    $BCSTAG = "$lndbcs/Icarus_Reynolds";
    $fvrtbcs = "g5gcm/bcs/realtime/SST";
    $sstfile = "dataoceanfile_MERRA_sst_1971-current.$ogrid.LE";
    $icefile = "dataoceanfile_MERRA_fraci_1971-current.$ogrid.LE";
}
elsif ($ogcm eq "e") {
    $ogcm_im  = 1440;
    $ogcm_jm  =  720;
    $ogrid    = "${ogcm_im}x${ogcm_jm}";
    $BCSTAG = "$lndbcs/Icarus_MERRA-2";
    $fvrtbcs = "g5gcm/bcs/SST";
    $sstfile = "dataoceanfile_MERRA2_SST.$ogrid.\$year.data";
    $icefile = "dataoceanfile_MERRA2_ICE.$ogrid.\$year.data";
}
elsif ($ogcm eq "f") {
    $ogcm_im  = 2880;
    $ogcm_jm  = 1440;
    $ogrid    = "${ogcm_im}x${ogcm_jm}";
    if ( $lndbcs eq "Icarus-NLv3" ) {
       $BCSTAG = "$lndbcs/Icarus-NLv3_Ostia";
    } else {
       $BCSTAG = "$lndbcs/Icarus_Ostia";
    }
    $fvrtbcs = "g5gcm/bcs/realtime/OSTIA_REYNOLDS";
    $sstfile = "dataoceanfile_OSTIA_REYNOLDS_SST.$ogrid.\$year.data";
    $icefile = "dataoceanfile_OSTIA_REYNOLDS_ICE.$ogrid.\$year.data";
}
elsif ($ogcm eq "C") {  # Cubed-Ocean
    $ogcm_im  = $agcm_im;
    $ogcm_jm  = $agcm_jm;
    $ogrid    = "${ogcm_im}x${ogcm_jm}";
    if ( $lndbcs eq "Icarus-NLv3" ) {
       $BCSTAG = "$lndbcs/Icarus-NLv3_Ostia";
    } else {
       $BCSTAG = "$lndbcs/Icarus_Ostia";
    }
    $fvrtbcs = "g5gcm/bcs/realtime/OSTIA_REYNOLDS";
    $sstfile = "dataoceanfile_OSTIA_REYNOLDS_SST.$ogrid.\$year.data";
    $icefile = "dataoceanfile_OSTIA_REYNOLDS_ICE.$ogrid.\$year.data";
}


$coupled = 0;

}
#......................................................................
sub create_g5bcs_script {

 my($rc);
 my($mydir) = @_;

 my($albedodata,$laigrndata);

 $agcm_im4 = sprintf "%04i", $agcm_im;
 $agcm_jm4 = sprintf "%04i", $agcm_jm;
 $ogcm_im4 = sprintf "%04i", $ogcm_im;
 $ogcm_jm4 = sprintf "%04i", $ogcm_jm;

 if ( $cubed ) {
     $bcsresa = "CF${agcm_im4}x6C";
     if ( $ogcm eq "C" ) {
         $bcsreso = "CF${ogcm_im4}x6C";
     } else {
         $bcsreso = "DE${ogcm_im4}xPE${ogcm_jm4}";
     }
 } else {
     $bcsresa = "DC${agcm_im4}xPC${agcm_jm4}";
     $bcsreso = "DE${ogcm_im4}xPE${ogcm_jm4}";
   }

#my($IMstr,$JMstr,$AGCM_GRIDNAME,$OGCM_GRIDNAME,$BCSRES,$bcsres);
#my($RES_DATELINE,$TILEDATA,$TILEBIN);

 $DATELINE = "DC";
 $bcsres  = "${bcsresa}_${bcsreso}";
 
if ( $cubed ) { 
      $IMstr           = sprintf "%04d", $agcm_im; #`echo $agcm_im | awk '{printf "%4.4i", $1}'`;
      $JMstr           = sprintf "%05d", $agcm_jm; #`echo $agcm_jm | awk '{printf "%5.5i", $1}'`;
      $AGCM_GRIDNAME   = "PE${agcm_im}x${agcm_jm}-CF";
      if ( $ogcm eq "C" ) {
        $OGCM_GRIDNAME   = "CF${ogcm_im}x6C";
      } else {
        $OGCM_GRIDNAME   = "PE${ogcm_im}x${ogcm_jm}-DE";
      }
      $BCSRES          = "c${agcm_im}";
      $RES_DATELINE    = "${agcm_im}x${agcm_jm}";
 } else { 
      $AGCM_GRIDNAME   = "PC${agcm_im}x${agcm_jm}-DC";
      $OGCM_GRIDNAME   = "PE${ogcm_im}x${ogcm_jm}-DE";
      $BCSRES          = "${agcm_im}x${agcm_jm}";
      $RES_DATELINE    = "${agcm_im}x${agcm_jm}_${DATELINE}";
 }
 $TILEDATA = "${bcsres}-Pfafstetter.til";
 $TILEBIN  = "${bcsres}-Pfafstetter.TIL";

 open(SCRIPT,">$outfile") or
 die ">>> ERROR <<< cannot write $outfile";

 print  SCRIPT <<"EOF";
#!/bin/csh -xf

  if ( \$#argv < 1 ) then
       echo " lnbcs error; must input date "
       exit 1
  else
       set nymd = \$1
       set year = `echo \$nymd | cut -c1-4`
  endif

  if ( !(\$?FVHOME) ) then
      echo " lnbcs requires FVHOME to be defined "
      exit 1
  endif
  if ( !(\$?FVROOT) ) then
      echo " lnbcs requires FVROOT to be defined "
      exit 1
  endif

  setenv COUPLED  $coupled
  setenv CUBED    $cubed
  setenv G5GCMBCS \$FVHOME/fvInput/g5gcm/bcs
  setenv G5GRTBCS \$FVHOME/fvInput/$fvrtbcs

# Possibly real-time boundary conditions
# --------------------------------------
  if ( \$COUPLED ) then

     if ( ! -e   \$G5GRTBCS/$ogrid/SEAWIFS_KPAR_mon_clim.$ogrid ) exit 1
     /bin/ln -sf \$G5GRTBCS/$ogrid/SEAWIFS_KPAR_mon_clim.$ogrid   SEAWIFS_KPAR_mon_clim.data

     if ( ! -e   \$G5GRTBCS/DC${agcm_im}xPC${agcm_jm}_TM${ogcm_im}xTM${ogcm_jm}-Pfafstetter.til ) exit 1
     /bin/ln -sf \$G5GRTBCS/DC${agcm_im}xPC${agcm_jm}_TM${ogcm_im}${ogcm_jm}-Pfafstetter.til   tile.data

     if ( ! -e   \$G5GRTBCS/DC${agcm_im}xPC${agcm_jm}_TM${ogcm_im}${ogcm_jm}-Pfafstetter.TRN ) exit 1
     /bin/ln -sf \$G5GRTBCS/DC${agcm_im}xPC${agcm_jm}_TM${ogcm_im}${ogcm_jm}-Pfafstetter.TRN   runoff.bin

     if ( ! -e   \$G5GRTBCS/$ogrid/tripolar_$ogrid.ascii ) exit 1
     /bin/ln -sf \$G5GRTBCS/$ogrid/tripolar_$ogrid.ascii  .

     if ( ! -e   \$G5GRTBCS/$ogrid/vgrid50.ascii ) exit 1
     /bin/ln -sf \$G5GRTBCS/$ogrid/vgrid50.ascii vgrid.ascii

  else

     if ( ! -e   \$G5GRTBCS/$ogrid/$sstfile ) exit 1
     /bin/ln -sf \$G5GRTBCS/$ogrid/$sstfile   sst.data

     if ( ! -e   \$G5GRTBCS/$ogrid/$icefile ) exit 1
     /bin/ln -sf \$G5GRTBCS/$ogrid/$icefile   fraci.data

     if ( ! -e   \$G5GRTBCS/$ogrid/SEAWIFS_KPAR_mon_clim.$ogrid ) exit 1
     /bin/ln -sf \$G5GRTBCS/$ogrid/SEAWIFS_KPAR_mon_clim.$ogrid   SEAWIFS_KPAR_mon_clim.data

     if ( ! -e   \$G5GCMBCS/$BCSTAG/$bcsres/$TILEDATA ) exit 1
     /bin/ln -sf \$G5GCMBCS/$BCSTAG/$bcsres/$TILEDATA   tile.data

  endif

# Climatological boundary conditions
# ----------------------------------
  if ( ! -e   \$G5GCMBCS/$BCSTAG/$bcsres/vegdyn_${RES_DATELINE}_24Aug2017.dat  ) exit 1
  /bin/ln -sf \$G5GCMBCS/$BCSTAG/$bcsres/vegdyn_${RES_DATELINE}_24Aug2017.dat    vegdyn.data

  if ( ! -e   \$G5GCMBCS/$BCSTAG/Shared/pchem.species.Clim_Prod_Loss.z_721x72.nc4  ) exit 1
  /bin/ln -sf \$G5GCMBCS/$BCSTAG/Shared/pchem.species.Clim_Prod_Loss.z_721x72.nc4    species.data

  if ( ! -e   \$G5GCMBCS/$BCSTAG/$bcsres/lai_clim_${RES_DATELINE}.data  ) exit 1
  /bin/ln -sf \$G5GCMBCS/$BCSTAG/$bcsres/lai_clim_${RES_DATELINE}.data    lai.data

  if ( ! -e   \$G5GCMBCS/$BCSTAG/$bcsres/green_clim_${RES_DATELINE}.data  ) exit 1
  /bin/ln -sf \$G5GCMBCS/$BCSTAG/$bcsres/green_clim_${RES_DATELINE}.data    green.data

  if ( ! -e   \$G5GCMBCS/$BCSTAG/$bcsres/ndvi_clim_${RES_DATELINE}.data  ) exit 1
  /bin/ln -sf \$G5GCMBCS/$BCSTAG/$bcsres/ndvi_clim_${RES_DATELINE}.data    ndvi.data

  if (  ! -e  \$G5GCMBCS/$BCSTAG/$bcsres/visdf_${RES_DATELINE}.dat  ) exit 1
  /bin/ln -sf \$G5GCMBCS/$BCSTAG/$bcsres/visdf_${RES_DATELINE}.dat    visdf.dat

  if (  ! -e  \$G5GCMBCS/$BCSTAG/$bcsres/nirdf_${RES_DATELINE}.dat  ) exit 1
  /bin/ln -sf \$G5GCMBCS/$BCSTAG/$bcsres/nirdf_${RES_DATELINE}.dat    nirdf.dat

  if(   ! -e  \$G5GCMBCS/$BCSTAG/$bcsres/topo_DYN_ave_${RES_DATELINE}.data   ) exit 1
  /bin/ln -sf \$G5GCMBCS/$BCSTAG/$bcsres/topo_DYN_ave_${RES_DATELINE}.data   topo_dynave.data

  if(   ! -e  \$G5GCMBCS/$BCSTAG/$bcsres/topo_GWD_var_${RES_DATELINE}.data  ) exit 1
  /bin/ln -sf \$G5GCMBCS/$BCSTAG/$bcsres/topo_GWD_var_${RES_DATELINE}.data  topo_gwdvar.data

  if(   ! -e  \$G5GCMBCS/$BCSTAG/$bcsres/topo_TRB_var_${RES_DATELINE}.data    ) exit 1
  /bin/ln -sf \$G5GCMBCS/$BCSTAG/$bcsres/topo_TRB_var_${RES_DATELINE}.data  topo_trbvar.data

# Convert tile file to binary
# ---------------------------
  if ( \$CUBED ) then
       if( -e        \$G5GCMBCS/$BCSTAG/$bcsres/Gnomonic_$bcsres.dat ) then
         /bin/ln -sf \$G5GCMBCS/$BCSTAG/$bcsres/Gnomonic_$bcsres.dat .
       endif
       /bin/ln -sf  \$G5GCMBCS/$BCSTAG/Shared/*bin .
       /bin/ln -sf  \$G5GCMBCS/$BCSTAG/Shared/*_c2l_*.nc4 .
  endif

  if( -e         \$G5GCMBCS/$BCSTAG/$bcsres/$TILEBIN ) then
     /bin/ln -sf \$G5GCMBCS/$BCSTAG/$bcsres/$TILEBIN tile.bin
  else
      \$FVROOT/bin/binarytile.x tile.data tile.bin
  endif

# Link to precipitation forcing data
# ----------------------------------
  if( ! -e  ExtData/PCP ) then
     /bin/ln -sf $pcp_loc ExtData/PCP
  endif

EOF

  $rc = system("chmod +x $outfile");
  die "Cannot chmod file lnbcs in run directory: $!\n" if ( $rc );

}
#......................................................................
sub usage {

   print <<"EOF";

NAME
     gen_lnbcs.pl - Generate lnbcs script for particular config of GCM
          
SYNOPSIS

     gen_lnbcs.pl [...options...] aim ajm ogcm 
          
DESCRIPTION

     The following parameters are required

     aim      number of x-grid points in Atmos GCM
     ajm      number of y-grid points in Atmos GCM
     ogcm     c, e, and f for low-res, MERRA-2, high res SST
              or
              C for cubed ocean BCs consistent with atmosphere res
     lndbcs   Icarus_Updated (or Icarus-NLv3)

OPTIONS

     -o       speficy (full) filename for output, e.g., /.../nobackup/expid/e501/run/lnbcs
     -ssdir   location of sst boundary condition files
     -fvhome  location of FVHOME
     -cubed   needed for cubed GCM
     -merra2  specify to set related BCs
     -h       prints this usage notice

EXAMPLE COMMAND LINE

     gen_lnbcs.pl -o lnbcs 288 181 f

NECESSARY ENVIRONMENT

OPTIONAL ENVIRONMENT

AUTHOR

     Ricardo Todling (Ricardo.Todling\@nasa.gov), NASA/GSFC/GMAO
     Last modified: 23Oct2019      by: R. Todling


EOF

  exit(1)

}

