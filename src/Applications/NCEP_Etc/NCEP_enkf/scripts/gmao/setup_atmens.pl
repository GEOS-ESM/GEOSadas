#!/usr/bin/env perl
# 
# setup_atmens - setup for an atmospheric ensemble experiment
#
#  25Nov2011 Todling  Initial code
#  30Nov2011 Todling  Add option to define scheme (right now: enkf or engsi)
#  18Apr2013 Todling  Update to 5.10 (and add new RC configurations)
#  22Jul2014 Todling  Add rc to control final observer; aod; post_egcm; odsmatch
#  08Dec2017 Todling  Edits to allow auto setup for diff resolutions
#  16Apr2018 Todling  Settings for Y. Zhu sat-bias-correction
#  02May2018 Todling  Unwired number of levels
#
#-----------------------------------------------------------------------------------------------------

use Env;                 # make env vars readily available
use File::Basename;      # for basename(), dirname()
use File::Path;          # for mkpath()
use File::Copy "cp";     # for cp()
use Getopt::Long;        # load module with GetOptions function
use Shell qw(cat rm);    # cat and rm commands
use Time::Local;         # time functions
use FindBin;             # so we can find where this script resides

# look for perl packages in the following locations
#--------------------------------------------------

$FVROOT = dirname($FindBin::Bin);
$FVROOT =~ s|/u/.realmounts/share|/share|;   # for portability across
                                             # NAS machines
                                             #   $fvbin = "$fvroot/bin";
                                             #     $user = getpwuid($<) unless ($user = $ENV{"USER"});
$user = getpwuid($<) unless ($user = $ENV{"USER"});

use lib ( "$FindBin::Bin", "$FVROOT/bin" );

my $scriptname = basename($0);

# Command line options

  GetOptions ( "atmens=s",
               "expdir=s",
               "fvhome=s",
               "nlevs=s",
               "lsmcm",
               "radbc",
               "vtxrlc",
               "nosppt",
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
     print "$0: you now must edit files under $AENSHOME\n\n";
     exit(0);
  } else {
     print "$0: failed to setup resources for atmos-ensemble\n\n";
     exit(1);
  }


#......................................................................

sub init {

   if ( $#ARGV  <  4 ) {
     print STDERR " Missing arguments; see usage:\n";
     usage();
   } else {              # required command line args
     $scheme      = $ARGV[0];
     $expid       = $ARGV[1];
     $aim         = $ARGV[2];
     $ajm         = $ARGV[3];
     $ogrid       = $ARGV[4];
   }

# process options

   $rc    = 0;

# allow for extra command line

   if ( $opt_expdir ) {
        $expdir = $opt_expdir;
   } else {
        $expdir = "/discover/nobackup/$user";
   }

   if ( $opt_fvhome ) {
        $FVHOME = $opt_fvhome;
   } else {
        $FVHOME = "$expdir/$expid";
   }

   $setvtx = 0;
   if ( $opt_vtxrlc ) {
      $setvtx = 1;
   }

   $lsmodel = 1;
   if ( $opt_lsmcm ) {
        $lsmodel = 2;
   }

   $setradbc = 0;
   if ( $opt_radbc ) {
      $setradbc = 1;
   }
   $nlevs = 72;
   if ( $opt_nlevs ) {
      $nlevs = $opt_nlevs;
   }

   $dosppt = 1;
   if ( $opt_nosppt ) {
      $dosppt = 0;
   }


# define location where ensemble members will reside
   if ( $opt_atmens ) {
        $ATMENS = $opt_atmens;
   } else {
        $ATMENS = "$FVHOME/atmens";
   }

# determined whether cubed or not
  $cubed = "";
  $jm_cubed = 6 * $aim;
  if ( $jm_cubed == $ajm ) { 
      $cubed = "-cubed";
      $grid_type = "Cubed-Sphere";
      $agcm_nf = 6;
      $latlon_agcm = "#";
      $cube_agcm = "";
  } else {
      $grid_type = "LatLon";
      $agcm_nf = 1;
      $latlon_agcm = "#";
      $cube_agcm = "";
  } 
  $agcm_im = $aim;
  $agcm_jm = $ajm;
  $agcm_lm = $nlevs;

# define ocean (prescribed) resolution
  if( "$ogrid" eq "c" ) {
    $ogcm_grid_type = "LatLon";
    $ogcm_im = 1440;
    $ogcm_jm = 720;
    $ogcm_lm = 34;
    $ogcm_nf  = 1;
    $cube_ogcm = "#";
    $latlon_ogcm = "";
  }
  if( "$ogrid" eq "f" ) {
    $ogcm_grid_type = "LatLon";
    $ogcm_im = 2880;
    $ogcm_jm = 1440; 
    $ogcm_lm = 34;
    $ogcm_nf  = 1;
    $cube_ogcm = "#";
    $latlon_ogcm = "";
  }
  if( "$ogrid" eq "C" ) {
    $ogcm_grid_type = "Cubed-Sphere";
    $ogcm_im = $aim;
    $ogcm_jm = $ajm;
    $ogcm_lm = 34;
    $ogcm_nf  = 6;
    $cube_ogcm = "";
    $latlon_ogcm = "#";
  }

# define layout depending on resolution
  $agcm_nx = 4; $agcm_ny = 12;
  if ( $agcm_im == 90 ){
     $agcm_nx =    4; $agcm_ny =   12;
     $miau_nx =    2; $miau_ny =   12;
     $obsv_nx =    4; $obsv_ny =    8;
     $stat_nx =    2; $stat_ny =    2;
     $obsv_im =  288; $obsv_jm =  181; $obsv_lm = $nlevs; $obsv_jcap = 126;
  }
  if ( $agcm_im == 180 ){
     $agcm_nx =    7; $agcm_ny =   12;
     $miau_nx =    2; $miau_ny =   12;
     $obsv_nx =    4; $obsv_ny =   14;
     $stat_nx =    2; $stat_ny =   14;
     $obsv_im =  576; $obsv_jm =  361; $obsv_lm = $nlevs; $obsv_jcap = 254;
  }

# build internal variables

  @generalrc = qw ( odsmatch.rc );

  @resources = qw ( AGCM.rc.tmpl
                    AtmEnsConfig.csh
		    aens_stoch.rc
                    atmens_storage.arc
                    atmens_efsostorage.arc
                    CAP.rc.tmpl
                    dyn_recenter.rc
                    GAAS_GridComp.rc
                    GEOS_ChemGridComp.rc
                    GSI_GridComp.rc.tmpl
                    GSI_GridComp_ensfinal.rc.tmpl
                    HISTAENS.rc.tmpl
                    mkiau.rc.tmpl
                    mp_stats.rc
                    nmcperts.rc
                    odsstats_ktonly.rc
                    post_egcm.rc );

  @rcenkf    = qw ( obs1gsi_mean.rc
                    obs1gsi_member.rc 
                    atmos_enkf.nml.tmpl
                    atmos_enkf_sens.nml.tmpl );

  @rcengsi   = qw ( gsi_mean.rc
                    gsi_member.rc );

  @rceasy    = qw ( easyeana.rc );
 
  @rcvtx     = qw ( vtrack.ctl.tmpl
                    vtrack.rc
                    vtx.ctl.tmpl );

# location where ensemble RC files reside
  $AENSHOME = "$FVHOME/run/atmens";

}
#......................................................................

sub install {

if ( ! -d $AENSHOME ) {
   $rc = system("/bin/mkdir -p $AENSHOME" );
}

# transfer resource files to proper location
# TBD: at this time, no editing is done of the resource
#      user must edit files as needed
foreach $fn ( @generalrc ) {
  cp("$FVROOT/etc/$fn","$AENSHOME/$fn");
}
foreach $fn ( @resources ) {
  chomp($fn);
  if ( "$fn" eq "dyn_recenter.rc" ) {
    if ( $dosppt ) {
       cp("$FVROOT/etc/atmens/dyn_recenter_l${agcm_lm}_sppt.rc","$AENSHOME/$fn");
    } else {
       cp("$FVROOT/etc/atmens/dyn_recenter_l${agcm_lm}.rc","$AENSHOME/$fn");
    }
  } else {
    cp("$FVROOT/etc/atmens/$fn","$AENSHOME/$fn");
  }
}

if ( "$scheme" eq "enkf" ) {
  foreach $fn ( @rcenkf ) {
    cp("$FVROOT/etc/atmens/$fn","$AENSHOME/$fn");
  }
}

if ( "$scheme" eq "engsi" ) {
# for now I (RT) want this to not be fully available until fully tested
# foreach $fn ( @rcengsi ) {
#   cp("$FVROOT/etc/atmens/$fn","$AENSHOME/$fn");
# }
  print "$0: Place your own gsi_mean.rc and gsi_member.rc under $AENSHOME.\n\n";
}

if ( "$scheme" eq "easy" ) {
  foreach $fn ( @rceasy ) {
    cp("$FVROOT/etc/atmens/$fn","$AENSHOME/$fn");
  }
}

# copy vortex relocator and tracker to ensemble control directory
if ( $setvtx ) {
   foreach $fn ( @rcvtx ) {
     if ( -e "$FVHOME/run/$fn" ) {
        cp("$FVHOME/run/$fn","$AENSHOME/$fn");
     }
   }
}

# main driver script controlling ensemble
cp("$FVROOT/bin/atm_ens.j","$FVHOME/run");

# generate boundary condition script
$cmd = "$FVROOT/bin/gen_lnbcs.pl $cubed -o $FVHOME/run/lnbcs_ens $aim $ajm $ogrid";
$rc = system($cmd);

# make sure .no_archiving exists in ATMENS
if ( ! -d "$ATMENS" ) {
   $rc = system("/bin/mkdir -p $ATMENS" );
}
$cmd = "touch $ATMENS/.no_archiving";
$rc = system($cmd);

# take of resolution and layout
ed_agcm_rc ("$AENSHOME");
ed_hist_rc ("$AENSHOME");
ed_miau_rc ("$AENSHOME");
ed_obsv_rc ("$AENSHOME");
ed_enkf_rc ("$AENSHOME");
ed_stat_rc ("$AENSHOME");

# take care of satbias acq
ed_satbias_acq ("$AENSHOME");
ed_aod4aens_acq ("$AENSHOME");

}
#......................................................................
sub ed_agcm_rc {

  my($mydir) = @_;

  my($acq);

  $tmprc  = "$mydir/tmp.rc";
  $thisrc = "$mydir/AGCM.rc.tmpl";

     open(LUN,"$thisrc")  || die "Fail to open $thisrc $!\n";
     open(LUN2,">$tmprc") || die "Fail to open tmp.rc $!\n";

     # Change variables to the correct inputs
     #---------------------------------------
     while( defined($rcd = <LUN>) ) {
        chomp($rcd);
        if($rcd =~ /\@NX/) {$rcd=~ s/\@NX/$agcm_nx/g; }
        if($rcd =~ /\@NY/) {$rcd=~ s/\@NY/$agcm_ny/g; }
        if($rcd =~ /\@GRID_TYPE/) {$rcd=~ s/\@GRID_TYPE/$grid_type/g; }
        if($rcd =~ /\@CUBE_AGCM/) {$rcd=~ s/\@CUBE_AGCM/$cube_agcm/g; }
        if($rcd =~ /\@LATLON_AGCM/) {$rcd=~ s/\@LATLON_AGCM/$latlon_agcm/g; }
        if($rcd =~ /\@AGCM_IM/) {$rcd=~ s/\@AGCM_IM/$agcm_im/g; }
        if($rcd =~ /\@AGCM_JM/) {$rcd=~ s/\@AGCM_JM/$agcm_jm/g; }
        if($rcd =~ /\@AGCM_LM/) {$rcd=~ s/\@AGCM_LM/$agcm_lm/g; }
        if($rcd =~ /\@AGCM_NF/) {$rcd=~ s/\@AGCM_NF/$agcm_nf/g; }

        if($rcd =~ /\@OGCM_GRID_TYPE/) {$rcd=~ s/\@OGCM_GRID_TYPE/$ogcm_grid_type/g; }
        if($rcd =~ /\@CUBE_OGCM/) {$rcd=~ s/\@CUBE_OGCM/$cube_ogcm/g; }
        if($rcd =~ /\@LATLON_OGCM/) {$rcd=~ s/\@LATLON_OGCM/$latlon_ogcm/g; }
        if($rcd =~ /\@OGCM_IM/) {$rcd=~ s/\@OGCM_IM/$ogcm_im/g; }
        if($rcd =~ /\@OGCM_JM/) {$rcd=~ s/\@OGCM_JM/$ogcm_jm/g; }
        if($rcd =~ /\@OGCM_LM/) {$rcd=~ s/\@OGCM_LM/$ogcm_lm/g; }
        if($rcd =~ /\@OGCM_NF/) {$rcd=~ s/\@OGCM_NF/$ogcm_nf/g; }

        if ( $dosppt ) {
           if($rcd =~ /\@AENS_DOSPPT/) {$rcd=~ s/\@AENS_DOSPPT/1/g; }
        } else {
           if($rcd =~ /\@AENS_DOSPPT/) {$rcd=~ s/\@AENS_DOSPPT/0/g; }
        }

        if ( $lsmodel == 2 ) {
           if($rcd =~ /\@LSM_CHOICE/) {$rcd=~ s/\@LSM_CHOICE/2/g; }
           if($rcd =~ /\@LSM_PARMS/) {$rcd=~ s/\@LSM_PARMS/ /g; }
        } else {
           if($rcd =~ /\@LSM_CHOICE/) {$rcd=~ s/\@LSM_CHOICE/1/g; }
           if($rcd =~ /\@LSM_PARMS/) {$rcd=~ s/\@LSM_PARMS/#/g; }
        }

        if($rcd =~ /\@EMISSIONS/) {$rcd=~ s/\@EMISSIONS/g5chem/g; }
        print(LUN2 "$rcd\n");
     }

     close(LUN);
     close(LUN2);
     cp($tmprc, $thisrc);
     unlink $tmprc;

}
#......................................................................
sub ed_hist_rc {

  my($mydir) = @_;

  my($acq);

  $tmprc  = "$mydir/tmp.rc";
  $thisrc = "$mydir/HISTAENS.rc.tmpl";

     open(LUN,"$thisrc")  || die "Fail to open $thisrc $!\n";
     open(LUN2,">$tmprc") || die "Fail to open tmp.rc $!\n";

     # Change variables to the correct inputs
     #---------------------------------------
     while( defined($rcd = <LUN>) ) {
        chomp($rcd);
        if ( $dosppt ) {
           if($rcd =~ /\@AENS_DOSPPT/) {$rcd=~ s/\@AENS_DOSPPT/ /g; }
        } else {
           if($rcd =~ /\@AENS_DOSPPT/) {$rcd=~ s/\@AENS_DOSPPT/#/g; }
        }

        print(LUN2 "$rcd\n");
     }

     close(LUN);
     close(LUN2);
     cp($tmprc, $thisrc);
     unlink $tmprc;

}
#......................................................................
sub ed_miau_rc {

  my($mydir) = @_;

  my($acq);

  $tmprc  = "$mydir/tmp.rc";
  $thisrc = "$mydir/mkiau.rc.tmpl";

     open(LUN,"$thisrc")  || die "Fail to open $thisrc $!\n";
     open(LUN2,">$tmprc") || die "Fail to open tmp.rc $!\n";

     # Change variables to the correct inputs
     #---------------------------------------
     while( defined($rcd = <LUN>) ) {
        chomp($rcd);
        if($rcd =~ /\@NX/) {$rcd=~ s/\@NX/$miau_nx/g; }
        if($rcd =~ /\@NY/) {$rcd=~ s/\@NY/$miau_ny/g; }
        if($rcd =~ /\@AGCM_IM/) {$rcd=~ s/\@AGCM_IM/$agcm_im/g; }
        if($rcd =~ /\@AGCM_JM/) {$rcd=~ s/\@AGCM_JM/$agcm_jm/g; }
        if($rcd =~ /\@AGCM_LM/) {$rcd=~ s/\@AGCM_LM/$agcm_lm/g; }
        print(LUN2 "$rcd\n");
     }

     close(LUN);
     close(LUN2);
     cp($tmprc, $thisrc);
     unlink $tmprc;

}
#......................................................................
sub ed_obsv_rc {

  my($mydir) = @_;

  my($acq);

  @observer_files = qw ( GSI_GridComp.rc.tmpl
                         GSI_GridComp_ensfinal.rc.tmpl
                         obs1gsi_mean.rc
                         obs1gsi_member.rc );
  $nsig_ext = 13;
  if ( $obsv_lm > 72 ) {
    $nsig_ext = 15;
  }

  foreach $file (@observer_files) {

     $tmprc  = "$mydir/tmp.rc";
     $thisrc = "$mydir/$file";

     open(LUN,"$thisrc")  || die "Fail to open $thisrc $!\n";
     open(LUN2,">$tmprc") || die "Fail to open tmp.rc $!\n";

     # Change variables to the correct inputs
     #---------------------------------------
     while( defined($rcd = <LUN>) ) {
        chomp($rcd);
        if($rcd =~ /\@NX/) {$rcd=~ s/\@NX/$obsv_nx/g; }
        if($rcd =~ /\@NY/) {$rcd=~ s/\@NY/$obsv_ny/g; }
        if($rcd =~ /\@AGCM_IM/) {$rcd=~ s/\@AGCM_IM/$obsv_im/g; }
        if($rcd =~ /\@AGCM_JM/) {$rcd=~ s/\@AGCM_JM/$obsv_jm/g; }
        if($rcd =~ /\@AGCM_LM/) {$rcd=~ s/\@AGCM_LM/$obsv_lm/g; }
        if($rcd =~ /\@GSI_IM/)  {$rcd=~ s/\@GSI_IM/$obsv_im/g; }
        if($rcd =~ /\@GSI_JM/)  {$rcd=~ s/\@GSI_JM/$obsv_jm/g; }
        if($rcd =~ /\@GSI_LM/)  {$rcd=~ s/\@GSI_LM/$obsv_lm/g; }
        if($rcd =~ /\@GSI_JCAP/){$rcd=~ s/\@GSI_JCAP/$obsv_jcap/g; }
        if($rcd =~ /\@NSIG_EXT/){$rcd=~ s/\@NSIG_EXT/$nsig_ext/g; }
        if ( $setradbc == 0 ) {
           if($rcd =~ /\@RADBC/) {$rcd=~ s/\@RADBC/!/g; }
        } else {
           if($rcd =~ /\@RADBC/) {$rcd=~ s/\@RADBC//g; }
        } 
         print(LUN2 "$rcd\n");
      }
     close(LUN);
     close(LUN2);
     cp($tmprc, $thisrc);
     unlink $tmprc;

  }
}
#......................................................................
sub ed_enkf_rc {

  return 0 unless ( "$scheme" eq "enkf" );

  my($mydir) = @_;

  my($acq);

  @enkf_files = qw ( atmos_enkf.nml.tmpl
                     atmos_enkf_sens.nml.tmpl );

  foreach $file (@enkf_files) {

     $tmprc  = "$mydir/tmp.rc";
     $thisrc = "$mydir/$file";

     open(LUN,"$thisrc")  || die "Fail to open $thisrc $!\n";
     open(LUN2,">$tmprc") || die "Fail to open tmp.rc $!\n";

     # Change variables to the correct inputs
     #---------------------------------------
     while( defined($rcd = <LUN>) ) {
        chomp($rcd);
        if ( $setradbc == 0 ) {
           if($rcd =~ /\@RADBC/) {$rcd=~ s/\@RADBC/!/g; }
        } else {
           if($rcd =~ /\@RADBC/) {$rcd=~ s/\@RADBC//g; }
        } 
         print(LUN2 "$rcd\n");
      }
     close(LUN);
     close(LUN2);
     cp($tmprc, $thisrc);
     unlink $tmprc;

  }
}
#......................................................................
sub ed_stat_rc {

  my($mydir) = @_;

  my($acq);

  $tmprc  = "$mydir/tmp.rc";
  $thisrc = "$mydir/mp_stats.rc";

     open(LUN,"$thisrc")  || die "Fail to open $thisrc $!\n";
     open(LUN2,">$tmprc") || die "Fail to open tmp.rc $!\n";

     # Change variables to the correct inputs
     #---------------------------------------
     while( defined($rcd = <LUN>) ) {
        chomp($rcd);
        if($rcd =~ /\@NX/) {$rcd=~ s/\@NX/$stat_nx/g; }
        if($rcd =~ /\@NY/) {$rcd=~ s/\@NY/$stat_ny/g; }
        if($rcd =~ /\@MP_STATS_IM/) {$rcd=~ s/\@MP_STATS_IM/$obsv_im/g; }
        if($rcd =~ /\@MP_STATS_JM/) {$rcd=~ s/\@MP_STATS_JM/$obsv_jm/g; }
        if($rcd =~ /\@MP_STATS_LM/) {$rcd=~ s/\@MP_STATS_LM/$obsv_lm/g; }
        print(LUN2 "$rcd\n");
     }

     close(LUN);
     close(LUN2);
     cp($tmprc, $thisrc);
     unlink $tmprc;

}
#......................................................................
sub ed_satbias_acq {

  my($mydir) = @_;

  my($acq);

  if( $setradbc ) {
    $mysatbiaspc = "$ATMENS/RST/$expid.ana_satbiaspc_rst.%y4%m2%d2_%h2z.txt => $expid.ana.satbiaspc.%y4%m2%d2_%h2z.txt"; 
    chomp($mysatbiaspc);
  } else {
    $mysatbiaspc = "";
  }

  $acq = "$mydir/satbias.acq";

 open(SCRIPT,">$acq") or
 die ">>> ERROR <<< cannot write $acq";
 print  SCRIPT <<"EOF";
#$ATMENS/central/$expid.ana.acftbias.%y4%m2%d2_%h2z.txt
#$ATMENS/central/$expid.ana.satbias.%y4%m2%d2_%h2z.txt
#$ATMENS/central/$expid.ana.satbang.%y4%m2%d2_%h2z.txt
#$ATMENS/RST/$expid.ana_acftbias_rst.%y4%m2%d2_%h2z.txt => $expid.ana.acftbias.%y4%m2%d2_%h2z.txt
$ATMENS/RST/$expid.ana_satbias_rst.%y4%m2%d2_%h2z.txt => $expid.ana.satbias.%y4%m2%d2_%h2z.txt
$ATMENS/RST/$expid.ana_satbang_rst.%y4%m2%d2_%h2z.txt => $expid.ana.satbang.%y4%m2%d2_%h2z.txt
$mysatbiaspc
EOF
}
#......................................................................
sub ed_aod4aens_acq {

  my($mydir) = @_;

  my($acq);

  $acq = "$mydir/aod4aens.acq";

 open(SCRIPT,">$acq") or
 die ">>> ERROR <<< cannot write $acq";
 print  SCRIPT <<"EOF";
$ATMENS/central/$expid.aod_a.sfc.%y4%m2%d2_%h200z.nc4
$ATMENS/central/$expid.aod_f.sfc.%y4%m2%d2_%h200z.nc4
$ATMENS/central/$expid.aod_k.sfc.%y4%m2%d2_%h200z.nc4
EOF
}
#......................................................................

sub usage {

   print <<"EOF";

NAME
     setup_atmens.pl - setup resources to allow running Hybrid ADAS
          
SYNOPSIS

     setup_atmens.pl [...options...] scheme
                                     expid
                                     aim
                                     ajm
                                     ogrid
          
DESCRIPTION


     The following parameters are required 

     scheme   enkf or engsi
     expid    experiment name, e..g., u000_c72
     aim      number of x-grid points in Atmos GCM
     ajm      number of y-grid points in Atmos GCM
     ogrid    c, f, or C for low- or high-resolution Ocean GCM


OPTIONS

     -atmens       location of ensemble members (default: \$FVHOME/atmens)
     -expdir       experiment location (default: /discover/nobackup/user)
     -fvhome       location of experiment home directory (default: \$expdir/\$expid)
     -nlevs        number of atmospheric levels (default: 72)
     -radbc        set up to run with Y. Zhu satellite bias correction
     -vtxrlc       use vortex tracker and relocator
     -nosppt       use to deactive SPPT scheme; use non-perturbed members
     -h            prints this usage notice

EXAMPLE COMMAND LINE

     setup_atmens.pl enkf u000_C72 90 540 C

NECESSARY ENVIRONMENT

OPTIONAL ENVIRONMENT

AUTHOR

     Ricardo Todling (Ricardo.Todling\@nasa.gov), NASA/GSFC/GMAO
     Last modified: 16Apr2018      by: R. Todling


EOF

  exit(1)

}
