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
#  08Jun2020 Todling  Export mp_stats_perts
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
$user = getpwuid($<) unless ($user = $ENV{"USER"});

use lib ( "$FindBin::Bin", "$FVROOT/bin" );

my $scriptname = basename($0);

# Command line options

  GetOptions ( "atmens=s",
               "acftbc=s",
               "expdir=s",
               "fvhome=s",
               "nlevs=s",
               "nodename=s",
               "lsmcm",
               "radbc",
               "vtxrlc",
               "nosppt",
               "ose",
               "rcorr",
	       "r21c",
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

   if ( $#ARGV  <  5 ) {
     print STDERR " Missing arguments; see usage:\n";
     usage();
   } else {              # required command line args
     $scheme      = $ARGV[0];
     $expid       = $ARGV[1];
     $aim         = $ARGV[2];
     $ajm         = $ARGV[3];
     $ogrid       = $ARGV[4];
     $lndbcs      = $ARGV[5];
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

   $lsmchoice = 1;
   if ( "$lndbcs" eq "Icarus-NLv3" ) {
      if ( $opt_lsmcm ) {
           $lsmchoice = 2;
      }
   }

   $bcopt = "";
   if ( $opt_r21c ) {
      $bcopt = "-r21c";
   }

   $setradbc = 0;
   if ( $opt_radbc ) {
      $setradbc = 1;
   }
   $nlevs = 72;
   if ( $opt_nlevs ) {
      $nlevs = $opt_nlevs;
   }

   $nodename = "hasw";
   if ( $opt_nodename ) {
      $nodename = $opt_nodename;
   }

   $dosppt = 1;
   if ( $opt_nosppt ) {
      $dosppt = 0;
   }

   $setacftbc = 0;
   if ( $opt_acftbc ) {
      $setacftbc = $opt_acftbc;
   }

   $dorcorr = 0;
   if ( $opt_rcorr ) {
      $dorcorr = 1;
   }

   $doose = 0;
   if ( $opt_ose ) {
      $doose = 1;
      $dosppt = 0; # make sure to deactivate SPPT regardless of what command line might be
      $ATMOSE = "$FVHOME/atmose";
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
  $dt = 450; # wired for all resolutions
  $odt = 3600; # wired for all resolutions
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
    $ogridname = "OC${ogcm_im}x${ogcm_jm}-DE";
  }
  if( "$ogrid" eq "f" ) {
    $ogcm_grid_type = "LatLon";
    $ogcm_im = 2880;
    $ogcm_jm = 1440; 
    $ogcm_lm = 34;
    $ogcm_nf  = 1;
    $cube_ogcm = "#";
    $latlon_ogcm = "";
    $ogridname = "OC${ogcm_im}x${ogcm_jm}-DE";
  }
  if( "$ogrid" eq "C" ) {
    $ogcm_grid_type = "Cubed-Sphere";
    $ogcm_im = $aim;
    $ogcm_jm = $ajm;
    $ogcm_lm = 34;
    $ogcm_nf  = 6;
    $cube_ogcm = "";
    $latlon_ogcm = "#";
    $ogridname = "OC${ogcm_im}x${ogcm_jm}-CF";
  }

  if ( $nodename eq "hasw" ) { $ncpus_per_node = 24; }
  if ( $nodename eq "sky"  ) { $ncpus_per_node = 36; }
  if ( $nodename eq "cas"  ) { $ncpus_per_node = 46; }
  $agcm_ncpus_per_node = -1;

# define layout depending on resolution
  $agcm_nx = 4; $agcm_ny = 12;
  if ( $agcm_im == 90 ){
     if ($nodename eq "hasw") {
         $enkf_cpus = 192;
         $agcm_nx =    4; $agcm_ny =   12;
         $miau_nx =    2; $miau_ny =   12;
         $obsv_nx =    4; $obsv_ny =    8;
         $stat_nx =    2; $stat_ny =    2;
     } elsif ($nodename eq "sky") {
#        $agcm_ncpus_per_node = 36;
         $enkf_cpus = 244;
         $agcm_nx =    3; $agcm_ny =   30;
         $miau_nx =    2; $miau_ny =   12;
         $obsv_nx =    4; $obsv_ny =    8;
         $stat_nx =    2; $stat_ny =    2;
     } elsif ($nodename eq "cas") {
         die "Sorry this node/resolution not set yet, aborting \n";
#        $agcm_ncpus_per_node = 46;
     } else {
         die "Unknown node name, aborting \n";
     }
     $chis_im =   90; $chis_jm =  540;  # cubed-resolution
     $dhis_im =  288; $dhis_jm =  181;  # diag-resolution output
     $hhis_im =  288; $hhis_jm =  181;  # high-resolution output
     $lhis_im =  288; $lhis_jm =  181;  #  low-resolution output
     $obsv_im =  288; $obsv_jm =  181; $obsv_lm = $nlevs; $obsv_jcap = 126;
  }
  if ( $agcm_im == 180 ){
     if ($nodename eq "hasw") {
         $enkf_cpus = 224;
         $agcm_nx =    6; $agcm_ny =   12;
         $miau_nx =    2; $miau_ny =   12;
         $obsv_nx =    4; $obsv_ny =   14;
         $stat_nx =    2; $stat_ny =   14;
     } elsif ($nodename eq "sky") {
#        $agcm_ncpus_per_node = 36;
         $enkf_cpus = 368;
         $agcm_nx =    4; $agcm_ny =   30;
         $miau_nx =    2; $miau_ny =   12;
         $obsv_nx =    4; $obsv_ny =   20;
         $stat_nx =    2; $stat_ny =   20;
     } elsif ($nodename eq "cas") {
#        $agcm_ncpus_per_node = 46;
         $enkf_cpus = 442;
         $agcm_nx =    6; $agcm_ny =   24;
         $miau_nx =    2; $miau_ny =   12;
         $obsv_nx =    4; $obsv_ny =   24;
         $stat_nx =    2; $stat_ny =   24;
         die "Sorry this node/resolution not set yet, aborting \n";
     } else {
         die "Unknown node name, aborting \n";
     }
     $chis_im =  180; $chis_jm = 1080;  # cubed-resolution
     $dhis_im =  288; $dhis_jm =  181;
     $hhis_im =  576; $hhis_jm =  361;
     $lhis_im =  576; $lhis_jm =  361;
     $obsv_im =  576; $obsv_jm =  361; $obsv_lm = $nlevs; $obsv_jcap = 254;
  }
  if ( $agcm_im == 360 ){
     if ($nodename eq "hasw") {
         $enkf_cpus = 672;
         $agcm_nx =    3; $agcm_ny =   72;
         $miau_nx =    4; $miau_ny =   24;
         $obsv_nx =    4; $obsv_ny =   14;
         $stat_nx =    3; $stat_ny =   12;
     } elsif ($nodename eq "sky") {
         die "Sorry this node/resolution not set yet, aborting \n";
#        $agcm_ncpus_per_node = 36;
     } elsif ($nodename eq "cas") {
         die "Sorry this node/resolution not set yet, aborting \n";
#        $agcm_ncpus_per_node = 46;
     } else {
         die "Unknown node name, aborting \n";
     }
     $chis_im =  360; $chis_jm = 2160;  # cubed-resolution
     $dhis_im =  288; $dhis_jm =  181;
     $hhis_im = 1152; $hhis_jm =  721;
     $lhis_im =  576; $lhis_jm =  361;
     $obsv_im =  576; $obsv_jm =  361; $obsv_lm = $nlevs; $obsv_jcap = 254;
  }
  $agcm_cpus = $agcm_nx * $agcm_ny;
  $miau_cpus = $miau_nx * $miau_ny;
  $obsv_cpus = $obsv_nx * $obsv_ny;
  $stat_cpus = $stat_nx * $stat_ny;

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
                    mp_stats_perts.rc
                    nmcperts.rc
                    odsstats_ktonly.rc
                    post_egcm.rc );

  @osercs =    qw ( AGCM.rc.tmpl
                    AtmOSEConfig.csh
                    atmens_storage.arc
                    atmens_efsostorage.arc
                    CAP.rc.tmpl
                    GAAS_GridComp.rc
                    GEOS_ChemGridComp.rc
                    GSI_GridComp.rc.tmpl
                    HISTAOSE.rc.tmpl
                    mkiau.rc.tmpl
                    mp_stats.rc
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
# location where OSE RC files reside
  $AOSEHOME = "$FVHOME/run/atmose";

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
$cmd = "$FVROOT/bin/gen_lnbcs.pl $cubed $bcopt -o $FVHOME/run/lnbcs_ens $aim $ajm $ogrid $lndbcs";
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
ed_conf_rc ("$AENSHOME","AtmEnsConfig.csh");
if ( $doose ) {
   ed_conf_rc("$AOSEHOME","AtmOSEConfig.csh");
}
ed_g5fvlay_rc ("$AENSHOME");

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

        if($rcd =~ /\@CONUS/)     {$rcd=~ s/\@CONUS/#/g; }
        if($rcd =~ /\@FV_HWT/)    {$rcd=~ s/\@FV_HWT/#/g; }
        if($rcd =~ /\@BACM_1M_/)  {$rcd=~ s/\@BACM_1M_/ /g; }  # Bacmeister microphysics (wired)
        if($rcd =~ /\@GFDL_1M_/)  {$rcd=~ s/\@GFDL_1M_/#/g; }
        if($rcd =~ /\@GMGB2_2M_/) {$rcd=~ s/\@MGB2_2M_/#/g; }
        if($rcd =~ /\@HIST_GOCART/) {$rcd=~ s/\@HIST_GOCART/ /g; }
        if($rcd =~ /\@CONVPAR_OPTION/)  {$rcd=~ s/\@CONVPAR_OPTION/GF/g; }

        if($rcd =~ /\@COUPLED/) {$rcd=~ s/\@COUPLED/#/g; } # wired for now

        if($rcd =~ /\@OCEAN_DT/) {$rcd=~ s/\@OCEAN_DT/${odt}/g; } # wired for now
        if($rcd =~ /\@OGCM_GRIDNAME/) {$rcd=~ s/\@OGCM_GRIDNAME/${ogridname}/g; } # wired for now
        if($rcd =~ /\@DATAOCEAN/) {$rcd=~ s/\@DATAOCEAN/ /g; } # wired for now
        if($rcd =~ /\@MOM5/) {$rcd=~ s/\@MOM5/#/g; } # wired for now
        if($rcd =~ /\@MOM6/) {$rcd=~ s/\@MOM6/#/g; } # wired for now
        if($rcd =~ /\@MIT/) {$rcd=~ s/\@MIT/#/g; } # wired for now
        if($rcd =~ /\@CICE4/) {$rcd=~ s/\@CICE4/#/g; } # wired for now
        if($rcd =~ /\@CICE6/) {$rcd=~ s/\@CICE6/#/g; } # wired for now

        if($rcd =~ /\@LONG_DT/)   {$rcd=~ s/\@LONG_DT/$dt/g; }

        if($rcd =~ /\@OCEAN_NAME/) {$rcd=~ s/\@OCEAN_NAME/MOM6/g; }  # wired
        if($rcd =~ /\@OGCM_GRID_TYPE/) {$rcd=~ s/\@OGCM_GRID_TYPE/$ogcm_grid_type/g; }
        if($rcd =~ /\@CUBE_OGCM/) {$rcd=~ s/\@CUBE_OGCM/$cube_ogcm/g; }
        if($rcd =~ /\@LATLON_OGCM/) {$rcd=~ s/\@LATLON_OGCM/$latlon_ogcm/g; }
        if($rcd =~ /\@OGCM_IM/) {$rcd=~ s/\@OGCM_IM/$ogcm_im/g; }
        if($rcd =~ /\@OGCM_JM/) {$rcd=~ s/\@OGCM_JM/$ogcm_jm/g; }
        if($rcd =~ /\@OGCM_LM/) {$rcd=~ s/\@OGCM_LM/$ogcm_lm/g; }
        if($rcd =~ /\@OGCM_NF/) {$rcd=~ s/\@OGCM_NF/$ogcm_nf/g; }

        if($rcd =~ /\@CLDMICRO/) {$rcd=~ s/\@CLDMICRO/1MOMENT/g; } # wired

        if($rcd =~ /\@RESTART_BY_OBSERVER/) {$rcd=~ s/\@RESTART_BY_OBSERVER/YES/g; }

        if ( $dosppt ) {
           if($rcd =~ /\@AENS_DOSPPT/) {$rcd=~ s/\@AENS_DOSPPT/1/g; }
        } else {
           if($rcd =~ /\@AENS_DOSPPT/) {$rcd=~ s/\@AENS_DOSPPT/0/g; }
        }

        if($rcd =~ /\@LSM_CHOICE/) {$rcd=~ s/\@LSM_CHOICE/$lsmchoice/g; }
        if ( "$lndbcs" eq "Icarus-NLv3" ) {
           if($rcd =~ /\@LSM_PARMS/) {$rcd=~ s/\@LSM_PARMS/ /g; }
        } else {
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
  if ($doose) {
     $thisrc = "$mydir/HISTAOSE.rc.tmpl";
  }

     open(LUN,"$thisrc")  || die "Fail to open $thisrc $!\n";
     open(LUN2,">$tmprc") || die "Fail to open tmp.rc $!\n";

     # Change variables to the correct inputs
     #---------------------------------------
     while( defined($rcd = <LUN>) ) {
        chomp($rcd);
        if ( $hhis_im == $lhis_im ) {
           if($rcd =~ /\@HRESAENS/) {$rcd=~ s/\@HRESAENS/#/g; }
        } else {
           if($rcd =~ /\@HRESAENS/) {$rcd=~ s/\@HRESAENS/ /g; }
        }
        if ( $dosppt ) {
           if($rcd =~ /\@AENS_DOSPPT/) {$rcd=~ s/\@AENS_DOSPPT/ /g; }
        } else {
           if($rcd =~ /\@AENS_DOSPPT/) {$rcd=~ s/\@AENS_DOSPPT/#/g; }
        }
        if($rcd =~ /\@CHIS_IM/) {$rcd=~ s/\@CHIS_IM/$chis_im/g; }
        if($rcd =~ /\@CHIS_JM/) {$rcd=~ s/\@CHIS_JM/$chis_jm/g; }
        if($rcd =~ /\@DHIS_IM/) {$rcd=~ s/\@DHIS_IM/$dhis_im/g; }
        if($rcd =~ /\@DHIS_JM/) {$rcd=~ s/\@DHIS_JM/$dhis_jm/g; }
        if($rcd =~ /\@HHIS_IM/) {$rcd=~ s/\@HHIS_IM/$hhis_im/g; }
        if($rcd =~ /\@HHIS_JM/) {$rcd=~ s/\@HHIS_JM/$hhis_jm/g; }
        if($rcd =~ /\@LHIS_IM/) {$rcd=~ s/\@LHIS_IM/$lhis_im/g; }
        if($rcd =~ /\@LHIS_JM/) {$rcd=~ s/\@LHIS_JM/$lhis_jm/g; }
        if($rcd =~ /\@AGCM_LM/) {$rcd=~ s/\@AGCM_LM/$agcm_lm/g; }

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
  $nsig_ext = 18;
  if ( $obsv_lm > 72 & $obsv_lm <= 132 ) {
    $nsig_ext = 21;
  } elsif ( $obsv_lm > 132 ) {
    $nsig_ext = 27;
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
sub ed_conf_rc {

  my($mydir,$conffn) = @_;

  my($acq);

  $tmprc  = "$mydir/tmp.rc";
  $thisrc = "$mydir/$conffn";

     open(LUN,"$thisrc")  || die "Fail to open $thisrc $!\n";
     open(LUN2,">$tmprc") || die "Fail to open tmp.rc $!\n";

     # Change variables to the correct inputs
     #---------------------------------------
     while( defined($rcd = <LUN>) ) {
        chomp($rcd);
        if($rcd =~ /\@ACFTBIAS/)            {$rcd=~ s/\@ACFTBIAS/$setacftbc/g;  }
        if($rcd =~ /\@AGCM_CPUS/)           {$rcd=~ s/\@AGCM_CPUS/$agcm_cpus/g; }
        if($rcd =~ /\@AGCM_NCPUS_PER_NODE/) {$rcd=~ s/\@AGCM_NCPUS_PER_NODE/$agcm_ncpus_per_node/g; }
        if($rcd =~ /\@DORCORR/)             {$rcd=~ s/\@DORCORR/$dorcorr/g; }
        if($rcd =~ /\@MIAU_CPUS/)           {$rcd=~ s/\@MIAU_CPUS/$miau_cpus/g; }
        if($rcd =~ /\@OBSV_CPUS/)           {$rcd=~ s/\@OBSV_CPUS/$obsv_cpus/g; }
        if($rcd =~ /\@STAT_CPUS/)           {$rcd=~ s/\@STAT_CPUS/$stat_cpus/g; }
        if($rcd =~ /\@ENKF_CPUS/)           {$rcd=~ s/\@ENKF_CPUS/$enkf_cpus/g; }
        if($rcd =~ /\@NODENAME/)            {$rcd=~ s/\@NODENAME/$nodename/g; }
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

  if( $setacftbc ) {
    $mysetacftbc = "$ATMENS/RST/$expid.ana_acftbias_rst.%y4%m2%d2_%h2z.txt => $expid.ana.acftbias.%y4%m2%d2_%h2z.txt"; 
    chomp($mysetacftbc);
  } else {
    $mysetacftbc = "#$ATMENS/RST/$expid.ana_acftbias_rst.%y4%m2%d2_%h2z.txt => $expid.ana.acftbias.%y4%m2%d2_%h2z.txt"; 
  }
  if( $setradbc ) {
    $mysatbiaspc = "$ATMENS/RST/$expid.ana_satbiaspc_rst.%y4%m2%d2_%h2z.txt => $expid.ana.satbiaspc.%y4%m2%d2_%h2z.txt"; 
    chomp($mysatbiaspc);
  } else {
    $mysatbiaspc = "#$ATMENS/RST/$expid.ana_satbiaspc_rst.%y4%m2%d2_%h2z.txt => $expid.ana.satbiaspc.%y4%m2%d2_%h2z.txt"; 
  }

  $acq = "$mydir/satbias.acq";

 open(SCRIPT,">$acq") or
 die ">>> ERROR <<< cannot write $acq";
 print  SCRIPT <<"EOF";
#$ATMENS/central/$expid.ana.acftbias.%y4%m2%d2_%h2z.txt
#$ATMENS/central/$expid.ana.satbias.%y4%m2%d2_%h2z.txt
#$ATMENS/central/$expid.ana.satbang.%y4%m2%d2_%h2z.txt
#$ATMENS/RST/$expid.ana_satbang_rst.%y4%m2%d2_%h2z.txt => $expid.ana.satbang.%y4%m2%d2_%h2z.txt
$ATMENS/RST/$expid.ana_satbias_rst.%y4%m2%d2_%h2z.txt => $expid.ana.satbias.%y4%m2%d2_%h2z.txt
$mysatbiaspc
$mysetacftbc
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
sub ed_g5fvlay_rc {

  return 0 unless ( $cubed );

  my($mydir) = @_;

  my($frun, $ft, $rcd);
  my($g5fvlayrc);

  $g5fvlayrc = "fvcore_layout.rc";

  $ft   = "$mydir/tmp.rc";
  $fetc = "$FVROOT/etc/$g5fvlayrc";
  $frun = "$mydir/$g5fvlayrc";

  open(LUN,"$fetc") || die "Fail to open $g5fvlayrc: $!\n";
  open(LUN2,">$ft") || die "Fail to open tmp.rc: $!\n";

  # Change variables to the correct inputs
  # RT: Only hydrostatic option supported for now - will revise for non-hydro case later
  #---------------------------------------
  while( defined($rcd = <LUN>) ) {
     chomp($rcd);
     if($rcd =~ /\@FV_HYDRO/)  {$rcd=~ s/\@FV_HYDRO/hydrostatic = .T./g; }
     if($rcd =~ /\@FV_MAKENH/) {$rcd=~ s/\@FV_MAKENH/Make_NH = .F./g; }
     if($rcd =~ /\@FV_SATADJ/) {$rcd=~ s/\@FV_SATADJ/do_sat_adj  = .F./g; }
     if($rcd =~ /\@FV_ZTRACER/){$rcd=~ s/\@FV_ZTRACER/z_tracer = .T./g; }
     if($rcd =~ /\@FV_NWAT/)   {$rcd=~ s/\@FV_NWAT/ /g; }
     if ( $agcm_im == 720 ) {
       if($rcd =~ /\@FV_N_SPLIT/)   {$rcd=~ s/\@FV_N_SPLIT/n_split = 12/g; }
     } else {
       if($rcd =~ /\@FV_N_SPLIT/)   {$rcd=~ s/\@FV_N_SPLIT/ /g; }
     }
     if($rcd =~ /\@FV_SCHMIDT/)     {$rcd=~ s/\@FV_SCHMIDT/do_schmidt = .false./g; }
     if($rcd =~ /\@FV_STRETCH_FAC/) {$rcd=~ s/\@FV_STRETCH_FAC/stretch_fac = 1.0/g; }
     if($rcd =~ /\@FV_TARGET_LON/)  {$rcd=~ s/\@FV_TARGET_LON/target_lon = 0.0/g; }
     if($rcd =~ /\@FV_TARGET_LAT/)  {$rcd=~ s/\@FV_TARGET_LAT/target_lat = 0.0/g; }
     if($rcd =~ /\@GFDL_PROG_CCN/)  {$rcd=~ s/\@GFDL_PROG_CCN/prog_ccn = .true./g; }
     if($rcd =~ /\@GFDL_USE_CCN/)   {$rcd=~ s/\@GFDL_USE_CCN/use_ccn = .true./g; }

     print(LUN2 "$rcd\n");
  }

  close(LUN);
  close(LUN2);

  cp($ft, $frun);
  unlink $ft;

# if ( $coupled ) {
#    my @these = ("$mometc/g5aodas_input.nml","$frun");
#    my $target = "/tmp/${user}_$$.txt";
#    merge_txt(\@these,$target);
#    mv($target, $frun);
# }

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
                                     lndbcs
          
DESCRIPTION


     The following parameters are required 

     scheme   enkf or engsi
     expid    experiment name, e..g., u000_c72
     aim      number of x-grid points in Atmos GCM
     ajm      number of y-grid points in Atmos GCM
     ogrid    c, f, or C for low- or high-resolution Ocean GCM
     lndbcs   land BCS: Icarus_Updated or Icarus-NLv3


OPTIONS

     -atmens       location of ensemble members (default: \$FVHOME/atmens)
     -expdir       experiment location (default: /discover/nobackup/user)
     -fvhome       location of experiment home directory (default: \$expdir/\$expid)
     -acftbc BC    controls aircraft bias correction (0,1,2,or 3)
     -lsmcm        catchment option (only applicable when pointing to Icarus-NLv3)
     -nlevs  LEVS  number of atmospheric levels (default: 72)
     -nosppt       use to deactive SPPT scheme; use non-perturbed members
     -ose          set up ensemble as an OSE-type experiment (NOT ALL READY YET)
     -radbc        set up to run with Y. Zhu satellite bias correction
     -rcorr        when specified will apply channel correlations
     -vtxrlc       use vortex tracker and relocator
     -h            prints this usage notice

EXAMPLE COMMAND LINE

     setup_atmens.pl enkf u000_C72 90 540 C Icarus_Updated

NECESSARY ENVIRONMENT

OPTIONAL ENVIRONMENT

AUTHOR

     Ricardo Todling (Ricardo.Todling\@nasa.gov), NASA/GSFC/GMAO
     Last modified: 05Feb2020      by: R. Todling


EOF

  exit(1)

}
