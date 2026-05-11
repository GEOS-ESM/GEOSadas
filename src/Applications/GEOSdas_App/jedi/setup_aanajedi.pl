#!/usr/bin/env perl
# 
# setup_aanajedi - setup for an atmospheric JEDI analysis
#
#  20Apr2015 Todling  Initial code
#  01May2015 Todling  Add 4d-capability
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

#use lib ( "$FindBin::Bin", "$FVROOT/bin" );
use lib ( "$FindBin::Bin" );

$fvbin = $FindBin::Bin;  # absolute path of fvbin
my $scriptname = basename($0);

# Command line options

  GetOptions ( "gcmres=s",
               "archive=s",
               "cvbc=s",
               "ensrpy=s",
               "exprpy=s",
               "fvhome=s",
               "iodadir=s",
               "jedihome=s",
               "jediroot=s",
               "jedistatic=s",
               "nodename=s",
               "nogsi",
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
#    print "$0: you now must edit files under $JEDIHOME\n\n";
     exit(0);
  } else {
     print "$0: failed to setup resources for GEOS-JEDI\n\n";
     exit(1);
  }


#......................................................................

sub init {

   if ( $#ARGV  <  2 ) {
     print STDERR " Missing arguments; see usage:\n";
     usage();
   } else {              # required command line args
     $scheme      = $ARGV[0];
     $expid       = $ARGV[1];
     $hres        = $ARGV[2];
     $vres        = $ARGV[3];
   }

# process options

   $rc    = 0;

# allow for extra command line

   if ( $ENV{"FVHOME"} ) {
       $fvhome = $ENV{"FVHOME"};
   } else {
      if ( $opt_fvhome ) {
        $fvhome = $opt_fvhome;
      } else {
        die "Env Var FVHOME or arg -fvhome needed \n";
      }
   }

   $jediqos = "#"; 
   if ( $ENV{"GEOSJEDI_QOS"} ) {
      $jediqos = "#SBATCH --qos=$GEOSJEDI_QOS";
   }

   $jedipartition = "#"; 
   if ( $ENV{"GEOSJEDI_PARTITION"} ) {
      $jedipartition = "#SBATCH --qos=$GEOSJEDI_PARTITION";
   }

   if ( $opt_jedistatic ) {
        $jedistatic = $opt_jedistatic;
   } else {
        $jedistatic = "/discover/nobackup/projects/gmao/advda/SwellStaticFiles";
   }

   $gsi2ioda = 0;
   if ( $opt_iodadir ) {
     if ( $opt_iodadir eq "/dev/null" ) {
        $iodadir = "/dev/null";
        $jedi_obs_opt = 3; # convert ncdiag-to-ioda on the fly
        $gsi2ioda = 1;
     } else {
        $iodadir = $opt_iodadir;
        $jedi_obs_opt = 2; # ioda files provided by user
     }
   }

   if ( $opt_cvbc ) {
        $cvbc = $opt_cvbc;  # =1 cycle varbc 
        if ($cvbc > 1) {die "invalid entry, cvbc 0/1 only.\n"};
   } else {
        $cvbc = 0; # do not cycle JEDI varBC yet (JEDI not handling aircraft Variances on output yet)
   }

   if ( $opt_jediroot ) {
        $jediroot = $opt_jediroot;
   } else {
        $jediroot = "/discover/nobackup/projects/gmao/advda/swell/JediBundles/fv3_soca_SLES15_01152026/build-intel-release";
   }

   if ( $opt_archive ) {
      $archive = $opt_archive;
   } else {
      if ( $ENV{"ARCHIVE"} ) {
        $archive = $ENV{"ARCHIVE"};
      } else {
         die "Env Var ARCHIVE or arg -archive needed \n";
      }
   }

   # Ensemble replay
   $ensrpy = 'self';
   $exprpy = 'self';
   if ( $opt_ensrpy && $opt_exprpy ) {
      $ensrpy = $opt_ensrpy;
      $exprpy = $opt_exprpy;
   }

   $nlevs = 72;
   if ( $opt_nlevs ) {
      $nlevs = $opt_nlevs;
   }

   $nodename = "mil";
   if ( $opt_nodename ) {
      $nodename = $opt_nodename;
   }

   if ( $opt_jedihome ) {
        $JEDIHOME = $opt_jedihome;
   } else {
        $JEDIHOME = "$fvhome/run/jedi";
   }

   $nogsi = 0;
   if ( $opt_nogsi ) {
      $nogsi = 1;
   }

# Swell is wired for now
  $swell_install = "/gpfsm/dnb10/projects/p61/rtodling/JEDI1/2026/SWELL/Apr";

# other settings
   $jediinput = "$fvhome/fv3-jedi";

# determined whether cubed or not
  $agcm_lm = $nlevs;

# define layout depending on resolution

  if ( $nodename eq "hasw" ) { $ncpus_per_node = 24; }
  if ( $nodename eq "sky"  ) { $ncpus_per_node = 36; }
  if ( $nodename eq "cas"  ) { $ncpus_per_node = 46; }
  if ( $nodename eq "mil"  ) { $ncpus_per_node = 126; }

# identify 3D vs ens-4D schemes
  if ( $scheme eq "hyb4denvar" or $scheme eq "hyb4dcenvar" or $scheme eq "hyb4dcenvar_seq" ) {
    $hybridvar = 1;
  } else {
    $hybridvar = 0; 
  }

# Var run configuration parameters
  $agcm_im = $hres;
  $difxlayout = 4;
  $difylayout = 2;
  if ( $opt_gcmres ) { $agcm_im = $opt_gcmres };
  $agcm_jm = 6 * $agcm_im;
  $agcm_lm = $vres;
  $cres = $hres + 1;
  $i1res = $cres; # resolution of inner loop (only used for BUMP opt for now)
  if ( $cres == 721 ) {
     if ( $scheme eq "hyb4denvar" ) {
       $varxlayout = 16;
       $varylayout = 7;
       $difxlayout = 4;
       $difylayout = 4;
       $gsixlayout = 21;
       $gsiylayout = 32;
       $perhost_var = 12;
       $gsibec_lat = 361;
       $gsibec_lon = 576;
     } elsif ( $scheme eq "hyb4dcenvar" or $scheme eq "hyb4dcenvar_seq" ) {
       $i1res = $hres / 2 + 1; # resolution of inner loop (only used for BUMP opt for now)
       die "You are pushing the envelop, no settings for this yet, aborting ... \n";
     } else {
       $varxlayout = 10;
       $varylayout = 10;
       $gsixlayout = 10;
       $gsiylayout = 6 * $gsixlayout;
       $perhost_var = 16;
       $gsibec_lat = 721;
       $gsibec_lon = 1152;
     }
  } elsif ( $cres == 361 ) {
     if ( $scheme eq "hyb4denvar" ) {
       $varxlayout = 16;
       $varylayout = 7;
       $gsixlayout = 21;
       $gsiylayout = 32;
       $perhost_var = 12;
     } elsif ( $scheme eq "hyb4dcenvar_seq" ) {
       $varxlayout = 16;
       $varylayout = 7;
       $gsixlayout = 21;
       $gsiylayout = 32;
       $perhost_var = 12;
       $i1res = $hres / 2 + 1; # resolution of inner loop (only used for BUMP opt for now)
     } elsif ( $scheme eq "hyb4dcenvar" ) {
       $varxlayout = 4;
       $varylayout = 4;
       $gsixlayout = 6;
       $gsiylayout = 16;
       $perhost_var = 12;
       $i1res = $hres / 2 + 1; # resolution of inner loop (only used for BUMP opt for now)
     } elsif ( $scheme eq "hyb3dcenvar" ) {
       $varxlayout = 10;
       $varylayout = 10;
       $gsixlayout = 10;
       $gsiylayout = 6 * $gsixlayout;
       $perhost_var = 16;
       $i1res = $hres / 2 + 1; # resolution of inner loop (only used for BUMP opt for now)
     } else {
       $varxlayout = 10;
       $varylayout = 10;
       $gsixlayout = 10;
       $gsiylayout = 6 * $gsixlayout;
       $perhost_var = 16;
     }
     $gsibec_lat = 361;
     $gsibec_lon = 576;
  } elsif ( $cres == 181 ) {
     if ( $scheme eq "hyb4denvar" ) {
       $varxlayout = 16;
       $varylayout = 7;
       $gsixlayout = 21;
       $gsiylayout = 32;
       $perhost_var = 12;
#      $gsibec_lat = 181;
#      $gsibec_lon = 288;
       $gsibec_lat = 361;
       $gsibec_lon = 576;
     } elsif ( $scheme eq "hyb4dcenvar_seq" ) {
       $varxlayout = 16;
       $varylayout = 7;
       $gsixlayout = 21;
       $gsiylayout = 32;
       $perhost_var = 12;
       $gsibec_lat = 361;
       $gsibec_lon = 576;
       $i1res = $hres / 2 + 1; # resolution of inner loop (only used for BUMP opt for now)
     } elsif ( $scheme eq "hyb4dcenvar" ) {
       $varxlayout = 4;
       $varylayout = 4;
       $gsixlayout = 6;
       $gsiylayout = 16;
       $perhost_var = 12;
       $gsibec_lat = 361;
       $gsibec_lon = 576;
       $i1res = $hres / 2 + 1; # resolution of inner loop (only used for BUMP opt for now)
     } elsif ( $scheme eq "hyb3dcenvar" ) {
       $varxlayout = 8;
       $varylayout = 8;
       $gsixlayout = 8;
       $gsiylayout = 6 * $gsixlayout;
       $perhost_var = 16;
       $gsibec_lat = 361;
       $gsibec_lon = 576;
       $i1res = $hres / 2 + 1; # resolution of inner loop (only used for BUMP opt for now)
     } else {
       $varxlayout = 8;
       $varylayout = 8;
       $gsixlayout = 8;
       $gsiylayout = 6 * $gsixlayout;
       $perhost_var = 16;
       $gsibec_lat = 361;
       $gsibec_lon = 576;
     }
  } elsif ( $cres == 91 ) {
     if ( $scheme eq "hyb4denvar" ) {
       die "Untested configuration, aborting ... ";
       $varxlayout = 8;
       $varylayout = 8;
       $gsixlayout = 16;
       $gsiylayout = 24;
       $perhost_var = 16;
     } elsif ( $scheme eq "hyb4dcenvar_seq" ) {
       die "Untested configuration, aborting ... ";
       $varxlayout = 12;
       $varylayout = 2;
       $gsixlayout = 8;
       $gsiylayout = 18;
       $perhost_var = 12;
     } elsif ( $scheme eq "hyb3dcenvar" ) {
       die "Untested configuration, aborting ... ";
       $varxlayout = 6;
       $varylayout = 6;
       $gsixlayout = 6;
       $gsiylayout = 6 * $gsixlayout;
       $perhost_var = 12;
     } else {
       $varxlayout = 6;
       $varylayout = 6;
       $gsixlayout = 6;
       $gsiylayout = 6 * $gsixlayout;
       $perhost_var = 12;
     }
     $gsibec_lat =  91;
     $gsibec_lon = 144;
  } else {
     die "Unknown resolution settings, aborting ... \n";
  }
  $gsibecres = "l${nlevs}x${gsibec_lon}y${gsibec_lat}";
  $ncpus_var = $varxlayout * $varylayout * 6;
  if ( $scheme eq "hyb4dcenvar" ) {$ncpus_var = $ncpus_var * 7}; # wired to hourly background
  $diffntasks = $difxlayout * $difylayout * 6;

# mkiau pe-settings
  $mkiau_nx = 2;
  $mkiau_ny = 12;
  if ( $agcm_im == 720 ) {
     $mkiau_nx = 4;
     $mkiau_ny = 24;
  }

# build internal variables

  @rc2conf   = qw ( diag2ioda.yaml
                    diffstates_geos.yaml
                    mkiau.rc.tenv
                    obsop_name_map.yaml );

  @rc2jedi   = qw ( JEDIanaConfig.csh
                    SWELLConfig.csh
                    jedi_acquire_bkg.j
                    jedi_acquire_ebkg.j
                    jedi_acquire_ioda.j
                    jedi_acquire_vbc.j
                    jedi_diffstates.j
                    jedi_run_var.j
                    ut_jedi.j
                  );

  @rc2jediobs = qw ( 0observations.yaml
                     aircraft_temperature.yaml
                     aircraft_wind.yaml
                     airs_aqua.yaml
                     amsr2_gcom-w1.yaml
                     amsua_aqua.yaml
                     amsua_metop-b.yaml
                     amsua_metop-c.yaml
                     amsua_n15.yaml
                     amsua_n19.yaml
                     atms_n20.yaml
                     atms_npp.yaml
                     avhrr3_metop-b.yaml
                     avhrr3_metop-c.yaml
                     avhrr3_n19.yaml
                     cris-fsr_n20.yaml
                     cris-fsr_npp.yaml
                     gmi_gpm.yaml
                     gps.yaml
                     iasi_metop-b.yaml
                     iasi_metop-c.yaml
                     mhs_metop-b.yaml
                     mhs_metop-c.yaml
                     mhs_n19.yaml
                     mls55_aura.yaml
                     omi_aura.yaml
                     ompslpnc_n21.yaml
                     ompslpnc_npp.yaml
                     ompsnm_npp.yaml
                     pibal.yaml
                     satwind.yaml
                     scatwind.yaml
                     sfcship.yaml
                     sfc.yaml
                     sondes.yaml
                     ssmis_f17.yaml
                   );

}
#......................................................................

sub install {

if ( ! -d $JEDIHOME ) {
   $rc = system("/bin/mkdir -p $JEDIHOME" );
}
if ( ! -d "$JEDIHOME/Config" ) {
   $rc = system("/bin/mkdir -p $JEDIHOME/Config" );
}
if ( ! -d "$JEDIHOME/Config/obs" ) {
   $rc = system("/bin/mkdir -p $JEDIHOME/Config/obs" );
}
# transfer resource files to proper location
# transfer resource files to proper location
# TBD: at this time, no editing is done of the resource
#      user must edit files as needed
foreach $fn ( @rc2jedi ) {
  chomp($fn);
  if ( $fn eq "jedi_run_var.j" ) {
    cp("$FVROOT/etc/jedi/$fn","$JEDIHOME/_${fn}"); # this is placed in JEDIHOME for convenience (when debug needed)
  } else {
    cp("$FVROOT/etc/jedi/$fn","$JEDIHOME/$fn");
  }
}

# Copy scheme yaml to proper location
foreach $fn ( @rc2conf ) {
  chomp($fn);
  cp("$FVROOT/etc/jedi/$fn","$JEDIHOME/Config/$fn");
}
cp("$FVROOT/etc/jedi/geos_${scheme}.yaml","$JEDIHOME/Config/geosvar.yaml");
if ( ! -e "$JEDIHOME/Config/geosvar.yaml" ) {
   die "File $JEDIHOME/Config/geosvar.yaml not found \n";
}

# Copy obs yamls to experiment config location
foreach $fn ( @rc2jediobs ) {
  chomp($fn);
  cp("$FVROOT/etc/jedi/obs/$fn","$JEDIHOME/Config/obs/$fn");
}

cp("$FVROOT/etc/jedi/geos_${scheme}.yaml","$JEDIHOME/Config/geosvar.yaml");

# take of resolution and layout
ed_conf_rc ("$JEDIHOME","JEDIanaConfig.csh");
ed_var_yaml ("$JEDIHOME/Config","geosvar.yaml");
if ( $hybridvar ) {
  ed_var_yaml ("$JEDIHOME/Config","diffstates_geos.yaml");
}
ed_mkiau_rc ("$JEDIHOME/Config","mkiau.rc.tenv");

# take care of satbias acq
ed_jedibkg_acq   ("$JEDIHOME/Config");
ed_jediebkg_acq  ("$JEDIHOME/Config",$ensrpy,$exprpy);
ed_jediebkgx_acq ("$JEDIHOME/Config",$ensrpy,$exprpy);
ed_jediioda_acq  ("$JEDIHOME/Config");
ed_jedivbc_acq   ("$JEDIHOME/Config");
ed_diffstate_job ("$JEDIHOME");

set_jedi_static("$jediroot","$jediinput",$cres,$i1res,$gsibecres);

# edit main DAS existing settings when GSI is bypassed
ed_rst4fcst_acq("$FVHOME/fcst/","$scheme");
ed_4dfcst03_acq("$FVHOME/fcst/","$scheme");

}
#......................................................................
sub set_jedi_static{

my($myroot,$mydir,$mycres,$myi1cres,$myllres) = @_;

# create JEDI work area and make sure .no_archiving exists in JEDI
if ( ! -d "$mydir" ) {
   $rc = system("/bin/mkdir -p $mydir" );
}
$cmd = "touch $mydir/.no_archiving";
$rc = system($cmd);

# create directory of static JEDI files to be seen by experiment
@static_dirs = qw (bkg bump fv3files  gsibec  rcov);
foreach $dir ( @static_dirs ) {
   $rc = system("/bin/mkdir -p $mydir/$dir" );
}

# bkg ...
Assignfn("$jedistatic/jedi/interfaces/geos_atmosphere/GEOS_CRTM_Surface/geos.crtmsrf.$mycres.nc4","$mydir/bkg/geos.crtmsrf.$mycres.nc4");

# gsibec ...
if ( $scheme eq "hyb4denvar" ) {

  Assignfn("$jedistatic/jedi/interfaces/geos_atmosphere/gsibec/1.0.2/hyb4d_gsibec_configuration_$myllres.nml","$mydir/gsibec/gsibec_configuration_$myllres.nml");

} elsif ( $scheme eq "hyb3denvar" ) {

  Assignfn("$jedistatic/jedi/interfaces/geos_atmosphere/gsibec/1.0.2/hyb3d_gsibec_configuration_$myllres.nml","$mydir/gsibec/gsibec_configuration_$myllres.nml");

} elsif ( $scheme eq "hyb3dcenvar" or $scheme eq "hyb4dcenvar" or scheme eq "hyb4dcenvar_seq" ) {

  Assignfn("$jedistatic/jedi/interfaces/geos_atmosphere/gsibec/1.0.2/cli_gsibec_configuration_$myllres.nml","$mydir/gsibec/gsibec_configuration_$myllres.nml");
  Assignfn("$jedistatic/jedi/interfaces/geos_atmosphere/bump/betac.c${myi1cres}l${nlevs}.nc4","$mydir/bump/betac.c${myi1cres}l${nlevs}.nc4");
  Assignfn("$jedistatic/jedi/interfaces/geos_atmosphere/bump/betae.c${myi1cres}l${nlevs}.nc4","$mydir/bump/betae.c${myi1cres}l${nlevs}.nc4");

} else {

  Assignfn("$jedistatic/jedi/interfaces/geos_atmosphere/gsibec/1.0.2/cli_gsibec_configuration_$myllres.nml","$mydir/gsibec/gsibec_configuration_$myllres.nml");

}
Assignfn("$jedistatic/jedi/interfaces/geos_atmosphere/gsibec/1.0.2/gsi-coeffs-gmao-global-$myllres.nc4","$mydir/gsibec/gsibec_coefficients_$myllres.nc4");

# Rcov ...
$files_tmp = `sh -c "ls $jedistatic/jedi/interfaces/geos_atmosphere/rcov/1.0.0/* 2>/dev/null"`;
chomp($files_tmp);
@files = split(/\n/,$files_tmp);
foreach $fullpathfn ( @files ) {
  my $fn = basename($fullpathfn);
  Assignfn("$fullpathfn","$mydir/rcov/$fn");
}

# fv3files
@build_dirs = qw (fv3files);
foreach $dir_in_build ( @build_dirs ) {
  $files_tmp = `sh -c "ls $jedistatic/jedi/interfaces/geos_atmosphere/$dir_in_build/* 2>/dev/null"`;
  chomp($files_tmp);
  @files = split(/\n/,$files_tmp);
  foreach $fullpathfn ( @files ) {
    my $fn = basename($fullpathfn);
    Assignfn("$fullpathfn","$mydir/$dir_in_build/$fn");
  }
}


}
#......................................................................
sub ed_mkiau_rc {

  my($mydir,$config) = @_;

  my($acq);

  $tmprc  = "$mydir/tmp.rc";
  $thisrc = "$mydir/$config";

     open(LUN,"$thisrc")  || die "Fail to open $thisrc $!\n";
     open(LUN2,">$tmprc") || die "Fail to open tmp.rc $!\n";

     # Change variables to the correct inputs
     #---------------------------------------
     while( defined($rcd = <LUN>) ) {
        chomp($rcd);
        if($rcd =~ /\@NX/) {$rcd=~ s/\@NX/$mkiau_nx/g; }
        if($rcd =~ /\@NY/) {$rcd=~ s/\@NY/$mkiau_ny/g; }
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
sub ed_conf_rc {

  my($mydir,$conffn) = @_;

  my($acq);

  $jedihyb = 0;
  $jediinc = 0;
  if ( $scheme eq "hyb4denvar" ) { 
     $jedihyb = 1;  # handle lat-lon ensemble
     $jediinc = 1;
  }
  if ( $scheme eq "hyb3denvar" ) { 
     $jedihyb = 1;  # handle lat-lon ensemble
  }
  if ( $scheme eq "hyb4dcenvar" or $scheme eq "hyb4dcenvar_seq" ) { 
     $jedihyb = 2;  # handle cubed ensemble
     $jediinc = 1;
  }
  if ( $scheme eq "hyb3dcenvar" ) { 
     $jedihyb = 2;  # handle cubed ensemble
  }

  $tmprc  = "$mydir/tmp.rc";
  $thisrc = "$mydir/$conffn";

     open(LUN,"$thisrc")  || die "Fail to open $thisrc $!\n";
     open(LUN2,">$tmprc") || die "Fail to open tmp.rc $!\n";

     # Change variables to the correct inputs
     #---------------------------------------
     while( defined($rcd = <LUN>) ) {
        chomp($rcd);
        if($rcd =~ /\@GEOSJEDI_QOS/)       {$rcd=~ s/\@GEOSJEDI_QOS/$jediqos/g;  }
        if($rcd =~ /\@GEOSJEDI_PARTITION/) {$rcd=~ s/\@GEOSJEDI_PARTITION/$jedipartition/g;  }

        if($rcd =~ /\@JEDI_FEEDBACK_VARBC/) {$rcd=~ s/\@JEDI_FEEDBACK_VARBC/$cvbc/g;  }
        if($rcd =~ /\@JEDI_HYBRID/)         {$rcd=~ s/\@JEDI_HYBRID/$jedihyb/g;  }
        if($rcd =~ /\@JEDI_INPUT/)          {$rcd=~ s/\@JEDI_INPUT/$jediinput/g;  }
        if($rcd =~ /\@JEDI_OBS_OPT/)        {$rcd=~ s/\@JEDI_OBS_OPT/$jedi_obs_opt/g;  }
        if($rcd =~ /\@JEDI_GSI2IODA/)       {$rcd=~ s/\@JEDI_GSI2IODA/$gsi2ioda/g;  }
        if($rcd =~ /\@JEDI_IAU_OVERWRITE/)  {$rcd=~ s/\@JEDI_IAU_OVERWRITE/$nogsi/g;  }
        if($rcd =~ /\@JEDI_ROOT/)           {$rcd=~ s/\@JEDI_ROOT/$jediroot/g;  }
        if($rcd =~ /\@JEDI_RUN_GETINC/)     {$rcd=~ s/\@JEDI_RUN_GETINC/$jediinc/g;  }
        if($rcd =~ /\@JEDI_STATIC_FILES/)   {$rcd=~ s/\@JEDI_STATIC_FILES/$jedistatic/g;  }
        if($rcd =~ /\@JEDI_DIF_NCPUS/)      {$rcd=~ s/\@JEDI_DIF_NCPUS/$diffntasks/g;  }
        if($rcd =~ /\@JEDI_VAR_NCPUS/)      {$rcd=~ s/\@JEDI_VAR_NCPUS/$ncpus_var/g;  }
        if($rcd =~ /\@JEDI_VAR_PERHOST/)    {$rcd=~ s/\@JEDI_VAR_PERHOST/$perhost_var/g;  }
        if($rcd =~ /\@OFFLIODADIR/)         {$rcd=~ s/\@OFFLIODADIR/$iodadir/g;  }

        if($rcd =~ /\@SWELL_INSTALL/)       {$rcd=~ s/\@SWELL_INSTALL/$swell_install/g;  }

        if($rcd =~ /\@NODENAME/)            {$rcd=~ s/\@NODENAME/$nodename/g; }
        print(LUN2 "$rcd\n");
     }

     close(LUN);
     close(LUN2);
     cp($tmprc, $thisrc);
     unlink $tmprc;

}
#......................................................................
sub ed_jedibkg_acq {

 my($mydir) = @_;
 my($acq);

 $acq = "$mydir/jedi_bkg.acq";
 open(SCRIPT,">$acq") or
 die ">>> ERROR <<< cannot write $acq";
 print  SCRIPT <<"EOF";
$archive/$expid/rs/Y%y4/M%m2/$expid.bkgcrst.%y4%m2%d2_%h2z.tar
EOF
}
#......................................................................
sub ed_jediebkg_acq {

 my($mydir,$my_rdir,$my_rexp) = @_;
 my($acq,$this,$this_dir,$this_exp);
 
 if ( $my_rdir eq "self" ) {
    $this_dir = "$archive/$expid/atmens/Y%y4/M%m2/";
    $this_exp = "$expid";
    
 } else {
    $this_dir = "$my_rdir";
    $this_exp = "$my_rexp";
    $this = "$this_dir/atmens/Y%y4/M%m2/$this_exp.atmens_ebkg.%y4%m2%d2_%h2z.tar => $expid.atmens_ebkg.%y4%m2%d2_%h2z.tar";
 }

 $acq = "$mydir/jedi_ebkg.acq";
 open(SCRIPT,">$acq") or
 die ">>> ERROR <<< cannot write $acq";
 print  SCRIPT <<"EOF";
$this
EOF
}
#......................................................................
sub ed_jediebkgx_acq {

 my($mydir,$my_rdir,$my_rexp) = @_;
 my($acq,$this,$this_dir,$this_exp);

 if ( $my_rdir eq "self" ) {
    $this_dir = "$archive/$expid/atmens/Y%y4/M%m2/";
    $this_exp = "$expid";
    
 } else {
    $this_dir = "$my_rdir";
    $this_exp = "$my_rexp";
    $this = "$this_dir/atmens/Y%y4/M%m2/$this_exp.atmens_ebkgx.%y4%m2%d2_%h2z.tar => $expid.atmens_ebkgx.%y4%m2%d2_%h2z.tar";
 }

 $acq = "$mydir/jedi_ebkgx.acq";

 open(SCRIPT,">$acq") or
 die ">>> ERROR <<< cannot write $acq";
 print  SCRIPT <<"EOF";
$this
EOF
}
#......................................................................
sub ed_jediioda_acq {

  my($mydir) = @_;

  my($acq);

  $acq = "$mydir/jedi_ioda.acq";

 open(SCRIPT,">$acq") or
 die ">>> ERROR <<< cannot write $acq";
 print  SCRIPT <<"EOF";
$archive/$expid/jedi/obs/Y%y4/M%m2/$expid.jedi_ioda.%y4%m2%d2_%h2z.tar
EOF
}
#......................................................................
sub ed_jedivbc_acq {

  my($mydir) = @_;

  my($acq);

  $acq = "$mydir/jedi_vbc.acq";

 open(SCRIPT,">$acq") or
 die ">>> ERROR <<< cannot write $acq";
 print  SCRIPT <<"EOF";
$archive/$expid/jedi/obs/Y%y4/M%m2/$expid.jedi_vbc.%y4%m2%d2_%h2z.tar
EOF
}
#......................................................................
sub ed_diffstate_job {

  my($mydir) = @_;

  my $tmprc  = "$mydir/tmp.rc";
  my $thisrc = "$mydir/jedi_diffstates.j";
    
  open(LUN,"$thisrc")  || die "Fail to open $thisrc $!\n";
  open(LUN2,">$tmprc") || die "Fail to open tmp.rc $!\n";

  # Change variables to the correct inputs
  #---------------------------------------
  while( defined($rcd = <LUN>) ) {
     chomp($rcd);
     if($rcd =~ /\@GEOSJEDI_QOS/)       {$rcd=~ s/\@GEOSJEDI_QOS/$jediqos/g;  }
     if($rcd =~ /\@GEOSJEDI_PARTITION/) {$rcd=~ s/\@GEOSJEDI_PARTITION/$jedipartition/g;  }
     print(LUN2 "$rcd\n");
  }
 
  close(LUN);
  close(LUN2);
  cp($tmprc, $thisrc);
  unlink $tmprc;

}
#......................................................................
sub ed_var_yaml {

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
        if($rcd =~ /\@AGCM_IM/)             {$rcd=~ s/\@AGCM_IM/$agcm_im/g;  }
        if($rcd =~ /\@JEDI_BKG_HRES/)       {$rcd=~ s/\@JEDI_BKG_HRES/$cres/g;  }
        if($rcd =~ /\@JEDI_INC_1RES/)       {$rcd=~ s/\@JEDI_INC_1RES/$i1res/g;  }
        if($rcd =~ /\@JEDI_BKG_VRES/)       {$rcd=~ s/\@JEDI_BKG_VRES/$vres/g;  }
        if($rcd =~ /\@JEDI_GSIBEC_NLAT/)    {$rcd=~ s/\@JEDI_GSIBEC_NLAT/$gsibec_lat/g;  }
        if($rcd =~ /\@JEDI_GSIBEC_NLON/)    {$rcd=~ s/\@JEDI_GSIBEC_NLON/$gsibec_lon/g;  }
        if($rcd =~ /\@JEDI_GSIBEC_NLEV/)    {$rcd=~ s/\@JEDI_GSIBEC_NLEV/$vres/g;  }
        if($rcd =~ /\@JEDI_DIF_NCPUS/)      {$rcd=~ s/\@JEDI_DIF_NCPUS/$diffntasks/g;  }
        if($rcd =~ /\@JEDI_DIF_XLAYOUT/)    {$rcd=~ s/\@JEDI_DIF_XLAYOUT/$difxlayout/g;  }
        if($rcd =~ /\@JEDI_DIF_YLAYOUT/)    {$rcd=~ s/\@JEDI_DIF_YLAYOUT/$difylayout/g;  }
        if($rcd =~ /\@JEDI_VAR_XLAYOUT/)    {$rcd=~ s/\@JEDI_VAR_XLAYOUT/$varxlayout/g;  }
        if($rcd =~ /\@JEDI_VAR_YLAYOUT/)    {$rcd=~ s/\@JEDI_VAR_YLAYOUT/$varylayout/g;  }
        if($rcd =~ /\@JEDI_VAR_GSIXLAYOUT/) {$rcd=~ s/\@JEDI_VAR_GSIXLAYOUT/$gsixlayout/g;  }
        if($rcd =~ /\@JEDI_VAR_GSIYLAYOUT/) {$rcd=~ s/\@JEDI_VAR_GSIYLAYOUT/$gsiylayout/g;  }

        print(LUN2 "$rcd\n");
     }

     close(LUN);
     close(LUN2);
     cp($tmprc, $thisrc);
     unlink $tmprc;

}
#......................................................................
sub ed_rst4fcst_acq {

  return 0 unless ( $nogsi );

  my($mydir,$scheme) = @_;

  my($frun, $ft, $acq);

  $acq = "$fvhome/$mydir/rst4fcst.acq";

  open(SCRIPT,">$acq") or
  die ">>> ERROR <<< cannot write $acq";
  print  SCRIPT <<"EOF";
$archive/$expid/rs/Y%y4/M%m2/$expid.rst.%y4%m2%d2_%h2z.tar
EOF
if ( ! $hybridvar ) {
 print  SCRIPT <<"EOF";
$archive/$expid/jedi/rs/Y%y4/M%m2/$expid.jedi_agcmrst.%y4%m2%d2_%h2z.tar
EOF
}
}
#......................................................................
sub ed_4dfcst03_acq {

  return 0 unless ( $nogsi );

  my($mydir,$scheme) = @_;

  if ( ! $hybridvar ) { return 0 };

  my($frun, $ft, $acq);

  $acq = "$fvhome/$mydir/fcst03.acq";
  open(SCRIPT,">$acq") or
  die ">>> ERROR <<< cannot write $acq";
  print  SCRIPT <<"EOF";
$archive/$expid/jedi/rs/Y%y4/M%m2/$expid.jedi_agcmrst.%y4%m2%d2_%h2z.tar => $expid.agcmrst.%y4%m2%d2_%h2z.tar
EOF
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

#......................................................................

sub usage {

   print <<"EOF";

NAME
     setup_aanajedi.pl - setup resources to allow running JEDI analysis in GEOS ADAS
          
SYNOPSIS

     setup_aanajedi.pl [...options...] scheme
                                       expid
                                       hres
                                       vres
          
DESCRIPTION


     The following parameters are required 

     scheme   3dvar       - 3D variational analysis
              3dfgat      - 3D first guess at appropriate time
              hyb3dcenvar - hybrid 3d-VAR using cubed ensemble (BUMP)
              hyb4dcenvar - hybrid 4d-En-Var using cubed ensemble (BUMP)
              hyb4dcenvar_seq - hybrid 4d-En-Var using cubed ensemble (BUMP) - sequential handling of window
              hyb3denvar  - hybrid 3d-En-Var using lat-lon ensemble (GSIBEC)
              hyb4denvar  - hybrid 4d-En-Var using lat-lon ensemble (GSIBEC)
     expid    experiment name, e.g., u000_c72
     hres     cubed horizontal var resolution, e.g., 90
     vres     vertical resolution, e.g., 72 (default)


OPTIONS

     -archive      location of archive (when bkg, others come from; default: /archive/u/\$user)
     -cvbc         cycle JEDI varBC, 0/1 (default: 1, i.e., cycle)
     -gcmres       specify resolution of underying AGCM (default: hres in arg list)
     -fvhome       location of experiment home directory (default: \$expdir/\$expid)
     -jedihome     location of ensemble members (default: \$FVHOME/run/jedi)
     -jediroot     location of JEDI build directory (default: /discover/nobackup/projects/gmao/advda/swell/JediBundles/fv3_soca_SLES15/build-intel-release)
     -jedistatic   location of JEDI static files (default: /discover/nobackup/projects/gmao/advda/SwellStaticFiles)
     -iodadir      location of pre-existing IODA files (default: /dev/null, ie, run ncdiag2ioda)
     -h            prints this usage notice

EXAMPLE COMMAND LINE

     setup_aanajedi.pl 3dfgat u000_C72 90 72

NECESSARY ENVIRONMENT

OPTIONAL ENVIRONMENT

      ARCHIVE            can be define in env or arg list
      FVHOME             can be define in env or arg list
      GEOSJEDI_QOS       can be used to defined slurm qos
      GEOSJEDI_PARTITION can be used to defined slurm partition

AUTHOR

     Ricardo Todling (Ricardo.Todling\@nasa.gov), NASA/GSFC/GMAO
     Last modified: 31May2025                     by: R. Todling


EOF

  exit(1)

}
