#!/usr/bin/env perl
# 
# setup_aanajedi - setup for an atmospheric JEDI analysis
#
#  20Apr2015 Todling  Initial code
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

  GetOptions ( "archive=s",
               "cvbc=s",
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
     $resolution  = $ARGV[2];
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

   if ( $opt_jedistatic ) {
        $jedistatic = $opt_jedistatic;
   } else {
        $jedistatic = "/discover/nobackup/projects/gmao/advda/SwellStaticFiles";
   }

   if ( $opt_iodadir ) {
        $iodadir = $opt_iodadir;
        $jedi_obs_opt = 2; # ioda files provided by user
   } else {
        $iodadir = "/dev/null";
        $jedi_obs_opt = 3; # convert ncdiag-to-ioda on the fly
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
        $jediroot = "/discover/nobackup/projects/gmao/advda/swell/JediBundles/fv3_soca_SLES15_02062025/build-intel-release";
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

# other settings
   $jediinput = "$fvhome/fv3-jedi";

# determined whether cubed or not
  $agcm_im = $aim;
  $agcm_jm = $ajm;
  $agcm_lm = $nlevs;

# define layout depending on resolution

  if ( $nodename eq "hasw" ) { $ncpus_per_node = 24; }
  if ( $nodename eq "sky"  ) { $ncpus_per_node = 36; }
  if ( $nodename eq "cas"  ) { $ncpus_per_node = 46; }
  if ( $nodename eq "mil"  ) { $ncpus_per_node = 126; }


# build internal variables

  @rc2conf   = qw ( mkiau.rc.tenv
                    obsop_name_map.yaml );

  @rc2jedi   = qw ( JEDIanaConfig.csh
                    SWELLConfig.csh
                    jedi_acquire_bkg.j
                    jedi_acquire_ioda.j
                    jedi_acquire_vbc.j
                    jedi_run_var.j
                    ut_jedi.j
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

# take of resolution and layout
ed_conf_rc ("$JEDIHOME","JEDIanaConfig.csh");
ed_var_yaml ("$JEDIHOME/Config","geosvar.yaml");

# take care of satbias acq
ed_jedibkg_acq   ("$JEDIHOME/Config");
ed_jediebkg_acq  ("$JEDIHOME/Config");
ed_jediebkgx_acq ("$JEDIHOME/Config");
ed_jediioda_acq  ("$JEDIHOME/Config");
ed_jedivbc_acq   ("$JEDIHOME/Config");

set_jedi_static("$jediroot","$jediinput",$resolution);

# edit main DAS existing settings when GSI is bypassed
ed_rst4fcst_acq("$FVHOME/fcst/","$scheme");
ed_4dfcst03_acq("$FVHOME/fcst/","$scheme");

}
#......................................................................
sub set_jedi_static{

my($myroot,$mydir,$myres) = @_;

# create JEDI work area and make sure .no_archiving exists in JEDI
if ( ! -d "$mydir" ) {
   $rc = system("/bin/mkdir -p $mydir" );
}
$cmd = "touch $mydir/.no_archiving";
$rc = system($cmd);

# create directory of static JEDI files to be seen by experiment
@static_dirs = qw (bkg  fieldmetadata  fv3files  gsibec  rcov);
foreach $dir ( @static_dirs ) {
   $rc = system("/bin/mkdir -p $mydir/$dir" );
}

# bkg ...
$res = $myres + 1;
Assignfn("$jedistatic/jedi/interfaces/geos_atmosphere/GEOS_CRTM_Surface/geos.crtmsrf.$res.nc4","$mydir/bkg/geos.crtmsrf.$res.nc4");

# gsibec ...
if ( $scheme == "hyb4denvar" ) {
  Assignfn("$jedistatic/jedi/interfaces/geos_atmosphere/gsibec/hyb_gsibec_configuration_c$res.nml","$mydir/gsibec/hyb_gsibec_configuration_c$res.nml");
} else {
  Assignfn("$jedistatic/jedi/interfaces/geos_atmosphere/gsibec/cli_gsibec_configuration_c$res.nml","$mydir/gsibec/cli_gsibec_configuration_c$res.nml");
}
Assignfn("$jedistatic/jedi/interfaces/geos_atmosphere/gsibec/gsibec_coefficients_c$res.nc4","$mydir/gsibec/gsibec_coefficients_c$res.nc4");

# Rcov ...
$files_tmp = `sh -c "ls $jedistatic/jedi/interfaces/geos_atmosphere/rcov/1.0.0/* 2>/dev/null"`;
chomp($files_tmp);
@files = split(/\n/,$files_tmp);
foreach $fullpathfn ( @files ) {
  my $fn = basename($fullpathfn);
  Assignfn("$fullpathfn","$mydir/rcov/$fn");
}

# fieldmetadata & fieldset
@build_dirs = qw (fieldmetadata fv3files);
foreach $dir_in_build ( @build_dirs ) {
  $files_tmp = `sh -c "ls $myroot/fv3-jedi/test/Data/$dir_in_build/* 2>/dev/null"`;
  chomp($files_tmp);
  @files = split(/\n/,$files_tmp);
  foreach $fullpathfn ( @files ) {
    my $fn = basename($fullpathfn);
    Assignfn("$fullpathfn","$mydir/$dir_in_build/$fn");
  }
}


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
        if($rcd =~ /\@JEDI_FEEDBACK_VARBC/) {$rcd=~ s/\@JEDI_FEEDBACK_VARBC/$cvbc/g;  }
        if($rcd =~ /\@JEDI_INPUT/)          {$rcd=~ s/\@JEDI_INPUT/$jediinput/g;  }
        if($rcd =~ /\@JEDI_OBS_OPT/)        {$rcd=~ s/\@JEDI_OBS_OPT/$jedi_obs_opt/g;  }
        if($rcd =~ /\@JEDI_ROOT/)           {$rcd=~ s/\@JEDI_ROOT/$jediroot/g;  }
        if($rcd =~ /\@JEDI_STATIC_FILES/)   {$rcd=~ s/\@JEDI_STATIC_FILES/$jedistatic/g;  }
        if($rcd =~ /\@JEDI_VAR_PERHOST/)    {$rcd=~ s/\@JEDI_VAR_PERHOST/$perhost_var/g;  }
        if($rcd =~ /\@OFFLIODADIR/)         {$rcd=~ s/\@OFFLIODADIR/$iodadir/g;  }

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

 my($mydir) = @_;
 my($acq);

 $acq = "$mydir/jedi_ebkg.acq";
 open(SCRIPT,">$acq") or
 die ">>> ERROR <<< cannot write $acq";
 print  SCRIPT <<"EOF";
$archive/$expid/atmens/Y%y4/M%m2/$expid.atmens_ebkg.%y4%m2%d2_%h2z.tar
EOF
}
#......................................................................
sub ed_jediebkgx_acq {

 my($mydir) = @_;
 my($acq);

 $acq = "$mydir/jedi_ebkgx.acq";
 open(SCRIPT,">$acq") or
 die ">>> ERROR <<< cannot write $acq";
 print  SCRIPT <<"EOF";
$archive/$expid/atmens/Y%y4/M%m2/$expid.atmens_ebkgx.%y4%m2%d2_%h2z.tar
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
sub ed_var_yaml {

  my($mydir,$conffn) = @_;

  my($acq);

  $tmprc  = "$mydir/tmp.rc";
  $thisrc = "$mydir/$conffn";
  my $cres = $resolution + 1;
  if ( $cres == 361 ) {
     if ( $scheme eq "hyb4denvar" ) {
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
     } else {
       $varxlayout = 8;
       $varylayout = 8;
       $gsixlayout = 8;
       $gsiylayout = 6 * $gsixlayout;
       $perhost_var = 16;
     }
     $gsibec_lat = 181;
     $gsibec_lon = 288;
  } elsif ( $cres == 91 ) {
     if ( $scheme eq "hyb4denvar" ) {
     } else {
       $varxlayout = 6;
       $varylayout = 6;
       $gsixlayout = 6;
       $gsiylayout = 6 * $gsixlayout;
     }
     $gsibec_lat =  91;
     $gsibec_lon = 144;
     $perhost_var = 16;
  } else {
     die "Unknown resolutio settings, aborting \n";
  }
  # the following will need ATTENTION:
  $obsop_mapdir = "$fvhome/run/jedi/Config";

     open(LUN,"$thisrc")  || die "Fail to open $thisrc $!\n";
     open(LUN2,">$tmprc") || die "Fail to open tmp.rc $!\n";

     # Change variables to the correct inputs
     #---------------------------------------
     while( defined($rcd = <LUN>) ) {
        chomp($rcd);
        if($rcd =~ /\@JEDI_BKG_RESOL/)      {$rcd=~ s/\@JEDI_BKG_RESOL/$cres/g;  }
        if($rcd =~ /\@JEDI_GSIBEC_NLAT/)    {$rcd=~ s/\@JEDI_GSIBEC_NLAT/$gsibec_lat/g;  }
        if($rcd =~ /\@JEDI_GSIBEC_NLON/)    {$rcd=~ s/\@JEDI_GSIBEC_NLON/$gsibec_lon/g;  }
        if($rcd =~ /\@JEDI_OBSOP_MAPDIR/)   {$rcd=~ s/\@JEDI_OBSOP_MAPDIR/$obsop_mapdir/g;  }
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
if ( $scheme ne "hyb4denvar" ) {
 print  SCRIPT <<"EOF";
$archive/$expid/jedi/rs/Y%y4/M%m2/$expid.jedi_agcm_import_rst.%y4%m2%d2_%h2%n2z.$ncsuffix => $expid.agcm_import_rst.%y4%m2%d2_%h2%n2z.nc4
EOF
}
}
#......................................................................
sub ed_4dfcst03_acq {

  return 0 unless ( $nogsi );

  my($mydir,$scheme) = @_;

  if ( $scheme ne "hyb4denvar" ) { return 0 };

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
                                       cres
          
DESCRIPTION


     The following parameters are required 

     scheme   3dvar, 3dfgat, or hyb4denvar
     expid    experiment name, e.g., u000_c72
     cre      var resolution, e.g., 90


OPTIONS

     -archive      location of archive (when bkg, others come from; default: /archive/u/\$user)
     -cvbc         cycle JEDI varBC, 0/1 (default: 1, i.e., cycle)
     -fvhome       location of experiment home directory (default: \$expdir/\$expid)
     -jedihome     location of ensemble members (default: \$FVHOME/run/jedi)
     -jediroot     location of JEDI build directory (default: /discover/nobackup/projects/gmao/advda/swell/JediBundles/fv3_soca_SLES15/build-intel-release)
     -jedistatic   location of JEDI static files (default: /discover/nobackup/projects/gmao/advda/SwellStaticFiles)
     -iodadir      location of pre-existing IODA files (default: /dev/null, ie, run ncdiag2ioda)
     -h            prints this usage notice

EXAMPLE COMMAND LINE

     setup_aanajedi.pl 3dfgat u000_C72 90

NECESSARY ENVIRONMENT

OPTIONAL ENVIRONMENT

      ARCHIVE      can be define in env or arg list
      FVHOME       can be define in env or arg list

AUTHOR

     Ricardo Todling (Ricardo.Todling\@nasa.gov), NASA/GSFC/GMAO
     Last modified: 25Apr2025                     by: R. Todling


EOF

  exit(1)

}
