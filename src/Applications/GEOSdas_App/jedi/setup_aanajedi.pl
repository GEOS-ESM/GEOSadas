#!/usr/bin/env perl
# 
# setup_aanajedi - setup for an atmospheric JEDI analysis
#
#  25Nov2011 Todling  Initial code
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

  GetOptions ( "jedihome=s",
               "jediroot=s",
               "jedidir=s",
               "iodadir=s",
               "expdir=s",
               "fvhome=s",
               "archive=s",
               "nodename=s",
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
     print "$0: you now must edit files under $JEDIHOME\n\n";
     exit(0);
  } else {
     print "$0: failed to setup resources for atmos-ensemble\n\n";
     exit(1);
  }


#......................................................................

sub init {

   if ( $#ARGV  <  1 ) {
     print STDERR " Missing arguments; see usage:\n";
     usage();
   } else {              # required command line args
     $scheme      = $ARGV[0];
     $expid       = $ARGV[1];
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

   if ( $opt_jedidir ) {
        $JEDIDIR = $opt_jedidir;
   } else {
        $JEDIDIR = "$FVHOME/jedi";
   }

   if ( $opt_iodadir ) {
        $iodadir = $opt_iodadir;
   } else {
        $iodadir = "/dev/null";
   }

   if ( $opt_jediroot ) {
        $jediroot = $opt_jediroot;
   } else {
        $jediroot = "/discover/nobackup/projects/gmao/advda/swell/JediBundles/fv3_soca_SLES15/build-intel-release";
   }

   if ( $opt_archive ) {
        $archive = $opt_archive;
   } else {
        if ( $ENV{"ARCHIVE"} ) {
          $archive = "$ARCHIVE";
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
        $JEDIHOME = "$FVHOME/run/jedi";
   }

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

  @rc2conf   = qw ( mkiau.rc.tenv );

  @rc2jedi   = qw ( JEDIanaConfig.csh
                    SWELLConfig.csh
                    jedi_acquire_bkg.j
                    jedi_acquire_ioda.j
                    jedi_acquire_vbc.j
                    _jedi_run_var.j
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
  print "$FVROOT/etc/jedi/$fn \n";
  cp("$FVROOT/etc/jedi/$fn","$JEDIHOME/$fn");
}

# Copy scheme yaml to proper location
foreach $fn ( @rc2conf ) {
  chomp($fn);
  cp("$FVROOT/etc/jedi/$fn","$JEDIHOME/Config/$fn");
}
cp("$FVROOT/etc/jedi/geos_$scheme.yaml","$JEDIHOME/Config/geosvar.yaml");

# create JEDI work area and make sure .no_archiving exists in JEDI
if ( ! -d "$JEDIDIR" ) {
   $rc = system("/bin/mkdir -p $JEDIDIR" );
}
print "$JEDIDIR \n";
$cmd = "touch $JEDIDIR/.no_archiving";
$rc = system($cmd);

# take of resolution and layout
ed_conf_rc ("$JEDIHOME","JEDIanaConfig.csh");

# take care of satbias acq
ed_jedibkg_acq ("$JEDIHOME/Config");
ed_jediioda_acq ("$JEDIHOME/Config");
ed_jedivbc_acq ("$JEDIHOME/Config");

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
        if($rcd =~ /\@JEDI_ROOT/)           {$rcd=~ s/\@JEDI_ROOT/$jediroot/g;  }
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
sub ed_jediioda_acq {

  my($mydir) = @_;

  my($acq);

  $acq = "$mydir/jedi_ioda.acq";

 open(SCRIPT,">$acq") or
 die ">>> ERROR <<< cannot write $acq";
 print  SCRIPT <<"EOF";
$archive/$expid/obs/Y%y4/M%m2/$expid.jedi_ioda.%y4%m2%d2_%h2z.tar
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
$archive/$expid/obs/Y%y4/M%m2/$expid.jedi_vbc.%y4%m2%d2_%h2z.tar
EOF
}
#......................................................................

sub usage {

   print <<"EOF";

NAME
     setup_aanajedi.pl - setup resources to allow running JEDI analysis in GEOS ADAS
          
SYNOPSIS

     setup_aanajedi.pl [...options...] scheme
                                       expid
          
DESCRIPTION


     The following parameters are required 

     scheme   3dvar, 3dfgat, or hyb4denvar
     expid    experiment name, e.g., u000_c72


OPTIONS

     -archive      location of archive (when bkg, others come from; default: /archive/u/\$user)
     -expdir       experiment location (default: /discover/nobackup/\$user)
     -fvhome       location of experiment home directory (default: \$expdir/\$expid)
     -jedihome     location of ensemble members (default: \$FVHOME/run/jedi)
     -jediroot     location of JEDI build directory
     -jedidir      location of workspace for JEDI (default: \$FVHOME/jedi)
     -iodadir      location of pre-existing IODA files (default: /dev/null, ie, run ncdiag2ioda)
     -h            prints this usage notice

EXAMPLE COMMAND LINE

     setup_aanajedi.pl 3dfgat u000_C72

NECESSARY ENVIRONMENT

OPTIONAL ENVIRONMENT

AUTHOR

     Ricardo Todling (Ricardo.Todling\@nasa.gov), NASA/GSFC/GMAO
     Last modified: 23Apr2025                     by: R. Todling


EOF

  exit(1)

}
