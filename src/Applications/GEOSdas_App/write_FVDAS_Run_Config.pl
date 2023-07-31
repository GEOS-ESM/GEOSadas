#!/usr/bin/perl -w
#=======================================================================
# name - write_FVDAS_Run_Config
#
# revision history
# 30Apr2009  Stassi  initial version
# 28Aug2009  Stassi  Update for pleiades
#=======================================================================
use strict;

# global variables
#-----------------
my ($filename);
my ($ARCH, $HOST);
my ($FVHOME, $FVROOT, $RUNDIR);
my ($AOD_OBSCLASS, $BERROR, $DO_ECS_OUT, $DO_REM_SYNC, $EXPID, $FVARCH,
    $FVBCS, $GID, $MONTHLY_MEANS, $MKSI_SIDB, $MKSIOZ_SIDB, $MKSICN_SIDB, $MP_SET_NUMTHREADS, $NCEPINPUT, $NOBACKUP,
    $OBSCLASS, $OBSCLASS_NOAIRS, $OMP_NUM_THREADS, $RUN_QUADS, $PYRADMON,
    $VTRACK, $VTXLEVS, $VTXRELOC);
my ($BASEDIR, $FCSTID, $FVDMGET, $G5MODULES, $PLOTS_LOC, $GEOSUTIL, $GTAG);
my ($qalter, $PBS_BIN, $DISCOVERSHARE);
my ($FVSHARE, $SHARE, $REM_GRADS_CONFIG, $G5MGRAM, $LATS4DLOC, $FVBIN,
    $FVPORTAL, $ERROR_LOG_NAME, $ERROR_ID, $ANASTAGE, $RSTINC,
    $STNOBS, $PBS_ID, $ARCH_QUEUE, $POST_QUEUE, $POST_CPUS);
my ($FVROOT_PORTAL, $RC_LOC, $RTAG, $ANAID, $CASE, $ERR, $ERROR_EXP,
    $FCHOME, $FCSTAGE, $FERR, $FVDOLMS, $FVGROUP, $FVPLOTS, $FVSCRA,
    $FVSILO, $FVSPOOL, $FVSTAGE, $FVWORK, $GRADS_CONFIG, $LIBSZ, $MONTHLY_X);
my ($ACFTBIAS, $NEWRADBC);
my (%vv);

# main program
#-------------
{
    init();
    writefile();
}

#=======================================================================
# name - init
#=======================================================================
sub init {
    use Getopt::Long;
    use File::Basename;
    my ($force, $TMPDIR);
    my ($geos5dir, $cmd, $ws);
    my ($remNODE, $remID, $node, $check);

    GetOptions("f"         => \$force,
               "pbsbin=s"  => \$PBS_BIN,
               "dmget=s"   => \$FVDMGET,
               "fvhome=s"  => \$FVHOME,
               "fvroot=s"  => \$FVROOT,
               "expid=s"   => \$EXPID,
               "remNODE=s" => \$remNODE,
               "remID=s"   => \$remID );

    # remote node and ID for Intranet plots
    #--------------------------------------
    $remID = $ENV{"USER"} unless $remID;
    $remNODE = "train" unless $remNODE and $remNODE eq "polar";

    # get values from local environment
    #----------------------------------
    chomp($ARCH = `uname -s`);
    chomp($HOST = `uname -n`);

    # required environment variables
    #-------------------------------
    $FVHOME = $ENV{"FVHOME"} unless $FVHOME;
    $FVROOT = $ENV{"FVROOT"} unless $FVROOT;
    $EXPID  = $ENV{"EXPID"}  unless $EXPID;
    die ">> ERROR << FVHOME environment variable not defined" unless $FVHOME;
    die ">> ERROR << FVROOT environment variable not defined" unless $FVHOME;
    die ">> ERROR << EXPID environment variable not defined"  unless $EXPID;
    $RUNDIR = "$FVHOME/run";
    die ">> ERROR << directory, $RUNDIR, not found"  unless (-d $RUNDIR);

    # exit if file already exists
    #----------------------------
    $filename = "$RUNDIR/FVDAS_Run_Config";
    if ((-e $filename) and (! $force)) {
        print "$filename already exists; not overwriting\n";
        exit;
    }

    # look for other environment variables
    #-------------------------------------
    $ACFTBIAS          = $ENV{"ACFTBIAS"};
    $AOD_OBSCLASS      = $ENV{"AOD_OBSCLASS"};
    $BERROR            = $ENV{"BERROR"};
    $DO_ECS_OUT        = $ENV{"DO_ECS_OUT"};
    $DO_REM_SYNC       = $ENV{"DO_REM_SYNC"};
    $FVARCH            = $ENV{"ARCHIVE"};
    $FVBCS             = $ENV{"FVBCS"};
    $GID               = $ENV{"GID"};
    $MONTHLY_MEANS     = $ENV{"MONTHLY_MEANS"};
    $MKSI_SIDB         = $ENV{"MKSI_SIDB"};
    $MKSIOZ_SIDB       = $ENV{"MKSIOZ_SIDB"};
    $MKSICN_SIDB       = $ENV{"MKSICN_SIDB"};
    $NEWRADBC          = $ENV{"NEWRADBC"};
    $MP_SET_NUMTHREADS = $ENV{"MP_SET_NUMTHREADS"};
    $NCEPINPUT         = $ENV{"NCEPINPUT"};
    $OBSCLASS          = $ENV{"OBSCLASS"};
    $OBSCLASS_NOAIRS   = $ENV{"OBSCLASS_NOAIRS"};
    $OMP_NUM_THREADS   = $ENV{"OMP_NUM_THREADS"};
    $PYRADMON          = $ENV{"PYRADMON"};
    $RUN_QUADS         = $ENV{"RUN_QUADS"};
    $VTRACK            = $ENV{"VTRACK"};
    $VTXLEVS           = $ENV{"VTXLEVS"};
    $VTXRELOC          = $ENV{"VTXRELOC"};

    $AOD_OBSCLASS = "none" unless $AOD_OBSCLASS;

    $TMPDIR = "\$TMPDIR" if $ENV{"TMPDIR"};
    $TMPDIR = "\$TMP"    if $ENV{"TMP"} and (! $TMPDIR);
    $TMPDIR = "\$HOME"   unless $TMPDIR;

    $NOBACKUP = $ENV{"NOBACKUP"};
    $NOBACKUP = "/nobackup/" .$ENV{"USER"} unless $NOBACKUP and -d $NOBACKUP;
    $NOBACKUP = $ENV{"HOME"} unless -d $NOBACKUP;

    $SHARE = $ENV{"SHARE"} if $ENV{"SHARE"};
    unless ($SHARE) {
        $DISCOVERSHARE = "/discover/nobackup/projects/gmao/share";
        $SHARE = $DISCOVERSHARE if -d $DISCOVERSHARE;
        $SHARE = "/share" unless $SHARE and -d $SHARE;
    }
    $FVSHARE = "$SHARE/dao_ops/";

    # calculate environment variable values
    #--------------------------------------
    chomp($BASEDIR   = `$FVROOT/bin/g5_modules basedir`);
    chomp($G5MODULES = `$FVROOT/bin/g5_modules modules`);
    chomp($GTAG      = `cat $FVROOT/etc/CVSTAG`);

    unless ($FVDMGET) {
        unless (system "which dmget >& /dev/null") {
            chomp($FVDMGET = `which dmget`);
        }
    }
    $FVDMGET = "dmget" unless $FVDMGET;
    #--if (($FVDMGET) and (-e $FVDMGET)) { $FVDMGET = "${HOST}:$FVDMGET"; }

    chomp($qalter = `which qalter`);
    $PBS_BIN = dirname($qalter);
    $FCSTID    = "f" . substr($EXPID, 1, 1000);
    $GEOSUTIL  = $FVROOT;

    # hard-coded values for OPS (until further guidance given)
    #---------------------------------------------------------
    $REM_GRADS_CONFIG = "$SHARE/dasilva/opengrads/setup.csh 1.9.8";
    $G5MGRAM          = "snare:/www/html/products/nwp/meteogram/cities";
    $LATS4DLOC        = "/home/dao_ops/operations/GrADS/current/";
    $FVBIN            = "/home/dao_ops/bin";
    $FVPORTAL         = "/portal/GMAO/gmao_ops/";
    $ERROR_LOG_NAME   = "-L DEFAULT";
    $ERROR_ID         = "ops";
    $ANASTAGE         = "/discover/nobackup/dao_ops/intermediate/flk/stage/g5ana";
    $RSTINC           = "21600";
    $STNOBS           = "~/stnobs/stnobs_v3.3/";
    $PBS_ID           = "FP";
    $ARCH_QUEUE       = "-q datamove";
    $POST_QUEUE       = "";
    $POST_CPUS        = 1;

    # environment variables based on other variables
    #-----------------------------------------------
    $FVROOT_PORTAL= $FVROOT;
    $RC_LOC       = $RUNDIR;
    $RTAG         = $GTAG;

    $ANAID        = "\$EXPID";
    $CASE         = "\$EXPID";
    $ERR          = "Err_Log.pl -N \$EXPID.job -I \$ERROR_ID "
        .                           "-X \$ERROR_EXP \$ERROR_LOG_NAME";
    $ERROR_EXP    = "\$EXPID";
    $FCHOME       = "\$FVHOME/run/fcst";
    $FCSTAGE      = "\$FVHOME/fcst";
    $FERR         = "Err_Log.pl -N \$FCSTID.job -I \$ERROR_ID "
        .                   "-X \$FCSTID \$ERROR_LOG_NAME";
    $FVDOLMS      = "polar:/www2/science/dolms/\$EXPID/TERRA_mm";
    $FVGROUP      = "\$GID" if $GID;

    # determine PLOTS_LOC location
    #---------------------------
    $PLOTS_LOC = "";
    if ($ENV{"USER"} eq "dao_ops") {
        $PLOTS_LOC = "dao_ops\@train:PLOTS/\$EXPID";
    }
    else {
        $geos5dir = "/gmao/intranet/research/modeling/agcm/geos5";
        $cmd = "tcsh -c 'if (-w $geos5dir) echo yes'";

        foreach $node ( $remNODE, "train", "polar" ) {
            #----------------------------------------
            # jcs, 20181009
            # short-circuit the system(ssh..) command
            # since it interferes with runjob script
            #----------------------------------------
            last;
            chomp($check = `ssh -q -oBatchMode=yes $remID\@$node "$cmd"`);
            if ($check eq "yes") {
                $PLOTS_LOC = "$remID\@$node:$geos5dir/\$EXPID";
                last;
            }
        }
        $PLOTS_LOC = "$remID\@$remNODE:$geos5dir/\$EXPID";
    }
        
    $FVSCRA       = "\$NOBACKUP/scratch"    if $ENV{"NOBACKUP"};
    $FVSCRA       = "$NOBACKUP/scratch" unless $ENV{"NOBACKUP"};

    $FVSILO       = "\$FVSCRA/\$EXPID";
    $FVSPOOL      = "\$FVHOME/spool";
    $FVSTAGE      = "/staging/dao_ops/\$GTAG/\$EXPID";
    $FVWORK       = "$TMPDIR/fvwork.\$\$";
    $GRADS_CONFIG = "\$FVSHARE/opengrads/setup.csh 1.9.8" if $FVSHARE;
    $LIBSZ        = "\$BASEDIR/\$ARCH/lib";
    $MONTHLY_X    = "\$FVROOT/bin/time_ave.x";

    # substitute variables for values
    #--------------------------------
    $vv{"ARCH"}      = $ARCH;
    $vv{"EXPID"}     = $EXPID;
    $vv{"FVBCS"}     = $FVBCS;
    $vv{"FVHOME"}    = $FVHOME;
    $vv{"GTAG"}      = $GTAG;
    $vv{"NCEPINPUT"} = $NCEPINPUT;
    $vv{"RTAG"}      = $RTAG;

    $BERROR        = variable_subst($BERROR, "NCEPINPUT")    if $NCEPINPUT;
    $FVBCS         = variable_subst($FVBCS,  "FVHOME")       if $FVHOME;
    $FVHOME        = variable_subst($FVHOME, "EXPID")        if $EXPID;
    $FVROOT        = variable_subst($FVROOT, "GTAG", "ARCH") if $GTAG and $ARCH;
    $FVROOT_PORTAL = variable_subst($FVROOT, "RTAG", "ARCH") if $RTAG and $ARCH;
    $NCEPINPUT     = variable_subst($NCEPINPUT, "FVBCS")     if $FVBCS;
}

#=======================================================================
# name - variable_subst
#=======================================================================
sub variable_subst {
    my ($string, @vars, $var, $val);
    $string = shift @_;
    @vars = @_;

    return unless $string;

    foreach $var (@vars) {
        $val = $vv{$var};
        if ($string =~ m/\b$val\b/) {
            $string =~ s/\b$val\b/\${$var}/g;
        }
    }
    return $string;
}

#=======================================================================
# name - FVDAS_Run_Config
#=======================================================================
sub writefile {
    use File::Basename;
    my ($ans);

    print "Writing $filename\n";
    open RUNCONF, "> $filename"
        or die ">> Error << Opening file $filename: $!";
    print RUNCONF "# $EXPID Run Time Configuration\n";
    print RUNCONF "#------------------------------\n";
    print RUNCONF "setenv GID $GID\n" if $GID;
    print RUNCONF "setenv ARCH `uname -s`\n";
    print RUNCONF "setenv HOST `uname -n`\n";
    print RUNCONF "setenv GTAG $GTAG\n" if $GTAG;
    print RUNCONF "setenv RTAG $RTAG\n" if $RTAG;
    print RUNCONF "setenv EXPID $EXPID\n" if $EXPID;
    print RUNCONF "setenv FVHOME $FVHOME\n";
    print RUNCONF "setenv FVROOT $FVROOT\n";
    print RUNCONF "setenv FVROOT_PORTAL $FVROOT_PORTAL\n" if $FVROOT_PORTAL;
    print RUNCONF "setenv FVSHARE $FVSHARE\n" if $FVSHARE;
    print RUNCONF "if (! \$?SHARE) setenv SHARE $SHARE\n";
    print RUNCONF "setenv GRADS_CONFIG \"$GRADS_CONFIG\"\n" if $GRADS_CONFIG;
    print RUNCONF "setenv REM_GRADS_CONFIG \"$REM_GRADS_CONFIG\"\n" if $REM_GRADS_CONFIG;
    print RUNCONF "setenv G5MGRAM $G5MGRAM\n" if $G5MGRAM;
    print RUNCONF "setenv LATS4DLOC $LATS4DLOC\n" if $LATS4DLOC;
    print RUNCONF "setenv FVARCH $FVARCH\n" if $FVARCH;
    print RUNCONF "setenv FVSPOOL $FVSPOOL\n" if $FVSPOOL;
    print RUNCONF "setenv BASEDIR $BASEDIR\n" if $BASEDIR;
    print RUNCONF "setenv PYRADMON $PYRADMON\n" if $PYRADMON;
    print RUNCONF "#------------------------------\n";
    print RUNCONF "setenv G5MODULES \"$G5MODULES\"\n" if $G5MODULES;
    print RUNCONF "source \$FVROOT/bin/g5_modules\n";
    print RUNCONF "#------------------------------\n";
    print RUNCONF "setenv FVBCS $FVBCS\n" if $FVBCS;
    print RUNCONF "setenv NCEPINPUT \$FVBCS\n" if $FVBCS;
    print RUNCONF "setenv BERROR $BERROR\n" if $BERROR;
    print RUNCONF "setenv RC_LOC $RC_LOC\n" if $RC_LOC;
    print RUNCONF "setenv FVGROUP $FVGROUP\n" if $FVGROUP;
    print RUNCONF "setenv FVSCRA $FVSCRA\n" if $FVSCRA;
    print RUNCONF "setenv FVSILO $FVSILO\n" if $FVSILO;
    print RUNCONF "setenv FVSTAGE $FVSTAGE\n" if $FVSTAGE;
    print RUNCONF "setenv FVBIN $FVBIN\n" if $FVBIN;
    print RUNCONF "setenv FVPORTAL $FVPORTAL\n" if $FVPORTAL;
    print RUNCONF "setenv FVWORK $FVWORK\n" if $FVWORK;
    print RUNCONF "setenv ERROR_LOG_NAME \"$ERROR_LOG_NAME\"\n" if $ERROR_LOG_NAME;
    print RUNCONF "setenv ERROR_ID $ERROR_ID\n" if $ERROR_ID;
    print RUNCONF "setenv ERROR_EXP $ERROR_EXP\n" if $ERROR_EXP;
    print RUNCONF "setenv ERR \"$ERR\"\n" if $ERR;
    print RUNCONF "setenv FCSTID $FCSTID\n" if $FCSTID;
    print RUNCONF "setenv FCHOME $FCHOME\n" if $FCHOME;
    print RUNCONF "setenv FCSTAGE $FCSTAGE\n" if $FCSTAGE;
    print RUNCONF "setenv FERR \"$FERR\"\n" if $FERR;
    print RUNCONF "setenv ANAID $ANAID\n" if $ANAID;
    print RUNCONF "setenv ANASTAGE $ANASTAGE\n" if $ANASTAGE;
    print RUNCONF "setenv RSTINC $RSTINC\n" if $RSTINC;
    print RUNCONF "setenv PBS_BIN \"$PBS_BIN\"\n" if $PBS_BIN;
    print RUNCONF "setenv PLOTS_LOC $PLOTS_LOC  # Monthly Plots Web server location\n";
    print RUNCONF "setenv GEOSUTIL $GEOSUTIL\n" if $GEOSUTIL;
    print RUNCONF "setenv MERRAID $EXPID\n";
    print RUNCONF "setenv STNOBS $STNOBS\n" if $STNOBS;
    print RUNCONF "setenv LIBSZ $LIBSZ\n" if $LIBSZ;
    print RUNCONF "setenv FVDMGET $FVDMGET\n";
    print RUNCONF "setenv PBS_ID $PBS_ID\n" if $PBS_ID;
    print RUNCONF "setenv DO_ECS_OUT $DO_ECS_OUT\n" if $DO_ECS_OUT;
    print RUNCONF "setenv DO_REM_SYNC $DO_REM_SYNC\n" if $DO_REM_SYNC;
    print RUNCONF "setenv RUN_QUADS $RUN_QUADS\n" if $RUN_QUADS;
    print RUNCONF "setenv RUN_MEANS $MONTHLY_MEANS\n" if $MONTHLY_MEANS;
    print RUNCONF "setenv MONTHLY_X \"$MONTHLY_X\"\n" if $MONTHLY_X;
    print RUNCONF "setenv FVDOLMS $FVDOLMS\n" if $FVDOLMS;
    print RUNCONF "setenv CASE $CASE\n" if $CASE;
    print RUNCONF "setenv MKSI_SIDB $MKSI_SIDB\n" if $MKSI_SIDB;
    print RUNCONF "setenv MKSIOZ_SIDB $MKSIOZ_SIDB\n" if $MKSIOZ_SIDB;
    print RUNCONF "setenv MKSICN_SIDB $MKSICN_SIDB\n" if $MKSICN_SIDB;
    print RUNCONF "setenv MP_SET_NUMTHREADS $MP_SET_NUMTHREADS\n" if $MP_SET_NUMTHREADS;
    print RUNCONF "setenv OMP_NUM_THREADS $OMP_NUM_THREADS\n" if $OMP_NUM_THREADS;
    print RUNCONF "setenv ARCH_QUEUE \"$ARCH_QUEUE\"\n" if $ARCH_QUEUE;
    print RUNCONF "setenv POST_QUEUE \"$POST_QUEUE\"\n" if $POST_QUEUE;
    print RUNCONF "setenv POST_CPUS $POST_CPUS\n" if $POST_CPUS;
    print RUNCONF "setenv VTXRELOC $VTXRELOC\n" if $VTXRELOC;
    print RUNCONF "setenv VTRACK $VTRACK\n" if $VTRACK;
    print RUNCONF "setenv VTXLEVS \"$VTXLEVS\"\n" if $VTXLEVS;
    print RUNCONF "setenv OBSCLASS \"$OBSCLASS\"\n" if $OBSCLASS;
    print RUNCONF "setenv OBSCLASS_NOAIRS \"$OBSCLASS_NOAIRS\"\n" if $OBSCLASS_NOAIRS;
    print RUNCONF "setenv AOD_OBSCLASS \"$AOD_OBSCLASS\"\n";
    print RUNCONF "setenv ACFTBIAS \"$ACFTBIAS\"\n";
    print RUNCONF "setenv NEWRADBC \"$NEWRADBC\"\n";
    print RUNCONF "setenv ANGLEBC \"$NEWRADBC\"\n";

    print RUNCONF "\numask 022\n";
    print RUNCONF "limit stacksize unlimited\n";
    print RUNCONF "set REMSH = /usr/bin/ssh\n";
    print RUNCONF "set RCP   = /usr/bin/scp\n";
    close RUNCONF;
}
