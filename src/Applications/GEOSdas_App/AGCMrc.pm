package AGCMrc;
#########################################################################
#
# name - AGCMrc
#
# purpose -
#   This package provides routines to
#
# !Revision History
#
# 16Jul2008  Stassi    Package added to CVS repository.
# 06Feb2009  Todling   GCM needs needs IAU in forecast mode.
# 04Mar2009  Todling   - gocart rst to boot (per 5.3.0 - Stassi)
#                      - comment certain AEROCLIM & other when gocart on
# 10Nov2009  Todling   Remove reference to agcm_internal (or agcm_import)
# 12Feb2010  Todling   Remove reference to vegdyn_internal_rst (Fortuna-2.1)
# 03May2010  Todling   Add reference to CARMA rst
# 19Aug2010  Todling   turb_internal_rst is needed for reproduc/ble fcst
# 09Oct2013  Todling   Add logics to handle precip-forcing option
#
#########################################################################
use strict;
use File::Basename;
require Exporter;
our @ISA = "Exporter";
our @EXPORT_OK = qw ( set_AGCM_envvars
                      set_AGCM_flags
                      AGCM_label_subst
                      AGCM_rsts
                      ed_g5agcm_rc );

# global variables
#-----------------
my ($gocart_tracers, $carma_tracers, $iau, $pcp_forced, $lsmodel_flag);
my ($fvhome, $fvroot);
my ($coupled);
my ($envvars_set, $flags_set, %subst);

#----------------------------------
# GCM Restart files
#----------------------------------
my (@rs5_core, @rs5_boot, @rs5_coupled, @rs5_others, @rs5_notused, @rs5_files);
my %list = (rs5_core     => \@rs5_core,
            rs5_boot     => \@rs5_boot,
            rs5_coupled  => \@rs5_coupled,
            rs5_others   => \@rs5_others,
            rs5_obsolete => \@rs5_notused,  # maintain obsolete interface
            rs5_notused  => \@rs5_notused,
            rs5_files    => \@rs5_files);

# these restarts are required
@rs5_core = qw ( rst.lcv
                 fvcore_internal_rst
                 gwd_internal_rst
                 catch_internal_rst
                 lake_internal_rst
                 landice_internal_rst
                 moist_internal_rst
                 openwater_internal_rst
                 saltwater_internal_rst
                 seaicethermo_internal_rst );

# these restarts can be bootstrapped
@rs5_boot = qw ( achem_internal_rst
                 cabc_internal_rst
                 cabr_internal_rst
                 caoc_internal_rst
                 du_internal_rst
                 gocart_internal_rst
                 gocart_import_rst
                 gocartdata_internal_rst
                 hemco_internal_rst
                 hemco_import_rst
                 irrad_internal_rst
                 moist_import_rst
                 ni_internal_rst
                 pchem_internal_rst
                 saltwater_import_rst
                 solar_internal_rst
                 ss_internal_rst
                 su_internal_rst
                 surf_import_rst
                 tr_internal_rst
                 tr_import_rst
                 turb_internal_rst
                 turb_import_rst
                 gwd_import_rst );

# warn but do not abort if these restarts are not present
@rs5_others = qw ( ana_satbang_rst
                   ana_satbias_rst
                   ana_satbiaspc_rst
                   bkg03_eta_rst
                   bkg03_sfc_rst
                   bkg06_eta_rst
                   bkg06_sfc_rst
                   bkg09_eta_rst
                   bkg09_sfc_rst
                   gaas_bkg_sfc_rst
                   traj_lcv_rst
                   ptrj_prs_rst );

# needed for coupled model
@rs5_coupled = qw ( ocean_internal_rst
                    mom_rst
                    mom1_rst
                    mom2_rst
                    mom3_rst
                    seaice_import_rst
                    seaice_internal_rst
                    seaicethermo_import_rst );

# these restarts are currently not in use
@rs5_notused = qw ( aiau_import_rst
                    aiau_import_checkout
                    carma_internal_rst
                    geosachem_internal_rst
                    gmichem_internal_rst
                    gmichem_import_rst
                    h2o_internal_rst
                    mam_internal_rst
                    orad_import_rst
                    stratchem_internal_rst 
                    stratchem_import_rst );


#=======================================================================
# name - set_AGCM_envvars
# purpose - set global variables: $fvhome and $fvroot
#=======================================================================
sub set_AGCM_envvars {
    my %envvars = @_;

    $envvars_set = 0;
    $fvhome = hashextract("fvhome",%envvars);
    $fvroot = hashextract("fvroot",%envvars);
    $coupled = hashextract("coupled",%envvars);
    $envvars_set = 1;

    # this array includes all but the notused restarts
    if ( $coupled ) {
      @rs5_files = (@rs5_core, @rs5_boot, @rs5_coupled, @rs5_others);
    } else {
      @rs5_notused = (@rs5_notused, @rs5_coupled);
      @rs5_files = (@rs5_core, @rs5_boot, @rs5_others);
    }

}

#=======================================================================
# name - set_AGCM_flags
# purpose - set global variables: $gocart_tracers and $iau
#=======================================================================
sub set_AGCM_flags {
    my %flags = @_;

    $flags_set = 0;
    $gocart_tracers = hashextract("gocart_tracers",%flags);
    $carma_tracers  = hashextract("carma_tracers", %flags);
    $lsmodel_flag   = hashextract("lsmodel_flag",  %flags);
    $iau            = hashextract("iau",           %flags);
    $pcp_forced     = hashextract("pcp_forced",    %flags);
    $flags_set = 1;
}

#=======================================================================
# name - AGCM_label_subst
# purpose - identify label substitution values
#=======================================================================
sub AGCM_label_subst {
    my ($label, $value);

    $label = shift @_;
    $value = shift @_;

    $subst{$label} = $value;
    return;
}

#=======================================================================
# name - hashextract
# purpose - use specified key to extract a value from supplied hash;
#           die if the value is not defined within the hash.
#=======================================================================
sub hashextract {
    my ($key,%hash,$value);

    $key = shift @_;
    %hash = @_;

    unless ( defined($value = $hash{$key}) ) {
        die ">>> Error <<< value for $key is not defined.";
    }
    return $value;
}

#=======================================================================
# name - AGCM_rsts
# purpose - return a specified array
#
# input parameter -
#  $arrName: name of array to return
#
# return value -
#  @arr: array named by input parameter
#  or 0 if specified array does not exist in this package
#=======================================================================
sub AGCM_rsts {
    my ($arrName, @arr);

    $arrName = shift @_;
    @arr = @{$list{$arrName}} if $list{$arrName};

    if (@arr) { return @arr; }
    else      { return 0; }
}

#=============================================================
# name - ed_g5agcm_rc
# purpose - identify lines to comment in the AGCM.rc file and
#           call outputAGCM() to edit the file.
#
# NOTE: To comment out a line in AGCM, use the first non-blank
#       characters in the AGCM line as a key in the $comment
#       hash and set its hash value to 1 (jcs-20070824).
#=============================================================
sub ed_g5agcm_rc {
    my($mydir);
    my($tmpl, $outfl, %comment, %uncomment);
    my($segs, $label, @notused, @boot);

    ($mydir) = shift @_;
    check_flags();

    $tmpl  = "$fvroot/etc/AGCM.rc.tmpl";

    # empty hashes
    #-------------
    %comment = ();
    %uncomment = ();

    # these lines should always be commented in this tag
    #---------------------------------------------------
    $comment{"AOA_FRIENDLIES"} = 1;

    # comment out notused restarts
    #------------------------------
    $segs = 2;
    @notused = get_agcm_labels($tmpl, $segs, @rs5_notused);
    foreach $label ( @notused ) { $comment{$label} = 1 };

    # comment these lines for forecasts
    #----------------------------------
    if ( $mydir eq "fcst" ) {
        $comment{"AGCM_ALPHA"}          = 1;
        $comment{"AGCM_BETA"}           = 1;
        $comment{"RECORD_FREQUENCY"}    = 1;
        $comment{"RECORD_REF_DATE"}     = 1;
        $comment{"RECORD_REF_TIME"}     = 1;
    }

    # comment these lines for both DAS and FCST runs
    #-----------------------------------------------
    if ( $mydir eq "run" || $mydir eq "fcst" ) {
        $comment{"OX_RELAXTIME"} = 1;
    }

    # comment these lines for DAS runs
    #---------------------------------
    if ( $mydir eq "run" ) {

    #   if ( $iau ) {
            $comment{"AGCM_ALPHA"}    = 1;
            $comment{"AGCM_BETA"}     = 1;
    #   }
    }

    # comment aiau restart in both fcst and run
    # -----------------------------------------
    $comment{"AIAU_IMPORT"} = 1;

    # comment these lines unless CARMA tracers turned on
    #---------------------------------------------------
    unless ( $carma_tracers ) {
        $comment{"CARMA_INTERNAL"} = 1;
    }

    # comment these lines unless GOCART tracers turned on
    #----------------------------------------------------
    if ( $gocart_tracers ) { $comment{"GOCART.data_INTERNAL"} = 1 }
    else                   { $comment{"GOCART_INTERNAL"} = 1 }

    # comment unused catch or catchCN restart
    #----------------------------------------
    if ($lsmodel_flag == 1) { 
        $comment{"CATCHCN_INTERNAL"} = 1;
        $comment{"CATCHCNCLM40_INTERNAL"} = 1;
        $comment{"CATCHCNCLM45_INTERNAL"} = 1;
    }
    if ($lsmodel_flag == 2) { $comment{"CATCH_INTERNAL"} = 1 }

    # comment these line when GOCART tracers turned on
    # ------------------------------------------------
    if ( $gocart_tracers ) {
        $comment{"AEROCLIM"}     = 1;
        $comment{"AEROCLIMDEL"}  = 1;
        $comment{"AEROCLIMYEAR"} = 1;
    }

    # uncomment precipation force except when specified
    # -------------------------------------------------
    if ( $pcp_forced ) {
        $uncomment{"#PRECIP_FILE"}  = 1;
        $uncomment{"#USE_PP_TAPER"} = 1;
    }

    # edit and output AGCM.rc.tmpl
    #-----------------------------
    $outfl = "AGCM.rc.tmpl";
    $subst{"\@BOOT"} = "NO";
    outputAGCM($tmpl, $outfl, $mydir, \%comment, \%uncomment);
  
    # edit and output AGCM.BOOTSTRAP.rc.tmpl
    #---------------------------------------
    $outfl = "AGCM.BOOTSTRAP.rc.tmpl";
    $subst{"\@BOOT"} = "YES";
    outputAGCM($tmpl, $outfl, $mydir, \%comment, \%uncomment);
}

#=======================================================================
# name - check_flags
# purpose - Check that all needed information is available prior to
#           editing the AGCM.rc.tmpl file.
#=======================================================================
sub check_flags {
    die ">>> Error <<< Environment variables have not been set."
        unless $envvars_set;

    die ">>> Error <<< Global flags have not been set."
        unless $flags_set;
}

#=======================================================================
# name - outputAGCM
# purpose - edit the AGCM.rc file
#=======================================================================
sub outputAGCM {
    my ($tmpl, $outfl, $mydir, $cPTR, $uPTR, $pPTR);
    my (%comment, %uncomment);
    my ($outfile, $ox_friendlies);
    my ($rcd, $label, $key, $rst);
    my (@DEFAULT_TYPE_FOUND);

    $tmpl  = shift @_;
    $outfl = shift @_;
    $mydir = shift @_;

    $cPTR  = shift @_;
    $uPTR  = shift @_;
    $pPTR  = shift @_;

    %comment   = %$cPTR;
    %uncomment = %$uPTR;

    $outfile = "$fvhome/$mydir/$outfl";

    # set value for ox_friendlies
    #----------------------------
    if ($mydir eq "run" || $mydir eq "fcst" ) {
        $subst{"\@OX_FRIENDLIES"} = "ANALYSIS:DYNAMICS:TURBULENCE:MOIST";
    }
    if ($mydir eq "amip") {
        $subst{"\@OX_FRIENDLIES"} = "ANALYSIS";
    }

    # open file to substitute values and comment specified lines
    #-----------------------------------------------------------
    open(LUN1,"< $tmpl")    || die "Fail to open $tmpl: $!\n";
    open(LUN2,"> $outfile") || die "Fail to open $outfile: $!\n";

    # check whether DEFAULT_CHECKPOINT_TYPE is defined in file
    #---------------------------------------------------------
    @DEFAULT_TYPE_FOUND = ();
    @DEFAULT_TYPE_FOUND = grep /DEFAULT_CHECKPOINT_TYPE/, <LUN1>;
    seek LUN1, 0, 0;

    # loop through input, modify, and write output
    #---------------------------------------------
    while( defined($rcd = <LUN1>) ) {
        chomp($rcd);

        # remove IMPORT and INTERNAL CHECKPOINT_TYPE lines (except AGCM)
        #---------------------------------------------------------------
        if ($rcd =~ /_CHECKPOINT_TYPE/) {
            next unless $rcd =~ /AGCM_/;
        }

        # value substitution
        #-------------------
        foreach $label (keys %subst) { $rcd =~ s/$label/$subst{$label}/g };

        # comment specified lines if key is in first non-blank position
        #--------------------------------------------------------------
        foreach $key ( keys %comment ) {
            if ($rcd =~ /^(\s*)$key/) {
                if ( $comment{"$key"} ) {$rcd = "\#".$rcd};
            }
        }
        # uncomment specified lines if key is in first non-blank position
        #----------------------------------------------------------------
        foreach $key ( keys %uncomment ) {
            if ($rcd =~ /^(\s*)$key/) {
                if ( $uncomment{"$key"} ) {$rcd = substr $rcd, 1;};
            }
        }
        print(LUN2 "$rcd\n");

        # add DEFAULT_CHECKPOINT_TYPE if needed
        #--------------------------------------
        next unless $rcd =~ /RECORD_REF_TIME:/;
        next if @DEFAULT_TYPE_FOUND;

        print(LUN2 "\n# Define default checkpoint type (pbinary or pnc4)\n");
        print(LUN2 "# ------------------------------------------------\n");
        print(LUN2 "DEFAULT_CHECKPOINT_TYPE: ");
        print(LUN2 $subst{'@DFLT_CHECKPOINT_TYPE'} ."\n");

        print(LUN2 "\n# Tracers definition \n");
        print(LUN2 "# ---------------------\n");
        print(LUN2 "TRI_increments:: \n");
        print(LUN2 "PCHEM::OX default \n");
        print(LUN2 ":: \n");
    }
    close(LUN1);
    close(LUN2);
}

#=======================================================================
# name: get_agcm_labels
#
# purpose: This subroutine returns the labels from the AGCM template
#          file which are associated with specified restarts.
# notes:
# 1. The labels typically come in four segments separated by "_",
#    e.g. "CATCH_INTERNAL_RESTART_FILE".
# 2. The user can specify how many segments from the label they want
#    returned.
#
# inputs:
#  $tmpl: AGCM.rc.tmpl file reference
#  $segs: number of label segments to return (starting with first)
#  @rstArr: list of restarts for which to find labels
#
# output:
#  @outarr: array of restart label segments
#=======================================================================
sub get_agcm_labels {
    my ($tmpl, $segs, @rstARR);
    my ($rcd, $label, $rst, $restart);
    my (@segments, $labelseg, @outarr);

    $tmpl = shift @_;
    $segs = shift @_;
    @rstARR = @_;

    open LUN, "< $tmpl" or die "Cannot open $tmpl";
    while ( $rcd = <LUN> ) {
        chomp $rcd;
        foreach $rst ( @rstARR ) {
            if ($rcd =~ $rst) {
                ($label, $restart) = split /:/, $rcd;
                if ($restart =~ /\s*(\S+)\s*/) {  # remove blank spaces
                    $restart = $1;  
                }
                $restart =~ s/\@BOOT//;

                # if label found, then extract requested segments
                #------------------------------------------------
                if (($restart) and ($rst eq $restart)) {
                    @segments = split /_/, $label;
                    $labelseg = join "_", @segments[0..$segs-1];
                    push @outarr, $labelseg;
                }
            }
        }
    }
    close LUN;
    return @outarr;
}

1;
