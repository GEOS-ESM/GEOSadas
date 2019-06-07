package AGCM_PERTrc;
#########################################################################
#
# name - AGCMrc
#
# purpose -
#   This package provides routines to
#
# !Revision History
#
# 14Apr2011  Todling   Create from Joe Stassi's AGCMrc.pm
#
#########################################################################
use strict;
use File::Basename;
require Exporter;
our @ISA = "Exporter";
our @EXPORT_OK = qw ( set_AGCM_PERT_envvars
                      set_AGCM_PERT_flags
                      AGCM_PERT_label_subst
                      ed_g5agcm_pert_rc );

# global variables
#-----------------
my ($im, $jm, $km, $nx, $ny);
my ($oim, $ojm, $okm);
my ($dt, $solardt, $irraddt);

my ($fvhome, $fvroot);

my ($envvars_set, $flags_set, %subst);

#=======================================================================
# name - set_AGCM_PERT_envvars
# purpose - set global variables: $fvhome and $fvroot
#=======================================================================
sub set_AGCM_PERT_envvars {
    my %envvars = @_;

    $envvars_set = 0;
    $fvhome = &hashextract("fvhome",%envvars);
    $fvroot = &hashextract("fvroot",%envvars);
    $envvars_set = 1;
}

#=======================================================================
# name - set_AGCM_PERT_flags
# purpose - set global variables: $gocart_tracers and $iau
#=======================================================================
sub set_AGCM_PERT_flags {
    my %flags = @_;

    $flags_set = 0;
#   something ...
    $flags_set = 1;
}

#=======================================================================
# name - AGCM_PERT_label_subst
# purpose - identify label substitution values
#=======================================================================
sub AGCM_PERT_label_subst {
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

#=============================================================
# name - ed_g5agcm_pert_rc
# purpose - identify lines to comment in the AGCM_pert.rc file and
#           call outputAGCM() to edit the file.
#
# NOTE: To comment out a line in AGCM, use the first non-blank
#       characters in the AGCM line as a key in the $comment
#       hash and set its hash value to 1 (jcs-20070824).
#=============================================================
sub ed_g5agcm_pert_rc {

    my($mydir);
    my($tmpl, $outfl, %comment);
    my($segs, $label, @obsolete, @boot);

    $mydir = shift @_;
    &check_flags();

    $tmpl  = "$fvroot/etc/AGCM_apert.rc.tmpl";
    foreach (keys %comment) { delete $comment{$_} };  # empty hash

    # comment out obsolete restarts
    #------------------------------
    $segs = 2;
#   @obsolete = &get_agcm_pert_labels($tmpl, $segs, @rs5_obsolete);
#   foreach $label ( @obsolete ) { $comment{$label} = 1 };

    # comment these lines for both DAS and FCST runs
    #-----------------------------------------------
    #if ( $mydir eq "run" || $mydir eq "fcst" ) {
    #    $comment{"OX_RELAXTIME"} = 1;
    #}

    # comment these lines for DAS runs
    #---------------------------------
    #if ( $mydir eq "run" ) {
    #}

    # edit and ouptut AGCM.rc.tmpl
    #-----------------------------
    $outfl = "AGCM_apert.rc.tmpl";
    &outputAGCM_PERT($tmpl, $outfl, $mydir, %comment);
  
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
# name - outputAGCM_PERT
# purpose - edit the AGCM_apert.rc file
#=======================================================================
sub outputAGCM_PERT {
    my ($tmpl, $outfl, $mydir, %comment);
    my ($outfile, $ox_friendlies);
    my ($rcd, $label, $key);

    $tmpl  = shift @_;
    $outfl = shift @_;
    $mydir = shift @_;
    %comment = @_;

    $outfile = "$fvhome/$mydir/$outfl";

    # open file to substitute values and comment specified lines
    #-----------------------------------------------------------
    open(LUN1,"< $tmpl")    || die "Fail to open $tmpl: $!\n";
    open(LUN2,"> $outfile") || die "Fail to open $outfile: $!\n";

    while( defined($rcd = <LUN1>) ) {
        chomp($rcd);

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
        print(LUN2 "$rcd\n");
    }
    close(LUN1);
    close(LUN2);
}

#=======================================================================
# name: get_agcm_pert_labels
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
sub get_agcm_pert_labels {
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
