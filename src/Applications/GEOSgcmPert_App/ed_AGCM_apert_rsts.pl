#!/usr/bin/perl -w
########################################################################
#
# name: ed_AGCM_apert_rsts.pl
#
# purpose - This script will add REF_DATE and REF_TIME values to the
#           AGCM.rc file based on information in the saverst.rc file.
#
# notes
# 1. See usage() function at bottom for usage information.
#
# 2. The date/time of the final restarts (see -ymde and -hmse parameters)
#    is not necessarily equal to the date/time of the final segment of
#    the GCM run, and it usually will not be.
#
# 3. The final restart date/time will always be included in the
#    RECORD_REF_DATE and RECORD_REF_TIME values written to the AGCM.rc,
#    regardless of the contents of the saverst.rc.
#
# 4. The start date/time of the GCM will never be included in the
#    RECORD_REF_DATE and RECORD_REF_TIME values written to the AGCM.rc,
#    regardless of the contents of the saverst.rc.
#
# !Revision History
# ----------------
# 16May2008   Stassi    Initial version of code.
# 10Feb2011   Todling   Renamed from original
#
########################################################################
use strict;
use Env;
use FindBin;
use lib ("$FindBin::Bin", "$FVROOT/bin");

my ($agcmIN, $agcmOUT, $saverst, $ymdb, $hmsb, $ymde, $hmse);
my ($name, @savehrs, @REFDATE, @REFTIME, @RSTFREQ, $rstfreq, $rec_final);
my ($fcst, $final);

# main program
#-------------
{
    use Saverst "readSaverst";

    &init();
    &readSaverst();
    &determine_rst_hrs();
    &add_rst_hrs_to_agcm();
}

#=======================================================================
# name - init
# purpose - get runtime parameters
#=======================================================================
sub init {
    use File::Basename;
    use Getopt::Long;
    my ($help, $datetime0, $datetime1);

    $name = basename $0;

    # get runtime inputs
    #-------------------
    GetOptions ("f=s"     => \$agcmIN,
                "o=s"     => \$agcmOUT,
                "s=s"     => \$saverst,
                "ymdb=s"  => \$ymdb,
                "hmsb=s"  => \$hmsb,
                "ymde=s"  => \$ymde,
                "hmse=s"  => \$hmse,
                "fcst"    => \$fcst,
                "final"   => \$final,
                "help"    => \$help,
                "h"       => \$help);
    &usage() if $help;

    # check required inputs
    #----------------------
    die ">>> ERROR <<< no -ymdb input: $!" unless $ymdb;
    die ">>> ERROR <<< no -ymde input: $!" unless $ymde;
    die ">>> ERROR <<< no -hmsb input: $!" unless $hmsb;
    die ">>> ERROR <<< no -hmse input: $!" unless $hmse;

    unless ($ymdb =~ /^\d{8}$/) {
        die ">>> ERROR <<< -ymdb (=$ymdb) input must be 8 digits: $!";
    }
    unless ($ymde =~ /^\d{8}$/) {
        die ">>> ERROR <<< -ymde (=$ymde) input must be 8 digits: $!";
    }
    unless ($hmsb=~/^\d$/ or $hmsb=~/^\d{2}$/ or $hmsb=~/^\d{6}$/ ) {
        die ">>> ERROR <<< -hmsb (=$hmsb) input must be 1, 2, or 6 digits: $!";
    }
    unless ($hmse=~/^\d$/ or $hmse=~/^\d{2}$/ or $hmse=~/^\d{6}$/ ) {
        die ">>> ERROR <<< -hmse (=$hmse) input must be 1, 2, or 6 digits: $!";
    }

    # default input and output files
    #-------------------------------
    $agcmIN  = "AGCM_apert.rc.tmpl" unless $agcmIN;
    $agcmOUT = "AGCM_apert.rc"      unless $agcmOUT;
    $saverst = "saverst.rc"   unless $saverst;

    $fcst = $ENV{"FORECAST"}  unless $fcst;

    $rec_final = "NO";
    $rec_final = "YES" if ($final);
    $rec_final = "NO"  if ($fcst);

    # put hms variables into correct format (zero out minutes and seconds)
    #---------------------------------------------------------------------
    if ($hmsb =~ /^(\d{1,2})\d{4}$/) {$hmsb = $1};
    if ($hmse =~ /^(\d{1,2})\d{4}$/) {$hmse = $1};

    if ($hmsb =~ /^\d{1}$/) { $hmsb  = "0".$hmsb };
    if ($hmse =~ /^\d{1}$/) { $hmse  = "0".$hmse };

    if ($hmsb =~ /^\d{2}$/) { $hmsb = $hmsb ."0000" };
    if ($hmse =~ /^\d{2}$/) { $hmse = $hmse ."0000" };

    # check for correct time order
    #-----------------------------
    $datetime0 = "$ymdb."."$hmsb";
    $datetime1 = "$ymde."."$hmse";
    if ($datetime0 > $datetime1) {
        die ">>> ERROR <<< starting datetime is later than end datetime: "
            . "$datetime0 > $datetime1: $!";
    }
    # set restart frequency 
    #----------------------
    $rstfreq = "000000";
    #--$rstfreq = "240000" if ($datetime1-$datetime0 > 1);
}

#=======================================================================
# name - determine_rst_hrs
# purpose - determine which reference dates and times should be added
#           to the list restarts to create
#=======================================================================
sub determine_rst_hrs {
    use Manipulate_time "tick";
    use Saverst qw(querySaverst setquietSaverst);

    my ($ymd, $hms);
    my ($rstymd, $rsthms, $rsthr);
    my $onehour = 60*60;   # number of seconds

    ($ymd, $hms) = ($ymdb, $hmsb);

    &setquietSaverst();  # turn on quiet mode for Saverst.pm
    foreach (1..24) {
        ($rstymd, $rsthms) = tick $ymd, $hms, $onehour;

        # always add restarts for end hour
        #---------------------------------
        if ("$rstymd"."$rsthms" eq "$ymde"."$hmse") {
            push @REFDATE, $rstymd;
            push @REFTIME, $rsthms;
            push @RSTFREQ, $rstfreq;
            last;
        }

        # add restart if hour listed in saverst file
        #-------------------------------------------
        $rsthr = substr($rsthms,0,2);
        if (&querySaverst($rsthr)) {
            push @REFDATE, $rstymd;
            push @REFTIME, $rsthms;
            push @RSTFREQ, $rstfreq;
        }
        ($ymd, $hms) = ($rstymd, $rsthms);
    }        
    &setquietSaverst(0);  # turn off quiet mode for Saverst.pm
}

#=======================================================================
# name - add_rst_hrs_to_agcm
# purpose - write the AGCM.rc file with reference dates and times added
#=======================================================================
sub add_rst_hrs_to_agcm {
    my ($line, @file);

    #------------------------------------#
    # read and edit info from input AGCM #
    #------------------------------------#
    open AGCM_IN, "< $agcmIN" or die ">>> ERROR <<< unable to open $agcmIN: $!";
    while (<AGCM_IN>) {
        chomp($line = $_);

        # set final record flag (ignore leading spaces and #'s)
        #-----------------------------------------------------------------------
        if ($line =~ /^\s*#*\s*RECORD_FINAL:/) {
            $line = "RECORD_FINAL: $rec_final";
        }

        # add restart frequency values (ignore leading spaces and #'s)
        #-------------------------------------------------------------
        {
            if ($line =~ /^\s*#*\s*RECORD_FREQUENCY:/) {
                $line = "RECORD_FREQUENCY:";
                if ($fcst) { $line = "#". $line; last; }
                foreach (@RSTFREQ) { $line .= "   $_" };
            }
        }

        # add reference date values (ignore leading spaces and #'s)
        #----------------------------------------------------------
        {
            if ($line =~ /^\s*#*\s*RECORD_REF_DATE:/) {
                $line = "RECORD_REF_DATE: ";
                if ($fcst) { $line = "#". $line; last; }
                foreach (@REFDATE) { $line .= " $_" };
            }
        }

        # add reference time values (ignore leading spaces and #'s)
        #----------------------------------------------------------
        {
            if ($line =~ /^\s*#*\s*RECORD_REF_TIME:/) {
                $line = "RECORD_REF_TIME: ";
                if ($fcst) {$line = "#". $line; last; }
                foreach (@REFTIME) { $line .= "   $_" };
            }
        }
        $line .= "\n";
        push @file, $line;
    }
    close AGCM_IN;

    #----------------------------------#
    # write edited info to output AGCM #
    #----------------------------------#
    open AGCM_OUT, "> $agcmOUT" or die ">>> ERROR <<< unable to open $agcmOUT: $!";
    foreach $line (@file) { print AGCM_OUT $line };
    close AGCM_OUT;
}

#=======================================================================
# name - usage
# purpose - prints usage information
#=======================================================================
sub usage {
    print <<"EOF";

NAME
     $name

SYNOPSIS
     $name [options] -ymdb ymd -hmsb h[ms] -ymde ymd -hmse h[ms]
     $name -h[elp]

FLAGS
     -ymdb ymd     start date of GCM run (8-digit yyyymmdd format)
     -hmsb h[ms]   start hour of GCM run (1-, 2-, or 6-digit hhmmss format)
     -ymde ymd     date of final restarts (8-digit yyyymmdd format)
     -hmse h[ms]   hour of final restarts (1-, 2-, or 6-digit hhmmss format)

     -h[elp]         prints this usage message

OPTIONS
     -f agcm_in      name of input file (defaults to "AGCM_apert.rc.tmpl")
     -o agcm_out     name of output file (defaults to "AGCM_apert.rc")
     -s saverst      name of input saverst (defaults to "saverst.rc")
     -fcst           flag to indicate a forecast run
     -final          force "RECORD_FINAL: YES" in AGCM_apert.rc file (default is "NO")

NOTES
     See script prologue for more information.

AUTHOR
     Joe Stassi, SAIC (joe.stassi\@nasa.gov)

EOF
exit;
}
