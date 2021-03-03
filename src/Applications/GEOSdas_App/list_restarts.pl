#!/usr/bin/env perl
#=======================================================================
# name - list_restarts
# purpose - print the list of restart files to standard output.
#
# see usage() function at end of script for more info
#=======================================================================
use strict;
use warnings;
use Env;
use FindBin qw($Bin);
use lib ("$Bin");

# global variables
#-----------------
my ($bkgHrFreq, $expid, $nhr, $noana, $nymd, $readme, $rstdir, $tarfile);
my $tempcnt = 0;

# main program
#-------------
{
    use Manipulate_time "tick";
    my ($pattern, %rstfiles);
    my ($bymd, $bhm, $bhms, $bhr, $delta_time);
    my ($altdir, $label, $last);
    my ($minus3hrs, $tymd, $thms, $thr);

    init();

    # first check for restart tar file
    #---------------------------------
    $tarfile = "$rstdir/${expid}.rst.${nymd}_${nhr}z.tar";
    if (-e $tarfile) { print "$tarfile\n" }
    else             { $tarfile = "" }

    # GCM restarts
    #-------------
    $pattern = "${expid}.*.${nymd}_${nhr}z.bin";
    foreach (get_filelist($pattern, $rstdir)) { $rstfiles{$_} = 1 }

    $pattern = "${expid}.*internal_rst.${nymd}_${nhr}z.nc4";
    foreach (get_filelist($pattern, $rstdir)) { $rstfiles{$_} = 1 }

    $pattern = "${expid}.*import_rst.${nymd}_${nhr}z.nc4";
    foreach (get_filelist($pattern, $rstdir)) { $rstfiles{$_} = 1 }

    $pattern = "${expid}.*.${nymd}_${nhr}z.ctl";
    foreach (get_filelist($pattern, $rstdir)) { $rstfiles{$_} = 1 }

    # ANA restarts
    #-------------
    unless ($noana) {

        # txt restarts
        #-------------
        $pattern = "${expid}.*.${nymd}_${nhr}z.txt";
        foreach (get_filelist($pattern, $rstdir)) { $rstfiles{$_} = 1 }

        # trak file
        #----------
        $minus3hrs = -3 * 60 * 60;  # in seconds
        ($tymd, $thms) = tick $nymd, "${nhr}0000", $minus3hrs;

        $thr = substr($thms, 0, 2);
        $altdir = alt_rstdir($rstdir, $nymd, $tymd);

        $pattern = "${expid}.trak.GDA.rst.${tymd}${thr}.txt";
        foreach (get_filelist($pattern, $rstdir)) { $rstfiles{$_} = 1 }
        foreach (get_filelist($pattern, $altdir)) { $rstfiles{$_} = 1 }

        # initialize bkg time
        #--------------------
        $bymd = $nymd;
        $bhr = $nhr;
        $bhms = $nhr."0000";
        while (length($bhms) < 6) { $bhms = "0".$bhms }

        # initialize variables for until loop
        #------------------------------------
        $label = 3;
        $last = 9;
        $delta_time = $bkgHrFreq * 60 * 60;  # in seconds

        # use alternate directory for bkgs which straddle month boundry
        #--------------------------------------------------------------
        until ($label > $last) {
            $altdir = alt_rstdir($rstdir, $nymd, $bymd);

            # get 03/06/09 bkg files
            #-----------------------
            $bhr = substr($bhms, 0, 2);
            $bhm = substr($bhms, 0, 4);

            while (length($label) < 2) { $label = "0".$label }

            # bkg
            #----
            $pattern = "${expid}.bkg${label}_*.${bymd}_${bhr}z.nc4";
            foreach (get_filelist($pattern, $rstdir)) { $rstfiles{$_} = 1 }
            foreach (get_filelist($pattern, $altdir)) { $rstfiles{$_} = 1 }

            $pattern = "${expid}.bkg${label}_*.${bymd}_${bhm}z.nc4";
            foreach (get_filelist($pattern, $rstdir)) { $rstfiles{$_} = 1 }
            foreach (get_filelist($pattern, $altdir)) { $rstfiles{$_} = 1 }

            # cbkg
            #-----
            $pattern = "${expid}.cbkg${label}_*.${bymd}_${bhr}z.nc4";
            foreach (get_filelist($pattern, $rstdir)) { $rstfiles{$_} = 1 }
            foreach (get_filelist($pattern, $altdir)) { $rstfiles{$_} = 1 }

            $pattern = "${expid}.cbkg${label}_*.${bymd}_${bhm}z.nc4";
            foreach (get_filelist($pattern, $rstdir)) { $rstfiles{$_} = 1 }
            foreach (get_filelist($pattern, $altdir)) { $rstfiles{$_} = 1 }

            # other bkg files
            #----------------
            unless ($label == 3) {
                $pattern = "${expid}.gaas_bkg_*_rst.${bymd}_${bhr}z.nc4";
                foreach (get_filelist($pattern, $rstdir)) { $rstfiles{$_} = 1 }
                foreach (get_filelist($pattern, $altdir)) { $rstfiles{$_} = 1 }

                $pattern = "${expid}.gaas_bkg_*_rst.${bymd}_${bhm}z.nc4";
                foreach (get_filelist($pattern, $rstdir)) { $rstfiles{$_} = 1 }
                foreach (get_filelist($pattern, $altdir)) { $rstfiles{$_} = 1 }

                $pattern = "${expid}.*bkg_*_rst.${bymd}_${bhr}z.nc4";
                foreach (get_filelist($pattern, $rstdir)) { $rstfiles{$_} = 1 }
                foreach (get_filelist($pattern, $altdir)) { $rstfiles{$_} = 1 }

                $pattern = "${expid}.*bkg_*_rst.${bymd}_${bhm}z.nc4";
                foreach (get_filelist($pattern, $rstdir)) { $rstfiles{$_} = 1 }
                foreach (get_filelist($pattern, $altdir)) { $rstfiles{$_} = 1 }
            }

            # advance bkg time and label
            #---------------------------
            ($bymd, $bhms) = tick $bymd, $bhms, $delta_time;
            $label += $bkgHrFreq;
        }
    }
    if ($readme) {
        $pattern = "README.${expid}*";
        foreach (get_filelist($pattern, $rstdir)) { $rstfiles{$_} = 1 }
    }
    foreach (sort keys %rstfiles) { print "$_\n" };
}

#=======================================================================
# name - init
# purpose - get runtime flags
#=======================================================================
sub init {
    use Getopt::Long;
    use File::Basename;
    my ($dao, $noprompt, $help, $flags);

    # get input options
    #------------------
    GetOptions( "bf=i"        => \$bkgHrFreq,
                "d=s"         => \$rstdir,
                "dao"         => \$dao,
                "expid=s"     => \$expid,
                "nymd|ymd=s"  => \$nymd,
                "nhms|hr=s"   => \$nhr,
                "noana"       => \$noana,
                "np|noprompt" => \$noprompt,
                "r"           => \$readme,
                "h|help"      => \$help );
    usage() if $help;

    # get and/or verify restart information
    #--------------------------------------
    $flags = "";
    $flags .= " -noprompt"     if $noprompt;
    $flags .= " -expid $expid" if $expid;
    $flags .= " -d $rstdir"    if $rstdir;
    $flags .= " -dao"          if $dao;
    $flags .= " -ymd $nymd"    if $nymd;
    $flags .= " -hr $nhr"      if $nhr;

    ($rstdir, $expid, $nymd, $nhr) = `$Bin/info_restarts.pl $flags`;
    die ">> Error << Unable to find restart information; " unless $rstdir;
    chomp($rstdir, $expid, $nymd, $nhr);

    # set background frequency
    #-------------------------
    $bkgHrFreq = 3 unless $bkgHrFreq and $bkgHrFreq == 1;
}

#=======================================================================
# name - get_filelist
# purpose - get list of files which match a given pattern
#=======================================================================
sub get_filelist {
    my ($pattern, $rsdir);
    my (@filelist, $file);
    $pattern = shift @_;
    $rsdir = shift@_;

    @filelist = ();
    $tempcnt++;

    # get filenames from tarfile
    #---------------------------
    if ($tarfile) {
        return @filelist unless $rsdir eq $rstdir;

        $pattern =~ s/\./\\\./g;
        $pattern =~ s/\*/\.*/g;
        foreach $file (`tar tf $tarfile`) {
            chomp($file);
            if ($file =~ m/^$pattern$/) { push @filelist, $file }
        }
    }

    # get filenames from $rsdir
    #--------------------------
    else {
        foreach $file (glob("$rsdir/$pattern")) {
            push @filelist, $file if -s $file;
        }
    }

    return @filelist;
}

#=======================================================================
# name: alt_rstdir
# purpose: take the restart directory for a given date ($rstdir1 and $ymd1), 
#          and use it to determine the restart directory for a different
#          date ($rstdir2 and $ymd2)
#
# input parameters
# => $rstdir1 : original restart directory
# => $ymd1    : original yyyymmdd
# => $ymd2    : new yyyymmdd
# => %replace : hash containing replacement strings where p2 is substituted
#               for p1 in the $rstdir if $replace{p1} = p2
#
# notes:
# 1. This is used to determine the restart directory for the bkg06 and
#    bkg09 files when the restarts fall on the last day of the month.
# 2. If Yyyyy amd Mmm values are not in the original restart directory
#    name then the new restart directory name will not be different,
#    regardless of whether the restarts are from the end of the month
#    or not.
#=======================================================================
sub alt_rstdir {
    my ($rstdir1, $ymd1, $ymd2, %replace);
    my ($year1, $month1, $year2, $month2, $rstdir2);

    # input parameters
    #-----------------
    $rstdir1 = shift @_;
    $ymd1    = shift @_;
    $ymd2    = shift @_;
    %replace = @_;
    
    # extract year and month values
    #------------------------------
    $year1  = substr($ymd1, 0, 4);
    $month1 = substr($ymd1, 4, 2);

    $year2  = substr($ymd2, 0, 4);
    $month2 = substr($ymd2, 4, 2);

    # determine rstdir for ymd2
    #--------------------------
    ($rstdir2 = $rstdir1) =~ s/Y$year1/Y$year2/;
    $rstdir2 =~ s/M$month1/M$month2/;

    # substitute replacement strings
    #-------------------------------
    foreach (keys %replace) { $rstdir2 =~ s/$_/$replace{$_}/ }

    return $rstdir2;
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    my $script = basename $0;
    print <<"EOF";

NAME
    $script

PURPOSE
    Print a list of available restart files to standard output.

SYNOPSIS
    $script [options]

OPTIONS
     -bf n            Hour frequency of bkg restarts, either 1 for every hour
                      or 3 (default) for every 3 hours
     -d dirname       Start location for the restart directory search;
                      defaults to \$ARCHIVE if defined or "." if not
     -dao             Get OPS restarts; this option works best when accompanied
                      by flag/value option -nymd and/or -expid
     -expid expid     Experiment ID of restart files 
     -nymd ymd        Data year/month/day of restart files
                      (yyyymmdd format or any substring thereof)
     -nhms hour       Data hour of restart files (can be in hh or hhmmss format)
     -noana           Model restarts only; do not copy analysis restarts
     -np              No interactive prompts
     -r               Look for README file and copy it if found
     -h               Print this usage message

ALTERNATE OPTIONS
     -ymd ymd         same as -nymd
     -hr hour         same as -nhms
     -noprompt        same as -np
     -help            same as -h

OUTPUT
    A list of restarts found is printed to standard input

NOTES
1. If a restart tarfile is the first entry on the output list, then the remaining
   files were found on the tarfile.
2. -ymd can take partial dates, e.g. 1214 (for Dec 14) or just 14
3. -hr can be given in hhmmss format.
4. This script does not attempt to verify that the set of restart files is complete.
   It includes only the restarts which it finds.

AUTHOR
    Joe Stassi, SAIC (joe.stassi\@nasa.gov)

EOF
    exit;
}
