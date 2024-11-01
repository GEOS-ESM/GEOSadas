#!/usr/bin/env perl
#=======================================================================
# name - monthly_setup.pl
# purpose - setup and rcfiles needed for monthly post processing.
#
# Note:
# This script calls the write_monthly_rc_arc.pl script to create
# the following files in the experiment run directory:
#   => run/monthly.rc
#   => run/monthly.arc
#   => run/monthly.keep.arc
#
# It calls the edhist.pl script to create the following file:
#   => run/monthly_plots/HISTORY.rc_tmpl
#
# And it also creates the following files 
#   => run/monthly_plots/plot/gcm_moveplot.j
#   => run/monthly_plots/plot/gcm_plot.tmpl
#   => run/monthly_plots/plot/gcm_quickplot.csh
#   => run/monthly_plots/plot/plot.rc
#
# And it copies the following file from the build's etc directory
#   => run/monthly.yyyymm.pl.tmpl
#
# The monthly_means_${yyyy}${mm}.pl script is created from the fvarchive.j
# script when it is run for the first day of the following month.
#
# revision history
# 30Apr2009  Stassi  Initial version derived from scripts by T.Owens
# 28Aug2009  Stassi  Add -site runtime flag for site-specific issues
# 24Nov2010  Stassi  Disable call to copy_rcfiles(); now handled in fvsetup
# 12Aug2011  Kokron  - Mods to allow script to recognize pleiades
#                    - Added get_siteID routine from esma_mpirun
#                    - name of MONTHLY_X no longer time_ave_mpi.x
# 14Sep2012  Stassi  Modified to write job files by editing templates;
#                    Added call to write_monthly_rc_arc.pl
# 22Oct2012  Stassi  Added subs write_plotHIST() and copy_plotfiles()
# 28Sep2016  Stassi  Renamed: monthly_means_setup.pl -> monthly_setup.pl
#=======================================================================
use strict;
use warnings;

use File::Basename qw(basename dirname);
use File::Copy qw(copy);
use File::Path qw(mkpath);
use Getopt::Long;

use FindBin qw($Bin);
use lib "$Bin";
use GMAO_utils qw(get_siteID);
use Perl_Config qw(perl_config);

# global variables
#-----------------
my ($EXPID, $FVETC, $GID, $numnodes, $scriptname, $siteID, $walltime);
my ($rundir, $run_mp_dir, $FVHOME, $FVROOT);
my ($thisnode);

# main program
#-------------
{
    init();
    write_rcfiles();
    write_plotfiles();
    copy_monthly_yyyymm_pl_tmpl();
}

#=======================================================================
# name - init
# purpose - get runtime options; check environment variables
#=======================================================================
sub init {
    my ($res, $hres, $help, $nodeflg, %opts);

    $scriptname = basename($0);
    $siteID = get_siteID();

    GetOptions( "expid=s"   => \$EXPID,
                "fvhome=s"  => \$FVHOME,
                "fvroot=s"  => \$FVROOT,
                "gid=s"     => \$GID,
                "nodeflg=s" => \$nodeflg,
                "res=s"     => \$res,
                "h"         => \$help );
    usage() if $help;

    $EXPID  = $ENV{"EXPID"} unless $EXPID;
    $FVHOME = $ENV{"FVHOME"} unless $FVHOME;
    $FVROOT = $ENV{"FVROOT"} unless $FVROOT;

    die ">> Error << EXPID environment variable not defined"  unless $EXPID;
    die ">> Error << FVHOME environment variable not defined" unless $FVHOME;
    die ">> Error << FVROOT environment variable not defined" unless $FVROOT;

    $rundir  = "$FVHOME/run";
    $run_mp_dir = "$rundir/monthly_plots";

    $FVETC  = "$FVROOT/etc";
    $ENV{"PATH"} = "$rundir:$FVROOT/bin:$ENV{PATH}";

    $GID = $ENV{"GID"} unless $GID;;
    $GID = `getsponsor.pl -dflt` unless $GID;
    chomp($GID);

    $opts{"verbose"} = 1;
    mkpath("$run_mp_dir/plot", \%opts);

    # source experiment config file
    #------------------------------
    %opts = ();
    $opts{"config_file"} = "$rundir/FVDAS_Run_Config";
    perl_config(%opts);

    # determine num nodes to use for monthly means plots job
    #-------------------------------------------------------
    $numnodes = 4;
    if    ($siteID eq "nccs") { $walltime =  "8:00:00" }
    elsif ($siteID eq "nas")  { $walltime = "12:00:00" }
        
    $thisnode = "hasw";
    if ($nodeflg) {
      $thisnode = $nodeflg;
    }
    if ($res) {
        $hres = substr($res, 0, 1) if $res;

        if ($res eq "C48" or $hres eq "a" or $hres eq "b") {
            $numnodes = 2;
            $walltime = "1:00:00"
        }
        if ($res eq "C90" or $res eq "c") {
            $numnodes = 2;
            $walltime = "2:00:00";
        }
    }
}

#=======================================================================
# name - write_rcfiles
# purpose - write the monthly.rc, monthly.arc, and monthly.keep.arc
#           files to the experiment run directory
#=======================================================================
sub write_rcfiles {
    my ($cmd);

    $cmd = "write_monthly_rc_arc.pl"
        .      " -H $rundir/HISTORY.rc.tmpl"
        .  " -inarc $rundir/silo.arc"
        . " -outdir $rundir";
    print "$cmd\n";
    system($cmd);
}

#=======================================================================
# name - write_plotfiles
# purpose - write monthly_plots HISTORY, data rc template files, and
#           plot.rc to $run_mp_dir directory
#=======================================================================
sub write_plotfiles {
    my ($flags, $cmd, $seasons, $freq, $vvflg);
    my ($infile, $outfil, %values, @setenvs, $GEOSgcm_App);

    # write HISTORY and tmpl files
    #-----------------------------
    $flags = "-plot $rundir/monthly.rc -o $run_mp_dir";
    $cmd = "edhist.pl $flags $rundir/HISTORY.rc.tmpl";
    print "$cmd\n";
    system($cmd);

    @setenvs = ();
    push @setenvs, "setenv I_MPI_DAPL_UD enable";
    push @setenvs, "unsetenv SLURM_MEM_PER_GPU";
    push @setenvs, "unsetenv SLURM_MEM_PER_NODE";

    # write gcm_plot.tmpl
    #--------------------
    $infile = "$FVETC/gcm_plot.tmpl";
    $outfil = "$run_mp_dir/plot/gcm_plot.tmpl";

    %values = ();
    $values{"\@PLOT_T"} = "12:00:00";
    $values{"\@PLOT_P"} = "SBATCH --nodes=4";
    $values{"\@PLOT_Q"} = "SBATCH --constraint=$thisnode";
    $values{"\@BATCH_GROUP"} = "SBATCH --account=$GID";
    $values{"\@SITE"} = uc($siteID);
    $values{"\@GEOSBIN"} = "$FVROOT/bin";
    $values{"\@GEOSSRC"} = $ENV{"GEOSUTIL"};

    $values{"\@MPT_SHEPHERD"} = "";
    $values{"\@LD_LIBRARY_PATH_CMD"} = "LD_LIBRARY_PATH";

    $values{"\@BATCH_TIME"} = "SBATCH --time=";
    $values{"\@BATCH_JOBNAME"} = "SBATCH --job-name=";
    $values{"\@BATCH_OUTPUTNAMEOUTPUT"} = "SBATCH --output=OUTPUT";

    replaceLabels($infile, $outfil, \%values,\@setenvs);
        
    # write gcm_moveplot.j; copy gcm_quickplot.csh
    #---------------------------------------------
    $GEOSgcm_App = "$FVROOT/bin";

    $infile = "$GEOSgcm_App/gcm_moveplot.j";
    $outfil = "$run_mp_dir/plot/gcm_moveplot.j";
    $values{"\@MOVE_Q"} = "SBATCH --partition=datamove";
    replaceLabels($infile, $outfil, \%values,\@setenvs);

    $infile = "$GEOSgcm_App/gcm_quickplot.csh";
    $outfil = "$run_mp_dir/plot/gcm_quickplot.csh";
    copy($infile, $outfil);

    # write plot.rc
    #--------------
    $infile = $ENV{"GEOSUTIL"} ."/post/plot.rc";
    $outfil = "$run_mp_dir/plot/plot.rc";
    
    $seasons = '"JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC "'
        .      '"DJF MAM JJA SON ANN"';
    $freq = "YEARLY";
    $vvflg = "-vv PLOT_SEASONS=$seasons,PLOT_FREQUENCY=$freq,PLOT_MOVE=OFF";

    $cmd = "vED $vvflg -o $outfil $infile";
    print "$cmd\n";
    system($cmd);
}

#=======================================================================
# name - replaceLabels
# purpose - replace labels in input file with values and write to output file
#
# input parameters
# => $infil: template file containing labels to be replaced
# => $outfl: output file containing values in place of labels
# => \%values: address to hash with $label => $value entries
# => \@setenvs: address to list with setenv lines
#
# notes:
# 1. $infil and $outfl should contain their directory path locations
# 2. If $outfl is a directory, then output will be written to a file in
#    that directory with the same filename as $infil.
#=======================================================================
sub replaceLabels {
    my ($infil, $outfl, $vAddr, $sAddr) = @_;
    my (%values, @setenvs);
    my ($outdir, $line, $label);

    %values = %$vAddr;
    @setenvs = @$sAddr;

    print "\nreplaceLabels < $infil\n";
    print   "replaceLabels > $outfl\n";

    die "Error. Input file not found: $infil;" unless -f $infil;
    $outfl = $outfl ."/" .basename($infil) if -d $outfl;
    $outdir = dirname($outfl);
    mkpath($outdir, {"verbose"=>1}) unless -d $outdir;

    foreach (keys %values) { print "replaceLabels: $_ => $values{$_}\n" }

    open INFIL, "< $infil" or die "Error opening INFIL file: $infil;";
    open OUTFL, "> $outfl" or die "Error opening OUTFL file: $outfl;";
    while (<INFIL>) {
        if (m/\@SETENVS/) {
            foreach $line (@setenvs) { print OUTFL "$line\n" }
            next;
        }
        foreach $label (keys %values) { s/$label/$values{$label}/g }
        print OUTFL;
    }
    close INFIL;
    close OUTFL;
}

#=======================================================================
# name - copy_monthly_yyyymm_pl_tmpl
# purpose - copy monthly.yyyymm.pl.tmpl file to experiment run directory
#=======================================================================
sub copy_monthly_yyyymm_pl_tmpl {
    my ($source, $dest);

    $source = "$FVROOT/etc/monthly.yyyymm.pl.tmpl";
    $dest = "$FVHOME/run/monthly.yyyymm.pl.tmpl";

    copy($source, $dest) or die "Error copying $source to $dest;";
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    print <<"EOF";
usage: $scriptname [options]
options
  -expid  expid       Experiment ID; defaults to \$ENV{"EXPID"}
  -fvhome fvhomedir   FVHOME directory location; defaults to \$ENV{"FVHOME"}
  -fvroot fvrootdir   FVROOT directory location; defaults to \$ENV{"FVROOT"}
  -gid groupid        Group ID; defaults to getsponsor.pl default
  -nodeflg            Node flag: hasw, sky, cas (default: hasw)
  -res                Horizontal resolution of atmosphere grid
EOF
exit;
}
