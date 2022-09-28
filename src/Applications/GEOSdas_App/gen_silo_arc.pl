#!/usr/bin/env perl
#=======================================================================
# name - gen_silo_arc.pl
#=======================================================================
use strict;
use warnings;
use FindBin qw($Bin);
use lib ("$Bin");

# global variables
#-----------------
my (@fcst_exc, @run_exc);
my ($fvhome, $rundir, $siloarc, $mstorage);
my ($edhist_pl, $echorc_x, $gsidiags_rc);

# main program
#-------------
{
    init();
    header_info();
    history_info();
    restart_info();
    gsiobs_info();
    other_info();
}

#=======================================================================
# name - init
# purpose - get runtime parameters and options
#=======================================================================
sub init {
    use Cwd qw(cwd);
    use WriteLog qw(move_);
    use Getopt::Long qw(GetOptions);
    use Perl_Config qw(perl_config);

    my ($help, $runconfig, %options, $fvroot, $siloarcT, $mstorageT);

    GetOptions( "h|help" => \$help,
                "fX=s@"  => \@fcst_exc,
                "rX=s@"  => \@run_exc );
    usage() if $help;

    # default values
    #---------------
    @fcst_exc = () unless @fcst_exc;
    @run_exc = ()  unless @run_exc;

    # exclude forecast prog.eta and prog.sfc
    #---------------------------------------
    foreach ("prog.eta", "prog.sfc") { push @fcst_exc, $_ }

    # get and check runtime parameter, fvhome
    #----------------------------------------
    $fvhome = shift @ARGV;
    usage() unless $fvhome;
    die "Error. Cannot find fvhome: $fvhome;" unless -d $fvhome;
    $fvhome = cwd() if $fvhome eq ".";

    $rundir = "$fvhome/run";
    die "Error. Cannot find rundir: $rundir;" unless -d $rundir;

    # get $FVROOT environment variable
    #---------------------------------
    $runconfig = "$fvhome/run/FVDAS_Run_Config";
    die "Error. Unable to find $runconfig;" unless -f $runconfig;
    $options{"config_file"} = $runconfig;
    $fvroot = $ENV{"FVROOT"};

    # define $siloarc and $mstorage
    #------------------------------
    $siloarc  = "$rundir/silo.arc";
    $siloarcT = "$rundir/silo.arc.orig";

    $mstorage  = "$rundir/mstorage.arc";
    $mstorageT = "$rundir/mstorage.arc.orig";

    # check for previous existing versions
    #-------------------------------------
    if (-e $siloarc) { move_($siloarc, $siloarcT) unless -e $siloarcT }
    unlink $siloarc if -e $siloarc;

    if (-e $mstorage) { move_($mstorage, $mstorageT) unless -e $mstorageT }
    unlink $mstorage if -e $mstorage;

    # check for needed executables and rc
    #------------------------------------
    $edhist_pl = "$fvroot/bin/edhist.pl";
    $echorc_x = "$fvroot/bin/echorc.pl";
    $gsidiags_rc = "$fvroot/etc/gsidiags.rc";

    die "Error. Cannot find $edhist_pl;"   unless -x $edhist_pl;
    die "Error. Cannot find $echorc_x;"    unless -x $echorc_x;
    die "Error. Cannot find $gsidiags_rc;" unless -f $gsidiags_rc;
}

#=======================================================================
# name - header_info
# purpose - write header info to top of silo.arc and mstorage.arc
#=======================================================================
sub header_info {
    use File::Basename;
    my ($script, $expid, $CVSTAG, $date, $FH);

    $script = basename $0;
    $expid = basename $fvhome;

    chomp($date = `date`);
    chomp($CVSTAG = `cat $rundir/CVSTAG`) if -e "$rundir/CVSTAG";
    $CVSTAG = "unknown" unless $CVSTAG;

    $FH = select;
    openarc();

    select SILO;
    write_header_info($expid, $script, $date, $CVSTAG);

    select MSTORE;
    write_header_info($expid, $script, $date, $CVSTAG);

    closearc();
    select $FH;
}

#=======================================================================
# name - write_header_info
# purpose - write header information
#=======================================================================
sub write_header_info {
    my ($expid, $script, $date, $CVSTAG) = @_;
    print <<"EOF";
#
# Archiving rules for fvDAS output.
#
# This is a PESTO (Put Experiment in Mass STOrage) resource file.
#
# The environment variable PESTOROOT refers to the destination archive
# location, e.g.,
#
# a) for moving files to SILO:
#    setenv PESTOROOT /discover/nobackup/$ENV{USER}/$expid
#
# b) for moving files to MASS STORAGE:
#    setenv PESTOROOT /archive/u/$ENV{USER}/$expid
#
# This file was generated automatically using the script, $script
# Tag: $CVSTAG; $date by $ENV{USER}
#...........................................................................
EOF
}

#=======================================================================
# name - history_info
# purpose - extract silo info from run and fcst dir HISTORY files
#=======================================================================
sub history_info {
    use WriteLog qw(system_);
    my ($hist, $flags, $rflags, $fflags, $name);

    $flags = "-arc -append -silo $siloarc -mstorage $mstorage";

    $fflags = $flags;
    $rflags = $flags;

    foreach $name (@fcst_exc) { $fflags .= " -X $name" }
    foreach $name (@run_exc)  { $rflags .= " -X $name" }

    # get info from run HISTORY file
    #-------------------------------
    $hist = "$fvhome/run/HISTORY.rc.tmpl";
    die "Error. Cannot find $hist;" unless -e $hist;
    system_("\n$edhist_pl $rflags $hist");

    # get info from fcst HISTORY file
    #--------------------------------
    $hist = "$fvhome/fcst/HISTORY.rc.tmpl";
    die "Error. Cannot find $hist;" unless -e $hist;
    system_("\n$edhist_pl $fflags -fcst $hist");
}

#=======================================================================
# name - restart_info
# purpose - get silo RST info from AGCM.rc file
#=======================================================================
sub restart_info {
    use AGCMrc qw(AGCM_rsts);
    my ($label, $lineB, $lineN, $lineT, $lineTAR, $line);
    my ($listname, @rsts, $rstL, $rst);

    $label = "#\n"
        .    "#   ------------------------\n"
        .    "#   GEOS-5 GCM RESTART FILES\n"
        .    "#   ------------------------\n";
    $lineB = '#${PESTOROOT}%s/rs/Y%y4/M%m2/%s._RSTID_.%y4%m2%d2_%h2z.bin';
    $lineN = '#${PESTOROOT}%s/rs/Y%y4/M%m2/%s._RSTID_.%y4%m2%d2_%h2z.nc4';
    $lineT = '#${PESTOROOT}%s/rs/Y%y4/M%m2/%s._RSTID_.%y4%m2%d2_%h2z.txt';

    openarc();
    printarc($label);

    foreach $listname (qw( rs5_core rs5_boot rs5_others )) {
        printarc("#\n# $listname\n#\n");
        @rsts = AGCM_rsts($listname);

        foreach $rst (@rsts) {
            $line = $lineB;
            $line = $lineT if $rst =~ m/ana_satb/;
            $line = $lineN if $rst =~ m/bkg/;
            $line = $lineN if $rst =~ m/gaas/;

            ($rstL = $line) =~ s/_RSTID_/$rst/;
            printarc("$rstL\n");
        }
    }
    printarc("#\n# restart tar file\n#\n");
    $lineTAR = '${PESTOROOT}%s/rs/Y%y4/M%m2/%s.agcmrst.%y4%m2%d2_%h2z.tar';
    printarc("$lineTAR\n");
    $lineTAR = '${PESTOROOT}%s/rs/Y%y4/M%m2/%s.incrst.%y4%m2%d2_%h2z.tar';
    printarc("$lineTAR\n");
    $lineTAR = '${PESTOROOT}%s/rs/Y%y4/M%m2/%s.rst.%y4%m2%d2_%h2z.tar';
    printarc("$lineTAR\n");
    $lineTAR = '${PESTOROOT}%s/rs/Y%y4/M%m2/%s.bkgcrst.%y4%m2%d2_%h2z.tar';
    printarc("$lineTAR\n");
    $lineTAR = '${PESTOROOT}%s/jedi/rs/Y%y4/M%m2/%s.jedi_agcmrst.%y4%m2%d2_%h2z.tar';
    printarc("$lineTAR\n");

    closearc();
}

#=======================================================================
# name - gsiobs_info
# purpose - get silo OBS info from gsidiags
#=======================================================================
sub gsiobs_info {
    my ($label, $line);
    my (@convlist, @satlist, @ozlist, $conv, $sat, $oz, $dline1, $dline2, $dline3, $dline4);
    my (%convlist, %satlist, %ozlist);

    $label = "#\n"
        .    "#   --------------------\n"
        .    "#   GSI DIAGNOSTIC FILES\n"
        .    "#   --------------------\n";
    $line = '${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s._ID_.%y4%m2%d2_%h2z';

    openarc();
    printarc($label);

    @convlist = ( split /\s+/, `$echorc_x -rc $gsidiags_rc convlist` );
    foreach (@convlist) { $convlist{$_} = 1 }
    printarc("#\n# convlist\n#\n");
    foreach $conv (sort keys %convlist) {
        next unless $conv;
        ($dline1 = $line) =~ s/_ID_/diag_$conv/;
        ($dline2 = $line) =~ s/_ID_/diag_${conv}_anl/;
        ($dline3 = $line) =~ s/_ID_/diag_${conv}_ges/;
        ($dline4 = $line) =~ s/_ID_/imp0hr_diag_$conv/;
        printarc("$dline1.ods\n");
        printarc("$dline2.bin\n");
        printarc("$dline3.bin\n");
        printarc("$dline2.nc4\n");
        printarc("$dline3.nc4\n");
        printarc("$dline4.ods\n\n");
    }

    @satlist = ( split /\s+/, `$echorc_x -rc $gsidiags_rc satlist` );
    foreach (@satlist) { $satlist{$_} = 1 }
#    $satlist{"conv"} = 1;
    printarc("#\n# satlist\n#\n");
    foreach $sat (sort keys %satlist) {
        next unless $sat;
        ($dline1 = $line) =~ s/_ID_/diag_$sat/;
        ($dline2 = $line) =~ s/_ID_/diag_${sat}_anl/;
        ($dline3 = $line) =~ s/_ID_/diag_${sat}_ges/;
        ($dline4 = $line) =~ s/_ID_/imp0hr_diag_$sat/;
        printarc("$dline1.ods\n");
        printarc("$dline2.bin\n");
        printarc("$dline3.bin\n");
        printarc("$dline2.nc4\n");
        printarc("$dline3.nc4\n");
        printsilo("$dline2.txt\n");
        printsilo("$dline3.txt\n");
        printarc("$dline4.ods\n\n");
    }

    @ozlist = ( split /\s+/, `$echorc_x -rc $gsidiags_rc ozlist` );
    foreach (@ozlist) { $ozlist{$_} = 1 }
    printarc("#\n# ozlist\n#\n");
    foreach $oz (sort keys %ozlist) {
        next unless $oz;
        ($dline1 = $line) =~ s/_ID_/diag_$oz/;
        ($dline2 = $line) =~ s/_ID_/diag_${oz}_anl/;
        ($dline3 = $line) =~ s/_ID_/diag_${oz}_ges/;
        ($dline4 = $line) =~ s/_ID_/imp0hr_diag_$oz/;
        printarc("$dline1.ods\n");
        printarc("$dline2.bin\n");
        printarc("$dline3.bin\n");
        printarc("$dline2.nc4\n");
        printarc("$dline3.nc4\n");
        printarc("$dline4.ods\n\n");
    }
    closearc();
}

#=======================================================================
# name - other_info
# purpose -
#=======================================================================
sub other_info {
    my $FH = select;
    openarc();

    select SILO;
    append_other_info();

    select MSTORE;
    append_other_info();

    closearc();
    select $FH;
}

#=======================================================================
# name - append_other_info
# purpose -
#=======================================================================
sub append_other_info {
    print <<"EOF";
#
#                   ---------------
#                   ANALYSIS FILES
#                   ---------------
#
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg.prs.%y4%m2%d2.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.prs.%y4%m2%d2.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bias.prs.%y4%m2%d2.nc4
\${PESTOROOT}%s/obs/Y%y4/M%m2/%s.aod.obs.%y4%m2%d2_%h2%n2z.ods
#
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.gsi.berror_stats.%y4%m2%d2_%h2z.tar
#
\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.ts.obs.%y4%m2%d2.ods
\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.ana.obs.%y4%m2%d2.ods
\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.ana.obs.%y4%m2%d2_%h2z.ods
\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.prepbufr.%y4%m2%d2.t%h2z.blk
\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.acft_profl.%y4%m2%d2.t%h2z.bfr
\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.gmao_global_convinfo.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.gmao_global_satinfo.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.gmao_global_ozinfo.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.sac.nl.%y4%m2%d2_%h2z.txt
#
\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.diag_ssmi_f%c%c.%y4%m2%d2_%h2z.ods
\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.diag_ssmi_f%c%c_%c%c%c.%y4%m2%d2_%h2z.bin
\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.diag_ssmis_%c%c%c.%y4%m2%d2_%h2z.ods
#
\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.gsidiags.%y4%m2%d2_%h2z.iter%c.tar
#
#                  -----------------------
#                  LAND DAS ANALYSIS FILES
#                  -----------------------
#
\${PESTOROOT}%s/lana/Y%y4/M%m2/%s.ens_avg.ldas_grid_inst_out.%y4%m2%d2_%h2%n2z.bin
\${PESTOROOT}%s/lana/Y%y4/M%m2/%s.ens_avg.ldas_grid_xhourly_out.%y4%m2%d2_%h2%n2z.bin
\${PESTOROOT}%s/lana/Y%y4/M%m2/%s.ens_avg.ldas_tile_inst_ensstd.%y4%m2%d2_%h2%n2z.bin
\${PESTOROOT}%s/lana/Y%y4/M%m2/%s.ens_avg.ldas_tile_inst_out.%y4%m2%d2_%h2%n2z.bin
\${PESTOROOT}%s/lana/Y%y4/M%m2/%s.ens_avg.ldas_tile_xhourly_out.%y4%m2%d2_%h2%n2z.bin
#
\${PESTOROOT}%s/lana/Y%y4/M%m2/%s.ldas_bias_inputs.%y4%m2%d2_%h2%n2z.nml
\${PESTOROOT}%s/lana/Y%y4/M%m2/%s.ldas_catparam.%y4%m2%d2_%h2%n2z.bin
\${PESTOROOT}%s/lana/Y%y4/M%m2/%s.ldas_driver_inputs.%y4%m2%d2_%h2%n2z.nml
\${PESTOROOT}%s/lana/Y%y4/M%m2/%s.ldas_ensprop_inputs.%y4%m2%d2_%h2%n2z.nml
\${PESTOROOT}%s/lana/Y%y4/M%m2/%s.ldas_ensupd_inputs.%y4%m2%d2_%h2%n2z.nml
\${PESTOROOT}%s/lana/Y%y4/M%m2/%s.ldas_out.%y4%m2%d2_%h2%n2z.txt
\${PESTOROOT}%s/lana/Y%y4/M%m2/%s.ldas_log.%y4%m2%d2_%h2%n2z.txt
#
\${PESTOROOT}%s/lana/%s.ldas_atmgrids.txt
\${PESTOROOT}%s/lana/%s.ldas_domain.txt
\${PESTOROOT}%s/lana/%s.ldas_tilecoord.txt
#
#                  ----------------------
#                  AEROSOL ANALYSIS FILES
#                  ----------------------
#
\${PESTOROOT}%s/obs/Level2/MYD04/Y%y4/M%m2/%s.MYD04_L2a.deep.%y4%m2%d2_%h2%n2z.ods
\${PESTOROOT}%s/obs/Level2/MYD04/Y%y4/M%m2/%s.MYD04_L2a.land.%y4%m2%d2_%h2%n2z.ods
\${PESTOROOT}%s/obs/Level2/MYD04/Y%y4/M%m2/%s.MYD04_L2a.ocean.%y4%m2%d2_%h2%n2z.ods
\${PESTOROOT}%s/obs/Level2/MOD04/Y%y4/M%m2/%s.MOD04_L2a.deep.%y4%m2%d2_%h2%n2z.ods
\${PESTOROOT}%s/obs/Level2/MOD04/Y%y4/M%m2/%s.MOD04_L2a.land.%y4%m2%d2_%h2%n2z.ods
\${PESTOROOT}%s/obs/Level2/MOD04/Y%y4/M%m2/%s.MOD04_L2a.ocean.%y4%m2%d2_%h2%n2z.ods
\${PESTOROOT}%s/obs/Level3/MYD04/Y%y4/M%m2/%s.MYD04_L3a.deep.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/obs/Level3/MYD04/Y%y4/M%m2/%s.MYD04_L3a.land.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/obs/Level3/MYD04/Y%y4/M%m2/%s.MYD04_L3a.ocean.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/obs/Level3/MOD04/Y%y4/M%m2/%s.MOD04_L3a.deep.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/obs/Level3/MOD04/Y%y4/M%m2/%s.MOD04_L3a.land.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/obs/Level3/MOD04/Y%y4/M%m2/%s.MOD04_L3a.ocean.%y4%m2%d2_%h2%n2z.nc4
#
\${PESTOROOT}%s/obs/Level3/MOD04/Y%y4/M%m2/%s.modis-t_land.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/obs/Level3/MOD04/Y%y4/M%m2/%s.modis-t_ocean.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/obs/Level3/MYD04/Y%y4/M%m2/%s.modis-a_land.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/obs/Level3/MYD04/Y%y4/M%m2/%s.modis-a_ocean.%y4%m2%d2_%h2%n2z.nc4
#
\${PESTOROOT}%s/chem/Y%y4/M%m2/%s.aod_a.sfc.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/chem/Y%y4/M%m2/%s.aod_d.sfc.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/chem/Y%y4/M%m2/%s.aod_f.sfc.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/chem/Y%y4/M%m2/%s.aod_k.sfc.%y4%m2%d2_%h2%n2z.nc4
#
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.ana_aod.log.%y4%m2%d2_%h2z.txt
#
#                   --------------------
#                   GSI 4DVAR/FGAT FILES
#                   --------------------
#
#\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.obsdiags.%c%c%c.%c%c%c%c.%y4%m2%d2_%h2z.bin
#\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.xhatsave.%c%c%c.%c%c%c%c.%y4%m2%d2_%h2z.bin
#\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.evec.%c%c%c.%c%c%c%c.%c%c%c%c.%y4%m2%d2_%h2z.bin
#\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.lanczvec.%c%c%c.%c%c%c%c.%c%c%c%c.%y4%m2%d2_%h2z.bin
#\${PESTOROOT}%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.zlanczos.%c%c%c.%c%c%c%c.%c%c%c%c.%y4%m2%d2_%h2z.bin
#
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.eta.%y4%m2%d2.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.sfc.%y4%m2%d2.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bias.eta.%y4%m2%d2.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg.eta.%y4%m2%d2.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg.sfc.%y4%m2%d2.nc4
#
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.aana.eta.%y4%m2%d2_%h2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.abkg.eta.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.eta.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.eta.%y4%m2%d2_%h2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana%c%c.eta.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.inst3d_met_p.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.inst3d_met_p.%y4%m2%d2_%h2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.prs.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.sfc.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.asm.inst3d_met_p.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.asm.inst3d_met_p.%y4%m2%d2_%h2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bias.eta.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg%c%c.eta.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg%c%c.sfc.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg.eta.%y4%m2%d2_%h2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg.inst3d_met_p.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg.inst3d_met_p.%y4%m2%d2_%h2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg.prs.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg.sfc.%y4%m2%d2_%h2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.cbkg%c%c.eta.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.finc.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.%s.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.inc.eta.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.inc.eta.%y4%m2%d2_%h2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.inc.sfc.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xana.eta.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xana.eta.%y4%m2%d2_%h2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xana.inst2d_met_x.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xana.inst3d_met_x.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xana.sfc.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xbkg.eta.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xbkg.inst2d_met_x.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xbkg.inst3d_met_x.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xbkg.sfc.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xinc.eta.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xinc.eta.%y4%m2%d2_%h2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xinc%c%c.eta.%y4%m2%d2_%h2z.nc4
#
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.aana.eta.%y4%m2%d2_%h2%n2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.abkg.eta.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.eta.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.eta.%y4%m2%d2_%h2%n2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana%c%c.eta.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.inst3d_met_p.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.inst3d_met_p.%y4%m2%d2_%h2%n2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.prs.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.sfc.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.asm.inst3d_met_p.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.asm.inst3d_met_p.%y4%m2%d2_%h2%n2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bias.eta.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg%c%c.eta.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg%c%c.sfc.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg.eta.%y4%m2%d2_%h2%n2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg.inst3d_met_p.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg.inst3d_met_p.%y4%m2%d2_%h2%n2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg.prs.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg.sfc.%y4%m2%d2_%h2%n2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.cbkg%c%c.eta.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.inc.eta.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.inc.eta.%y4%m2%d2_%h2%n2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.inc.sfc.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xana.eta.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xana.eta.%y4%m2%d2_%h2%n2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xana.inst2d_met_x.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xana.inst3d_met_x.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xana.sfc.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xbkg.eta.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xbkg.inst2d_met_x.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xbkg.inst3d_met_x.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xbkg.sfc.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xinc.eta.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xinc.eta.%y4%m2%d2_%h2%n2z.iter%c.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.xinc%c%c.eta.%y4%m2%d2_%h2%n2z.nc4
#
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.cana.eta.%y4%m2%d2.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.cana.prs.%y4%m2%d2.nc4
\${PESTOROOT}%s/obs/Y%y4/M%m2/%s.cana.obs.%y4%m2%d2.ods
#
#\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg.spcsig.%y4%m2%d2_%h2z.bin
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.anl.sig.%y4%m2%d2_%h2z.bin
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.anl.sfc.%y4%m2%d2_%h2z.bin
#\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.pcpbias.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.acftbias.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.satbias.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.satbiaspc.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.satbang.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.ana.prepqc.%y4%m2%d2_%h2z.bfr
#
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.fvpert.eta.%y4%m2%d2_%h2z.iter%c.nc4
#
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkgvar_rewgt.%y4%m2%d2_%h2z.grd
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkgvar_smooth.%y4%m2%d2_%h2z.grd
#
#                 --------------------------
#                 STORM TRACKING/RELOC FILES
#                 --------------------------
#
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.vtx.mix.%y4%m2%d2_%h2z.bin
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.vtx.mix.%y4%m2%d2_%h2z.ctl
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.vtx.prs.%y4%m2%d2_%h2z.bin
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.vtx.prs.%y4%m2%d2_%h2z.ctl
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.vtx%c%c.mix.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.vtx%c%c.mix.%y4%m2%d2_%h2z.ctl
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.vtx%c%c.prs.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.vtx%c%c.prs.%y4%m2%d2_%h2z.ctl
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.trak.%c%c%c.all.%y4%m2%d2%h2.txt
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.trak.%c%c%c.atcf.%y4%m2%d2%h2.txt
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.trak.%c%c%c.atcfunix.%y4%m2%d2%h2.txt
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.trak.%c%c%c.radii.%y4%m2%d2%h2.txt
#
#                 -------------------------
#                 ANALYSIS FILES (FORECAST)
#                 -------------------------
#
\${PESTOROOT}%s/ana/Y%y4/M%m2/%s.bkg.sfc.%y4%m2%d2_%h2z+%y4%m2%d2.nc4
#
#                 -----------------
#                 PROGNOSTIC FILES
#                 -----------------
#
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.prog.eta.%y4%m2%d2.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.prog.prs.%y4%m2%d2.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.prog.sfc.%y4%m2%d2.nc4
#
#                 ---------------------------
#                 TRAJECTORY FILES (FORECAST)
#                 ---------------------------
#
#\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.itraj.lcv.%y4%m2%d2_%h2z+%y4%m2%d2_%h2%n2z.nc4
#\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.ftraj.lcv.%y4%m2%d2_%h2z+%y4%m2%d2_%h2%n2z.nc4
#\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.traj.lcv.%y4%m2%d2_%h2z+%y4%m2%d2_%h2%n2z.nc4
#\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.traj.lcv.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
#\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.traj.lcv.%y4%m2%d2_%h2z+%y4%m2%d2.nc4
#\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.ptrj.prs.%y4%m2%d2_%h2z+%y4%m2%d2_%h2%n2z.nc4
#\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.ptrj.prs.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
#\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.ptrj.prs.%y4%m2%d2.nc4
#
#                 -----------------
#                 DIAGNOSTIC FILES
#                 -----------------
#
\${PESTOROOT}%s/diag/Y%y4/M%m2/%s.diag.sfc.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/diag/Y%y4/M%m2/%s.diag.eta.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/diag/Y%y4/M%m2/%s.diag.prs.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/diag/Y%y4/M%m2/%s.diag_lsm.sfc.%y4%m2%d2.nc4
#
#                   -------------------------
#                   GEOS-5 GCM RE-START FILES
#                   -------------------------
#
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.agcm_import_rst.%y4%m2%d2_%h2%n2z.%c%c%c
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.agcm09_import_rst.%y4%m2%d2_%h2%n2z.%c%c%c
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.agcm_import_rst_f.%y4%m2%d2_%h2z.%c%c%c
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.agcm_internal_rst.%y4%m2%d2_%h2z.%c%c%c
#\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.ocean_internal_rst.%y4%m2%d2_%h2z.%c%c%c
#
#\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.trak.GDA.rst.%y4%m2%d2%h2.txt
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.traj_lcv_rst.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.mtrj_lcv_rst.%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.ptrj_prs_rst.%y4%m2%d2_%h2%n2z.nc4
#
#                    -----------------------
#                    GEOS-DAS RE-START FILES
#                    -----------------------
#
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.abkg_eta_rst.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.bkg_eta_rst.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.cbkg_eta_rst.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.bkg_sfc_rst.%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.abkg%c%c_eta_rst.%y4%m2%d2_%h2z.nc4
#\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.cbkg%c%c_eta_rst.%y4%m2%d2_%h2z.nc4
#\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.ana_pcpbias_rst.%y4%m2%d2_%h2z.txt
#\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.ana_acftbias_rst.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.ana_gesfile_rst.%y4%m2%d2_%h2z.bin
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.biasinp.%y4%m2%d2_%h2z.bin
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.biasinp.%y4%m2%d2_%h2z.ctl
#
#                    -----------------------
#                    LAND-DAS RE-START FILES
#                    -----------------------
#
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.ens%n%n%n%n.catch_ldas_rst.%y4%m2%d2_%h2%n2z.bin
\${PESTOROOT}%s/rs/Y%y4/M%m2/%s.ens%n%n%n%n.pert_ldas_rst.%y4%m2%d2_%h2%n2z.bin
#
#                 -----------------
#                       CQC
#                 -----------------
#
\${PESTOROOT}%s/cqc/Y%y4/M%m2/%s.cqc.obs.%y4%m2%d2.ods
\${PESTOROOT}%s/cqc/Y%y4/M%m2/%s.cqc.obs.%y4%m2%d2.bfr
#
#                 -----------------
#                     LISTING
#                 -----------------
#
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.ExtData.%y4%m2%d2_%h2z.rc
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.gcm.log.%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.ana.log.%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.ana_stats.log.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.sac.log.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.ods.log.%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.sobs.log.%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.sobs_stats.log.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.cnv2prs.log.%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.sana.log.%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.daotovs.log.%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.cqc.log.%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.prepqc.log.%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.trak.log.%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.vtx.log.%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.zeit.reg.%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.asens%c_%c%c%c.log.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.ods%c_%c%c%c.log.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.atm_ens.log.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.listing.%y4%m2%d2_%sz-%sz.txt
#
\${PESTOROOT}%s/scanbuf/Y%y4/M%m2/%s.airs.%y4%m2%d2.t%h2z.log
\${PESTOROOT}%s/scanbuf/Y%y4/M%m2/%s.eos_amsua.%y4%m2%d2.t%h2z.log
#
#                 -----------------
#                   JEDI LISTING
#                 -----------------
\${PESTOROOT}%s/jedi/etc/Y%y4/M%m2/%s.jedi_%c%c%c.log.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/jedi/etc/Y%y4/M%m2/%s.jedi_%c%c%c%c.log.%y4%m2%d2_%h2z.txt
#
#               -------------------------
#                    JEDI ANALYSIS
#               -------------------------
#
\${PESTOROOT}%s/jedi/ana/Y%y4/M%m2/%s.jedi_ana.%y4%m2%d2_%h2z.tar
\${PESTOROOT}%s/jedi/obs/Y%y4/M%m2/%s.jedi_hofx.%y4%m2%d2_%h2z.tar
\${PESTOROOT}%s/jedi/obs/Y%y4/M%m2/%s.jedi_osen.%y4%m2%d2_%h2z.tar
\${PESTOROOT}%s/jedi/obs/Y%y4/M%m2/%s.jedi_ioda.%y4%m2%d2_%h2z.tar
#
#               -------------------------
#                    LSM FILES
#               -------------------------
\${PESTOROOT}%s/diag/Y%y4/M%m2/%s.lsm.sfc.%y4%m2%d2.nc4
#
#                 ----------------------
#                     LISTING (FORECAST)
#                 ----------------------
#
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.gcm.log.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.txt
#
#               ---------------------
#                    RC FILES
#               ---------------------
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.AGCM.rc.%y4%m2%d2_%h2z
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.Chem_Registry.rc.%y4%m2%d2_%h2z
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.GEOS_ChemGridComp.rc.%y4%m2%d2_%h2z
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.GSI_GridComp.%y4%m2%d2_%h2z.rc
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.HISTORY.%y4%m2%d2_%h2z.rc
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.HISTORY.rc.%y4%m2%d2_%h2z
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.fvcore_layout.rc.%y4%m2%d2_%h2z
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.fvpsas.rc.%y4%m2%d2_%h2z
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.g5convert.rc.%y4%m2%d2_%h2z
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.gmao_global_coinfo.rc.%y4%m2%d2_%h2z
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.gmao_global_convinfo.rc.%y4%m2%d2_%h2z
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.gmao_global_ozinfo.rc.%y4%m2%d2_%h2z
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.gmao_global_pcpinfo.rc.%y4%m2%d2_%h2z
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.gmao_global_satinfo.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.gsi.%y4%m2%d2_%h2z.rc
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.gsi.rc.%y4%m2%d2_%h2z
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.obsys.%y4%m2%d2_%h2z.rc
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.obsys.rc.%y4%m2%d2_%h2z
\${PESTOROOT}%s/rc/Y%y4/M%m2/%s.sac.nl.%y4%m2%d2_%h2z.txt
#
#                 ---------------------------
#                 PROGNOSTIC FILES (FORECAST)
#                 ---------------------------
#
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.prog.inst3d_met_p.%y4%m2%d2_%h2z+%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.prog.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.prog.prs.%y4%m2%d2_%h2z+%y4%m2%d2_%h2%n2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.prog.sfc.%y4%m2%d2_%h2z+%y4%m2%d2_%h2%n2z.nc4
#
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.prog.inst3d_met_p.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.prog.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.prog.eta.%y4%m2%d2_%h2z+%y4%m2%d2.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.prog.prs.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.prog.prs.%y4%m2%d2_%h2z+%y4%m2%d2.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.prog.sfc.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.prog.sfc.%y4%m2%d2_%h2z+%y4%m2%d2.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.vtx.mix.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.vtx.mix.%y4%m2%d2_%h2z+%y4%m2%d2.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.vtx.prs.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.vtx.prs.%y4%m2%d2_%h2z+%y4%m2%d2.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.ana.obs.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.ods
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.inst3_3d_aiau_Np.%y4%m2%d2_%h2z+%y4%m2%d2_%h2%n2z.nc4
#
#                 -----------------------------
#                 STORM TRACKING FORECAST FILES
#                 -----------------------------
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.trak.%c%c%c.all.%y4%m2%d2%h2+%y4%m2%d2%h2.txt
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.trak.%c%c%c.atcf.%y4%m2%d2%h2+%y4%m2%d2%h2.txt
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.trak.%c%c%c.atcfunix.%y4%m2%d2%h2+%y4%m2%d2%h2.txt
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.trak.%c%c%c.radii.%y4%m2%d2%h2+%y4%m2%d2%h2.txt
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.tcvitals.%y4%m2%d2%h2+%y4%m2%d2%h2.txt
#
#                 ---------------------------
#                    SINGULAR VECTOR FILES
#                 ---------------------------
#
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.svalu.%y4%m2%d2.txt
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.fvpert.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.fvpert.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.%s.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.icnop.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.fcnop.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.isvec.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.%s.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.fsvec.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.%s.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.fsens_%c%c%c.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.fsens_%c%c%c.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.Jnormf.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.Jgradf_%c%c%c.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.fsensainc_%c%c%c.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.fsensainc_%c%c%c.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.Jnormfainc.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.Jgradfainc_%c%c%c.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.Xdot_%c%c%c.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.Mferr_%c%c%c.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.EMferr_%c%c%c.eta.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/prog/Y%y4/M%m2/D%d2/H%h2/%s.EMferrnorm.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.txt

\${PESTOROOT}%s/obs/Y%y4/M%m2/%s.diag_conv_%y4%m2%d2_%h20000z.tar

# 
#                 -------------------------------
#                    MOM6 OUTPUT: Wired for now
#                 -------------------------------
# 
\${PESTOROOT}%s/mom/Y%y4/M%m2/%s.forcing.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/mom/Y%y4/M%m2/%s.prog_z.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4
\${PESTOROOT}%s/mom/Y%y4/M%m2/%s.sfc_ave.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z.nc4

EOF
}

#=======================================================================
# name - openarc
# purpose - 
#=======================================================================
sub openarc {
    open SILO, ">> $siloarc"
        or die "ERROR, opening silo.arc for append: $siloarc;";
    open MSTORE, ">> $mstorage"
        or die "ERROR, opening mstorage.arc for append: $mstorage;";
}

#=======================================================================
# name - printarc
# purpose - 
#=======================================================================
sub printarc {
    print SILO "@_";
    print MSTORE "@_";
}

#=======================================================================
# name - printsilo
# purpose - print only to silo
#=======================================================================
sub printsilo {
    print SILO "@_";
}

#=======================================================================
# name - closearc
# purpose - 
#=======================================================================
sub closearc {
    close SILO;
    close MSTORE;
}

#=======================================================================
# name - pause
# purpose -
#=======================================================================
sub pause { print "Hit <cr> to continue .. "; my $dummy = <STDIN> }

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    use File::Basename qw(basename);
    my $name = basename $0;
    print <<"EOF";

purpose: this script creates the silo.arc and mstorage.arc files for an
experiment based on information in the experiment home directory

usage: $name fvhome [options]
where
      fvhome is the experiment home directory

options
      -fX name(s)   colon- or comma-separated collections names
                    from fcst HISTORY.rc to comment in silo.arc

      -rX name(s)   colon- or comma-separated collections names
                    from run HISTORY.rc to comment in silo.arc

      -h            prints usage message

NOTE: The -fX and -rX flags can be listed multiple times and the name(s) associated
      with each flag can can be a single name or multiple names separated by ":" or ","
EOF
exit;
}
