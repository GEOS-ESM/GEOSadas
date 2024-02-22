#!/usr/bin/env perl
#=======================================================================
# name - write_monthly_rc_arc.pl
# purpose -
#   write the following files:
#      => monthly.rc ....... monthly post processing resource file
#      => monthly.arc ...... monthly archive resource file
#      => monthly.keep.arc . monthly keep archive resource file
#
#   See usage() for run options.
#
#   based on information in HISTORY.rc.tmpl and silo.arc
#   (the experiment HISTORY and archive resource files)
#
# Notes:
# 1. The monthly.arc file contains outputs to archive and then delete from
#    the experiment directory.
# 2. The monthly.keep.arc contains names of monthly and diurnal files which
#    get archived but should not be deleted from the experiment directory.
#
# global variables:
# => $siloarc: experiment archive resource file name [silo.arc]
# => $histfile: experiment HISTORY resource file name [HISTORY.rc.tmpl]
#
# => $outRc: name of monthly resource file [monthly.rc]
# => $outArc: name of monthly archive resource file [monthly.arc]
# => $outArk: name of monthly keep archive resource file [monthly.keep.arc]
#
# => $rcFLG:  write $outRc  if "true"
# => $arkFLG: write $outArk if "true"
# => $arcFLG: write $outArc if "true"
#
# => %list: keys = analysis products, e.g. "ana.eta", and names of model
#                  collections found in $histfile, e.g. "inst1_2d_asm_Nx"
#           values = a single code containing one or more of the following
#                   letters, {C, M, T, P}, with the following meanings:
#                     C => comment entry in monthly.rc
#                     M => create monthly products
#                     T => tar this product
#                     P => create monthly plots for this product
#                the code, except for the 'C', is written to the monthly.rc file
# => %freq: keys = names of collections found in $histfile
#           values = hour frequency (e.g. 1, 3, or 6) as specified in $histfile
# => %mode: keys = names of collections found in $histfile
#           values = mode ("inst" or "tavg") as specified in $histfile
#
# key local variables in main program:
# => @outRcArr: lines to write to the $outRc file  (monthly.rc)
# => @outArkArr: lines to write to the $outArk file (monthly.keep.arc)
# => @outArcArr: lines to write to the $outArc file (monthly.arc)
#
# Revision History
# ----------------
# 27Oct2010  Stassi  Initial version
# 28Sep2016  Stassi  Renamed: write_mm_rc_arc.pl -> write_monthly_rc_arc.pl
#=======================================================================
use strict;
use warnings;
use File::Basename;

# global variables
#-----------------
my ($histfile, $siloarc, $ncana);
my ($outRc, $outArc, $outArk);
my ($rcFLG, $arcFLG, $arkFLG);
my (%list, %freq, %mode);

my $script = basename $0;

# analysis and aod output names with default hour types
# (hard-coded until I can figure out a way to get them from somewhere else)
# (hour types are defined in the Manipulate_time.pm module)
#--------------------------------------------------------------------------
my %hourtype = ( "ana.eta"           => "inst3d",
                 "ana.inst3d_met_p"  => "inst3d",
                 "ana.satbang"       => "inst3d",
                 "ana.satbias"       => "inst3d",
                 "asm.inst3d_met_p"  => "inst2d",
                 "bkg.inst3d_met_p"  => "inst3d",
                 "bkg03.eta"         => "bkg03",
                 "bkg03.sfc"         => "bkg03",
                 "bkg06.eta"         => "inst3d",
                 "bkg06.sfc"         => "inst3d",
                 "bkg09.eta"         => "bkg03",
                 "bkg09.sfc"         => "bkg03",
                 "cbkg09.eta"        => "bkg03",
                 "inc.eta"           => "inst3d",
                 "trak.GDA.all"      => "inst3d",
                 "trak.GDA.atcf"     => "inst3d",
                 "trak.GDA.atcfunix" => "inst3d",
                 "trak.GDA.radii"    => "inst3d",
                 "aod_a.sfc"         => "inst2d",
                 "aod_d.sfc"         => "inst2d",
                 "aod_f.sfc"         => "inst2d",
                 "aod_k.sfc"         => "inst2d" );

# set default to "do not tar" for these collections
#--------------------------------------------------
my %noTarList = ( "inst3_3d_asm_Np" => 1 ); 
my %commentList = ( "asm.eta"      => 1,
                    "bkg.eta"      => 1,
                    "bkg.sfc"      => 1,
                    "cbkg.eta"     => 1,
                    "gaas_bkg.sfc" => 1,
                    "prog.eta"     => 1,
                    "prog.sfc"     => 1,
                    "ptrj.prs"     => 1,
                    "traj.lcv"     => 1,
                    "bkg_clcv_rst" => 1,
                    "vtx.mix"      => 1,
                    "vtx.prs"      => 1,
                    "asm_inst_3hr_glo_C180x180x6_v72"  => 1,
                    "asm_tavg_3hr_glo_C180x180x6_v72"  => 1,
                    "asm_inst_1hr_glo_C180x180x6_slv"  => 1,
                    "cld_tavg_3hr_glo_C180x180x6_v72"  => 1,
                    "mst_tavg_3hr_glo_C180x180x6_v73"  => 1,
                    "mst_tavg_3hr_glo_C180x180x6_v72"  => 1,
                    "rad_tavg_3hr_glo_C180x180x6_v72"  => 1,
                    "trb_tavg_3hr_glo_C180x180x6_v73"  => 1,
                    "tdt_tavg_3hr_glo_C180x180x6_v72"  => 1,
                    "udt_tavg_3hr_glo_C180x180x6_v72"  => 1,
                    "qdt_tavg_3hr_glo_C180x180x6_v72"  => 1,
                    "odt_tavg_3hr_glo_C180x180x6_v72"  => 1,
                    "slv_tavg_1hr_glo_C180x180x6_slv"  => 1,
                    "flx_tavg_1hr_glo_C180x180x6_slv"  => 1,
                    "rad_tavg_1hr_glo_C180x180x6_slv"  => 1,
                    "lnd_tavg_1hr_glo_C180x180x6_slv"  => 1,
                    "lfo_tavg_1hr_glo_C180x180x6_slv"  => 1,
                    "lfo_inst_1hr_glo_C180x180x6_slv"  => 1,
                    "ocn_tavg_1hr_glo_C180x180x6_slv"  => 1,
                    "aer_inst_3hr_glo_C180x180x6_v72"  => 1,
                    "chm_inst_3hr_glo_C180x180x6_v72"  => 1,
                    "aer_tavg_3hr_glo_C180x180x6_slv"  => 1,
                    "adg_tavg_3hr_glo_C180x180x6_slv"  => 1,
                    "chm_tavg_3hr_glo_C180x180x6_slv"  => 1,
                    "nav_tavg_3hr_glo_C180x180x6_v72"  => 1,
                    "nav_tavg_3hr_glo_C180x180x6_v73"  => 1,
                    "ctm_tavg_1hr_glo_C180x180x6_v72"  => 1,
                    "ctm_inst_1hr_glo_C180x180x6_v72"  => 1,
                    "asm_const_0hr_glo_C180x180x6_slv" => 1 );
# main program
#-------------
{
    my ($anaflags, @outRcArr, @outArkArr, @outArcArr);

    init();
    if ($ncana) { $anaflags = "T" }
    else        { $anaflags = "CT" }
    foreach (keys %hourtype) { $list{$_} = $anaflags }
    
    get_list_from_HIST();
    get_info_from_SILO(\@outRcArr, \@outArcArr, \@outArkArr);

    write_output($outRc,  @outRcArr)  if $rcFLG;
    write_output($outArc, @outArcArr) if $arcFLG;
    write_output($outArk, @outArkArr) if $arkFLG;
}

#=======================================================================
# name - init
# purpose - get runtime parameter; set values for global variables
#=======================================================================
sub init {
    use Getopt::Long qw(:config no_ignore_case);
    use File::Basename qw(basename);
    my ($outdir, $help);

    GetOptions( "H=s"      => \$histfile,
                "outdir=s" => \$outdir,
                "inarc=s"  => \$siloarc,

                "rc|rcflg"   => \$rcFLG,
                "arc|arcflg" => \$arcFLG,
                "ark|arkflg" => \$arkFLG,

                "outrc=s"  => \$outRc,
                "outark=s" => \$outArk,
                "outarc=s" => \$outArc,

                "nc"       => \$ncana,
                "h|help"   => \$help );
    usage() if $help;

    # input files
    #------------
    $siloarc  = "silo.arc"        unless $siloarc;
    $histfile = "HISTORY.rc.tmpl" unless $histfile;

    die "Error. Cannot find siloarc, $siloarc;"   unless -e $siloarc;
    die "Error. Cannot find histfile, $histfile;" unless -e $histfile;

    # write all outputs by default
    #-----------------------------
    unless ($rcFLG or $arcFLG or $arkFLG) {
        $rcFLG  = 1;
        $arcFLG = 1;
        $arkFLG = 1;
    }

    # output files
    #-------------
    if ($outRc) { $outRc .= "/monthly.rc" if -d $outRc }
    else        { $outRc = "monthly.rc" }

    if ($outArc) { $outArc .= "/monthly.arc" if -d $outArc }
    else        { $outArc = "monthly.arc" }

    if ($outArk) { $outArk .= "/monthly.keep.arc" if -d $outArk }
    else         { $outArk = "monthly.keep.arc" }

    if ($outdir) {
        die "Error. Cannot find directory: $outdir;" unless -d $outdir;
        $outRc  = "$outdir/" .basename $outRc;
        $outArc = "$outdir/" .basename $outArc;
        $outArk = "$outdir/" .basename $outArk;
    }
}

#=======================================================================
# name - get_list_from_HIST
# purpose - add collection names from $histfile to list
#=======================================================================
sub get_list_from_HIST {
    my ($colFLG, $extFLG);
    my ($colon, $doublecolon);

    $colFLG = 0;   # set =1 after "COLLECTIONS:" label found
    $extFLG = 0;   # set =1 while extracting collection names

    $colon = ":";
    $doublecolon = "::";

    # get collection information from $histfile
    #------------------------------------------
    open HIST, "< $histfile" or die "Error during open $histfile;";
    while (<HIST>) {

        # look for start of COLLECTIONS
        #------------------------------
        unless ($colFLG) {
            if (/^\s*COLLECTIONS:/) {
                $colFLG = 1;
                $extFLG = 1; 
                extract_collection($_);
            }
            next;
        }

        # extract collection names
        #-------------------------
        if ($extFLG) {
            extract_collection($_);
            $extFLG = 0 if /$doublecolon/;
            next;
        }

        # get frequency and mode information
        #-----------------------------------
        extract_freq($_) if /\.frequency\s*$colon/;
        extract_mode($_) if /\.mode\s*$colon/;
    }
    close HIST;
}

#=======================================================================
# name - extract_collection
# purpose - extract collection name from line in $histfile and add
#           name to %list
#
# input parameter
# => $line: line from $histfile containing a collection name
#
#=======================================================================
sub extract_collection {
    my ($line, $name);

    $line = shift;

    if ( $line =~ /'(.*)'/ or $line =~ /"(.*)"/ ) {
        $name = $1;
        die "Error. Duplicate collection name: $name;" if $list{$name};

        # exclude collection names which include a '+' character
        # exclude collection names which include a '.' character (for now)
        #-----------------------------------------------------------------
        return if $name =~ m/\+/;

        # determine processing flags
        #---------------------------
        $list{$name}  = '';
        $list{$name} .= 'C' if $line =~ /^\s*\#/ or $commentList{$name};
        $list{$name} .= 'M' if means_type($name);
        $list{$name} .= 'T' unless $noTarList{$name};
        $list{$name} .= 'P' if plots_type($name);
    }
}

#=======================================================================
# name - means_type
# purpose - return True value, if monthly means can be calculated
#           for the named data collection
#
# input parameter
# => $name: name of data collection
#=======================================================================
sub means_type {
    my $name = shift;
    return unless $name =~ m/inst/ or $name =~ m/tavg/;
    return if $name =~ m/Nv$/ or $name =~ /Ne$/;
    return 1;
}

#=======================================================================
# name - plots_type
# purpose - return True value, if monthly plots can be produced
#           for the named data collection
#
# input parameter
# => $name: name of data collection
#=======================================================================
sub plots_type {
    my $name = shift;
    return unless means_type($name);
    return unless $name =~ m/Cp$/ or $name =~ m/Np$/ or $name =~ m/Nx$/ 
               or $name =~ m/slv$/ or $name =~ m/p42$/;
    return 1;
}

#=======================================================================
# name - extract_freq
# purpose - extract collection frequency from line in $histfile
#
# input parameter
# => $line: line from $histfile containing frequency information for a collection
#=======================================================================
sub extract_freq {
     my ($line);
    $line = shift;
    if ( $line =~  /^\s*(\S*)\.frequency\s*:\s*(\d{6})\s*,/ ) {
        $freq{$1} = $2 / 10000;
    }
}

#=======================================================================
# name - extract_mode
# purpose - extract collection mode from line in $histfile
#
# input parameter
# => $line: line from $histfile containing mode information for a collection
#=======================================================================
sub extract_mode {
     my ($line, $name);
    $line = shift;
    if ( $line =~  /^\s*(\S*)\.mode\s*:/ ) {
        $name = $1;
        $mode{$name} = "inst" if $line =~ /instantaneous/;
        $mode{$name} = "tavg" if $line =~ /time-averaged/;
    }
}

#=======================================================================
# name - get_info_from_SILO
# purpose - use information from the experiment archive file (e.g. silo.arc)
#           combined with info extracted from $histfile to create lines to
#           write to $outRc and $outArk for each collection.
#
# output parameters
# => $outRcArrAddr: address of @line array to hold lines to write to $outRc
#=======================================================================
sub get_info_from_SILO {
    my ($cleanlog, $diurnal, $etcdir, $fetchlog, $flags, $format);
    my ($htype, $line, $listflags, $maxlen, $mline, $mline1, $mmlog);
    my ($monthly, $name, $outArcArrAddr,$outArkArrAddr, $outRcArrAddr);
    my ($ptarfile, $stem, $tarfile);
    my (%found, %listvalues, %tempOutRcHash);
    my (@parts, @tempOutArkArr);

    $outRcArrAddr  = shift;  # address to @outRcArr
    $outArcArrAddr = shift;  # address to @outArcArr
    $outArkArrAddr = shift;  # address to @outArkArr

    # %listvalues will be used to group output in monthly.rc
    #-------------------------------------------------------
    foreach (values %list) { $listvalues{$_} = 1 }
    %found = ();

    # get collection locations and file name templates from $siloarc
    #---------------------------------------------------------------
    open SILO, "< $siloarc" or die "Error during open $siloarc;";
    while (<SILO>) {
        next unless /\${PESTOROOT}/;
        next if /\+/;

        chomp($line = $_);
        foreach $name (sort keys %list) {
            next if $found{$name};

            $mline = matchline($name, $line);
            next unless $mline;

            # modified line for $outRc file
            #------------------------------
            ($mline1 = $mline) =~ s|\${PESTOROOT}\%s/||;
            $mline1 =~ s|%s|\${EXPID}|;
            next if $mline1 eq $mline;

            $tempOutRcHash{$mline1} = $list{$name};
            $found{$name} = 1;

            # line stem for $outArk file
            #---------------------------
            if ($mline =~ /(^.*$name\b)/) {
                $stem = $1;
                push @tempOutArkArr, $stem;
            }
        }
    }
    close SILO;

    # add additional hourtype requirements
    #-------------------------------------
    foreach (qw(bkg_clcv_rst)) {
        $hourtype{$_} = "$mode{$_}$freq{$_}";
    }

    # entries for monthly.rc file
    #----------------------------
    @$outRcArrAddr = ();
    $maxlen = maxlength(keys %tempOutRcHash)+4;
    foreach $listflags (sort keys %listvalues) {
        foreach $mline (sort keys %tempOutRcHash) {
            if ($tempOutRcHash{$mline} eq $listflags) {
                $name = "";
                $name = $1 if $mline =~ /\${EXPID}\.(.*)\.%y/;

                $mline = "#".$mline if $listflags =~ m/C/;
                ($flags = $listflags) =~ s/C//;

                $htype = $hourtype{$name};
                if ($htype) {
                    $format = "%-${maxlen}s%-6s%s";
                    $mline = sprintf($format, $mline, $flags, $htype);
                }
                else {
                    $format = "%-${maxlen}s%s";
                    $mline = sprintf($format, $mline, $flags);
                }
                push @$outRcArrAddr, $mline;
            }
        }
        push @$outRcArrAddr, "";
    }

    # entries for monthly.arc and monthly.keep.arc file
    #--------------------------------------------------
    @$outArcArrAddr = ();
    @$outArkArrAddr = ();

    foreach $stem (sort @tempOutArkArr) {

        $tarfile = "$stem.%y4%m2.tar";
        $ptarfile = "$stem.%y4%m2.partial.tar";
        push @$outArcArrAddr, $tarfile;
        push @$outArcArrAddr, $ptarfile;

        #---------------------------------------------------------------
        # uncomment the following lines to exclude from the archive list
        # the monthly products which we think will never be created
        #---------------------------------------------------------------
        #--@parts = split /[.]/, $stem;
        #--$name = $parts[-1];
        #--next unless means_type($name);
        #---------------------------------------------------------------

        $diurnal = "$stem.diurnal.%y4%m2.nc4";
        $monthly = "$stem.monthly.%y4%m2.nc4";

        push @$outArkArrAddr, $diurnal;
        push @$outArkArrAddr, $monthly;
        push @$outArkArrAddr, "#";
    }

    # final entries for monthly.arc file
    #-----------------------------------
    $etcdir = "\${PESTOROOT}%s/etc/Y%y4/M%m2";

    $fetchlog = "$etcdir\/%s.prefetch.%y4%m2.log.txt.%s.tar";
    $mmlog = "$etcdir\/%s.means.%y4%m2.log.txt.%s.tar";
    $cleanlog = "$etcdir\/%s.tarandclean.%y4%m2.log.txt.%s.tar";

    push @$outArcArrAddr, "#";
    push @$outArcArrAddr, $fetchlog;
    push @$outArcArrAddr, $mmlog;
    push @$outArcArrAddr, $cleanlog;
    push @$outArcArrAddr, "#";
}

#=======================================================================
# name - matchline
# purpose - determine whether a product or collection name matches
#           a pattern within a line taken from silo.arc
#
# input parameters
# => $name: product name to match within $line
# => $line: line from silo.arc file
#
# return value (if match is found)
# => $mline: modified $line with %c tokens in product name pattern
#            replaced by actual characters from $name
# notes:
# 1. This routine looks for $name sandwiched between "%s." and ".%y" in $line
# 2. A %c token in $line can match any single character within $name
# 3. Nothing is returned if no match is found.
#=======================================================================
sub matchline {
    my ($name, $line) = @_;
    my ($matchfound, $mline, $ll, $nn, $l_char, $n_char);

    $line =~ s/%c/?/g;
    $name = "%s.$name.%y";
    $matchfound = 0;

    for (0..length($line)-1) {
        $ll = $_;
        $nn = 0;
        $mline = $line;

        while (1) {
            $l_char = substr($line, $ll, 1);
            $n_char = substr($name, $nn, 1);

            if ($l_char eq "?") {
                substr($mline, $ll, 1) = $n_char;
                $l_char = $n_char;
            }
            last unless $l_char eq $n_char;

            if (++$nn >= length($name)) {
                $matchfound = 1;
                last;
            }
            last if ++$ll >= length($line);
        }
        last if $matchfound;
    }
    $mline =~ s/\?/%c/g;
    return $mline if $matchfound;
}

#=======================================================================
# name - maxlength
# purpose - get maximum length of string array entries
#=======================================================================
sub maxlength {
    my @strArr = @_;
    my $max = 0;
    foreach (@strArr) { $max = length($_) if length($_) > $max }
    return $max;
}

#=======================================================================
# name - write_output
# purpose - write array output to $outfile
#
# input parameter
# => $outfile: name of output file
# => @outArr: array containing lines to write to $outfile
#=======================================================================
sub write_output {
    use File::Copy;
    my ($outfile, @outArr, $outfile_save);

    ($outfile, @outArr) = @_;

    # check for pre-existing file
    #----------------------------
    $outfile_save = "$outfile~";
    move $outfile, $outfile_save if -e $outfile;

    # write output
    #-------------
    open OUTF, "> $outfile" or die "Error during open $outfile;";

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    if ($outfile eq $outRc) {
        print OUTF <<"EOF1";
# Monthly Post Processing Resource file
#--------------------------------------
# NOTES:
#
# 1. This file is written by the $script script based on information in the
#    experiment HISTORY.rc.tmpl and silo.arc files and lists of analysis and
#    aod outputs in $script.
#
# 2. Each record contains the directory path template for a data collection
#    from the HISTORY.rc file and a processing flag where
#
#        M: calculate monthly means
#        T: tar monthly files
#        P: produce monthly plots (requires monthly plots available)
#
#    For example,
#
#    > diag/Y%y4/M%m2/\${EXPID}.tavg1_2d_ocn_Nx.%y4%m2%d2_%h2%n2z.nc4  MTP
#    > diag/Y%y4/M%m2/\${EXPID}.tavg3_3d_udt_Np.%y4%m2%d2_%h2%n2z.nc4  MT
#    > chem/Y%y4/M%m2/\${EXPID}.inst3_3d_chm_Nv.%y4%m2%d2_%h2%n2z.nc4  T
#
# 3. filetype and hourtype are extracted from the filename template.
#
#        template = \${EXPID}.tavg1_2d_ocn_Nx.%y4%m2%d2_%h2%n2z.nc4
#        filetype =          tavg1_2d_ocn_Nx
#        hourtype =          tavg1
#
# 4. An alternate hourtype can be specified by adding it to the end of the record.
#
#    For example
#
#    > diag/Y%y4/M%m2/\${EXPID}.tavg1_2d_ocn_Nx.%y4%m2%d2_%h2%n2z.nc4  MTP  daily
#    > diag/Y%y4/M%m2/\${EXPID}.tavg3_3d_udt_Np.%y4%m2%d2_%h2%n2z.nc4  MT
#    > chem/Y%y4/M%m2/\${EXPID}.inst3_3d_chm_Nv.%y4%m2%d2_%h2%n2z.nc4  T
#
# 5. One can add or remove the 'M', 'T', and/or 'P' flags for individual records.
#    Alternatively, all flags can be turned off by commenting the line.
#
# 6. Monthly means processing will fail if the script cannot find inputs for
#    each uncommented product with the 'M' flag.
#
# 7. Monthly plots will fail unless the monthly means are available at plot time.
#
# 8. The 'P' flag does not control the plotting program directly, but rather it
#    controls what goes into the plot HISTORY.rc_tmpl file. Therefore, changes
#    to the 'P' flag must be accompanied by the recreation of HISTORY.rc_tmpl.
#--------------------------------------------------------------------------------
EOF1
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    elsif ($outfile eq $outArc) {
        print OUTF <<"EOF2";
# Monthly post processing archive resource file
#----------------------------------------------
# NOTE:
#  This file is written by the $script script based on
#  information in the experiment HISTORY.rc.tmpl and silo.arc files
#  and lists of analysis and aod outputs in $script.
#--------------------------------------------------------------------------------
EOF2
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    elsif ($outfile eq $outArk) {
        print OUTF <<"EOF3";
# Monthly post processing archive resource file
# (for files which should not be deleted after they are archived)
#----------------------------------------------------------------
# NOTE:
#  This file is written by the $script script based on
#  information in the experiment HISTORY.rc.tmpl and silo.arc files
#  and lists of analysis and aod outputs in $script.
#--------------------------------------------------------------------------------
EOF3
    }
    foreach (@outArr) { print OUTF "$_\n" }
    close OUTF;
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {

    print <<"EOF";

usage: $script [options]

description: Write monthly post processing resource and archive resource files
             based on information in the experiment HISTORY and archive resource
             files.

OPTIONS [default values in brackets]

INPUT/OUTPUT OPTIONS
   -H histfile     name of input HISTORY resource file ["HISTORY.rc.tmpl"]
   -inarc siloarc  name of input experiment archive resource file ["silo.arc"]
   -outdir outdir  name of output directory ["."]

PROCESSING OPTIONS [see note #1]
   -rc             write monthly.rc file
   -arc            write monthly.arc file
   -ark            write monthly.keep.arc file

OUTPUT FILENAME OPTIONS [see note #2]
   -outrc outRc    name of output monthly rc file ["monthly.rc"]
   -outarc outArc  name of output monthly arc file ["monthly.arc"]
   -outark outArk  name of output monthly keep arc file ["monthly.keep.arc"]

OTHER OPTIONS
   -nc             do not comment the analysis and aod lines in monthly.rc
   -h              print usage information

note:
1. If no PROCESSING OPTIONS are specified, then all outputs will be written;
   otherwise, only the specified output(s) will be written
2. If directory paths are included in the OUTPUT FILENAME OPTIONS, then there is
   no need to also include the -outdir flag and value. However, if a output
   directory is specified by the -outdir flag, then any directory paths in the
   OUTPUT FILENAME OPTIONS will be ignored.

EOF
exit;
}
