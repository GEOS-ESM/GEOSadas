#!/usr/bin/env perl
#
# swell_setup.pl - install the swell layer into a GEOS-ADAS experiment (called by fvsetup)
#
# usage: swell_setup.pl -fvhome DIR -fvroot DIR [-set KEY=VALUE ...]
#                       [-mkiau IM,JM,LM] [-rst4fcst ARCHIVE,EXPID]
#
#   -set KEY=VALUE      experiment setting written to $FVHOME/run/swell/swell_config.csh
#                       (repeatable; a later call updates the same key)
#   -mkiau IM,JM,LM     install $FVHOME/run/swell/mkiau.rc.tenv for the AGCM grid
#   -rst4fcst ARCH,EXP  in $FVHOME/fcst/rst4fcst.acq, replace the GSI IAU (agcm_import_rst)
#                       by the JEDI IAU archive (jedi_agcmrst); the restart line stays
#
# swell_config.csh is copied from $FVROOT/etc/swell once; the experiment settings live in
# a marked block at its top as "if ( ! $?KEY ) setenv KEY VALUE", ahead of the defaults,
# so they can still be overridden from the environment at run time.
#=========================================================================================

use strict;
use warnings;
use File::Copy qw(copy);
use File::Path qw(make_path);
use Getopt::Long;

my ($fvhome, $fvroot, @sets, $mkiau, $rst4fcst);
GetOptions( "fvhome=s"   => \$fvhome,
            "fvroot=s"   => \$fvroot,
            "set=s"      => \@sets,
            "mkiau=s"    => \$mkiau,
            "rst4fcst=s" => \$rst4fcst ) or usage();
usage() unless ( $fvhome and $fvroot );

my $etc    = "$fvroot/etc/swell";
my $rundir = "$fvhome/run/swell";
make_path($rundir);

set_config(@sets) if @sets;
install_mkiau($mkiau) if $mkiau;
add_rst4fcst($rst4fcst) if $rst4fcst;
exit 0;

#-----------------------------------------------------------------------------------------
sub set_config {
    my (@kv) = @_;
    my $cfg = "$rundir/swell_config.csh";
    my ($beg, $end) = ("# >>> experiment settings (swell_setup.pl)",
                       "# <<< experiment settings");

    unless ( -e $cfg ) {
        copy("$etc/swell_config.csh", $cfg) or die ">>> ERROR <<< cannot copy $etc/swell_config.csh: $!\n";
    }
    open(my $in, "<", $cfg) or die ">>> ERROR <<< cannot read $cfg: $!\n";
    my @lines = <$in>;
    close($in);

    # current block (if any) and where it goes: after the leading comment header
    my (%val, @order, @rest, $ib);
    my ($inblock, $afterblock) = (0, 0);
    foreach my $line (@lines) {
        if ( $afterblock ) { $afterblock = 0; next if ( $line =~ /^\s*$/ ); }
        if ( $line =~ /^\Q$beg\E/ ) { $inblock = 1; $ib = scalar(@rest); next; }
        if ( $line =~ /^\Q$end\E/ ) { $inblock = 0; $afterblock = 1; next; }
        if ( $inblock ) {
            if ( $line =~ /setenv\s+(\w+)\s+(.*?)\s*$/ ) { push @order, $1 unless exists $val{$1}; $val{$1} = $2; }
            next;
        }
        push @rest, $line;
    }
    unless ( defined $ib ) {
        # after the header, which ends with the first "# -----" rule (and a blank line)
        $ib = 0;
        $ib++ while ( $ib < @rest and $rest[$ib] !~ /^# -{20,}/ );
        $ib = ( $ib < @rest ) ? $ib + 1 : 0;
        $ib++ if ( $ib < @rest and $rest[$ib] =~ /^\s*$/ );
    }

    foreach my $s (@kv) {
        my ($k, $v) = split(/=/, $s, 2);
        die ">>> ERROR <<< bad -set '$s' (expected KEY=VALUE)\n" unless ( defined $v and $k =~ /^\w+$/ );
        $v = "\"\"" if ( $v eq "" );
        push @order, $k unless exists $val{$k};
        $val{$k} = $v;
    }

    my @block = ("$beg\n");
    push @block, sprintf("if ( ! \$?%s ) setenv %s %s\n", $_, $_, $val{$_}) foreach @order;
    push @block, "$end\n\n";

    splice(@rest, $ib, 0, @block);
    open(my $out, ">", $cfg) or die ">>> ERROR <<< cannot write $cfg: $!\n";
    print $out @rest;
    close($out);
    print "swell_setup.pl: $cfg: " . join(" ", map { "$_=$val{$_}" } @order) . "\n";
}

#-----------------------------------------------------------------------------------------
sub install_mkiau {
    my ($grid) = @_;
    my ($im, $jm, $lm) = split(/,/, $grid);
    die ">>> ERROR <<< bad -mkiau '$grid' (expected IM,JM,LM)\n" unless ( $im and $jm and $lm );
    my ($src, $dst) = ("$etc/mkiau.rc.tenv", "$rundir/mkiau.rc.tenv");
    open(my $in, "<", $src)  or die ">>> ERROR <<< cannot read $src: $!\n";
    open(my $out, ">", $dst) or die ">>> ERROR <<< cannot write $dst: $!\n";
    while ( my $line = <$in> ) {
        $line =~ s/\@AGCM_IM/$im/g;
        $line =~ s/\@AGCM_JM/$jm/g;
        $line =~ s/\@AGCM_LM/$lm/g;
        print $out $line;
    }
    close($in);
    close($out);
    print "swell_setup.pl: $dst for ${im}x${jm}x${lm}\n";
}

#-----------------------------------------------------------------------------------------
sub add_rst4fcst {
    my ($arg) = @_;
    my ($archive, $expid) = split(/,/, $arg);
    die ">>> ERROR <<< bad -rst4fcst '$arg' (expected ARCHIVE,EXPID)\n" unless ( $archive and $expid );
    $archive =~ s|/+$||;
    my $acq  = "$fvhome/fcst/rst4fcst.acq";
    my $line = "$archive/$expid/jedi/rs/Y%y4/M%m2/$expid.jedi_agcmrst.%y4%m2%d2_%h2z.tar";

    # the forecast restarts stay; the GSI IAU (agcm_import_rst) gives way to the JEDI one
    unless ( -e $acq ) {
        print "swell_setup.pl: no $acq, forecast acquire not changed\n";
        return;
    }
    open(my $in, "<", $acq) or die ">>> ERROR <<< cannot read $acq: $!\n";
    my @lines = grep { !/\.agcm_import_rst\./ and !/\.jedi_agcmrst\./ } <$in>;
    close($in);
    open(my $out, ">", $acq) or die ">>> ERROR <<< cannot write $acq: $!\n";
    print $out @lines, "$line\n";
    close($out);
    print "swell_setup.pl: $acq acquires the JEDI IAU forcing\n";
}

#-----------------------------------------------------------------------------------------
sub usage {
    print "usage: swell_setup.pl -fvhome DIR -fvroot DIR [-set KEY=VALUE ...]\n"
        . "                      [-mkiau IM,JM,LM] [-rst4fcst ARCHIVE,EXPID]\n";
    exit 1;
}
