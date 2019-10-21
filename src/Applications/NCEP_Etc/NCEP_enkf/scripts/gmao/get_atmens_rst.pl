#!/usr/bin/env perl
use strict;
use warnings;
use Cwd qw(cwd);
use File::Basename qw(basename dirname);
use File::Copy qw(cp mv);
use File::Path qw(mkpath rmtree);
use FindBin qw($Bin);
use lib "$Bin";
use Manipulate_time qw(tick);

# flush STDOUT buffer
#--------------------
$| = 1;

# global variables
#-----------------
my ($atmens_dir);
my ($arcdir, $expid, $newid, $yyyymmdd, $yyyy, $mm, $hh);

my %vopts = ( "verbose" => 1 );

# main program
#-------------
{
    my ($pwd);
    my ($three_hr_sec, $ymd3, $hms3, $yyyy3, $mm3, $hh3);
    my ($atmens_date_dir, $atmens_date3_dir);
    my ($rdnperts_dates3_txt);
    my ($atmens_stat_dir, $atmens_ebkg_dir, $atmens_erst_dir, $atmens_ecbkg_dir);
    my ($tarfile, $tarpath, $label, $pid);
    my ($ens, $mem, $mfile, $mfile_new);
    my (@tarList);
    
    init();
    chdir($atmens_dir);
    system_("touch .no_archiving");
    $pwd = cwd();

    $three_hr_sec = 3 * 60 * 60;
    ($ymd3, $hms3) = tick($yyyymmdd, "${hh}0000", $three_hr_sec);
    ($yyyy3, $mm3) = ( $ymd3 =~ m/^(\d{4})(\d{2})\d{2}$/ );
    ($hh3) = ( $hms3 =~ m/^(\d{2})\d{4}$/ );

    $atmens_date_dir = "$arcdir/$expid/atmens/Y$yyyy/M$mm";
    $atmens_date3_dir = "$arcdir/$expid/atmens/Y$yyyy3/M$mm3";

    $rdnperts_dates3_txt = "$expid.rndperts.dates.${ymd3}_${hh3}z.txt";
    cp_("$atmens_date3_dir/$rdnperts_dates3_txt", $pwd);
    rename_new($rdnperts_dates3_txt);

    foreach $label ("stat", "ebkg", "ecbkg", "erst") {
        $tarfile = "$expid.atmens_$label.${yyyymmdd}_${hh}z.tar";
        $tarpath = "$atmens_date_dir/$tarfile";
        push @tarList, $tarpath;
    }

    defined($pid = fork) or die "Error while attempting to fork;";
    unless ($pid) {
        system "dmget @tarList";
        exit;
    }
    foreach $tarpath (@tarList) { system_("tar xvf $tarpath") }

    $atmens_stat_dir = "$expid.atmens_stat.${yyyymmdd}_${hh}z";
    foreach $ens (<$atmens_stat_dir/ens*>) { mv_($ens, $pwd) }
    foreach $ens (<ens*>) { rename_new($ens) }
    rmtree($atmens_stat_dir, \%vopts) or die "Error; rmtree $atmens_stat_dir;";

    $atmens_ebkg_dir = "$expid.atmens_ebkg.${yyyymmdd}_${hh}z";
    foreach $mem (<$atmens_ebkg_dir/mem*>) { mv_($mem, $pwd) }
    rmtree($atmens_ebkg_dir, \%vopts) or die "Error; rmtree $atmens_ebkg_dir;";

    $atmens_erst_dir  = "$expid.atmens_erst.${yyyymmdd}_${hh}z";
    $atmens_ecbkg_dir = "$expid.atmens_ecbkg.${yyyymmdd}_${hh}z";
    foreach $mem (<mem0*>) {
        next unless -d $mem;
        foreach $mfile (<$atmens_erst_dir/$mem/${expid}*>)  { mv_($mfile, $mem) }
        foreach $mfile (<$atmens_ecbkg_dir/$mem/${expid}*>) { mv_($mfile, $mem) }
        rename_new($mem)
    }
    rmtree($atmens_erst_dir) or die "Error; rmtree $atmens_erst_dir;";
    rmtree($atmens_ecbkg_dir) or die "Error; rmtree $atmens_ecbkg_dir;";
}

#=======================================================================
# name - init
# purpose - get runtime parameters and flags
#=======================================================================
sub init {
    use Getopt::Long qw(GetOptions);
    my ($fvhome, $help, $exparcdir);

    GetOptions("fvhome=s" => \$fvhome,
               "h"        => \$help);
               
    usage() if $help;

    # get runtime parameters
    #-----------------------
    usage() unless scalar(@ARGV) == 4;
    ($exparcdir, $newid, $yyyymmdd, $hh) = @ARGV;

    $exparcdir =~ s/[\s\/]*$//;
    $arcdir = dirname($exparcdir);
    $expid = basename($exparcdir);

    if ($yyyymmdd =~ m/^(\d{4})(\d{2})\d{2}$/) {
        $yyyy = $1;
        $mm = $2;
    }
    else { die "Error; cannot decipher yyyymmdd = $yyyymmdd;" }

    # set FVHOME
    #-----------
    $fvhome = "$ENV{NOBACKUP}/$newid" unless $fvhome;
    die "Error. Cannot find fvhome directory: $fvhome;" unless -d $fvhome;

    # set directory location for ensemble restarts
    #---------------------------------------------
    $atmens_dir = "$fvhome/atmens";
    mkpath($atmens_dir, \%vopts) unless -d $atmens_dir;
    
}

#=======================================================================
# name - rename_new
#=======================================================================
sub rename_new {
    my ($name, $newname, $dirname);
    $name = shift @_;

    if (-f $name) {
        ($newname = $name) =~ s/$expid\./$newid\./;
        mv_($name, $newname);
    }

    elsif (-d $name) {
        $dirname = $name;
        foreach $name (<$dirname/$expid.*>) {
            ($newname = $name) =~ s/$expid\./$newid\./;
            mv_($name, $newname);
        }
    }
}

#=======================================================================
# name - cp_
# purpose - wrapper to sub cp() with an echo
#=======================================================================
sub cp_ {
    my ($source, $target) = @_;
    cp($source, $target);
    print "cp($source, $target)\n";
}

#=======================================================================
# name - mv_
# purpose - wrapper to sub mv() for renaming a file
#                    or sub rename() for renaming a directory
#=======================================================================
sub mv_ {
    my ($source, $target) = @_;

    if (-d $source and -f $target) {
        die "Error. Cannot mv dir to file: mv($source, $target);";
    }

    if (-f $source) {
        mv($source, $target);
        print "mv($source, $target)\n";
    }
    else {
        $target = "$target/" . basename($source) if -d $target;
        rename($source, $target);
        print "rename($source, $target)\n";
    }
}

#=======================================================================
# name - system_
# purpose - wrapper to sub system() with an echo
#=======================================================================
sub system_ {
    my $cmd = shift @_;
    print ("$cmd\n");
    system($cmd);
}

#=======================================================================
# name - pause
# purpose - 
#=======================================================================
sub pause {
    print "hit <cr> to continue ... ";
    my $dummy = <STDIN>;
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    my $name = basename($0);
    print << "EOF";

purpose: copy initial ensemble restarts from an existing experiment

usage: $name arcdir newid yyyymmdd hh [options]

where
   exparcdir = archive location of experiment directory with existing ensemble restarts
   newid     = id for new experiment
   yyyymmdd  = starting date for new experiment
   hh        = starting hh for new experiment

options
   -fvhome FVHOME    home directory for new experiment [\$NOBACKUP/\$newid]
   -h                print usage information

EOF
;
    exit();
}
