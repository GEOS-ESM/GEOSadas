#!/usr/bin/env perl
#=======================================================================
#
# name: info_restarts.pl
#
# purpose: this script retrieves the following restart information
#          and prints it to standard output:
#           - restart directory location
#           - expid
#           - date
#           - hour
# notes:
# 1. the primary purpose of this script is to get complete restart
#    information when starting with minimal info, e.g. an abbreviated
#    restart directory location.
# 2. the script will either deduce missing information or prompt the
#    user if multiple options are available.
# 3. if -noprompt is specified, the script will not prompt for missing
#    information but will fail if all information cannot be deduced
#    from supplied parameters.
# 4. if an initial restart directory location is not provided with the
#    -d option, then the initial location will default to $ARCHIVE, if
#    it is defined, or else to "."
# 5. -ymd can take partial dates, e.g. 1214 (for dec 14) or just 14
# 6. -hr can be given in hhmmss format.
# 7. see usage at end of script for input/output information
#
# !revision history
# ----------------
# 08dec2008   stassi    initial version of code.
#
#=======================================================================
use strict;
use warnings;
use Env;
use FindBin qw($Bin);
use lib ("$Bin");

# global variables
#-----------------
my ($rstdir, $dao, $expid, $yyyymmdd, $nhr);
my ($date, $yyyymm, $year, $month);
my ($noprompt, $debug, $script, $help);
my (@fname_list);

# main program
#-------------
{
    init();
    get_fname_list();

    get_expid();
    get_yyyymm();
    get_yyyymmdd();
    get_nhr();

    print_results();
}

#=======================================================================
# name - init
# purpose - get input flags
#=======================================================================
sub init {
    use File::Basename;
    use File::Copy;
    use Getopt::Long;

    $script = basename $0;

    # default values
    #---------------
    $expid    = "";
    $date     = "";
    $nhr      = "";
    $yyyymm   = 0;
    $yyyymmdd = 0;

    # get input options
    #------------------
    GetOptions("d=s"         => \$rstdir,
               "dao"         => \$dao,
               "expid=s"     => \$expid,
               "ymd|nymd=s"  => \$date,
               "hr|nhms=s"   => \$nhr,
               "np|noprompt" => \$noprompt,
               "db|debug"    => \$debug,
               "h|help"      => \$help);
    usage() if $help;

    # extract year and month from $date
    #----------------------------------
    if ($date) {
        if (length($date) == 8) {
            $yyyymmdd = $date;
            $yyyymm = substr($date, 0, 6);
            $year   = substr($date, 0, 4);
            $month  = substr($date, 4, 2);
        }
    }

    # adjust $nhr, if necessary
    #--------------------------
    if (length($nhr) == 5) { $nhr = substr($nhr, 0, 1) };
    if (length($nhr) == 6) { $nhr = substr($nhr, 0, 2) };
    if (length($nhr) == 1) { $nhr = "0".$nhr };

    # look for OPS restarts
    #----------------------
    $rstdir = "/home/dao_ops" if $dao;

    # default location for restarts
    #------------------------------
    unless ($rstdir) {
        $ARCHIVE = $ENV{"ARCHIVE"};
        if ($ARCHIVE) {$rstdir = $ARCHIVE; }
        else          {$rstdir = ".";      }
    }

    # remove leading and trailing blanks, and trailing slash
    #-------------------------------------------------------
    $rstdir =~ s/^\s+|\s+$//g;
    $rstdir =~ s/\/*$//g;

    # verify directory locations
    #---------------------------
    die ">> ERROR << restart directory does not exist: $rstdir\n"
        unless -d $rstdir;
}

#=======================================================================
# name - get_fname_list
# purpose - Get list of rst.lcv and restart tar files to identify potential
#           set of restarts
#=======================================================================
sub get_fname_list {
    my ($lcv_pattern, $tar_pattern);

    $lcv_pattern = fname_pattern("lcv");
    $tar_pattern = fname_pattern("tar");
    if ($debug) {
        print "$script; get_yyyymmdd(): lcv_pattern = $lcv_pattern\n";
        print "$script; get_yyyymmdd(): tar_pattern = $tar_pattern\n";
    }
    @fname_list = (<$lcv_pattern*>, <$tar_pattern*>);
    weed_fname_list();
}

#=======================================================================
# name - fname_pattern
# purpose - return a string containing a pattern for finding lcv files
#           or restart tar files.
#
# input parameter
# => flag:   if eq "lcv", then return pattern for finding lcv restart files
#            if eq "tar", then return pattern for finding restart tar files
#
# return value:
# => pattern
#=======================================================================
sub fname_pattern {
    my ($pattern, $flag);
    $flag = shift @_;
    die "Error. Unknown flag = $flag;" if $flag ne "lcv" and $flag ne "tar";

    #---------------------------------------------------
    # search the following directories for rst.lcv files
    # - $rstdir
    # - $rstdir/rs/Y????/M??
    # - $rstdir/$expid/rs/Y????/M??
    #---------------------------------------------------
    $pattern = "";

    if ($expid) { $pattern = "{,$expid/}"; }
    else        { $pattern = "{,*/}"; }

    $pattern .= "run/.../archive/" if $dao;

    if ($year and $month) { $pattern = "{,${pattern}rs/Y$year/M$month/}" }
    else                  { $pattern = "{,${pattern}rs/Y????/M??/}" }

    $pattern = "$rstdir/$pattern";
    $pattern .= "$expid"     if $expid;
    $pattern .= "*rst\.lcv*" if $flag eq "lcv";
    $pattern .= "*rst*"      if $flag eq "tar";
    $pattern .= "$yyyymmdd*" if $yyyymmdd;
    $pattern .= "_${nhr}z"   if $nhr;
    $pattern .= "\.bin"      if $flag eq "lcv";
    $pattern .= "\.tar"      if $flag eq "tar";

    return $pattern;
}

#=======================================================================
# name - get_expid
# purpose - Deduce expid from list of rst.lcv and restart tar files.
#           Query user if multiple options are available.
#=======================================================================
sub get_expid {
    use File::Basename;
    my (@expidArr, $fname, $num);

    return if $expid;

    @expidArr = ();
    foreach $fname (@fname_list) {
        next if $date and $fname !~ /${date}_/;
        next if $nhr  and $fname !~ /_$nhr/;

        $expid = "";
        $expid = $1 if basename($fname) =~ /^(.*)\.rst.*/;
        push @expidArr, $expid if $expid;
    }

    if (@expidArr) {
        @expidArr = remove_duplicates(@expidArr);
        $num = scalar @expidArr;
        if ($num == 1) { $expid = $expidArr[0]; }
        else           { $expid = pickone("expid", "", @expidArr); }
    }
    weed_fname_list();
    return;
}

#=======================================================================
# name - get_yyyymm
# purpose - Deduce yyyymm from list of rst.lcv and restart tar files.
#           Query user if multiple options are available.
#=======================================================================
sub get_yyyymm {
    use File::Basename;
    my (@yyyymmArr, $fname, $num);

    return if $yyyymm;

    @yyyymmArr = ();
    foreach $fname (@fname_list) {
        next if $expid and $fname !~ /$expid/;

        $yyyymm = "";
        $yyyymm = $1 if basename($fname) =~ /\.(\d{6})\d{2}_\d{2}z\./;
        push @yyyymmArr, $yyyymm if $yyyymm;
    }

    if (@yyyymmArr) {
        @yyyymmArr = remove_duplicates(@yyyymmArr);
        $num = scalar @yyyymmArr;
        if ($num == 1) { $yyyymm = $yyyymmArr[0]; }
        else           { $yyyymm = pickone("yyyymm", "", @yyyymmArr); }
    }
    $year  = substr($yyyymm, 0, 4);
    $month = substr($yyyymm, 4, 2);
    $yyyymm = "$year$month";

    weed_fname_list();
    return;
}

#=======================================================================
# name - get_yyyymmdd
# purpose - Deduce yyyymmdd from list of rst.lcv and restart tar files.
#           Query user if multiple options are available.
#=======================================================================
sub get_yyyymmdd {
    my (@ymdArr, $fname, $ymd, $num);

    return if $yyyymmdd;

    @ymdArr = ();
    foreach $fname (@fname_list) {
        next if $yyyymm and $fname !~ /^$rstdir\//;

        $ymd = "";
        $ymd = $1 if $fname =~ /^.*\.(\d{8})_\d{2}z\.\w{3}$/;
        push @ymdArr,  $ymd if $ymd;
    }

    if (@ymdArr) {
        @ymdArr = remove_duplicates(@ymdArr);
        $num = scalar @ymdArr;
        if ($num == 1) { $yyyymmdd = $ymdArr[0]; }
        else           { $yyyymmdd = pickone("yyyymmdd", "", @ymdArr); }
    }
    weed_fname_list();
    return;
}

#=======================================================================
# name - get_nhr
# purpose - Deduce nhr from list of rst.lcv and restart tar files.
#           Query user if multiple options are available.
#=======================================================================
sub get_nhr {    
    my (@hourArr, $fname, $hour, $num);

    return if $nhr;

    @hourArr = ();
    foreach $fname (@fname_list) {
        $hour = 0;
        $hour = $1 if $fname =~ /^.*\.\d{8}_(\d{2})z\.\w{3}$/;
        push @hourArr, $hour if $hour;
    }
    if (@hourArr) {
        @hourArr = remove_duplicates(@hourArr);
        $num = scalar @hourArr;
        if ($num == 1) { $nhr = $hourArr[0]; }
        else           { $nhr = pickone("hour", "", @hourArr); }
    }
    weed_fname_list();
    return;
}

#=======================================================================
# name - print_results
# purpose - print rstdir, expid, yyyymmdd, and nhr to standard output
#=======================================================================
sub print_results {
    my ($rstdir, %rdhash, @rdkeys);

    # verify that selection has been
    # narrowed down to single directory choice
    #-----------------------------------------
    foreach (@fname_list) {
        $rstdir = dirname $_;
        $rdhash{$rstdir} = 1
    }
    @rdkeys = keys %rdhash;

    # still more than one option?
    #----------------------------
    if (scalar(@rdkeys) > 1) {
        $rstdir = pickone("rstdir", "", @rdkeys);
        @rdkeys = ( $rstdir );
    }

    # print results
    #--------------
    if (scalar(@rdkeys) == 1) {
        $rstdir = $rdkeys[0];
        #-------------------
        print "$rstdir\n";
        print "$expid\n";
        print "$yyyymmdd\n";
        print "$nhr\n";
        #-------------------
    } else {
        die ">> ERROR << Unable to deduce restart info; ";
    }
}

#=======================================================================
# name - pickone
# purpose - pick one from an array of values
#
# input parameters
# - $label: to identify what the values represent
# - $dfltval: default value to present to user when selection
# - @arr: array of values from which to choose
#
# output
# - the selected value from the array
#
# notes
# 1. Use "" for $dfltval if no default value
# 2. $dfltval defaults to first element in array if no default is given
#    or if the given $dfltval is not in the array
# 3. If input array has only one value, then it is automatically selected
#=======================================================================
sub pickone {
    my ($label, $dfltval, @arr, %arrH);
    my ($cnt, $dflt, $i);
    my ($index, $ans, $theONE);

    $label   = shift @_;
    $dfltval = shift @_;
    @arr = @_;
    $cnt = scalar @arr;

    return $arr[0] if $cnt == 1;

    if ($cnt > 1 and $noprompt) {
        return $dfltval if $dfltval;
        die ">> ERROR << noprompt mode; more than one \"$label\" available";
    }

    $theONE = "";
    foreach (@arr) { $arrH{$_} = 1 }
    while (1) {
        $dflt = 1;
        print STDERR "\nWhich $label?\n";
        for ($i=1; $i<=$cnt; $i++) {
            $index = $i-1;
            print STDERR " $i: $arr[$index]\n";
            $dflt = $i if $arr[$index] eq $dfltval;
        }
        print STDERR "Make selection [$dflt]: ";
        chomp($ans = <STDIN>); $ans =~ s/^\s+|\s+$//g;
        $ans = $dflt if $ans eq "";

        # user gives value instead of index
        #----------------------------------
        if ($arrH{$ans}) {
            $theONE = $ans;
        }

        # use gives index
        #----------------
        elsif ($ans =~/^\d+$/ and $ans >= 1 and $ans <= $cnt) {
            $index = $ans - 1;
            $theONE = $arr[$index];
        }

        return $theONE if $theONE;
    }
}

#=======================================================================
# name - remove_duplicates
# purpose - remove duplicate values from an array and then sort
#
# input parameter
# => @arr: input array
#
# output
# => sorted array with duplicates removed
#=======================================================================
sub remove_duplicates {
    my (%deja, $val, @arr);

    %deja = ();
    foreach $val (@_) {
        push @arr, $val unless $deja{$val};
        $deja{$val} = 1;
    }
    return sort(@arr);
}

#=======================================================================
# name - weed_fname_list
# purpose - weed out filenames from @fname_list if they do not meet the
#           expid and datetime criteria
#=======================================================================
sub weed_fname_list {
    use File::Basename;
    my (@newlist, $fname, $bname);

    if ($debug) {
        foreach (@fname_list) { print "fname: $_\n" }
        print scalar(@fname_list)."\n";
        pause();
    }
    @newlist = ();
    foreach $fname (@fname_list) {
        $bname = basename $fname;
        next if $expid    and $bname !~ m/$expid/;
        next if $yyyymm   and $bname !~ m/$yyyymm/;
        next if $yyyymmdd and $bname !~ m/${yyyymmdd}_/;
        next if $nhr      and $bname !~ m/_${nhr}z/;
        push @newlist, $fname;
    }
    if ($debug) {
        foreach (@newlist) { print "newlist: $_\n" }
        print scalar(@newlist)."\n";
        pause();
    }
    @fname_list = @newlist;
}

#=======================================================================
# name - pause
# purpose -
#=======================================================================
sub pause {
    print "Hit <cr> to continue ... ";
    my $dummy = <STDIN>;
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    print STDERR <<"EOF";

NAME
     $script

PURPOSE
     Find available restarts based on runtime inputs;
     query user if more than one set of restarts is available.

RETURN VALUES
     => rstdir       restart directory location
     => expid        experiment ID
     => ymd          year/month/day of restarts
     => hr           hour of restarts

SYNOPSIS
     $script [options]

OPTIONS
     -d dirname       Start location for the restart directory search;
                      defaults to \$ARCHIVE if defined, or "." if not
     -dao             Get OPS restarts; this option works best when accompanied
                      by flag/value option -nymd and/or -expid
     -expid expid     Experiment ID of restart files 
     -nymd ymd        Data year/month/day of restart files
                      (yyyymmdd format or any substring thereof)
     -nhms hour       Data hour of restart files (can be in hh or hhmmss format)
     -np              No interactive prompts
     -h               Print this usage message

ALTERNATE OPTIONS
     -ymd ymd         same as -nymd
     -hr hour         same as -nhms
     -noprompt        same as -np
     -help            same as -h

OUTPUT
     This script prints four lines to standard output.
       \$rstdir     restart directory location
       \$expid      experiment id
       \$yyyymmdd   date stamp of restarts (yyyymmdd)
       \$nhr        hour stamp of restarts

AUTHOR
     Joe Stassi, SAIC (joe.stassi\@nasa.gov)

EOF
exit;
}
