#!/usr/bin/env perl
#=======================================================================
#
# name: copy_restarts.pl
#
# purpose - This script will transfer a set of restarts from one directory
#           to another by using either copy, symlink, forkcopy, or link.
#
# notes
# 1. *** THIS SCRIPT DOES NOT ATTEMPT TO VERIFY THAT THE SET OF RESTART
#    FILES IS COMPLETE. It copies only the restarts which it finds.
#
# 2. The default transfer mode is "copy". If more than one transfer mode
#    is specified, the order of priority is: copy > symlink > forkcopy > link
# 3. In noprompt mode, the operation will fail if enough information is not
#    given to specify a single set of restarts.
# 4. See usage() function the end of the script for flag information.
#
# !Revision History
# ----------------
# 09Jun2008   Stassi    Initial version of code.
# 03Mar2010   Stassi    Added -X flag to exclude individual restarts
# 08Jul2010   Stassi    Changed name from move_restarts.pl to copy_restarts.pl
#=======================================================================
use strict;
use warnings;
use Env;
use FindBin qw($Bin);
use lib ("$Bin");

# global variables
#-----------------
my ($alltar, $bkgHrFreq, $rstdir, $expid, $nymd, $nhr, $destdir, $newid);
my ($dmget, $merra2, $noana, $noprompt, @include, @exclude, $readme);
my ($mode, @pidarr);
my ($script, $tcmd);

# main program
#-------------
{
    use Verbose "v_wait";
    init();
    verify();
    transfer_restarts();
    v_wait(@pidarr) if @pidarr;
}

#=======================================================================
# name - init
# purpose - get input flags; set tcmd to symlink, copy, forkcopy, or link
#=======================================================================
sub init {
    use Cwd;
    use File::Basename;
    use File::Copy;
    use Getopt::Long;
    my ($dao, $link, $symlink, $copy, $forkcopy, $help);
    my ($year, $month, $flagsI);

    $script = basename $0;
    Getopt::Long::Configure("pass_through");
    GetOptions( "np|noprompt" => \$noprompt );
    usage() if $noprompt and ! @ARGV;

    Getopt::Long::Configure("pass_through");
    GetOptions( "alltar"      => \$alltar,
                "bf=i"        => \$bkgHrFreq,
                "d=s"         => \$rstdir,
                "dao"         => \$dao,
                "dest=s"      => \$destdir,
                "expid=s"     => \$expid,
                "ymd|nymd=s"  => \$nymd,
                "hr|nhms=s"   => \$nhr,
                "newid=s"     => \$newid,
                "dmget!"      => \$dmget,
                "merra2"      => \$merra2,
                "noana"       => \$noana,
                "r"           => \$readme,
                "link"        => \$link,
                "symlink"     => \$symlink,
                "copy"        => \$copy,
                "forkcopy"    => \$forkcopy,
                "I=s@"        => \@include,
                "X=s@"        => \@exclude,
                "h|help"      => \$help )
        or die "Error in command line arguments;";
    usage() if $help;

    # get and/or verify restart information
    #--------------------------------------
    $flagsI = "";
    $flagsI .= " -d $rstdir"    if $rstdir;
    $flagsI .= " -dao"          if $dao;
    $flagsI .= " -expid $expid" if $expid;
    $flagsI .= " -ymd $nymd"    if $nymd;
    $flagsI .= " -hr $nhr"      if $nhr;
    $flagsI .= " -noprompt"     if $noprompt;

    ($rstdir, $expid, $nymd, $nhr) = `$Bin/info_restarts.pl $flagsI`;
    die ">> Error << Unable to find restart information; " unless $rstdir;
    chomp($rstdir, $expid, $nymd, $nhr);

    # default values
    #---------------
    $destdir = cwd      unless $destdir;
    $newid   = $expid   unless $newid;
    $dmget   = 1        unless defined($dmget);
    @include = ()       unless @include;
    @exclude = ()       unless @exclude;

    # determine mode
    #---------------
    $copy = 1 unless $forkcopy or $symlink or $link;

    if ($link)     { $mode = "link";     $tcmd = \&my_link;     }
    if ($forkcopy) { $mode = "forkcopy"; $tcmd = \&my_forkcopy; }
    if ($symlink)  { $mode = "symlink";  $tcmd = \&my_symlink;  }
    if ($copy)     { $mode = "copy";     $tcmd = \&my_copy;     }

    # verify directory locations
    #---------------------------
    die ">> ERROR << restart directory does not exist: $rstdir"
        unless -d $rstdir;
    die ">> ERROR << destination directory does not exist: $destdir"
        unless -d $destdir;
}

#=======================================================================
# name - verify
# purpose - ask user to verify restarts before getting them
#=======================================================================
sub verify {
    my ($ans, $dflt);

    return if $noprompt;
    $dflt = "y";
    {
        print "\n   Please verify your selections\n"
            .   "   -----------------------------\n"
            .   "   rstdir:        $rstdir\n"
            .   "   destdir:       $destdir\n"
            .   "   expid:         $expid\n";
        print   "   newid:         $newid\n" if $newid ne $expid;
        print   "   date/time:     ${nymd}_${nhr}z\n"
            .   "   get files by:  $mode\n"
            .   "   -----------------------------\n"
            .   "   Are these correct (y/n)? [$dflt] ";
        chomp($ans = lc <STDIN>); $ans = $dflt unless $ans =~ /\S+/;
        $ans = substr($ans, 0, 1);
        redo unless $ans eq "y" or $ans eq "n";
    }

    if ($ans eq "n") {
        print "\nExiting without getting restart files.\n";
        exit;
    }
}

#=======================================================================
# name - transfer_restarts
# purpose - transfer restarts to destination directory
#=======================================================================
sub transfer_restarts {
    use File::Basename;

    my ($ans, $base, $cmd, $cnt, $dest, $echoflag, $flags, $flagsL, $flagsD);
    my ($found, $hr, $name, $newname, $overwriteall, $pid, $tarfile, $tflag);
    my (@rstnames, @rstfiles);

    $flags  = "-d $rstdir -expid $expid -ymd $nymd -hr $nhr";
    $flags .= " -noana"          if $noana;
    $flags .= " -r"              if $readme;
    $flags .= " -bf $bkgHrFreq"  if $bkgHrFreq;

    # get list of restarts
    #---------------------
    $flagsL = $flags ." -noprompt" if $noprompt;
    chomp( @rstfiles = `$Bin/list_restarts.pl $flags` );

    # check to see if files are migrated
    #-----------------------------------
    if ($dmget) {
        $flagsD = $flags ." -noprompt";

        defined($pid = fork) or die ">> Error << forking dmget_restarts;";
        unless ($pid) {
            system "$Bin/dmget_restarts.pl $flagsD";
            exit; # exiting child process
        }
    }

    # check for tarfile and remove from list if found
    #------------------------------------------------
    $tarfile = "";
    $cnt = scalar(@rstfiles);
    for (1..$cnt) {
        $name = shift @rstfiles;
        if ($name =~ m/\.tar$/) { $tarfile = $name }
        else                    { push @rstfiles, $name }
    }

    if ($tarfile) {
        print "$script: Extracting restarts from tarfile: $tarfile\n";
        @rstfiles = (`tar tf $tarfile`) if $alltar;
    }

    # check for included and excluded files
    #--------------------------------------
    @rstnames = ();
    foreach $name (@rstfiles) {
        chomp($name);

        # check for included files
        #-------------------------
        if (@include) {
            $found = 0;
            foreach (@include) {
                s/\./\\\./g;
                if ($name =~ m/$_/) {
                    $found = 1;
                    last;
                }
            }
            next unless $found;
        }

        # check for excluded files
        #--------------------------
        $found = 0;
        foreach (@exclude) {
            s/\./\\\./g;
            if ($name =~ m/$_/) {
                $found = 1;
                last;
            }
        }
        next if $found;
        push @rstnames, $name;
    }

    # loop through restart names
    #---------------------------
    $overwriteall = 0;
    foreach $name (@rstnames) {

        $base = basename $name;
        if ($expid eq $newid) {
            $newname = $base;
            $tflag = "";
            $echoflag = 0;
        }
        else {
            ($newname = $base) =~ s/\b$expid\b/$newid/g;
            $tflag = "--transform=s/$expid/$newid/g";
            $echoflag = 1;
        }
        $dest = "$destdir/$newname";

        # warn if interactively overwriting a file
        #-----------------------------------------
        unless ($noprompt or $overwriteall) {
            if (-e $dest) {
                print "$script: overwrite '$dest' (y/n/a)? [n] ";
                chomp($ans = <STDIN>);
                $overwriteall = 1 if lc $ans eq "a";
                next unless lc $ans eq "y" or $overwriteall;
            }
        }

        # extract from tarfile ...
        #--------------------------
        if ($tarfile) {
            $cmd = "tar -C $destdir -xf $tarfile $name $tflag";
            print "$cmd\n";
            system($cmd);
        }

        # ... or transfer file
        #---------------------
        else {
            &$tcmd($name, $dest, $echoflag);
        }
    }

    # check for MERRA2 aircraft bias correction restart
    #--------------------------------------------------
    if ($merra2) {
        $hr = substr($nhr, 0, 2);
        $dest = "$destdir/$newid.ana_acftbias_rst.${nymd}_${hr}z.txt";
        system("touch $dest") unless -e $dest;
    }
}

#=======================================================================
# name - my_forkcopy
# purpose - wrapper for the verbose forkcopy subroutine
#=======================================================================
sub my_forkcopy {
    use Verbose "v_forkcopy";
    my ($source, $dest, $echoflag, $pid);

    $source   = shift @_;
    $dest     = shift @_;
    $echoflag = shift @_;
    $pid = v_forkcopy($source, $dest, $echoflag);
    push @pidarr, $pid;
}

#=======================================================================
# name - my_copy
# purpose - wrapper for the verbose copy subroutine
#=======================================================================
sub my_copy {
    use Verbose "v_copy";
    my ($source, $dest, $echoflag);

    $source   = shift @_;
    $dest     = shift @_;
    $echoflag = shift @_;
    v_copy($source, $dest, $echoflag);
}

#=======================================================================
# name - my_link
# purpose - wrapper for the verbose link subroutine
#=======================================================================
sub my_link {
    use Verbose "v_link";
    my ($target, $linkname, $echoflag);

    $target   = shift @_;
    $linkname = shift @_;
    $echoflag = shift @_;
    v_link($target, $linkname, $echoflag);
}

#=======================================================================
# name - my_symlink
# purpose - wrapper for the verbose symlink subroutine
#=======================================================================
sub my_symlink {
    use Verbose "v_symlink";
    my ($target, $linkname, $echoflag);

    $target   = shift @_;
    $linkname = shift @_;
    $echoflag = shift @_;
    v_symlink($target, $linkname, $echoflag);
}

#=======================================================================
# name -usage
# purpose - print usage information
#=======================================================================
sub usage {
    print <<"EOF";

NAME
     $script

PURPOSE
     Get DAS restart files from one directory to a destination
     directory either by linking or copying.

SYNOPSIS
     $script [options]

OPTIONS
     -alltar          If restart tarfile is found, then extract everything
     -bf n            Hour frequency of bkg restarts, either 1 for every hour
                      or 3 (default) for every 3 hours
     -d dirname       Start location for the restart directory search;
                      defaults to \$ARCHIVE if defined, or "." if not
     -dao             Get OPS restarts; this option works best when accompanied
                      by flag/value option -nymd and/or -expid
     -dest dirname    Location of destination directory (defaults to local)
     -expid expid     Experiment ID of restart files
     -nymd ymd        Data year/month/day of restart files
                      (yyyymmdd format or any substring thereof)
     -nhms hour       Data hour of restart files (can be in hh or hhmmss format)
     -merra2          Touch aircraft bias correction restart if not found
     -newid newid     Replace the expid in the restart file names with this
     -nodmget         Do not launch a job to dmget files prior to copying
     -noana           Model restarts only; do not copy analysis restarts
     -np              No interactive prompts
     -I pattern       Include only restarts which have pattern as part of file name;
     -X pattern       Exclude restarts which have pattern as part of file name;
     -r               Look for README file and copy it if found
     -h               Print this usage message

ALTERNATE OPTIONS
     -ymd ymd         same as -nymd
     -hr hour         same as -nhms
     -noprompt        same as -np
     -help            same as -h

MODE OPTIONS (Choose no more than one of the following)
     -copy            Copy files (default)
     -symlink         Create symbolic link to restart files
     -forkcopy        Copy files in forked processes
     -link            Create link to files

AUTHOR
     Joe Stassi, SAIC (joe.stassi\@nasa.gov)

EOF
exit;
}
