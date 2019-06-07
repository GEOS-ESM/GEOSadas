#!/usr/bin/perl -w
#=======================================================================
# name - ceres_edit.pl
# purpose - This interactive script will make file modifications needed
#           for a CERES run.
#
# notes
# 1. Specifically, this script will modify the following files within the
#    experiment run directory:
#    - (g5das job file)
#    - gmao_global_convinfo.rc
#    - gsi.rc.tmpl
# 2. See usage message at bottom for run flag information.
#
# !Revision History
#  ----------------
# 24Jun2008   Stassi    Initial version of code.
#=======================================================================
use strict;

# global variables
#-----------------
my ($name, $fvrun, $jfname);

# main program
#-------------
{
    &init();
    &edit_jobfile();
    &edit_convinfo();
    &edit_gsirc();
}

#=======================================================================
# name - init
# purpose - set jobfile name; get runtime flags; initialize @exclude array
#=======================================================================
sub init {
    use File::Basename;
    use Getopt::Long;
    my $help;

    $name = basename $0;

    GetOptions("d=s"        => \$fvrun,
               "j=s"        => \$jfname,
               "h"          => \$help,
               "help"       => \$help);
    &usage() if $help;
    die ">>> Error <<< jobfile (-j) not given;" unless $jfname;

    # defaults
    #---------
    $fvrun = "." unless $fvrun;
    die ">>> Error <<< directory not found; $fvrun: $!" unless (-d $fvrun);

    # introductory label
    #-------------------
    print "\nModifications for CERES processing"
        . "\n----------------------------------\n";
    print "directory: $fvrun\n\n" unless ($fvrun eq ".");
}

#=======================================================================
# name - edit_jobfile
# purpose - edit jobfile for ceres processing
#=======================================================================
sub edit_jobfile {
    use File::Basename;

    my ($jf, $jforig);
    my (@exclude, $excl, $cnt);
    my ($file1, $file2);
    
    # check for existence of file
    #----------------------------
    $jf = "$fvrun/$jfname";
    unless (-e $jf) { 
        warn ">>> Error <<< name: cannot find job file: $jf;";
        return;
    }

    # rename original before modifying
    #---------------------------------
    $jforig = "$jf.orig";
    &renamefile($jf, $jforig);

    # entries to exclude from the OBSCLASS listing;
    # note: merra_wspd_bufr was originally included in this list
    #       but then removed as per direction from R.Lucchesi.
    #--------------------------------------------------------
    @exclude = qw( merra_goesnd_prep_bufr
                   merra_nmodis_prep_bufr
                   merra_ers2
                   merra_airs_bufr );

    # open files
    #-----------
    open ORIG, "< $jforig" or die ">>> Error <<< opening $jforig: $!";
    open JF,   "> $jf"     or die ">>> Error <<< opening $jf: $!";

    # write lines from original to new
    #---------------------------------
    $cnt = 0;
    foreach (<ORIG>) {
        foreach $excl (@exclude) { $cnt++ if s/${excl} *,*// }
        print JF or die ">>> Error <<< while writing to $jf: $!";
    }

    $file1 = basename $jforig;
    $file2 = basename $jf;
    print "$cnt edit(s) in file: $file1 -> $file2\n\n";
    close ORIG;
    close JF;
}

#=======================================================================
# name - edit_convinfo
# purpose - edit the gmao_global_convinfo.rc file for ceres processing
#=======================================================================
sub edit_convinfo {
    my ($ci, $ciorig, $cnt);
    my ($file1, $file2);

    # check for existence of file
    #----------------------------
    $ci = "$fvrun/gmao_global_convinfo.rc";
    unless (-e $ci) { 
        warn ">>> Error <<< cannot find rc file: $ci;";
        return;
    }

    # rename original before modifying
    #---------------------------------
    $ciorig = "$ci.orig";
    &renamefile($ci, $ciorig);

    # open files
    #-----------
    open ORIG, "< $ciorig" or die ">>> Error <<< opening $ciorig: $!";
    open CI,   "> $ci"     or die ">>> Error <<< opening $ci: $!";

    # write lines from original to new
    #---------------------------------
    $cnt = 0;
    foreach (<ORIG>) {
        $cnt++ if s/(uv\s+224\s+) 1/$1-1/;
        $cnt++ if s/(uv\s+257\s+) 1/$1-1/;
        $cnt++ if s/(uv\s+258\s+) 1/$1-1/;
        $cnt++ if s/(uv\s+259\s+) 1/$1-1/;
        print CI or die ">>> Error <<< while writing to $ci: $!";
    }

    $file1 = basename $ciorig;
    $file2 = basename $ci;
    print "$cnt edit(s) in file: $file1 -> $file2\n\n";
    close ORIG;
    close CI;
}

#=======================================================================
# name - edit_gsirc
# purpose - edit the gsi.rc.tmpl file for ceres processing
#=======================================================================
sub edit_gsirc {
    my ($gs, $gsorig, $cnt);
    my ($dum, $dum2);
    my ($file1, $file2);

    # check for existence of file
    #----------------------------
    $gs = "$fvrun/gsi.rc.tmpl";
    unless (-e $gs) { 
        warn ">>> Error <<< cannot find rc file: $gs;";
        return;
    }

    # rename original before modifying
    #---------------------------------
    $gsorig = "$gs.orig";
    &renamefile($gs, $gsorig);

    # open files
    #-----------
    open ORIG, "< $gsorig" or die ">>> Error <<< opening $gsorig: $!";
    open GS,   "> $gs"     or die ">>> Error <<< opening $gs: $!";

    # write lines from original to new
    #---------------------------------
    $cnt = 0;
    $dum = "'DUMMY'";

    foreach (<ORIG>) {
        $cnt++ if s/dfile\(5\)=\S+,/dfile(5)='DUMMY',/;
        $cnt++ if s/dfile\(6\)=\S+,/dfile(6)='DUMMY',/;
        $cnt++ if s/dfile\(83\)=\S+,/dfile(83)='DUMMY',/;
        $cnt++ if s/dfile\(91\)=\S+,/dfile(91)='DUMMY2',/;
        $cnt++ if s/dfile\(92\)=\S+,/dfile(92)='DUMMY2',/;
        $cnt++ if s/dfile\(93\)=\S+,/dfile(93)='DUMMY2',/;
        print GS or die ">>> Error <<< while writing to $gs: $!";
    }

    $file1 = basename $gsorig;
    $file2 = basename $gs;
    print "$cnt edit(s) in file: $file1 -> $file2 "
        . "(edited lines may not differ)\n\n"; # since substituting for \S+
    close ORIG;
    close GS;
}

#=======================================================================
# name - renamefile
# purpose - rename file from one name to another
#
# input parameters
# - $filename: file to rename
# - $newname:  new file name
#=======================================================================
sub renamefile {
    use File::Basename;

    my ($filename, $newname, $ans, $yes);
    my ($file1, $file2);

    $filename = shift @_;
    $newname  = shift @_;
    $yes = 1;

    if (-e $newname) {
        {
            undef $ans;
            $yes = 0;

            print "$newname already exists; overwrite (y/n)? [n] ";
            chomp($ans = lc <STDIN>);
            $ans = "n" if ($ans =~ /^\s*$/);
            redo unless ($ans eq "n" or $ans eq "y");
            $yes = 1 if ($ans eq "y");
        }            
    }
    if ($yes) {
        $file1 = basename $filename;
        $file2 = basename $newname;
        print "rename $file1, $file2\n";
        rename $filename, $newname;
    }
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    print <<"EOF";

usage: $name [-d dirname] -j jobfile
       $name -h[elp]

  where

    -d dirname      location of files to be modified (experiment run directory)
    -j jobfile      name of experiment job file or other file containing
                    OBSCLASS information

    -h[elp]         prints this usage message

EOF
exit;
}
