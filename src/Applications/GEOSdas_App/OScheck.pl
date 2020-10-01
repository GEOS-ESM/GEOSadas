#!/usr/bin/env perl
#=======================================================================
# name - OScheck.pl
# purpose - Routines to operate on a system file which gives information
#           about the Operating System; See usage() for details.
#=======================================================================
use strict;
use warnings;

# global variables
#-----------------
my ($blddir, $copydir, $display, $sysfname, $version, $quiet, @sysfile);

# main program
#-------------
{
    init();

    # note: none of these subroutine calls return
    #--------------------------------------------
    if ($display)  { displayfile(); exit() }
    if ($blddir)   { comparefiles()  }
    if ($copydir)  { copyfile()      }
    if ($sysfname) { printsysfname() }
    if ($version)  { printversion()  }
}

#=======================================================================
# name - init
# purpose - get runtime parameters and check for existence of system OS file
#=======================================================================
sub init {
    use Getopt::Long;
    my ($help, $sfile, $fname);

    # runtime parameters
    #-------------------
    GetOptions( "cmp=s"   => \$blddir,
                "cp=s"    => \$copydir,
                "display" => \$display,
                "sysfile" => \$sysfname,
                "v"       => \$version,
                "q"       => \$quiet, 
                "h|help"  => \$help );
    usage() if $help;
    $display = 1 unless $blddir or $copydir or $sysfname or $version;

    # global variables
    #-----------------
    foreach $fname (qw(os-release SuSE-release)) {
        $sfile = "/etc/$fname";
        push @sysfile, $sfile if -e $sfile;
    }
    die "Cannot find /etc/os-release or /etc/SuSE-release;" unless @sysfile;
}

#=======================================================================
# name - displayfile
# purpose - print file to STDOUT
#
# input arguments
# => $file: (optional) name of file to print; defaults to $sfile
#=======================================================================
sub displayfile {
    my ($file, $border);

    $file = shift @_;
    $file = $sysfile[0] unless $file;

    $border = "-"x (length($file) + 6);
    print "\nFile: $file\n$border\n";
    open OSFILE, "< $file" or die "Error opening file: $file;";
    foreach (<OSFILE>) { print $_ }
    close OSFILE;
}

#=======================================================================
# name - comparefiles
# purpose - compare system OS file to version in $blddir directory
#=======================================================================
sub comparefiles {
    my ($sfile, $myfile, $diffstatus, $status);

    foreach (@sysfile) {
        $sfile = $_;
        $myfile = "$blddir/" .basename($sfile);
        if (-e $myfile) {
            last;
        }
    }       
    checkx($myfile);

    $diffstatus = system "diff -bwi $sfile $myfile >& /dev/null";
    $status = $diffstatus >> 8;

    if ($status) {
        print "\nWarning: Files $sfile and $myfile differ\n";
        displayfile($sfile);
        displayfile($myfile);
    }
    else {
        print "Files $sfile and $myfile are equivalent.\n";
    }
    exit $status;
}

#=======================================================================
# name - copyfile
# purpose - copy system OS file to $copydir
#=======================================================================
sub copyfile {
    use File::Copy ("copy");
    my ($myfile);

    checkx($copydir, "d");
    $myfile = "$copydir/" .basename($sysfile[0]);
    if (-f $myfile) {
        warn "$myfile already exists and will not be overwritten.\n";
        exit;
    }
    copy $sysfile[0], $myfile or die "Error. cp $sysfile[0] $myfile;";
    print "$sysfile[0] copied to $myfile\n";
    exit;
}

#=======================================================================
# name - printsysfname
# purpose - print full pathname of system OS file
#=======================================================================
sub printsysfname {
    print "$sysfile[0]\n";
    exit;
}

#=======================================================================
# name - printversion
# purpose - print version number from system OS file
#=======================================================================
sub printversion {
    my $ver;

    if (check($sysfile[0])) {
        open OSFILE, "< $sysfile[0]" or die "Error opening file: $sysfile[0];";
        foreach (<OSFILE>) { $ver = $1 if /VERSION\s*=\s*(\S+)/i }
        close OSFILE;
        warn "VERSION not found in $sysfile[0];" unless defined($ver);
    }
    if ($ver) { print "$ver\n" }
    else      { print "0\n"    }
    exit;
}

#=======================================================================
# name - check
# purpose - check for existence of file or directory
#
# input arguments
# => $file: name of file or directory to check
# => $type: (optional) ="f" to check existence of files (default)
#                      ="d" to check existence of directory
# return value
# = 1 if found
# = 0 if not found
#=======================================================================
sub check {
    my ($name, $type, $tname);

    $name = shift @_;
    $type = shift @_;
    $type = "f" unless $type and $type eq "d";

    if ($type eq "f") {
        unless (-f $name) {
            warn "Warning. file not found: $name;" unless $quiet;
            return;
        }    
    }
    elsif ($type eq "d") {
        unless (-d $name) {
            warn "Warning. directory not found: $name;";
            return;
        }    
    }
    return 1;
}

#=======================================================================
# name - checkx
# purpose - wrapper for check subroutine; exit if file not found
#=======================================================================
sub checkx {
    my $found = check(@_);
    exit unless $found;
}

#=======================================================================
# name - usage
# purpose - print usage information to standard output
#=======================================================================
sub usage {
    use File::Basename;
    my $name = basename($0);

    print << "EOF";

usage: $name [option]

  options:
    -cmp dir   compare system OS file to one in the build\'s etc directory
    -cp dir    copy system OS file to dir
    -display   print full pathname and contents of system OS file to STDOUT
    -sysfile   print full pathname of system OS file
    -v         print VERSION info from system OS file to STDOUT
    -q         quiet mode; do not print warning if system OS file not found
    -h         print usage information

  Notes:
  1. The -display option is the default option if no other option is given.
  2. The -cmp option ignores differences in case and white space.

EOF
exit;
}
