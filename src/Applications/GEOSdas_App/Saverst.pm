package Saverst;
#########################################################################
#
# name - Saverst
#
# purpose -
#   This package provides routines to write, read, and query the
#   saverst.rc resource file.
#
# !Revision History
#
# 09Jun2008  Stassi    Package added to CVS repository.
#
#########################################################################
use strict;
use File::Basename;
require Exporter;
our @ISA = "Exporter";
our @EXPORT_OK = qw ( writeSaverst
                      readSaverst
                      getSavehrs
                      getSavestr
                      querySaverst
                      saverstName
                      saverstStatus
                      setquietSaverst );

# global variables
#-----------------
my ($callscript, $quiet);
my ($saverst, $file_written, $file_read);
my ($savestring, @savehrs);

# initialize global variables
#----------------------------
$callscript = basename $0;
$quiet = 0;

$saverst = "";
$file_written = 0;
$file_read    = 0;


#=======================================================================
# name - writeSaverst
# purpose - write the saverst.rc file
#
# input parameters
#   $inputline: string variable containing restart hours to save [default -1]
#   $dir: (optional) directory where saverst file will be written [default "."]
#   $filename: (optional) alternate name for saverst file
#=======================================================================
sub writeSaverst {
    my ($inputline, $dir, $filename);

    # input parameters
    #-----------------
    $inputline = shift @_;
    $dir       = shift @_;
    $filename  = shift @_;

    # defaults
    #---------
    $inputline = "-1"         unless $inputline;
    $dir       = "."          unless $dir;
    $filename  = "saverst.rc" unless $filename;
    @savehrs = ();

    # set global variables
    #---------------------
    $saverst = "$dir/$filename";
    ($savestring, @savehrs) = &clean($inputline);

    # write saverst file
    #-------------------
    print "$callscript: Writing to $saverst: $savestring\n" unless $quiet;
    open SAVERST, "> $saverst" or die ">>> Error <<< opening $saverst: $!";
    print SAVERST "$savestring\n" or die ">>> Error <<< writing $saverst: $!";
    close SAVERST or warn ">> Warning << error closing $saverst: $!";
    $file_written = 1;
    $file_read    = 0;
}

#======================================================================
# name - readSaverst
# purpose - read 2-digit hour values from saverst.rc file
#
# input parameters
#   $dir: (optional) directory location of saverst.rc file [default "."]
#   $filename: (optional) alternate name for saverst.rc file
#
#=======================================================================
sub readSaverst {
    my ($dir, $filename);
    my (@lines, $line);

    $dir = shift @_;
    $dir = "." unless $dir;

    $filename = shift @_;
    $filename = "saverst.rc" unless $filename;
    $saverst  = "$dir/$filename";

    # open file
    #----------
    print "$callscript: Reading $saverst\n" unless $quiet;
    open SAVERST, "< $saverst" or die ">>> ERROR <<< cannot open $saverst: $!";

    # read saverst file
    #------------------
    while (<SAVERST>) {
        if (/\@/) { last };
        chomp($line = $_);
        push @lines, $line;
    }
    close SAVERST or warn ">> Warning << problem closing $saverst: $!";
    ($savestring, @savehrs) = &clean(@lines);
    print "$callscript: $saverst contents: $savestring\n" unless $quiet;

    $file_read    = 1;
    $file_written = 0;
}

#=======================================================================
# name - getSavehrs
# purpose - return global @savehrs array
#=======================================================================
sub getSavehrs {

    die ">>> Error <<< querying saverst file before writing or reading it;"
        unless (($file_written) or ($file_read));

    return @savehrs;
}

#=======================================================================
# name - getSavestr
# purpose - return value of global $savestring variable
#=======================================================================
sub getSavestr {

    die ">>> Error <<< querying saverst file before writing or reading it;"
        unless ($file_written or $file_read);

    if ($savestring =~ /\@/) { return "-1"; }
    else                     { return $savestring; }
}

#=======================================================================
# name - querySaverst
# purpose - check whether an hour is included in the saverst file
#
# inputs
# - $hour: hour to check (1- or 2-digiti)
#
# outputs
#   $found
#     = 0 (false) if hour not found in saverst
#     = 1 (true)  if hour found
#=======================================================================
sub querySaverst {
    my ($hour, $hr, $found);

    $hour = shift @_;
    $hour = "0".$hour if ($hour =~ /^\d$/);

    die ">>> Error <<< no input hour given;" unless $hour;
    die ">>> Error <<< querying saverst file before writing or reading it;"
        unless ($file_written or $file_read);

    $found = 0;
    foreach $hr (@savehrs) {
        if ($hr eq $hour) {
            print "$callscript: hour $hour found in $saverst\n" unless $quiet;
            $found = 1;
            last;
        }
    }
    return $found;
}

#=======================================================================
# name - saverstName
# purpose - return the name of the last saverst.rc file accessed
#=======================================================================
sub saverstName {
    warn ">> Warning << saverst.rc file has not been written or read;"
        unless $saverst;
    return $saverst;
}

#=======================================================================
# name - saverstStatus
# purpose - echo status of the saverst.rc file to standard output
#           and also send return status to calling program.
#
# return value
#  =0: the saverst file has neither been written nor read
#  =1: the saverst file has been written
#  =2: the saverst file has bee read
#=======================================================================
sub saverstStatus {

    if ($file_written and $file_read) {
        die ">> Error << ambiguous status of saverst.rc file;" };

    unless ($file_written or $file_read) {
        print "$callscript: the saverst.rc file has not been written or read;"
            unless $quiet;
        return 0;
    }

    if ($file_written) {
        print "$callscript: saverst file status = 'written' ($saverst)\n"
            unless $quiet;
        return 1;
    }

    if ($file_read) {
        print "$callscript: saverst file status = 'read' ($saverst)\n"
            unless $quiet;
        return 2;
    }
}

#=======================================================================
# name - clean
# purpose - filter junk out of input line(s); 
#           return single string with 2-digit hour values;
#           store 2-digit hour values in sorted global array, @savehrs
#=======================================================================
sub clean {
    my (@inputlines, $line);
    my ($cleanline, @cleanarr);
    my (@hrs, $hr);
    my ($count, $prevhr);

    # get input line(s)
    #------------------
    @inputlines = @_;

    # initialize output array
    #------------------------
    @cleanarr = ();

    # process each input line (normally just one)
    #--------------------------------------------
    foreach (@inputlines) {
        last if (/\@/ or /-1/);        # quit looking if "@" or "-1" found

        chomp($line = $_);
        $line =~ s/,/ /g;              # convert commas to spaces
        @hrs = split /\s+/, $line;     # split on spaces

        foreach $hr (@hrs) {
            if ($hr =~ /^\d$/) {       # convert 1-digit values to 2-digits
                $hr = "0".$hr;
            }

            # save 2-digit hour values in global array
            #-----------------------------------------
            push @cleanarr, $hr if ($hr =~ /^\d{2}$/);
        }
    }

    # sort global array and remove duplicate hours
    #---------------------------------------------
    @cleanarr = sort @cleanarr;
    $count    = scalar(@cleanarr);
    $prevhr   = -999;

    foreach (1..$count) {
        $hr = shift @cleanarr;
        push @cleanarr, $hr unless ($hr == $prevhr);
        $prevhr = $hr;
    }

    # reassemble 2-digit hour values into a single string
    #----------------------------------------------------
    if (@cleanarr) { $cleanline = ""; }
    else          { $cleanline = "\@SAVERSTHRS"; }

    foreach (@cleanarr) { $cleanline .= " $_" };
    return $cleanline, @cleanarr;
}

#=======================================================================
# setquietSaverst
# purpose - set the quiet flag for this package
#
# input
#  $quietflag (optional)
#    =0: turn quiet flag off
#    =1: turn quiet flag on (default)
#=======================================================================
sub setquietSaverst {
    my $quietflag;

    $quietflag = shift @_;
    $quietflag = 1 unless defined($quietflag);

    # set global variable
    #--------------------
    $quiet = $quietflag;
}

1;
