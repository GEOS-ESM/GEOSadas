#!/usr/bin/env perl
#=======================================================================
# name - update_input_IDs.pl
# purpose - update codeID and fvsetupID in testsuites *.input files
#
# revision history
# 06Dec2021  Stassi   Initial version
# 26Jan2022  Stassi   Modified to update codeID
#=======================================================================
use strict;
use warnings;
use File::Basename qw(basename dirname);
use File::Copy qw(move);
use Getopt::Long qw(GetOptions);

# global variables
#-----------------
my ($codeID, $debug, $fvsetup, $fvsetupID, $gitcmd, $noprompt, $quiet);
my (%inputList);

# main program
#-------------
{
    my ($cmd, $inputfile, $is_latest_tag_an_ancestor);
    my ($Project_WC_REVISION_HASH, $Project_WC_LATEST_TAG);

    init();
    unless (defined($fvsetupID)) {
        $cmd = "$gitcmd hash-object $fvsetup | cut -c1-10";
        chomp($fvsetupID = `$cmd`);
        if ($debug) {
            print "$cmd\n";
            print "fvsetupID = $fvsetupID\n";
        }
    }
    unless (defined($codeID)) {
        $Project_WC_REVISION_HASH = revision_hash();
        $Project_WC_LATEST_TAG = latest_tag();
        $is_latest_tag_an_ancestor =
            ancestor_check($Project_WC_LATEST_TAG, $Project_WC_REVISION_HASH);

        if ($is_latest_tag_an_ancestor == 0) {
            $codeID = $Project_WC_REVISION_HASH;
        }
        elsif ($is_latest_tag_an_ancestor == 1) {
            $codeID = $Project_WC_LATEST_TAG;
        }
        print "codeID = $codeID\n" if $debug;
    }
    foreach $inputfile (sort keys %inputList) {
        update_ID_values($inputfile)
    }
}

#=======================================================================
# name - init
# purpose - get runtime options and fvsetup command
#=======================================================================
sub init {
    my ($file, $fvsrc, $help, $inputfile, $pwd, @values);

    # get runtime options
    #--------------------
    GetOptions( "d=s"         => \$fvsrc,
                "cid=s"       => \$codeID,
                "fid=s"       => \$fvsetupID,
                "np|noprompt" => \$noprompt,
                "db|debug"    => \$debug,
                "q"           => \$quiet,
                "h|help"      => \$help );
    usage() if $help;

    unless (defined($fvsetupID)) {

        # user specifies fvsetup or where to find it
        #-------------------------------------------
        if ($fvsrc) {
            if (-f $fvsrc) {
                $fvsetup = $fvsrc;
            }
            else {
                die "Error. Cannot find directory, $fvsrc;" unless -d $fvsrc;
                $fvsetup = "$fvsrc/Applications/GEOSdas_App/fvsetup";
                $fvsetup = "$fvsrc/fvsetup" unless -e $fvsetup;
                unless (-e $fvsetup) {
                    die "Error. Cannot find fvsetup under directory, $fvsrc;";
                }
            }
        }

        # look for default fvsetup
        #-------------------------
        else {
            $pwd = `pwd -L`;
            $fvsetup = dirname($pwd) ."/fvsetup";
            die "Error. Cannot find, $fvsetup;" unless -e $fvsetup;
        }
    }

    # user supplies *.input file list
    #--------------------------------
    if (@ARGV) {
        %inputList = ();
        foreach $inputfile (@ARGV) {
            @values = split /,/, $inputfile;
            foreach $file (@values) {
                $file .= ".input" unless $file =~ m/.input$/;
                unless (-e $file) {
                    warn "WARNING: Cannot find file, $file;";
                    pause();
                    next;
                }
                $inputList{$file} = 1 if $file;
            }
        }
    }

    # or default to all *.input files in local directory
    #---------------------------------------------------
    else { foreach $file (<*.input>) { $inputList{$file} = 1 } }

    # warn if no *.input files are found
    #-----------------------------------
    unless (%inputList) {
        print "WARNING: No *.input files found.\n";
        usage();
    }

    # check git command
    #------------------
    chomp($gitcmd = `which git`);
    die "Error. Cannot find git command;" unless $gitcmd and -x $gitcmd;

    debug() if $debug;
}

#=======================================================================
# name - revision_hash
# purpose - return value of Project_WC_REVISION_HASH
#=======================================================================
sub revision_hash {
    my ($revision_hash, $cmd);
    $cmd = "$gitcmd rev-parse --verify -q --short=7 HEAD";
    chomp($revision_hash = `$cmd`);
    if ($debug) {
        print "$cmd\n";
        print "revision_hash = $revision_hash\n";
    }
    return $revision_hash;
}

#=======================================================================
# name - latest_tag
# purpose - return value of Project_WC_LATEST_TAG
#=======================================================================
sub latest_tag {
    my ($latest_tag, $cmd);
    $cmd = "$gitcmd describe --tags --abbrev=0";
     chomp($latest_tag = `$cmd`);
    if ($debug) {
        print "$cmd\n";
        print "latest_tag = $latest_tag\n";
    }
    return $latest_tag;
}

#=======================================================================
# name - ancestor_check
# purpose - return value for $is_latest_tag_an_ancestor
#
# input parameters
# => $latest_tag: 
# => $revision_hash: 
#=======================================================================
sub ancestor_check {
    my ($latest_tag, $revision_hash, $ancestor_check, $cmd);
    $latest_tag = shift @_;
    $revision_hash = shift @_;
    $cmd = "$gitcmd merge-base --is-ancestor $latest_tag $revision_hash";
     chomp($ancestor_check = `$cmd`);
    if ($debug) {
        print "$cmd\n";
        print "ancestor_check = $ancestor_check\n";
    }
    $ancestor_check = 0 unless $ancestor_check;
    return $ancestor_check;
}

#=======================================================================
# name - update_ID_values
# purpose - update the codeID and fvsetupID values in the given input file
#
# input parameters
# => $inputfile: the *.input file to update
#=======================================================================
sub update_ID_values {
    my ($inputfile, $ans, $line, $updated);
    my ($old_fvID, $old_cdID, $tilde_file, $tmpfile);

    $inputfile = shift @_;
    $tmpfile = $inputfile .".tmp";
    unlink $tmpfile if -e $tmpfile;
    $updated = 0;

    # check to see that fvsetupID is set in file
    #-------------------------------------------
    open (NPUT, "< $inputfile") or die "Error(1) opening file, $inputfile";
    if ($fvsetupID) {
        unless (grep { /^\s*fvsetupID:/ } <NPUT> ) {
            if ($noprompt) {
                $ans = "y";
            }
            else {
                print "\nWARNING: fvsetupID value is not set in $inputfile.\n";
                print "$inputfile: (ADD LINE?) fvsetupID: $fvsetupID";
                print " (y/n) [y]: ";
                chomp($ans = <STDIN>);
                if (lc($ans) eq "a") { $ans = "y"; $noprompt = 1 }
            }
            if ($ans eq "" or lc($ans) eq "y") {
                system("sed -i 5i'fvsetupID: $fvsetupID' $inputfile > $tmpfile");
                print "$inputfile: UPDATED\n" unless $quiet;
                $updated = 1;
            }     
        }
    }
    close NPUT;

    # check to see that codeID is set in file
    #----------------------------------------
    open (NPUT, "< $inputfile") or die "Error(2) opening file, $inputfile";
    if ($codeID) {
        unless (grep { /^\s*codeID:/ } <NPUT> ) {
            if ($noprompt) {
                $ans = "y";
            }
            else {
                print "\nWARNING: codeID value is not set in $inputfile.\n";
                print "$inputfile: (ADD LINE?) codeID: $codeID";
                print " (y/n) [y]: ";
                chomp($ans = <STDIN>);
                if (lc($ans) eq "a") { $ans = "y"; $noprompt = 1 }
            }
            if ($ans eq "" or lc($ans) eq "y") {
                system("sed -i 5i'codeID: $codeID' $inputfile > $tmpfile");
                print "$inputfile: UPDATED\n" unless $quiet;
                $updated = 1;
            }     
        }
    }
    close NPUT;

    # transfer lines from inputfile to tmpfile
    #-----------------------------------------
    open (NPUT, "< $inputfile") or die "Error opening file, $inputfile";
    open (TMP, "> $tmpfile") or die "Error opening tmpfile, $tmpfile";

    while (<NPUT>) {
        $line = $_;

        # replace old codeID with new ...
        #-----------------------------------
        if ($codeID and m/^\s*codeID:/) {
            if (m/^\s*codeID:\s*(\w+)/) {
                $old_cdID = $1;
                $line = updateVal($inputfile, $line, 1, $codeID, $old_cdID);
            }
            else { $line = updateVal($inputfile, $line, 1, $codeID) }
        }

        # replace old fvsetupID with new ...
        #-----------------------------------
        if ($fvsetupID and m/^\s*fvsetupID:/) {
            if (m/^\s*fvsetupID:\s*(\w+)/) {
                $old_fvID = $1;
                $line = (updateVal($inputfile, $line, 2, $fvsetupID, $old_fvID));
            }
            else { $line = updateVal($inputfile, $line, 2, $fvsetupID) }
        }
        $updated = 1 unless $line eq $_;
        print TMP $line;
    }
    close NPUT;
    close TMP;

    # update file, if there was a change
    #-----------------------------------
    if ($updated) {
        $tilde_file = $inputfile ."~";
        if (-e $tilde_file) {
            unlink $inputfile;
            move $tmpfile, $inputfile;
        }
        else {
            move $inputfile, $tilde_file;
            move $tmpfile, $inputfile;
        }
    }
    else {
        print "$inputfile: NO CHANGE\n" unless $quiet;
        unlink $tmpfile;
    }
}

#=======================================================================
# name - updateVal
# purpose - prompt user before updating codeID or fvsetupID value
#
# input parameters
# => $file: name of *.input file being updated
# => $line: the line in the file being updated (containind ID value)
# => $valFLG: == 1 for codeID, == 2 for fvsetupID
# => $newVal: new ID value
# => $oldVal: (optional) old ID value
#=======================================================================
sub updateVal {
    my ($ans, $file, $line, $valFLG, $newVal, $oldVal, $updateVal, $vType);
    $file = shift @_;
    $line = shift @_;
    $valFLG = shift @_;
    $newVal = shift @_;
    $oldVal = shift @_;

    if ($valFLG == 1) { $vType = "codeID" }
    else              { $vType = "fvsetupID" }

    # return line as is, if no change or if $newVal == 0
    #---------------------------------------------------
    return $line if $oldVal and $oldVal eq $newVal;
    return $line unless $newVal;
            
    # prompt user before making update
    #---------------------------------
    unless ($noprompt) {
        if ($oldVal) { print "$file: update $vType, $oldVal => $newVal" }
        else         { print "$file: update $vType => $newVal" }
        print " (y/n) [y]: ";
        chomp($ans = <STDIN>);
    }
    else { $ans = "y" }
    if (lc($ans) eq "a") { $ans = "y"; $noprompt = 1 }

    # update line
    #------------
    if ($ans eq "" or lc($ans) eq "y") {
        if ($oldVal) { $line =~ s/$oldVal/$newVal/ }
        else         { $line = "$vType: $newVal\n" }
        unless ($quiet) {
            if ($oldVal) { print "$file UPDATED: $vType, $oldVal => $newVal\n" }
            else         { print "$file UPDATED: $vType => $newVal\n" }
        }
    }
    return $line;
}

#=======================================================================
# name - pause
# purpose - pause interactive processing for user to consider previous output
#=======================================================================
sub pause {
    print "Hit <cr> to continue ... ";
    my $dummy = <STDIN>;
    return;
}

#=======================================================================
# name - debug
# purpose - print debug info
#=======================================================================
sub debug {
    print "="x35 ."\n";
    print "fvsetup: $fvsetup\n" if $fvsetup;
    print "gitcmd: $gitcmd\n";
    foreach (sort keys %inputList) { print "input: $_\n" }
    print "="x35 ."\n";
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    my $script = basename $0;
    print <<"EOF";

This utility will update the codeID and fvsetupID values in the testsuite
*.input files. See Note #1

usage: $script [dotinput(s)] [options]
where dotInput(s) are *.input file names; see Notes #2, #3, and #4

options
 -d fvsrc         directory location for fvsetupID; see Note #5
 -cid codeID      code git hash ID to use for update; if 0, then do not update;
                  defaults to system calculated code ID
 -fid fvsetupID   fvsetup git hash ID to use for update; if 0, then do not update;
                  defaults to abbreviated git hash of fvsetup in fvsrc
 -np              no prompt; do not prompt before making updates
 -q               quiet mode; do not print update messages
 -h,-help         print usage information

Notes
1. This script should be run in the src/Applications/GEOSdas_App directory.
2. If no dotInput files are listed, then the script will default to use all
   dotInput files in the local directory.
3. Multiple dotInput files can be listed, separated by commas, no spaces
4. All dotInput files listed must have the ".input" extension; However, they
   can be specified without including the ".input" extension, e.g. C48f,C90C
5. The fvsrc directory value can be set to the top src directory in the GEOSadas
   checkout, or to a directory containing fvsetup, or it can point directly to
   the fvsetup script itself.

EOF
exit;
}
