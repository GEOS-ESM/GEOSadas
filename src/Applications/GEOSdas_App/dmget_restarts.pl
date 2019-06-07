#!/usr/bin/perl -w
#=======================================================================
#
# name - dmget_restarts.pl
# purpose - This script will submit a background job to dmget restart
#           files in the archives. It was designed to be called during
#           job setup.
#
# notes
# 1. If the dmget and dmls commands are available, then the script
#    determines which restarts need to be demigrated and forks a job
#    to dmget those files.s
# 2. If dmget and dmls are not available, the script determines which
#    restarts might need to be demigrated, and gives user the option
#    to submit a job to dmget those files.
# 3. For option in #2 above, the user must be able to login to dali
#    without a password. The script will give no indication if either
#    login or dmget is unsuccessful.
# 4. -ymd can take partial dates, e.g. 1214 (for Dec 14) or just 14
# 5. -hr can be given in hhmmss format.
# 6. See usage (dmget_restarts.pl -h) for more usage information.
#
# revision history
# 20Mar2008  Stassi   Initial version of code.
# 26Nov2008  Stassi   Modified to use list_restarts.pl script
# 04Mar2009  Todling  Changed API to comply w/ standards (nymd/nhms)
# 01May2009  Stassi   Fixed errors.
# 13Apr2010  Stassi   Added -noprompt flag; changed dirac to dali
#=======================================================================
use strict;

# global variables
#-----------------
my ($expid, $rstdir, $bkgHrFreq, $nymd, $nhr, $noana, $readme, $noprompt);
my ($dmf_access, $dmls, $dmget, $mach, $name);

# main program
#-------------
{
    my ($ans, @files, @offlinefiles, @archdirfiles);

    init();
    determine_dmf_access();
    exit if $dmf_access eq "unavailable";

    # local dmget
    #------------
    if ($dmf_access eq "local") {

        # this takes longer than remote dmget; so ask user first
        #-------------------------------------------------------
        unless ($noprompt) {
            print "  Check DM status of restarts (y/n)? [n] ";
            chomp($ans = lc <STDIN>);
            exit unless $ans eq "y";
        }

        @files = listrstfiles();
        @offlinefiles = offline(@files);
        local_demigrate(@offlinefiles) if @offlinefiles;
    }

    # remote dmget
    #-------------
    if ($dmf_access eq "remote") {
        @files = listrstfiles();
        @archdirfiles = archdir(@files);
        remote_demigrate(@archdirfiles) if @archdirfiles;
    }
}

#=======================================================================
# name - init
# purpose - Initialize global variables from the runtime flags.
#=======================================================================
sub init {
    use FindBin qw($Bin);
    use Getopt::Long;
    use File::Basename;
    my ($dao, $help, $year, $month, $hr, $flags);

    $name = basename $0;

    # get runtime flags
    #------------------
    GetOptions( "bf=i"        => \$bkgHrFreq,
                "d=s"         => \$rstdir,
                "dao"         => \$dao,
                "expid=s"     => \$expid,
                "nymd|ymd=s"  => \$nymd,
                "nhms|hr=s"   => \$nhr,
                "noana"       => \$noana,
                "np|noprompt" => \$noprompt,
                "r"           => \$readme,
                "h|help"      => \$help );
    usage() if $help;

    # get and/or verify restart information
    #--------------------------------------
    $flags = "";
    $flags .= " -noprompt"     if $noprompt;
    $flags .= " -expid $expid" if $expid;
    $flags .= " -d $rstdir"    if $rstdir;
    $flags .= " -dao"          if $dao;
    $flags .= " -ymd $nymd"    if $nymd;
    $flags .= " -hr $nhr"      if $nhr;

    ($rstdir, $expid, $nymd, $nhr) = `$Bin/info_restarts.pl $flags`;
    die ">> Error << Unable to find restart information; " unless $rstdir;
    chomp($rstdir, $expid, $nymd, $nhr);
}

#=======================================================================
# name - determine_dmf_access
# purpose - Determine whether the job can locally execute the dmget
#           command or whether it needs to ssh the command to dali.
# notes -
# 1. Currently can only execute dmls and dmget from palm.
# 2. ssh dmget from palm, if the dmls and dmget commands cannot be found.
# 3. ssh dmget from discover
# 4. No action will be taken on machines other than palm or discover.
#=======================================================================
sub determine_dmf_access {
    my ($machine);

    # which machine?
    #---------------
    $machine = `uname -n`;
    $mach = $machine;
    $mach = "discover" if $machine =~ /^discover/;

    # get dmls and dmget commands
    #----------------------------
    chomp($dmls  = `which dmls`)  unless system "which dmls  >& /dev/null";
    chomp($dmget = `which dmget`) unless system "which dmget >& /dev/null";

    # jobs on palm have direct dmf access
    #------------------------------------
    $dmf_access = "unavailable";

    #--$dmf_access = "remote" if $mach eq "discover" or $mach eq "palm";
    #--$dmf_access = "local"  if $mach eq "palm" and $dmls and $dmget;
    $dmf_access = "local" if $dmls and $dmget;
}

#=======================================================================
# name - listrstfiles
# purpose - Return list of restarts.
#
# notes -
# 1. Use abs_path() function to get absolute pathname of file
#=======================================================================
sub listrstfiles {
    use Cwd "abs_path";
    use FindBin qw($Bin);
    my ($flags, $file, $line, $dummy);
    my (@rstfiles, @allfiles);

    # get list of files
    #-------------------
    $flags = "-d $rstdir -expid $expid -ymd $nymd -hr $nhr";
    $flags .= " -noana" if $noana;
    $flags .= " -r"     if $readme;
    $flags .= " -bf $bkgHrFreq" if $bkgHrFreq;

    @allfiles = ();
    chomp( @rstfiles = `$Bin/list_restarts.pl $flags` );

    # if tarfiles found, then all other listed files are on the tarfile
    #------------------------------------------------------------------
    foreach (@rstfiles) {
        if (m/\.tar$/) { @rstfiles = ( $_ ); last }
    }

    # get absolute path for each file, to determine if on archive
    #------------------------------------------------------------
    foreach (@rstfiles) { push @allfiles, abs_path($_) }

    return @allfiles;
}

#=======================================================================
# name - offline
# purpose - Take list of files; return subset of files which are
#           offline, i.e. those which need to be demigrated.
# notes -
# 1. The dmls command must be locally defined.
#=======================================================================
sub offline {
    my ($file, $line, @offline);
    my @files = @_;

    return unless $dmls;
    foreach $file (@files) {
        chomp($line = `$dmls -l $file`);
        push @offline, $file if $line =~ /\(OFL\)/;
    }
    return @offline;
}

#=======================================================================
# name - archdir
# purpose - Take list of files; return subset of files which contain
#           "/achive/" within their name.
# notes -
# 1. The filenames in @files include the directory path location.
#=======================================================================
sub archdir {
    my ($file, $line, @archdirfiles);
    my @files = @_;

    foreach $file (@files) {
        push @archdirfiles, $file if $file =~ /\/archive\//;
    }
    return @archdirfiles;
}

#=======================================================================
# name - local_demigrate
# purpose - Launch background job to locally demigrate list of files.
#
# notes -
# 1. The dmget command must be locally defined.
#=======================================================================
sub local_demigrate {
    my ($pid, $dummy);
    my @files = @_;

    exit unless $dmget;
    print "\n" unless $noprompt;
    print "  Launching background job to demigrate restart files.\n";

    defined($pid = fork) or die ">> Error << during fork operation.";
    exec "$dmget @files" unless $pid;  # child process starts and ends here

    unless ($noprompt) {
        print "  Hit <cr> to continue ... ";
        $dummy = <STDIN>;
    }
    exit;
}

#=======================================================================
# name - remote_demigrate
# purpose - Send background ssh job to dali to demigrate list of files.
#
# notes -
# 1. Since the dmls command is not local, it is not known whether the
#    files actually need to be demigrated. However, it will do no harm
#    to dmget files which are already available.
# 2. The remote dmget will work only if the following conditions are true:
#    - user has dali account
#    - user can log into dali account without providing a password
#    - username is the same on local and dali accounts
#    Otherwise, nothing happens, no harm done.
# 3. It is easy to set up dali to be able to log in without a
#    password. If you do not know how, then ask somebody.
#=======================================================================
sub remote_demigrate {
    my ($pid, $ans, $cmd, $status, $dummy);
    my (@files, $sendjob);

    @files = @_;

    print "\n  Some restart files may need to be demigrated.\n";
    if ($noprompt) { $sendjob = 1; }
    else           { $sendjob = sendjobQuery(); }

    if ($sendjob) {
        print "  Sending dmget job to dali (no status feedback available).\n";

        $cmd = "ssh -q -o BatchMode=yes dali dmget @files";
        defined($pid = fork) or die ">> Error << during fork operation.";
        exec $cmd unless $pid; # child process starts and ends here

    } else {
        print "  No attempt to demigrate restart files.\n";
    }
    print "\n";
    exit;
}

#=======================================================================
# name - sendjobQuery
# purpose - query user whether is willing and able to send demigrate job
#           to dali
#=======================================================================
sub sendjobQuery {
    my ($sendjob, $ans);

    $sendjob = 0;
    print "  Can you log into dali from $mach without a password (y/n) [n]? ";
    chomp($ans = lc <STDIN>);

    if ($ans eq "y") {
        print "  Submit job to dali to dmget restarts (y/n) [y]? ";
        chomp($ans = lc <STDIN>);

        unless ($ans eq "n") {
            $sendjob = 1;
        }
        return $sendjob;
    }
}

#=======================================================================
# name - usage
# purpose - Print information on how to use this script.
#=======================================================================
sub usage {
    print <<"EOF";

NAME
     $name

PURPOSE
     Submit a background job to demigrate restart files.

SYNOPSIS
     $name -d rstdir -expid expid -ymd nymd -hr nhr
     $name -h

OPTIONS
     -bf n            Hour frequency of bkg restarts, either 1 for every hour
                      or 3 (default) for every 3 hours
     -d dirname       Start location for the restart directory search;
                      defaults to \$ARCHIVE if defined, or "." if not
     -dao             Get OPS restarts; this option works best when accompanied
                      by flag/value option -nymd and/or -expid
     -expid expid     Experiment ID of restart files 
     -nymd ymd        Data year/month/day of restart files
                      (yyyymmdd format or any substring thereof)
     -nhms hour       Data hour of restart files (can be in hh or hhmmss format)
     -noana           Model restarts only; do not copy analysis restarts
     -np              No interactive prompt
     -r               Look for README file and copy it if found
     -h               Print this usage message

ALTERNATE OPTIONS
     -ymd ymd         same as -nymd
     -hr hour         same as -nhms
     -noprompt        same as -np
     -help            same as -h

EXAMPLE
     $name -d /share/stassi/restarts/rs/Y2007/M12 -expid c0145 -ymd 20071216 -hr 21

AUTHOR
     joe.stassi\@nasa.gov

EOF
exit;
}
