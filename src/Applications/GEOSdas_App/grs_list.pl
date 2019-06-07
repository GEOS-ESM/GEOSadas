#!/usr/bin/env perl
#=======================================================================
# name - grs_list.pl
# purpose - get list of GCM restarts from AGCM.rc file
#=======================================================================
use strict;
use warnings;
use FindBin qw($Bin);
use lib $Bin;
use AGCMrc qw(AGCM_rsts);

# global variables
#-----------------
my ($agcmrc, $flag, %grslist, %bootable);

# main program
#-------------
{
    my ($label, $grs);

    init();
    open AGCM, "< $agcmrc" or die "Error. Unable to open AGCM: $agcmrc;";
    foreach (<AGCM>) {
        next unless m/_checkpoint\b/;
        next if m/^\s*#/;

        $grs = undef;
        $grs = $1 if m/\s(\S+)_checkpoint/;
        next unless $grs;

        if ($flag == 2) { next unless $bootable{$grs} }
        $grslist{$grs} = 1 if $grs;
    }
    foreach (sort keys %grslist) { print "$_\n" }
}

#=======================================================================
# name - init
# purpose - get runtime parameters and options
#=======================================================================
sub init {
    use Getopt::Long;
    my ($help, @rs5_boot, $bootbase);

    # get options
    #------------
    GetOptions( "rc=s"   => \$agcmrc,
                "flg=s"  => \$flag,
                "h|help" => \$help );
    usage() if $help;

    # defaults
    #---------
    $agcmrc = "AGCM.rc" unless $agcmrc;
    $flag = 1 unless $flag;
    $flag = 1 unless $flag == 2;

    # make list of bootable restarts
    #-------------------------------
    if ($flag == 2) {
        %bootable = ();
        @rs5_boot = AGCM_rsts("rs5_boot");
        foreach (@rs5_boot) {
            ($bootbase = $_) =~ s/_rst//;
            $bootable{$bootbase} = 1;
        }
    }

    die "Error. Cannot find AGCM rc file: $agcmrc;" unless -e $agcmrc;
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    use File::Basename qw(basename);
    my $script = basename $0;

    print <<"EOF";

purpose: extract and print list of GCM restarts from AGCM.rc file
usage: $script <options>

options
  -rc  AGCM.rc   name of AGCM rc file; default = \"AGCM.rc\"
  -flg n         flag indicating which restarts to return
                   n=1 => return all GCM restarts (default)
                   n=2 => return all GCM bootstrap restart files

EOF
exit;
}
