#!@DASPERL@
#
# This daemon is responsible for doing QC on the MERRA observing system while the GCM 
# is still running. The daemon looks for a file called prepqc_trigger that is created
# by the GCM after the analysis time background eta and sfc files are created.  Once
# this file is noticed, the daemon calls the usual ssprepqc Perl routine.
# 
#
# !REVISION HISTORY:
#
# 15Jun2007  Kokron  Initial working code
# 09Oct2007  Kokron  Mofified to look for new trigger file name.  Also did some reformatting
#  
# 10Apr2008  Owens/Meta added sort uniq to prepbufr processing
# 17Apr2008  Owens/Meta added handling of hardwired times in in obsys.rc and
#                       support for running combfrd.x with date time arguments
# 13Mar2009  Todling    generalized NC filename extension


use Env qw(STRICT EXPID FVHOME FVWORK FVROOT); # make env vars readily available

$EXPID  || die "ERROR. EXPID not defined: $!"  unless ($EXPID);
$FVHOME || die "ERROR. FVHOME not defined: $!" unless ($FVHOME);
$FVWORK || die "ERROR. FVWORK not defined: $!" unless ($FVWORK);
$FVROOT || die "ERROR. FVWORK not defined: $!" unless ($FVROOT);
$NCSUFFIX || die "ERROR. FVWORK not defined: $!" unless ($NCSUFFIX);
use lib ( "${FVROOT}/bin" );
use File::Basename;      # for basename(), dirname()
use File::Copy;          # for cp() and move
use Manipulate_time;     # token_resolve() 

while(1) { # Start the daemon

    if ( ! -e "prepqc.daemon.running") {system("touch prepqc.daemon.running")};

    opendir(WORK, $FVWORK) || die "prepqc_daemon: can't opendir FVWORK: $!";
    @triggers = null; $trigger_file = null;
    @triggers = grep{/prepqc_trigger/} readdir(WORK);
    $trigger_file = $triggers[0];
    closedir(WORK);

    if ($trigger_file && (-e $trigger_file)) { # Sniff for the trigger file from the GCM

        @fields = split /\./, $trigger_file;
        $date_time = $fields[$#fields-1];        

        @fields = split /_/, $date_time;
        $nymd = $fields[0];
        $nhms = $fields[1];

        $yyyy = substr($nymd,0,4);
        $mm   = substr($nymd,4,2);
        $dd   = substr($nymd,6,2);
        $hh   = substr($nhms,0,2);
        unlink("$trigger_file");

        # Make sure upper-air bkg is a legitimate dyn-vector
        #---------------------------------------------------
        if ( ! -e "replay.acq" ) {
            $bkgfile = `echorc.x -template $EXPID $nymd $nhms upper-air_bkg_filename`;
            chomp($bkgfile);
            print "bkgfile is ($bkgfile) \n";

            $pid = getppid();
            $bkgtmp  = "tmp00.$pid.$NCSUFFIX";
            move("$bkgfile","$bkgtmp");

            $cmd = "$FVROOT/bin/dyn2dyn.x -o $bkgfile $bkgtmp";
            print "$cmd\n";
            $rc = system($cmd);
            $exit_code = $rc >> 8;
            print "$0: dyn2dyn \$exit_code =  $exit_code\n";
            die ">>>> ERROR <<< running dyn2dyn" if (($exit_code)||(! -e $bkgfile)); 
        }

        # Combine pre-qc files...
        #-------------------------
        $opt_pb = "";
        $pbname = `echorc.x -template $EXPID $nymd $nhms -fill %s.prepbufr.%y4%m2%d2.t%h2z.blk`;
        chomp($pbname);
        $pbdtg  = token_resolve( "%y4%m2%d2%h2", "$nymd", "$nhms");

        print"pbname is ($pbname)\n";
        print"pbdtg  is ($pbdtg )\n";

        if (-e "pre-qc.acq") { 
            if ( ! -z "pre-qc.acq" ) { 
                open(FH1,"+>input_combfr.txt") || die "prepqc: could not open input_combfr.txt";
                foreach $qcpat ( `cat pre-qc.acq | sort | uniq`) {
                    $pat = `echorc.x -template dummy $nymd $nhms -fill $qcpat`;
                    print FH1 ("$pat") if ( -e $pat  && ! -z $pat ) ;
                }
                close(FH1);
                if ( -z "input_combfr.txt "{
                   print"No pre-qc files present, skipping PREPQC\n";
                   system("touch prepqc.$nymd.$nhms.done");
                   return
                }
                `combfrd.x -d $pbdtg $pbname < input_combfr.txt`;
                open(FH1,"+>data_types.log") || die "prepqc: could not open data_types.log";
                @types = `scanbuf0.x  $pbname`;
                print FH1 ("@types");
                close(FH1);

                if ( grep /PROFLR/, @types ) {
                    $ENV{"PROFQC"} = 1;
                } else {
                    $ENV{"PROFQC"} = 0;
                }
                print"PROFQC is ($ENV{PROFQC})\n";
  
                if ( grep /AIRCFT/, @types ) {
                    $ENV{"ACFTQC"} = 1;
                } else {
                    $ENV{"ACFTQC"} = 0;
                }
                print "ACFTQC is ($ENV{ACFTQC})\n";

                if ( grep /AIRCAR/, @types ) {
                    $ENV{"ACARSQC"} = 1;
                } else {
                    $ENV{"ACARSQC"} = 0;
                }
                print"ACARSQC is ($ENV{ACARSQC})\n";

                `ls -l $pbname`;
            } 
        }

        `ls -l $pbname`;
        if ( -z $pbname ) {
            unlink("$pbname");
            $ENV{"opt_pb"} = "";
        } else {
            $ENV{"opt_pb"} = "-pb $pbname";
        }

        $doPREPQC = 0;
        if ( $hh== 00 || $hh == 06 || $hh== 12 || $hh== 18 ) {
            $doPREPQC  = 1  if ( $ENV{PREPQC}  || $opt_pqc );
            #--print "doPREPQC is ($doPREPQC)\n";
        }

        $fvhome = dirname($FindBin::Bin) unless ( $fvhome = $FVHOME );
        $expid  = basename($fvhome);

        # NCEP pre-processing QC
        #-----------------------
        prepqc() if ( $doPREPQC );

        # All done. Drop a canary file for fvana::prepqc to look for
        #-----------------------------------------------------------
        system("touch prepqc.$nymd.$nhms.done");

    } else {

        # Not ready  Wait here
        #---------------------
        #-- print "Not ready \n";
        sleep(1);

  }
} # close the while loop
   

#......................................................................
sub prepqc {                        # runs NCEP preprocessing QC subsystem

    $bkgsfc  = `echorc.x -template $EXPID $nymd $nhms surface_bkg_filename`;
    chomp($bkgsfc);

    $prepqc_new   = ( $opt_pb   or $prepb  = "$expid.prepbufr.${nymd}.t${hh}z.blk" );

    $dynf = `echorc.x -template $EXPID $nymd $nhms upper-air_bkg_filename`;
    chomp($dynf);

    # Only execute if the input PREPBUFR file exists and is non-empty
    #----------------------------------------------------------------
    if ( -e $prepqc_new && -s $prepqc_new ) {

        $prepqc_old = "prepbufr.pre-qc.${nymd}.t${hh}z.blk" ;

        # Rename original prepqc file to serve as input to PREPQC
        #--------------------------------------------------------
        copy("$prepqc_new","$prepqc_old");
        unlink("$prepqc_new");

        $cmd = "gmao_prepqc -r $fvhome/run -o $prepqc_new $nymd $nhms $prepqc_old $dynf";
        print "$0: $cmd\n" unless ( $opt_q );

        $rc = System ( $cmd, "prepqc.log" );
        print "$0: prepqc \$rc =  $rc\n" unless ( $opt_q );
        die ">>>> ERROR <<< running prepqc" if ( $rc );
    }
    else {
        print "$0: missing/empty PREPQC file $prepqc_new\n" unless ( $opt_q );
        die ">>>> ERROR <<< running PREPQC in strict mode" if ( $STRICT );
    }
}

#......................................................................

#
# System: This routine saves stdout/stderr, redirects it to a specified file,
#         runs a shell command using this new stdout/stderr, and finally
#         restores the original stdout/stderr.
#

sub System {

    my ( $cmd, $logfile ) = @_;
    my ( @zname );

    open SAVEOUT, ">&STDOUT";  # save stdout
    open SAVEERR, ">&STDERR";  # save stderr

    open STDOUT, ">>$logfile" or die "can't redirect stdout";
    open STDERR, ">>$logfile" or die "can't redirect stderr";

    select STDERR; $| = 1;     # make it unbuffered
    select STDOUT; $| = 1;     # make it unbuffered

    @zname = split(" ", $cmd);
    $rc1 = system( "zeit_ci.x $zname[0]");

    $rc = system ( $cmd );     # run the shell command

    $rc2 = system( "zeit_co.x $zname[0]");

    # Bitwise shift returns actual UNIX return code
    $exit_code = $rc >> 8;

    close STDOUT;
    close STDERR;

    open STDOUT, ">&SAVEOUT" ;  # restore stdout
    open STDERR, ">&SAVEERR" ;  # restore stdout

    return $exit_code;

}
