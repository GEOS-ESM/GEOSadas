#!/usr/bin/perl
#
#  Creates PNG images from GEOS-4 forecast files for the TC4 WxMaps
#  website. This is a
#
#.........................................................................

use FindBin;                     # find location of the this script
use lib "$FindBin::Bin/GrADS";   # make perl libraries available
use   Time::Local;
use Getopt::Std;
use   File::Basename;
use   Data::Dumper;
use File::Copy "cp";     # for cp()
use Cwd 'realpath';

# Initialize
# ----------
  Initialize();

# Load and start GrADS only if user wants validation
# --------------------------------------------------
###  if ( $opt_V ) {
       eval {
              use  Grads::Gerl;
              grads { Bin=>"grads", Verb=>1, Echo=>0, Port=>0, Window=>0 }
	    };
       die "Cannot start GrADS" if ( $@ );
###   }

# For each Date-time group
# ------------------------
DATE:  for $dtg ( @DTG ) {

#     Is it time?
#     -----------
      ($yyyy,$mm,$dd,$hh) = @$dtg;
###      $this_time = "$yyyy$mm$dd$hh";
###      next DATE if ( $this_time < $Beg_time );

      print "\nCreating DDF on $yyyy-$mm-$dd $hh" . "Z\n" if ( $opt_v );

#     Create assimilation DDFs
#     ------------------------
      if ( $opt_A ) {

	  if( $opt_F) {
	      $FTYPES = [split(",",$opt_F)];
	  }
	  else {
	      $FTYPES = get_ftypes("$expdir/das",$yyyy,$mm,$dd,$hh);
	  }

          if( $opt_X) {
              $XTYPES = [split(",",$opt_X)];
          }
          else{
              $XTYPES = ();
          }

	  for $ftype ( @{$FTYPES} ) {
              if( grep /^$ftype$/, @{$XTYPES} ) {
		print "$ftype found in -X option - Skipping.\n";
	      }
              else{
                  print "create_DDF ( $ddir,$ftype,$expdir/das, $yyyy,$mm,$dd,$hh )\n";
	          $rc = create_DDF ( "$ddir",$ftype,"$expdir/das",
		     	  		 $yyyy,$mm,$dd,$hh );
	          die "cannot create DDF for file type $ftype " if ( $rc );
              }#XTYPES
	  } # ftype

      } # assimilation mode

#     Create Nature Run DDFs
#     ----------------------
      elsif ( $opt_N ) {

        # LOOP over collections
	my $nCollections = @COLLECTIONS;
	for (my $iCollection=0; $iCollection<$nCollections; $iCollection++) {
          print "collection: $COLLECTIONS[$iCollection]\n";
          $FTYPES = get_ftypes("$coll_dirs[$iCollection]",$yyyy,$mm,$dd,$hh);
          for $ftype ( @{$FTYPES} ) {
            $rc = create_DDF ( "$ddir",$ftype,"$coll_dirs[$iCollection]",
                               $yyyy,$mm,$dd,$hh );
            die "cannot create DDF for file type $ftype " if ( $rc );
          } # ftype
        }
      }

#     Create Larry DDFs
#     ----------------------
      elsif ( $opt_L ) {

         # LOOP over collections
         my $nCollections = @COLLECTIONS;
         for (my $iCollection=0; $iCollection<$nCollections; $iCollection++) {
            print "collection: $COLLECTIONS[$iCollection]\n";
            $FTYPES = get_ftypes("$coll_dirs[$iCollection]",$yyyy,$mm,$dd,$hh);
            for $ftype ( @{$FTYPES} ) {
               $rc = create_DDF ( "$ddir",$ftype,"$coll_dirs[$iCollection]",
                  $yyyy,$mm,$dd,$hh );
               die "cannot create DDF for file type $ftype " if ( $rc );
            } # ftype
         }
      }

#     Create Forecast DDFs
#     --------------------
      else {
	  if( $opt_F) {
	      $FTYPES = [split(",",$opt_F)];
	  }
	  else {
	      $FTYPES = get_ftypes("$expdir/forecast",$yyyy,$mm,$dd,$hh);
	  }
          if( $opt_X) {
              $XTYPES = [split(",",$opt_X)];
              print "XTYPES = $XTYPES\n";
          }
          else{
              $XTYPES = ();
          }

	  for $ftype ( @{$FTYPES} ) {
              if( grep /^$ftype$/, @{$XTYPES} ) {
                  print "$ftype found in -X option - Skipping.\n";
              }
              else{

	          $rc = create_DDF ( "$ddir",$ftype,"$expdir/forecast",
					 $yyyy,$mm,$dd,$hh );
	          die "cannot create DDF for file type $ftype " if ( $rc );
              }#XTYPES

	  } # ftype

     }  # forecast mode

  } # dtg

# All done
# --------
  Finalize();

  exit(0);


#
#.........................................................................

sub Initialize {

  $Iam = "g5_ddf";

  @MONTH  = qw( jan feb mar apr may jun jul aug sep oct nov dec );


# Parse cmd line args
# -------------------
  getopts('cLNMAahvVd:p:w:s:x:F:E:X:T:');
  my $argc = @ARGV;
  usage() if ( $opt_h || $argc < 1 );

  $expdir = File::Spec->rel2abs($ARGV[0]);
  $yyyy = $ARGV[1];

  die "Invalid experiment dir <$expdir> " unless ( -d "$expdir" );

  $expid = basename ( "$expdir" ) unless ( $expid = $opt_E );

  $ddir = "$expdir"  unless ( $ddir = $opt_d );
  $wdir = "."        unless ( $wdir = $opt_w );

# Produce a ctl file
# ------------------
  $makectl = 0;
  if ( $opt_c ) { $makectl = 1; }
  if ( $opt_x ) { $xpat = $opt_x; }
  else          { $xpat = "nc4";  }

  unless ( $Style = $opt_s ) {
      $Style = "by_ftype";
      $Style = "flat" if ( $opt_A or $opt_N or $opt_L );
  }

# For nature run, create list of collections
# ------------------------------------------
  if ($opt_N){

    @coll_dirs = ();
    @COLLECTIONS = ();

    if ($opt_M){
      ## ONLY monthly means
      # 0.5000_deg/inst
      my @tmp_coll_dirs = split(" ", `/bin/ls -1d $expdir/DATA/0.5000_deg/inst/*mo_*`);
      push(@coll_dirs, @tmp_coll_dirs);
      # 0.5000_deg/tavg
      my @tmp_coll_dirs = split(" ", `/bin/ls -1d $expdir/DATA/0.5000_deg/tavg/*mo_*`);
      push(@coll_dirs, @tmp_coll_dirs);
      # 0.5000_deg/tdav
      my @tmp_coll_dirs = split(" ", `/bin/ls -1d $expdir/DATA/0.5000_deg/tdav/*mo_*`);
      push(@coll_dirs, @tmp_coll_dirs);
      # 0.0625_deg/inst
      my @tmp_coll_dirs = split(" ", `/bin/ls -1d $expdir/DATA/0.0625_deg/inst/*mo_*`);
      push(@coll_dirs, @tmp_coll_dirs);
      # 0.0625_deg/tavg
      my @tmp_coll_dirs = split(" ", `/bin/ls -1d $expdir/DATA/0.0625_deg/tavg/*mo_*`);
      push(@coll_dirs, @tmp_coll_dirs);
    } else{
      ## WITHOUT monthly means
      # 0.5000_deg/inst
      my @tmp_coll_dirs = split(" ", `/bin/ls -1d $expdir/DATA/0.5000_deg/inst/* | grep -v "mo_"`);
      push(@coll_dirs, @tmp_coll_dirs);
      # 0.5000_deg/tavg
      my @tmp_coll_dirs = split(" ", `/bin/ls -1d $expdir/DATA/0.5000_deg/tavg/* | grep -v "mo_"`);
      push(@coll_dirs, @tmp_coll_dirs);
      # 0.5000_deg/tdav
      my @tmp_coll_dirs = split(" ", `/bin/ls -1d $expdir/DATA/0.5000_deg/tdav/* | grep -v "mo_"`);
      push(@coll_dirs, @tmp_coll_dirs);
      # 0.0625_deg/inst
      my @tmp_coll_dirs = split(" ", `/bin/ls -1d $expdir/DATA/0.0625_deg/inst/* | grep -v "mo_"`);
      push(@coll_dirs, @tmp_coll_dirs);
      # 0.0625_deg/tavg
      my @tmp_coll_dirs = split(" ", `/bin/ls -1d $expdir/DATA/0.0625_deg/tavg/* | grep -v "mo_"`);
      push(@coll_dirs, @tmp_coll_dirs);
    }

    my $base;
    chomp(@coll_dirs);
    for $dir ( @coll_dirs ) {
      $base = `basename $dir`;
      $base =~ s/\n//g;
      push(@COLLECTIONS, $base);
    }

    print "collections: @COLLECTIONS\n";
  }

# For Larry Experiments, create list of collections
# -------------------------------------------------
  if ($opt_L){

    @coll_dirs = ();
    @COLLECTIONS = ();

    my @tmp_coll_dirs = split(" ", `/bin/ls -1d $expdir/holding/*`);
    push(@coll_dirs, @tmp_coll_dirs);

    my $base;
    chomp(@coll_dirs);
    for $dir ( @coll_dirs ) {
      $base = `basename $dir`;
      $base =~ s/\n//g;
      push(@COLLECTIONS, $base);
    }

    print "collections: @COLLECTIONS\n";
  }

# Hardwired titles
# ----------------
  set_Titles();

  @DTG = ();

# Some options do not make sense in analysis/nature run mode
# ----------------------------------------------------------
  if ( $opt_A or $opt_N or $opt_L ) {
       $opt_a = undef;
       if ( "$yyyy" eq "latest" ) {
             $argc = 1;
             $yyyy = undef;
       }
   }

# Determine Date-time group to work with
# --------------------------------------
  $Beg_time = -1;  # all dates
  if ( "$yyyy" eq "latest" ) {   # forecast mode only

      $Doing_latest = 1 unless ( $opt_s );
      @DTG = find_DTGS ( "$expdir/forecast", "latest" );

  } elsif ( $argc == 5 ) {       # analysis or forecast mode only

      my $mm = $ARGV[2]; my $dd = $ARGV[3]; my $hh = $ARGV[4];

      $Beg_time = "$yyyy$mm$dd$hh";
      if ( $opt_a ) {                         # never in analysis mode
	  @DTG = find_DTGS ( "$expdir/forecast", "all" );
      } else {                                # analysis/forecast mode
          push @DTG, [ ($yyyy,$mm,$dd,$hh) ];
      }

  } elsif ( $opt_a ) {          # forecast mode only

    @DTG = find_DTGS ( "$expdir/forecast", "all" );

  } else {

      if ( $opt_A ) {
	   @DTG = find_DTGS ( "$expdir/das", "first" );
      } elsif ( $opt_N ) {
	   @DTG = find_DTGS ( "$coll_dirs[0]", "first");
      } elsif ( $opt_L ) {
	   @DTG = find_DTGS ( "$coll_dirs[0]", "first");
      } else {
	   usage();    # error in forecast mode
      }

  }


}

#.........................................................................
sub find_latest {


}

#.........................................................................
sub find_DTGS {
    my $rootdir = shift;
    my $what    = shift;  # either "all" or latest"

    my ($yyyy,$mm,$dd,$hh);
    my @DTG;

    my $alldirs;
    if ( $opt_A ) {
      $alldirs = `/bin/ls -1d $rootdir/Y????/M??/D??`;
    } elsif ($opt_N) {
      if ($opt_M){
	$alldirs = `/bin/ls -1d $rootdir/Y????/M??`;
      } else{
	$alldirs = `/bin/ls -1d $rootdir/Y????/M??/D??`;
      }
    } elsif ( $opt_L ) {
      $alldirs = `/bin/ls -1d $rootdir/Y????/M??/D??`;
    } else {
      $alldirs = `/bin/ls -1d $rootdir/Y????/M??/D??/H??`;
    }
    
    my @alldirs = split "\n", $alldirs;  # all avalilable dirs

#   Handle special cases: first and latest
#   --------------------------------------
    if ( "$what" eq "first" ) {
         my $first = "$alldirs[0]";
         @alldirs = ();
         push @alldirs,$first;

    } elsif ( "$what" eq "latest" ) {
         my $latest = pop @alldirs;
         @alldirs = ();
         push @alldirs, $latest;
         print "latest = $latest\n";
    }


    my $dir;
    my @tokens;

#   Parse and store times of each directory
#   ----------------------------------------
DIR: for $dir ( @alldirs ) {
    my @tokens;
      next DIR unless ( @tokens = parse_dir ( "$dir" ) );
      push @DTG,\@tokens;
    }

    return @DTG;

}

#.........................................................................
sub get_ftypes {
    my ($rootdir,$yyyy,$mm,$dd,$hh) = @_;

    my $idir;
    if ( "$rootdir" =~ /das$/ ) {
	$idir = "$rootdir/Y$yyyy/M$mm/D$dd";
    } else {
	$idir = "$rootdir/Y$yyyy/M$mm/D$dd/H$hh";
    }

    #pc: nature run is simular to das
    if ( $opt_N ) {
      if ($opt_M){
        $idir = "$rootdir/Y$yyyy/M$mm";
      } else {
	$idir = "$rootdir/Y$yyyy/M$mm/D$dd";
      }
    }

    #mat: Larry is like nature run
    if ( $opt_L ) {
        $idir = "$rootdir/Y$yyyy/M$mm/D$dd";
    }

    my @files = get_flist("$idir","*.$xpat");

    my ($file,$expid,$ftype,$ftime,$fex);
    my @tokens;

    my %Ftypes;

FILE:
    foreach $file ( @files ) {
        next FILE unless ( @tokens = parse_fname(basename($file)) );
	($null,$ftype,$ftime,$fex) = @tokens;

	$Ftypes{$ftype} = 1;

    }

    my @ftypes = ( keys %Ftypes );

    return \@ftypes;

}

#.........................................................................

sub make_ctl {
    my $ddf = shift;
    my $ctl = shift;
    my $dsetpath = shift;

    my @ddfsplit = split("/", $ddf);

    $_ = $opt_T;
    #
    #  Pass a template to one and only one specfic collection
    #
    #  if there is pattern matching between current collection
    #  Use template as input file(ddf) to create "CTL" File.
    #
    /$ddfsplit[$#ddfsplit]/ && do {
	$ddf = $_;
    };

    ###print "\nddf: $ddf\n\n";

    ga_('reinit');
    eval {
	$fh = Open("$ddf","XDF");
    };
    if(!$fh) {
	$fh = Open("$ddf","SDF") ;
    }

    $qu = Query('attr'); # get units thro querying attr
    $qh = Query('ctlinfo');
    ga_("close 1");

    ### print "dset: $qh->{dset}\n";

    if( $qh->{dset} =~ /%/ ) {
	$options_template = "OPTIONS template";
    }
    #
    # Set dsetpath to opt_p
    # Take care of last character of path. Make sure it is "/"
    #
    if( defined $dsetpath ) {
	$last = substr $dsetpath,-1,1;
	if( $last !~ /\// ) {
	    $dsetpath .= "/";
	}
	my @DSET = split('/', $qh->{dset});
	$qh->{dset} = $dsetpath . $DSET[$#DSET];
    }
    #
    #
    #
    $xdef = "xdef ".$qh->{xdef};

    if( length($qh->{xdef}) > 80 ) {
	@xdef = split(' ',$qh->{xdef});
	$xdef = sprintf("xdef $xdef[0] linear -180  %7.5f", 360/$xdef[0]);
    }
    $ydef = "ydef ".$qh->{ydef};
    if( length($qh->{ydef}) > 80 ) {
	@ydef = split(' ',$qh->{ydef});
	$ydef = sprintf("ydef $ydef[0] linear -90  %7.5f", 180/$ydef[0]);
    }


    $zdeflevels = "";
    @zdef = split(' ',$qh->{zdef});

# Negative increment linear levels must be computed
# -------------------------------------------------
    if( $zdef[3] == -1 ) {
      $zdef[1] = "levels";
      #pchakrab $zstart = $zdef[2] - $zdef[0] + 1;
      #pchakrab for( $i = $zstart; $i <= $zdef[2]; $i++) {
      for( $i = $zdef[2]; $i > 0; $i--) {
	$zdeflevels .= $i." ";
	if( ($i % 7) == 0  ) { $zdeflevels .= "\n "; }
      }
    }
    else {
      for( $i = 2; $i <  $#zdef+1; $i++) {
	$zdeflevels .= $zdef[$i]." ";
	if( ($i % 7) == 0  ) { $zdeflevels .= "\n "; }
      }
    }

    if ("$zdef[1]" eq "levels") {
      $options_template .= " zrev";
    }

# For nature run try to get relative path for dset
# ------------------------------------------------
    ### print "BEFORE: $qh->{dset}\n";
    if (opt_N) { $qh->{dset}=rel_path($qh->{dset}, $ctl, $expdir); }
    ### print "AFTER: $qh->{dset}\n";

    open CTL, ">$ctl" or  die "cannot open file $ctl";

    print CTL <<EOF  or  die "cannot write to  file $ctl";
dset $qh->{dset}
$options_template
title $qh->{title}
undef $qh->{undef}
dtype $qh->{dtype}
$xdef
$ydef
zdef $zdef[0] $zdef[1] $zdeflevels
tdef $qh->{tdef}
vars $qh->{vars}
EOF

    foreach $key (keys %{$qh}) {
	$key =~ /=/  && do {
	    print CTL $key." ".$qh->{$key}."\n";
	}
    }
    print CTL "endvars\n";

    # print units
    foreach $key (keys %{$qh}) {
      $key =~ /=/  && do {
	my @strippedKey = split('=>', $key);
	my $corrKey = $strippedKey[1];
	print CTL "\@ $corrKey String units " . $qu->{$corrKey} . "\n";
      }
    }
    close CTL;
}

#.........................................................................
sub rel_path() {
  my ($loc_dset, $loc_ctl, $loc_exp) = @_;

  my $ctl_realpath = realpath($loc_ctl);

  my @spltDset = split($expdir, $loc_dset);
  my $rel_dset = realpath($expdir) . $spltDset[1];

  my @spltCtl = split('/opendap', $ctl_realpath);
  if (index($rel_dset, $spltCtl[0]) != -1){
    # for nature run, the ctl files are 3 directories down
    $rel_dset = '^../../../' . $spltDset[1];
    return $rel_dset;
  }
  else{
    return $loc_dset;
  }
}

#.........................................................................
sub create_DDF {

    my ($ddir,$ftype,$rootdir,$iyyyy,$imm,$idd,$ihh) = @_;

    if ( $opt_A ) {
      print "- Creating Assimilation DDF for file type <$ftype> ... " if ( $opt_v );
    } elsif ($opt_N){
      if ($opt_M){
	print "- Creating Nature Run (monthly mean) DDF for file type <$ftype> ..." if ($opt_v);
      } else{
	print "- Creating Nature Run DDF for file type <$ftype> ..." if ($opt_v);
      }
    } elsif ($opt_L){
       print "- Creating Larry Experiment DDF for file type <$ftype> ..." if ($opt_v);
    } else {
      print "- Creating Forecast DDF for file type <$ftype> ... " if ( $opt_v );
    }

    my $idir;
    if ( $opt_A ) {
      $idir = "$rootdir/Y????/M??/D??";
    } elsif ($opt_N){
      if ($opt_M){
	$idir = "$rootdir/Y????/M??";
      } else {
	$idir = "$rootdir/Y????/M??/D??";
      }
    } elsif ($opt_L){
         $idir = "$rootdir/Y????/M??/D??";
    } else {
      $idir = "$rootdir/Y$iyyyy/M$imm/D$idd/H$ihh";
    }

#   Get file list for this type
#   ---------------------------
    my @files = get_flist("$idir","*.$ftype.*.$xpat");


#   Get times valid times for first and last files
#   ----------------------------------------------
    my $nt = @files;  return 1  unless ( $nt );
    my $th1 = get_vtime("$files[0]");
    my $th2 = get_vtime("$files[$nt-1]");

    $lastfile = $files[$nt-1];

#   This assumes no file is missing; it could be made more robust
#   -------------------------------------------------------------
    my $dt_min = 180.; # make something up for when nt=1
    if ( $nt>1 ) {
      $dt_min = int ( 0.5 + ( $th2->{secs} - $th1->{secs} ) / (60 * ($nt-1)) );
      $dt_incr = $dt_min;
    }
    my $dt_hour = int ( 0.5 + $dt_min/60. );
    if ( $dt_hour >= 0 ) {
	$dt_incr = $dt_hour;
    }

#   Create DSET template
#   --------------------
    ($null,$ftype,$ftime,$fex) =
                    parse_fname(basename($files[0]));
    my ($itime,$vtime) = split('\+',$ftime,2);
    unless ( $vtime ) {           # analysis files
	$vtime = $itime;
	$itime = "";
    }

#   Get increment hour from ftype
#   -----------------------------
    if( $ftype =~ 'modis' ) {
        $tincr = 3;
    } else {
        my @splt = split('_', $ftype);
	$_ = $splt[0];
	/^[a-zA-Z]+(\d+)(.*)/;
	$tincr = $1;
 	if ( $2 ){ $tincr .= $2; }
 	else{ $tincr .= "hr"; }
    }

    ###print "\n\nftype: $ftype\n";
    ###print "tincr: $_, $tincr\n\n";

    my $tlen=length($vtime);
    print "\nftype=$ftype vtime=$vtime length=$tlen \n";
    my $tnum;

    if ( length($vtime) == 6 ) {
	$template = '%y4%m2';
	# Monthly file compute number of time step
        # ----------------------------------------
	$tnum = $nt;
	###print "files: @files\n\n";

    } elsif ( length($vtime) == 8 ) {
	$template = '%y4%m2%d2';
	# Daily  file compute number of time step
        # ---------------------------------------
	$tnum = $dt_incr*$nt/$tincr;

    } elsif ( length($vtime) == 12 ) {
        $template = '%y4%m2%d2_%h2z';
	# Hourly file, Nnumber of time step = nb files
	# --------------------------------------------
	$tnum = $nt;

    } elsif ( length($vtime) == 13 ) {
        $template = '%y4%m2%d2_%h2%n2.V01';
	if ( $opt_A ) {
          $expid = "GEOS.fp.asm";
        }
        else {
          $expid = "GEOS.fp.fcst";
        }
	# Hourly file, Nnumber of time step = nb files
	# --------------------------------------------
	$tnum = $nt;

    } elsif ( length($vtime) == 14 ) {
        $template = '%y4%m2%d2_%h2%n2z';
	# Hourly file, Nnumber of time step = nb files
	# --------------------------------------------
	$tnum = $nt;
    } else {
        die "cannot template valid time = <$vtime> ";
    }
    $tnum = int($tnum); # pchakrab: not sure if this is reqd, but doesn't hurt

    $template = "$itime+$template" if ( $itime );

    #
    # Set dsetpath to opt_p
    # Take care of last character of path. Make sure it is "/"
    #
    if( defined $dsetpath ) {
	$last = substr $dsetpath,-1,1;
	if( $last !~ /\// ) {
	    $dsetpath .= "/";
	}
	my @DSET = split('/', $qh->{dset});
	$qh->{dset} = $dsetpath . $DSET[$#DSET];
    }

    my $dset;
    #
    # Do we force a path in DSET?
    #
    if( ( $opt_p ) && !( $opt_c )) {
      # Change dsetpath
      $dsetpath = $opt_p;
      # Take care of last character of path. Make sure it is "/"
      $last = substr $dsetpath,-1,1;
      if( $last !~ /\// ) {
	$dsetpath .= "/";
      }
      $dset = "$dsetpath" .
	"$expid.$ftype.$template.$fex";
      if ( $opt_A ) {
	$kind = "assim";
      }
      else {
	$kind = "fcast";
      }
    }
    elsif ( $opt_A ) {
      $dset = "$rootdir" .
	'/Y%y4/M%m2/D%d2' .
	  "/$expid.$ftype.$template.$fex";
      $kind = "assim";
    } elsif ( $opt_N ) {
      if ($opt_M){
        $dset = "$rootdir" . '/Y%y4/M%m2' . "/$expid.$ftype.$template.$fex";
	$kind = "";
      } else{
        $dset = "$rootdir" . '/Y%y4/M%m2/D%d2' . "/$expid.$ftype.$template.$fex";
	$kind = "";
      }
    } elsif ( $opt_L ) {
        $dset = "$rootdir" . '/Y%y4/M%m2/D%d2' . "/$expid.$ftype.$template.$fex";
	$kind = "";
    } else  {
      $dset = dirname("$files[0]") . "/$expid.$ftype.$template.$fex";
      $kind = "fcast";
    }
    print "dset=$dset\n";

    my $title = get_Title("$ftype","$lastfile");

#   Determine output directory based on style
#   pchakrab: for Nature Runs, output dir is
#   handled separately
#   -----------------------------------------
    my $dir;
    if ( "$Style" eq "flat" ) {
          $dir = "$ddir/opendap/$kind";
    } elsif ( "$Style" eq "by_ftype" ) {
          $dir = "$ddir/opendap/$kind/$ftype";
    } elsif ( "$Style" eq "by_date" ) {
          my $ith = get_itime("$files[0]");
          $dir = "$ddir/opendap/$kind/Y$ith->{yyyy}/M$ith->{mm}/D$ith->{dd}";
    } elsif ( "$Style" eq "by_fdate" ) {
          my $ith = get_itime("$files[0]");
	  $dir = "$ddir/opendap/$kind/$ftype/Y$ith->{yyyy}/M$ith->{mm}";
    } elsif ( "$Style" eq "with_data" ) {
	  $dir = dirname($files[0]);
    } else {
          die "unknown output style <$Style> ";
    }

    my $ddf = "$dir/$expid.$ftype.$itime.ddf";
    my $ctl = "$dir/$expid.$ftype.$itime.ctl";

    if ( $opt_A ) {
	$ddf = "$dir/$ftype";
	if ( $makectl )  {
	    $ctl = "$dir/$ftype.ctl";
	}
    } elsif ($ opt_N ) {
      #   Special handling for nature run
      #   -------------------------------
	my @tmpoutarr = split("/", $rootdir);
	pop(@tmpoutarr);
	$tmpout = join("/", @tmpoutarr);
        $tmpout =~ s/DATA/opendap/g;
	if ($opt_d){
	  my @spltstr = split("/opendap", $tmpout);
	  $dir = $ddir . "/opendap" . $spltstr[1];
	} else{
	  $dir = $tmpout;
	}
        $ddf = "$dir/$ftype";
	if ( $makectl ) {
	    $ctl = "$dir/$ftype.ctl";
	}
   } elsif ( $opt_L ) {
       $ddf = "$dir/$ftype";
       if ( $makectl )  {
          $ctl = "$dir/$ftype.ctl";
       }
    } else {
	$ddf = "$dir/$ftype.$itime";
	if( $makectl )  {
	    $ctl = "$dir/$ftype.$itime.ctl";
	}
    }


    my $rc  = system ( "mkdir -p $dir" );
    die "cannot create directory $dir " if ( $rc );

#   TO DO: for robustness this should be discovered thru hdfdump -h
#   ---------------------------------------------------------------
    my $tvar = "time";  # to do: discover this!
    my $tzero = "$th1->{hh}:$th1->{nn}Z$th1->{dd}$th1->{mmm}$th1->{yyyy}";

    ###print "TINCR: $tincr\n";
    print "TDEF $tvar $tnum LINEAR $tzero $tincr\n";

#   Write DDF
#   ---------
    open DDF, ">$ddf" or  die "cannot open file $ddf";
    print DDF <<EOF   or  die "cannot write to file $ddf";
DSET $dset
TITLE $title
OPTIONS TEMPLATE
TDEF $tvar $tnum LINEAR $tzero $tincr
EOF
    close DDF;

#   Validade DDF
#   -----------
    if ( $opt_V ) {
       $fh = Open("$ddf","XDF") || die "cannot open DDF file $ddf ";
       my $var = $fh->{vars}[2];
       my $fid = $fh->{fid};
       $rc = ga_ <<EOF;
             set gxout stat
             set t $nt
             display $var
             close $fid
EOF
       die "cannot validate DDF file $ddf " if ( $rc );
       print "validated ... \n" if ( $opt_v );
   }


    if( $makectl ) {
	if ( $opt_A ) {
          print "- Creating Assimilation CTL for file type <$ftype> ... " if ( $opt_v )	;
       } elsif ( $opt_N ) {
          print "- Creating Nature Run CTL for file type <$ftype> ... " if ( $opt_v )	;
       } elsif ( $opt_L ) {
          print "- Creating Larry Run CTL for file type <$ftype> ... " if ( $opt_v )	;
       } else {
          print "- Creating Forecast CTL for file type <$ftype> ... " if ( $opt_v )	;
	}
       
	my $dsetpath = undef;
	if( $opt_p ) {
	    $dsetpath = $opt_p ;
	}
	make_ctl($ddf,$ctl, $dsetpath);

#   Validade CTL
#   -----------
	if ( $opt_V ) {
	    ga_('reinit');
	    $fh = Open("$ctl","CTL") || die "cannot open CTL file $ctl ";
	    my $var = $fh->{vars}[0];
	    my $fid = $fh->{fid};
	    $rc = ga_ <<EOF;
                  set gxout stat
                  set t $nt
                  display $var
                  close $fid
EOF
            die "cannot validate CTL file $ctl " if ( $rc );
	    print "validated ... " if ( $opt_v );
	}
	rename($ctl,$ddf);
    }

#  Place copy of latest using flat style
#  -------------------------------------
   if ( $Doing_latest ) {
        my $lddf = "$ddir/opendap/$kind/$ftype.latest";
        cp("$ddf","$lddf");
	print "latest cloned ..." if ( $opt_v );
   }

   print "done!\n\n" if ( $opt_v );

   return 0;

}

#.........................................................................
sub parse_dir {

    my $dir = shift;

    my @tokens = split "/", $dir;
    my $n = @tokens;
    die "invalid directory <$dir> " if ( $n < 3 );

    my ($yyyy,$mm,$dd,$hh);

    if ( $opt_A ) {
      $hh   = "00"; # why not?
      $dd   = substr($tokens[$n-1],1,2);
      $mm   = substr($tokens[$n-2],1,2);
      $yyyy = substr($tokens[$n-3],1,4);
    } elsif ($opt_N) {
      if ($opt_M) {
	$hh   = "00"; # why not?
	$dd   = "01"; # again, why not?
	$mm   = substr($tokens[$n-1],1,2);
	$yyyy = substr($tokens[$n-2],1,4);
      } else{
	$hh   = "00"; # why not?
	$dd   = substr($tokens[$n-1],1,2);
	$mm   = substr($tokens[$n-2],1,2);
	$yyyy = substr($tokens[$n-3],1,4);
      }
    } elsif ($opt_L) {
      $hh   = "00"; # why not?
      $dd   = substr($tokens[$n-1],1,2);
      $mm   = substr($tokens[$n-2],1,2);
      $yyyy = substr($tokens[$n-3],1,4);
    } else {
      $hh   = substr($tokens[$n-1],1,2);
      $dd   = substr($tokens[$n-2],1,2);
      $mm   = substr($tokens[$n-3],1,2);
      $yyyy = substr($tokens[$n-4],1,4);
    }

    my $this_time = "$yyyy$mm$dd$hh";
    if ( $this_time < $Beg_time ) {
	return ();
    } else {
	return ($yyyy,$mm,$dd,$hh);
    }

}

#.........................................................................
sub get_flist {       # get all files in directory/file patterns $dpat/$fpat
    my $dpat = shift; # directory pattern
    my $fpat = shift; # file pattern

    my ($dir,$files,$file);
    my @files; my @FILES;

    my $dirs = `/bin/ls -1d $dpat`;
    my @dirs = split "\n", $dirs;

DIR: for $dir ( @dirs ) {
        next DIR unless ( parse_dir("$dir") );
        $files = `find $dir -type f -name '$fpat' -print -o -type l -name '$fpat' -print`;
        @files = split "\n", $files;
        @sortfiles = sort @files;
        @files = @sortfiles;
        for $file ( @files ) {
            push @FILES, $file;
        }
    }
    return @FILES;
}

#.........................................................................
sub get_vtime {

    my $file = shift;
    my ($null,$ftype,$ftime,$fex) = parse_fname($file);
    my ($itime,$vtime) = split('\+',$ftime,2);
    $vtime = $itime unless ( $vtime ); # analysis files

    my %th;
       $th{vtime} = $vtime;
       $th{yyyy} = substr($vtime,0,4);
       $th{mm}   = substr($vtime,4,2)-1;
       $th{mmm}  = $MONTH[$th{mm}];
       $th{dd}   = substr($vtime,6,2);
       $th{hh}   = substr($vtime,9,2);
       $th{nn}   = substr($vtime,11,2);
       $th{nn}   = "00" if ( "$th{nn}" eq "z" );

       $th{dd}   = "01" if ( "$th{dd}" eq "" ); # in case of monthly means
       $th{hh}   = "00" if ( "$th{hh}" eq "" );
       $th{nn}   = "00" if ( "$th{nn}" eq "" );

       $th{secs} = timegm(0,$th{nn},$th{hh},$th{dd},$th{mm},$th{yyyy});

    return \%th;

}

sub get_itime {

    my $file = shift;

    my ($null,$ftype,$ftime,$fex) = parse_fname($file);
    my ($itime,$vtime) = split('\+',$ftime,2);

    my %th;
       $th{itime} = $itime;
       $th{yyyy} = substr($itime,0,4);
       $th{mm}   = substr($itime,4,2)-1;
       $th{mmm}  = $MONTH[$th{mm}];
       $th{dd}   = substr($itime,6,2);
       $th{hh}   = substr($itime,9,2);
       $th{nn}   = substr($itime,11,2);
       $th{nn}   = "00" if ( "$th{nn}" eq "z" );
       $th{secs} = timegm(0,$th{nn},$th{hh},$th{dd},$th{mm},$th{yyyy});

    return \%th;

}


#.........................................................................
sub parse_fname {
       my $file = shift;

       my ($null,$ftype,$ftime,$fex,$fkind,$flev);
       my @tokens = split('\.', basename($file));

#      Standard GEOS-5 file
#      --------------------
       if ( @tokens == 4 ) {
	   ($expid,$ftype,$ftime,$fex) = @tokens;
       }

#      GEOS-4 type filename convention
#      -------------------------------
       elsif ( @tokens == 5 ) {
	   ($expid,$fkind,$flev,$ftime,$fex) = @tokens;
           $ftype = "$fkind" . "." . "$flev";
       }

#      GEOS-DISC type filename convention
#      DAS.ops.asm.tavg1_2d_flx_Nx.GEOS572.20110731_2130.V01.nc4
#      -------------------------------
       elsif ( @tokens == 8 ) {
           ($null,$null,$null,$ftype,$null,$ftime,$null,$fex) = @tokens;
       }

#      GEOS-DISC type filename convention (e5100_fp and beyond)
#      GEOS.fp.asm.tavg3_3d_udt_Nv.20130515_0730.V01.nc4
#      -------------------------------
       elsif ( @tokens == 7 ) {
           ($null,$null,$null,$ftype,$ftime,$null,$fex) = @tokens;
       }


#      Non-standard file name
#      ----------------------
       else {
	   warn "Ignoring non-standard file " .
	        basename($file) . "\n";
           return undef;
       }

       return ($null,$ftype,$ftime,$fex);

}

#.........................................................................
sub Finalize {
  exit 0;
}

sub get_fTitle {  # Use GrADS to retrieve file Title
    my $file = shift;
    $fh = Open("$file","SDF") || die "cannot open $file ";
    my $fid = $fh->{fid};
    $rc = ga_("reinit");
    return $fh->{title};
}

#.........................................................................
sub get_Title {
    my $ftype = shift;
    my $file  = shift;

#   Some names are hardwired
#   ------------------------
    return "$Title{$ftype}" if ( $Title{$ftype} ); # known name

#   Get title from file metadata
#   ----------------------------
    my $title;
    eval { $title = get_fTitle("$file") };
    print "File title=$title\n";
    return $title if $title;

#   Last resort, make something up
#   ------------------------------
    my ($t,$n,$z) = split '_', "$ftype", 3;

    my $title = "";

    $title = "Instantaneous 2D " if ( "$t" eq "inst2d" );
    $title = "Instantaneous 3D " if ( "$t" eq "inst3d" );
    $title = "Time Averaged 2D " if ( "$t" eq "tavg2d" );
    $title = "Time Averaged 3D " if ( "$t" eq "tavg3d" );

    $title = "$title" . "$n Fields ";

    $title = "$title" . "(p-coords)"         if ( "$z" eq "p" );
    $title = "$title" . "(lcv-coords)"       if ( "$z" eq "v" );
    $title = "$title" . "(lcv edge-coords)"  if ( "$z" eq "e" );

    return $title;

}

#.........................................................................
sub set_Titles {

    %Title = (

inst2d_hwl_x => "Instantaneous 2D Aerosol, CO/CO2 Fields for Hyperwall",
inst2d_met_x => "Instantaneous 2D Meteorological Fields",
inst3d_met_p => "Instantaneous 3D Meteorological Fields (p-coords)",
inst3d_trj_v => "Instantaneous 3D Met Fields for Trajectory Calculations (lcv-coords)",
inst3d_aer_v => "Instantaneous 3D Aerosol Bin-Concentrations (lcv-coords)",
inst3d_chm_v => "Instantaneous 3D CO/CO2  Concentrations",

tavg2d_aer_x => "Time Averaged 2D Aerosol Diagnostics",
tavg2d_chm_x => "Time Averaged 2D CO/CO2  Diagnostics",
tavg2d_met_x => "Time Averaged 2D Meteorological Diagnostics",
tavg3d_aer_p => "Time Averaged 3D Aerosol Concentrations (p-coords)",
tavg3d_chm_p => "Time Averaged 3D CO/CO2  Concentrations (p-coords)",
tavg3d_cld_v => "Time Averaged 3D Cloud Diagnostics (lcv-coords)",
tavg3d_dyn_v => "Time Averaged 3D Dynamical Fields (lcv-coords)",
tavg3d_met_e => "Time Averaged 3D Meteorological Fields (lcv edge-coords)",
tavg3d_met_p => "Time Averaged 3D Meteorological Fields (p-coords)",
tavg3d_mst_v => "Time Averaged 3D Moisture Fields (lcv-coords",
tavg3d_prs_v => "Time Averaged 3D Pressure Fields (lcv-coords)",
tavg3d_tmp_v => "Time Averaged 3D Temperature Fields (lcv-coords)",
tavg3d_wnd_v => "Time Averaged 3D Wind Fields (lcv-coords)",
'modis-a_land' => "MODIS Aerosol Neural Net Retrievals (Aqua,Land)",
'modis-t_land' => "MODIS Aerosol Neural Net Retrievals (Terra,Land)",
'modis-a_ocean' => "MODIS Aerosol Neural Net Retrievals (Aqua,Ocean)",
'modis-t_ocean' => "MODIS Aerosol Neural Net Retrievals (Terra,Ocean)",

# Copied descriptions from the ~/operations/prep_disc directory.
# TO-DO: develop code that looks in the HDF file for LongName and extracts it for the
#        OpenDAP title if it exists.  (RL 2013-08-10)

const_2d_asm_Nx => "GEOS5 FP 2d constants",
inst1_2d_lfo_Nx => "GEOS5 FP 2d time-averaged land surface forcing",
inst1_2d_smp_Nx => "GEOS5 FP 2d instantaneous diagnostics for SMAP",
inst3_2d_asm_Nx => "GEOS5 FP 2d assimilated state",
inst3_2d_smp_Nx => "GEOS5 FP 2d instantaneous diagnostics for SMAP",
inst3_3d_aer_Nv => "GEOS5 FP 3d instantaneous aerosol diagnostics",
inst3_3d_asm_Np => "GEOS5 FP 3d assimilated state on pressure levels",
inst3_3d_asm_Nv => "GEOS5 FP 3d assimilated state on native levels",
inst3_3d_chm_Nv => "GEOS5 FP 3d instantaneous chemistry diagnostics",
inst3_3d_tag_Np => "GEOS5 FP 3d tag",
tavg1_2d_flx_Nx => "GEOS5 FP 2d time-averaged surface flux diagnostics",
tavg1_2d_lfo_Nx => "GEOS5 FP 2d instantaneous land surface forcing",
tavg1_2d_lnd_Nx => "GEOS5 FP 2d time-averaged land surface diagnostics",
tavg1_2d_ocn_Nx => "GEOS5 FP 2d time-averaged ocean related variables",
tavg1_2d_rad_Nx => "GEOS5 FP 2d time-averaged radiation diagnostics",
tavg1_2d_slv_Nx => "GEOS5 FP 2d time-averaged single level diagnostics",
tavg3_2d_adg_Nx => "GEOS5 FP 2d time-averaged extended aerosol diagnostics",
tavg3_2d_aer_Nx => "GEOS5 FP 2d time-averaged primary aerosol diagnostics",
tavg3_2d_chm_Nx => "GEOS5 FP 2d time-averaged chemistry diagnostics",
tavg3_2d_ctm_Cx => "GEOS5 FP 2d reduced-resolution time-averaged parameters for CTM support",
tavg3_2d_ctm_Nx => "GEOS5 FP 2d time-averaged parameters for CTM support",
tavg3_2d_smp_Nx => "GEOS5 FP 2d time-averaged diagnostics for SMAP",
tavg3_3d_asm_Nv => "GEOS5.10.0 FP 3d time-averaged assimilated state on native levels",
tavg3_3d_cld_Nv => "GEOS5 FP 3d time-averaged cloud diagnostics",
tavg3_3d_ctm_Ce => "GEOS5 FP 3d reduced-resolution time-averaged parameters for CTM support at edges",
tavg3_3d_ctm_Cv => "GEOS5 FP 3d reduced-resolution time-averaged parameters for CTM support",
tavg3_3d_ctm_Ne => "GEOS5 FP 3d time-averaged parameters for CTM support at edges",
tavg3_3d_ctm_Nv => "GEOS5 FP 3d time-averaged parameters for CTM support",
tavg3_3d_lsf_Ne => "GEOS5 FP 3d time-averaged large-scale flux at edges",
tavg3_3d_lsf_Nv => "GEOS5 FP 3d time-averaged large-scale flux",
tavg3_3d_mst_Ne => "GEOS5 FP 3d time-averaged moist processes diagnostics at edges",
tavg3_3d_mst_Nv => "GEOS5 FP 3d time-averaged moist processes diagnostics",
tavg3_3d_nav_Ne => "GEOS5 FP 3d time-averaged vertical coordinate navigation at edges",
tavg3_3d_nav_Nv => "GEOS5 FP 3d time-averaged vertical coordinate navigation",
tavg3_3d_odt_Nv => "GEOS5 FP 3d time-averaged ozone tendencies",
tavg3_3d_qdt_Nv => "GEOS5 FP 3d time-averaged moisture tendencies",
tavg3_3d_rad_Nv => "GEOS5 FP 3d time-averaged radiation diagnostics",
tavg3_3d_tdt_Nv => "GEOS5 FP 3d time-averaged temperature tendencies",
tavg3_3d_trb_Ne => "GEOS5 FP 3d time-averaged turbulence diagnostics at edges",
tavg3_3d_udt_Nv => "GEOS5 FP 3d time-averaged wind tendencies",

    );

    ### print Dumper(\%Title);

}

#.........................................................................

sub usage {

   print <<"EOF";

NAME
     g5_ddf - Creates DDF for GEOS-5 DAS/Forecasts

SYNOPSIS

     g5_ddf.pl    [OPTIONS]  expdir  yyyy mm dd hh
     g5_ddf.pl    [OPTIONS]  expdir  latest
     g5_ddf.pl -a [OPTIONS]  expdir  [yyyy mm dd hh]
     g5_ddf.pl -A [OPTIONS]  expdir  [yyyy mm dd hh]

DESCRIPTION

     Given an experiment root directory and date/time, this script
     examines the experiment subdirectories forecast/ or das/ and
     creates GrADS Data Descriptor Files (DDFs) for accessing the data
     files files as an aggregated time series (using GrADS templates).

     In FORECAST mode (the default), one can specify a particular
     date/date for the initial condition of the forecast or "latest"
     for having the last available forecast date/time. When ALL
     (option -a) initial conditions are desired, the date/time on
     the command line specifies the first date/time to be considered;
     this is useful to weeding out files in the inital configuration
     phase of the experiment.

     In ASSIMILATION mode (option -A), a single DDF is generated for
     each file file type. The optional date/time on the command line
     specifies the the first date/time to be considered.


OPTIONS
     -A        assimilation mode
     -L        Larry Takacs experiment mode
     -N        nature run mode [does NOT create montly DSET]
     -M        create monthly DSET [ONLY for nature runs]
     -a        all times starting at yyyy mm dd hh
     -c        force ctl file creation
     -d ddir   destination directory for DDF tree; default is expdir
     -E expid  default expid is last node of supplied expdir path
     -F ftypes comma separated collections (default is all )
     -X ftypes comma separated collections to exclude (default is none )
     -h        print this page
     -p        force new path for DSET
     -T tmplte Force a template file for a collection (nc4/hdf).
     -s style  directory structure style for output DDF files. If \$ddir
               is the destination directory (option -d), define
                    \$dir = \$ddir/opendap/assim    or
                    \$dir = \$ddir/opendap/fcast
               as the case may be for assimilation/forecasts. The "style"
               option can be one of these:
                flat      --- all files under \$dir; this is the
                              DEFAULT in ASSIMILATION mode
                by_ftype  --- files under \$dir/\$ftype, where
                              \$ftype is the file type (second "." delimited
			      token in file name); this is the DEFAULT in
                              FORECAST mode
                by_date   --- files under \$dir/Y\%y4/M\%m2/D\%d2
                by_fdate  --- files under \$dir/\$ftype/Y\%y4/M\%m2
                with_data --- same directory as data files (not recommended
                              for OpenDAP server setup)
     -V        use GrADS to validade DDF;
     -v        verbose mode
     -x        file extension; default is nc4;
     -w dir    directory for temporary files; default is "."

EXAMPLES

    1) Assimilation DDFs starting from 2007-07-01 00Z
     % g5_ddf.pl -A -v -d . /portal/tc4/gmao_ops/d5_tc4_01 2007 07 01 00

    2) Forecast DDFs for 2007-07-01 00Z initial condition with GrADS
       validation
     % g5_ddf.pl -V -v -d . /portal/tc4/gmao_ops/d5_tc4_01 2007 07 01 00

    3) All Forecast DDFs from  2007-07-01 00Z initial condition
     % g5_ddf.pl -a -v -d . /portal/tc4/gmao_ops/d5_tc4_01 2007 07 01 00

    4) Forecast DDFs for latest initial condition; a copy of the DDF
       is placed at the top directory with "latest_fcst" replacing
       the initial condition data (only when -s is not specified)
     % g5_ddf.pl    -v -d . /portal/tc4/gmao_ops/d5_tc4_01 latest

    5) Create forecast CTLs for latest initial condition;
     % g5_ddf.pl -c -v -d . /portal/tc4/gmao_ops/d5_tc4_01 latest

    6) Assimilation DDFs starting from 2009-11-10 00z for collections:
       inst2d_hwl_x and tavg3d_aer_p;
     % g5_ddf.pl -F inst2d_hwl_x,tavg3d_aer_p -x hdf -c -v -A \
          -d . /portal/gmao_ops/d520_fp/ 2009 10 31 00

    7) Force a new path to DSET.
     % g5_ddf.pl -c -p /temp/portal -v -d . /portal/tc4/gmao_ops/d5_tc4_01 latest

    8) For nature run (no monthly dset)
     % g5_ddf.pl -N -v -c -d . /path/to/expdir 

    9) For nature run (only monthly dset)
     % g5_ddf.pl -N -M -v -c -d . /path/to/expdir

    10) For runs made with the Larry Takacs script
     % g5_ddf.pl -L -v -c -d . /path/to/expdir

BUGS
     The time step for each DDF is determined by examining the first and the
     last time in the resolved list and dividing the the number of files.
     This would fail if there are missing files.

     The nature run outputs are organized as:
           <exp_dir>/<collection_id>/Yyyyy/Mmm/Ddd.
     g5_ddf assumes that all collections have the same directory structure
     which may not always be the case.

AUTHOR
     Arlindo da Silva, NASA/GSFC.

EOF

  exit(1)

 }
