#!/usr/bin/perl
#
#  Creates PNG images from GEOS-4 forecast files for the TC4 WxMaps
#  website. This is a  
#
#.........................................................................

use   Time::Local;
use Getopt::Std; 
use   File::Basename;

use  Gerl;

# Initialize
# ----------
  Initialize();

# Start GrADS
# -----------
  grads { Bin=>$GABIN, Verb=>1, Echo=>0, Port=>0, Window=>0 };

# Open data file
# --------------
  $fh = Open "$fname" ||  die "cannot open $fname";

  set mpdset, "mres";

# Loop over time steps on file
# ----------------------------
TIME: for $t ( 1..$fh->{nt} ) {

#   Set valid time
#   --------------
    set t, $t;
    set_valid_time();
    last TIME if ( $tau > $TAU );

#   Loop over regions
#   -----------------
    for $region ( @REGION ) {

        $Region = substr($expid,0,2) . "-" . $region;

#       Set horizontal domain for this region
#       -------------------------------------
	set lon, "$LON{$region}";
	set lat, "$LAT{$region}";

	for $lev ( @LEV ) {

	    set lev, $lev;
 
        # Convert winds to knots
	    define uu, "1.94384 * u"; 
	    define vv, "1.94384 * v"; 

	    plot_uvt()   if ( $doing_uvt   );
	    plot_uvrh()  if ( $doing_uvrh  );
	    plot_uvpr()  if ( $doing_uvpr  );
	    plot_uvslp() if ( $doing_uvslp );

	}  # lev

    } # region

} # time
  
# All done
# --------
  Finalize();
  exit(0);


#.........................................................................

sub Initialize {

# Parse cmd line args
# -------------------
  getopts('hvd:g:w:T:');
  usage() if ( $opt_h || $#ARGV < 0 );

# Load configuration file
# -----------------------
#use    XML::Simple;
#use   Data::Dumper;
#  my $cf = XMLin(); 
#  print Dumper($cf);

# Hardwire these for now, eventually get these from cf
# ----------------------------------------------------
  $Iam = "tc4_wx";
  $FTYPE  = "inst3d_met_p";
  @TAU    = qw(00 03 06 09 12 15 18 21 24 27 30 33 36 39 42 45 48);
  @LEV    = qw( 925 850 700 500 300 200 100 );
  @MONTH  = qw( jan feb mar apr may jun jul aug sep oct nov dec );

  @REGION = qw(tc4_small tc4_large);
  $LON{tc4_small} = ' -100 -40';    $LON{tc4_large} = '-120 -20';
  $LAT{tc4_small} = '   -5  25';    $LAT{tc4_large} = ' -15  35';

# Select 
  if ( $opt_g ) {
      $doing_uvt   = 0;
      $doing_uvrh  = 0;
      $doing_uvpr  = 0;
      $doing_uvslp = 0;
      eval "\$doing_$opt_g = 1";
  } else {
      $doing_uvt   = 1;
      $doing_uvrh  = 1;
      $doing_uvpr  = 1;
      $doing_uvslp = 1;
  }
  unless ( $doing_uvt || $doing_uvrh || $doing_uvpr || $doing_uvslp ) {
      die "no valid image group selected; check your -g option: <$opt_g> ";
  }

# Parse file name
# ---------------
  $fname = $ARGV[0] or die "missing file name";
  ($expid,$ftype,$ftime,$fex) = split('\.', basename($fname),4);

# Parse initial time string
# -------------------------
  ($itime,$vtime) = split('\+',$ftime);
  $iyyyy = substr($itime,0,4);
  $imm   = substr($itime,4,2);
  $immm  = $MONTH[$imm-1];
  $idd   = substr($itime,6,2);
  $ihh   = substr($itime,9,2);

  $yyyy=$iyyyy; $mm=$imm; $dd=$idd; $hh=$ihh;
  $dtg = "$iyyyy$imm$idd$ihh";

# Maximum tau to plot
# -------------------
  $TAU = 72 unless ( $TAU = $opt_T );

# Only handles one file type for now
# ----------------------------------
  if ( "$ftype" =~ $FTYPE ) {
      $ftype = $FTYPE;  # because $ftype may have "unz"
  } else {
      warn "$Iam: ignoring file type $ftype\n" if $opt_v;
      exit(0);
  }

# Special case: GFS forecast from NOAA OpenDAP server
# ---------------------------------------------------
  $tdir = "." unless ( $tdir = $opt_w );
  if ( "$expid" =~ /^gfs/ ) {
      $PRODUCT = "NOAA/NCEP - GFS"; 
      $SKIP{tc4_small} = 2;  $SKIP{tc4_large} = 3;
      $GABIN = "gradsdods";
      $fname = create_gfs_ddf();
      $doing_gfs = 1;
  }
  else {
      $PRODUCT = "NASA/GSFC Global Modeling and Assimilation Office - GEOS-5"; 
      if ( "$expid" =~ /^d5/ ) {
	    $SKIP{tc4_small} = 4;  $SKIP{tc4_large} = 6;
      } else {
	    $SKIP{tc4_small} = 8;  $SKIP{tc4_large} = 12;
      }
      $GABIN = "gradshdf";
      $doing_gfs = 0;
  } 

# Image related settings
# ----------------------
  $ddir = "." unless ( $ddir = $opt_d );
  $imfile = "$tdir/$Iam.$$~"; # temp file name 
  $imdir = "$ddir/DTGS/$dtg";
  if ( "$ddir" =~ /\:/ ) {   # remote destination
        ( $rhost,$rdir) = split ":", $ddir;
        $rdir = "$rdir/DTGS/$dtg";
        $rc = system("ssh -q $rhost /bin/mkdir -p $rdir");
  } else {
        $rc = system("mkdir -p $imdir");
  }
  die "cannot create image directory $imdir" if ( $rc );

###  $gxfile = "$Iam.$$.gx~"; # temp file name 
###  $gxyat = `which gxyat`;
###  die "cannot find required metadata translator gxyat" if ( $? );
###  chomp($gxyat);    

#   Define Standard Colors
#   ----------------------
    %CT = %{DefineStdCT()};

}

#.........................................................................

sub set_valid_time {

#   Get valid date/time from environment
#   ------------------------------------
    my $qh = Query "time" or die "cannot query time ";
    $vyyyy = $qh->{yyyy}[0];
    $vmm   = $qh->{mm}[0];
    $vdd   = $qh->{dd}[0];
    $vhh   = $qh->{hh}[0];

#   Determine forecast lead time (in ours)
#   --------------------------------------
    my $isec = timegm(0,0,$ihh,$idd,$imm,$iyyyy);
    my $vsec = timegm(0,0,$vhh,$vdd,$vmm,$vyyyy);
    $tau = int( ($vsec - $isec) /  ( 60 * 60 ) );
    $taustr  = sprintf '%03d', $tau;

}

#.........................................................................

sub create_gfs_ddf {  # hardwire URL for now
  
    $gfsfile = "$tdir/gfs.$$.ddf";

    open(GFS,">$gfsfile") or die "cannot open $gfsfile";

    my $hhz = "$ihh" . "z";
    print GFS <<EOF;
DSET http://nomad2.ncep.noaa.gov:9090/dods/gfs/gfs$iyyyy$imm$idd/gfs_$hhz
TDEF time 61 LINEAR $hhz$idd$immm$iyyyy 6hr
VARS 6
oprate=>prectot 0 0 Surface Precipitation [kg/m2/s]
prmsl=>slp      0 0  Sea Level Pressure
ugrd=>u        26 0 Zonal Wind [m/s]
vgrd=>v        26 0 Meridional Wind [m/s]
tmp=>t         26 0 Temperature [K]
rh             26 0 Relative humidity [%]
ENDVARS
EOF

   close GFS;

   return $gfsfile;

}

#.........................................................................

sub gxyat_ {

    my $img = shift; # destination image file

    print "$Iam: creating $img ... " if ( $opt_v );

    $rc = gxyat "$imfile";
    die "error writing local image file <$imfile>"    if ( $rc );
    $rc = system("scp -q $imfile $img");
    die "error writing destination image file <$img>" if ( $rc );

    print "done!\n" if ( $opt_v );

}

#.........................................................................

sub Finalize {

    unlink("$imfile");
    unlink("$gfsfile");

}

#.........................................................................

sub external_gxyat_ {

    my $img = shift;

    enable 'print', "$gxfile";
    print_;
    disable 'print';
    print "$Iam: creating $img ... " if ( $opt_v );
    $rc = system("$gxyat -o $img $gxfile");
    die "error running $gxyat" if ( $rc );
    print "done!\n" if ( $opt_v );
}

#.........................................................................

#                      ------------------
#                      Plotting Functions
#                      ------------------

#.........................................................................

sub plot_uvt {

  query('time');
  my $day = rword(1,6);
  my $vtime = "$day ${vhh}z $vyyyy-$vmm-${vdd}";

  my $img = "$imdir/p$lev.uvt.$taustr.$Region.png";
  my $skip = $SKIP{$region};

  clear;

# Generate contour levels
# -----------------------
  set gxout, "stat";
  display "t-273";
  my $cmin = rword(9,5);
  my $cmax = rword(9,6);
  my $cint = rword(9,7);
  my $cnum = int 1.5 + ($cmax - $cmin)/$cint;
  if ( $cnum > 12 ) {
      $cnum = 12;
      $cmax = $cmin + ($cnum-1) * $cint;
  }
  $clevs = Get_CLEVS($cmin,$cmax,$cnum,"LINEAR");

# Blue/Red Color Table
# --------------------
  Bind_CLEVS ( $clevs, $CT{Rainbow}, 4, "LINEAR", 20 );  

# Do the plotting
# ---------------
  $rc = ga_ <<"EOF";
        set gxout shaded
        set grads off
        set cint 1
        set map  0 1 10
        display t - 273
        run cbarc * * * 4
        draw title $lev hPa Winds [knots] and Temperature [C] \\ Valid: $vtime (`3t`0= $tau)
        set cthick 5
        set ccolor 1
        set gxout barb
        d skip(uu,$skip,$skip);vv
        set string 4 c
        draw string 5.25 8.25 $PRODUCT Forecast Initialized on ${hh}z $yyyy-$mm-${dd} 

EOF

    die "cannot plot winds and T from $fname" if ( $rc );

#   Write the image file
#   --------------------
    gxyat_("$img");

}

#.........................................................................

sub plot_uvrh {

  query('time');
  my $day = rword(1,6);
  my $vtime = "$day ${vhh}z $vyyyy-$vmm-${vdd}";

  my $img = "$imdir/p$lev.uvrh.$taustr.$Region.png";
  my $skip = $SKIP{$region};

  my $factor;
  
  if ( $doing_gfs ) { $factor = 1;   }
  else              { $factor = 100; }

  clear;

# Blue/Red Color Table
# --------------------
  Bind_CLEVS ( "0 10 20 30 40 50 60 70 80 90 100 110",
               $CT{RedBlues}, 2, "LINEAR", 20 );  

# Do the plotting
# ---------------
  $rc = ga_ <<"EOF";
        set gxout shaded
        set grads off
        set map  7      1       10
        display $factor*rh
        run cbarc * * * 2
        draw title $lev hPa Winds [knots] and Relative Humidity [%] \\ Valid: $vtime (`3t`0= $tau)
        set cthick 5
        set ccolor 1
        set gxout barb
        d skip(uu,$skip,$skip);vv
        set string 4 c
        draw string 5.25 8.25 $PRODUCT Forecast Initialized on ${hh}z $yyyy-$mm-${dd} 

EOF

    die "cannot plot winds and RH from $fname" if ( $rc );

#   Write the image file
#   --------------------
    gxyat_("$img");

}
#.........................................................................

sub plot_uvpr {

  return if ( $doing_gfs && $tau == 0 );

  query('time');
  my $day = rword(1,6);
  my $vtime = "$day ${vhh}z $vyyyy-$vmm-${vdd}";

  my $img = "$imdir/p$lev.uvpr.$taustr.$Region.png";
  my $skip = $SKIP{$region};

  clear;

# Use warm rainbow color table
# ----------------------------
  Bind_CLEVS ( "1 2 4 6 8 16 32 64", $CT{Rain}, 4, "LOG", 20 );  

# Do the plotting
# ---------------
  $rc = ga_ <<"EOF";
        set gxout shaded
        set grads off
        set map  4 1 10
        display prectot * 24 * 60 * 60
        run cbarc * * * 4
        draw title $lev hPa Winds [knots] and Surface Precipitation [mm/day] \\ Valid: $vtime (`3t`0= $tau)
        set cthick 5
        set ccolor 1
        set gxout barb
        d skip(uu,$skip,$skip);vv
        set string 4 c
        draw string 5.25 8.25 $PRODUCT Forecast Initialized on ${hh}z $yyyy-$mm-${dd} 

EOF

    die "cannot plot winds and precipitation from $fname" if ( $rc );

#   Write the image file
#   --------------------
    gxyat_("$img");

}

#.........................................................................

sub plot_uvslp {

  query('time');
  my $day = rword(1,6);
  my $vtime = "$day ${vhh}z $vyyyy-$vmm-${vdd}";

  my $img = "$imdir/p$lev.uvslp.$taustr.$Region.png";
  my $skip = $SKIP{$region};

  SLP_RGB();

###  my $ccols = "55  54  53  52   51   50   49   48   47   46   45   44   43   42   41   40   99   60   61   62   63   64   65   66   67   68   69";
###  my $clevs = "952 960 968  972  976  980  984  988  992  996 1000 1002 1004 1006 1008 1010 1012 1014 1016 1018 1020 1024 1026 1030 1038 1046";

  my $ccols = "55  54  53  52   51   50   49   48   47   46   45   44   43   42   41   40    0    0    0    0   60   61  62 63 64   65   66   67   68   69";
  my $clevs = "952 960 968  972  976  980  984  988  992  996 1000 1002 1004 1006 1008 1010 1012 1014 1016 1018 1020 1024 1026 1030 1038 1046";

# Do the plotting
# ---------------
  $rc = ga_ <<"EOF";
        clear
        set gxout shaded
        set grads off
        set map  12 1 10
        set clevs $clevs
        set ccols $ccols
        display slp/100
        run cbarc * * * 4
        draw title $lev hPa Winds [knots] and Sea Level Pressure [hPa] \\ Valid: $vtime (`3t`0= $tau)
        set gxout contour
        set cthick 8
        set ccolor 1
        set clevs $clevs
        display slp/100
        set cthick 5
        set ccolor 97
        set gxout stream
        d uu;vv
        set string 4 c
        draw string 5.25 8.25 $PRODUCT Forecast Initialized on ${hh}z $yyyy-$mm-${dd} 

EOF

    die "cannot plot winds and SLP from $fname" if ( $rc );

#   Write the image file
#   --------------------
    gxyat_("$img");

}

#.........................................................................

#                      ----------------------
#                      Color Table Management
#                      ----------------------

#.....................................................................

sub Bind_CLEVS {

    my ( $clevs, $rgba, $nsub, $type, $cbeg ) = @_;

#   Refinement
#   ----------
    $rgba_f  = Refine_RGB($rgba,$nsub);
    $ccols_f =    Set_RGB($rgba_f,$cbeg);
    $clevs_f = Refine_CLEVS($clevs,$nsub,$type);
    
#   Bind refined contour levels/colors
#   ----------------------------------
    $rc = ga_ <<"EOF";
          set clevs $clevs_f
          set ccols $ccols_f
EOF

    die "cannot bind contour levels/colors" if ( $rc );

}
 
#.....................................................................

sub Refine_CLEVS {

   my ( $clevs, $nsub, $type ) = @_;

   my @clevs = split " ", $clevs;

   my $m, $ns, $dclev;

   my @clevs_f = undef;
   if ( uc "$type" eq "LOG" ) {
       for $m ( 0..@clevs-2 ) {
	   $dclev = ( log($clevs[$m+1]) - log($clevs[$m]) ) / $nsub;
	   push @clevs_f,$clevs[$m]; 
	   for $ns ( 2..$nsub ) {
	       $clev = log($clevs[$m]) + ($ns-1) * $dclev;
               $clev = exp($clev);
               $clev = sprintf "%3.2f", $clev;
	       push @clevs_f, "$clev" ;
	   } 
       }
       push @clevs_f,$clevs[@clevs-1]; 

   } else {
       for $m ( 0..@clevs-2 ) {
	   $dclev = ( $clevs[$m+1] - $clevs[$m] ) / $nsub;
	   push @clevs_f,$clevs[$m]; 
	   for $ns ( 2..$nsub ) {
	       $clev = $clevs[$m] + ($ns-1) * $dclev;
	       push @clevs_f, "$clev" ;
	   } 
       }
       push @clevs_f,$clevs[@clevs-1]; 
   }

   $clevs_f = join(' ', @clevs_f);

   return $clevs_f;

}
#.....................................................................

sub Refine_RGB {                          # Refine colors

    my ( $rgba, $nsub ) = @_;

    my $n, $dch;
    my $rgba_f;

    $nc = @$rgba;
    $nc_f = $nsub * $nc -1; # no. colors after refinement

    $rgba_f[0] = [];
    for $n ( 1..$nc_f-1) { push(@$rgba_f,[]); }

    $nch = @{$rgba->[0]}; 

    for $ch ( 0..$nch-1 ) { # for each [x,] r, g, b, a
	$n = 0;
	for $col ( 0..$nc-2 ) {
            $dch = ($rgba->[$col+1][$ch] - $rgba->[$col][$ch]) / $nsub ;
            $rgba_f[$n][$ch] = $rgba->[$col][$ch]; $n++;
            if ( $ch==0 && $nch>4 ) {
		for $ns ( 1..$nsub-1 ) {
		    $rgba_f[$n][$ch] = $rgba_f[$n-1][$ch] + $dch;
		    $n++;
		}
	    } else {
		for $ns ( 1..$nsub-1 ) {
		    $rgba_f[$n][$ch] = int($rgba_f[$n-1][$ch] + $dch + 0.5);
		    $n++;
		}
	    }
	}
	$rgba_f[$n][$ch] = $rgba->[$nc-1][$ch]; 
    }

    return \@rgba_f;

}

#.....................................................................

sub Set_RGB {
    my ( $rgba, $cbeg ) = @_;
    my $ccols = "";
    for $n (0..@$rgba-1) {
        $c = $cbeg + $n;
        $cols = "$c " . join(' ',@{$rgba->[$n]});
        set rgb, "$cols";
        $ccols = "$ccols $c";
    }
    return $ccols;

}

#....................................................................

sub Get_CLEVS {

    my ( $cmin, $cmax, $cnum, $type ) = @_;

    my $doing_log = 0;
    if ( uc "$type" eq "LOG" ) {
         $cmin = log($cmin);
         $cmax = log($cmax);
         $doing_log = 1;
    } 

    $dc = ($cmax - $cmin) / ($cnum-1) if ( $cnum );
    my @clevs = undef;
    for $n ( 0..$cnum-1 ) {
        $clev = $cmin + $n * $dc;
        if ( $doing_log ) { $clev = exp($clev); }
        $clev = sprintf "%3.2f", $clev;
        push @clevs, " $clev" ;
    }
    $clevs = join(' ', @clevs);
#    set clevs, "$clevs";

    return $clevs;

}

#....................................................................

sub Set_CCOLS {

    my ( $cbeg, $cnum ) = @_;

    for $n ( 0..$cnum-1 ) {
        $ccol = $cbeg + $n;
        push @ccols, " $ccol" ;
    }
    $ccols = join(' ', @ccols);
    set ccols, "$ccols";

    return 0;

}

#.........................................................................

sub DefineStdCT {

  my %ct = {};

# Default GrADS Rainbow sequence
# ------------------------------
  $Rainbow[0]  = [ 160,   0, 200, 0 ];  
  $Rainbow[1]  = [ 130,   0, 220, 0 ];  
  $Rainbow[2]  = [  30,  60, 255, 0 ];  
  $Rainbow[3]  = [   0, 160, 255, 0 ];  
  $Rainbow[4]  = [   0, 200, 200, 0 ];  
  $Rainbow[5]  = [   0, 210, 140, 0 ];  
  $Rainbow[6]  = [   0, 220,   0, 0 ];  
  $Rainbow[7]  = [ 160, 230,  50, 0 ];  
  $Rainbow[8]  = [ 230, 220,  50, 0 ];  
  $Rainbow[9]  = [ 230, 175,  45, 0 ];  
  $Rainbow[10] = [ 240, 130,  40, 0 ];  
  $Rainbow[11] = [ 250,  60,  60, 0 ];  
  $Rainbow[12] = [ 240,   0, 130, 0 ];  $ct{Rainbow} = \@Rainbow;


# Warm Rainbow sequence
# ---------------------
#  $Rain[0] = [ 230, 255, 225, 100 ];
  $Rain[0] = [ 255, 255, 255, 100 ];
  $Rain[1] = [ 200, 255, 190,  90 ];
  $Rain[2] = [ 180, 250, 170,  80 ];
  $Rain[3]  = [   0, 220,   0, 0 ];  
  $Rain[4]  = [ 160, 230,  50, 0 ];  
  $Rain[5]  = [ 230, 220,  50, 0 ];  
  $Rain[6]  = [ 230, 175,  45, 0 ];  
  $Rain[7] = [ 240, 130,  40, 0 ];  
  $Rain[8] = [ 250,  60,  60, 0 ];  $ct{Rain} = \@Rain;
#  $Rain[9] = [ 240,   0, 130, 0 ];  


# Light GREEN to dark green
# -------------------------
  $Greens[0] = [ 230, 255, 225, 100 ];
  $Greens[1] = [ 200, 255, 190,  90 ];
  $Greens[2] = [ 180, 250, 170,  80 ];
  $Greens[3] = [ 150, 245, 140,  60 ];
  $Greens[4] = [ 120, 245, 115,  40 ];
  $Greens[5] = [  80, 240,  80,  20 ];
  $Greens[6] = [  55, 210,  60,   0 ];
  $Greens[7] = [  30, 180,  30,   0 ];
  $Greens[8] = [   5, 150,   5,   0 ];     $ct{Greens} = \@Greens;

# Light BLUE to dark blue
# -----------------------
  $Blues[0] = [ 225, 255, 255,  90 ];
  $Blues[1] = [ 180, 240, 250,  60 ];
  $Blues[2] = [ 150, 210, 250,  30 ]; 
  $Blues[3] = [ 120, 185, 250,  20 ];
  $Blues[4] = [  80, 165, 245,  10 ];
  $Blues[5] = [  60, 150, 245,   0 ];
  $Blues[6] = [  40, 130, 240,   0 ];
  $Blues[7] = [  30, 110, 235,   0 ];
  $Blues[8] = [  20, 100, 210,   0 ];      $ct{Blues} = \@Blues;

# Light PURPLE to dark purple
# ---------------------------
  $Purples[0] = [ 220, 220, 255, 100 ];
  $Purples[1] = [ 192, 180, 255, 90 ];
  $Purples[2] = [ 160, 140, 255, 40 ];
  $Purples[3] = [ 128, 112, 235, 20 ];
  $Purples[4] = [ 112,  96, 220, 10 ];
  $Purples[5] = [  72,  60, 200, 0 ];
  $Purples[6] = [  60,  40, 180, 0 ];
  $Purples[7] = [  45,  30, 165, 0 ];
  $Purples[8] = [  40,   0, 160, 0 ];      $ct{Purples} = \@Purples;

# Light PINK to dark rose  
# -----------------------
  $Pinks[0] = [ 255, 230, 230, 100 ];
  $Pinks[1] = [ 255, 200, 200,  80 ];
  $Pinks[2] = [ 248, 160, 160,  60 ];
  $Pinks[3] = [ 230, 140, 140,  30 ];
  $Pinks[4] = [ 230, 112, 112,  20 ];
  $Pinks[5] = [ 230,  80,  80,  10 ];
  $Pinks[6] = [ 200,  60,  60,   0 ];
  $Pinks[7] = [ 180,  40,  40,   0 ];
  $Pinks[8] = [ 164,  32,  32,   0 ];      $ct{Pinks} = \@Pinks;


# Red-Grey-Blue Sequence
# ----------------------
  $RedBlues[0]  = [  35,  35,  220,   0 ];        
  $RedBlues[1]  = [  50,  50,  230,   0 ];        
  $RedBlues[2]  = [  70,  70,  235,   0 ];        
  $RedBlues[3]  = [  90,  90,  240,   0 ];        
  $RedBlues[4]  = [ 110, 110,  245,   0 ];          
  $RedBlues[5]  = [ 150, 150,  250,   0 ];           
  $RedBlues[ 6] = [ 250, 150,  150,   0 ];         
  $RedBlues[ 7] = [ 245, 120,  120,   0 ];         
  $RedBlues[ 8] = [ 240,  90,   90,   0 ];       
  $RedBlues[ 9] = [ 230,  50,   50,   0 ];       
  $RedBlues[10] = [ 190,  25,   25,   0 ];          
  $RedBlues[11] = [ 170,  10,   10,   0 ];         
  $RedBlues[12] = [ 150,   1,    1,   0 ];  $ct{RedBlues} = \@RedBlues;

# All Done.
# ---------
  return \%ct;

}

#.........................................................................

sub SLP_RGB {

    $rc = ga_ <<"EOF";
        set rgb 40 190 190  255         
        set rgb 41 150 150  250         
        set rgb 42 110 110  245        
        set rgb 43 90 90  240        
        set rgb 44 70 70  235        
        set rgb 45 50 50  230        
        set rgb 46 42 42  225        
        set rgb 47 35 35  220        
        set rgb 48 28 28 205        
        set rgb 49 20 20 190        
        set rgb 50 15 15 180        
        set rgb 51 10 10 170        
        set rgb 52  5 5 165        
        set rgb 53 3 3 150        
        set rgb 54 1 1 135        
        set rgb 55 1 1 120        
        set rgb 60 255 180 180        
        set rgb 61 250 150 150        
        set rgb 62 245 120 120        
        set rgb 63 240 90 90        
        set rgb 64 235 70 70        
        set rgb 65 230 50 50        
        set rgb 66 220  35 35           
        set rgb 67 190 25 25           
        set rgb 68 170  10  10        
        set rgb 69 150  1 1        
        set rgb 97   0 90 0        
        set rgb 98 250 250 250        
        set rgb 99 165 165 165        
EOF

    die "cannot set SLP colors" if ( $rc );

}

#.........................................................................

sub external_gxyat_ {

    my $img = shift;

    enable 'print', "$gxfile";
    print_;
    disable 'print';
    print "$Iam: creating $img ... " if ( $opt_v );
    $rc = system("$gxyat -o $img $gxfile");
    die "error running $gxyat" if ( $rc );
    print "done!\n" if ( $opt_v );
}

#.........................................................................

sub usage {

   print <<"EOF";

NAME
     tc4_wx - Quick PNG Images for TC4 WxMaps
          
SYNOPSIS

     tc4_wx.pl [OPTIONS] inst3d_met_p_filename
          
DESCRIPTION
     This script uses GrADS to produce customized plots for the
     TC4 WxMap website. The resulting images are also scp
     to the /portal. You can pass any of the output
     HDF files as input, this script will pick and choose the
     ones it wants to work on.     

OPTIONS
     -d dir   destination directory for PNG files; default is "."
     -h       print this page       
     -g type  process only this image group; valid options are
                 uvt   ---  winds and temperature
                 uvrh  ---  winds and relative himidity
                 uvpr  ---  winds and precipitation
                 uvslp ---  winds and sea level pressure
              By default all file types are processed.
     -T       maximum tau to plot; default is 72
     -v       verbose mode
     -w dir   directory for temporary files; default is "."

EXAMPLES
     % source /share/dasilva/opengrads/setup.csh
     % tc4_wx.pl -v -g uvt -d dirac:/portal/tc4/gds/Contents/tc4_wx
               d5_tc4_01.inst3d_met_p.20070701_00z+20070701_0300z.hdf

AUTHOR
     Arlindo da Silva, NASA/GSFC.

EOF

  exit(1)

 }
