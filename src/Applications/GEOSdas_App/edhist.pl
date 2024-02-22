#!/usr/bin/env perl
#=======================================================================
# name: edhist.pl
#
# purpose - This script allows easy editing of the COLLECTIONS list
#           at top of a HISTORY.rc.tmpl template file.
#
# key global variables
# => @headings: all lines which come before COLLECTIONS list
# => @topList: array containing order of COLLECTIONS list
# => @bottomList: array containing order of collection definitions
# => %cdef: hash containing collection definitions
# => %clutter: hash containing the unexpected stuff, such as a comment character
#              or label, found in front of expected components in an input line
#              from the HISTORY.rc.tmpl file.
# => %comments: hash containing comment lines for
#               - collections in COLLECTIONS list
#               - collection traits
#               - collection field variables
#               - end of collection
# => %traitHash: hash containing order of traits for each collection
#
# Note: See usage (edhist.pl -h) for usage information.
#
# !Revision History
#
# 08Mar2008   Stassi  Initial version of code.
# 21Mar2008   Stassi  Consolidated user options
# 29Sep2009   Stassi  Added -str1, str2, -i, -v flags, and auto-edit mode
#                     Added -Xall, -Iall, -X, -I, and -pm flags
# 12Jul2010   Stassi  Added -list option and sub print_list_to_STDOUT()
# 11Jun2011   Stassi  Added -Xdash to remove "-"/"+-" collection name endings
# 08Jul2011   Stassi  mods to check for input errors; better output control
# 16Aug2012   Stassi  Added -plot option for creating monthly plots HIST file
# 14Jan2014   Stassi  Added -sum option
# 27Mar2014   Stassi
#=======================================================================
use strict;
use warnings;
use FindBin qw($Bin);
use lib ("$Bin");

# global variables
#-----------------
my (@headings, @topList, @bottomList);
my (%cdef, %clutter, %comments, %traitHash, %warnOnce);
my (%substVal, %cleanD, %cleanP);

my ($INfile, $OUTfile, $inplace, $prompt, $debug, $printList, $mnthlyRC, $sum);
my ($includeall, $excludeall, @include, @exclude, @incPM, @excPM, @excDEP);
my ($plotHIST, $plotHISTdir, $incDASH, $excDASH, $incPLUS, $excPLUS);
my ($rmDASH, $rmPLUS, $quietCM, $quietCL, $orderFLG, $margin);
my ($addFLG, $arcFLG, $appFLG, $fcstFLG, $silo_arc, $mstorage_arc);

# main program
#-------------
{
    init();
    parse_input();
    apply_runtime_choices();

    if ($prompt)   { interactive_edit() }
    if ($plotHIST) { plot_edit()        }

    if ($printList)    { print_list_to_STDOUT(); exit }
    if (defined($sum)) { sum_slices();           exit }

    if ($addFLG) { add_silo_mstorage_traits()      }
    if ($arcFLG) { write_silo_mstorage_arc(); exit }

    write_HISTfile();
}

#=======================================================================
# name - init
# purpose - get the runtime parameters; determine name of input and
#           output template files.
#=======================================================================
sub init {
    use Cwd ("abs_path");
    use File::Basename ("dirname");
    use Getopt::Long;
    my (@list, @substitutions, @str1, @str2, $quiet, $help);
    my ($arrAddr, @new, $name, $pp, $ans, $dflt);
    my ($string, @assign, $label, $value, $OUTdflt);

    # $margin used for formatting in sum_slices()
    #--------------------------------------------
    $margin = 24;

    # get runtime flags
    #------------------
    Getopt::Long::Configure "no_ignore_case";
    GetOptions ( "o=s"        => \$OUTfile,
                 "i"          => \$inplace,
                 "Iall"       => \$includeall,
                 "Xall"       => \$excludeall,
                 "I=s@"       => \@include,
                 "X=s@"       => \@exclude,
                 "Ipm=s@"     => \@incPM,
                 "Xpm=s@"     => \@excPM,
                 "Xdep=s@"    => \@excDEP,
                 "ID"         => \$incDASH,
                 "XD"         => \$excDASH,
                 "IP"         => \$incPLUS,
                 "XP"         => \$excPLUS,
                 "rD"         => \$rmDASH,
                 "rP"         => \$rmPLUS,
                 "s=s"        => \%substVal,
                 "str1|s1=s@" => \@str1,
                 "str2|s2=s@" => \@str2,
                 "p"          => \$prompt,
                 "q:3"        => \$quiet,
                 "order"      => \$orderFLG,
                 "db|debug:s" => \$debug,
                 "h|help"     => \$help,

                 # monthly plots history
                 #----------------------
                 "plot:s"     => \$mnthlyRC,

                 # list collections
                 #-----------------
                 "list:s"     => \$printList,   # optional value by design
                                                # to catch if no value given
                 # sum slices
                 #-----------
                 "sum:1"      => \$sum,

                 # arc file options
                 #-----------------
                 "add"        => \$addFLG,
                 "arc"        => \$arcFLG,
                 "append"     => \$appFLG,
                 "fcst"       => \$fcstFLG,
                 "silo=s"     => \$silo_arc,
                 "mstorage=s" => \$mstorage_arc);

    usage() if $help;

    $arcFLG = 1 if $appFLG;
    if ($arcFLG) { $mnthlyRC = undef; $addFLG = 1}

    if ($debug) {
        foreach (@str1)          { print "str1 = \#$_#\n" }
        foreach (@str2)          { print "str2 = \#$_#\n" }
        foreach (keys %substVal) { print "substVal{$_} = \#$substVal{$_}#\n" }
    }

    # quiet options
    #--------------
    if ($quiet) { $quiet = 3 unless $quiet == 1 or $quiet == 2 }
    else        { $quiet = 0 }

    $quietCL = 1 if $quiet == 1 or $quiet == 3;
    $quietCM = 1 if $quiet == 2 or $quiet == 3;

    # check input HISTORY template filename
    #--------------------------------------
    $INfile = shift @ARGV if @ARGV;
    $INfile = "HISTORY.rc.tmpl" unless $INfile;
    if ($debug) { print "INfile = #$INfile#\n" }

    while (! -e $INfile) {
        if ($prompt) {
            print "\nCannot find HISTORY template, '$INfile'.";
            print "\nEnter input HISTORY filename: ";
            chomp($ans = <STDIN>); $ans and $INfile = $ans;
        }
        die "ERROR: Cannot find HISTORY template, '$INfile'. " unless -e $INfile;
    }

    # check for plot HISTORY option
    #------------------------------
    if (defined($mnthlyRC)) {
        $mnthlyRC = "monthly.rc" unless $mnthlyRC;
        $mnthlyRC .= "/monthly.rc" if -d $mnthlyRC;
        die "ERROR. Cannot find $mnthlyRC;" unless -f $mnthlyRC;
        $plotHIST = 1;
        $orderFLG = 1;
    }

    # check $arcFLG options
    #----------------------
    if ($arcFLG) {
        $silo_arc = "silo.hist.arc" unless $silo_arc;
        $mstorage_arc = "mstorage.hist.arc" unless $mstorage_arc;
    }

    # check input parameters
    #-----------------------
    if (defined($printList)) {
        die "ERROR. No -list option given;" unless $printList;
        die "ERROR. Unrecognizable -list option: $printList;" unless
            $printList eq "inc" or $printList eq "inc:" or $printList eq "inc," or
            $printList eq "all" or $printList eq "all:" or $printList eq "all," or
            $printList eq "exc" or $printList eq "exc:" or $printList eq "exc,";
    }

    # separate exclusions and inclusions joined by ":" or ","
    #--------------------------------------------------------
    @include = () unless @include;
    @exclude = () unless @exclude;
    @excDEP = () unless @excDEP;
    @incPM = () unless @incPM;
    @excPM = () unless @excPM;

    foreach $arrAddr (\@include, \@exclude, \@incPM, \@excPM, \@excDEP) {
        @new = ();
        foreach (@$arrAddr) {
            @list = split /[:|,]/;
            foreach $name (@list) { push @new, rm_dash_plus($name) if $name }
        }
        @$arrAddr = @new;
    }

    # consistency checks
    #-------------------
    if ($includeall and $excludeall) {
        die "ERROR: Cannot choose both -Iall and -Xall;";
    }
    $rmDASH = 0 unless $rmDASH;
    $rmPLUS = 0 unless $rmPLUS;

    # Note: cannot have both -rD and -rP since this can cause definition conflict
    #----------------------------------------------------------------------------
    if ($rmDASH and $rmPLUS)   { die "ERROR: Cannot choose both -rD and -rP;" }
    if ($incDASH and $excDASH) { die "ERROR: Cannot choose both -ID and -XD;" }
    if ($incPLUS and $excPLUS) { die "ERROR: Cannot choose both -IP and -XP;" }

    foreach $name (@include) {
        if (found($name, \@exclude)) {
            die "ERROR: $name marked for both exclusion and inclusion";
        }
    }
    $excPLUS = 1 if $rmDASH;
    $excDASH = 1 if $rmPLUS;

    # change "+" -> "\+" in @incPM, and @excPM
    #-----------------------------------------
    foreach $arrAddr (\@incPM, \@excPM) {
        foreach (1..scalar(@$arrAddr)) {
            $pp = shift @$arrAddr;
            $pp =~ s/\s+//g;        # remove blanks spaces
            $pp =~ s/\+/\\\+/g;     # pre-append '\' to '+'
            while ($pp =~ /\\\\\+/) { $pp =~ s/\\\\\+/\\\+/ }  # change '\\' to '\'
            push @$arrAddr, $pp;
        }
    }

    # Check output HISTORY template filename
    #---------------------------------------
    if ($plotHIST) { $OUTdflt = "HISTORY.rc_tmpl" }
    else           { $OUTdflt = $INfile           }

    unless ($OUTfile) { $OUTfile = $OUTdflt if $inplace }
    unless ($OUTfile) {
        if ($prompt) {
            $dflt = "${OUTdflt}.out";
            print "Enter output HISTORY filename? [$dflt] ";
            chomp($OUTfile = <STDIN>);
            $OUTfile = $dflt unless $OUTfile;
        }
    }
    if ($OUTfile) {
        $OUTfile = "$OUTfile/$OUTdflt" if -d $OUTfile;
        $plotHISTdir = dirname(abs_path($OUTfile)) if $plotHIST;
    }

    # string substitutions
    #---------------------
    if ($#str1 != $#str2) {
        die "ERROR: Mismatched number of string substitutions";
    }
    for (0..$#str1) { $substVal{$str1[$_]} = $str2[$_] }
}

#=======================================================================
# name - parse_input
# purpose - parse collection information from input HISTORY file.
#
# note -
#   This routine checks for the following error conditions
#   - collections defined multiple times
#   - collections listed but not defined
#   - collections defined but not listed
#   - collections with name inconsistencies within its definition
#=======================================================================
sub parse_input {
    my ($look4head, $look4list, $look4def, $look4fields);
    my ($name, $name1, $cname, $trait, $traitN, $value);
    my ($cline, @xtra, $xtra, @var, @src, $src, $dep);
    my ($label, @parts, $ending);
    my ($set, @keys, $foundFLG, $errmsg);
    my (%nametrait, $nnn, $mmm, $value_mmm);

    # initialize control variables
    #-----------------------------
    $look4head = 1;
    $look4list = 0;
    $look4def = 0;
    $look4fields = 0;
    $cline = "";

    # read input file
    #----------------
    open INFILE, "< $INfile" or die "ERROR opening $INfile: $!";
    while (<INFILE>) {

        # apply substitutions
        #--------------------
        foreach $label (keys %substVal) { s/$label/$substVal{$label}/g }

        if ( /COLLECTIONS:/ ) {
            $look4head = 0;
            $look4list = 1;
        }

        # (2) get list of collections from top of file
        #---------------------------------------------
        if ($look4list) {
            if ( /([\'|\"])(\S+)\1/ ) {
                $name = $2;

                if ( found($name, \@topList) ) {
                    warn "WARNING: $name is listed more than once"
                        ." in COLLECTIONS list.\n";
                }
                else {
                    push @topList, $name;
                    $comments{$name} = $cline;
                    $cline = "";

                    ($name1 = $name) =~ s/\+/\\\+/g;
                    $xtra = clean($1) if /(.*)[\'|\"]$name1/;
                    $xtra = "" unless $xtra;
                    $xtra = "" if $xtra =~ /COLLECTIONS/;
                    $clutter{$name} = $xtra;
                }
            }
            elsif (commentLine($_)) { $cline .= $_; next }

            if ( /\s::/ ) {
                $look4list = 0;
                $look4def = 1;
                $name = "";
                $cline = "";
            }
            next;
        }

        # (1) get headings from top of file
        #----------------------------------
        if ($look4head) { push @headings, $_; next }

        # (4) get collection fields
        #--------------------------
        if ($look4fields and (($_ !~ m/\b(\S+)\.(\S+)\s*:(.*)$/)| ($_ !~ m/\b(\S+)\.(\S+)(\S+)\.(\S+)\s*:(.*)$/)) ) {
            if ( (/[\'|\"]/) and (/\,/) ) {
                $value = clean($_);
                store($cname, $name, $traitN, $value);

                strip_fields($cname, $value, \@xtra, \@var, \@src);
                $comments{"$cname.$var[0].$xtra[0]"} = $cline;
                $cline = "";

                # comment collections which include excluded dependency
                #------------------------------------------------------
                foreach $dep (@excDEP) {
                    last if $clutter{$cname};

                    foreach $src (@src) {
                        $clutter{$cname} = "#" if $src =~ /([\"|\'])$dep\1/i;
                    }
                }
            }
            elsif (commentLine($_)) {

                #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                # DO NOT COMMENT DOUBLE-COLON ENDING IN COLLECTION DESCRIPTION
                #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                #-------------------------------------------------------------
                # - there is no need to do it
                # - multiple double-colons, commented or otherwise, make it
                #   difficult for parcer to determine end of description
                #-------------------------------------------------------------
                if ( /\s::/ ) {
                    $errmsg = "\nERROR. '::' is commented "
                        .       "in collection description: $name;";
                    die $errmsg;
                }
                $cline .= $_;
                next;
            }

            if ( /\s::/ ) {
                $look4fields = 0;
                $look4def = 1;
                $comments{"$cname.end"} = $cline;
                $cline = "";
            }
            next;
        }

        # (3) look for collection traits
        #-------------------------------
        if ( /\b(\S+)\.(\S+)\s*:(.*)$/ ) {
            $name  = clean($1);
            $trait = clean($2);
            $value = clean($3);
            $value =~ s/\s*,$// unless $trait eq "fields";

            ($name1 = $name) =~ s/\+/\\\+/g;
            $xtra = clean($1) if /(.*)$name1/;
            $xtra = "" unless $xtra;

            # check for name consistency
            #---------------------------
            unless ($look4def) {
                if ($name ne $cname) {
                    warn "WARNING: Name inconsistency in $cname collection: "
                        . "changing $name.$trait -> $cname.$trait\n";
                    $clutter{"$cname.$traitN"} = $clutter{"$name.$traitN"};
                    $name = $cname;
                }
            }

            # look for duplicate traits
            #--------------------------
            $nnn = 0;
            while ($nametrait{"$name.$trait.N" .++$nnn}) { next };
            $traitN = "$trait.N$nnn";

            $nametrait{"$name.$traitN"} = 1;
            foreach $mmm (1..($nnn-1)) {
                next if $mmm == $nnn;
                unless ($xtra or $clutter{"$name.$trait.N$mmm"}) {
                    $value_mmm = $cdef{"$name.$trait.N$mmm"};
                    warn "\nWARNING: multiple values found:\n"
                        .  "WARNING   * $name.$trait = " .$value_mmm ."\n"
                        .  "WARNING   * $name.$trait = " .$value ."\n"
                        .  "WARNING  commenting all but last value\n\n";
                    $clutter{"$name.$trait.N$mmm"} = "#!!!!!!!";
                }
            }

            # check for comments and clutter
            #-------------------------------
            $clutter{"$name.$traitN"} = $xtra;
            if (commentLine($_)) {
                warn "WARNING: $name.$trait is commented.\n" unless $quietCM;
            }
            elsif ($clutter{"$name.$traitN"}) {
                warn "WARNING: $name.$trait is cluttered.\n" unless $quietCL;
            }

            # store first collection trait info
            #----------------------------------
            if ($look4def) {
                store("", $name, $traitN, $value);
                $cname = $name;
                $look4def = 0;
            }

            # store subsequent collection trait info
            #---------------------------------------
            else {
                store($cname, $name, $traitN, $value);
            }

            if ($trait eq "fields") {
                $cdef{"${cname}_numvars"}++ if $value !~ m/^#/;
                $look4fields = 1;
            }
            $comments{"$cname.$traitN"} = $cline;
            $cline = "";

            next;
        }

        # or a comment line?
        #-------------------
        $cline .= $_ if commentLine($_);
    }
    close INFILE;

    # if removing dash/plus endings ("-", "+-"), then store
    # modified collection name endings in %cleanD and %cleanP
    #-------------------------------------------------------------------
    # %cleanD
    # - keys: last section of collection names with "-" ending
    # - values last section of collection names without endings
    #
    # %cleanP
    # - keys: last section of collection names with "+-" ending
    # - values last section of collection names without endings
    #-------------------------------------------------------------------
    %cleanD = ();
    %cleanP = ();
    foreach (@topList) {
        @parts = split /[\.|\_]/;
        $ending = $parts[-1];

        if ($ending =~ /(\b.*\b)-\s*$/) {
            $value = $1;
            $cleanD{$ending} = $value;
        }
        if ($ending =~ /(\b.*\b)\+-\s*$/) {
            $value = $1;
            $ending =~ s/\+/\\\+/;
            $cleanP{$ending} = $value;
        }
    }

    # check that each name in COLLECTIONS is defined
    #-----------------------------------------------
    foreach $name (@topList) {
        $name1 = rm_dash_plus($name);
        unless (found($name1, \@bottomList)) {
            die "ERROR: '$name1' is listed in COLLECTIONS"
                ." but not defined in $INfile;";
        }
    }

    # warn if user include/exclude requests not found
    #------------------------------------------------
    for (0..$#include) {
        $set = shift @include;
        $foundFLG = found($set, \@topList);
        if ($foundFLG) { push @include, $set; }
        else           { warn "WARNING: -I '$set' not found in $INfile\n";
                         pause() }
    }
    for (0..$#incPM) {
        $set = shift @incPM;
        $foundFLG = found($set, \@topList, 1);
        if ($foundFLG) { push @incPM, $set; }
        else           { warn "WARNING: -Ipm '$set' not found in $INfile\n";
                         pause() }
    }
    for (0..$#exclude) {
        $set = shift @exclude;
        $foundFLG = found($set, \@topList);
        if ($foundFLG) { push @exclude, $set; }
        else           { warn "WARNING: -X '$set' not found in $INfile\n";
                         pause() }
    }
    for (0..$#excPM) {
        $set = shift @excPM;
        $foundFLG = found($set, \@topList, 1);
        if ($foundFLG) { push @excPM, $set; }
        else           { warn "WARNING: -Xpm '$set' not found in $INfile\n";
                         pause() }
    }
}

#=======================================================================
# name - store
# purpose - store information from collection definitions
#=======================================================================
sub store {
    my ($cname, $name, $trait, $traitN, $value, $cline);

    $cname  = shift @_;
    $name   = shift @_;
    $traitN = shift @_;
    $value  = shift @_;
    $cline  = shift @_;
    $cline  = "" unless $cline;

    ($trait = $traitN) =~ s/\.N\d+$//;

    # first entry
    #------------
    if ($cname) {
        die "\nERROR: Name inconsistency in $cname collection:\n"
            . "         collection: $cname\n"
            . "         trait:      $name.$trait\n"
            . "" if $cname ne $name;
    }
    else {

        # is it defined more than once?
        #------------------------------
        die "\nERROR: '$name' is defined multiple times in $INfile;"
            if found($name, \@bottomList);
        push @bottomList, $name;
        $cname = $name;

        # is it listed in COLLECTIONS?
        #-----------------------------
        unless (found($name, \@topList)) {
            warn "WARNING: $name is defined but not in COLLECTIONS list."
                ." Adding commented name to list.\n";
            push @topList, $name;
            $clutter{$name} = "#";
        }
    }

    # store traits in order
    #----------------------
    if ($traitHash{$name}) { $traitHash{$name} .= ":$traitN" }
    else                   { $traitHash{$name}  =  "$traitN" }

    # store values
    #-------------
    $value = "" unless defined($value);
    storeVals($name, $traitN, $value, $cline);
}

#=======================================================================
# name - storeVals
# purpose - store collection definition in %cdef
#=======================================================================
sub storeVals {
    my ($name, $trait, $traitN, $value);

    $name   = shift @_;
    $traitN = shift @_;
    $value  = shift @_;

    ($trait = $traitN) =~ s/\.N\d+$//;

    # store new values
    #-----------------
    unless (defined($cdef{"$name.$traitN"})) {
        $cdef{"$name.$traitN"} = $value;
        return;
    }

    # append new "fields" value
    #--------------------------
    if ($trait eq "fields") {
        $cdef{"$name.$traitN"} .= "\n$value";
        $cdef{"${name}_numvars"}++ if $value !~ /^#/;
        chomp($cdef{"$name.$traitN"});
        return;
    }
}

#=======================================================================
# name - print_list_to_STDOUT
# purpose - print list of collections to standard output
#
# global variable
# => $printList: if eq "all", then print all collections
#                if eq "inc", then print only the included collections
#                if eq "exc", then print only the excluded collections
#=======================================================================
sub print_list_to_STDOUT {
    my (@plist, $plist, $name, $name1);

    @plist = ();
    $plist = "";

    foreach $name (@topList) {
        $name1 = rm_dash_plus($name);

        if ($printList =~ "all") { push @plist, $name1 }
        if ($printList =~ "inc") { push @plist, $name1 unless $clutter{$name} }
        if ($printList =~ "exc") { push @plist, $name1     if $clutter{$name} }
    }
    if ($printList =~ ":") {
        $plist = join ":", @plist;
        print "$plist\n";
    }
    elsif ($printList =~ ",") {
        $plist = join ",", @plist;
        print "$plist\n";
    }
    else {
        if (@plist) { foreach (@plist) { print "$_\n" } }
    }
}

#=======================================================================
# name - write_silo_mstorage_arc
# purpose - Write arc files, $silo_arc and $mstorage_arc, based on info
#           in the HISTORY.rc file
#=======================================================================
sub write_silo_mstorage_arc {
    my ($label, $name, $name1, $silo, $template, $silo_path, $msFLG, $comment);
    my ($dt, $dt1, $ext);

    if ($appFLG) {
        open SILO, ">> $silo_arc"
            or die "ERROR, opening silo.arc for append: $silo_arc;";

        open MSTORE, ">> $mstorage_arc"
            or die "ERROR, opening mstorage.arc for append: $mstorage_arc;";
    }
    else {
        open SILO, "> $silo_arc"
            or die "ERROR, opening silo.arc: $silo_arc;";

        open MSTORE, "> $mstorage_arc"
            or die "ERROR, opening mstorage.arc: $mstorage_arc;";
    }

    $label = "#\n"
        .    "#   -------------------\n"
        .    "#   HISTORY COLLECTIONS\n"
        .    "#   -------------------\n"
        .    "#   $INfile\n"
        .    "#\n";

    print SILO $label;
    print MSTORE $label;

    foreach $name (@topList) {
        $silo = $cdef{"$name.silo.N1"};
        $template = $cdef{"$name.template.N1"};
        if ($fcstFLG and $template =~ m/^(.+)\.(.+)$/) {
            $dt  = $1;
            $ext = $2;

            if ($dt =~ /h2%n2z/) { ($dt1 = $dt) =~ s/%n2// }
            else                 { $dt1 = $dt }

            $template = "$dt1+$dt.$ext" 
        }
        $name1 = rm_dash_plus($name);

        $silo_path = "\${PESTOROOT}%s/$silo/%s.$name1.$template";
        $silo_path =~ s/\'//g;
        $silo_path =~ s/>>>NCSUFFIX<<</nc4/;
        $msFLG = $cdef{"$name.mstorage.N1"};

        $comment = undef;
        $comment = $comments{$name} if $comments{$name};
        $comment = "#\n$comment#\n" if $comment;

        $silo_path = "#$silo_path" if found($name, \@exclude);

        print SILO $comment if $comment;
        print SILO "$silo_path\n";

        print MSTORE $comment if $comment;
        print MSTORE "#--" unless lc($msFLG) eq "'yes'";
        print MSTORE "$silo_path\n";
    }
    close SILO;
    close MSTORE;
}

#=======================================================================
# name - sum_slices
#=======================================================================
sub sum_slices {
    my ($name, @levels, @arr2D, @arr3Dv, @arr3De, @arr3Dp, @arrPL, %pressLevs);
    my ($max, $len, $len1, $len2, $fmt, $totSlices, $numLevs, $label);
    my (%frequency, $freq, $cfreq, @arrFRQ);

    $max = -1;

    # get frequency info and number of levels for each collection
    #------------------------------------------------------------
    foreach $name (@topList) {
        next if $clutter{$name} =~ m/^\s*#/;

        $len = length($name);
        $max = $len if $len > $max;

        $freq = $cdef{"$name.frequency.N1"};
        die "Error. Unable to determine frequency of $name;" unless $freq;
        $frequency{$freq} = 1;

        if ($name =~ m/_2d_/ or $name =~ m/\.sfc/) {
            $cdef{"${name}_numlevs"} = 1;
            push @arr2D, $name;
        }
        elsif ($name =~ m/_Nv/ or $name =~ m/\.eta/) {
            $cdef{"${name}_numlevs"} = 72;
            push @arr3Dv, $name;
        }
        elsif ($name =~ m/_Ne/) {
            $cdef{"${name}_numlevs"} = 73;
            push @arr3De, $name;
        }
        elsif ($name =~ m/_Np/ or $name =~ m/\.prs/) {
            @levels = split /\s+/, $cdef{"$name.levels.N1"};
            $cdef{"${name}_numlevs"} = scalar @levels;
            push @arr3Dp, $name;
            $pressLevs{scalar(@levels)} = 1;
        }
        else {
            die "Error. Unable to determine number of layers for $name;"
        }
    }

    unless (@arr2D or @arr3Dv or @arr3De or @arr3Dp) {
        die "Error. No collections found in $INfile;";
    }

    if ($sum) {
        $len1 = $margin + 4;
        $len2 = $max + 2;
        $fmt = "%${len1}s %${len2}s %8s %8s %9s\n";

        printf "\n$fmt", "Name", "#levs", "freq", "#vars", "#slices";
        printf $fmt, "----", "-----", "----", "-----", "-------";
    }

    $totSlices = 0;
    if ($sum == 0 or $sum == 1) {
        sum_totSlices("2D Collections",  $max, \@arr2D,  \$totSlices);
        sum_totSlices("3Dv Collections", $max, \@arr3Dv, \$totSlices);
        sum_totSlices("3De Collections", $max, \@arr3De, \$totSlices);
        foreach $numLevs (sort keys %pressLevs) {
            @arrPL = ();
            foreach $name (@arr3Dp) {
                push @arrPL, $name if $cdef{"${name}_numlevs"} == $numLevs;
            }
            $label = "$numLevs-Pressure Levels";
            sum_totSlices($label, $max, \@arrPL, \$totSlices);
        }
    }
    elsif ($sum == 2) {
        foreach $freq (sort keys %frequency) {
            @arrFRQ = ();
            foreach $name (@arr2D, @arr3Dv, @arr3De, @arr3Dp) {
                $cfreq = $cdef{"$name.frequency.N1"};
                push @arrFRQ, $name if $cfreq eq $freq;
            }
            sum_totSlices("freq=$freq", $max, \@arrFRQ, \$totSlices);
        }
    }
    if ($sum) {
        $len = $margin + $max + 34;
        printf "\n%${len}s\n\n", "Total Number of Slices per Day = $totSlices";
    }
    else {
        print $totSlices ."\n";
    }
}

#=======================================================================
# name - sum_totSlices
# purpose -
#=======================================================================
sub sum_totSlices {
    my ($label, $max, $names, $totSlices);
    my ($name, $freq, $perday, $numVars, $numLevs, $numSlices, $subtotal);
    my ($first, $len);

    $label     = shift @_;
    $max       = shift @_;
    $names     = shift @_;  # this is really the array address
    $totSlices = shift @_;  # this is really the variable address

    return unless @$names;

    $first = 1;
    $subtotal = 0;

    printf "\n%${margin}s", "$label: " if $sum;
    foreach $name (@$names) {
        $freq = $cdef{"${name}.frequency.N1"};
        $perday = 24 / freqhours($freq);
        $numVars = $cdef{"${name}_numvars"};
        $numLevs = $cdef{"${name}_numlevs"};
        $numSlices = $numVars * $numLevs * $perday;
        $subtotal += $numSlices;
        $$totSlices += $numSlices;

        if ($first)  { $first = 0        }
        elsif ($sum) { print " "x$margin }
        printf "%-${max}s = %3d   x  %3d   x  %3d = %7d\n",
        $name, $numLevs, $perday, $numVars, $numSlices if $sum;
    }
    $len = $margin + $max + 24;
    printf "%${len}s   %7d\n", "subtotal", $subtotal if $sum;
}

#=======================================================================
# name - apply_runtime_choices
# purpose - apply inclusions and exclusions specified by the runtime
#           flags: -Xall, -Iall, -I, -X, and -pm
#=======================================================================
sub apply_runtime_choices {
    my ($name, $name1, $inc, $exc, $ending);

    # first, apply all include or exclude
    #------------------------------------
    if ($excludeall) { foreach (@topList) { $clutter{$_} = "#"; } }
    if ($includeall) { foreach (@topList) { $clutter{$_} = "";  } }

    # next, apply pattern matching include and exclude
    #-------------------------------------------------
    foreach $name (@topList) {
        $name1 = rm_dash_plus($name);
        foreach (@excPM) { $clutter{$name} = "#" if $name1 =~ /$_/ }
    }
    foreach $name (@topList) {
        $name1 = rm_dash_plus($name);
        foreach (@incPM) { $clutter{$name} = "" if $name1 =~ /$_/ }
    }

    # next, apply dash/plus include and exclude
    #------------------------------------------
    if ($excDASH) {
        foreach $ending (keys %cleanD) {
            foreach (@topList) { $clutter{$_} = "#" if /$ending$/ }
        }
    }
    if ($excPLUS) {
        foreach $ending (keys %cleanP) {
            foreach (@topList) { $clutter{$_} = "#" if /$ending$/ }
        }
    }
    if ($incDASH) {
        foreach $ending (keys %cleanD) {
            foreach (@topList) { $clutter{$_} = "" if /$ending$/ }
        }
    }
    if ($incPLUS) {
        foreach $ending (keys %cleanP) {
            foreach (@topList) { $clutter{$_} = "" if /$ending$/ }
        }
    }

    # lastly, apply specific includes and excludes
    #---------------------------------------------
    foreach $name (@topList) {
        $name1 = rm_dash_plus($name);
        foreach $exc (@exclude) {
            $clutter{$name} = "#" if $exc eq $name;
            $clutter{$name} = "#" if $exc eq $name1;
        }
        foreach $inc (@include) {
            $clutter{$name} = "" if $inc eq $name;
            $clutter{$name} = "" if $inc eq $name1;
        }
    }
}

#=======================================================================
# name - interactive_edit
# purpose - allow user to modify the list of collections before writing
#           the output HISTORY template.
#=======================================================================
sub interactive_edit {
    my ($selection, $cnt, $max, $name, $name1, $last);
    my ($ans, %list, @incl, @excl, $nopause);

    $last = 5;

    while (1) {
        print "\nEdit HISTORY template COLLECTIONS\n"
            .   "---------------------------------\n"
            .   "0. Quit without change\n"
            . "\n1. Show defined collections\n"
            .   "2. Show/edit included collections\n"
            .   "3. Show/edit excluded collections\n"
            . "\n4. Show HISTORY template names\n"
            .   "5. Write edited HISTORY template\n";
        print "\nSelection (0-$last) [0]: ";
        chomp($selection = lc <STDIN>);

        unless ($selection) {
            print "Are you sure you want to quit without writing output (y/n) [y]? ";
            chomp($ans = lc <STDIN>);
            unless ($ans eq "n") {
                print "\nExiting. No output written.\n\n";
                exit;
            }
            next;
        }
        $nopause = 0;

        #=============================
        # reject non-numeric responses
        #=============================
        unless ($selection =~ /^\s*\d+\s*$/) {
            print "\nUnrecognizable selection: $selection\n"
                .   "Try again.\n\n";
            pause();
            next;
        }

        #==============================
        # reject out-of-range responses
        #==============================
        if ($selection < 0 or $selection > $last) {
            print "\nInvalid selection: $selection\n"
                .   "Try again.\n\n";
            pause();
            next;
        }

        if ($selection == 1) {
            #~~~~~~~~~~~~~~~~~~~~~~~~~
            # Show defined collections
            #~~~~~~~~~~~~~~~~~~~~~~~~~
            print "\nDefined Collections\n"
                .   "-------------------\n";
            $cnt = 0;
            while (1) {
                $cnt = 0;
                foreach $name (@topList) {
                    $name1 = rm_dash_plus($name);
                    printf "%2d) %s\n", ++$cnt, $name1;
                }
                print "\n";
                next if toggle_dash_plus();
                $nopause = 1;
                last;
            }

        } elsif ($selection == 2) {
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Show/edit included collections
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            unless (cnt_included()) {
                print "All collections are excluded.\n";
            } else {
                $cnt = 0;
                print "\nIncluded Collections\n"
                    .   "--------------------\n";
                foreach $name (@topList) {
                    $name1 = rm_dash_plus($name);
                    unless ($clutter{$name}) {
                        printf "%2i. %s", ++$cnt, $name1;
                        print "\n";
                        $list{$cnt} = $name;
                    }
                }
                printf "%2i. <exclude all>\n", ++$cnt;
                print   "--------------------\n"
                    .   "Exclude which collection(s)? ";
                $max = $cnt;
                @excl = userResponse($max);
                $nopause = 1 unless @excl;

                # check for "exclude all" option
                #-------------------------------
                if (found($max, \@excl)) {
                    foreach (@topList ) {
                        $clutter{$_} = "#";
                    }
                    print "All collections have been excluded.\n";

                } else {

                    # exclude selected collections
                    #-----------------------------
                    foreach (@excl) { $clutter{$list{$_}} = "#" };
                }
            }

        } elsif ($selection == 3) {
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Show/edit excluded collections
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            unless (cnt_excluded()) {
                print "All collections are included.\n";
            } else {
                $cnt = 0;
                print "\nExcluded Collections\n"
                    .   "--------------------\n";
                foreach $name (@topList) {
                    $name1 = rm_dash_plus($name);
                    if ($clutter{$name}) {
                        printf "%2i. %s", ++$cnt, $name1;
                        print "\n";
                        $list{$cnt} = $name;
                    }
                }
                printf "%2i. <include all>\n", ++$cnt;
                print   "--------------------\n"
                    .   "Include which collection(s)? ";
                $max = $cnt;
                @incl = userResponse($max);
                $nopause = 1 unless @incl;

                # check for "include all" option
                #-------------------------------
                if (found($max, \@incl)) {
                    foreach (@topList) {
                        $clutter{$_} = "";
                    }
                    print "All collections have been included.\n";

                } else {

                    # add selected collections
                    #-------------------------
                    foreach (@incl) { $clutter{$list{$_}} = "" };
                }
            }

        } elsif ($selection == 4) {
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Show HISTORY template names
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            print "\n Input: $INfile\n"
                .   "Output: $OUTfile\n\n";

        } elsif ($selection == 5) {
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Write edited HISTORY template
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            unless (cnt_included()) {
                print "\nWARNING: no collections are currently included."
                    . "\nAre you sure you want to write HISTORY template (y/n) [n]? ";
                chomp($ans = lc <STDIN>);
                next unless ($ans eq "y");
            }
            last;
        }
        pause() unless $nopause;
    }
}

#=======================================================================
# name - plot_edit
# purpose - edit the HISTORY information for outputting the
#           the HISTORY.rc_tmpl for the monthly_plots
#=======================================================================
sub plot_edit {
    use File::Basename qw(basename);
    my ($template, $processflags, $htype, $name, $name1, @new, %Pflg);

    # include only Cp, Np, and Nx collections
    #----------------------------------------
    @new = ();
    foreach $name (@topList) {
        $name =~ s/_NCKS$//;
        next unless $name =~ m/Cp$/ or $name =~ m/Np$/ or $name =~ m/Nx$/
            or $name =~ m/slv$/ or $name =~ m/p42$/ or $name =~ m/v72$/ or $name =~ m/v73$/;
        push @new, $name;
    }
    @topList = @new;

    # get names from monthly rcfile
    #------------------------------
    open MRC, "< $mnthlyRC" or die "ERROR opening $mnthlyRC";
    foreach (<MRC>) {
        next if commentLine($_);
        ($template, $processflags, $htype) = split(/\s+/, $_, 3);
        $processflags = "" unless $processflags;
        $htype = "" unless $htype;

        $name = (split /[.]/, basename($template))[1];
        next unless found($name, \@topList);
        next unless $processflags =~ m/P/;
        $Pflg{$name} = 1;

        # comment in collections list if commented here
        #----------------------------------------------
        if (/^\s*\#/) {
            $name1 = found($name, \@topList);
            comment(\$clutter{$name1}) if $name1;
        }

        # write datarc template file
        #---------------------------
        write_datarc($template, $name) if $plotHISTdir;
    }
    close MRC;

    # only include collections which are to be plotted
    #-------------------------------------------------
    @new = ();
    foreach (@topList) { next unless $Pflg{$_}; push @new, $_ }
    @topList = @new;

    # adjust traits as necessary
    #---------------------------
    foreach $name (@topList) {
        delete_trait($name, "end_date");
        delete_trait($name, "end_time");
        add_grads_ddf($name);
    }
}

#=======================================================================
# name - write_datarc
# purpose - write data rc template file
#=======================================================================
sub write_datarc {
    my ($template, $name);
    my ($dirloc, $datarc, $FH);

    #~~~~~~~~~~~~~~~~~~~~~~~~~#
    return unless $plotHISTdir;
    #~~~~~~~~~~~~~~~~~~~~~~~~~#

    ($template, $name) = @_;
    $dirloc = undef;

    # write rc file for const data
    #-----------------------------
    if ($name =~ m/^const/) {
        $dirloc = "/$1" if $template =~ m|#*(\w+/)|;
        unless ($dirloc) {
            print "WARNING: Unable to write datarc template for $name;";
            #~~~~~#
            return;
            #~~~~~#
        }

        $datarc = "$plotHISTdir/$name.tmpl";
        open DATARC, "> $datarc" or die "Error opening $datarc;";
        $FH = select;
        select DATARC;
        print "DSET >>>FVHOME<<<$dirloc>>>EXPID<<<.$name.nc4\n";
        print "TITLE >>>EXPID<<<\n";
        print "OPTIONS template\n";
        print "TDEF time 1 LINEAR 00:00Z01>>>TDEF<<< 1mo\n";
        close DATARC;
        select $FH;
        #~~~~~#
        return;
        #~~~~~#
    }

    # write rc file for all other data
    #---------------------------------
    $dirloc = "/$1" if $template =~ m|#*(.*M%m2/)|;
    unless ($dirloc) {
        print "WARNING: Unable to write datarc template for $name;";
        #~~~~~#
        return;
        #~~~~~#
    }

    $datarc = "$plotHISTdir/$name.tmpl";
    open DATARC, "> $datarc" or die "Error opening $datarc;";
    $FH = select;
    select DATARC;
    print "DSET >>>FVHOME<<<$dirloc>>>EXPID<<<.$name.monthly.\%y4\%m2.nc4\n";
    print "TITLE >>>EXPID<<<\n";
    print "OPTIONS template\n";
    print "TDEF time >>>NFILES<<< LINEAR 00:00Z01>>>TDEF<<< 1mo\n";
    close DATARC;
    select $FH;
}

#=======================================================================
# name - delete_trait
# purpose - delete trait from collection name
#=======================================================================
sub delete_trait {
    my ($name, $trait, @traits, $trt, @new);
    $name  = shift @_;
    $trait = shift @_;

    return unless $traitHash{$name};
    @traits = split/[:|,]/, $traitHash{$name};
    foreach $trt (@traits) { push @new, $trt unless $trt =~ /$trait.N\d+/ }
    $traitHash{$name} = join ":", @new;
}

#=======================================================================
# name - add_grads_ddf
# purpose - add grads_ddf trait to collection name
#=======================================================================
sub add_grads_ddf {
    my ($name, $name1, @traits, $trt, @new);
    $name = shift @_;

    # add to %traitHash
    #------------------
    return unless $traitHash{$name};
    @traits = split/[:|,]/, $traitHash{$name};
    foreach $trt (@traits) {
        push @new, $trt;
        push @new, "grads_ddf.N1" if $trt eq "descr.N1";
    }
    $traitHash{$name} = join ":", @new;

    # add definition
    #---------------
    ($name1 = $name) =~ s/[-|+-]$//;
    $cdef{"$name.grads_ddf.N1"} = "'>>>PLOTSDIR<<</$name1.tabl'";
    $comments{"$name.grads_ddf.N1"} = "";
    $clutter{"$name.grads_ddf.N1"} = "";
}

#=======================================================================
# name - write_HISTfile
# purpose - Write the output HISTORY template.
#=======================================================================
sub write_HISTfile {
    use Cwd ("abs_path");
    use File::Copy ("move");
    my ($OUTtilde, $ans, $first, $num, $name, $name1, @list, $xtra);

    # save backup of input before overwriting
    #----------------------------------------
    if ($OUTfile) {
        if (-e $OUTfile) {
             $OUTtilde = "$OUTfile~";
             move $OUTfile, $OUTtilde unless -e $OUTtilde;
        }
        open OUTF, "> $OUTfile" or die "ERROR opening $OUTfile: $!";
        select OUTF;
    }

    # print headings
    #---------------
    foreach (@headings) { print $_ }

    # print COLLECTIONS list
    #-----------------------
    @list = @topList;
    $first = 1;
    $num = 1;

    print "COLLECTIONS:";
    foreach $name (@list) {

        # COLLECTIONS comments
        #---------------------
        $name1 = rm_dash_plus($name);
        if ($comments{$name}) {
            if ($first) { print "\n"; $first = 0 }
            print $comments{$name}
        }

        # COLLECTIONS names
        #------------------
        $xtra = $clutter{$name};
        $xtra = pad($xtra, 12) if $xtra or ! $first;
        $xtra = "\n$xtra" if $first and $xtra;
        $xtra = "" unless $xtra;

        print "$xtra '$name1'\n";
        $first = 0;
    }

    # end COLLECTIONS list
    #---------------------
    print "\n" if ($first);
    print " "x13 . "::\n";

    if ($orderFLG) { @list = @topList    }
    else           { @list = @bottomList }

    # print collection definitions
    #-----------------------------
    foreach $name (@list) { write_collection_def($name) }

    # close output
    #-------------
    if ($OUTfile) {
        close OUTF;
        select STDOUT;
    }
}

#=======================================================================
# name - add_silo_mstorage_traits
# purpose - add the silo and mstorage traits to each collection if they
#           are not present
#
# Note: This sub can be used to transform HISTORY files which do not already
#       have the silo and mstorage traits. It will no longer be required
#       after HISTORY files have been transformed.
#=======================================================================
sub add_silo_mstorage_traits {
    my (@anaID, @chemID, @diagID, @progID, $name, $storage);

    @anaID  = qw( vtx .eta .sfc .prs );
    @chemID = qw( _adg_ _aer_ _ctm_ _chm_ _gas_ _nav_ _tag_ 
                   adg_  aer_  ctm_  chm_  gas_  nav_  tag_ );
    @diagID = qw( _asm_ _chm_ _cld_ _csp_ _dyn_ _ext_ _flx_ _glc_
                  _hwl_ _int_ _lfo_ _lnd_ _lsf_ _met_ _mst_ _ocn_
                  _odt_ _qdt_ _rad_ _slv_ _tdt_ _tmp_ _trb_ _udt_ _wnd_ 
                   asm_        cld_  csp_  dyn_  ext_  flx_  glc_
                   hwl_  int_  lfo_  lnd_  lsf_  met_  mst_  ocn_
                   odt_  qdt_  rad_  slv_  tdt_  tmp_  trb_  udt_  wnd_ );
    @progID = qw( prog traj ptrj );

    # add silo trait if not present
    #------------------------------
  outer: foreach $name (@bottomList) {

      next outer if $name =~ m/_rst/ and $name ne "bkg_clcv_rst";
      unless ($traitHash{$name} =~ m/\bsilo\b/) {
          $traitHash{$name} =~ s/:template:/:template:silo.N1:/;

          $clutter{"${name}.silo.N1"} = "";
          $comments{"${name}.silo.N1"} = "";

          foreach (@progID) {
              if ($name =~ m/$_/) {
                  $cdef{"${name}.silo.N1"} = "'prog/Y%y4/M%m2'";
                  next outer;
              }
          }

          foreach (@anaID) {
              if ($name =~ m/$_/) {
                  if ($fcstFLG) {
                      $cdef{"${name}.silo.N1"} = "'prog/Y%y4/M%m2/D%d2/H%h2'";
                  } else {
                      $cdef{"${name}.silo.N1"} = "'ana/Y%y4/M%m2'";
                  }
                  next outer;
              }
          }

          foreach (@chemID) {
              if ($name =~ m/$_/) {
                  if ($fcstFLG) {
                      $cdef{"${name}.silo.N1"} = "'prog/Y%y4/M%m2/D%d2/H%h2'";
                  } else {
                      $cdef{"${name}.silo.N1"} = "'chem/Y%y4/M%m2'";
                  }
                  next outer;
              }
          }

          foreach (@diagID) {
              if ($name =~ m/$_/) {
                  if ($fcstFLG) {
                      $cdef{"${name}.silo.N1"} = "'prog/Y%y4/M%m2/D%d2/H%h2'";
                  } else {
                      $cdef{"${name}.silo.N1"} = "'diag/Y%y4/M%m2'";
                  }
                  next outer;
              }
          }

          # default if not caught by any of previous tests
          #-----------------------------------------------
          $cdef{"${name}.silo.N1"} = "'other/Y%y4/M%m2'";
      }
  }

    # add mstorage trait if not present
    #----------------------------------
    foreach $name (@bottomList) {
        unless ($traitHash{$name} =~ m/\bmstorage\b/) {
            $traitHash{$name} =~ s/:silo.N1:/:silo.N1:mstorage.N1:/;
            $storage = "'yes'";
            $cdef{"${name}.mstorage.N1"} = $storage;
            $clutter{"${name}.mstorage.N1"} = "";
            $comments{"${name}.mstorage.N1"} = "";
        }
    }
}

#=======================================================================
# name - write_collection_def
# purpose - write formatted collection definition
#=======================================================================
sub write_collection_def {
    my ($name, $name1, $tmax, $blanks);
    my (@traits, $trait, $traitN, $label, $value);
    my ($line, @xtra, $xtra, @var, @src, @alias, @minmax, @len, $i);
    my ($fmt1, $fmt2, $fmt3, $fmt4, $diff);

    $name = shift @_;
    $name1 = rm_dash_plus($name);

    @traits = split /[:|,]/, $traitHash{$name} if $traitHash{$name};
    remove_array_duplicates(\@traits);

    $tmax = 0;
    foreach $traitN (@traits) {
        ($trait = $traitN) =~ s/\.N\d+$//;
        $tmax = length("$name1.$trait") if length("$name1.$trait") > $tmax;
    }
    $tmax += 4;

    foreach $traitN (@traits) {
        ($trait = $traitN) =~ s/\.N\d+$//;

        print $comments{"$name.$traitN"};
        $xtra = pad($clutter{"$name.$traitN"}, 1);

        # print traits (except fields)
        #-----------------------------
        unless ($trait eq "fields") {
            $label = "$xtra $name1.$trait";
            $value = $cdef{"$name.$traitN"};
            if ($value =~ /#/) { printf "%-${tmax}s %s\n",   "$label:", $value }
            else               { printf "%-${tmax}s %s ,\n", "$label:", $value }
            next;
        }

        # print fields
        #-------------
        printf "%-${tmax}s", "$xtra $name1.fields:";

        $line = $cdef{"$name.$traitN"};
        strip_fields($name, $line, \@xtra, \@var, \@src, \@alias, \@minmax, \@len);

        $fmt1 = "%s %s\n";
        $fmt2 = "%s %-$len[0]s  , %-$len[1]s  ,\n";
        $fmt3 = "%s %-$len[0]s  , %-$len[1]s  , %-$len[2]s  ,\n";
        $fmt4 = "%s %-$len[0]s  , %-$len[1]s  , %-$len[2]s  , %-$len[3]s  ,\n";

        if   ($minmax[0]) {
            printf $fmt4, $xtra[0], $var[0], $src[0], $alias[0], $minmax[0];
        }
        elsif ($alias[0]) {
            printf $fmt3, $xtra[0], $var[0], $src[0], $alias[0];
        }
        else {
            printf $fmt2, $xtra[0], $var[0], $src[0];
        }

        for $i (1..$#var) {
            print $comments{"$name.$var[$i].$xtra[$i]"};

            $xtra = pad($xtra[$i], $tmax);

            if ($minmax[$i]) {
                printf $fmt4, $xtra, $var[$i], $src[$i], $alias[$i], $minmax[$i];
            }
            elsif ($alias[$i]) {
                printf $fmt3, $xtra, $var[$i], $src[$i], $alias[$i];
            }
            elsif ($src[$i]) {
                printf $fmt2, $xtra, $var[$i], $src[$i];
            }
            else {
                printf $fmt1, $xtra, $var[$i];
            }
        }
    }
    print $comments{"$name.end"} if $comments{"$name.end"};
    print " "x$tmax ."::\n";
}

#=======================================================================
# name - strip_fields
# purpose - strip variable, source, alias, and min/max from collection field
#=======================================================================
sub strip_fields {
    my ($name, $fieldStr, $xAddr, $vAddr, $sAddr, $aAddr, $mAddr, $lAddr);
    my (@fieldLines, $line, $var, $src, $alias, $minmax, $xtra, @vparts);
    my ($warning, $templine, $cnt1, $cnt2);

    $name = shift @_;
    $fieldStr = shift @_;
    $xAddr = shift @_;
    $vAddr = shift @_;
    $sAddr = shift @_;
    $aAddr = shift @_;
    $mAddr = shift @_;
    $lAddr = shift @_;

    # initialize return arrays
    #-------------------------
    @$xAddr = ();
    @$vAddr = ();
    @$sAddr = ();
    @$aAddr = ();
    @$mAddr = ();
    @$lAddr = (0, 0, 0, 0);

    @fieldLines = split /\n/, $fieldStr;
    foreach (@fieldLines) {
        $line = $_;

        # split line into components
        #---------------------------
        ($var, $src, $alias, $minmax) = split /[,]/;
        $var = "" unless $var;
        $src = "" unless $src;
        $alias = "" unless $alias;
        $minmax = "" unless $minmax;

        # catch anything extra found in front of variable name
        #-----------------------------------------------------
        $xtra = "";
        if ( $var =~ /(^.*)(([\'|\"]).*\3)/ ) {
            $xtra = $1;
            $var = $2;
        }

        # remove leading and trailing blanks
        #-----------------------------------
        foreach (\$xtra, \$var, \$src, \$alias, \$minmax) { $$_ = clean($$_) }
        $xtra .= " " if $xtra;

        # check comma count
        #------------------
        $templine = $line; $cnt1 = 0;
        foreach ($var, $src, $alias, $minmax) { ++$cnt1 if $_ }
        $cnt2 = ($templine =~ s/,/x/g);
        if ($cnt2 > $cnt1) {
            warn "\nERROR: too many commas found in $name.fields.$var\n"
                ."  => $line\n\n";
            die;
        }
        elsif ($cnt2 < $cnt1) {
            $warning = "WARNING: missing comma(s) in $name.fields.$var\n"
                .      "WARNING  => $line\n";
            unless ($warnOnce{$warning}) {
                warn $warning;
                $warnOnce{$warning} = 1;
            }
        }

        # check that each field contains $var and $src
        #---------------------------------------------
        if ( ($src and ! $var) or ($alias and ! $var) ) {
            warn "\nERROR: variable name not found for $name.fields\n"
                ."  => $line\n\n";
            die;
        }
        if ( ($var and ! $src) or ($alias and ! $src) ) {
            warn "\nERROR: variable source not found in $name.$var\n"
                ."  => $line\n\n";
            die;
        }

        # note if variable is commented
        #------------------------------
        if ( commentLine($_) ) {
            $clutter{"$name.$var"} = "#";
            $warning = "WARNING: $name $var field is commented.\n";
            unless ($warnOnce{$warning}) {
                warn $warning unless $quietCM;
                $warnOnce{$warning} = 1;
            }
        }

        # get lengths for formatting purposes
        #------------------------------------
        @$lAddr[0] = length($var)    if length($var)    > @$lAddr[0];
        @$lAddr[1] = length($src)    if length($src)    > @$lAddr[1];
        @$lAddr[2] = length($alias)  if length($alias)  > @$lAddr[2];
        @$lAddr[3] = length($minmax) if length($minmax) > @$lAddr[3];

        # store $var, $src, and $alias in arrays
        #---------------------------------------
        push @$xAddr, $xtra;
        push @$vAddr, $var;
        push @$sAddr, $src;
        push @$aAddr, $alias;
        push @$mAddr, $minmax;
    }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#           UTILITY SUBROUTINES
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#=======================================================================
# name - clean
# purpose - remove leading and trailing blank spaces from a string
#=======================================================================
sub clean {
    my $string = shift @_;
    $string =~ s/^\s*|\s*$//g;
    return $string;
}

#=======================================================================
# name - comment
# purpose - Add comment character to beginning of a string if it is
#           not already there.
#=======================================================================
sub comment {
    my ($strAddr);
    $strAddr = shift @_;
    $$strAddr = "" unless defined($$strAddr);
    $$strAddr = "#" . $$strAddr unless $$strAddr =~ /^\s*\#/;
}

#=======================================================================
# name - commentLine
# purpose - return true (=1) if line contains only a comment or whitespace
#=======================================================================
sub commentLine {
    my $line;

    $line = shift @_;
    return 1 if $line =~ /^\s*\#/;
    return 1 if $line =~ /^\s*$/;

    return 0;
}

#=======================================================================
# name - cnt_included
# purpose - returns the number of defined collections which are
#           currently included in the collection list to be written
#           to output.
# note -
#  1. The return value changes as the user includes or excludes
#     collections from the list.
#=======================================================================
sub cnt_included {
    my $num;
    $num = 0;
    foreach (@topList) { $num++ unless $clutter{$_} };
    return $num;
}

#=======================================================================
# name - cnt_excluded
# purpose - returns the number of collections which are NOT included in
#           the collection list to be written to output.
# note -
#  1. The return value changes as the user includes or excludes
#     collections from the list.
#=======================================================================
sub cnt_excluded {
    my $num;
    $num = 0;
    foreach (@topList) { $num++ if $clutter{$_} }
    return $num;
}

#=======================================================================
# name - found
# purpose - determine whether a value (or pattern) is found in an array
#
# input parameters
# => $value: value being searched
# => $arrAddr: address of array where search is being made
# => $pmFLG: (optional) pattern match flag
#             =0 (default) look for exact match during search
#             =1 use pattern matching during search
#=======================================================================
sub found {
    my($value, $arrAddr, $pmFLG);
    my (@array, $foundFLG, $name, $name1);

    # input parameters
    #-----------------
    $value   = shift @_;
    $arrAddr = shift @_;
    $pmFLG  = shift @_;

    @array = @$arrAddr;
    $foundFLG = 0;
    return unless $value;

    $value = rm_dash_plus($value);
    foreach $name (@array) {
        $name1 = rm_dash_plus($name);
        if ($pmFLG) { $foundFLG = $name if $name1 =~ /$value/; next }
        else        { $foundFLG = $name if $name1 eq $value;   next }
    }
    return $foundFLG;
}

#=======================================================================
# name - freqhours
# purpose - convert frequency in hhmmss format to number of hours
#=======================================================================
sub freqhours {
    my ($freq, $hrs, $mins, $secs, $hours);
    $freq = shift @_;

    $hrs  = substr($freq, -6, 2);
    $mins = substr($freq, -4, 2);
    $secs = substr($freq, -2, 2);

    $hours = $hrs + ($mins*60 + $secs*3600)/3600;
    return $hours;
}

#=======================================================================
# name - pad
# purpose - add blank spaces to end of a string to make it a certain length
#
# input parameters
# => $string: string to pad
# => $len: desired length for padded string
#=======================================================================
sub pad {
    my ($string, $len, $diff);
    $string = shift @_;
    $len = shift @_;

    $string = "" unless $string;
    $diff = $len - length($string);
    $string .= " "x$diff if $diff > 0;
    return $string;
}

#=======================================================================
# name - pause
# purpose - pause output
#=======================================================================
sub pause {
    my $dummy;
    return unless $prompt;
    print "Hit <cr> to continue .. "; $dummy = <STDIN>;
}

#=======================================================================
# name - remove_array_duplicates
# purpose - remove duplicate values from array
#=======================================================================
sub remove_array_duplicates {
    my ($arrAddr, $last, $value, %found);

    $arrAddr = shift @_;
    $last = $#$arrAddr;

    foreach (0..$last) {
        $value = shift @$arrAddr;
        next if $found{$value};

        $found{$value} = 1;
        push @$arrAddr, $value;
    }
}

#=======================================================================
# name - rm_dash_plus
# purpose - filter plus/dash endings from collection names in input string
#=======================================================================
sub rm_dash_plus {
    my ($string);
    $string = shift @_;

    if ($rmDASH) { foreach (keys %cleanD) { $string =~ s/$_/$cleanD{$_}/ } }
    if ($rmPLUS) { foreach (keys %cleanP) { $string =~ s/$_/$cleanP{$_}/ } }

    return $string;
}

#=======================================================================
# name - toggle_dash_plus
# purpose - toggles global variables, $rmDASH and $rmPLUS, which controls whether
#           the "-" and "+-" endings are included in the collection names
#=======================================================================
sub toggle_dash_plus {
    my ($ans);
    return unless %cleanD or %cleanP;

    if ($rmDASH)     { print "Hit 'd' to restore - name extensions\n"  }
    elsif ($rmPLUS)  { print "Hit 'p' to restore +- name extensions\n" }
    else             { print "Hit 'd' to remove - name extensions\n"
                           . "Hit 'p' to remove +- name extensions\n"  }
    print "Hit <cr> to continue ..\n";
    chomp($ans = lc <STDIN>);
    return if $ans =~ /^\s*$/;

    if ($ans eq "d") {
        if ($rmDASH) { $rmDASH = 0 }
        else         { $rmDASH = 1 unless $rmPLUS }
    }
    elsif ($ans eq "p") {
        if ($rmPLUS) { $rmPLUS = 0 }
        else         { $rmPLUS = 1 unless $rmDASH }
    }
    else { print "Unrecognizable response\n"; pause() }
    return 1;
}

#=======================================================================
# name - userResponse
# purpose - get integer inputs from user and weed out
#           all other responses
#=======================================================================
sub userResponse {
    my ($max, $line, @array);
    my ($size, $sel);
    $max = shift @_;

    chomp($line = <STDIN>);
    return unless ($line);

    $line =~ s/,/ /g;
    @array = split /\s+/, $line;

    $size = scalar @array;
    for (1..$size) {
        $sel = shift @array;
        next unless ($sel =~ /^\d+$/);        # integer check
        next if ($sel < 1) or ($sel > $max);  # range check
        $sel = $sel + 1 - 1;                  # convert from character
        push @array, $sel;
    }
    return @array;
}

#=======================================================================
# name - usage
# purpose - prints usage information
#=======================================================================
sub usage {
    use File::Basename;
    my ($script, $FH, $info);

    $script = basename $0;
    $FH = select;
    open(INFO, ">", \$info) or die "ERROR opening ";

    select INFO;
    print <<"EOF";

NAME
     $script

PURPOSE
     Edit the COLLECTIONS list located at the top of the HISTORY.rc.tmpl file
     or return a list of the collections in the file

SYNOPSIS
     $script [edit options] [infile]
     $script -plot [mnthlyRC]
     $script -list [list options] [infile]
     $script -sum [n]
     $script [arc file options]

where
     infile is the HISTORY file; defaults to "HISTORY.rc.tmpl"

EDIT OPTIONS
     -o outfile      name of output HISTORY template file; defaults to STDOUT;
                     if outfile is directory name, then the output will be written
                     in directory with same name as infile
     -i              edit input file "in place"; i.e. overwrite input with output

     -Iall           include all collections except those specified by -X
     -Xall           exclude all collections except those specified by -I
     -I names        names to include in list of collections (see NOTE #3)
     -X names        names to exclude from list of collections (see NOTE #3)
     -Ipm patterns   match patterns used to determine which names
                     to include in list of collections (see NOTE #3)
     -Xpm patterns   match patterns used to determine which names
                     to exclude from list of collections (see NOTE #3)
     -Xdep source    exclude collections which include data from source, e.g GOCART
     -ID             include collections which end with "-"
     -XD             exclude collections which end with "-"
     -IP             include collections which end with "+-"
     -XP             exclude collections which end with "+-"

     -rD             remove "-" from collection name endings
     -rP             remove "+-" from collection name endings

     -s str1=str2    edit substitute val for label

     -s1 str1        substitute str1 with str2 from -s2 flag
     -s2 str2        substitute str1 from -s1 flag with str2

     -p              prompt user for interactive input
     -q [n]          quiet mode; turns off some of the WARNINGs
                        n==1 turn off clutter warnings
                        n==2 turn off comment warnings
                        n==3 turn off clutter and comment warnings (default)
     -order          print definitions in same order as listed in top COLLECTIONS
     -h              prints this usage message

MONTHLY PLOTS HISTORY
     -plot [mnthlyRC]  name or directory location of monthly.rc file to
                       use for writing the monthly_plots HISTORY file;
                       defaults to "monthly.rc" if name is not provided

LIST OPTIONS
     all             list all collections
     inc             list only the included collections
     exc             list only the excluded collections

     Add ":" or "," to end of list option to get output as single string with
     names separated by colons or commas;

     Example:  $script -list inc     (prints included collections, one per line)
               $script -list inc:    (prints included collections in string
                                      separated by colons)
               $script -list inc,    (prints included collections in string
                                      separated by commas)

SUM SLICES
     -sum [n]       sum the number of output slices per 24-hour period;
                    i.e sum of variables times levels times frequency
                        for each output collection

                    if n=0, then print only the final value
                    if n=1, then print details grouped by number of atmosphere levels
                    if n=2, then print details grouped by data frequency
                    * defaults to n=1 if 'n' is excluded.

                    You may want to use the -q flag with -sum to turn off WARNINGs

ARC FILE OPTIONS
     -add            add silo and mstorage traits to collections that do not have them;
                     if the -arc flag is not called, then the HISTORY.rc file will be
                     written with silo and mstorage traits for all collections

     -arc            write arc files: silo.hist.arc and mstorage.hist.arc
                     (see -silo and -mstorage flags)

     -X names        collection names to comment in silo.arc, if with -arc flag (see NOTE #3)
     -append         with -arc flag; append to arc files if they already exist,
                     rather than overwriting

     -fcst           with -arc flag; write arc entries for forecast output

     -silo name      with -arc flag; use alternate name for silo.hist.arc output

     -mstorage name  with -arc flag; use alternate name for mstorage.hist.arc output

NOTES
1. The -s, -s1, and -s2 flags can be listed multiple times
2. Each -s1 flag must have a corresponding -s2 flag and vice versa
3. The -I, -X, -Ipm, -Xpm flags can be listed multiple times and the value
   (names or patterns) associated with each flag can can contain either one
   entry or multiple entries separated by ":" or ","
4. If the -plot flag is present, then data rc template files will be written in the
   same directory as the HISTORY file for each collection in the monthly.rc file.
   Note that these files will not be written if the HISTORY file is written to STDOUT.

AUTHOR
     Joe Stassi, SAIC (joe.stassi\@nasa.gov)

EOF
close INFO;

    select $FH;
    system("echo \"$info\" | more");
    exit;
}
