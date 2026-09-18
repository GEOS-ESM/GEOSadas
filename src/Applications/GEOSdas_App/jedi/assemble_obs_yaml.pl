#!/usr/bin/perl
use strict;
use warnings;
use File::Spec;

# Need at least: dir + one file + output
die "Usage: perl combine.pl <dir> <file1> [file2 ...] <output>\n"
    unless @ARGV >= 3;

my $dir = shift @ARGV;        # directory
my $output_file = pop @ARGV;  # last arg = output file
my @files = @ARGV;            # remaining = input files

open(my $out_fh, ">", $output_file)
    or die "Cannot open output file '$output_file': $!";

foreach my $file (@files) {

    my $fullpath = File::Spec->catfile($dir, $file);

    unless (-f $fullpath) {
        warn "Skipping '$fullpath' (not found or not a regular file)\n";
        next;
    }

    open(my $in_fh, "<", $fullpath)
        or die "Cannot open file '$fullpath': $!";

    print $out_fh "#==== $file ====\n";  # optional separator

    while (my $line = <$in_fh>) {
        print $out_fh $line;
    }

    close($in_fh);
}

close($out_fh);

print "Files have been combined into $output_file\n";
