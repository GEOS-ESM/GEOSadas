#!/usr/bin/env perl
use strict;
use warnings;

# Usage check
if (@ARGV != 3) {
    die "Usage: $0 <file_to_insert> <target_file> <target_string>\n";
}

my ($insert_file, $target_file, $target_string) = @ARGV;

# Read insert file
open(my $fh_insert, '<', $insert_file) or die "Cannot open $insert_file: $!";
my @insert_lines = <$fh_insert>;
close($fh_insert);

# Read target file
open(my $fh_target, '<', $target_file) or die "Cannot open $target_file: $!";
my @target_lines = <$fh_target>;
close($fh_target);

# Find the index where the string occurs
my $found = 0;
for (my $i = 0; $i < @target_lines; $i++) {
    if ($target_lines[$i] =~ /\Q$target_string\E/) {
        splice(@target_lines, $i, 0, @insert_lines);  # Insert *before* matching line
        $found = 1;
        last;
    }
}

if (!$found) {
    die "Target string '$target_string' not found in $target_file\n";
}

# Write back to the file
open(my $fh_out, '>', $target_file) or die "Cannot write to $target_file: $!";
print $fh_out @target_lines;
close($fh_out);

#print "Inserted content of '$insert_file' into '$target_file' before line containing '$target_string'\n";

