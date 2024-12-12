#!/usr/bin/env perl
#
use POSIX qw/ceil/;
use POSIX qw/floor/;
use strict;
use warnings;

my $MYNAME = "atmens_ntasks.pl";
my ($this_ntasks_per_node, $thisnode, $myntasks, $ncores_needed, $nexecs_fit, $myntasks_per_node);
 
my ($MYTASK_NCPUS, $ipoe) = @ARGV;
if (not defined $MYTASK_NCPUS) {
  die "Need NCPUS\n";
}
 
if (not defined $ipoe) {
  die "${MYNAME}: Need DST\n";
}

$this_ntasks_per_node = `facter processorcount`;
$this_ntasks_per_node = $this_ntasks_per_node - 2;
$myntasks = $ipoe * $MYTASK_NCPUS;
$ncores_needed = ceil($myntasks / $this_ntasks_per_node + 0.1);
$nexecs_fit    = floor($this_ntasks_per_node / $MYTASK_NCPUS );
if ( $nexecs_fit == 0 ) {$nexecs_fit = 1};
$myntasks = $ncores_needed * $this_ntasks_per_node;
$myntasks_per_node = $nexecs_fit * $MYTASK_NCPUS;

# the above does not give a decent partition of the borgs
# so let me wire a mildly wasteful way of doing this
# basically the following grabs as many nodes as instances
# of a exec being distributed within a single job; the 
# following also accomodates the case when an instance of 
# an exec needs more than a single node to run. 
# The code above was aimed at doing this a little more efficiently
# and less wastefully, but I can quite get it to work, so ...
$ncores_needed = ceil($MYTASK_NCPUS / $this_ntasks_per_node + 0.1);
$myntasks = $ipoe * $ncores_needed * $this_ntasks_per_node;
$myntasks_per_node = -1;

my $JOBGEN_NCPUS = $myntasks;
my $JOBGEN_NCPUS_PER_NODE = $myntasks_per_node;

#print "per-node = $this_ntasks_per_node \n";
#print "cores = $ncores_needed  \n";
#print "fit = $nexecs_fit  \n";
print "$JOBGEN_NCPUS \n";
print "$JOBGEN_NCPUS_PER_NODE \n";

