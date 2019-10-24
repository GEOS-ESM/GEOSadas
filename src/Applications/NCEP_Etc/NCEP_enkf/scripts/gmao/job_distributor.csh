#!/bin/csh
#
# 11Feb2013  Todling  - Initial code, adapted from Doris Pan original script
# 23May2016  Thompson - Edited for MPT usage. Needs a script tentatively called
#                       mptfix.pl that alters the MPI run cmd from mpirun
#                       to a corrected mpirun call that passes in nodes.
#                       Requires new option passed in, -usrppn, for procs per node
# 24Jun2016  Thompson - Due to fixes in mptfix.pl, no longer need to pass in -usrppn
#
#

setenv dryrun 

setenv FAILED 0
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set path = ( . $FVHOME/run $FVROOT/bin $path )

set usage="\
Usage:  $0 -machfile machinefile -usrcmd usrcmd -usrntask ntask "

if ( $#argv == 0 ) then
   echo $0: missing arguments >&2;echo "$usage" >&2;exit 1
endif
if ( "$1" != "-machfile" ) then
   echo $0: missing machfile >&2;echo "$usage" >&2;exit 1
endif
set filename = $2
echo "filename = $filename"

if ( "$3" != "-usrcmd" ) then
   echo $0: missing usrcmd >&2;echo "$usage" >&2;exit 1
endif
set usrcmd = $4
echo "user_command = $usrcmd"

if ( "$5" != "-usrntask" ) then
   echo $0: missing usrntask >&2;echo "$usage" >&2;exit 1
endif
set usrntask = $6
echo "user_ntask = $usrntask"


if ( ! -e $PBS_NODEFILE ) then
  echo "Please execute the Poe script in a valid PBS session"
  exit 3
endif

set LIST_NODES = nodeFile_${PBS_JOBID}
uniq ${PBS_NODEFILE} > $TMPDIR/${LIST_NODES}

# Number of available nodes
set num_nodes = `cat $TMPDIR/${LIST_NODES} | wc -l`
echo num_nodes $num_nodes

set npes = `cat ${PBS_NODEFILE} | wc -l`
@ num_jobs = $npes / $usrntask
echo "will be running $num_jobs jobs!"

# First boot the mpd ring
mpdboot -n $num_nodes -r ssh -f $PBS_NODEFILE

# Split user command file
set uline = ()
foreach line (`cat $usrcmd` )
  set uline = ( $line $uline ) 
end
# Loop until all jobs have been launched
@ job_index = 1
@ icnt = 0
rm -f $filename.*
foreach line (`cat ${PBS_NODEFILE} `)
      echo $line >> $filename.$job_index
      if ( $icnt == $usrntask - 1 ) then
        mptfix.pl $uline[$job_index] $filename.$job_index $usrntask
        $dryrun $uline[$job_index] &
        @ job_index = $job_index + 1
        @ icnt = 0
      else
        @ icnt++
      endif
    end
wait

echo "All tasks done!"
mpdallexit 

