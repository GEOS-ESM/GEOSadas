#!/bin/csh
#
# 11Feb2013  Todling  - Initial code, adapted from Doris Pan original script
# 23May2016  Thompson - Edited for MPT usage. Needs a script tentatively called
#                       mptfix.pl that alters the MPI run cmd from mpiexec_mpt
#                       to a corrected mpirun call that passes in nodes.
#                       Requires new option passed in, -usrppn, for procs per node
# 24Jun2016  Thompson - Due to fixes in mptfix.pl, no longer need to pass in -usrppn
# 29May2020  Todling  - Tentative fix for SLURM
#
#

setenv MYNAME "job_distributor.csh"

setenv dryrun echo # use this when debugging
setenv dryrun 

setenv FAILED 0
if ( !($?FVROOT) ) setenv FAILED 1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

if ( ($?FVHOME) ) then
  set path = ( . $FVHOME/run $FVROOT/bin $path )
else
  set path = ( . $FVROOT/bin $path )
endif

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

if ( "$7" != "-njobs" ) then
   echo $0: missing njobs >&2;echo "$usage" >&2;exit 1
endif
set njobs = $8
echo "njobs = $njobs"


if ( $?SLURM_JOBID ) then

  # Number of available nodes
  if ( $?SLURM_JOB_NUM_NODES ) then
     set num_nodes = $SLURM_JOB_NUM_NODES
  else
     set num_nodes = `sinfo -all -N -n "$SLURM_NODELIST" | grep -v NODELIST | cut -c1-8 | uniq | wc -l`
  endif
  if ( $?SLURM_NTASKS_PER_NODE ) then
    set ntasks_per_node = $SLURM_NTASKS_PER_NODE
  else
    set ntasks_per_node = $num_nodes
  endif

  # Mimic old PBS_NODEFILE
  setenv PBS_NODEFILE PBS_NODEFILE_${SLURM_JOBID} 
  if(-e $PBS_NODEFILE) /bin/rm $PBS_NODEFILE
  echo " usrntask $usrntask"
  echo " njobs $njobs"
  echo " num_nodes $num_nodes"
  echo " ntasks_per_node $ntasks_per_node"
  @ tasks_per_nodes = ($usrntask * $njobs) / $num_nodes
  set tasklist = ()
  @ nd = 0
  while ($nd < $num_nodes)
    set tasklist = ($tasklist,$tasks_per_nodes)
    @ nd++
  end
  set tasklist = `echo $tasklist | cut -c2-`
  arbitrary.pl $tasklist >> $PBS_NODEFILE
else
  @ ntasks = $usrntask
  if ( ! -e $PBS_NODEFILE ) then
    echo "Please execute the Poe script in a valid PBS session"
    exit 3
  endif
  set LIST_NODES = nodeFile_${PBS_JOBID}
  uniq ${PBS_NODEFILE} > $TMPDIR/${LIST_NODES}
  # Number of available nodes
  set num_nodes = `cat $TMPDIR/${LIST_NODES} | wc -l`
  echo "num_nodes: " $num_nodes
endif

set npes = `cat ${PBS_NODEFILE} | wc -l`
 @ num_jobs = $npes / $usrntask
echo "can run up to $num_jobs jobs!"

# First boot the mpd ring
#mpdboot -n $num_nodes -r ssh -f $PBS_NODEFILE

# Split user command file
set uline = ()
foreach line (`cat $usrcmd` )
  set uline = ( $line $uline ) 
end
set ncmds = $#uline
echo "will be running $ncmds jobs!"

# Loop until all jobs have been launched
@ job_index = 1
@ icnt = 0
rm -f $filename.*
foreach line (`cat ${PBS_NODEFILE} `)
      echo $line >> $filename.$job_index
      if ( $icnt == $usrntask - 1 && $job_index <= $ncmds ) then
        mptfix.pl -debug $uline[$job_index] $filename.$job_index $usrntask
        $dryrun $uline[$job_index] &
        @ job_index = $job_index + 1
        @ icnt = 0
      else
        @ icnt++
      endif
    end
wait

echo "All tasks done!"
#mpdallexit 

