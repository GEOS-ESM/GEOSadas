#/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable

usage_() {
    echo
    echo "usage: $0 job_out_file"
    echo "  e.g. $0 %ECF_JOBOUT%"
    echo
}

# Arguments
if [ $# -ne 1 ] ; then
    usage_
    exit 1
fi
job_out_file=$1

# Get job id from job out file and kill the job
job_id=$(grep "Submitted batch job " $job_out_file | awk '{print $4}')
/usr/slurm/bin/scancel $job_id
