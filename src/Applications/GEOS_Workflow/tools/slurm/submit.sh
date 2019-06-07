#/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable

usage_() {
    echo
    echo "usage: $0 job_file job_out_file"
    echo "  e.g. $0 %ECF_JOB% %ECF_JOBOUT%"
    echo
}

# Arguments
if [ $# -ne 2 ] ; then
    usage_
    exit 1
fi
job_file=$1
job_out_file=$2

# Submit job
/usr/slurm/bin/sbatch $job_file &> $job_out_file
