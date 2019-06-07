#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="AtmensSummarizeTimings"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="atmens_global"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Summarize timings
zeit_pr.x -i $FVWORK/.zeit
