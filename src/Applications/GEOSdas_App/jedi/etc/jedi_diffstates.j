#!/bin/csh -x
#---- blow  needs to be unwired ----
@GEOSJEDI_QOS
@GEOSJEDI_PARTITION
#SBATCH --ntasks-per-node=12 --ntasks=${JEDI_DIF_NCPUS}
#SBATCH --constraint=mil
#^^^^ above needs to be unwired ^^^^
#SBATCH --account=$GID
#SBATCH --job-name=diffstates     
#SBATCH --time=1:00:00
#SBATCH -o output.%A_%a
#SBATCH --array=0-12:2
 
setenv JEDI_ROOT $JEDI_ROOT
source $JEDI_ROOT/modules

cd $JEDIWRK/$GETINCWORK.${SLURM_ARRAY_TASK_ID}

$JEDI_GETINC_MPIRUN $JEDI_ROOT/bin/fv3jedi_diffstates.x my.yaml > input.${SLURM_ARRAY_TASK_ID}

/bin/mv *inc*nc4 ../
