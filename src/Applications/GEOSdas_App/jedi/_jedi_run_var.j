#!/bin/csh -fx
# ------------------------------
#SBATCH --account=g0613
#SBATCH --qos=dastest
#SBATCH --partition=preops
#SBATCH --job-name=jedivar
#SBATCH --output=jedivar.log.o%j.txt
#_SBATCH --ntasks=96
#_SBATCH --ntasks-per-node=16
#SBATCH --ntasks=240
#SBATCH --constraint=sky
#SBATCH --time=2:00:00

source $JEDIBUILD/modules

cd $JEDIWORK

$JEDI_FV3VAR_MPIRUN $JEDIBUILD/bin/fv3jedi_var.x $MYCONF |& tee -a $FVWORK/$JEDIVARLOG

