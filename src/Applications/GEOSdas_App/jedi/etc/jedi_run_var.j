#!/bin/csh -fx
# ------------------------------
#SBATCH --account=g0613
#SBATCH --qos=dastest
#SBATCH --partition=preops
#SBATCH --job-name=jedivar
#SBATCH --output=jedivar.log.o%j.txt
#_SBATCH --ntasks=240
#SBATCH --nodes=10
#SBATCH --constraint=mil
#SBATCH --time=2:00:00

source $JEDI_ROOT/modules

cd $JEDIWORK

$JEDI_FV3VAR_MPIRUN $JEDIBUILD/bin/fv3jedi_var.x $MYCONF |& tee -a $FVWORK/$JEDIVARLOG

