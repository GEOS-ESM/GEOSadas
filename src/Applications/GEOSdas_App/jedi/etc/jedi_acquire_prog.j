#!/bin/csh -xvf
#SBATCH --account=$GID
#SBATCH --partition=datamove
#SBATCH --time=1:00:00

cd $ACQWORK
acquire -v -rc $ACQWORK/jedi_prog.acq  -d $ACQWORK -s $FVHOME/spool   -ssh $NYMD $NHMS 060000 1
exit
