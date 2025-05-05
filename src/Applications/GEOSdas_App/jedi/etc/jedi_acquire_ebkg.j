#!/bin/csh -xvf
#SBATCH --account=$GID
#SBATCH --partition=datamove
#SBATCH --time=1:00:00

cd $ACQWORK
if ( $JEDI_HYBRID == 1 ) then
  acquire -v -rc $JEDIETC/jedi_ebkg.acq  -d $ACQWORK -s $FVHOME/spool   -ssh $NYMD $NHMS  060000 1
else
  acquire -v -rc $JEDIETC/jedi_ebkgx.acq  -d $ACQWORK -s $FVHOME/spool  -ssh $NYMD $NHMS  060000 1
endif
exit
