#!/bin/csh -xvf
#SBATCH --account=$GID
#SBATCH --partition=datamove
#SBATCH --time=1:00:00

cd $ACQWORK
setenv ACQDIR $FVHOME/run/jedi/Config
if ( -e $ACQDIR/jedi_vbc.acq.BOOTSTRAP ) then
  acquire -v -rc $ACQDIR/jedi_vbc.acq.BOOTSTRAP  -d $ACQWORK -s $FVHOME/spool   -ssh $NYMDP $NHMSP 060000 1
  /bin/mv $ACQDIR/jedi_vbc.acq.bootstrap $ACQDIR/jedi_vbc.acq.BOOTSTRAP.DONE
else
  acquire -v -rc $ACQDIR/jedi_vbc.acq  -d $ACQWORK -s $FVHOME/spool   -ssh $NYMDP $NHMSP 060000 1
endif
exit
