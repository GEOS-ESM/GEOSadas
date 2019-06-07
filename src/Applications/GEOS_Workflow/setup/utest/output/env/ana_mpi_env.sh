export NCPUS=$NCPUS_GSI
export MPIRUN_ANA="mpiexec_mpt -np $NCPUS_GSI GSIsa.x"
export MPIRUN_SAC="mpiexec_mpt -np $NCPUS_SAC sac.x"
