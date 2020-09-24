# Env Vars for Atmospheric Analysis Ensemble Mambo-Jambo
# ======================================================

#setenv REGRID_QOS advda
#setenv ATMENS_IGNORE_CHKPNT 1
setenv RSTEXT nc4

setenv LOCAL_ACQUIRE 1
setenv IGNORE_0 1
setenv STRICT 0
unsetenv FCSTIMES 
setenv DORCORR 0

unsetenv PMI_RANK
unsetenv PMI_FD
unsetenv PMI_JOBID
unsetenv PMI_SIZE

setenv ATMENS_BATCHSUB "sbatch"

# Turn online (variational) bias correction on by default
setenv PREPQC   0  # The ensemble obsever never runs PREPQC (gets inputs from central instead)
setenv ACFTBIAS @ACFTBIAS  # 0: no aircraft bias correction
                   # 1: MERRA2 style (offline correction)
                   # 2: 5_25_2 onward (online var correction)

# archiving
# ---------
setenv ENSARCH_ALLBKG 1   # set this and ALL bkg files are saved in tar ball
setenv ENSARCH_FIELDS "eana,ebkg,ecbkg,edia,ebaer,erst,eprg"
setenv ENSARCH_WALLCLOCK 2:00:00
setenv ARCHLOC $FVARCH

# common to all
# -------------
setenv JOBMONITOR_MAXSLEEP_MIN 180
setenv ATMENS_DEBUG  1
setenv ATMENS_VERBOSE 1               # this can be put instead around each script call in atm_ens.j
setenv ATMENS4ARCH $FVHOME            # location where dir with files to arch is held before arch
setenv ATMENSLOC   $FVHOME/atmose     # location of recycled files for atmospheric ensemble
setenv ATMENSETC   $FVHOME/run/atmose # location of RC files
#etenv TIMEINC 360 # analysis frequency in minutes (script not general enough for this to be anything)
#etenv ASYNBKG 180 # background frequency in minutes (script not general enough for this to be anything)
#etenv VAROFFSET 180   # abs value of time off from 1st synoptic hour of var window
setenv JOBGEN_NCPUS_PER_NODE 16
setenv ENSPARALLEL 2           # 0 - does serial ensemble
                               # Otherwise ensemle runs in parallel mode
                               # and in this case, the satellite bias
                               # correction files can be treated in  
                               # either of the following two ways:
                               # 1 - bias estimates from current hybrid cycle 
                               # 2 - bias estimates from previous hybrid cycle
setenv ATMENS_DO4DIAU 0
setenv ENSOBSVR -1             # bias correction files expected to be available for each member and control

# ensemble GAAS and AERO EnKF
# ---------------------------
setenv MODIS_L2_HDF 1
setenv NCPUS_AOD   8
setenv MPIRUN_AOD  "$ATMENS_MPIRUN "
setenv AENS_GAAS_OPT   2  # 1 members use central GAAS
                          # 2 analyze each member with PSAS
                          # 3 do (2), add EnKF-based AOD analysis (off aod.or.concentrations)
                          # 4 EnKF-based AOD analysis (off aod.or.concentrations)
setenv ATMENKFAERO_QNAME compute
setenv ATMENKFAERO_WALLCLOCK 1:00:00
setenv AENKFAERO_NCPUS 192
setenv MPIRUN_ATMENKFAERO "$ATMENS_MPIRUN -np $AENKFAERO_NCPUS enkf_aero.x"

# atmos_ens2gcm.csh
# -----------------
setenv AENS_IAU_DSTJOB 8
setenv IAU_QNAME compute
setenv IAU_WALLCLOCK 1:00:00
setenv ENSIAU_NCPUS @MIAU_CPUS
setenv MPIRUN_ENSIAU  "$ATMENS_MPIRUN -np $ENSIAU_NCPUS $IAUX"

# atmos_enkf.j
# ------------
setenv ATMENKF_QNAME compute
setenv ATMENKF_WALLCLOCK 1:00:00
#setenv ATMENKF_MPIPROCS 4
setenv AENKF_NCPUS @ENKF_CPUS
setenv MPIRUN_ATMENKF "$ATMENS_MPIRUN -np $AENKF_NCPUS enkf_geos.x"

# gcm_ensemble.j
# --------------
setenv AENS_GCM_DSTJOB 4
setenv AGCM_QNAME compute
setenv AGCM_WALLCLOCK 1:00:00
setenv ENSGCM_NCPUS @AGCM_CPUS
setenv MPIRUN_ENSGCM  "$ATMENS_MPIRUN -np $ENSGCM_NCPUS GEOSgcm.x"   # esma_mpirun does not work in this context
setenv RSTSTAGE4AENS  $ATMENSLOC/RST                                 # TBD: location of mean-fcst restarts


# adjgcm_ensemble.j
# -----------------
#setenv AENS_GCMADJ_DSTJOB 4
setenv AGCMADJ_QNAME compute
setenv AGCMADJ_WALLCLOCK 1:00:00
setenv ENSGCMADJ_NCPUS 96
setenv MPIRUN_ENSGCMADJ "$ATMENS_MPIRUN -np $ENSGCMADJ_NCPUS GEOSgcmPert.x"   # esma_mpirun does not work in this context
setenv ADMRUN_OPT_BEGIN "$ATMENS_MPIRUN -np $ENSGCMADJ_NCPUS GEOSgcmPert.x"

# vortex tracker
# --------------
setenv ENSVTRK_NCPUS 1
setenv VTRKFRQF 360
setenv VTXLEVS "1000 925 850 700 600 500 400 300 250 200 150 100 70 50 30 20 10"

# obsvr_ensemble.j
# ----------------
setenv AENS_OBSVR_DSTJOB 4
setenv OBSVR_QNAME compute
setenv OBSVR_WALLCLOCK 0:45:00
setenv ENSGSI_NCPUS @OBSV_CPUS
setenv MPIRUN_ENSANA  "$ATMENS_MPIRUN -np $ENSGSI_NCPUS GSIsa.x"     # esma_mpirun does not work in this context

# setup_perts.csh
#----------------
setenv AENS_PERTS_DSTJOB 8
setenv PERTS_QNAME compute
setenv PERTS_WALLCLOCK 1:00:00
setenv PERTS_NCPUS 24 
setenv PERTS_ENSTAT_MPIRUN "$ATMENS_MPIRUN -np $PERTS_NCPUS mp_stats.x"

# pert-energy calculation
#------------------------
setenv AENSTAT_NCPUS @STAT_CPUS
setenv AENSTAT_MPIRUN "$ATMENS_MPIRUN -np $AENSTAT_NCPUS mp_stats.x"
setenv AENSTAT_WALLCLOCK 0:30:00
setenv AENSTAT_QNAME compute

# post-egcm calculations
# ----------------------
setenv PEGCM_NCPUS @STAT_CPUS
setenv PEGCM_WALLCLOCK 1:00:00
setenv PEGCM_QNAME compute


# NOTES:
