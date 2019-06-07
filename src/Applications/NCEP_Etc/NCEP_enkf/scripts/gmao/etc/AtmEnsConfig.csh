# Env Vars for Atmospheric Analysis Ensemble Mambo-Jambo
# ======================================================

#setenv REGRID_QOS advda
#setenv RSTEXT nc4
#setenv ATMENS_IGNORE_CHKPNT 1
#setenv ENSARCH_FIELDS "eana,ebkg,stat,ecbkg,eoi0,edia,ebaer,eaaer,eprg,eniana"

setenv IGNORE_0 1
setenv STRICT 0
unsetenv FCSTIMES 
setenv DORCORR 0

unsetenv PMI_RANK
unsetenv PMI_FD
unsetenv PMI_JOBID
unsetenv PMI_SIZE

# archiving
# ---------
setenv ENSARCH_ALLBKG 1   # set this and ALL bkg files are saved in tar ball
setenv ENSARCH_FIELDS "eana,ebkg,stat,ecbkg,eoi0,edia,ebaer,eaaer,erst,eprg,eniana"
setenv ENSARCH_WALLCLOCK 2:00:00
setenv ARCHLOC /archive/u/$user

# common to all
# -------------
setenv JOBMONITOR_MAXSLEEP_MIN 180
setenv ATMENS_DEBUG  1
setenv ATMENS_VERBOSE 1        # this can be put instead around each script call in atm_ens.j
setenv ATMENS4ARCH $FVHOME     # locations where dir with files to arch is held before arch
setenv ATMENSLOC   $FVHOME     # locations of recycled files for atmospheric ensemble
setenv ATMENSETC   $FVHOME/run/atmens
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

# atmens_recenter.csh
# -----------------
setenv AENSADDINFLOC addperts  # path relative to FVWORK
setenv AENS_ADDINFLATION 1     # apply additive inflation to each analysis member
setenv AENS_DONORECENTER 0     # do not recenter ensemble
setenv ADDINF_FACTOR 0.35      # additive inflation coeff (seems small after mass-div fix)
setenv ADDINF_FACTOR_SPPT 0.2  # additive inflation for when SPPT is used
#setenv RECENTER_WALLCLOCK 0:10:00
#setenv ENSRECENTER_NCPUS 2
#setenv RECENTER_QNAME nccs1    # this is an issue when submitting this to general 
setenv RECENTER_WALLCLOCK 1:00:00
setenv ENSRECENTER_NCPUS 4
setenv RECENTER_QNAME compute    # there is an issue when submitting this to general 
#setenv AENS_RECENTER_DSTJOB 4  # _RT commented out for testing (OPS robustness)

# ensemble GAAS and AERO EnKF
# ---------------------------
setenv MODIS_L2_HDF 1
setenv NCPUS_AOD   8
setenv MPIRUN_AOD  "mpiexec_mpt "
setenv AENS_GAAS_OPT   1  # 1 members use central GAAS
                          # 2 analyze each member with PSAS
                          # 3 do (2), add EnKF-based AOD analysis (off aod.or.concentrations)
                          # 4 EnKF-based AOD analysis (off aod.or.concentrations)
setenv ATMENKFAERO_QNAME compute
setenv ATMENKFAERO_WALLCLOCK 1:00:00
setenv AENKFAERO_NCPUS 192
setenv MPIRUN_ATMENKFAERO "mpiexec_mpt -np $AENKFAERO_NCPUS enkf_aero.x"

# atmos_ens2gcm.csh
# -----------------
#setenv AENS_IAU_DSTJOB 4
setenv IAU_QNAME compute
setenv IAU_WALLCLOCK 1:00:00
setenv ENSIAU_NCPUS 24
setenv MPIRUN_ENSIAU  "mpiexec_mpt -np $ENSIAU_NCPUS $IAUX"

# atmos_enkf.j
# ------------
setenv ATMENKF_QNAME compute
setenv ATMENKF_WALLCLOCK 1:00:00
#setenv ATMENKF_MPIPROCS 4
setenv AENKF_NCPUS 192 
setenv MPIRUN_ATMENKF "mpiexec_mpt -np $AENKF_NCPUS enkf_geos.x"

# gcm_ensemble.j
# --------------
setenv AENS_GCM_DSTJOB 4
setenv AGCM_QNAME compute
setenv AGCM_WALLCLOCK 1:00:00
setenv ENSGCM_NCPUS 48
setenv MPIRUN_ENSGCM  "mpiexec_mpt -np $ENSGCM_NCPUS GEOSgcm.x"   # esma_mpirun does not work in this context
setenv RSTSTAGE4AENS  $ATMENSLOC/atmens/RST                              # TBD: location of mean-fcst restarts


# adjgcm_ensemble.j
# -----------------
setenv AENS_GCMADJ_DSTJOB 1
setenv AGCMADJ_QNAME compute
setenv AGCMADJ_WALLCLOCK 1:00:00
setenv ENSGCMADJ_NCPUS 96
setenv MPIRUN_ENSGCMADJ "mpiexec_mpt -np $ENSGCMADJ_NCPUS GEOSgcmPert.x"   # esma_mpirun does not work in this context
setenv ADMRUN_OPT_BEGIN "mpiexec_mpt -np $ENSGCMADJ_NCPUS GEOSgcmPert.x"

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
setenv ENSGSI_NCPUS 32
setenv MPIRUN_ENSANA  "mpiexec_mpt -np $ENSGSI_NCPUS GSIsa.x"     # esma_mpirun does not work in this context

# setup_perts.csh
#----------------
#setenv AENS_PERTS_DSTJOB 4  !_RT commented out for test OPS robustness test
setenv PERTS_QNAME compute
setenv PERTS_WALLCLOCK 1:00:00
setenv PERTS_NCPUS 1

# pert-energy calculation
#------------------------
setenv AENSTAT_NCPUS 4
setenv AENSTAT_MPIRUN "mpiexec_mpt -np $AENSTAT_NCPUS mp_stats.x"
setenv AENSTAT_WALLCLOCK 0:30:00
setenv AENSTAT_QNAME compute

# post-egcm calculations
# ----------------------
setenv PEGCM_NCPUS 4
setenv PEGCM_WALLCLOCK 1:00:00
setenv PEGCM_QNAME compute


# NOTES:

