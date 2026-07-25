# SLURM specials
setenv GEOSJEDI_QOS @GEOSJEDI_QOS
setenv GEOSJEDI_PARTITION  @GEOSJEDI_PARTITION

# Top options
setenv JEDI_SET    1           # bring bkg/obs/ens
setenv JEDI_RUN    1           # run JEDI var executable
setenv JEDI_HYBRID @JEDI_HYBRID           # control opts for hyb JEDI
setenv JEDI_MKIAU  1           # calculates IAU output
setenv JEDI_POST   0           # process results (move files, etc)
setenv JEDI_IAU_OVERWRITE  @JEDI_IAU_OVERWRITE   # overwrite GSI-IAU with JEDI-IAU (when cycling)
setenv JEDI_MKIAU_CUBED 0      # 0=use lat-lon ana/bkg; 1=use cubed ana/bkg (no remap)
setenv JEDI_RUN_ADANA_TEST 0   # run adjoint JEDI-Var
setenv JEDI_VAROFFSET 10800    # background time offset
setenv JEDI_FEEDBACK_VARBC @JEDI_FEEDBACK_VARBC   # controls whether or not to feedback biases
                                                  # caution: not sure aircraft bias cycle properly in JEDI

setenv JEDI_SWELLUSE 0  # bypass use of SWELL for now
setenv SWELL_INSTALL @SWELL_INSTALL
setenv OFFLINE_IODA_DIR @OFFLIODADIR # /discover/nobackup/projects/gmao/dadev/rtodling/archive/530/x0049/R2D2DataStore/Local/v2/

# Details ...
setenv MAPLFIX      0
setenv JEDI_OBS_OPT @JEDI_OBS_OPT  # 1= point to xexp-like set (data in tar-balls; data from existing exp)
                                   # 2= point to existing set of ncdiag-ioda-converted set (swell/.../DATE/geos_atmosphere)
                                   # 3= generate on the fly based on GSI (nc4) diags (TBD)

setenv JEDI_ROOT @JEDI_ROOT

# Specific to run procedure
setenv JEDI_RUN_ANA      1
setenv JEDI_RUN_CNVANA   0   # convert cc ana and/or inc output to ll
setenv JEDI_RUN_GETINC   @JEDI_RUN_GETINC   # calc cubed inc from diff of cubed ana and bkg
setenv JEDI_RUN_UPDRST   0   # not desirable

setenv JEDI_DIF_NCPUS    @JEDI_DIF_NCPUS
setenv JEDI_NCPUS        @JEDI_VAR_NCPUS

setenv JEDI_ADDINC_MPIRUN "mpirun -np 12"
setenv JEDI_CNVANA_MPIRUN "mpirun -np 12"
setenv JEDI_CNVENS_MPIRUN "mpirun -np 12"
setenv JEDI_CNVINC_MPIRUN "mpirun -np 12"
setenv JEDI_GETINC_MPIRUN "mpirun -np $JEDI_DIF_NCPUS"
setenv JEDI_FV3VAR_MPIRUN "mpirun -perhost @JEDI_VAR_PERHOST -np $JEDI_NCPUS"

setenv JEDI_MKIAU_MPIRUN "mpirun "

setenv JEDI_STATIC_FILES @JEDI_STATIC_FILES
setenv JEDI_CRTM_COEFFS  $FVHOME/fvInput/gsi/etc/JEDI-CRTM-2.4.1j1-GMAO-1/Little_Endian/
setenv JEDI_INPUT @JEDI_INPUT

# post
setenv JEDI_CONCAT_IODA  0
