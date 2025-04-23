
# Top options
setenv JEDI_SET    1           # bring bkg/obs/ens
setenv JEDI_RUN    1           # run JEDI var executable
setenv JEDI_HYBRID 0           # control opts for hyb JEDI
setenv JEDI_MKIAU  1           # calculates IAU output
setenv JEDI_POST   0           # process results (move files, etc)
setenv JEDI_IAU_OVERWRITE  0   # overwrite GSI-IAU with JEDI-IAU (when cycling)
setenv JEDI_RUN_ADANA_TEST 0   # run adjoint JEDI-Var
setenv JEDI_VAROFFSET 10800    # background time offset
setenv JEDI_FEEDBACK_VARBC 1   # controls whether or not to feedback biases

setenv JEDI_SWELLUSE 0  # bypass use of SWELL for now
setenv OFFLINE_IODA_DIR @OFFLIODADIR # /discover/nobackup/projects/gmao/dadev/rtodling/archive/530/x0049/R2D2DataStore/Local/v2/

# Details ...
setenv MAPLFIX      0
setenv JEDI_OBS_OPT 2  # 1= point to FP-like set (data in tar-balls)
                       # 2= point to FP-like set (data in separate files DATE/geos_atmosphere)
                       # 3= generate on the fly based on GSI (nc4) diags

setenv JEDI_GSI2IODA 0
setenv JEDI_OBS_DIR $FVWORK/IODA

setenv JEDI_ROOT @JEDI_ROOT

# Specific to run procedure
setenv JEDI_RUN_ANA      1
setenv JEDI_RUN_CNVANA   1   # convert cc ana output to ll
setenv JEDI_RUN_UPDRST   0   # not desirable

setenv JEDI_ADDINC_MPIRUN "mpirun -np 12"
setenv JEDI_CNVANA_MPIRUN "mpirun -np 12"
setenv JEDI_CNVENS_MPIRUN "mpirun -np 12"
setenv JEDI_CNVINC_MPIRUN "mpirun -np 12"
setenv JEDI_NCPUS 48
setenv JEDI_NCPUS 216
setenv JEDI_NCPUS 252
setenv JEDI_FV3VAR_MPIRUN "mpirun -perhost 8 -np $JEDI_NCPUS"
setenv JEDI_FV3VAR_MPIRUN "mpirun            -np $JEDI_NCPUS"

setenv JEDI_MKIAU_MPIRUN "mpirun "

setenv JEDI_CRTM_COEFFS /discover/nobackup/projects/gmao/dadev/rtodling/JEDI/x49/j49rt00/fvInput/gsi/etc/r21c_ncep20221018/Little_Endian/

# post
setenv JEDI_CONCAT_IODA  0
