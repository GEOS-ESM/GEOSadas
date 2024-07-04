
# Top options
setenv JEDI_SET    1
setenv JEDI_RUN    1
setenv JEDI_HYBRID 0
setenv JEDI_MKIAU  1
setenv JEDI_POST   0
setenv JEDI_IAU_OVERWRITE  0   # overwrite IAU tendency from GSI with that from JEDI
setenv JEDI_RUN_ADANA_TEST 0
setenv JEDI_VAROFFSET 10800
setenv JEDI_FEEDBACK_VARBC 1

setenv JEDI_SWELLUSE 0  # bypass use of SWELL for now
setenv OFFLINE_IODA_DIR /discover/nobackup/projects/gmao/dadev/rtodling/archive/530/x0049/R2D2DataStore/Local/v2/

# Details ...
setenv MAPLFIX      0
setenv JEDI_OBS_OPT 2  # 1= point to FP-like set (data in tar-balls)
                       # 2= point to FP-like set (data in separate files DATE/geos_atmosphere)
                       # 3= generate on the fly based on GSI (nc4) diags

setenv JEDI_GSI2IODA 0
setenv JEDI_OBS_DIR $FVWORK/IODA

setenv JEDI_ROOT $PRJ/JEDI1/Jun23/jedi12/build-intel-release
setenv JEDI_ROOT $PRJ/JEDI1/Aug23/jedi_bundle/build-intel-release
setenv JEDI_ROOT $PRJ/JEDI1/Aug23/31/jedi/build-intel-release
setenv JEDI_ROOT $PRJ/JEDI1/Oct10/jedi/build-intel-release
setenv JEDI_ROOT $PRJ/JEDI1/Jan31/jedi/build-intel-release
setenv JEDI_ROOT $PRJ/JEDI1/2024/Jun13/jedi/build-intel-release

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
