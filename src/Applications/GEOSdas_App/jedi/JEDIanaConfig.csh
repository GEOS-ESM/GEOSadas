
# Top options
setenv JEDI_SET    1
setenv JEDI_RUN    1
setenv JEDI_HYBRID 1
setenv JEDI_MKIAU  1
setenv JEDI_POST   0
setenv JEDI_IAU_OVERWRITE  1   # overwrite IAU tendency from GSI with that from JEDI
setenv JEDI_RUN_ADANA_TEST 0
setenv JEDI_VAROFFSET 10800

setenv JEDI_SWELLUSE 0  # bypass use of SWELL for now
setenv OFFLINE_IODA_DIR /discover/nobackup/projects/gmao/dadev/rtodling/x0048/ioda
setenv OFFLINE_IODA_DIR /discover/nobackup/projects/gmao/dadev/rtodling/x0048/SwellExperiments/x0048-swell-convert_ncdiags/run/

# Details ...
setenv MAPLFIX      0
setenv JEDI_OBS_OPT 2  # 1= point to FP-like set
                       # 2= generate on the fly based on GSI (nc4) diags

setenv JEDI_GSI2IODA 1
setenv JEDI_OBS_DIR $FVWORK/IODA

setenv JEDI_ROOT $PRJ/JEDI1/Jun23/jedi12/build-intel-release
setenv JEDI_ROOT $PRJ/JEDI1/Aug23/jedi_bundle/build-intel-release
setenv JEDI_ROOT $PRJ/JEDI1/Aug23/31/jedi/build-intel-release

# Specific to run procedure
setenv JEDI_RUN_ANA      1
setenv JEDI_RUN_CNVANA   1   # convert cc ana output to ll
setenv JEDI_RUN_UPDRST   0   # not desirable

setenv JEDI_ADDINC_MPIRUN "mpirun -np 12"
setenv JEDI_CNVANA_MPIRUN "mpirun -np 12"
setenv JEDI_CNVENS_MPIRUN "mpirun -np 12"
setenv JEDI_CNVINC_MPIRUN "mpirun -np 12"
setenv JEDI_NCPUS 720
setenv JEDI_FV3VAR_MPIRUN "mpirun -perhost 8 -np $JEDI_NCPUS"
setenv JEDI_FV3VAR_MPIRUN "mpirun -np $JEDI_NCPUS"

setenv JEDI_MKIAU_MPIRUN "mpirun "

setenv JEDI_CRTM_COEFFS /discover/nobackup/projects/gmao/dadev/rtodling/JEDI/j48rt4/fvInput/gsi/etc/fix_ncep20221018/REL-2.4.0-jcsda/CRTM_Coeffs/Little_Endian/

# post
setenv JEDI_CONCAT_IODA  0
