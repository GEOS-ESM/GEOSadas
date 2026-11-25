# SWELL_INSTALL (JEDIanaConfig.csh, JEDIatmensConfig.csh) is a python venv with swell
# installed in it. The ADAS (GEOSpyD) python settings break the venv; swell detects
# the NCCS platform from HOSTNAME.
if ( $?PYTHONPATH ) unsetenv PYTHONPATH
if ( $?PYTHONHOME ) unsetenv PYTHONHOME
if ( ! $?HOSTNAME ) setenv HOSTNAME `hostname`
source $SWELL_INSTALL/bin/activate.csh
