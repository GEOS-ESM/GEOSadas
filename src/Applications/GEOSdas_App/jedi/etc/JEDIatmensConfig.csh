# This is used to either defined JEDI-ATMENS-specific settings or
# override env definitions from JEDIanaConfig
# SLURM specials

# Top options
setenv JEDI_RUN_ANA    0       # make sure turn off central ADAS analysis ...
setenv JEDI_RUN_EAANA  1       # and turn on running of ensemble analysis
#setenv JEDI_MKIAU  1           # calculates IAU output
#setenv JEDI_MKIAU_CUBED 0      # 0=use lat-lon ana/bkg; 1=use cubed ana/bkg (no remap)

# Details ...
setenv JEDI_OBS_OPT 1          # 1= point to xexp-like set (data in tar-balls; data from existing exp)
                               # 2= point to existing set of ncdiag-ioda-converted set (swell/.../DATE/geos_atmosphere)
                               # 3= generate on the fly based on GSI (nc4) diags (TBD)

setenv SWELL_INSTALL @SWELL_INSTALL
setenv JEDI_ROOT @JEDI_ROOT
setenv JEDI_SRC @JEDI_SRC


