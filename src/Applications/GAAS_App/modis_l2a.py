#!/usr/bin/env python
# -W ignore::DeprecationWarning

"""

  Simple wrapper script to parse Prep config file and create ODS with MODIS NNR aerosol retrievals.
  
  February 2011.
  arlindo.dasilva@nasa.gov
"""

from os       import system
from optparse import OptionParser
from MAPL     import Config

if __name__ == "__main__":
    
#   Parse command line options
#   --------------------------
    parser = OptionParser(usage="Usage: %prog prep_config_file isotime",
                          version='modis_l2a-1.0.0' )
    parser.add_option("-n", "--dryrun",
                      action="store_true", dest="dryrun",
                      help="Dry run.")

    (options, args) = parser.parse_args()
    
    if len(args) == 2:
        prep_config, isotime = args
    else:
        parser.error("must have 2 arguments: prep_config_filename isotime")

    # Parse prep config
    # -----------------
    cf = Config(prep_config,delim=' = ')
 
    Options =     " --expid=" + cf('MODIS_L2A_EXPID')        + \
                 " --l2_dir=" + cf('MODIS_L2A_L2_DIR')       + \
                    " --res=" + cf('MODIS_L2A_RESOLUTION')   + \
                   "  --dir=" + cf('MODIS_L2A_OUT_DIR')      + \
                  " --fname=" + cf('MODIS_L2A_OUT_TEMPLATE') + \
                    " --net=" + cf('MODIS_L2A_NN_FILE')      + \
                 " --aer_x="  + cf('MODIS_L2A_AER_X')  + \
              " --blank_ods=" + cf('MODIS_L2A_BLANK_ODS')   

    if cf('MODIS_L2A_OVERWRITE').upper() == 'YES': Options += " --force"
    if   cf('MODIS_L2A_VERBOSE').upper() == 'YES': Options += " -v"
              
    # Generate products
    # -----------------
    i = 0
    Coll = cf('MODIS_L2A_COLLECTION').split(',')
    for ident in cf('MODIS_L2A_IDENTS').split(','):
        coll = Coll[i] 
        cmd = "mxd04_l2a.py %s --collection=%s %s %s "%(Options,Coll[i],ident,isotime)
        print cmd
        if not options.dryrun:
            if system(cmd):
                raise ValueError, "mxd04_l2a.py failed for %s on %s "%(ident,isotime)

        i += 1
    

