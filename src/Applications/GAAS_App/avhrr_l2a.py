#!/usr/bin/env python
# -W ignore::DeprecationWarning

"""

  Simple wrapper script to parse Prep config file and create ODS with NNR AVHRR retrievals.
  
  October 2013
  arlindo.dasilva@nasa.gov
"""

from os       import system
from optparse import OptionParser
from MAPL     import Config

if __name__ == "__main__":
    
#   Parse command line options
#   --------------------------
    parser = OptionParser(usage="Usage: %prog prep_config_file nymd nhms",
                          version='avhrr_l2a-1.0.0' )
    parser.add_option("-n", "--dryrun",
                      action="store_true", dest="dryrun",
                      help="Dry run.")

    (options, args) = parser.parse_args()
    
    if len(args) == 3:
        prep_config, nymd, nhms = args
    else:
        parser.error("must have 3 arguments: prep_config_filename nymd nhms")

    # Parse prep config
    # -----------------
    cf = Config(prep_config,delim=' = ')

    Options =     " --expid=" + cf('AVHRR_L2A_EXPID')        + \
                 " --l2_dir=" + cf('AVHRR_L2A_L2_DIR')       + \
                    " --res=" + cf('AVHRR_L2A_RESOLUTION')   + \
                   "  --dir=" + cf('AVHRR_L2A_OUT_DIR')      + \
                  " --fname=" + cf('AVHRR_L2A_OUT_TEMPLATE') + \
                    " --net=" + cf('AVHRR_L2A_NN_FILE')      + \
                   " --wind=" + cf('AVHRR_L2A_WIND_FILE')    + \
                    " --tpw=" + cf('AVHRR_L2A_TPW_FILE')     + \
                    " --aod=" + cf('AVHRR_L2A_AOD_FILE')     + \
              " --blank_ods=" + cf('AVHRR_L2A_BLANK_ODS')    + \
                " --coxmunk=" + cf('AVHRR_L2A_COXMUNK_LUT') 

    if cf('AVHRR_L2A_OVERWRITE').upper() == 'YES': Options += " --force"
    if   cf('AVHRR_L2A_VERBOSE').upper() == 'YES': Options += " -v"
              
    # Generate products
    # -----------------
    i = 0
    for orbit in cf('AVHRR_L2A_ORBITS').split(','):
        cmd = "patmosx_l2a.py %s %s %s %s"%(Options,orbit,nymd,nhms)
        print cmd
        if not options.dryrun:
            if system(cmd):
                raise ValueError, "patmosx_l2a.py failed for %s on %s %s"%(orbit,nymd,nhms)

        i += 1
    

