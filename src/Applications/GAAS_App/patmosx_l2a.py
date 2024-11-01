#!/usr/bin/env python3
# -W ignore::DeprecationWarning

"""
  A Python script to create AVHRR Neural Net Retrievals, 

  This utility reads AVHRR Level2 files (or its NPZ conterparts) and
  creates an ODS file with the retrieve 550 nm AOD, as well as a
  *gritas* type gridded output.
  
  October 2013
  arlindo.dasilva@nasa.gov
"""

import warnings
warnings.simplefilter('ignore',DeprecationWarning)
warnings.simplefilter('always',UserWarning)

import os
import sys

from time         import clock
from optparse     import OptionParser   # Command-line args  
from datetime     import datetime
from numpy        import load

from pyobs        import NPZ
from avhrr_nnr    import AVHRR_NNR
from MAPL         import strTemplate

prod = 'patmosx_v05r02' # baseline product hardwired for now
              
#---------------------------------------------------------------------
def makethis_dir(filename):
    """Creates the relevant directory if necessary."""
    path, filen = os.path.split(filename)
    if path != '':
        rc = os.system('mkdir -p '+path)
        if rc:
            raise IOError("could not create directory "+path)

def patmosx_has_obs(fname):
    """
    Returns true if PATMOSX reflectance file has something in it.
    """
    return 'latitude' in list(load(fname).keys())

#---------------------------------------------------------------------

if __name__ == "__main__":

    expid = 'nnr_001'
    
#   Defaults may be platform dependent
#   ----------------------------------
    if os.path.exists('/nobackup/AVHRR/Level2/'): # New calculon
        l2_path = '/nobackup/AVHRR/Level2/Synoptic'
        out_dir = '/nobackup/AVHRR/Level%lev/NNR/Y%y4/M%m2'
        blank_ods = '/nobackup/NNR/Misc/blank.ods'
        coxmunk_lut = '/nobackup/NNR/Misc/avhrr.cox-munk_lut.npz'

        MAerDir = '/nobackup/MERRAero'
        MDir = '/nobackup/MERRA'
        nn_file = '/nobackup/NNR/Net/'+expid+'.avhrr_Tau.net'

    else: # Must be somewhere else, no good defaults
        out_dir      = './'
        l2_path = './'
        blank_ods = 'blank.ods'
        coxmunk_lut = 'cox-munk_lut.npz'
        MAerDir = './'
        MDir = './'
        nn_file = expid+'.avhrr_Tau.net'

    aod_file  = MAerDir + '/inst2d_hwl_x.ddf'
    tpw_file  = MDir + '/int_Nx'
    wind_file = MDir + '/slv_Nx'

    out_tmpl = '%s.%prod_l%leva.%orb.%y4%m2%d2_%h2%n2z.%ext'
    res = 'e'
    
#   Parse command line options
#   --------------------------
    parser = OptionParser(usage="Usage: %prog [options] asc|des nymd nhms",
                          version='patmosx_l2a-1.0.0' )

    parser.add_option("-x", "--expid", dest="expid", default=expid,
                      help="Experiment id (default=%s)"\
                           %expid )

    parser.add_option("-d", "--dir", dest="out_dir", default=out_dir,
                      help="Output directory (default=%s)"\
                           %out_dir )

    parser.add_option("-B", "--blank_ods", dest="blank_ods", default=blank_ods,
                      help="Blank ODS file name for fillers  (default=%s)"\
                           %blank_ods )

    parser.add_option("-o", "--fname", dest="out_tmpl", default=out_tmpl,
                      help="Output file name template (default=%s); ODS file name will be derived from it by changing extension to '.ods' and replacing 'Level3' with 'Level2'."\
                           %out_tmpl )

    parser.add_option("-L", "--l2_dir", dest="l2_path", default=l2_path,
                      help="Top directory for AVHRR Level 2 files (default=%s)"\
                           %l2_path )
    parser.add_option("-M", "--coxmunk", dest="coxmunk_lut", default=coxmunk_lut,
                      help="File name for Cox-Munk LUT (default=%s)"\
                           %coxmunk_lut )

    parser.add_option("-N", "--net", dest="nn_file", default=nn_file,
                      help="Neural net file template  (default=%s)"\
                           %nn_file )

    parser.add_option("-A", "--aod", dest="aod_file", default=aod_file,
                      help="Gridded speciated AOD file (default=%s)"\
                           %aod_file )

    parser.add_option("-T", "--tpw", dest="tpw_file", default=tpw_file,
                      help="Gridded TPW file (default=%s)"\
                           %tpw_file )

    parser.add_option("-W", "--wind", dest="wind_file", default=wind_file,
                      help="Gridded Wind file (default=%s)"\
                           %wind_file )

    parser.add_option("-r", "--res", dest="res", default=res,
                      help="Resolution for gridded output (default=%s)"\
                           %out_tmpl )

    parser.add_option("-u", "--uncompressed",
                      action="store_true", dest="uncompressed",
                      help="Do not use n4zip to compress gridded/ODS output file (default=False)")

    parser.add_option("-F", "--force",
                      action="store_true", dest="force",
                      help="Overwrites output file")

    parser.add_option("-v", "--verbose",
                      action="store_true", dest="verbose",
                      help="Turn on verbosity.")

    (options, args) = parser.parse_args()
    
    if len(args) == 3:
        orb, nymd, nhms = args
    else:
        parser.error("must have 3 arguments: orbit nymd nhms")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        
    if options.verbose:
        print("")
        print("                          AVHRR NNR Retrievals")
        print("                          --------------------")
        print("")
        t0 = clock()

#   Time variables
#   --------------
    nymd_, nhms_ = ( int(nymd), int(nhms) )
    year, month, day = (nymd_/10000, (nymd_%10000)/100, nymd_%100)
    hour = nhms_/10000
    syn_tyme = datetime(year,month,day,hour,0,0) # no minute, second
            
#   Form output gridded file name
#   -----------------------------
    out_tmpl = options.out_dir+'/'+options.out_tmpl
    out_tmpl = out_tmpl.replace('%prod',prod).replace('%orb',orb).\
                        replace('%lev','3').replace('%ext','nc4')
    out_file = strTemplate(out_tmpl,expid=options.expid,nymd=nymd,nhms=nhms)
    name, ext = os.path.splitext(out_file)

#   Form ODS file name
#   ------------------
    ods_tmpl = options.out_dir+'/'+options.out_tmpl
    ods_tmpl = ods_tmpl.replace('%prod',prod).replace('%orb',orb).\
                        replace('%lev','2').replace('%ext','ods')
    ods_file = strTemplate(ods_tmpl,expid=options.expid,nymd=nymd,nhms=nhms)
    if os.path.exists(ods_file) and (options.force is not True):
        print("avhrr_l3a: Output ODS file <%s> exists --- cannot proceed."%ods_file)
        raise IOError("Specify --force to overwrite existing output file.")

#   Produce Level 2 AVHRR Aerosol Retrievals
#   ----------------------------------------
    if options.verbose:
        print("Retrieving AVHRR AOD from %s (%sing) on "%(prod,orb),syn_tyme)

    t = syn_tyme
    patmosx = options.l2_path+'/Y%d/M%02d/D%02d/%s.%s.%d%02d%02d_%02dz.npz'%\
                             (t.year,t.month,t.day,prod,orb,t.year,t.month,t.day,t.hour)

    options.tpw_file = strTemplate(options.tpw_file,nymd=nymd,nhms=nhms)
    options.aod_file = strTemplate(options.aod_file,nymd=nymd,nhms=nhms)
    options.wind_file = strTemplate(options.wind_file,nymd=nymd,nhms=nhms)

    # Special handle empty files
    # --------------------------
    if patmosx_has_obs(patmosx):
        a = AVHRR_NNR(patmosx, Verb=options.verbose)
    else:
        a = NPZ(patmosx)
        
    if a.nobs < 1:
        if options.verbose:
            print('WARNING: no observation for this time in file <%s>'%ods_file)
    
    # elif any(a.iValid) == False:
    elif len(a.tyme[a.iValid]) < 2:
        if options.verbose:
            print('WARNING: not enough GOOD observation for this time in file <%s>'%ods_file)
        a.nobs = 0

    else:
        
        # Attach state dependent metadata
        # -------------------------------
        a.speciate(options.aod_file)
        a.sampleG5(int_x=options.tpw_file,slv_x=options.wind_file)
        a.getCoxMunk(options.coxmunk_lut) # oceanic albedo

        # Evaluate Neural Net
        # -------------------
        a.apply(options.nn_file) 

#   Write ODS
#   ---------
    if os.path.exists(ods_file) and options.force:
        os.remove(ods_file) # remove ODS file
    makethis_dir(ods_file)
    if a.nobs>0:
        a.writeODS(syn_tyme,ods_file,doNNR=True)
    else:
        if os.system('ods_blank.x %s %s %s %s'%(options.blank_ods,nymd,nhms,ods_file)):
            warnings.warn('cannot create empty output file <%s>'%ods_file)
        else:
            if options.verbose:
                print("[w] Wrote empty ODS file "+ods_file)

#   Write gridded output file
#   -------------------------
    if os.path.exists(out_file) and options.force:
        os.remove(out_file) # remove gridded file
    makethis_dir(out_file)
    if a.nobs>0:
        a.writeGridded(syn_tyme,filename=out_file,res=options.res,doNNR=True,doFilter=False)

#   Compress nc output unless the user disabled it
#   ----------------------------------------------
    if a.nobs>0:
        if not options.uncompressed:
            if os.system("n4zip "+out_file):
                warnings.warn('cannot compress output file <%s>'%out_file)
            if os.system("n4zip "+ods_file):
                warnings.warn('cannot compress output ods file <%s>'%ods_file)
