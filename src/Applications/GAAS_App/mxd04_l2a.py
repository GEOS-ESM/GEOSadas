#!/usr/bin/env python3
# -W ignore::DeprecationWarning

"""
  A Python script to create NNR retrievals.
  It now uses class MXD04 to directly read MODIS Aerosol Level 2 
  Files (MOD04/MYD04).

  This utility reads MODIS Level2 files and creates an ODS file with
  NNR retrievals, as well as a *gritas* type gridded output.
  
  February 2011, revised Novembre 2016 for MODIS Collection 6.
  arlindo.dasilva@nasa.gov
"""

import warnings
warnings.simplefilter('ignore',DeprecationWarning)
warnings.simplefilter('always',UserWarning)

import os
import sys
import subprocess

from optparse        import OptionParser   # Command-line args  
from dateutil.parser import parse as isoparse
from mxd04_nnr       import MxD04_NNR
from MAPL.config     import strTemplate

Ident = dict( modo = ('MOD04','ocean'),
              modl = ('MOD04','land'),
              modd = ('MOD04','deep'),
              mydo = ('MYD04','ocean'),
              mydl = ('MYD04','land'),
              mydd = ('MYD04','deep')
            )
              
#---------------------------------------------------------------------
def makethis_dir(filename):
    """Creates the relevant directory if necessary."""
    path, filen = os.path.split(filename)
    if path != '':
        rc = os.system('mkdir -p '+path)
        if rc:
            raise IOError("could not create directory "+path)
        
#---------------------------------------------------------------------
def wavs_callback(option, opt, value, parser):
    setattr(parser.values, option.dest, value.split(','))
if __name__ == "__main__":

    expid = 'nnr3'
    ident = 'modo'
    
#   Defaults may be platform dependent
#   ----------------------------------
    if os.path.exists('/nobackup/MODIS/Level2/'): # New calculon
        l2_path = '/nobackup/MODIS/Level2/'
        out_dir = '/nobackup/NNR/%coll/Level%lev/%prod/Y%y4/M%m2'
        nn_file = '/nobackup/NNR/Net/nnr_003.%ident_Tau.net'
        blank_ods = '/nobackup/NNR/Misc/blank.ods'
        aer_x   = '/nobackup/NNR/Misc/tavg1_2d_aer_Nx'
        slv_x   = '/nobackup/NNR/Misc/tavg1_2d_slv_Nx'
    else: # Must be somewhere else, no good defaults
        out_dir      = './'
        l2_path = './'
        nn_file = '%ident_Tau.net'
        blank_ods = 'blank.ods'
        aer_x   = 'tavg1_2d_aer_Nx'        
        slv_x   = 'tavg1_2d_slv_Nx'

    out_tmpl = '%s.%prod_l%leva.%algo.%y4%m2%d2_%h2%n2z.%ext'
    coll = '006'
    res = 'c'
    nsyn = 8
    aodmax = 2.0
    cloud_thresh = 0.7
    cloudFree = None
    aodSTD = 3.0
    aodLength = 0.5
    wavs = '440,470,550,660,870'
    
#   Parse command line options
#   --------------------------
    parser = OptionParser(usage="Usage: %prog [options] ident isotime",
                          version='mxd04_l2a-1.0.0' )


    parser.add_option("-x", "--expid", dest="expid", default=expid,
                      help="Experiment id (default=%s)"\
                           %expid )

    parser.add_option("-d", "--dir", dest="out_dir", default=out_dir,
                      help="Output directory (default=%s)"\
                           %out_dir )

    parser.add_option("-A", "--aer_x", dest="aer_x", default=aer_x,
                      help="GrADS ctl for speciated AOD file (default=%s)"\
                           %aer_x )

    parser.add_option("-S", "--slv_x", dest="slv_x", default=slv_x,
                      help="GrADS ctl for column absorbers file (default=%s)"\
                           %slv_x )    

    parser.add_option("--nsyn", dest="nsyn", default=nsyn,type="int",
                      help="Number of synoptic times (default=%d)"\
                           %nsyn )

    parser.add_option("-B", "--blank_ods", dest="blank_ods", default=blank_ods,
                      help="Blank ODS file name for fillers  (default=%s)"\
                           %blank_ods )

    parser.add_option("-C", "--collection", dest="coll", default=coll,
                      help="MODIS collection (default=%s)"\
                           %coll )

    parser.add_option("-o", "--fname", dest="out_tmpl", default=out_tmpl,
                      help="Output file name template (default=%s); ODS file name will be derived from it by changing extension to '.ods' and replacing 'Level3' with 'Level2'."\
                           %out_tmpl )

    parser.add_option("-L", "--l2_dir", dest="l2_path", default=l2_path,
                      help="Top directory for MODIS Level 2 files (default=%s)"\
                           %l2_path )

    parser.add_option("-N", "--net", dest="nn_file", default=nn_file,
                      help="Neural net file template  (default=%s)"\
                           %nn_file )

    parser.add_option("-r", "--res", dest="res", default=res,
                      help="Resolution for gridded output (default=%s)"\
                           %res )

    parser.add_option("--cloud_thresh", dest="cloud_thresh", default=cloud_thresh,type='float',
                      help="Cloud fractions threshhold for good data (default=%f)"\
                           %cloud_thresh )

    parser.add_option("--cloudFree", dest="cloudFree", default=cloudFree,
                      help="Extra check for cloudiness when high AOD values are predicted. If not provided, no check is performed. (default=%s)"\
                           %cloudFree )

    parser.add_option("--aodmax", dest="aodmax", default=aodmax,type='float',
                      help="max AOD value that will be accepted when cloud fraction is greater than cloudFree (default=%f)"\
                           %aodmax )

    parser.add_option("--aodSTD", dest="aodSTD", default=aodSTD,type='float',
                      help="number of standard deviations to check for AOD outliers (default=%f)"\
                           %aodSTD )

    parser.add_option("--aodLength", dest="aodLength", default=aodLength,type='float',
                      help="length scale (degrees) to check for AOD outliers (default=%f)"\
                           %aodLength )

    parser.add_option("--wavs", dest="wavs", default=wavs,type='string',action='callback',callback=wavs_callback,
                      help="wavelength to output AOD from predicted Angstrom Exponent (default=%s)"\
                           %wavs )
        
    parser.add_option("-u", "--uncompressed",
                      action="store_true", dest="uncompressed",default=False,
                      help="Do not use n4zip to compress gridded/ODS output file (default=False)")

    parser.add_option("-F", "--force",
                      action="store_true", dest="force",default=False,
                      help="Overwrites output file")

    parser.add_option("-v", "--verbose",
                      action="store_true", dest="verbose",default=False,
                      help="Turn on verbosity.")

    (options, args) = parser.parse_args()
    
    if len(args) == 2:
        ident, isotime = args
        prod, algo = Ident[ident]
    else:
        parser.error("must have 3 arguments: ident, date and time")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        
    if options.verbose:
        print("")
        print("                          MODIS Level 2A Processing")
        print("                          -------------------------")
        print("")

#   Time variables
#   --------------
    syn_time = isoparse(isotime)
    nymd     = str(syn_time.date()).replace('-','')
    nhms     = str(syn_time.time()).replace(':','')
            
#   Form output gridded file name
#   -----------------------------
    out_tmpl = options.out_dir+'/'+options.out_tmpl
    out_tmpl = out_tmpl.replace('%coll',options.coll).replace('%prod',prod).replace('%algo',algo).replace('%lev','3').replace('%ext','nc4')
    out_file = strTemplate(out_tmpl,expid=options.expid,nymd=nymd,nhms=nhms)
    name, ext = os.path.splitext(out_file)
    if os.path.exists(out_file) and (options.force is not True):
        print("mxd04_l2a: Output Gridded file <%s> exists --- cannot proceed."%out_file)
        raise IOError("Specify --force to overwrite existing output file.")    
    if os.path.exists(out_file) and options.force:
        os.remove(out_file)    

#   Form ODS file name
#   ------------------
    ods_tmpl = options.out_dir+'/'+options.out_tmpl
    ods_tmpl = ods_tmpl.replace('%coll',options.coll).replace('%prod',prod).replace('%algo',algo).replace('%lev','2').replace('%ext','ods')
    ods_file = strTemplate(ods_tmpl,expid=options.expid,nymd=nymd,nhms=nhms)
    if os.path.exists(ods_file) and (options.force is not True):
        print("mxd04_l2a: Output ODS file <%s> exists --- cannot proceed."%ods_file)
        raise IOError("Specify --force to overwrite existing output file.")
    if os.path.exists(ods_file) and options.force:
        os.remove(ods_file)

#   Aerosol composition file name
#   -----------------------------
    if options.aer_x[-3:] == 'nc4':
      aer_x = strTemplate(options.aer_x,expid=options.expid,nymd=nymd,nhms=nhms)
    else:
      aer_x = options.aer_x

#   Column absorbers
#   ----------------
    if options.slv_x[-3:] == 'nc4':
      slv_x = strTemplate(options.slv_x,expid=options.expid,nymd=nymd,nhms=nhms)
    else:
      slv_x = options.slv_x
        
#   MODIS Level 2 NNR Aerosol Retrievals
#   ------------------------------------
    if options.verbose:
        print("NNR Retrieving %s %s on "%(prod,algo.upper()),syn_time)

    if options.cloudFree == 'None':
        options.cloudFree = None
    elif options.cloudFree == None:
        pass
    else:
        options.cloudFree = float(options.cloudFree)

    modis = MxD04_NNR(options.l2_path,prod,algo.upper(),syn_time,aer_x,slv_x,
                      coll=options.coll,
                      cloud_thresh=options.cloud_thresh,
                      cloudFree = options.cloudFree,
                      aodmax = options.aodmax,
                      aodSTD = options.aodSTD,
                      aodLength = options.aodLength,
                      wavs = options.wavs,
                      nsyn=options.nsyn,
                      verbose=options.verbose)
    if modis.nobs < 1:
        if options.verbose:
            print('WARNING: no observation for this time in file <%s>'%ods_file)
    
    elif any(modis.iGood) == False:
        if options.verbose:
            print('WARNING: no GOOD observation for this time in file <%s>'%ods_file)
        modis.nobs = 0

    nn_file = options.nn_file.replace('%ident',ident)
    modis.apply(nn_file)

#   Write ODS
#   ---------
    makethis_dir(ods_file)
    if modis.nobs>0:
        modis.writeODS(ods_file,revised=True,nsyn=options.nsyn)
    else:
        if os.system('ods_blank.x %s %s %s %s'%(options.blank_ods,nymd,nhms,ods_file)):
            warnings.warn('cannot create empty output file <%s>'%ods_file)
        else:
            if options.verbose:
                print("[w] Wrote empty ODS file "+ods_file)

#   Write gridded output file (revised channels only)
#   -------------------------------------------------
    makethis_dir(out_file)
    if modis.nobs>0:
      if str.isdigit(options.res):
        modis.writeg(filename=out_file,refine=int(options.res),channels=modis.channels_)
      else:
        modis.writeg(filename=out_file,res=options.res,channels=modis.channels_)

#   Write ungridded data
#   --------------------
#    name, ext = os.path.splitext(out_file)
#    npz_file = name.replace('Level3','Level2') + '.npz'
#    makethis_dir(npz_file)
#    modis.write(npz_file)
    
#   Compress nc output unless the user disabled it
#   ----------------------------------------------
    if modis.nobs>0:
        if not options.uncompressed:
            if subprocess.call("n4zip " + out_file,shell=True):
                warnings.warn('cannot compress output file <%s>'%out_file)
            if subprocess.call("n4zip " + ods_file,shell=True):
                warnings.warn('cannot compress output ods file <%s>'%ods_file)
