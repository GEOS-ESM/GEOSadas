#!/usr/bin/env python3
"""

Utility to read MISR ODS and write a smaller file with data only over land.

"""

import sys
import os

from numpy import sort, array

from pyods import ODS
from pyobs import igbp 

Bright = True # if True, keep only pixels brighter than Alb_min
Alb_min = 0.15

vpath = '/nobackup/Emissions/Vegetation/GL_IGBP_INPE/'
apath = '/nobackup/MODIS/Level3/ALBEDO/albedo_clim.ctl'

if __name__ == "__main__":

    def blank(nymd,nhms):
        print("    - NO DATA for %8d %6d "%(nymd, nhms))
        ods = ODS(nobs=1,kt=45,kx=313)
        ods.qcx[0] = 1
        ods.ks[0] = 1
        ods.lev[0] = 550
        return ods
    
    def doList(List):
        """
        Recursively, look for files in list; list items can
        be files or directories.
        """
        for item in List:
            if os.path.isdir(item):      doDir(item)
            elif os.path.isfile(item):   doFile(item)
            else:
                print("%s is not a valid file or directory, ignoring it"%item)

    def doDir(dir):
        """Recursively, look for files in directory."""
        for item in sort(os.listdir(dir)):
            path = dir + os.sep + item
            if os.path.isdir(path):      doDir(path)
            elif os.path.isfile(path):   doFile(path)
            else:
                print("%s is not a valid file or directory, ignoring it"%item)


    def doFile(fname):

        if fname.split('.')[-1] != 'ods':
            print("[*] NOT ODS "+fname)
            return

        dirn = os.path.abspath(os.path.dirname(fname))
        basen = os.path.basename(fname)
        
        nymd = int(fname.split('.')[-2])

        if Bright:
            landdn = dirn.replace('/ODS/','/ODS_Bright/')
            landfn = landdn+'/'+basen.replace('aero_','bright_')
        else:
            landdn = dirn.replace('/ODS/','/ODS_Land/')
            landfn = landdn+'/'+basen.replace('aero_','aland_')

        if os.path.exists(landfn):
            print("[ ] Skipping "+landfn)
            return

        os.system('/bin/mkdir -p '+landdn)

        print("[x] Working on "+landfn)

        for nhms in range(0,240000,30000):

            # AOD and 550 nm
            # --------------
            ods = ODS(fname,nymd,nhms,only_good=True).select(kt=45,lev=558.)

            # Get vegetation type as means of filtering out water
            # ---------------------------------------------------
            try:
                v = igbp.getDetailedVeg(ods.lon,ods.lat,vpath)
                
                # I = (v==15)|(v>=17)|(ods.lat<-60.) # water and ice
                I = (v==15)|(v>=17) # water and ice
                I = array((I==False))      # land

                if any(I):
                    ods = ods.__copy__(Indices=I)
                else:
                    ods = blank(nymd,nhms)

                if Bright and any(I):
                    ods.addVar(ga,expr='albedo',clmYear=2000)
                    I = (ods.albedo>Alb_min)
                    if any(I):
                        ods = ods.__copy__(Indices=I)
                    else:
                        ods = blank(nymd,nhms)

                if any(I):
                    print("    + Processing %8d %6d with %5d obs left"%(nymd, nhms, len(v[I])))

            except:
                ods = blank(nymd,nhms)

            if ods.nobs == 0:
                ods = blank(nymd,nhms)
                
            ods.write(landfn,nymd,nhms,nsyn=8)


        os.system("n4zip "+landfn)

    #---
            
    if len(sys.argv) < 2 :
        print('Usage: ')
        print('        %s misr_ods_files/dir_names'%os.path.basename(sys.argv[0]))
        sys.exit(1)

    # Albedo dataset
    # --------------
    if Bright:
        from grads import GrADS
        ga = GrADS(Echo=False,Window=False)
        fh = ga.open(apath)
        print(fh)

    # Process list of files or directories
    # ------------------------------------
    doList(sys.argv[1:])

