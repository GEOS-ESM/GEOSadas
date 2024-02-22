#!/usr/bin/env python3
"""
Splits AERONET into synoptic chunks.
"""

import os
import sys

from datetime  import date, datetime, timedelta

from pyobs.aeronet import AERONET_L2, granules


#---------------------------------------------------------------------
def makethis_dir(path):
    """Creates the relevant directory if necessary."""
    if path != '':
        rc = os.system('mkdir -p '+path)
        if rc:
            raise IOError("could not create directory "+path)

if __name__ == "__main__":

    RootDir = '/nobackup/AERONET/MERRA-2'
    ods_blank = '/nobackup/NNR/Misc/blank.ods'

    oneday = timedelta(seconds=24*60*60)
    onehour = timedelta(seconds=60*60)
        
    if len(sys.argv)<2:
        print("Usage:")
        print("        aeronet4_all.py  year1 [year2]")
        sys.exit(1)
    else:
        y1 = sys.argv[1]
        if len(sys.argv)>2:
            y2 = sys.argv[2]
        else:
            y2 = y1
        Years = list(range(int(y1),int(y2)+1))
            
    # Loop over years
    # ---------------
    for year in Years:
        tyme0 = datetime(year,1,1)
        toy = datetime(year,12,31) - tyme0
        ndays = 1 + int(toy.total_seconds()/(24*60*60))
        for doy in range(1,ndays+1):

            today = tyme0 + (doy-1) * oneday

            print('Day: ', today)

            # Read AERONET for this day
            # -------------------------
            Files = granules(today,bracket='left')
            a = AERONET_L2(Files,Verbose=True)

            # Output directories
            # ------------------
            dirL2 = RootDir + '/Level2/ODS/Y%d/M%02d'%(today.year,today.month)
            dirL3 = RootDir + '/Level3/Y%d/M%02d'%(today.year,today.month)

            for h in range(0,24,3):

                tyme = tyme0 + (doy-1) * oneday + h * onehour

                # Make sure directories exist
                # ---------------------------
                makethis_dir(dirL2)
                makethis_dir(dirL3)

                # Write & compress the files
                # --------------------------
                fnL2, nobs = a.writeODS(tyme,dir=dirL2)                
                fnL3 = a.writeGridded(tyme,dir=dirL3)

            # Compress daily file
            # -------------------
            if a.nobs>0:
                if os.system("n4zip "+fnL2+" > /dev/null"):
                    warnings.warn('cannot compress output ODS file <%s>'%fnL2)
                if os.system("n4zip "+fnL3+" > /dev/null"):
                    warnings.warn('cannot compress output NC4 file <%s>'%fnL3)
            else:
                os.system("touch %s.empty"%fnL2)
