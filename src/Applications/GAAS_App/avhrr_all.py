#!/usr/bin/env python
"""
Splits AVHRR into synoptic chunks.
"""

import os
import sys

from datetime  import date, datetime, timedelta

if __name__ == "__main__":

    oneday = timedelta(seconds=24*60*60)
    onehour = timedelta(seconds=60*60)
        
    if len(sys.argv)<2:
        print "Usage:"
        print "        avhrr_all.py  year1 [year2]"
        sys.exit(1)
    else:
        y1 = sys.argv[1]
        if len(sys.argv)>2:
            y2 = sys.argv[2]
        else:
            y2 = y1
        Years = range(int(y1),int(y2)+1)
            
    # Loop over years
    # ---------------
    for year in Years:
        tyme0 = datetime(year,1,1)
        toy = datetime(year,12,31) - tyme0
        ndays = 1 + int(toy.total_seconds()/(24*60*60))
        for doy in range(1,ndays+1):

            for h in range(0,24,3):
            
                tyme = tyme0 + (doy-1) * oneday + h * onehour

                nymd = tyme.year*10000 + tyme.month*100   + tyme.day
                nhms = tyme.hour*10000 + tyme.minute*100  + tyme.second

                cmd = 'python avhrr_l2a.py -v asc %d %d'%(nymd,nhms)

                print cmd
                os.system(cmd)
