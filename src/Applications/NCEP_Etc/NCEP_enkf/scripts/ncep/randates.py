#!/usr/bin/env python
import sys
from dateutils import *
import random
date1 = sys.argv[1]
date2 = sys.argv[2]
nanals = int(sys.argv[3])
dates = random.sample(daterange(date1,date2,24),nanals)
for date in dates:
    print ('%s' % date)
