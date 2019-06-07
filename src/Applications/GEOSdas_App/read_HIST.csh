#!/bin/csh -f
# Reads either format of HISTORY.rc and prints the file names with expid added and a trailing "."
# Usage: read_HIST.csh HISTORY.rc
# Expects the EXPID environment variable to be set
# T.Owens 20071005

set new = `grep -c "COLLECTIONS:" $1`

if ( ! $new ) then
grep filename $1 | grep -v null | cut -d: -f2 | sed -e 's/,//g' -e "s/'/ /g" -e "s/%s/$EXPID/"| cut -d '%' -f1 | sort -u 
else
cat $1 | awk -F' ' '{STRING = $1; if (STRING == "COLLECTIONS:") {doPRINT = 1;  print ENVIRON["EXPID"]"."$2"."}; if (STRING == "::") doPRINT = 0; if ((STRING != "#")&&(STRING != "COLLECTIONS:")&&(doPRINT)) print ENVIRON["EXPID"]"."$1"."}' | sed -e "s/'//g" | sort -u 
endif 

