#!/bin/csh
#
# Usage:  xtract_runh.csh inputname outputname runh
#
#    inputname   name of input prepbufr file
#    outputname  name of output prepbufr file
#    runh        file with names of RUNH 'subset' names to extract
#
#


/bin/rm fort.11 fort.51 fort.52

ln -s $1 fort.11
ln -s $2 fort.51
ln -s output.$2.txt fort.52
(wc -l $3; cat $3) | /home/msienkie/share/paqc/prepobs_listheaders.fd/listheaders.x 

