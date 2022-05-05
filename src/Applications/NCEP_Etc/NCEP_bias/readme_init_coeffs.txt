Using init_coeffs.x to create initial bias coefficients

 Usage:  init_coeffs.x [ flags ] expid yyyymmddhh      
  
  expid    - Experiment ID of input diag files
  yyyymmddhh - year-month-day-hour of input diag files
  
 Flags:  
   -h              print this usage information
   -d template     diag file template (default _ges.%y4%m2%d2_%h2z.bin) 
   -g filename     location of "gsi.rc.tmpl" used to build diag file names
   -m mode         bias correction mode (default: 1)
                                      -2  var=1e4 for initialized coefficients
                                      -1  zero bias coeff, var=1e4
                                       0  zero bias coeff, var=0  
                                       1  mean/scanangle fit      
   -nstep steps    number of cross-track positions (default: 90)
   -s filename     satinfo input file name (default=gmao_global_satinfo.rc)
       -iuse       if satinfo file set, use "iuse" values from the file    
   -v              set verbose = .true.
   -H directory    location of FVHOME 
   -P prefix       prefix template for diag files (e.g. directory path)
   -qc             use qc marks from diag file to screen obs 
 
 If adding entries for new instrument to prior satbias/satbias_pc file:
   -b filename     satbias_in input file name (optional)
   -p filename     satbias_pc input file name (optional)
 
 ENVIRONMENT VARIABLES (if not specified on command line):
    FVHOME -  needed for satinfo, scaninfo, tlapmean resource files

Output files:  satbias_out (coefficients) satbias_pc.out (precondtioner)
  a satbias_ang.out (not needed by GSI) is also written, which
  has cross-track values from evaluating the polynomial coefficients


The init_coeffs.x program reads sensor/instrument/satellite id (dsis) and
channel numbers from the satinfo file, and uses the gsi.rc.tmpl file to map
between the sis values and the satellite type and platform (dtype,dplat) used
for naming the diag files.  The program will fill in any missing tlapmean
values using information from the gmao_global_tlapmean.rc if available.

By default, the program attempts to read diag files from the current
directory with names in the format 
     %s.diag_(dtype)_(dplat)_ges.%y4%m2%d2_%h2z.bin
where the %s is substituted with the expid specified on the command line

Note: Since this program uses the "gsi.rc.tmpl" file to get values for 'dtype',
'dplat', and 'dsis' for each satellite instrument being fitted, if you are 
using the 'generic' value for these variables in your "gsi.rc.tmpl" to 
configure your instrument you will need to supply another file with an 
'OBS_INPUT::' table like in the "gsi.rc.tmpl" which specifies the actual
'dtype', 'dplat' and 'dsis' values for your satellite(s).

--------------------- EXAMPLES --------------------------------------------

1) Using archived diag_*_ges.*.bin files as input

Set prefix to point to the experiment files in the archive
  -P /archive/u/dao_it/%s/obs/Y%y4/M%m2/D%d2/H%h2/%s.  (keep the dot at end)
Set -H to point to FVHOME location of current or former experiment
  -H /discover/nobackup/projects/gmao/obsdev/dao_it/x0033

We may want to use the qc decisions recorded in the diag file to screen
out bad observations, if the diag file comes from a well spun-up experiment.
(Especially for channels with possible cloud contamination...)  

init_coeffs.x -qc -P  /archive/u/dao_it/%s/obs/Y%y4/M%m2/D%d2/H%h2/%s. \
  -H /discover/nobackup/projects/gmao/obsdev/dao_it/x0033 x0033 yyyymmddhh

2) Adding coefficients for a single instrument to a previously
spun-up coefficient set.  Sample diag file is in the current directory
 sample.diag_inst_sat_ges.yyyymmdd_hhz.bin, input satbias_in and satbias_pc
are previously spun-up coefficient files

init_coeffs.x -b (satbias_in)  -p (satbias_pc) sample yyyymmddhh

3) Starting with a bias coefficient file from a prior experiment
Generate a satbiaspc file with the preconditioner values for all channels
with initialized coefficients reset to 1.e4 - this will force the coefficients
to readjust for a new date/time/experiment

init_coeffs.x -m -2 -b satbias_in sample yyyymmddhh

4) Running in FVWORK after the analysis step

The program could be used in a two step analysis configuration to bootstrap
the bias coefficients. The radiance data is initially read in as passive and
the diag files from the first analysis are used to create initialized
coefficients to be used in a second analysis.

mv satbias_out orig.satbias_out
mv satbias_pc.out orig.satbias_pc.out
init_coeffs.x -b orig.satbias_out -p orig.satbias_pc.out -P '' \
   -d .%y4%m2%d2%h2 -g gsiparm.anl  dummy yyyymmddhh


5) Generating new tlapmean values

If the instrument being fitted does not have an entry in the
gmao_global_tlapmean.rc file or the tlapmean sample count is nonzero
and less than 100 the program will also update the tlapmean value
based on the lapse-rate values read in from the input diag file.
This can be repeated for multiple dates/times to generate a 
tlapmean value for a new instrument.  You would want to cycle the 
satbias_out back in as the satbias_in for subsequent dates.

init_coeffs.x -b satbias_in $expid yyyymmddhh
mv satbias_out satbias_in
...
