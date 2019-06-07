#!/bin/csh

#source $ODV/RT/G517/src/g5_modules

setenv ERROR0 1
setenv DOMEAN 1
setenv DOMEMS 1
setenv EFSENS 1
setenv DOXTRA 0

# atmens_noad_efsens.csh - adjoint-free ensemble-based estimate of
#                          forecast sensitivity to initial conditions. 
#
# !TODO:
#  A. compare (1) w/ same from central adm fsens
#  B. need to correct: std-dev used for fsens estimate still using Tv instead of T
#
# !REVISION HISTORY:
#
#  27Jul2017  Todling   Initial script
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars (not to become global)
setenv MYNAME atmens_noad_efsens.csh

if ( $#argv < 6 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - run forecast sensitivty for each ensemble member"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid nymd nhms taub aver iniwrt"
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  initial date of forecast, as in YYYYMMDD"
   echo "   nhms   -  initial time of forecast, as in HHMMSS"
   echo "   taub   -  forecast interval EFSO calcualted for (in hours)"
   echo "   aver   -  verification type (ana,asm,or niana) "
   echo "   iniwrt -  initial error with respect to (ana,bkg,or niana) "
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "    This procedure is controls fvsens calls to the members of the "
   echo "  ensemble. "
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME b541iau 20091019 000000 24 asm setrc"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    AENSTAT_MPIRUN   - mp_stats MPI command line         "
   echo "    ATMENSETC        - location of experiment            "
   echo "    FVWORK           - location of work directory        "
   echo " "
   echo " REQUIRED RC FILES"
   echo " "
   echo "    initadj.rc       - RC file defining norm and LPO     "
   echo "    atmens_efsens.rc - RC file specifics for this application"
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "    NCSUFFIX          - suffix of hdf/netcdf files (default: nc4)"
   echo "    STAGEEFSENS       - location where to stage fsens output "
   echo "                        (default: /dev/null - no stage)"
   echo " "
   echo " SEE ALSO"
   echo "   fvsens - driver for model adjoint runs"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Created modified: 27Jul2017   by: R. Todling"
   echo "     Last modified: 09Aug2017      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?AENSTAT_MPIRUN)) setenv FAILED 1
if ( !($?ATMENSETC)     ) setenv FAILED 1
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVWORK)        ) setenv FAILED 1

if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4
if ( !($?STAGEEFSENS)   ) setenv STAGEEFSENS /dev/null
if ( !($?MPIRUN1PE)     ) setenv MPIRUN1PE "mpiexec_mpt -np 1"

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid = $1
set nymd  = $2
set nhms  = $3
set ftau_hr = $4
set aver  = $5
# initial error w/ respect to ana (or bkg, or niana)
set iniwrt = $6

set hh = `echo $nhms | cut -c1-2`
set yyyymmddhh = ${nymd}${hh}

# Check availability of RC files
set MYRCFILE = $FVHOME/run/ageps/atmens_efsens_${hh}.rc
if ( ! -e $MYRCFILE ) then
  set MYRCFILE = $FVHOME/run/ageps/atmens_efsens.rc 
  if ( ! -e $MYRCFILE ) then
     echo " ${MYNAME}: cannot find $MYRCFILE"
     exit 1
  endif
endif
set INITADJRC = $FVHOME/run/ageps/initadj.rc
if ( ! -e $INITADJRC ) then
  echo " ${MYNAME}: cannot find $INITADJRC"
  exit 1
endif

set adtau_hr = `echorc.x -rc $MYRCFILE adm_tau_hr`
@ adtau_sec = $adtau_hr * 3600
@ ana_offset = 3 * 3600
@ ftau_sec = $ftau_hr * 3600

# First synoptic date/time from starting date/time
# ------------------------------------------------
set date0 = (`tick $nymd $nhms $ana_offset`)
set nymd0 = $date0[1]
set nhms0 = $date0[2]
set yyyy0 = `echo $nymd0 | cut -c1-4`
set mm0   = `echo $nymd0 | cut -c5-6`
set hh0   = `echo $nhms0 | cut -c1-2`
set yyyymmddhh0 = ${nymd0}${hh0}

# Forecast verification date/time
# -------------------------------
set vdate = (`tick $nymd0 $nhms0 $ftau_sec`)
set vnymd = $vdate[1]
set vnhms = $vdate[2]
set vhh   = `echo $vnhms | cut -c1-2`

# Initial date/time of forecast
# -----------------------------
set adate = (`tick $vnymd $vnhms -$adtau_sec`)
set anymd = $adate[1]
set anhms = $adate[2]
set ayyyy = `echo $anymd | cut -c1-4`
set amm   = `echo $anymd | cut -c5-6`
set ahh   = `echo $anhms | cut -c1-2`
set ayyyymmddhh = ${anymd}${ahh}

# initial error w/ respect to ana (or bkg, or niana)
set inierr = bkgerr
if ( $iniwrt == "ana"   ) set inierr = anaerr
if ( $iniwrt == "niana" ) set inierr = nianaerr

setenv ENSWORK $FVWORK
cd $ENSWORK

set members = `/bin/ls -d $ENSWORK/mem* | wc`
set nmem = $members[1]

# calculate error in initial condition from ensemble analysis (diff from ensemble mean) 
if ($ERROR0) then
   # if not available, calculate ensemble mean analysis
   if ( ! -e ensmean/$expid.$iniwrt.eta.${anymd}_${ahh}z.$NCSUFFIX  ) then
      if ( ! -d ensmean ) mkdir ensmean
      if ( ! -d ensrms  ) mkdir ensrms
      $AENSTAT_MPIRUN  -rc $ATMENSETC/mp_stats.rc -o ensmean/$expid.$iniwrt.eta.${anymd}_${ahh}z.$NCSUFFIX \
                                               -stdv  ensrms/$expid.$iniwrt.eta.${anymd}_${ahh}z.$NCSUFFIX \
                                               -ene   ensrms/$expid.aerr.eta.${anymd}_${ahh}z.$NCSUFFIX \
                                                        mem*/$expid.$iniwrt.eta.${anymd}_${ahh}z.$NCSUFFIX
      if ( ! -e ensmean/$expid.$iniwrt.eta.${anymd}_${ahh}z.$NCSUFFIX  ) then
         echo " ${MYNAME}: cannot find ensmean/$expid.$iniwrt.eta.${anymd}_${ahh}z.$NCSUFFIX"
         exit 1
      endif
   endif
   foreach dir (`/bin/ls -d mem*`)
      cd $dir
      if ( ! -e $expid.$inierr.eta.${anymd}_${ahh}z.$NCSUFFIX ) then
         dyndiff.x -g5       $expid.$iniwrt.eta.${anymd}_${ahh}z.$NCSUFFIX \
            ../ensmean/$expid.$iniwrt.eta.${anymd}_${ahh}z.$NCSUFFIX \
                    -o $expid.$inierr.eta.${anymd}_${ahh}z.$NCSUFFIX
         if ($status) then
            echo " ${MYNAME}: failed calculating initial time error for $dir, aborting ..."
            exit 97
         endif
      endif
      cd -
   end
endif
# E-normalized forecast error at verification time for mean of ensemble forecast
if ($DOMEAN) then
   # if not available, calculate ensemble mean forecast
   if ( ! -e updated_ens/ensmean/$expid.prog.eta.${vnymd}_${vhh}z.$NCSUFFIX ) then
      if ( ! -d updated_ens/ensmean ) mkdir updated_ens/ensmean
      if ( ! -d updated_ens/ensrms  ) mkdir updated_ens/ensrms
      $AENSTAT_MPIRUN  -rc $ATMENSETC/mp_stats.rc \
                                                  -o updated_ens/ensmean/$expid.prog.eta.${vnymd}_${vhh}z.$NCSUFFIX \
                                               -stdv  updated_ens/ensrms/$expid.prog.eta.${vnymd}_${vhh}z.$NCSUFFIX \
                                               -ene   updated_ens/ensrms/$expid.perr.eta.${vnymd}_${vhh}z.$NCSUFFIX \
                                                        updated_ens/mem*/$expid.prog.eta.${vnymd}_${vhh}z.$NCSUFFIX
     if ( ! -e updated_ens/ensmean/$expid.prog.eta.${vnymd}_${vhh}z.$NCSUFFIX ) then
        echo " ${MYNAME}: cannot find updated_ens/ensmean/$expid.prog.eta.${vnymd}_${vhh}z.$NCSUFFIX "
        exit 2
     endif
   endif
   if ( ! -e ensmean/$expid.ana.eta.${vnymd}_${vhh}z.$NCSUFFIX ) then
      $AENSTAT_MPIRUN  -rc $ATMENSETC/mp_stats.rc -o ensmean/$expid.ana.eta.${vnymd}_${vhh}z.$NCSUFFIX \
                                               -stdv  ensrms/$expid.ana.eta.${vnymd}_${vhh}z.$NCSUFFIX \
                                                        mem*/$expid.ana.eta.${vnymd}_${vhh}z.$NCSUFFIX
      if ( ! -e ensmean/$expid.ana.eta.${vnymd}_${vhh}z.$NCSUFFIX ) then
         echo " ${MYNAME}: cannot find ensmean/$expid.ana.eta.${vnymd}_${vhh}z.$NCSUFFIX "
         exit 2
      endif
   endif
   if ( ! -e updated_ens/ensmean/$expid.prgEerr.eta.${vnymd}_${vhh}z.$NCSUFFIX ) then
      dyndiff.x -g5  -txt updated_ens/ensmean/$expid.Jnormf.${nymd}_${hh}z+${vnymd}_${vhh}z.txt \
                          updated_ens/ensmean/$expid.prog.eta.${vnymd}_${vhh}z.$NCSUFFIX \
                                      ensmean/$expid.ana.eta.${vnymd}_${vhh}z.$NCSUFFIX \
                                -ene_scale -rc $INITADJRC \
                                -o updated_ens/ensmean/$expid.prgEerr.eta.${vnymd}_${vhh}z.$NCSUFFIX
      if ($status) then
         echo " ${MYNAME}: failed E-normalizing mean forecast, aborting ..."
         exit 97
      endif
    endif
   cd ensmean/
   ln -sf ../updated_ens/ensmean/$expid.prgEerr.eta.${vnymd}_${vhh}z.$NCSUFFIX .
   # just for diagnostic purposes ...
   set norm = `echorc.x -rc $INITADJRC pert_norm`
   if (! -e ../updated_ens/ensmean/$expid.fene_${norm}.eta.${nymd}_${hh}z+${vnymd}_${vhh}z.$NCSUFFIX ) then
      ln -sf $expid.prgEerr.eta.${vnymd}_${vhh}z.$NCSUFFIX Eerr.eta.$NCSUFFIX
      dyn_efsens.x -verb -debug Eerr.eta.$NCSUFFIX -mem 0 -fcstlen $adtau_hr \
                   -eo ../updated_ens/ensmean/$expid.fene_${norm}.eta.${nymd}_${hh}z+${vnymd}_${vhh}z.$NCSUFFIX \
                   ${vnymd} ${vhh}0000 null null null
      if ( $status ) then
         echo " ${MYNAME}: failed calc of energy field for fcst error at verif time, aborting ..."
         exit 97
      endif
   endif
   cd -

endif

# E-normalized forecast error at verification time for each member
if ($DOMEMS) then
  foreach dir (`/bin/ls -d mem*`)
     cd $dir
     if ( ! -e $expid.prgEerr.eta.${vnymd}_${vhh}z.$NCSUFFIX ) then
        dyndiff.x -g5 -ene_scale -txt $expid.Jnormf.${nymd}_${hh}z+${vnymd}_${vhh}z.txt \
             ../updated_ens/$dir/$expid.prog.eta.${vnymd}_${vhh}z.$NCSUFFIX \
                      ../ensmean/$expid.ana.eta.${vnymd}_${vhh}z.$NCSUFFIX  \
                  -rc $INITADJRC  -o $expid.prgEerr.eta.${vnymd}_${vhh}z.$NCSUFFIX
        if ($status) then
           echo " ${MYNAME}: failed E-normalizing member $dir, aborting ..."
           exit 98
        endif
     endif
     cd -
  end
endif

# Estimate ensemble-based forecast sensitivity to initial condition
if ($EFSENS) then
   set norm = `echorc.x -rc $INITADJRC pert_norm`
   if ( ! -e updated_ens/ensmean/$expid.fsens_${norm}.eta.${nymd}_${hh}z+${vnymd}_${vhh}z-${anymd}_${ahh}z.$NCSUFFIX ) then
      if ( $DOXTRA ) then
         dyn_efsens.x -verb -mem $nmem -fcstlen $adtau_hr \
                   -o $expid.fsens_${norm}.eta.${nymd}_${hh}z+${vnymd}_${vhh}z-${anymd}_${ahh}z.$NCSUFFIX \
                   ${anymd} ${ahh}0000 \
                   $expid.$iniwrt.eta.${anymd}_${ahh}z.$NCSUFFIX \
                   $expid.$inierr.eta.${anymd}_${ahh}z.$NCSUFFIX \
                   $expid.prgEerr.eta.${vnymd}_${vhh}z.$NCSUFFIX
      else
         dyn_efsens.x -verb -mem $nmem -fcstlen $adtau_hr \
                   -o $expid.fsens_${norm}.eta.${nymd}_${hh}z+${vnymd}_${vhh}z-${anymd}_${ahh}z.$NCSUFFIX \
                   -eo $expid.iene_${norm}.eta.${nymd}_${hh}z+${vnymd}_${vhh}z-${anymd}_${ahh}z.$NCSUFFIX \
                   ${anymd} ${ahh}0000 \
                   $expid.$iniwrt.eta.${anymd}_${ahh}z.$NCSUFFIX \
                   $expid.$inierr.eta.${anymd}_${ahh}z.$NCSUFFIX \
                   $expid.prgEerr.eta.${vnymd}_${vhh}z.$NCSUFFIX
      endif
      if ($status) then
         echo " ${MYNAME}: failed estimating forecast sensitivity, aborting ..."
         exit 99
      endif
      if ( $STAGEEFSENS != "/dev/null" ) then
         if ( -d $STAGEEFSENS ) then
            /bin/cp $expid.fsens_${norm}.eta.${nymd}_${hh}z+${vnymd}_${vhh}z-${anymd}_${ahh}z.$NCSUFFIX $STAGEEFSENS
         endif
      endif
      /bin/mv $expid.fsens_${norm}.eta.${nymd}_${hh}z+${vnymd}_${vhh}z-${anymd}_${ahh}z.$NCSUFFIX updated_ens/ensmean/
      if ( -e $expid.iene_${norm}.eta.${nymd}_${hh}z+${vnymd}_${vhh}z-${anymd}_${ahh}z.$NCSUFFIX ) then
         /bin/mv $expid.iene_${norm}.eta.${nymd}_${hh}z+${vnymd}_${vhh}z-${anymd}_${ahh}z.$NCSUFFIX  updated_ens/ensmean/
      endif
   endif

   if ( $DOXTRA ) then
      # Convert sensitivity to perturbation 
      if ( ! -e pert.eta.${anymd}_${ahh}z.$NCSUFFIX ) then
         $MPIRUN1PE fsens2pert.x -g5 -rc $INITADJRC -o  pert.eta.${anymd}_${ahh}z.$NCSUFFIX \
                      -sens updated_ens/ensmean/$expid.fsens_${norm}.eta.${nymd}_${hh}z+${vnymd}_${vhh}z-${anymd}_${ahh}z.$NCSUFFIX \
                      -ref  ensmean/$expid.$iniwrt.eta.${anymd}_${ahh}z.$NCSUFFIX
      endif

      # Calculate total energy from perturbation
      if ( -e pert.eta.${anymd}_${ahh}z.$NCSUFFIX ) then
       if ( ! -e updated_ens/ensmean/$expid.iene_${norm}.eta.${nymd}_${hh}z+${vnymd}_${vhh}z-${anymd}_${ahh}z.$NCSUFFIX ) then
          set eps_eer = `echorc.x -rc $INITADJRC ehrendorfer_errico_raedder_eps`
          $MPIRUN1PE pertenergy.x -g5 -notag -o updated_ens/ensmean/$expid.iene_${norm}.eta.${nymd}_${hh}z+${vnymd}_${vhh}z-${anymd}_${ahh}z.$NCSUFFIX \
                           -eps_eer $eps_eer -pick $anymd ${ahh}0000 \
                           -pert pert.eta.${anymd}_${ahh}z.$NCSUFFIX \
                           -ref ensmean/$expid.$iniwrt.eta.${anymd}_${ahh}z.$NCSUFFIX
       endif
      endif
   endif # DOXTRA

endif

touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
exit(0)
