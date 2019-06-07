#!/bin/csh

# gcm_ensemble - runs ensemble of atmospheric GCMs
#
# !REVISION HISTORY:
#
#    Oct2011  Todling   Initial script
#  05Nov2011  Todling   Parallelized
#  11Nov2011  Todling   Each forecast has now its own rsts
#  15Nov2011  Todling   Revisit calc of mean/rms - via script now
#  21Nov2011  Todling   Add rsts bootstrap capability
#  31Jan2012  Todling   Add rsts regrid capability
#  22Apr2012  Todling   Updates to recent configuration of GCM
#  25May2012  Todling   Updates to Ganymed-1_0 GCM
#  05Oct2012  Todling   Move archiving and positioning of updated ens to main
#  20Oct2012  Todling   Move stats calculation to post_egcm
#  28Jan2013  Todling   - Add handle to allow extending fcst beyond cycle interval
#                       - Add handle to allow output at final time
#  12Mar2013  Todling   Implement options for distribute multi-work jobs
#  23Mar2013  Todling   Implement opt to link rst from central in case of easy-ana
#  28Feb2014  El Akkraoui  Revisit logic for re-submit of distributed jobs
#  15Sep2014  Todling   Add memtag to AGCM.rc for replay mode
#  10Aug2016  RT/EL Akkraoui Copy fv-layout to member directory
#  10Aug2016  Todling   Allow ensemble to have different fv-layout if desired
#  03Mar2017  Todling   Trigger for 4DIAU
#  21Mar2017  Todling   Edit GAAS_GridComp to set member AOD analysis when applicable
#  13Apr2017  Todling   Add knob for GEOS EPS
#  04Aug2018  Todling   Revisit/Update regridding option/mechanism
#-------------------------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME gcm_ensemble.csh

if ( $#argv < 6 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME"
   echo " "
   echo "  $MYNAME  - run multiple copies of (atmospheric) GCM "
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME  expid nymd nhms tfcst nlons nlats"
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  initial date of forecast, as in YYYYMMDD "
   echo "   time   -  initial time of forecast, as HHMMSS"
   echo "   tfcst  -  forecast length in hours"
   echo "   nlons  -  number of longitudes in bkg (im) (for history)"
   echo "   nlats  -  number of latitudes  in bkg (jm) (for history)"
   echo " "
   echo " DESCRIPTION "
   echo " "
   echo "   This procedure runs multiple copies of the, presently "
   echo "   atmospheric, GCM. These integrations are, in principle, "
   echo "   forced with an ensemble of IAU-increments)."
   echo " "
   echo "   The usual, main RC files for the GCM must be present in"
   echo "   the directory defined by ATMENSETC. Typically, this are"
   echo "   the files listed under REQUIRED RESOUCE FILES below."
   echo " "
   echo "   Furthermore, a file named lnbcs_ens must be placed under"
   echo "   the run directory. The reason why this file is not placed"
   echo "   under ATMENSETC is simply because executables are to live"
   echo "   either in the bin of the build or in the run directory, and"
   echo "   lnbcs is an executable script."
   echo " "
   echo "   Just as in the regular DAS, similar tricks to bootstrap restarts"
   echo "   apply here for the ensemble of GCMs, that is, restarts can be boot-"
   echo "   strapped by placing a file named AGCM.BOOTSTRAP.rc.tmpl under the "
   echo "   ATMENSETC directory. Analogously, once can request the GCM to "
   echo "   integrate beyond the usual 12-hour period per cycle by placing "
   echo "   specific files to control those particular instances. For example,"
   echo "   if one wishes to integrate the 00z predictions out to 24-hours, " 
   echo "   placing files CAP_21.rc.tmpl and HISTAENS_21.rc.tmpl in the "
   echo "   ATMENSETC directory with properly defined length of integration"
   echo "   and desirable history will do it."
   echo " "
   echo " Example of valid command line:"
   echo " $MYNAME b541iau 20091018 210000 12 144 91"
   echo " "
   echo " REQUIRED RESOURCE FILES"
   echo " "
   echo "   CAP.rc.tmpl      - determine length of integration"
   echo "   AGCM.rc.tmpl     - defines specific restarts, and GCM parameters"
   echo "   HISTAENS.rc.tmpl - defines output of ensemble of GCMs"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENSETC     - location of resource files        "
   echo "    ATMENSLOC     - location of current ensemble      "
   echo "    ASYNBKG       - frequency of background (minutes) "
   echo "    FVBCS         - location of fvInput               "
   echo "    FVHOME        - location of experiment            "
   echo "    FVROOT        - location of DAS build             "
   echo "    FVWORK        - location of work directory        "
   echo "    GID           - group ID to run job under         "
   echo "    MPIRUN_ENSGCM - define mpi command for GEOSgcm.x  "
   echo "    RSTSTAGE4AENS - location of restarts              "
   echo "    TIMEINC       - analysis frequency (minutes)      "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMGEPS        - trigger for GEOS EPS              "
   echo "    NCSUFFIX       - suffix of hdf/netcdf files (default: nc4)"
   echo "    ENSPARALLEL    - when set, runs all ensemble components in parallel "
   echo "                     (default: off)"
   echo "    ENSGCM_NCPUS   - when parallel ens on, this sets NCPUS for AGCM integration"
   echo "    AENS_GCM_DSTJOB- distribute multiple works within smaller jobs"
   echo "    AGCM_WALLCLOCK - wall clock time to run agcm, default 1:00:00 "
   echo "    AGCM_QNAME     - name of queue (default: NULL, that is, let pbs pick) "
   echo "    ATMENS_DO4DIAU - trigger to run 4DIAU "
   echo "    REGRID_QOS     - qos for regrid"
   echo "    RSTEXT         - defines extension for model rst files: nc4 or bin"
   echo " "
   echo " REMARKS"
   echo " "
   echo "   1. When atmens_rst_regrid.rc is present in ATMENSETC"
   echo "      this script will regrid the restart in that file "
   echo "      to the desired resolution; these will not be recycled."
   echo "   2. When running GEOS EPS the length of forecast and history"
   echo "      can be controlled by dropping the following two files:"
   echo "          CAP_hh.rc.tmpl (or simply CAP.rc.tmpl)"
   echo "          HISTAGEPS_hh.rc.tmpl (or simply HISTAGEPS.rc.tmpl)"
   echo "      in the FVHOME/run/ageps directory."
   echo " "
   echo " SEE ALSO"
   echo "   atmos_ens2gcm.csh - calculation of IAU increments"
   echo "   atm_ens_geps.j    - main job script controlling GEPS"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 20Apr2017      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?ATMENSETC)     ) setenv FAILED 1
if ( !($?ATMENSLOC)     ) setenv FAILED 1
if ( !($?ASYNBKG)       ) setenv FAILED 1
if ( !($?FVBCS)         ) setenv FAILED 1
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1
if ( !($?FVWORK)        ) setenv FAILED 1
if ( !($?GID)           ) setenv FAILED 1
if ( !($?HYBRIDGSI)     ) setenv FAILED 1
if ( !($?REGRID_QOS)    ) setenv REGRID_QOS compute
if ( !($?RSTEXT)        ) setenv RSTEXT bin
if ( !($?RSTSTAGE4AENS) ) setenv FAILED 1
if ( !($?TIMEINC)       ) setenv FAILED 1

if ( !($?ATMGEPS)       ) setenv ATMGEPS 0
if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4
if ( !($?ENSPARALLEL)   ) setenv ENSPARALLEL 0
if ( !($?AENS_GCM_DSTJOB) ) setenv AENS_GCM_DSTJOB 0
if ( !($?AGCM_WALLCLOCK)) setenv AGCM_WALLCLOCK 1:00:00
if ( !($?AGCM_QNAME)    ) setenv AGCM_QNAME NULL
if ( !($?ATMENS_DO4DIAU)) setenv ATMENS_DO4DIAU 0
if ( !($?ATMENS_IGNORE_CHKPNT)) setenv ATMENS_IGNORE_CHKPNT 0

if ( $ENSPARALLEL ) then
   if ( !($?MPIRUN_ENSGCM) ) setenv FAILED 1
   if ( !($?ENSGCM_NCPUS) ) then
     setenv FAILED 1
   else
     setenv JOBGEN_NCPUS $ENSGCM_NCPUS
   endif
endif

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid = $1
set nymdb = $2
set nhmsb = $3
set tfcst = $4
set nlons = $5
set nlats = $6
set hhb   = `echo $nhmsb | cut -c1-2`
set yyyymmddhh = ${nymdb}${hhb}

setenv ENSWORK $FVWORK
if (-e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo " ${MYNAME}: already done"
   exit(0)
endif
setenv EXPID $expid  # this variable is usually defined, but make sure since vED needs it!

#source $FVROOT/bin/g5_modules
set path = ( . $FVHOME/run $FVROOT/bin $path )

@ tfcst_sc = $tfcst * 3600
set enddate = `tick $nymdb $nhmsb  $tfcst_sc`
   set nymde = $enddate[1]
   set nhmse = $enddate[2]
   set hhe   = `echo $nhmse | cut -c1-2`

@ ioskip = $TIMEINC * 60 # TBD: this is ok for IAU, but is it general enough?
set iobeg = `tick $nymdb $nhmsb  $ioskip`
    set ionymdb = $iobeg[1]
    set ionhmsb = $iobeg[2]
    set iohhb   = `echo $iobeg[2] | cut -c1-2`
    set ionymde = $nymde
    set ionhmse = $nhmse

set members = `/bin/ls -d $ENSWORK/mem* | wc`
set nmem = $members[1]

# Quick checks
# ------------
 which lnbcs_ens
 if( $status ) then
     echo " ${MYNAME}: cannot find lnbcs_ens (should be in $FVHOME/run, aborting ..."
     exit(1)
 endif

# Get positioned inside ENSWORK
# -----------------------------
cd  $ENSWORK
touch .no_archiving

@ bkgfreq_hr = $ASYNBKG / 60
set bkgfreq_hhmss = ${bkgfreq_hr}0000

setenv LINK_RST $HYBRIDGSI 

# Clean up: make sure no bkg.eta/sfc files exist in the mem$memtag directories
# ----------------------------------------------------------------------------
# TBD be careful ...

# Check on need to regrid
# -----------------------
if (-e $ATMENSETC/atmens_rst_regrid.rc ) then
    if( -e $ENSWORK/.DONE_RST_REGRID ) /bin/rm $ENSWORK/.DONE_RST_REGRID
    set grs_regrid = ( `grep internal_rst $ATMENSETC/atmens_rst_regrid.rc  | grep -vE "^[ ]*\#" | cut -d: -f2 | sed -e's/^[ ]*\(.*[^ ]\)[ ]*$/\1/' -e 's/_rst//'` )

    # loop over regridded files and link them to sub-workdir
    # ------------------------------------------------------
    mkdir -p $ENSWORK/rst2regrid
    cd $ENSWORK/rst2regrid
    set rstotrgd = `echo $#grs_regrid`
    @ id = 0
    while ( $id < $rstotrgd )
         @ id++
         /bin/ln -sf $RSTSTAGE4AENS/*.$grs_regrid[$id]_rst.${nymdb}_${hhb}z.$RSTEXT .
    end

    # now regrid
    # ----------
    set inpdir   = $ENSWORK/rst2regrid
    set outdir   = $ENSWORK/rst2regrid/new
    set rstver   = `echorc.x -rc $ATMENSETC/atmens_rst_regrid.rc BC_TAG`
    set ens_nlon = `echorc.x -rc $ATMENSETC/AGCM.rc.tmpl AGCM_IM`
    set ens_nlat = `echorc.x -rc $ATMENSETC/AGCM.rc.tmpl AGCM_JM`
    set ens_nlev = `echorc.x -rc $ATMENSETC/AGCM.rc.tmpl AGCM_LM`
    @ six_nlon = 6 * $ens_lon
    if ( $ens_nlat == $six_nlat ) then
      set eres = C$ens_lon
    else
      if ( $ens_lon ==   72 ) set eres = "a"
      if ( $ens_lon ==  144 ) set eres = "b"
      if ( $ens_lon ==  288 ) set eres = "c"
      if ( $ens_lon ==  576 ) set eres = "d"
      if ( $ens_lon == 1152 ) set eres = "e"
      if ( $ens_lon == 2304 ) set eres = "f"
    endif
    # note: the following is not designed to convert from old to new version of RSTs
    regrid.pl -qos $REGRID_QOS -ymd $nymdb -hr $hhb -grout $eres -levsout $ens_nlev -outdir $outdir -d $inpdir \
              -nobkg -nolbl -nolcv \
              -expid $EXPID -tagin $rstver -oceanin CS -tagout $rstver -rs 3 -oceanout CS -grpID $GID -zoom 4 -np
    if ($status) then
         echo " ${MYNAME}: Failed to regrid RST files, Aborting ... "
         exit(1)
    else
         @ id = 0
         mkdir $ENSWORK/rst2regrid/Hold
         while ( $id < $rstotrgd ) # clean up: remove links
              @ id++
              /bin/mv $ENSWORK/rst2regrid/*$grs_regrid[$id]_rst*.$RSTEXT $ENSWORK/rst2regrid/Hold/
              /bin/mv $ENSWORK/rst2regrid/new/*$grs_regrid[$id]_rst* $ENSWORK/rst2regrid/
         end
         touch $ENSWORK/.DONE_RST_REGRID
    endif
    cd $ENSWORK

else # no regridding ...

    # check to see if doing easy case (simplified scheme)
    # ---------------------------------------------------
    if ( -e $ATMENSETC/easyeana.rc ) then
       # check resolution
       set ens_res = `echorc.x -rc $ATMENSETC/easyeana.rc ensemble_resolution`
       set cnt_res = `echorc.x -rc $ATMENSETC/easyeana.rc central_das_resolution`
       # if ensemble and central run at same resolution ...
       #  simply link RSTs from central, i.e., all members begin from same RSTs
       # ----------------------------------------------------------------------
       if ( "$ens_res" == "$cnt_res" ) then
           setenv LINK_RST $RSTSTAGE4AENS
       endif
    endif

endif


# If not all done yet ...
# -----------------------
if(! -e .DONE_ENSFCST ) then
  # Loop over members
  # REMARK: For parallelization purposes the inside of the
  #         loop below should moved into a separate script.
  # -------------------------------------------------------
  /bin/rm $ENSWORK/agcm_poe.*
  /bin/rm $ENSWORK/agcm_machfile*

  if ( $ENSPARALLEL ) then
     set nfiles = `/bin/ls $ENSWORK/.DONE_MEM*_${MYNAME}.$yyyymmddhh | wc -l`
     echo "${MYNAME}: number of already available files  ${nfiles}"
     @ ntodo = $nmem - $nfiles 
  endif 
  @ ipoe = 0
  @ npoe = 0
  @ fpoe = 0
  @ ic = 0
  while ( $ic < $nmem )

     # Get positioned
     # --------------
     @ ic++
     set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
     cd mem${memtag}

     if(! -e $ENSWORK/.DONE_MEM${memtag}_${MYNAME}.$yyyymmddhh ) then

        # For now, only IAU rst is different all other rst's same
        # -------------------------------------------------------
        ln -sf agcm_import_mem${memtag}_rst agcm_import_rst

        # Bring in restarts from HYBRIDGSI here
        # -------------------------------------
        if ( -e $ATMENSETC/AGCM.BOOTSTRAP.rc.tmpl ) then
          set myagcmrc = $ATMENSETC/AGCM.BOOTSTRAP.rc.tmpl
        else
          set myagcmrc = $ATMENSETC/AGCM.rc.tmpl
          if ( $ATMGEPS ) then
              if ( -e $FVHOME/run/ageps/AGCM.rc.tmpl ) set myagcmrc = $FVHOME/run/ageps/AGCM.rc.tmpl
          endif 
        endif
        echo " $MYNAME : Caution, must use restarts before hi-res GCM runs"
        echo " $MYNAME : ================================================="
        set grs_list = ( `grep _rst $myagcmrc | grep -vE "^[ ]*\#" | cut -d: -f2 | sed -e's/^[ ]*\(.*[^ ]\)[ ]*$/\1/' -e 's/_rst//'` )
        set rstot = `echo $#grs_list`
        @ id = 0
        while ( $id < $rstot )
          @ id++
          if( $grs_list[$id] != "agcm_import" ) then
             if (-e $ENSWORK/.DONE_RST_REGRID ) then # in case not all RSTs are cycled (that is, some are regrided)
                 @ iv = 0
                 @ found = 0
                 while ( $iv < $rstotrgd ) #&& $found ) # loop over regridded rst's
                      @ iv++
                      if ( $grs_list[$id] == $grs_regrid[$iv] ) then  # if rst in AGCM.rc is one that's been regridded ...
                         set found = 1
                         /bin/ln -sf $ENSWORK/rst2regrid/*$grs_regrid[$iv]_rst* $grs_list[$id]_rst
                      endif
                 end
                 if ( ! $found ) then # if RST in AGCM not among those regridded, then it's among those cycled ...
                     /bin/ln -sf $HYBRIDGSI/mem${memtag}/$expid.$grs_list[$id]_rst.${nymdb}_${hhb}z.$RSTEXT $grs_list[$id]_rst
                 endif
             else # expect the full suite of RSTs to be cycling
                 if ( "$LINK_RST" == "$HYBRIDGSI" ) then
                    /bin/ln -sf $LINK_RST/mem${memtag}/$expid.$grs_list[$id]_rst.${nymdb}_${hhb}z.$RSTEXT $grs_list[$id]_rst
                 else
                    /bin/ln -sf $LINK_RST/$expid.$grs_list[$id]_rst.${nymdb}_${hhb}z.$RSTEXT $grs_list[$id]_rst
                 endif
             endif # <REGRID>
          endif
        end

        # All of the RCs for GOCART and what not ... TBD: this mambo-jambo needs attention
        # --------------------------------------------------------------------------------
        /bin/ln -sf $ENSWORK/ExtData $ENSWORK/mem${memtag}/
        /bin/cp $ATMENSETC/GEOS_ChemGridComp.rc .
        /bin/cp $FVHOME/run/gocart/*.rc         .
        if(-e ExtData.rc) /bin/rm -f ExtData.rc
        set  extdata_files = `/bin/ls -1 *_ExtData.rc`
        cat $extdata_files > $ENSWORK/mem${memtag}/ExtData.rc

	if(-e $ATMENSETC/aens_stoch.rc ) /bin/ln -sf  $ATMENSETC/aens_stoch.rc $ENSWORK/mem${memtag}/stoch.rc

        # Edit and copy GAAS resource files
        # ---------------------------------
        if ( $GAAS_ANA ) then
            if (! -e $ATMENSETC/GAAS_GridComp.rc ) then
                echo "${MYNAME}: missing GAAAS_GridComp.rc file, aborting"
                exit(1)
            endif
            # Note: The vED command below requires FVWORK to be defined, in this case, as
            #       the local directory ... so we temporarily reset FVWORK and set it back
            #       to its original value at the of the for-each below.
            setenv oriFVWORK $FVWORK
            setenv FVWORK    $ENSWORK/mem${memtag}
            setenv MEMTAG    $memtag
            foreach file ( $FVHOME/run/gaas/*.rc )
                set target = $ENSWORK/mem${memtag}/$file:t
                if ($file:t == fvpsas.rc) continue
                set myfile = $file
                if ($file:t == GAAS_GridComp.rc) then
                   set myfile = $ATMENSETC/GAAS_GridComp.rc
                endif
                vED -env $myfile -o $target
                echo "cat $target"
                cat $target
            end
            unsetenv MEMTAG
            setenv   FVWORK    $oriFVWORK
            if ( -e $ATMENSETC/GAAS.BOOTSTRAP ) then
               # what are we supposed to do here?
            endif
        endif
 
        # FV-core layout file
        # -------------------
        if ( -e $ATMENSETC/fvcore_layout.rc ) then
           /bin/cp $ATMENSETC/fvcore_layout.rc .
        else
           /bin/cp $FVHOME/run/fvcore_layout.rc .
        endif
        /bin/cp fvcore_layout.rc input.nml

        # Prepare CAP
        # -----------
        set this_cap = $ATMENSETC/CAP.rc.tmpl
        if ( $ATMGEPS ) then
           if ( -e $FVHOME/run/ageps/CAP_${hhb}.rc.tmpl ) then
              set this_cap = $FVHOME/run/ageps/CAP_${hhb}.rc.tmpl
           else
              if ( -e $FVHOME/run/ageps/CAP.rc.tmpl ) then
                 set this_cap = $FVHOME/run/ageps/CAP.rc.tmpl
              endif
           endif
        else
           if ( -e $ATMENSETC/CAP_${hhb}.rc.tmpl ) then
              set this_cap = $ATMENSETC/CAP_${hhb}.rc.tmpl
           endif
        endif
        /bin/cp $this_cap  CAP.rc

        set tfinal = ( `echorc.x -rc CAP.rc JOB_SGMT` )
        @ tfinal_sc = ( $tfinal[1] * 24 + $tfinal[2] / 10000 ) * 3600
        set fnldate = `tick $nymdb $nhmsb  $tfinal_sc`
        set nymdf = $fnldate[1]
        set nhmsf = $fnldate[2]
        set hhf   = `echo $nhmsf | cut -c1-2`
        set ionymdf = $nymdf
        set ionhmsf = $nhmsf
        

        # Prepare AGCM
        # ------------
         /bin/rm -f sed_file
         echo "s/>>>EXPID<<</${expid}/1"     > sed_file
         echo "s/>>>RECFINL<<</NO/1"         >> sed_file
         echo "s/>>>REFDATE<<</${ionymdb}/1" >> sed_file
         echo "s/>>>REFTIME<<</${ionhmsb}/1" >> sed_file
         echo "s/>>>FCSDATE<<</31760704/1"   >> sed_file
         echo "s/>>>FCSTIME<<</000000/1"     >> sed_file
         if ($ATMENS_DO4DIAU) then
           echo "s/>>>FORCEDAS<<</#/1"       >> sed_file
           echo "s/>>>4DIAUDAS<<<//1"        >> sed_file
         else
           echo "s/>>>FORCEDAS<<<//1"        >> sed_file
           echo "s/>>>4DIAUDAS<<</#/1"       >> sed_file
         endif
         echo "s/>>>FORCEGCM<<</#/1"         >> sed_file
         echo "s/>>>DATAOCEAN<<<//1"         >> sed_file
         echo "s/>>>COUPLED<<</#/1"          >> sed_file
         echo "s/>>>MEMBER<<</${memtag}/1"   >> sed_file
         /bin/rm -f ./AGCM.rc
         sed -f sed_file  $myagcmrc > ./AGCM.rc

        # Prepare HISTORY
        # ---------------
         /bin/rm -f sed_file
         echo "s/>>>EXPID<<</${expid}/1"            > sed_file
         echo "s/>>>IOBBKGD<<</${ionymdb}/1"       >> sed_file
         echo "s/>>>IOBBKGT<<</${ionhmsb}/1"       >> sed_file
         echo "s/>>>IOEDATE<<</${ionymde}/1"       >> sed_file
         echo "s/>>>IOETIME<<</${ionhmse}/1"       >> sed_file
         echo "s/>>>IOFDATE<<</${ionymdf}/1"       >> sed_file
         echo "s/>>>IOFTIME<<</${ionhmsf}/1"       >> sed_file
         echo "s/>>>BKGFREQ<<</${bkgfreq_hhmss}/1" >> sed_file
         echo "s/>>>MEMBER<<</${memtag}/1"         >> sed_file
         echo "s/>>>HIST_IM<<</${nlons}/1"         >> sed_file
         echo "s/>>>HIST_JM<<</${nlats}/1"         >> sed_file
         echo "s/>>>NCSUFFIX<<</${NCSUFFIX}/1"     >> sed_file
         /bin/rm -f ./HISTORY.rc
         set this_hist = $ATMENSETC/HISTAENS.rc.tmpl
         if ( $ATMGEPS ) then
            if ( -e $FVHOME/run/ageps/HISTAGEPS_${hhb}.rc.tmpl ) then
               set this_hist = $FVHOME/run/ageps/HISTAGEPS_${hhb}.rc.tmpl
            else
               if ( -e $FVHOME/run/ageps/HISTAGEPS.rc.tmpl ) then
                  set this_hist = $FVHOME/run/ageps/HISTAGEPS.rc.tmpl
               endif
            endif
         else
            if ( -e $ATMENSETC/HISTAENS_${hhb}.rc.tmpl ) then
               set this_hist = $ATMENSETC/HISTAENS_${hhb}.rc.tmpl
            endif
         endif
         sed -f sed_file  $this_hist  > ./HISTORY.rc

        # Link in BCS
        # -----------
        lnbcs_ens $nymdb

        # Run ensemble
        # ------------
        if( -e cap_restart ) /bin/rm cap_restart
        echo $nymdb $nhmsb > cap_restart

        if( $ENSPARALLEL ) then
             @ fpoe++
             if ( $AENS_GCM_DSTJOB != 0 ) then # case of multiple jobs within few larger ones
                # collect multiple gcm calls into jumbo file
                if ( $ipoe < $AENS_GCM_DSTJOB ) then # nmem better devide by AENS_GCM_DSTJOB
                   @ ipoe++
                   set this_script_name = `pwd`/agcm_mem${memtag}.j
                   echo $this_script_name >> $ENSWORK/agcm_poe.$npoe
                   chmod +x $ENSWORK/agcm_poe.$npoe
                endif
                set machfile = "-machfile $ENSWORK/agcm_machfile$npoe.$ipoe"
             else
                set machfile = ""
             endif

             jobgen.pl \
                  -egress EGRESS -q $AGCM_QNAME $machfile \
                  -xc "update_ens.csh $expid $memtag bkg $ENSWORK/mem${memtag} NULL $NCSUFFIX" \
                  agcm_mem${memtag}     \
                  $GID                  \
                  $AGCM_WALLCLOCK       \
                  "$MPIRUN_ENSGCM |& tee -a $ENSWORK/agcm_mem${memtag}.log" \
                  $ENSWORK/mem${memtag} \
                  $MYNAME               \
                  $ENSWORK/.DONE_MEM${memtag}_${MYNAME}.$yyyymmddhh \
                  "Atmos GCM Failed"


             if ( $AENS_GCM_DSTJOB != 0 ) then
                if ( -e agcm_mem${memtag}.j ) then
                   chmod +x agcm_mem${memtag}.j
                else
                   echo " ${MYNAME}: AGCM Failed to generate PBS jobs for Member ${memtag}, Aborting ... "
                   touch $ENSWORK/.FAILED
                   exit(1)
                endif

                if ( ($ipoe == $AENS_GCM_DSTJOB) || (($fpoe == $ntodo) && ($ipoe < $AENS_GCM_DSTJOB) ) ) then
                   @ myncpus = $ipoe * $ENSGCM_NCPUS
                   setenv JOBGEN_NCPUS $myncpus
                   jobgen.pl \
                        -q $AGCM_QNAME \
                        agcm_dst${npoe}     \
                        $GID                \
                        $AGCM_WALLCLOCK    \
                        "job_distributor.csh -machfile $ENSWORK/agcm_machfile$npoe -usrcmd $ENSWORK/agcm_poe.$npoe -usrntask $ENSGCM_NCPUS" \
                        $ENSWORK  \
                        $MYNAME             \
                        $ENSWORK/.DONE_POE${npoe}_${MYNAME}.$yyyymmddhh \
                        "AGCM Failed for Member ${npoe}"
                   if (! -e agcm_dst${npoe}.j ) then
                      echo " ${MYNAME}: AGCM Failed to generate DST PBS jobs for Member ${memtag}, Aborting ... "
                      touch $ENSWORK/.FAILED
                      exit(1)
                   endif
                   /bin/mv agcm_dst${npoe}.j $ENSWORK/
                   # this job is really not monitored; the real work done by agcm_mem${memtag}.j is monitored
                   qsub $ENSWORK/agcm_dst${npoe}.j
                   touch .SUBMITTED
                   @ ipoe = 0 # reset counter
                   @ npoe++
                endif 
             else
                if ( -e agcm_mem${memtag}.j ) then
                   qsub agcm_mem${memtag}.j
                   touch $ENSWORK/.SUBMITTED
                else
                   echo " ${MYNAME}: Failed to generate PBS jobs for Atmos GCM, Aborting ... "
                   exit(1)
                endif
             endif # <DSTJOB>

        else

             $MPIRUN_ENSGCM |& tee -a $ENSWORK/agcm_mem${memtag}.log
             /bin/ls ./EGRESS
             set model_status = $status
             if ( $model_status ) then
                echo " ${MYNAME}: failed to run GCM for member $memtag, aborting ..."
                touch $ENSWORK/.FAILED
                exit(1)
             endif
             touch $ENSWORK/.DONE_MEM${memtag}_${MYNAME}.$yyyymmddhh

             # Store updated ensemble
             # ----------------------
             update_ens.csh $expid $memtag bkg $ENSWORK/mem${memtag} NULL $NCSUFFIX

        endif

     endif # if -e DONE_ENSFCST

     # back to work directory
     cd ../

  end # end loop over members

  # Monitor status of ongoing jobs
  # ------------------------------
  if ( $ENSPARALLEL ) then
     jobmonitor.csh $nmem $MYNAME $ENSWORK $yyyymmddhh
     if ($status) then
         echo "${MYNAME}: cannot complete due to failed jobmonitor, aborting"
         exit(1)
     endif
  endif
 
  # Check that forecasts have taken place successfully
  # TBD: make this an independent script to be used elsewhere
  # --------------------------------------------------
  @ ic = 0
  @ idone = 0
  while ( $ic < $nmem )
       # Get positioned
       # --------------
       @ ic++
       set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
       if ( -e $ENSWORK/.DONE_MEM${memtag}_${MYNAME}.$yyyymmddhh ) then
          @ idone = $idone + 1
       endif
  end

  if ( $idone == $nmem ) touch .DONE_ENSFCST
endif # check .DONE_ENSFCST

# when done and doing bootstrap, make to reset to regular resource file
# ---------------------------------------------------------------------
if ( (-e .DONE_ENSFCST) && (-e $ATMENSETC/AGCM.BOOTSTRAP.rc.tmpl) ) then
  /bin/mv $ATMENSETC/AGCM.BOOTSTRAP.rc.tmpl $ATMENSETC/AGCM.BOOTSTRAP.rc.tmpl.DONE
  echo " ${MYNAME}: done with bootstraping restarts"
endif
if ( (-e .DONE_ENSFCST) && (-e $ATMENSETC/GAAS.BOOTSTRAP) ) then
  /bin/mv $ATMENSETC/GAAS.BOOTSTRAP $ATMENSETC/GAAS.BOOTSTRAP.DONE
  echo " ${MYNAME}: done with bootstraping GAAS"
endif


# Handle extra history output (beyond bkg-related history)
# --------------------------------------------------------
if (! -e $ENSWORK/.DONE_GCM_DIAG_UPD ) then
  @ ic = 0
  while ( $ic < $nmem )
     # Get positioned
     # --------------
     @ ic++
     set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
     cd mem${memtag}
     update_ens.csh $expid $memtag diag $ENSWORK/mem${memtag} HISTORY.rc $NCSUFFIX
     cd -
  end
  touch $ENSWORK/.DONE_GCM_DIAG_UPD
endif

# Now that forecasts finished cleanly, rename all restarts
# --------------------------------------------------------
if ( "$LINK_RST" == "$HYBRIDGSI" ) then # won't do it in easy-ana case when central_res = ensmble_res
   @ ic = 0
   while ( $ic < $nmem )
     @ ic++
     set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
     if (! -e $ENSWORK/.DONE_UPDATE_RST_MEM${memtag}.$yyyymmddhh ) then

        # Get positioned
        # --------------
        cd $ENSWORK/mem$memtag

        set chkpnt_date = `echorc.x -rc AGCM.rc RECORD_REF_DATE`
        set store_chkpnt = 1
        if ( $chkpnt_date[1] < 17760705 ) then
           set store_chkpnt = 0
        endif

        # Place restarts in updated directory
        # -----------------------------------
        if ( $store_chkpnt ) then
        echo " $MYNAME : Moving restarts to updated location of ensemble"
        echo " $MYNAME : ==============================================="
        set grs_list = ( `grep _checkpoint AGCM.rc | grep -vE "^[ ]*\#" | cut -d: -f2 | sed -e's/^[ ]*\(.*[^ ]\)[ ]*$/\1/' -e 's/_checkpoint//'` )
        set rstot = `echo $#grs_list`
        @ id = 0
        while ( $id < $rstot )
          @ id++
          if( $grs_list[$id] != "agcm_import" ) then
              if( -e $grs_list[$id]_checkpoint.${ionymdb}_${iohhb}00z.$RSTEXT ) then
#----
                 if (-e $ENSWORK/.DONE_RST_REGRID ) then # in case not all RSTs are cycled (that is, some are regrided)
                    @ iv = 0
                    @ found = 0
                    while ( $iv < $rstotrgd ) #&& $found ) # loop over regridded rst's
                         @ iv++
                         if ( $grs_list[$id] == $grs_regrid[$iv] ) then  # if rst in AGCM.rc is one that's been regridded ...
                            set found = 1
                         endif
                    end
                    if ( ! $found ) then # save only those really needed (not being re-gridded)
                        /bin/mv $grs_list[$id]_checkpoint.${ionymdb}_${iohhb}00z.$RSTEXT \
                                $ENSWORK/updated_ens/mem${memtag}/$expid.$grs_list[$id]_rst.${ionymdb}_${iohhb}z.$RSTEXT
                    endif
                 else # expect the full suite of RSTs to be cycling
#----
                    /bin/mv $grs_list[$id]_checkpoint.${ionymdb}_${iohhb}00z.$RSTEXT \
                            $ENSWORK/updated_ens/mem${memtag}/$expid.$grs_list[$id]_rst.${ionymdb}_${iohhb}z.$RSTEXT
                 endif # <REGRID>
              else
                 if ( $ATMENS_IGNORE_CHKPNT ) then
                    echo " ${MYNAME}: cannot finding mem${memtag}/$grs_list[$id]_checkpoint, but continuing ..."
                 else
                    echo " ${MYNAME}: trouble finding mem${memtag}/$grs_list[$id]_checkpoint, aborting ..."
                    exit(1)
                 endif
              endif
          endif
        end # <while RSTs exist>
        endif # <store checkpoint files>
        touch $ENSWORK/.DONE_UPDATE_RST_MEM${memtag}.$yyyymmddhh
     endif # <check RST update>
   end
endif # recycling of RSTs

# clean up
# --------
/bin/rm agcm_dst*
/bin/rm agcm_poe*

# made it down here, all done
# ---------------------------
touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
