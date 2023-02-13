#!/bin/csh

# gcm_ensemble - set up resources to run member of the ensemble of atmospheric GCMs
#
# !REVISION HISTORY:
#
#  22Jun2020  Todling   Split from gcm_ensemble.csh
#-------------------------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME gcm_ensset_rc.csh

if ( $#argv < 7 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME"
   echo " "
   echo "  $MYNAME  - run multiple copies of (atmospheric) GCM "
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME  expid nymd nhms tfcst nlons nlats member"
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  initial date of forecast, as in YYYYMMDD "
   echo "   nhms   -  initial time of forecast, as in HHMMSS "
   echo "   tfcst  -  forecast length in hours"
   echo "   nlons  -  number of longitudes in bkg (im) (for history)"
   echo "   nlats  -  number of latitudes  in bkg (jm) (for history)"
   echo "   member -  member name, e.g., mem002, or ensctrl"
   echo " "
   echo " DESCRIPTION "
   echo " "
   echo "   This procedure prepares the resource files needed to run"
   echo "   an instance of the atmospheric GCM. "
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
   echo " $MYNAME b541iau 20091018 210000 12 144 91 mem001"
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
   echo "    FVBCS         - location of fvInput               "
   echo "    FVHOME        - location of experiment            "
   echo "    FVROOT        - location of DAS build             "
   echo "    FVWORK        - location of work directory        "
   echo "    TIMEINC       - analysis frequency (minutes)      "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMGEPS        - trigger for GEOS EPS              "
   echo "    NCSUFFIX       - suffix of hdf/netcdf files (default: nc4)"
   echo "    ATMENS_DO4DIAU - trigger to run 4DIAU "
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
   echo "   gcm_ensemble.csh  - runs ensemble of GCMs"
   echo "   atmos_ens2gcm.csh - calculation of IAU increments"
   echo "   atm_ens_geps.j    - main job script controlling GEPS"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 21Jun2020      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

if ( !($?ATMGEPS)       ) setenv FAILED 1
if ( !($?ATMENSETC)     ) setenv FAILED 1
if ( !($?ATMENS_DO4DIAU)) setenv FAILED 1
if ( !($?ASYNBKG)       ) setenv FAILED 1
if ( !($?FVBCS)         ) setenv FAILED 1
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1
if ( !($?FVWORK)        ) setenv FAILED 1
if ( !($?HYBRIDGSI)     ) setenv FAILED 1
if ( !($?LINK_RST)      ) setenv FAILED 1
if ( !($?TIMEINC)       ) setenv FAILED 1

if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4
if ( !($?RSTEXT)        ) setenv RSTEXT bin

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid  = $1
set nymdb  = $2
set nhmsb  = $3
set tfcst  = $4
set nlons  = $5
set nlats  = $6
set member = $7
set hhb    = `echo $nhmsb | cut -c1-2`
set yyyymmddhh = ${nymdb}${hhb}

set mempfx = `echo $member | cut -c1-3`
if ( $mempfx == "mem" ) then
  set memidx = `echo $member | cut -c4-`
else
  set memidx = 99999
endif

setenv ENSWORK $FVWORK
if (-e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo " ${MYNAME}: already done"
   exit(0)
endif
setenv EXPID $expid  # this variable is usually defined, but make sure since vED needs it!

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

@ bkgfreq_hr  =  $ASYNBKG / 60
@ bkgfreq_mn  =  $ASYNBKG - $bkgfreq_hr * 60
set bkgfreq_hh = `echo $bkgfreq_hr |awk '{printf "%02d", $1}'`
set bkgfreq_mm = `echo $bkgfreq_mn |awk '{printf "%02d", $1}'`
set bkgfreq_hhmn = ${bkgfreq_hh}${bkgfreq_mm}
set bkgfreq_nhms = ${bkgfreq_hhmn}00

cd $ENSWORK/$member

        # For now, only IAU rst is different all other rst's same
        # -------------------------------------------------------
        ln -sf agcm_import_${member}_rst agcm_import_rst

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
                     /bin/ln -sf $HYBRIDGSI/${member}/$expid.$grs_list[$id]_rst.${nymdb}_${hhb}z.$RSTEXT $grs_list[$id]_rst
                 endif
             else # expect the full suite of RSTs to be cycling
                 if ( "$LINK_RST" == "$HYBRIDGSI" ) then
                    /bin/ln -sf $LINK_RST/${member}/$expid.$grs_list[$id]_rst.${nymdb}_${hhb}z.$RSTEXT $grs_list[$id]_rst
                 else
                    /bin/ln -sf $LINK_RST/$expid.$grs_list[$id]_rst.${nymdb}_${hhb}z.$RSTEXT $grs_list[$id]_rst
                 endif
             endif # <REGRID>
          endif
        end

        # All of the RCs for GOCART and what not ... TBD: this mambo-jambo needs attention
        # --------------------------------------------------------------------------------
        /bin/ln -sf $ENSWORK/ExtData $ENSWORK/${member}/
        /bin/cp $ATMENSETC/GEOS_ChemGridComp.rc .
        /bin/cp $FVHOME/run/gocart/*.rc         .
        /bin/cp $FVHOME/run/gocart/*.yaml       .
        if(-e ExtData.rc) /bin/rm -f ExtData.rc
        set  extdata_files = `/bin/ls -1 *_ExtData.rc`
        cat $extdata_files > $ENSWORK/${member}/ExtData.rc
        if ( ${nymdb}${hhb} >= 2021103021 ) then
           foreach line (`grep -ni qfed $ENSWORK/${member}/ExtData.rc | gawk '{print $1}' FS=":"`)
             sed -i "${line}s/.006./.061./" $ENSWORK/${member}/ExtData.rc
           end
        endif

        /bin/mv WSUB_ExtData.rc WSUB_ExtData.tmp
        cat WSUB_ExtData.tmp | sed -e '/^WSUB_NATURE/ s#ExtData.*#/dev/null#' > WSUB_ExtData.rc
        /bin/rm WSUB_ExtData.tmp


	if(-e $ATMENSETC/aens_stoch.rc ) /bin/ln -sf  $ATMENSETC/aens_stoch.rc $ENSWORK/${member}/stoch.rc

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
            setenv FVWORK    $ENSWORK/${member}
            setenv MEMTAG    $member
            foreach file ( $FVHOME/run/gaas/*.rc )
                set target = $ENSWORK/${member}/$file:t
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

        # A bit of a hack to cope with latest handing of GAAS settings
        # ------------------------------------------------------------
        foreach fn (`ls $EXPID.aod*.$NCSUFFIX`)
          set sfx = `echo $fn | cut -d. -f2-`
          ln -sf $fn das.$sfx 
        end
 
        # FV-core layout file
        # -------------------
        if ( -e $ATMENSETC/fvcore_layout.rc ) then
           /bin/cp $ATMENSETC/fvcore_layout.rc .
        else
           /bin/cp $FVHOME/run/fvcore_layout.rc .
        endif
        /bin/cp $FVHOME/run/*.yaml .
        /bin/cp fvcore_layout.rc input.nml

        /bin/cp $FVHOME/run/GEOS_SurfaceGridComp.rc .

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
         echo "s/>>>MEMBER<<</${member}/1"   >> sed_file
         echo "s/>>>MEMIDX<<</${memidx}/1"   >> sed_file
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
         echo "s/>>>BKGFREQ<<</${bkgfreq_nhms}/1"  >> sed_file
         echo "s/>>>MEMBER<<</${member}/1"         >> sed_file
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

        #  Run bundleParser.py
        #  -------------------
        python bundleParser.py
        construct_extdata_yaml_list.py ./GEOS_ChemGridComp.rc

        # Link in BCS
        # -----------
        lnbcs_ens $nymdb

cd -
