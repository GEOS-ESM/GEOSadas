#!/bin/csh

# gcm_ensrst_wrap - handle GCM output for give ensemble member
#
# !REVISION HISTORY:
#
#  22Jun2022  Todling   Extracted from gcm_ensemble.csh
#-------------------------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME gcm_ensrst_wrap.csh

if ( $#argv < 4 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME"
   echo " "
   echo "  $MYNAME  - handle restarts at end of GCM run"
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME  expid nymd nhms memdir"
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  date of restarts, as in YYYYMMDD "
   echo "   nhms   -  time of restarts, as in YYYYMMDD "
   echo "   memdir -  member dir name (e.g., mem001, or ensctrl)"
   echo " "
   echo " DESCRIPTION "
   echo " "
   echo "   This procedure renames checkpoint restarts at end of GCM run."
   echo " "
   echo " Example of valid command line:"
   echo " $MYNAME b541iau 20091018 210000 mem001"
   echo " "
   echo " REQUIRED RESOURCE FILES"
   echo " "
   echo "   AGCM.rc.tmpl     - defines specific restarts, and GCM parameters"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    FVWORK        - location of work directory        "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo " REMARKS"
   echo " "
   echo " SEE ALSO"
   echo "   gcm_ensemble.csh - calculation of IAU increments"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 22Jun2020      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0

if ( !($?FVWORK)) setenv FAILED 1

if ( !($?ATMENS_IGNORE_CHKPNT)) setenv ATMENS_IGNORE_CHKPNT 0
if ( !($?RSTEXT)              ) setenv RSTEXT bin

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid   = $1 
set ionymdb = $2
set ionhmsb = $3
set memdir  = $4
set iohhb = `echo $ionhmsb | cut -c1-2`

setenv ENSWORK $FVWORK

     # Get positioned
     # --------------
     cd $ENSWORK/$memdir

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
                                $ENSWORK/updated_ens/${memdir}/$expid.$grs_list[$id]_rst.${ionymdb}_${iohhb}z.$RSTEXT
                    endif
                 else # expect the full suite of RSTs to be cycling
#----
                    /bin/mv $grs_list[$id]_checkpoint.${ionymdb}_${iohhb}00z.$RSTEXT \
                            $ENSWORK/updated_ens/${memdir}/$expid.$grs_list[$id]_rst.${ionymdb}_${iohhb}z.$RSTEXT
                 endif # <REGRID>
              else
                 if ( $ATMENS_IGNORE_CHKPNT ) then
                    echo " ${MYNAME}: cannot finding ${memdir}/$grs_list[$id]_checkpoint, but continuing ..."
                 else
                    echo " ${MYNAME}: trouble finding ${memdir}/$grs_list[$id]_checkpoint, aborting ..."
                    exit(1)
                 endif
              endif
          endif
        end # <while RSTs exist>

     endif # <store checkpoint files>
exit(0)
