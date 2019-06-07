GetBkgFileList_(){

    #
    # Input arguments:
    #   $1: date
    #   $2: time
    #   $3: varoffset
    #   $4: timeinc
    #   $5: asynbkg
    #   $6: obswindow
    #
    # Output:
    #   bkg_dateb - date/time of last bkg files within this cycle
    #   sfcbkg_lst
    #   upabkg_lst
    #   bkgsfc_lst
    #   bkgupa_lst
    #   cbkg_lst
    #   chembkg_lst
    #   abkg_lst
    #   bkgsfcrst_lst
    #   bkguparst_lst
    #   abkgetarst_lst
    #   cbkgetarst_lst
    #   gaasbkg_lst
    #   gaasbkgrst_lst
    #

    if [ $# -ne 6 ]; then
	echo "Usage: GetBkgFileList_ date time varoffset timeinc asynbkg obswindow"
	return 1
    fi

    # input arguments
    local nymd_g2a_=$1
    local nhms_g2a_=$2
    local varoffset_=$3
    local timeinc_=$4
    local asynbkg_=$5
    local obswindow_=$6

    # shorthands
    local anatime=$timeinc_
    local anatime_hrs=$(($anatime/60))
    local dtasyn_hrs=$(($asynbkg_/60))
    local aoffset_hrs=$(($varoffset_/60))
    local obstwindow_hrs=$(($obswindow_/60))
    local varwindow_hrs=$(($timeinc_/60))

    # bkgbits0/f
    local bkgbits0 bkgbitsf
    if [ $aoffset_hrs -eq 0 ]; then
	bkgbits0=$(($anatime_hrs-$obstwindow_hrs/2))
	bkgbitsf=$(($varwindow_hrs+$obstwindow_hrs/2))
    else
	bkgbits0=$(($anatime_hrs-$aoffset_hrs))
	bkgbitsf=$(($varwindow_hrs+$aoffset_hrs))
    fi
    local bkgbits0_sec=$(($bkgbits0*3600))

    #
    # build list of bkg filenames within this assimilation cycle
    #

    sfcbkg_lst=()
    upabkg_lst=()
    bkgsfc_lst=()
    bkgupa_lst=()
    cbkg_lst=()
    chembkg_lst=()
    abkg_lst=()
    bkgsfcrst_lst=()
    bkguparst_lst=()
    abkgetarst_lst=()
    cbkgetarst_lst=()
    gaasbkg_lst=()
    gaasbkgrst_lst=()

    local nbkg bkgbits fhr myname

    nbkg=0
    bkgbits=$bkgbits0
    bkg_dateb=($(tick $nymd_g2a_ $nhms_g2a_ -$bkgbits0_sec))
    local bkg_dtsec=$(($asynbkg_*60))

    while [ $bkgbits -le $bkgbitsf ]; do

        fhr=$(echo $bkgbits |awk '{printf "%02d", $1}')

        myname=$(echorc.x -template $EXPID ${bkg_dateb[0]} ${bkg_dateb[1]} sfcrst_bkg${fhr}_filename)
        bkgsfcrst_lst+=($myname)

        myname=$(echorc.x -template $EXPID ${bkg_dateb[0]} ${bkg_dateb[1]} uparst_bkg${fhr}_filename)
        bkguparst_lst+=($myname)

        myname=$(echorc.x -template $EXPID ${bkg_dateb[0]} ${bkg_dateb[1]} aerorst_bkg${fhr}_filename)
        abkgetarst_lst+=($myname)

        myname=$(echorc.x -template $EXPID ${bkg_dateb[0]} ${bkg_dateb[1]} chemrst_bkg${fhr}_filename)
        cbkgetarst_lst+=($myname)

        myname=$(echorc.x -template $EXPID ${bkg_dateb[0]} ${bkg_dateb[1]} surface_bkg_filename)
        sfcbkg_lst+=($myname)

        myname=$(echorc.x -template $EXPID ${bkg_dateb[0]} ${bkg_dateb[1]} upper-air_bkg_filename)
        upabkg_lst+=($myname)

        myname=$(echorc.x -template $EXPID ${bkg_dateb[0]} ${bkg_dateb[1]} surface_bkg${fhr}_filename)
        bkgsfc_lst+=($myname)

        myname=$(echorc.x -template $EXPID ${bkg_dateb[0]} ${bkg_dateb[1]} upper-air_bkg${fhr}_filename)
        bkgupa_lst+=($myname)

        myname=$(echorc.x -template $EXPID ${bkg_dateb[0]} ${bkg_dateb[1]} chem_bkg${fhr}_filename)
        chembkg_lst+=($myname)

        myname=$(echorc.x -template $EXPID ${bkg_dateb[0]} ${bkg_dateb[1]} aero_bkg_filename)
        abkg_lst+=($myname)

        myname=$(echorc.x -template $EXPID ${bkg_dateb[0]} ${bkg_dateb[1]} chem_bkg_filename)
        cbkg_lst+=($myname)

        if [ $bkgbits -ne $bkgbits0 ]; then
            myname=$(echorc.x -template $EXPID ${bkg_dateb[0]} ${bkg_dateb[1]} gaas_bkg_filename)
            gaasbkg_lst+=($myname)

            myname=$(echorc.x -template $EXPID ${bkg_dateb[0]} ${bkg_dateb[1]} gaasrst_bkg_filename)
            gaasbkgrst_lst+=($myname)
        fi

	# increment
        bkg_dateb=($(tick ${bkg_dateb[0]} ${bkg_dateb[1]} $bkg_dtsec))
        bkgbits=$(($bkgbits+$dtasyn_hrs))
        ((nbkg++))

    done # < while

    # date/time of last bkg files within this cycle
    bkg_dateb=($(tick ${bkg_dateb[0]} ${bkg_dateb[1]} -$bkg_dtsec))

}
