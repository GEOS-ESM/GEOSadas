TagAndRecycleGcm_(){

    # Globals used:
    # FVROOT, FVWORK, EXPID, rtag, rtags, RSTSUFFIX, RSTHOLD

    local grs_list rs

    grs_list=$($FVROOT/bin/grs_list.pl -rc $FVWORK/AGCM.rc)
    for rs in $grs_list; do
	    if [ "$rs" == "agcm_import" ] || [ "$rs" == "aiau_import" ] ; then
            continue
	    fi
	    if [ -e ${rs}_rst ]; then
            /bin/mv -f ${rs}_rst $EXPID.${rs}_rst.$rtag.$RSTSUFFIX
	        /bin/cp $EXPID.${rs}_rst.$rtag.$RSTSUFFIX $RSTHOLD/ &
	    fi
        # TODO: check final_fcst
        #if ( $final_fcst ) then
        #   /bin/cp $EXPID.${rs}_rst.$rtag.$RSTSUFFIX ${fcstage}/ &
        #fi
    done
    wait

    /bin/mv -f fvpsas.log  $EXPID.gcm.log.$rtags.txt

}

TagAndRecycleAna_(){

    # Globals used:
    # FVWORK, ACFTBIAS, EXPID, rtag, rtags, RSTHOLD, ThisScriptDir
    # ana_nymde, ana_nhmse, VAROFFSET, TIMEINC, ASYNBKG, OBSWINDOW

    # satbias/satbang/satbiaspc/acftbias
    local ars_list="satbias satbang"
    if [ $NEWRADBC -ne 0 ] || [ $ANGLEBC -ne 0 ]; then
        ars_list="$ars_list satbiaspc"
    fi
    if [ $ACFTBIAS -ne 0 ]; then
    	ars_list="$ars_list acftbias"
    fi
    for rs in $ars_list; do
	    /bin/mv $rs $EXPID.ana_${rs}_rst.$rtag.txt
	    /bin/cp $EXPID.ana_${rs}_rst.$rtag.txt $RSTHOLD/ &
    done
    if [ $ANGLEBC -ne 0 ] && [ $DIAGTAR -ne 0 ]; then
        /bin/mv radstat $EXPID.ana_radstat_rst.$rtag.tar
        /bin/cp $EXPID.ana_radstat_rst.$rtag.tar $RSTHOLD/ &
    fi
    wait

    # TODO: biasinp files??

    # bkg files
    source $ThisScriptDir/BkgFileList.sh
    GetBkgFileList_ $ana_nymde $ana_nhmse $VAROFFSET $TIMEINC $ASYNBKG $OBSWINDOW
    nbkgfiles=${#bkgsfc_lst[@]}
    n=0
    while [ $n -lt $nbkgfiles ]; do
        if [ -e ${bkgsfc_lst[$n]} ]; then
            /bin/cp     ${bkgsfc_lst[$n]}              ${bkgsfcrst_lst[$n]}  &
            /bin/cp     ${bkgsfc_lst[$n]}     $RSTHOLD/${bkgsfcrst_lst[$n]}  &
        else
            /bin/cp     ${sfcbkg_lst[$n]}              ${bkgsfcrst_lst[$n]}  &
	    /bin/cp     ${sfcbkg_lst[$n]}     $RSTHOLD/${bkgsfcrst_lst[$n]}  &
        fi
	if [ -e ${bkgupa_lst[$n]} ]; then
	    /bin/cp     ${bkgupa_lst[$n]}              ${bkguparst_lst[$n]}  &
            /bin/cp     ${bkgupa_lst[$n]}     $RSTHOLD/${bkguparst_lst[$n]}  &
        else
            /bin/cp     ${upabkg_lst[$n]}              ${bkguparst_lst[$n]}  &
            /bin/cp     ${upabkg_lst[$n]}     $RSTHOLD/${bkguparst_lst[$n]}  &
        fi
        if [ -e ${cbkgetarst_lst[$n]} ]; then
            /bin/cp     ${cbkgetarst_lst[$n]}          ${cbkgetarst_lst[$n]} &
	    /bin/cp     ${cbkgetarst_lst[$n]} $RSTHOLD/${cbkgetarst_lst[$n]} &
        else
            /bin/cp     ${cbkg_lst[$n]}                ${cbkgetarst_lst[$n]} &
            /bin/cp     ${cbkg_lst[$n]}       $RSTHOLD/${cbkgetarst_lst[$n]} &
        fi
	if [ -e ${abkgetarst_lst[$n]} ]; then
	    /bin/cp     ${abkgetarst_lst[$n]}          ${abkgetarst_lst[$n]} &
            /bin/cp     ${abkgetarst_lst[$n]} $RSTHOLD/${abkgetarst_lst[$n]} &
        else
            /bin/cp     ${abkg_lst[$n]}                ${abkgetarst_lst[$n]} &
            /bin/cp     ${abkg_lst[$n]}       $RSTHOLD/${abkgetarst_lst[$n]} &
        fi
        ((n++))
    done

    if [ $GAAS_ANA -ne 0 ]; then
	nbkgfiles=${#gaasbkg_lst[@]}
	n=0
	while [ $n -lt $nbkgfiles ]; do
	    if [ -e ${gaasbkg_lst[$n]} ]; then
		/bin/cp ${gaasbkg_lst[$n]}             ${gaasbkgrst_lst[$n]} &
		/bin/cp ${gaasbkg_lst[$n]}    $RSTHOLD/${gaasbkgrst_lst[$n]} &
	    fi
	    ((n++))
	done
    fi

    wait

    # log files
    logfiles=( \
	ana.log obs.log sobs.log trak.log vtx.log ods.log \
	sana.log cqc.log prepqc.log \
	)
    for lf in ${logfiles[@]}; do
	if [ -e $lf ]; then
            /bin/mv -f $lf $EXPID.$lf.$rtags.txt
	fi
    done

}
