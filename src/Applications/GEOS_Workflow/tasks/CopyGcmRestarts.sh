CopyGcmRestarts_(){

    #
    # Input argument:
    #   $1: location of stage/recycle directories
    #   $2: initial condition time, e.g. 19990901_18z

    # Assumptions:
    # (1) grs_list.pl and rst_date are in PATH

    if [ $# -ne 2 ]; then
        echo "Usage: CopyGcmRestarts_ </path/to/stage> <itime>"
        return 1
    fi

    local grs_list grs_boot
    local rs rsbase rsfile
    local STGDIR itime

    STGDIR=$1
    itime=$2

    # restarts
    grs_list=$(grs_list.pl -rc $FVWORK/AGCM.rc.tmpl -flg 1)
    grs_boot=$(grs_list.pl -rc $FVWORK/AGCM.rc.tmpl -flg 2)
    for rs in ${grs_list[@]}
    do
	rsbase=$STGDIR/$EXPID.${rs}_rst.$itime
	rsfile=$rsbase.bin
	if [ ! -e $rsfile ]; then
	    rsfile=$rsbase.nc4
	fi
        # TODO: Check this logic
	if [ ! -e $rsfile ]; then
	    if [[ "${grs_boot[@]}" == *"$rs"* ]]; then
		continue
	    fi
	fi
	/bin/cp $rsfile $FVWORK/${rs}_rst &
    done
    wait

    # cap_restart
    /bin/rm -f $FVWORK/cap_restart
    rst_date $FVWORK/d_rst > $FVWORK/cap_restart

}
