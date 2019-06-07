GetFcstEpochTimes_(){

    # Assume: this function is run in the FVWORK directory
    # Output:
    #   FcsBegEpoch, FcsEndEpoch
    #   where Fcs(Beg,End)Epoch are date/time arrays of the form (yyyymmdd, hhmmss)

    # Local variables
    local rstnow TIMEFILE foffset_sec mycap
    local FcsLenEpoch FcsLenSecs

    rstnow=($(rst_date d_rst))
    TIMEFILE=simtimes.yaml
    foffset_sec=$(grep "^foffset_sec:" $TIMEFILE | cut -d":" -f2 | tr -d " ")

    FcsBegEpoch=(${rstnow[@]})
    if [ $ifcst -ne 0 ]; then
	# force first 6-hr of fcst w/ IAU inc
	# tick clock forward by 3 hrs
	FcsBegEpoch=($(tick ${FcsBegEpoch[@]} $foffset_sec))
    fi

    mycap=CAP.rc.tmpl
    hh=$(echo ${rstnow[1]} | cut -c1-2)
    if [ -e CAP_${hh}.rc.tmpl ]; then
	mycap=CAP_${hh}.rc.tmpl
    fi
    FcsLenEpoch=($(grep -i JOB_SGMT $mycap|awk '{printf "%d %d",$2, $3}'))
    FcsLenSecs=$((${FcsLenEpoch[0]}*86400 + ${FcsLenEpoch[1]}/10000*3600))

    if [ $ifcst -ne 0 ]; then
        # subtract 3hr from actual forecast length when
        # fcst started w/ 3hr offset
	FcsLenSecs=$(( $FcsLenSecs-$foffset_sec ))
    fi

    FcsEndEpoch=($(tick ${FcsBegEpoch[@]} $FcsLenSecs))

}
