GetTrajectoryTimes_(){

    # Assume: (1) This function is run in the FVWORK directory
    #         (2) Variable DateTimeDir is defined in the calling script
    # Output:
    #   TrjBegEpoch, TrjEndEpoch
    #   where Trj(Beg,End)Epoch are arrays of the form (yyyymmdd hhmmss)

    TrjBegEpoch=(22000101 000000)  # default is to never output trajectory
    TrjEndEpoch=(22000101 000000)  # default is to never output trajectory

    # Local variables
    local rstnow hh doit
    local nlagmin nlagmax nintmax lag_hrs nlag nv
    local fini_hrs lags_hrs lag_hrs this_fcst_hr int_hrs

    rstnow=($(rst_date d_rst))
    hh=$(echo ${rstnow[1]} | cut -c1-2)

    if  [ ! -e fvsens.ccmrun.namelist.tmpl ] && \
	[ ! -e fvsvec.ccmrun.namelist.tmpl ] && \
	[ ! -e fvoseledec.ccmrun.namelist.tmpl ] && \
	[ ! -e fvsens.ccmrun.namelist_${hh}.tmpl ]; then
	return 0
    fi

    doit=0
    if [ -e initadj.rc ] && [ -e fvsens.ccmrun.namelist.tmpl ]; then
	((doit++))
    fi
    if [ -e initadj.rc ] && [ -e fvsens.ccmrun.namelist_${hh}.tmpl ]; then
	((doit++))
    fi
    if [ -e  fvsvec.rc ] && [ -e fvsvec.ccmrun.namelist.tmpl ]; then
	((doit++))
    fi
    if [ -e  oseledec.rc ] && [ -e fvoseledec.ccmrun.namelist.tmpl ]; then
	((doit++))
    fi

    if [ $doit -eq 0 ]; then
	exit 0
    fi

    # # The following are calculated in DetermineGcmTimes but main script has
    # # this call placed too late for forecast applications to grad information!
    # FcsBegEpoch=${rstnow[@]}
    # if [ $ifcst -ne 0 ] || [ $DOIAU -ne 0 ]; then
    # 	FcsBegEpoch=($(tick $FcsBegEpoch $foffset_sec))  # tick clock 3hrs forward
    # fi
    # mycap=CAP.rc.tmpl
    # if [ -e CAP_${hh}.rc.tmpl ]; then
    # 	mycap=CAP_${hh}.rc.tmpl
    # fi
    # FcsLenEpoch=($(grep -i JOB_SGMT $mycap|awk '{printf "%d %d",$2, $3}'))
    # FcsLenSecs=$((${FcsLenEpoch[0]}*86400 + ${FcsLenEpoch[1]}/10000*3600))
    # if [ $ifcst -ne 0 ] || [ $DOIAU -ne 0 ]; then
    #     # subtract 3hr from actual forecast length when
    #     #   fcst started w/ 3hr offset
    # 	FcsLenSecs=$(($FcsLenSecs-$foffset_sec))
    # fi
    # FcsEndEpoch=($(tick ${FcsBegEpoch[@]} $FcsLenSecs))

    # FcsBegEpoch, FcsEndEpoch
    source $DateTimeDir/FcstEpochTimes.sh
    GetFcstEpochTimes_

    nlagmin=9999
    nlagmax=-1
    nintmax=-1
    local fn
    for fn in fvsens.ccmrun.namelist.tmpl fvsvec.ccmrun.namelist.tmpl \
	fvoseledec.ccmrun.namelist.tmpl fvsens.ccmrun.namelist_${hh}.tmpl; do
	echo "fn: $fn"
	if [ -e $fn ]; then
	    if [ -e fcstrules.rc ]; then
		fini_hrs=$(echorc.x -rc fcstrules.rc forecast_initial_hour)
		lags_hrs=$(echorc.x -rc fcstrules.rc integration_time_lag_start_hrs)
		this_fcst_hr=$(echo ${FcsBegEpoch[1]} | cut -c1-2)
		lag_hrs=-1
		nlag=${#lags_hrs[@]}
		nv = 0
		while [ $nv -lt $nlag ] # determine syn hr to start PERT integration
		do
		    ((nv++))
		    if [ "$this_fcst_hr" == "${fini_hrs[$nv]}" ]; then
			lag_hrs=${lags_hrs[$nv]}
		    fi
		done
		if [ $lag_hrs -eq -1 ]; then
		    exit 1
		    # TODO: turn on error logging
		    #
		    #Err_Log.pl -N ${EXPID}.job.${bnymd} -C 99 \
		    #	-I $ERROR_ID -X $ERROR_EXP -E 5 $ERROR_LOG_NAME \
		    #	-D "${EXPID}.job.${bnymd} FATAL ERROR: $myname cannot determine time lag for PERT integration"
		fi

                # Against all rules, edit template file and overwrite
		# it (rendering template half-way useless)
		/bin/rm -f sed_file
		/bin/mv $fn $fn.tmp
		echo "s/>>>ADMBEGTLAG<<</${lag_hrs}/1"  > sed_file # actual date to write rst
		sed -f sed_file  ./$fn.tmp   > ./$fn
	    fi

	    lag_hrs=($(echorc.x -rc $fn integration_time_lag_start_hrs))
	    int_hrs=$(echorc.x -rc $fn integration_length_hrs)
	    nlag=${#lag_hrs[@]}
	    nv=0
	    while [ $nv -lt $nlag ]; do
		if [ ${lag_hrs[$nv]} -lt $nlagmin ]; then
		    nlagmin=${lag_hrs[$nv]}
		fi
		if [ ${lag_hrs[$nv]} -gt $nlagmax ]; then
		    nlagmax=${lag_hrs[$nv]}
		fi
		((nv++))
	    done
	    if [ $int_hrs -gt $nintmax ]; then
		nintmax=$int_hrs
	    fi
	fi
    done

    echo " max int time for perturbation applications is $nintmax hrs"
    echo " max lag time for perturbation applications is $nlagmax hrs"
    echo " min lag time for perturbation applications is $nlagmin hrs"

    nlagmin=$(($nlagmin * 3600))
    nlagmax=$(($nlagmax * 3600))
    nintmax=$(($nintmax * 3600 + $nlagmax))

    # Global results
    TrjEndEpoch=($(tick ${FcsEndEpoch[@]} -$nlagmin))
    TrjBegEpoch=($(tick ${FcsEndEpoch[@]} -$nintmax))

}
