WriteTimefile4Fcst_(){

    # Input arguments:
    #   timefile, fvwork, fvroot
    # Output:
    #   write times to fvwork/timefile
    if [ $# -ne 5 ]; then
	    echo -n "Usage: "
	    echo "WriteTimefile4Fcst_ <TIMEFILE> <FVWORK> <FVROOT> <FVHOME> <ifcst>"
	    return 1
    fi
    local timefile_=$1
    local fvwork_=$2
    local fvroot_=$3
    local fvhome_=$4
    local ifcst_=$5
    if [ -e $timefile_ ]; then
	    echo "File [$timefile_] already exists. Aborting..."
	    return 1
    fi
    touch $timefile_

    local mydrst RST_DATE rstdate gcm_nymd0 gcm_nhms0

    # Shorthands
    mydrst=$fvwork_/d_rst
    RST_DATE=$fvroot_/bin/rst_date
    TICK=$fvroot_/bin/tick
    caprc=$fvhome_/fcst/CAP.rc.tmpl

    # begin date/time of forecast
    fcst_beg=($($RST_DATE $mydrst))

    # end date/time of forecast
    nd_hh=($(grep -i JOB_SGMT $caprc|awk '{printf "%d %d",$2, $3}'))
    nsecs=$((${nd_hh[0]}*86400 + ${nd_hh[1]}/10000*3600))
    fcst_end=($($TICK ${fcst_beg[@]} $nsecs))

    # foffset_sec
    if [ $ifcst_ -ne 0 ]; then
	    if [ $FCSTOFFSET -ne 0 ]; then
	        foffset_sec=$((FCSTOFFSET*60))
	    else
	        foffset_sec=10800 # default is 3 hrs due to IAU
	    fi
    else
	    foffset_sec=$FCSTOFFSET
    fi

    # Write to file
    echo "# Forecast times" >> $timefile_
    echo "fcst_beg_nymd: ${fcst_beg[0]}" >> $timefile_
    echo "fcst_beg_nhms: ${fcst_beg[1]}" >> $timefile_
    echo "fcst_end_nymd: ${fcst_end[0]}" >> $timefile_
    echo "fcst_end_nhms: ${fcst_end[1]}" >> $timefile_
    echo "fcst_nsecs: $nsecs" >> $timefile_
    echo "foffset_sec: $foffset_sec" >> $timefile_

}

WriteTimefile4DAS_(){

    # TODO: split into two - (1) DAS times (2) Acquire times

    # Arguments
    # timefile, fvwork, fvroot, timeinc, varoffset, obswindow
    if [ $# -ne 6 ]; then
	    echo -n "Usage: "
	    echo "WriteTimefile4DAS_ <TIMEFILE> <FVWORK> <FVROOT> <TIMEINC> <VAROFFSET> <OBSWINDOW>"
	    return 1
    fi
    local timefile=$1
    local fvwork=$2
    local fvroot=$3
    local timeinc=$4
    local varoffset=$5
    local obswindow=$6
    if [ -e $timefile ]; then
	    echo "File [$timefile] already exists. Aborting..."
	    return 1
    fi
    touch $timefile

    # Shorthands
    local mydrst=$fvwork/d_rst
    local TICK=$fvroot/bin/tick
    local RST_DATE=$fvroot/bin/rst_date
    local aoffset_sec=$(($varoffset*60))

    #-Segment-times-start------------

    local nymd1g nhms1g
    local nymd1 nhms1

    read nymd1g nhms1g <<< $($RST_DATE $mydrst)
    read nymd1 nhms1 <<< $($TICK $nymd1g $nhms1g $aoffset_sec)

    local gcm_nymd0 gcm_nhms0
    local gcm_nymdb gcm_nhmsb
    local ana_nymde ana_nhmse
    local nymdb nhmsb
    local rstdate this_seg_end

    local anafreq=$timeinc
    local afreqhr=$(( $anafreq/60 )) # ana freq in hours
    local afreqsc=$(( $anafreq*60 )) # ana freq in seconds
    
    rstdate=($($RST_DATE $mydrst))
    # begin date/time/hour of forecast
    gcm_nymd0=${rstdate[0]}
    gcm_nhms0=${rstdate[1]}
    if [ $varoffset -ne 0 ]; then
	    # gcm initial date/time for next cycle
	    this_seg_end=($($TICK ${rstdate[@]} ${afreqsc}))
	    gcm_nymdb=${this_seg_end[0]}
	    gcm_nhmsb=${this_seg_end[1]}
	    # first synoptic date/time within var window
	    rstdate=($($TICK $gcm_nymd0 $gcm_nhms0 $aoffset_sec))
	    nymdb=${rstdate[0]}
	    nhmsb=${rstdate[1]}
	    # next analysis date/time
	    this_seg_end=($($TICK ${rstdate[@]} $afreqsc))
	    ana_nymde=${this_seg_end[0]}
	    ana_nhmse=${this_seg_end[1]}
    else
	    # gcm initial date/time for next cycle
	    nymdb=$gcm_nymd0
	    nhmsb=$gcm_nhms0
	    this_seg_end=$($TICK ${rstdate[@]} $afreqsc)
	    ana_nymde= ${this_seg_end[0]}
	    ana_nhmse= ${this_seg_end[1]}
	    gcm_nymdb= $ana_nymde
	    gcm_nhmsb= $ana_nhmse
    fi

    # Write to file
    echo "# Segment times" >> $timefile
    echo "nymd1: $nymd1" >> $timefile
    echo "nhms1: $nhms1" >> $timefile
    echo "gcm_nymd0: $gcm_nymd0" >> $timefile
    echo "gcm_nhms0: $gcm_nhms0" >> $timefile
    echo "gcm_nymdb: $gcm_nymdb" >> $timefile
    echo "gcm_nhmsb: $gcm_nhmsb" >> $timefile
    echo "ana_nymde: $ana_nymde" >> $timefile
    echo "ana_nhmse: $ana_nhmse" >> $timefile
    echo "nymdb: $nymdb" >> $timefile
    echo "nhmsb: $nhmsb" >> $timefile

    #-Segment-times-end--------------

    #-Restart-times-start------------

    local wrt_rst_nymd wrt_rst_nhms
    local wrt_fcs_now
    local wrt_fcs_nymd wrt_fcs_nhms

    wrt_rst_nymd=$gcm_nymdb
    wrt_rst_nhms=$gcm_nhmsb
    wrt_fcs_now=($($TICK $gcm_nymd0 $gcm_nhms0  $aoffset_sec))
    wrt_fcs_nymd=${wrt_fcs_now[0]}
    wrt_fcs_nhms=${wrt_fcs_now[1]}

    # Write to file
    echo "" >> $timefile
    echo "# Restart times" >> $timefile
    echo "wrt_rst_nymd: $wrt_rst_nymd" >> $timefile
    echo "wrt_rst_nhms: $wrt_rst_nhms" >> $timefile
    echo "wrt_fcs_nymd: $wrt_fcs_nymd" >> $timefile
    echo "wrt_fcs_nhms: $wrt_fcs_nhms" >> $timefile

    #-Restart-times-end--------------

    #-Trajectory-times-start---------

    #-Trajectory-times-end-----------

    #-Acquire-times-start------------

    # nsteps
    local obstwindow_hrs=$(( $obswindow/60 ))
    local varwindow_hrs=$(( $timeinc/60 ))
    local ntv=$(( $varwindow_hrs/$obstwindow_hrs ))
    local nsteps=$(( 24/$obstwindow_hrs + $ntv - 1 ))
    if [ $nsteps -gt 4 ]; then nsteps=4; fi # restrict, if needed

    # bnymd/bnhms
    local bnymd=$nymd1
    local bnhms=$nhms1

    # enymd
    if [ ! -f $FVWORK/replay.acq ]; then
        # trick to get nhmsa w/o ref to fvpsasdt
        local buf=($(tick 20010101 000000 $afreqsc))
        local nhmsa=${buf[1]#0} # "#0" removes leading zero
    else
        nhmsa=$(( $afreqhr*10000 ))
    fi
    local nsecs=$(( ($nhmsa/10000)*3600 ))
    local esecs=$(( $nsteps*$nsecs ))
    local enymd=($(tick $nymd1g $nhms1g $esecs))    
    
    # Write to file
    echo "" >> $timefile
    echo "# Acquire times" >> $timefile
    echo "acq_nsteps: $nsteps" >> $timefile
    echo "acq_bnymd: $bnymd" >> $timefile
    echo "acq_bnhms: $bnhms" >> $timefile
    echo "acq_enymd: ${enymd[0]}" >> $timefile

    #-Acquire-times-end--------------

    echo "Finished writing [$timefile]"

}
