GetInitialDateTime_(){

    #
    # Input arguments:
    #   $1: dir where initial restart files are staged
    #   $2: FVWORK
    #   $3: prescribed initial time, TIME0, for sanity check
    # Output:
    #   copy d_rst to FVWORK
    # No exported env vars used
    #

    if [ $# -ne 3 ]; then
        echo "Usage: GetInitialDateTime_ <stagedir> <fvwork> <TIME0>"
        return 1
    fi

    # input arguments
    local stgdir_=$1
    local fvwork_=$2
    local time0_=$3
    
    local rst_lcv_list rst_lcv_oldest itime
    
    # Initial time - pick oldest restart for initial condition
    rst_lcv_list=($(/bin/ls -1t $stgdir_/$EXPID.rst.lcv.????????_??z.bin))
    if [ $? -ne 0 ]; then
	    echo "$myname: no *.rst.lcv.* files found, nothing to do"
	    exit 1
    fi
    rst_lcv_oldest=${rst_lcv_list[0]}
    itime=$(basename ${rst_lcv_oldest} | awk -F'.' '{print $4}')

    # Sanity check
    # -------
    # TIME0 is an ecflow suite variable
    # This should be the only place where ecflow
    # and GEOS workflow scripts are tighly coupled
    # -------
    if [ "$itime" != "$time0_" ]; then
	    echo "TIME0 from ecflow is $time0_, while itime from rst.lcv file is $itime"
	    exit 1
    fi

    # Copy d_rst to FVWORK
    /bin/cp $stgdir_/$EXPID.rst.lcv.$itime.bin $fvwork_/d_rst

}
