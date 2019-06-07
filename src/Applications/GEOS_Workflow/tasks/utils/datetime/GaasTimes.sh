GetGaasTimes_(){

    # Uses global variables FVWORK, FVROOT
    # Sets global variables
    # gaasDateBeg, gaasTimeBeg

    local TICK=$FVROOT/bin/tick
    local rstdate=($($FVROOT/bin/rst_date $FVWORK/d_rst))

    local gaasDateTimeBeg=($($TICK ${rstdate[@]} 0 030000))
    gaasDateBeg=${gaasDateTimeBeg[0]}
    gaasTimeBeg=${gaasDateTimeBeg[1]}

}
