! NAME:
!      CAVH_CPI.INC
!
! DESCRIPTION:
!      Include file for avhrcl "cpids" related parameters (calibration).
!
!      Fortran 90 (free format)
!  
! OWNER:
!  Marcel Derrien
! HISTORY (MODIFICATIONS/CHANGES) 
! (version   date      comment              author)
!   1.0     09/95     initial               Marcel Derrien
!   1.1     03/04/99  ir_centwa to be real  P C Dibben

	integer nbvischn                         ! No. of vis channels for
                                                 !  spacecraft
        integer versnb                           ! Version number for the cal
                                                 !  par file
        integer versyr                           ! Version year for the cal par
                                                 !  file
        integer versdy                           ! Version day for the cal par
                                                 !  file
        double precision c1,c2                   ! Planck constants
        real conv_prt(5,4)                       ! Coefficients to be used to
                                                 !  convert counts from a PRT
                                                 !  to temperature in K
        real weight_prt(4)                       ! Weights assigned to each of
                                                 !  the PRTs
        real vis_slope(3,2),vis_int(3,2)         ! Slopes and intercepts for
                                                 !  the visible channels 
!            usage : vis_slope(chn , num) vis_int(chn , num)
!    	        	chn  1 to 3 -- num of the channel
!                       num  1 slope and intercept 1
!	                     2 slope and intercept 2
        real vis_eqwi(3),vis_isi(3),vis_ecwa(3)  ! Equivalent width solar
                                                 !  irradiance effective central
                                                 !  wavenumber (from Nechel and
                                                 !  Lab (1984) solar spectral
                                                 !  irradiance data - see NESS
                                                 !  107 appendix B)
        double precision ir_startwa(3),ir_wavinc(3) ! Starting wave and
                                                    !  increment for the IR
                                                    !  response functions
        double precision ir_nresp(60,3)          ! Response functions array
        integer it1(4),it2(4)                    ! Temperature limits (K) for 4
                                                 !  temerpature imtervals
        real    ir_centwa(3,4)                   ! Central wave numbers (cm-1)
                                                 !  for 4 temperature intervals
        integer sp_min(6),sp_max(6)              ! Space views min and max for
                                                 !  gross limit filter
                                                 !  (calibration)
        integer bb_min(3),bb_max(3)              ! Black body IR view min and
                                                 !  max for gross limit filter
        integer cth_min(4),cth_max(4)            ! Thermistors counts limits
                                                 !  min and max for gross limit
                                                 !  filter            
        integer vis_cntlim(3)                    ! Limit for visible coeff. (1
                                                 !  and 2) use
        real a_radcor(3),b_radcor(3),c_radcor(3) ! The coefficients used to
                                                 !  correct for the non-
                                                 !  linearities in the IR
                                                 !  channels
                                                 !    3 channel  3A or 3B
                                                 !    4 channel 4
                                                 !    5 channel 5
                                                 ! a_radcor for A , b_radcor for
                                                 !  B , c_radcor for C from
                                                 !  NESS107 appendix B
        real rsp(3)
        double precision avh_bandcor(3,3)        ! Band correction values
                                                 !  avh_bandcor(i,j)
                                                 !        i 1---> 3
                                                 !     1 central wavenumber
                                                 !     2 intercept to band
                                                 !       correct
                                                 !     3 slope to band correct
                                    ! t* = avh_bandcor(3,i)t + avh_bandcor(2 ,i)
                                                 !     j 1---> 3
                                                 !       channel number
! Radiance from Space to correct non-linearity (NESS107 appendix B)

        common/cavhcpids/versnb , versyr , versdy , conv_prt , weight_prt, &
                        sp_min , sp_max , bb_min , bb_max , cth_min , cth_max, &
                        nbvischn , vis_cntlim , vis_slope , vis_int , &
                        vis_eqwi , vis_isi , vis_ecwa , ir_startwa , &
                        ir_wavinc , ir_nresp , it1 , it2 , ir_centwa , &
                        a_radcor , b_radcor , c_radcor , rsp , avh_bandcor

        double precision planck1(3),planck2(3)
        common/cplanck/c1,c2,planck1,planck2
