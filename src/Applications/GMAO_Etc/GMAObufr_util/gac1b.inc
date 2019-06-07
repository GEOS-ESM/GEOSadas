! NAME:
!      GAC1B.INC
! 
! DESCRIPTION:
!      Include file for AVHRR(GAC) 1B format handling.
!
!      Fortran 90 (free format)
!  
! OWNER:
!  Marcel Derrien
! HISTORY (MODIFICATIONS/CHANGES):
! (version   date      comment            author)
!   1.0     03/95     initial            Marcel Derrien
!   2.0     07/11/95  new release from NOAA available 30/10/95
!                     still problems in integer boudaries alignments
!                     and undefined scaling factors
!   3.0     11/05/98  modified to conform to NOAA release dated 
!                     01 April 1998 (gpg # 13 for header record)
!
!           Please note that :
!            the format is NOAA one from byte 1 to 21752 )
!              the telemetry part does not conform to NOAA version
!              ( bytes 21753 to 21882 in our format)
!              it does not include the clouds from AVHRR part
! 
!
!###########################################################################
!
!               AVHRR(GAC) 1B Data Set 
!
!###########################################################################
!
!  parameters definitions

       integer, parameter :: avh_mxirchn=3,avh_mxvischn=3,avh_mxhrpchn=5  
       integer, parameter :: avh_mxpix=409           ! maximun pixels number
       integer, parameter :: reclavh=4608            ! Total number of bytes in
                                                     !  AVHRR data record
       integer, parameter :: cavh_size = reclavh/4   ! Number of 4-byte words
                                                     !  in AVHRR data record
       integer, parameter :: nwords=682              ! sensor data length
                                                     !  (number of integer*4)

!###########################################################################
!
!               HEADER RECORD
!
!###########################################################################
!
!		 GENERAL INFORMATION
!
       character*3  avh_h_siteid        ! Data set creation site ID (1-3)
       character*1  avh_h_blank         ! ASCII blank (4-4)
       integer*2    avh_h_l1bversnb     ! Level 1B format version number (5-6)
       integer*2    avh_h_l1bversyr     ! Level 1B format version year (7-8)
       integer*2    avh_h_l1bversdy     ! Level 1B format version day of year
                                        !  (9-10)
       integer*2    avh_h_reclg         ! Reserved for logical record length
                                        !  (11-12)
       integer*2    avh_h_blksz         ! Reserved for block size (13-14)
       integer*2    avh_h_hdrcnt        ! Count of header records in data set
                                        !  (15-16)
       integer*2    avh_h_filler0(3)    ! (17-22)
       character*42 avh_h_dataname      ! Data set name (23-64)
       character*8  avh_h_prblkid       ! Processing block id (65-72)
       integer*2    avh_h_satid         ! NOAA spacecraft id code (73-74)
       integer*2    avh_h_instid        ! Instrument identification (75-76)
       integer*2    avh_h_datatyp       ! Data type code (77-78)
                                        !  (1 = LAC; 2 = GAC; 3 = HRPT; 4 = TIP;
                                        !   5 = HIRS; 6 = MSU; 7 = SSU; 8 = DCS;
                                        !   9 = SEM; 10 = AMSU-A; 11 = AMSU-B)
       integer*2    avh_h_tipsrc        ! TIP source code (79-80)
                                        !  (0=not applicable; 1=GAC embedded;
                                        !   2=stored; 3=third CDA;
                                        !   4=HRPT embedded)
       integer    avh_h_startdatajd     ! Start of data set day count (81-84)
                                        !  from 00h,1 Jan 1950
       integer*2    avh_h_startdatayr   ! Start of data set year (85-86)
       integer*2    avh_h_startdatady   ! Start of data set day of year (87-88)
       integer    avh_h_startdatatime   ! Start of data set UTC time of day in
                                        !  milliseconds (89-92)
       integer    avh_h_enddatajd       ! End of data set day count (93-96)
                                        !  from 00h,1 Jan 1950
       integer*2    avh_h_enddatayr     ! End of data set year (97-98)
       integer*2    avh_h_enddatady     ! End of data set day of year (99-100)
       integer      avh_h_enddatatime   ! End of data set UTC time of day in
                                        !  milliseconds (101-104)
       integer*2    avh_h_cpidsyr       ! Year of last CPIDS update (105-106)
       integer*2    avh_h_cpidsdy       ! Day of year of last CPIDS update
                                        !  (107-108)
       integer*2    avh_h_filler1(4)    ! Filler (109-116)
!
!		 DATA SET QUALITY INDICATORS
!
       integer*4   avh_h_inststat1         ! First instrument status (117-120)
!          bit 15 : motor/telemetry       (0 = off; 1 = on) 
!          bit 14 : electronics/telemetry (0 = off; 1 = on)
!          bit 13 : channel 1  status     (0 = disable; 1 = enable)
!          bit 12 : channel 2  status     (0 = disable; 1 = enable)
!          bit 11 : channel 3A status     (0 = disable; 1 = enable)
!          bit 10 : channel 3B status     (0 = disable; 1 = enable)
!          bit  9 : channel 4  status     (0 = disable; 1 = enable)
!          bit  8 : channel 5  status     (0 = disable; 1 = enable)

       integer*2   avh_h_filler2           ! Filler (121-122)
       integer*2   avh_h_statchrecnb       ! Record number of status change
                                           !  (123-124) (if 0, none occurred)
       integer*4   avh_h_inststat2         ! Second instrument status (125-128) 
                                           !  (if previous word is 0, no change)
       integer*2   avh_h_scnlin            ! Count of scan lines in this data
                                           !  set (129-130)
       integer*2   avh_h_callocscnlin      ! Count of calibrated, earth located
                                           !  scan lines in this data set
                                           !  (131-132)
       integer*2   avh_h_misscnlin         ! Count of missing scan lines
                                           !  (133-134)
       integer*2   avh_h_datagaps          ! Count of data gaps in this data
                                           !  set (135-136)
       integer*2   avh_h_okdatafr          ! Count of data frames without frame
                                           !  sync word errors (137-138)
       integer*2   avh_h_pacsparityerr     ! Count of PACS detected TIP parity
                                           !  errors (139-140)
       integer*2   avh_h_auxsyncerrsum     ! Sum of all auxiliary sync errors
                                           !  detected in the input data
                                           !  (141-142) 
       integer*2   avh_h_timeseqerr        ! Time sequence error (143-144)
                                           !  (0=none; otherwise the record
                                           !  number of the first occurrence)
       integer*2   avh_h_timeseqerrcode    ! Time sequence error code (145-146)
       integer*2   avh_h_socclockupind     ! SOCC clock update indicator
                                           !  (147-148) (0=none during this
                                           !  orbit; otherwise the record number
                                           !  of the first occurrence)
       integer*2   avh_h_locerrind         ! Earth location error indicator
                                           !  (149-150) (0= none during this
                                           !  orbit; otherwise the record number
                                           !  of the first occurrence)
       integer*2   avh_h_locerrcode        ! Earth location error code (151-152)
       integer*2   avh_h_pacsstatfield     ! PACS status bit field (153-154)
                                           !  bit 15-3 : spare
                                           !  bit  2   : pseudo noise
                                           !     (0 = normal data; 1 = P/N data)
                                           !  bit  1   : tape direction
                                           !     (0 = time decrementing)
                                           !  bit  0   : data mode
                                           !     (0 = test data; 1= flight data)
       integer*2   avh_h_pacsdatasrc       ! PACS data source  (155-156)
                                           !   (0 = unused; 1 = Gilmore;
                                           !    2 = Wallops; 3 = SOCC)
       integer     avh_h_filler3           ! (157-160)
       character*8 avh_h_spare1            ! Spare (reserved for ingester)
                                           !  (161-168)
       character*8 avh_h_spare2            ! Spare (reserved of the
                                           !  decommutation) (169-176)
       integer*2 avh_h_filler4(5)          ! Filler (177-186)

!
!		 CALIBRATION
!
       integer*2 avh_h_racalind      ! Ramp/auto calibration indicators bit
                                     !  field (187-188)
!	   bit 5:  ramp non-linearity for GAC, LAC, and HRPT channel 5
!	   bit 4:  ramp non-linearity for GAC, LAC, and HRPT channel 4
!	   bit 3:  ramp non-linearity for GAC, LAC, and HRPT channel 3B
!	   bit 2:  ramp non-linearity for GAC, LAC, and HRPT channel 3A
!	   bit 1:  ramp non-linearity for GAC, LAC, and HRPT channel 2
!  	   bit 0:  ramp non-linearity for GAC, LAC, and HRPT channel 1

       integer*2 avh_h_solarcalyr    ! Year of most recent solar channel
                                     !  calibration
       integer*2 avh_h_solarcaldy    ! Day of year of most recent solar channel
                                     !  calibration
       integer*2 avh_h_pcalalgind    ! Primary calibration algorithm id 
       integer*2 avh_h_pcalalgopt    ! Primary calibration algorithm option
                                     !  selected 
       integer*2 avh_h_scalalgind    ! Secondary calibration algorithm id 
       integer*2 avh_h_scalalgopt    ! Secondary calibration algorithm option
                                     !  selected 
       integer*2 avh_h_irttcoef(6,4) ! IR target temperature conversion
                                     !  coefficients
       integer*4 avh_h_filler5(2)    ! (249-256)
!
!		 RADIANCE TO TEMPERATURE CONVERSION
!
       integer avh_h_albcnv(2,avh_mxvischn)      ! (257-280)
!                albedo conversion 
!                order of channels : 1, 2, 3A.
!                for nochannel = 1 to 3A :
!                  10**1 x (albedo-radiance nochannel solar filtered irradiance 
!                          in wavelength)
!                  10**3 x (albedo-radiance nochannel equivalent filter width 
!                          in wavelength)

       integer avh_h_radtempcnv(3,avh_mxirchn)   !  (281-316)
!                radiance to temperature conversion 
!                order of channels : 3B, 4, 5.
!                for nochannel = 3B to 5:
!            10**2 x (temperature-radiance nochannel central wavenumber Ch3b)
!        or  10**3 x (temperature-radiance nochannel central wavenumber Ch4,5)
!            10**5 x (temperature-radiance nochannel constant 1)
!            10**6 x (temperature-radiance nochannel constant 2)

       integer*4 avh_h_filler6(3)                ! Filler <Zero Fill> (317-328)

!		 NAVIGATION
!
       character*8 avh_h_modelid      ! (329-336)
       integer*2   avh_h_nadloctol    ! 10 x (nadir earth location tolerance in
                                      !  kilometers)
       integer*2   avh_h_locbit       ! Earth location bit field
!            bit 0 : attitude error correction (0 = not corrected)

       integer*2   avh_h_filler7

       integer*2   avh_h_rollerr      ! 10**3 x (constant roll attitude error
                                      !  in degrees)
       integer*2   avh_h_pitcherr     ! 10**3 x (constant pitch attitude error
                                      !  in degrees)
       integer*2   avh_h_yawerr       ! 10**3 x (constant yaw attitude error in
                                      !  degrees)
       integer*2   avh_h_epoyr        ! Epoch year for orbit vector
       integer*2   avh_h_epody        ! Day of epoch year for orbit vector
       integer   avh_h_epotime        ! Epoch UTC time of day in milliseconds
                                      !  for orbit vector
       integer   avh_h_smaxis         ! 10**5 x (semi-major axis in kilometers)
       integer   avh_h_eccen          ! 10**8 x (eccentricity)
       integer   avh_h_incli          ! 10**5 x (inclination in degrees)
       integer   avh_h_argper         ! 10**5 x (argument of perigee in degrees)
       integer   avh_h_rascnod        ! 10**5 x (right ascension of the
                                      !  ascending node in degrees)
       integer   avh_h_manom          ! 10**5 x (mean anomaly in degrees)
       integer   avh_h_xpos           ! 10**5 x (position vector x component in
                                      !  kilometers)
       integer   avh_h_ypos           ! 10**5 x (position vector y component in
                                      !  kilometers)
       integer   avh_h_zpos           ! 10**5 x (position vector z component in
                                      !  kilometers)
       integer   avh_h_xvel           ! 10**8 x (velocity vector x-dot component
                                      !  in kilometers/second)
       integer   avh_h_yvel           ! 10**8 x (velocity vector y-dot component
                                      !  in kilometers/second)
       integer   avh_h_zvel           ! 10**8 x (velocity vector z-dot component
                                      !  in kilometers/second)
       integer   avh_h_earthsun       ! 10**6 x (earth/sun distance ratio)
       integer*4   avh_h_filler8(4)   ! Filler <Zero Fill> (409-424)
!
!	         ANALOG TELEMETRY CONVERSION
!
!      conversion coefficients
!
! 
       integer*2 avh_h_pchtemp(5)     ! Patch temperature coef. (425-434)
       integer*2 avh_h_reserved1      ! (435-436)
       integer*2 avh_h_pchtempext(5)  ! Patch temperature extended coef.
       integer*2 avh_h_reserved2       
       integer*2 avh_h_pchpow(5)      ! Patch power coef.
       integer*2 avh_h_reserved3       
       integer*2 avh_h_rdtemp(5)      ! Radiator temperature coef.
       integer*2 avh_h_reserved4        
       integer*2 avh_h_bbtemp1(5)     ! Black body temperature 1 coef.
       integer*2 avh_h_reserved5        
       integer*2 avh_h_bbtemp2(5)     ! Black body temperature 2 coef.   
       integer*2 avh_h_reserved6        
       integer*2 avh_h_bbtemp3(5)     ! Black body temperature 3 coef. 
       integer*2 avh_h_reserved7        
       integer*2 avh_h_bbtemp4(5)     ! Black body temperature 4 coef.
       integer*2 avh_h_reserved8        
       integer*2 avh_h_eleccur(5)     ! Electronics current  coef.
       integer*2 avh_h_reserved9        
       integer*2 avh_h_motorcur(5)    ! Motor current  coef.
       integer*2 avh_h_reserved10        
       integer*2 avh_h_earthpos(5)    ! Earth shield position coef.
       integer*2 avh_h_reserved11       
       integer*2 avh_h_electemp(5)    ! Electronics temperature  coef.
       integer*2 avh_h_reserved12       
       integer*2 avh_h_chtemp(5)      ! Cooler housing  temperature coef. 
       integer*2 avh_h_reserved13       
       integer*2 avh_h_bptemp(5)      ! Baseplate temperature coef.
       integer*2 avh_h_reserved14       
       integer*2 avh_h_mhtemp(5)      ! Motor housing temperature  coef.
       integer*2 avh_h_reserved15        
       integer*2 avh_h_adcontemp(5)   ! A/D converter temperature  coef.
       integer*2 avh_h_reserved16        
       integer*2 avh_h_d4bvolt(5)     ! Detector #4 bias voltage  coef.
       integer*2 avh_h_reserved17        
       integer*2 avh_h_d5bvolt(5)     ! Detector #5 bias voltage  coef.
       integer*2 avh_h_reserved18        
       integer*2 avh_h_bbtempchn3B(5) ! Black body temperature channel 3B coef.
       integer*2 avh_h_reserved19        
       integer*2 avh_h_bbtempchn4(5)  ! Black body temperature channel 4  coef.
       integer*2 avh_h_reserved20        
       integer*2 avh_h_bbtempchn5(5)  ! Black body temperature channel 5 coef.
       integer*2 avh_h_reserved21        
       integer*2 avh_h_refvolt(5)     ! Reference voltage coef.
       integer*2 avh_h_reserved22     ! (687-688)   

!
!		 FILLER
!
       integer*2 avh_h_filler9(1960) ! Filler bytes to equal data record length
                                     !  (689-4608)
       integer avh_hd(cavh_size)
       
       equivalence ( avh_hd , avh_h_siteid )
       
       common/gac1bhd/ avh_h_siteid , avh_h_blank , avh_h_l1bversnb , &
                       avh_h_l1bversyr , avh_h_l1bversdy , avh_h_reclg , &
                       avh_h_blksz , avh_h_hdrcnt , avh_h_filler0 , &
                       avh_h_dataname , avh_h_prblkid , avh_h_satid , & 
                       avh_h_instid , avh_h_datatyp , avh_h_tipsrc , & 
                       avh_h_startdatajd , avh_h_startdatayr , &
                       avh_h_startdatady , avh_h_startdatatime , &
                       avh_h_enddatajd , avh_h_enddatayr , avh_h_enddatady , &
                       avh_h_enddatatime , avh_h_cpidsyr , avh_h_cpidsdy , &  
                       avh_h_filler1 , avh_h_inststat1 , avh_h_filler2 , & 
                       avh_h_statchrecnb , avh_h_inststat2 , avh_h_scnlin , &
                       avh_h_callocscnlin , avh_h_misscnlin , avh_h_datagaps , &
                       avh_h_okdatafr , avh_h_pacsparityerr , &
                       avh_h_auxsyncerrsum , avh_h_timeseqerr , & 
                       avh_h_timeseqerrcode , avh_h_socclockupind , &
                       avh_h_locerrind , avh_h_locerrcode , &
                       avh_h_pacsstatfield  , avh_h_pacsdatasrc , &
                       avh_h_filler3 , avh_h_spare1 , avh_h_spare2 , &
                       avh_h_filler4 , avh_h_racalind , avh_h_solarcalyr , &
                       avh_h_solarcaldy , avh_h_pcalalgind, avh_h_pcalalgopt , &
                       avh_h_scalalgind , avh_h_scalalgopt , avh_h_irttcoef , &
                       avh_h_filler5 , avh_h_albcnv , avh_h_radtempcnv , &
                       avh_h_filler6 , avh_h_modelid , avh_h_nadloctol , &
                       avh_h_locbit, avh_h_filler7 , avh_h_rollerr , &
                       avh_h_pitcherr , avh_h_yawerr , avh_h_epoyr , &
                       avh_h_epody , avh_h_epotime , avh_h_smaxis , &
                       avh_h_eccen , avh_h_incli , avh_h_argper , &
                       avh_h_rascnod , avh_h_manom , avh_h_xpos , avh_h_ypos , &
                       avh_h_zpos , avh_h_xvel , avh_h_yvel , avh_h_zvel , &
                       avh_h_earthsun , avh_h_filler8 , avh_h_pchtemp , &
                       avh_h_reserved1 , avh_h_pchtempext , avh_h_reserved2 , &
                       avh_h_pchpow , avh_h_reserved3 , avh_h_rdtemp , & 
                       avh_h_reserved4 , avh_h_bbtemp1 , avh_h_reserved5 , &
                       avh_h_bbtemp2 , avh_h_reserved6 , avh_h_bbtemp3 , &
                       avh_h_reserved7 , avh_h_bbtemp4 , avh_h_reserved8 , &
                       avh_h_eleccur , avh_h_reserved9 , avh_h_motorcur , & 
                       avh_h_reserved10 , avh_h_earthpos , avh_h_reserved11 , &
                       avh_h_electemp , avh_h_reserved12 , avh_h_chtemp , &
                       avh_h_reserved13  , avh_h_bptemp , avh_h_reserved14 , & 
                       avh_h_mhtemp , avh_h_reserved15 , avh_h_adcontemp , & 
                       avh_h_reserved16 , avh_h_d4bvolt , avh_h_reserved17 , &
                       avh_h_d5bvolt , avh_h_reserved18 , avh_h_bbtempchn3B , &
                       avh_h_reserved19 , avh_h_bbtempchn4 , &
                       avh_h_reserved20 , avh_h_bbtempchn5 , &
                       avh_h_reserved21 , avh_h_refvolt , avh_h_reserved22 , &
                       avh_h_filler9 

!
!##############################################################################
!
!                DATA RECORD 
!
!##############################################################################
!
!	         SCAN LINE INFORMATION 
!
       integer*2 avh_scnlin	 ! Scan line number (1-2)
       integer*2 avh_scnlinyr	 ! Scan line year   (2-4)
       integer*2 avh_scnlindy	 ! Scan line day of year  (5-6)
       integer*2 avh_clockdrift	 ! Satellite clock drift delta in millisec (7-8)
       integer   avh_scnlintime	 ! Scan line UTC time of day in millisec (9-12)
       integer*2 avh_scnlinbit	 ! Scan line bit field  (13-14)
!                bit 15 : (0 = ascending data; 1 = descending data)
!                bit 14 : (1 = scan time corrected for clock drift)
!                bit 13 : (1 = earth location corrected for TIP attitude)
!                bit  0 : channel 3 select (0 = 3A; 1 = 3B)

       integer*2 avh_filler0(5)  ! <Zero Fill> (15-24)
!
!                QUALITY INDICATORS
!    
       integer avh_qualind 	           ! Quality indicator bit field (25-28)
!                bit 31 : (1 = do not use data for product generation)
!                bit 30 : (1 = time sequence error detected in this frame)
!                bit 29 : (1 = data gap precedes this frame)
!                bit 28 : (1 = insufficient data for calibration)
!                bit 27 : (1 = earth location data not available)
!                bit 26 : (1 = sync lock dropped during this frame)
!                bit 25 : (1 = frame sync word error greater than zero)
!                bit 24 : (1 = frame sync previously dropped lock)
!                bit 23 : (1 = flywheeling detected during this frame)
!                bit 22 : (1 = bit slippage detected during this frame)
!                bit  9 : tip parity in first  minor frame
!                bit  8 : tip parity in second minor frame
!                bit  7 : tip parity in third  minor frame
!                bit  6 : tip parity in fourth minor frame
!                bit  5 : tip parity in fifth  minor frame
!                bit  4 : (1 = reflected sunlight detected channel 3B)
!                bit  3 : (1 = reflected sunlight detected channel 4)
!                bit  2 : (1 = reflected sunlight detected channel 5)
!                bit  1 : (1 = resync occured on this frame)
!                bit  0 : (1 = pseudo noise occured on this frame)

       integer avh_scnlinqual              ! Scan line quality flags (29-32)
!
!                Time Problem Code
!
!                bit 31-24 : spare
!                bit 23 : time field is bad but can probably be inferred from 
!                         the previous good time
!                bit 22 : time filed is bad and can't be inferred from
!                         the previous good time
!                bit 21 : this record starts a sequence that is inconsistent 
!                         with previous time (i.e., there is a time
!                         discontinuity).this may or may not associated
!                         with a spacecraft clock update.(see bit 26 above)
!                bit 20 : start of a sequence that apparently repeats scan times
!                         that have been previously accepted.
!                bit 19-16 : spare
!
!                Calibration Problem Code
!
!                (all bits set to 0 indicates normal calibration)
!                bit 15 : scan line was not calibred because of bad time
!                bit 14 : scan line was calibred using fewer than the 
!                         preferred number of scan lines because of proximity
!                         to start or end of data set or to a data gap
!                bit 13 : scan line was not calibred because of bad or
!                         insufficient PRT data
!                bit 12 : scan line was calibred but with marginal PRT data
!                bit 11 : some uncalibred channels on this scan 
!                         (see channel indicators)
!                bit 10 : spare
!                bit 09 : spare
!                bit 08 : spare
!
!                Earth location Problem Code
!
!                (all bits set to 0 implies the earth location was normal)
!                bit 07 : not earth located because of bad time
!                bit 06 : earth location questionable because of 
!                         questionable time code 
!                         (see time problem flags above)
!                bit 05 : earth location questionable -- only marginal
!                          agreement with reasonableness check    
!                bit 04 : earth location questionable -- fails 
!                         reasonnableness check
!                bit 03-00 : spare 

       integer*2 avh_calqual(avh_mxirchn)  ! Calibration quality flags (33-38)
!                order of channels : 3B, 4, 5.
!                (all bits off implies a good calibration)
!                bit 7 : this channel is not calibred
!                bit 6 : this channel is calibred but questionable
!                bit 5 : not good blackboody count for scan line
!                bit 4 : not good space view counts for scan line
!                bit 3 : insufficient PRT data
!                bit 2 : some bad blackboody view counts for this line
!                bit 1 : some bad space view counts for this line
!                bit 0 : some bad PRT data

       integer*2 avh_cbiterr	           ! Count of bit errors in frame sync
                                           !  (39-40)
       integer*4 avh_filler1(2)	           ! Filler (41-48)
!
!	         CALIBRATION COEFFICIENTS
!	
       integer avh_calvis(5,3,avh_mxvischn)                ! (49-228)
!        first index
!                1: operational set
!                2: test set
!                3: prelaunch set
!        second index
!                1   10^10 x (solar calibration nochannel slope 1)
!                2   10^7 x (solar calibration nochannel intercept 1)
!                3   10^10 x (solar calibration nochannel slope 2, active)
!                4   10^7 x (solar calibration nochannel intercept 2)
!                5   solar calibration nochannel  intersection
!       third index 
!                order of channels : 1, 2, 3A.
!                for nochannel = 1 to 3A :

       integer avh_calir(3,2,avh_mxirchn) !IR calibration  ! (229-300)
!                order of channels : 3B, 4, 5.
!     first index:
! 
!               1 coefficient 1
!               2 coefficient 2
!               3 coefficient 3
!     second index:
!	        1 operational
!               2 prelaunch 
!     third index 
!		channel number 
!          we get              
!                for nochannel = 3B to 5 :
!                   10**6 x (IR calibration nochannel coeff. 1, operational)
!                   10**6 x (IR calibration nochannel coeff. 2, operational)
!                   10**6 or 10**7 x (IR calibration nochannel coeff. 3,
!                                     operational)
!                   10**6 x (IR calibration nochannel coefficient 1, prelaunch)
!                   10**6 x (IR calibration nochannel coefficient 2, prelaunch)
!                   10**6 x (IR calibration nochannel coefficient 3, prelaunch)
!
           integer*4 avh_filler2(3) ! filler 	<Zero Fill> (301-312)
!
!	         NAVIGATION
!
       integer*4 avh_navstat    ! Navigation Status Bit Field (313-316)
!             bits 31-17: zero fill
!             bit 16: earth location corrected for TIP Euler Angles
!             bits 15 - 12: earth location indicator 
!                     (0 = earth location available; 
!                      1 = user ephemeris files greater than 24 hours old;
!                      2 = no earth location available)
!             bits 11 - 8: spacecraft attitude control 
!                     (0 = operating inYGC or NOMINAL mode;
!                      1 = operating in another mode;
!                      2 = attitude exceeds nominal tolerance;
!                      3 = both 1 and 2)
!            bits 7 - 4: attitude SMODE 
!                     (0 = NOMINAL mode;
!                      1 = rate nulling mode;
!                      2 = YGC mode; 
!                      3 = search mode;
!                      4 = coast mode)
!            bits 3 - 0: attitude PWT1P$AC 
!                     (0 = NOMINAL mode/no test; 
!                      1 = yaw axis test in progress;
!                      2 = roll axis test in progress; 
!                      3 = pitch axis test in progress)       

       integer avh_attangtime   ! Time associated with TIP Euler angles
                                !  (seconds) (317-320)
       integer*2 avh_rollang    ! 10**3 x roll angle in degrees (321-322)
       integer*2 avh_pitchang   ! 10**3 x pitch angle in degrees (323-324)
       integer*2 avh_yawang     ! 10**3 x yaw angle in degrees (325-326)
       integer*2 avh_scalti     ! 10 x spacecraft altitude (MSL) in km in this
                                !  scan line (327-328)    
       integer*2 avh_ang(3,51)  ! Set of 3 angles in degrees for point 25 to
                                !  point 2025 every 40 points (329-634)
                                !  first angle : 10**2 x(solar zenith angle)
                                !  second angle: 10**2 x(sat zenith angle)
                                !  third angle : 10**2 x(relative azimuth angle)
       integer*2 avh_filler3(3) ! (635-640)
       integer*4 avh_pos(2,51)  ! Lat/lon pair in degrees for point 25 to point
                                !  2025 every 40 points (641-1048)
                                !  first  : 10**4 x (latitude )
                                !  second : 10**4 x (longitude )
                                !  (North lat and East lon are positive)

       integer*4 avh_filler4(2) ! Filler (1049-1056)
!
!	         HRPT MINOR FRAME TELEMETRY (103 integer*2 words)
!

       integer*2 avh_telem(103)     ! Telemetry data (HRPT minor frame format)
                                    !  it corresponds to the 103 first 10 bit
                                    !  words from HRPT. They are packed three
                                    !  (10 bit) words in four bytes, right
                                    !  justified. (1057-1262)
       integer*2 avh_filler5        ! <Zero Fill> (1263-1264)

!	         AVHRR SENSOR DATA
!                      Sensor Data, Band Interleaved by Pixel (BIP)
       integer*4 avh_hrpt(nwords)   ! (1265-3992) nwords = 682
                                    !  set of 5 channels every point
       integer*4 avh_filler6(2)     ! Filler (3993-4000)
!
!                 Digital B TELEMETRY
!
       integer*2 avh_bitflag1       ! Digital B Telemetry (4001-4002)
!      Invalid Word Bit Flags
       integer*2 avh_dbdata         ! AVHRR Digital B Data (4003-4004)
       integer*4 avh_filler7(3)     ! Filler (4005-4016)

!
!                 ANOLOG HOUSEKEEPING DATA (TIP)
!
       integer*4 avh_bitflag2       ! Invalid Word Bit Flags ( 4017-4020)
!
! Xu Li, 2005/12/13: fix a bug in declaration of avh_instemp, this may affect
!                    CLAVR decode
!
!      integer*1 avh_instemp        ! (4021-4042)
       integer*1 avh_instemp(22)    ! (4021-4042)
       integer*2 avh_filler8(3)     ! Filler (4043-4048)
!
!                 CLOUD FROM AVHRR (CLAVR)
!
       integer*4 clv_bit             ! CLAVR Bit Field (4049-4052)
       integer*4 clv_rev             ! Reserved (4053-4056)
       integer*2 ccmc(52)            ! CCM(Clear/Cloud/Mixed) Codes (4057-4160)
       integer*4 avh_filler9(112)    ! Filler (4161-4608)

       integer avh_dt(cavh_size)

       common/gac1bdt/ avh_scnlin , avh_scnlinyr , avh_scnlindy , &
                       avh_clockdrift , avh_scnlintime , avh_scnlinbit , &
                       avh_filler0 , avh_qualind , avh_scnlinqual , &
                       avh_calqual , avh_cbiterr , avh_filler1 , avh_calvis , &
                       avh_calir , avh_filler2 , avh_navstat , &
                       avh_attangtime , avh_rollang , avh_pitchang , &
                       avh_yawang , avh_scalti , avh_ang , avh_filler3 , &
                       avh_pos , avh_filler4 ,  avh_telem , avh_filler5 , &
                       avh_hrpt , avh_filler6 , avh_bitflag1, &
                       avh_dbdata,avh_filler7, avh_bitflag2,avh_instemp, &
                       avh_filler8,  clv_bit, clv_rev, ccmc, avh_filler9

!!!!!  equivalence (avh_scnlin,avh_dt(1))
       equivalence (avh_dt,avh_scnlin)
