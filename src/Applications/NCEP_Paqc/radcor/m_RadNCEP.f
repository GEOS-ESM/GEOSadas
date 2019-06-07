!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOI
! !MODULE: m_RadNCEP -- Implements the NCEP radiation bias correction tables.
!
! !DESCRIPTION:
!     This module implements the radiation bias correction tables
!     as described in Collins (1999) and in the preprocessing source
!     code developed at NCEP.  The computed temperature ($T_Cor$) and
!     height ($Z_Cor$) corrections are given so $T_2 = T_1 - T_Cor$ and
!     $Z_2 = Z_1 - Z_Cor$ where $T_1$ and $T_2$ are the temperatures
!     before and after the corrections and $Z_1$ and $Z_2$ are similarly
!     defined for heights.       
!
! !REFERENCES:
!    \begin{description}
!    \item Collins, William G., 1999: Determination of new adjustment
!        tables in order to bring radiosonde temperature and height
!         measurements from different sonde types into relative
!         agreement.,  NCEP/EMC.  Web site: http://wwwt.emc.ncep.noaa.gov/
!         mmb/papers/collins/new_tables/new_tables.html

!    \end{description}
!
! !INTERFACE:
      module      m_RadNCEP
      implicit    NONE
      private	! except

      public ::
     .   ncepTab,         ! NCEP's lookup table
     .   ncepAlg,         ! ... and algorithm.
     .   SunAlt           ! NCEP's formulae for sun angle computations
      public ::           ! Integer codes denoting ...
     .   ncep5            ! ... the NCEP table, version 5

      interface SunAlt
         module procedure
     .      SunAlt_,      ! sun angle computations with ...
     .      SunAlt_noDr   ! ... without balloon drift data.
      end interface
!
! !REVISION HISTORY:
!     28Nov2003  C. Redder  Original code
!     30Mar2004  C. Redder  Added the public procedure ncepAlg and
!                           the generic interface, SunAlt.
!     07Jul2004  C. Redder  Added comments in module prologue
!EOI
!-----------------------------------------------------------------

!     List of mandatory pressure levels in the lookup tables
!     ------------------------------------------------------
      integer, parameter ::
     .   NP_Mand = 16   ! Number of mandatory pressure levels
      real,    parameter ::
     .    P_Mand ( NP_Mand ) = (/  1000.0,  925.0,  850.0,  700.0,
     .                              500.0,  400.0,  300.0,  250.0,
     .                              200.0,  150.0,  100.0,   70.0,
     .                               50.0,   30.0,   20.0,   10.0  /)

!     No bias corrections at and below this level
!     -------------------------------------------
      real, parameter::
     .   Pres_Cutoff = 700.0 * ( 1.0 + 10.0e-5 ) ! hPa

!     Codes for table versions
!     ------------------------
      integer, parameter ::
     .   ncep5 = 5

!     Type for real with the necessary precision
!     ------------------------------------------
      integer,     parameter ::
     .   HP    = selected_real_kind (12)

      contains

!...................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: ncepAlg () --- Equivalent implementation of NCEPS temperature bias correction algorithm.
!
! !DESCRIPTION:
!     This routine implements an temperature bias correction algorithm
!     that is equivalent the NCEP's scheme.
!
! !REFERENCES:
!
! !INTERFACE:
      subroutine ncepAlg ( rkx, DLat, DLon, P, SynTime, LTime, DETime,
     .                          TCor, ZCor )
      use m_soundings, only : logP2Y, CalcZ, P2List
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,       intent (in)  :: 
     .   rkx         ! WMO code for instrument type
      real,          intent (in)  ::
     .   DLat   (:), ! Balloon drift latitude (deg)
     .   DLon   (:), ! ... and longitude (deg, -90=90W)
     .   P      (:)  ! Pressure level (in hPa)
      integer,       intent (in)  ::
     .   SynTime,    ! Synoptic time (in YYYYMMDDHH format)
     .   LTime       ! Balloon launch time since synoptic time (s)
      real,          intent (in)  ::
     .   DETime (:)  ! Balloon drift time since launch (s)
!
! !OUTPUT PARAMETERS:
      real,          intent (out) ::
     .   TCor   (:), ! Temperature correction (in deg K or C)
     .   ZCor   (:)  ! Height correction (m)
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     25Mar2004  C. Redder  Original version
!                           
! EOP
!-------------------------------------------------------------------------

      real, parameter :: PMin = Pres_Cutoff
      integer  :: iLev, NLev, iP_Mand, Loc_, Len_, iLev_Top, iVersion
      real     :: P_, P_Bottom, P_Top
      integer, dimension ( NP_Mand ) :: Loc, Len
      real,    dimension ( NP_Mand ) :: SE_Mand, TCor_Mand, ZCor_Mand,
     .                                 DET_Mand, DLon_Mand, DLat_Mand

      iVersion = ncep5

!     Inde
!     ----
      NLev = min ( size ( DLon ),
     .             size ( DLat ),
     .             size ( P ),
     .             size ( DETime ),
     .             size ( TCor ),
     .             size ( ZCor ))

!     Indentify mandatory pressure levels within the given levels
!     -----------------------------------------------------------
      call P2List  ( P, P_Mand, Loc, Len )

!     Get drift information at mandatory levels
!     -----------------------------------------
      call logP2Y  ( P, DLon,   P_Mand, DLon_Mand )
      call logP2Y  ( P, DLat,   P_Mand, DLat_Mand )
      call logP2Y  ( P, DETime, P_Mand, DET_Mand  )

!     Calculate the solar angle
!     -------------------------
      call SunAlt_  ( DLon_Mand, DLat_Mand,
     .                SynTime,   LTime, DET_Mand,  SE_Mand )

!     Set temp corrections from NCEP's look up table (ver 5)
!     ------------------------------------------------------
      call ncepTab ( rkx,       P_Mand, SE_Mand, TCor_Mand,
     .              iVer = iVersion )

!     Determine the height corrections
!     --------------------------------
      call CalcZ   ( P_Mand, TCor_Mand,          ZCor_Mand )

!     Determine height corrections below/above
!     the lowest/highest mandatory pressure level
!     -------------------------------------------
      P_Bottom = P_Mand ( 1 )
      P_Top    = P_Mand ( NP_Mand )
      do iLev  = 1, NLev
         P_            = P ( iLev )
         TCor ( iLev ) = 0.0
         if ( P_ .ge. P_Bottom ) TCor ( iLev ) = TCor_Mand ( 1 )
         if ( P_ .le. P_Top    ) TCor ( iLev ) = TCor_Mand ( NP_Mand )
      end do
      call CalcZ   ( P, TCor, ZCor )

!     Apply correction at mandatory pressure levels
!     for temperature and heights
!     ---------------------------------------------
      iLev_Top = 0
      do iP_Mand = 1, NP_Mand
         Len_ = Len ( iP_Mand )
         Loc_ = Loc ( iP_Mand )
         iLev_Top = Loc_ + Len_ - 1
         if      ( Len_ .eq. 1 ) then
            TCor    ( Loc_ ) = TCor_Mand ( iP_Mand )
            ZCor    ( Loc_ ) = ZCor_Mand ( iP_Mand ) + ZCor ( Loc_ )
         else if ( Len_ .gt. 1 ) then
            do iLev = Loc_, iLev_Top
               TCor ( iLev ) = TCor_Mand ( iP_Mand )
               ZCor ( iLev ) = ZCor_Mand ( iP_Mand ) + ZCor ( iLev )
            end do
         end if
      end do

      do iLev = iLev_Top + 1, NLev
         ZCor ( iLev ) = ZCor ( iLev ) + ZCor ( iP_Mand )
      end do

      return
      end subroutine ncepAlg

!...................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: SunAlt_ () --- Compute the sun angle from NCEP's most recent formulae
!
! !DESCRIPTION:
!
! !REFERENCES:
!
      subroutine SunAlt_ ( DLat, DLon, SynTime, LTime, DETime, SE )
      use m_SunAlt, only : Julian
      implicit NONE
!
! !INPUT PARAMETERS:
      real,          intent (in)  ::
     .   DLat   (:), ! Balloon drift latitude (deg)
     .   DLon   (:)  ! ... and longitude (deg, -90=90W)
      integer,       intent (in)  ::
     .   SynTime,    ! Synoptic time (in YYYYMMDDHH format)
     .   LTime       ! Balloon launch time since synoptic time (s)
      real,          intent (in)  ::
     .   DETime (:)  ! Balloon drift time since launch (s)
!
! !OUTPUT PARAMETERS:
      real,          intent (out) ::
     .   SE     (:)  ! Solar elevation angle (deg)
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     30Mar2004  C. Redder   Original code
!                           
! EOP
!-------------------------------------------------------------------------

      integer :: JDay0, DayYr, Year, Hour, NDaysYr, iPt, NPts
      real ( kind = HP ) :: DAngl, SDAngl, CDAngl, TSNoon, pi, Deg2Rad,
     .                      DrHour, DrHour0, AFY, SAFY, CAFY, DrLon,
     .                      ADrLon, SHA, DrLat, SE_, SSOD, RLat 

!      Need exception handling for the case when the time is missing
!      Put it in another routine?      
!     -------------------------------------------------------------

!     Compute the day of the year (=DayYr), hour of the
!     day (=Hour), and the number of days in the year (=NDaysYr)
!     ----------------------------------------------------------
      Year    =          SynTime / 1000000
      Hour    = mod    ( SynTime,  100 ) 
      JDay0   = Julian ( Year    * 10000   + 0101 )
      DayYr   = Julian ( SynTime / 100   ) - JDay0 + 1
      NDaysYr = Julian ( Year    * 10000 + 1231 ) - JDay0 + 1

      pi      = 2.0_HP * atan2 ( 1.0_HP, 0.0_HP )
      Deg2Rad = pi / 180.0_HP

      DAngl   = 2.0_HP * pi * ( real ( DayYr,   kind = HP ) - 79.0_HP )
     .                        / real ( NDaysYr, kind = HP )
      SDAngl  = sin ( DAngl )
      CDAngl  = cos ( DAngl )
      TSNoon  = -0.0300_HP * SDAngl
     .        -  0.1200_HP * CDAngl
     .        +  0.3300_HP * SDAngl * CDAngl
     .        +  0.0016_HP * SDAngl **2
     .        -  0.0008_HP

      DrHour0 = real ( Hour,  kind = HP )
     .        + real ( LTime, kind = HP ) / 3600.0_HP
      NPts    = min ( size ( DLon ),
     .                size ( DLat ),
     .                size ( DETime ),
     .                size ( SE ))
      do iPt = 1, NPts
         DrHour = DrHour0 + DETime ( iPt ) / 3600.0_HP
         DrLon  =           DLon   ( iPt )
         DrLat  =           DLat   ( iPt )

!        Compute angular fraction of year (=AFY)
!        ----------------------------------------
         AFY    = ( 2.0_HP * pi / real ( NDaysYr, kind = HP ))
     .          * ( real ( DayYr, kind = HP ) - 1.0_HP
     .                                        + ( DrHour / 24.0_HP ))
         SAFY   = sin ( AFY )
         CAFY   = cos ( AFY )

!        Compute the sine of solar declination (=SSOD)
!        ---------------------------------------------
         SSOD   = 0.3978492_HP
     .          * sin ( 4.885780_HP 
     .                + AFY
     .                + 0.033420_HP *   SAFY
     .                - 0.001388_HP *   CAFY 
     .                + 0.000696_HP *   SAFY     * CAFY
     .                + 0.000028_HP * ( SAFY **2 - CAFY**2 ))
         ADrLon = mod ( 720.0_HP + 360.0_HP - DrLon, 360.0_HP )

!        Compute solar hour angle (=SHA)
!        -------------------------------
         SHA    = Deg2Rad * (( 15.0_HP
     .                    *  ( TSNoon + DrHour + 36.0_HP ))
     .                    -    ADrLon )
         RLat   = Deg2Rad * DrLat
         SE_    = asin ( SSOD * sin ( RLat )
     .                 + sqrt ( 1.0_HP - SSOD**2 )
     .                 * cos  ( RLat)  * cos ( SHA ))
         SE ( iPt )    = real ( SE_ / Deg2Rad )
      end do

      return
      end subroutine SunAlt_

!...................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: SunAlt_noDr () --- Compute the sun angle from NCEP's formulae with no balloon drift information 
!
! !DESCRIPTION:
!
! !REFERENCES:
      subroutine SunAlt_noDr ( Lat, Lon, SynTime, DHR, SE )
      use m_SunAlt, only : Julian
      implicit NONE
!
! !INPUT PARAMETERS:
      real,             intent (in)  ::
     .   Lat,           ! Balloon drift latitude (deg)
     .   Lon            ! ... and longitude (deg, -90=90W)
      integer,          intent (in)  ::
     .   SynTime,       ! Synoptic time (in YYYYMMDDHH format)
     .   DHR            ! Observation time since synoptic time (s)
!
! !OUTPUT PARAMETERS:
      real,             intent (out) ::
     .   SE ( NP_Mand ) ! Solar elevation angle (deg)
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     30Mar2004  C. Redder   Original code
!                           
! EOP
!-------------------------------------------------------------------------

      real :: SunBA
      real, dimension (2) :: DLat, DLon, DETime, SE_Dr

      DLat = Lat
      DLon = Lon
      DETime ( 1 ) = DHR - 0.2 * 3600.0
      DETime ( 2 ) = DHR + 1.3 * 3600.0

      call SunAlt_ ( DLat, DLon, SynTime, DHR, DETime, SE_Dr )

      SunBA = SE_Dr ( 2 ) - SE_Dr ( 1 ) ! For between 700 and 10 mb
                                        !  NOTE: SE_Dr(1)-SE_Dr (3) are
                                        !  crude est. at 1000, 925 & 850 mb
      SE (  1 ) = SE_Dr ( 1 ) - ( 0.0826254 * SunBA )
      SE (  2 ) = SE_Dr ( 1 ) - ( 0.0647634 * SunBA )
      SE (  3 ) = SE_Dr ( 1 ) - ( 0.0456353 * SunBA )
      SE (  4 ) = SE_Dr ( 1 ) + ( 0.0000000 * SunBA )
      SE (  5 ) = SE_Dr ( 1 ) + ( 0.0791979 * SunBA )
      SE (  6 ) = SE_Dr ( 1 ) + ( 0.1317209 * SunBA )
      SE (  7 ) = SE_Dr ( 1 ) + ( 0.1994348 * SunBA )
      SE (  8 ) = SE_Dr ( 1 ) + ( 0.2423491 * SunBA )
      SE (  9 ) = SE_Dr ( 1 ) + ( 0.2948721 * SunBA )
      SE ( 10 ) = SE_Dr ( 1 ) + ( 0.3625860 * SunBA )
      SE ( 11 ) = SE_Dr ( 1 ) + ( 0.4580233 * SunBA )
      SE ( 12 ) = SE_Dr ( 1 ) + ( 0.5419766 * SunBA )
      SE ( 13 ) = SE_Dr ( 1 ) + ( 0.6211745 * SunBA )
      SE ( 14 ) = SE_Dr ( 1 ) + ( 0.7414114 * SunBA )
      SE ( 15 ) = SE_Dr ( 1 ) + ( 0.8308487 * SunBA )
      SE ( 16 ) = SE_Dr ( 1 ) + ( 1.0000000 * SunBA )
 
      return
      end subroutine SunAlt_noDr
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: ncepTab () --- Lookup tables for NCEP temparature bias correction
!
! !DESCRIPTION:
!
! !REFERENCES:
!
! !INTERFACE:
      subroutine ncepTab ( rkx, P, SE, TCor, iVer, PCutoff )
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,    intent (in) ::
     .   rkx      ! WMO code for radiosonde instrument type
      real,       intent (in),  dimension (:) ::
     .   P,       ! Pressure level (in hPa, assumed to be sorted
     .            !   in descending order)
     .   SE       ! Solar elevation angle (in deg).
      integer,    intent (in),  optional ::
     .   iVer     ! Integer code for the NCEP table ...
                  !   = ncep5        - ... version 5
                  ! Notes: Any invalid code is set to the default.
                  !   The constants are defined in module header.
      logical,    intent (in),  optional ::
     .   PCutoff  ! = .true. to set temperature corrections to zero
                  !  if pressure level is below the cutoff.  Default:
                  !  PCutOff = .false.
!
! !OUTPUT PARAMETERS:
      real,       intent (out), dimension (:) ::
     .   TCor     ! Temperature corrections
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     28Nov2003  C. Redder   Original code.
!     24May2004  C. Redder   Add the optional argument PCutoff
! EOP
!.................................................................
      integer :: iVersion
      logical :: use_cutoff

!     Implement options
!     -----------------
      use_cutoff = .true.
      if ( present ( PCutOff )) use_cutoff = PCutoff

      iVersion = ncep5
      if ( present ( iVer )) iVersion = iVer
      if      ( iVersion .eq. ncep 5 ) then
         call ncepTab5 ( rkx, use_cutoff, P, SE, TCor )
      else
         call ncepTab5 ( rkx, use_cutoff, P, SE, TCor )
      end if

      return
      end subroutine ncepTab
!...................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: ncepTab5 () --- Lookup table for NCEP temparature bias correction, version 5
!
! !DESCRIPTION:
!     This routine corrects the temperature for short and longwave
!     radiations effects on the radionsonde thermister using the
!     NCEP lookup table, version 5.  The radiation correction tables
!     are based upon observed minus forecast increments from an NWP
!     assimilation.  These increments are adjusted so that the 
!     corrections for the Vaisala RS80 digicora sonde become zero.
!
! !REFERENCES:
!
! !INTERFACE:
      pure subroutine ncepTab5 ( rkx, use_cutoff, P, SE, TCor )
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,        intent (in) :: 
     .   rkx          ! WMO code for instrument type
      logical,        intent (in) ::
     .   use_cutoff   ! = .true. to use 700 mb pressure cutoff  
      real,           intent (in),  dimension (:) ::
     .   P,           ! Pressure level (in hPa)
     .   SE           ! Solar elevation angle (in deg).
!
! !OUTPUT PARAMETERS:
      real,           intent (out), dimension (:) ::
     .   TCor         ! Temperature correction (in deg K or C)
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     28Nov2003  C. Redder   Original code, adapted from the NCEP subroutine,
!                            RADT5
!     29Jan2004  C. Redder   Split correction tables to make code portable
!                            on SGI
!     24Mar2004  C. Redder   Removed optional output argument, iTab.
!     24May2004  C. Redder   Added the required argument, use_cutoff.
!                           
! EOP
!-------------------------------------------------------------------------

      real, parameter :: PLowestLev = Pres_Cutoff

      integer, parameter ::
     .   NT      = 28,      ! Number of tables
     .   NP      = NP_Mand, ! Number of columns for pressure levels
     .   NSE     =  5,      ! ... and rows for solar elevation angles
     .                      !     in the lookup table
     .   TableSz = NP * NSE ! Size of each correction table

!     ITable assigns proper temperature correction table to inst. types.
!     -----------------------------------------------------------------
      integer :: I
      integer, parameter :: ITables ( 1 : 99 ) =    
     .!  rkx   =  1   2   3   4   5   6   7   8   9  10  11  12 
     .       (/  -1, -1, -1, -1, -1, -1, -1, -1, -1,  2,  3,  4,
     .
     .!          13  14  15  16  17  18  19  20  21  22  23  24
     .            0,  0,  0,  0,  0,  0,  0,  5,  6,  7,  0,  0,
     .
     .!          25  26  27  28  29  30  31  32  33  34  35  36
     .            0,  8,  9, 10, 11,  0,  0, 12,  0,  0,  0,  0,
     .
     .!          37  38  39  40  41  42  43  44  45  46  47  48
     .           13,  0,  0,  0,  0,  0, 14, 15, 15, 16, 17,  0,
     .
     .!          49  50  51  52  53  54  55  56  57  58  59  60
     .           18, 19, 20, 21,  0, -1, -1, -1, -1, -1, -1, 22,

     .!          61  62  63  64  65  66  67  68  69  70  71  72
     .            0, 24, 25,  0,  0, -1, -1, -1, -1, -1, 26, -1,
     .
     .!          73  74  75  76     77  - 89          90  -  96
     .           -1, -1, -1, 27, ( -1, I = 1, 13 ), ( 0, I = 1, 7),
     .
     .!          97  98  99
     .           -1, -1, -1 /)

!     Cut off angles at each level.
!     ----------------------------
!        The cutoff angle (CO) at each level is calculated using the
!        relation:
!             CO  = -1.76459 * (Z**.40795) ,
!        where Z is a reference height for the level (in kilometers)
!        (See NMC Office Note 306, Table 4 for list of reference heights.)
       real,    parameter ::
     .   CutOff ( NP ) = (/    -0.73,  -1.58,  -2.06,  -2.77,
     .                         -3.56,  -3.95,  -4.36,  -4.58,
     .                         -4.83,  -5.12,  -5.49,  -5.79,
     .                         -6.06,  -6.43,  -6.72,  -7.17 /)
      integer, parameter ::
     .   rkx_Min = lbound ( ITables, Dim = 1 ),
     .   rkx_Max = ubound ( ITables, Dim = 1 )

!     Pressure values
!     --------------- 
      real,    parameter ::
     .   HP     ( NP ) = P_Mand,
     .
     .! ... and sun angles
     .! ------------------
     .   HSE  ( NSE  ) = (/   -90.0,  -10.0,   15.0,   30.0,  60.0 /)

     .! ... for the temperature correction tables (in tenths of deg C)
     .! -------------------------------------------------------------- 
     .! This set of tables is modified to insert reasonable numbers
     .!    for entries with 0 or small count.  Data are for all types 
     .!    available for 1999.  Values are smoothed with a single
     .!    pass of weighted 1-2-1 smoother in the vertical.  Values
     .!    are increment differences between given type and Vaisala
     .!    RS-80 Digicora (type 61).

      real, parameter :: Table_IType_9  ( TableSz ) = (/
     .!
     .!      UNKNOWN TYPE  ( IType = 9 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa)
     .    .9,   .1, -7.0, -8.4,-16.6, ! 1000
     .  -1.2,  -.7, -2.1,  -.7, -3.5, !  925
     .    .3,  2.1,   .6,  2.6,  2.2, !  850
     .   1.7,  3.4,   .8,  2.5,  3.0, !  700
     .   2.3,  3.3,   .7,  2.1,   .0, !  500
     .   2.5,  3.7,  2.9,  3.3,  -.1, !  400
     .   3.0,  4.6,  6.4,  5.7,  3.1, !  300
     .   3.4,  5.9,  7.0,  6.8,  3.9, !  250
     .   1.5,  6.7,  3.1,  4.2,  -.7, !  200
     .  -2.0,  6.7, -1.9, -1.1, -8.8, !  150
     .  -3.9,  2.0, -3.4, -4.3,-14.5, !  100
     .  -2.6, -6.2,  2.2,   .5,-10.2, !   70
     .   -.7, -7.4,  8.9,  7.9,  3.6, !   50
     .   -.4,  -.5, 13.3, 11.4,  5.2, !   30
     .   1.2,  7.8, 18.6, 10.7, -1.3, !   20
     .   9.5, 15.3, 21.2, 10.0, 10.0  !   10
     . /)

      real, parameter :: Table_IType_10 ( TableSz ) = (/
     .!
     .! (U.S.) NOAA / VIZ TYPE  A ( IType = 10 )
     .! ----------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa)
     .   3.7, -9.9, -8.2,-14.9,   .5, ! 1000
     .   -.1,  -.7,  -.3, -1.1,  7.1, !  925
     .    .9,  3.0,  2.4,  3.1,  5.5, !  850
     .   1.5,  2.2,  1.7,  5.3,  1.2, !  700
     .    .0,   .0,  -.6,  5.3,   .0, !  500
     .  -1.3,  -.1,  -.4,  6.3,  1.3, !  400
     .   -.5,  1.9,  2.4,  9.4,  4.3, !  300
     .   1.5,  1.7,  1.5, 11.8,  6.3, !  250
     .   3.0, -1.3, -4.6, 10.9,  5.3, !  200
     .    .9, -1.2,-10.9,  6.5,   .9, !  150
     .  -3.1,   .1, -9.3,  4.3, -3.0, !  100
     .  -1.3, -1.7,  3.4,  8.7,  3.7, !   70
     .   1.9,  1.2, 14.0, 14.5, 11.0, !   50
     .   3.4,  5.5, 21.1, 20.3, 11.4, !   30
     .  13.3,  7.5, 25.8, 22.5,   .9, !   20
     .  27.5, 28.0, 29.0, 22.5, 22.5  !   10
     . /)

      real, parameter :: Table_IType_11 ( TableSz ) = (/
     .!
     .! (U.S.) NOAA / VIZ TYPE  B ( IType = 11 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa)
     .  -9.5,  2.9,  1.6,-22.5,-22.5, ! 1000
     .  -5.1, -1.8,  1.2, -1.9, -1.9, !  925
     .  -2.9, -1.2,   .6,  1.6,  1.6, !  850
     .  -2.1, -1.0,  -.1,  -.4,  -.4, !  700
     .  -2.1, -1.6,  -.9,  2.1,  2.1, !  500
     .  -2.2,  -.7,   .1,  4.6,  4.6, !  400
     .   -.8,  1.7,  2.8,  3.1,  3.1, !  300
     .    .0,  1.0,  3.4,  2.1,  2.1, !  250
     .  -1.5, -2.7,   .8, -2.5, -2.5, !  200
     .  -3.4, -2.5,  1.7, -3.0, -3.0, !  150
     .  -4.0, -1.0,  6.7,   .4,   .4, !  100
     .  -3.3,   .3,  9.3, -5.0, -5.0, !   70
     .  -3.5,  2.8, 13.4, -1.6, -1.6, !   50
     .  -6.3,  4.9, 20.0, 20.4, 20.4, !   30
     .  -8.9,  8.2, 24.8, 34.6, 34.6, !   20
     .  -8.9, 10.0, 26.2, 34.6, 34.6  !   10
     . /)

      real, parameter :: Table_IType_12 ( TableSz ) = (/
     .!
     .! (U.S.) NOAA / SPACE DATA CORP. ( IType = 12 )
     .! ---------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa)
     .  -7.4,-28.8,-34.4,-35.8,-35.8, ! 1000
     .   1.3,-13.4, -3.5,  3.6,  3.6, !  925
     .    .6, -4.3,  1.2,  3.2,  3.2, !  850
     .   -.8,  3.2, -2.1,  -.5,  -.5, !  700
     .  -2.1,  4.8, -2.5,  -.9,  -.9, !  500
     .  -4.1,  4.6, -2.0,   .8,   .8, !  400
     .  -4.0, 10.0,  -.2,  3.9,  3.9, !  300
     .  -2.1, 15.5,  1.8,  7.8,  7.8, !  250
     .   2.9, 17.8,  4.8, 12.7, 12.7, !  200
     .   9.0, 22.3, 11.9, 15.8, 15.8, !  150
     .   6.4, 20.5, 10.5, 10.2, 10.2, !  100
     .   1.5,  7.8,  2.5,  4.5,  4.5, !   70
     .  -2.3,  2.0,  1.5,  4.6,  4.6, !   50
     .  -3.5,  9.0,  4.3, 10.6, 10.6, !   30
     .   1.5, 25.6,  8.0,  8.0,  8.0, !   20
     .  12.0, 18.4, 21.0, 21.0, 21.0  !   10
     . /)

      real, parameter :: Table_IType_20 ( TableSz ) = (/
     .!
     .! INDIAN MET. SERVICE TYPE  MK3 ( IType = 20 )
     .! --------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa)
     .  -3.7,  -.8,  4.3, 19.2, 19.2, ! 1000
     .  -3.5, -1.4,  2.3,  9.7,  9.7, !  925
     .    .1,  1.4,  3.5,  9.7,  9.7, !  850
     .   -.5,  1.9,  4.3, 11.0, 11.0, !  700
     .  -5.9,   .1,  3.6, 10.4, 10.4, !  500
     .  -8.7,   .5,  4.4, 10.8, 10.8, !  400
     .  -8.9,  3.8,  7.7, 16.2, 16.2, !  300
     . -10.8,  3.4,  9.2, 19.7, 19.7, !  250
     . -13.8, -2.1,  6.1, 16.1, 16.1, !  200
     . -15.9, -5.0,  3.1,  8.8,  8.8, !  150
     . -12.4,  -.6,  5.7,  4.3,  4.3, !  100
     .   -.6, 10.9, 14.4,  9.8,  9.8, !   70
     .   5.5, 19.5, 21.5, 19.2, 19.2, !   50
     .   1.8, 17.5, 19.2, 13.6, 13.6, !   30
     .  -3.6, 12.9, 13.0,  1.0,  1.0, !   20
     .  -1.6, 14.3, 11.4, -3.1, -3.1  !   10
     . /)

      real, parameter :: Table_IType_21 ( TableSz ) = (/
     .!
     .! (S. KOREA) VIZ/JIN YANG MARK I MICRO ( IType = 21 )
     .! ---------------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa)
     .  -7.6, -3.5,-16.0,   .9,   .9, ! 1000
     .  -5.1, -2.6, -5.2,  -.1,  -.1, !  925
     .  -3.9, -2.1, -3.6,  -.9,  -.9, !  850
     .  -2.9, -3.5, -3.2,   .2,   .2, !  700
     .  -1.8, -3.8,  -.3,  2.8,  2.8, !  500
     .    .3,   .8,  3.7,  6.2,  6.2, !  400
     .   3.1, 13.6,  4.1,  9.9,  9.9, !  300
     .   4.3, 21.7,  5.1, 11.9, 11.9, !  250
     .    .8, 16.8,  7.0, 11.0, 11.0, !  200
     .  -4.7,  9.1,  4.4, 10.2, 10.2, !  150
     .  -7.4,  5.2,  1.3,  9.3,  9.3, !  100
     .  -7.6,  9.7,   .3,  6.2,  6.2, !   70
     .  -6.3, 11.4,  2.4,  6.6,  6.6, !   50
     .  -3.8, 13.7,  7.5, 11.3, 11.3, !   30
     .  -3.7, 17.1, 16.3, 15.8, 15.8, !   20
     .  -6.9, 13.0, 25.8, 18.6, 14.7  !   10
     . /)

      real, parameter :: Table_IType_22 ( TableSz ) = (/
     .!
     .!    (JAPAN) MEISEI  RS2-80  ( IType = 22 )
     .! -----------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa)
     . -10.6, 16.0,-14.7, -7.5,   .9, ! 1000
     . -10.4, 14.5, -6.4, -3.1, -9.3, !  925
     .  -7.5,  1.4, -3.3, -1.6, -6.6, !  850
     .  -4.1, -4.7, -3.8,  -.1,   .8, !  700
     .  -2.1, -4.9, -2.9,  1.2,  3.1, !  500
     .  -1.1, -6.7, -1.5,  2.8,  3.4, !  400
     .    .1,-12.6,  1.1,  5.6,  5.6, !  300
     .  -1.0,-15.9,  5.0,  6.9,  7.4, !  250
     .  -5.1,-13.2,  5.8,  6.2,  7.1, !  200
     .  -8.2, -6.9,  5.5,  7.0,  7.5, !  150
     .  -8.7, -8.9,  4.3,  7.7,  7.7, !  100
     .  -7.2, -8.6,  4.0,  6.6,  4.6, !   70
     .  -1.3, -5.5,  6.8,  9.1,  4.4, !   50
     .   3.6, -6.4,  5.4, 11.9,  5.5, !   30
     .   4.9, -7.9,  1.6, 11.2,  6.2, !   20
     .   6.0,   .7,  6.8, 12.8, 12.8  !   10
     . /)

      real, parameter :: Table_IType_26 ( TableSz ) = (/
     .!
     .!  (SWITZERLAND) METEOLABOR BASORA ( IType = 26 )
     .! -----------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa)
     .  12.1,  4.4,  6.2, -9.7,-11.3, ! 1000
     .  12.1,  4.4,  6.2, -9.7,-11.3, !  925
     .   2.2,  2.3,  2.6, -3.7, -7.3, !  850
     .  -2.7,  1.0,  -.4, -1.4, -3.7, !  700
     .  -1.4,  1.8,  -.6,   .0,   .3, !  500
     .   -.4,  3.0,   .6,  1.7,  2.6, !  400
     .  -1.2,  3.2,  1.0,  1.4,  2.3, !  300
     .   -.8,  1.3,  1.2,   .8,  3.5, !  250
     .   -.6, -1.4,  3.4,  2.5,  9.1, !  200
     .  -1.7, -1.6,  4.7,  5.7, 14.0, !  150
     .   -.8,  -.7,  3.6,  6.4, 11.6, !  100
     .    .3,  -.6,  2.9,  4.3,  2.7, !   70
     .  -2.5, -1.0,  2.6,  1.2, -3.1, !   50
     .  -5.1,  1.9,  3.5,  1.0, -2.4, !   30
     .  -4.2,  3.9,  2.7,  3.7,  -.7, !   20
     .  -2.8,   .3, -4.6,  5.3,  1.0  !   10
     . /)


      real, parameter :: Table_IType_27 ( TableSz ) = (/
     .!
     .!      (RUSSIA) AVK-MRZ ( IType = 27 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa)
     .   4.2, -2.2,  4.7,   .4,  -.4, ! 1000
     .  -1.6,   .1,  1.7,  -.4,  -.4, !  925
     .  -3.1,  1.7,  1.0,  -.3,  -.3, !  850
     .  -1.8,  2.0,  2.4,  1.4,  1.4, !  700
     .   -.3,  2.5,  4.8,  4.7,  4.7, !  500
     .    .0,  2.9,  5.5,  6.9,  9.3, !  400
     .   -.2,  1.1,  4.3,  6.1,  5.7, !  300
     .    .4,   .1,  4.7,  7.4,  5.6, !  250
     .   2.7,  3.3, 10.7, 14.2, 14.9, !  200
     .   4.2,  6.0, 14.6, 18.0, 22.6, !  150
     .   4.4,  5.9, 13.6, 16.8, 23.7, !  100
     .   3.2,  4.7, 11.3, 11.8, 11.8, !   70
     .   1.3,  4.1, 10.7,  8.0,  8.0, !   50
     .   -.1,  3.5, 12.5,  6.9,  6.9, !   30
     .  -1.0,  2.8, 15.5,  6.4,  6.4, !   20
     .   1.5,  6.1, 19.2,  7.6,  7.6  !   10
     . /)

      real, parameter :: Table_IType_28 ( TableSz ) = (/
     .!
     .!  (RUSSIA) METEORIT  MARZ2-1 ( IType = 28 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa)
     .  -3.3,  1.3, 10.2,  -.3,  -.3, ! 1000
     .  -3.1,  -.1,   .8,  2.5,  2.5, !  925
     .  -2.2,  1.8,  -.2,  1.4,  1.4, !  850
     .    .0,  3.6,  1.6,  1.7,  1.7, !  700
     .   2.7,  4.2,  3.6,  4.4,  4.4, !  500
     .   4.1,  4.1,  3.8,  6.0,  6.0, !  400
     .   2.8,  1.9,  2.9,  5.5,  5.5, !  300
     .    .8,  -.7,  6.2,  9.9,  9.9, !  250
     .   1.4,  2.0, 14.1, 21.1, 21.1, !  200
     .   3.9,  5.8, 17.3, 26.3, 26.3, !  150
     .   5.5,  6.3, 16.7, 25.6, 25.6, !  100
     .   3.9,  6.4, 14.4, 22.7, 22.7, !   70
     .   -.4,  6.0, 12.1, 21.9, 21.9, !   50
     .  -5.1,  4.8, 11.6, 21.5, 21.5, !   30
     .  -8.3,  5.4, 12.9, 21.8, 21.8, !   20
     .  -6.3, 10.1, 17.9, 24.3, 24.3  !   10
     . /)

      real, parameter :: Table_IType_29 ( TableSz ) = (/
     .!
     .!  (RUSSIA) METEORIT  MARZ2-2 ( IType = 29 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa)
     . -14.2,  1.9, -2.0, 11.0, 11.0, ! 1000
     .  -1.9, -5.1, -7.8,  5.3,  5.3, !  925
     .   3.1, -2.9, -3.8,  3.2,  3.2, !  850
     .   6.5,   .3,  2.3,  3.5,  3.5, !  700
     .   5.6,  2.2,  5.8,  6.3,  6.3, !  500
     .  -1.6,  1.3,  5.3,  8.7,  9.7, !  400
     .  -9.0, -2.2,   .9,  9.5,  9.5, !  300
     .  -9.1, -1.4,   .0, 11.4, 11.4, !  250
     .  -1.1,  4.8,  6.9, 16.0, 16.0, !  200
     .   4.5,  7.5, 11.2, 18.7, 18.7, !  150
     .   4.7,  6.9, 13.7, 17.6, 17.6, !  100
     .   1.9,  7.0, 16.0, 13.4, 13.4, !   70
     .  -1.2,  8.2, 15.5, 11.2, 11.2, !   50
     .  -3.1,  8.4, 12.1, 11.3, 14.4, !   30
     .  -1.9,  6.7,  6.4,  7.3,  7.3, !   20
     .   0.9,  7.7,  4.1,  1.8,  1.8  !   10
     . /)

      real, parameter :: Table_IType_32 ( TableSz ) = (/
     .!
     .!  (CHINA) SHANGHAI RADIO ( IType = 32 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa)
     .  -4.1,  3.0,  2.3, -9.0, -9.0, ! 1000
     .   -.9,   .2,   .8, -2.9, -2.9, !  925
     .    .9,  2.0,  1.8,  1.2,  1.2, !  850
     .   2.9,  3.6,  4.9,  6.1,  6.1, !  700
     .   4.5,  5.1,  8.8, 11.2, 11.2, !  500
     .   5.2,  7.2, 11.3, 14.4, 14.4, !  400
     .   5.2,  8.0, 11.3, 14.6, 14.6, !  300
     .   5.1,  7.3, 10.7, 13.0, 13.0, !  250
     .   6.8,  7.2, 10.2, 12.2, 12.2, !  200
     .   7.6,  7.2,  8.4, 13.3, 13.3, !  150
     .   3.2,  3.9,  2.4,  9.3,  9.3, !  100
     .  -4.4, -2.7, -5.9,  -.1,  -.1, !   70
     .  -8.7, -5.1, -8.1, -3.5, -3.5, !   50
     . -10.4,   .4, -3.4, -1.1, -1.1, !   30
     . -10.9,  8.8,  3.2,  1.4,  1.4, !   20
     .  -6.6, 17.0,  9.2,  5.0,  5.0  !   10
     . /)

      real, parameter :: Table_IType_37 ( TableSz ) = (/
     .!
     .!  VAISALA  RS80 ( IType = 37 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa)
     .  -2.4,  3.4, -1.2,  2.0, -1.5, ! 1000
     .   3.5, -3.6,   .9,  4.5,  1.1, !  925
     .   3.6,  -.7,  1.0,  4.1,  1.3, !  850
     .    .9,  -.3,  1.0,  2.3,  2.0, !  700
     .   -.7,  -.2,   .7,  2.1,  2.7, !  500
     .   -.6,  1.0,   .6,  3.4,  3.2, !  400
     .    .5,  1.1,  1.2,  5.4,  4.5, !  300
     .   1.4, -1.0,  1.0,  6.8,  5.2, !  250
     .   1.2, -2.2,  1.1,  6.4,  5.3, !  200
     .  -1.0,  -.5,  2.9,  4.9,  3.7, !  150
     .  -3.5,   .7,  2.7,  3.2, -1.4, !  100
     .  -2.0,   .4,  -.5,  3.6, -3.8, !   70
     .   1.5,  -.1,  -.2,  6.3,  1.3, !   50
     .   1.5,  -.2,  4.7,  7.5,  8.8, !   30
     .  -1.1,  1.0,  5.3,  4.8, 16.8, !   20
     .   -.6,  5.4, -1.7,  5.2, 16.4  !   10
     . /)

      real, parameter :: Table_IType_43 ( TableSz ) = (/
     .!
     .!  (U.S.) AIR IS - 4A - 1680 ( IType = 43 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa)
     .  -6.2,  4.6,  8.6, -8.9, -8.9, ! 1000
     .  -2.7, 10.0,   .7, -1.3, -1.3, !  925
     .   -.8,  8.0,   .8,   .9,   .9, !  850
     .   -.6,  4.7,  1.5,  4.0,  4.0, !  700
     .  -1.9,  -.2,   .2,  6.9,  6.9, !  500
     .  -3.4, -4.8,  -.1,  8.2,  8.2, !  400
     .  -3.8, -7.5,   .2,  9.2,  9.2, !  300
     .  -4.2, -5.8,  -.5, 10.5, 10.5, !  250
     .  -6.3,   .6, -3.7, 10.7, 10.7, !  200
     .  -7.4,  5.4, -8.6, 12.5, 12.5, !  150
     .  -5.7,  6.5, -8.1, 16.6, 16.6, !  100
     .  -1.5,  6.4,  2.4, 20.9, 20.3, !   70
     .   8.5,  6.0, 15.7, 27.7, 27.8, !   50
     .  20.9,  4.8, 23.3, 37.1, 37.1, !   30
     .  20.9, 18.3, 26.3, 45.2, 45.2, !   20
     .  20.9, 18.3, 26.3, 45.2, 45.2  !   10
     . /)

      real, parameter :: Table_IType_45 ( TableSz ) = (/
     .!
     .!    (U.S.) RS MSS  ( IType = 45 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa) 
     .  -5.7, -5.9, -9.5,  1.3,  6.3, ! 1000
     .  -3.0, -2.3,  2.3,  5.1,  5.9, !  925
     .    .0,  -.7,  1.0,  4.3,  2.4, !  850
     .  -1.3, -2.1,   .0,  1.6,   .0, !  700
     .  -3.1, -3.4,   .8,   .4, -2.2, !  500
     .  -3.3, -3.4,   .9,  -.5, -2.8, !  400
     .  -2.9, -2.3,  1.2, -1.2, -1.8, !  300
     .  -3.2, -2.6,  1.9, -1.3, -1.5, !  250
     .  -4.6, -4.3,  3.0, -1.4, -2.5, !  250
     .  -7.3, -3.8,  4.8, -1.7, -2.4, !  150
     .  -8.8, -3.8,  3.9,  1.9,  -.5, !  100
     .  -5.6, -2.9,  1.7, 14.2,  3.7, !   70
     .   -.3,   .5,  3.4, 15.1, 10.7, !   50
     .   2.8,  4.9,  8.2,  2.1, 15.1, !   30
     .   2.6, 10.5, 14.8, -9.8, 14.7, !   20
     .   4.1, 19.1, 29.1, -1.6, 16.9  !   10
     . /)

      real, parameter :: Table_IType_46 ( TableSz ) = (/
     .!
     .!   (U.S.) AIR IS-4A-403 ( IType = 46 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa) 
     .  -9.4, 13.2, -2.1, -2.6, -2.6, ! 1000
     .   -.9,  1.0, -2.4,  1.8,  1.8, !  925
     .    .8,  -.5,  1.1,  3.8, 15.0, !  850
     .    .1,  2.7,   .8,  4.3, 14.4, !  700
     .   -.3,  5.3,  2.4,  5.0, 11.8, !  500
     .    .0,  5.0,  3.7,  5.1, 18.0, !  400
     .    .2,  3.7,  1.5,  4.9, 22.6, !  300
     .   1.3,  5.1, -1.1,  6.1, 21.8, !  250
     .   4.1,  6.6, -2.8,  7.7, 23.1, !  200
     .   4.2,  5.3, -4.1, 10.3, 27.7, !  150
     .   1.5,  3.2, -5.0, 13.5, 32.4, !  100
     .   1.3,  2.9, -6.9, 15.3, 15.3, !   70
     .   6.4,  5.7,  4.3, 20.6, 20.6, !   50
     .  15.1, 10.4, 21.5, 28.6, 28.6, !   30
     .  21.7, 14.0, 25.2, 30.3, 30.3, !   20
     .  27.5, 13.8, 21.4, 32.2, 32.2  !   10
     . /)

      real, parameter :: Table_IType_47 ( TableSz ) = (/
     .!
     .!   (JAPAN) MEISEI  RS2-91 ( IType = 47 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa) 
     .  -5.9, -6.7,  -.4,   .4,  -.6, ! 1000
     .  -4.3,  3.2,  2.8,  -.7, -1.6, !  925
     .  -2.7,   .9,  -.6, -2.1, -1.3, !  850
     .  -2.2, -2.3, -5.9, -2.3,  -.7, !  700
     .   -.9, -3.9, -4.4, -1.0, -3.0, !  500
     .   1.9, -5.7, -2.3,  1.3, -1.2, !  400
     .   4.7, -8.5, -2.3,  3.9,  2.9, !  300
     .   5.2, -6.1,   .1,  5.4,  4.3, !  250
     .   2.3,  1.1,  3.1,  4.6,  3.5, !  200
     .  -2.4,  1.9,  2.2,  3.7,  4.8, !  150
     .  -6.8, -1.5, -3.5,  1.7,  5.6, !  100
     .  -8.6, -4.5, -7.3, -2.1,  1.5, !   70
     .  -6.2, -7.3, -9.2, -2.0,   .4, !   50
     .  -3.0, -9.4,  -.8,   .3,  1.1, !   30
     .  -5.4, -9.4, 22.1,  1.5, -1.1, !   20
     .  -8.6, -7.5, 39.3,  4.7,   .0  !   10
     . /)

      real, parameter :: Table_IType_49 ( TableSz ) = (/
     .!
     .!    (U.S.) VIZ MARK II  ( IType = 49 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa) 
     .   5.0, -1.8,  3.9,  3.2, -8.4, ! 1000
     .   3.0, -1.7,  2.4,  4.1, -3.0, !  925
     .    .5, -1.8,   .8,  3.5,  -.8, !  850
     .  -1.7, -1.7,  -.2,  2.3,  -.7, !  700
     .  -2.9,  -.5,   .5,  2.2,  -.1, !  500
     .  -2.5,  1.6,  1.7,  3.4,  1.3, !  400
     .  -1.9,  3.4,  2.7,  5.1,  2.7, !  300
     .  -1.7,  2.1,  2.7,  5.2,  3.4, !  250
     .  -1.2,  -.7,  1.9,  3.6,  2.3, !  200
     .  -2.1,  -.8,  3.2,  3.2,   .4, !  150
     .  -4.2,   .3,  4.5,  4.1,   .7, !  100
     .  -4.2,   .6,  4.3,  5.3,  5.4, !   70
     .  -3.2,   .9,  5.8,  8.5, 10.1, !   50
     .  -3.7,  1.9, 10.2, 14.0, 12.4, !   30
     .  -7.5,  3.0, 13.7, 17.9, 14.3, !   20
     .  -9.0,  4.8, 11.6, 16.4, 18.0  !   10
     . /)

      real, parameter :: Table_IType_50 ( TableSz ) = (/
     .!
     .!    (GERMANY) GRAW DFM-90 ( IType = 50 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa) 
     .   -.3,  2.6,  2.2,-10.1,-20.9, ! 1000
     .   -.3, -2.1,  4.6,  5.9, -3.7, !  925
     .  -1.3, -2.0,  2.4,  4.7, -2.0, !  850
     .  -2.7,  -.9,  1.6,  2.9, -1.2, !  700
     .  -2.8,  1.0,  3.3,  4.2,  3.6, !  500
     .   -.7,  4.0,  5.8,  7.2,  7.2, !  400
     .   2.8,  7.0,  7.6,  9.3,  8.5, !  300
     .   4.0,  6.9,  7.8,  8.9,  9.4, !  250
     .   2.4,  4.5,  7.1,  8.5,  8.5, !  200
     .   1.8,  3.9,  7.7, 10.7, 10.7, !  150
     .   2.7,  4.3,  8.5, 11.3, 11.3, !  100
     .   2.8,  5.4,  8.9,  7.8,  7.8, !   70
     .   2.7,  9.7, 10.4,  4.5,  4.5, !   50
     .   2.2, 15.3, 12.0,  2.8,  2.8, !   30
     .   1.4, 19.6, 10.5,  1.1,  1.1, !   20
     .   4.7, 25.1,  8.5,  1.1,  1.1  !   10
     . /)

      real, parameter :: Table_IType_51 ( TableSz ) = (/
     .!
     .!  (U.S.) NOAA / VIZ TYPE  B2 ( IType = 51 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa) 
     .   -.7,   .8, -1.3,  2.7,  5.3, ! 1000
     .  -3.1, -2.5, -1.0,  4.4,  5.9, !  925
     .  -4.0, -2.1, -1.9,  2.9,  3.4, !  850
     .  -4.6, -1.9, -1.6,  -.8,   .6, !  700
     .  -4.5, -1.5,  -.1, -2.0, -1.7, !  500
     .  -3.9,   .0,  1.5,  -.8,  -.1, !  400
     .  -2.7,  1.8,  3.1,  1.9,  3.7, !  300
     .  -1.8,  1.4,  4.3,  4.0,  3.8, !  250
     .  -1.9,   .8,  5.7,  5.2,  2.0, !  200
     .  -1.1,  2.4,  8.6,  9.1,  3.4, !  150
     .  -1.5,  2.0,  9.0, 11.3,  7.4, !  100
     .  -4.8,  -.5,  6.8,  9.8, 11.3, !   70
     .  -8.2, -1.8,  6.0, 10.6, 14.5, !   50
     . -11.0,  -.3,  7.6, 13.0, 14.6, !   30
     . -11.6,  3.0,  9.9, 12.7,  8.2, !   20
     .  -8.4,  7.4, 12.8,  9.3,  -.3  !   10
     . /)

      real, parameter :: Table_IType_52 ( TableSz ) = (/
     .!
     .!  VAISALA RS80/MICROARTS ( IType = 52 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa) 
     .   5.5,  1.3,  1.3,  3.9,  5.3, ! 1000
     .   -.8, -3.1,  -.7,  1.8,  2.5, !  925
     .  -3.5, -3.5, -3.2,   .1,   .4, !  850
     .  -3.7, -3.2, -3.7, -1.6, -2.0, !  700
     .  -2.6, -2.5, -2.9, -2.7, -5.1, !  500
     .  -1.7, -1.1, -2.4, -2.9, -4.9, !  400
     .   -.9,   .6, -2.0, -3.0, -2.2, !  300
     .    .2,   .0, -2.1, -3.6, -2.5, !  250
     .   1.4, -1.7, -2.9, -4.0, -6.4, !  200
     .   2.2,  -.9, -2.4, -1.3, -8.8, !  150
     .    .5, -2.3, -3.3,  1.5, -7.3, !  100
     .  -3.6, -5.5, -7.0, -1.2, -3.5, !   70
     .  -6.0, -6.7, -9.1, -5.4,  -.8, !   50
     .  -6.2, -5.3, -7.8, -6.6,   .1, !   30
     .  -4.7, -1.9, -4.7, -5.5, -7.8, !   20
     .    .0,  3.1,  -.4, -2.2,-16.4  !   10
     . /)

      real, parameter :: Table_IType_60 ( TableSz ) = (/
     .!
     .!  VAISALA RS80/MICROCORA ( IType = 60 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa) 
     .    .4,  -.2,  8.5,  2.2, -5.9, ! 1000
     .  -2.7, -4.8, -1.2, -5.7, -4.1, !  925
     .  -3.9, -1.5,  1.3, -2.9, -4.5, !  850
     .  -1.5,  4.6,  4.6,   .3, -2.6, !  700
     .    .9,  5.1,  3.4,  1.0,   .8, !  500
     .   1.2,  2.1,  -.1,   .6,  1.3, !  400
     .    .6, -2.1, -3.7,   .8,   .2, !  300
     .   1.2, -7.1, -7.0,  1.0,  2.3, !  250
     .   2.0, -8.9, -9.4,   .1,  4.4, !  200
     .  -1.3, -6.4, -7.8, -3.3,  2.6, !  150
     .  -6.2, -5.2, -8.1, -8.1, -2.2, !  100
     .  -3.7, -3.2, -8.2, -7.6, -7.6, !   70
     .   1.3, -1.5, -3.5, -3.6, -3.6, !   50
     .   2.7,   .0,  1.8, -1.2, -1.2, !   30
     .   3.4,  3.2,  6.4,  1.4,  1.4, !   20
     .   5.5,  7.7, 14.3,  5.2,  5.2  !   10 
     . /)

      real, parameter :: Table_IType_61 ( TableSz ) = (/
     .!
     .!  VAISALA RS80/DIGICORA OR MARWIN ( IType = 61 )
     .! -----------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa) 
     .    .0,   .0,   .0,   .0,   .0, ! 1000
     .    .0,   .0,   .0,   .0,   .0, !  925
     .    .0,   .0,   .0,   .0,   .0, !  850
     .    .0,   .0,   .0,   .0,   .0, !  700
     .    .0,   .0,   .0,   .0,   .0, !  500
     .    .0,   .0,   .0,   .0,   .0, !  400
     .    .0,   .0,   .0,   .0,   .0, !  300
     .    .0,   .0,   .0,   .0,   .0, !  250
     .    .0,   .0,   .0,   .0,   .0, !  200
     .    .0,   .0,   .0,   .0,   .0, !  150
     .    .0,   .0,   .0,   .0,   .0, !  100
     .    .0,   .0,   .0,   .0,   .0, !   70
     .    .0,   .0,   .0,   .0,   .0, !   50
     .    .0,   .0,   .0,   .0,   .0, !   30
     .    .0,   .0,   .0,   .0,   .0, !   20
     .    .0,   .0,   .0,   .0,   .0  !   10
     . /)

      real, parameter :: Table_IType_62 ( TableSz ) = (/
     .!
     .!  VAISALA RS80/PCCORA ( IType = 62 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa) 
     .  -4.5, -5.0, -5.3, -3.3, -7.7, ! 1000
     .  -1.4, -3.7, -3.3, -1.1, -4.2, !  925
     .  -1.0, -2.1, -2.0,  -.2, -3.8, !  850
     .  -1.1, -1.1,  -.8,  -.5, -2.0, !  700
     .   -.5,  -.3,  -.3,  -.1,   .3, !  500
     .    .4,   .9,  -.3,  1.3,  2.5, !  400
     .    .9,  2.6,   .1,  2.3,  3.5, !  300
     .    .7,  2.3,   .2,  2.3,  3.2, !  250
     .    .5,   .3,  -.8,   .7,  3.0, !  200
     .    .0, -1.0, -2.9, -1.4,  2.0, !  150
     .    .2, -1.5, -4.3, -2.4, -1.7, !  100
     .   1.3, -1.2, -3.1, -2.1, -6.0, !   70
     .   1.6,  -.4,  -.7, -1.6, -7.0, !   50
     .    .7,  1.7,  2.8,  -.4, -6.6, !   30
     .  -1.6,  4.5,  6.0,  1.0, -4.9, !   20
     .   -.9,  8.6,  6.8,  4.4,  1.8  !   10
     . /)

      real, parameter :: Table_IType_63 ( TableSz ) = (/
     .!
     .!  VAISALA RS80/STAR  ( IType = 63 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa) 
     .  -1.1, -2.4,  8.3, 10.8,  4.6, ! 1000
     .   -.6,  1.3,  6.0,  4.1,   .7, !  920
     .  -1.0,  1.9,  3.4,  1.9,   .4, !  850
     .  -1.2,  1.8,  1.6,   .5,   .7, !  700
     .  -1.0,  1.7,   .7,  -.2,   .6, !  500
     .    .4,  1.4,   .6,  -.5,   .8, !  400
     .   2.9,  1.5,  1.4,  -.5,  2.1, !  300
     .   3.9,  1.2,  1.1, -1.2,  2.9, !  250
     .   1.4,  1.2,  -.4, -3.2,  1.2, !  200
     .  -1.5,  2.3, -1.1, -4.6, -2.3, !  150
     .  -1.1,  2.5, -1.2, -2.8, -2.8, !  100
     .   2.3,  2.5,   .0,   .8,   .4, !   70
     .   4.7,  3.1,  3.0,  2.8,  1.2, !   50
     .   2.6,  4.4,  6.8,  5.2,   .0, !   30
     .  -2.4,  7.3, 11.3,  8.1,  -.5, !   20
     .  -1.3, 14.2, 15.7, 13.1,  4.3  !   10
     . /)

      real, parameter :: Table_IType_71 ( TableSz ) = (/
     .!
     .!  VAISALA RS90/DIGICORA OR MARWIN ( IType = 71 )  
     .! -----------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa) 
     .  -3.6,  -.1, -3.8,-13.5, -6.9, ! 1000
     .  -4.0, -2.9, -3.4, -6.7, -4.8, !  925
     .  -4.5, -2.3, -3.4, -5.3, -5.9, !  850
     .  -3.7, -1.4, -2.2, -4.7, -5.5, !  700
     .  -2.5,  -.9, -1.5, -3.6, -3.7, !  500
     .  -2.6,  -.2, -1.2, -2.0, -2.0, !  400
     .  -2.6,   .9,  -.4,  -.1, -1.1, !  300
     .   -.6,   .7,   .2,  2.1,   .5, !  250
     .   1.8,   .0,  1.3,  5.1,  4.5, !  200
     .   1.7,   .0,  3.2,  7.5,  9.1, !  150
     .    .7,  -.4,  2.9,  6.6, 11.5, !  100
     .   -.4, -1.3,   .4,  1.1,  6.0, !   70
     .  -1.7, -1.4,  -.6, -1.7,  1.6, !   50
     .  -3.7,  -.4,  1.7,  1.7,  2.6, !   30
     .  -4.0,  1.5,  5.7,  7.1,  6.1, !   20
     .   2.7,  7.4, 12.6, 14.3, 14.1  !   10
     . /)

      real, parameter :: Table_IType_76 ( TableSz ) = (/
     .!
     .!  (RUSSIAN FED.) AVK-RF95-ARMA ( IType = 76 )
     .! --------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa)
     .  -2.1,  1.6, -7.8, 11.8, 11.8, ! 1000
     .  -6.2, -1.2, -4.1,  4.8,  4.8, !  920
     .  -4.7,  1.2, -1.2,  1.5,  1.5, !  850
     .  -3.0,  2.3,  1.5,  -.6,  -.6, !  700
     .  -3.9,  1.5,  1.7, -1.1, -1.1, !  500
     .  -2.8,   .0, -2.1, -2.3, -2.3, !  400
     .  -1.0, -3.1, -8.9, -5.3, -5.3, !  300
     .    .6, -4.1,-13.3, -2.3, -2.3, !  250
     .   3.3,  -.5, -2.6,  7.1,  7.1, !  200
     .   3.8,   .5,  7.4, 11.2, 11.2, !  150
     .   3.2,  1.4, 10.8, 12.3, 12.3, !  100
     .   -.9,  2.3,  9.8,  8.5,  8.5, !   70
     .  -4.1,   .7,  6.9,  3.8,  3.8, !   50
     .   -.5, -2.4,  4.2,  1.1,  4.4, !   30
     .   2.6, -5.8,  1.3, -1.0,  8.0, !   20
     .   2.6, -5.9,  1.3,  1.9, 16.2  !   10
     . /)

      real, parameter :: Table_IType_90 ( TableSz ) = (/
     .!
     .!  UNKNOWN TYPE ( IType = 90 )
     .! -------------------------------------------
     .! NITE   -10    15    30    60  ! Solar Angle
     .! ----------------------------- ! Level (hPa)
     .  11.2,   .4, -9.9,-15.6,-15.6, ! 1000
     .   3.9,  1.5, -2.2, -8.9, -8.9, !  925
     .  -1.6,   .5,  -.7, -4.6, -4.6, !  850
     .  -3.2, -2.9, -1.0, -3.4, -3.4, !  700
     .  -3.1, -2.4,  -.1, -2.4, -2.4, !  500
     .  -3.1,  -.2,  1.0,  -.9,  -.9, !  400
     .  -3.0,  1.7,  2.1,  1.4,  1.4, !  300
     .  -1.2,  1.4,  3.0,  4.1,  4.1, !  250
     .   2.9,   .8,  4.1,  6.0,  6.0, !  200
     .   4.0,  4.4,  6.7,  7.0,  7.0, !  150
     .   -.2,  5.8,  8.9,  7.3,  7.3, !  100
     .  -4.1,  3.9,  9.0,  7.0,  6.3, !   70
     .  -7.2,  2.6,  8.9,  9.2, 10.8, !   50
     .  -8.1,  6.0, 12.2, 15.0, 15.0, !   30
     .  -3.5,  9.5, 16.3, 19.0, 19.0, !   20
     .   4.5,  8.0, 15.6, 21.7, 21.7  !   10
     . /)

      real, parameter ::
     .   TenthsDegC ( NSE, NP, NT ) = Reshape ( (/
     .                      ! Table no. ! Type name
     . !----------------------------------------------
     .      Table_IType_9,  !      1    ! UNKNOWN TYPE
     .      Table_IType_10, !      2    ! (U.S.) NOAA / VIZ TYPE A
     .      Table_IType_11, !      3    ! (U.S.) NOAA / VIZ TYPE  B
     .      Table_IType_12, !      4    ! (U.S.) NOAA / SPACE DATA CORP.
     .      Table_IType_20, !      5    ! INDIAN MET. SERVICE TYPE  MK3
     .      Table_IType_21, !      6    ! (S. KOREA) VIZ/JIN YANG MARK I MICRO
     .      Table_IType_22, !      7    ! (JAPAN) MEISEI  RS2-80
     .      Table_IType_26, !      8    ! (SWITZERLAND) METEOLABOR BASORA
     .      Table_IType_27, !      9    ! (RUSSIA) AVK-MRZ
     .      Table_IType_28, !     10    ! (RUSSIA) METEORIT  MARZ2-1
     .      Table_IType_29, !     11    ! (RUSSIA) METEORIT  MARZ2-2
     .      Table_IType_32, !     12    ! (CHINA) SHANGHAI RADIO
     .      Table_IType_37, !     13    ! VAISALA  RS80
     .      Table_IType_43, !     14    ! (U.S.) AIR IS - 4A - 1680
     .      Table_IType_45, !     15    ! (U.S.) RS MSS
     .      Table_IType_46, !     16    ! (U.S.) AIR IS-4A-403
     .      Table_IType_47, !     17    ! (JAPAN) MEISEI  RS2-91
     .      Table_IType_49, !     18    ! (U.S.) VIZ MARK II
     .      Table_IType_50, !     19    ! GERMANY) GRAW DFM-90
     .      Table_IType_51, !     20    ! (U.S.) NOAA / VIZ TYPE  B2
     .      Table_IType_52, !     21    ! VAISALA RS80/MICROARTS
     .      Table_IType_60, !     22    ! VAISALA RS80/MICROCORA
     .      Table_IType_61, !     23    ! VAISALA RS80/DIGICORA OR MARWIN
     .      Table_IType_62, !     24    ! VAISALA RS80/PCCORA
     .      Table_IType_63, !     25    ! VAISALA RS80/STAR
     .      Table_IType_71, !     26    ! VAISALA RS90/DIGICORA OR MARWIN
     .      Table_IType_76, !     27    ! (RUSSIAN FED.) AVK-RF95-ARMA
     .      Table_IType_90  !     28    ! UNKNOWN TYPE
     .    /), (/ NSE, NP, NT /))        ! -----     

      integer :: iTable, NObs, iOb
      real    ::  HTCor ( NP )

      NObs       =  min ( size ( P ), size ( SE ), size ( TCor ))
      if ( NObs .eq. 0 ) return

!     Get table number
!     ----------------
      iTable    = 0
      if ( rkx .ge. rkx_Min .and.
     .     rkx .le. rkx_Max) iTable = ITables ( rkx )          

!     If table exists for the given instrument type ...
!     -------------------------------------------------
      if ( iTable .gt. 0 ) then
         call IntrpTab ( HP, HSE, CutOff,     ! ... compute the corrections
     .                   TenthsDegC ( :, :, iTable ), P, SE, TCor ) 
         do iOb = 1, NObs
            TCor ( iOb ) = 0.1 * TCor ( iOb ) ! ... and convert to deg K
         end do
      else
         iTable = 0                           ! else apply no corrections
         do iOb = 1, NObs
            TCor ( iOb ) = 0
         end do
      end if

!     If desired, set corrections to zero for all lower levels
!     --------------------------------------------------------
      if ( use_cutoff ) then
         do iOb = 1, NObs
            if ( P ( iOb ) .ge. PLowestLev ) TCor ( iOb ) = 0.0
         end do
      end if

      return
      end subroutine ncepTab5
!...................................................................
!-------------------------------------------------------------------------
!         Nas/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: IntrpTab () --- Interpolate values from a given correction table
! 
! !DESCRIPTION:
!     This routine interpolates in log pressure and solar angle from a
!     given bias correction table.  This routine is designed to be low
!     level.
!
! !INTERFACE:
!
      pure subroutine IntrpTab ( HP, HSE, CutOff, Table, P, SE, TCor )
!
! !INPUT PARAMETERS: 
      implicit NONE
      real,    intent  (in) ::
     .   HP     (:),   ! Pressure
     .   HSE    (:),   ! ... and solar angles in table header
     .   CutOff (:),   ! Cutoff angle between night and day
     .   Table  (:,:), ! Bias correction
     .   P      (:),   ! Observation pressure
     .   SE     (:)    ! ... and solar angle
!
! !OUTPUT PARAMETER:
      real,    intent (out) ::
     .   TCor   (:)    ! Temperature corrections
!
! !REVISION HISTORY: 
!     22Apr2002  C. Redder  Origional code
!     06Feb2003  C. Redder  Added the optinal parameter, IBug
!     19Mar2003  C. Redder  Removed the optional parameter, IBug
!     22Mar2004  C. Redder  Fixed bug in cut-off angle interpolation,
!                           and search for proper sun angle interval.
!                           Made cutoff criteria consistent with that
!                           implemented at NCEP.  Assume level to be 
!                           10 hPa for all levels above.
! EOP
!-------------------------------------------------------------------------

      integer :: iHSE, iiHSE, iHP, iiHP, NObs, iOb, NP, NSE, i_HSE
      real    ::  HSE1,   HSE2, HP1, HP2, DT1, DT2, RP, SEi_Last,
     .            RSE, Pi, SEi, CO,  HP_Top, HP_Bottom

      NP        = size (  HP )
      HP1       = HP   (  NP )
      HP2       = HP   (   1 )
      HP_Top    = HP   (  NP )
      HP_Bottom = HP   (   1 )
      NSE       = size ( HSE )
      HSE1      = HSE  ( NSE )
      HSE2      = HSE  (   1 )
      Pi        = 0.0

!     Observation loop
!     ----------------
      NObs = min  ( size ( P ), size ( SE ), size ( TCor ))
      do iOb = 1, NObs

!        If pressure has changed ...
!        ---------------------------
         if ( P ( iOb ) .ne. Pi ) then

!           ... determine the table entry number for the pressure
!           -----------------------------------------------------
            Pi  = max ( min ( P ( iOb ), HP_Bottom ), HP_Top )
            if ( Pi .gt. HP1 .or. Pi .lt. HP2 ) then
               do iHP = 1, NP - 1
                  iiHP = iHP
                  if ( Pi .ge. HP ( iHP + 1 )) exit
               end do
            end if

!           ... and calculate the interpolation factors
!           -------------------------------------------
            HP1 = HP  ( iiHP     )
            HP2 = HP  ( iiHP + 1 )
            RP  = log ( Pi / HP1 ) / log ( HP2 / HP1 )
            CO  = RP * ( CutOff ( iiHP + 1 ) - CutOff ( iiHP ))
     .          +        CutOff ( iiHP )
         end if

!        Determine the table entry number for the solar angle
!        ----------------------------------------------------
         SEi = max ( min ( SE ( iOb ), HSE ( NSE )), HSE ( 1 ))
         if ( SEi .lt. HSE1 .or. SEi .gt. HSE2 ) then
            do iHSE = 2, NSE - 1
               i_HSE = iHSE
               if ( SEi .lt. HSE  ( iHSE  + 1 )) exit
            end do
         end if
         HSE1   =   HSE ( i_HSE     )
         HSE2   =   HSE ( i_HSE + 1 )

!        ... and calculate the interpolation parameters ...
!        --------------------------------------------------
         if      (   SEi .lt. CO ) then ! ... for nightime conditions
            RSE   =   0.0
            iiHSE =   1
         else if ( i_HSE .eq. 2  ) then ! ... near sunrise/sunset
            RSE   = ( SEi - CO   ) / ( HSE2 - CO   )
            iiHSE =   i_HSE
         else                           ! ... for daytime conditions
            RSE   = ( SEi - HSE1 ) / ( HSE2 - HSE1 )
            iiHSE =   i_HSE
         end if

!        Perform 2-dimensional interpolation
!        -----------------------------------
         DT1   = RSE * ( Table ( iiHSE + 1, iiHP      )
     .                 - Table ( iiHSE,     iiHP      ))
     .                 + Table ( iiHSE,     iiHP      )
         DT2   = RSE * ( Table ( iiHSE + 1, iiHP  + 1 )
     .                 - Table ( iiHSE,     iiHP  + 1 ))
     .                 + Table ( iiHSE,     iiHP  + 1 )
         TCor ( iOb ) = RP * ( DT2 - DT1 ) + DT1
      end do

      return
      end subroutine IntrpTab
!...................................................................
      end module m_RadNCEP
!====================================================================

