*====================================================================

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_SunAlt -- Utility routines for calculating the sun's altitude
!
! !DESCRIPTION:
!     Given the horizontal coordinates and the date and time at universal
!     time (UTC) for a set of points, the routines in this module determine
!     (or check) the sun's altitude for each point.  The algorithm
!     implemented in the routines is discribed in detail in the Nautical
!     Almanac Office of the United States and Her Majesty's Nautical
!     Almanac Office (2003).  However, the constants in the algorithm have
!     undergone slight changes to make the algrithm valid between the
!     years 1800 and 2200.  The algorithm is only accurate to within
!     approximately 0.25 minutes of altitude in a vacuum as the equation
!     of the equinoxes has not been included.  In atmospheric conditions,
!     refraction can increase the sun's altitude by as much as 0.60 deg near
!     sunrise/sunset at sea-level.  The effect rapidly decreases with
!     increasing sun-altitude so that error becomes less than 0.005 deg
!     at sun altitudes greater than 15 deg.  Also, the refraction effect
!     is roughly proportional to the atmospheric pressure.  Internal
!     arithmatic is performed in double precision.  Care must be taken to
!     specify the Julian hour since 12 UTC, Jan 1, 2000.
!
! !REFERENCE:
!     Nautical Almanac Office of the United States Observatory and Her
!        Majesty's Nautical Almanac Office in the Rutherford Appleton
!        Laboratory, 2003:  The Astronomical Almanac, US Government 
!        Printing Office, Washington, DC/The Stationery Office, London.
!
! !INTERFACE:
!
      module    m_SunAlt
      implicit  NONE
      private ! except

!     Subroutines and functions
!     -------------------------
      public ::
     .   SunAlt,         ! Calculate the sun's altitude
     .   JDay_2000,      ! Julian day ...
     .   JHRef_2000,     ! ... and hour at 12 UTC January 1, 2000
     .   Julian,         ! Converts from "calendar" date to Julian day ...
     .   CalDate,        ! ... and vice versa
     .   Check_Date,     ! Check the "calendar" date
     .   Check_DateTime, ! ... and date and time
     .   JulHr,          ! Converts Julian date and time to Julian hour
     .   CalDHr          ! ... and vice versa

      interface SunAlt
         module procedure
     .      SunAlt_SDHT0ET,
     .      SunAlt_SDHT,
     .      SunAlt_JHrT,
     .      SunAlt_JHRefNPts
      end interface

      interface JulHr
         module procedure
     .      JulHr1,
     .      JulHrN
      end interface

      interface CalDHr
         module procedure
     .      CalDHr1
      end interface

      interface Julian
         module procedure
     .      JulDay1,   ! Compute Julian day for a given date
     .      JulDayN,   ! ... or for given dates
     .      JulHr1,    ! ... Julian hours for a given date/time
     .      JulHrN     ! ....or for given dates/times
      end interface

      interface CalDate
         module procedure
     .      CalDate1,
     .      CalDateN
      end interface

      interface Check_Date
         module procedure
     .      Check_Date1
      end interface

      interface Check_DateTime
         module procedure
     .      Check_DateTime1
      end interface

      integer, parameter ::
     .   HrRef      = 12,
     .   JDay_2000  = 2451545,       ! Julian day and
     .   JHRef_2000 = JDay_2000 * 24 ! ... hour on at 12 UTC,
!                                    !   Jan 01, 2000
! !REVISION HISTORY:
!     16May2001  C. Redder  Original version
!     06May2002  C. Redder  Incorporated the interfaces Julian and CalDate
!                           from the module m_Julian
!     15Jul2002  C. Redder  Renamed the module procedure SunAlt_JHr to
!                           SunAlt_JHRefNPts.  Added the module proedures
!                           SunAlt_, SunAlt_JHRef, SunAlt_NPts to the
!                           interface, SunAlt.  Created the interface,
!                           JulHr.
!     25Nov2003  C. Redder  Added module procedure, SunAlt_SDHT0ET to
!                           the generic interface, SunAlt.  Removed the 
!                           module procedure, SunAlt_NPts.  Rename the
!                           module procedures, SunAlt_SJHr and SunAlt_,
!                           to SunAlt_SDHT and SunAlt_JHrT.  Removed
!                           the generic routine, Check_SunAlt.
!     12Dec2003  C. Redder  Removed the module procedure, SunAlt_JHRef,
!                           from the generic interface, SunAlt.
!     19Dec2006  C. Redder  Added the generic interface, Check_Date,
!                           Check_DateTime and CalDHr, and added the
!                           module procedures JulHr1 and JulHrN to the
!                           interface Julian.
! EOP
!-----------------------------------------------------------------

!     Scratch space
!     -------------
      integer     ScrSpSz
      parameter ( ScrSpSz   = 5000 )

!     Type for real with the necessary precision
!     ------------------------------------------
      integer,     parameter ::
     .   HP   = selected_real_kind (12)

      contains

!...................................................................
!-------------------------------------------------------------------------
!         Nas/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: SunAlt_SDHT0ET() --- Calculates the sun's altitude for a given synoptic date and hour, intial time (from synoptic) and time since lauch
! 
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine SunAlt_SDHT0ET ( Lat, Lon, SYMDH, Time0, ETime, Alt )
!
! !INPUT PARAMETERS: 
      implicit NONE
      real,    intent  (in), dimension (:) ::
     .         Lat,    ! Latitudes  in degrees within the range  [-90, 90]
     .         Lon     ! Longitudes in degrees within the range [-180,180]
     .                 !    where -90 is 90W
      integer, intent  (in) ::
     .         SYMDH,  ! Synoptic date/hour in YYYYMMDDHH format.
     .         Time0   ! Initial time since synoptic date/hour (s)
      real,    intent  (in), dimension (:) ::
     .         ETime   ! Elapse time since Time0 (s)
!
! !OUTPUT PARAMETER:
      real,    intent (out), dimension (:) ::
     .         Alt     ! The sun's altitude in degrees
!
! !REVISION HISTORY: 
!     25Nov2003  C. Redder   Origional code
!     12Dec2003  C. Redder   Modifications made in response to changes
!                            in the interface to the routine,
!                            SunAlt_JHRefNPts
!EOP
!-------------------------------------------------------------------------

      integer :: JH_Ref, JulHr, NSeg, iSeg, iSegBeg, iSegEnd, SegSz,
     .           iSegPt, iPt, NPts
      real    :: BTime
      integer, dimension ( ScrSpSz ) :: DHrScr  
      real,    dimension ( ScrSpSz ) :: LatScr, LonScr, TimeScr

!     Split the input data into sections and ...
!     ------------------------------------------
      NPts   = min ( size ( Lon ),   size ( Lat ),
     .               size ( ETime ), size ( Alt ))
      NSeg   = ( NPts - 1 ) / ScrSpSz + 1

!     Set the synoptic time tags
!     --------------------------
      BTime  = real ( Time0 )
      JH_Ref = JHRef_2000
      JulHr  = JulHr1 ( SYMDH / 100, mod ( SYMDH, 100 ))
      SegSz  = min ( NPts, ScrSpSz )
      do iPt = 1, SegSz
          DHrScr ( iPt ) = JulHr - JH_Ref
      end do

!     ... and process the data section by section
!     -------------------------------------------
      iSegBeg = 1
      do iSeg = 1, NSeg
         iSegEnd = min ( iSegBeg + ScrSpSz - 1, NPts )
         SegSz   = iSegEnd - iSegBeg + 1
         do iSegPt = 1, SegSz
            iPt = iSegBeg + iSegPt - 1
            LonScr  ( iSegPt ) = Lon   ( iPt )
            LatScr  ( iSegPt ) = Lat   ( iPt )
            TimeScr ( iSegPt ) = ETime ( iPt ) + BTime
         end do
         call SunAlt_JHRefNPts ( SegSz,  LatScr, LonScr,
     .                           DHrScr, TimeScr,
     .                           Alt   ( iSegBeg : iSegEnd ))
         iSegBeg = iSegEnd + 1
      end do

      return
      end subroutine SunAlt_SDHT0ET

!...................................................................
!-------------------------------------------------------------------------
!         Nas/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: SunAlt_SDHT() --- Calculates the sun's altitude for a given synoptic Julian hour and minute.
! 
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine SunAlt_SDHT ( Lat, Lon, SYMDH, Time, Alt )
!
! !INPUT PARAMETERS: 
      implicit NONE
      real,    intent (in),  dimension (:) ::
     .         Lat,   ! Latitudes in  degrees within the range [-90, 90]
     .         Lon    ! Longitudes in degress within the range [-180,180]
                      ! where -90 is 90W
      integer, intent (in) ::
     .         SYMDH  ! Synoptic date/hour in YYYYMMDDHH format.
      real,    intent (in),  dimension (:) ::
     .         Time   ! Time since the synptic date/hour (s).
!
! !OUTPUT PARAMETER:
      real,    intent (out), dimension (:) ::
     .         Alt    ! The sun's altitude in degrees
!
! !REVISION HISTORY: 
!     16May2001  C. Redder   Origional code
!     18Apr2002  C. Redder   Changed method of determining the number of
!                            points from the size of the input arrays.
!     25Nov2003  C. Redder   Reversed the order of the input parameters,
!                            Lat and Lon.  Replaced the required input
!                            parameters, SJulHr and Minutes, with 
!                            SYMDH and Time, respectively.  Changed the
!                            units of the input parameter, Time, from
!                            minutes to seconds.  Removed the optional
!                            input parameter, JHRef.
!     12Dec2003  C. Redder   Modifications made in response to changes
!                            in the interface to the routine,
!                            SunAlt_JHRefNPts
!EOP
!-------------------------------------------------------------------------

      integer :: JH_Ref, JulHr, NSeg, iSeg, iSegBeg, iSegEnd, SegSz,
     .           iSegPt, iPt, NPts
      integer, dimension ( ScrSpSz ) :: DHrScr  
      real,    dimension ( ScrSpSz ) :: LatScr, LonScr, TimeScr

!     Split the input data into sections and ...
!     ------------------------------------------
      NPts    = min ( size ( Lon  ), size ( Lat ),
     .                size ( Time ), size ( Alt ))
      NSeg   = ( NPts - 1 ) / ScrSpSz + 1

!     Set the synoptic time tags
!     --------------------------
      JH_Ref = JHRef_2000
      JulHr  = JulHr1 ( SYMDH / 100, mod ( SYMDH, 100 ))
      SegSz  = min ( NPts, ScrSpSz )
      do iPt = 1, SegSz
          DHrScr ( iPt ) = JulHr - JH_Ref
      end do

!     ... and process the data section by section
!     -------------------------------------------
      iSegBeg = 1
      do iSeg = 1, NSeg
         iSegEnd = min ( iSegBeg + ScrSpSz - 1, NPts )
         SegSz   = iSegEnd - iSegBeg + 1
         do iSegPt = 1, SegSz
            iPt = iSegBeg + iSegPt - 1
            LonScr  ( iSegPt ) = Lon  ( iPt )
            LatScr  ( iSegPt ) = Lat  ( iPt )
            TimeScr ( iSegPt ) = Time ( iPt )
         end do
         call SunAlt_JHRefNPts ( SegSz,  LatScr,  LonScr,
     .                           DHrScr, TimeScr,
     .                           Alt   ( iSegBeg : iSegEnd ))
         iSegBeg = iSegEnd + 1
      end do

      return
      end subroutine SunAlt_SDHT

!...................................................................
!-------------------------------------------------------------------------
!         Nas/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS       !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: SunAlt_JHrT() --- Calculates the sun's altitude for given Julian hours and time since Julian hours
! 
! !DESCRIPTION:
!
! !INTERFACE:
!
      subroutine SunAlt_JHrT ( Lat, Lon, DelHr, Time, Alt )
!
! !INPUT PARAMETERS:
      implicit NONE
      real,    intent (in)  ::
     .   Lat   (:), ! Latitudes  in degrees within the range [ -90, 90]
     .   Lon   (:)  ! Longitudes in degress within the range [-180,180]
                    !   where -90 is 90W
      integer, intent (in)  ::
     .   DelHr (:)  ! Hours since 12 UTC, Jan. 01, 2000
      real,    intent (in)  ::
     .   Time  (:)  ! Time since Julian hour (s).
!
! !OUTPUT PARAMETERS: 
      real,    intent (out) ::
     .   Alt   (:)  ! The sun's altitude in degrees
!
! !REVISION HISTORY: 
!     15Jul2002  C. Redder   Original code
!     25Nov2003  C. Redder   Reversed the order of the input parameters,
!                            Lat and Lon.  Replaced the input parameter
!                            Minutes with Time.
!     12Dec2003  C. Redder   Replaced the required parameter, JulHr with
!                            DelHr.
!EOP
!-------------------------------------------------------------------------
      integer :: NPts, JHRef

      NPts   = min ( size ( Lon  ), size ( Lat ),
     .               size ( Time ), size ( Alt ))
      JHRef = JHRef_2000
      call SunAlt_JHRefNPts ( NPts,  Lat,   Lon,
     .                        DelHr, Time, Alt )

      return
      end subroutine SunAlt_JHrT

!...................................................................
!-------------------------------------------------------------------------
!         Nas/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: SunAlt_JHRefNPts() --- Calculates the sun's altitude.
! 
! !DESCRIPTION:
!
! !INTERFACE:
!
      subroutine SunAlt_JHRefNPts ( NPts, Lat, Lon, DelHr, Time, Alt )
!
! !INPUT PARAMETERS: 
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer,   intent (in)  ::
     .   NPts            ! Number of points
      real,      intent (in)  ::
     .   Lat   ( NPts ), ! Latitudes  in degrees within the range [ -90, 90]
     .   Lon   ( NPts )  ! Longitudes in degress within the range [-180,180]
                         !   where -90 is 90W
      integer,   intent (in)  ::
     .   DelHr ( NPts )  ! Hours since noon UTC, Jan 1, 2000 AD
      real,      intent (in)  ::
     .   Time  ( NPts )  ! Time since Julian hour (s).
!
! !OUTPUT PARAMETERS: 
      real,      intent (out) ::
     .   Alt   ( NPts )  ! The sun's altitude in degrees
!
! !REVISION HISTORY: 
!     01Apr1994  CCY         Earliest recorded update
!     15Jan1998  C. Redder   Extensive changes to interface, variable
!                            names and documentation.
!     16May2001  C. Redder   Incorporated into the module, m_SunAlt
!     27Jul2001  C. Redder   Fixed bug in ensuring that longitude and 
!                            obliquity of the ecliptic is in the range
!                            [0,360).
!     15Jul2002  C. Redder   Renamed routine, SunAlt_JHr to SunAlt_JHRefNPts
!     25Nov2003  C. Redder   Reversed the order of the input parameters,
!                            Lat and Lon.  Replaced the input parameter,
!                            Minutes, with Time.
!     12Dec2003  C. Redder   Removed the required argument, JHRef.  Changed
!                            precision of internal variables from double to
!                            HP (defined in module header).  Replaced the
!                            the required argument, JulHr, with, DelHr.
!EOP
!-------------------------------------------------------------------------

      integer, parameter ::
     .   JDRef_Astr = JDay_2000, ! Astronomical reference date ...
     .   JHRef_Astr = JHRef_2000 ! ... and hour for noon UTC January 1, 2000

!     Number of days per century
!     --------------------------
      real ( kind = HP ), parameter :: NDays_100yrs = 36525.0_HP

!     Local variables
!     ---------------
      integer :: iPt     ! index for do loop
      real ( kind = HP ) ::
     .        pi,        ! = 3.14159265358979323846264....
     .        Deg2Rad,   ! degrees to radians = pi / 180
     .        DelDays,   ! Number of days since 12 UTC, Jan 1, 2001
     .        MeanLon,   ! mean longitude
     .        ANom,      ! anomaly
     .        EclLon,    ! longitude of the ecliptic
     .        Obliq,     ! obliquity of the ecliptic
     .        ZeroUTC,   ! Del day at 0 UTC of the current day.
     .        RtAscSun,  ! right ascension of the sun
     .        DecSun,    ! declination of the sun
     .        UTC_Hr,    ! UTC hour of the day 
     .        TimeJC,    ! time at 0 on current day in julian
     .                   !   centuries from 0 UTC, Jan 1, 2000
     .        LonTNut,   ! Total nutation in longitude
     .        Equinox,   ! Equation of equinox
     .        GMST,      ! Greenwich mean siderial time
     .        GAST,      ! Greenwich apparent siderial time
     .        LAST,      ! Local mean siderial time
     .        ALon,      ! longitude value in the interval [ 0, 360 ]
     .        ALat,      ! latitude value in the interval [ -90, 90 ]
     .        HrAngle,   ! hour angle
     .        SinAlt,    ! sine of the sun's altitude
     .        AltD       ! Sun's altitude in degrees
      real ( kind=HP) :: DD, TT, GMST1, Cor

!     Calculate pi and parameters for conversion
!     ------------------------------------------
      pi      = 2.0_HP * atan2 ( 1.0_HP, 0.0_HP )
      Deg2Rad = pi / 180.0_HP

      do iPt = 1, NPts

!        Calculate astronomical Julian day and
!        convert from Julian day based on time and
!        date as defined in the calling routine
!        -----------------------------------------
         DelDays  = ( real ( DelHr ( iPt ), kind = HP )
     .              + real ( Time  ( iPt ), kind = HP ) / 3600.0_HP )
     .              / 24.0_HP

!        Begin calculations for right ascension and declination
!        of the sun ....
!        ------------------------------------------------------

!        Center the day, calulate mean logitude of sun and anomaly
!        ---------------------------------------------------------
c         MeanLon  = 280.460_HP + 0.9856474_HP * DelDays
c         ANom     = 357.528_HP + 0.9856003_HP * DelDays
         MeanLon = 280.459_HP + 0.98564736_HP * DelDays         
         ANom    = 357.529_HP + 0.98560028_HP * DelDays

!        Determine the equivalent value in the range [0,360)
!        ---------------------------------------------------
         MeanLon  = modulo ( MeanLon, 360.0_HP )
         ANom     = modulo ( ANom,    360.0_HP )

!        Find longitude and obliquity of the ecliptic (in degress)
!        ---------------------------------------------------------
         EclLon   = MeanLon 
     .            + 1.915_HP  * sin (          ANom * Deg2Rad )
     .            + 0.020_HP  * sin ( 2.0_HP * ANom * Deg2Rad )
c         Obliq    = 23.439_HP - 0.0000004_HP * DelDays
         Obliq    = 23.439_HP - 0.00000036_HP * DelDays

!        Convert to radians
!        ------------------
         EclLon   = EclLon * Deg2Rad
         Obliq    = Obliq  * Deg2Rad

!        Declination in radians
!        ----------------------
         DecSun   = asin ( sin ( Obliq ) * sin ( EclLon ) )

!        Right ascension in radians
!        --------------------------
         RtAscSun = atan ( cos ( Obliq ) * tan ( EclLon ) )
         if ( RtAscSun .lt. 0.0_HP ) RtAscSun = RtAscSun + 2.0_HP * pi

!        Put right ascension into same quandrant
!        as the longitude of the ecliptic
!        ---------------------------------------
         if ( EclLon   .gt. pi .and.
     .        EclLon   .lt. 1.5_HP * pi )
     .        RtAscSun = RtAscSun  + pi
         if ( EclLon   .gt. pi / 2.0_HP .and.
     .        EclLon   .le. pi )
     .        RtAscSun = RtAscSun - pi

!        End calculations for right ascension and declination of the
!        and begin the final calculations for the sun altitude ....
!        -----------------------------------------------------------

!        Find time in Julian centuries from J2000 at 0 UTC
!        -------------------------------------------------
         ZeroUTC =   real ( floor ( DelDays + 0.5_HP ), kind = HP )
         UTC_Hr  = ( DelDays + 0.5_HP - ZeroUTC ) * 24.0_HP
         TimeJC  = ( ZeroUTC - 0.5_HP ) / NDays_100yrs

!        Obtain the total nutation in longitude
!        --------------------------------------
         LonTNut = 0.0_HP

!        Evaluate the equation of equinox
!        --------------------------------
         Equinox = 0.0_HP

!        Find Greenwich mean siderial time (GMST) and Greenwich
!        apparent sideral time (GMST) in hours and center it 0 - 24
!        ----------------------------------------------------------
         GMST    = 24110.54841_HP 
     .           + 8640184.812886_HP * TimeJC
     .           + TimeJC ** 2 * ( 0.093104_HP  - 6.2e-6_HP * TimeJC )
         GMST    = GMST / 3600.0_HP
         GMST    = modulo ( GMST + UTC_Hr * 1.0027379093_HP, 24.0_HP )
         GAST    = modulo ( GMST + Equinox, 24.0_HP )

!        Extract coordinates and ensure that the longitude and
!        latitudes are in the intervals [ 0, 360 ) and [ -90, 90 ]
!        ---------------------------------------------------------
         ALon    = real ( Lon ( iPt ), kind = HP )
         ALon    = mod ( ALon + 360.0_HP, 360.0_HP )
         ALat    = real ( Lat ( iPt ), kind = HP )
         ALat    = max ( ALat, -90.0_HP )
         ALat    = min ( ALat,  90.0_HP )

!        Find local apparent siderial time (LAST)
!        and hour angle in radians
!        ---------------------------------------
         LAST    = GAST * 360.0_HP / 24.0_HP + ALon
         LAST    = mod ( LAST, 360.0_HP )
         HrAngle = LAST * Deg2Rad - RtAscSun
         HrAngle = mod ( HrAngle, 2.0_HP * pi )
         if ( HrAngle .gt. pi )
     .      HrAngle = HrAngle - 2.0_HP * pi
         if ( HrAngle .lt. -1.0_HP * pi )
     .      HrAngle = HrAngle + 2.0_HP * pi

!        Find altitude in degrees
!        ------------------------
         SinAlt = sin ( DecSun ) * sin ( ALat * Deg2Rad )
     .          + cos ( DecSun ) * cos ( ALat * Deg2Rad )
     .                           * cos ( HrAngle )
         AltD   = real ( asin ( SinAlt ) / Deg2Rad )

!        Correction due refraction in the atmosphere 
!        --------------------------------------------
         Cor = 0.0_HP

!        Save result
!        -----------
         Alt ( iPt ) = AltD + Cor

c       print *, Alt(iPt)
c      write ( *, 22 ) int ( UTC_hr ),
c     .         nint ( mod ( UTC_hr, 1.0_HP ) * 60.0_HP ),
c     .         UTC_Hr, 
c     .         modulo ( GAST / Deg2Rad, 360.0 ),
c     .         modulo ( LAST / Deg2Rad, 360.0 ),
c     .         DecSun / Deg2Rad, HrAngle / Deg2Rad
c 22      format ( i2,':', i2.2,1x,f5.2,4(1x,f9.3,1x))

      end do

      return
      end subroutine SunAlt_JHRefNPts

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: JulHr1() --- Returns Julian hour 
! 
! !DESCRIPTION:
!     The routine returns the Julian hour where the value is consistent
!     with the definition of JHRef_2000 in the module prologue.
!
! !INTERFACE: 
!
      function JulHr1 ( CalDate, Time, HHMMSS )
!
! !INPUT PARAMETERS:
      implicit NONE
      integer, intent (in) ::
     .   CalDate,       ! Calendar date (= YYYYMMDD)
     .   Time           ! Time          (= HH)
      logical, intent (in), optional ::
     .   HHMMSS         ! = .true. if time is in the format HHMMSS.  If
                        !   MM => 30, then the returned value is rounded up
                        !   to the next hour.  Default: HHMMSS = .false.
!
! !OUTPUT PARAMETERS:
      integer :: JulHr1 ! Julian hour number
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     15Jul2002  C. Redder   Original code.
! EOP
!-------------------------------------------------------------------------

!     Other variables
!     ---------------
      logical :: HHMMSS_format
      integer :: Hour, Minute

!     Implement option to set the format for the input parameter Time
!     ---------------------------------------------------------------
      HHMMSS_format = .false.
      if ( present ( HHMMSS )) HHMMSS_format = HHMMSS
      if ( HHMMSS_format ) then
         Hour   = Time / 10000
         Minute = mod ( Time, 10000 ) / 100
         if ( Minute .ge. 30 ) Hour = Hour + 1
      else 
         Hour   = Time
      end if

!     Calculate the Julian hour
!     -------------------------
      JulHr1 = JulDay1 ( CalDate ) * 24 + Hour - HrRef

      return
      end function JulHr1

!!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: JulHrN() --- Returns Julian hours
! 
! !DESCRIPTION:
!     The routine returns the Julian hours where the values are consistent
!     with the definition of JHRef_2000 in the module prologue.
!
! !INTERFACE: 
!
      function JulHrN ( CalDates, Times, HHMMSS )
!
! !INPUT PARAMETERS:
      implicit NONE
      integer, intent (in), dimension (:) ::
     .   CalDates,  ! Calendar dates (= YYYYMMDD)
     .   Times      ! Times          (= HH)
      logical, intent (in), optional      ::
     .   HHMMSS     ! = .true. if the times are in the format HHMMSS.  If
                    !   MM => 30, then the returned values are rounded up
                    !   to the next hour.  Default: HHMMSS = .false.
!
! !OUTPUT PARAMETERS:
      integer, dimension ( min ( size ( CalDates ), size ( Times ))) ::
     .   JulHrN     ! Julian day number
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     15Jul2002  C. Redder   Original code.
!     25Oct2003  C. Redder   Fixed bug in determining the number of dates
! EOP
!-------------------------------------------------------------------------

!     Other variables
!     ---------------
      integer ::  iDate, NDates

      NDates = min ( size ( CalDates ), size ( Times ))
      do iDate = 1, NDates
         JulHrN ( iDate ) = JulHr1 ( CalDates ( iDate ),
     .                               Times    ( iDate ),
     .                               HHMMSS = HHMMSS )
      end do

      return
      end function JulHrN

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: JulDay1() --- Returns a Julian day
! 
! !DESCRIPTION:
!     The routine returns the Julian day number based on the calendar date.
!
! !INTERFACE: 
!
      function JulDay1 ( CalDate )
!
! !INPUT PARAMETERS:
      implicit NONE
      integer, intent (in) :: CalDate  ! Calendar date (=YYYYMMDD)
!
! !OUTPUT PARAMETERS:
      integer              :: JulDay1  ! Julian day number
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     17Apr2002  C. Redder   Original code.  Routine was adapted from the
!                            ODS library routine ODS_Julian
!     06May2002  C. Redder   Incorporate from the module, m_Julian
!     06Jan2003  C. Redder   Remove first declaration of iGreg
!     20Dec2006  C. Redder   Renamed function from Julian1 to JulDay1
!
! EOP
!-------------------------------------------------------------------------

!     Other variables
!     ---------------
      integer ::  Year, Month, Day, CD
!     iGreg - Gregorian Calendar adopted Oct 12, 1582
      integer, parameter :: iGreg = 15 + 31 * ( 10 + 12 * 1582 )
      integer ::  JulDay
      integer ::  jy, jm, ja

      CD     = abs ( CalDate )
      Year   =       CD / 10000
      if ( CalDate < 0 ) Year = -Year
      Month  = mod ( CD,  10000 ) / 100
      Day    = mod ( CD,    100 )
 
!     Change year 0 to year 1
!     -----------------------
      if ( Year  .eq. 0 ) Year = 1

!     Account for the nonexisting year 0
!     ----------------------------------
      if ( Year  .lt. 0 ) Year = Year + 1

      if ( Month .gt. 2 ) then
         jy = Year
         jm = Month + 1

      else
         jy = Year  - 1
         jm = Month + 13

      endif

      JulDay = int ( 365.25  * jy )
     .       + int ( 30.6001 * jm )
     .       + Day + 1720995

!     Test whether to change to Gregorian Celendar
!     --------------------------------------------
      if ( Day + 31 * ( Month + 12 * Year ) .ge. iGreg) then
        ja     = int ( 0.01 * jy )
        Julday = JulDay + 2 - ja + int ( 0.25 * ja )

      endif

      JulDay1 = JulDay

      return
      end function JulDay1

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: JulDayN() --- Returns Julian days
! 
! !DESCRIPTION:
!     The routine returns an array Julian day numbers based on the calendar
!     dates.
!
! !INTERFACE: 
!
      function JulDayN ( CalDates )
!
! !INPUT PARAMETERS:
      implicit NONE
      integer, intent (in), dimension (:)      ::
     .   CalDates ! Calendar dates (= YYYYMMDD)
!
! !OUTPUT PARAMETERS:
      integer, dimension ( size ( CalDates ) ) ::
     .   JulDayN  ! Julian day number
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     17Apr2001  C. Redder   Original code.  Adapted 
!     06May2002  C. Redder   Incorporate from the module, m_Julian
!     20Dec2006  C. Redder   Renamed function from JulianN to JulDayN
! EOP
!-------------------------------------------------------------------------

!     Other variables
!     ---------------
      integer ::  iDate, NDates

      NDates = size ( CalDates )
      do iDate = 1, NDates
         JulDayN ( iDate ) = JulDay1 ( CalDates ( iDate ) )

      end do

      return
      end function JulDayN

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CalDHr1 () --- Julian hour to calendar date and hour
! 
! !DESCRIPTION:
!     The routine returns the calendar day based on the Julian day number.
!
! !INTERFACE: 
!
      function CalDHr1 ( JulHr, HHMMSS )
!
! !INPUT PARAMETERS:
      implicit NONE
      integer, intent (in)           ::
     .   JulHr           ! Julian hour number
      logical, intent (in), optional ::
     .   HHMMSS          ! = .true. if the time is to be in the format
                         !   HHMMSS.  Default: HHMMSS = .false.
!
! !OUTPUT PARAMETERS:
      integer, dimension (2) ::
     .   CalDHr1         ! Date in "calendar" format
                         !  (1) = date (=YYYYMMDD)
!                        !  (2) = time (=HH or HHMMSS where MMSS = 0000)
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     21Dec2006  C. Redder   Original code.
! EOP
!-------------------------------------------------------------------------

!     Other variables
!     ---------------
      integer :: JulDay, Date, Hour, Time
      logical :: HHMMSS_format 

!     Implement format
!     ----------------
      HHMMSS_format = .false.
      if ( present ( HHMMSS )) HHMMSS_format = HHMMSS

!     Compute hour and JulDay
!     -------=---------------
      Hour   = mod ( JulHr + HrRef, 24 )
      JulDay =     ( JulHr + HrRef - Hour ) / 24

!     Compute date and time (in desired format)
!     -----------------------------------------
      Date   = CalDate1 ( JulDay )
      Time   = Hour
      if ( HHMMSS_format ) Time = Hour * 10000

!     Save date and time
!     ------------------
      CalDHr1 (1) = Date
      CalDHr1 (2) = Time

      return
      end function CalDHr1

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CalDate1 () --- Convert Julian day to calendar date
! 
! !DESCRIPTION:
!     The routine returns the calendar day based on the Julian day number.
!
! !INTERFACE: 
!
      function CalDate1 ( JulDay )
!
! !INPUT PARAMETERS:
      implicit NONE
      integer, intent (in) :: JulDay   ! Julian day number
!
! !OUTPUT PARAMETERS:
      integer              :: CalDate1 ! Date in "calendar" format
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     17Apr2001  C. Redder   Original code.  Routine was adapted from the
!                            ODS library routine ODS_CalDat
!     06May2002  C. Redder   Incorporate from the module, m_Julian
! EOP
!-------------------------------------------------------------------------

!     Other variables
!     ---------------
      integer :: Year, Month, Day
!     iGreg - Gregorian Calendar adopted Oct 12, 1582
      integer, parameter :: iGreg = 2299161 ! Calendar adopted Oct 12, 1582
      integer :: Alpha
      integer :: ja, jb, jc, jd, je

!     Cross-over to Gregorian Calendar produces this correction
!     ---------------------------------------------------------
      if ( JulDay .ge. iGreg ) then
         Alpha = int ((( JulDay - 1867216 ) - 0.25 ) / 36524.25 )
         ja    = JulDay + 1 + Alpha - int ( 0.25 * Alpha )

      else ! no correction
!     --------------------
         ja    = JulDay

      endif

      jb    = ja + 1524
      jc    = int ( 6680. + (( jb - 2439870 ) - 122.1 )
     .      / 365.25)
      jd    = 365 * jc + int ( 0.25 * jc )
      je    = int (( jb - jd ) / 30.6001 )

      Day   = jb - jd - int ( 30.6001 * je )

      Month = je - 1
      if ( Month .gt. 12 ) Month = Month - 12

      Year  = jc - 4715
      if ( Month .gt. 2 ) Year = Year - 1
      if ( Year  .le. 0 ) Year = Year - 1

      CalDate1 = Year * 10000 + Month * 100 + Day 

      return
      end function CalDate1

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CalDateN () --- Convert Julian days to calendar dates
! 
! !DESCRIPTION:
!     The routine returns an array of calendar days based on the Julian day
!     numbers.
!
! !INTERFACE: 
!
      function CalDateN ( JulDays )
!
! !INPUT PARAMETERS:
      implicit NONE
      integer,    intent (in), dimension (:) ::
     .   JulDays  ! Julian day numbers
!
! !OUTPUT PARAMETERS:
      integer,    dimension ( size ( JulDays ) ) ::
     .   CalDateN ! Dates in "calendar" format
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     17Apr2001  C. Redder   Original code.
!     06May2002  C. Redder   Incorporate from the module, m_Julian
! EOP
!-------------------------------------------------------------------------

!     Other variables
!     ---------------
      integer :: iDay, NDays

      NDays = size ( JulDays )
      do iDay = 1, NDays
         CalDateN ( iDay ) = CalDate1 ( JulDays ( iDay ) )

      end do

      return
      end function CalDateN
!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Check_Date1() --- Checks calendar date
! 
! !DESCRIPTION:
!     The routine checks the given calendar date and returns 1
!     if the date is invalid or 0 if valid.
!
! !INTERFACE: 
!
      function Check_Date1 ( CalDate, Message )
!
! !INPUT PARAMETERS:
      implicit NONE
      integer,           intent (in)           ::
     .   CalDate       ! Date (= YYYYMMDD)
!
! !OUTPUT PARAMETERS:
      character (len=*), intent(out), optional ::
     .   Message       ! Return text message

      integer                                  ::
     .   Check_Date1   ! Index number
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     19Dec2006  C. Redder   Original code.
! EOP
!-------------------------------------------------------------------------
      integer, dimension (12), parameter ::
     .   MaxDay = ( / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
      integer :: DD, Year, Month, Day, stat, DayMax
      logical :: leap_year
      character (len=40) :: EMessage

!     Extract the components of the date
!     ----------------------------------
      DD    = abs ( CalDate )
      Year  =       DD / 10000
      Month = mod ( DD,  10000 ) / 100
      Day   = mod ( DD,  100   )

!     Initialize output
!     -----------------
      stat     = 0
      EMessage = ' '

!     Check month
!     -----------
      if ( Month .lt. 1 .or. Month .gt. 12 ) then
         stat = 1
         write ( EMessage,'(a,i2.2,a)')
     .      'Invalid month (=', Month, ').'
      end if

!     ... day (account for leap year)
!     -------------------------------
      if ( stat .eq. 0 ) then
         leap_year = ( mod ( Year,   4 ) .eq. 0   .and. 
     .                 mod ( Year, 100 ) .ne. 0 ) .or.
     .                 mod ( Year, 400 ) .eq. 0
         DayMax = MaxDay ( Month )
         if ( leap_year .and. Month .eq. 2    ) DayMax = 29
         if ( Day .lt. 1 .or. Day .gt. DayMax ) then
            stat = 1
            write ( EMessage,'(a,i2.2,a)')
     .         'Invalid day (=', Day, ').'
         end if
      end if

!     Return status and, if desired, appropriate error message
!     --------------------------------------------------------
      Check_Date1 = stat
      if ( present ( Message )) Message = EMessage

      return
      end function Check_Date1
!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Check_DateTime1() --- Checks calendar date and time
! 
! !DESCRIPTION:
!     The routine checks the given calendar date and time and returns 1
!     if the date is invalid or 0 if valid. 
!
! !INTERFACE: 
!
      function Check_DateTime1 ( CalDate, Time, Message, HHMMSS )
!
! !INPUT PARAMETERS:
      implicit NONE
      integer,           intent (in)           ::
     .   CalDate,        ! Calendar date (= YYYYMMDD)
     .   Time            ! Time          (=   HHMMSS)
      logical,           intent (in), optional ::
     .   HHMMSS          ! = .true. if the times are in the format HHMMSS.
                         !   HHMMSS.  Default: HHMMSS = .false.
!
! !OUTPUT PARAMETERS:
      character (len=*), intent(out), optional ::
     .   Message         ! Return text message

      integer                                  ::
     .   Check_DateTime1 ! Index number
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     19Dec2006  C. Redder   Original code.
! EOP
!-------------------------------------------------------------------------

      integer :: TT, Hour, Minute, Second, stat
      logical :: HHMMSS_format
      character (len=40) :: EMessage

!     Check the date
!     --------------
      Check_DateTime1 = Check_Date1 ( CalDate, Message )
      if ( Check_DateTime1 .ne. 0 ) return
 
!     Implement option to set the format for the input parameter Time
!     ---------------------------------------------------------------
      HHMMSS_format = .false.
      if ( present ( HHMMSS )) HHMMSS_format = HHMMSS

!     Extract the components of the date
!     ----------------------------------
      TT = abs ( Time )
      if ( HHMMSS_format ) then
         Hour   =       TT / 10000
         Minute = mod ( TT,  10000 ) / 100
         Second = mod ( TT,  100   )
      else 
         Hour   = TT
         Minute = 00
         Second = 00
      end if

!     Initialize output
!     -----------------
      stat     = 0
      EMessage = ' '

!     Check hour
!     -----------
      if ( Hour .lt. 0 .or. Hour .gt. 23 ) then
         stat = 1
         write ( EMessage,'(a,i2.2,a)')
     .      'Invalid hour (=', Hour, ').'
      end if

!     ... minute
!     ----------
      if ( stat .eq. 0 ) then
         if ( Minute .lt. 0 .or. Minute .gt. 59 ) then
            stat = 1
            write ( EMessage,'(a,i2.2,a)')
     .        'Invalid hour (=', Minute, ').'
         end if
      end if

!     ... second
!     ----------
      if ( stat .eq. 0 ) then
         if ( Second .lt. 0 .or. Second .gt. 59 ) then
            stat = 1
            write ( EMessage,'(a,i2.2,a)')
     .        'Invalid hour (=', Second, ').'
         end if
      end if

!     Return status and, if desired, appropriate error message
!     --------------------------------------------------------
      Check_DateTime1 = stat
      if ( present ( Message )) Message = EMessage

      return
      end function Check_DateTime1
!..............................................................
      end module m_SunAlt
*====================================================================
