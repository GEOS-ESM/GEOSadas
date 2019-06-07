!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_soundings --
!
! !DESCRIPTION:
!
! !INTERFACE:
!
      module    m_soundings
      use       m_stdatm, only : Z_Max => ZMax
      implicit  NONE
      private ! except

      public ::
     .   Wind_UVtoSD, ! Convert wind U and V components to speed and direction
     .   Wind_SDtoUV, ! ... and wind speed and direction to U and V components
     .   LLPath,      ! Determine the path of a ballon
     .   Z2Time,      ! Computes time since launch from heights and rise rates
     .   Z2RRate,     ! Computes rise rate from heights
     .   ZT2RRate,    ! Computes rise rate from heights and time.
     .!   ZTime2RR,    ! Computes rise rate from heights and time.
     .   dList,       ! Generates a list of obs ...
     .   dYdx,        ! ... for estimating derivatives
     .   CheckP,      ! Check pressure for reasonable physical values.
     .   SetPList,    ! Generates a list of pressure levels
     .   P2List,      ! Determines if level is on list.
     .   P2logP,      ! Computes the natural log of pressure.
     .   Z2Y,         ! Interpolates in linear heights
     .   logP2Y,      ! ... or log presssure
     .   ExtrapM,     ! Extrapolates mass variables
     .   Scan_ks,     ! Scans ks for sounding indices
     .   Z2Alt,       ! Converts geopotential height to alt (or vice versa)
     . ! ZCorr,       ! Computes geopotential heights corrections
     .   CalcZ        ! Computes geopotential heights.

      public ::
     .   ZMin,        ! Minimum and
     .   ZMax         ! ... maximum height assumed in these routines

      interface Wind_UVtoSD
         module procedure
     .      Wind_UVtoSD_,   ! Convert from speed/dir without
     .      Wind_UVtoSD_wM  ! ... or with masking
      end interface
      interface Wind_SDtoUV
         module procedure
     .      Wind_SDtoUV_,   ! Convert from speed/dir without
     .      Wind_SDtoUV_wM  ! ... or with masking
      end interface
      interface dList
         module procedure
     .      dList_,         ! Get list of obs for der estimation without ...
     .      dList_M         ! ... or with masking.
      end interface
      interface CheckP
         module procedure   ! Check pressure and, if necessary, modify
     .      CheckP_Flag     ! ... the flag
      end interface
      interface SetPList
         module procedure
     .!      logPList_kt,    ! Get list of obs for interpolation without ...
     .!      logPList_Mkt,   ! ... or with a mask.
     .      logPList_M      ! ... or with only a mask
      end interface
      interface logP2Y
         module procedure
     .      logP2Y_,         ! Interpolate without
     .      logP2Y_mask      ! ... or with masking in log pressure
      end interface
      interface Z2Y
         module procedure
     .      Z2Y_,            ! Interpolate without
     .      Z2Y_mask         ! ... or with masking in height
      end interface
c      interface ZCorr
c         module procedure   ! Calculate geopotential height corrections from
c     .      ZCorr_TW,       ! ... air temperature changes and mixing ratio
c     .      ZCorr_TV        ! ... virtual temperature changes
c      end interface
      interface CalcZ
         module procedure   ! Calculate the geopotential heights
     .      CalcZ_anchor,   ! ... with a given anchor
     .      CalcZ_noanchor  ! ... without a given anchor
      end interface

      interface dYdx
         module procedure
     .      dYdx_,
     .      dYdx_list
      end interface

! !REVISION HISTORY:
!     05Nov2001  C. Redder  Original version
!     18Apr2002  C. Redder  Added the routine, Scan_ks
!     14Aug2002  C. Redder  Made the routine name TCorr a generic interface.  
!                           Added generic interfaces toRH, toAT and ObList.
!                           Added public routine logP2Z.  Made logP2X a 
!                           public routine.
!     30Aug2002  C. Redder  Added routines, toRH_kt and toAT_kt to the
!                           generic interfaces, toRH and toAT
!     03Sep2002  C. Redder  Removed the generic interface toRH and toAT.
!     19Dec2002  C. Redder  Added the data structures, ks_info, ks_data and
!                           ks_lists.  Added the generic interfaces,
!                           ksInfo_Init, ksInfo_Clean
!     08Jan2003  C. Redder  Added generic interface, Aux_Nullify
!     30Jan2003  C. Redder  Moved auxilliary data structures and utilities
!                           to the module, m_RadAux
!     19Aug2003  C. Redder  Renamed module procedure, logPList_ and
!                           logPList_M to logPList_kt and logPList_Mkt and
!                           the module procedure, logPList_M, to the
!                           generic interface, logPList.
!     15Oct2003  C. Redder  Replace generic interface, logPList, with
!                           SetPList. and added the interface CheckP
!     30Oct2003  C. Redder  Renamed public routine from logP2X to lopP2Y
!                           and added public routine, CalcZ. Renamed
!                           pulic routine from logP2M to ExtrapM.
!     19Nov2003  C. Redder  Replaced generic interface, ZCorr, with
!                           CalcZ.
!     04Feb2004  C. Redder  Added the routine Z2Y.
!     24Mar2004  C. Redder  Made the routine logP2Y a generic interface
!                           and added the public routine, P2List.
!     05Aug2004  C. Redder  Change the public routine xY2dYdx to dYdx
!                           and made it a generic interface.
!     24Mar2004  C. Redder  Made the routine Z2Y a generic interface
!     27Mar2007  C. Redder  Add the generic interfaces Wind_UVtoSD and
!                           Wind_SDtoUV
!EOP
!-----------------------------------------------------------------

      integer, parameter ::
     .   HEIGHT  = 6,
     .   TEMP    = 8,
     .   DENSITY = 0

!     Physical constants
!     ------------------
      integer,   parameter :: HP = selected_real_kind (12)
      real (HP), parameter ::
     .   g  = 9.80665_HP, ! Gravity constant (m/sec**2)
     .   Ra = 287.04_HP,  ! The individual gas constant for dry air and ...
     .   Rv = 461.00_HP   ! ... water vapor (Joules/(kg-deg K))

      real,      parameter ::
     .   REarth    = 6369000.0,     ! Earth's radius (m)
     .   ZMin      =   -2000.0,     ! Miminum and ...
     .   ZMax      =     Z_Max,     ! Maximum height allowed (m)
     .   MinZDiff  =      5.0,      ! Minimum abs height difference allowed (m)
     .   MinPDiff  = 0.0005,        ! Minimum relative difference and ...
     .   MinPRatio = 1.0 + MinPDiff ! ... ratio between 2 consecutive levels


      contains
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Wind_UVtoSD_wM -- Convert from zonal and meridional winds to wind speed and direction with masking
!
! !DESCRIPTION:
!     Convert from zonal and meridional winds to wind speed and direction
!     with masking.
!
! !INTERFACE:
      subroutine Wind_UVtoSD_wM ( U, V, Mask, WSp, WDir )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,          intent (in)  ::
     .   U    (:), ! Zonal wind (m/s, <0 from the east)
     .   V    (:)  ! Meridional wind (m/s, <0 from the north)
      logical,       intent (in)  ::
     .   Mask (:)  ! = .true. to convert
!
! !OUTPUT PARAMETERS:
      real,          intent (out) ::
     .   WSp  (:), ! Wind speed (m/s)
     .   WDir (:)  ! Wind dir   (deg,   90 - from the east,
!                                    ! 180 - from the south )
!     Note: Number of observations is determined by the minimum size of all
!           array arguments except Mask.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     26Mar2007  C. Redder  Original code.
! EOP
!-------------------------------------------------------------------------

      real, parameter :: WSp_Threshold = 0.00001
      integer :: iOb, NObs, NMask
      real    :: Rad2Deg, WSp_, WDir_
      logical :: last_mask, mask_in

!     Determine the number of observations
!     ------------------------------------
      NObs  = min ( size ( WSp ), size ( WDir ), ! ... observations
     .              size ( U   ), size ( V    ))
      NMask = min ( size ( Mask ), NObs )        ! ... masks
      if ( NObs .le. 0 ) return                  ! Nothing to do

!     Set initial mask variables
!     --------------------------
      last_mask = .true.
      if ( NMask .gt. 0 ) last_mask = Mask ( NMask )

!     
      Rad2Deg = 180.0 / atan2 ( 0.0, -1.0 )

!     Process all obs
!     ---------------
      do iOb = 1, NObs

!        Determine if ob is to be processed
!        ----------------------------------
         mask_in = last_mask
         if ( iOb .le. NMask ) mask_in = Mask ( iOb )

!        If so, ...
!        ----------
         if ( mask_in ) then

!           ... calculate the wind speed
!           ----------------------------
            WSp_     = sqrt ( U ( iOb ) ** 2 + V ( iOb ) ** 2 )

!           ... and if the wind speed is not zero
!           -------------------------------------
            if ( WSp_ .ge. WSp_Threshold ) then

!              ... compute the direction angle
!              -------------------------------
               WDir_ = Rad2Deg * atan2 ( -U ( iOb ), -V ( iOb ))
               if ( WDir_ .lt. 0.0 ) WDir_ = WDir_ + 360.0
            else

!              ... otherwise, set both speed and direction to zero 
!              ---------------------------------------------------
               WSp_  = 0.0
               WDir_ = 0.0
            end if
            WSp  ( iOb ) = WSp_
            WDir ( iOb ) = WDir_
         end if
      end do

      return
      end subroutine Wind_UVtoSD_wM
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Wind_UVtoSD_ -- Convert from zonal and meridional winds to wind speed and direction without masking
!
! !DESCRIPTION:
!     Convert from zonal and meridional winds to wind speed and direction
!     with masking.
!
! !INTERFACE:
      subroutine Wind_UVtoSD_ ( U, V, WSp, WDir )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,          intent (in)  ::
     .   U    (:), ! Zonal wind (m/s, <0 from the east)
     .   V    (:)  ! Meridional wind (m/s, <0 from the north)
!
! !OUTPUT PARAMETERS:
      real,          intent (out) ::
     .   WSp  (:), ! Wind speed (m/s)
     .   WDir (:)  ! Wind dir   (deg, 90 - from the east,
!                  !                 180 - from the south )
!     Note: Number of observations is determined by the minimum size of all
!           array arguments.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     26Mar2007  C. Redder  Original code.
! EOP
!-------------------------------------------------------------------------

      integer :: iOb, NObs, NMask
      logical :: Mask (0)

      call Wind_UVtoSD_wM ( U, V, Mask, WSp, WDir )

      return
      end subroutine Wind_UVtoSD_
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Wind_SDtoUV_wM -- Convert from wind speed and direction to zonal and meridional wind components with masking
!
! !DESCRIPTION:
!     This convert convert wind speed and direction to zonal and
!     meridional wind components with masking
!
! !INTERFACE:
      subroutine Wind_SDtoUV_wM ( WSp, WDir, Mask, U, V )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,          intent (in)  ::
     .   WSp  (:), ! Wind speed (m/s)
     .   WDir (:)  ! Wind dir   (deg,   90 - from the east,
      logical,       intent (in)  :: ! 180 - from the south )
     .   Mask (:)  ! = .true. to convert
!
! !OUTPUT PARAMETERS:
      real,          intent (out) ::
     .   U    (:), ! Zonal wind (m/s, <0 from the east)
     .   V    (:)  ! Meridional wind (m/s, <0 from the north)
!
!     Note: Number of observations is determined by the minimum size of all
!           array arguments except Mask.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     26Mar2007  C. Redder  Original code.
! EOP
!-------------------------------------------------------------------------

      integer :: iOb, NObs, NMask
      real    :: sin_dir, cos_dir, Deg2Rad
      logical :: last_mask, mask_in

!     Determine the number of observations
!     ------------------------------------
      NObs  = min ( size ( WSp ), size ( WDir ), ! ... observations
     .              size ( U   ), size ( V    ))
      NMask = min ( size ( Mask ), NObs )        ! ... masks
      if ( NObs .le. 0 ) return                  ! Nothing to do

!     Set initial mask variables
!     --------------------------
      last_mask = .true.
      if ( NMask .gt. 0 ) last_mask = Mask ( NMask )
     
      Deg2Rad = atan2 ( 0.0, -1.0 ) / 180.0

!     Process all obs
!     ---------------
      do iOb = 1, NObs

!        Determine if ob is to be processed
!        ----------------------------------
         mask_in = last_mask
         if ( iOb .le. NMask ) mask_in = Mask ( iOb )

         if ( mask_in ) then
            U ( iOb ) = - WSp ( iOb ) * sin ( WDir ( iOb ) * Deg2Rad )
            V ( iOb ) = - WSp ( iOb ) * cos ( WDir ( iOb ) * Deg2Rad )
         end if
      end do

      return
      end subroutine Wind_SDtoUV_wM
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Wind_SDtoUV_ -- Convert from wind speed and direction to zonal and meridional wind components without masking
!
! !DESCRIPTION:
!     This convert convert wind speed and direction to zonal and
!     meridional wind components without masking
!
! !INTERFACE:
      subroutine Wind_SDtoUV_ ( WSp, WDir, U, V )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,          intent (in)  ::
     .   WSp  (:), ! Wind speed (m/s)
     .   WDir (:)  ! Wind dir   (deg,   90 - from the east,
!
! !OUTPUT PARAMETERS:
      real,          intent (out) ::
     .   U    (:), ! Zonal wind (m/s, <0 from the east)
     .   V    (:)  ! Meridional wind (m/s, <0 from the north)
!
!     Note: Number of observations is determined by the minimum size of all
!           array arguments.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     26Mar2007  C. Redder  Original code.
! EOP
!-------------------------------------------------------------------------

      integer :: iOb, NObs, NMask
      logical :: Mask (0)

      call Wind_SDtoUV_wM ( WSp, WDir, Mask, U, V )

      return
      end subroutine Wind_SDtoUV_
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: LLPath -- Determine the path of a balloon
!
! !DESCRIPTION:
!     This routine determines the path of a balloon given the launch
!     location, time since launch and horizontal winds.  The input 
!     data is assumed to be sorted according to time since launch.
!     The path is specified by the latitude and longitude at each
!     time since launch.
!
! !INTERFACE:
      subroutine LLPath ( SLat, SLon, Time, U, V, Lat, Lon )
      use        m_ellipsoid, only : DirSph, Direct, Deg2Rad, REarthMean
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,          intent (in)  ::
     .   SLat,     ! Station latitude  (deg, -90 = 90S)
     .   SLon,     ! ... and longitude (deg, -90 = 90W)
     .   Time (:), ! Time since launch (s)
     .   U    (:), ! U wind component  (m/s)
     .   V    (:)  ! V wind component  (m/s)
!
! !OUTPUT PARAMETERS:
      real,          intent (out) ::
     .   Lat  (:), ! Latitude (deg, -90 = 90S)
     .   Lon  (:)  ! ... and longiude of each observation (deg, -90 = 90W)
!
!     Note: Number of levels is determined by the minimum size of all
!           array arguments.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     05Nov2001  C. Redder  Original code.
!     18Apr2002  C. Redder  Changed method of determining the number of
!                           points from the size of the input arrays.
!     13Aug2002  C. Redder  Added required output argument, rstat.  Revised
!                           code to use allocation statement to create
!                           scratch space which was formally arrays of
!                           assumed size.
!     21Feb2003  C. Redder  Added temporary storage station lat/lon for
!                           intermediate calculations
!     17Sep2003  C. Redder  Removed required argument rstat and replaced
!                           allocatable scratch space with fixed scratch
!                           space.
!     09Aug2007  C. Redder  Made the parameter, DisMin, a real of kind HP.
! EOP
!-------------------------------------------------------------------------

      integer,   parameter :: ScrSz = 255
      real (HP), parameter :: DisMin = tiny ( 1.0_HP )
      real,dimension ( ScrSz ) :: SSLat, SSLon, Dist, Azim, Azim2
      real (HP) :: X, Y, ThisT, ThisU, ThisV, DeltaT,  Dis,
     .                   LastT, LastU, LastV, SPI
      real      :: XX, YY, RE, Deg2R
      integer :: NPts, iPt, NSeg, iSeg, iSegBeg, iSegEnd, SegSz, iSegPt

c      real:: lat1, lon1, x1, y1, z1, lat2, lon2, x2, y2, z2, dis12
c      real, parameter ::
c     .   PI2Rad = 3.141592653589793238462643383279502884 / 180.0

      NPts  = min ( size ( Time ), size ( U   ), size ( V   ),
     .                             size ( Lat ), size ( Lon ))
      if ( NPts .le. 0 ) return

      SegSz = min ( ScrSz, NPts )
      do iSegPt = 1, SegSz
         SSLat ( iSegPt ) = SLat
         SSLon ( iSegPt ) = SLon
      end do

      RE     = REarthMean
      Deg2R  = Deg2Rad
      LastT  = 0.0_HP     ! Time of launch
      LastU  = U (1)
      LastV  = V (1)
      X      = 0.0_HP     ! X and Y are coordinates on a plane with the
      Y      = 0.0_HP     !   station at the origin and positive X and Y
                          !   implies north and east of the station
      NSeg    = ( NPts - 1 ) / ScrSz + 1
      iSegBeg =   1
      do iSeg = 1, NSeg
         iSegEnd = min ( iSegBeg + ScrSz - 1, NPts )
         SegSz   = iSegEnd - iSegBeg + 1
         do iSegPt = 1, SegSz
            iPt    = iSegBeg + iSegPt - 1
            ThisT  = Time ( iPt )
            ThisU  = U    ( iPt )
            ThisV  = V    ( iPt )
            DeltaT = ThisT - LastT
            X      = X + 0.5_HP * DeltaT * ( LastU + U ( iPt ))
            Y      = Y + 0.5_HP * DeltaT * ( LastV + V ( iPt ))
            XX     = real ( X )
            YY     = real ( Y )
            Dist ( iSegPt ) = sqrt  ( XX * XX + YY * YY ) / RE
            Azim ( iSegPt ) = 0.0          
            if ( Dist ( iSegPt ) .ge. DisMin )
     .         Azim ( iSegPt ) = atan2 ( XX, YY ) / Deg2R
            LastT  = ThisT
            LastU  = ThisU
            LastV  = ThisV
         end do

         call DirSph ( SSLat ( : SegSz ), SSLon,
     .                 Dist, Azim, Lat ( iSegBeg : iSegEnd ),
     .                             Lon ( iSegBeg : iSegEnd ), Azim2 )
c      lat1 = lat ( iSegEnd )
c      lon1 = lon ( iSegEnd )
c      x1   = cos ( lat1*pi2rad )*cos ( lon1*pi2rad ) 
c      y1   = cos ( lat1*pi2rad )*sin ( lon1*pi2rad ) 
c      z1   = sin ( lat1*pi2rad )
      call Direct ( SSLat ( : SegSz ), SSLon,
     .                 Dist, Azim, Lat ( iSegBeg : iSegEnd ),
     .                             Lon ( iSegBeg : iSegEnd ), Azim2 )
c      if ( nint(SLat*100.0).eq. 4269 .and.
c     .     nint(SLon*100.0).eq.-7383)then
c         print *, SLat, SLon
c         do iSegPt = 1, SegSz
c            iPt    = iSegBeg + iSegPt - 1
c            print *, iPt, U(iPt), V(iPt), Time(iPt), Lat(iPt), Lon(iPt)
c         end do
c      end if
c      lat2 = lat ( iSegEnd )
c      lon2 = lon ( iSegEnd )
c      print *, iSeg, iSegEnd, slat, slon, lat2, lon2,
c     .         RE*dist(SegSz), time(iSegEnd)
c      x2   = cos ( lat2*pi2rad )*cos ( lon2*pi2rad ) 
c      y2   = cos ( lat2*pi2rad )*sin ( lon2*pi2rad ) 
c      z2   = sin ( lat2*pi2rad )
c      dis12 = acos (x1*x2 + y1*y2 + z1*Z2) 
c      print *, iSeg, lat(isegbeg), lon(isegbeg),
c     .         lat1, lon1, lat2, lon2,
c     .         RE* dist(SegSz), RE*dis12

         iSegBeg = iSegEnd + 1
      end do

      return
      end subroutine LLPath

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Z2RRate -- Estimates rise rate for given balloon type
!
! !DESCRIPTION:
!     This routine estimates the rise rate for a given balloon type based
!     on height data.  The rise rate function is a second degree polynomial
!     of height whose coefficients were determined by Bert Eskridge at the
!     National Climatic Data Center.  For levels above 30 km a linear
!     equation of height is used instead with coefficients determined so
!     that the equation and its derivative is continuous at 30 km.
!
! !INTERFACE:
      subroutine Z2RRate ( Z, BType, RRate )
      implicit   NONE
!
! !INPUT PARAMETERS: 
      real,              intent (in)  ::
     .   Z     (:)     ! Geopotential (?) height (m)
      character (len=*), intent (in)  ::
     .   BType         ! Balloon type.  If type is not valid then TRUK
!                      !   RS80 balloon is assumed
! !OUTPUT PARAMETERS: 
      real,              intent (out) ::
     .   RRate (:)     ! Rise rate (m/s)
!
!     Note: Number of levels is determined by the minimum size of all
!           array arguments.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     05Nov2001  C. Redder  Original code.
!     18Apr2002  C. Redder  Changed method of determining the number of
!                           points from the size of the input arrays.
!     21Aug2003  C. Redder  Added the balloon type, NCEP
! EOP
!-------------------------------------------------------------------------

      real    :: RR0, RR1, RR2, RR_ZMax, RRp_ZMax, RR, ZZ
      integer :: iOb, NObs
      real, parameter :: ZMax = 30000.0 ! Maximum height for zone where 2nd
                                        !    degree polynonial is valid

!     Select the regression coefficients for the given balloon type
!     ------------------------------------------------------------- 
      select case ( BType )
      case ( 'CANADA'    )
         RR0 = 4.8
         RR1 = 2.304929E-5
         RR2 = 1.519369E-9

      case ( 'HONG KONG' )
         RR0 = 4.8
         RR1 = 2.304929E-5
         RR2 = 1.519369E-9

      case ( 'GZZ'       )
         RR0 = 5.9
         RR1 = 2.304929E-5
         RR2 = 1.519369E-9

      case ( 'TRUK RS80' )
         RR0 = 4.514193
         RR1 = 3.310521E-5
         RR2 = 2.353235E-9

      case ( 'TRUK VIZ'  )
         RR0 = 4.273892
         RR1 = 2.869528E-5
         RR2 = 2.731693E-9

      case ( 'NCEP'      )
         RR0 = 5.0
         RR1 = 0.0
         RR2 = 0.0

      case default         ! is the type 'TRUK RS80'
         RR0 = 4.514193
         RR1 = 3.310521E-5
         RR2 = 2.353235E-9

      end select

!     Determine coefficients for linear function valid above ZMax
!     -----------------------------------------------------------
      RR_ZMax  = RR0 + RR1 * ZMax +       RR2 * ZMax ** 2
      RRp_ZMax =       RR1        + 2.0 * RR2 * ZMax

!     Determine the rise rate
!     -----------------------
      NObs = min ( size ( Z ), size ( RRate ))
      do iOb = 1, NObs
         ZZ = Z ( iOb )
         RR = RR0 + RR1 * ZZ + RR2 * ZZ ** 2 
         if ( ZZ .gt. ZMax ) RR = RR_ZMax + RRp_ZMax * ( ZZ - ZMax )
                               ! Use linear equation above ZMax
         RRate (iOb) = RR
      end do

      return
      end subroutine Z2RRate

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Z2Time -- Estimates time since launch from height and balloon rise rate
!
! !DESCRIPTION:
!     This routine estimates the time since launch from height and balloon 
!     rise rate data.
!
! !INTERFACE:
      subroutine Z2Time ( Z, StnElv, RRate, ETime, alt )
      implicit   NONE
!
! !INPUT PARAMETERS: 
      real,           intent (in) ::
     .   Z     (:), ! Height    (m)
     .   StnElv,    ! Station elevation (m)
     .   RRate (:)  ! Rise rate (m/s)
      logical,     intent (in),  optional ::
     .   alt        ! = .true. to treat input heights as altitudes.
!                   !   Default: alt = .false.
! !OUTPUT PARAMETERS:
      real,       intent (out), dimension (:) ::
     .   ETime (:)  ! Time since launch (s)
!
!     Note: Number of levels is determined by the minimum size of all
!           array arguments.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     05Nov2001  C. Redder  Original code.
!     18Apr2002  C. Redder  Changed method of determining the number of
!                           observations from the size of the input arrays.
!     27Oct2003  C. Redder  Reset elapsed time to zero for all underground
!                           levels.
! EOP
!-------------------------------------------------------------------------

      logical :: altitude
      integer :: iOb, NObs
      real    :: T0, dZ, RR, ZZ, ZZLast, StnElv_

      NObs = min ( size ( Z ), size ( RRate ), size ( ETime ))
      if ( NObs .le. 0 ) return             ! No obs, so nothing to do

      altitude  = .false.
      if ( present ( alt ) ) altitude = alt ! Impliment option

!     Time from launch to first level
!     -------------------------------
      StnElv_   = 1.0001 * StnElv
      ZZ        = Z (1)
      if ( altitude ) ZZ = ZZ / ( 1.0 + ZZ / REarth )
      dZ        = ZZ - StnElv
      ETime (1) = 0.0
      if ( ZZ .ge. StnElv_ ) ETime (1) = dZ / RRate (1) 

!     ... and to the remaining levels
!     -------------------------------
      do iOb = 2, NObs
         T0     = ETime (iOb - 1)
         ZZLast = Z (iOb - 1)
         ZZ     = Z (iOb)
         if ( altitude ) ZZLast = ZZLast / ( 1.0 + ZZLast / REarth )
         if ( altitude ) ZZ     = ZZ     / ( 1.0 + ZZ     / REarth )
         dZ     = ZZ - ZZLast
         RR     = 0.5 * ( RRate (iOb - 1) + RRate (iOb) )
         ETime (iOb) = 0.0
         if ( ZZ .ge. StnElv_ ) ETime (iOb) = T0 + dZ / RR
      end do

      return
      end subroutine Z2Time

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: ZT2RRate -- Calculates the rise rate from height data and time since launch !
! !DESCRIPTION:
!     This routine calculates the rise rate from height and time.  Each
!     rise rate value is valid from the corresponding level to the next
!     lower level.  The rise rate at all underground levels is set to the
!     value at the surface.
!
! !INTERFACE:
      subroutine ZT2RRate ( Z, StnElv, ETime, RRate, alt )
      implicit   NONE
!
! !INPUT PARAMETERS: 
      real,           intent (in) ::
     .   Z     (:), ! Height    (m)
     .   StnElv,    ! Station elevation (m)
     .   ETime (:)  ! Time since launch (s)
      logical,        intent (in),  optional ::
     .   alt        ! = .true. to treat input heights as altitudes.
!                   !   Default: alt = .false.
! !OUTPUT PARAMETERS:
      real,        intent (out), dimension (:) ::
     .   RRate (:)  ! Rise rate (m/s)
!
!     Note: Number of levels is determined by the minimum size of all
!           array arguments.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     29Oct2003  C. Redder  Original code.
! EOP
!-------------------------------------------------------------------------

      real, parameter ::
     .   dtAbsMin  = 0.09,
     .   dtMin     = 60.0,
     .   RRate_Def = 5.0 ! (in s)
      logical :: altitude
      integer :: iOb, NObs, iLast, iSfc
      real    :: dt, ET, ETLast, dZ, ZZ, ZZLast, RRLast

      NObs = min ( size ( Z ), size ( ETime ), size ( RRate ))
      if ( NObs .le. 0 ) return             ! No obs, so nothing to do

      altitude = .false.
      if ( present ( alt ) ) altitude = alt ! Impliment option

!     Determine the ground level
!     --------------------------
      iSfc = 0
      do iOb = 1, NObs
         ZZ = Z   ( iOb )
         if ( altitude ) ZZ = ZZ / ( 1.0 + ZZ / REarth )
         dZ = ZZ - StnElv
         if ( dZ .ge. 0.0 ) then
            iSfc = iOb
            exit
         end if
      end do

!     If all obs are underground then use default 
!     -------------------------------------------
      if ( iSfc .eq. 0 ) then
         do iOb = 1, NObs
            RRate ( iOb ) = RRate_Def
         end do
         return
      end if

!     Find next level up
!     ------------------
      iLast  = iSfc
      ETLast = ETime ( iSfc )
      dt     = 0.0
      ZZLast = StnElv
      do iOb = iSfc + 1, NObs
         ET    = ETime ( iOb )
         dt    = ET - ETLast
         iLast = iOb
         if ( dt .ge. dtMin ) exit            
      end do

!     If none is found then use default
!     ---------------------------------
      if ( dt .lt. dtAbsMin ) then
         do iOb = 1, NObs
            RRate ( iOb ) = RRate_Def
         end do
         return
      end if

!     Comput rise rate for first layer
!     --------------------------------
      ZZ = Z   ( iSfc )
      if ( altitude ) ZZ = ZZ / ( 1.0 + ZZ / REarth )
      ZZLast = ZZ

      ZZ = Z   ( iLast )
      if ( altitude ) ZZ = ZZ / ( 1.0 + ZZ / REarth )
      dZ = ZZ - ZZLast
      ZZLast = ZZ

      RRLast = dZ / dt
      ETLast = ETime ( iLast )

!     Assign rise rate at all levels
!     ------------------------------
      do iOb = 1, NObs
         ET  = ETime ( iOb )
         dt  = ET - ETLast
         if ( dt .ge. dtMin ) then
            ZZ = Z ( iOb )
            if ( altitude ) ZZ  = ZZ / ( 1.0 + ZZ / REarth )
            dZ = ZZ - ZZLast
            RRLast = dZ / dt
            ZZLast = ZZ
            ETLast = ET
         end if
         RRate ( iOb ) = RRLast
      end do

      return
      end subroutine ZT2RRate
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Z2Alt -- Convert geopotential height to altitude (or vice versa)
!
! !DESCRIPTION:
!     This routine convert geopotential height to altitude (or vice versa)
!     rise rate data.
!
! !INTERFACE:
      subroutine Z2Alt ( InVal, OutVal, Alt2Z )
      implicit   NONE
!
! !INPUT PARAMETERS: 
      real,         intent (in) ::
     .   InVal  (:) ! Geopotential height (or altitude if Alt2Z = .true.)(m)
      logical,      intent (in),  optional ::
     .   Alt2Z      ! = .true. to convert altitudes to geopotential heights
!                   !   Default: Alt2Z = .false.
! !OUTPUT PARAMETERS:
      real,       intent (out), dimension (:) ::
     .   OutVal (:) ! Altitude (or geopotential height if Alt2Z = .true.)(m)
!
!     Note: Number of levels is determined by the minimum size of all
!           array arguments.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     19Apr2002  C. Redder  Original code.
! EOP
!-------------------------------------------------------------------------
      integer :: iZ, NZ
      logical :: calculate_Z
      real    :: Val

!     Implement option to convert geopotential height
!     -----------------------------------------------
      calculate_Z = .false.
      if ( present ( Alt2Z )) calculate_Z = Alt2Z

!     Convert ...
!     -----------
      NZ = min ( size ( InVal ), size ( OutVal ))
      if ( calculate_Z ) then   ! ... altitude to geopotential height
         do iZ = 1, NZ
            Val = InVal ( iZ )
            Val = Val / ( 1.0 + Val / REarth )
            OutVal ( iZ ) = Val
         end do
      else                      ! ... geopotential height to altitude
         do iZ = 1, NZ
            Val = InVal ( iZ )
            Val = Val / ( 1.0 - Val / REarth )
            OutVal ( iZ ) = Val
         end do
      endif

      return
      end subroutine Z2Alt

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: dList_ -- Generates list for derivative estimatation
!
! !DESCRIPTION:
!     This routine is the same as dzList_M without the input argument, Mask.
!     See the prologue to the routine dtList_M for more information.
!
! !INTERFACE:
      subroutine dList_ ( x, dxMin, kt, ktsel, LSz, List )
!
! !INPUT PARAMETERS:
      implicit   NONE
      real,           intent (in)  ::
     .   x     (:), ! Domain variable of the derivative (e.g height, time)
     .   dxMin      ! Minimum increment allowed for x (for stable estimate).
                    !   If dxMax < 0, then x is assumed to be decreasing
      integer,        intent (in)  ::
     .   kt    (:), ! Data type
     .   ktsel      ! Selected data type index, kt.
!
! !OUTPUT PARAMETERS: 
      integer,       intent (out) ::
     .   LSz,      ! Size of the ...
     .   List (:)  ! List of valid obs.  Each entry is an index to the
                   !   arrays x and kt.
!
!     Note: Number of input observations is determined by the minimum size
!           of all array arguments (except Mask).
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     19Aug2002  C. Redder  Original code.
! EOP
!-------------------------------------------------------------------------

      call dList_M ( x, dxMin, kt, ktsel, (/.true./), LSz, List )

      return
      end subroutine dList_

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: dList_M -- Generates list via mask for derivative estimation
!
! !DESCRIPTION:
!     This routine generates a list observations for estimating the 
!     derivative for a given data type and mask array.  The routine
!     insures that sufficient time exist between two successive vertical
!     levels to grarantee a stable estimated value.  However, the lowest
!     and highest valid observations will always be included as the first
!     and last entry in the lists.  The observation attributes are assumed
!     to be sorted and rearranged in increasing of the domain variable
!     (e.g. time, height)
!
! !INTERFACE:
      subroutine dList_M ( x, dxMin, kt, ktsel, Mask, LSz, List )
      use m_StdAtm, only : StdZ2P, StdP2Z
!
! !INPUT PARAMETERS:
      implicit NONE
      real,          intent (in)  ::
     .   x    (:), ! Domain variable of the derivative (e.g height, time)
     .   dxMin     ! Minimum increment allowed for x (for stable estimate)
                   !   If dxMax < 0, then x is assumed to be decreasing
      integer,       intent (in)  ::
     .   kt   (:), ! Data type
     .   ktsel     ! Selected data type index, kt.
      logical,       intent (in)  ::
     .   Mask (:)  ! = .true./.false. to mask in/out (i.e. accept/reject)
!
! !OUTPUT PARAMETERS: 
      integer,       intent (out) ::
     .   LSz,      ! Size of the ...
     .   List (:)  ! List of valid obs.  Each entry is an index to the
                   !   arrays P and kt.
!
!     Note: Number of input observations is determined by the minimum size
!           of all array arguments (except Mask).
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     14Aug2002  C. Redder  Original code.
! EOP
!-------------------------------------------------------------------------

      logical :: accept, masked_in
      integer :: NObs, iOb, iTop, NMask
      real    :: xi, xLast, xTop, Sign, dx_Min
      real, parameter :: RError = 10.0 ** ( -precision (1.0))

      LSz   = 0                                    ! Default list size
      NMask = size ( Mask )
      NObs  = min ( size ( x ), size ( kt ), size ( List ))
      if ( NObs .le. 0 ) return                    ! Nothing to do

!     Scan each input observation
!     ---------------------------
      Sign   = 1.0
      if ( dxMin < 0 ) Sign = -1.0
      dx_Min = abs ( dxMin )
      iTop   = 0
      xTop   = x(1) -       dx_Min
      xLast  = x(1) - 2.0 * dx_Min
      do iOb = 1, NObs
         xi        = x (iOb) * Sign
         masked_in = .true.
         if ( NMask .gt. 0 ) masked_in = Mask ( min ( iOb, NMask ))
                                                   ! If an ob
         accept    =  kt (iOb) .eq. ktsel .and.    ! ... has the selected kt
     .                masked_in                    ! ... and is masked in 

         if ( accept ) then                        ! ... then locate ...
            if ( xi .gt. xTop ) then               ! ... the highest level
               xTop = xi + 2.0 * RError * abs (xi) !     for which valid data
               iTop = iOb                          !     exists
            end if
         end if

         if ( accept )                             ! ... and if the domain var
     .      accept = xi - xLast .ge. dx_Min        !     is changing enough

         if ( accept ) then
            LSz        = LSz + 1                   ! ... then add ob to the
            List (LSz) = iOb                       !     list
            xLast      = xi
         end if
      end do

!     Ensure that the highest valid ob and ...
!     ----------------------------------------
      if ( iTop .gt. 0 ) then
         if ( iTop .ne. List (LSz)) then
            if ( LSz .eq. 1 ) LSz = LSz + 1        ! ... lowest ob is
            List (LSz) = iTop                      !     included in the list
         end if
      end if

      return
      end subroutine dList_M
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CheckP_Flag -- Checks pressure and, if necessary, resets the flag
!
! !DESCRIPTION:
!     This routine checks the pressure for ensure that the values are 
!     physically and, if necessary, resets the flag to the value supplied
!     by the calling routine.
!
! !INTERFACE:
      subroutine CheckP_Flag ( P, Reset, QC )
      use m_StdAtm, only : StdZ2P
!
! !INPUT PARAMETERS:
      implicit   NONE
      real,           intent (in)  ::
     .   P     (:)  ! Pressure level (mb or hPa)
      integer,        intent (in)  ::
     .   Reset      !  Reset value if pressure is rejected
!
! !INPUT/OUTPUT PARAMETERS: 
      integer,        intent (inout) ::
     .   QC    (:)  !  Quality control flags
!
!     Note: Number of input observations is determined by the minimum size
!           of all input array arguments.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     15Oct2003  C. Redder  Original code.
!
! EOP
!-------------------------------------------------------------------------
      integer :: iLev, NLev
      real    :: PMin, PMax

      NLev = min ( size ( P ), size ( QC ))
      PMax = StdZ2P ( ZMin )
      PMin = StdZ2P ( ZMax )
      do iLev = 1, NLev
         if ( P  ( iLev ) .gt. PMax .or. P ( iLev ) .lt. PMin )
     .        QC ( iLev ) = Reset
      end do

      return
      end subroutine CheckP_Flag
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: logPList_M -- Generates list of unique pressure levels
!
! !DESCRIPTION:
!     This routine generates a list of unique pressure levels assumed to
!     be sorted in decreasing order of pressure.  Each level selected by
!     this routine has a pressure differences between adjacent levels at
!     or above a threshold defined by the parameter, MinPRatio, which is
!     internal to this module.  However, the lowest and highest valid
!     observations will always be included as the first and last entry in
!     the lists.
!
! !INTERFACE:
      subroutine logPList_M ( P, LSz, PList, IList, Indx )
      use m_StdAtm, only : StdZ2P
!
! !INPUT PARAMETERS:
      implicit   NONE
      real,           intent (in)  ::
     .   P     (:)  ! Pressure level (mb or hPa)
!
! !OUTPUT PARAMETERS: 
      integer,        intent (out) ::
     .   LSz        ! Size of the ...
      real,           intent (out), optional ::
     .   PList (:)  ! ... list of valid ob pressure levels.
      integer,        intent (out), optional ::
     .   IList (:), ! ... or list of valid obs.  Each entry is an index to
     .              !   the array P and Mask.
     .   Indx  (:)  ! Index for assigning each element in P to a list entry
                    !   in PList (or List).  If Indx = 0, then the 
!                   !   corresponding value in P is physically unreasonable. 
!
!     Note: Number of input observations is determined by the minimum size
!           of all input array arguments except PList and IList
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     18Aug2003  C. Redder  Original code.
!     10Aug2003  C. Redder  Removed required input argument Mask.  Replaced
!                           the output required argument List with the 
!                           optional output arguments, PList, IList and
!                           Indx.
! EOP
!-------------------------------------------------------------------------

      logical :: accept, get_PList, get_IList, get_Indx
      integer :: NObs, iOb, iTop, iLast
      real    :: Pi, PLast, PMin, PMax, PTop

      get_PList = present ( PList )         ! Determine which output
      get_IList = present ( IList )         !   values to return.
      get_Indx  = present ( Indx  )

      LSz       = 0                         ! Default list size
      NObs      = size ( P )                ! Number of ob
      if ( get_Indx ) NObs = min ( NObs, size ( Indx ))
      if ( NObs .le. 0 ) return             ! Nothing to do

!     Scan each input observation
!     ---------------------------
      iTop   = 0
      iLast  = 0
      PMax   = StdZ2P ( ZMin )
      PMin   = StdZ2P ( ZMax )
      PTop   = PMax
      PLast  = P(1) * ( 1.0 + 2.0 * MinPDiff )
      do iOb = 1, NObs
         Pi       = max ( PMin, min ( PMax, P ( iOb )))
         if ( Pi .lt. PTop ) then           ! Update the location of
            PTop  = Pi                      !   the highest level so 
            iTop  = iOb                     !   far
         end if
         accept = PLast / Pi .ge. MinPRatio ! If the pressure is
         if ( accept ) then                 !   decreasing enough
            LSz   = LSz + 1                 !   then add ob to the
            PLast = Pi                      !   list
            iLast = iOb
            if ( Get_PList ) PList ( LSz ) = Pi
            if ( Get_IList ) IList ( LSz ) = iOb
         end if
         if    ( Get_Indx  ) Indx  ( iOb ) = LSz
      end do

!     Ensure that the ob at the largest domain value is ...
!     -----------------------------------------------------
      if ( iTop .gt. 0 ) then
         if ( iTop .ne. iLast ) then
            if ( Get_PList ) PList ( LSz ) = PTop ! ... included in the list
            if ( Get_IList ) IList ( LSz ) = iTop
         end if
      end if

      return
      end subroutine logPList_M
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: dYdx_ -- Estimate derivatives
!
! !DESCRIPTION:
!     From a given set of range values Y (e.g. temperature) and
!     its corresponding domain values x (e.g. time, height), this
!     routine estimates derivatives (e.g  lapse rate).  If no
!     derivatives can be estimated, then zero or a user-defined value
!     is assigned for all domain values.  The sets are assumed to be
!     sorted and rearranged according to increasing or decreasing
!     order of the domain values.
!
! !INTERFACE:
      subroutine dYdx_ ( x, Y, dxMin, dYdx, C )
!
! !INPUT PARAMETERS:
      implicit NONE
      real,          intent (in)  ::
     .   x    (:), ! Domain value
     .   Y    (:), ! Range values
     .   dxMin     ! Smallest domain increment allowed for estimation
      real,        intent (in), optional ::
     .   C         ! = constant that is assigned if the estimation
                   !   cannot be made.  Default: C = 0.0 
!
! !OUTPUT PARAMETERS: 
      real,          intent (out) ::
     .   dYdx (:)  ! Estimated derivatives
!
!     Note: Number of observations is determined by the minimum size of
!           all array arguments.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     10Aug2004  C. Redder  Original code.
! EOP
!-------------------------------------------------------------------------

c      integer, parameter :: ScrSz = 255
      integer, parameter :: ScrSz = 20
      integer :: NObs, iOb, iOb1, NSeg, iSeg, iSegBeg, iSegEnd, SegSz,
     .          iList,  ListSz, LastLSz, LastLoc,
     .           PrevLoc, NextLoc, Loc ( ScrSz + 2 )
      real    :: dYdx_Loc, This_x, Last_x, dxMin_
      logical :: x_increasing, x_decreasing

      NObs  = min  ( size ( x    ),
     .               size ( Y    ),
     .               size ( dYdx ))
      if ( NObs .le. 0 ) return

      x_increasing = x ( NObs ) .gt. x ( 1 )
      x_decreasing = .not. x_increasing
      dxMin_       = abs ( dxMin )

      ListSz    = 1
      Loc ( 1 ) = 1
      Last_x    = x ( 1 )
      LastLoc   = 1

      iList     = 0
      NextLoc   = 0
 
      dYdx_Loc = 0.0                    ! ... set default value
      if ( present ( C )) dYdx_Loc = C 

!     Process observation by segments
!     -------------------------------
      NSeg    = ( NObs - 1 ) / ScrSz + 1
      iSegBeg = 1
      do iSeg = 1, NSeg
         iSegEnd = min ( iSegBeg + ScrSz - 1, NObs )
         SegSz   = iSegEnd - iSegBeg + 1

!        Determine where the altitudes change
!        ------------------------------------
         iOb1    = max ( iSegBeg + 1, LastLoc )
         do iOb  = iOb1, NObs
            This_x = x ( iOb )
            if ( LastLoc         .gt. iSegEnd ) exit
            if (( This_x - Last_x .ge. dxMin_ .and. x_increasing ) .or.
     .          ( Last_x - This_x .ge. dxMin_ .and. x_decreasing )) then
               ListSz         = ListSz + 1
               Loc ( ListSz ) = iOb
               Last_x         = This_x
               LastLoc        = iOb
            end if
         end do

         do iOb  = iSegBeg, iSegEnd

!           Update dTdt when altiudes change
!           --------------------------------
            if ( iOb   .ge. NextLoc .and.
     .           iList .lt. ListSz ) then
               iList   = iList + 1
               PrevLoc = Loc ( max ( iList - 1, 1      ))
               NextLoc = Loc ( min ( iList + 1, ListSz ))
               if ( NextLoc .gt. PrevLoc )
     .            dYdx_Loc  = ( Y ( NextLoc ) - Y ( PrevLoc ))
     .                      / ( x ( NextLoc ) - x ( PrevLoc ))
            end if

!           ... save the derivative
!           -----------------------
            dYdx ( iOb ) = dYdx_Loc            

         end do

         iList          = 1
         LastLSz        = ListSz
         ListSz         = min ( ListSz, 2 )
         Loc ( ListSz ) = Loc ( LastLSz )
         if ( LastLSz .ge. 2 ) Loc ( 1 ) = Loc ( LastLSz - 1 )

         iSegBeg        = iSegEnd + 1
      end do

      return
      end subroutine dYdx_
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: dYdx_list -- Estimate derivatives with list
!
! !DESCRIPTION:
!     From a given list of valid range values Y (e.g. temperature) and
!     its corresponding domain values x (e.g. time, height), this
!     routine estimates derivatives (e.g  lapse rate).  If the domain
!     variable is larger/smaller than the maximum/minimum, then the
!     derivative is set that at the corresponding extreme domain value.
!     If no estimation can be made for any domain value, then zero or a
!     user-defined value is assigned for all domain values.  The list is
!     assumed to be sorted and rearranged according increasing or
!     decreasing order of the domain values.
!
! !INTERFACE:
      subroutine dYdx_list ( x, Y, List, dYdx, C )
!
! !INPUT PARAMETERS:
      implicit NONE
      real,          intent (in)  ::
     .   x    (:), ! Domain value
     .   Y    (:)  ! Observation values
      integer,       intent (in)  ::
     .   List (:)  ! List of valid obs.  Each entry is an index to the
                   !   arrays, x, Y and dYdx
      real,        intent (in), optional ::
     .   C         ! = constant that is assigned if the list is empty.
                   !   Default: C = 0.0 
!
! !OUTPUT PARAMETERS: 
      real,          intent (out) ::
     .   dYdx (:)  ! Estimated derivatives
!
!     Note: Number of observations is determined by the minimum size of all
!           array arguments (except List).  The size of the list = size (List)
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     19Aug2002  C. Redder  Original code.
!     05Aug2004  C. Redder  Changed subroutine name from xY2dYdx to 
!                           dYdx_list.
! EOP
!-------------------------------------------------------------------------

      integer :: NObs, iOb, LSz, k, kObA, kObB, kOb1, kObN
      real    :: dYdxk, dYdx1, dYdxN

      LSz   = size ( List )
      NObs  = min  ( size ( x    ),
     .               size ( Y    ),
     .               size ( dYdx ))
      if ( NObs .le. 0 ) return

!     For the case when the list is ...
!     ---------------------------------
      if ( LSz .lt. 2 ) then                      ! ... insufficient
         dYdx ( : NObs ) = 0.0                    ! ... use constant value
         if ( present ( C )) dYdx ( : NObs ) = C 
         return                                   ! ... return

      else                                        ! ... sufficient to
         kOb1   =   List (   1 )
         kObN   =   List ( LSz )
         dYdx1  = ( Y ( List (   2 )) - Y ( kOb1 ))
     .          / ( x ( List (   2 )) - x ( kOb1 ))
         dYdxN  = ( Y ( kObN ) - Y ( List ( LSz - 1 )))
     .          / ( x ( kObN ) - x ( List ( LSz - 1 )))
      end if                                      ! ... get the derivatives
                                                  !     at the domain edges
!     Estimate derivatives
!     --------------------
      k      = 1
      do iOb = 1, NObs
         if      ( iOb .le. kOb1  ) then          ! If outside the domain ...
            dYdxk = dYdx1                         ! ... then extrapolate.

         else if ( iOb .gt. kObN  ) then
            dYdxk = dYdxN

         else
            if ( iOb .gt. List (k)) then
               kObA  = List (k)                   ! ... else estimate the
               kObB  = List (k + 1)               !     derivative
               dYdxk = ( Y ( kObB ) - Y ( kObA ))
     .               / ( x ( kObB ) - x ( kObA ))
               k     = k + 1                      ! Point to the next entry
            end if                                !     in the list
         end if
         dYdx (iOb) = dYdxk

      end do

      return
      end subroutine dYdx_list

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: P2logP -- Computes the natural log of pressure
!
! !DESCRIPTION:
!     This routine computes the natural log of pressure.  Each also 
!     checks, and if necessary modifies, to ensure that each value is
!     realistic.
!
! !INTERFACE:
      subroutine P2logP ( P, logP )
      use        m_StdAtm,   only : StdZ2P
!
! !INPUT PARAMETERS:
      implicit NONE
      real,           intent (in)  ::
     .   P     (:)  ! Pressure level (mb or hPa)
!
! !OUTPUT PARAMETERS: 
      real,           intent (out) ::
     .   logP  (:)  ! natural log of pressure
!
!     Note: Number of input observation is determined by the minimum size
!           of all array arguments.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     07Nov2003  C. Redder  Original code
! EOP
!-------------------------------------------------------------------------
      integer ::  NLev, iLev, iLast
      real    ::  PMin, PMax, P2, P2Last

      NLev   = min ( size ( P ), size ( logP ))
      if ( NLev .le. 0 ) return

      PMax   = StdZ2P ( ZMin )
      PMin   = StdZ2P ( ZMax )

      iLast          = 1
      P2Last         = max ( PMin, min ( PMax, P ( iLast )))
      logP ( iLast ) = log ( P2Last )
      do iLev = 2, NLev
         P2   = max ( PMin, min ( PMax, P ( iLev )))
         if ( P2 .eq. P2Last ) then
            logP ( iLev ) = logP ( iLast )  ! No need recompute for
         else                               !   the same pressure value
            logP ( iLev ) = log  ( P2 )
         end if
         iLast  = iLev
         P2Last = P2
      end do

      return
      end subroutine P2logP
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: ExtrapM -- Extrapolates mass variables in for given list of levels
!
! !DESCRIPTION:
!     For a given list of pressures or its natural log, this routine
!     extrapolates mass variables (i.e height in m, temperature in deg K,
!     or density in kg / m^3) in log pressure at any level that is above
!     the highest or below the lowest valid observation.  The
!     extraploation is performed by first computing the deviation from
!     the 1976 standard atmosphere at the highest or lowest pressure.
!     Then this deviation is added to the standard atmosphere value at
!     the desired pressure level.  If no valid observations are given,
!     then the 1976 standard atmosphere is assumed for all levels.  If
!     the top and bottom observations in the list are vertically too
!     close for stable computation, then the mean is taken as the only
!     valid observation in the list.  The list is assumed to be sorted
!     and rearranged in decreasing order of pressure.
!
! !INTERFACE:
      subroutine ExtrapM ( P, Mask, Obs, PList, M, alt, var, logP )
      use        m_StdAtm,   only : StdZ2P, StdP2Z, StdP2T, StdP2R
!
! !INPUT PARAMETERS:
      implicit NONE
      real,           intent (in)    ::
     .   P     (:)  ! Pressure or its natural log at each level (mb or hPa)
      logical,        intent (in)    ::
     .   Mask  (:)  ! = .true./.false. to mask in/out (i.e. accept/reject)
      real,           intent (in)    ::
     .   Obs   (:), ! Observation
     .   PList (:)  ! List of levels at which interpolated values are to
                    !   obtained (given as pressure or its natural log,
                    !   mb or hPa)
      logical,        intent (in), optional ::
     .   alt        ! = .true. to treat heights (input and output) as
                    !   altitude.  Default: alt = .false.
      integer,        intent (in), optional ::
     .   var        ! = 6 (default), 8 or 0 to interpolate height,
                    !   temperature or densitiy.  If the given value is
                    !   invalid then the default is assumed.
      logical,        intent (in), optional ::
     .   logP       ! = .true. if levels are given in natural log of
!                   !   pressure.  Default: logP = .false.
! !INPUT/OUTPUT PARAMETERS: 
      real,           intent (inout) ::
     .   M     (:)  ! Extrapolated values
!
!     Note: Number of input observations is determined by the minimum size
!           of all array arguments (except PList and M).  The size of the
!           list = minimum size of the arrays, PList and M
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     02Nov2001  C. Redder  Original code.
!     23Apr2002  C. Redder  Changed method of determining the number of
!                           observations from the size of the input arrays.
!                           Added input argument, Mask.
!     14Aug2002  C. Redder  Removed required input arguments, kt and Mask. 
!                           Added reqired input argument, List and optional
!                           argument, var.  Renamed output argment Z to M.
!                           Renamed routine from, logP2Z_kt_wMask to logP2M.
!                           Fixed bugs concerning empty and near empty list
!                           of valid values in obs array.
!     30Sep2003  C. Redder  Replaced routine.  Removed the required input
!                           argument, List, and added the required input
!                           arguments, Mask and PList.
!     30Oct2003  C. Redder  Added optional input argument, logP.  Removed
!                           call to logP2Y that performs interpolation.
!                           Renamed routine to ExtrapM
! EOP
!-------------------------------------------------------------------------

      integer :: NLev, NLev_Ob, NLev_L, iLev_Ob_Top, iLev_Ob_Bottom,
     .           iLev, iLev_Ob, iLev_Ob_Last, iLev_L, ktsel
      real    :: P_L, P_Ob
      real    :: MK, M1, M2, DBottom, DTop, MBottom, MTop
      real    :: P1, P2, Pk, PBottom, PTop, PMin,    PMax
      real    :: P_LBottom, P_LTop
      logical :: too_close, mask_in, linearP

      NLev_Ob = min  ( size ( P     ), size ( Obs ), size ( Mask ))
      NLev_L  = min  ( size ( PList ), size ( M   ))
      if ( NLev_L .le. 0 ) return

!     Implement option to assume levels
!     are given in linear or log pressure
!     -----------------------------------
      linearP = .true.
      if ( present ( logP )) linearP = .not. logP

!     Determine mass variable
!     -----------------------
      ktsel = HEIGHT
      if ( present ( var )) then
         if ( var .eq. TEMP    ) ktsel = TEMP
         if ( var .eq. DENSITY ) ktsel = DENSITY
      end if

      if ( linearP ) then
         PMax  = StdZ2P ( ZMin )
         PMin  = StdZ2P ( ZMax )
      else
         PMax  = log ( StdZ2P ( ZMin ))
         PMin  = log ( StdZ2P ( ZMax ))
      end if

!     Locate the ...
!     --------------
      iLev_Ob_Bottom = 0
      do iLev_Ob = 1, NLev_Ob
         P_Ob    = P    ( iLev_Ob )
         mask_in = Mask ( iLev_Ob ) .and.
     .             P_Ob .ge. PMin   .and.
     .             P_Ob .le. PMax
         if ( mask_in ) then
            iLev_Ob_Bottom = iLev_Ob                ! ... lowest valid ob
            exit
         end if
      end do
      if ( iLev_Ob_Bottom .le. 0       ) iLev_Ob_Bottom = 1

      iLev_Ob_Top    = NLev_Ob + 1
      do iLev_Ob = NLev_Ob, 1, -1
         P_Ob    = P    ( iLev_Ob )
         mask_in = Mask ( iLev_Ob ) .and.
     .             P_Ob .ge. PMin   .and.
     .             P_Ob .le. PMax
         if ( mask_in ) then
            iLev_Ob_Top = iLev_Ob                   ! ... highest valid ob
            exit
         end if
      end do
      if ( iLev_Ob_Top    .gt. NLev_Ob ) iLev_Ob_Top    = 0

!     For the case when the list is ...
!     ---------------------------------
      if ( iLev_Ob_Top    .eq. 0 ) then             ! ... empty
         DTop       = 0.0                           ! ... use standard atm
         DBottom    = 0.0                           !     to assign heights
         P_LBottom  = PMax
         P_LTop     = PMin
         if ( linearP ) then
            PBottom =       P_LBottom
            PTop    =       P_LTop
         else                                       ! ... convert back to
            PBottom = exp ( P_LBottom )             !     pressure (if
            PTop    = exp ( P_LTop    )             !     necessary)
         end if

      else                                          ! ... not empty
         P_LBottom  = P   ( iLev_Ob_Bottom )        ! ... get the pressure 
         P_LTop     = P   ( iLev_Ob_Top    )
         if ( linearP ) then
            PBottom =       P_LBottom
            PTop    =       P_LTop
         else                                       ! ... convert back to
            PBottom = exp ( P_LBottom )             !     pressure (if
            PTop    = exp ( P_LTop    )             !     necessary)
         end if
         MBottom    = Obs ( iLev_Ob_Bottom )        ! ... and values at the
         MTop       = Obs ( iLev_Ob_Top    )        !     top and bottom

         too_close  = PBottom / PTop .lt. MinPRatio ! ... and if the top and
         if ( too_close ) then                      !     bottom levels are
            PBottom = ( PBottom + PTop ) / 2.0      !     too close
            MBottom = ( MBottom + MTop ) / 2.0      ! ... combine the levels
            PTop    =   PBottom                     !     into one so that 
            MTop    =   MBottom                     !     interpolation is
         end if                                     !     never performed

         if      ( ktsel .eq. TEMP    ) then        ! ... calculated deviation
            DBottom = MBottom - StdP2T ( PBottom )  !     from standard atm at
            DTop    = MTop    - StdP2T ( PTop    )  !     bottom and top 
         else if ( ktsel .eq. DENSITY ) then        !     levels
            DBottom = MBottom - StdP2R ( PBottom )
            DTop    = MTop    - StdP2R ( PTop    )
         else
            DBottom = MBottom - StdP2Z ( PBottom, alt = alt )
            DTop    = MTop    - StdP2Z ( PTop,    alt = alt )
         end if

         if ( too_close ) then                      ! ... prevent round-off
            PTop    = PMin                          !     error from degrading
            PBottom = PMax                          !     the interpolation
         end if
      end if

!     Extrapolate
!     -----------
      do iLev_L = 1, NLev_L
         P_L = min ( max ( PList ( iLev_L ), PMin ), PMax )
         if      ( P_L .gt. P_LBottom ) then ! ... if below the lowest ...
            if ( .not. linearP      ) P_L = exp    ( P_L )
            if ( ktsel .eq. TEMP    ) Mk  = StdP2T ( P_L ) + DBottom
            if ( ktsel .eq. DENSITY ) Mk  = StdP2R ( P_L ) + DBottom
            if ( ktsel .eq. HEIGHT  ) Mk  = StdP2Z ( P_L, alt = alt )
     .                                    + DBottom
            M ( iLev_L ) = Mk

         else if ( P_L .le. P_LTop    ) then ! ... or above the highest pres-
                                             !   sure level.
            if ( .not. linearP      ) P_L = exp    ( P_L )
            if ( ktsel .eq. TEMP    ) Mk  = StdP2T ( P_L ) + DTop
            if ( ktsel .eq. DENSITY ) Mk  = StdP2R ( P_L ) + DTop
            if ( ktsel .eq. HEIGHT  ) Mk  = StdP2Z ( P_L, alt = alt )
     .                                    + DTop
            M ( iLev_L ) = Mk
         end if
      end do

      return
      end subroutine ExtrapM

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
!
! !ROUTINE: P2List --  Return navigators of valid levels
!
! !DESCRIPTION:
!     This routine returns the navigators for each segment within the
!     array of given pressure values.  Each each segment contains a
!     common pressure level on a given list.  The pressure values and
!     the list are assumed to be sorted in descending order.  If the
!     segment is not found, then the set length is zero and the
!     location, \verb|iBeg|, is assigned so that \verb|Keys[iBeg-1]
!     <~X <=~Keys[iBeg]|!
!
! !INTERFACE:
      subroutine P2List ( P, PList, Loc, Len )
!
! !INPUT PARAMETERS:
      implicit NONE
      real,           intent (in)  ::
     .   P     (:), ! Pressure or its natural log at each level (mb or hPa)
     .   PList (:)  ! List of valid levels 
!
! !OUTPUT PARAMETERS: 
      integer,        intent (out) ::
     .   Loc   (:), ! Location and
     .   Len   (:)  ! ... length of segments of array argument P that 
                    !   valid pressures.
!
!     Note: Number of input levels is determined by the size of the 
!           the array argument, P and the list size is determined by the
!           minimum size of the array arguments, PList, Loc and Len.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     25Mar2004  C. Redder  Original code.
! EOP
!-------------------------------------------------------------------------

c      integer, parameter :: ScrSz = 13
      integer, parameter :: NSigFig = precision ( 1.0 )
      real,    parameter :: Tol     = 10.0 ** ( -NSigFig + 1 )
      integer :: NLev, NLev_Ob, NLev_L,
     .           iLev, iLev_Ob, iLev_L
      real    :: P_L, P_Ob, P_L_minus, P_L_plus
      logical :: valid_level

!     Determine the size of the list
!     ------------------------------
      NLev_L    =  min ( size ( PList ), size ( Loc ), size ( Len ))
      NLev_Ob   =  size ( P )

!     Initialize navigators
!     ---------------------
      do iLev_L = 1, NLev_L
         Loc ( iLev_L ) = NLev_Ob + 1
         Len ( iLev_L ) = 0
      end do

!     Nothing more to do if either the array or the list is missing
!     -------------------------------------------------------------
      if ( NLev_Ob .le. 0 .or. 
     .     NLev_L  .le. 0 ) return

      iLev_Ob   =  NLev_Ob
      P_Ob      =  P ( NLev_Ob )

      iLev_L    =  NLev_L
      P_L       =  PList ( iLev_L )
      P_L_minus =  P_L * ( 1.0 - Tol )
      P_L_plus  =  P_L * ( 1.0 + Tol )

!     Scan backwards
!     --------------
      NLev      =  NLev_Ob + NLev_L
      do iLev   =  1, NLev
         if ( P_L_plus  .lt. P_Ob ) then
            if ( iLev_L .le. 1 ) exit
            iLev_L      = iLev_L - 1          ! ... check the list
            P_L         = PList ( iLev_L    )
            P_L_minus   = P_L * ( 1.0 - Tol )
            P_L_plus    = P_L * ( 1.0 + Tol )
            Loc ( iLev_L ) = iLev_Ob + 1

         else
            valid_level = P_Ob .gt. P_L_minus .and.
     .                    P_Ob .lt. P_L_plus  ! ... the array of pressures
            if ( valid_level ) Len ( iLev_L ) = Len ( iLev_L ) + 1
            Loc ( iLev_L ) = iLev_Ob
            if ( iLev_Ob .le. 1 ) exit
            iLev_Ob     = iLev_Ob - 1
            P_Ob        = P ( iLev_Ob )
         end if
      end do

!     Set the navigators for the remaining list entries.
!     --------------------------------------------------
      do iLev = iLev_L - 1, 1, -1
         Loc ( iLev ) = 1
      end do

      return
      end subroutine P2List

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
!
! !ROUTINE: logP2Y_ -- Interpolates a variable in log pressure for a given list of pressures
!
! !DESCRIPTION:
!     For a given list of pressures or its natural log, this routine
!     interpolates a variable in log pressure.  If any pressure level is
!     above the highest or below the lowest observation, then the value is
!     set to that at the highest or lowest level.  If no valid observations
!     are given, then a constant is assigned for all pressure levels.  The 
!     list is assumed to be sorted and rearranged in decreasing order of
!     pressure.
!
! !INTERFACE:
      subroutine logP2Y_ ( P, Obs, PList, Y, C, YExt, logP )
!
! !INPUT PARAMETERS:
      implicit NONE
      real,           intent (in)  ::
     .   P     (:)  ! Pressure or its natural log at each level (mb or hPa)
      real,           intent (in)  ::
     .   Obs   (:), ! Observation
     .   PList (:)  ! List of levels at which interpolated values are to
                    !   obtained (given as pressure or its natural log,
                    !   mb or hPa)
      real,           intent (in), optional ::
     .   C,         ! = constant that is assigned if the list is empty.
                    !   Default: C = 0.0 
     .   YExt  (:)  ! Extrapolation.values to be used for Y.  An
                    !   adjustment is applied so Y is continuous at the
                    !   points between interpolated and extrapolated
                    !   values.
      logical,        intent (in), optional ::
     .   logP       ! = .true. if levels are given in natural log of
!                   !   pressure.  Default: logP = .false.
! !OUTPUT PARAMETERS: 
      real,          intent (out) ::
     .   Y     (:)  ! Interpolated values
!
!     Note: Number of input observation is determined by the minimum
!           size of all array arguments (except PList and Y).  The size
!           of the list = minimum size of the arrays, PList and Y.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     24Mar2004  C. Redder  Original code.
!     30Aug2006  C. Redder  Made corrections to the above paragraph
!                           beginning with Note:
!     13Mar2006  C. Redder  Added the optional argument, YExt.
! EOP
!-------------------------------------------------------------------------
      logical, dimension (0) :: Mask

      call logP2Y_mask ( P, Mask, Obs, PList, Y, C, YExt, logP = logP )

      return
      end subroutine logP2Y_
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
!
! !ROUTINE: logP2Y_mask -- Interpolates with masking a variable in log pressure for a given list of pressures
!
! !DESCRIPTION:
!     For a given list of pressures or its natural log, this routine
!     interpolates with masking a variable in log pressure.  If any
!     pressure level is above the highest or below the lowest observation,
!     then the value is set to that at the highest or lowest level (unless
!     YExt is present).  If no valid observations are given, then a
!     constant is assigned for all pressure levels.  The list is assumed
!     to be sorted and rearranged in decreasing order of pressure.
!
! !INTERFACE:
      subroutine logP2Y_mask ( P, Mask, Obs, PList, Y, C, YExt, logP )
      use        m_StdAtm, only : StdZ2P
!
! !INPUT PARAMETERS:
      implicit NONE
      real,           intent (in)  ::
     .   P     (:)  ! Pressure or its natural log at each level (mb or hPa)
      logical,        intent (in)  ::
     .   Mask  (:)  ! = .true./.false. to mask in/out (i.e. accept/reject)
      real,           intent (in)  ::
     .   Obs   (:), ! Observation
     .   PList (:)  ! List of levels at which interpolated values are to
                    !   obtained (given as pressure or its natural log,
                    !   mb or hPa)
      real,           intent (in), optional ::
     .   C,         ! = constant that is assigned if the list is empty.
     .              !   Default: C = 0.0 
     .   YExt  (:)  ! Extrapolation.values to be used for Y.  An
                    !   adjustment is applied so Y is continuous at the
                    !   points between interpolated and extrapolated
                    !   values.
      logical,        intent (in), optional ::
     .   logP       ! = .true. if levels are given in natural log of
!                   !   pressure.  Default: logP = .false.
! !OUTPUT PARAMETERS: 
      real,          intent (out) ::
     .   Y     (:)  ! Interpolated/extrapolated values
!
!     Note: Number of input observation (NObs) is determined by the
!           minimum size of all array arguments (except Mask, PList, Y
!           and YExt ).  The size of the list = minimum size of the
!           arrays, PList, Y and, if present, YExt.  If the size of
!           Mask < NObs, then Mask = .true. for all obs with no
!           corresponding Mask value.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     02Nov2001  C. Redder  Original code.
!     18Apr2002  C. Redder  Changed method of determining the number of
!                           observations from the size of the input arrays.
!                           Change the attribute of theinput argument
!                           ktsel from optional to required.  Added input
!                           argument Mask.
!     14Aug2002  C. Redder  Removed required input arguments, kt, ktsel, 
!                           and Mask.  Added reqired input argument, List.
!                           Renamed routine from,  logP2X_kt_wMto logP2X.
!                           Fixed bugs concerning empty and near empty
!                           list of valid values in obs array.
!     30Sep2003  C. Redder  Replaced routine.  Removed the required input
!                           argument, List, and added the required input
!                           arguments, Mask and PList.
!     30Oct2003  C. Redder  Renamed routine from logP2X to logP2Y and
!                           added optional input argument, logP
!     24Mar2004  C. Redder  Renamed routine from logP2Y to logP2Y_mask
!     30Aug2006  C. Redder  Made corrections to the above paragraph
!                           beginning with Note:  Made changes in
!                           indexing the array Mask.
! EOP
!-------------------------------------------------------------------------

c      integer, parameter :: ScrSz = 13
      integer, parameter :: ScrSz = 255
      integer, dimension ( ScrSz ) :: iUp, iDown
      integer :: NLev, NLev_Ob, NLev_L, iLev_Ob_Bottom, iLev_Ob_Top,
     .           iLev, iLev_Ob, iLev_L, iLev_Ob_Last, NSeg, iSeg,
     .           iSegBeg, iSegEnd, SegSz, iSegPt, iD, iU, MaskSz,
     .           This_iUp, Last_iUp, Last_iDown, iLev_Ob_Beg
      real    :: P_L, P_Ob, P_Ob_Last, Pk_Last, PMin, PMax, Tol,
     .           P_L_Top_Seg, P_L_Bottom_Seg, P_L_Max, P_L_Min
      real    :: YK, Y1, Y2, Y_Ob_Bottom, Y_Ob_Top, CC
      real    :: P1, P2, Pk, P_OB_Bottom, P_Ob_Top
      real    :: YEk, delY_Bottom, delY_Top, Y_Ob, Y_L
      logical :: mask_in, LLev_was_last, linearP, new_ObLevel, log_P,
     .           use_YExt, new_level

!     Implement options to set constant
!     -----------------
      CC = 0.0
      if ( present ( C ) ) CC = C

!     ... to use predefined values to extrapolate 
!     -------------------------------------------
      use_YExt = present ( YExt )

!     ... to assume levels are given in linear or log pressure
!     --------------------------------------------------------
      log_P   = .false.
      if ( present ( logP )) log_P = logP
      linearP = .not. log_P

!     Determine dimensions
!     --------------------
      MaskSz  =        size ( Mask  )
      NLev_Ob = min  ( size ( P     ), size ( Obs ))
      NLev_L  = min  ( size ( PList ), size ( Y   ))
      if ( use_YExt ) NLev_L =  min ( size ( YExt ), NLev_L )
      if ( NLev_L .le. 0 ) return

      if ( linearP ) then
         PMax  = StdZ2P ( ZMin )
         PMin  = StdZ2P ( ZMax )
         Tol   = MinPRatio
      else
         PMax  = log ( StdZ2P ( ZMin ))
         PMin  = log ( StdZ2P ( ZMax ))
         Tol   = log ( MinPRatio )
      end if

!     Obtain the pressure and the valid observed value at the lowest level
!     --------------------------------------------------------------------
      iLev_Ob_Bottom = 0
      do iLev_Ob = 1, NLev_Ob
         P_Ob    = P    ( iLev_Ob )
         mask_in = P_Ob .ge. PMin   .and.
     .             P_Ob .le. PMax
         if ( iLev_Ob .le. MaskSz )
     .        mask_in      = mask_in .and. Mask ( iLev_Ob )
         if ( mask_in ) then
            iLev_Ob_Bottom = iLev_Ob  ! ... lowest valid ob
            exit
         end if
      end do
      if ( iLev_Ob_Bottom .le. 0 ) then
         P_Ob_Bottom    = PMin        ! If no valid ob is found set the
         Y_Ob_Bottom    = CC          !    parameters so that the routine
         iLev_Ob_Bottom = 1           !    will properly set the output
      else                            !    values to CC
         P_Ob_Bottom    = P   ( iLev_Ob_Bottom )
         Y_Ob_Bottom    = Obs ( iLev_Ob_Bottom )
      end if

!     Determine the necessary adjustment to the YExt
!     values to insure continuity at P_Ob_Bottom
!     ----------------------------------------------
      delY_Bottom = 0.0
      if ( use_YExt ) then
         if      ( P_Ob_Bottom .ge. PList ( 1      )) then
            delY_Bottom = Y_Ob_Bottom - YExt ( 1 ) 

         else if ( P_Ob_Bottom .le. PList ( NLev_L )) then
            delY_Bottom = Y_Ob_Bottom - YExt ( NLev_L ) 

         else if ( log_P .and.
     .             PList ( 1 ) - PList ( NLev_L ) .lt. Tol ) then
            delY_Bottom = Y_Ob_Bottom
     .                  - 0.5 * ( YExt ( 1 ) + YExt ( NLev_L ))

         else if ( linearP .and.
     .             PList ( 1 ) / PList ( NLev_L ) .lt. Tol ) then
            delY_Bottom = Y_Ob_Bottom
     .                  - 0.5 * ( YExt ( 1 ) + YExt ( NLev_L ))

         else
            P_L_Min     =  PList ( NLev_L ) * Tol
            if ( log_P )
     .         P_L_Min  =  PList ( NLev_L ) + Tol
            Y_Ob        =  Y_Ob_Bottom
            P_Ob        =  P_Ob_Bottom
            do iLev_L = 1, NLev_L
               P_L      = PList ( iLev_L )
               Y_L      =  YExt ( iLev_L )
               if      ( P_L .gt. P_Ob .and. 
     .                   P_L .ge. P_L_Min ) then
                  P1 =   P_L
                  Y1 =   Y_L
               else if ( P_L .le. P_Ob    ) then
                  P2 =   P_L
                  if ( log_P ) then
                     new_level = P1 - P2 .ge. Tol
                  else
                     new_level = P1 / P2 .ge. Tol
                  end if
                  if ( new_Level ) then
                     Y2 = Y_L
                     Pk = P_Ob
                     if ( log_P ) then
                        delY_Bottom = Y_Ob - Y1 -     ( Y2 - Y1 ) 
     .                                          *     ( Pk - P1 )
     .                                          /     ( P2 - P1 )
                     else
                        delY_Bottom = Y_Ob - Y1 -     ( Y2 - Y1 ) 
     .                                          * log ( Pk / P1 )
     .                                          / log ( P2 / P1 )
                     end if
                     exit
                  end if
               end if
            end do
         end if
         Y_Ob_Bottom = 0.0
      end if

!     ... and the highest level
!     -------------------------
      iLev_Ob_Top = NLev_Ob + 1
      do iLev_Ob = NLev_Ob, 1, -1
         P_Ob    = P    ( iLev_Ob )
         mask_in = P_Ob .ge. PMin   .and.
     .             P_Ob .le. PMax
         if ( iLev_Ob .le. MaskSz )
     .        mask_in   = mask_in .and. Mask ( iLev_Ob )
         if ( mask_in ) then
            iLev_Ob_Top = iLev_Ob     ! ... highest valid ob
            exit
         end if
      end do
      if ( iLev_Ob_Top    .gt. NLev_Ob ) then
         P_Ob_Top     = PMax
         Y_Ob_Top     = CC
         iLev_Ob_Top  = 0
      else
         P_Ob_Top     = P   ( iLev_Ob_Top    )
         Y_Ob_Top     = Obs ( iLev_Ob_Top    )
      end if

      delY_Top = 0.0
      if ( use_YExt ) then
      end if

!     Determine the necessary adjustment to the YExt
!     values to insure continuity at P_Ob_Top
!     ----------------------------------------------
      delY_Top = 0.0
      if ( use_YExt ) then
         if      ( P_Ob_Top .ge. PList ( 1      )) then
            delY_Top = Y_Ob_Top - YExt ( 1 ) 

         else if ( P_Ob_Top .le. PList ( NLev_L )) then
            delY_Top = Y_Ob_Top - YExt ( NLev_L ) 

         else if ( log_P .and.
     .             PList ( 1 ) - PList ( NLev_L ) .lt. Tol ) then
            delY_Top = Y_Ob_Top
     .               - 0.5 * ( YExt ( 1 ) + YExt ( NLev_L ))

         else if ( linearP .and.
     .             PList ( 1 ) / PList ( NLev_L ) .lt. Tol ) then
            delY_Top = Y_Ob_Top
     .               - 0.5 * ( YExt ( 1 ) + YExt ( NLev_L ))

         else
            P_L_Max     =  PList ( 1 ) / Tol
            if ( log_P )
     .         P_L_Max  =  PList ( 1 ) - Tol
            Y_Ob        =  Y_Ob_Top
            P_Ob        =  P_Ob_Top
            do iLev_L = NLev_L, 1, -1
               P_L      = PList ( iLev_L )
               Y_L      =  YExt ( iLev_L )
               if      ( P_L .lt. P_Ob .and. 
     .                   P_L .le. P_L_Max ) then
                  P1 =   P_L
                  Y1 =   Y_L
               else if ( P_L .ge. P_Ob    ) then
                  P2 =   P_L
                  if ( log_P ) then
                     new_level = P2 - P1 .ge. Tol
                  else
                     new_level = P2 / P1 .ge. Tol
                  end if
                  if ( new_Level ) then
                     Y2 = Y_L
                     Pk = P_Ob
                     if ( log_P ) then
                        delY_Top = Y_Ob - Y1 -     ( Y2 - Y1 ) 
     .                                       *     ( Pk - P1 )
     .                                       /     ( P2 - P1 )
                     else
                        delY_Top = Y_Ob - Y1 -     ( Y2 - Y1 ) 
     .                                       * log ( Pk / P1 )
     .                                       / log ( P2 / P1 )
                     end if
                     exit
                  end if
               end if
            end do
         end if
         Y_Ob_Top = 0.0
      end if

!     Process the list in segments 
!     ----------------------------
      iLev_Ob_Last    = iLev_Ob_Bottom
      Last_iUp        = iLev_Ob_Last

      NSeg    = ( NLev_L - 1 ) / ScrSz + 1
      iSegBeg = 1
      do iSeg = 1, NSeg

         iSegEnd = min ( iSegBeg + ScrSz - 1, NLev_L )
         SegSz   = iSegEnd - iSegBeg + 1

!        Locate the next lower valid observation ...
!        -------------------------------------------
         iLev_Ob       =  iLev_Ob_Last
         P_Ob          =  P_Ob_Top / MinPRatio
         if ( NLev_Ob .gt. 0 ) P_Ob = P ( iLev_Ob_Last )
         P_Ob_Last     =  P_Ob

         iLev_L        =  iSegBeg
         P_L           =  PList ( iLev_L  )
         LLev_was_last = .false.

         NLev     = SegSz + iLev_Ob_Top - iLev_Ob_Last + 1
         do iLev  = 1, NLev
            if ( P_Ob    .gt. P_L .and.
     .           iLev_Ob .lt. iLev_Ob_Top ) then
               mask_in = .true.
               if ( iLev_Ob .le. MaskSz ) mask_in = Mask ( iLev_Ob )
               if ( mask_in ) then
                  if ( log_P ) then
                     new_ObLevel   = P_Ob_Last - P_Ob .ge. Tol
                  else
                     new_ObLevel   = P_Ob_Last / P_Ob .ge. Tol
                  end if
                  if ( new_ObLevel ) then
                     iLev_Ob_Last  = iLev_Ob
                     P_Ob_Last     =  P_Ob
                     if ( LLev_was_last ) iUp ( iSegPt ) = iLev_Ob_Last
                     LLev_was_last = .false.
                  end if
               end if
               iLev_Ob   = iLev_Ob + 1
               P_Ob      = P    ( iLev_Ob )

            else
               iSegPt           =  iLev_L - iSegBeg + 1
               iDown ( iSegPt ) =  iLev_Ob_Last
               iUp   ( iSegPt ) =  0
               LLev_was_last    = .true.
               if ( iSegPt .ge. SegSz ) exit
               iLev_L           =  iLev_L + 1
               P_L              =  PList ( iLev_L )

            end if
         end do

!        Locate first valid ob above P_L_Top_Seg
!        ---------------------------------------
         P_L_Top_Seg = PList ( iSegEnd )
         iLev_Ob_Beg = max ( iLev_Ob_Last, Last_iUp )
         This_iUp    = iLev_Ob_Top
         do iLev_Ob  = iLev_Ob_Beg, iLev_Ob_Top
            P_Ob     = P    ( iLev_Ob )
            mask_in  = .true.
            if ( iLev_Ob .le. MaskSz ) mask_in = Mask ( iLev_Ob )
            This_iUp = iLev_Ob
            if ( mask_in ) then
               if ( log_P ) then
                  new_ObLevel = P_Ob_Last - P_Ob .ge. Tol
               else
                  new_ObLevel = P_Ob_Last / P_Ob .ge. Tol
               end if
               if ( new_ObLevel .and. P_Ob .le. P_L_Top_Seg ) exit
            end if
         end do

!        ... and the next higher observation for each level in the list
!        --------------------------------------------------------------
         Last_iUp      = This_iUp
         iUp ( SegSz ) = Last_iUp
         do iSegPt = SegSz - 1, 1, -1
            This_iUp   = iUp ( iSegPt )
            if ( This_iUp .ne. 0 ) Last_iUp = This_iUp
            iUp ( iSegPt ) = Last_iUp
         end do

!        Interpolate or extrapolate
!        --------------------------
         Pk_Last = PList ( iSegBeg ) * MinPRatio
         do iLev_L   = iSegBeg, iSegEnd
            iSegPt   = iLev_L - iSegBeg + 1 
            Pk       = PList ( iLev_L )
            YEk      = 0.0
            if      ( use_YExt )
     .         YEk   = YExt ( iLev_L )           ! Extraplolate if ...
            if      ( Pk .gt. P_Ob_Bottom ) then ! ... below the lowest ...
               Yk    = Y_Ob_Bottom + YEk + delY_Bottom

            else if ( Pk .le. P_Ob_Top    ) then ! ... or above the highest
               Yk    = Y_Ob_Top    + YEk + delY_Top  ! level with valid ob

            else if ( Pk .ne. Pk_Last     ) then ! If the listed level has
               iD    = iDown   ( iSegPt )        !   changed then assume
               iU    = iUp     ( iSegPt )        !   constant layer mean
               Y1    = Obs     ( iD )            !   scale height and then ...
               P1    = P       ( iD )            !
               Y2    = Obs     ( iU )            !
               P2    = P       ( iU )            !
               if      ( Pk .eq. P1 .or.         ! ... if at valid obser-
     .                   iD .eq. iU       ) then !   vation level, then use
                  Yk = Obs     ( iD )            !   the ob value
               else if ( log_P            ) then ! ... else, interpolate
                  Yk = Y1 + ( Y2 - Y1 )          !   assuming that levels
     .                    * ( P1 - Pk )          !   are given in log
     .                    / ( P1 - P2 )          !   pressures
               else
                  Yk = Y1 + ( Y2 - Y1 )          ! ... or linear pressures
     .                    * log ( P1 / Pk )
     .                    / log ( P1 / P2 )
               end if
            end if
            Y ( iLev_L ) = Yk
            Pk_Last      = Pk
         end do

         iSegBeg      = iSegEnd + 1
         Last_iDown   = iDown ( SegSz )
         Last_iUp     = iUp   ( SegSz )
         iLev_Ob_Last = Last_iDown
      end do

      return
      end subroutine logP2Y_mask
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
!
! !ROUTINE: Z2Y_ -- Interpolates a variable in linear heights for a given list of heights
!
! !DESCRIPTION:
!     For a given list of heights, this routine interpolates a variable in
!     linear heights.  If any height is above the highest or below the
!     lowest observation, then the value is set to that at the highest or
!     lowest level.  If no valid observations are given, then a constant
!     is assigned for levels.  The list is assumed to be sorted and
!     rearranged in increasing order of heights.
!
! !INTERFACE:
      subroutine Z2Y_ ( Z, Obs, ZList, Y, C, YExt )
!
! !INPUT PARAMETERS:
      implicit NONE
      real,           intent (in)  ::
     .   Z     (:)  ! Height of each level (m)
      real,           intent (in)  ::
     .   Obs   (:), ! Observation
     .   ZList (:)  ! List of levels at which interpolated values are to
                    !   obtained (given as heights,m)
      real,           intent (in), optional ::
     .   C,         ! = constant that is assigned if the list is empty.
     .              !   Default: C = 0.0 
     .   YExt  (:)  ! Extrapolation.values to be used for Y.  An
                    !   adjustment is applied so Y is continuous at the
                    !   points between interpolated and extrapolated
!                   !   values.
! !OUTPUT PARAMETERS: 
      real,          intent (out) ::
     .   Y     (:)  ! Interpolated values
!
!     Note: Number of input observation is determined by the minimum
!           size of all array arguments (except PList and Y).  The size
!           of the list = minimum size of the arrays, PList and Y.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     30Aug2006  C. Redder  Original code.
!     24Mar2007  C. Redder  Added the optional argument YExt
! EOP
!-------------------------------------------------------------------------
      logical, dimension (0) :: Mask

      call Z2Y_mask ( Z, Mask, Obs, ZList, Y,
     .                C    = C,
     .                YExt = YExt )

      return
      end subroutine Z2Y_
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
!
! !ROUTINE: Z2Y_mask -- Interpolates with masking a variable in linear heights for a given list of heights
!
! !DESCRIPTION:
!     For a given list of heights, this routine interpolates with
!     masking a variable in linear heights.  If any height is above the
!     highest or below the lowest observation, then the value is set to
!     that at the highest or lowest level (unless YExt is present).
!     If no valid observations are given, then a constant is assigned
!     for levels.  The list is assumed to be sorted and rearranged in
!     increasing order of heights.
!
! !INTERFACE:
      subroutine Z2Y_mask ( Z, Mask, Obs, ZList, Y, C, YExt )
      use        m_StdAtm, only : StdZ2P
!
! !INPUT PARAMETERS:
      implicit NONE
      real,           intent (in)  ::
     .   Z     (:)  ! Height of each level (m)
      logical,        intent (in)  ::
     .   Mask  (:)  ! = .true./.false. to mask in/out (i.e. accept/reject)
      real,           intent (in)  ::
     .   Obs   (:), ! Observation
     .   ZList (:)  ! List of levels at which interpolated values are to
                    !   obtained (given as heights,m)
      real,           intent (in), optional ::
     .   C,         ! = constant that is assigned if the list is empty.
     .              !   Default: C = 0.0 
     .   YExt  (:)  ! Extrapolation.values to be used for Y.  An
                    !   adjustment is applied so Y is continuous at the
                    !   points between interpolated and extrapolated
!                   !   values.
! !OUTPUT PARAMETERS: 
      real,          intent (out) ::
     .   Y     (:)  ! Interpolated/extrapolated values
!
!     Note: Number of input observation (NObs) is determined by the
!           minimum size of all array arguments (except Mask, ZList, Y
!           and YExt ).  The size of the list = minimum size of the
!           arrays, ZList, Y and, if present, YExt.  If the size of
!           Mask < NObs, then Mask = .true. for all obs with no
!           corresponding Mask value.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     04Feb2004  C. Redder  Original code.
!     30Aug2006  C. Redder  Allowed the size of Mask to be smaller
!                           than the size of Z and Obs.  Renamed the
!                           routine Z2Y to Z2Y_mask.
! EOP
!-------------------------------------------------------------------------

c      integer, parameter :: ScrSz = 13
      integer, parameter :: ScrSz = 255
      integer, dimension ( ScrSz ) :: iUp, iDown
      integer :: NLev, NLev_Ob, NLev_L, iLev_Ob_Bottom, iLev_Ob_Top,
     .           iLev, iLev_Ob, iLev_L, iLev_Ob_Last, NSeg, iSeg,
     .           iSegBeg, iSegEnd, SegSz, iSegPt, iD, iU, MaskSz,
     .           This_iUp, Last_iUp, Last_iDown, iLev_Ob_Beg,
     .           iMask_Max, iMask
      real    :: Z_L, Z_Ob, Z_Ob_Last, Zk_Last, Tol,
     .           Z_L_Top_Seg, Z_L_Bottom_Seg, Z_L_Max, Z_L_Min
      real    :: YK, Y1, Y2, Y_Ob_Bottom, Y_Ob_Top, CC
      real    :: Z1, Z2, Zk, Z_OB_Bottom, Z_Ob_Top
      real    :: YEk, delY_Bottom, delY_Top, Y_Ob, Y_L
      logical :: mask_in, LLev_was_last, new_ObLevel, use_YExt,
     .           new_Level

!     Impliment options
!     -----------------
      CC = 0.0
      if ( present ( C ) ) CC = C

      use_YExt = present ( YExt )
    
!     Determine dimensions
!     --------------------
      MaskSz    =        size ( Mask  )
      NLev_Ob   = min  ( size ( Z     ), size ( Obs ))
      NLev_L    = min  ( size ( ZList ), size ( Y   ))
      if ( use_YExt )    NLev_L =  min ( size ( YExt ), NLev_L )
      if ( NLev_L .le. 0 ) return

      Tol   = MinZDiff

!     Obtain the pressure and the valid observed value at the lowest level
!     --------------------------------------------------------------------
      iLev_Ob_Bottom = 0
      do iLev_Ob = 1, NLev_Ob
         Z_Ob    = Z    ( iLev_Ob )
         mask_in = Z_Ob .ge. ZMin   .and.
     .             Z_Ob .le. ZMax
         if ( iLev_Ob .le. MaskSz )
     .        mask_in   = mask_in .and. Mask ( iLev_Ob )
         if ( mask_in ) then
            iLev_Ob_Bottom = iLev_Ob  ! ... lowest valid ob
            exit
         end if
      end do
      if ( iLev_Ob_Bottom .le. 0 ) then
         Z_Ob_Bottom    = ZMin        ! If no valid ob is found set the
         Y_Ob_Bottom    = CC          !    parameters so that the routine
         iLev_Ob_Bottom = 1           !    will properly set the output
      else                            !    values to CC
         Z_Ob_Bottom    = Z   ( iLev_Ob_Bottom )
         Y_Ob_Bottom    = Obs ( iLev_Ob_Bottom )
      end if

!     Determine the necessary adjustment to the YExt
!     values to insure continuity at Z_Ob_Bottom
!     ----------------------------------------------
      delY_Bottom = 0.0
      if ( use_YExt ) then
         if      ( Z_Ob_Bottom .le. ZList ( 1      )) then
            delY_Bottom = Y_Ob_Bottom - YExt ( 1 ) 

         else if ( Z_Ob_Bottom .ge. ZList ( NLev_L )) then
            delY_Bottom = Y_Ob_Bottom - YExt ( NLev_L ) 

         else if ( ZList ( NLev_L ) - ZList ( 1 ) .lt. Tol ) then
            delY_Bottom = Y_Ob_Bottom
     .                  - 0.5 * ( YExt ( 1 ) + YExt ( NLev_L ))
         else
            Z_L_Max     =  ZList ( NLev_L ) - Tol
            Y_Ob        =  Y_Ob_Bottom
            Z_Ob        =  Z_Ob_Bottom
            do iLev_L = 1, NLev_L
               Z_L      = ZList ( iLev_L )
               Y_L      =  YExt ( iLev_L )
               if      ( Z_L .lt. Z_Ob .and. 
     .                   Z_L .le. Z_L_Max ) then
                  Z1 =   Z_L
                  Y1 =   Y_L
               else if ( Z_L .ge. Z_Ob    ) then
                  Z2 =   Z_L
                  new_Level = Z2 - Z1 .gt. Tol
                  if ( new_Level ) then
                     Y2 = Y_L
                     Zk = Z_Ob
                     delY_Bottom = Y_Ob - Y1 - ( Y2 - Y1 ) 
     .                                       * ( Zk - Z1 )
     .                                       / ( Z2 - Z1 )
                     exit
                  end if
               end if
            end do
         end if
         Y_Ob_Bottom = 0.0
      end if

!     ... and the highest level
!     -------------------------
      iLev_Ob_Top = NLev_Ob + 1
      do iLev_Ob = NLev_Ob, 1, -1
         Z_Ob    = Z    ( iLev_Ob )
         mask_in = Z_Ob .ge. ZMin   .and.
     .             Z_Ob .le. ZMax
         if ( iLev_Ob .le. MaskSz )
     .        mask_in   = mask_in .and. Mask ( iLev_Ob )
         if ( mask_in ) then
            iLev_Ob_Top = iLev_Ob     ! ... highest valid ob
            exit
         end if
      end do
      if ( iLev_Ob_Top    .gt. NLev_Ob ) then
         Z_Ob_Top     = ZMax
         Y_Ob_Top     = CC
         iLev_Ob_Top  = 0
      else
         Z_Ob_Top     = Z   ( iLev_Ob_Top    )
         Y_Ob_Top     = Obs ( iLev_Ob_Top    )
      end if

!     Determine the necessary adjustment to the YExt
!     values to insure continuity at Z_Ob_Top
!     ----------------------------------------------
      delY_Top = 0.0
      if ( use_YExt ) then
         if      ( Z_Ob_Top .le. ZList ( 1      )) then
            delY_Top    = Y_Ob_Top - YExt ( 1 ) 

         else if ( Z_Ob_Top .ge. ZList ( NLev_L )) then
            delY_Top    = Y_Ob_Top - YExt ( NLev_L ) 

         else if ( ZList ( NLev_L ) - ZList ( 1 ) .lt. Tol ) then
            delY_Top    = Y_Ob_Top
     .                  - 0.5 * ( YExt ( 1 ) + YExt ( NLev_L ))
         else
            Z_L_Min     =  ZList ( 1 ) + Tol
            Y_Ob        =  Y_Ob_Top
            Z_Ob        =  Z_Ob_Top
            do iLev_L = NLev_L, 1, -1
               Z_L      = ZList ( iLev_L )
               Y_L      =  YExt ( iLev_L )
               if      ( Z_L .gt. Z_Ob .and. 
     .                   Z_L .ge. Z_L_Min ) then
                  Z2 =   Z_L
                  Y2 =   Y_L
               else if ( Z_L .le. Z_Ob    ) then
                  Z1 =   Z_L
                  new_Level = Z2 - Z1 .gt. Tol
                  if ( new_Level ) then
                     Y1 = Y_L
                     Zk = Z_Ob
                     delY_Top = Y_Ob - Y1 - ( Y2 - Y1 ) 
     .                                    * ( Zk - Z1 )
     .                                    / ( Z2 - Z1 )
                     exit
                  end if
               end if
            end do
         end if
         Y_Ob_Top = 0.0
      end if

!     Process the list in segments 
!     ----------------------------
      iLev_Ob_Last    = iLev_Ob_Bottom
      Last_iUp        = iLev_Ob_Last

      NSeg    = ( NLev_L - 1 ) / ScrSz + 1
      iSegBeg = 1
      do iSeg = 1, NSeg

         iSegEnd = min ( iSegBeg + ScrSz - 1, NLev_L )
         SegSz   = iSegEnd - iSegBeg + 1

!        Locate the next lower valid observation ...
!        -------------------------------------------
         iLev_Ob       =  iLev_Ob_Last
         Z_Ob          =  Z_Ob_Top + MinZDiff
         if ( NLev_Ob .gt. 0 ) Z_Ob = Z ( iLev_Ob_Last )
         Z_Ob_Last     =  Z_Ob

         iLev_L        =  iSegBeg
         Z_L           =  ZList ( iLev_L  )
         LLev_was_last = .false.

         NLev     = SegSz + iLev_Ob_Top - iLev_Ob_Last + 1
         do iLev  = 1, NLev
            if ( Z_Ob    .lt. Z_L .and.
     .           iLev_Ob .lt. iLev_Ob_Top ) then
               mask_in = .true.
               if ( iLev_Ob .le. MaskSz ) mask_in = Mask ( iLev_Ob )
               if ( mask_in ) then
                  new_ObLevel   = Z_Ob - Z_Ob_Last .ge. Tol
                  if ( new_ObLevel ) then
                     iLev_Ob_Last  = iLev_Ob
                     Z_Ob_Last     =  Z_Ob
                     if ( LLev_was_last ) iUp ( iSegPt ) = iLev_Ob_Last
                     LLev_was_last = .false.
                  end if
               end if
               iLev_Ob   = iLev_Ob + 1
               Z_Ob      = Z    ( iLev_Ob )

            else
               iSegPt           =  iLev_L - iSegBeg + 1
               iDown ( iSegPt ) =  iLev_Ob_Last
               iUp   ( iSegPt ) =  0
               LLev_was_last    = .true.
               if ( iSegPt .ge. SegSz ) exit
               iLev_L           =  iLev_L + 1
               Z_L              =  ZList ( iLev_L )

            end if
         end do

!        Locate first valid ob above P_L_Top_Seg
!        ---------------------------------------
         Z_L_Top_Seg = ZList ( iSegEnd )
         iLev_Ob_Beg = max ( iLev_Ob_Last, Last_iUp )
         This_iUp    = iLev_Ob_Top
         do iLev_Ob  = iLev_Ob_Beg, iLev_Ob_Top
            Z_Ob     = Z ( iLev_Ob )
            This_iUp = iLev_Ob
            mask_in  = .true.
            if ( iLev_Ob .le. MaskSz ) mask_in = Mask ( iLev_Ob )
            if ( mask_in ) then
               new_ObLevel = Z_Ob - Z_Ob_Last .ge. Tol
               if ( new_ObLevel .and. Z_Ob .ge. Z_L_Top_Seg ) exit
            end if
         end do

!        ... and the next higher observation for each level in the list
!        --------------------------------------------------------------
         Last_iUp      = This_iUp
         iUp ( SegSz ) = Last_iUp
         do iSegPt = SegSz - 1, 1, -1
            This_iUp   = iUp ( iSegPt )
            if ( This_iUp .ne. 0 ) Last_iUp = This_iUp
            iUp ( iSegPt ) = Last_iUp
         end do

!        Interpolate or extrapolate
!        --------------------------
         Zk_Last = ZList ( iSegBeg ) - MinZDiff
         do iLev_L   = iSegBeg, iSegEnd
            iSegPt   = iLev_L - iSegBeg + 1 
            Zk       = ZList ( iLev_L )
            YEk      = 0.0
            if      ( use_YExt )
     .         YEk   = YExt ( iLev_L )           ! Extraplolate if ...
            if      ( Zk .lt. Z_Ob_Bottom ) then ! ... below the lowest ...
               Yk    = Y_Ob_Bottom + YEk + delY_Bottom

            else if ( Zk .ge. Z_Ob_Top    ) then ! ... or above the highest
               Yk    = Y_Ob_Top    + YEk + delY_Top  ! level with valid ob

            else if ( Zk .ne. Zk_Last     ) then ! If the listed level has
               iD    = iDown   ( iSegPt )        !   changed then
               iU    = iUp     ( iSegPt )        !
               Y1    = Obs     ( iD )            !
               Z1    = Z       ( iD )            !
               Y2    = Obs     ( iU )            !
               Z2    = Z       ( iU )            !
               if      ( Zk .eq. Z1 .or.         ! ... if at valid obser-
     .                   iD .eq. iU       ) then !   vation level, then use
                  Yk = Obs     ( iD )            !   the ob value
               else                              ! ... else, interpolate
                  Yk = Y1 + ( Y2 - Y1 )
     .                    * ( Z1 - Zk )
     .                    / ( Z1 - Z2 )
               end if
            end if
            Y ( iLev_L ) = Yk
            Zk_Last      = Zk
         end do

         iSegBeg      = iSegEnd + 1
         Last_iDown   = iDown ( SegSz )
         Last_iUp     = iUp   ( SegSz )
         iLev_Ob_Last = Last_iDown
      end do

      return
      end subroutine Z2Y_mask
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Scan_ks () --- Scan the sounding index
! 
! !DESCRIPTION:
!     This routine scans the sounding index attribute for sounding and
!     assigns the appropriate array index for each observations.  The
!     observations are assumed to be sorted according to increasing 
!     sounding index
!
! !INTERFACE:
      subroutine Scan_ks ( ks, ks2, nks, iBeg, Len )
!
! !INPUT PARAMETERS:
      implicit   NONE
      integer, intent (in),  dimension (:) ::
     .   ks    ! Sounding index
!
! !OUTPUT PARAMETERS:
      integer, intent (out), dimension (:) ::
     .   ks2   ! Array index for each sounding
      integer, intent (out) ::
     .   nks   ! Number of soundings
      integer, intent (out), dimension (:), optional ::
     .   iBeg, ! Location and
     .   Len   ! ... length of each sounding
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     11Jul2001  C. Redder  Original code
!     18Apr2002  C. Redder  Modified the way the number of observations are
!                           determined.
! EOP
!-------------------------------------------------------------------------

      integer :: NObs, iOb, ksNObs
      logical :: GetBeg, GetLen

      NObs = min ( size ( ks ), size ( ks2 ))
      nks  = 0
      if ( NObs .le. 0 ) return

      GetBeg = present ( iBeg )
      GetLen = present (  Len )
      nks       = 1
      ks2 ( 1 ) = 1
      ksNObs    = 1
      if ( GetBeg ) iBeg  ( 1 ) = 1
      do iOb = 2, NObs
         if ( ks ( iOb ) .ne. ks ( iOb - 1 ) ) then
            if ( GetLen )  Len ( nks ) = ksNObs
            nks    = nks + 1
            if ( GetBeg ) iBeg ( nks ) = iOb
            ksNObs = 0
         end if
         ks2 ( iOb ) = nks
         ksNObs      = ksNObs + 1

      end do
      if ( GetLen ) Len ( nks ) = ksNObs

      return
      end subroutine Scan_ks
!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CalcZ_anchor -- Computes geopotential heights with anchor
!
! !DESCRIPTION:
!     This routine uses the virtual temperature to compute the geopotential
!     heights based on the assumptions that the virtual temperature between
!     two levels changes linearly with log pressure.  This routine assumes
!     that the arrays are sorted in ascending order of pressure.  The height
!     is adjusted by the given achor.
!
! !INTERFACE:
!
      subroutine CalcZ_anchor ( P0, TV0, Z0, P, TV, Z, logP )
      implicit   NONE
!
! !INPUT PARAMETERS: 
      real,             intent (in)  ::
     .   P0, TV0, Z0, ! Height, Z0, (m) at pressure, P0 (hPa or mb) and
     .                !   virtual temperature TV0 (degK)
     .   P  (:),      ! Pressure (hPa or mb)
     .   TV (:)       ! Temperature corrections (deg C or K)
      logical,          intent (in), optional ::
     .   logP         ! = .true. if input levels are given in log
!                     !   pressure.  Default: logP = .false.
! !OUTPUT PARAMETERS:
      real,           intent (out) ::
     .   Z  (:)     ! Geopotential heights (m)
!
!     Note: Number of values is determined by the minimum size of all
!           array arguments.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     19Nov2003  C. Redder  Original code.
!
! EOP
!-------------------------------------------------------------------------

      real, parameter ::
     .   ep = Ra / Rv
      real :: P1, P2, logP0, logP1, ZDelta
      integer :: iOb, NObs, iOb0
      logical :: log_P

      call CalcZ_noanchor ( P, TV, Z, logP )

      NObs  = min ( size ( P ), size ( TV ), size ( Z ))
      if ( NObs .le. 0 ) return

      log_P = .false.
      if ( present ( logP )) log_P  = logP

!     Find closest lower level for the anchor
!     ---------------------------------------
      iOb0  = 0
      if ( P0 .le. P ( 1 ) .and.
     .     P0 .gt. P ( NObs )) then

         P1      = P  ( 1 )
         do iOb  = 2, NObs
            P2   = P  ( iOb )
            iOb0 = iOb - 1
            if ( P0 .le. P1 .and. P0 .gt. P2 ) exit
            P1   = P2
         end do
      else if ( P0 .le. P ( NObs )) then
         iOb0    = NObs
      end if

!     Calculate the adustment by the anchor
!     -------------------------------------
      if      ( iOb0 .ge. 1 ) then
         if ( log_P ) then
            logP0 =       P0
            logP1 =       P ( iOb0 )
         else
            logP0 = log ( P0 )
            logP1 = log ( P ( iOb0 ))
         end if
         ZDelta = Z0 - Ra * ( TV0    + TV ( iOb0 ))
     .                    * ( logP1  - logP0 )
     .                    / ( 2.0 * g ) - Z ( iOb0 )
      else
         if ( log_P ) then
            logP0 =       P0
            logP1 =       P ( 1 )
         else
            logP0 = log ( P0 )
            logP1 = log ( P ( 1 ))
         end if
         ZDelta = Z0 + Ra * ( TV0    + TV ( 1 )   )
     .                    * ( logP0  - logP1      )
     .                    / ( 2.0 * g )
      endif

!     Add ajustment to each height value
!     ----------------------------------
      do iOb = 1, NObs
         Z ( iOb ) = Z ( iOb ) + ZDelta
      end do

      return
      end subroutine CalcZ_anchor
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CalcZ_noanchor -- Computes geopotential heights with no anchor
!
! !DESCRIPTION:
!     This routine uses the virtual temperature to compute the geopotential
!     heights based on the assumptions that the virtual temperature between
!     two levels changes linearly with log pressure.  This routine assumes
!     that the arrays are sorted in ascending order of pressure.  The height
!     at the lowest level is assumed to be 0.
!
! !INTERFACE:
!
      subroutine CalcZ_noanchor ( P, TV, Z, logP )
      implicit   NONE
!
! !INPUT PARAMETERS: 
      real,           intent (in)  ::
     .   P  (:),    ! Pressure (hPa or mb)
     .   TV (:)     ! Temperature corrections (deg C or K)
      logical,        intent (in), optional ::
     .   logP       ! = .true. if input levels are given in log
!                   !   pressure.  Default: logP = .false.
! !OUTPUT PARAMETERS:
      real,           intent (out) ::
     .   Z  (:)     ! Geopotential heights (m)
!
!     Note: Number of values is determined by the minimum size of all
!           array arguments.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     19Nov2003  C. Redder  Original code.
!
! EOP
!-------------------------------------------------------------------------

      real (HP), parameter ::
     .   ep = Ra / Rv
      real (HP) ::        ! As required to accumulate the sum, the
     .   ZSum, P1, P2,    !    precision is set to 12 digits.
     .   Tv1, Tv2, logP1, logP2
      integer :: iOb, NObs, iOb0
      logical :: log_P

      NObs     = min ( size ( P ), size ( TV ), size ( Z ))
      if ( NObs .le. 0 ) return

      log_P    = .false.
      if ( present ( logP )) log_P = logP

      ZSum     = 0.0_HP                   ! Accumulated height

      P1       = P   (  1 )
      Tv1      = TV  (  1 )
      if ( log_P ) then                   ! Assume input pressure 
         logP1 =       P1                 ! ... is in log P
      else
         logP1 = log ( P1 )               ! ... not in log P
      end if

      iOb0     = 0
      Z ( 1 )  = 0.0
      do iOb   = 2, NObs
         P2    = P  ( iOb )
         Tv2   = TV ( iOb )

         if ( log_P ) then
            logP2 =       P2
         else
            logP2 = log ( P2 )
         end if
         ZSum  = ZSum + Ra * ( Tv1    + Tv2   )  ! Update sum
     .                     * ( logP1  - logP2 )
     .                     / ( 2.0_HP * g )
         P1    = P2                              ! Current level becomes
         logP1 = logP2                           !   the next lower level
         Tv1   = Tv2                             !   during the next do-
                                                 !   loop iteration
         Z ( iOb ) = ZSum                        ! Save height
      end do

      return
      end subroutine CalcZ_noanchor
!....................................................................
      end module m_soundings
!====================================================================
