!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOI
! !MODULE: m_StdAtm -- Return parameters for the 1976 standard atmosphere.
!
! !DESCRIPTION:
!     This module contains routines that return the parameters for the
!     1976 standard atmosphere at given geopotential height(s) or pressure
!     level(s).  The list of returned parameters and their default units
!     is as follows:
! \begin{verbatim}
!                                units
!     Z  - geoopotential height     m
!     P  - pressure                 HPa (or mb)
!     T  - absolute temperature     deg K
!     R  - density of dry air (rho) Kg/m^3
! \end{verbatim}
!     The output parameter from these routines are valid only for heights
!     upto the mesopause at 84.852 km (282,152 ft) or pressures as low as
!     0.00373 HPa.  If the input values are outside this range, then the
!     output parameters will be returned but should be considered
!     inaccurate.  Altitude instead of geopotential height can be used by
!     setting the optional parameter \verb|alt| to \verb|.true.|
!
! !INTERFACE:
!
      module      m_StdAtm
      implicit    NONE
      private	! except

      public ::
     .   StdP2Z,    ! Returns the standard geopotential height
     .   StdP2T,    ! ... temperature
     .   StdP2R,    ! ... or pressure for a single pressure level.
     .   StdAtmP,   ! Returns height, temperature and/or density for
     .              !   given pressure level(s)
     .   StdZ2P,    ! Returns the standard height
     .   StdZ2T,    ! ... temperature
     .   StdZ2R,    ! ... or pressure for a single height value.
     .   StdAtmZ    ! Returns pressure, temperature and/or density for
                    !   given geopotential height(s)
      public ::
     .   ZMax       ! Max height for the standard atmosphere (in m)
!
! !REVISION HISTORY:
!     25Jun2001  C. Redder  Revised for use at the DAO
!     28Sep2001  C. Redder  Minor revisions to module prologue and added the
!                           functions StdP2T, StdZ2T, StdZ2R, and StdP2R
!     07Oct2003  C. Redder  Added the public entity, ZMax
!
!EOI
!-----------------------------------------------------------------

!     Parameters defining the layers of the standard atmosphere where
!     HTab, TTab, and PTab are the geopotential height (in m),
!     temperature (in deg K) at the base of the layer and normalized
!     pressure at the bottom of each layer, and LTab, is the lapse rate
!     (= -dT/dZ in deg K/km) for the each layer. 
!     -----------------------------------------------------------------
      integer, parameter :: NLayers = 8
      real,    parameter, dimension ( NLayers ) ::
     .  ZTab = (/  0.0,         11.0,         20.0,         32.0,
     .            47.0,         51.0,         71.0,         84.852   /),
     .  TTab = (/288.15,       216.65,       216.65,       228.65,
     .           270.65,       270.65,       214.65,       186.946   /),
     .  LTab = (/ -6.5,          0.0,          1.0,          2.8,
     .             0.0,         -2.8,         -2.0,          0.0     /),
     .  PTab = (/1.0,          2.233611E-1,  5.403295E-2,  8.5666784E-3,
     .           1.0945601E-3, 6.6063531E-4, 3.9046834E-5, 3.68501E-6/)

!     
      real,    parameter ::
     .  ZMin = -2000.0,
     .  ZMax =  84852.0

!     Geophysical parameters
!     ----------------------
      real, parameter ::
     .  tpz_km =    11.0,  ! Height at the tropopause in km
     .  slt_K  =  TTab (1),! Temperature at sea level in deg K
     .  tpzDef = 11000.0,  ! Default height at the tropopause
     .  slpDef =  1013.25, ! Default sea level pressure (HPa or mb)
     .  sltDef =  TTab (1),! ... temperature        (deg K)
     .  slrDef =     1.225,! ... density of dry air (Kg / m^3)
     .  REarth =  6369.0,  ! Radius of the Earth    (km)
     .  GMR    = 34.163195 ! = 1000 * g0 / R where R is the gas constant 
                           !   for dry air (=287.05307 J/(kg-deg K)) and
      contains             !   g0 is the global average of gravity at
                           !   mean sea level (=9.80665 m/s^2)

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: StdAtmZ - Computes 1976 standards atmospheric parameters at given geopotential height(s)
!
! !DESCRIPTION: 
!     This routine computes and returns parameters for the 1976 standard
!     atmosphere at given geopotential height(s).
!
! !INTERFACE:
      pure subroutine StdAtmZ ( Z, P, T, R, alt )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,      intent (in),  dimension (:) ::
     .   Z     ! Height (in m)
      logical,   intent (in),  optional      ::
     .   alt   ! = .true. if Z is altitude.  Default: alt = .false.
!
! !OUTPUT PARAMETERS: 
      real,      intent (out), dimension (:), optional ::
     .   P,    ! Pressure (in hPa or mb)
     .   T,    ! Absolute temperature (in deg K)
     .   R     ! Density of dry air (in Kg/m^3)
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     25Jun2001  C. Redder  Original code.
!     28Sep2001  C. Redder  Added units to parameter description in proglogue
!     10Oct2001  C. Redder  Made function pure
!     18Apr2002  C. Redder  Modified the method of determining the number of
!                           levels.
! EOP
!-------------------------------------------------------------------------

      logical :: geo, ReturnP, ReturnT, ReturnR
      real    :: Zkm, Zt, Ps, Ts, Rs, PP, TT, RR, TB, LR
      real    :: DeltaZ, ZConvF
      integer :: NLev, iLev, i, j, k, iLayer

!     Define ...
!     ----------
      Zt = tpzDef                     ! ... height at tropopause
      Ps = slpDef                     ! ... sea level pressure
      Ts = sltDef                     ! ... absolute temp at sea level
      Rs = slrDef                     ! ... sea level density

      geo     = .true.
      if ( present ( alt ) ) geo = .not. alt
      ReturnP = .false.
      if ( present ( P ) ) ReturnP = .true.
      ReturnT = .false.
      if ( present ( T ) ) ReturnT = .true.
      ReturnR = .false.
      if ( present ( R ) ) ReturnR = .true.

!     For each level ...
!     ------------------
      ZConvF = tpz_km / Zt
      NLev   = size ( Z )
      if ( ReturnP ) NLev = min ( NLev, size ( P ))
      if ( ReturnT ) NLev = min ( NLev, size ( T ))
      if ( ReturnR ) NLev = min ( NLev, size ( R ))
      do iLev = 1, NLev

!        Convert height to local (i.e. default) units
!        --------------------------------------------
         Zkm = ZConvF * Z ( iLev )

!        ... and, if necessary, to geopotential height
!        ---------------------------------------------
         if ( .not. geo ) Zkm = Zkm / ( 1.0 + Zkm / REarth )

!        Search for the correct layer
!        ----------------------------
         if      ( Zkm .lt. ZTab ( 2 ) ) then
            i = 1
         else if ( Zkm .lt. ZTab ( 3 ) ) then 
            i = 2
         else if ( Zkm .lt. ZTab ( 4 ) ) then
            i = 3
         else if ( Zkm .lt. ZTab ( 5 ) ) then
            i = 4
         else if ( Zkm .gt. ZTab ( NLayers ) ) then
            i = NLayers
         else                    ! perform binary search for upper layers
            i = 5
            j = NLayers
            do iLayer = 1, NLayers
               k = ( i + j ) / 2 ! start from midpoint
               if ( Zkm .lt. ZTab ( k ) ) then
                  j = k
               else
                  i = k
               end if
               if ( j .le. i+1) exit
            end do
         end if

!        After the correct layer is found determine ...
!        ---------------------------------------------
         LR     = LTab ( i )                       ! ... lapse rate
         TB     = TTab ( i )                       ! ... base temp
         DeltaZ = Zkm - ZTab ( i )                 ! ... height above base
         TT     = TB + LR * DeltaZ                 ! ... local temp

!        ... pressure ratio within a layer of constant ...
!        -------------------------------------------------
         if ( nint ( LR * 1000.0 ) .eq. 0 ) then   ! ... temperature
            PP = PTab ( i ) * exp ( -GMR  * DeltaZ / TB )
         else                                      ! ... lapse rate
            PP = PTab ( i ) * ( TB / TT ) ** ( GMR / LR )
         end if

!        Return ...
!        ----------
         if ( ReturnP ) P ( iLev ) =   Ps * PP          ! ... pressure
         if ( ReturnT ) T ( iLev ) =   TT * Ts / slt_K  ! ... temp
         if ( ReturnR ) R ( iLev ) =   Rs * PP          ! ... density
     .                             / ( TT / slt_K )
      end do

      return
      end subroutine StdAtmZ
*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: StdAtmP - Computes 1976 standard atmospheric parameters at given pressure level(s)
!
! !DESCRIPTION: 
!     This routine computes and returns the parameters for the 1976
!     standard atmosphere at given pressure level(s).
!
! !INTERFACE:
      pure subroutine StdAtmP ( P, Z, T, R, alt )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,      intent (in),  dimension (:) ::
     .   P     ! Pressure (in hPa or mb)
      logical,   intent (in),  optional      ::
     .   alt   ! = .true. if Z is altitude.  Default: alt = .false.
!
! !OUTPUT PARAMETERS: 
      real,      intent (out), dimension (:), optional ::
     .   Z,    ! Height (in m)
     .   T,    ! Absolute temperature (in deg K)
     .   R     ! Density of dry air (in Kg/m^3)
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     25Jun2001  C. Redder  Original code.
!     28Sep2001  C. Redder  Added units to parameter description in proglogue
!     10Oct2001  C. Redder  Made subroutine pure
!     18Apr2002  C. Redder  Modified the method of determining the number of
!                           levels.
!
! EOP
!-------------------------------------------------------------------------

      logical :: geo, ReturnZ, ReturnT, ReturnR
      real    :: Zkm, Zt, Ps, Ts, Rs, PP, TT, RR, TB, LR
      real    :: DeltaZ, ZConvF
      integer :: NLev, iLev, i, j, k, iLayer

!     Define ...
!     ----------
      Zt = tpzDef                     ! ... height at tropopause
      Ps = slpDef                     ! ... sea level pressure
      Ts = sltDef                     ! ... absolute temp at sea level
      Rs = slrDef                     ! ... sea level density
      geo     = .true.
      if ( present ( alt ) ) geo = .not. alt
      ReturnZ = .false.
      if ( present ( Z ) ) ReturnZ = .true.
      ReturnT = .false.
      if ( present ( T ) ) ReturnT = .true.
      ReturnR = .false.
      if ( present ( R ) ) ReturnR = .true.

!     For each level ...
!     ------------------
      ZConvF = Zt / tpz_km
      NLev   = size ( P )
      if ( ReturnZ ) NLev = min ( NLev, size ( Z ))
      if ( ReturnT ) NLev = min ( NLev, size ( T ))
      if ( ReturnR ) NLev = min ( NLev, size ( R ))
      do iLev = 1, NLev

!        Check pressure level ...
!        ------------------------
         if ( Ps .gt. 0 ) then
            PP = max ( P ( iLev ), 0.1 * Ps * PTab ( NLayers ) )
         else
            PP = min ( P ( iLev ), 0.1 * Ps * PTab ( NLayers ) )
         end if

!        ... and convert to pressure ratio
!        ---------------------------------
         PP    = PP / Ps

!        Search for the correct layer
!        ----------------------------
         if      ( PP .gt. PTab ( 2 ) ) then
            i = 1
         else if ( PP .gt. PTab ( 3 ) ) then 
            i = 2
         else if ( PP .gt. PTab ( 4 ) ) then
            i = 3
         else if ( PP .gt. PTab ( 5 ) ) then
            i = 4
         else if ( PP .lt. PTab ( NLayers ) ) then
            i = NLayers
         else                    ! perform binary search for upper layers
            i = 5
            j = NLayers
            do iLayer = 1, NLayers
               k = ( i + j ) / 2 ! start from midpoint
               if ( PP .ge. PTab ( k ) ) then
                  j = k
               else
                  i = k
               end if
               if ( j .le. i+1) exit
            end do
         end if

!        After the correct layer is found determine ...
!        ---------------------------------------------
         LR = LTab ( i )                                ! ... lapse rate
         TB = TTab ( i )                                ! ... base temp

!        ... local temperature and height within the layer of constant ...
!        -----------------------------------------------------------------
         if ( nint ( LR * 1000.0 ) .eq. 0 ) then        ! ... temperature
            TT     =   TB
            DeltaZ =  -TB * log ( PP / PTab ( i ) ) / GMR
         else                                           ! ... lapse rate
            TT     = TTab ( i ) * ( PP / PTab ( i ) ) ** ( -LR / GMR )
            DeltaZ = ( TT - TB ) / LR
         end if
         Zkm = ZTab ( i ) + DeltaZ

!        If desired, convert to altitude
!        -------------------------------
         if ( .not. geo ) Zkm = Zkm / ( 1.0 - Zkm / REarth )

!        Return ...
!        ----------
         if ( ReturnZ ) Z ( iLev ) =   Zkm * ZConvF      ! ... height
         if ( ReturnT ) T ( iLev ) =   TT  * Ts / slt_K  ! ... temp
         if ( ReturnR ) R ( iLev ) =   Rs  * PP          ! ... density
     .                             / ( TT / sltDef )

      end do

      return
      end subroutine StdAtmP
*....................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: StdZ2P - Computes pressure at a single height in the 1976 standard atmosphere
!
! !DESCRIPTION: 
!     This routine computes and returns the pressure for the 1976
!     standard atmosphere at a single height.
!
! !INTERFACE:
      pure function StdZ2P ( Z, alt )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,      intent (in)            ::
     .   Z     ! Height (in m)
      logical,   intent (in),  optional ::
     .   alt   ! = .true. if Z is altitude.  Default: alt = .false.
!
! !OUTPUT PARAMETERS: 
      real :: StdZ2P ! Pressure (in hPa or mb)
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     25Jun2001  Redder  Original code.
!     28Sep2001  Redder  Added units to parameter description in proglogue
!     10Oct2001  Redder  Made function pure
!
! EOP
!-------------------------------------------------------------------------
      real, dimension (1) :: P

      call StdAtmZ ( (/Z/), P, alt = alt )
      StdZ2P = P (1)

      end function StdZ2P
*....................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: StdP2Z - Computes height at a single pressure level in the 1976 standard atmosphere
!
! !DESCRIPTION: 
!     This routine computes and returns the height for the 1976 standard
!     atmosphere at a single pressure level.
!
! !INTERFACE:
      pure function StdP2Z ( P, alt )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,      intent (in)            ::
     .   P     ! Pressure (in hPa or mb)
      logical,   intent (in),  optional ::
     .   alt   ! = .true. if Z is altitude.  Default: alt = .false.
!
! !OUTPUT PARAMETERS: 
      real :: StdP2Z ! Height (in m)
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     25Jun2001  Redder  Original code.
!     28Sep2001  Redder  Added units to parameter description in proglogue
!     10Oct2001  Redder  Made function pure
!
! EOP
!-------------------------------------------------------------------------
      real, dimension (1) :: Z

      call StdAtmP ( (/P/), Z, alt = alt )
      StdP2Z = Z (1)

      end function StdP2Z
*....................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: StdZ2T - Computes temperature at a single height in the 1976 standard atmosphere
!
! !DESCRIPTION: 
!     This routine computes and returns the temperature for the 1976
!     standard atmosphere at a single height.
!
! !INTERFACE:
      pure function StdZ2T ( Z, alt )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,      intent (in)            ::
     .   Z     ! Height (in m)
      logical,   intent (in),  optional ::
     .   alt   ! = .true. if Z is altitude.  Default: alt = .false.
!
! !OUTPUT PARAMETERS: 
      real :: StdZ2T ! Temperature (in deg K)
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     28Sep2001  Redder  Original code.
!     10Oct2001  Redder  Made function pure
!
! EOP
!-------------------------------------------------------------------------
      real, dimension (1) :: T

      call StdAtmZ ( (/Z/), T = T, alt = alt )
      StdZ2T = T (1)

      end function StdZ2T
*....................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: StdP2T - Computes temperature at a single pressure level in the 1976 standard atmosphere
!
! !DESCRIPTION: 
!     This routine computes and returns the temperature for the 1976 standard
!     atmosphere at a single pressure level.
!
! !INTERFACE:
      pure function StdP2T ( P )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,      intent (in)            ::
     .   P     ! Pressure (in hPa or mb)
!
! !OUTPUT PARAMETERS: 
      real :: StdP2T ! Temperature (in deg K)
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     28Sep2001  Redder  Original code.
!     10Oct2001  Redder  Made function pure
!
! EOP
!-------------------------------------------------------------------------
      real, dimension (1) :: T

      call StdAtmP ( (/P/), T = T )
      StdP2T = T (1)

      end function StdP2T
*....................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: StdZ2R - Computes air density at a single height in the 1976 standard atmosphere
!
! !DESCRIPTION: 
!     This routine computes and returns the density of dry air for the
!     1976 standard atmosphere at a single height.
!
! !INTERFACE:
      pure function StdZ2R ( Z, alt )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,      intent (in)            ::
     .   Z     ! Height (in m )
      logical,   intent (in),  optional ::
     .   alt   ! = .true. if Z is altitude.  Default: alt = .false.
!
! !OUTPUT PARAMETERS: 
      real :: StdZ2R ! Density of dry air (in Kg/m^3)
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     28Sep2001  Redder  Original code.
!     10Oct2001  Redder  Made function pure
!
! EOP
!-------------------------------------------------------------------------
      real, dimension (1) :: R

      call StdAtmZ ( (/Z/), R = R, alt = alt )
      StdZ2R = R (1)

      end function StdZ2R
*....................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: StdP2R - Computes air density at a single pressure level in the 1976 standard atmosphere
!
! !DESCRIPTION: 
!     This routine computes and returns the density of dry air for the
!     1976 standard atmosphere at a single pressure level.
!
! !INTERFACE:
      pure function StdP2R ( P )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,      intent (in)            ::
     .   P     ! Pressure (in hPa or mb)
!
! !OUTPUT PARAMETERS: 
      real :: StdP2R ! Density of dry air (in Kg/m^3)
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     28Sep2001  Redder  Original code.
!     10Oct2001  Redder  Made function pure
!
! EOP
!-------------------------------------------------------------------------
      real, dimension (1) :: R

      call StdAtmP ( (/P/), R = R )
      StdP2R = R (1)

      end function StdP2R
*....................................................................
      end module m_StdAtm
!====================================================================

