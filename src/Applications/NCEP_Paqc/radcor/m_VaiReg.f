!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOI
! !MODULE: m_VaiReg -- Implements the Vaisala regression correction model for radiation bias.
!
! !DESCRIPTION:
!     This module contains some routines for implementing the correction
!     for correcting radiation biases for m_VaiReg rawinsondes.
!
! !INTERFACE:
!
      module      m_VaiReg
      implicit    NONE
      private	! except

!      public :: RS80Lag, RS80Cld
      public :: VaiReg    ! Vaisala RS80 regression model
      public :: RS80Ref   ! Table lookup based on altitude and solar elevation
                          !    angle at reference conditions for Vaisala RS80
!
! !REVISION HISTORY:
!     11Jul2002  C. Redder  Original code
!
!EOI
!-----------------------------------------------------------------

      contains
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VaiReg () --- Regression model for radiation bias of Vaisala RS80 sondes
!
! !DESCRIPTION:
!     This program corrects RS80 temperature data that has not had any
!     corrections applied.  The data is corrected using a regression
!     algorithm.  The regression algorithm estimeate the  VAICOR (RS80)
!     heat balance model predicted temperature error.  The regression
!     algorithm is based on a large number of VAICOR runs.  The
!     regression program was validated by comparison with the VAICOR
!     predicted error for a series of 4 day and 4 night flights.
!     agreement was within 0.2 K at all altitudes.
!
! !REFERENCES:
!
! !INTERFACE:
      pure subroutine VaiReg ( P, Alt, T, dTdt, SE,  RRate,
     .                         CldAmt, CldAlt, TCorr )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,         intent (in) ::
     .   P     (:), ! Pressure (mb or hPa)
     .   Alt   (:), ! Altitude (or height, in m)
     .   T     (:), ! Upper air temperatures (in deg K)
     .   dTdt  (:), ! Temperature change wrt time (deg C/s)
     .   SE    (:), ! Solar elevation angle (in degrees).
     .   RRate (:), ! Rise rate (in m/s)
     .   CldAmt,    ! Cloud (water and ice) cover (in %)
     .   CldAlt     ! Altitude of cloud top (in m) 
!
! !OUTPUT PARAMETERS:
      real,         intent (out) ::
     .   TCorr (:)  ! Temperature correction (in deg K or C = T_old - T_new)
!
!     Note: Number of levels is determined by the minimum size among all
!           array arguments.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     11Jul2002  C. Redder   Adapted from the code from NCDC
!     19Aug2002  C. Redder   Added required arguments P and dTdt and switched
!                            the order of the arguments, Alt and T.
! EOP
!-------------------------------------------------------------------------

      real    :: Alti, CAmt
      integer :: NObs, iOb

      NObs = min ( size ( P     ),      ! Number of observations
     .             size ( Alt   ),
     .             size ( T     ),
     .             size ( SE    ),
     .             size ( RRate ),
     .             size ( TCorr ))

!     Initialize and ...
!     ------------------
      TCorr ( : NObs ) = 0.0

!     ... add correction terms due to reference conditions and rise rate
!     ------------------------------------------------------------------
      call RS80Ref ( Alt ( : NObs ), SE,     RRate, TCorr )

!     ... and due to clouds
!     ---------------------
      call RS80Cld ( Alt ( : NObs ), SE, (/ CldAmt /), (/ CldAlt /),
     .                                              TCorr )

!     Reduce or eliminate corrections (except
!     due to lag) at altitudes below cloud top
!     ----------------------------------------
      do iOb = 1, NObs
         Alti = Alt ( iOb )
         CAmt = min ( 100.0, max ( 0.0, CldAmt ))
         if ( Alti .lt. CldAlt ) TCorr ( iOb ) = 0.0
         TCorr ( iOb ) = TCorr ( iOb ) * ( 1.0 - CAmt / 100.0 )
      end do

!     Add corrections due to lag
!     --------------------------
      call RS80Lag ( P ( : NObs ), T, dTdt, RRate, TCorr )

      return
      end subroutine VaiReg

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: RS80Ref () --- Adds the Vaisala RS80 reference temperature correction
!
! !DESCRIPTION:
!     This subroutine adds, to a given temperature correction, the term for
!     the referece temperature corrections modified by a factor accounting
!     for the rise rate of thermister.   The reference corrections are
!     determined via table lookup and linear interpolation for the Vaisala
!     RS80 thermister rod thermistor as a function of solar elevation and
!     altitude.  The table entries were generated by the VAICOR model
!     (Luers and Eskridge, 1995) assuming a set of reference conditions.
!     The set includes 1966 Supplemental Atmospheric Mid-Latitude Spring
!     temperature and pressure profiles, 5.0 m/s rise rate, vegetation
!     ground cover, 10% relative humidity, 280~deg~K surface temperature,
!     no clouds and the LOWTRAN7 rural (5~km visibility) tropospheric
!     aerosol profile (LOWTRAN #2 aerosol profile). LOWTRAN is a radiative
!     transfer model (Kneizys et al., 1988).  The lag correction is not
!     included in this table.  Nightime conditions are assumed for solar
!     elevation angles at or less than -7 deg.
!
! !REFERENCES:
!     Kneizys, F., E. Shettle, L. Abreu, J. Chetwynd, G. Anderson, W.
!          Gallery, J. Selby, and S. Clough, 1988: User's guide to
!          LOWTRAN-7.  AFGL-TR-88-0177, Air Force Geophysics Laboratory,
!          Hanscom Air Force Base, MA, 137~pp.
!     Luers, J. K. and R. E. Eskridge, 1995: Temperature corrections for 
!          the VIZ and Vaisala radiosondes.  J. Apl Meteo., 34, 1241-1253
!
! !INTERFACE:
      pure subroutine RS80Ref ( Alt, SE, RRate, TCorr )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,     intent (in),    dimension (:) ::
     .   Alt,   ! Altitude (in m)
     .   SE,    ! Solar elevation angle (in deg).
     .   RRate  ! Rise rate (in m/s).  If factor accounting for rise rate
                !   is not desired, then set size (RRate) = 0.
!
! !INPUT/OUTPUT PARAMETERS:
      real,     intent (inout), dimension (:) ::
     .   TCorr  ! On input,  the given temperature correction (in deg K or C). 
                ! On output, the input value plus the reference temperature
                !   correction modified by the rise factor.
!
!     Note: Number of levels is determined by the minimum size among all
!           array arguments (except RRate).
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     11Jul2002  C. Redder   Original code, adapted from the subroutine
!                            VIZTab in the module m_VIZReg.
!     15Aug2002  C. Redder   Added required input argument, RRate.  Modified
!                            argument TCorr to be an input as well as an
!                            output argument.
! EOP
!-------------------------------------------------------------------------

      real    ::  DT1,   DT2,  RAlt,   RSE,   Alti, SEi, FTRR, RRi,
     .            HSE1,  HSE2, HAlt1,  HAlt2, DTRef
      integer :: iHSE, iiHSE, iHAlt, iiHAlt,  NObs, iOb, NRR

      integer, parameter ::
     .   NAlt =  8, ! Number of columns for altitudes
     .   NSE  =  8  ! ... and rows for solar elevation angles
     .              !     in the reference tables
      real,    parameter ::
     .   HAlt ( NAlt ) = (/ 0., 5., 10., 15., 20., 25., 30., 35. /),
     .   HSE  ( NSE  ) = (/
     .    -7.0,  -5.0,  -3.0,   1.0,   5.0,  15.0,  45.0,  80.0/),! Alt (km)
     .   !--------------------------------------------------------!---------
     .   DegC ( NSE, NAlt ) = Reshape ( (/                        !
     .     0.00,  0.00,  0.00,  0.01,  0.02,  0.04,  0.10,  0.17, !  0.0
     .     0.00,  0.00,  0.00,  0.11,  0.22,  0.33,  0.35,  0.32, !  5.0
     .     0.00,  0.00,  0.00,  0.35,  0.48,  0.57,  0.55,  0.49, ! 10.0
     .     0.00,  0.00,  0.06,  0.61,  0.75,  0.84,  0.76,  0.68, ! 15.0
     .     0.00,  0.00,  0.48,  1.02,  1.20,  1.28,  1.15,  1.00, ! 20.0
     .     0.00,  0.00,  0.94,  1.61,  1.76,  1.84,  1.61,  1.41, ! 25.0
     .     0.00,  0.11,  1.48,  2.26,  2.40,  2.46,  2.15,  1.87, ! 30.0
     .     0.00,  0.13,  1.77,  2.71,  2.88,  2.95,  2.58,  2.24  ! 35.0
     .    /), (/ NSE, NAlt /) ) ! ------------------------------------------
     .   !-7.0,  -5.0,  -3.0,   1.0,   5.0,  15.0,  45.0,  80.0   ! Sun elev
     .                                                            ! (deg)

      HAlt1 = HAlt ( NAlt )
      HAlt2 = HAlt (    1 )
      HSE1  = HSE  (  NSE )
      HSE2  = HSE  (    1 )

!     Observation loop
!     ----------------
      NRR  =       size ( RRate )
      NObs = min ( size ( Alt ), size ( SE ), size ( TCorr ))
      do iOb = 1, NObs

!        Determine the table entry number for the altitude
!        -------------------------------------------------
         Alti = Alt ( iOb ) / 1000.0
         if ( Alti .lt. HAlt1 .or. Alti .gt. HAlt2 ) then
            do iHAlt = 1, NAlt - 1
               iiHAlt = iHAlt
               if ( Alti .lt. HAlt ( iHAlt + 1 )) exit
            end do
         end if

!        ... and calculate the interpolation factors
!        -------------------------------------------
         HAlt1 = HAlt ( iiHAlt     )
         HAlt2 = HAlt ( iiHAlt + 1 )
         RAlt  = ( HAlt1 - Alti ) / ( HAlt1 - HAlt2 )

!        Do the same for the solar angle
!        -------------------------------
         SEi  = max ( min ( SE ( iOb ), HSE ( NSE )), HSE ( 1 ))
         if ( SEi .lt. HSE1 .or. SEi .gt. HSE2 ) then
            do iHSE = 1, NSE - 1
               iiHSE = iHSE
               if ( SEi .lt. HSE ( iHSE + 1 )) exit
            end do
         end if
         HSE1  =   HSE ( iiHSE     )
         HSE2  =   HSE ( iiHSE + 1 )
         RSE   = ( HSE1 - SEi ) / ( HSE1 - HSE2 )

!        Perform 2-dimensional interpolation
!        -----------------------------------
         DT1   = RSE * ( DegC ( iiHSE + 1, iiHAlt     )
     .                 - DegC ( iiHSE,     iiHAlt     ))
     .                 + DegC ( iiHSE,     iiHAlt     )
         DT2   = RSE * ( DegC ( iiHSE + 1, iiHAlt + 1 )
     .                 - DegC ( iiHSE,     iiHAlt + 1 ))
     .                 + DegC ( iiHSE,     iiHAlt + 1 )
         DTRef = RAlt * ( DT2 - DT1 ) + DT1

!        Account for RRate
!        -----------------
         RRi   = 5.0
         if ( NRR .ne. 0 ) RRi = RRate ( min ( iOb, NRR ))
         FTRR  = sqrt ( 5.0 / max ( RRi, 0.0001 ))

!        Add to input value
!        ------------------
         TCorr ( iOb ) = DTRef * FTRR + TCorr ( iOb )
      end do

      return
      end subroutine RS80Ref

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: RS80Lag () --- Adds the lag correction for Vaisala RS80 sondes
!
! !DESCRIPTION:
!     This subroutine adds, to a given temperature correction, the term for
!     the RS80 lag correction by calculating the temperature gradient over
!     consecutive points.   The lag is calculated using Antikainen's
!     equation.  The lag correction is added only if there are at least 
!     two observation and the first and last obervations were taken at
!     60 seconds apart.
!
! !INTERFACE:
      pure subroutine RS80Lag ( P, T, dTdt, RRate, TCorr )
      use m_stdatm, only : StdZ2P, StdZ2T, StdZ2R
!
! !INPUT PARAMETERS:
      implicit NONE
      real,         intent (in)    ::
     .   P     (:), ! Pressure (hPa or mb)
     .   T     (:), ! Temperature (in deg K )
     .   dTdt  (:), ! Temperature change wrt time (deg C/s)
     .   RRate (:)  ! Rise rate (in m/s)
!
! !OUTPUT PARAMETERS:
      real,         intent (inout) ::
     .   TCorr (:)  ! On input,  the given temperature correction (in deg K)
                    ! On output, the input value plus the correction due to
!                   !   lag.
! !SEE ALSO:
!
! !REVISION HISTORY:
!     11Jul2002  C. Redder   Original code, adapted from a NCDC subroutine.
!     19Aug2002  C. Redder   Replaced the required input argument, rho, with
!                            P and made it the first argument.  Removed the
!                            required argument, Alt, and added the required
!                            argument dTdt.  Modified argument TCorr to be
!                            an input as well as an output argument.
! EOP
!-------------------------------------------------------------------------
      integer :: NObs,  iOb
      real    :: lamda, rise, rho, Ra
      Ra = StdZ2P ( 0.0 ) / (  StdZ2T ( 0.0 ) *  StdZ2R ( 0.0 ))

      NObs = min ( size ( P     ),  ! Number of observations
     .             size ( T     ),
     .             size ( dTdt  ),
     .             size ( RRate ),
     .             size ( TCorr ))

!     For each observation ...
!     ------------------------
      do iOb = 1, NObs
         rise    =  RRate ( iOb )

!        Calculate the lag correction using Antikainen's equation
!        with a modified coefficient of 3.00 rather than 2.28.
!        --------------------------------------------------------
         rho   = P ( iOb ) / ( Ra * T ( iOb )) 
!         lamba = 2.28 * ( rho * rise ) ** ( -0.41 )
!                                      ! Different units in Antikainen
         lamda = 3.00 * ( 0.1 * rho * rise ) ** ( -0.41 )
         TCorr ( iOb ) = -1.0 * lamda * dTdt ( iOb )

      end do

      return
      end subroutine RS80Lag

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: RS80Cld () --- Adds the temperature correction due to cloud cover
!
! !DESCRIPTION:
!     This subroutine adds, to a given temperature correction, the term due
!     to cloud cover for day flights.  There is no nighttime correction for
!     clouds at solar angles less than zero.
!
! !INTERFACE:
      pure subroutine RS80Cld ( Alt, SE, CAmt, CAlt, TCorr )
!
! !INPUT PARAMETERS:
      implicit NONE
      real,      intent (in),    dimension (:) ::
     .   Alt,  ! Altitude (in m)
     .   SE,   ! Solar elevation angle (in deg).
     .   CAmt, ! Cloud amount (in %)
     .   CAlt  ! Cloud altitude (in m)
!
!     Note: If the size of CldAmt is less than Alt, then the last value in
!           CldAmt is used for all remaining values in Alt.  Same is true
!           for CldAlt.
!
! !OUTPUT PARAMETERS:
      real,      intent (inout), dimension (:) ::
     .   TCorr ! On input,  the given temperature correction (in deg K or C). 
               ! On output, the input value plus the temperature correction
!              !   due to clouds.
! !SEE ALSO:
!
! !REVISION HISTORY:
!     11Jul2002  C. Redder   Original code, adapted from a NCDC subroutine.
!     15Aug2002  C. Redder   Added required input arguments, SE and CAmt.
!                            Modified argument TCorr to be an input as well
!                            as an output argument.
!
! EOP
!-------------------------------------------------------------------------

      real    ::  HCAlt1, HCAlt2, HAlt1, HAlt2,  DTCld, Deg2Rad, RCAmt,
     .            DT1,    DT2,    RAlt,  RCAlt,  Alti,  CAlti,   SEi
      integer :: iHCAlt, iiHCAlt, iHAlt, iiHAlt,
     .            NObs,  NICAmt,  NICAlt, iOb

      integer, parameter ::
     .    NAlt =  8, ! Number of columns for altitudes
     .   NCAlt =  5  ! ... and rows for cloud altitude
     .               !     in the cloud correction table
      real,    parameter ::
     .    HAlt (  NAlt ) = (/ 0., 5., 10., 15., 20., 25., 30., 35. /),
     .   HCAlt ( NCAlt ) = (/
     .     0.0,   2.0,   4.0,   5.7,  10.0 /), ! Alt (km)
     .   !-------------------------------------!---------
     .   DegC ( NCAlt, NAlt ) = Reshape ( (/   !
     .     0.00,  0.00,  0.00,  0.00,  0.00,   !  0.0
     .     0.00,  0.04,  0.13,  0.00,  0.00,   !  5.0
     .     0.00,  0.07,  0.14,  0.25,  0.31,   ! 10.0
     .     0.00,  0.10,  0.20,  0.35,  0.42,   ! 15.0
     .     0.00,  0.16,  0.30,  0.52,  0.62,   ! 20.0
     .     0.00,  0.21,  0.41,  0.71,  0.85,   ! 25.0
     .     0.00,  0.27,  0.53,  0.93,  1.12,   ! 30.0
     .     0.00,  0.33,  0.63,  1.12,  1.34    ! 35.0
     .    /), (/ NCAlt, NAlt /) ) ! ---------------------
     .   ! 0.0,   2.0,   4.0,   5.7,  10.0     ! Cloud alt
     .                                         ! (km)

      Deg2Rad  = atan2 ( 0.0, -1.0 ) / 180.0  ! Factor for converting
                                              !   degrees to radians
      HAlt1  = HAlt  (  NAlt )
      HAlt2  = HAlt  (     1 )
      HCAlt1 = HCAlt ( NCAlt )
      HCAlt2 = HCAlt (     1 )

!     Observation loop
!     ----------------
      NICAmt =       size ( CAmt )
      NICAlt =       size ( CAlt )
      NObs   = min ( size (  Alt ), size ( SE ), size ( TCorr ))
      if ( NICAmt .eq. 0 .or. 
     .     NICAlt .le. 0 ) NObs = 0
      do iOb = 1, NObs

!        Determine the table entry number for the altitude
!        -------------------------------------------------
         Alti  = Alt ( iOb ) / 1000.0
         if (  Alti .lt.  HAlt1 .or.
     .         Alti .gt.  HAlt2 ) then
            do iHAlt = 1, NAlt - 1
               iiHAlt = iHAlt
               if ( Alti .lt. HAlt ( iHAlt + 1 )) exit
            end do
         end if

!        ... and calculate the interpolation factors
!        -------------------------------------------
         HAlt1 = HAlt ( iiHAlt     )
         HAlt2 = HAlt ( iiHAlt + 1 )
         RAlt  = ( HAlt1 - Alti ) / ( HAlt1 - HAlt2 )

!        Do the same for the cloud altitude
!        ----------------------------------
         if ( iOb .le. NICAlt ) then
            CAlti  = max ( min ( CAlt ( iOb   ) / 1000.0,
     .                          HCAlt ( NCAlt )),
     .                          HCAlt ( 1     ))
            if ( CAlti .lt. HCAlt1 .or.
     .           CAlti .gt. HCAlt2 ) then
               do iHCAlt = 1, NCAlt - 1
                  iiHCAlt = iHCAlt
                  if ( CAlti .lt. HCAlt ( iHCAlt + 1 )) exit
               end do
            end if
            HCAlt1 =   HCAlt  ( iiHCAlt     )
            HCAlt2 =   HCAlt  ( iiHCAlt + 1 )
            RCAlt  = ( HCAlt1 - CAlti ) / ( HCAlt1 - HCAlt2 )
         end if

!        Perform 2-dimensional interpolation
!        -----------------------------------
         DT1   = RCAlt * ( DegC ( iiHCAlt + 1, iiHAlt     )
     .                   - DegC ( iiHCAlt,     iiHAlt     ))
     .                   + DegC ( iiHCAlt,     iiHAlt     )
         DT2   = RCAlt * ( DegC ( iiHCAlt + 1, iiHAlt + 1 )
     .                   - DegC ( iiHCAlt,     iiHAlt + 1 ))
     .                   + DegC ( iiHCAlt,     iiHAlt + 1 )
         DTCld = RAlt * ( DT2 - DT1 ) + DT1

!        No correction if altitude is below cloud top
!        --------------------------------------------
         if ( Alti .lt. CAlti ) DTCld = 0.0

!        Account for solar angle and cloud coverage
!        ------------------------------------------
         SEi   = min (  90.0, max ( 0.0, SE ( iOb )))
         RCAmt = min ( 100.0, max ( 0.0, CAmt ( min ( iOb, NICAmt ))))
         DTCld = DTCld * cos ( Deg2Rad * ( 90.0 - SEi ))
     .                 * RCAmt / 100.0

!        Add to given correction
!        -----------------------
         TCorr ( iOb ) = DTCld + TCorr ( iOb ) 

      end do

      return
      end subroutine RS80Cld
!.................................................................
      end module m_VaiReg
!====================================================================
