!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOI
! !MODULE: m_VIZReg -- Implements the VIZ regression correction model for radiation bias.
!
! !DESCRIPTION:
!     This module contains some routines for implementing the correction
!     for correcting radiation biases for VIZ rawinsondes.
!
! !INTERFACE:
!
      module      m_VIZReg
      implicit    NONE
      private	! except

      public ::
     .   VIZReg,     ! VIZ regression model
     .   VIZRef,     ! Table lookup based on altitude and solar elevation
     .               !    angle at reference conditions
     .   VIZ_rkxList ! List of valid WMO instr types for VIZ sondes
c      public :: VIZLag
!
! !REVISION HISTORY:
!     09Oct2001  C. Redder  Original code
!     22Apr2002  C. Redder  Changed the name of RefTable to RefTab
!     11Jul2002  C. Redder  Changed the name of RefTab to VIZTab
!     21Nov2006  C. Redder  Added declaration for VIZ_rkxList
!
!EOI
!-----------------------------------------------------------------

!     List of VIZ sondes by WMO radiosonde instrument type
!     ----------------------------------------------------
      integer, dimension ( 9 ), parameter ::
     .   VIZ_rkxList = (/ 9, 10, 14, 21, 31, 38, 49, 51, 65 /) 

      contains

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VIZReg () --- Regression model for radiation bias of VIZ sondes
!
! !DESCRIPTION:
!
! !REFERENCES:
!
! !INTERFACE:
      pure subroutine VIZReg ( TSurf, P, Alt, T, LRate, SE, RRate,
     .                         CirTh, CirAlt, CldAmt, CldAlt, CldTemp,
     .                         TCorr )
      use m_StdAtm, only : StdAtmZ, StdZ2T
      implicit NONE
!
! !INPUT PARAMETERS:
      real,          intent (in) ::
     .   TSurf,      ! Surface or ground temperature (in deg K)
     .   P      (:), ! Pressure (mb or hPa)
     .   Alt    (:), ! Altitude (or height, in m)
     .   T      (:), ! Upper air temperatures (in deg K)
     .   LRate  (:), ! Lapse rate (i.e dT/dAlt, in degK / m )
     .   SE     (:), ! Solar elevation angle (in degrees).
     .   RRate  (:), ! Rise rate (in m/s)
     .   CirTh,      ! Thickness of any cirrus (i.e ice) clouds (in m)
     .   CirAlt,     ! Altitude of cirrus cloud top (in m)
     .   CldAmt,     ! Water cloud cover (in %)
     .   CldAlt,     ! Altitude of cloud top (in m) 
     .   CldTemp     ! Cloud top temperature (in deg K)
! !OUTPUT PARAMETERS:
      real,         intent (out) ::
     .   TCorr  (:)  ! Temperature correction (i.e. T_old - T_new,
     .               !                         in deg K or C)
!
!     Note: Number of levels is determined by the minimum size among all
!           array arguments.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     01Oct2001  C. Redder   Adapted the code from NCDC
!     18Apr2002  C. Redder   Revised formula that accounts for the effects
!                            of cirrus clouds and provides a smooth
!                            transitions at dusk or dawn.  Added input
!                            parameter, CldTemp.  Changed method of 
!                            determining number of levels from size of
!                            input arrays.  Change the call to RefTable
!                            to a subroutine call to the routine RefTab.
!     12Jul2002  C. Redder   Call to function TLag change to a call to
!                            the subroutine VIZLag.
!     19Aug2002  C. Redder   Added required arguments P and dTdt, switched
!                            the order of the arguments, Alt and T.
!     17Aug2004  C. Redder   Replace the argument dTdt with LRate.  Fixed
!                            coding error that calculates the effect of
!                            surface temperature on the total correction.
! EOP
!-------------------------------------------------------------------------
c   1) Need more altitude entries in the reference table especially
c      at upper levels where large changes exist with small altitude changes.

      real, parameter ::
     .   Min_CldAmt =  0.0001,
     .   Min_CirTh  =  0.0001,
     .   DaySE      =  2.0,
     .   NightSE    = -3.0
      real    ::
     .   CirAltkm, CirThkm, CldAltkm, T_ct, T_eff,
     .   DelT_surf, DelT_bgnd,  DelT_cc, DelT_cir,
     .   DelT_ircc, DelT_solcc, DelT,
     .   DelT_30,  TSurf_ref, TmT, Alt_i, T_i, SE_i, TRef_i, RTO, Weight
      integer :: iOb, NObs
      logical :: clouds, cirrus, night

      NObs     = min ( size ( P     ),    ! Number of levels
     .                 size ( Alt   ),
     .                 size ( T     ),
     .                 size ( SE    ),
     .                 size ( RRate ),
     .                 size ( TCorr ))
      CirAltkm = CirAlt / 1000.0          ! Convert to km
      CirThkm  = CirTh  / 1000.0          !  ""
      CldAltkm = CldAlt / 1000.0          !  ""
      clouds   = CldAmt .gt. Min_CldAmt   ! Detemine if water and
      cirrus   = CldAmt .gt. Min_CirTh    ! ... cirrus clouds are present

!     Calculate condition for the standard (or referece) atmosphere
!     -------------------------------------------------------------
      call StdAtmZ ( Alt ( : NObs ), T = TCorr )
      TSurf_ref = StdZ2T ( Z = 0.0 )

!     ... and the effective temperature at cloud top
!     ---------------------------------------------- 
      T_eff = ( 100.0 - CldAmt ) * TSurf   / 100.0
     .      +           CldAmt   * CldTemp / 100.0

!     Process for each loop
!     ---------------------
      do iOb = 1, NObs
         TRef_i   = TCorr ( iOb )
         T_i      = T     ( iOb )
         Alt_i    = Alt   ( iOb ) / 1000.0 ! Convert to km
         SE_i     = SE    ( iOb )
         night    = SE_i .lt. 0.0

!        Calculate the effects due to infrared radiation
!        from the surface (if no clouds are present)
!        -----------------------------------------------
         DelT_surf    =  0.0
         if ( .not. clouds ) then
            TmT       = TSurf - TSurf_ref 
            DelT_30   = 0.0145  * TmT +  0.0000754 * TmT ** 2
            DelT_surf = DelT_30 + ( Alt_i      - 30.0 )
     .                          * ( 0.000476   * TmT
     .                            + 0.00000233 * TmT ** 2 )
         end if

!        ... due to the background temperature
!        -------------------------------------
         TmT        = T_i - TRef_i
         if ( TmT .ge. 10.0 ) then
            DelT_30 = -0.32  + 0.0755 * TmT
         else
            DelT_30 =  0.042 * TmT
         end if
         DelT_bgnd  = -DelT_30 * ( max ( Alt_i, 0.0 ) / 30.0 ) ** 1.3

!        ... due to cloud cover for the
!        ------------------------------
         DelT_cc = 0.0
         if ( clouds ) then
            TmT           = T_eff - TSurf_ref ! ... infrared
            DelT_ircc     = 0.0185   * TmT
     .                    + 0.000615 * TmT * ( Alt_i - 30.0 )
            DelT_solcc    = 0                 ! ... and solar components
            if ( .not. night ) then           !     (if daytime)
               DelT_30    = 0.27 
     .                    - 0.003 * ( min ( 90.0 - SE_i, 90.0 ))
               DelT_solcc = DelT_30 + ( DelT_30 / 26.0 )
     .                              * ( Alt_i   - 30.0 )
            endif
            DelT_cc       = DelT_ircc + DelT_solcc

         end if

!        ... due to cirrus clouds
!        ------------------------
         if      ( SE_i .le. NightSE ) then  ! ... during the night
            Weight = 1.0
         else if ( SE_i .ge. DaySE   ) then  ! ... during the day
            Weight = 1.0
         else                                ! ... at dusk or dawn
            Weight =  ( SE_i - DaySE ) / ( NightSE - DaySE )
         end if
         DelT_30   = -0.16 * CirThkm
         RTO       =   1.0 - 0.2 * Weight
         DelT_cir  = ( 1.0 + ( 1.0 / ( 30.0 - CirAltkm * Weight ))
     .                     * ( Alt_i - 30.0 ))
     .             * ( CirAltkm * 0.25 + 0.5 ) * RTO * DelT_30

c            if ( night ) then
c               DelT_cir = ( DelT_30  + ( DelT_30 / ( 30.0 - CirAltkm ))
c     .                               * ( Alt_i - 30.0 ))
c     .                  * ( CirAltkm * 0.25 + 0.5 ) * 0.8
c            else
c               DelT_cir = ( DelT_30  + ( DelT_30 / 30.0 )
c     .                               * ( Alt_i - 30.0 ))
c     .                  * ( CirAltkm * 0.25 + 0.5 )
c            end if

!        ... and the total temperature correction
!        ----------------------------------------
         DelT = DelT_surf
     .        + DelT_bgnd
     .        + DelT_cc
     .        + DelT_cir
         TCorr ( iOb ) = DelT
      end do

!     Add the bias correction for the reference conditions
!     ----------------------------------------------------
      call VIZRef ( Alt ( : NObs ), SE, TCorr )

!     Eliminate corrections (except due to
!     lag) at altitudes below liquid cloud top
!     ----------------------------------------
      do iOb = 1, NObs
         Alt_i = Alt ( iOb ) / 1000.0 ! Convert to km
         if ( clouds .and. Alt_i .le. CldAltkm ) TCorr ( iOb ) = 0.0
      end do

!     Add the lag correction
!     ----------------------
      call VIZLag ( P ( : NObs ), T, LRate, RRate, TCorr )

      return
      end subroutine VIZReg

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VIZLag () --- Adds the lag correction for VIZ sondes
!
! !DESCRIPTION:
!     This routines adds, to a given temperature correction, the lag
!     correction using an equation derived in Saunders (1976).  A
!     factor of 0.5 is added for a better emperical fit to the VIZCOR
!     model. The lag correction is added only if there are at least 
!     two observation and the first and last obervations were taken at
!     60 seconds apart.
!
! !REFERENCES:
!     Saunders, 1976:
!
! !INTERFACE:
      pure subroutine VIZLag ( P, T, LRate, RRate, TCorr )
      use m_stdatm, only : StdZ2P, StdZ2T, StdZ2R
!
! !INPUT PARAMETERS:
      implicit NONE
      real,         intent (in)    ::
     .   P     (:), ! Pressure (hPa or mb)
     .   T     (:), ! Temperature (in deg K )
     .   LRate (:), ! Lapse rate (i.e dT/dAlt, in degK / m )
     .   RRate (:)  ! Rise rate (in m/s)
! !OUTPUT PARAMETERS:
      real,         intent (inout) ::
     .   TCorr (:)  ! On input,  the given temperature correction (in deg K)
                    ! On output, the input value plus the correction due to
!                   !   lag.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     10Oct2001  C. Redder   Original code, adapted from a NCDC subroutine.
!     12Jul2002  C. Redder   Changed unit for input parameter, Alt, from
!                            km to m.  Change function to a subroutine and
!                            renamed it VizLag
!     19Aug2002  C. Redder   Replaced the required input argument, rho, with
!                            P and made it the first argument.  Removed the
!                            required argument, Alt, and added the required
!                            argument dTdt.  Modified argument TCorr to be
!                            an input as well as an output argument.
!     03Aug2004  C. Redder   Replaced the input argument, dTdt, with LRate.
!                            dTdt (temperature change wrt time) is now
!                            computed internally.
! EOP
!-------------------------------------------------------------------------

      integer :: NObs, iOb
      real    :: lamda, RR, rho, Ra, DTLag, dTdt
      Ra   = StdZ2P ( 0.0 ) / (  StdZ2T ( 0.0 ) *  StdZ2R ( 0.0 ))

      NObs = min ( size ( P     ),  ! Number of observations
     .             size ( T     ),
     .             size ( LRate ),
     .             size ( RRate ),
     .             size ( TCorr ))
      if ( NObs .le. 0 ) return

!     For each observation ...
!     ------------------------
      do iOb  = 1, NObs

!        ... calculate the lag correction
!        --------------------------------
         RR    =  RRate ( iOb )
         dTdt  =  LRate ( iOb ) * RR
         rho   =  P ( iOb ) / ( Ra * T ( iOb )) 
         lamda =  9.77  * ( rho * RR ) ** ( -0.43 )
         DTLag = -1.0   * lamda * dTdt

!        ... save the correction
!        ----------------------
         TCorr ( iOb ) = 0.5 * DTLag + TCorr ( iOb ) ! Add factor of 0.5

      end do

      return
      end subroutine VIZLag

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VIZRef () --- Adds the VIZ reference temperature correction
!
! !DESCRIPTION:
!     This subroutine adds, to a given temperature correction, the
!     reference temperature correction via table lookup and linear
!     interpolation for the VIZ rod thermistor as a subroutine of solar
!     elevation and altitude.  The table entries were generated by the
!     VIZCOR model (Luers and Eskridge, 1995) assuming a set of reference
!     conditions.  The set includes 1966 Supplemental Atmospheric
!     Mid-Latitude Sring temperature and pressure profiles, vegetation
!     ground cover, 10% relative humidity, 288~deg~K surface temperature,
!     no clouds and the LOWTRAN7 rural (5~km visibility) tropospheric
!     aerosol profile (LOWTRAN #2 aerosol profile).  LOWTRAN is a radiative
!     transfer model (Kneizys, 1988). This table is replaces and combines
!     the calculation of the terms, $\Delta T_{ref}$ and $\Delta T_{sz}$ in
!     Equation~1 in Luers (1994).  The lag correction is no included in
!     this table.  Nightime conditions are assumed for solar elevation
!     angles at or less than -7 deg.
!
! !REFERENCES:
!     Kneizys, F., E. Shettle, L. Abreu, J. Chetwynd, G. Anderson, W.
!          Gallery, J. Selby, and S. Clough, 1988: User's guide to
!          LOWTRAN-7.  AFGL-TR-88-0177, Air Force Geophysics Laboratory,
!          Hanscom Air Force Base, MA, 137~pp.
!     Luers, J. K., 1994: VIZ table regression model.  UDRI-Tr-94-003,
!          University of Dayton, 21 pp. [Available from University of
!          Dayton, Dayton, OH 45469]
!     Luers, J. K. and R. E. Eskridge, 1995: Temperature corrections for 
!          the VIZ and Vaisala radiosondes.  J. Apl Meteo., 34, 1241-1253
!
! !INTERFACE:
      pure subroutine VIZRef ( Alt, SE, TCorr )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,     intent (in),    dimension (:) ::
     .   Alt,   ! Altitude (in m)
     .   SE     ! Solar elevation angle (in deg).
!
! !OUTPUT PARAMETERS:
      real,     intent (inout), dimension (:) ::
     .   TCorr  ! On input,  the given temperature correction (in deg K or C). 
                ! On output, the input value plus the reference temperature
                !   correction.
! !SEE ALSO:
!
! !REVISION HISTORY:
!     10Oct2001  C. Redder   Original code, adapted from a NCDC subroutine.
!     22Apr2002  C. Redder   Made routine a subroutine
!     12Jul2002  C. Redder   Changed unit for input parameter, Alt, from
!                            km to m
!     15Aug2002  C. Redder   Modified the argument TCorr to be an input as
!                            well as an output argument.     
! EOP
!-------------------------------------------------------------------------

      real    ::  HSE1,  HSE2, HAlt1,  HAlt2,
     .            DT1,   DT2,  RAlt,   RSE,  Alti, SEi, DTRef
      integer :: iHSE, iiHSE, iHAlt, iiHAlt, NObs, iOb
c      real    :: DT1, DT2, R1, R2, SE0
c      integer :: iSE, iiSE, iAlt, iiAlt

      integer, parameter ::
     .   NAlt =  9, ! Number of columns for altitudes
     .   NSE  = 13  ! ... and rows for solar elevation angles
     .              !     in the reference tables
      real,    parameter ::
     .   HAlt ( NAlt ) = (/ 0., 5., 10., 15., 20., 25., 30., 35., 40./),
     .   HSE  ( NSE  ) = (/
     .    -7.0,  -5.0,  -3.0,  -1.0,   0.0,   5.0,  10.0,  20.0,
     .           30.0,  45.0,  60.0,  75.0,  90.0 /),             ! Alt (km)
     .   !--------------------------------------------------------!---------
     .   DegC ( NSE, NAlt ) = Reshape ( (/                        !
     .    -0.47, -0.47, -0.47, -0.47, -0.47, -0.45, -0.43, -0.39, !  0.0
     .           -0.33, -0.28, -0.22, -0.17, -0.06,               ! 
     .    -0.33, -0.33, -0.33, -0.30, -0.29, -0.20, -0.16, -0.12, !  5.0
     .           -0.08, -0.02,  0.02,  0.06,  0.07,               !
     .    -0.07, -0.07, -0.07,  0.04,  0.08,  0.17,  0.21,  0.26, ! 10.0
     .            0.30,  0.38,  0.44,  0.49,  0.50,               !
     .    -0.04, -0.04, -0.01,  0.19,  0.23,  0.33,  0.37,  0.43, ! 15.0
     .            0.50,  0.59,  0.68,  0.74,  0.76,               !
     .    -0.12, -0.10,  0.14,  0.29,  0.34,  0.46,  0.51,  0.59, ! 20.0
     .            0.67,  0.81,  0.93,  1.02,  1.05,               !
     .    -0.39, -0.37,  0.05,  0.24,  0.30,  0.42,  0.47,  0.57, ! 25.0
     .            0.68,  0.86,  1.03,  1.15,  1.19,               !
     .    -0.82, -0.81, -0.13,  0.09,  0.15,  0.26,  0.32,  0.44, ! 30.0
     .            0.58,  0.81,  1.03,  1.18,  1.24,               !
     .    -1.61, -1.60, -0.81, -0.57, -0.50, -0.37, -0.30, -0.16, ! 35.0
     .            0.00,  0.27,  0.52,  0.70,  0.76,               !
     .    -2.88, -2.81, -1.99, -1.73, -1.65, -1.52, -1.45, -1.29, ! 40.0
     .           -1.12, -0.84, -0.58, -0.40, -0.32                !
     .    /), (/ NSE, NAlt /)) ! -------------------------------------------
     .   !-7.0,  -5.0,  -3.0,  -1.0,   0.0,   5.0,  10.0,  20.0,  ! Sun elev
     .   !       30.0,  45.0,  60.0,  75.0,  90.0                 ! (deg)

      HAlt1 = HAlt ( NAlt )
      HAlt2 = HAlt (    1 )
      HSE1  = HSE  (  NSE )
      HSE2  = HSE  (    1 )

!     Observation loop
!     ----------------
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
               if ( SEi  .lt. HSE  ( iHSE + 1 )) exit
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

!        Add referece correction
!        -----------------------
         TCorr ( iOb ) = DTRef + TCorr ( iOb )

      end do

!     Determine the table entry number for the solar angle
!     ----------------------------------------------------
c      SE0 = max ( min ( SE, HSE ( NSE ) ), HSE ( 1 ) ) 
c      do iSE = 1, 12
c         iiSE = iSE
c         if ( SE0  .lt. HSE  ( iSE  + 1 ) ) exit
c      end do

!     ... and for altitude
!     --------------------
c      do iAlt = 1, 8
c         iiAlt = iAlt
c         if ( Alt  .lt. HAlt ( iAlt + 1 ) ) exit
c      end do

!     Perform 2-dimensional linear interpolation
!     ------------------------------------------
c      R1       = ( HSE  ( iiSE  ) -  SE0 )
c     .         / ( HSE  ( iiSE  ) - HSE  ( iiSE  + 1 ))
c      R2       = ( HAlt ( iiAlt ) -  Alt  )
c     .         / ( HAlt ( iiAlt ) - HAlt ( iiAlt + 1 ))
c      DT1      = R1 * ( DegC ( iiSE + 1, iiAlt     )
c     .                - DegC ( iiSE,     iiAlt     ))
c     .                + DegC ( iiSE,     iiAlt     )
c      DT2      = R1 * ( DegC ( iiSE + 1, iiAlt + 1 )
c     .                - DegC ( iiSE,     iiAlt + 1 ))
c     .                + DegC ( iiSE,     iiAlt + 1 )
c      VIZTable = R2 * ( DT2 - DT1 ) + DT1

      return
      end subroutine VIZRef
!.................................................................
      end module m_VIZReg
!====================================================================
