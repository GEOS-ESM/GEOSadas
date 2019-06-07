!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOI
! !MODULE: m_humidity -- Humidity conversion routines
!
! !DESCRIPTION:
!     This module contains routines that convert humidity values. 
!     The function from Magnus (1844), with various coefficients, is 
!     used in this module to determine the saturation vapor pressure
!     (SVP) as a function of temperature.  The equations and/or 
!     coefficient sets are identified by an integer code.  (Note: An
!     invalid code is reset to default.)
!
! !REFERENCES:
!     Alduchov, Oleg. A. and Robert E. Eskridge, 1996: Improved Magnus
!          form apporiximations of saturation vapor pressure.  Journal of 
!          Applied Meteorology, 35, 601-609.
!
!     Magnus, G., 1844: Versuche uber die Spannkrafte des Wasserdampfes.
!          Ann. Phys. Chem., 61, 225.
!
!     Rogers, R. R., 1979: A Short Course in Cloud Physics, 2nd. Ed., 
!          Pergamon Press, Oxford. 235pp.
!
!     Tetens, O., 1930: Uber einige meteorologisch1 Begriffe., Z. Geophys.,
!          6, 297-309.
!
! !INTERFACE:
!
      module      m_humidity
      implicit    NONE
      private	! except

      public ::           ! Routine to convert from vapor pressure (VP) to ...
     .   VPtoDP,  VP2DP,  ! ... dew point (DP)
     .   VPtoFP,  VP2FP,  ! ... frost point (FP)
     .   VPtoSH,  VP2SH,  ! ... specific humidity (SH)
     .   VPtoMR,  VP2MR,  ! ... mixing ratio (MR)
     .   VPtoRH,  VP2RH   ! ... relative humidity (RH)

      public ::           ! Routines to convert from ...
     .   DPtoVP,  DP2VP,  ! ... dew point (DP)
     .   FPtoVP,  FP2VP,  ! ... frost point (FP)
     .   SHtoVP,  SH2VP,  ! ... specific humidity (SH)
     .   MRtoVP,  MR2VP,  ! ... mixing ratio (MR)
     .   RHtoVP,  RH2VP   ! ... relative humidity (RH)
      public ::           ! Routines to convert from
     .   VPTtoRH, VPT2RH, ! ... VP and air temperature to RH
     .   RHTtoVP, RHT2VP  ! ... RH and air temperature to VP

!
!     Note: Routine names of the form xxtoxx and xx2xx implies a
!           subroutine and function, respectively.
!
      public ::           ! Routine to check ...
     .   CheckMR          ! ... mixing ratio (should not increase with
                          !   height the strotophere.)

      public ::           ! Equation code for determining the SVP
     .   wEqtn_AE96,      ! ... over water from AE96
     .   wEqtn_NCEP,      ! ... over water from NCEP
     .   iEqtn_AE96       ! ... over ice   from AE96

      interface DPtoVP
         module procedure
     .      DPtoVP_pure,  ! Convert for pure water vapor or ...
     .      DPtoVP_moist  ! ... moist air (AE96 only)
      end interface

      interface VPtoDP
         module procedure
     .      VPtoDP_pure,  ! Convert for pure water vapor or ...
     .      VPtoDP_moist  ! ... moist air (AE96 only)
      end interface

      interface FPtoVP
         module procedure
     .      FPtoVP_pure,  ! Convert for pure water vapor or ...
     .      FPtoVP_moist  ! ... moist air (AE96 only)
      end interface

      interface VPtoFP
         module procedure
     .      VPtoFP_pure,  ! Convert for pure water vapor or ...
     .      VPtoFP_moist  ! ... moist air (AE96 only)
      end interface

      interface VPtoRH
         module procedure
     .      VPtoRH_wnp,   ! Convert by neglecting ...
     .      VPtoRH_wp     ! ... and without neglecting air pressure data
      end interface

      interface RHtoVP
         module procedure
     .      RHtoVP_wnp,   ! Convert by neglecting ...
     .      RHtoVP_wp     ! ... and without neglecting air pressure data
      end interface

      interface VPTtoRH
         module procedure
     .      VPTtoRH_wnp,  ! Convert by neglecting ...
     .      VPTtoRH_wp    ! ... and without neglecting air pressure data
      end interface

      interface RHTtoVP
         module procedure
     .      RHTtoVP_wnp,  ! Convert by neglecting ...
     .      RHTtoVP_wp    ! ... and without neglecting air pressure data
      end interface

!
! !REVISION HISTORY:
!     29Oct2001  C. Redder  Original code
!      ~   2003  C. Redder  Added the generic interfaces, VPTtoRH, and
!                           RHTtoVP.
!     02Apr2004  C. Redder  Added the routine CheckMR.  Generalized
!                           computation of Ew and Ei to include other
!                           coefficients using the Magnus formuation.     
!
!EOI
!-----------------------------------------------------------------

      real,    parameter ::
     .   Ra   =  287.0,      ! The individual gas constant for dry air and ...
     .   Rv   =  461.0,      ! ... water vapor (j/(kg-deg K, from Rogers, 1979)
     .   RaRv =  Ra / Rv,
     .   SHUnit   = 1000.0,  ! Factors that set the unit of specific humidity
     .   MRUnit   = 1000.0,  ! ... and mixing ratios to g/kg.
     .   PctF     =  100.0,  ! ... and the relative humidity to %
     .   T0       =  273.16  ! ... at the reference temperature (deg K)
      integer, parameter ::
     .   ScrSz  = 1000       ! Scratch size

      integer, parameter ::  ! Equation codes for determining the SVP
     .   wEqtn_AE96 = 0,     ! ... over water from AE96
     .   wEqtn_NCEP = 1,     ! ... over water from NCEP
     .   iEqtn_AE96 = 0      ! ... over ice   from AE96

      contains

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Get_wMagnus --- Get coefficients for Magnus formulation over water
! 
! !INTERFACE:
      pure subroutine  Get_wMagnus ( Ew0, aw, bw, Eqtn )
!
! !USES
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,       intent (in), optional ::
     .   Eqtn      ! Equation code for coefficient sets (see prologue,
                   !   default: 0)
!
! !INPUT/OUTPUT PARAMETERS:
      real,          intent (out) ::
     .   Ew0,      ! Saturation vapor pressure at temperature, T0
     .   aw,       ! coefficent a
     .   bw        ! ... and b (Equation 6 in AE96)
!
! !DESCRIPTION:
!
! !REVISION HISTORY: 
!     03Apr2004  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------

      integer :: Code_
      Code_ = wEqtn_AE96
      if ( present ( Eqtn )) Code_ = Eqtn

!     Set coefficients from ...
!     -------------------------
      if ( Code_ .eq. wEqtn_NCEP ) then ! ... from ncep (or Tetens, 1930)
         Ew0 =    6.1078
         aw  =   17.269
         bw  =  237.3

      else                              ! ... from AE96 (Eq. 21 in AE96,
         Ew0 =    6.1094                !                default)
         aw  =   17.625 
         bw  =  243.04

      end if

      return
      end subroutine Get_wMagnus

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Get_iMagnus --- Get coefficients for Magnus formulation over ice
! 
! !INTERFACE:
      pure subroutine  Get_iMagnus ( Ei0, ai, bi, Eqtn )
!
! !USES
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,       intent (in), optional ::
     .   Eqtn      ! Equation code for coefficient sets (see prologue,
                   !   default: 0)
!
! !INPUT/OUTPUT PARAMETERS:
      real,          intent (out) ::
     .   Ei0,      ! Saturation vapor pressure at temperature, T0
     .   ai,       ! coefficent a
     .   bi        ! ... and b (Equation 6 in AE96)
!
! !DESCRIPTION:
!
! !REVISION HISTORY: 
!     03Apr2004  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------
      integer :: Code_

      Code_ = iEqtn_AE96
      if ( present ( Eqtn )) Code_ = Eqtn

!     Set coefficients from ...
!     -------------------------
         Ei0 =    6.1121 ! ... AE96 (Eq. 21 in AE96, default)
         ai  =   22.587
         bi  =  273.86

      return
      end subroutine Get_iMagnus

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CheckMR --- Edit stratospheric values of mixing ratio
! 
! !INTERFACE:
      subroutine  CheckMR ( T, P, MR )
!
! !USES
      implicit NONE
!
! !INPUT PARAMETERS:
      real,          intent (in)    ::
     .   T  (:),   ! Temperature (K)
     .   P  (:)    ! Pressure (hPa)
!
! !INPUT/OUTPUT PARAMETERS:
      real,          intent (inout) ::
     .   MR (:)    ! Mixing ratio (g/kg)
!
! !DESCRIPTION:
!     This routine insures that the mixing ratio in the stratosphere 
!     does not increase with height.
!
! !REVISION HISTORY: 
!     23Mar2004  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------
      real, parameter ::
     .   TP_Bottom  = 299.0,
     .   TP_Top     =  90.0
      real    :: TP_Temp, Ti, Pi, MRi, MR_Max
      integer :: iLev, NLev, iTP_Bottom, iTP_Top, iTPause
 

      NLev = min ( size ( T ), size ( P ), size ( MR ))
      if ( NLev       .le. 0 ) return

!     If all data is well within the troposphere,
!     then there is nothing more to do
!     ------------------------------------------
      if ( P ( NLev ) .ge. TP_Bottom ) return

!     Locate the tropause or the lowest layer above the tropause
!     ----------------------------------------------------------
      iTPause = 1
      if ( P ( 1 ) .gt. TP_Top ) then
         iTPause = NLev
         TP_Temp = 2.0 * T ( 1 )
         do iLev = NLev - 1, 1, -1
            Ti = T ( iLev )
            Pi = P ( iLev )
            if      ( Ti .lt. TP_Temp .and.
     .                Pi .gt. TP_Top ) then
               iTPause = iLev
               TP_Temp = Ti
            else if ( Pi .le. TP_Top ) then
               iTPause = iLev
            end if
            if ( Pi .ge. TP_Bottom ) exit
         end do
      end if

!     Ensure that mixing ratio does not increase in the stratophere
!     -------------------------------------------------------------
      MR_Max = MR ( iTPause )
      do iLev = iTPause + 1, NLev
         MRi         = MR ( iLev )
         MR_Max      = min ( MRi, MR_Max )
         MR ( iLev ) = MR_Max

      end do

      return
      end subroutine CheckMR

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: DPtoVP_pure () --- Dew point to vapor pressure for pure water vapor
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from dew point
!     temperature to vapor pressure for pure water vapor.
!
! !INTERFACE:
      pure subroutine DPtoVP_pure ( DP, VP, Eqtn )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in)           :: DP (:) ! dew point (deg K)
      integer, intent (in), optional :: Eqtn   ! equation code (default: 0)
!
! !OUTPUT PARAMETERS:
      real,    intent (out)          :: VP (:) ! vapor pressure (mb or hPa)
!
!     Note: The minimum size of the arrays determines the number of values
!           to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     19Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of values are
!                            determined.
!     03Apr2004  C. Redder   Added the optional argument, Eqtn
! EOP
!-------------------------------------------------------------------------

      real    :: TC, Ew0, aw, bw
      integer :: iHum, NHum

      call Get_wMagnus ( Ew0, aw, bw, Eqtn = Eqtn )

      NHum = min ( size ( DP ), size ( VP ))
      do iHum = 1, NHum
         TC          = DP ( iHum ) - T0             ! from deg K to deg C
         VP ( iHum ) = Ew0 * exp ( aw * TC / ( bw + TC )) 
      end do

      return
      end subroutine DPtoVP_pure

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: DPtoVP_moist () --- Dew point to vapor pressure for moist air
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from dew point
!     temperature to vapor pressure for moist air.
!
! !INTERFACE:
      pure subroutine DPtoVP_moist ( DP, P, VP )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in)  ::
     .   DP (:), ! dew point (deg K)
     .   P  (:)  ! air pressure (mb or hPa)
!
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   VP (:)  ! vapor pressure (mb or hPa)
!
!     Note: The minimum size of the input arrays DP and VP determine
!           the number of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     19Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of values are
!                            determined.
!
! EOP
!-------------------------------------------------------------------------

      real, parameter :: a = 4.5e-6, c = 1.00071
      real    :: PP
      integer :: iHum, NHum, NP

!     Calculate vapor pressure for pure water vapor
!     ---------------------------------------------
      call DPtoVP_pure ( DP, VP )

!     ... and then add the enhancement factor
!     ---------------------------------------
      NP   = size ( P )
      NHum = min  ( size ( DP ), size ( VP ))  
      if ( NP .eq. 0 ) NHum = 0
      do iHum = 1, NHum
         PP = P ( min ( iHum, NP ))
         VP ( iHum ) = c * exp ( a * PP ) * VP ( iHum )
      end do

      return
      end subroutine DPtoVP_moist

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: DP2VP () --- Dew point to vapor pressure
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from dew point
!     temperature to vapor pressure for moist air (AE96 only) or pure
!     water vapor.
!
! !INTERFACE:
      pure function DP2VP ( DP, P, Eqtn )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in)           :: DP    ! dew point (deg K)
      real,    intent (in), optional :: P     ! air pressure (mb or hPa) if
                                              !   moist air is assumed
      integer, intent (in), optional :: Eqtn  ! equation code (default: 0)
!
! !OUTPUT PARAMETERS:
      real                           :: DP2VP ! vapor pressure (mb or hPa)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     19Oct2001  C. Redder   Original code.
!     03Apr2004  C. Redder   Added the optional argument, Eqtn
!
! EOP
!-------------------------------------------------------------------------
      real :: VP (1)

!     Convert for ...
!     ---------------
      if ( present ( P ) ) then
         call DPtoVP_moist ( (/DP/), (/P/), VP ) ! ... moist air
      else
         call DPtoVP_pure  ( (/DP/),        VP, Eqtn = Eqtn )
     .                                           ! ... pure water vapor
      end if
      DP2VP = VP (1)

      return
      end function DP2VP

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VPtoDP_pure () --- Vapor pressure to dew point for pure water vapor
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from vapor pressure
!     to dew point temperature for pure water vapor.
!
! !INTERFACE:
      pure subroutine VPtoDP_pure ( VP, DP, Eqtn )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in)           :: VP (:) ! vapor pressure (mb or hPa)
      integer, intent (in), optional :: Eqtn   ! equation code (default: 0)
!
! !OUTPUT PARAMETERS:
      real,    intent (out)          :: DP (:) ! dew point (deg K)
!
!     Note: The minimum size of the arrays determines the number of values
!           to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     19Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of values are
!                            determined.
!     03Apr2004  C. Redder   Added the optional argument, Eqtn
!
! EOP
!-------------------------------------------------------------------------

      real    :: TC, x, Ew0, aw, bw
      integer :: iHum, NHum

      call Get_wMagnus ( Ew0, aw, bw, Eqtn = Eqtn )

      NHum = min ( size ( VP ), size ( DP ))
      do iHum = 1, NHum
         x  = log ( VP ( iHum ) / Ew0 )
         TC = bw * x / ( aw - x )
         DP ( iHum ) = TC + T0         ! from deg C to deg K
      end do

      return
      end subroutine VPtoDP_pure

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VPtoDP_moist () --- Vapor pressure to dew point for moist air
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from vapor pressure
!     to dew point temperature for moist air.
!
! !INTERFACE:
      pure subroutine VPtoDP_moist ( VP, P, DP )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in)  ::
     .   VP (:), ! vapor pressure (mb or hPa)
     .   P  (:)  ! air pressure (mb or hPa)
!
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   DP (:)  ! dew point (deg K)
!
!     Note: The minimum size of the arrays VP and DP determines the number
!           of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     19Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of values are
!                            determined.
!     21Feb2003  C. Redder   Added temporary storage for intermediate 
!                            calculations.
!     28Oct2003  C. Redder   Revised management of scratch space
! EOP
!-------------------------------------------------------------------------

      real, parameter :: a = 4.5e-6, c = 1.00071
      real    :: PP
      integer :: iHum, NHum, NP, iSeg, NSeg,
     .           iSegHum, iSegBeg, iSegEnd, SegSz
      real, dimension ( ScrSz ) :: Tmp

!     Remove enhancement factor
!     -------------------------
      NP   = size ( P )
      NHum = min  ( size ( VP ), size ( DP ))
      if ( NP .eq. 0 ) NHum = 0
      do iHum = 1, NHum
         PP = P ( min ( iHum, NP ))
         DP ( iHum ) = VP ( iHum ) / ( c * exp ( a * PP ))
      end do

!     ... before calculating the vapor pressure
!     -----------------------------------------
      NSeg    = ( NHum - 1 ) / ScrSz + 1
      iSegBeg =   1
      do iSeg = 1, NSeg
         iSegEnd = min ( iSegBeg + ScrSz - 1, NHum )
         SegSz   = iSegEnd - iSegBeg + 1

         do iSegHum = 1, SegSz
            iHum            = iSegBeg + iSegHum - 1
            Tmp ( iSegHum ) = DP ( iHum )
         end do
         call VPtoDP_pure ( Tmp ( : SegSz ), DP ( iSegBeg : iSegEnd ))
         iSegBeg = iSegEnd + 1         
      end do

      return
      end subroutine VPtoDP_moist

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VP2DP () --- Vapor pressure to dew point
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from vapor pressure
!     to dew point temperature for moist air (AE96) or pure water vapor.
!
! !INTERFACE:
      pure function VP2DP ( VP, P, Eqtn )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in)           :: VP    ! vapor pressure (mb or hPa)
      real,    intent (in), optional :: P     ! air pressure (mb or hPa) if
                                              !   moist air is assumed
      integer, intent (in), optional :: Eqtn  ! Equation code (default: 0)
!
! !OUTPUT PARAMETERS:
      real                           :: VP2DP ! dew point (deg K)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     19Oct2001  C. Redder   Original code.
!     03Apr2004  C. Redder   Added the optional argument, Eqtn
!
! EOP
!-------------------------------------------------------------------------

      real :: DP (1)

!     Convert for ...
!     ---------------
      if ( present ( P ) ) then
         call VPtoDP_moist ( (/VP/), (/P/), DP ) ! ... moist air
      else
         call VPtoDP_pure  ( (/VP/),        DP, Eqtn = Eqtn )
     .                                           ! ... pure water vapor
      end if
      VP2DP = DP (1)

      return
      end function VP2DP
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: FPtoVP_pure () --- Frost point to vapor pressure for pure water vapor
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from frost point
!     temperature to vapor pressure for pure water vapor.
!
! !INTERFACE:
      pure subroutine FPtoVP_pure ( FP, VP, Eqtn )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in)           :: FP (:) ! frost point (deg K)
      integer, intent (in), optional :: Eqtn   ! equation code (default: 0)
!
! !OUTPUT PARAMETERS:
      real,    intent (out)          :: VP (:) ! vapor pressure (mb or hPa)
!
!     Note: The minimum size of the arrays determines the number of values
!           to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     19Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of values are
!                            determined.
!     03Apr2004  C. Redder   Added the optional argument, Eqtn
!
! EOP
!-------------------------------------------------------------------------

      real    :: TC, Ei0, ai, bi
      integer :: iHum, NHum

      call Get_iMagnus ( Ei0, ai, bi, Eqtn = Eqtn )

      NHum = min ( size ( FP ), size ( VP ))
      do iHum = 1, NHum
         TC          = FP ( iHum ) - T0             ! from deg K to deg C
         VP ( iHum ) = Ei0 * exp ( ai * TC / ( bi + TC )) 
      end do

      return
      end subroutine FPtoVP_pure

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: FPtoVP_moist () --- Frost point to vapor pressure for moist air
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from frost point
!     temperature to vapor pressure for moist air.
!
! !INTERFACE:
      pure subroutine FPtoVP_moist ( FP, P, VP )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in)  ::
     .   FP (:), ! frost point (deg K)
     .   P  (:)  ! air pressure (mb or hPa)
!
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   VP (:)  ! vapor pressure (mb or hPa)
!
!     Note: The minimum size of the arrays FP and VP determines the number
!           of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     19Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of values are
!                            determined.
!
! EOP
!-------------------------------------------------------------------------

      real, parameter :: a = 8.0e-6, c = 0.99882
      real    :: PP
      integer :: iHum, NHum, NP

!     Calculated vapor pressure for pure water vapor
!     ----------------------------------------------
      call FPtoVP_pure ( FP, VP )

!     ... and then add enhancement factor
!     -----------------------------------
      NP   = size ( P )
      NHum = min  ( size ( FP ), size ( VP ))
      if ( NP .eq. 0 ) NHum = 0
      do iHum = 1, NHum
         PP = P ( min ( iHum, NP ))
         VP ( iHum ) = c * exp ( a * PP ) * VP ( iHum )
      end do

      return
      end subroutine FPtoVP_moist

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: FP2VP () --- Frost point to vapor pressure
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from frost point to
!     temperature to vapor pressure for moist air (AE96 only) or pure
!     water vapor.
!
! !INTERFACE:
      pure function FP2VP ( FP, P, Eqtn )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in)           :: FP    ! frost point (deg K)
      real,    intent (in), optional :: P     ! air pressure (mb or hPa) if
                                              !   moist air is assumed
      integer, intent (in), optional :: Eqtn  ! equation code (default: 0)
!
! !OUTPUT PARAMETERS:
      real                           :: FP2VP ! vapor pressure (mb or hPa)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     19Oct2001  C. Redder   Original code.
!     03Apr2004  C. Redder   Added the optional argument, Eqtn
!
! EOP
!-------------------------------------------------------------------------
      real :: VP (1)

!     Convert for ...
!     ---------------
      if ( present ( P ) ) then
         call FPtoVP_moist ( (/FP/), (/P/), VP ) ! ... moist air
      else
         call FPtoVP_pure  ( (/FP/),        VP, Eqtn = Eqtn )
     .                                           ! ... pure water vapor
      end if
      FP2VP = VP (1)

      return
      end function FP2VP

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VPtoFP_pure () --- Vapor pressure to frost point for pure water vapor
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from vapor pressure
!     to frost point temperature for pure water vapor.
!
! !INTERFACE:
      pure subroutine VPtoFP_pure ( VP, FP, Eqtn )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in)           :: VP (:) ! vapor pressure (mb or hPa)
      integer, intent (in), optional :: Eqtn   ! equation code (default: 0)
!
! !OUTPUT PARAMETERS:
      real,    intent (out)          :: FP (:) ! frost point (deg K)
!
!     Note: The minimum size of the arrays determines the number
!           of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     19Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of values are
!                            determined.
!     03Apr2004  C. Redder   Added the optional argument, Eqtn
!
! EOP
!-------------------------------------------------------------------------

      real    :: TC, x, Ei0, ai, bi
      integer :: iHum, NHum

      call Get_iMagnus ( Ei0, ai, bi, Eqtn = Eqtn )

      NHum = min ( size ( VP ), size ( FP ))
      do iHum = 1, NHum
         x  = log ( VP ( iHum ) / Ei0 )
         TC = bi * x / ( ai - x )
         FP ( iHum ) = TC + T0         ! from deg C to deg K
      end do

      return
      end subroutine VPtoFP_pure

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VPtoFP_moist () --- Vapor pressure to frost point with moist air
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from vapor pressure
!     to frost point temperature for moist air.
!
! !INTERFACE:
      pure subroutine VPtoFP_moist ( VP, P, FP )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in)  ::
     .   VP (:), ! vapor pressure (mb or hPa)
     .   P  (:)  ! air pressure (mb or hPa)
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   FP (:)  ! frost point (deg K)
!
!     Note: The minimum size of the arrays VP and FP determines the number
!           of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     19Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of values are
!                            determined.
!     21Feb2003  C. Redder   Added temporary storage for intermediate 
!                            calculations.
!     28Oct2003  C. Redder   Revised management of scratch space
! EOP
!-------------------------------------------------------------------------

      real, parameter :: a = 8.0e-6, c = 0.99882
      real    :: PP
      integer :: iHum, NHum, NP, iSeg, NSeg,
     .           iSegHum, iSegBeg, iSegEnd, SegSz
      real, dimension ( ScrSz ) :: Tmp

!     Remove enhancement factor
!     -------------------------
      NP   = size ( P )
      NHum = min  ( size ( VP ), size ( FP ))
      if ( NP .eq. 0 ) NHum = 0
      do iHum = 1, NHum
         PP = P ( min ( iHum, NP ))
         FP ( iHum ) = VP ( iHum ) / ( c * exp ( a * PP ))
      end do

!     ... before calculating the vapor pressure
!     -----------------------------------------
      NSeg    = ( NHum - 1 ) / ScrSz + 1
      iSegBeg =   1
      do iSeg = 1, NSeg
         iSegEnd = min ( iSegBeg + ScrSz - 1, NHum )
         SegSz   = iSegEnd - iSegBeg + 1

         do iSegHum = 1, SegSz
            iHum            = iSegBeg + iSegHum - 1
            Tmp ( iSegHum ) = FP ( iHum )
         end do

         call VPtoFP_pure ( Tmp ( : SegSz ), FP ( iSegBeg : iSegEnd ))
         iSegBeg = iSegEnd + 1         
      end do

      return
      end subroutine VPtoFP_moist

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VP2FP () --- Vapor pressure to frost point
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from vapor pressure to
!     frost point temperature for moist air (AE96 only) or pure water vapor.
!
! !INTERFACE:
      pure function VP2FP ( VP, P, Eqtn )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in)           :: VP    ! vapor pressure (mb or hPa)
      real,    intent (in), optional :: P     ! air pressure (mb or hPa) if
                                              !   moist air is assumed
      integer, intent (in), optional :: Eqtn  ! equation code (default: 0)
!
! !OUTPUT PARAMETERS:
      real                           :: VP2FP ! frost point (deg K)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     19Oct2001  C. Redder   Original code.
!     03Apr2004  C. Redder   Added the optional argument, Eqtn
!
! EOP
!-------------------------------------------------------------------------

      real :: FP (1)

!     Convert for ...
!     ---------------
      if ( present ( P ) ) then
         call VPtoFP_moist ( (/VP/), (/P/), FP ) ! ... moist air
      else
         call VPtoFP_pure  ( (/VP/),        FP, Eqtn = Eqtn )
     .                                           ! ... pure water vapor
      end if
      VP2FP = FP (1)

      return
      end function VP2FP

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VPtoSH () --- Vapor pressure to specific humidity
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from vapor pressure
!     to specific humidity.
!
! !INTERFACE:
      pure subroutine VPtoSH ( VP, P, SH )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in) ::
     .   VP (:), ! vapor pressure (mb or hPa)
     .   P  (:)  ! air pressure (mb or hPa)
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   SH (:)  ! specific humidity (g/kg)
!
!     Note: The minimum size of the arrays VP and SH determines the number
!           of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     19Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of values are
!                            determined.
! EOP
!-------------------------------------------------------------------------

      real    :: E, PP
      integer :: iHum, NHum, NP

      NP   = size ( P )
      NHum = min  ( size ( VP ), size ( SH ))
      if ( NP .eq. 0 ) NHum = 0
      do iHum = 1, NHum
         E  = VP ( iHum )
         PP = P  ( min ( iHum, NP ))
         SH ( iHum ) = SHUnit * RaRv * E / ( PP - ( 1.0 - RaRv ) * E )
      end do

      return
      end subroutine VPtoSH

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VP2SH () --- vapor pressure to specific humidity
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from vapor pressure 
!     to specific humidity.
!
! !INTERFACE:
      pure function VP2SH ( VP, P )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in) :: VP    ! vapor pressure (mb or hPa)
      real,    intent (in) :: P     ! air pressure (mb or hPa)
! !OUTPUT PARAMETERS:
      real                 :: VP2SH ! specific humidity (g/kg)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     19Oct2001  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real :: SH (1)

      call VPtoSH ( (/VP/), (/P/), SH )
      VP2SH = SH (1)

      return
      end function VP2SH

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: SHtoVP () --- Specific humidity to vapor pressure
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from specific
!     humidity to vapor pressure.
!
! !INTERFACE:
      pure subroutine SHtoVP ( SH, P, VP )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in) ::
     .   SH (:), ! specific humidity (g/kg)
     .   P  (:)  ! air pressure (mb or hPa)
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   VP (:)  ! vapor pressure (mb or hPa)
!
!     Note: The minimum size of the arrays SH and BP determines the number
!           of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     19Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of values are
!                            determined.
! EOP
!-------------------------------------------------------------------------

      real    :: Q, PP
      integer :: iHum, NHum, NP

      NP   =        size (  P )
      NHum = min  ( size ( SH ), size ( VP ))
      if ( NP .eq. 0 ) NHum = 0
      do iHum = 1, NHum
         Q  = SH ( iHum ) / SHUnit
         PP = P  ( min ( iHum, NP ))
         VP ( iHum ) = Q * PP / ( RaRv + Q * ( 1.0 - RaRv ))
      end do

      return
      end subroutine SHtoVP

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: SH2VP () --- Vapor pressure to specific humidity
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from specific humidity 
!     to vapor pressure.
!
! !INTERFACE:
      pure function SH2VP ( SH, P )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in) :: SH    ! specific humidity (g/kg)
      real,    intent (in) :: P     ! air pressure (mb or hPa)
! !OUTPUT PARAMETERS:
      real                 :: SH2VP ! vapor pressure (mb or hPa)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     19Oct2001  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real :: VP (1)

      call SHtoVP ( (/SH/), (/P/), VP )
      SH2VP = VP (1)

      return
      end function SH2VP

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VPtoMR () --- Vapor pressure to mixing ratio
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from vapor pressure
!     to mixing ratio.
!
! !INTERFACE:
      pure subroutine VPtoMR ( VP, P, MR )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in) ::
     .   VP (:), ! vapor pressure (mb or hPa)
     .   P  (:)  ! air pressure (mb or hPa)
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   MR (:)  ! mixing ratio (g/kg)
!
!     Note: The minimum size of the array VP and MR determines the number
!           of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     19Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of values are
!                            determined.
!
! EOP
!-------------------------------------------------------------------------

      real    :: E, PP
      integer :: iHum, NHum, NP

      NP   = size ( P )
      NHum = min  ( size ( VP ), size ( MR ))
      if ( NP .eq. 0 ) NHum = 0
      do iHum = 1, NHum
         E  = VP ( iHum )
         PP = P  ( min ( iHum, NP ))
         MR ( iHum ) = MRUnit * RaRv * E / ( PP - E )
      end do

      return
      end subroutine VPtoMR

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VP2MR () --- Vapor pressure to mixing ratio
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from vapor pressure 
!     to mixing ratio.
!
! !INTERFACE:
      pure function VP2MR ( VP, P )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in) :: VP    ! vapor pressure (mb or hPa)
      real,    intent (in) :: P     ! air pressure (mb or hPa)
! !OUTPUT PARAMETERS:
      real                 :: VP2MR ! mixing ratio (g/kg)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     19Oct2001  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real :: MR (1)

      call VPtoMR ( (/VP/), (/P/), MR )
      VP2MR = MR (1)

      return
      end function VP2MR

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: MRtoVP () --- Mixing ratio to vapor pressure
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from specific
!     humidity to vapor pressure.
!
! !INTERFACE:
      pure subroutine MRtoVP ( MR, P, VP )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in) ::
     .   MR (:), ! mixing ratio (g/kg)
     .   P  (:)  ! air pressure (mb or hPa)
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   VP (:)  ! vapor pressure (mb or hPa)
!
!     Note: The minimum size of the array MR and VP determines the number
!           of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     19Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of values are
!                            determined.
! EOP
!-------------------------------------------------------------------------

      real    :: W, PP
      integer :: iHum, NHum, NP

      NP   = size ( P )
      NHum = min  ( size ( MR ), size ( VP ))
      if ( NP .eq. 0 ) NHum = 0
      do iHum = 1, NHum
         W  = MR ( iHum ) / MRUnit
         PP = P  ( min ( iHum, NP ))
         VP ( iHum ) = W * PP / ( RaRv + W )
      end do

      return
      end subroutine MRtoVP

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: MR2VP () --- Vapor pressure to mixing ratio
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from mixing ratio 
!     to vapor pressure.
!
! !INTERFACE:
      pure function MR2VP ( MR, P )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in) :: MR    ! mixing ratio (g/kg)
      real,    intent (in) :: P     ! air pressure (mb or hPa)
! !OUTPUT PARAMETERS:
      real                 :: MR2VP ! vapor pressure (mb or hPa)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     19Oct2001  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real :: VP (1)

      call MRtoVP ( (/MR/), (/P/), VP )
      MR2VP = VP (1)

      return
      end function MR2VP

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VPtoRH_wnp () --- Vapor pressure to relative humidity by neglecting air pressure
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from vapor pressure
!     to relative humidity by neglecting air pressure
!
! !INTERFACE:
      pure subroutine VPtoRH_wnp ( VP, SVP, RH )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in) ::
     .   VP (:), ! vapor pressure (mb or hPa)
     .  SVP (:)  ! saturation vapor pressure (mb or hPa)
!
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   RH (:)  ! relative humidity (%)
!
!     Note: The minimum size of the arrays determines the number
!           of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     29Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of values are
!                            determined.
! EOP
!-------------------------------------------------------------------------

      real    :: EE, EW
      integer :: iHum, NHum

      NHum = min ( size ( VP ), size ( SVP ), size ( RH ))
      do iHum = 1, NHum
         EE =  VP ( iHum )
         EW = SVP ( iHum )
         RH ( iHum ) = PctF * EE / EW
      end do

      return
      end subroutine VPtoRH_wnp

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VPtoRH_wp () --- Vapor pressure to relative humidity without neglecting air pressure
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from vapor pressure
!     to relative humidity without neglecting air pressure.
!
! !INTERFACE:
      pure subroutine VPtoRH_wp ( VP, SVP, P, RH )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in) ::
     .   VP (:), ! vapor pressure (mb or hPa)
     .  SVP (:), ! saturation vapor pressure (mb or hPa)
     .   P  (:)  ! ... and pressure (mb or hPa)
!
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   RH (:)  ! relative humidity (%)
!
!     Note: The minimum size of the arrays VP, SVP and RH determines the
!           number of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     29Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of values are
!                            determined.
!
! EOP
!-------------------------------------------------------------------------

      real    :: EE, EW, PP
      integer :: iHum, NHum, NP

      NP   = size ( P  )
      NHum = min  ( size ( VP ), size ( SVP ), size ( RH )) 
      if ( NP .eq. 0 ) NHum = 0
      do iHum = 1, NHum
         EE =  VP ( iHum )
         EW = SVP ( iHum )
         PP =   P ( min ( iHum, NP ))
         RH ( iHum ) = PctF * EE * ( PP - EW ) / ( EW * ( PP - EE ))
      end do

      return
      end subroutine VPtoRH_wp

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VP2RH () --- Vapor pressure to relative humidity
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from vapor pressure to
!     relative humdity.
!
! !INTERFACE:
      pure function VP2RH ( VP, SVP, P )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in) ::
     .   VP,   ! vapor pressure (mb or hPa)
     .  SVP    ! saturation vapor pressure (mb or hPa)
      real,    intent (in), optional ::
     .    P    ! air pressure (mb or hPa)
!
! !OUTPUT PARAMETERS:
      real ::
     .   VP2RH ! relative humidity (%)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     29Oct2001  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real :: RH (1)

!     Convert for ...
!     ---------------
      if ( present ( P ) ) then
         call VPtoRH_wp  ((/VP/), (/SVP/), (/P/), RH )
      else
         call VPtoRH_wnp ((/VP/), (/SVP/),        RH )
      end if
      VP2RH = RH (1)

      return
      end function VP2RH
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: RHtoVP_wnp () --- Relative humidity to vapor pressure by neglecting air pressure 
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from relative humidity
!     to vapor pressure by neglecting air pressure.
!
! !INTERFACE:
      pure subroutine RHtoVP_wnp ( RH, SVP, VP )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,       intent (in) ::
     .   RH  (:), ! relative humidity (%)
     .   SVP (:)  ! saturation vapor pressure (mb or hPa)
!
! !OUTPUT PARAMETERS:
      real,       intent (out) ::
     .   VP  (:)  ! vapor pressure (mb or hPa)
!
!     Note: The minimum size of the arrays determines the
!           number of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     29Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of values are
!                            determined.
!
! EOP
!-------------------------------------------------------------------------

      real    :: EW
      integer :: iHum, NHum

      NHum = min ( size ( RH ), size ( SVP ), size ( VP ))
      do iHum = 1, NHum
         EW = SVP ( iHum )
         VP ( iHum ) = RH ( iHum ) * EW / PctF
      end do

      return
      end subroutine RHtoVP_wnp

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: RHtoVP_wp () --- Relative humidity to vapor pressure without neglecting air pressure
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from relative
!     humidity to vapor pressure without neglecting air pressure.
!
! !INTERFACE:
      pure subroutine RHtoVP_wp ( RH, SVP, P, VP )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,       intent (in) ::
     .   RH  (:), ! relative humidity (%)
     .   SVP (:), ! air pressure (mb or hPa)
     .   P   (:)  ! ... and pressure (mb or hPa)
!
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   VP  (:)  ! vapor pressure (mb or hPa)
!
!     Note: The minimum size of the arrays RH, SVP and VP determines the
!           number of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     29Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of values are
!                            determined.
!
! EOP
!-------------------------------------------------------------------------

      real    :: f, EW, PP
      integer :: iHum, NHum, NP

      NP   = size ( P  )
      NHum = min  ( size ( RH ), size ( SVP ), size ( VP ))
      if ( NP .eq. 0 ) NHum = 0
      do iHum = 1, NHum
         f  = RH ( iHum ) / PctF
         EW = SVP ( iHum )
         PP = P  ( min ( iHum, NP ))
         VP ( iHum ) = f * EW * PP / ( PP - EW * ( 1.0 - f ))
      end do

      return
      end subroutine RHtoVP_wp

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: RH2VP () --- Relative humidity to vapor pressure.
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from relative
!     humidity to vapor pressure
!
! !INTERFACE:
      pure function RH2VP ( RH, SVP, P )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in) ::
     .   RH,   ! relative humidity (%)
     .   SVP   ! saturation vapor pressure (mb or hPa)
      real,    intent (in), optional ::
     .   P     ! air pressure (mb or hPa)
!
! !OUTPUT PARAMETERS:
      real ::
     .   RH2VP ! vapor pressure (mb or hPa)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     29Oct2001  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real :: VP (1)

!     Convert for ...
!     ---------------
      if ( present ( P ) ) then
         call RHtoVP_wp  ((/RH/), (/SVP/), (/P/), VP )
      else
         call RHtoVP_wnp ((/RH/), (/SVP/),        VP )
      end if
      RH2VP = VP (1)

      return
      end function RH2VP
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VPTtoRH_wnp () --- Vapor pressure and air temperature to relative humidity by neglecting air pressure
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from vapor pressure
!     and air temperature to relative humidity by neglecting air pressure
!     The routine determines the saturation vapor pressure (for pure water
!     vapor) from temperature before converting to relative humidity.!
!
! !INTERFACE:
      pure subroutine VPTtoRH_wnp ( VP, T, RH )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in) ::
     .   VP (:), ! vapor pressure (mb or hPa)
     .   T  (:)  ! air temperature (deg K)
!
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   RH (:)  ! relative humidity (%)
!
!     Note: The minimum size of the arrays determines the number
!           of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     28Oct2003  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real, dimension ( ScrSz ) :: SVP
      integer :: iHum, NHum, iSeg, NSeg, iSegBeg, iSegEnd, SegSz

      NHum = min ( size ( VP ), size ( T ), size ( RH ))

      NSeg    = ( NHum - 1 ) / ScrSz + 1
      iSegBeg =   1
      do iSeg = 1, NSeg
         iSegEnd = min ( iSegBeg + ScrSz - 1, NHum )
         SegSz   = iSegEnd - iSegBeg + 1

         call DPtoVP_pure ( T  ( iSegBeg : iSegEnd ), SVP )
         call VPtoRH_wnp  ( VP ( iSegBeg : iSegEnd ), SVP,
     .                      RH ( iSegBeg : iSegEnd ))
         iSegBeg = iSegEnd + 1
      end do

      return
      end subroutine VPTtoRH_wnp

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VPTtoRH_wp () --- Vapor pressure and air temperature to relative humidity without neglecting air pressure
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from vapor pressure
!     and air temperature to relative humidity without neglecting air
!     pressure.  The routine determines the saturation vapor pressure (for
!     pure water vapor) from temperature before converting to relative
!     humidity. 
!
! !INTERFACE:
      pure subroutine VPTtoRH_wp ( VP, T, P, RH )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in) ::
     .   VP (:), ! vapor pressure (mb or hPa)
     .   T  (:), ! temperature (deg K)
     .   P  (:)  ! ... and pressure (mb or hPa)
!
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   RH (:)  ! relative humidity (%)
!
!     Note: The minimum size of the arrays VP, T and RH determines the
!           number of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     28Oct2003  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real, dimension ( ScrSz ) :: SVP, PP
      integer :: iHum, NHum, NP, iSeg, NSeg,
     .           iSegHum, iSegBeg, iSegEnd, SegSz

      NP   = size ( P  )
      NHum = min  ( size ( VP ), size ( T ), size ( RH )) 
      if ( NP .eq. 0 ) NHum = 0

      NSeg    = ( NHum - 1 ) / ScrSz + 1
      iSegBeg =   1
      do iSeg = 1, NSeg
         iSegEnd = min ( iSegBeg + ScrSz - 1, NHum )
         SegSz   = iSegEnd - iSegBeg + 1

         call DPtoVP_pure ( T  ( iSegBeg : iSegEnd ), SVP )
         do iSegHum = 1, SegSz
            iHum           = iSegBeg + iSegHum - 1
            PP ( iSegHum ) = P ( min ( iHum, NP ))
         end do
         call VPtoRH_wp   ( VP ( iSegBeg : iSegEnd ), SVP, PP,
     .                      RH ( iSegBeg : iSegEnd ))
         iSegBeg = iSegEnd + 1
      end do
      return
      end subroutine VPTtoRH_wp
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VPT2RH () --- Vapor pressure and air temperature to relative humidity
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from vapor pressure 
!     and air temperature to relative humdity.  The routine determines the
!     saturation vapor pressure (for pure water vapor) from temperature
!     before converting to relative humidity
!
! !INTERFACE:
      pure function VPT2RH ( VP, T, P )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,     intent (in) ::
     .   VP,    ! vapor pressure (mb or hPa)
     .   T      ! air temperature (deg K)
      real,     intent (in), optional ::
     .   P      ! air pressure (mb or hPa)
!
! !OUTPUT PARAMETERS:
      real ::
     .   VPT2RH ! relative humidity (%)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     28Oct2003  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real :: RH (1)

!     Convert for ...
!     ---------------
      if ( present ( P )) then
         call VPTtoRH_wp  ((/VP/), (/T/), (/P/), RH )
      else
         call VPTtoRH_wnp ((/VP/), (/T/),        RH )
      end if
      VPT2RH = RH (1)

      return
      end function VPT2RH
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: RHTtoVP_wnp () --- Relative humidity and air temperature to vapor pressure by neglecting air pressure 
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from relative humidity
!     and air temperature to vapor pressure by neglecting air pressure.
!     The routine determines the saturation vapor pressure (for pure water
!     vapor) from temperature before converting to relative humidity
!
! !INTERFACE:
      pure subroutine RHTtoVP_wnp ( RH, T, VP )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,       intent (in) ::
     .   RH  (:), ! relative humidity (%)
     .   T   (:)  ! air temperature (deg K)
!
! !OUTPUT PARAMETERS:
      real,       intent (out) ::
     .   VP  (:)  ! vapor pressure (mb or hPa)
!
!     Note: The minimum size of the arrays determines the
!           number of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     28Oct2003  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real, dimension ( ScrSz ) :: SVP
      integer :: iHum, NHum, iSeg, NSeg, iSegBeg, iSegEnd, SegSz

      NHum    = min ( size ( RH ), size ( T ), size ( VP ))

      NSeg    = ( NHum - 1 ) / ScrSz + 1
      iSegBeg =   1
      do iSeg = 1, NSeg
         iSegEnd = min ( iSegBeg + ScrSz - 1, NHum )
         SegSz   = iSegEnd - iSegBeg + 1

         call DPtoVP_pure ( T  ( iSegBeg : iSegEnd ), SVP )
         call RHtoVP_wnp  ( RH ( iSegBeg : iSegEnd ), SVP,
     .                      VP ( iSegBeg : iSegEnd ))
         iSegBeg = iSegEnd + 1
      end do

      return
      end subroutine RHTtoVP_wnp
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: RHTtoVP_wp () --- Vapor pressure and air temperature to relative humidity without neglecting air pressure
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from relative humidity
!     and air temperature to vapor pressure without neglecting air pressure.
!     The routine determines the saturation vapor pressure (for pure water
!     vapor) from temperature before converting to temperature.
!
! !INTERFACE:
      pure subroutine RHTtoVP_wp ( RH, T, P, VP )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,       intent (in) ::
     .   RH  (:), ! relative humidity (%)
     .   T   (:), ! air temperature (deg K)
     .   P   (:)  ! ... and pressure (mb or hPa)
!
! !OUTPUT PARAMETERS:
      real,       intent (out) ::
     .   VP  (:)  ! vapor pressure (mb or hPa)
!
!     Note: The minimum size of the arrays RH, T and VP determines the
!           number of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     28Oct2003  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real, dimension ( ScrSz ) :: SVP, PP
      integer :: iHum, NHum, NP, iSeg, NSeg,
     .           iSegHum, iSegBeg, iSegEnd, SegSz

      NP   = size ( P  )
      NHum = min  ( size ( RH ), size ( T ), size ( VP ))
      if ( NP .eq. 0 ) NHum = 0

      NSeg    = ( NHum - 1 ) / ScrSz + 1
      iSegBeg =   1
      do iSeg = 1, NSeg
         iSegEnd = min ( iSegBeg + ScrSz - 1, NHum )
         SegSz   = iSegEnd - iSegBeg + 1

         call DPtoVP_pure ( T  ( iSegBeg : iSegEnd ), SVP )
         do iSegHum = 1, SegSz
            iHum           = iSegBeg + iSegHum - 1
            PP ( iSegHum ) = P  ( min ( iHum, NP ))
         end do
         call RHtoVP_wp ( RH ( iSegBeg : iSegEnd ), SVP, PP,
     .                    VP ( iSegBeg : iSegEnd )) 
         iSegBeg = iSegEnd + 1
      end do

      return
      end subroutine RHTtoVP_wp
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: RHT2VP () --- Relative humidity and air temperature to vapor pressure.
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from relative humidity
!     and air temperature to vapor pressure.  The routine determines the
!     saturation vapor pressure (for pure water vapor) from temperature
!     before converting to temperature.
!
! !INTERFACE:
      pure function RHT2VP ( RH, T, P )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,     intent (in) ::
     .   RH,    ! relative humidity (%)
     .   T      ! air temperature (deg K)
      real,     intent (in), optional ::
     .   P      ! air pressure (mb or hPa)
!
! !OUTPUT PARAMETERS:
      real ::
     .   RHT2VP ! vapor pressure (mb or hPa)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     28Oct2003  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real :: VP (1)

!     Convert for ...
!     ---------------
      if ( present ( P ) ) then
         call RHTtoVP_wp  ((/RH/), (/T/), (/P/), VP )
      else
         call RHTtoVP_wnp ((/RH/), (/T/),        VP )
      end if
      RHT2VP = VP (1)

      return
      end function RHT2VP
!......................................................................

      end module m_humidity
!====================================================================
