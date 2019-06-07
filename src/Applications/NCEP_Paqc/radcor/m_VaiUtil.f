!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOI
! !MODULE: m_VaiUtil -- Factory derived utilities for handling data from Vaisala rawinsondes
!
! !DESCRIPTION:
!     This module contains some routines for handling data from Vaisala
!     rawinsondes.  The utilities are based on algorithms implemented by
!     the factory as described in Vaisala, 1983 with more recent versions
!     of the lookup tables.  The computed temperature ($T_Cor$) and
!     height ($Z_Cor$) corrections are given so $T_2 = T_1 - T_Cor$ and
!     $Z_2 = Z_1 - Z_Cor$ where $T_1$ and $T_2$ are the temperatures
!     before and after the corrections and $Z_1$ and $Z_2$ are similarly
!     defined for heights.
!
! !REFERENCES:
!    \begin{description}
!    \item Vaisala, 1983: PTU Process in Microcora (internal document). 
!          Vaisala, Helsinki, Finland, 22~pp.
!    \end{description}
!
! !INTERFACE:
      module      m_VaiUtil
      implicit    NONE
      private	! except

c      public :: RS80Reg  ! Vaisala RS80 regression model
      public ::
     .  ! VaiCor,          ! Vaisala correction scheme
     .  ! VentF,           ! Ventilation factor for the sensor
     .  ! VaiTab,          ! Factory temperature bias correction tables
     .  ! SunAlt,          ! Vaisala's formulae to calculate the sun's
     .                    !   elevation angle.
     .   RHTtoDP,         ! Converts relative humidity/air temperature
     .                    !   to dew point and
     .   DPTtoRH          ! ... dew point/air temperature to relative
     .                    !   humidity using Vaisala's formulae.
      public ::           ! Integer codes denoting ...
     .   RS80,            ! ... the RS80 model
     .   RS80_86,         ! ... the 1986 version
     .   RS80_93,         ! ... the 1993 version
     .   RS80_57H,        ! ... the 1993 version (for US rawinsondes)
     .   RS80_57H_bug,    !    ... with the bug in the elapsed
     .   RS90             ! ... the RS90 model
      public ::           ! List of ...
     .   Vai_rkxList,     ! ... WMO codes of Vaisala raob instrument types
     .   rkx_RS80_57H,    ! Instrument code for Vaisala R80_57H (US NWS)
     .   rkx_RS80_Marwin  ! ... Vaisala RS80/DigiCora or Marwins (Canada)
      !interface VentF
      !   module procedure
     .!      VentF_        ! Ventilation factor for the entire profile
     .!!      VentF_,       ! Ventilation factor for the entire profile
     .!!      VentF_PTK     ! ... for one section at a time
      !end interface
      !interface SunAlt
      !   module procedure
     .!      SunAlt_SYMDH, ! Input time arguments with respect to launch
     .!      SunAlt_NYMD,  ! ... in absolute time in calendar format
     .!      SunAlt_SJHr   ! ... and in minutes since synoptic Julian hour
      !end interface
!
! !REVISION HISTORY:
!     22Apr2002  C. Redder  Original code
!     06Feb2003  C. Redder  Added the integer codes, RS80_57H and
!                           RS80_57H_bug.
!     19Mar2003  C. Redder  Removed the integer code RS80_57H_bug.
!     23Sep2003  C. Redder  Added the integer code RS80_57H_bug back.
!                           Made the public entity a generic interface.
!     20Nov2006  C. Redder  Added the public routines, RHTtoDH and
!                           DPT_RH.and the public constants, Vai_rkxList,
!                           rkx_RS80_57H and rkx_RS80_Marwin.
!     02May2007  C. Redder  Removed all code with proprietary info
!EOI
!-----------------------------------------------------------------

      integer, parameter ::
     .   RS80         = 1,
     .   RS80_86      = 2,
     .   RS80_93      = 3,
     .   RS80_57H     = 4,
     .   RS80_57H_bug = 5,
     .   RS90         = 6 

      real,    parameter ::
     .   BugFact_57H  = 5.0 / 3.0

      real,    parameter ::
     .   a_Claus      =  150.0,
     .   b_Claus      = 2711.5,
     .   T0_Claus     =  273.16

!     WMO instrument type code for ...
!     --------------------------------
      integer, parameter ::
     .   rkx_RS80_57H     = 52, ! ... NWS RS80-57H 
     .   rkx_RS80_Marwin  = 61

!     List of WMO instrument code for Vaisala rawinsondes
!     ---------------------------------------------------
      integer, parameter ::
     .   Vai_rkxListSz = 10
      integer, parameter ::
     .   Vai_rkxList ( Vai_rkxListSz )
     ,                 = (/ 37, 52, 60, 61, 62, 63, 71, 72, 73, 74 /)
      contains

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: RHTtoDP () --- Converts relative humidity and air temperature to dew point using Vaisala's formulae.
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from relative humidity
!     and air temperature to dew point using Vaisala's formulae.  Note:
!     code in this version has been replaced to protect proprietary info
!     The dew point is set to the air temperature (i.e RH is assumed to
!     be 100 percent)
!
! !INTERFACE:
      pure subroutine RHTtoDP ( RH, T, DP )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,       intent (in) ::
     .   RH  (:), ! relative humidity (%)
     .   T   (:)  ! air temperature (deg K)
!
! !OUTPUT PARAMETERS:
      real,       intent (out) ::
     .   DP  (:)  ! dew point (deg K)
!
!     Note: The minimum size of the arrays determines the
!           number of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     09Nov2006  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      integer :: iHum, NHum
      real    :: DPi, Ti, RHi, K, Tdel, x, logrh

      NHum    = min ( size ( RH ), size ( T ), size ( DP ))

      do iHum  = 1, NHum
         RHi   = RH ( iHum ) / 100.0
         Ti    = T  ( iHum )
         DPi   = Ti
         DP ( iHum ) = DPi
      end do

      return
      end subroutine RHTtoDP
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: DPTtoRH () --- Converts dew point and air temperature to relative humidity using Vaisala's formulae.
!
! !DESCRIPTION:
!     This routine converts a set of humidity values from dew point
!     and air temperature to relative humidity using Vaisala's formulae
!     Note: code in this version has been replaced to protect
!     proprietary info.  The return RH is set 100 percent
!
! !INTERFACE:
      pure subroutine DPTtoRH ( DP, T, RH )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,       intent (in) ::
     .   DP  (:), ! dew point (deg K)
     .   T   (:)  ! air temperature (deg K)
!
! !OUTPUT PARAMETERS:
      real,       intent (out) ::
     .   RH  (:)  ! relative humidity (%)
!
!     Note: The minimum size of the arrays determines the
!           number of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     09Nov2006  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      integer :: iHum, NHum
      real    :: DPi, Ti, RHi, Tdel, x, RHS

      NHum    = min ( size ( DP ), size ( T ), size ( RH ))

      do iHum = 1, NHum
         DPi  = DP ( iHum )
         Ti   = T  ( iHum )
         RHi  = 100.0
         RH ( iHum ) = RHi 

      end do

      return
      end subroutine DPTtoRH
!...................................................................
      end module m_VaiUtil
!====================================================================

