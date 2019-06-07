!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOI
! !MODULE: m_temperature -- Temperature conversion routines
!
! !DESCRIPTION:
!     This module contains routines that convert temperature values. 
!
! !REFERENCES:
!     Rogers, R. R., 1979: A Short Course in Cloud Physics, 2nd. Ed., 
!          Pergamon Press, Oxford. 235pp.
!
! !INTERFACE:
!
      module      m_temperature
      implicit    NONE
      private	! except

      public ::           ! Routines to convert from air temp (AT) to ...
     .   ATtoPT, AT2PT,   ! ... potential temperature (PT)
     .   ATtoVT, AT2VT    ! ... virtual temperature (VT)

      public ::           ! Routines to convert from ...
     .   PTtoAT, PT2AT,   ! ... potential temperature (PT)
     .   VTtoAT, VT2AT    ! ... virtual temperature (VT)
      public ::           ! Equation code for determining the VT
     .   VT_Exact,        ! ... using the exact equations
     .   VT_NCEP          ! ... NCEPS's approximation
!
      interface ATtoVT
         module procedure ! Convert from air temperature and ..
     .      ATtoVT_vp,    ! ... vapor pressure or
     .      ATtoVT_mr     ! ... mixing ratio to virtual temperature
      end interface

      interface AT2VT
         module procedure ! Convert from air temperature and ..
     .      AT2VT_vp,     ! ... vapor pressure or
     .      AT2VT_mr      ! ... mixing ratio to virtual temperature
      end interface
!
      interface VTtoAT
         module procedure ! Convert from virtual temperature and ..
     .      VTtoAT_vp,    ! ... vapor pressure or
     .      VTtoAT_mr     ! ... mixing ratio to air temperature
      end interface
!
      interface VT2AT
         module procedure ! Convert from virtual temperature and ..
     .      VT2AT_vp,     ! ... vapor pressure or
     .      VT2AT_mr      ! ... mixing ratio to air temperature
      end interface
!
!     Note: Routine names of the form xxtoxx and xx2xx implies a
!           subroutine and function, respectively.
!
! !REVISION HISTORY:
!     23Jul2002  C. Redder  Original code
!EOI
!-----------------------------------------------------------------
      real, parameter ::
     .   Ra     =  287.0,   ! The individual gas constant for dry air and ...
     .   Rv     =  461.0,   ! ... water vapor (j/(kg-deg K, from Rogers,
     .   RaRv   =  Ra / Rv  !   1979)
      real, parameter ::
     .   Cp     =  1004.0,  ! Specific heat at constant pressure
     .   Cv     =   717.0,  ! ... at constant volume for dry air
     .                      ! (j/(kg-deg K, from Rogers, 1979)
     .   k      =  ( Cp - Cv ) / Cp,
     .   PS_Def =  1000.0   ! Default value for the final pressure for
                            !   the potential temperature (mb).

      integer, parameter ::
     .   VT_Exact =  0,       ! Exact equation
     .   VT_NCEP  =  1        ! ... NCEP's approximation

      contains
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: ATtoPT () --- Air temperature to potential temperature
!
! !DESCRIPTION:
!     This routine converts a set of temperature values from air temperature
!     to potential temperature
!
! !INTERFACE:
      pure subroutine ATtoPT ( AT, P, PT, PS )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in)  ::
     .   AT (:), ! air temperature (deg K)
     .   P  (:)  ! air pressure (mb or hPa)
      real,      intent (in), optional ::
     .   PS      ! reference pressure (mb or hPa).  Default: PS = 1000.0 mb
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   PT (:)  ! potential temperature (deg K)
!
!     Note: The minimum size of the arrays AT and PT determines the number
!           of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     23Jul2002  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real    ::  T, PP, PSS
      integer :: iT, NT, NP

      PSS  = PS_Def
      if ( present ( PS )) PSS = PS

      NP   = size ( P )
      NT   = min  ( size ( AT ), size ( PT ))
      if ( NP .eq. 0 ) NT = 0
      do iT = 1, NT
         T  = AT ( iT )
         PP = PSS / P  ( min ( iT, NP ))
         PT ( iT ) = T * PP ** k
      end do

      return
      end subroutine ATtoPT

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: PTtoAT () --- Potential temperature to air temperature
!
! !DESCRIPTION:
!     This routine converts a set of temperature values from potential
!     temperature to air temperature
!
! !INTERFACE:
      pure subroutine PTtoAT ( PT, P, AT, PS )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in)  ::
     .   PT (:), ! potential temperature (deg K)
     .   P  (:)  ! air pressure (mb or hPa)
      real,      intent (in), optional ::
     .   PS      ! reference pressure (mb or hPa).  Default: PS = 1000.0 mb
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   AT (:)  ! air temperature (deg K)
!
!     Note: The minimum size of the arrays PT and AT determines the number
!           of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     23Jul2002  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real    ::  T, PP, PSS
      integer :: iT, NT, NP

      PSS  = PS_Def
      if ( present ( PS )) PSS = PS

      NP   = size ( P )
      NT   = min  ( size ( AT ), size ( PT ))
      if ( NP .eq. 0 ) NT = 0
      do iT = 1, NT
         T  = PT ( iT )
         PP = PSS / P  ( min ( iT, NP ))
         AT ( iT ) = T * PP ** (-k)
      end do

      return
      end subroutine PTtoAT

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: AT2PT () --- Air temperature to potential temperature
!
! !DESCRIPTION:
!     This routine converts a set of temperature values from air
!     temperature to potential temperature
!
! !INTERFACE:
      pure function AT2PT ( AT, P, PS )
      implicit NONE
!
! !INPUT PARAMETERS:
      real, intent (in)           :: AT    ! air temperature (deg K)
      real, intent (in)           :: P     ! air pressure (mb or hPa)
      real, intent (in), optional :: PS    ! reference pressure (mb or hPa).
                                           !   Default: PS = 1000.0 mb
! !OUTPUT PARAMETERS:
      real                        :: AT2PT ! potential temperature (deg K)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     23Jul2002  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real :: PT (1)

      call ATtoPT ( (/AT/), (/P/), PT, PS )
      AT2PT = PT (1)

      return
      end function AT2PT

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: PT2AT () --- Potential temperature to air temperature
!
! !DESCRIPTION:
!     This routine converts a set of temperature values from potential
!     temperature to air temperature
!
! !INTERFACE:
      pure function PT2AT ( PT, P, PS )
      implicit NONE
!
! !INPUT PARAMETERS:
      real, intent (in)           :: PT    ! potential temperature (deg K)
      real, intent (in)           :: P     ! air pressure (mb or hPa)
      real, intent (in), optional :: PS    ! reference pressure (mb or hPa).
                                           !   Default: PS = 1000.0 mb
! !OUTPUT PARAMETERS:
      real                        :: PT2AT ! air temperature (deg K)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     23Jul2002  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real :: AT (1)

      call PTtoAT ( (/PT/), (/P/), AT, PS )
      PT2AT = AT (1)

      return
      end function PT2AT
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: ATtoVT_vp () --- Air temperature and vapor pressure to virtual temperature
!
! !DESCRIPTION:
!     This routine converts a set of temperature values from air temperature
!     and vapor pressure to virtual temperature.
!
! !INTERFACE:
      pure subroutine ATtoVT_vp ( AT, P, VP, VT, Eqtn )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in)  ::
     .   AT (:), ! air temperature (deg K)
     .   P  (:), ! air pressure   (mb or hPa)
     .   VP (:)  ! vapor pressure (mb or hPa)
      integer,   intent (in), optional ::
     .   Eqtn    ! equation code (default: 0)
!
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   VT (:)  ! virtual temperature (deg K)
!
!     Note: The minimum size of the arrays AT, VP and VT determines the
!           number of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     23Jul2002  C. Redder   Original code.
!     20Apr2004  C. Redder   Added the optional argument, Eqtn.
!
! EOP
!-------------------------------------------------------------------------

      real    ::  T, W,  PP
      integer :: iT, NT, NP, Code_

      Code_ = VT_Exact
      if ( present ( Eqtn )) Code_ = Eqtn

      NP   = size ( P )
      NT   = min  ( size ( AT ), size ( VP ), size ( VT ))
      if ( NP .eq. 0 ) NT = 0

      if ( Code_ .eq. VT_NCEP ) then
         do iT = 1, NT
            T  = AT ( iT )
            PP = P  ( min ( iT, NP ))
            W  = RaRv * VP ( iT ) / ( PP - VP ( iT ))
            VT ( iT ) = T * ( 1.0 + 0.61 * W )
         end do

      else
         do iT = 1, NT
            T  = AT ( iT )
            PP = P  ( min ( iT, NP ))
            W  = RaRv * VP ( iT ) / ( PP - VP ( iT ))
            VT ( iT ) = T * ( 1.0 + W / RaRv )
     .                    / ( 1.0 + W )
         end do

      end if

      return
      end subroutine ATtoVT_vp
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: ATtoVT_mr () --- Air temperature and mixing ratio to virtual temperature
!
! !DESCRIPTION:
!     This routine converts a set of temperature values from air temperature
!     and mixing ratio to virtual temperature
!
! !INTERFACE:
      pure subroutine ATtoVT_mr ( AT, MR, VT, Eqtn )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in)  ::
     .   AT (:), ! air temperature (deg K)
     .   MR (:)  ! mixing ratio (g/kg)
      integer,   intent (in), optional ::
     .   Eqtn    ! equation code (default: 0)
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   VT (:)  ! virtual temperature (deg K)
!
!     Note: The minimum size of the arrays AT, MR and VT determines the
!           number of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     23Jul2002  C. Redder   Original code.
!     20Apr2004  C. Redder   Added the optional argument, Eqtn.
!
! EOP
!-------------------------------------------------------------------------

      real    ::  T, MRR
      integer :: iT, NT, Code_

      Code_ = VT_Exact
      if ( present ( Eqtn )) Code_ = Eqtn

      NT = min  ( size ( AT ), size ( MR ), size ( VT ))

      if ( Code_ .eq. VT_NCEP ) then
         do iT = 1, NT
            T   = AT ( iT )
            MRR = 0.001 * MR ( iT )
            VT ( iT ) = T * ( 1.0 + 0.61 * MRR )
         end do

      else
         do iT = 1, NT
            T   = AT ( iT )
            MRR = 0.001 * MR ( iT )
            VT ( iT ) = T * ( 1.0 + MRR / RaRv )
     .                    / ( 1.0 + MRR )
         end do

      end if

      return
      end subroutine ATtoVT_mr
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VTtoAT_vp () --- Virtual temperature and vapor pressure to airtemperature
!
! !DESCRIPTION:
!     This routine converts a set of temperature values from virtual
!     temperature and vapor pressure to air temperature.
!
! !INTERFACE:
      pure subroutine VTtoAT_vp ( VT, P, VP, AT, Eqtn )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in)  ::
     .   VT (:), ! virtual temperature (deg K)
     .   P  (:), ! air pressure   (mb or hPa)
     .   VP (:)  ! vapor pressure (mb or hPa)
      integer,   intent (in), optional ::
     .   Eqtn    ! equation code (default: 0)
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   AT (:)  ! air temperature (deg K)
!
!     Note: The minimum size of the arrays VT, VP and AT determines the
!           number of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     23Jul2002  C. Redder   Original code.
!     20Apr2004  C. Redder   Added the optional argument, Eqtn.
!
! EOP
!-------------------------------------------------------------------------

      real    ::  T, W,  PP
      integer :: iT, NT, NP, Code_

      Code_ = VT_Exact
      if ( present ( Eqtn )) Code_ = Eqtn

      NP   = size ( P )
      NT   = min  ( size ( VT ), size ( VP ), size ( AT ))
      if ( NP .eq. 0 ) NT = 0

      if ( Code_ .eq. VT_NCEP ) then
         do iT = 1, NT
            T  = VT ( iT )
            PP = P  ( min ( iT, NP ))
            W  = RaRv * VP ( iT ) / ( PP - VP ( iT ))
            AT ( iT ) = T / ( 1.0 + 0.61 * W )

         end do

      else
         do iT = 1, NT
            T  = VT ( iT )
            PP = P  ( min ( iT, NP ))
            W  = RaRv * VP ( iT ) / ( PP - VP ( iT ))
            AT ( iT ) = T * ( 1.0 + W )
     .                    / ( 1.0 + W / RaRv )
         end do

      end if

      return
      end subroutine VTtoAT_vp
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VTtoAT_mr () --- Virtual temperature and mixing ratio to air temperature
!
! !DESCRIPTION:
!     This routine converts a set of temperature values from virtural
!     temperature and mixing ratio to air temperature
!
! !INTERFACE:
      pure subroutine VTtoAT_mr ( VT, MR, AT, Eqtn )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,      intent (in)  ::
     .   VT (:), ! virtual temperature (deg K)
     .   MR (:)  ! mixing ratio (g/kg)
      integer,   intent (in), optional ::
     .   Eqtn    ! equation code (default: 0)
! !OUTPUT PARAMETERS:
      real,      intent (out) ::
     .   AT (:)  ! air temperature (deg K)
!
!     Note: The minimum size of the arrays AT, MR and VT determines the
!           number of values to convert.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     23Jul2002  C. Redder   Original code.
!     20Apr2004  C. Redder   Added the optional argument, Eqtn.
!
! EOP
!-------------------------------------------------------------------------

      real    ::  T, MRR
      integer :: iT, NT, Code_

      Code_ = VT_Exact
      if ( present ( Eqtn )) Code_ = Eqtn

      NT   = min  ( size ( VT ), size ( MR ), size ( AT ))

      if ( Code_ .eq. VT_NCEP ) then
         do iT = 1, NT
            T   = VT ( iT )
            MRR = 0.001 * MR ( iT )
            AT ( iT ) = T / ( 1.0 + 0.61 * MRR )
         end do
      else
         do iT = 1, NT
            T   = VT ( iT )
            MRR = 0.001 * MR ( iT )
            AT ( iT ) = T * ( 1.0 + MRR )
     .                    / ( 1.0 + MRR / RaRv )
         end do

      end if

      return
      end subroutine VTtoAT_mr
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: AT2VT_vp () --- Air temperature and vapor pressure to virtual temperature
!
! !DESCRIPTION:
!     This routine converts a set of temperature values from air
!     temperature and vapor pressure to virtual temperature
!
! !INTERFACE:
      pure function AT2VT_vp ( AT, P, VP, Eqtn )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in) :: AT       ! air temperature (deg K)
      real,    intent (in) :: P        ! air pressure (mb or hPa)
      real,    intent (in) :: VP       ! vapor pressure (mb or hPa)
      integer, intent (in),   optional ::
     .                        Eqtn     ! equation code (default: 0)
!
! !OUTPUT PARAMETERS:
      real                 :: AT2VT_vp ! virtual temperature (deg K)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     23Jul2002  C. Redder   Original code.
!     20Apr2004  C. Redder   Added the optional argument, Eqtn.
!
! EOP
!-------------------------------------------------------------------------

      real :: VT (1)

      call ATtoVT_vp ( (/AT/), (/P/), (/VP/), VT, Eqtn )
      AT2VT_vp = VT (1)

      return
      end function AT2VT_vp

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: AT2VT_mr () --- Air temperature and mixing ratio to virtual temperature
!
! !DESCRIPTION:
!     This routine converts a set of temperature values from air
!     temperature and mixing ratio to virtual temperature
!
! !INTERFACE:
      pure function AT2VT_mr ( AT, MR, Eqtn )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in) :: AT       ! air temperature (deg K)
      real,    intent (in) :: MR       ! vapor pressure  (mb or hPa)
      integer, intent (in),   optional ::
     .                        Eqtn     ! equation code (default: 0)
!
! !OUTPUT PARAMETERS:
      real                 :: AT2VT_mr ! virtual temperature (deg K)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     23Jul2002  C. Redder   Original code.
!     20Apr2004  C. Redder   Added the optional argument, Eqtn.
!
! EOP
!-------------------------------------------------------------------------

      real :: VT (1)

      call ATtoVT_mr ( (/AT/), (/MR/), VT, Eqtn )
      AT2VT_mr = VT (1)

      return
      end function AT2VT_mr

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VT2AT_vp () --- Virtual temperature and vapor pressure to air temperature
!
! !DESCRIPTION:
!     This routine converts a set of temperature values from virtual
!     temperature and vapor pressure to air temperature
!
! !INTERFACE:
      pure function VT2AT_vp ( VT, P, VP, Eqtn )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in) :: VT       ! virtual temperature (deg K)
      real,    intent (in) :: P        ! air pressure (mb or hPa)
      real,    intent (in) :: VP       ! vapor pressure (mb or hPa)
      integer, intent (in),   optional ::
     .                        Eqtn     ! equation code (default: 0)
!
! !OUTPUT PARAMETERS:
      real                 :: VT2AT_vp ! air temperature (deg K)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     23Jul2002  C. Redder   Original code.
!     20Apr2004  C. Redder   Added the optional argument, Eqtn.
!
! EOP
!-------------------------------------------------------------------------

      real :: AT (1)

      call VTtoAT_vp ( (/VT/), (/P/), (/VP/), AT, Eqtn )
      VT2AT_vp = AT (1)

      return
      end function VT2AT_vp

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: VT2AT_mr () --- Virtual temperature and mixing ratio to air temperature
!
! !DESCRIPTION:
!     This routine converts a set of temperature values from virtual
!     temperature and mixing ratio to air temperature
!
! !INTERFACE:
      pure function VT2AT_mr ( VT, MR, Eqtn )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in) :: VT       ! virtual temperature (deg K)
      real,    intent (in) :: MR       ! vapor pressure  (mb or hPa)
      integer, intent (in),   optional ::
     .                        Eqtn     ! equation code (default: 0)
!
! !OUTPUT PARAMETERS:
      real                 :: VT2AT_mr ! air temperature (deg K)
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     23Jul2002  C. Redder   Original code.
!     20Apr2004  C. Redder   Added the optional argument, Eqtn.
!
! EOP
!-------------------------------------------------------------------------

      real :: AT (1)

      call VTtoAT_mr ( (/VT/), (/MR/), AT, Eqtn )
      VT2AT_mr = AT (1)

      return
      end function VT2AT_mr
!......................................................................
      end module m_temperature
!====================================================================
