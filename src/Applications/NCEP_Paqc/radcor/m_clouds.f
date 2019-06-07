!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOI
! !MODULE: m_clouds -- Routines for cloud analysis based on raob profiles
!
! !DESCRIPTION:
!     
!
! !REFERENCES:
!     Chernykh, I. V. and R. E. Eskridge, 1996: Determination of cloud 
!          amount and level from radiosonde data.  Journal of Applied 
!          Meteorology, 35, 1362-1369.
!
!     Rogers, R. R., 1979: A Short Course in Cloud Physics, 2nd. Ed., 
!          Pergamon Press, Oxford. 235pp.
!
! !INTERFACE:
!
      module        m_clouds
      implicit      NONE
      private	  ! except
      public ::
     .   SetClds, ! Set up cloud list the data structure, cloud_data. 
     .   CldList, ! Returns list of predicted cloud layers
     .   IceCld,  ! Returns parameters for a respresentative ice ... 
     .   WtrCld,  ! ... and water cloud layer from a list from routine CldList.
     .   AbyNReg, ! Returns the number of regions in the Arabey diagram
     .   AbyInqR, ! Inquire about a region in the Arabey diagram
     .   AbyReg   ! Returns the region number in the Arabey diagram

      public ::
     .   CldInit,       ! Initialize data structure
     .   CldClean,      ! Clean data structure
     .   CldNullify,    ! Nullify pointers in data structure
     .   cloud_data,    ! Data structure accessed by the routine SetClds
     .   cloud_input,   ! Input data for SetClds
     .   cloud_scratch, ! Scratch space for the generic interface, CldList
     .   cloud_list     ! Output cloud list from the routine SetClds 
c     .   Missing, ! = .true. if value is missing
c     .   AMiss    ! = 1.0e15 to denote missing value (DAO standard)
      interface CldInit  
         module procedure ! Initialize the structure for ...
     .      Init_Cloud,   ! ... all data
     .      Init_Input,   ! ... the input data
     .      Init_Scratch, ! ... the scratch space
     .      Init_List     ! ... the list of clouds
      end interface
      interface CldClean
         module procedure ! Clean up the structure for ...
     .      Clean_Cloud,  ! ... all data
     .      Clean_Input,  ! ... the input data
     .      Clean_Scratch,! ... the scratch space
     .      Clean_List    ! ... the list of clouds
      end interface
      interface CldNullify
         module procedure
     .      Nullify_Cloud,
     .      Nullify_Input,
     .      Nullify_Scratch,
     .      Nullify_List
      end interface
      interface CldList
         module procedure     ! Returns list of predicted cloud layer 
     .      CldList_TMaskScr, ! ... with scratch and input mask for temp
     .      CldList_TMask,    ! ... with no scratch but input mask for temp
     .      CldList_Scr,      ! ... with scratch but no input mask for temp
     .      CldList_          ! ... without scratch and input mask for temp
      end interface
!
! !REVISION HISTORY:
!     22Oct2001  C. Redder  Original code
!     17Apr2002  C. Redder  Incorporated the routine Missing from the
!                           file m_RadUtil.f (now renamed m_CSpline.f)
!     23Jul2002  C. Redder  Made the routine CldList a generic interface.
!                           Removed AMiss for public access.
!     01Dec2003  C. Redder  Added the module procedures, CldList_TMaskScr
!                           and CldList_Scr, to the generic interface,
!                           CldList.  Added the generic interface,
!                           CldInit, CldClean, and CldNullify
!EOI
!-----------------------------------------------------------------

      character (len=*), parameter:: MyModule = 'm_clouds'
      real, parameter ::
     .   T0        = 273.15,    ! Temperature at 0 deg C (deg K)
     .   dHCld_Min = 50.0,      ! Minimum cloud thickness
     .   TICld_Max = T0 - 12.0, ! Max temp assumed for ice clouds (deg K)
     .   AMiss     = 1.0e15     ! Denotes missing value (DAO standard)

!     Parameters defining the profile grid (in routine CldList)
!     ---------------------------------------------------------
      integer, parameter :: NGrPts = 153       ! Number of grid points
      real,    parameter :: dHGr   = 100.0     ! Grid resolution (m)

!     Parameters defining the Arabey diagram
!     --------------------------------------
      integer, parameter ::
     .   NAReg = 4,  ! Number of Arabey regions for cloud amount
     .   NTReg = 3   ! ... and temperature intervals
      real,    parameter ::                                     ! Cloud amount
     .   CldAmtMin ( NAReg ) = (/  80.0, 60.0, 20.0,  0.0 /),   ! mininum and
     .   CldAmtMax ( NAReg ) = (/ 100.0, 80.0, 60.0, 20.0 /),   ! ... maximum
     .   TRegMin ( NTReg ) = ! Minimum and ...    ! Cloud amount!  by region
     .      (/ -70.0,   -10.0,     0.0 /),!(deg C)! threshhold 
     .   !----------------------------------------!------------
     .   a ( NTReg, NAReg - 1 ) = Reshape ( (/    !  100%
     .          -0.1,    -0.02,    0.,            !   80%
     .          -0.1225, -0.045,   0.,            !   60%
     .          -0.15,   -0.09,    0.             !   20%
     .    /), (/ NTReg, NAReg - 1 /) ),           !    0%
     .   ! ----------------------------------------------------
     .   b ( NTReg, NAReg - 1 ) = Reshape ( (/    !  100%
     .           0.0,     0.8,     0.8,           !   80%
     .           1.225,   2.0,     2.0,           !   60%
     .           2.3,     2.9,     2.9            !   20%
     .    /), (/ NTReg, NAReg - 1 /) ),           !    0%
     .   ! ----------------------------------------------------
     .   TRegMax ( NTReg ) =               ! ... maximum regional temperature
     .   (/    -10.0,     0.0,    40.0 /), !(deg C)
     .   dHTresh = 500.0 ! Maximum thickness for thin incomplete saturation
                         !   layer (m)

!     Scratch space for routine listed in the generic interface, CldList
!     ------------------------------------------------------------------
      integer, parameter :: NLev_Def = 255
      type cloud_input
         integer :: 
     .      NVct,     ! Size of the vectors
     .      NLev      ! Number of levels (<= NVct)
         real,    dimension (:), pointer ::
     .      P,        ! Pressure (mb or hPa),
     .      H,        ! ... altitude (m),
     .      T,        ! ... temperature (deg K),
     .      RH        ! ... relative humidity (%)
         logical, dimension (:), pointer ::
     .      TMask,    ! Mask for temperature,
     .      RHMask    ! ... and relative humidity values.  Any value whose  
                      !   mask is set to .false. is ignored.
      end type
      type cloud_scratch
         integer :: 
     .      NVct,     ! Size of the vectors
     .      NLev      ! Number of levels (<= NVct)

         real, dimension (:), pointer ::
     .      HT, TT, TTpp, HH, DPD, DPDpp, RH, RHpp, u
      end type

      integer, parameter :: NLayers_Def = 255
      type cloud_list
         integer :: 
     .      NVct,     ! Size of the vectors
     .      NLayers   ! Number of cloud layers (<= NVct)
         real ::
     .      IceCldBH, ! Equivalent height of base (m)
     .      IceCldTH, ! ... and top of representative cloud (m)
     .      WtrAmt,   ! Cloud amount (i.e. coverrage, %),
     .      WtrCldBH, ! ... height of base (= WCTH - thickness, m)
     .      WtrCldTH  ! ... and top of representative cloud (m)
         integer, dimension (:), pointer ::
     .      iAby (:)  ! The region number in the Arabey cloud coverage
     .                !   diagram (where 1 corresponds to near overcast),
         real,    dimension (:), pointer ::
     .      CBH  (:), ! ... the cloud base height (m),
     .      CBT  (:), ! ... and temperature (deg K),
     .      CTH  (:), ! ... the cloud top  height (m),
     .      CTT  (:)  ! ... and temperature (deg K) for each cloud layer
      end type

      type cloud_data
         type ( cloud_input   ) :: Input
         type ( cloud_scratch ) :: Scratch 
         type ( cloud_list    ) :: List 
      end type

      contains

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: SetClds () --- Derives cloud information
!
! !DESCRIPTION:
!     This routine derives a list of clouds from the humidity and
!     temperature profiles.  From this list, representative cloud
!     information is obtained.
!
! !INTERFACE:
      subroutine SetClds ( CloudData )
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( cloud_data ), intent (inout) :: CloudData
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     02Dec2003  C. Redder   Original code.
! EOP
!-------------------------------------------------------------------------
      integer :: NLev, NL
      real    :: IceCldBH, IceCldTH, WtrAmt, WtrCldBH, WtrCldTH 
      integer, dimension (:), pointer :: iAby
      logical, dimension (:), pointer :: TMask, RHMask
      real,    dimension (:), pointer :: P, H, T, RH,
     .                                   CBH, CBT, CTH, CTT

      NLev   =  CloudData % Input % NLev
      P      => CloudData % Input % P      ( : NLev )
      H      => CloudData % Input % H      ( : NLev )
      T      => CloudData % Input % T      ( : NLev )
      TMask  => CloudData % Input % TMask  ( : NLev )
      RH     => CloudData % Input % RH     ( : NLev )
      RHMask => CloudData % Input % RHMask ( : NLev )
      iAby   => CloudData % List  % iAby
      CBH    => CloudData % List  % CBH
      CBT    => CloudData % List  % CBT
      CTH    => CloudData % List  % CTH
      CTT    => CloudData % List  % CTT

      call CldList_TMaskScr ( P, H, T, TMask, RH,  RHMask,
     .                        CloudData % Scratch,
     .                        NL, iAby, CBH, CTH, CBT, CTT )
      CloudData % List % NLayers = NL
      iAby   => CloudData % List  % iAby ( : NL )
      CBH    => CloudData % List  % CBH  ( : NL )
      CBT    => CloudData % List  % CBT  ( : NL )
      CTH    => CloudData % List  % CTH  ( : NL )
      CTT    => CloudData % List  % CTT  ( : NL )

      call IceCld ( iAby, CBH, CTH, CBT, CTT, IceCldBH, IceCldTH )

      CloudData % List  % IceCldBH = IceCldBH
      CloudData % List  % IceCldBH = IceCldTH

      call WtrCld ( iAby, CBH, CTH, CBT, CTT,
     .              WtrAmt, WtrCldBH, WtrCldTH )

      CloudData % List  % WtrAmt   = WtrAmt
      CloudData % List  % WtrCldBH = WtrCldBH
      CloudData % List  % WtrCldBH = WtrCldTH

      return
      end subroutine SetClds
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CldList_TMask () --- Returns list of cloudy layers with masks for temperature and vapor pressure
!
! !DESCRIPTION:
!     This routine is the same CldList_TMaskScr without the argument,
!     scratch.  See the prologue to the routine CldList_TMaskScr for
!     more information.
!
! !INTERFACE:
      subroutine CldList_TMask ( P,  H,    T,   TMask, RH,  RHMask,
     .                           NL, iAby, CBH, CTH,   CBT, CTT, rstat )
      use m_AdvError, only : WPErr, Err => ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in) ::
     .   P      (:), ! Pressure (mb or hPa),
     .   H      (:), ! ... altitude (m),
     .   T      (:), ! ... temperature (deg K),
     .   RH     (:)  ! ... relative humidity (%)
      logical, intent (in) ::
     .   TMask  (:), ! Mask for temperature,
     .   RHMask (:)  ! ... and relative humidity values.  Any value whose  
                     !   mask is set to .false. is ignored.
!
! !OUTPUT PARAMETERS:
      integer, intent (out) ::
     .   NL,         ! The number of cloud layers
     .   iAby   (:),   ! The region number in the Arabey cloud coverage
     .               !   diagram (where 1 corresponds to near overcast),
     .   rstat       ! Returned status code from allocation statement
      real,    intent (out) ::
     .   CBH    (:), ! ... the cloud base height (m),
     .   CBT    (:), ! ... and temperature (deg K),
     .   CTH    (:), ! ... the cloud top  height (m),
     .   CTT    (:)  ! ... and temperature (deg K) for each cloud layer
!
!     Note: Number of levels is determined by the minimum size among all
!           input array arguments (declared as type real).
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     01Dec2003  C. Redder   Original code.
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter::
     .   MyName = MyModule // '::CldList_TMask'
      integer :: NLev

      type ( cloud_scratch ) :: scratch 

      NLev  = min ( size ( H  ), size ( P  ),
     .              size ( T  ), size ( RH ))

      call Init_Scratch ( NLev, scratch, rstat )
      if ( rstat .ne. 0 ) then
         call WPErr ( MyName, Err ( 'Scratch_Init' ))
         return
      end if

      call CldList_TMaskScr ( P,  H,    T, TMask, RH,  RHMask, scratch,
     .                        NL, iAby, CBH, CTH, CBT, CTT )

      call Clean_Scratch ( scratch )

      return
      end subroutine CldList_TMask
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CldList_TMaskScr () --- Returns list of cloudy layers with masks for temperature and vapor pressure
!
! !DESCRIPTION:
!     For given set of pressure, temperature (with mask values) and vapor
!     pressure (with mask values ) observations sorted in increasing order
!     according to height, this routine returns a list of vertical layers
!     in which clouds are likely to exist and the predicted cloud amounts
!     in each layer.  The prediction algorithm is discussed in detail in
!     Chernykh and Eskridge (1996).  
!
! !INTERFACE:
      subroutine CldList_TMaskScr ( P, H, T, TMask, RH, RHMask, scratch,
     .                             NL, iAby, CBH,  CTH, CBT, CTT )
      use m_humidity, only : VP2DP,   RH2VP, SVP => DP2VP
      use m_CSpline,  only : CSpline, CSplY, CSplYPP
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in) ::
     .   P      (:), ! Pressure (mb or hPa),
     .   H      (:), ! ... altitude (m),
     .   T      (:), ! ... temperature (deg K),
     .   RH     (:)  ! ... relative humidity (%)
      logical, intent (in) ::
     .   TMask  (:), ! Mask for temperature,
     .   RHMask (:)  ! ... and relative humidity values.  Any value whose  
                     !   mask is set to .false. is ignored.
!
! !INPUT/OUTPUT PARAMETERS:
      type ( cloud_scratch ), intent (inout) ::
     .   scratch     ! Scratch space provided by the calling routine.
!
! !OUTPUT PARAMETERS:
      integer, intent (out) ::
     .   NL,         ! The number of cloud layers
     .   iAby   (:)    ! The region number in the Arabey cloud coverage
     .               !   diagram (where 1 corresponds to near overcast)
      real,    intent (out) ::
     .   CBH    (:), ! ... the cloud base height (m),
     .   CBT    (:), ! ... and temperature (deg K),
     .   CTH    (:), ! ... the cloud top  height (m),
     .   CTT    (:)  ! ... and temperature (deg K) for each cloud layer
!
!     Note: Number of levels is determined by the minimum size among all
!           input array arguments (except TMask).
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     22Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Changed method of determining the number 
!                            of observations from the size of the input
!                            arrays.
!     13Aug2002  C. Redder   Changed method of determining the size of
!                            internal scratch arrays in order to be
!                            consistent with the determination of the
!                            number of observations.  Added the input
!                            arguments TMask and RHMask which replaces
!                            the use of AMiss to mask out undesired 
!                            values.  Renamed routine CldList to
!                            CldList_TMask.  Change input argument from
!                            VP (vapor pressure) to RH (relative humidity)
!                            Added scratch space for call to routine
!                            CSpline.  Added required output argument,
!                            rstat.  Revised code to use allocation
!                            statement to create scratch space which was
!                            formally arrays of assumed size.  Switched
!                            the order of the input arguments P and H.
!     01Dec2003  C. Redder   Changed the name of the routine from
!                            CldList_TMask to CldList_TMaskScr.  Added
!                            input/output parameter, scratch.
! EOP
!-------------------------------------------------------------------------

      logical :: high_enough, valid_T, valid_RH
      integer :: iOb, NObs, NTObs, NHObs, iPt, kPt, kBeg, iBeg, iEnd
      integer :: NTMask, NRHMask
      real    :: HTBelow, HHBelow, H0, VP
      real,    dimension (:), pointer ::
     .   HT, TT, TTpp, HH, DPD, DPDpp, RH_, RHpp, u 
      integer, dimension ( NGrPts ) :: kaT, kaH
      real,    dimension ( NGrPts ) ::
     .   GrT, GrTpp, GrDPD, GrRH, GrRHpp
      real,    parameter ::                    ! Gridded heights values (m)
     .   GrH ( NGrPts ) = (/( ( iPt - 1  ) * dHGr, iPt = 1, NGrPts )/)

      NL      = 0                              ! No cloud layer yet found
      NTMask  = size (  TMask )
      NRHMask = size ( RHMask )


!     Allocate scratch space
!     ----------------------
      NObs  = min ( size ( H  ), size ( P  ),  ! Number of ob reports
     .              size ( T  ), size ( RH ), scratch % NVct )
      if ( NObs .le. 2 ) return                ! Not enough data

      HT    => scratch % HT
      TT    => scratch % TT
      TTpp  => scratch % TTpp
      HH    => scratch % HH
      DPD   => scratch % DPD
      DPDpp => scratch % DPDpp
      RH_   => scratch % RH
      RHpp  => scratch % RHpp
      u     => scratch % u
c      allocate ( HT ( NObs ), TT   ( NObs ), TTpp  ( NObs ),
c     .           HH ( NObs ), DPD  ( NObs ), DPDpp ( NObs ),
c     .           RH_( NObs ), RHpp ( NObs ), u     ( NObs ),
c     .           stat = istat )
c      if ( istat .ne. 0 ) then
c         rstat = istat
c         call WPErr ( MyName, Alloc ( istat, 'HT,TT,TTpp,HH,DPD,DPDpp,'
c     .                                    // 'RH_,RHpp,u', NObs ))
c         return
c      end if

!     For all observations ...
!     ------------------------
      NTObs  = 0
      NHObs  = 0
      HTBelow = H ( 1 ) - 1.1 * dHCld_Min
      HHBelow = H ( 1 ) - 1.1 * dHCld_Min
      do iOb = 1, NObs

!        ... check to determine if ...
!        -----------------------------
         valid_T  = .true.                     ! ... temperature and
         valid_RH = .true.                     ! ... rel humidity are valid
         if (  NTMask .gt. 0 ) Valid_T  =  TMask ( min ( iOb,  NTMask ))
         if ( NRHMask .gt. 0 ) Valid_RH = RHMask ( min ( iOb, NRHMask ))

!        ... select only the temperature reports whose ...
!        -------------------------------------------------
         high_enough     = H (iOb) - HTBelow .ge. dHCld_Min
         if ( high_enough .and.                ! ... heights are increasing
     .        valid_T ) then                   ! ... and temperatures
                                               !     are valid
            NTObs = NTObs + 1                  ! Number of selected obs
            HT   (NTObs) = H (iOb)
            TT   (NTObs) = T (iOb)
            HTBelow      = H (iOb)             ! Save height for check at
         end if                                !   next level

!        ... and only the humdity reports whose ...
!        ------------------------------------------
         high_enough     = H (iOb) - HHBelow .ge. dHCld_Min
         if ( high_enough .and.                ! ... heights are increasing
     .        valid_T     .and.                ! ... and temperatures
     .        valid_RH )  then                 ! ... and relative humidity
                                               !     are valid
            NHObs        = NHObs + 1           ! Number of selected obs
            HH  (NHObs)  = H  (iOb)
            RH_ (NHObs)  = RH (iOb)            ! Save relative humidity and
                                               ! ... convert rel humidity to
            VP           = RH2VP ( RH (iOb),   ! ... and vapor pressure
     .                       SVP ( T  (iOb)), P (iOb))
            DPD (NHObs)  = T (iOb)             ! ... and dew pt depression
     .                   - VP2DP ( VP )
            HHBelow      = H (iOb)             ! Save height for check at
         end if                                !   next level
      end do
      if ( NTObs .le. 2 .or.                   ! Not enough temperature or
     .     NHObs .le. 2 ) return               !   humidity reports selected

!     Assingn each grid point with the appropriate ...
!     ------------------------------------------------
      call Getka   ( HT (:NTObs), kaT )        ! ... height and ...
      call Getka   ( HH (:NHObs), kaH )        ! ... temperature observation

!     Generate a gridded profile via cubic spline interpolation of ...
!     ----------------------------------------------------------------
      call CSpline ( HT (:NTObs), TT, u, TTpp  )  ! ... temperature and its
      call CSplY   ( HT (:NTObs), TT,    TTpp,   kaT, GrH, GrT   ) ! 2nd deri-
      call CSplYPP ( HT (:NTObs),        TTpp,   kaT, GrH, GrTpp ) ! vative

      call CSpline ( HH (:NHObs), DPD, u, DPDpp)  ! ... dew point depression
      call CSplY   ( HH (:NHObs), DPD,    DPDpp, kaH, GrH, GrDPD )
c      call CSplY   ( HH (:NHObs), DPD,    DPDpp,     GrH, GrDPD )

      call CSpline ( HH (:NHObs), RH_, u, RHpp )  ! ... relative humidity and its
      call CSplY   ( HH (:NHObs), RH_,    RHpp,  kaH, GrH, GrRH  ) ! 2nd deri-
      call CSplYPP ( HH (:NHObs),         RHpp,  kaH, GrH, GrRHpp) ! vative
c      call CSplY   ( HH (:NHObs), RH_,    RHpp,       GrH, GrRH  )
c      call CSplYPP ( HH (:NHObs),         RHpp,       GrH, GrRHpp)

!     Select only the grids points that will have valid intepolated values
!     --------------------------------------------------------------------
      iBeg = 1      + count ( kaT == 0    .or. kaH == 0    )
      iEnd = NGrPts - count ( kaT == NObs .or. kaH == NObs )

!     ... as input for the cloud search routine.
!     ------------------------------------------
      call ScanPr ( GrH    ( iBeg : iEnd ),
     .              GrT    ( iBeg : iEnd ),
     .              GrTpp  ( iBeg : iEnd ),
     .              GrDPD  ( iBeg : iEnd ),
     .              GrRH   ( iBeg : iEnd ),
     .              GrRHpp ( iBeg : iEnd ),
     .              NL, iAby, CBH, CTH, CBT, CTT )

      return
      contains

!     Interval routine to match every grid point with the
!     appropriate layer between two observations.  Zero / NObs
!     is assigned for any grid point above / below the range
!     of  height reports where NObs is the number of reports
!     --------------------------------------------------------
      pure subroutine Getka ( H, ka )
      real,    intent (in)  :: H  (:)
      integer, intent (out) :: ka (:)

      integer :: NObs, iOb, iBeg, kPt, iPt

      iBeg = 1
      NObs = size ( H )
      do iOb = 1, NObs
         kPt  = min ( int ( H ( iOb ) / dHGr ) + 1, NGrPts )
         do iPt = iBeg, kPt
            ka ( iPt ) = iOb - 1
         end do
         iBeg = kPt + 1
      end do

      do iPt = iBeg, NGrPts
         ka ( iPt ) = NObs
      enddo

      return
      end subroutine Getka
      end subroutine CldList_TMaskScr

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CldList_ -- Returns list of cloudy layers with mask for vapor pressure
!
! !DESCRIPTION:
!     This routine is the same CldList_TMaskScr without the arguments,
!     TMask and scratch.  See the prologue to the routine
!     CldList_TMaskScr for more information.
!
! !INTERFACE:
      subroutine CldList_ ( P,   H,   T,   RH,  RHMask,
     .                      NL, iAby, CBH, CTH, CBT, CTT, rstat )
      use m_AdvError, only : WPErr, Err => ErrStat
!
! !INPUT PARAMETERS:
      implicit   NONE
      real,    intent (in)  ::
     .   P      (:), ! Pressure (mb or hPa),
     .   H      (:), ! ... altitude (m),
     .   T      (:), ! ... temperature (deg K),
     .   RH     (:)  ! ... relative humidity (%)
      logical, intent (in)  ::
     .   RHMask (:)  ! ... and relative humidity values.  Any value
                     !   whose mask is set to .false. is ignored.
!
! !OUTPUT PARAMETERS:
      integer, intent (out) ::
     .   NL,         ! The number of cloud layers
     .   iAby   (:),   ! The region number in the Arabey cloud coverage
     .               !   diagram (where 1 corresponds to near overcast),
     .   rstat       ! Returned status code from allocation statement
      real,    intent (out) ::
     .   CBH    (:), ! ... the cloud base height (m),
     .   CBT    (:), ! ... and temperature (deg K),
     .   CTH    (:), ! ... the cloud top  height (m),
     .   CTT    (:)  ! ... and temperature (deg K) for each cloud layer
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     12Aug2002  C. Redder  Origional code.
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter:: MyName = MyModule // '::CldList_'

      call CldList_TMask ( P,  H,    T, (/.true./), RH,  RHMask,
     .                     NL, iAby, CBH, CTH, CBT, CTT, rstat )
      if ( rstat .ne. 0 )
     .   call WPErr ( MyName, Err ( 'CldList_TMask' ))

      return
      end subroutine CldList_

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CldList_Scr () --- Returns list of cloudy layers with masks for temperature and vapor pressure
!
! !DESCRIPTION:
!     This routine is the same CldList_TMaskScr without the input argument,
!     TMask.  See the prologue to the routine CldList_TMaskScr for more
!     information.
!
! !INTERFACE:
      subroutine CldList_Scr ( P,  H, T, RH,  RHMask, scratch,
     .                         NL, iAby, CBH, CTH, CBT,   CTT )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent (in) ::
     .   P      (:), ! Pressure (mb or hPa),
     .   H      (:), ! ... altitude (m),
     .   T      (:), ! ... temperature (deg K),
     .   RH     (:)  ! ... relative humidity (%)
      logical, intent (in) ::
     .   RHMask (:)  ! ... and relative humidity values.  Any value
                     !   whose mask is set to .false. is ignored.
!
! !INPUT/OUTPUT PARAMETERS:
      type ( cloud_scratch ), intent (inout) ::
     .   scratch     ! Scratch space provided by the calling routine.
!
! !OUTPUT PARAMETERS:
      integer, intent (out) ::
     .   NL,         ! The number of cloud layers
     .   iAby   (:)    ! The region number in the Arabey cloud coverage
                     !   diagram (where 1 corresponds to near overcast)
      real,    intent (out) ::
     .   CBH    (:), ! ... the cloud base height (m),
     .   CBT    (:), ! ... and temperature (deg K),
     .   CTH    (:), ! ... the cloud top  height (m),
     .   CTT    (:)  ! ... and temperature (deg K) for each cloud layer
!
!     Note: Number of levels is determined by the minimum size among all
!           input array arguments (except TMask)
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     01Dec2003  C. Redder   Original code.
! EOP
!-------------------------------------------------------------------------

      call CldList_TMaskScr ( P, H,    T, (/.true./), RH,  RHMask,
     .                        scratch, NL, iAby, CBH, CTH, CBT, CTT )

c      allocate ( HT ( NObs ), TT   ( NObs ), TTpp  ( NObs ),
c     .           HH ( NObs ), DPD  ( NObs ), DPDpp ( NObs ),
c     .           RH_( NObs ), RHpp ( NObs ), u     ( NObs ),
c     .           stat = istat )

      return
      end subroutine CldList_Scr
!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: ScanPr () --- Scans profiles for clouds
!
! !DESCRIPTION:
!     For given temperature and humidity profiles, this routine scans
!     for cloudy layers and determines its amounts and levels using
!     the criteria described in Chernykh and Eskridge (1996).  The top
!     2 levels are skipped to avoid spurious cirrus clouds that may  
!     result from the the natural boundary condition in cubic spline
!     interpolation.
!
! !INTERFACE:
      pure subroutine ScanPr ( H,  T,    Tpp, DPD, RH,  RHpp,
     .                         NL, iAby, CBH, CTH, CBT, CTT )
      implicit NONE
!
! !INPUT PARAMETERS:
      real,    intent  (in) ::
     .   H    (:), ! Altitude (m),
     .   T    (:), ! ... temperature (deg K),
     .   Tpp  (:), ! ... its second derivative (deg K/m**2),
     .   DPD  (:), ! ... dew point (deg K),
     .   RH   (:), ! ... relative humidity (%)
     .   RHpp (:)  ! ... its second derivative (%/m**2)
!
! !OUTPUT PARAMETERS:
      integer, intent (out) ::
     .   NL,       ! The number of cloud layers
     .   iAby (:)  ! The region number in the Arabey cloud coverate diagram
                   !   (where 1 corresponds to near overcast),
      real,    intent (out) ::
     .   CBH  (:), ! ... the cloud base height (m),
     .   CBT  (:), ! ... and temperature (deg K),
     .   CTH  (:), ! ... the cloud top  height (m),
     .   CTT  (:)  ! ... and temperature (deg K) for each cloud layer
!
!     Note: The number of levels in the profiles is determined by the
!           size of the array, H.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     22Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Added code to prevent index errors for 
!                            output arrays. 
! EOP
!-------------------------------------------------------------------------

      logical :: clouds_exist, base_found, top_found
      integer :: NLev, iLev, iBase, iTop, iRHMax, NLMax
      real    :: RHMax, dHCld

!     Initialize ...
!     --------------
      NL          =  0              ! ... number of cloud layera found
      base_found  = .false.         ! ... flags for finding the base ...
      top_found   = .false.         ! ... and top of the cloud layer

!     ... maximum number of levels
!     ----------------------------
      NLMax       =  min ( size ( CBH ), size ( CBT ),
     .                     size ( CTH ), size ( CTT )) 

!     Set ...
!     -------
      NLev = size ( H )
      if ( NLev .eq. 0 ) return     ! ... and return if no levels exist
                                    !     in the profiles
!     For each level in the profile
!     -----------------------------
      do iLev = 1, NLev

!        ... determine if the clouds may exist in the profile
!        ----------------------------------------------------
         clouds_exist = Tpp  (iLev) .ge. 0.0  .and.
     .                  RHpp (iLev) .le. 0.0  

!        ... and if they may
!        -------------------
         if ( clouds_exist ) then
            if ( base_found ) then  ! ... and if the cloud base has already
               if ( RHMax .lt. RH (iLev) ) then        ! been found
                  iRHMax  = iLev    ! ... keep track of the level with the
                   RHMax  = RH (iLev) ! the maximum RH in the cloud layer
               end if
            else
               base_found = .true.  ! ... if base has not been found
               iBase      = iLev    ! ... then new cloud layer is assumed
               iRHMax     = iBase
               RHMax      = RH (iLev)
            endif
            if ( iLev .eq. NLev ) then
               top_found  = .true.  ! ... assume that the cloud top will
               iTop       = iLev    !     not exceed the top of the profile
            end if

!        If clouds definitely do not exist ...
!        -------------------------------------
         else
            if ( base_found ) then  ! ... and if the base has been found
               top_found = .true.   ! ... then the cloud top is assumed
               iTop      = iLev - 1 !     to be at the previous level
            end if
         end if

!        If the cloud top is found ...
!        -----------------------------
         if ( top_found ) then
            NL = NL + 1             ! ... add the cloud layer to the list
                                    ! ... and save its parameters
            dHCld      = max ( H (iTop) - H (iBase), dHCld_Min )
            if ( NL .le. NLMax ) then
               iAby (NL)  = AbyReg ( T (iRHMax), DPD (iRHMax), dHCld )
               CTH  (NL)  = H (iTop)
               CTT  (NL)  = T (iTop)
               CBH  (NL)  = min ( H (iBase), H (iTop) - dHCld_Min )  
               CBT  (NL)  = T (iBase)
            end if

            base_found = .false.    ! ... reset flags for next cloud layer
            top_found  = .false.
         end if
      end do

      return
      end subroutine ScanPr

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: IceCld () --- Returns equivalent cirrus cloud layer based on a cloud list
!
! !DESCRIPTION:
!     Based on a list of clouds given by the routine CldList, this routine
!     returns the parameters of a representative ice cloud layer assumed
!     to have a coverage of 100 percent.  The top of representative layer
!     is also assume to be the same as that for the top layer in the input
!     list.  The thickness of the representative layer is the sum of the
!     thicknesses of all listed layers each of which is weighted by the
!     cloud amount.  Cloud layers with nearly clear coverage as well as
!     those below the lowest layer with cloud top temperatures at and
!     below 261.15 deg K (-12 deg C) are ignored.
!
! !INTERFACE:
      pure subroutine IceCld ( iAby, CBH, CTH, CBT, CTT, ICBH, ICTH )
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,     intent (in)  ::
     .   iAby (:)  ! The region number in the Arabey cloud coverate diagram
                   !   (where 1 corresponds to near overcast),
      real,        intent (in)  ::
     .   CBH  (:), ! ... the cloud base height (m),
     .   CBT  (:), ! ... and temperature (deg K),
     .   CTH  (:), ! ... the cloud top  height (m),
     .   CTT  (:)  ! ... and temperature (deg K) for each cloud layer
!
! !OUTPUT PARAMETERS:
      real,        intent (out) ::
     .   ICBH,     ! Equivalent height of base (m)
     .   ICTH      ! ... and top of representative cloud (m)
!
!     Note: The number of cloud layers in the list is determined by the
!           minimum size of all the input arrays.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     30Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Changed method of determining the number 
!                            of cloud layers from the size of the input
!                            arrays.
! EOP
!-------------------------------------------------------------------------
      integer :: iL, NL, iBottom, iReg, NReg
      real    :: CThick, CldAmt

!     Locate base of zone of possible ice clouds
!     ------------------------------------------
      NL      = min ( size ( iAby ), size ( CBH ), size ( CBT ),
     .                               size ( CTH ), size ( CTT ))
      iBottom = NL + 1
      NReg    = AbyNReg ()
      do iL = 1, NL
         if ( CTT  (iL) .le. TICld_Max ) then
            iBottom = iL
            exit
         end if
      end do

!     Determine equivalent thickness
!     ------------------------------
      ICBH   = 0.0
      ICTH   = 0.0
      CThick = 0.0
      NReg   = AbyNReg ()
      do iL = iBottom, NL
         iReg = iAby (iL)
         if ( iReg .lt. NReg ) then       !Ignore nearly clear layers
            CldAmt = AbyInqR ( iAby (iL), 'CldAmtAve' )
            CThick = CThick + ( CTH (iL) - CBH ( iL )) * CldAmt / 100.0
            ICTH   =  CTH (iL)
            ICBH   = ICTH - CThick
            
         end if
      end do

      return
      end subroutine IceCld

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: WtrCld () --- Returns equivalent water cloud layer based on a cloud list
!
! !DESCRIPTION:
!     Based on a list of clouds given by the routine CldList, this routine
!     returns the parameters of a representative water cloud layer.  The
!     height of the representative cloud top is the average top among all
!     layers in the input list each of which is normally weighted by the
!     amount the layer is exposed to the sky.  The thickness of the
!     representative layer is the sum of the thicknesses of all listed
!     layers each of which is normally weighted by the cloud amount.  Cloud
!     layers with nearly clear coverage as well as those with thickness of
!     less than 500 meters are ignored.  In addition, layers above the
!     highest layer with cloud top temperature above 261.15 deg K (-12 deg
!     C) are also ignored. 
!
! !INTERFACE:
      pure subroutine WtrCld ( iAby,  CBH,  CTH, CBT, CTT,
     .                         WAmt, WCBH, WCTH )
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,     intent (in)  ::
     .   iAby (:)  ! The region number in the Arabey cloud coverate diagram
                   !   (where 1 corresponds to near overcast),
      real,        intent (in)  ::
     .   CBH  (:), ! ... the cloud base height (m),
     .   CBT  (:), ! ... and temperature (deg K),
     .   CTH  (:), ! ... the cloud top  height (m),
     .   CTT  (:)  ! ... and temperature (deg K) for each cloud layer
!
! !OUTPUT PARAMETERS:
      real,        intent (out) ::
     .   WAmt,     ! Cloud amount (i.e. coverrage, %),
     .   WCBH,     ! ... height of base (= WCTH - thickness, m )
     .   WCTH      ! ... and top of representative cloud (m)
!
!     Note: The number of cloud layers in the list is determined by the
!           minimum size of all the input arrays.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     30Oct2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Changed method of determining the number 
!                            of cloud layers from the size of the input
!                            arrays.
!     09Dec2003  C. Redder   Fixed index bug in last call to AbyInqR. 
! EOP
!-------------------------------------------------------------------------

      logical :: cloudy
      integer :: iL, NL, iTop, iReg, NReg
      real    :: WThick, AmtAdd, WSum, CldAmt, dH
      real, parameter :: CThickMin = 500.0 ! minimum cloud thicknes (m)

!     Locate the maximum possible top of the water cloud
!     --------------------------------------------------
      NL   = min ( size ( iAby ), size ( CBH ), size ( CBT ),
     .                            size ( CTH ), size ( CTT ))
      iTop = 0
      NReg = AbyNReg ()
      do iL = NL, 1, -1
         if ( CTT  (iL) .gt. TICld_Max ) then
            iTop = iL
            exit
         end if
      end do

!     Determine amount, top and thickness of representative cloud
!     ------------------------------------------------------------
      WAmt   = 0.0
      WThick = 0.0
      WCTH   = 0.0
      NReg   = AbyNReg ()
      do iL = iTop, 1, -1
         iReg   = iAby (iL)
         dH     = CTH (iL) - CBH ( iL )
         cloudy = iReg .lt. NReg .and. dH .ge. CThickMin
         if ( cloudy ) then
            CldAmt =   AbyInqR ( iAby (iL), 'CldAmtAve' ) / 100.0
            AmtAdd = ( 1.0 - WAmt ) * CldAmt      ! Amount of the layer
                                                  !   presented to the sky
                                                  ! Update weighted ...
            WCTH   =   WCTH   + CTH (iL) * AmtAdd ! ... mean cloud top height
            WThick =   WThick + dH       * CldAmt ! ... and sum of thickness
            WAmt   =   WAmt   + AmtAdd
         end if
      end do

!     Normalize the weighted ...
!     --------------------------
      if ( WAmt .gt. AbyInqR ( NReg, 'CldAmtAve' ) / 100.0 ) then
         WCTH   = WCTH   / WAmt                   ! ... mean cloud top height
         WThick = WThick / WAmt                   ! ... and sum of thicknesses

      end if
      WAmt = WAmt * 100.0                         ! Convert to percent
      WCBH = WCTH - WThick                        ! The height of the
                                                  !   cloud base
      return
      end subroutine WtrCld

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: AbyNReg () --- Return the number of regions in the Araby diagram
!
! !DESCRIPTION:
!     This routine returns the number of cloud amount regions in the Arabey diagram.
!
! !INTERFACE:
      pure function AbyNReg ( T )
      implicit NONE
!
! !INPUT PARAMETERS:
      logical, intent (in), optional ::
     .   T ! = .true. if the number of temperature intervals is to be 
           !   returned instead of the number of cloud amount regions.
           !   Default: T = .false. 
!
! !OUTPUT PARAMETERS: 
      integer :: AbyNReg ! Number of regions
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     22Oct2001  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      AbyNReg = NAReg
      if ( present ( T ) ) then
         if ( T ) AbyNReg = NTReg
      end if

      return      
      end function AbyNReg

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: AbyInqR () --- Inquire about a given region in the Arabey diagram
!
! !DESCRIPTION:
!     This routine returns the specified information for a given cloud
!     amount region or temperature interval in the Arabey diagram.
!
! !INTERFACE:
      pure function AbyInqR ( iReg, quant )
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,           intent (in) ::
     .   iReg   ! Region or temperature interval number.  Zero implies the
                !    maximum number.  If the number is invalid, then -1.0
                !    if returned
      character (len=*), intent (in) ::
     .   quant  ! Quantity to be returned.
                !   The valid choices are (case sensitive) ...
                !   'CldAmtAve' - cloud amount average  (%)
                !   'CldAmtMin' - ... mininum (%)
                !   'CldAmtMax' - ... maximum for the given region (%).
                !   'TAve'      - temperature average (deg K)
                !   'TMin'      - ... mininum (deg K)
                !   'TMax'      - ... maximum (deg K) for the given interval
                ! If the choice is invalid, then -1.0 is returned.
!
! !OUTPUT PARAMETERS:
      real :: AbyInqR ! Returned inAverage cloud cover (%)
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     22Oct2001  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      integer :: iiReg

      AbyInqR = -1.0  ! default

!     Get information abount the cloud amount region  ...
!     ---------------------------------------------------
      if ( iReg .ge. 0 .and.
     .     iReg .le. NAReg ) then
         iiReg = iReg
         if      ( iReg .eq. 0 ) iiReg = NAReg

         if      ( quant .eq. 'CldAmtAve' ) then
            AbyInqR = 0.5 * ( CldAmtMin (iiReg) + CldAmtMax (iiReg))
         else if ( quant .eq. 'CldAmtMin' ) then
            AbyInqR = CldAmtMin (iiReg)
         else if ( quant .eq. 'CldAmtMax' ) then
            AbyInqR = CldAmtMax (iiReg)
         end if
      end if

!     ... temperature interval in the Arabey diagram
!     ----------------------------------------------
      if ( iReg .ge. 0 .and.
     .     iReg .le. NTReg ) then
         iiReg = iReg
         if      ( iReg  .eq. 0 ) iiReg = NTReg

         if      ( quant .eq.  'TAve' ) then
            AbyInqR = 0.5 * ( TRegMin (iiReg) + TRegMax (iiReg)) + T0
         else if ( quant .eq.  'TMin' ) then
            AbyInqR = TRegMin (iiReg) + T0
         else if ( quant .eq.  'TMax' ) then
            AbyInqR = TRegMax (iiReg) + T0
         end if
      end if

      return
      end function AbyInqR

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: AbyReg () --- Returns the region number in the Arabey diagram.
!
! !DESCRIPTION:
!     This routine returns the region number in the Arabey cloud amount
!     diagram (see Chernykh and Eskridge, 1996) for given temperature and
!     dewpoint depression and cloud thickness.  Region 1 corresponds to
!     near overcast,
!
! !INTERFACE:
      pure function AbyReg ( T, DPD, dH )
      implicit NONE
!
! !INPUT PARAMETERS:
      real, intent (in) ::
     .   T,      ! Temperature           (deg K)
     .   DPD,    ! Dew point depresseion (deg C or K)
     .   dH      ! Cloud thickness       (m)
!
! !OUTPUT PARAMETERS:
      real              ::
     .   AbyReg ! Region number
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     22Oct2001  C. Redder   Original code, adapted from the routine, MOSH,
!                            in the file, Vizcorr.for, (version dated 12 Dec
!                            2000) provided by the National Climatic Data
!                            Center.
! EOP
!-------------------------------------------------------------------------

      real    :: X0, Y0, YM
      integer :: iTReg, iXReg, iAReg, iYReg

      X0 = T - T0 
      Y0 = DPD

!     Find appropriate temperature range
!     ----------------------------------
      iTReg = 1
      do iXReg = NTReg, 2, -1
         if ( X0 .gt. TRegMin ( iXReg ) ) then
            iTReg = iXReg
            exit ! loop since temperature range is found
         end if
      end do

!     ... and then the Arabey region
!     ------------------------------
      iAReg = NAReg
      do iYReg = 1, NAReg - 1
         YM = a ( iTReg, iYReg ) * X0 + b ( iTReg, iYReg )
         if ( Y0 .le. YM ) then
            iAReg = iYReg
            exit ! loop since Arabey region is found
         end if 
      end do

!     Special case for thin incomplete saturation layer
!     -------------------------------------------------
      if ( dH .lt. dHTresh ) then
         if ( iAReg .eq. 2 ) iAReg = 1
      end if

      AbyReg = iAReg

      return
      end function AbyReg

!......................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Missing () --- Check for missing value
!
! !DESCRIPTION:
!     This routine checks a value and returns true if set to AMiss
!
! !INTERFACE:
      pure function Missing ( Val, VMiss )
      implicit NONE
!
! !INPUT PARAMETERS:
      real, intent (in)           :: Val     ! Value to be check
      real, intent (in), optional :: VMiss   ! Denotes missing value.
!                                            !   Default: AMiss = 1.0**15
! !OUTPUT PARAMETERS:
      logical                     :: Missing ! = .true. if Val is set to
!                                            !   missing
! !SEE ALSO:
!
! !REVISION HISTORY:
!     22Oct2001  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real, parameter ::
     .   RError = 10.0 ** (-precision (1.0)),
     .   RTol   = AMiss * RError

      if ( .not. present ( VMiss ) ) then
         Missing = abs ( Val - AMiss ) .le. RTol
      else
         Missing = abs ( Val - VMiss ) .le. VMiss * RError 
      end if

      return
      end function Missing

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Init_Cloud --- Allocates necessary memory and initializes variables
! 
! !INTERFACE:
      subroutine Init_Cloud ( NLev, this, stat )
!
! !USES
      use m_AdvError, only : WPErr, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS: 
      integer,              intent (in)    ::
     .   NLev   ! Number of observations
!
! !INPUT/OUTPUT PARAMETERS:
      type ( cloud_data ),  intent (inout) ::
     .   this   ! Data structure
!
! !OUTPUT PARAMETER:
      integer,              intent   (out) ::
     .   stat   ! returned status code from allocation statement
!
! !DESCRIPTION:
!     This routine allocates memory for the data structure, cloud_data.
!     This routine also initializes the data structure parameters.
!
! !REVISION HISTORY: 
!     02Dec2003  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::Init_Cloud'
      integer :: istat

      stat = 0

      call Init_Input   ( NLev, this % Input,   stat = stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Init_Input'   ))
         return
      end if

      call Init_Scratch ( NLev, this % Scratch, stat = stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Init_Scratch' ))
         call Clean_Input   ( this % Input   )
         return
      end if

      call Init_List    ( NLev, this % List,    stat = stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Init_List'    ))
         call Clean_Input   ( this % Input   )
         call Clean_Scratch ( this % Scratch )
         return
      end if

!     All done
!     --------
      return

      end subroutine Init_Cloud

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Init_Input --- Allocates necessary memory and initializes variables
! 
! !INTERFACE:
      subroutine Init_Input ( NLev, this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Alloc
      implicit NONE
!
! !INPUT PARAMETERS: 
      integer,              intent (in)    ::
     .   NLev   ! Number of levels
!
! !INPUT/OUTPUT PARAMETERS:
      type ( cloud_input ), intent (inout) ::
     .   this   ! Data structure
!
! !OUTPUT PARAMETER:
      integer,              intent   (out) ::
     .   stat   ! returned status code from allocation statement
!
! !DESCRIPTION:
!     This routine allocates memory for the auxiliary data structure,
!     cloud_input.  This routine also initializes the data structure
!     parameters.
!
! !REVISION HISTORY: 
!     02Dec2003  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::Init_Input' 
      integer :: nvct, nnlev

!     Store parameters for data structure
!     -----------------------------------
      nnlev = NLev
      if ( nnlev .lt. 0 ) nnlev = NLev_Def
      nvct  = max ( 1, nnlev )     

!     ... store parameters for data structure
!     ---------------------------------------
      this % NVct = nvct
      this % NLev = 0

!     Allocate memory
!     ---------------
      allocate ( this % P       ( nvct ),
     .           this % H       ( nvct ),
     .           this % T       ( nvct ),
     .           this % TMask   ( nvct ),
     .           this % RH      ( nvct ),
     .           this % RHMask  ( nvct ),
     .           stat = stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName,
     .                Alloc ( stat, 'this%P,'
     .                           // 'this%H,'
     .                           // 'this%T,'
     .                           // 'this%TMask,'
     .                           // 'this%RH,'
     .                           // 'this%RHMask', nvct ))
         return
      end if

!     All done
!     --------
      return

      end subroutine Init_Input

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Init_Scratch --- Allocates necessary memory and initializes variables
! 
! !INTERFACE:
      subroutine Init_Scratch ( NLev, this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Alloc
      implicit NONE
!
! !INPUT PARAMETERS: 
      integer,                intent (in)    ::
     .   NLev   ! Number of levels
!
! !INPUT/OUTPUT PARAMETERS:
      type ( cloud_scratch ), intent (inout) ::
     .   this   ! Data structure
!
! !OUTPUT PARAMETER:
      integer,                intent   (out) ::
     .   stat   ! returned status code from allocation statement
!
! !DESCRIPTION:
!     This routine allocates memory for the auxiliary data structure,
!     cloud_scratch.  This routine also initializes the data structure
!     parameters.
!
! !REVISION HISTORY: 
!     02Dec2003  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::Init_Scratch' 
      integer :: nvct, nnlev

!     Store parameters for data structure
!     -----------------------------------
      nnlev = NLev
      if ( nnlev .lt. 0 ) nnlev = NLev_Def
      nvct  = max ( 1, nnlev )     

!     ... store parameters for data structure
!     ---------------------------------------
      this % NVct = nvct
      this % NLev = 0

!     Allocate memory
!     ---------------
      allocate ( this % HT      ( nvct ),
     .           this % TT      ( nvct ),
     .           this % TTpp    ( nvct ),
     .           this % HH      ( nvct ),
     .           this % DPD     ( nvct ),
     .           this % DPDpp   ( nvct ),
     .           this % RH      ( nvct ),
     .           this % RHpp    ( nvct ),
     .           this % u       ( nvct ),
     .           stat = stat )
c      allocate ( HT ( NObs ), TT   ( NObs ), TTpp  ( NObs ),
c     .           HH ( NObs ), DPD  ( NObs ), DPDpp ( NObs ),
c     .           RH_( NObs ), RHpp ( NObs ), u     ( NObs ),
c     .           stat = istat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName,
     .                Alloc ( stat, 'this%HT,'
     .                           // 'this%TT,'
     .                           // 'this%TTpp,'
     .                           // 'this%HH,'
     .                           // 'this%DPD,'
     .                           // 'this%DPDpp,'
     .                           // 'this%RH,'
     .                           // 'this%RHpp,'
     .                           // 'this%u', nvct ))
         return
      end if

!     All done
!     --------
      return

      end subroutine Init_Scratch

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Init_List --- Allocates necessary memory and initializes variables
! 
! !INTERFACE:
      subroutine Init_List ( NLayers, this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Alloc
      implicit NONE
!
! !INPUT PARAMETERS: 
      integer,             intent (in)    ::
     .   NLayers ! Number of layers
!
! !INPUT/OUTPUT PARAMETERS:
      type ( cloud_list ), intent (inout) ::
     .   this    ! Data structure
!
! !OUTPUT PARAMETER:
      integer,             intent   (out) ::
     .   stat    ! returned status code from allocation statement
!
! !DESCRIPTION:
!     This routine allocates memory for the auxiliary data structure,
!     cloud_list.  This routine also initializes the data structure
!     parameters.
!
! !REVISION HISTORY: 
!     02Dec2003  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::Init_List' 
      integer :: nvct, nnlayers

!     Set and ...
!     -----------
      nnlayers = NLayers
      if ( nnlayers .lt. 0 ) nnlayers = NLayers_Def
      nvct  = max ( 1, nnlayers )

!     ... store parameters for data structure
!     ---------------------------------------
      this % NVct     = nvct
      this % NLayers  = 0
      this % IceCldBH = 0.0
      this % IceCldTH = 0.0
      this % WtrAmt   = 0.0
      this % WtrCldBH = 0.0
      this % WtrCldTH = 0.0

!     Allocate memory
!     ---------------
      allocate ( this % iAby  ( nvct ),
     .           this % CBH   ( nvct ),
     .           this % CBT   ( nvct ),
     .           this % CTH   ( nvct ),
     .           this % CTT   ( nvct ),
     .           stat = stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName,
     .                Alloc ( stat, 'this%iAby,'
     .                           // 'this%CBH,'
     .                           // 'this%CBT,'
     .                           // 'this%CTH,'
     .                           // 'this%CTT', nvct ))
         return
      end if

!     All done
!     --------
      return

      end subroutine Init_List

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Clean_Cloud --- Deallocate memory
! 
! !INTERFACE:
      subroutine Clean_Cloud ( this, stat )
!
! !USES
      use m_AdvError, only : PErr, Dealloc, ErrStat
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( cloud_data ),  intent (inout) ::
     .   this   ! Data structure
!
! !OUTPUT PARAMETER:
      integer, optional,    intent  (out) ::
     .   stat   ! Error return code.  If no error, then zero is returned.
                !   If not present, then the status code from the
                !   deallocate statements are ignored
!
! !DESCRIPTION:
!     This routine deallocates memory for the data structure of type
!     cloud_data.
!
! !REVISION HISTORY: 
!     02Dec2003  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Cloud'
      logical :: check_stat

!     Clear parameters
!     ----------------
      check_stat = present ( stat )

!     Deallocate
!     ----------
      call Clean_Input   ( this % Input,   stat )
      if ( check_stat ) then
         if ( stat .ne. 0 ) then
            call PErr ( MyName, ErrStat ( 'Clean_Input'   ) // '\W' )
            return
         end if
      end if

      call Clean_Scratch ( this % Scratch, stat )
      if ( check_stat ) then
         if ( stat .ne. 0 ) then
            call PErr ( MyName, ErrStat ( 'Clean_Scratch' ) // '\W' )
            return
         end if
      end if

      call  Clean_List   ( this % List,    stat )
      if ( check_stat ) then
         if ( stat .ne. 0 ) then
            call PErr ( MyName, ErrStat ( 'Clean_List'    ) // '\W' )
            return
         end if
      end if

!     All done
!     --------
      return

      end subroutine Clean_Cloud

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Clean_Input --- Deallocate memory
! 
! !INTERFACE:
      subroutine Clean_Input ( this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( cloud_input ), intent (inout) ::
     .   this   ! Data structure
!
! !OUTPUT PARAMETER:
      integer, optional,    intent   (out) ::
     .   stat   ! Error return code.  If no error, then zero is returned.
                !   If not present, then the status codes from the 
                !   deallocate statements are ignored
!
! !DESCRIPTION:
!     This routine deallocates memory for the data structure of type
!     cloud_input.
!
! !REVISION HISTORY: 
!     02Dec2003  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Input'
      logical :: check_stat
      integer :: istat

!     Clear parameters
!     ----------------
      this % NVct = 0
      this % NLev = 0

!     Initialize status options and codes
!     -----------------------------------
      check_stat    = present ( stat )
      if ( check_stat ) stat = 0

!     Deallocate memory
!     -----------------
      deallocate ( this % P,
     .             this % H,
     .             this % T,
     .             this % TMask,
     .             this % RH,
     .             this % RHMask,
     .             stat = istat )
      if ( istat .ne. 0 .and.  check_stat ) then
         call WPErr ( MyName,
     .                Dealloc ( stat, 'this%P,'
     .                             // 'this%H,'
     .                             // 'this%T,'
     .                             // 'this%TMask,'
     .                             // 'this%RH,'
     .                             // 'this%RHMask' ))
         stat = istat
         return
      end if

!     Reinitialize pointers
!     ---------------------
      call Nullify_Input ( this )

!     All done
!     --------
      return

      end subroutine Clean_Input

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Clean_Scratch --- Deallocate memory
! 
! !INTERFACE:
      subroutine Clean_Scratch ( this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( cloud_scratch ), intent (inout) ::
     .   this   ! Data structure
!
! !OUTPUT PARAMETER:
      integer, optional,      intent   (out) ::
     .   stat   ! Error return code.  If no error, then zero is returned.
                !   If not present, then the status code from the
                !   deallocate statements are ignored
!
! !DESCRIPTION:
!     This routine deallocates memory for the data structure of type
!     cloud_scratch.
!
! !REVISION HISTORY: 
!     02Dec2003  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Scratch'
      logical :: check_stat
      integer :: istat

!     Clear parameters
!     ----------------
      this % NVct = 0
      this % NLev = 0

!     Initialize status options and codes
!     -----------------------------------
      check_stat    = present ( stat )
      if ( check_stat ) stat = 0

!     Deallocate memory
!     -----------------
      deallocate ( this % HT,
     .             this % TT,
     .             this % TTpp,
     .             this % HH,
     .             this % DPD,
     .             this % DPDpp,
     .             this % RH,
     .             this % RHpp,
     .             this % u,
     .             stat = istat )
      if ( istat .ne. 0 .and.  check_stat ) then
         call WPErr ( MyName,
     .                Dealloc ( stat, 'this%HT,'
     .                             // 'this%TT,'
     .                             // 'this%TTpp,'
     .                             // 'this%HH,'
     .                             // 'this%DPD,'
     .                             // 'this%DPDpp,'
     .                             // 'this%RH,'
     .                             // 'this%RHpp,'
     .                             // 'this%u' ))
         stat = istat
         return
      end if

!     Reinitialize pointers
!     ---------------------
      call Nullify_Scratch ( this )

!     All done
!     --------
      return

      end subroutine Clean_Scratch
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Clean_List --- Deallocate memory
! 
! !INTERFACE:
      subroutine Clean_List ( this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( cloud_list ), intent (inout) ::
     .   this   ! Data structure
!
! !OUTPUT PARAMETER:
      integer, optional,   intent   (out) ::
     .   stat   ! Error return code.  If no error, then zero is returned.
                !   If not present, then the status code from the
                !   deallocate statements are ignored
!
! !DESCRIPTION:
!     This routine deallocates memory for the data structure of type
!     cloud_list.
!
! !REVISION HISTORY: 
!     02Dec2003  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // ':Clean_List'
      logical :: check_stat
      integer :: istat

!     Clear parameters
!     ----------------
      this % NVct     = 0
      this % NLayers  = 0
      this % IceCldBH = 0.0
      this % IceCldTH = 0.0
      this % WtrAmt   = 0.0
      this % WtrCldBH = 0.0
      this % WtrCldTH = 0.0

!     Initialize status options and codes
!     -----------------------------------
      check_stat  = present ( stat )
      if ( check_stat ) stat = 0

!     Deallocate memory
!     -----------------
      deallocate ( this % iAby,
     .             this % CBH,
     .             this % CBT,
     .             this % CTH,
     .             this % CTT,
     .             stat = istat )
      if ( istat .ne. 0 .and.  check_stat ) then
         call WPErr ( MyName,
     .                Dealloc ( stat, 'this%iAby,'
     .                             // 'this%CBH,'
     .                             // 'this%CBT,'
     .                             // 'this%CTH,'
     .                             // 'this%CTT'))
         stat = istat
         return
      end if

!     Reinitialize pointers
!     ---------------------
      call Nullify_List ( this )

!     All done
!     --------
      return

      end subroutine Clean_List
!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Cloud - Initializes pointers in the input data structure
!
! !INTERFACE:
      subroutine Nullify_Cloud ( this )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( cloud_data ), intent ( inout ) ::
     .   this  ! data structure to be nullified
!
! !REVISION HISTORY:
!     02Dec2003  C. Redder  Initial code
!EOP
!.................................................................

      call Nullify_Input   ( this % Input   )
      call Nullify_Scratch ( this % Scratch )
      call Nullify_List    ( this % List    )

      return
      end subroutine Nullify_Cloud

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Input - Initializes pointers
!
! !INTERFACE:
      subroutine Nullify_Input ( this )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( cloud_input ), intent ( inout ) ::
     .   this  ! data structure to be nullified
!
! !REVISION HISTORY:
!     02Dec2003  C. Redder  Initial code
!EOP
!.................................................................

      nullify ( this % P,
     .          this % H,
     .          this % T,
     .          this % TMask,
     .          this % RH,
     .          this % RHMask )

      return
      end subroutine Nullify_Input

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Scratch - Initializes pointers
!
! !INTERFACE:
      subroutine Nullify_Scratch ( this )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( cloud_scratch ), intent ( inout ) ::
     .   this  ! data structure to be nullified
!
! !REVISION HISTORY:
!     02Dec2003  C. Redder  Initial code
!EOP
!.................................................................

      nullify ( this % HT,
     .          this % TT,
     .          this % TTpp,
     .          this % HH,
     .          this % DPD,
     .          this % DPDpp,
     .          this % RH,
     .          this % RHpp,
     .          this % u )

      return
      end subroutine Nullify_Scratch

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_List - Initializes pointers
!
! !INTERFACE:
      subroutine Nullify_List ( this )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( cloud_list ), intent ( inout ) ::
     .   this  ! data structure to be nullified
!
! !REVISION HISTORY:
!     02Dec2003  C. Redder  Initial code
!EOP
!.................................................................

      nullify ( this % iAby,
     .          this % CBH,
     .          this % CBT,
     .          this % CTH,
     .          this % CTT )

      return
      end subroutine Nullify_List
!.................................................................
      end module m_clouds
!====================================================================
