!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_convert --
!
! !DESCRIPTION:
!
! !INTERFACE:
!
      module    m_convert
      use       m_temperature, only : VT_Exact,   VT_NCEP
      use       m_humidity,    only : wEqtn_AE96, wEqtn_NCEP, iEqtn_AE96

      implicit  NONE
      private ! except

      public ::
     .   SelectHEq,     ! Select a formulae for computing humidity
     .   SelectVar,     ! Select a temperature or humidity variable
     .   CheckH,        ! Check humidity values
     .   ConvertH,      ! Convert between two specified humidity variables
     .   ConvertT,      ! ... or temperature variables.
     .   ComputeVT,     ! Compute virtual temperature from temperature
     .   toHVar,        ! Convert to (or from) specified humidity variable
     .   toTVar,        ! ... temperature variable
     .   TtoVT,         ! ... from temperature variable to virtual temp
     .   ktMask,        ! Modifies given logical based on kt's
     .   toVarkt        ! Replaces valid data type index (kt) with
                        !   the kx for a given variable.

      public ::         ! Symbolic constants
     .   Temperature,
     .   Humidity
      public ::         ! Constants set to their data type indices (kt). 
     .   Mixing_Ratio,          ! g/kg
     .   Dew_Point,             ! deg K 
     .   Vapor_Pressure,        ! mb or hPa
     .   Relative_Humidity,     ! %
     .   Specific_Humidity,     ! g/kg
     .   Air_Temperature,       ! deg K
     .   Potential_Temperature, ! deg K
     .   Virtual_Temperature    ! deg K
      public ::           ! Lists of valid data type indices for
     .   kt_HumList,      ! ... humidity variables
     .   kt_TempList,     ! ... temperature variables
     .   kt_VTempList     ! ... virtual temperature variables.
      public ::           ! Equation code for determining the SVP
     .   wEqtn_AE96,      ! ... over water from AE96
     .   wEqtn_NCEP,      ! ... over water from NCEP
     .   wEqtn_Vai,       ! ... over water from Vaisala.
     .   iEqtn_AE96       ! ... over ice   from AE96
      public ::           ! Equation code for determining the VT
     .   VT_Exact,        ! ... using the exact equations
     .   VT_NCEP          ! ... NCEPS's approximation

      interface CheckH
         module procedure 
     .      CheckH_byObList
      end interface
      interface ConvertH
         module procedure 
     .      ConvertH_byObList
      end interface
      interface ConvertT
         module procedure 
     .      ConvertT_byObList
      end interface
      interface ComputeVT
         module procedure 
     .      ComputeVT_byObList
      end interface
      interface toHVar
         module procedure 
     .      toHVar_wMkt1,       ! ... with kt_in/out and Mask
     .      toHVar_wMask,       ! ... with given kt's and mask
     .      toHVar_             ! ... or without mask
      end interface
      interface toTVar
         module procedure 
     .      toTVar_wMkt1,       ! ... with kt_in/out and Mask
     .      toTVar_wMask,       ! ... with given kt's and mask
     .      toTVar_             ! ... or without mask
      end interface
      interface TtoVT 
         module procedure 
     .      TtoVT_wMask,        ! ... with given kt's and mask
     .      TtoVT_              ! ... or without mask
      end interface
!
! !REVISION HISTORY:
!     03Sep2002  C. Redder  Original version
!     20Oct2003  C. Redder  Added the public routine SelectVar and defined
!                           variables in the module header.  Added the
!                           routine, toRH_byObList, toAT_byObList and
!                           toAT_byObList to the generic interfaces, toRH,
!                           toAT and toVT, respectively.  Added public
!                           constants, Temperature and Humidity.
!     19Apr2004  C. Redder  Renamed generic interfaces form toRH and toAT,
!                           to toHVar and toTVar and removed the module
!                           procedures, toRH_byObList and toRH_Inq, from
!                           toHVar and toAT_byObList and toAT_Inq from 
!                           toTVar.  Made public symobolic constants set 
!                           to the appropriate kt for temperature,
!                           virtual temperature and humidity variables.
!                           Added the generic interfaces, ConvertT and
!                           ConvertH.  Added the public entities,
!                           wEqtn_AE96, wEqtn_NCEP, iEqtn_AE96,
!                           VT_Exact and VT_NCEP
!     20Nov2006  C. Redder  Added the public routine, SelectHEq and the
!                           public constant wEqtn_Vai.
!     20Mar2007  C. Redder  Added the module procedures toHVar_wMkt and
!                           toTVar_wMkt to the generic interfaces,
!                           toHVar and toTVar respectively
!EOP
!-----------------------------------------------------------------

!     Variable types
!     --------------
      integer, parameter ::
     .   Temperature = 0,
     .   Humidity    = 1

!     Data type indices (kt) for humidity
!     -----------------------------------
      integer, parameter ::
     .   Vapor_Pressure        =  0,   ! mb or hPa (not official kt)
     .   Mixing_Ratio          =  7,   ! g/kg
     .   Dew_Point             =  9,   ! deg K 
     .   Relative_Humidity     = 10,   ! %
     .   Specific_Humidity     = 11    ! g/kg

!     ... temperature
!     ---------------
      integer, parameter ::
     .   Air_Temperature       =  8,   ! deg K
     .   Potential_Temperature = 37    ! deg K

!     ... virtual temperature
!     -----------------------
      integer, parameter ::
     .   Virtual_Temperature   = 44    ! deg K

!     Equation codes for determining the equation for the SVP
!     -------------------------------------------------------
      integer, parameter ::
     .   wEqtn_Vai  = 2      ! ... over water from Vaisala instruments

!     Dimension for scratch space
!     ---------------------------
      integer, parameter ::
     .   ktMin                 =  7,
     .   ktMax                 = 44

!     Highest level for which data will
!     be scanned in function SelectVar for ...
!     ----------------------------------------
      real,    parameter ::
     .   PMin_Temperature      = 0.0,  ! ... temperature (all levels)
     .   PMin_Humidity         = 100.0 ! ... humidity (hPa).

!     Valid range for physically reasonable dew points
!     ------------------------------------------------
      real,    parameter ::
     .   DP_min = 150.0,
     .   DP_max = 325.0

!     List of data type indices for the recognized ... 
!     ------------------------------------------------
      integer, parameter ::
     .   MaxListSz  = 4,
     .
     . ! ... humidity variables
     . ! ----------------------
     .   HumListSz  = 4,
     .   kt_HumList     ( HumListSz ) = (/ Relative_Humidity,   ! Default
     .                                     Mixing_Ratio,
     .                                     Dew_Point,
     .                                     Specific_Humidity /),
     . ! ... temperature variables
     . ! -------------------------
     .   TempListSz = 2,
     .   kt_TempList   ( TempListSz ) = (/ Air_Temperature,     ! Default
     .                                     Potential_Temperature /),
     .
     . ! ... virtual temperature variables
     . ! ---------------------------------
     .   VTempListSz = 1,
     .   kt_VTempList ( VTempListSz ) = (/ Virtual_Temperature /)
 
      contains

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SelectHEq --- Select the equation for the saturated vapor pressure from WMO instrument type 
! 
! !INTERFACE:
      function SelectHEq ( rkx )
      use  m_VaiUtil,  only : Vai_rkxList
      use  m_RadLists, only : rkx_names
!
! !USES
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,       intent (in) ::
     .   rkx       ! WMO rawinsonde instrument type.  If input value is
!                  !   invalid (i.e out of range), then -1 is returned
! OUTPUT PARAMETERS:
      integer ::
     .   SelectHEq ! Integer code for the saturation vapor equation
!
! !DESCRIPTION:
!
! !REVISION HISTORY: 
!     20Nov2006  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------

      integer, parameter :: Vai_rkxListSz = size ( Vai_rkxList ),
     .                      nrkx          = size ( rkx_names   )
      logical :: wList_initialized = .false.
      integer :: wCodeList ( nrkx ) = wEqtn_AE96 ! default
      integer :: iList, ListSz, rkx_

      if ( .not. wList_initialized ) then
!!         commented out in response to the elimation of proprietary
!!         code in the module, m_VaiUtil.f
!!         do iList = 1, Vai_rkxListSz
!!            wCodelist ( Vai_rkxList ( iList )) = wEqtn_Vai
!!        end do
         wList_initialized = .true.
      end if

      SelectHEq = -1

      rkx_   = rkx
      if ( rkx_ .eq. 0 ) rkx_ = nrkx
      if ( rkx_ .ge. 1 .and.
     .     rkx_ .le. nrkx ) SelectHEq = wCodeList ( rkx_ )

      return
      end function SelectHEq

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: SelectVar -- Select temperature or humidity variable
!
! !DESCRIPTION:
!     This function selects a temperature or humidity variable from an
!     observation data stream within a profile based on the vertical
!     distribution and of data of valid data. The ideal distribution
!     of points is defined so that all points are equidistant in log P
!     space from their adjacent points.  The variable with the
!     distribution closest to the ideal and has the largest range is
!     selected and its corresponding data type index (kt) is returned.
!     This function assumes that the pressure values have been checked
!     and are masked out if necessary.  Only humidity observationa at
!     pressure levels at and below 100 hPa are considered.
!
! !INTERFACE:
      function SelectVar ( P, kt, Mask, var )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,        intent (in)  ::
     .   P    (:)  ! Pressure    (hPa or mb)
      integer,     intent (in)  ::
     .   kt   (:)  ! Data type index.
      logical,     intent (in)  ::
     .   Mask (:)  ! = .true./.false. to mask in/out (i.e. accept/reject)
      integer,     intent (in), optional ::
     .   var       ! Desired variable type.  Valid choices are listed
                   !   in module header.  If choice is invalid var,
                   !   is set to the default, Temperature.
!
! !OUTPUT PARAMETERS: 
      integer ::
     .   SelectVar ! data type index of selected variable
!
!     Note: Number of values is determined by the minimum size of all
!           required array arguments.
!
!     Note: See the module header for the list of recognized
!           humidity and temperature variables
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     16Oct2003  C. Redder  Original code.
!
! EOP
!....................................................................

      integer :: kt_Table   ( ktMin : ktMax ), iList_Min, NNonNull,
     .           kt_List    ( MaxListSz ), ListSz, var_,
     .           kt_NObs    ( MaxListSz ), ikt, iList,
     .           ky_NFact   ( MaxListSz ), iOb, NObs, NTObs
      real    :: kt_Sum     ( MaxListSz ), kt_Fit, Fact,
     .           kt_Prod    ( MaxListSz ), kt_MaxGauge, kt_Gauge,
     .           kt_PTop    ( MaxListSz ),
     .           kt_PBottom ( MaxListSz ), Range,
     .           kt_Incr    ( MaxListSz ), PTop, PBottom, PMin,
     .           kt_P       ( MaxListSz ), Pi, Pii, ProdMax
      logical :: masked_in


!     Seclect variable type
!     ---------------------
      var_ = Temperature
      if ( present ( var )) var_ = var

!     ... and implement
!     -----------------
      if ( var_ .eq. Humidity ) then
         PMin   = PMin_Humidity
         ListSz =  HumListSz
         do iList = 1, ListSz
            kt_List ( iList ) = kt_HumList  ( iList )
         end do
      else
         PMin   = PMin_Temperature
         ListSz = TempListSz
         do iList = 1, ListSz
            kt_List ( iList ) = kt_TempList ( iList )
         end do
      end if

!     Set default for the case with no obs
!     ------------------------------------
      SelectVar = kt_List ( 1 )
      NTObs     = min ( size ( P ), size ( kt ), size ( Mask ))
      if ( NTObs .eq. 0 ) return 

!     Initialize internal variables
!     -----------------------------
      do ikt = ktMin, ktMax
         kt_Table ( ikt ) = 0
      end do
      do iList = 1, ListSz
         kt_Table   ( kt_List ( iList )) = iList
         kt_NObs    ( iList ) = 0
         kt_PTop    ( iList ) = P ( 1     )
         kt_PBottom ( iList ) = P ( NTObs )
      end do

!     Count the number of valid obs and determine the range of P for each kt
!     ----------------------------------------------------------------------
      do iOb = 1, NTObs
         Pi        = P    ( iOb )
         ikt       = kt   ( iOb )
         iList     = 0
         if ( ikt .ge. ktMin .and.
     .        ikt .le. ktMax ) iList = kt_Table ( ikt )
         masked_in = Mask ( iOb )    .and.
     .               Pi    .ge. PMin .and.
     .               iList .gt. 0
         if ( masked_in ) then
            kt_NObs    ( iList ) =       kt_NObs    ( iList ) + 1
            kt_PTop    ( iList ) = min ( kt_PTop    ( iList ), Pi )
            kt_PBottom ( iList ) = max ( kt_PBottom ( iList ), Pi )
         end if
      end do

!     Proceed no further if only one kt has valid obs
!     -----------------------------------------------
      NNonNull = 0
      do iList = 1, ListSz
         NObs = kt_NObs ( iList )
         if ( NObs .gt. 0 ) then
            NNonNull  = NNonNull + 1
            SelectVar = kt_List ( iList )
         end if
      end do
      if ( NNonNull .le. 1 ) return

!     Initialize sums and products and
!     define the ideal distribution of points
!     ---------------------------------------
      do iList = 1, ListSz
         NObs = kt_NObs ( iList )
         kt_Prod     ( iList ) = 1.0
         kt_Sum      ( iList ) = 0.0
         kt_P        ( iList ) = kt_PBottom ( iList )
         kt_Incr     ( iList ) = 1.0
         if ( NObs .ge. 2 ) then
            kt_Incr  ( iList ) = 10.0 ** ( -log10 ( kt_PBottom ( iList )
     .                                          /   kt_PTop    ( iList))
     .                                          / ( NObs - 1          ))
         end if
      end do

!     Determine the fit to the ideal for each
!     valid point and the sum over all points
!     ---------------------------------------
      ProdMax = sqrt ( huge ( 1.0 ))
      do iOb = 1, NTObs
         Pi         = P    ( iOb )
         ikt        = kt   ( iOb )
         iList      = 0
         if ( ikt .ge. ktMin .and.
     .        ikt .le. ktMax ) iList = kt_Table ( ikt )
         masked_in  = Mask ( iOb )    .and.
     .                Pi    .ge. PMin .and.
     .                iList .gt. 0
         if ( masked_in ) then
            Pii     = kt_P ( iList )
            if ( Pi .ge. Pii ) then
               Fact = Pi / Pii
            else
               Fact = Pii / Pi
            end if
            kt_Prod ( iList ) = kt_Prod ( iList ) * Fact
            if ( kt_Prod ( iList ) .gt. ProdMax ) then
               kt_Sum  ( iList ) = kt_Sum ( iList )
     .                           + log10 ( kt_Prod ( iList ))
               kt_Prod ( iList ) = 1.0
            end if
            kt_P   ( iList ) = Pii * kt_Incr ( iList )
         end if
      end do

!     After accounting for range, the number of observations and the fit
!     of the data distribution to the ideal, choose the data type index
!     ------------------------------------------------------------------
      kt_MaxGauge = 0.0
      iList_Min   = 1
      do iList    = 1, ListSz
         NObs     = kt_NObs    ( iList )
         PTop     = kt_PTop    ( iList )
         PBottom  = kt_PBottom ( iList )
         Range    = abs ( log10 ( 10.0 * PBottom / PTop ))
         if ( PBottom .le. 1.001 * PTop .or. NObs .le. 0 ) then
            kt_Fit = 1.0
         else
            kt_Fit = max ( kt_Sum          ( iList )
     .                   + log10 ( kt_Prod ( iList )),
     .                     0.00001 * real ( NObs ) * Range ) ! to prevent
     .                   /         ( real ( NObs ) * Range ) !   division by
         end if                                              !   zero
         kt_Gauge    = real ( NObs ) * Range / kt_Fit

         if ( kt_Gauge .gt. kt_MaxGauge ) then
            kt_MaxGauge = kt_Gauge
            iList_Min   = iList
         end if
      end do

      SelectVar = kt_List ( iList_Min )

      return
      end function SelectVar
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CheckH_byObList -- Check humidity values by ob list
!
! !DESCRIPTION:
!     This routine checks humidity variables to ensure that the
!     humidity values are physically reasonable.
!
! !INTERFACE:
      subroutine CheckH_byObList ( ObList, P, T,   ! Required and ...
     .                             kt_in, Obs, Valid,
     .                             SVP_Eqtn )      ! ... optional
      use m_humidity, only : MRtoVP, VPtoMR,       !   parameters
     .                       DPtoVP, VPtoDP, DP2VP,
     .                       RHtoVP, VPtoRH,
     .                       SHtoVP, VPtoSH
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,         intent (in)    ::
     .   ObList (:)  ! List of observation by array index number
      real,            intent (in)    ::
     .   P      (:), ! Pressure    (hPa or mb)
     .   T      (:)  ! Temperature (deg K)
      integer,         intent (in)    ::
     .   kt_in       ! The specified data type index for all input data
      real,            intent (in)    ::
     .   Obs    (:)  ! Observed values
      integer,         intent (in), optional ::
     .   SVP_Eqtn    ! Equation used to compute saturation vapor pressure.
                     !   See module, m_humidity, for default and further
                     !   details.
!
! !INPUT/OUTPUT PARAMETERS: 
      logical,         intent (out)   ::
     .   Valid  (:)  ! = .true. if humidity is physically reasonable
!
!     Note: Number of values is determined by the size of ObList
!
!     Note: See the module header for the list of recognized
!           humidity variables
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     10May2004  C. Redder  Original code.
!
! EOP
!....................................................................

      integer, parameter :: ScrSz = 255
      real, dimension ( ScrSz ) :: Ps, Ts, Hs, Hum, SVP
      real :: Hs_max, Hs_min
      integer, dimension ( ScrSz ) :: List
      integer :: NObs, NSeg, iSeg, iSegBeg, iSegEnd, SegSz, iSegPt, iPt,
     .           iList, ListSz
      logical :: SVP_required, valid_kt

      NObs = size ( ObList )      
      if ( NObs  .eq. 0 ) return       ! No obs to check

!     Determine if saturation vapor pressure is required
!     --------------------------------------------------
      SVP_required = kt_in  .eq. Relative_Humidity 

!     Set the humidity range
!     ----------------------
      if ( kt_in .eq. Dew_Point ) then
         Hs_min = DP_min
         Hs_max = DP_max
      else
         Hs_min = DP2VP ( DP_min, Eqtn = SVP_Eqtn )
         Hs_max = DP2VP ( DP_max, Eqtn = SVP_Eqtn )
      end if

!     Process the listed obs in segments
!     ----------------------------------
      NSeg    = ( NObs - 1 ) / ScrSz + 1
      iSegBeg = 1
      do iSeg = 1, NSeg
         iSegEnd = min ( iSegBeg + ScrSz - 1, NObs )
         SegSz   = iSegEnd - iSegBeg + 1

!        Gather the values to be processed into the scratch space
!        --------------------------------------------------------
         ListSz = 0
         do iSegPt = 1, SegSz
            iPt           = ObList ( iSegBeg + iSegPt - 1 )
            Valid ( iPt ) = .false.
            if ( Obs ( iPt ) .ge. 0.0 ) then
               ListSz          = ListSz + 1
               List ( ListSz ) = iPt
               Ps ( iSegPt )   = P      ( iPt )
               Ts ( iSegPt )   = T      ( iPt )
               Hs ( iSegPt )   = Obs    ( iPt )
            end if
         end do

!        ... calculate the saturation vapor pressure (if necessary)
!        ----------------------------------------------------------
         if ( SVP_required )
     .      call DPtoVP ( Ts  ( : ListSz ),
     .                    SVP ( : ListSz ), Eqtn = SVP_Eqtn ) 

!        ... convert from the variable, kt_in,
!            to vapor pressure or dew point
!        -------------------------------------
         valid_kt = .false.
         if      ( kt_in  .eq. Mixing_Ratio      ) then
            call MRtoVP ( Hs  ( : ListSz ),
     .                    Ps  ( : ListSz ),
     .                    Hum ( : ListSz )) 
            valid_kt = .true.

         else if ( kt_in  .eq. Relative_Humidity ) then
            call RHtoVP ( Hs  ( : ListSz ),
     .                    SVP ( : ListSz ),
     .                    Ps  ( : ListSz ),
     .                    Hum ( : ListSz ))
            valid_kt = .true.

         else if ( kt_in  .eq. Specific_Humidity ) then
            call SHtoVP ( Hs  ( : ListSz ),
     .                    Ps  ( : ListSz ),
     .                    Hum ( : ListSz ))
            valid_kt = .true.

         else if ( kt_in  .eq. Vapor_Pressure .or.
     .             kt_in  .eq. Dew_Point         ) then
            do iList = 1, ListSz
               Hum ( iList ) = Hs ( iList )
            end do
            valid_kt = .true.

         end if

!        Check and scatter the values back to the output array
!        -----------------------------------------------------
         if ( valid_kt ) then
            do iList = 1, ListSz
               iPt           = List  ( iList )
               Valid ( iPt ) = Hs    ( iList ) .ge. Hs_min .and.
     .                         Hs    ( iList ) .le. Hs_max
            end do
         end if

!        Locate next segment
!        -------------------
         iSegBeg = iSegEnd + 1
      end do

      return
      end subroutine CheckH_byObList

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: toHVar_wMkt1 -- Convert obs to/from a specified humidity variable with masking and a given kt
!
! !DESCRIPTION:
!     This routine converts humidity observations to/from a variable,
!     specified by the data type index, kt_out.  Any observation report
!     masked out is ignored.   If kt_in or kt_out are invalid then no
!     conversions are made.
!
! !INTERFACE:
      subroutine toHVar_wMkt1 ( P, T, Mask, kt_in, kt_out, Obs, ! Required and
     .                          SVP_Eqtn             )          ! ... optional
!                                                               !    parameters
! !INPUT PARAMETERS:
      implicit   NONE
      real,             intent (in)    ::
     .   P       (:), ! Pressure    (hPa or mb)
     .   T       (:)  ! Temperature (deg K)
      logical,          intent (in)    ::
     .   Mask    (:)  ! = .true./.false. to mask in/out (i.e. accept/reject)
      integer,          intent (in)    ::
     .   kt_in,       ! Data type index for input humidity data
     .   kt_out       ! ...and output data (Use zero for vapor pressure.)
      integer,          intent (in), optional ::
     .   SVP_Eqtn     ! Equation used to compute saturation vapor pressure.
                      !   See module, m_humidity, for default and further
                      !   details.
! !INPUT/OUTPUT PARAMETERS: 
      real,             intent (inout) ::
     .   Obs     (:)  ! Observed values
!
!     Note: Number of values is determined by the minimum size of all
!           required array arguments (except Mask).
!
!     Note: See the module header for the list of recognized
!           humidity variables
!
! !SEE ALSO: 
!     toHVar_    - convert obs to (or from) humidity var for given kt's
!
! !REVISION HISTORY:
!     21Mar2007  C. Redder  Original code.
! EOP
!....................................................................

      integer, parameter :: ScrSz = 255
      integer :: ObList   ( ScrSz ),
     .           NTObs, NObs, iOb, NMask
      logical :: masked_in, last_mask

!     Nothing to do if kt_in and kt_out are the same
!     ----------------------------------------------
      if ( kt_in .eq. kt_out ) return

!     Determine the number of observations
!     ------------------------------------
      NTObs = min ( size ( P  ), size ( T   ), ! ... observations
     .              size ( Obs ))
      NMask = min ( size ( Mask ), NTObs )     ! ... masks
      if ( NTObs .le. 0 ) return               ! Nothing to do

      last_mask = .true.
      if ( NMask .gt. 0 ) last_mask = Mask ( NMask )

!     Process all obs
!     ---------------
      NObs = 0
      do iOb = 1, NTObs

!        Determine if ob is to be processed
!        ----------------------------------
         masked_in = last_mask
         if ( iOb .le. NMask ) masked_in = Mask ( iOb )

!        If ob is to precessed 
!        ---------------------
         if ( masked_in ) then

!           then, if there is room to add another list entry
!           ------------------------------------------------
            if ( NObs .lt. ScrSz ) then

                 ! ... then add to the list
!                --------------------------
               NObs = NObs + 1

            else ! ... process the listed obs and clean the scratch
!           ------------------------------------------------------- 
               call ConvertH_byObList ( ObList, P, T, kt_in, kt_out,
     .                                  Obs,
     .                                  SVP_Eqtn = SVP_Eqtn )
               NObs = 1
            end if

!           Save entry in list
!           -----------------
            ObList ( NObs ) = iOb
         end if
      end do

!     process all obs that are remaining on the list
!     ---------------------------------------------
      if ( NObs .gt. 0 )
     .   call ConvertH_byObList ( ObList ( : NObs ),
     .                            P, T, kt_in, kt_out, Obs,
     .                            SVP_Eqtn = SVP_Eqtn )

      return
      end subroutine toHVar_wMkt1

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: toHVar_wMask -- Convert obs to/from a specified humidity variable with masking and given kt's
!
! !DESCRIPTION:
!     This routine converts humidity observations to/from a variable,
!     specified by the data type index, kt0.  Any observation report
!     masked out or without a recognized humidity kt is ignored.
!
! !INTERFACE:
      subroutine toHVar_wMask ( P, T, kt, Mask, kt0, Obs, ! Required and
     .                          backwd,   SVP_Eqtn )      ! ... optional
!                                                         !   parameters
! !INPUT PARAMETERS: 
      implicit   NONE
      real,             intent (in)    ::
     .   P       (:), ! Pressure    (hPa or mb)
     .   T       (:)  ! Temperature (deg K)
      integer,          intent (in)    ::
     .   kt      (:)  ! Data type index.
      logical,          intent (in)    ::
     .   Mask    (:)  ! = .true./.false. to mask in/out (i.e. accept/reject)
      integer,          intent (in)    ::
     .   kt0          ! The specified humidity variable (Use zero for
     .                !   vapor pressure.)
      logical,          intent (in), optional ::
     .   backwd       ! = .true. to convert backwards from the variable
                      !   specified by kt0.  Default: backwd = .false.
      integer,          intent (in), optional ::
     .   SVP_Eqtn     ! Equation used to compute saturation vapor pressure.
                      !   See module, m_humidity, for default and further
                      !   details.
! !INPUT/OUTPUT PARAMETERS: 
      real,             intent (inout) ::
     .   Obs     (:)  ! Observed values
!
!     Note: Number of values is determined by the minimum size of all
!           required array arguments (except Mask).
!
!     Note: See the module header for the list of recognized
!           humidity variables
!
! !SEE ALSO: 
!     toHVar_    - convert obs to (or from) humidity var for given kt's
!
! !REVISION HISTORY:
!     09Aug2002  C. Redder  Original code.
!     21Feb2003  C. Redder  Added temporary storage for Obs
!     21Oct2003  C. Redder  Removed required parameter, rstat.  Major
!                           changes in the internal code to use scratch
!                           space of fix size (i.e. remove allocate
!                           statements) and to remove calls to external
!                           sort routines.  Removed the optional
!                           arguments, ktRH, nkt, ktList.
!     03Apr2004  C. Redder  Added the arguments, kt0 (required) and
!                           SVP_Eqtn (optional).  Made changes in response
!                           to interface modifictions to the routine,
!                           ConvertH_byObList.  Renamed the optional
!                           argument, fromRH, to backwd and the routine
!                           from toRH_wMask to toHVar_wMask.
!     12May2004  C. Redder  Fixed bug to prevent non-temperature data from
!                           being processed and to prevent the dummy
!                           argument from being accessed when not
!                           associated.
! EOP
!....................................................................

      integer, parameter :: ScrSz = 255
      integer :: ObLists    ( HumListSz, ScrSz ),
     .           ObList                ( ScrSz ),
     .           kt_NObs    ( HumListSz ),
     .           kt_Table   ( ktMin : ktMax ), NTObs, NObs, iOb,
     .           NMask, iOb_kt, kt_in, kt_out, ikt, iList
      logical :: masked_in, last_mask, from_kt0

!     Determine the number of observations
!     ------------------------------------
      NTObs = min ( size ( P  ), size ( T   ), ! ... observations
     .              size ( kt ), size ( Obs ))
      NMask = min ( size ( Mask ), NTObs )     ! ... masks
      if ( NTObs .le. 0 ) return               ! Nothing to do

!     Impliment option
!     ----------------
      from_kt0 = .false.
      if ( present ( backwd ))  from_kt0 = backwd

!     Initialize internal variables
!     -----------------------------
      do ikt = ktMin, ktMax
         kt_Table ( ikt ) = 0
      end do
      do iList = 1, HumListSz
         kt_Table   ( kt_HumList ( iList )) = iList
         kt_NObs    ( iList ) = 0
      end do

      last_mask = .true.
      if ( NMask .gt. 0 ) last_mask = Mask ( NMask )

!     Process all obs
!     ---------------
      do iOb = 1, NTObs
         ikt       = kt   ( iOb )
         iList     = 0
         if ( ikt .ge. ktMin .and.
     .        ikt .le. ktMax ) iList = kt_Table ( ikt )

!        Determine if ob is to be processed
!        ----------------------------------
         masked_in = last_mask
         if ( iOb .le. NMask ) masked_in = Mask ( iOb )
         masked_in = masked_in      .and.
     .               kt0   .ne. ikt .and.
     .               iList .gt. 0 

!        If ob is to precessed 
!        ---------------------
         if ( masked_in ) then
            NObs = kt_NObs ( iList )

!           then, if there is room to add another list entry for given kt
!           -------------------------------------------------------------
            if ( NObs .lt. ScrSz ) then

                 ! ... then add to the list
!                --------------------------
               NObs = NObs + 1

            else ! ... process the listed obs and clean the scratch
!           ------------------------------------------------------- 
               do iOb_kt = 1, NObs
                  ObList ( iOb_kt ) = ObLists ( iList, iOb_kt )
               end do
               if ( from_kt0 ) then
                  kt_in  = kt0
                  kt_out = kt_HumList ( iList )
               else
                  kt_in  = kt_HumList ( iList )
                  kt_out = kt0
               end if
               call ConvertH_byObList ( ObList, P, T, kt_in, kt_out,
     .                                  Obs,
     .                                  SVP_Eqtn = SVP_Eqtn )
               NObs = 1
            end if

!           Save entry in list
!           -----------------
            ObLists ( iList, NObs ) = iOb
            kt_NObs ( iList )       = NObs
         end if
      end do

!     For each valid kt, process all obs
!     that are remaining on the list
!     ----------------------------------
      do iList = 1, HumListSz
         if ( from_kt0 ) then
            kt_in  = kt0
            kt_out = kt_HumList ( iList )
         else
            kt_in  = kt_HumList ( iList )
            kt_out = kt0
         end if
         NObs = kt_NObs    ( iList )
         if ( NObs .gt. 0 .and. kt_in .ne. kt_out ) then
            do iOb_kt = 1, NObs
               ObList ( iOb_kt ) = ObLists ( iList, iOb_kt )
            end do
            call ConvertH_byObList ( ObList ( : NObs ),
     .                               P, T, kt_in, kt_out, Obs,
     .                               SVP_Eqtn = SVP_Eqtn )
         end if
      end do

      return
      end subroutine toHVar_wMask
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: toHVar_ -- Convert obs to/from a specified humidity variable with given kt's
!
! !DESCRIPTION:
!     This routine converts observations to/from a variable specified
!     by the data type index, kt0.  Any observation report without a
!     recognized humidity kt is ignored.
!
! !INTERFACE:
      subroutine toHVar_ ( P, T, kt, kt0, Obs, backwd, SVP_Eqtn )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,             intent (in)    ::
     .   P       (:), ! Pressure    (hPa or mb)
     .   T       (:)  ! Air temperature (deg K)
      integer,          intent (in)    ::
     .   kt      (:)  ! Data type index.
      integer,          intent (in)    ::
     .   kt0          ! The specified humidity variable (Use zero for
     .                !   vapor pressure.)
      logical,          intent (in), optional ::
     .   backwd       ! = .true. to convert backwards from the variable
                      !   specified by kt0.  Default: backwd = .false.
      integer,          intent (in), optional ::
     .   SVP_Eqtn     ! Equation used to compute satuation vapor pressure.
                      !   See module, m_humidity, for default and further
                      !   details.
! !INPUT/OUTPUT PARAMETERS: 
      real,             intent (inout) ::
     .   Obs     (:)  ! Observed values
!
!     Note: Number of values is determined by the minimum size of all
!           required array arguments.
!
! !SEE ALSO: 
!     toHVar_Inq    - returns useful parameters
!     toHVar_wMask  - convert obs to (or from) variable for given
!                     kt's and masks
!
! !REVISION HISTORY:
!     09Aug2002  C. Redder  Original code.
!     03Sep2002  C. Redder  Changes in the prologue
!     21Oct2003  C. Redder  Removed output parameter, rstat
!     03Apr2004  C. Redder  Added the arguments, kt0 (required) and
!                           SVP_Eqtn (optional).  Renamed the argument
!                           from toRH to backwd (optional). Renamed the
!                           routine from toRH_ to toHVar_.
! EOP
!....................................................................

      call toHVar_wMask ( P, T, kt, (/.true./), kt0, Obs,
     .                    backwd   = backwd,
     .                    SVP_Eqtn = SVP_Eqtn )

      return
      end subroutine toHVar_
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: ConvertH_byObList -- Convert from humidity variable to another by ob list
!
! !DESCRIPTION:
!     This routine converts from one humidity variable to another as
!     specified by the data type index (kt).  Only data specified on
!     the given list will be processed.
!
! !INTERFACE:
      subroutine ConvertH_byObList ( ObList, P, T,   ! Required and ...
     .                               kt_in,  kt_out, Obs,
     .                               SVP_Eqtn )      ! ... optional
      use m_humidity, only :  MRtoVP,  VPtoMR,       !   parameters
     .                        DPtoVP,  VPtoDP,
     .                        RHtoVP,  VPtoRH,
     .                        SHtoVP,  VPtoSH
      use m_VaiUtil,  only : RHTtoDP, DPTtoRH
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,         intent (in)    ::
     .   ObList (:)  ! List of observation by array index number
      real,            intent (in)    ::
     .   P      (:), ! Pressure    (hPa or mb)
     .   T      (:)  ! Temperature (deg K)
      integer,         intent (in)    ::
     .   kt_in,      ! The specified data type index for all data on input
     .   kt_out      ! ... and output data. (Use zero for vapor pressure)
      integer,         intent (in), optional ::
     .   SVP_Eqtn    ! Equation used to compute satuation vapor pressure.
                     !   See module, m_humidity, for default and further
!                    !   details.
! !INPUT/OUTPUT PARAMETERS: 
      real,             intent (inout) ::
     .   Obs    (:)  ! Observed values
!
!     Note: Number of values is determined by the size of ObList
!
!     Note: See the module header for the list of recognized
!           humidity variables
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     21Oct2003  C. Redder  Original code.
!     03Apr2004  C. Redder  Replaced input arguments kt0 and fromRH with
!                           kt_in, kt_out and SVP_Eqtn.  Renamed the 
!                           routine from toRH_byObList to ConvertH_byObList.
!                           The routine is now generalize to convert
!                           between all recognized humidity variables
!     20Nov2006  C. Redder  Modify the code to handle the special case
!                           when Vaisala formulae are involved.     
!     03May2007  C. Redder  Fixed bug regarding the processing of
!                           Vaisala data.
! EOP
!....................................................................

      integer, parameter :: ScrSz = 255
      real, dimension ( ScrSz ) :: Ps, Ts, Hs, VP, SVP
      integer :: NObs, NSeg, iSeg, iSegBeg, iSegEnd, SegSz, iSegPt, iPt
      logical :: SVP_required, SVP_Eqtn_selected, Vai_Eqtn_selected

      NObs = size ( ObList )      
      if ( NObs  .eq. 0 .or.           ! No obs to convert
     .     kt_in .eq. kt_out ) return  ! Specified variables are identical

      SVP_Eqtn_selected    =  present ( SVP_Eqtn )

!     Implement special case for converting RH to DP
!     or vice versa using the formulae from Vaisala
!     -----------------------------------------------
      Vai_Eqtn_selected = .false.
      if ( SVP_Eqtn_selected       ) then
      if ( SVP_Eqtn .eq. wEqtn_Vai ) then
!!         commented out in response to the elimation of proprietary
!!         code in the module, m_VaiUtil.f
!!         Vai_Eqtn_selected = kt_in    .eq. Relative_Humidity .and.
!!     .                       kt_out   .eq. Dew_Point         .or.
!!     .                       kt_in    .eq. Dew_Point         .and.
!!     .                       kt_out   .eq. Relative_Humidity
         SVP_Eqtn_selected = Vai_Eqtn_selected
      end if
      end if

!     Determine if saturation vapor pressure is required
!     --------------------------------------------------
      SVP_required = kt_in  .eq. Relative_Humidity .or.
     .               kt_out .eq. Relative_Humidity

!     Process the listed obs in segments
!     ----------------------------------
      NSeg    = ( NObs - 1 ) / ScrSz + 1
      iSegBeg = 1
      do iSeg = 1, NSeg
         iSegEnd = min ( iSegBeg + ScrSz - 1, NObs )
         SegSz   = iSegEnd - iSegBeg + 1

!        Gather the values to be processed into the scratch space
!        --------------------------------------------------------
         do iSegPt = 1, SegSz
            iPt           = ObList ( iSegBeg + iSegPt - 1 )
            Ps ( iSegPt ) = P      ( iPt )
            Ts ( iSegPt ) = T      ( iPt )
            Hs ( iSegPt ) = Obs    ( iPt )
         end do

!        ... convert for the special casee mentioned above
!        -------------------------------------------------
         if ( Vai_Eqtn_selected ) then
            if      ( kt_in  .eq. Relative_Humidity  .and.
     .                kt_out .eq. Dew_Point         ) then
               call   RHTtoDP(( Hs ( : SegSz )),
     .                          Ts ( : SegSz ),
     .                          Hs ( : SegSz ))
            else if ( kt_in  .eq. Dew_Point          .and.
     .                kt_out .eq. Relative_Humidity ) then
               call   DPTtoRH(( Hs ( : SegSz )),
     .                          Ts ( : SegSz ),
     .                          Hs ( : SegSz ))
            end if

!        ... or ...
!        ----------
         else

!           ... calculate the saturation vapor pressure (if necessary)
!           ----------------------------------------------------------
            if    ( SVP_required      ) then
               if ( SVP_Eqtn_selected ) then
                  call DPtoVP ( Ts  ( : SegSz ),
     .                          SVP ( : SegSz ), Eqtn = SVP_Eqtn ) 
               else
                  call DPtoVP ( Ts  ( : SegSz ),
     .                          SVP ( : SegSz )) 
               end if
            end if

!           ... convert from the variable, kt_in, to vapor pressure
!           -------------------------------------------------------
            if      ( kt_in  .eq. Mixing_Ratio      ) then
               call    MRtoVP ( Hs  ( : SegSz ),
     .                          Ps  ( : SegSz ),
     .                          VP  ( : SegSz )) 

            else if ( kt_in  .eq. Dew_Point         ) then
               if ( SVP_Eqtn_selected ) then
                  call DPtoVP ( Hs  ( : SegSz ),
     .                          VP  ( : SegSz ), Eqtn = SVP_Eqtn ) 
               else
                  call DPtoVP ( Hs  ( : SegSz ),
     .                          VP  ( : SegSz )) 
               end if
            else if ( kt_in  .eq. Relative_Humidity ) then
               call    RHtoVP ( Hs  ( : SegSz ),
     .                          SVP ( : SegSz ),
     .                          Ps  ( : SegSz ),
     .                          VP  ( : SegSz ))

            else if ( kt_in  .eq. Specific_Humidity ) then
               call    SHtoVP ( Hs  ( : SegSz ),
     .                          Ps  ( : SegSz ),
     .                          VP  ( : SegSz )) 

            else if ( kt_in  .eq. Vapor_Pressure    ) then
               do iSegPt = 1, SegSz
                  VP ( iSegPt ) = Hs ( iSegPt )
               end do

            end if

!           ... convert vapor pressure to the variable, kt_out
!           --------------------------------------------------
            if      ( kt_out .eq. Mixing_Ratio      ) then
               call    VPtoMR ( VP  ( : SegSz ),
     .                          Ps  ( : SegSz ),
     .                          Hs  ( : SegSz )) 

            else if ( kt_out .eq. Dew_Point         ) then
               if ( SVP_Eqtn_selected ) then
                  call VPtoDP ( VP  ( : SegSz ),
     .                          Hs  ( : SegSz ), Eqtn = SVP_Eqtn ) 
               else
                  call VPtoDP ( VP  ( : SegSz ),
     .                          Hs  ( : SegSz )) 
               end if

            else if ( kt_out .eq. Relative_Humidity ) then
               call    VPtoRH ( VP  ( : SegSz ),
     .                          SVP ( : SegSz ),
     .                          Ps  ( : SegSz ),
     .                          Hs  ( : SegSz ))

            else if ( kt_out .eq. Specific_Humidity ) then
               call    VPtoSH ( VP  ( : SegSz ),
     .                          Ps  ( : SegSz ),
     .                          Hs  ( : SegSz )) 

            else if ( kt_out .eq. Vapor_Pressure    ) then
               do iSegPt = 1, SegSz
                  Hs ( iSegPt ) = VP ( iSegPt )
               end do

            end if
         end if

!        Scatter the converted values back to input/output array
!        -------------------------------------------------------
         do iSegPt = 1, SegSz
            iPt         = ObList ( iSegBeg + iSegPt - 1 )
            Obs ( iPt ) = Hs     ( iSegPt )
         end do

!        Locate next segment
!        -------------------
         iSegBeg = iSegEnd + 1
      end do

      return
      end subroutine ConvertH_byObList

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: toTVar_wMkt1 -- Convert obs to/from a specified temperature variable with masking and a given kt
!
! !DESCRIPTION:
!     This routine converts temperature observations to/from a variable,
!     specified by the data type index, kt_in.  Any observation report
!     masked out is ignored.  If kt_in or kt_out are invalid then no
!     conversions are made.
!
! !INTERFACE:
      subroutine toTVar_wMkt1 ( P, Mask, kt_in, kt_out, Obs )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,         intent (in)    ::
     .   P    (:) ! Pressure    (hPa or mb)
      logical,      intent (in)    ::
     .   Mask (:) ! = .true./.false. to mask in/out (i.e. accept/reject)
      integer,          intent (in)    ::
     .   kt_in,   ! Data type index for input data
     .   kt_out   ! ...and output data (Use zero for vapor pressure.)
!
! !INPUT/OUTPUT PARAMETERS: 
      real,         intent (inout) ::
     .   Obs  (:) ! Observed values
!
!     Note: Number of values is determined by the minimum size of all
!           required array arguments (except Mask).
!
!     Note: See the module header for the list of recognized
!           temperature variables
!
! !SEE ALSO: 
!     toTVar_    - convert obs to (or from) temperature var for given kt's
!
! !REVISION HISTORY:
!     21Mar2007  C. Redder  Original code.
! EOP
!....................................................................

      integer, parameter :: ScrSz = 255
      integer :: ObList   ( ScrSz ), NTObs, NObs, iOb, NMask
      logical :: masked_in, last_mask

!     Nothing to do if kt_in and kt_out are the same
!     ----------------------------------------------
      if ( kt_in .eq. kt_out ) return

!     Determine the number of observations
!     ------------------------------------
      NTObs = min ( size ( P    ), size ( Obs )) ! ... observations
      NMask = min ( size ( Mask ), NTObs )       ! ... masks
      if ( NTObs .le. 0 ) return                 ! Nothing to do

      last_mask = .true.
      if ( NMask .gt. 0 ) last_mask = Mask ( NMask )

!     Process all obs
!     ---------------
      NObs = 0
      do iOb = 1, NTObs

!        Determine if ob is to be processed
!        ----------------------------------
         masked_in = last_mask
         if ( iOb .le. NMask ) masked_in = Mask ( iOb )

!        If ob is to precessed 
!        ---------------------
         if ( masked_in ) then

!           then, if there is room to add another list entry
!           ------------------------------------------------
            if ( NObs .lt. ScrSz ) then

              ! ... then add to the list
!             --------------------------
               NObs = NObs + 1

            else ! ... process the listed obs and clean the scratch
!           ------------------------------------------------------- 
               call ConvertT_byObList ( ObList, P, kt_in, kt_out, Obs )
               NObs = 1
            end if

!           Save entry in list
!           ------------------
            ObList ( NObs ) = iOb
         end if
      end do

!     Process all obs that are remaining on the list
!     ----------------------------------------------
      if ( NObs .gt. 0 )
     .   call ConvertT_byObList ( ObList ( : NObs ),
     .                            P, kt_in, kt_out, Obs )

      return
      end subroutine toTVar_wMkt1
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: toTVar_wMask -- Convert obs to/from a specified temperature variable with masking and given kt's
!
! !DESCRIPTION:
!     This routine converts temperature observations to/from a variable,
!     specified by the data type index, kt0.  Any observation report
!     masked out or without a recognized temperature kt is ignored.
!
! !INTERFACE:
      subroutine toTVar_wMask ( P, kt, Mask, kt0, Obs, ! Required and
     .                          backwd )               ! ... optional
!                                                      !   parameters
! !INPUT PARAMETERS: 
      implicit   NONE
      real,         intent (in)    ::
     .   P    (:) ! Pressure    (hPa or mb)
      integer,      intent (in)    ::
     .   kt   (:) ! Data type index.
      logical,      intent (in)    ::
     .   Mask (:) ! = .true./.false. to mask in/out (i.e. accept/reject)
      integer,      intent (in)    ::
     .   kt0      ! The specified temperature variable.
      logical,      intent (in), optional ::
     .   backwd   ! = .true. to convert backwards from the variable
                  !   specified by kt0.  Default: backwd = .false.
!
! !INPUT/OUTPUT PARAMETERS: 
      real,         intent (inout) ::
     .   Obs  (:) ! Observed values
!
!     Note: Number of values is determined by the minimum size of all
!           required array arguments (except Mask).
!
!     Note: See the module header for the list of recognized
!           temperature variables
!
! !SEE ALSO: 
!     toTVar_    - convert obs to (or from) temperature var for given kt's
!
! !REVISION HISTORY:
!     09Aug2002  C. Redder  Original code.
!     03Sep2002  C. Redder  Removed the required input arguments, T and RH.
!                           Removed the entry for the layer mean virtual
!                           temperature in the list of valid kts
!     21Feb2003  C. Redder  Added temporary storage for Obs
!     21Oct2003  C. Redder  Removed required parameter, rstat.  Major
!                           changes in the internal code to use scratch
!                           space of fix size (i.e. remove allocate
!                           statements) and to remove calls to external
!                           sort routines.  Removed the optional
!                           arguments, ktAT, nkt, ktList.
!     03Apr2004  C. Redder  Added the arguments, kt0 (required).  Made
!                           Made changes in response to interface
!                           modifictions to the routine, ConvertT_byObList.
!                           Renamed the optional argument, fromAT, to
!                           backwd and the routine from toAT_wMask to
!                           toTVar_wMask.
!     12May2004  C. Redder  Fixed bug to prevent non-humdity data from
!                           being processed and to prevent the dummy
!                           argument from being accessed when not
!                           associated.
! EOP
!....................................................................

      integer, parameter :: ScrSz = 255
      integer :: ObLists    ( TempListSz, ScrSz ),
     .           ObList                 ( ScrSz ),
     .           kt_NObs    ( TempListSz ),
     .           kt_Table   ( ktMin : ktMax ), NTObs, NObs, iOb,
     .           NMask, iOb_kt, kt_in, kt_out, ikt, iList
      logical :: masked_in, last_mask, from_kt0

!     Determine the number of observations
!     ------------------------------------
      NTObs = min ( size ( P   ), size ( kt ), ! ... observations
     .              size ( Obs ))
      NMask = min ( size ( Mask ), NTObs )     ! ... masks
      if ( NTObs .le. 0 ) return               ! Nothing to do

!     Impliment optiona
!     -----------------
      from_kt0 = .false.
      if ( present ( backwd ))  from_kt0 = backwd

!     Initialize internal variables
!     -----------------------------
      do ikt = ktMin, ktMax
         kt_Table ( ikt ) = 0
      end do
      do iList = 1, TempListSz
         kt_Table ( kt_TempList ( iList )) = iList
         kt_NObs  ( iList ) = 0
      end do

      last_mask = .true.
      if ( NMask .gt. 0 ) last_mask = Mask ( NMask )

!     Process all obs
!     ---------------
      do iOb = 1, NTObs
         ikt       = kt   ( iOb )
         iList     = 0
         if ( ikt .ge. ktMin .and.
     .        ikt .le. ktMax ) iList = kt_Table ( ikt )

!        Determine if ob is to be processed
!        ----------------------------------
         masked_in = last_mask
         if ( iOb .le. NMask ) masked_in = Mask ( iOb )
         masked_in = masked_in      .and.
     .               kt0   .ne. ikt .and.
     .               iList .gt. 0 

!        If ob is to precessed 
!        ---------------------
         if ( masked_in ) then
            NObs = kt_NObs ( iList )

!           then, if there is room to add another list entry for given kt
!           -------------------------------------------------------------
            if ( NObs .lt. ScrSz ) then

                 ! ... then add to the list
!                --------------------------
               NObs = NObs + 1

            else ! ... process the listed obs and clean the scratch
!           ------------------------------------------------------- 
               do iOb_kt = 1, NObs
                  ObList ( iOb_kt ) = ObLists ( iList, iOb_kt )
               end do
               if ( from_kt0 ) then
                  kt_in  = kt0
                  kt_out = kt_TempList ( iList )
               else
                  kt_in  = kt_TempList ( iList )
                  kt_out = kt0
               end if
               call ConvertT_byObList ( ObList, P, kt_in, kt_out, Obs )
               NObs = 1
            end if

!           Save entry in list
!           -----------------
            ObLists ( iList, NObs ) = iOb
            kt_NObs ( iList )       = NObs
         end if
      end do

!     For each valid kt, process all obs
!     that are remaining on the list
!     ----------------------------------
      do iList = 1, TempListSz
         if ( from_kt0 ) then
            kt_in  = kt0
            kt_out = kt_TempList ( iList )
         else
            kt_in  = kt_TempList ( iList )
            kt_out = kt0
         end if
         NObs = kt_NObs    ( iList )
         if ( NObs .gt. 0 .and. kt_in .ne. kt_out ) then
            do iOb_kt = 1, NObs
               ObList ( iOb_kt ) = ObLists ( iList, iOb_kt )
            end do
            call ConvertT_byObList ( ObList ( : NObs ),
     .                               P, kt_in, kt_out, Obs )
         end if
      end do

      return
      end subroutine toTVar_wMask
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: toTVar_ -- Convert obs to/from a specified temperature variable given kt's
!
! !DESCRIPTION:
!     This routine converts observations to/from a temperature variable,
!     specified by the data type index, kt0.  Any observation report
!     without a recognized temperature kt is ignored.
!
! !INTERFACE:
      subroutine toTVar_ ( P, kt, kt0, Obs, backwd )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,             intent (in)    ::
     .   P       (:)  ! Pressure    (hPa or mb)
      integer,          intent (in)    ::
     .   kt      (:)  ! Data type index.
      integer,          intent (in)    ::
     .   kt0          ! The specified temperature variable
      logical,          intent (in), optional ::
     .   backwd       ! = .true. to convert backwards from the variable
                      !   specified by kt0.  Default: backwd = .false.
! !INPUT/OUTPUT PARAMETERS: 
      real,             intent (inout) ::
     .   Obs     (:)  ! Observed values
!
!     Note: Number of values is determined by the minimum size of all
!           required array arguments.
!
! !SEE ALSO: 
!     toTVar_wMask  - convert obs to (or from) temperature variable for
!                     given kt's and masks
!
! !REVISION HISTORY:
!     09Aug2002  C. Redder  Original code.
!     03Sep2002  C. Redder  Changes in the prologue
!     21Oct2003  C. Redder  Removed output parameter, rstat
!     03Apr2004  C. Redder  Added the arguments, kt0 (required).  Renamed
!                           the argument from toAT to backwd (optional).
!                           Renamed the routine from toAT_ to toTVar_.
! EOP
!....................................................................

      call toTVar_wMask ( P, kt, (/.true./), kt0, Obs,
     .                    backwd = backwd )

      return
      end subroutine toTVar_

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: ConvertT_byObList -- Convert from temperature variable to another by ob list
!
! !DESCRIPTION:
!     This routine converts from one temperature variable to another as
!     specified by the data type index (kt).  Only data specified on
!     the on the given will be processed.
!
! !INTERFACE:
      subroutine ConvertT_byObList ( ObList, P,
     .                               kt_in,  kt_out, Obs )
      use m_temperature, only :  PTtoAT, ATtoPT
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,         intent (in)    ::
     .   ObList (:)  ! List of observation by array index number
      real,            intent (in)    ::
     .   P      (:)  ! Pressure    (hPa or mb)
      integer,         intent (in)    ::
     .   kt_in,      ! The specified data type index for all data on input
     .   kt_out      ! ... and output data.
!
! !INPUT/OUTPUT PARAMETERS: 
      real,             intent (inout) ::
     .   Obs    (:)  ! Observed values
!
!     Note: Number of values is determined by the size of ObList
!
!     Note: See the module header for the list of recognized
!           temperature variables
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     21Oct2003  C. Redder  Original code.
!     03Apr2004  C. Redder  Replaced input arguments kt0 and fromRH with
!                           kt_in, kt_out.  Renamed the routine from
!                           toAT_byObList to ConvertT_byObList.  The
!                           routine is now generalize to convert
!                           between all recognized temperature variables.
! EOP
!....................................................................

      integer, parameter :: ScrSz = 255
      real, dimension ( ScrSz ) :: Ps, Ts, AT
      integer :: NObs, NSeg, iSeg, iSegBeg, iSegEnd, SegSz, iSegPt, iPt

      NObs = size ( ObList )      
      if ( NObs  .eq. 0 .or.           ! No obs to convert
     .     kt_in .eq. kt_out ) return  ! Specified variables are identical

!     Process the listed obs in segments
!     ----------------------------------
      NSeg    = ( NObs - 1 ) / ScrSz + 1
      iSegBeg = 1
      do iSeg = 1, NSeg
         iSegEnd = min ( iSegBeg + ScrSz - 1, NObs )
         SegSz   = iSegEnd - iSegBeg + 1

!        Gather the values to be processed into the scratch space
!        --------------------------------------------------------
         do iSegPt = 1, SegSz
            iPt           = ObList ( iSegBeg + iSegPt - 1 )
            Ps ( iSegPt ) = P      ( iPt )
            Ts ( iSegPt ) = Obs    ( iPt )
         end do

!        ... convert from the variable, kt_in, to air temperature
!        --------------------------------------------------------
         if      ( kt_in  .eq. Potential_Temperature ) then
            call PTtoAT ( Ts  ( : SegSz ),
     .                    Ps  ( : SegSz ),
     .                    AT  ( : SegSz )) 

         else if ( kt_in  .eq. Air_Temperature       ) then
            do iSegPt = 1, SegSz
               AT ( iSegPt ) = Ts ( iSegPt )
            end do

         end if

!        ... convert from air_temperature to the variable, kt_out
!        --------------------------------------------------------
         if      ( kt_out .eq. Potential_Temperature ) then
            call ATtoPT ( AT  ( : SegSz ),
     .                    Ps  ( : SegSz ),
     .                    Ts  ( : SegSz )) 

         else if ( kt_out .eq. Air_Temperature       ) then
            do iSegPt = 1, SegSz
               Ts ( iSegPt ) = AT ( iSegPt )
            end do

         end if

!        Scatter the converted values back to input/output array
!        -------------------------------------------------------
         do iSegPt = 1, SegSz
            iPt         = ObList ( iSegBeg + iSegPt - 1 )
            Obs ( iPt ) = Ts     ( iSegPt )
         end do

!        Locate next segment
!        -------------------
         iSegBeg = iSegEnd + 1
      end do

      return
      end subroutine ConvertT_byObList

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: TtoVT_wMask -- Convert between temperature and virtual temperature with masking
!
! !DESCRIPTION:
!     This routine converts from (or to) a temperature (T) to (or from) a
!     virtual temperature (VT) variable for all observation reports with a
!     valid data type index (kt).  Any observation report masked out or
!     without a valid kt is ignored.
!
! !INTERFACE:
      subroutine TtoVT_wMask ( P, MR, kt, Mask, kt0, Obs, ! Required and
     .                         backwd, VT_Eqtn )          ! ... optional
!                                                         !   parameters
! !INPUT PARAMETERS: 
      implicit   NONE
      real,            intent (in)    ::
     .   P      (:), ! Pressure    (hPa or mb)
     .   MR     (:)  ! Mixing ration (g/kg)
      integer,         intent (in)    ::
     .   kt     (:)  ! Data type index.
      logical,         intent (in)    ::
     .   Mask   (:)  ! = .true./.false. to mask in/out (i.e. accept/reject)
      integer,         intent (in)    ::
     .   kt0         ! The specified temperature variable
      logical,         intent (in), optional ::
     .   backwd      ! = .true. to convert from a VT variable.  Default:
                     !   backwd = .false.
      integer,         intent (in), optional ::
     .   VT_Eqtn     ! Equation used to compute the virtual temperature.
                     !   See module, m_temperature, for default and
!                    !   further details.
! !INPUT/OUTPUT PARAMETERS: 
      real,             intent (inout) ::
     .   Obs    (:)  ! Observed values
!
!     Note: Number of values is determined by the minimum size of all
!           required array arguments  (except Mask).
!
!     Note: See the module header for the list of recognized
!           T and VT variables
!
! !SEE ALSO: 
!     TtoVT_ - convert obs to (or from) VT variable for given kt's
!
! !REVISION HISTORY:
!     20Apr2004  C. Redder  Original code.
!     12May2004  C. Redder  Fixed bug to prevent non-temperature data
!                           from being processed and to prevent the dummy
!                           argument from being accessed when not
!                           associated.
! EOP
!....................................................................

      integer, parameter :: ScrSz = 255
      integer :: ObLists    ( VTempListSz, ScrSz ),
     .           ObList                  ( ScrSz ),
     .           kt_NObs    ( VTempListSz ),
     .           kt_Table   ( ktMin : ktMax ), NTObs, NObs, iOb,
     .           NMask, iOb_kt, kt_in, kt_out, ikt, iList
      logical :: masked_in, last_mask, from_kt0

!     Determine the number of observations
!     ------------------------------------
      NTObs = min ( size ( P  ), size ( MR  ), ! ... observations
     .              size ( kt ), size ( Obs ))
      NMask = min ( size ( Mask ), NTObs )     ! ... masks
      if ( NTObs .le. 0 ) return               ! Nothing to do

!     Impliment optiona
!     -----------------
      from_kt0 = .false.
      if ( present ( backwd ))  from_kt0 = backwd

!     Initialize internal variables
!     -----------------------------
      do ikt = ktMin, ktMax
         kt_Table ( ikt ) = 0
      end do
      do iList = 1, VTempListSz
         kt_Table ( kt_VTempList ( iList )) = iList
         kt_NObs  ( iList ) = 0
      end do

      last_mask = .true.
      if ( NMask .gt. 0 ) last_mask = Mask ( NMask )

!     Process all obs
!     ---------------
      do iOb = 1, NTObs
         ikt       = kt   ( iOb )
         iList     = 0
         if ( ikt .ge. ktMin .and.
     .        ikt .le. ktMax ) iList = kt_Table ( ikt )

!        Determine if ob is to be processed
!        ----------------------------------
         masked_in = last_mask
         if ( iOb .le. NMask ) masked_in = Mask ( iOb )
         masked_in = masked_in      .and.
     .               kt0   .ne. ikt .and.
     .               iList .gt. 0 

!        If ob is to precessed 
!        ---------------------
         if ( masked_in ) then
            NObs = kt_NObs ( iList )

!           then, if there is room to add another list entry for given kt
!           -------------------------------------------------------------
            if ( NObs .lt. ScrSz ) then

                 ! ... then add to the list
!                --------------------------
               NObs = NObs + 1

            else ! ... process the listed obs and clean the scratch
!           ------------------------------------------------------- 
               do iOb_kt = 1, NObs
                  ObList ( iOb_kt ) = ObLists ( iList, iOb_kt )
               end do
               if ( from_kt0 ) then
                  kt_in  = kt0
                  kt_out = kt_VTempList ( iList )
               else
                  kt_in  = kt_VTempList ( iList )
                  kt_out = kt0
               end if
               call ComputeVT_byObList ( ObList, P, MR,
     .                                   kt_in,  kt_out, Obs,
     .                                   VT_Eqtn = VT_Eqtn )
               NObs = 1
            end if

!           Save entry in list
!           -----------------
            ObLists ( iList, NObs ) = iOb
            kt_NObs ( iList )       = NObs
         end if
      end do

!     For each valid kt, process all obs
!     that are remaining on the list
!     ----------------------------------
      do iList = 1, VTempListSz
         if ( from_kt0 ) then
            kt_in  = kt0
            kt_out = kt_VTempList ( iList )
         else
            kt_in  = kt_VTempList ( iList )
            kt_out = kt0
         end if
         NObs = kt_NObs    ( iList )
         if ( NObs .gt. 0 .and. kt_in .ne. kt_out ) then
            do iOb_kt = 1, NObs
               ObList ( iOb_kt ) = ObLists ( iList, iOb_kt )
            end do
            call ComputeVT_byObList ( ObList ( : NObs ),
     .                                P, MR, kt_in, kt_out, Obs,
     .                                VT_Eqtn = VT_Eqtn )
         end if
      end do

      return
      end subroutine TtoVT_wMask

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: TtoVT_ -- Convert between temperature and virtual temperature with masking
!
! !DESCRIPTION:
!     This routine converts from (or to) a temperature (T) to (or from) a
!     virtual temperature (VT) variable for all observation reports with a
!     valid data type index (kt).  Any observation report without a valid
!     kt is ignored.
!
! !INTERFACE:
      subroutine TtoVT_ ( P, MR, kt, kt0, Obs, backwd, VT_Eqtn )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,             intent (in)    ::
     .   P       (:), ! Pressure    (hPa or mb)
     .   MR      (:)  ! Mixing ratio (g/kg)
      integer,          intent (in)    ::
     .   kt      (:)  ! Data type index.
      integer,          intent (in)    ::
     .   kt0          ! The specified temperature variable
      logical,          intent (in), optional ::
     .   backwd       ! = .true. to convert backwards from the variable
                      !   specified by kt0.  Default: backwd = .false.
      integer,          intent (in), optional ::
     .   VT_Eqtn      ! Equation used to compute the virtual temperature.
                      !   See module, m_temperature, for default and
!                     !   further details.
! !INPUT/OUTPUT PARAMETERS: 
      real,             intent (inout) ::
     .   Obs     (:)  ! Observed values
!
!     Note: Number of values is determined by the minimum size of all
!           required array arguments.
!
! !SEE ALSO: 
!     TtoVT_wMask  - convert obs to (or from) temperature variable for
!                    given kt's and masks
!
! !REVISION HISTORY:
!     20Apr2004  C. Redder  Original code.
!
! EOP
!....................................................................

      call TtoVT_wMask ( P, MR, kt, (/.true./), kt0, Obs,
     .                   backwd  = backwd,
     .                   VT_Eqtn = VT_Eqtn )

      return
      end subroutine TtoVT_

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: ComputeVT_byObList -- Convert from temperature variable to another by ob list
!
! !DESCRIPTION:
!     This routine converts from one temperature variable to another as
!     specified by the data type index (kt).  Only data specified on
!     the on the given will be processed.
!
! !INTERFACE:
      subroutine ComputeVT_byObList ( ObList, P, MR,        ! Required and ...
     .                                kt_in,  kt_out, Obs,
     .                                VT_Eqtn )             ! ... optional
      use m_temperature, only :  ATtoVT, VTtoAT,            !   parameters
     .                           ATtoPT, PTtoAT
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,         intent (in)    ::
     .   ObList (:)  ! List of observation by array index number
      real,            intent (in)    ::
     .   P      (:), ! Pressure    (hPa or mb)
     .   MR     (:)  ! Mixing ratio (g/kg)
      integer,         intent (in)    ::
     .   kt_in,      ! The specified data type index for all data on input
     .   kt_out      ! ... and output data.
      integer,         intent (in), optional ::
     .   VT_Eqtn     ! Equation used to compute the virtual temperature.
                     !   See module, m_temperature, for default and
!                    !   further details.
! !INPUT/OUTPUT PARAMETERS: 
      real,             intent (inout) ::
     .   Obs    (:)  ! Observed values
!
!     Note: Number of values is determined by the size of ObList
!
!     Note: See the module header for the list of recognized
!           temperature variables
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     20Apr2004  C. Redder  Original code.
! EOP
!....................................................................

      integer, parameter :: ScrSz = 255
      real, dimension ( ScrSz ) :: Ps, MRs, Ts, AT
      integer :: NObs, NSeg, iSeg, iSegBeg, iSegEnd, SegSz, iSegPt, iPt

      NObs = size ( ObList )      
      if ( NObs  .eq. 0 .or.           ! No obs to convert
     .     kt_in .eq. kt_out ) return  ! Specified variables are identical

!     Process the listed obs in segments
!     ----------------------------------
      NSeg    = ( NObs - 1 ) / ScrSz + 1
      iSegBeg = 1
      do iSeg = 1, NSeg
         iSegEnd = min ( iSegBeg + ScrSz - 1, NObs )
         SegSz   = iSegEnd - iSegBeg + 1

!        Gather the values to be processed into the scratch space
!        --------------------------------------------------------
         do iSegPt = 1, SegSz
            iPt            = ObList ( iSegBeg + iSegPt - 1 )
            Ps  ( iSegPt ) = P     ( iPt )
            MRs ( iSegPt ) = MR    ( iPt )
            Ts  ( iSegPt ) = Obs   ( iPt )
         end do

!        ... convert from the variable, kt_in, to air temperature
!        --------------------------------------------------------
         if      ( kt_in  .eq. Potential_Temperature ) then
            call PTtoAT ( Ts  ( : SegSz ),
     .                    Ps  ( : SegSz ),
     .                    AT  ( : SegSz )) 

         else if ( kt_in  .eq. Virtual_Temperature   ) then
            call VTtoAT ( Ts  ( : SegSz ),
     .                    MRs ( : SegSz ),
     .                    AT  ( : SegSz ),
     .                    Eqtn = VT_Eqtn )

         else if ( kt_in  .eq. Air_Temperature       ) then
            do iSegPt = 1, SegSz
               AT ( iSegPt ) = Ts ( iSegPt )
            end do

         end if

!        ... convert from air_temperature to the variable, kt_out
!        --------------------------------------------------------
         if      ( kt_out .eq. Potential_Temperature ) then
            call ATtoPT ( AT  ( : SegSz ),
     .                    Ps  ( : SegSz ),
     .                    Ts  ( : SegSz )) 

         else if ( kt_out .eq. Virtual_Temperature   ) then
            call ATtoVT ( AT  ( : SegSz ),
     .                    MRs ( : SegSz ),
     .                    Ts  ( : SegSz ),
     .                    Eqtn = VT_Eqtn )

         else if ( kt_out .eq. Air_Temperature       ) then
            do iSegPt = 1, SegSz
               Ts ( iSegPt ) = AT ( iSegPt )
            end do

         end if

!        Scatter the converted values back to input/output array
!        -------------------------------------------------------
         do iSegPt = 1, SegSz
            iPt         = ObList ( iSegBeg + iSegPt - 1 )
            Obs ( iPt ) = Ts     ( iSegPt )
         end do

!        Locate next segment
!        -------------------
         iSegBeg = iSegEnd + 1
      end do

      return
      end subroutine ComputeVT_byObList

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: ktMask -- Modifies mask by kt value
!
! !DESCRIPTION:
!     This routine modifies the given logical array by the expression,
!
!          Mask = Mask .and. in_kt_list
!
!     where in_kt_list is set to .true. if the corresponding kt is in
!     the list of valid kt's.  Otherwise in_kt_list is set to .false. 
!
! !INTERFACE:
      subroutine ktMask ( kt, Mask, var )
      use m_AdvError, only : WPErr, Alloc
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer,       intent (in)    ::
     .   kt   (:)  ! Data type indices
      integer,       intent (in), optional ::
     .   var       ! Desired variable type.  Valid choices are listed
                   !   in module header.  If choice is invalid, var
                   !   is set to the default, Temperature.
!
! !INPUT/OUTPUT PARAMETERS: 
      logical,       intent (inout) ::
     .   Mask (:)  ! Mask values
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     03Sep2003  C. Redder  Original code.
! EOP
!....................................................................

      logical :: valid_kt ( ktMin : ktMax ), a_valid_kt
      integer :: kt_List  ( MaxListSz ),
     .           NObs, iOb, ListSz, iList, ikt, var_

!     Nothing to do if there are no obs
!     ---------------------------------
      NObs  = min ( size ( kt ), size ( Mask ))
      if ( NObs .le. 0 ) return

!     Seclect variable type
!     ---------------------
      var_ = Temperature
      if ( present ( var )) var_ = var

!     Get list size of valid kt's and kt of desired variable
!     ------------------------------------------------------
      if      ( var_ .eq. Humidity ) then
         ListSz   = HumListSz
         do iList = 1, ListSz
            kt_List ( iList ) = kt_HumList  ( iList )
         end do
      else
         ListSz   = TempListSz
         do iList = 1, ListSz
            kt_List ( iList ) = kt_TempList ( iList )
         end do
      end if

!     Initialize internal variables
!     -----------------------------
      do ikt = ktMin, ktMax
         valid_kt ( ikt ) = .false.
      end do
      do iList = 1, ListSz
         valid_kt ( kt_List ( iList )) = .true.
      end do

!     Modify the masks
!     ----------------
      do iOb = 1, NObs
         ikt          =  kt ( iOb )
         a_valid_kt   = .false.
         if ( ikt   .ge. ktMin  .and.
     .        ikt   .le. ktMax ) a_valid_kt = valid_kt ( ikt )
         Mask ( iOb ) =  Mask ( iOb ) .and. a_valid_kt
      end do

      return
      end subroutine ktMask
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: toVarkt -- Replaces data type indices
!
! !DESCRIPTION:
!     This routine replaces valid data type indicies for a 
!     with the data type index for a given variable.
!
! !INTERFACE:
      subroutine toVarkt ( kt, var )
      use m_AdvError, only : WPErr, Alloc
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer,       intent (in), optional ::
     .   var       ! Desired variable type.  Valid choices are listed
                   !   in module header.  If choice is invalid, var
                   !   is set to the default, Temperature.
!
! !INPUT/OUTPUT PARAMETERS: 
      integer,       intent (inout) ::
     .   kt   (:)  ! Data type indices
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     03Sep2002  C. Redder  Original code.
!     21Oct2003  C. Redder  Removed the require output argument,
!                           rstat, and internal error handling.
! EOP
!....................................................................

      integer :: kt_Table ( ktMin : ktMax ),
     .           kt_List  ( MaxListSz ),
     .           NObs, iOb, ListSz, iList, ikt, var_

!     Nothing to do if there are no obs
!     ---------------------------------
      NObs  = size ( kt )
      if ( NObs .le. 0 ) return

!     Seclect variable type
!     ---------------------
      var_ = Temperature
      if ( present ( var )) var_ = var

!     Get list size of valid kt's and kt of desired variable
!     ------------------------------------------------------
      if      ( var_ .eq. Humidity ) then
         ListSz   = HumListSz
         do iList = 1, ListSz
            kt_List ( iList ) = kt_HumList ( iList )
         end do
      else
         ListSz   = TempListSz
         do iList = 1, ListSz
            kt_List ( iList ) = kt_TempList ( iList )
         end do
      end if

!     Initialize internal variables
!     -----------------------------
      do ikt = ktMin, ktMax
         kt_Table ( ikt ) = 0
      end do
      do iList = 1, ListSz
         kt_Table ( kt_List ( iList )) = iList
      end do

!     Reset kt's of obs whose kt's are on the list
!     --------------------------------------------
      do iOb = 1, NObs
         ikt   = kt ( iOb )
         iList = 0
         if ( ikt   .ge. ktMin  .and.
     .        ikt   .le. ktMax ) iList  = kt_Table ( ikt )
         if ( iList .ne. 0 ) kt ( iOb ) = kt_List  ( 1 )
      end do

      return
      end subroutine toVarkt
!....................................................................
      end module m_convert
!====================================================================
