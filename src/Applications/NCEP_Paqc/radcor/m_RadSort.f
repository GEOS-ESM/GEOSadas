!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_RadSort -- Radcor sort routines
!
! !DESCRIPTION:
!
! !INTERFACE:
!
      module      m_RadSort
      implicit    NONE
      private   ! except

      public ::
     .   IndexSet,
     .   IndexList,
     .   IndexSort,
     .!   IndexMerge,
     .   BIndexSet,
     .   VerifySort,
     .   VerifyISort,
     .   NavInit,
     .   NavNum,
     .   R2ManExp

      interface IndexList
         module procedure
     .      IndexList_log1,  !
     .      IndexList_char1, !
     .      IndexList_int1,  !
     .      IndexList_intN,  !
     .      IndexList2_log1  !
      end interface
      interface IndexSort
         module procedure
     .      IndexSort_Int,
     .      IndexSort_Real,
     .      IndexSort_Char,
     .      IndexSort2_Int
      end interface
      !interface IndexMerge
      !   module procedure
     .!      IndexSort_Int,
     .!      IndexSort_Char
      !end interface
      interface NavInit
         module procedure
     .      NavInit_,
     .      NavInit_index
      end interface
      interface BIndexSet
         module procedure
     .      BIndexSet_scaler,
     .      BIndexSet_IKeys,
     .      BIndexSet_RKeys,
     .      BIndexSet_IIKeys,
     .      BIndexSet_IRKeys
      end interface
      interface VerifySort
         module procedure
     .      VerSort_integer,
     .      VerSort_Binteger,
     .      VerSort_real,
     .      VerSort_Breal
      end interface
      interface VerifyISort
         module procedure
     .      VerISort_integer,
     .      VerISort_Binteger,
     .      VerISort_real,
     .      VerISort_Breal
      end interface
      interface R2ManExp
         module procedure
     .      R2ManExp_,
     .      R2ManExp_Indx
      end interface
! 
! !REVISION HISTORY:
!     09Aug2002  C. Redder  Original code
!     21Oct2002  C. Redder  Added generic interface, IndexSort.
!     18Dec2002  C. Redder  Added module procedure, IndexSort_Real, to
!                           generic interface IndexSor.  Added generic
!                           interface, NavInit.
!     02Jan2003  C. Redder  Added the generic interfaces, VerifySort and
!                           VerifyISort.
!     31Jan2003  C. Redder  Added the generic interface, R2ManExp
!     06Jun2003  C. Redder  Added the generic interface, BIndexSet.
!                           Added the module procedures, VerSort_Binteger
!                           and VerSort_Breal to the generic interface,
!                           VerifySort and the module procedures, 
!                           VerSort_Binteger and VerSort_Breal to the 
!                           the interface VerifyISort.
!     12Aug2003  C. Redder  Added the module procedures, IndexList2_log1
!                           and IndexSort2_Int, to the generic interfaces,
!                           IndexList and IndexSort, resepctively.
!EOP
!-----------------------------------------------------------------
      character (len=*), parameter :: MyModule = 'm_RadSort'
      contains

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: IndexSet -- Initialize index array
!
! !INTERFACE:
      subroutine IndexSet ( Indx )
      implicit   NONE
!
! !OUTPUT PARAMETERS:
      integer,          intent (out) ::
     .   Indx    (:)  ! Sorting indices
!
!     Note: The size of the array, Indx, determines the number of keys.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     22Jul2002  C. Redder  Origional code.
!
!EOP
!....................................................................
      integer :: iKey, NKeys

      NKeys = size ( Indx )
      do iKey = 1, NKeys
         Indx ( iKey ) = iKey
      end do

      return
      end subroutine IndexSet

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: BIndexSet_scaler -- Initialize bin index array with scaler integer
!
! !INTERFACE:
      subroutine BIndexSet_scaler ( BIndx, Val )
      implicit   NONE
!
! !INPUT PARAMETERS:
      integer, optional, intent (in)  ::
     .   Val           ! Scaler integer.  Default: Val = 0
!
! !OUTPUT PARAMETERS:
      integer,           intent (out) ::
     .   BIndx   (:)   ! Bin indices
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     22Jul2002  C. Redder  Origional code.
!
!EOP
!....................................................................

      integer :: iIndx, NIndx, VVal

      VVal  = 0
      if ( present ( Val )) VVal = Val
      NIndx = size ( BIndx )
      do iIndx = 1, NIndx
         BIndx ( iIndx ) = VVal
      end do

      return
      end subroutine BIndexSet_scaler

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: BIndexSet_IKeys -- Initialize bin index array with sorted integer keys
!
! !INTERFACE:
      subroutine BIndexSet_IKeys ( BIndx, Keys )
      implicit   NONE
!
! !INPUT PARAMETERS:
      integer,           intent (in)    ::
     .   Keys    (:)   ! Sorted keys
!
! !INPUT/OUTPUT PARAMETERS:
      integer,           intent (inout) ::
     .   BIndx   (:)   ! Bin indices
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     22Jul2002  C. Redder  Origional code.
!
!EOP
!....................................................................

      integer :: iBin, iKey, NKeys, ThisKey, LastKey, ThisBin, LastBin

      NKeys = min ( size ( BIndx ), size ( Keys ))
      LastBin     = BIndx ( 1 )
      LastKey     = Keys  ( 1 )
      iBin        = 1
      BIndx ( 1 ) = iBin
      do iKey = 2, NKeys
         ThisBin  = BIndx ( iKey )
         ThisKey  = Keys  ( iKey )
         if ( ThisBin .ne. LastBin .or.
     .        ThisKey .ne. LastKey ) iBin = iBin + 1
         BIndx ( iKey ) = iBin
         LastBin  = ThisBin
         LastKey  = ThisKey
      end do

      return
      end subroutine BIndexSet_IKeys

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: BIndexSet_RKeys -- Initialize bin index array with sorted real keys
!
! !INTERFACE:
      subroutine BIndexSet_RKeys ( BIndx, Keys )
      implicit   NONE
!
! !INPUT PARAMETERS:
      real,              intent (in)    ::
     .   Keys    (:)   ! Sorted keys
!
! !INPUT/OUTPUT PARAMETERS:
      integer,           intent (inout) ::
     .   BIndx   (:)   ! Bin indices
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     22Jul2002  C. Redder  Origional code.
!
!EOP
!....................................................................

      integer :: iBin, iKey, NKeys, ThisKey, LastKey, ThisBin, LastBin

      NKeys = min ( size ( BIndx ), size ( Keys ))
      LastBin     = BIndx ( 1 )
      LastKey     = Keys  ( 1 )
      iBin        = 1
      BIndx ( 1 ) = iBin
      do iKey = 2, NKeys
         ThisBin  = BIndx ( iKey )
         ThisKey  = Keys  ( iKey )
         if ( ThisBin .ne. LastBin .or.
     .        ThisKey .ne. LastKey ) iBin = iBin + 1
         BIndx ( iKey ) = iBin
         LastBin  = ThisBin
         LastKey  = ThisKey
      end do

      return
      end subroutine BIndexSet_RKeys

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: BIndexSet_IIKeys -- Initialize bin index array with sorting indices and sorted integer keys
!
! !INTERFACE:
      subroutine BIndexSet_IIKeys ( BIndx, Indx, Keys )
      implicit   NONE
!
! !INPUT PARAMETERS:
      integer,           intent (in)    ::
     .   Indx    (:),  ! Sorting indices
     .   Keys    (:)   ! Keys
!
! !INPUT/OUTPUT PARAMETERS:
      integer,           intent (inout) ::
     .   BIndx   (:)   ! Bin indices. Array must be conformable to Keys
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     22Jul2002  C. Redder  Origional code.
!
!EOP
!....................................................................

      integer :: iBin, iKey, NKeys, ThisIndx, ThisKey, LastKey,
     .           ThisBin, LastBin

      NKeys = min ( size ( BIndx ), size ( Keys ))
      LastBin     = BIndx ( Indx ( 1 ))
      LastKey     = Keys  ( Indx ( 1 ))
      iBin        = 1
      BIndx ( 1 ) = iBin
      do iKey = 2, NKeys
         ThisIndx = Indx  ( iKey )
         ThisBin  = BIndx ( iKey )
         ThisKey  = Keys  ( ThisIndx )
         if ( ThisBin .ne. LastBin .or.
     .        ThisKey .ne. LastKey ) iBin = iBin + 1
         BIndx ( iKey ) = iBin
         LastBin  = ThisBin
         LastKey  = ThisKey
      end do

      return
      end subroutine BIndexSet_IIKeys

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: BIndexSet_IRKeys -- Initialize bin index array with sorting indices and sorted real keys
!
! !INTERFACE:
      subroutine BIndexSet_IRKeys ( BIndx, Indx, Keys )
      implicit   NONE
!
! !INPUT PARAMETERS:
      integer,           intent (in)    ::
     .   Indx    (:)   ! Sorting indices
      real,              intent (in)    ::
     .   Keys    (:)   ! Keys
!
! !INPUT/OUTPUT PARAMETERS:
      integer,           intent (inout) ::
     .   BIndx   (:)   ! Bin indices.  Array must be conformable to Keys
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     22Jul2002  C. Redder  Origional code.
!
!EOP
!....................................................................

      integer :: iBin, iKey, NKeys, ThisIndx, ThisKey, LastKey,
     .           ThisBin, LastBin

      NKeys = min ( size ( BIndx ), size ( Keys ))
      LastBin     = BIndx ( Indx ( 1 ))
      LastKey     = Keys  ( Indx ( 1 ))
      iBin        = 1
      BIndx ( 1 ) = iBin
      do iKey = 2, NKeys
         ThisIndx = Indx  ( iKey )
         ThisBin  = BIndx ( iKey )
         ThisKey  = Keys  ( ThisIndx )
         if ( ThisBin .ne. LastBin .or.
     .        ThisKey .ne. LastKey ) iBin = iBin + 1
         BIndx ( iKey ) = iBin
         LastBin  = ThisBin
         LastKey  = ThisKey
      end do

      return
      end subroutine BIndexSet_IRKeys

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
! BOP
!
! !IROUTINE: RankMerge_Int - A stable merge index sorting of integers
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine RankMerge_Int ( value_i, value_j, rank_i, rank_j )
      implicit none

! !INPUT PARAMETERS:
!
      integer, dimension(:), intent (in)    ::
     .   value_j,    ! value of j-vec
     .   value_i     ! value of i-vec
!
! !OUTPUT PARAMETERS:
      integer, dimension(:), intent (inout) ::
     .   rank_i,     ! rank of i-vec
     .   rank_j      ! rank of j-vec
!
! !REVISION HISTORY:
!     21Aug2003  C. Redder  02Jan2003 - C. Redder - Original code
!
! EOP
!-----------------------------------------------------------------------

      return
      end subroutine RankMerge_int
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: IndexList_log1 -- Sort logicals according to given value
!
! !DESCRIPTION:
!     This routine sorts (or bins) logical values according to a given
!     value.
!
! !INTERFACE:
      subroutine IndexList_log1 ( Indx, Keys, Key0, Len0, rstat )
      use m_AdvError, only : Alloc, WPErr, Arr
!
! !INPUT PARAMETERS: 
      implicit   NONE
      logical,          intent (in)    ::
     .   Keys    (:), ! Keys
     .   Key0         ! Given key
!
! !INPUT/OUTPUT PARAMETERS: 
      integer,          intent (inout) ::
     .   Indx    (:)  ! Sorting indices
!
! !OUTPUT PARAMETERS:
      integer,          intent (out)   ::
     .   Len0,        ! Length of set with the given value
     .   rstat        ! Returned status from the allocation statement
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     09Aug2002  C. Redder  Origional code.
!     24Oct2002  C. Redder  Corrected setting for the parameters string,
!                           MyName
!     01Nov2002  C. Redder  Corrected line that determines NKeys 
!     07Jan2003  C. Redder  Removed the internal variable istat
!     08Aug2003  C. Redder  Replaced the section that sorts with a call to
!                           to the external subroutine, IndexList2_log1. 
!EOP
!....................................................................

      logical :: Keyi
      integer :: NKeys, iKey, iBeg, iBegN
      integer, dimension (:), allocatable :: Indx2, LIndx
      character (len=*), parameter :: MyName = MyModule
     .                                     // '::IndexList_log1'

!     Default status is valid
!     -----------------------
      rstat = 0

!     Nothing number of keys is zero
!     ------------------------------
      NKeys = size ( Indx )
      if ( NKeys .eq. 0 ) return

!     Allocate scratch space
!     ----------------------
      allocate ( Indx2 ( NKeys ), stat = rstat )
      if ( rstat .ne. 0 ) then
         call WPErr ( MyName, Alloc ( rstat, 'Indx2', NKeys ))
         return
      end if

!     Sort with external scratch space provided
!     -----------------------------------------
      call IndexList2_log1 ( Indx, Indx2, Keys, Key0, Len0 )

!     Clean up
!     --------
      deallocate ( Indx2 )
      return

      return
      end subroutine IndexList_log1

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: IndexList2_log1 -- Sort logicals according to given value in external scratch space
!
! !DESCRIPTION:
!     This routine sorts (or bins) logical values according to a given
!     value.  The scratch space is external and is an subroutine argument.
!
! !INTERFACE:
      subroutine IndexList2_log1 ( Indx, Indx2, Keys, Key0, Len0 )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      logical,          intent (in)    ::
     .   Keys    (:), ! Keys
     .   Key0         ! Given key
!
! !INPUT/OUTPUT PARAMETERS: 
      integer,          intent (inout) ::
     .   Indx    (:), ! Sorting indices
     .   Indx2   (:)  ! External scratch space with the same size as Indx
!
! !OUTPUT PARAMETERS:
      integer,          intent (out)   ::
     .   Len0         ! Length of set with the given value
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     12Aug2003  C. Redder  Origional code.
!EOP
!....................................................................

      logical :: Keyi
      integer :: NKeys, iKey, iBeg, iBegN

!     Nothing number of keys is zero
!     ------------------------------
      NKeys = size ( Indx )
      if ( NKeys .eq. 0 ) return

!     Determine set size
!     ------------------
      Len0 = 0
      do iKey = 1, NKeys
         Keyi = Keys ( Indx ( iKey ))
         if ( Keyi .eqv. Key0 ) Len0 = Len0 + 1
      end do

!     Sort according to each list element
!     -----------------------------------
      iBeg  = 1
      iBegN = Len0 + 1
      do iKey = 1, NKeys
         Keyi = Keys ( Indx ( iKey ))
         if ( Keyi .eqv. Key0 ) then
            Indx2 ( iBeg  ) = Indx ( iKey )
            iBeg  = iBeg  + 1
         else
            Indx2 ( iBegN ) = Indx ( iKey )
            iBegN = iBegN + 1
         end if
      end do

!     Save sort indices
!     -----------------
      do iKey = 1, NKeys 
         Indx ( iKey ) = Indx2 ( iKey )
      end do

      return

      return
      end subroutine IndexList2_log1

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: IndexList_char1 -- Sort characters strings according to a given string
!
! !DESCRIPTION:
!     This routine sorts (or bins) character strings according to a given
!     string.
!
! !INTERFACE:
      subroutine IndexList_char1 ( Indx, Keys, Key0, Len0, rstat )
      use m_AdvError, only : Alloc, WPErr, Arr
!
! !INPUT PARAMETERS: 
      implicit   NONE
      character,        intent (in)    ::
     .   Keys    (:), ! Keys
     .   Key0         ! Given key
!
! !INPUT/OUTPUT PARAMETERS: 
      integer,          intent (inout) ::
     .   Indx    (:)  ! Sorting indices
!
! !OUTPUT PARAMETERS:
      integer,          intent (out)   ::
     .   Len0,        ! Length of set with the given value
     .   rstat        ! Returned status from the allocation statement
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     09Aug2002  C. Redder  Origional code.
!     01Nov2002  C. Redder  Corrected line that determines NKeys 
!     07Jan2003  C. Redder  Removed the internal variable istat
!
!EOP
!....................................................................

      character (len=len(Keys)) :: Keyi
      integer :: NKeys, iKey, iBeg, iBegN
      integer, dimension (:), allocatable :: Indx2, LIndx
      character (len=*), parameter :: MyName = MyModule
     .                                     // '::IndexList_char1'

!     Default status is valid
!     -----------------------
      rstat = 0

!     Nothing number of keys is zero
!     ------------------------------
      NKeys = size ( Indx )
      if ( NKeys .eq. 0 ) return

!     Allocate scratch space
!     ----------------------
      allocate ( Indx2 ( NKeys ), stat = rstat )
      if ( rstat .ne. 0 ) then
         call WPErr ( MyName, Alloc ( rstat, 'Indx2', NKeys ))
         return
      end if

!     Determine set size
!     ------------------
      Len0 = 0
      do iKey = 1, NKeys
         Keyi = Keys ( Indx ( iKey ))
         if ( Keyi .eq. Key0 ) Len0 = Len0 + 1
      end do

!     Sort according to each list element
!     -----------------------------------
      iBeg  = 1
      iBegN = Len0 + 1
      do iKey = 1, NKeys
         Keyi = Keys ( Indx ( iKey ))
         if ( Keyi .eq. Key0 ) then
            Indx2 ( iBeg  ) = Indx ( iKey )
            iBeg  = iBeg  + 1
         else
            Indx2 ( iBegN ) = Indx ( iKey )
            iBegN = iBegN + 1
         end if
      end do

!     Save sort indices
!     -----------------
      do iKey = 1, NKeys 
         Indx ( iKey ) = Indx2 ( iKey )
      end do

!     Clean up
!     --------
      deallocate ( Indx2 )
      return

      return
      end subroutine IndexList_char1

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: IndexList_int1 -- Sort integers according to given value
!
! !DESCRIPTION:
!     This routine sorts (or bins) integer values according to a given
!     value.
!
! !INTERFACE:
      subroutine IndexList_int1 ( Indx, Keys, Key0, Len0, rstat )
      use m_AdvError, only : Alloc, WPErr, Arr
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,          intent (in)    ::
     .   Keys    (:), ! Keys
     .   Key0         ! Given key
!
! !INPUT/OUTPUT PARAMETERS: 
      integer,          intent (inout) ::
     .   Indx    (:)  ! Sorting indices
!
! !OUTPUT PARAMETERS:
      integer,          intent (out)   ::
     .   Len0,        ! Length of set with the given value
     .   rstat        ! Returned status from the allocation statement
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     28Jan2003  C. Redder  Origional code.
!EOP
!....................................................................

      integer :: Keyi
      integer :: NKeys, iKey, iBeg, iBegN
      integer, dimension (:), allocatable :: Indx2, LIndx
      character (len=*), parameter :: MyName = MyModule
     .                                     // '::IndexList_int1'

!     Default status is valid
!     -----------------------
      rstat = 0

!     Nothing number of keys is zero
!     ------------------------------
      NKeys = size ( Indx )
      if ( NKeys .eq. 0 ) return

!     Allocate scratch space
!     ----------------------
      allocate ( Indx2 ( NKeys ), stat = rstat )
      if ( rstat .ne. 0 ) then
         call WPErr ( MyName, Alloc ( rstat, 'Indx2', NKeys ))
         return
      end if

!     Determine set size
!     ------------------
      Len0 = 0
      do iKey = 1, NKeys
         Keyi = Keys ( Indx ( iKey ))
         if ( Keyi .eq. Key0 ) Len0 = Len0 + 1
      end do

!     Sort according to each list element
!     -----------------------------------
      iBeg  = 1
      iBegN = Len0 + 1
      do iKey = 1, NKeys
         Keyi = Keys ( Indx ( iKey ))
         if ( Keyi .eq. Key0 ) then
            Indx2 ( iBeg  ) = Indx ( iKey )
            iBeg  = iBeg  + 1
         else
            Indx2 ( iBegN ) = Indx ( iKey )
            iBegN = iBegN + 1
         end if
      end do

!     Save sort indices
!     -----------------
      do iKey = 1, NKeys 
         Indx ( iKey ) = Indx2 ( iKey )
      end do

!     Clean up
!     --------
      deallocate ( Indx2 )
      return

      return
      end subroutine IndexList_int1

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: IndexList_intN -- Sort integers according to given list of values
!
! !DESCRIPTION:
!     This routine sorts integers according to a given list of values.
!     This routine was designed for values in the list with a limited
!     range.
!
! !INTERFACE:
      subroutine IndexList_intN ( Indx, Keys, List, Loc, Len, rstat )
      use m_AdvError, only : Alloc, WPErr, Arr
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,          intent (in)    ::
     .   Keys    (:), ! Keys
     .   List    (:)  ! List of valid keys
!
! !INPUT/OUTPUT PARAMETERS: 
      integer,          intent (inout) ::
     .   Indx    (:)  ! Sorting indices
!
! !OUTPUT PARAMETERS: 
      integer,          intent (out)   ::
     .   Loc     (:), ! Location and
     .   Len     (:), ! ... length of each set of keys in the list
     .   rstat        ! Returned status from the allocation statement.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     09Aug2002  C. Redder  Origional code.
!     07Jan2003  C. Redder  Removed the internal variable istat
!EOP
!....................................................................

      logical :: inList
      integer :: LMin,  LMax, ListSz, iList
      integer :: NKeys, iKey, Keyi,   iBeg,  iBegN
      integer, dimension (:), allocatable :: Indx2, LIndx
      character (len=*), parameter :: MyName = MyModule
     .                                     // '::IndexList_integerN'

!     Default status is valid
!     -----------------------
      rstat   = 0

!     Nothing to do if the list size or the number of keys is zero
!     ------------------------------------------------------------
      ListSz  = size ( List )
      NKeys   = size ( Indx )
      if ( ListSz .eq. 0 .or.
     .     NKeys  .eq. 0 ) return

!     Find range of values in list
!     ----------------------------
      LMin = minval ( List )
      LMax = maxval ( List )

!     Allocate scratch space
!     ----------------------
      allocate ( Indx2 ( NKeys  ), LIndx ( LMin : LMax ), stat = rstat )
      if ( rstat .ne. 0 ) then
         call WPErr ( MyName, Alloc ( rstat, Arr ( 'Indx2', NKeys  )
     .                                    // Arr ( 'LIndx', LMin,LMax)))
         return
      end if

!     Map each value in the list to its list location
!     -----------------------------------------------
      LIndx ( LMin : LMax ) = -1
      do iList = 1, ListSz
          LIndx ( List ( iList )) = iList 
      end do

!     Determine set size ...
!     ----------------------
      Len ( : ListSz ) = 0
      do iKey = 1, NKeys
         Keyi = Keys ( Indx ( iKey ))
         if ( Keyi .ge. LMin .and.
     .        Keyi .le. LMax ) then
            iList  = LIndx ( Keyi )
            inList = iList .ne. - 1
            if ( inList ) Len ( iList ) = Len ( iList ) + 1
         end if
      end do

!     ... and set location corresponding to each entry in the list
!     ------------------------------------------------------------ 
      iBeg  = 1
      do iList = 1, ListSz
         Loc ( iList ) = iBeg
         iBeg          = iBeg + Len ( iList )
      end do
      iBegN = iBeg

!     Sort according to each list element
!     -----------------------------------
      do iKey = 1, NKeys
         Keyi = Keys ( Indx ( iKey ))
         if ( Keyi .ge. LMin .and.
     .        Keyi .le. LMax ) then
            iList  = LIndx ( Keyi )
            inList = iList .ne. - 1
            if ( inList ) then
               iBeg  = Loc ( iList )
               Indx2 ( iBeg  ) = Indx ( iKey )
               iBeg  = iBeg  + 1
               Loc ( iList )   = iBeg
            else
               Indx2 ( iBegN ) = Indx ( iKey )
               iBegN = iBegN + 1
            end if
         else
               Indx2 ( iBegN ) = Indx ( iKey )
               iBegN = iBegN + 1
         end if
      end do

!     Reinitialize set locations
!     --------------------------
      iBeg  = 1
      do iList = 1, ListSz
         Loc ( iList ) = iBeg
         iBeg          = iBeg + Len ( iList )
      end do

!     Save sort indices
!     -----------------
      do iKey = 1, NKeys 
         Indx ( iKey ) = Indx2 ( iKey )
      end do

!     Clean up
!     --------
      deallocate ( Indx2, LIndx )
      return
      end subroutine IndexList_IntN

!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: IndexSort_Int - A stable merge index sorting of integers.
!
! !DESCRIPTION:
!
! !INTERFACE:
!
      subroutine IndexSort_Int ( Indx, Keys, rstat, descend )
      use m_AdvError, only : Alloc, WPErr, Arr
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,           intent (in)    ::
     .   Keys    (:)   ! Sorting keys
      logical, optional, intent (in)    ::
     .   descend       ! = .true. to sort in descending order
     .                 !   (Default: descend = .false.)
!
! !INPUT/OUTPUT PARAMETERS: 
      integer,           intent (inout) ::
     .   Indx    (:)   ! Sorting indices
!
! !OUTPUT PARAMETERS: 
      integer,           intent (out)   ::
     .   rstat         ! Returned status from the allocation statement.
!
! !REVISION HISTORY:
!     21Oct2002 - C. Redder - Code adapted from mpeu library routine iSort_
!                             in the module m_MergeSorts.  The routine was
!                             originally created by Jing Guo.
!     21Feb2003 - C. Redder - Fixed bug by initializing the returned status
!                             to zero
!     08Aug2003 - C. Redder - Replaced call to the internal subroutine,
!                             MergeSort, with a call to the external 
!                             subroutine, IndexSort2_Int. 
!EOP
!-----------------------------------------------------------------------

      logical :: dsnd
      integer :: NKeys
      integer, dimension(:), allocatable :: Indx2
      character (len=*), parameter :: MyName = MyModule
     .                                     // '::IndexSort_Int'

      rstat = 0 ! default status

      NKeys = size ( Indx )
      if ( NKeys  .eq. 0 ) return

      allocate( Indx2 ( NKeys ) , stat = rstat )
      if ( rstat .ne. 0 ) then
         call WPErr ( MyName, Alloc ( rstat, Arr ( 'Indx2', NKeys  )))
         return
      end if

      call IndexSort2_Int ( Indx, Indx2, Keys, descend )

      deallocate ( Indx2 )
      return

      end subroutine IndexSort_Int

!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: IndexSort2_Int - A stable merge index sorting of integers with external scratch space
!
! !DESCRIPTION:
!
! !INTERFACE:
!
      subroutine IndexSort2_Int ( Indx, Indx2, Keys, descend )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,           intent (in)    ::
     .   Keys    (:)   ! Sorting keys
      logical, optional, intent (in)    ::
     .   descend       ! = .true. to sort in descending order
     .                 !   (Default: descend = .false.)
!
! !INPUT/OUTPUT PARAMETERS: 
      integer,           intent (inout) ::
     .   Indx    (:),  ! Sorting indices
     .   Indx2   (:)   ! External scratch space with the same size as Indx 
!
! !REVISION HISTORY:
!     12Aug2003 - C. Redder  Original version
!EOP
!-----------------------------------------------------------------------

      logical :: dsnd
      integer :: NKeys

      dsnd  = .false.
      if ( present ( descend )) dsnd = descend

      NKeys = size ( Indx )
      if ( NKeys  .eq. 0 ) return

      call MergeSort_()

      contains

      subroutine MergeSort_()
      implicit none
      integer :: MStep, LStep
      integer :: lb, lm, le

      MStep = 1
      do while( MStep < NKeys )
         LStep = MStep * 2

         lb = 1
         do while ( lb < NKeys )
            lm = lb + MStep
            le = min ( lm - 1 + MStep, NKeys )

            call merge_ ( lb, lm, le )
            Indx ( lb : le ) = Indx2 ( lb : le )
            lb = le + 1
         end do

         MStep = LStep
      end do
      end subroutine MergeSort_

      subroutine merge_ ( lb, lm, le )
      integer,intent(in) :: lb, lm, le
      integer :: l1, l2, l

      l1 = lb
      l2 = lm
      do l = lb, le
         if      ( l2 .gt. le ) then
            Indx2 ( l ) = Indx ( l1 )
            l1 = l1 + 1
         else if ( l1. ge. lm ) then
            Indx2 ( l ) = Indx ( l2 )
            l2 = l2 + 1
         else
            if ( dsnd ) then
               if ( Keys ( Indx ( l1 )) .ge. Keys ( Indx ( l2 ))) then
                  Indx2 ( l ) = Indx ( l1 )
                  l1 = l1 + 1
               else
                  Indx2 ( l ) = Indx ( l2 )
                  l2 = l2 + 1
               endif
            else
               if ( Keys ( Indx ( l1 )) .le. Keys ( Indx ( l2 ))) then
                  Indx2 ( l ) = Indx ( l1 )
                  l1 = l1 + 1
               else
                  Indx2 ( l ) = Indx ( l2 )
                  l2 = l2 + 1
               end if
            end if
         end if
      end do
      end subroutine merge_
      end subroutine IndexSort2_Int

!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: IndexSort_Real - A stable merge index sorting of real numbers.
!
! !DESCRIPTION:
!
! !INTERFACE:
!
      subroutine IndexSort_Real ( Indx, Keys, rstat, descend )
      use m_AdvError, only : Alloc, WPErr, Arr
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,              intent (in)    ::
     .   Keys    (:)   ! Sorting keys
      logical, optional, intent (in)    ::
     .   descend       ! = .true. to sort in descending order
     .                 !   (Default: descend = .false.)
!
! !INPUT/OUTPUT PARAMETERS: 
      integer,           intent (inout) ::
     .   Indx    (:)   ! Sorting indices
!
! !OUTPUT PARAMETERS: 
      integer,           intent (out)   ::
     .   rstat         ! Returned status from the allocation statement.
!
! !REVISION HISTORY:
!     21Oct2002 - C. Redder - Code adapted from mpeu library routine iSort_
!                             in the module m_MergeSorts.  The routine was
!                             originally created by Jing Guo.
!     07Jan2003 - C. Redder - Fixed bug by initializing the returned status
!                             to zero
!EOP
!-----------------------------------------------------------------------

      logical :: dsnd
      integer :: NKeys
      integer, dimension(:), allocatable :: Indx2
      character (len=*), parameter :: MyName = MyModule
     .                                     // '::IndexSort_Real'

      rstat = 0 ! default status

      dsnd  = .false.
      if ( present ( descend )) dsnd = descend

      NKeys = size ( Indx )
      if ( NKeys  .eq. 0 ) return

      allocate( Indx2 ( NKeys ) , stat = rstat )
      if ( rstat .ne. 0 ) then
         call WPErr ( MyName, Alloc ( rstat, Arr ( 'Indx2', NKeys  )))
         return
      end if

      call MergeSort_()

      deallocate ( Indx2 )

      contains

      subroutine MergeSort_()
      implicit none
      integer :: MStep, LStep
      integer :: lb, lm, le

      MStep = 1
      do while( MStep < NKeys )
         LStep = MStep * 2

         lb = 1
         do while ( lb < NKeys )
            lm = lb + MStep
            le = min ( lm - 1 + MStep, NKeys )

            call merge_ ( lb, lm, le )
            Indx ( lb : le ) = Indx2 ( lb : le )
            lb = le + 1
         end do

         MStep = LStep
      end do
      end subroutine MergeSort_

      subroutine merge_ ( lb, lm, le )
      integer,intent(in) :: lb, lm, le
      integer :: l1, l2, l

      l1 = lb
      l2 = lm
      do l = lb, le
         if      ( l2 .gt. le ) then
            Indx2 ( l ) = Indx ( l1 )
            l1 = l1 + 1
         else if ( l1. ge. lm ) then
            Indx2 ( l ) = Indx ( l2 )
            l2 = l2 + 1
         else
            if ( dsnd ) then
               if ( Keys ( Indx ( l1 )) .ge. Keys ( Indx ( l2 ))) then
                  Indx2 ( l ) = Indx ( l1 )
                  l1 = l1 + 1
               else
                  Indx2 ( l ) = Indx ( l2 )
                  l2 = l2 + 1
               endif
            else
               if ( Keys ( Indx ( l1 )) .le. Keys ( Indx ( l2 ))) then
                  Indx2 ( l ) = Indx ( l1 )
                  l1 = l1 + 1
               else
                  Indx2 ( l ) = Indx ( l2 )
                  l2 = l2 + 1
               end if
            end if
         end if
      end do
      end subroutine merge_
      end subroutine IndexSort_Real
!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: IndexSort_Char - A stable merge index sorting of characters
!
! !DESCRIPTION:
!
! !INTERFACE:
!
      subroutine IndexSort_Char ( Indx, Keys, rstat, descend )
      use m_AdvError, only : Alloc, WPErr, Arr
!
! !INPUT PARAMETERS: 
      implicit   NONE
      character (len=*), intent (in)    ::
     .   Keys    (:)   ! Sorting keys
      logical, optional, intent (in)    ::
     .   descend       ! = .true. to sort in descending order
     .                 !   (Default: descend = .false.)
!
! !INPUT/OUTPUT PARAMETERS: 
      integer,           intent (inout) ::
     .   Indx    (:)   ! Sorting indices
!
! !OUTPUT PARAMETERS: 
      integer,           intent (out)   ::
     .   rstat         ! Returned status from the allocation statement.
!
! !REVISION HISTORY:
!     28Jan2003 - C. Redder - Code adapted from the routine IndexSort_Real
!EOP
!-----------------------------------------------------------------------

      logical :: dsnd
      integer :: NKeys
      integer, dimension(:), allocatable :: Indx2
      character (len=*), parameter :: MyName = MyModule
     .                                     // '::IndexSort_Char'

      rstat = 0 ! default status

      dsnd  = .false.
      if ( present ( descend )) dsnd = descend

      NKeys = size ( Indx )
      if ( NKeys  .eq. 0 ) return

      allocate( Indx2 ( NKeys ) , stat = rstat )
      if ( rstat .ne. 0 ) then
         call WPErr ( MyName, Alloc ( rstat, Arr ( 'Indx2', NKeys  )))
         return
      end if

      call MergeSort_()

      deallocate ( Indx2 )

      contains

      subroutine MergeSort_()
      implicit none
      integer :: MStep, LStep
      integer :: lb, lm, le

      MStep = 1
      do while( MStep < NKeys )
         LStep = MStep * 2

         lb = 1
         do while ( lb < NKeys )
            lm = lb + MStep
            le = min ( lm - 1 + MStep, NKeys )

            call merge_ ( lb, lm, le )
            Indx ( lb : le ) = Indx2 ( lb : le )
            lb = le + 1
         end do

         MStep = LStep
      end do
      end subroutine MergeSort_

      subroutine merge_ ( lb, lm, le )
      integer,intent(in) :: lb, lm, le
      integer :: l1, l2, l

      l1 = lb
      l2 = lm
      do l = lb, le
         if      ( l2 .gt. le ) then
            Indx2 ( l ) = Indx ( l1 )
            l1 = l1 + 1
         else if ( l1. ge. lm ) then
            Indx2 ( l ) = Indx ( l2 )
            l2 = l2 + 1
         else
            if ( dsnd ) then
               if ( LGE ( Keys ( Indx ( l1 )),
     .                    Keys ( Indx ( l2 )))) then
                  Indx2 ( l ) = Indx ( l1 )
                  l1 = l1 + 1
               else
                  Indx2 ( l ) = Indx ( l2 )
                  l2 = l2 + 1
               endif
            else
               if ( LLE ( Keys ( Indx ( l1 )),
     .                    Keys ( Indx ( l2 )))) then
                  Indx2 ( l ) = Indx ( l1 )
                  l1 = l1 + 1
               else
                  Indx2 ( l ) = Indx ( l2 )
                  l2 = l2 + 1
               end if
            end if
         end if
      end do
      end subroutine merge_
      end subroutine IndexSort_Char
!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: VerSort_integer - Verify that integer keys are sorted 
!
! !DESCRIPTION:
!     This routine verifies that the integer keys are sorted by
!     identifying the first (or, if desired, last) key that is out
!     of order.  If the keys are in fact sorted then zero is returned.
!
! !INTERFACE:
      function VerSort_integer ( Keys, descend, back )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,              intent (in)    ::
     .   Keys    (:)      ! Keys to be examined
      logical, optional,    intent (in)    ::
     .   descend,         ! = .true. if sorted in descending order
     .                    !   (Default: descend = .false.)
     .   back             ! = .true. to identify the last index that is
!                         !   out of order.  (Default: back = .false.)
! !OUTPUT PARAMETERS: 
      integer                              ::
     .   VerSort_integer  ! First (or last) array element in Keys that is
!                         !   out of order or zero if all are in order.
! !REVISION HISTORY:
!     06Jun2003 - C. Redder - original code
!
!-----------------------------------------------------------------------
      integer, dimension (0) :: BIndx

      VerSort_integer
     .   = VerSort_Binteger ( BIndx, Keys, descend, back )

      return
      end function VerSort_integer
!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: VerSort_Binteger - Verify that integer keys are sorted 
!
! !DESCRIPTION:
!     This routine verifies that integer keys are sorted by identifying
!     the first (or, if desired, last) key that is out of order.  If
!     the keys are in fact sorted then zero is returned.. If desired,
!     this routine can also verify that the keys are sorted within each
!     bin defined as having common values of previously verified key(s).
!     This feature can permit this and other similar routines to verify
!     a sort among multiple keys.  The following program illustrates
!     the verification among multiple keys.
! \begin{verbatim}
!        program example
!        use m_RadSort, only : VerifySort, IndexSet, IndexSort
!        logical :: sorted
!        integer :: rstat
!        integer, parameter :: M = 15
!        integer, dimension ( M ) :: Indx, BIndx,
!     &     ks  = (/  2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 5, 5 /),
!     &     Lev = (/  4, 4, 6, 6, 8, 8, 8, 2, 2, 3, 3, 7, 7, 1, 1 /),
!     &     kt  = (/  4, 5, 4, 5, 4, 5, 6, 4, 5, 7, 8, 4, 5, 6, 8 /)
!        BIndx ( : M ) = 0
!        sorted =              VerifySort ( BIndx, ks  ) .eq. 0
!        sorted = sorted .and. VerifySort ( BIndx, Lev ) .eq. 0
!        sorted = sorted .and. VerifySort ( BIndx, kt  ) .eq. 0
!        call IndexSet ( Indx )
!        if ( .not. sorted ) then
!           call IndexSort ( Indx, kt,  rstat )
!           call IndexSort ( Indx, Lev, rstat )
!           call IndexSort ( Indx, ks,  rstat )
!        end if
!        end program example
! \end{verbatim}
!      The final value for {\tt sorted} will be {\tt .true.} so that none
!      of the sort routines will be called.  If the array sizes had been
!      very large, then these verifying routine could save significant
!      computational cost.  Note that the entire array {\tt BIndx} is
!      initialized to a single reasonable value (in this case {\tt 0}) by
!      the calling program and then updated by each call to {\tt VerSort}.
!      Thus, the result is dependent on the order in which the keys are
!      processed.  The keys must be processed so that the keys verified
!      first must be sorted last and vice versa.  If the key {\tt kt}
!      had been processed before {\tt Lev} then {\tt sorted} would have
!      been {\tt false}, and the sorting routines would have been called.
!      The array {\tt BIndx} can also be initialized by routines under
!      the generic interface name, BIndexSet.  Finally, the portion of
!      the code,
! \begin{verbatim}
!        BIndx ( : M ) = 0
!        sorted =              VerSort ( BIndx, ks  )
!        sorted = sorted .and. VerSort ( BIndx, Lev )
!        sorted = sorted .and. VerSort ( BIndx, kt  )
!        if ( .not. sorted ) then 
! \end{verbatim}
!      could have been rewritten to
! \begin{verbatim}
!        BIndx ( : M ) = 0
!        if ( .not. VerifySort ( BIndx, ks  ) .eq. 0 .or.
!             .not. VerifySort ( BIndx, Lev ) .eq. 0 .or.
!             .not. VerifySort ( BIndx, kt  ) .eq. 0 ) then 
! \end{verbatim}
!      The result, as before, depends on the order in which the keys 
!      are processed.  A similar program could be written with Indx
!      as an argument to the verify routines.
!
! !INTERFACE:
      function VerSort_Binteger ( BIndx, Keys, descend, back )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,              intent (in)    ::
     .   Keys    (:)      ! Keys to be examined
      logical, optional,    intent (in)    ::
     .   descend,         ! = .true. if sorted in descending order
     .                    !   (Default: descend = .false.)
     .   back             ! = .true. to identify the last index that is
!                         !   out of order.  (Default: back = .false.)
! !INPUT/OUTPUT PARAMETERS:
      integer,              intent (inout) ::
     .   BIndx  (:)       ! Bin index number
!
!OUTPUT PARAMETERS: 
      integer                              ::
     .   VerSort_Binteger ! First (or last) array element in Keys that is
!                         !   out of order or zero if all are in order.
! !REVISION HISTORY:
!     02Jan2003 - C. Redder - Original code
!     06Jun2003 - C. Redder - Replaced the argument PKeys with BIndx.
!
!-----------------------------------------------------------------------
      integer :: iKey, NKeys, Verify, Beg, End, Incr, NBins
      integer :: ThisKey, LastKey, ThisBin, LastBin
      logical :: failed, ascend, test, backwds, verified, check_bins

      NKeys    =  size (  Keys )
      NBins    =  size ( BIndx )
      if ( NBins .eq. 0 .and.
     .     NKeys .ne. 0 ) then
         check_bins = .false.               ! Ignore bins if the size
      else                                  !   of BIndx is zero
         check_bins = .true.
         NKeys = min ( NKeys, NBins )
      end if

      ascend   = .true.                     ! Implement options to set
      if ( present ( descend )) ascend  = .not. descend ! sorting trend
      backwds  = .false.                    ! ... or to return last
      if ( present ( back    )) backwds = back ! sorting index that is
                                            !     out of order
      Beg      =  2                         ! Set do loop indices for if
      End      =  NKeys                     ! ... the option back is
      Incr     =  1                         !     turned off 
      if ( backwds ) then                   ! ... or is turned on
         Beg   =  NKeys
         End   =  2
         Incr  = -1
      end if

      ThisBin  =  0
      LastBin  =  0
      Verify   =  0                         ! Assume keys are sorted
      do iKey  =  Beg, End, Incr            !   until shown otherwise
         if ( check_bins ) then
            ThisBin = BIndx ( iKey     )
            LastBin = BIndx ( iKey - 1 )
         end if
         ThisKey    = Keys  ( iKey     )
         LastKey    = Keys  ( iKey - 1 )    ! Test is necesary only if
         test       = ThisKey  .ne. LastKey ! ... keys change and
     .          .and. ThisBin  .eq. LastBin ! ... bin numbers do not
         failed     = .false.
         if ( test   )                      ! If necessary, test
     .      failed  = .not. ( ThisKey .gt. LastKey .eqv. ascend )
         if ( failed ) then                 ! If test failed,
            Verify  = iKey                  ! ... save result and
            exit                            ! ... exit loop.
         end if
      end do

!     Update BIndx
!     ------------
      verified = Verify .eq. 0
      if ( verified ) call BIndexSet_IKeys ( BIndx, Keys )

!     Return result
!     -------------
      VerSort_Binteger = Verify

      return
      end function VerSort_Binteger

!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: VerSort_real - Verify that real keys are sorted 
!
! !DESCRIPTION:
!     This routine verifies that the real keys are sorted by
!     identifying the first (or, if desired, last) key that is out
!     of order.  If the keys are in fact sorted then zero is returned.
!
! !INTERFACE:
      function VerSort_real ( Keys, descend, back )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,                 intent (in)    ::
     .   Keys    (:)      ! Keys to be examined
      logical, optional,    intent (in)    ::
     .   descend,         ! = .true. if sorted in descending order
     .                    !   (Default: descend = .false.)
     .   back             ! = .true. to identify the last index that is
!                         !   out of order.  (Default: back = .false.)
! !OUTPUT PARAMETERS: 
      integer                              ::
     .   VerSort_real     ! First (or last) array element in Keys that is
!                         !   out of order or zero if all are in order.
! !REVISION HISTORY:
!     06Jun2003 - C. Redder - original code
!
!-----------------------------------------------------------------------
      integer, dimension (0) :: BIndx

      VerSort_real
     .   = VerSort_Breal ( BIndx, Keys, descend, back )

      return
      end function VerSort_real
!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: VerSort_Breal - Verify that real keys are sorted 
!
! !DESCRIPTION:
!     This routine verifies that real keys are sorted by identifying the
!     first (or, if desired, last) key that is out of order.  If the
!     keys are in fact sorted then zero is returned.  This routine can
!     be used to verify a sort among multiple keys.  For further
!     explanation, see the routine, VerSort_integer.
!
! !INTERFACE:
      function VerSort_Breal ( BIndx, Keys, descend, back )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,                 intent (in)    ::
     .   Keys    (:)      ! Keys to be examined
      logical, optional,    intent (in)    ::
     .   descend,         ! = .true. if sorted in descending order
     .                    !   (Default: descend = .false.)
     .   back             ! = .true. to identify the last index that is
!                         !   out of order.  (Default: back = .false.)
! !INPUT/OUTPUT PARAMETERS:
      integer,              intent (inout) ::
     .   BIndx  (:)       ! Bin index number
!
! !OUTPUT PARAMETERS: 
      integer                              ::
     .   VerSort_Breal    ! First (or last) array element in Keys that is
!                         !   out of order or zero if all are in order.
! !REVISION HISTORY:
!     02Jan2003 - C. Redder - Original code
!     06Jun2003 - C. Redder - Replaced the argument PKeys with BIndx.
!
!-----------------------------------------------------------------------
      integer :: iKey, NKeys, Verify, Beg, End, Incr, NBins
      integer :: ThisBin, LastBin
      real    :: ThisKey, LastKey
      logical :: failed, ascend, test, backwds, verified, check_bins

      NKeys    =  size (  Keys )
      NBins    =  size ( BIndx )
      if ( NBins .eq. 0 .and.
     .     NKeys .ne. 0 ) then
         check_bins = .false.               ! Ignore bins if the size
      else                                  !   of BIndx is zero
         check_bins = .true.
         NKeys = min ( NKeys, NBins )
      end if

      ascend   = .true.                     ! Implement options to set
      if ( present ( descend )) ascend  = .not. descend ! sorting trend
      backwds  = .false.                    ! ... or to return last
      if ( present ( back    )) backwds = back ! sorting index that is
                                            !     out of order
      Beg      =  2                         ! Set do loop indices for if
      End      =  NKeys                     ! ... the option back is
      Incr     =  1                         !     turned off 
      if ( backwds ) then                   ! ... or is turned on
         Beg   =  NKeys
         End   =  2
         Incr  = -1
      end if

      ThisBin  =  0
      LastBin  =  0
      Verify   =  0                         ! Assume keys are sorted
      do iKey  =  Beg, End, Incr            !   until shown otherwise
         if ( check_bins ) then
            ThisBin = BIndx ( iKey     )
            LastBin = BIndx ( iKey - 1 )
         end if
         ThisKey    = Keys  ( iKey     )
         LastKey    = Keys  ( iKey - 1 )    ! Test is necesary only if
         test       = ThisKey  .ne. LastKey ! ... keys change and
     .          .and. ThisBin  .eq. LastBin ! ... bin numbers do not
         failed     = .false.
         if ( test   )                      ! If necessary, test
     .      failed  = .not. ( ThisKey .gt. LastKey .eqv. ascend )
         if ( failed ) then                 ! If test failed,
            Verify  = iKey                  ! ... save result and
            exit                            ! ... exit loop.
         end if
      end do

!     Update BIndx
!     ------------
      verified = Verify .eq. 0
      if ( verified ) call BIndexSet_RKeys ( BIndx, Keys )

!     Return result
!     -------------
      VerSort_Breal = Verify

      return
      end function VerSort_Breal

!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: VerISort_integer - Verify that indices of integer keys are sorted 
!
! !DESCRIPTION:
!     This routine verifies that the indices of the integer keys are sorted
!     by identifying the first (or, if desired, last) sorting index that
!     is out of order.  If the keys are in fact sorted then zero is
!     returned.
!
! !INTERFACE:
      function VerISort_integer ( Indx, Keys, descend, back )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,              intent (in)    ::
     .   Indx    (:),     ! Sorting index of keys
     .   Keys    (:)      ! Keys to be examined
      logical, optional,    intent (in)    ::
     .   descend,         ! = .true. if sorted in descending order
     .                    !   (Default: descend = .false.)
     .   back             ! = .true. to identify the last index that is
!                         !   out of order.  (Default: back = .false.)
! !OUTPUT PARAMETERS: 
      integer                              ::
     .   VerISort_integer ! First (or last) array element in Indx that is
!                         !   out of order or zero if all are in order.
! !REVISION HISTORY:
!     06Jun2003 - C. Redder - original code
!
!-----------------------------------------------------------------------
      integer, dimension (0) :: BIndx

      VerISort_integer
     .   = VerISort_Binteger ( BIndx, Indx, Keys, descend, back )

      return
      end function VerISort_integer
!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: VerISort_Binteger - Verify that indices of integer keys are sorted 
!
! !DESCRIPTION:
!     This routine verifies that the indices of the integer keys are
!     sorted by identifying the first (or, if desired, last) sorting
!     index that is out of order.  If the indices are in fact sorted
!     then zero is returned.  This routine can be used to verify a
!     sort among multiple keys.  For further explanation, see the
!     routine, VerSort_integer.
!
! !INTERFACE:
      function VerISort_Binteger ( BIndx, Indx, Keys, descend, back )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,               intent (in)    ::
     .   Indx    (:),      ! Sorting index of keys
     .   Keys    (:)       ! Keys to be examined
      logical, optional,     intent (in)    ::
     .   descend,          ! = .true. if sorted in descending order
     .                     !   (Default: descend = .false.)
     .   back              ! = .true. to identify the last index that is
!                          !   out of order.  (Default: back = .false.)
! !INPUT/OUTPUT PARAMETERS:
      integer,               intent (inout) ::
     .   BIndx  (:)        ! Bin index number.  This array must be
!                          !   conformable to Indx
! !OUTPUT PARAMETERS: 
      integer                               ::
     .   VerISort_Binteger ! First (or last) array element in Indx that is
!                          !   out of order or zero if all are in order.
! !REVISION HISTORY:
!     02Jan2003 - C. Redder - Original code
!     06Jun2003 - C. Redder - Replaced the argument PKeys with BIndx.
!
!-----------------------------------------------------------------------
      integer :: iKey, NKeys, ThisIndx, LastIndx, Verify, Beg, End, Incr
      integer :: ThisKey, LastKey, ThisBin, LastBin, NBins
      logical :: failed, ascend, test, backwds, verified, check_bins

      NKeys    =  size (  Indx )
      NBins    =  size ( BIndx )
      if ( NBins .eq. 0 .and.
     .     NKeys .ne. 0 ) then
         check_bins = .false.               ! Ignore bins if the size
      else                                  !   of BIndx is zero
         check_bins = .true.
         NKeys = min ( NKeys, NBins )
      end if

      ascend   = .true.                     ! Implement options to set
      if ( present ( descend )) ascend  = .not. descend ! sorting trend
      backwds  = .false.                    ! ... or to return last
      if ( present ( back    )) backwds = back ! sorting index that is
                                            !     out of order

      Beg      =  2                         ! Set do loop indices for if
      End      =  NKeys                     ! ... the option back is
      Incr     =  1                         !     turned off 
      if ( backwds ) then                   ! ... or is turned on
         Beg   =  NKeys
         End   =  2
         Incr  = -1
      end if

      ThisBin  =  0
      LastBin  =  0
      Verify   =  0                         ! Assume keys are sorted
      do iKey  =  Beg, End, Incr            !   until shown otherwise
         ThisIndx   = Indx  ( iKey )
         LastIndx   = Indx  ( iKey - 1 ) 
         if ( check_bins ) then
            ThisBin = BIndx ( iKey )
            LastBin = BIndx ( iKey - 1 )
         end if
         ThisKey    = Keys  ( ThisIndx )
         LastKey    = Keys  ( LastIndx )    ! Test is necesary only if
         test       = ThisKey  .ne. LastKey ! ... keys change and
     .         .and.  ThisBin  .eq. LastBin ! ... bin numbers do not
         failed     = .false.
         if ( test   )                      ! If necessary, test
     .      failed  = .not. ( ThisKey .gt. LastKey .eqv. ascend )
         if ( failed ) then                 ! If test failed,
            Verify  = iKey                  ! ... save result and 
            exit                            ! ... exit loop.
         end if
      end do

!     Update BIndx
!     ------------
      verified = Verify .eq. 0
      if ( verified ) call BIndexSet_IIKeys ( BIndx, Indx, Keys )

!     Return result
!     -------------
      VerISort_Binteger = Verify

      return
      end function VerISort_Binteger

!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: VerISort_real - Verify that indices of real keys are sorted 
!
! !DESCRIPTION:
!     This routine verifies that the indices of the real keys are sorted
!     by identifying the first (or, if desired, last) sorting index that
!     is out of order.  If the indices are in fact sorted then zero is
!     returned.
!
! !INTERFACE:
      function VerISort_real ( Indx, Keys, descend, back )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,              intent (in)    ::
     .   Indx    (:)      ! Sorting index of keys
      real,                 intent (in)    ::
     .   Keys    (:)      ! Keys to be examined
      logical, optional,    intent (in)    ::
     .   descend,         ! = .true. if sorted in descending order
     .                    !   (Default: descend = .false.)
     .   back             ! = .true. to identify the last index that is
!                         !   out of order.  (Default: back = .false.)
! !OUTPUT PARAMETERS: 
      integer                              ::
     .   VerISort_real    ! First (or last) array element in Indx that is
!                         !   out of order or zero if all are in order.
! !REVISION HISTORY:
!     06Jun2003 - C. Redder - original code
!
!-----------------------------------------------------------------------
      integer, dimension (0) :: BIndx

      VerISort_real
     .   = VerISort_Breal ( BIndx, Indx, Keys, descend, back )

      return
      end function VerISort_real
!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: VerISort_Breal - Verify that indices of real keys are sorted 
!
! !DESCRIPTION:
!     This routine verifies that the indices of the real keys are sorted
!     by identifying the first (or, if desired, last) sorting index that
!     is out of order.  This routine can be used to verify a sort among
!     multiple keys.  For further explanation, see the routine,
!     VerSort_integer.
!
! !INTERFACE:
      function VerISort_Breal ( BIndx, Indx, Keys, descend, back )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,               intent (in)    ::
     .   Indx    (:)       ! Sorting index of keys
      real,                  intent (in)    ::
     .   Keys    (:)       ! Keys to be examined
      logical, optional,     intent (in)    ::
     .   descend,          ! = .true. if sorted in descending order
     .                     !   (Default: descend = .false.)
     .   back              ! = .true. to identify the last index that is
!                          !   out of order.  (Default: back = .false.)
! !INPUT/OUTPUT PARAMETERS:
      integer,               intent (inout) ::
     .   BIndx  (:)        ! Bin index number.  This array must be
!                          !   conformable to Indx
! !OUTPUT PARAMETERS: 
      integer                               ::
     .   VerISort_Breal    ! First (or last) array element in Indx that is
!                          !   out of order or zero if all are in order.
! !REVISION HISTORY:
!     02Jan2003 - C. Redder - Original code
!     06Jun2003 - C. Redder - Replaced the argument PKeys with BIndx.
!
!-----------------------------------------------------------------------
      integer :: iKey, NKeys, ThisIndx, LastIndx, Verify, Beg, End,
     .           Incr, NBins
      real    :: ThisKey, LastKey, ThisBin, LastBin
      logical :: failed, ascend, test, backwds, verified, check_bins

      NKeys    =  size (  Indx )
      NBins    =  size ( BIndx )
      if ( NBins .eq. 0 .and.
     .     NKeys .ne. 0 ) then
         check_bins = .false.               ! Ignore bins if the size
      else                                  !   of BIndx is zero
         check_bins = .true.
         NKeys = min ( NKeys, NBins )
      end if
      ascend   = .true.                     ! Implement options to set
      if ( present ( descend )) ascend  = .not. descend ! sorting trend
      backwds  = .false.                    ! ... or to return last
      if ( present ( back    )) backwds = back  ! sorting index that is
                                            !     out of order
      Beg      =  2                         ! Set do loop indices for if
      End      =  NKeys                     ! ... the option back is
      Incr     =  1                         !     turned off 
      if ( backwds ) then                   ! ... or is turned on
         Beg   =  NKeys
         End   =  2
         Incr  = -1
      end if

      ThisBin  =  0
      LastBin  =  0
      Verify   =  0                         ! Assume keys are sorted
      do iKey  =  Beg, End, Incr            !   until shown otherwise
         ThisIndx   = Indx  ( iKey )
         LastIndx   = Indx  ( iKey - 1 ) 
         if ( check_bins ) then
            ThisBin = BIndx ( iKey )
            LastBin = BIndx ( iKey - 1 )
         end if
         ThisKey    = Keys  ( ThisIndx )
         LastKey    = Keys  ( LastIndx )    ! Test is necesary only if
         test       = ThisKey  .ne. LastKey ! ... keys change and
     .          .and. ThisBin  .eq. LastBin ! ... bin numbers do not
         failed     = .false.
         if ( test   )                      ! If necessary, test
     .      failed  = .not. ( ThisKey .gt. LastKey .eqv. ascend )
         if ( failed ) then                 ! If test failed,
            Verify  = iKey                  ! ... save result and
            exit                            ! ... exit loop.
         end if
      end do

!     Update BIndx
!     ------------
      verified = Verify .eq. 0
      if ( verified ) call BIndexSet_IRKeys ( BIndx, Indx, Keys )

!     Return result
!     -------------
      VerISort_Breal = Verify

      return
      end function VerISort_Breal

!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: NavInit_ - Initialize navigators from sorted integer keys
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine NavInit_ ( SKeys, NSect, Loc, Len )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,        intent (in)    ::
     .   SKeys (:)  ! Keys
!
! !OUTPUT PARAMETERS: 
      integer,        intent (out)   ::
     .   NSect      ! Number of sections
      integer,        intent (out), optional ::
     .   Loc   (:), ! Location and ...
     .   Len   (:)  ! ... size of each section 
!
! !REVISION HISTORY:
!     18Dec2002 - C. Redder - Original code
!
!-----------------------------------------------------------------------
      integer :: iKey, NKeys
      logical :: save_Loc, save_Len

      save_Loc = present ( Loc )
      save_Len = present ( Len )

      NSect = 0
      NKeys = size ( SKeys )
      if ( NKeys .eq. 0 ) return            ! Nothing to do

      NSect = 1
      if ( save_Loc ) Loc ( 1 ) = 1         ! Begin first section
      if ( save_Len ) Len ( 1 ) = 1
      do iKey = 2, NKeys

!        If keys do not change ... 
!        -------------------------
         if ( SKeys ( iKey - 1 ) .eq.
     .        SKeys ( iKey     )) then      ! ... increment section size
            if ( save_Len ) Len ( NSect ) = Len ( NSect ) + 1

         else ! ...
!        ----------
            NSect = NSect + 1               ! ... new section begins
            if ( save_Loc ) Loc ( NSect ) = iKey
            if ( save_Len ) Len ( NSect ) = 1

         end if
      end do

      return
      end subroutine NavInit_
!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: NavInit - Initialize navigators from sorting index of integer keys
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine NavInit_index ( Indx, Keys, NSect, Loc, Len )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,        intent (in)    ::
     .   Indx (:),  ! Sorting index
     .   Keys (:)   ! Keys
!
! !OUTPUT PARAMETERS: 
      integer,        intent (out)   ::
     .   NSect      ! Number of sections
      integer,        intent (out), optional ::
     .   Loc  (:),  ! Location and ...
     .   Len  (:)   ! ... size of each section 
!
! !REVISION HISTORY:
!     18Dec2002 - C. Redder - Original code
!
!-----------------------------------------------------------------------
      integer :: iKey, NKeys, iIndx1, iIndx2
      logical :: save_Loc, save_Len

      save_Loc = present ( Loc )
      save_Len = present ( Len )

      NSect = 0
      NKeys = size ( Indx )
      if ( NKeys .eq. 0 ) return               ! Nothing to do

      NSect = 1
      if ( save_Loc ) Loc ( 1 ) = 1            ! Begin first section
      if ( save_Len ) Len ( 1 ) = 1
      do iKey = 2, NKeys

!        If key does not change ... 
!        --------------------------
         iIndx1 = Indx ( iKey - 1 )
         iIndx2 = Indx ( iKey )
         if ( Keys ( iIndx1 ) .eq.
     .        Keys ( iIndx2 )) then            ! ... increment section size
            if ( save_Len ) Len ( NSect ) = Len ( NSect ) + 1

         else ! ...
!        ----------
            NSect = NSect + 1                  ! ... new section begins
            if ( save_Loc ) Loc ( NSect ) = iKey
            if ( save_Len ) Len ( NSect ) = 1

         end if
      end do

      return
      end subroutine NavInit_index
!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: NavNum - Return navigator index number for each vector element
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine NavNum ( Loc, Len, Vect )
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,        intent (in)    ::
     .   Loc  (:),  ! Sorting index
     .   Len  (:)   ! Keys
!
! !OUTPUT PARAMETERS: 
      integer,        intent (out)   ::
     .   Vect (:)   ! Vector
!
! !REVISION HISTORY:
!     18Dec2002 - C. Redder - Original code
!
!-----------------------------------------------------------------------
      integer :: iNav, NNav, iVect, iBeg, iEnd

      NNav = min ( size ( Loc ), size ( Len ))

      do iNav = 1, NNav
         iBeg = Loc ( iNav )
         iEnd = Len ( iNav ) + iBeg - 1
         do iVect = iBeg, iEnd
            Vect ( iVect ) = iNav
         end do
      end do

      return
      end subroutine NavNum
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: R2ManExp_ -- Represent reals as parameters for scientific notation
!
! !DESCRIPTION:
!     This routine returns the parameters for representation of real numbers
!     in scientific nontation.  That is, given the number of significant
!     digits, this routine returns the exponent and mantessa in integer format
!     so that R = Man * 10 ** ( Exp - NSigDigits + 1 ) where Man is the
!     mantessa stored as a signed integer, Exp is the exponenet and NSigDigits
!     is the number of significant digits and R is the given real number.
!
!     This routine was implemented to sort floating point keys with limited
!     number of significant digits and range of several orders of magnitude.
!     This sort can be accomplished by representing the real numbers as two
!     integer keys.  An example is the pressure level which usually reported
!     with a precision of about four digits but can have values that range
!     from 1100~mb down to 0.01~mb.  However, in order to properly sort, the
!     mantessas must all have the same sign.
!
! !INTERFACE:
      subroutine R2ManExp_ ( NSigDigits, R, Man, Exp )
      implicit   NONE
! !INPUT PARAMETERS:
      integer,       intent (in) ::
     .   NSigDigits  ! Number of significant digits
      real,          intent (in) ::
     .   R (:)       ! Real numbers 
!
! !OUTPUT PARAMETERS:
      integer,       intent (out) ::
     .   Man (:),    ! Mantessas
     .   Exp (:)     ! Exponents
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     30Jan2003  C. Redder  Origional code.
!
!EOP
!....................................................................

      integer :: NR, iR, NSDig, M, E, ManMax
      integer, parameter :: NSDigMax = precision ( R )
      double precision :: RR, AbsRR, LogAbsR
      double precision, parameter :: RMin = tiny ( R )

      NR           = size ( R )
      Man ( : NR ) = 0
      Exp ( : NR ) = 0
      NSDig        = min ( max  ( NSigDigits, 0 ), NSDigMax )
      if ( NSDig .eq. 0 ) return

      ManMax   = 10 ** NSDig
      do iR = 1, NR
         RR    = dble ( R   ( iR ))
         AbsRR = dble ( abs ( RR ))
         if ( AbsRR .lt. RMin ) then
            M = 0
            E = 1
         else
            LogAbsR = log10 (    AbsRR )
            E       = floor ( LogAbsR  )
            M       = nint  ( 10 ** ( LogAbsR - E + NSDig - 1 ))
            if ( RR .lt. 0.0d0  ) M = -M
            if ( M  .ge. ManMax ) then
               M = ManMax / 10
               E = E + 1
            end if 
         end if
         Man ( iR ) = M
         Exp ( iR ) = E
      end do

      return
      end subroutine R2ManExp_

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: R2ManExp_Indx -- Represent sorted reals as parameters for scientific notation
!
! !DESCRIPTION:
!     This routine returns the parameters for representation of sorted real
!     numbers in scientific nontation.  That is, given the number of
!     significant digits, this routine returns the exponent and mantessa in
!     integer format so that R = Man * 10 ** ( Exp - NSigDigits + 1 ) where
!     Man is the mantessa stored as a signed integer, Exp is the exponenet
!     and NSigDigits is the number of significant digits and R is the given
!     real number.
!
!     This routine was implemented to sort floating point keys with limited
!     number of significant digits and range of several orders of magnitude.
!     This sort can be accomplished by representing the real numbers as two
!     integer keys.  An example is the pressure level which usually reported
!     with a precision of about four digits but can have values that range
!     from 1100~mb down to 0.01~mb.  However, in order to properly sort, the
!     mantessas must all have the same sign.
!
! !INTERFACE:
      subroutine R2ManExp_Indx ( NSigDigits, Indx, R, Man, Exp )
      implicit   NONE
! !INPUT PARAMETERS:
      integer,       intent (in) ::
     .   NSigDigits, ! Number of significant digits
     .   Indx (:)    ! Sorting Indices
      real,          intent (in) ::
     .   R    (:)    ! Real numbers 
!
! !OUTPUT PARAMETERS:
      integer,       intent (out) ::
     .   Man  (:),   ! Mantessas
     .   Exp  (:)    ! Exponents
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     30Jan2003  C. Redder  Origional code.
!
!EOP
!....................................................................

      integer :: NR, iiR, iR, NSDig, M, E, ManMax
      integer, parameter :: NSDigMax = precision ( R )
      double precision :: RR, AbsRR, LogAbsR
      double precision, parameter :: RMin = tiny ( R )

      NR           = size ( Indx )
      Man ( : NR ) = 0
      Exp ( : NR ) = 0
      NSDig        = min ( max  ( NSigDigits, 0 ), NSDigMax )
      if ( NSDig .eq. 0 ) return

      ManMax   = 10 ** NSDig
      do iR = 1, NR
         iiR   = Indx ( iR )
         RR    = dble ( R   ( iiR ))
         AbsRR = dble ( abs ( RR ))
         if ( AbsRR .lt. RMin ) then
            M = 0
            E = 1
         else
            LogAbsR = log10 (    AbsRR )
            E       = floor ( LogAbsR  )
            M       = nint  ( 10 ** ( LogAbsR - E + NSDig - 1 ))
            if ( RR .lt. 0.0d0  ) M = -M
            if ( M  .ge. ManMax ) then
               M = ManMax / 10
               E = E + 1
            end if 
         end if
         Man ( iiR ) = M
         Exp ( iiR ) = E
      end do

      return
      end subroutine R2ManExp_Indx

!....................................................................
      end module m_RadSort
!====================================================================

