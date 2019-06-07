!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_Range -- Routines that get the ranges for given value(s) among sorted keys.
!
!
! !DESCRIPTION:
!     This module contains routines that search for a given integer value
!     or character string among keys sorted and/or rearranged in ascending
!     or descending order.  Since the keys are sorted, all entries with the
!     given value are assigned to a set defined by navigators, \verb|iBeg|
!     (the location of the first member) and \verb|Len| (the size or length
!     of the set).  If the value is not found, then the set length is zero
!     and the location, \verb|iBeg|, is assigned so that \verb|Keys[iBeg-1]
!     <~X <=~Keys[iBeg]|, where \verb|X| is the given value(s) and
!     \verb|Keys| are the keys.  An optional argument, \verb|iEed|, can
!     also be returned to specify the last element of the set.  If the
!     value is not found, then  \verb|Keys[iEnd] <=~X <~Keys[iEnd+1]|.
!
!     The following FORTRAN 90 program illustrates the use of the routines.  
! \begin{verbatim}
!     program test
!     use m_SortingTools, only : IndexSet,  IndexSort
!     use m_Range,        only : InitRange, GetRange
!     implicit NONE
!     integer, parameter :: NVal = 20
!     integer, dimension (NVal) :: ks = (/2,2,2,4,4,4,3,3,3,3,
!                                         1,1,1,1,7,7,4,4,6,8/)
!     integer, dimension (NVal) :: Indx
!     integer :: stat,  iVal
!     integer :: iBeg1, Len1, iEnd1, X
!
!     call IndexSet  ( Indx )
!     call IndexSort ( Indx, ks, stat = stat )
!
!     ks ( : NVal ) = (/( ks ( Indx ( iVal ) ), iVal = 1, NVal )/)
!
!     X=3
!     call InitRange ( ks,    iBeg1, Len1 )
!     call GetRange  ( ks, X, iBeg1, Len1 )
!     end program test
! \end{verbatim}
!     The routine \verb|IndexSet| initializes the sorting index array and
!     \verb|IndexSort| sort the array \verb|ks| which results in the
!     rearranged arrays.
! \begin{verbatim}
!     ks = (/1,1,1,1,2,2,2,3,3,3,3,4,4,4,4,4,6,7,7,8/)
! \end{verbatim}
!     The navigators , \verb|iBeg1| and \verb|Len1|, are then initialized
!     by the routine \verb|InitRange|, and assigned the values, 8 and 4.
!     If \verb|X|~=~5, then the navigators would then be assigned the
!     values 17 and 0.  If \verb|X|~=~9, then the values would be 21
!     and~0.
!
!     The routines can also be called to narrow the search by passing
!     navigators by searching among multiple keys as illustrated in the
!     following program.
! \begin{verbatim}
!     program test
!     use m_SortingTools, only : IndexSet,  IndexSort
!     use m_Range,        only : InitRange, GetRange
!     implicit NONE
!     integer, parameter :: NVal = 20
!     integer, dimension (NVal) :: ks = (/2,2,2,4,4,4,3,3,3,3,
!                                         1,1,1,1,7,7,4,4,6,8/)
!     integer, dimension (NVal) :: kt = (/6,4,5,7,4,3,6,3,3,3,
!    .                                    2,2,3,3,4,5,5,4,3,2/)
!     integer, dimension (NVal) :: Indx
!     integer :: stat, iVal
!     integer :: iBeg1, Len1, iEnd1, X
!
!     call IndexSet  ( Indx )
!     call IndexSort ( Indx, kt, stat = stat )
!     call IndexSort ( Indx, ks, stat = stat )
!
!     ks ( : NVal ) = (/( ks ( Indx ( iVal ) ), iVal = 1, NVal )/)
!     kt ( : NVal ) = (/( kt ( Indx ( iVal ) ), iVal = 1, NVal )/)
!
!     X=3
!     call InitRange ( ks,    iBeg1, Len1 )
!     call  GetRange ( ks, X, iBeg1, Len1 )
!     X=6
!     call  GetRange ( kt, X, iBeg1, Len1 )
!     end program test
! \end{verbatim}
!     Sorting and rearranging results in the following arrays:
! \begin{verbatim}
!     ks = (/1,1,1,1,2,2,2,3,3,3,3,4,4,4,4,4,6,7,7,8/)
!     kt = (/2,2,3,3,4,5,6,3,3,3,6,3,4,4,5,7,3,4,5,2/)
! \end{verbatim}
!     After the first call to \verb|GetRange|, the navigators have the
!     values, 8 and 4, as seen in the first program.  Then the navigators
!     are passed into the second call to the same routine.  During this
!     call, the routine searches for the given value, verb|X| (=6) among
!     elements 8 through 11 (inclusive) in the key, \verb|kt| and returns
!     the navigator values, 11 and 1.  Note that the arguments for the
!     navigators are passed in as input and output arguments so that
!     they must be initialized by a call to \verb|InitRange| or a
!     previous call to \verb|GetRange|.  Also note that only the portion
!     being searched (i.e elements 8 through 11) need to be sorted even
!     though the entire array of keys (this is, \verb|kt|) is not.
!     Finally, note that the search among the keys must be performed in
!     reverse order to the sort.  That is, the key, \verb|ks|, is sorted
!     first and searched last, and \verb|kt| is sorted last but searched
!     first.
!
!     The module contains routines that search with keys referenced by the
!     sorting indices passed in as the argument, \verb|Indx|.  The output
!     navigators then reference the elements in the sorting index array
!     rather than the keys.  For example, the first program can be modified
!     to the following:
! \begin{verbatim}
!     program test
!     use m_SortingTools, only : IndexSet,  IndexSort
!     use m_Range,        only : InitRange, GetIRange
!     implicit NONE
!     integer, parameter :: NVal = 20
!     integer, dimension (NVal) :: ks = (/2,2,2,4,4,4,3,3,3,3,
!                                         1,1,1,1,7,7,4,4,6,8/)
!     integer, dimension (NVal) :: Indx
!     integer :: stat,  iVal
!     integer :: iBeg1, Len1, iEnd1, X
!
!     call IndexSet  ( Indx )
!     call IndexSort ( Indx, ks, stat = stat )
!
!     X=3
!     call InitRange ( Indx,        iBeg1, Len1 )
!     call GetIRange ( Indx, ks, X, iBeg1, Len1 )
!     end program test
! \end{verbatim}
!     The call to the sorting routine produces the index array,
! \begin{verbatim}
!     Indx = (/11,12,13,14,1,2,3,7,8,9,10,4,5,6,17,18,19,15,16,20/)
! \end{verbatim}
!     and the call to \verb|GetIRange| returns the same navigators, 8 and 4,
!     which refer to the indices 7, 8, 9, 10 and to the array elements
!     \verb|ks(7)|, \verb|ks(8)|, \verb|ks(9)| and \verb|ks(10)|, the set
!     of keys with the given value \verb|X| (=3).  Note, that the
!     navigators are initialized by passing \verb|Indx| into the routine
!     \verb|InitRange| rather than the key, \verb|ks|.  In general, the
!     navigators should be initialized based on the size of the array with
!     the sorting indices rather than the keys since the sizes are not 
!     necessarily the same.
!
! !INTERFACE:
!
      module      m_Range
      implicit    NONE
      private	! except

!     Routine to initialize the navigator for the range
!     -------------------------------------------------
      public :: InitRange
!
!     Routines that reference the keys directly ...
!     ---------------------------------------------
      public :: Range     ! Returns the navigator
      public :: Range2    ! ... and end position of a given values/string
      public :: GetRange  ! Locate all occurrences of given value(s)/string(s)
!
!     ... and by sorting indices
!     --------------------------
      public :: IRange    ! Returns the navigator
      public :: IRange2   ! ... and end position of a given values/string
      public :: GetIRange ! Locate all occurrences of given value(s)/string(s)

      interface InitRange
         module procedure
     .      InitRange_Dim,    ! Initialize navigators by dimension of ...
     .      InitRange_IArr,   ! ... the given integer and
     .      InitRange_CArr    ! ... character array
      end interface
      interface Range
         module procedure
     .      Range_Int,        ! ... integer keys 
     .      Range_Char        ! ... character keys 

      end interface

      interface Range2
         module procedure
     .      Range2_Int,       ! ... integer keys
     .      Range2_Char       ! ... character keys

      end interface

      interface GetRange
         module procedure
     .      GetRange_Int,     ! ... integer keys
     .      GetRange_IntArr,  ! ... integer keys, array of given values
     .      GetRange_Char,    ! ... character keys
     .      GetRange_CharArr  ! ... character keys, array of given values

      end interface

      interface IRange
         module procedure
     .      IRange_Int,       ! ... integer keys
     .      IRange_Char       ! ... character keys

      end interface

      interface IRange2
         module procedure
     .      IRange2_Int,      ! ... integer keys
     .      IRange2_Char      ! ... character keys

      end interface

      interface GetIRange
         module procedure
     .      GetIRange_Int,    ! ... integer keys
     .      GetIRange_IntArr, ! ... integer keys, array of given values
     .      GetIRange_Char,   ! ... character keys
     .      GetIRange_CharArr ! ... character keys, array of given values

      end interface
!
! !REVISION HISTORY:
!     20Apr2001  C. Redder  Original code
!     13Jan2003  C. Redder  Added generic interface, InitRange.  Modified
!                           prologue as a result of the changes in the
!                           module procedures for the generic interfaces,
!                           GetRange and GetIRange.
!EOP
!-----------------------------------------------------------------

      contains

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  InitRange_Dim() --- Initializes navigators with a given array size
!
! !DESCRIPTION:
!     This routine initializes the navigators with a given array size
!
! !INTERFACE:
      subroutine InitRange_Dim ( ArrSz, iBeg, Len )
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer, intent (in) ::
     .   ArrSz ! size of the array
!
! !OUTPUT PARAMETERS: 
      integer, intent (out), dimension (:) ::
     .   iBeg, ! The initialized navigators
     .   Len
!
! !REVISION HISTORY: 
!     13Jan2003  Redder    Origional code.
!
! EOP
!-------------------------------------------------------------------------

      iBeg = 1
      Len  = ArrSz

      return
      end subroutine InitRange_Dim
*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  InitRange_IArr() --- Initializes navigators by determining the size of a given integer array
!
! !DESCRIPTION:
!     This routine initializes by determining the size of a given integer array
!
! !INTERFACE:
      subroutine InitRange_IArr ( Arr, iBeg, Len )
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer, intent (in),  dimension (:) ::
     .   Arr   ! Given array
!
! !OUTPUT PARAMETERS: 
      integer, intent (out), dimension (:) ::
     .   iBeg, ! The initialized navigators
     .   Len
!
! !REVISION HISTORY: 
!     13Jan2003  Redder    Origional code.
!
! EOP
!-------------------------------------------------------------------------

      call InitRange_Dim ( size ( Arr ), iBeg, Len )

      return
      end subroutine InitRange_IArr

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  InitRange_CArr() --- Initializes navigators by determining the size of a given char array
!
! !DESCRIPTION:
!     This routine initializes by determining the size of a given char array
!
! !INTERFACE:
      subroutine InitRange_CArr ( Arr, iBeg, Len )
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*), intent (in),  dimension (:) ::
     .   Arr   ! Given array
!
! !OUTPUT PARAMETERS: 
      integer,           intent (out), dimension (:) ::
     .   iBeg, ! The initialized navigators
     .   Len
!
! !REVISION HISTORY: 
!     13Jan2003  Redder    Origional code.
!
! EOP
!-------------------------------------------------------------------------

      call InitRange_Dim ( size ( Arr ), iBeg, Len )

      return
      end subroutine InitRange_CArr

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Range_Int() --- Returns the navigators for a given integer value
!
! !DESCRIPTION:
!     This function returns the navigator for a given integer value.
!
! !INTERFACE:
      function Range_Int ( Keys,  X,     ! Required and
     .                     iBeg0, Len0 ) ! optional parameters.
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer,       intent (in), dimension (:) ::
     .   Keys        ! Set of keys to be search
      integer,       intent (in)  ::
     .   X           ! The given value
      integer,       intent (in), optional ::
     .   iBeg0, Len0 ! The navigator defining the search boundaries.
!
! !OUTPUT PARAMETERS: 
      integer,     dimension ( 2 ) ::
     .   Range_Int   ! ( 1 ) = iBeg
                     ! ( 2 ) =  Len
!
! !REVISION HISTORY: 
!     20Apr2001  Redder    Origional code.
!     13Jan2003  Redder    Changes in call to GetRange_IntArr
!
! EOP
!-------------------------------------------------------------------------

      integer, dimension ( 1 ) :: X1, iBeg1, Len1

      X1    ( 1 ) = X
      iBeg1 ( 1 ) = 1
      if ( present ( iBeg0 ) ) iBeg1 ( 1 ) = iBeg0
      Len1  ( 1 ) = size ( Keys )
      if ( present (  Len0 ) )  Len1 ( 1 ) =  Len0
      call GetRange_IntArr ( Keys, X1, iBeg1, Len1 )
      Range_Int ( 1 ) = iBeg1 ( 1 )
      Range_Int ( 2 ) =  Len1 ( 1 )

      return
      end function Range_Int

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Range2_Int() --- Returns the navigator and end position for a given integer value
!
! !DESCRIPTION:
!     This function returns the navigator and end position for a given
!     integer value.
!
! !INTERFACE:
      function Range2_Int ( Keys,  X,     ! Required and
     .                      iBeg0, Len0 ) ! optional parameters.
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer,       intent (in), dimension (:) ::
     .   Keys        ! Set of keys to be search
      integer,       intent (in)  ::
     .   X           ! The given value
      integer,       intent (in), optional ::
     .   iBeg0, Len0 ! The navigator defining the search boundaries.
!
! !OUTPUT PARAMETERS: 
      integer,       dimension ( 3 ) ::
     .   Range2_Int  ! ( 1 ) = iBeg
                     ! ( 2 ) =  Len
                     ! ( 3 ) = iEnd
!
! !REVISION HISTORY: 
!     20Apr2001  Redder    Origional code.
!     13Jan2003  Redder    Changes in call to GetRange_IntArr
!
! EOP
!-------------------------------------------------------------------------

      integer, dimension ( 1 ) :: X1, iBeg1, Len1, iEnd1

      X1    ( 1 ) = X
      iBeg1 ( 1 ) = 1
      if ( present ( iBeg0 ) ) iBeg1 ( 1 ) = iBeg0
      Len1  ( 1 ) = size ( Keys )
      if ( present (  Len0 ) )  Len1 ( 1 ) =  Len0
      call GetRange_IntArr ( Keys, X1, iBeg1, Len1, iEnd1 )
      Range2_Int ( 1 ) = iBeg1 ( 1 )
      Range2_Int ( 2 ) =  Len1 ( 1 )
      Range2_Int ( 3 ) = iEnd1 ( 1 )

      return
      end function Range2_Int

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  GetRange_Int() --- Search for all occurrances of a given integer value
!
! !DESCRIPTION:
!     This routine searches for all occurrences of a given integer value
!
! !INTERFACE:
      subroutine GetRange_Int ( Keys, X, iBeg, Len, iEnd )
!
! !INPUT PARAMETERS: 
      integer, intent (in), dimension (:) ::
     .   Keys  ! Set of keys to be search
      integer, intent (in)    ::
     .   X     ! The given value
!
! !INPUT/OUTPUT PARAMETERS: 
      integer, intent (inout) ::
     .   iBeg, ! On input:  The navigator for the search boundaries
     .   Len   ! On output: The navigator defining the set with
               !   the given value.
!
! !OUTPUT PARAMETERS: 
      integer, intent (out), optional ::
     .   iEnd  ! The location of the last occurrence.
!
! !REVISION HISTORY: 
!     20Apr2001  Redder  Origional code.
!     13Jan2003  Redder  Removed the optional arguments, iBeg0 and Len0
!                        and modified iBeg and Len to be input/output
!
! EOP
!-------------------------------------------------------------------------

      integer, dimension ( 1 ) :: X1, iBeg1, Len1, iEnd1

      X1    ( 1 ) =  X
      iBeg1 ( 1 ) = iBeg
       Len1 ( 1 ) =  Len
      call GetRange_IntArr ( Keys, X1, iBeg1, Len1, iEnd1 )
      iBeg = iBeg1 ( 1 )
       Len =  Len1 ( 1 )
      if ( present ( iEnd ) ) iEnd = iEnd1 ( 1 ) 

      return
      end subroutine GetRange_Int

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  GetRange_IntArr() --- Search for all occurrances given integer value(s)
!
! !DESCRIPTION:
!     This routine searches for all occurrences given integer value(s).
!
! !INTERFACE:
      subroutine GetRange_IntArr ( Keys, X, iBeg, Len, iEnd )
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer, intent (in),    dimension (:) ::
     .   Keys, ! Set of keys to be search
     .   X     ! The given value(s)
!
! !INPUT/OUTPUT PARAMETERS: 
      integer, intent (inout), dimension (:) ::
     .   iBeg, ! On input:  The navigator(s) for the search boundaries
     .   Len   ! On output: The navigator(s) defining the set(s) with
               !   the given value(s).
!
! !OUTPUT PARAMETERS: 
      integer, intent (out),   dimension (:), optional ::
     .   iEnd  ! The locations of the last occurrences.
!
! !REVISION HISTORY: 
!     20Apr2001  Redder    Origional code.
!     13Jan2003  Redder    Removed the optional arguments, iBeg0 and Len0
!                          and modified iBeg and Len to be input/output
! EOP
!-------------------------------------------------------------------------
      integer :: XX, iX, NX, NKeys, iKey
      integer :: iMid1, iBeg1, iEnd1, Len1, iMid2, iBeg2, iEnd2
      logical :: decreasing

      NX    = size ( X    ) ! Number of searches 
      NKeys = size ( Keys ) ! Number of keys

      do iX = 1, NX
         XX    = X ( iX )

!        Set search boundaries
!        ---------------------
         iBeg1 = max ( iBeg ( iX ), 1 )
         iEnd1 = min (  Len ( iX ) + iBeg1 - 1, NKeys )
         Len1  = iEnd1 - iBeg1 + 1

!        Decreasing order?
!        -----------------
         if ( Len1 .gt. 0 )
     .      decreasing = Keys ( iEnd1 ) .lt. Keys ( iBeg1 )

!        Locate one end of the set
!        -------------------------
         iBeg1 = iBeg1 - 1
         iEnd1 = iEnd1 + 1
         iBeg2 = iBeg1
         iEnd2 = iEnd1
         LocateLoop1 : do iKey = iBeg1, iEnd1
            if ( iEnd1 - iBeg1 .le. 1 ) exit LocateLoop1
            iMid1 = ( iEnd1 + iBeg1 ) / 2
            if ( decreasing .neqv. XX .lt. Keys ( iMid1 ) ) then
               iEnd1 = iMid1
               if ( XX .ne. Keys ( iMid1 ) ) iEnd2 = iMid1
            else
               iBeg1 = iMid1
               if ( XX .ne. Keys ( iMid1 ) ) iBeg2 = iMid1
            end if

         end do LocateLoop1

!        Locate the other end
!        --------------------
         LocateLoop2 : do iKey = iBeg2, iEnd2
            if ( iEnd2 - iBeg2 .le. 1 ) exit LocateLoop2
            iMid2 = ( iEnd2 + iBeg2 ) / 2
            if ( decreasing .neqv. XX .gt. Keys ( iMid2 ) ) then
               iBeg2 = iMid2
            else
               iEnd2 = iMid2
            end if
         end do LocateLoop2

         Len  ( iX ) = abs ( iBeg2 - iBeg1 )
         iBeg ( iX ) = min ( iBeg1,  iBeg2 ) + 1

      end do

!     If desired, find the end position
!     ---------------------------------
      if ( present ( iEnd ) ) then
         do iX = 1, NX
            iEnd ( iX ) = iBeg ( iX ) + Len ( iX ) - 1
         end do
      end if

      return
      end subroutine GetRange_IntArr

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  IRange_Int() --- Returns the index navigators for a given integer value
!
! !DESCRIPTION:
!     This function returns the index navigator for a given integer value.
!
! !INTERFACE:
      function IRange_Int ( Indx,  Keys,  X, ! Required and
     .                      iBeg0, Len0 )    ! optional parameters.
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer,       intent (in), dimension (:) ::
     .   Indx,       ! The sorting indices
     .   Keys        ! Set of keys to be search
      integer,       intent (in)  ::
     .   X           ! The given value
      integer,       intent (in), optional ::
     .   iBeg0, Len0 ! The navigator defining the search boundaries.
!
! !OUTPUT PARAMETERS: 
      integer,     dimension ( 2 ) ::
     .   IRange_Int  ! ( 1 ) = iBeg
                     ! ( 2 ) =  Len
!
! !REVISION HISTORY: 
!     20Apr2001  Redder    Origional code.
!     13Jan2003  Redder    Changes in call to GetIRange_IntArr
!
! EOP
!-------------------------------------------------------------------------

      integer, dimension ( 1 ) :: X1, iBeg1, Len1

      X1    ( 1 ) = X
      iBeg1 ( 1 ) = 1
      if ( present ( iBeg0 ) ) iBeg1 ( 1 ) = iBeg0
      Len1  ( 1 ) = size ( Keys )
      if ( present (  Len0 ) )  Len1 ( 1 ) =  Len0
      call GetIRange_IntArr ( Indx, Keys, X1, iBeg1, Len1 )
      IRange_Int ( 1 ) = iBeg1 ( 1 )
      IRange_Int ( 2 ) =  Len1 ( 1 )

      return
      end function IRange_Int

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  IRange2_Int() --- Returns the index navigator and end position for a given integer value
!
! !DESCRIPTION:
!     This function returns the index navigator and end position for a given
!     integer value.
!
! !INTERFACE:
      function IRange2_Int ( Indx,  Keys, X, ! Required and
     .                       iBeg0, Len0 )   ! optional parameters.
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer,       intent (in), dimension (:) ::
     .   Indx        ! The sorting indices
      integer,       intent (in), dimension (:) ::
     .   Keys        ! Set of keys to be search
      integer,       intent (in)  ::
     .   X           ! The given value
      integer,       intent (in), optional ::
     .   iBeg0, Len0 ! The navigator defining the search boundaries.
!
! !OUTPUT PARAMETERS: 
      integer,       dimension ( 3 ) ::
     .   IRange2_Int ! ( 1 ) = iBeg
                     ! ( 2 ) =  Len
                     ! ( 3 ) = iEnd
!
! !REVISION HISTORY: 
!     20Apr2001  Redder    Origional code.
!     13Jan2003  Redder    Changes in call to GetIRange_IntArr
!
! EOP
!-------------------------------------------------------------------------

      integer, dimension ( 1 ) :: X1, iBeg1, Len1, iEnd1

      X1    ( 1 ) = X
      iBeg1 ( 1 ) = 1
      if ( present ( iBeg0 ) ) iBeg1 ( 1 ) = iBeg0
      Len1  ( 1 ) = size ( Keys )
      if ( present (  Len0 ) )  Len1 ( 1 ) =  Len0
      call GetIRange_IntArr ( Indx, Keys, X1, iBeg1, Len1, iEnd1 )
      IRange2_Int ( 1 ) = iBeg1 ( 1 )
      IRange2_Int ( 2 ) =  Len1 ( 1 )
      IRange2_Int ( 3 ) = iEnd1 ( 1 )

      return
      end function IRange2_Int

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  GetIRange_Int() --- Search for all occurrances of a given integer value
!
! !DESCRIPTION:
!     This routine searches for all occurrences of a given integer value
!
! !INTERFACE:
      subroutine GetIRange_Int ( Indx, Keys, X, iBeg, Len, iEnd )
!
! !INPUT PARAMETERS: 
      integer, intent (in), dimension (:) ::
     .   Indx, ! The sorting indices
     .   Keys  ! Set of keys to be search
      integer, intent (in)    ::
     .   X     ! The given value
!
! !INPUT/OUTPUT PARAMETERS: 
      integer, intent (inout) ::
     .   iBeg, ! On input:  The navigator for the search boundaries
     .   Len   ! On output: The navigator defining the set with
               !   the given value.
!
! !OUTPUT PARAMETERS: 
      integer, intent (out), optional ::
     .   iEnd  ! The location of the last occurrence.
!
! !REVISION HISTORY: 
!     20Apr2001  Redder  Origional code.
!     13Jan2003  Redder  Removed the optional arguments, iBeg0 and Len0
!                        and modified iBeg and Len to be input/output
!
! EOP
!-------------------------------------------------------------------------

      integer, dimension ( 1 ) :: X1, iBeg1, Len1, iEnd1

      X1    ( 1 ) =  X
      iBeg1 ( 1 ) = iBeg
       Len1 ( 1 ) =  Len
      call GetIRange_IntArr ( Indx, Keys, X1, iBeg1, Len1, iEnd1 )
      iBeg = iBeg1 ( 1 )
       Len =  Len1 ( 1 )
      if ( present ( iEnd ) ) iEnd = iEnd1 ( 1 ) 

      return
      end subroutine GetIRange_Int

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  GetIRange_IntArr() --- Search for all occurrances given integer value(s)
!
! !DESCRIPTION:
!     This routine searches for all occurrences given integer value(s).
!
! !INTERFACE:
      subroutine GetIRange_IntArr ( Indx, Keys, X, iBeg, Len ,iEnd )
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer, intent (in),    dimension (:) ::
     .   Indx, ! The sorting indices
     .   Keys, ! Set of keys to be search
     .   X     ! The given value(s)
!
! !INPUT/OUTPUT PARAMETERS: 
      integer, intent (inout), dimension (:) ::
     .   iBeg, ! On input:  The navigator(s) for the search boundaries
     .   Len   ! On output: The navigator(s) defining the set(s) with
               !   the given value(s).
!
! !OUTPUT PARAMETERS: 
      integer, intent (out),   dimension (:), optional ::
     .   iEnd  ! The locations of the last occurrences.
!
! !REVISION HISTORY: 
!     20Apr2001  Redder    Origional code.
!     17Jan2002  Redder    Fixed bug in determining whether the value for
!                          the logical variable, decreasing.
!     13Jan2003  Redder    Removed the optional arguments, iBeg0 and Len0
!                          and modified iBeg and Len to be input/output
! EOP
!-------------------------------------------------------------------------

      integer :: XX, iX, NX,   NKeys, iKey, iIMid
      integer :: iMid1, iBeg1, iEnd1, Len1, iMid2, iBeg2, iEnd2
      logical :: decreasing

      NX    = size ( X    ) ! Number of searches 
      NKeys = size ( Indx ) ! Number of keys

      do iX = 1, NX
         XX    = X ( iX )

!        Set search boundaries
!        ---------------------
         iBeg1 = max ( iBeg ( iX ), 1 )
         iEnd1 = min (  Len ( iX ) + iBeg1 - 1, NKeys )
         Len1  = iEnd1 - iBeg1 + 1

!        Decreasing order?
!        -----------------
         if ( Len1 .gt. 0 )
     .      decreasing = Keys ( Indx ( iEnd1 )) .lt.
     .                   Keys ( Indx ( iBeg1 ))

!        Locate one end of the set
!        -------------------------
         iBeg1 = iBeg1 - 1
         iEnd1 = iEnd1 + 1
         iBeg2 = iBeg1
         iEnd2 = iEnd1
         LocateLoop1 : do iKey = iBeg1, iEnd1
            if ( iEnd1 - iBeg1 .le. 1 ) exit LocateLoop1
            iMid1 = ( iEnd1 + iBeg1 ) / 2
            iIMid =    Indx ( iMid1 )
            if ( decreasing .neqv. XX .lt. Keys ( iIMid ) ) then
               iEnd1 = iMid1
               if ( XX .ne. Keys ( iIMid ) ) iEnd2 = iMid1
            else
               iBeg1 = iMid1
               if ( XX .ne. Keys ( iIMid ) ) iBeg2 = iMid1
            end if

         end do LocateLoop1

!        Locate the other end
!        --------------------
         LocateLoop2 : do iKey = iBeg2, iEnd2
            if ( iEnd2 - iBeg2 .le. 1 ) exit LocateLoop2
            iMid2 = ( iEnd2 + iBeg2 ) / 2
            if ( decreasing .neqv. XX .gt. Keys ( Indx ( iMid2 ))) then
               iBeg2 = iMid2
            else
               iEnd2 = iMid2
            end if
         end do LocateLoop2

         Len  ( iX ) = abs ( iBeg2 - iBeg1 )
         iBeg ( iX ) = min ( iBeg1,  iBeg2 ) + 1

      end do

!     If desired, find the end position
!     ---------------------------------
      if ( present ( iEnd ) ) then
         do iX = 1, NX
            iEnd ( iX ) = iBeg ( iX ) + Len ( iX ) - 1
         end do
      end if

      return
      end subroutine GetIRange_IntArr

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Range_Char() --- Returns the navigators for a given character string
!
! !DESCRIPTION:
!     This function returns the navigator for a given character string.
!
! !INTERFACE:
      function Range_Char ( Keys,  X,     ! Required and
     .                      iBeg0, Len0 ) ! optional parameters
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*), intent (in), dimension (:) ::
     .   Keys            ! Set of keys to be search
      character (len=*), intent (in)  ::
     .   X               ! The given value
      integer,           intent (in), optional ::
     .   iBeg0, Len0     ! The navigator defining the search boundaries.
!
! !OUTPUT PARAMETERS: 
      integer,           dimension ( 2 ) ::
     .   Range_Char      ! ( 1 ) = iBeg
                         ! ( 2 ) =  Len
!
! !REVISION HISTORY: 
!     20Apr2001  Redder    Origional code.
!     13Jan2003  Redder    Changes in call to GetRange_CharArr
!
! EOP
!-------------------------------------------------------------------------

      integer, dimension ( 1 ) :: iBeg1, Len1

      iBeg1 ( 1 ) = 1
      if ( present ( iBeg0 ) ) iBeg1 ( 1 ) = iBeg0
      Len1  ( 1 ) = size ( Keys )
      if ( present (  Len0 ) )  Len1 ( 1 ) =  Len0
      call GetRange_CharArr ( Keys, (/ X /), iBeg1, Len1 )
      Range_Char ( 1 ) = iBeg1 ( 1 )
      Range_Char ( 2 ) =  Len1 ( 1 )

      return
      end function Range_Char

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Range2_Char() --- Returns the navigators and end poistion for a given character string
!
! !DESCRIPTION:!
!     This function returns the navigator and end position for a given
!     character string.
!
! !INTERFACE:
      function Range2_Char ( Keys,  X,     ! Required and
     .                       iBeg0, Len0 ) ! optional parameters
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*), intent (in), dimension (:) ::
     .   Keys            ! Set of keys to be search
      character (len=*), intent (in)  ::
     .   X               ! The given value
      integer,           intent (in), optional ::
     .   iBeg0, Len0     ! The navigator defining the search boundaries.

!
! !OUTPUT PARAMETERS: 
      integer,           dimension ( 3 ) ::
     .   Range2_Char     ! ( 1 ) = iBeg
                         ! ( 2 ) =  Len
                         ! ( 3 ) = iEnd
!
! !REVISION HISTORY: 
!     20Apr2001  Redder    Origional code.
!     13Jan2003  Redder    Changes in call to GetRange_CharArr
!
! EOP
!-------------------------------------------------------------------------

      integer, dimension ( 1 ) :: iBeg1, Len1, iEnd1

      iBeg1 ( 1 ) = 1
      if ( present ( iBeg0 ) ) iBeg1 ( 1 ) = iBeg0
      Len1  ( 1 ) = size ( Keys )
      if ( present (  Len0 ) )  Len1 ( 1 ) =  Len0
      call GetRange_CharArr ( Keys, (/ X /), iBeg1, Len1, iEnd1 )
      Range2_Char ( 1 ) = iBeg1 ( 1 )
      Range2_Char ( 2 ) =  Len1 ( 1 )
      Range2_Char ( 3 ) = iEnd1 ( 1 )

      return
      end function Range2_Char

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  GetRange_Char() --- Search for all occurrances of given character string(s)
!
! !DESCRIPTION:
!     This routine searches for all occurrences of given character string(s).
!
! !INTERFACE:
      subroutine GetRange_Char ( Keys, X, iBeg, Len, iEnd )
!
! !INPUT PARAMETERS: 
      character (len=*), intent (in), dimension (:) ::
     .   Keys  ! Set of keys to be search
      character (len=*), intent (in)    ::
     .   X     ! The given value
!
! !INPUT/OUTPUT PARAMETERS: 
      integer,           intent (inout) ::
     .   iBeg, ! On input:  The navigator for the search boundaries
     .   Len   ! On output: The navigator defining the set with
               !   the given value.
!
! !OUTPUT PARAMETERS: 
      integer,           intent (out), optional ::
     .   iEnd  ! The location of the last occurrence.
!
! !REVISION HISTORY: 
!     20Apr2001  Redder  Origional code.
!     13Jan2003  Redder  Removed the optional arguments, iBeg0 and Len0
!                        and modified iBeg and Len to be input/output
! EOP
!-------------------------------------------------------------------------

      integer, dimension ( 1 ) :: iBeg1, Len1, iEnd1

      iBeg1 ( 1 ) = iBeg
       Len1 ( 1 ) =  Len
      call GetRange_CharArr ( Keys, (/ X /), iBeg1, Len1, iEnd1 )
      iBeg = iBeg1 ( 1 )
       Len =  Len1 ( 1 )
      if ( present ( iEnd ) ) iEnd = iEnd1 ( 1 ) 

      return
      end subroutine GetRange_Char

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  GetRange_CharArr() --- Search for all occurrances of given character string(s)
!
! !DESCRIPTION:
!     This routine searches for all occurrences of given character string(s).
!
! !INTERFACE:
      subroutine GetRange_CharArr ( Keys, X, iBeg, Len, iEnd )
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*), intent (in),    dimension (:) ::
     .   Keys, ! Set of keys to be search
     .   X     ! The given value(s)
!
! !INPUT/OUTPUT PARAMETERS: 
      integer,           intent (inout), dimension (:) ::
     .   iBeg, ! On input:  The navigator(s) for the search boundaries
     .   Len   ! On output: The navigator(s) defining the set(s) with
               !   the given value(s).
!
! !OUTPUT PARAMETERS: 
      integer, optional, intent (out),   dimension (:) ::
     .   iEnd  ! The locations of the last occurrences.
!
! !REVISION HISTORY: 
!     20Apr2001  Redder    Origional code.
!     17Jan2002  Redder    Fixed bug in determining whether the value for
!                          the logical variable, decreasing.
!     13Jan2003  Redder    Removed the optional arguments, iBeg0 and Len0
!                          and modified iBeg and Len to be input/output
!     09Mar2007  Redder    Replaced .lt. and .gt. operators with the
!                          function LLT and LGT to ensure reproducibility
!                          among different computer platforms and
!                          consistency with sort routines
! EOP
!-------------------------------------------------------------------------

      integer :: itemp
      integer :: iX, NX, NKeys, iKey
      integer :: iMid1, iBeg1, iEnd1, Len1, iMid2, iBeg2, iEnd2
      logical :: decreasing

      NX    = size ( X    ) ! Number of searches 
      NKeys = size ( Keys ) ! Number of keys

      do iX = 1, NX

!        Set search boundaries
!        ---------------------
         iBeg1 = max ( iBeg ( iX ), 1 )
         iEnd1 = min (  Len ( iX ) + iBeg1 - 1, NKeys )
         Len1  = iEnd1 - iBeg1 + 1

!        Decreasing order?
!        -----------------
         if ( Len1 .gt. 0 )
     .      decreasing = LLT ( Keys ( iEnd1 ), Keys ( iBeg1 ))

!        Locate one end of the set
!        -------------------------
         iBeg1 = iBeg1 - 1
         iEnd1 = iEnd1 + 1
         iBeg2 = iBeg1
         iEnd2 = iEnd1
         LocateLoop1 : do iKey = iBeg1, iEnd1
            if ( iEnd1 - iBeg1 .le. 1 ) exit LocateLoop1
            iMid1 = ( iEnd1 + iBeg1 ) / 2
            if ( decreasing .neqv.
     .           LLT ( X ( iX ),  Keys ( iMid1 ))) then
               iEnd1 = iMid1
               if ( X ( iX ) .ne. Keys ( iMid1 ) ) iEnd2 = iMid1
            else
               iBeg1 = iMid1
               if ( X ( iX ) .ne. Keys ( iMid1 ) ) iBeg2 = iMid1
            end if

         end do LocateLoop1

!        Locate the other end
!        --------------------
         LocateLoop2 : do iKey = iBeg2, iEnd2
            if ( iEnd2 - iBeg2 .le. 1 ) exit LocateLoop2
            iMid2 = ( iEnd2 + iBeg2 ) / 2
            if ( decreasing .neqv.
     .           LGT ( X ( iX ),  Keys ( iMid2 ))) then
               iBeg2 = iMid2
            else
               iEnd2 = iMid2
            end if
         end do LocateLoop2

          Len ( iX ) = abs ( iBeg2 - iBeg1 )
         iBeg ( iX ) = min ( iBeg1,  iBeg2 ) + 1

      end do

!     If desired, find the end position
!     ---------------------------------
      if ( present ( iEnd ) ) then
         do iX = 1, NX
            iEnd ( iX ) = iBeg ( iX ) + Len ( iX ) - 1
         end do
      end if

      return
      end subroutine GetRange_CharArr
*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  IRange_Char() --- Returns the index navigators for a given character string
!
! !DESCRIPTION:
!     This function returns the index navigator for a given character string.
!
! !INTERFACE:
      function IRange_Char ( Indx,  Keys, X, ! Required and
     .                       iBeg0, Len0 )   ! optional parameters
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer,           intent (in), dimension (:) ::
     .   Indx            ! The sorting indices
      character (len=*), intent (in), dimension (:) ::
     .   Keys            ! Set of keys to be search
      character (len=*), intent (in)  ::
     .   X               ! The given value
      integer,           intent (in), optional ::
     .   iBeg0, Len0     ! The navigator defining the search boundaries.
!
! !OUTPUT PARAMETERS: 
      integer,           dimension ( 2 ) ::
     .   IRange_Char     ! ( 1 ) = iBeg
                         ! ( 2 ) =  Len
!
! !REVISION HISTORY: 
!     20Apr2001  Redder    Origional code.
!     13Jan2003  Redder    Changes in call to GetIRange_CharArr
!
! EOP
!-------------------------------------------------------------------------

      integer, dimension ( 1 ) :: iBeg1, Len1

      iBeg1 ( 1 ) = 1
      if ( present ( iBeg0 ) ) iBeg1 ( 1 ) = iBeg0
      Len1  ( 1 ) = size ( Keys )
      if ( present (  Len0 ) )  Len1 ( 1 ) =  Len0
      call GetIRange_CharArr ( Indx, Keys, (/ X /), iBeg1, Len1 )
      IRange_Char ( 1 ) = iBeg1 ( 1 )
      IRange_Char ( 2 ) =  Len1 ( 1 )

      return
      end function IRange_Char

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  IRange2_Char() --- Returns the index navigators and end position for a given character string
!
! !DESCRIPTION:!
!     This function returns the index navigator and end position for a given
!     character string.
!
! !INTERFACE:
      function IRange2_Char ( Indx,  Keys, X, ! Required and
     .                        iBeg0, Len0 )   ! optional parameters
!
! !INPUT PARAMETERS:
      implicit NONE
      integer,           intent (in), dimension (:) ::
     .   Indx            ! The sorting indices
      character (len=*), intent (in), dimension (:) ::
     .   Keys            ! Set of keys to be search
      character (len=*), intent (in)  ::
     .   X               ! The given value
      integer,           intent (in), optional ::
     .   iBeg0, Len0     ! The navigator defining the search boundaries.

!
! !OUTPUT PARAMETERS: 
      integer,           dimension ( 3 ) ::
     .   IRange2_Char    ! ( 1 ) = iBeg
                         ! ( 2 ) =  Len
                         ! ( 3 ) = iEnd
!
! !REVISION HISTORY: 
!     20Apr2001  Redder    Origional code.
!     13Jan2003  Redder    Changes in call to GetIRange_CharArr
!
! EOP
!-------------------------------------------------------------------------

      integer, dimension ( 1 ) :: iBeg1, Len1, iEnd1

      iBeg1 ( 1 ) = 1
      if ( present ( iBeg0 ) ) iBeg1 ( 1 ) = iBeg0
      Len1  ( 1 ) = size ( Keys )
      if ( present (  Len0 ) )  Len1 ( 1 ) =  Len0
      call GetIRange_CharArr ( Indx, Keys, (/ X /), iBeg1, Len1, iEnd1 )
      IRange2_Char ( 1 ) = iBeg1 ( 1 )
      IRange2_Char ( 2 ) =  Len1 ( 1 )
      IRange2_Char ( 3 ) = iEnd1 ( 1 )

      return
      end function IRange2_Char

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  GetIRange_Char() --- Search for all occurrances of given character string(s)
!
! !DESCRIPTION:
!     This routine searches for all occurrences of given character string(s).
!
! !INTERFACE:
      subroutine GetIRange_Char ( Indx, Keys, X, iBeg, Len, iEnd )
!
! !INPUT PARAMETERS: 
      integer,           intent (in), dimension (:) ::
     .   Indx  ! The sorting indices 
      character (len=*), intent (in), dimension (:) ::
     .   Keys  ! Set of keys to be search
      character (len=*), intent (in)    ::
     .   X     ! The given value
!
! !INPUT/OUTPUT PARAMETERS: 
      integer,           intent (inout) ::
     .   iBeg, ! On input:  The navigator for the search boundaries
     .   Len   ! On output: The navigator defining the set with
               !   the given value.
!
! !OUTPUT PARAMETERS: 
      integer,           intent (out), optional ::
     .   iEnd  ! The location of the last occurrence.
!
! !REVISION HISTORY: 
!     20Apr2001  Redder  Origional code.
!     13Jan2003  Redder  Removed the optional arguments, iBeg0 and Len0
!                        and modified iBeg and Len to be input/output
! EOP
!-------------------------------------------------------------------------

      integer, dimension ( 1 ) :: iBeg1, Len1, iEnd1

      iBeg1 ( 1 ) = iBeg
       Len1 ( 1 ) =  Len
      call GetIRange_CharArr ( Indx, Keys, (/ X /), iBeg1, Len1, iEnd1 )
      iBeg = iBeg1 ( 1 )
       Len =  Len1 ( 1 )
      if ( present ( iEnd ) ) iEnd = iEnd1 ( 1 ) 

      return
      end subroutine GetIRange_Char

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  GetIRange_CharArr() --- Search for all occurrances of given character string(s)
!
! !DESCRIPTION:
!     This routine searches for all occurrences of given character string(s).
!
! !INTERFACE:
      subroutine GetIRange_CharArr ( Indx, Keys, X, iBeg, Len, iEnd )
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer,           intent (in),    dimension (:) ::
     .   Indx  ! The sorting indices 
      character (len=*), intent (in),    dimension (:) ::
     .   Keys, ! Set of keys to be search
     .   X     ! The given value(s)
!
! !INPUT/OUTPUT PARAMETERS: 
      integer,           intent (inout), dimension (:) ::
     .   iBeg, ! On input:  The navigator(s) for the search boundaries
     .   Len   ! On output: The navigator(s) defining the set(s) with
               !   the given value(s).
!
! !OUTPUT PARAMETERS: 
      integer, optional, intent (out),   dimension (:) ::
     .   iEnd  ! The locations of the last occurrences.
!
! !REVISION HISTORY: 
!     20Apr2001  Redder    Origional code.
!     17Jan2002  Redder    Fixed bug in determining whether the value for
!                          the logical variable, decreasing.
!     13Jan2003  Redder    Removed the optional arguments, iBeg0 and Len0
!                          and modified iBeg and Len to be input/output
!     09Mar2007  Redder    Replaced .lt. and .gt. operators with the
!                          function LLT and LGT to ensure reproducibility
!                          among different computer platforms and
!                          consistency with sort routines.
! EOP
!-------------------------------------------------------------------------

      integer :: iX, NX, NKeys, iKey,  iIMid
      integer :: iMid1,  iBeg1, iEnd1, Len1, iMid2, iBeg2, iEnd2
      logical :: decreasing

      NX    = size ( X    ) ! Number of searches 
      NKeys = size ( Indx ) ! Number of keys

      do iX = 1, NX

!        Set search boundaries
!        ---------------------
         iBeg1 = max ( iBeg ( iX ), 1 )
         iEnd1 = min (  Len ( iX ) + iBeg1 - 1, NKeys )
         Len1  = iEnd1 - iBeg1 + 1

!        Decreasing order?
!        -----------------
         if ( Len1 .gt. 0 )
     .      decreasing = LLT ( Keys ( Indx ( iEnd1 )),
     .                         Keys ( Indx ( iBeg1 )))

!        Locate one end of the set
!        -------------------------
         iBeg1 = iBeg1 - 1
         iEnd1 = iEnd1 + 1
         iBeg2 = iBeg1
         iEnd2 = iEnd1
         LocateLoop1 : do iKey = iBeg1, iEnd1
            if ( iEnd1 - iBeg1 .le. 1 ) exit LocateLoop1
            iMid1 = ( iEnd1 + iBeg1 ) / 2
            iIMid =    Indx ( iMid1 )
            if ( decreasing .neqv.
     .           LLT ( X ( iX ),  Keys ( iIMid ))) then
               iEnd1 = iMid1
               if ( X ( iX ) .ne. Keys ( iIMid ) ) iEnd2 = iMid1
            else
               iBeg1 = iMid1
               if ( X ( iX ) .ne. Keys ( iIMid ) ) iBeg2 = iMid1
            end if

         end do LocateLoop1

!        Locate the other end
!        --------------------
         LocateLoop2 : do iKey = iBeg2, iEnd2
            if ( iEnd2 - iBeg2 .le. 1 ) exit LocateLoop2
            iMid2 = ( iEnd2 + iBeg2 ) / 2
            if ( decreasing .neqv.
     .           LGT ( X ( iX ),  Keys ( Indx ( iMid2 )))) then
               iBeg2 = iMid2
            else
               iEnd2 = iMid2
            end if
         end do LocateLoop2

          Len ( iX ) = abs ( iBeg2 - iBeg1 )
         iBeg ( iX ) = min ( iBeg1,  iBeg2 ) + 1

      end do

!     If desired, find the end position
!     ---------------------------------
      if ( present ( iEnd ) ) then
         do iX = 1, NX
            iEnd ( iX ) = iBeg ( iX ) + Len ( iX ) - 1
         end do
      end if

      return
      end subroutine GetIRange_CharArr
*....................................................................

      end module m_Range
*====================================================================
