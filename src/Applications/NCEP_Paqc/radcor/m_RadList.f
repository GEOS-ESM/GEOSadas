!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_RadList -- Radcor utility routines for searching lists
!
! !DESCRIPTION:
!
! !INTERFACE:
!
      module      m_RadList
      implicit    NONE
      private   ! except

      public ::
     .   IndexMap,       ! Map indices 
     .   Get_kxMeta      ! Get OMS file name and list of kx's
      interface IndexMap
         module procedure
     .      IndexMapR2R, ! ... from real array to real array
     .      IndexMapI2I  ! ... from integer array to integer array
      end interface
! 
! !REVISION HISTORY:
!     10Dec2002  C. Redder  Original code
!EOP
!-----------------------------------------------------------------
      character (len=*), parameter :: MyModule = 'm_RadList'

      contains
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Get_kxMeta () --- Get kx meta-data 
!
! !DESCRIPTION:
!     This routine scans an array of character strings to obtain certain
!     parameters.  The parameters include the name of the OMS file for
!     raobs (kx0=7) or for a given kx0.  Then, if desired, the routine
!     determines the list of kx's that correspond to the OMS file.
!
! !INTERFACE:
      subroutine Get_kxMeta ( kxMeta, OMSFile, stat, nkx, kxList, kx0 ) 
      use m_TextUtil, only : XML, FXML
      use m_AdvError, only : WPErr, ItoA, Alloc
!
! !INPUT PARAMETERS:
      implicit   NONE
      character (len=*),   intent (in)  ::
     .   kxMeta (:)        ! Meta data information for each data source index
      integer,             intent (in),  optional ::
     .   kx0               ! Base data type index.  Default: kx0 = 7
!
! !OUTPUT PARAMETERS:
      character (len=*),   intent (out) ::
     .   OMSFile           ! Name of OMS file
      integer,             intent (out) ::
     .   stat              ! Returned status code
      integer,             intent (out), optional ::
     .   nkx,              ! Report number
     .   kxList (:)        ! Layer number where 1 at and below station level
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     09Dec2002  C. Redder   Origional code
!
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName  =  MyModule // '::Get_kxMeta',
     .   TagName = 'file' 
      integer,           parameter ::
     .   kx0_Def = 7
      character (len=len(kxMeta)), dimension (:), allocatable ::
     .   FileNames
      integer :: kx_0, kxMax, n_kx, ikx, istat
      logical :: XML_format, get_kxList, get_nkx

!     Impliment option to get list of data type indices
!     -------------------------------------------------
      get_nkx    = .false.
      if ( present ( nkx    )) get_nkx    = .true.
      get_kxList = .false.
      if ( present ( kxList )) get_kxList = .true.

!     ... and to set the base data type index
!     ---------------------------------------
      kxMax = size ( kxMeta )
      kx_0  = kx0_Def
      if ( present ( kx0 )) kx_0 = kx0
      if ( kx_0 .lt. 1 .or.             ! ... and perform range check
     .     kx_0 .gt. kxMax ) then
         call WPErr ( MyName, 'kx0 (='
     .                     //  trim ( ItoA ( kx_0  )) // ') '
     .                     // 'is outside of the valid range (=[1,'
     .                     //  trim ( ItoA ( kxMax )) // '])')
         stat = 1
         return
      end if

!     Get OMS file name from kx_meta
!     ------------------------------ 
      XML_format = .true.              ! ... if in XML format
      OMSFile    =  FXML ( kxMeta ( kx_0 ), TagName, stat )

      if      ( stat .eq. 1 ) then     ! ... if not in XML format (i.e. the 
         OMSFile    =  kxMeta ( kx_0 ) !     status code indicates the start
         XML_format = .false.          !     tag is absent)

      else if ( stat .ne. 0 ) then     ! Error handling
         call WPErr ( 'm_TextUtil::FXML',
     .                 FXML ( kxMeta ( kx_0 ), TagName, istat ))
         call WPErr ( MyName, '   kx0    = ' // trim ( ItoA   ( kx_0 ))
     .             // '\n\H{12}   kxMeta = ' // trim ( kxMeta ( kx_0 )))
         stat = 2
         return

      end if

!     Return with valid status code if the list
!     size nor the list itself is desired
!     -----------------------------------------
      stat = 0
      if ( .not. get_nkx .and. .not. get_kxList ) return

!     Allocate scratch space
!     ----------------------
      allocate ( FileNames ( kxMax ), stat = stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, Alloc ( stat, 'FileNames', kxMax ))
         stat = 3
         return
      end if

!     Get file names if ...
!     ---------------------
      if ( XML_format ) then           ! ... in XML format
         call XML ( kxMeta, TagName, FileNames, stat )
         if ( stat .ne. 0 ) then
            call WPErr ( 'm_TextUtil::XML',
     .                    FXML ( kxMeta ( stat ), TagName, istat ))
            call WPErr (  MyName,
     .                '   kx     = ' // trim ( ItoA   ( stat ))
     .     // '\n\H{12}   kxMeta = ' // trim ( kxMeta ( stat )))
            deallocate (  FileNames )
            stat = 2
            return
         end if
      else
         FileNames = kxMeta            ! ... not in XML format
      end if

!     Get list size and list of kx's with the OMS filename
!     ----------------------------------------------------
      n_kx = 0
      do ikx = 1, kxMax
         if ( FileNames ( ikx ) .eq. OMSFile ) then
            n_kx = n_kx + 1
            if ( get_kxList ) kxList ( n_kx ) = ikx
         end if
      end do

      if ( get_nkx ) nkx = n_kx

      deallocate ( FileNames )

      return
      end subroutine Get_kxMeta

!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: IndexMapR2R - Map indices from a real array to real array
!
! !DESCRIPTION:
!     This routine maps indices from a real array to a real array.  Any
!     entry in array 1 that has no match in array 2 is assigned a map
!     index of 0.
!
! !INTERFACE:
      subroutine IndexMapR2R ( RIndx1, RIndx2, iMap, rstat )
      use m_AdvError, only : Alloc, WPErr, Arr, Err => ErrStat
      use m_RadSort,  only : IndexSet, IndexSort
!
! !INPUT PARAMETERS: 
      implicit   NONE
      real,              intent (in)    ::
     .   RIndx1 (:),   ! Indices in array1
     .   RIndx2 (:)    ! ... and in array2
!
! !OUTPUT PARAMETERS: 
      integer,           intent (out)   ::
     .   iMap   (:),   ! Map index from obs to oms
     .   rstat         ! Returned status from the allocation statement.
!
! !REVISION HISTORY:
!     21Oct2002 - C. Redder - Original code
!
!-----------------------------------------------------------------------

      integer :: N1, i1, N2, i2
      integer, allocatable, dimension (:) ::
     .   Indx1, Indx2, Indx, iBeg, Len
      real    :: RMin, RMax
      integer, parameter ::
     .   iMap_def =  0,
     .   IMin     = -Huge (1),
     .   IMax     =  Huge (1)
      character (len=*), parameter :: MyName =  MyModule
     .                                      // '::IndexMapR2R'

!     Default status
!     --------------
      rstat = 0

!     Nothing to do if the number of observations or meta-data is zero
!     ----------------------------------------------------------------
      N1 = size ( RIndx1 )
      N2 = size ( RIndx2 )
      if ( N1 .le. 0 ) return     
      if ( N2 .le. 0 ) then
         iMap ( : N1 ) = iMap_def
         return
      end if

!     Allocate scratch space
!     ----------------------
      allocate ( Indx1 ( N1 ), Indx2 ( N2 ), stat = rstat )
     .           
      if ( rstat .ne. 0 ) then
         call WPErr ( MyName,
     .                Alloc ( rstat, Arr ( 'Indx1', N1 )
     .                            // Arr ( 'Indx2', N2 )))
         return
      end if

!     Convert xm to integer and initialize sorting indices
!     ----------------------------------------------------
      RMin = real ( IMin )
      RMax = real ( IMax )
      do i1 = 1, N1
         Indx1 ( i1 ) = nint ( max ( RMin, min ( RMax, RIndx1 ( i1 ))))
      end do
      do i2 = 1, N2
         Indx2 ( i2 ) = nint ( max ( RMin, min ( RMax, RIndx2 ( i2 ))))
      end do

!     Sort iarr2
!     ----------
      call IndexMapI2I ( Indx1, Indx2, iMap, rstat )
      if ( rstat .ne. 0 ) then
         deallocate ( Indx1, Indx2 )
         call WPErr ( MyName, Err ( 'IndexMapI2I' ))
         return
      end if

!     Clean up
!     --------
      deallocate ( Indx1, Indx2 )

      return
      end subroutine IndexMapR2R

!....................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: IndexMapI2I - Map indices from an integer array to integer array
!
! !DESCRIPTION:
!     This routine maps indices from a integer array to a integer array.  
!     Any entry in array 1 that has no match in array 2 is assigned a map
!     index of 0.
!
! !INTERFACE:
      subroutine IndexMapI2I ( Indx1, Indx2, iMap, rstat )
      use m_AdvError, only : Alloc, WPErr, Arr, Err => ErrStat
      use m_Range,    only : InitRange, GetIRange
      use m_RadSort,  only : IndexSet, IndexSort, VerifySort
!
! !INPUT PARAMETERS: 
      implicit   NONE
      integer,           intent (in)    ::
     .   Indx1 (:),    ! Indices in array1
     .   Indx2 (:)     ! ... and in array2
!
! !OUTPUT PARAMETERS: 
      integer,           intent (out)   ::
     .   iMap   (:),   ! Map index from obs to oms
     .   rstat         ! Returned status from the allocation statement.
!
! !REVISION HISTORY:
!     24Oct2002 - C. Redder - Original code
!     02Jan2002 - C. Redder - Added test to determine sort is needed
!     13Jan2003 - C. Redder - Added call to InitRange
!
!-----------------------------------------------------------------------

      integer :: N1, i1, N2, i2
      logical :: sorted
      integer, allocatable, dimension (:) :: Indx, iBeg, Len
      integer, parameter :: iMap_def = 0
      character (len=*), parameter :: MyName =  MyModule
     .                                      // '::IndexMapI2I'

!     Default status
!     --------------
      rstat = 0

!     Nothing to do if the number of observations or meta-data is zero
!     ----------------------------------------------------------------
      N1 = size ( Indx1 )
      N2 = size ( Indx2 )
      if ( N1 .le. 0 ) return     
      if ( N2 .le. 0 ) then
         iMap ( : N1 ) = iMap_def
         return
      end if

!     Allocate scratch space
!     ----------------------
      allocate ( iBeg ( N1 ), Len ( N1 ), Indx ( N2 ), stat = rstat )
      if ( rstat .ne. 0 ) then
         call WPErr ( MyName,
     .                Alloc ( rstat, Arr ( 'iBeg,Len', N1 )
     .                            // Arr ( 'Indx',     N2 )))
         return
      end if

!     Initialize sorting indices
!     --------------------------
      call IndexSet ( Indx )

!     Sort iarr2 (if necessary)
!     -------------------------
      sorted = VerifySort ( Indx2 ) .eq. 0
      if ( sorted ) then
         call IndexSort ( Indx, Indx2, rstat )
         if ( rstat .ne. 0 ) then
            deallocate ( iBeg, Len, Indx )
            call WPErr ( MyName, Err ( 'IndexSort' ))
            return
         end if
      end if

!     Match observation and meta-data via xm
!     --------------------------------------
      call InitRange ( Indx,               iBeg, Len )
      call GetIRange ( Indx, Indx2, Indx1, iBeg, Len )
      do i1 = 1, N1
         if ( Len ( i1 ) .eq. 1 ) then         ! Make sure each matchup is
            iMap  ( i1 ) = Indx ( iBeg ( i1 )) !   unique
         else                                  ! Otherwise array index error
            iMap  ( i1 ) = iMap_def            !   or matchup to duplicate
         end if                                !   reports could occur
      end do

!     Clean up
!     --------
      deallocate ( iBeg, Len, Indx )

      return
      end subroutine IndexMapI2I
!....................................................................
      end module m_RadList
!====================================================================

