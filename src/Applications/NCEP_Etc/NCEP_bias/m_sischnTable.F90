module m_sischnTable
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_sischnTable
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-04-22
!
! abstract: - searchable table of satellite/instrument/sensor(SIS) + channel(CHN)
!
! program history log:
!   2015-06-10  j guo   - Fixed a locate_() bug found by Meta Sienkiewicz.
!                         This bug may return a wrong location but "near"
!                         where it should be, for a value not present in
!                         the table.
!   2010-04-22  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:

  implicit none
  private	! except

  public :: sischnTable			! class type, a searchable table of (sis,chn).
  public :: sischnTable_build		! construct a sischnTable.
  public :: sischnTable_verify		! check the internal of a sischnTable construction.
  public :: sischnTable_locate		! search for a sischnTable entry, by a rank or by a key
  public :: sischnTable_clean		! destruct a sischnTable

! Usecase 1: Test self-consistency of a sischnTable
!
!	use m_sischnTable, only: sischnTable
!	use m_sischnTable, only: sischnTable_build
!	use m_sischnTable, only: sischnTable_locate
!	use m_sischnTable, only: sischnTable_clean
!	use m_die,only: die,perr
!	implicit none
!
!  [.. upon given a list of paired keys:
!	character(len=*),intent(in):: sis(:)
!	integer         ,intent(in):: chn(:)
!  ..]
!	type(sischnTable):: tabl
!	integer:: i,l
!
!	call sischnTable_build(tabl,sis,chn)	! construct a table
!	do i=1,size(sis)			! go through all entries in their original order.
!	  l=sischnTable_locate(tabl,sis(i),chn(i))	! locate the storage location, not their rank.
!	  if(l==i) cycle
!
!	  call perr("test",'bad key at i =',i)
!	  call perr("test",'           l =',l)
!	  call perr("test",'         sis =',sis(i))
!	  call perr("test",'         chn =',chn(i))
!	  call  die("test")
!	enddo
!	call sischnTable_clean(tabl)

! Usecase 2: list entries according to their ranks
!
!	use m_sischnTable, only: sischnTable
!	use m_sischnTable, only: sischnTable_build
!	use m_sischnTable, only: sischnTable_locate
!	use m_sischnTable, only: sischnTable_clean
!	implicit none
!
!  [.. upon given a list of paired keys:
!	character(len=*),intent(in):: sis(:)
!	integer         ,intent(in):: chn(:)
!  ..]
!	type(sischnTable):: tabl
!	integer:: i,l
!
!	call sischnTable_build(tabl,sis,chn)	! construct a table
!	do i=1,size(sis)			! go through all entries in their original order.
!	  l=sischnTable_locate(tabl,i)		! locate the storage location, by its rank.
!	  print'(2i5,2x,a,2x,i5)',i,l,sis(l),chn(l)		! display this rank and this entry
!	enddo
!	call sischnTable_clean(tabl)

  type sischn
    private
    character(len=20)	:: sis	! sis indicator
    integer		:: chn	! channel number
  end type sischn

  type sischnTable
    private
    type(sischn),allocatable,dimension(:):: keys
    integer     ,allocatable,dimension(:):: indx
  end type sischnTable

  interface sischnTable_build    ; module procedure  build_; end interface
  interface sischnTable_verify   ; module procedure verify_; end interface
  interface sischnTable_locate   ; module procedure &
    lindex_, &
    locate_; end interface
  interface sischnTable_clean    ; module procedure  clean_; end interface

  character(len=*),parameter :: myname="m_sischnTable"
contains

subroutine build_(tabl,sis,chn)
  implicit none
  type(sischnTable),intent(out):: tabl
  character(len=*),dimension(:),intent(in):: sis
  integer         ,dimension(:),intent(in):: chn
  integer:: n
  n=size(sis)
  allocate(tabl%indx(n))
  allocate(tabl%keys(n))
  call set_(tabl%keys,sis,chn)
  call indexSort_(tabl%indx,tabl%keys)
end subroutine build_

subroutine verify_(tabl,stat,verbose)
	! verify a table
  use bias_mpeu, only: die,perr
  implicit none
  type(sischnTable),intent(in):: tabl
  integer,optional,intent(out):: stat
  logical,optional,intent(in ):: verbose
  logical:: verb_
  integer:: istat,i,m,n
  character(len=*),parameter :: myname_=myname//'.verify_'
  n=size(tabl%indx)

  verb_=.true.
  if(present(stat)) verb_=.false.
  if(present(verbose)) verb_=verbose

  if(present(stat)) stat=0
  if(n==0) return

!  do i=1,n
!    m=tabl%indx(i)
!    write(6,'(2i5,4x,a,i6)') i,m,tabl%keys(m)%sis,tabl%keys(m)%chn
!  enddo

  if(n==1) then
    m=tabl%indx(1)
    if(m==1) return
    if(verb_) then
      call perr(myname_,'not in order, at rank i =',1)
      call perr(myname_,'               %indx(i) =',m)
    endif
    if(.not.present(stat)) call die(myname_)
    stat=-1     ! mark a bad %indx value
    return
  endif

  istat=0       ! assume it is inorder
  m=tabl%indx(1)
  do i=2,size(tabl%indx)
    n=tabl%indx(i)
    if( tabl%keys(m)%sis> tabl%keys(n)%sis .or.  &
       (tabl%keys(m)%sis==tabl%keys(n)%sis .and. &
        tabl%keys(m)%chn>=tabl%keys(n)%chn) ) then
      istat=i-1
      exit
    endif
    m=n
  enddo

  if(istat/=0.and.verb_) then
    call perr(myname_,'not in order, at rank i =',istat)
    call perr(myname_,'             %indx(i  ) =',m)
    call perr(myname_,'             %indx(i+1) =',n)
    call perr(myname_,'              keys(i  ) ="'//tochar_(tabl%keys(m))//'"')
    call perr(myname_,'              keys(i+1) ="'//tochar_(tabl%keys(n))//'"')
    if(.not.present(stat)) call die(myname_)
    stat=istat
    return
  endif

end subroutine verify_

function lindex_(tabl,rank)
	! locate an entry by its rank.
  use bias_mpeu, only: die,perr
  implicit none
  type(sischnTable),intent(in):: tabl
  integer          ,intent(in):: rank
  integer:: lindex_
  character(len=*),parameter :: myname_=myname//'.lindex_'

  if(rank<=0.or.rank>size(tabl%indx)) then
    call perr(myname_,'bad ranking number, rank =',rank)
    call perr(myname_,'              table size =',size(tabl%indx))
    call die(myname_)
  endif

  lindex_=tabl%indx(rank)
end function lindex_

function locate_(tabl,sis,chn)
        ! binary search
  implicit none
  type(sischnTable),intent(in):: tabl
  character(len=*),intent(in):: sis
  integer         ,intent(in):: chn
  integer:: locate_

  integer:: lb,ub,i,m
  type(sischn):: akey
  lb=1
  ub=size(tabl%indx)
  locate_=-1
  do
    i=(lb+ub)/2
    locate_=tabl%indx(i)
    m=compare_(sischn(sis,chn),tabl%keys(locate_))
    if(m==0) return
    if(m< 0) ub=i-1
    if(m> 0) lb=i+1
    if(ub<lb) then
      locate_=-1
      return
    endif
  enddo
end function locate_

subroutine clean_(tabl)
  implicit none
  type(sischnTable),intent(inout):: tabl
  deallocate(tabl%indx)
  deallocate(tabl%keys)
end subroutine clean_

function compare_(akey,bkey) result(m)
  implicit none
  type(sischn),intent(in):: akey,bkey
  integer:: m
  m=-huge(m)
  if    (akey%sis<bkey%sis) then
    m=-1
  elseif(akey%sis>bkey%sis) then
    m=+1
  elseif(akey%chn<bkey%chn) then
    m=-1
  elseif(akey%chn>bkey%chn) then
    m=+1
  else
    m= 0
  endif
end function compare_

subroutine set_(keys,sis,chn)
  implicit none
  type(sischn),dimension(:),intent(out):: keys
  character(len=*),dimension(:),intent(in):: sis
  integer,dimension(:),intent(in):: chn
  keys(:)%sis=sis(:)
  keys(:)%chn=chn(:)
end subroutine set_

subroutine indexSort_(indx,keys)
  use bias_mpeu, only: indexSet
  use bias_mpeu, only: indexSort
  implicit none
  integer,dimension(:),intent(out):: indx
  type(sischn),dimension(:),intent(in) :: keys
  call indexSet (indx)
  call indexSort(indx,keys(:)%chn)
  call indexSort(indx,keys(:)%sis)
end subroutine indexSort_

function tochar_(sisc)
  implicit none
  type(sischn),intent(in):: sisc
  character(len=26):: tochar_
  write(tochar_,'(a20,i6.4)') sisc%sis,sisc%chn
end function tochar_
end module m_sischnTable
