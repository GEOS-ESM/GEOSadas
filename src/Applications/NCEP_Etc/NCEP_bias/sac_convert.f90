program sac_convert
!
! sac_convert - convert a satbang file in the old format into the new format
!               with satellite/instrument/sensor indicators (SIS).
!

  use m_sischnTable,only: sischnTable
  use m_sischnTable,only: sischnTable_build
  use m_sischnTable,only: sischnTable_verify
  use m_sischnTable,only: sischnTable_clean
  use m_stdio,only: stdin,stdout
  implicit none

  character(len=*),parameter:: myname="sac_convert"

  type sactable
    integer :: ich,jch,nusat
    character(len=20) :: satsensor
    real :: tlap
    real,dimension(90) :: c_ang
  end type sactable

  integer,parameter:: mrec=2000
  type(sactable):: recs(mrec)
  integer:: nrec

  type(sischnTable):: srch
!!logical,parameter:: sorted=.true.
!!logical,parameter:: sorted=.false.

  logical:: sorted
  namelist/setup/sorted
  read(stdin,setup)

  call rdtable(stdin,nrec,recs)		! read table from stdin
  if(sorted) then
    call sischnTable_build(srch,recs(1:nrec)%satsensor,recs(1:nrec)%jch)
    call sischnTable_verify(srch)
    call wttable(stdout,nrec,recs,order=srch)	! write sorted table to stdout
    call sischnTable_clean(srch)
  else
    call wttable(stdout,nrec,recs)	! write table to stdout in the same order
  endif

contains
subroutine rdtable(ui,n,r)
  use m_sisind,only : sisind_get
  use m_die   ,only: die
  implicit none
  integer,intent(in):: ui
  integer,intent(out):: n
  type(sactable),dimension(:),intent(out):: r

  character(len=*),parameter:: myname_=myname//"rdtable"
  integer :: ich,jch,nusat
  character(len=20) :: satsensor
  real :: tlap
  real,dimension(90) :: c_ang
  integer :: m

  m=size(r)
  n=0
  do
     read(ui,100,end=200,err=300) ich,nusat,jch,tlap,c_ang
     call sisind_get(nusat,satsensor)
     n=n+1
     if(n>m) call die(myname_,'do not have enough space, size(r) =',m)
     r(n)=sactable(ich,jch,nusat,satsensor,tlap,c_ang)
  enddo
100 format(10x,3I5,e15.6/9(4x,10f7.3/))
200 continue
  return
300 continue
  call die(myname_,'error on in rdtable(), at record #',n)
end subroutine rdtable

subroutine wttable(uo,n,r,order)
  use m_sischnTable,only: sischnTable
  use m_sischnTable,only: sischnTable_locate
  implicit none
  integer,intent(in):: uo
  integer,intent(in):: n	! size of r
  type(sactable),dimension(:),intent(in):: r
  type(sischnTable),optional,intent(in):: order

  character(len=*),parameter:: myname_=myname//"wttable"
  integer:: i,l

  do i=1,n
    l=i; if(present(order)) l=sischnTable_locate(order,rank=i)
    write(uo,110) i,r(l)%satsensor,r(l)%jch,r(l)%tlap,r(l)%c_ang
  enddo
110 format(I5,1x,A20,1x,I5,e15.6/9(4x,10f7.3/))
end subroutine wttable

end program sac_convert
