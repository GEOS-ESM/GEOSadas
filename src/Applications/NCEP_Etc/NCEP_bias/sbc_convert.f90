program sbc_convert
!
! sbc_convert - convert a satbias file in the old format into the new format
!               with satellite/instrument/sensor indicators (SIS).
!

  use m_sischnTable,only: sischnTable
  use m_sischnTable,only: sischnTable_build
  use m_sischnTable,only: sischnTable_verify
  use m_sischnTable,only: sischnTable_clean
  use m_stdio,only: stdin,stdout
  implicit none

  character(len=*),parameter:: myname="sbc_convert"

  type sbctable
    integer :: ich,ichan,nusat
    character(len=20) :: satsensor
    real,dimension(5) :: predr
  end type sbctable

  integer,parameter:: mrec=2000
  type(sbctable):: recs(mrec)
  integer:: nrec

  type(sischnTable):: srch
!!logical,parameter:: sorted=.true.
!!logical,parameter:: sorted=.false.

  logical:: sorted
  namelist/setup/sorted
  read(stdin,setup)

  call rdtable(stdin,nrec,recs)		! read table from stdin
  if(sorted) then
    call sischnTable_build(srch,recs(1:nrec)%satsensor,recs(1:nrec)%ichan)
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
  type(sbctable),dimension(:),intent(out):: r

  character(len=*),parameter:: myname_=myname//"rdtable"
  integer :: ich,ichan,nusat
  character(len=20) :: satsensor
  real,dimension(5) :: predr
  integer :: m

  m=size(r)
  n=0
  do
     read(ui,100,end=200,err=300) ich,nusat,ichan,predr
     call sisind_get(nusat,satsensor)
     n=n+1
     if(n>m) call die(myname_,'do not have enough space, size(r) =',m)
     r(n)=sbctable(ich,ichan,nusat,satsensor,predr)
  enddo
100 format(3I5,10f12.6)
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
  type(sbctable),dimension(:),intent(in):: r
  type(sischnTable),optional,intent(in):: order

  character(len=*),parameter:: myname_=myname//"wttable"
  integer:: i,l

  do i=1,n
    l=i; if(present(order)) l=sischnTable_locate(order,rank=i)
    write(uo,110) i,r(l)%satsensor,r(l)%ichan,r(l)%predr
  enddo
110 format(I5,1x,a20,1x,i5,10f12.6)
end subroutine wttable

end program sbc_convert
