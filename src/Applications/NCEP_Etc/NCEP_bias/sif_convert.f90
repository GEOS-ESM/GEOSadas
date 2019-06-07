program sif_convert
!
! sif_convert - convert a satinfo file in the old format into the new format
!               with satellite/instrument/sensor indicators (SIS).
!

  use m_sischnTable,only: sischnTable
  use m_sischnTable,only: sischnTable_build
  use m_sischnTable,only: sischnTable_verify
  use m_sischnTable,only: sischnTable_clean
  use m_stdio,only: stdin,stdout
  implicit none

  character(len=*),parameter:: myname="sif_convert"

  type siftable
    integer :: ich,jch,nusat
    character(len=20) :: satsensor
    integer :: iuse
    real,dimension(5) :: rest
  end type siftable

  integer,parameter:: mrec=2000
  type(siftable):: recs(mrec)
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
  use m_die   ,only: perr,die
  implicit none
  integer,intent(in):: ui
  integer,intent(out):: n
  type(siftable),dimension(:),intent(out):: r

  character(len=*),parameter:: myname_=myname//"rdtable"
  integer :: ich,jch,nusat,iuse
  character(len=20) :: satsensor
  real,dimension(5) :: rest
  integer :: m
  character(len=99) :: line

  m=size(r)
  n=0
  do
     read(*,'(a)',end=200) line
     if(line(1:1)=="!") cycle
     if(index(line,"sat chan iuse   err   pol   ermax  var_b var_pg")/=0) cycle
     read(line,*,err=300) nusat,jch,iuse,rest
     call sisind_get(nusat,satsensor)
     n=n+1;ich=n

     if(n>m) call die(myname_,'do not have enough space, size(r) =',m)
     r(n)=siftable(ich,jch,nusat,satsensor,iuse,rest)
  enddo
100 format(3I5,10f12.6)
200 continue
  return
300 continue
  call perr(myname_,'error on in rdtable(), at record #',n)
  call perr(myname_,'buffer = "'//trim(line)//'"')
  call die(myname_)
end subroutine rdtable

subroutine wttable(uo,n,r,order)
  use m_sischnTable,only: sischnTable
  use m_sischnTable,only: sischnTable_locate
  implicit none
  integer,intent(in):: uo
  integer,intent(in):: n	! size of r
  type(siftable),dimension(:),intent(in):: r
  type(sischnTable),optional,intent(in):: order

  character(len=*),parameter:: myname_=myname//"wttable"
  integer:: i,j,l

  do i=1,n
    l=i; if(present(order)) l=sischnTable_locate(order,rank=i)
    j=max(18,len_trim(r(l)%satsensor))
    write(uo,110) r(l)%satsensor(1:j),r(l)%jch,r(l)%iuse,r(l)%rest(1),0.0,r(l)%rest(3:5)
  enddo
110 format(1x,a18,2i5,f8.3,f8.3,f8.3,f8.3,f8.3)
end subroutine wttable

end program sif_convert
