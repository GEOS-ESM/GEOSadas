subroutine getcases(numcases,mype)
! This routine gets the names and number of available
! forecast pairs

  use type_kinds, only: fp_kind
  use variables, only: ak5,bk5,maxcases,nsig,dimbig,hybrid,&
      sigi,sigl,filename,na,nb
  implicit none
 
  integer,intent(in) :: mype
  integer,intent(out) :: numcases

  integer,dimension(4):: idateg
  integer nmin24(dimbig),nmin48(dimbig),idate5(5)
  integer nmina,nminb
  integer i24,ierror,j48,ncount,ncases,loop
  integer nming,ncase,inges,i,j,k
  real*4 hourg4
  real*4,dimension(nsig):: sigl4
  real*4,dimension(nsig+1):: ak5r4,bk5r4,sigi4
  real(fp_kind) hourg,ps0

  rewind 10
  ncases=0
  do loop=1,dimbig
    read(10,'(a100)',err=20,end=20)filename(loop)
    ncases=ncases+1
  end do
20  continue
  close(10)

  nmin24=-1
  nmin48=-1
  inges=50
  do loop=1,ncases
    open(inges,file=filename(loop),form='unformatted',action='read')
    rewind inges
     
    if (.not.hybrid) then
      read(inges,err=25,end=25)
      read(inges,err=25,end=25)hourg4,idateg,sigi4,sigl4
        sigi=sigi4
        sigl=sigl4
    else
      read(inges,err=25,end=25)
      read(inges,err=25,end=25)hourg4,idateg,ak5r4,bk5r4
      do k = 1,nsig+1
        ak5(k)=ak5r4(nsig+2-k)/1000.
        bk5(k)=bk5r4(nsig+2-k)
      end do
!     if(mype==0 .and. loop==1)then
!       do k = 1,nsig+1
!         print*,'k= ',k,ak5(k),bk5(k)
!       enddo
!     endif
    end if

    close(inges)
    hourg = hourg4
    idate5(1)=idateg(4)
    idate5(2)=idateg(2)
    idate5(3)=idateg(3)
    idate5(4)=idateg(1)
    idate5(5)=0
    call w3fs21(idate5,nming)
    nming=nming+60*hourg
    if(nint(hourg).eq.24) nmin24(loop)=nming
    if(nint(hourg).eq.48) nmin48(loop)=nming
25 continue
  enddo

  ncase=0
  ncount=0
  do loop=1,ncases
    i24=-1
    nmina=-1
    nminb=-1
    if(nmin24(loop).gt.0) then
      ncount=ncount+1
      if(ncount.eq.1)then
        nmina=nmin24(loop)
        i24=loop
        j48=-1
        do j=1,ncases
          if(nmin48(j).eq.nmin24(loop)) then
            nminb=nmin48(j)
            ncase=ncase+1
            na(ncase)=i24
            nb(ncase)=j
! write(6,*) 'nmin,na,nb=',ncase,nmin24(loop),na(ncase),nb(ncase)
          end if
        end do
        ncount=0
      end if  ! endif ncount
    end if    ! endif nmin24(loop)
  enddo       ! end loop to ncases

  if(mype==0)write(6,*)' number of cases available = ',ncase
  if(ncase.eq.0) then
    write(6,*)' no cases to process'
    call mpi_finalize(ierror)
    stop
  end if

  numcases=min(ncase,maxcases)
  if(mype==0)write(6,*)' number of cases to process for generating background stats = ',numcases

! before calculation of vertical smoother indices, need to load proxy sigma
! values for hybrid mode
  if(hybrid) then
    ps0=1./101.324
    do k=1,nsig
      sigl(k)=0.5*((ak5(k+1)+ak5(k))*ps0+bk5(k+1)+bk5(k))
      sigi(k)=ak5(k)*ps0+bk5(k)
    end do
    sigi(nsig+1)=ak5(nsig+1)*ps0+bk5(nsig+1)
    if(mype==0)then
      do k = 1,nsig
        print*,'k= ',k,sigl(k)
      enddo
    endif
  end if

  return
  end subroutine getcases


