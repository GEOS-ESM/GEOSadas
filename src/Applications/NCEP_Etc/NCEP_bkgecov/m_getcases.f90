subroutine m_getcases(numcases,mype)

  use type_kinds, only : fp_kind
  use variables,  only : ak5,bk5,maxcases,nsig,dimbig
  use variables,  only : hybrid,filename,na,nb
  use variables,  only : readperts
  use m_fvHeader, only : fvHeader_read

  implicit none
 
  integer,intent(in)  :: mype
  integer,intent(out) :: numcases

! local variables
                                                                                
  integer i24,ierror,j48,ncount,ncases,loop,nming,ncase,i,j,k
  integer nmin24(dimbig),nmin48(dimbig),idate5(5)
  integer nmina,nminb,nymd,nhms
  real(fp_kind) ps0

  rewind 10
  ncases=0
  do loop=1,dimbig
    read(10,'(a100)',err=20,end=20)filename(loop)
    ncases=ncases+1
  end do
20  continue
  close(10)

  if(readperts) then
    ncase=ncases
  else
    nmin24=-1
    nmin48=-1
   
    do loop=1,ncases
   
      call fvHeader_read(loop, nymd, nhms, mype)
   
      idate5(1)=    nymd/10000      ! year
      idate5(2)=mod(nymd/100, 100)  ! month
      idate5(3)=mod(nymd,     100)  ! day
      idate5(4)=    nhms/10000      ! hour
      idate5(5)=0
   
      call w3fs21(idate5,nming)
   
      i24=0
      i24=scan(trim(filename(loop)),'+')
      if ( i24 == 0 ) then
        write(6,*)'file name template not correct'
        call mpi_finalize(ierror)
        stop     
      end if
   
      if(filename(loop)(i24+1:i24+2).eq.'24') nmin24(loop)=nming
      if(filename(loop)(i24+1:i24+2).eq.'48') nmin48(loop)=nming
   
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
            end if
          end do
          ncount=0
        end if  ! endif ncount
      end if    ! endif nmin24(loop)
    enddo       ! end loop to ncases

  endif ! .not.readperts

  if(ncase.eq.0) then
    write(6,*)'m_getcases: no cases to process'
    call mpi_finalize(ierror)
    stop
  end if

  numcases=min(ncase,maxcases)

  if ( mype==0 ) then
    write(6,*)'number of cases available = ',ncase
    write(6,*)'number of cases used for generating bk stats: ',numcases
    if (readperts) then
      do loop = 1, ncase
        write(6,'(i5,1x,a)')loop,trim(filename(loop))
      end do
    else
      do loop = 1, ncase
        write(6,'(i5,2(1x,a))')loop,trim(filename(na(loop))),trim(filename(nb(loop)))
      end do
    endif
  end if

  return

  end subroutine m_getcases


