program sischnTable_utest
  use m_sischnTable, only: sischnTable
  use m_sischnTable, only: sischnTable_build
  use m_sischnTable, only: sischnTable_verify
  use m_sischnTable, only: sischnTable_locate
  use m_sischnTable, only: sischnTable_clean

  use bias_mpeu, only: stderr,stdout,stdin
  use bias_mpeu, only: die

  implicit none
  character(len=*),parameter :: myname='sischnTable_utest'
  integer:: i,k,km,jpch,ios
  type(sischnTable):: sac_table
  integer,parameter:: MSIS=10000

  character(len=20) :: satsen(MSIS)
  integer :: jchnum(MSIS)
  integer:: istat

  istat=0

  call acfile_get_(stdin,satsen,jchnum,nrec=jpch)
	if(jpch>MSIS) then
	  istat=ior(istat,1)
	  write(stderr,'(a   )') '>> ERROR << insurficient storage (satsen,jchnum)'
	  write(stderr,'(a,i6)') '>> ERROR << jpch(satsen) =',jpch
	  write(stderr,'(a,i6)') '>> ERROR << size(satsen) =',MSIS
	  write(stderr,'(a   )') '>> ERROR << test continue by setting jpch to size(satsen)'
	  write(stdout,'(a   )') 'test continue by setting jpch to size(satsen)'
	  jpch=MSIS
	endif

  call sischnTable_build(sac_table,satsen(:jpch),jchnum(:jpch))
  call sischnTable_verify(sac_table)
  
  write(stdout,'(a,i0,a)') '>> list/test ',jpch,' records in their original order <<'
  do i=1,jpch
    k=sischnTable_locate(sac_table,satsen(i),jchnum(i))
    if(k/=i) then
      istat=ior(istat,2)
      write(stderr,'(a,2i5,4x,a,i6)') '>> ERROR << ',i,k,satsen(k),jchnum(k)
    else
      write(stdout,'(2i5,4x,a,i6)') i,k,satsen(i),jchnum(i)
    endif
  enddo

  write(stdout,'(a,i0,a)') '>> list/test ',jpch,' records in their ranked order <<'
  km=0
  do i=1,jpch
    k=sischnTable_locate(sac_table,rank=i)
    if(km>0) then
      if( satsen(km)> satsen(k) .or. &
         (satsen(km)==satsen(k) .and. jchnum(km)>=jchnum(k)) ) then
        istat=ior(istat,4)
        write(stderr,'(a,2i5,4x,a,i6)') '>> ERROR << ',i,k,satsen(k),jchnum(k)
      endif
    endif
    write(stdout,'(2i5,4x,a,i6)') i,k,satsen(k),jchnum(k)
    km=k
  enddo

  write(stdout,'(a,i0,a)') '>> list/test ',jpch,' records with unknown key values <<'
  do i=1,jpch
    k=sischnTable_locate(sac_table,satsen(i),-jchnum(i))
    if(k<1.or.k>jpch) then
      write(stdout,'(a,2i5,4x,a,i6)') '>> EXPECTED << ',i,k,satsen(i),-jchnum(i)
    else
      istat=ior(istat,8)
      write(stderr,'(a,2i5,4x,a,i6)') '>> ERROR << ',i,k,satsen(k),-jchnum(k)
    endif
  enddo

  call sischnTable_clean(sac_table)
  if(istat/=0) call die(myname,'test failed with stat =',istat)

contains
subroutine acfile_get_(lunit,sis,chn,nrec)
  implicit none
  integer,intent(in):: lunit
  character(len=*),dimension(:),intent(out):: sis
  integer         ,dimension(:),intent(out):: chn
  integer         ,             intent(out):: nrec

  integer:: msize
  integer:: k,ios

  integer :: ich
  character(len=len(sis)) :: satsen
  integer :: jchnum
  real :: tlap
  real,dimension(90) :: c_ang

  msize=size(sis)

!!open(lunit,file=ac_file,status='old')
  k=0
  do
    read(lunit,'(I5,1x,A20,1x,I5,e15.6/9(4x,10f7.3/))',iostat=ios) &
      ich,satsen,jchnum,tlap,c_ang
    if(ios/=0) exit
    k=k+1
    if(k>msize) cycle
    sis(k)=satsen
    chn(k)=jchnum
  enddo
!!close(lunit)
  nrec=k
end subroutine acfile_get_
end program sischnTable_utest
