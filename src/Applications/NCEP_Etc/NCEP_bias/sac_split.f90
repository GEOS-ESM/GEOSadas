program sac_split
!
! sac_split - split a sac data file in the old format into new format
! with satellite/instrument/sensor indicators (SIS), and one
! (SIS,channel) per file.
!

  use m_sisind,only : sisind_get
  implicit none
  integer :: ich,jch,nusat
  character(len=20) :: satsensor
  real :: tlap
  real,dimension(90) :: c_ang
  character(len=30) :: satsenchn	! for filenames

  do
     read(*,100,err=200,end=200) ich,nusat,jch,tlap,c_ang

     call sisind_get(nusat,satsensor)

     write(satsenchn,'(2a,i4.4)') trim(satsensor),'.',jch
!     write(0,*) 'open "',trim(satsenchn),'" for write'

     open(10,file=satsenchn,status='new')
     write(10,110) ich,satsensor,jch,tlap,c_ang
     close(10)
  end do
200 continue

100 format(10x,3I5,e15.6/9(4x,10f7.3/))
110 format(I5,1x,A20,1x,I5,e15.6/9(4x,10f7.3/))

end program sac_split
