program sbc_split
!
! sbc_split - split a SBC (bias) data file in the old format into new
! format with satellite/instrument/sensor indicators (SIS), and one
! (SIS,channel) per file.
!

  use m_sisind,only : sisind_get
  implicit none
  integer :: ich,ichan,nusat
  character(len=20) :: satsensor
  real,dimension(5) :: predr
  character(len=30) :: satsenchn	! for filenames

  do
     read(*,'(3I5,10f12.6)',end=200) ich,nusat,ichan,predr

     call sisind_get(nusat,satsensor)

     write(satsenchn,'(2a,i4.4)') trim(satsensor),'.',ichan

     open(10,file=satsenchn,status='new')
     write(10,'(I5,1x,a20,1x,i5,10f12.6)') ich,satsensor,ichan,predr
     close(10)
  end do
200 continue

end program sbc_split
