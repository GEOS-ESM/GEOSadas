program sbc_reseq
!
! sbc_reseq()	: re-sequence sbc (bias) data entries
!
  implicit none
  integer :: ich,ichan,mch
  character(len=20) :: satsensor
  real,dimension(5) :: predr
  character(len=30) :: satsenchn	! for filenames

  mch=0
  do
     read(*,*,end=200) satsenchn

     open(10,file=satsenchn,status='old')
     read(10,'(I5,1x,A20,1x,I5,10f12.6)') ich,satsensor,ichan,predr
     close(10)

     mch=mch+1
     write(*,'(I5,1x,A20,1x,I5,10f12.6)') mch,satsensor,ichan,predr
  end do
200 continue
end program sbc_reseq
