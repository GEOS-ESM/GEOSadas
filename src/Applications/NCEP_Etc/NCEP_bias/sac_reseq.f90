program sac_reseq
!
! sac_reseq()	: re-sequence sac data entries
!
  implicit none
  integer :: ich,jch,mch
  character(len=20) :: satsensor
  real :: tlap
  real,dimension(90) :: c_ang
  character(len=30) :: satsenchn	! for filenames

  mch=0
  do
     read(*,*,end=200) satsenchn
!     write(0,*) 'open "',trim(satsenchn),'" for read'

     open(10,file=satsenchn,status='old')
     read(10,110) ich,satsensor,jch,tlap,c_ang
     close(10)

     mch=mch+1
     write(*,110) mch,satsensor,jch,tlap,c_ang
  end do
200 continue
110 format(I5,1x,A20,1x,I5,e15.6/9(4x,10f7.3/))
end program sac_reseq
