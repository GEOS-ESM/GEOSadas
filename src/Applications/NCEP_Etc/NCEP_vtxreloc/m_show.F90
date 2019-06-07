module m_show
implicit none
private
public :: show,showstm,showlev,showwav
contains

subroutine show(hdata,name)
  implicit none
  real,dimension(:,:,:),intent(in) :: hdata
  character(len=*),optional,intent(in) :: name

  integer :: im,jm,ms
  integer :: k

  im=size(hdata,1)
  jm=size(hdata,2)
  ms=size(hdata,3)

  if(present(name)) then
    print'(1x,3(a,i5))','show("'//trim(name)//'"), ',im,' x',jm,' x',ms
  else
    print'(1x,3(a,i5))','show(""), ',im,' x',jm,' x',ms
  endif
  
  do k=1,ms
    print'(i5,2x,3e20.12)',k,   sum(hdata(:,:,k)), &
    			     minval(hdata(:,:,k)), &
    			     maxval(hdata(:,:,k))
  end do
end subroutine show

subroutine showwav(hdata,name)
  implicit none
  real(4),dimension(:,:),intent(in) :: hdata
  character(len=*),optional,intent(in) :: name

  integer :: im,ms
  integer :: k

  im=size(hdata,1)
  ms=size(hdata,2)

  if(present(name)) then
    print'(1x,3(a,i5))','show("'//trim(name)//'"), ',im,' x',ms
  else
    print'(1x,3(a,i5))','show(""), ',im,' x',ms
  endif
  
  do k=1,ms
    print'(i5,2x,3e20.12)',k,   sum(hdata(:,k)), &
    			     minval(hdata(:,k)), &
    			     maxval(hdata(:,k))
  end do
end subroutine showwav

subroutine showlev(hdata,name)
  implicit none
  real,dimension(:,:),intent(in) :: hdata
  character(len=*),optional,intent(in) :: name
  integer :: im,jm
  im=size(hdata,1)
  jm=size(hdata,2)
  if(present(name)) then
    print'(1x,2(a,i5))','show("'//trim(name)//'"), ',im,' x',jm
  else
    print'(1x,2(a,i5))','show(""), ',im,' x',jm
  endif

  print'(2x,a3,2x,3e20.12)','---', sum(hdata(:,:)), &
      minval(hdata(:,:)),maxval(hdata(:,:))
end subroutine showlev

subroutine showstm(hdata,name)
  implicit none
  real,dimension(:,:,:,:),intent(in) :: hdata
  character(len=*),optional,intent(in) :: name

  integer :: k
  do k=1,size(hdata,4)
    if(present(name)) then
      print'(1x,a,i5,a,i5)','showstm("'//trim(name)//'"), istm=', &
      	k,' /',size(hdata,4)
    else
      print'(1x,a,i5)','showstm(""), nstm=',size(hdata,4)
    endif
    call show(hdata(:,:,:,k),name=name)
  end do
end subroutine showstm
end module m_show
