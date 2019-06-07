integer function isrchfge(nx1,x,y)
  use type_kinds, only: fp_kind
  implicit none
  integer nx1,k
  real(fp_kind) y
  real(fp_kind),dimension(nx1):: x

  do k=1,nx1
    if(y<=x(k)) then
      isrchfge=k
      go to 100
    end if
  end do
  isrchfge=nx1+1
  if(nx1<=0) isrchfge=0
100 continue

  return
end
