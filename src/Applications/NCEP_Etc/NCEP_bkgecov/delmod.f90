module delmod 
contains
subroutine delvars(sf,vp,t,ps,q,oz,cw,mype)

  use type_kinds, only: fp_kind,single
  use variables,  only: nlat,nlon,nsig,lat1,lon1,zero,biasrm
  use variables,  only: tcon,vpcon,half,pscon,istart
  use variables,  only: bbt,bbs,bbv,bbp
  use variables,  only: bboz,bbq
 
  implicit none

!  19Apr2020 Todling - remove hydromets from interface (not used here)

  real(fp_kind),dimension(lat1,lon1,nsig),intent(inout):: sf,vp,t,q,oz,cw
  real(fp_kind),dimension(lat1,lon1     ),intent(inout):: ps
  integer,intent(in):: mype

  real(fp_kind),dimension(lat1,lon1):: bal
  integer       :: i,j,k,m,ix,mm1

  mm1=mype+1

  if (.not. biasrm) then ! remove the mean only
    ps = ps - bbp
    t  =  t - bbt 
    vp = vp - bbv
    sf = sf - bbs
    q  = q -  bbq
    oz = oz - bboz
  endif

! Subtract off SF Component of Temperature difference
  do m=1,nsig
    bal(:,:)=zero
    do k=1,nsig
      do i=1,lat1
        ix=istart(mm1)+i-1
        do j=1,lon1
          bal(i,j)=bal(i,j)+tcon(ix,m,k)*sf(i,j,k)
        end do
      end do
    end do
    do j=1,lon1
      do i=1,lat1
        t(i,j,m) = t(i,j,m) - bal(i,j)
      end do
    end do 
  end do !end do m

! Balanced part of VP
  do k=1,nsig
    bal(:,:)=zero
    do i=1,lat1
      ix=istart(mm1)+i-1
      do j=1,lon1
        bal(i,j)=vpcon(ix,k)*sf(i,j,k)
        vp(i,j,k) = vp(i,j,k) - bal(i,j)
      end do
    end do
  end do

! 'Balanced' part of SF
  bal(:,:)=zero
  do k=1,nsig 
    do i=1,lat1
      ix=istart(mm1)+i-1
      do j=1,lon1
        bal(i,j) = bal(i,j) + pscon(ix,k)*sf(i,j,k)
      end do
    end do
  end do
  do j=1,lon1
    do i=1,lat1
      ps(i,j) = ps(i,j) - bal(i,j)
    end do
  end do

 return
end subroutine delvars

subroutine hydrobias(qi,ql,qr,qs)

  use type_kinds, only: fp_kind,single
  use variables,  only: biasrm,hydromet
  use variables,  only: nsig,lat1,lon1
  use variables,  only: bbqi,bbql,bbqr,bbqs
 
  implicit none

!  19Apr2020 Todling - moved hydro out of delvars (they were not being used
!                      anyway), this helps control use of hydromets - unclear
!                      why bias not removed from these!

  real(fp_kind),dimension(lat1,lon1,nsig),intent(inout):: qi,ql,qr,qs

  if (.not. biasrm) then ! remove the mean only
    if(hydromet) then
!      qi = qi - bbqi
!      ql = ql - bbql
!      qr = qr - bbqr
!      qs = qs - bbqs
    endif
  endif

 return
end subroutine hydrobias

subroutine delbal(sf,vpb,tb,pb,mype)

  use type_kinds, only: fp_kind,single
  use variables,  only: nlat,nlon,nsig,lat1,lon1,zero,biasrm
  use variables,  only: tcon,vpcon,half,pscon,istart
  use variables,  only: bbt,bbs,bbv,bbp
 
  implicit none

  real(fp_kind),dimension(lat1,lon1,nsig),intent(in):: sf
  real(fp_kind),dimension(lat1,lon1,nsig),intent(inout):: vpb,tb
  real(fp_kind),dimension(lat1,lon1     ),intent(inout):: pb
  integer,intent(in):: mype

  real(fp_kind),dimension(lat1,lon1):: bal
  integer       :: i,j,k,m,ix,mm1

  mm1=mype+1

! Subtract off SF Component of Temperature difference
  do m=1,nsig
    bal(:,:)=zero
    do k=1,nsig
      do i=1,lat1
        ix=istart(mm1)+i-1
        do j=1,lon1
          bal(i,j)=bal(i,j)+tcon(ix,m,k)*sf(i,j,k)
        end do
      end do
    end do
    do j=1,lon1
      do i=1,lat1
        tb(i,j,m) = bal(i,j)
      end do
    end do 
  end do !end do m

! Balanced part of VP
  do k=1,nsig
    bal(:,:)=zero
    do i=1,lat1
      ix=istart(mm1)+i-1
      do j=1,lon1
        bal(i,j)=vpcon(ix,k)*sf(i,j,k)
        vpb(i,j,k) =  bal(i,j)
      end do
    end do
  end do

! 'Balanced' part of SF
  bal(:,:)=zero
  do k=1,nsig 
    do i=1,lat1
      ix=istart(mm1)+i-1
      do j=1,lon1
        bal(i,j) = bal(i,j) + pscon(ix,k)*sf(i,j,k)
      end do
    end do
  end do
  do j=1,lon1
    do i=1,lat1
      pb(i,j) = bal(i,j)
    end do
  end do

 return
end subroutine delbal
end module delmod


