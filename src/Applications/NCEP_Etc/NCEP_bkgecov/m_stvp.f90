subroutine m_stvp(grdsf,grdvp,vor,div)

  use type_kinds, only: fp_kind,double
  use variables,  only: nlat,nlon,nsig
  use variables,  only: lgaus,nGlat,nGlon
  use specgrid,   only: jcap,enn1,nc,ncd2,factvml
  use specgrid,   only: sptez_s,load_grid,fill_ns
  use specgrid,   only: init_spec_grid,destroy_spec_grid
  use specgrid,   only: init_spec_vars,destroy_spec_vars
  use specgrid,   only: imax,jmax

  implicit none

  real(fp_kind),dimension(nGlat,nGlon,nsig),intent(in) :: vor
  real(fp_kind),dimension(nGlat,nGlon,nsig),intent(in) :: div
  real(fp_kind),dimension(nlat,nlon,nsig),intent(out) :: grdsf
  real(fp_kind),dimension(nlat,nlon,nsig),intent(out) :: grdvp

! local variables 
  integer :: k, i2,i2m1,i,i1
  real(fp_kind),allocatable,dimension(:,:) :: grid
  real(fp_kind),allocatable,dimension(:,:) :: grid2
  real(fp_kind),allocatable,dimension(:,:) :: wrkspec1,wrkspec2

  call init_spec_vars(jcap)
  allocate ( wrkspec1(nc, nsig) ) 
  allocate ( wrkspec2(nc, nsig) ) 

!  call init_spec_grid(nGlat,nGlon,.true.)
  call init_spec_grid(nGlat,nGlon,lgaus)
  allocate ( grid (imax, jmax) )
  allocate ( grid2(nGlat, nGlon) )

  do k= 1,nsig
    grid2(:,:)=vor(:,:,k)  
    call load_grid(grid2,nGlat,nGlon,grid)
    call sptez_s(wrkspec1(1,k),grid,-1)
    grid2(:,:)=div(:,:,k)  
    call load_grid(grid2,nGlat,nGlon,grid)
    call sptez_s(wrkspec2(1,k),grid,-1)
  end do
  deallocate ( grid2, grid)
    
  do k= 1, nsig 
    wrkspec1(1,k)=0.
    wrkspec1(2,k)=0.
    wrkspec2(1,k)=0.
    wrkspec2(2,k)=0.
    do i=2,ncd2
      i2=2*i; i2m1=i2-1
      wrkspec1(i2,k)=factvml(i2)*wrkspec1(i2,k)/(-enn1(i))
      wrkspec1(i2m1,k)=factvml(i2m1)*wrkspec1(i2m1,k)/(-enn1(i))
      wrkspec2(i2,k)=factvml(i2)*wrkspec2(i2,k)/(-enn1(i))
      wrkspec2(i2m1,k)=factvml(i2m1)*wrkspec2(i2m1,k)/(-enn1(i))
    end do
  end do

  call destroy_spec_grid

  call init_spec_grid(nlat,nlon,lgaus)
  allocate ( grid (imax, jmax) )
  allocate ( grid2(nlat,nlon) )

  do k =1, nsig
    call sptez_s(wrkspec1(1,k),grid,1)
    call fill_ns(grid,nlat,nlon,grid2)
    grdsf(:,:,k) = grid2(:,:)  ! stream function
    call sptez_s(wrkspec2(1,k),grid,1)
    call fill_ns(grid,nlat,nlon,grid2)
    grdvp(:,:,k) = grid2(:,:)  ! velocity potential
  end do

  deallocate ( grid2 )
  deallocate ( grid )
  deallocate ( wrkspec1, wrkspec2 )

  call destroy_spec_grid
  call destroy_spec_vars

  return

end subroutine m_stvp

subroutine swapij_(aij,aji,nlat,nlon,nsig)
                                                                                
  use type_kinds, only : fp_kind, double
  implicit none
  integer,intent(in) :: nlat,nlon,nsig
  real(fp_kind), dimension(nlat,nlon,nsig),intent(in ) :: aij
  real(double),dimension(nlon,nlat,nsig),intent(out) :: aji
  integer :: i,j,k
                                                                                
  do k=1,nsig
    do i=1,nlon
      aji(i,1:nlat,k)=aij(1:nlat,i,k)
    end do
  end do
                                                                                
end subroutine swapij_

