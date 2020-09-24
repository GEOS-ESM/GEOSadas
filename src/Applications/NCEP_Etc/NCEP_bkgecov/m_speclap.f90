subroutine m_speclap(grid2,itype)

  use type_kinds, only: fp_kind
  use variables,  only: nlat,nlon,lgaus
  use specgrid,   only: enn1,nc,ncd2,factsml,factvml
  use specgrid,   only: init_spec_grid,destroy_spec_grid
  use specgrid,   only: init_spec_vars,destroy_spec_vars
  use specgrid,   only: sptez_s,load_grid,fill_ns
  use specgrid,   only: jcap,imax,jmax

  implicit none

  integer,intent(in) :: itype
  real(fp_kind),dimension(nlat,nlon),intent(inout) :: grid2

! local variables 
  integer :: i2,i2m1,i 
  real(fp_kind),allocatable,dimension(:):: wrkspec
  real(fp_kind),allocatable,dimension(:,:):: grid

  allocate ( wrkspec(nc) )
  allocate ( grid(imax, jmax) )

  call init_spec_vars(jcap)
  call init_spec_grid(nlat,nlon,lgaus)
  call load_grid(grid2,nlat,nlon,grid)
  call sptez_s(wrkspec,grid,-1)
  if(itype==1)then
    do i=1,ncd2
      i2=2*i; i2m1=i2-1
      wrkspec(i2)=factvml(i2)*wrkspec(i2)*(-enn1(i))
      wrkspec(i2m1)=factvml(i2m1)*wrkspec(i2m1)*(-enn1(i))
    end do
  else if(itype==2)then
    do i=1,ncd2
      i2=2*i; i2m1=i2-1
      wrkspec(i2)=factsml(i2)*wrkspec(i2)*(-enn1(i))
      wrkspec(i2m1)=factsml(i2m1)*wrkspec(i2m1)*(-enn1(i))
    end do
  endif
  call sptez_s(wrkspec,grid,1)
  call fill_ns(grid,nlat,nlon,grid2)

  call destroy_spec_grid
  call destroy_spec_vars
  deallocate ( wrkspec )
  deallocate ( grid    )

  return

end subroutine m_speclap

  
