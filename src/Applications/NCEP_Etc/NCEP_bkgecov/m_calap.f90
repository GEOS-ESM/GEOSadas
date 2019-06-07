subroutine m_calap(grd,itype)

  use type_kinds, only: fp_kind,double
  use variables,  only: nlat,nlon,nsig
  use variables,  only: nGlat,nGlon,glats,lgaus,pi
  use specgrid,   only: jcap,enn1,nc,ncd2,factvml,factsml
  use specgrid,   only: sptez_s,load_grid,fill_ns
  use specgrid,   only: init_spec_grid,destroy_spec_grid
  use specgrid,   only: imax,jmax

  use m_llInterp, only : llInterp
  use m_llInterp, only : llInterp_init
  use m_llInterp, only : llInterp_clean
  use m_llInterp, only : llInterp_atog

  implicit none

  integer,intent(in) :: itype
  real(fp_kind),dimension(nlat,nlon),intent(inout) :: grd

! local variables 
  integer :: k, i2,i2m1,i,i1
  real(fp_kind),allocatable,dimension(:) :: wrkspec
  real(fp_kind),allocatable,dimension(:,:) :: grid
  real(fp_kind),allocatable,dimension(:,:) :: grds
  real(double),allocatable,dimension(:):: tlats
  real(double),allocatable,dimension(:,:) :: grdd
  real(double),allocatable,dimension(:,:) :: grdg
  type(llInterp) :: obll

  allocate ( wrkspec(nc) ) 

  call init_spec_grid(nGlat,nGlon,.true.)
  allocate ( tlats(nGlat) )
  allocate ( grds(nGlat, nGlon) )
  allocate ( grid (imax, jmax) )

!  tlats = glats * 180.0 / pi
  call llInterp_init(obll,nlon,nlat,nGlon,nGlat)
  deallocate ( tlats )

  allocate( grdd(nlon, nlat) )
  allocate( grdg(nGlon,nGlat) )

  call swapij2_(grd,grdd,nlat,nlon)
  call llInterp_atog(obll,grdd,grdg, vector=.false.)
  call swapij2d_(grdg,grds,nGlat,nGlon)

  call llInterp_clean(obll)
  deallocate (grdd, grdg )

  call load_grid(grds,nGlat,nGlon,grid)
  call sptez_s(wrkspec,grid,-1)
  deallocate(grid,grds)
    
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

  call destroy_spec_grid

  call init_spec_grid(nlat,nlon,lgaus)
  allocate ( grds (nlat,nlon) )
  allocate ( grid (imax, jmax) )

  call sptez_s(wrkspec,grid,1)
  call fill_ns(grid,nlat,nlon,grds)
  grd = grds  

  deallocate ( grds )
  deallocate ( grid )
  deallocate ( wrkspec )

  call destroy_spec_grid

  return

end subroutine m_calap

subroutine swapij2_(aij,aji,nlat,nlon)
                                                                                         
  use type_kinds, only : fp_kind, double
  implicit none
  integer,intent(in) :: nlat,nlon
  real(fp_kind), dimension(nlat,nlon),intent(in ) :: aij
  real(double),dimension(nlon,nlat),intent(out) :: aji
  integer :: i,j,k

  do i=1,nlon
    aji(i,1:nlat)=aij(1:nlat,i)
  end do

end subroutine swapij2_
