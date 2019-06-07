subroutine AppCSEdgeCreateF(IM_World, LonEdge, LatEdge, LonCenter, LatCenter, rc)
    integer,            intent(in   ) :: IM_World
    real(8), intent(inout) :: LonEdge(IM_World+1,IM_World+1,6)
    real(8), intent(inout) :: LatEdge(IM_World+1,IM_World+1,6)
! opt stuff bound not to work ... Todling
    real(8), intent(inout), optional :: LonCenter(IM_World,IM_World,6)
    real(8), intent(inout), optional :: LatCenter(IM_World,IM_World,6)
    integer, optional,  intent(out  ) :: rc
end subroutine AppCSEdgeCreateF

subroutine cube2latlon(npx, npy, nlon, nlat, data_cs, data_ll)

  implicit none
  
  integer, intent(in) :: npx, npy, nlon, nlat
  real, dimension(npx , npy ), intent(in ) :: data_cs
  real, dimension(nlon, nlat), intent(out) :: data_ll
  
end subroutine cube2latlon

subroutine latlon2cube(npx, npy, nlon, nlat, data_ll, data_cs)

 implicit none

 integer, intent(in) :: npx, npy, nlon, nlat
 real, dimension(npx , npy ), intent(out) :: data_cs
 real, dimension(nlon, nlat), intent(in ) :: data_ll

end subroutine latlon2cube

subroutine getweightsc2c
end subroutine getweightsc2c

subroutine CubeHaloInit(comm, im_world, npes, nx, ny, domainIdx)
  integer :: comm, im_world, npes, nx, ny
  integer :: domainIdx
end subroutine CubeHaloInit

subroutine CubeHalo(domainIdx, input)
  integer :: domainIdx
  real *4 :: input(:,:)
end subroutine CubeHalo
