! it's sad but necessary
#ifdef _MAYBE_
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
#endif /* _MAYBE_ */

! not the best place for this ...
! rename writit to xwritit to hide it from other versions
subroutine xwritit ( q,im,jm,lm,ku,Grid )
      use ESMF, only: ESMF_VMGetCurrent
      use ESMF, only: ESMF_VMGet
      use ESMF, only: ESMF_VM
      use ESMF, only: ESMF_GridCompGet
      use ESMF, only: ESMF_GridComp
      use ESMF, only: ESMF_Grid
      use MAPL_Mod, only: ArrayGather
      use MAPL_Mod, only: MAPL_GridGet
      implicit none
      type ( ESMF_Grid ) Grid
      integer  im,jm,lm
      real   q(im,jm,lm)
!
      type(ESMF_GridComp),pointer :: GC
      type(ESMF_VM) :: vm
      real,   allocatable :: glo(:,:)
      real*4, allocatable ::   a(:,:)
      integer  L,ku,img,jmg,myid,status
      integer  dims(3)
      call ESMF_VMGetCurrent(vm=vm, rc=status)
      call ESMF_VMGet(vm, localPET=myid)
      call MAPL_GridGet(Grid, globalCellCountPerDim=DIMS, RC=STATUS)
      img=dims(1)
      jmg=dims(2)
      allocate ( glo(img,jmg) )
      allocate (   a(img,jmg) )
      do L=1,lm
!        call timebeg ('   Gather')
         call ArrayGather(q(:,:,L), glo, Grid, rc=status)
!        call timeend ('   Gather')
         if( myid.eq.0 ) then
                       a = glo
             write(ku) a
         endif
      enddo
      deallocate ( glo )
      deallocate ( a   )
      return
end subroutine xwritit
subroutine CubeHaloInit(comm, im_world, npes, nx, ny, domainIdx)
  integer :: comm, im_world, npes, nx, ny
  integer :: domainIdx
end subroutine CubeHaloInit

subroutine CubeHalo(domainIdx, input)
  integer :: domainIdx
  real *4 :: input(:,:)
end subroutine CubeHalo
