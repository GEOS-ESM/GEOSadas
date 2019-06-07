implicit none

character(len=*),parameter::fname='/discover/nobackup/vbuchard/enswork.ENKF_DFT1/aer_mem028.ext_Nc.2015050100.nc4'
character(len=*),parameter::fnout='myaod.nc4'
integer fid, readgfio, rc
integer im,jm,km,lm,nvars,ngatts,kbeg
integer nymd,nhms
character(len=80):: vnamein
real(8),allocatable :: grid_aod(:,:,:)
real(4),allocatable :: grads(:,:)

nymd = 20150501
nhms = 0
kbeg = 1
readgfio = 1


  call  GFIO_Open ( fname, READGFIO, fid, rc )
  if ( rc == 0 ) then
     print *, '[r] read gridded AOD for'//trim(fname)
  else
     print *,'[x] error opening AOD background file in gfio' //trim(fname)//', rc= ', rc
!    call die (myname)
     stop
  endif 

  call GFIO_DimInquire (fid,im,jm,km,lm,nvars,ngatts,rc)
  if ( rc == 0 ) then
     print *, '[r] Getting dimension info from file' //trim(fname), rc
  else
     print *,'[x] error getting file dimension from gfioDimInquire' //trim(fname)//', rc= ', rc
!    call die (myname)
     stop
     STOP
  endif 
  allocate( grid_aod(im,jm,km))
  allocate( grads(im,jm))
  
  print *,'DimInquire', 'im = ',im, 'jm=', jm, 'km=', km, 'lm=', lm, 'nvars=', nvars,ngatts, 'rc=', rc
!  print *, 'lon', lon

  vnamein = 'taod'
  print*, 'vname=', vnamein
  call GFIO_GetVar ( fid, vnamein, nymd , nhms, im, jm, kbeg, km, grid_AOD, rc)
  print*, 'rrrrrc', rc
  if ( rc == 0 ) then
     print *, '[r] Readind taod from gridded file' //trim(fname)
  else
     print *,'[x] error getting taod from gfiogetvar' //trim(fname)//', rc= ', rc
!    call die (myname)
     STOP
  endif 

  call GFIO_close ( fid, rc )

  call writnc4_

contains 
    subroutine writnc4_
    real(8),parameter:: missing_val = 1.d15
    real(8) lon(im),lat(jm),lev(km)
    real(8) lat_min,lon_min,lat_max,lon_max,lat_del,lon_del
    integer i,j,fid,err
    integer prec,timeinc
    integer, parameter :: READ_WRITE = 0

    
    character(len=256)              :: title, source, contact, levunits
    character(len=256), allocatable :: vname(:), vtitle(:), vunits(:)

    real(8), allocatable :: valid_range(:,:), packing_range(:,:)
    integer, allocatable :: kmvar(:)

    prec = 0
    timeinc = 060000

    nvars=1
    allocate(vname(nvars),vtitle(nvars),vunits(nvars),kmvar(nvars))
    allocate(valid_range(2,nvars), packing_range(2,nvars))

    vname(1) = vnamein
    vtitle(1) = 'AOD'
    vunits(1) = '1'
    kmvar(1) = 0

   do j = 1, nvars
      do i = 1, 2
         valid_range(i,j) = missing_val
         packing_range(i,j) = missing_val
      end do
   end do

     lon_del = 360.0d0 / im
     lon_min = -180.0d0
     lon_max = lon_min + (im-1) * lon_del
     lat_min = -90.0d0
     lat_max = +90.0d0
     lat_del = ( lat_max - lat_min ) / ( jm-1)

     levunits='hPa'

     do j = 1, jm
        lat(j) = lat_min + (j-1) * lat_del
     enddo
     do i = 1, im
        lon(i) = lon_min + (i-1) * lon_del
     enddo
     lev(1)=1000.d0

    title='test'
    source='gmao'
    contact='vb'
    call GFIO_Create ( trim(fnout), title, source, contact, missing_val,  &
                       im, jm, km, lon, lat, lev, levunits,         &
                       nymd, nhms, timeinc,                         &
                       nvars, vname, vtitle, vunits, kmvar,         &
                       valid_range, packing_range, prec,            &
                       fid, err )

    call GFIO_Open ( fnout, READ_WRITE, fid, err )
    print *, sum(grid_aod)
    print *
    print *
    print *
    print *
    print *, grid_aod(1:5,1:5,1)
    call GFIO_PutVar ( fid, vname(1), nymd, nhms, im, jm, 0,  1, grid_aod(:,:,1), err)
    call GFIO_close ( fid, err )
    end subroutine writnc4_
end
    
