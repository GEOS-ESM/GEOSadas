!  HISTORY:
!
!    ?????2010 Zhu, Y.     - Original code
!    18Apr2010 Todling     - add implicit none
!                          - add command line input/output: resolution
!    ????????? El Akkraoui - changes to make it look more like NMC-code itself
!    10May2018 Todling     - a little more strealined in preparation for interp
!                          - add ability to vertically interpolate berror
!    27Aug2021 Todling     - introduce NetCDF version
!
!   Declare local variables

  program write_berror_global

  use m_nc_berror, only: nc_berror_vars_init
  use m_nc_berror, only: nc_berror_vars_final
  use m_nc_berror, only: nc_berror_vars_comp
  use m_nc_berror, only: nc_berror_vars_copy
  use m_nc_berror, only: nc_berror_vars
  use m_nc_berror, only: nc_berror_read
  use m_nc_berror, only: nc_berror_write
  implicit none

  real(4),allocatable,dimension(:)::  corp_avn,hwllp_avn
  real(4),allocatable,dimension(:,:):: corsst_avn,hwllsst_avn
  real(4),allocatable,dimension(:,:)::  bv_avn,wgv_avn,corqq_avn,pput_avn
  real(4),allocatable,dimension(:,:,:):: corz_avn,hwll_avn,vztdq_avn,agv_avn
  real(4),allocatable,dimension(:,:):: corz,corzq,hwll,vztdq

  type(nc_berror_vars) ivars
  type(nc_berror_vars) xvars

  integer, parameter :: luin =22
  integer, parameter :: luout=45
  integer, parameter :: lugrd=46
  integer isig,ilat,ilon  ! dims in file
  integer msig,mlat,mlon  ! user dims
  integer i,j,k,m,ncfggg,iret,kindex
  integer status

  character(len=256)  argv, ifname, ofname
  character(255) grdfile
  character(len=5) var(40)
  character(len=256) ncfile
  logical merra2current ! convert older format to current format
  logical hydromet,dimsonly
  logical :: nc_read_test = .true.
  logical :: verbose
  logical :: convert_endian

  dimsonly = .false.
  hydromet = .true.
  merra2current =.false.
  ncfile = 'NULL'

  call init_()

  call get_berror_dims_(ilon,ilat,isig)
  if(dimsonly) then
    print * , "input resolution: ", ilon, ' x ', ilat, ' x ', isig
    stop
  endif

  call nc_berror_vars_init(ivars,ilon,ilat,isig)

  if (merra2current) then
     call berror_old_read_(mlon,mlat,msig)
     deallocate ( corp_avn,hwllp_avn,corsst_avn,hwllsst_avn )
     deallocate ( corz_avn,corqq_avn,hwll_avn,vztdq_avn,agv_avn )
     deallocate ( bv_avn,wgv_avn,pput_avn )
     deallocate ( corz,corzq,hwll,vztdq )
  else
     call berror_read_(ivars)
  endif

  if (mlat/=ivars%nlat.and.mlon/=ivars%nlon.and.msig/=ivars%nsig) then
     print *, 'cannot interpolate all three dims at one, try horz than vert ...'
     call exit(1)
  endif
  if (mlat/=ivars%nlat.or.mlon/=ivars%nlon) then
     write(6,'(a)') ' Horizontally interpolating error covariance fields ...'
     call nc_berror_vars_init(xvars,ilon,ilat,isig)
     call nc_berror_vars_copy(ivars,xvars)
     call nc_berror_vars_final(ivars)
     call nc_berror_vars_init(ivars,mlon,mlat,isig)
     call hinterp_berror_vars_(xvars,ivars)
     write(6,'(a)') ' Finish horizontal interpolation.'
     call nc_berror_vars_final(xvars)
  endif
  if (msig/=ivars%nsig) then
     write(6,'(a)') ' Vertically interpolating error covariance fields ...'
     call nc_berror_vars_init(xvars,ilon,ilat,isig)
     call nc_berror_vars_copy(ivars,xvars)
     call nc_berror_vars_final(ivars)
     call nc_berror_vars_init(ivars,ilon,ilat,msig)
     call nc_berror_vars_copy(xvars,ivars) ! copy lat/lon fields
     call vinterp_berror_vars_(xvars,ivars)
     write(6,'(a)') ' Finish vertical interpolation.'
     call nc_berror_vars_final(xvars)
  endif
  call berror_write_(ivars,merra2current)
  if(trim(ncfile)/='NULL') then
     call be_write_nc_(ncfile,ivars)
     if ( nc_read_test ) then
        call nc_berror_read(ncfile,xvars,status)
        call be_write_nc_('again.nc',xvars)
        call nc_berror_vars_comp(ivars,xvars,status)
        call nc_berror_vars_final(xvars)
     endif
  endif
  call berror_write_grads_(ivars)

  call nc_berror_vars_final(ivars)


contains
  subroutine init_

  character(len=255) :: argv
  integer fixargs,ncount,iarg,argc

  argc = iargc()
  if ( argc < 1 ) then 
     print *
     print *, "Usage: write_berror_global.x [options] ifname ofname nlon nlat nlev"
     print *
     print *, " OPTIONS:"
     print *, "   -nohydro    - handles case w/o hydrometeors"
     print *, "   -nc  FNAME  - output errors in NetCDF format to file FNAME"
     print *, "   -verb       - echo additional information as it proceeds"
     print *, "   -endian     - convert endian"
     print *, ""
     print *
     stop
  end if

  verbose = .false.
  convert_endian = .false.
  fixargs = 5
  iarg = 0; ncount=0
  do i = 1, 32767
     iarg = iarg + 1
     if ( iarg .gt. argc ) exit
     call GetArg ( iarg, argv )
     select case (trim(argv))
       case('-endian')
          convert_endian = .true.
       case('-nohydro')
          hydromet = .false.
       case('-verb')
          verbose = .true.
       case('-nc')
          iarg = iarg + 1
          call GetArg ( iarg, ncfile )
       case default
          ncount = ncount + 1
          if (ncount > fixargs) exit
          if (ncount==1) then
             ifname=trim(argv)
          else if (ncount==2) then
             ofname=trim(argv)
          else if (ncount==3) then
             read(argv,*) mlon
          else if (ncount==4) then
             read(argv,*) mlat
          else if (ncount==5) then
             read(argv,*) msig
          endif
     end select
  enddo

  if (ncount==1) then
    dimsonly=.true.
  else
    print * , "input  filename: ", trim(ifname)
    print * , "output filename: ", trim(ofname)
    print * , "desired output resolution: ", mlon, ' x ', mlat, ' x ', msig
  endif

  end subroutine init_

  subroutine berror_old_read_(ilon,ilat,isig)

  integer, intent(in) :: ilon,ilat,isig
  integer :: nlon,nlat,nsig

  if (convert_endian) then
     open(luin,file=trim(ifname),form='unformatted',convert='big_endian')
  else
     open(luin,file=trim(ifname),form='unformatted')
  endif

  nlon=ilon
  rewind luin
  read(luin)nsig,nlat
  if(isig/=isig.or.nlat/=ilat) then
    print*, 'berror_old_read_, inconsistent dims, aborting ...'
    call exit(1)
  endif

! Allocate arrays in stats file
  allocate ( corp_avn(nlat),hwllp_avn(nlat), &
             corsst_avn(nlat,nlon),hwllsst_avn(nlat,nlon) )

  allocate ( corz_avn(1:nlat,1:nsig,1:10) )
  allocate ( corqq_avn(1:nlat,1:nsig) )
  allocate ( hwll_avn(nlat,1:nsig,1:10) )
  allocate ( vztdq_avn(nlat,nsig,1:10) )
  allocate ( agv_avn(nlat,1:nsig,1:nsig) )
  allocate ( bv_avn(nlat,1:nsig),wgv_avn(nlat,1:nsig) )
  allocate ( pput_avn(nlat,1:nsig) )

  allocate ( corz(1:nlat,1:nsig),corzq(1:nlat,1:nsig))
  allocate ( hwll(nlat,1:nsig))
  allocate ( vztdq(nlat,nsig))

  rewind luin
     read(luin) nsig,nlat, &
     corz_avn(:,:,1:4),corqq_avn,corz_avn(:,:,5:10),corp_avn, &
     hwll_avn,hwllp_avn,vztdq_avn,&
     agv_avn,bv_avn,wgv_avn, &
     corsst_avn,hwllsst_avn
  close(luin)

  end subroutine berror_old_read_

  subroutine get_berror_dims_(nlon,nlat,nsig)
  integer, intent(out) :: nlon,nlat,nsig
  if (convert_endian) then
     open(luin,file=trim(ifname),form='unformatted',convert='big_endian')
  else
     open(luin,file=trim(ifname),form='unformatted')
  endif
  read(luin) nsig,nlat,nlon
  close(luin)
  end subroutine get_berror_dims_
  
  subroutine berror_read_(vr)

  type(nc_berror_vars) vr
  integer nlat,nlon,nsig

  var=' '

  if (convert_endian) then
     open(luin,file=trim(ifname),form='unformatted',convert='big_endian')
  else
     open(luin,file=trim(ifname),form='unformatted')
  endif
     rewind luin
     read(luin) nsig,nlat,nlon
     read(luin) vr%tcon,vr%vpcon,vr%pscon
     read(luin) var(1),nsig
     read(luin) vr%sfvar
     read(luin) vr%sfhln
     read(luin) vr%sfvln
     read(luin) var(2),nsig
     read(luin) vr%vpvar
     read(luin) vr%vphln
     read(luin) vr%vpvln
     read(luin) var(3),nsig
     read(luin) vr%tvar
     read(luin) vr%thln
     read(luin) vr%tvln
     read(luin) var(4),nsig
     read(luin) vr%qvar,vr%nrhvar
     read(luin) vr%qhln
     read(luin) vr%qvln
     if (hydromet) then 
     read(luin) var(5),nsig
     read(luin) vr%qivar
     read(luin) vr%qihln
     read(luin) vr%qivln
     read(luin) var(6),nsig
     read(luin) vr%qlvar
     read(luin) vr%qlhln
     read(luin) vr%qlvln
     read(luin) var(7),nsig
     read(luin) vr%qrvar
     read(luin) vr%qrhln
     read(luin) vr%qrvln
     read(luin) var(8),nsig
     read(luin) vr%qsvar
     read(luin) vr%qshln
     read(luin) vr%qsvln
     endif
     read(luin) var(9),nsig
     read(luin) vr%ozvar
     read(luin) vr%ozhln
     read(luin) vr%ozvln
     read(luin) var(10),nsig
     read(luin) vr%cvar
     read(luin) vr%chln
     read(luin) vr%cvln
     read(luin) var(11),m
     read(luin) vr%psvar
     read(luin) vr%pshln
     read(luin) var(12),m
     read(luin) vr%varsst
     read(luin) vr%corlsst
   close(luin)
  end subroutine berror_read_

  subroutine berror_write_(vr,m2c)

  implicit none

  type(nc_berror_vars) vr
  logical, intent(in) :: m2c
  integer  i,j,k,m,nlon,nlat,nsig

  var=' '
  var(1)='sf'
  var(2)='vp'
  var(3)='t'
  var(4)='q'
  var(5)='qi'
  var(6)='ql'
  var(7)='qr'
  var(8)='qs'
  var(9)='oz'
  var(10)='cw'
  var(11)='ps'
  var(12)='sst'
 
  nlat=vr%nlat
  nlon=vr%nlon
  nsig=vr%nsig

  if ( m2c ) then
     vr%sfvar=0.0; vr%vpvar=0.0; vr%tvar=0.0; vr%qvar=0.0; vr%cvar=0.0; vr%nrhvar=0.0;vr%ozvar=0.0
     vr%sfhln=0.0; vr%vphln=0.0; vr%thln=0.0; vr%qhln=0.0; vr%chln=0.0; vr%ozhln =0.0
     vr%sfvln=0.0; vr%vpvln=0.0; vr%tvln=0.0; vr%qvln=0.0; vr%cvln=0.0; vr%ozvln =0.0
     vr%qivar=0.0; vr%qlvar=0.0; vr%qsvar=0.0;vr%qsvar=0.0
     vr%qihln=0.0; vr%qlhln=0.0; vr%qrhln=0.0;vr%qshln=0.0
     vr%qivln=0.0; vr%qlvln=0.0; vr%qrvln=0.0;vr%qsvln=0.0
     vr%pscon=0.0; vr%vpcon=0.0; vr%varsst=0.0; vr%corlsst=0.0;  vr%tcon=0.0; vr%psvar =0.0
   
     do i=1,nlat
       vr%psvar(i)=corp_avn(i)
       vr%pshln(i)=hwllp_avn(i)
     end do
     do j=1,nlon
       do i=1,nlat
         vr%varsst(i,j)=corsst_avn(i,j)
         vr%corlsst(i,j)=hwllsst_avn(i,j)
       end do
     end do

!    Load single precision arrays for visualization
     do k=1,nsig
       do i=1,nlat
         vr%sfvar(i,k) =corz_avn(i,k,1)
         vr%vpvar(i,k) =corz_avn(i,k,2)
         vr%tvar(i,k)  =corz_avn(i,k,3)
         vr%qvar(i,k)  =corz_avn(i,k,4)
         vr%nrhvar(i,k)=corqq_avn(i,k)
         vr%qivar(i,k) =corz_avn(i,k,5)
         vr%qlvar(i,k) =corz_avn(i,k,6)
         vr%qrvar(i,k) =corz_avn(i,k,7)
         vr%qsvar(i,k) =corz_avn(i,k,8)
         vr%ozvar(i,k) =corz_avn(i,k,9)
         vr%cvar(i,k)  =corz_avn(i,k,10)

         vr%sfhln(i,k)=hwll_avn(i,k,1)
         vr%vphln(i,k)=hwll_avn(i,k,2)
         vr%thln(i,k) =hwll_avn(i,k,3)
         vr%qhln(i,k) =hwll_avn(i,k,4)
         vr%qihln(i,k)=hwll_avn(i,k,5)
         vr%qlhln(i,k)=hwll_avn(i,k,6)
         vr%qrhln(i,k)=hwll_avn(i,k,7)
         vr%qshln(i,k)=hwll_avn(i,k,8)
         vr%ozhln(i,k)=hwll_avn(i,k,9)
         vr%chln(i,k) =hwll_avn(i,k,10)

         vr%sfvln(i,k)=vztdq_avn(i,k,1)
         vr%vpvln(i,k)=vztdq_avn(i,k,2)
         vr%tvln(i,k) =vztdq_avn(i,k,3)
         vr%qvln(i,k) =vztdq_avn(i,k,4)
         vr%qivln(i,k)=vztdq_avn(i,k,5)
         vr%qlvln(i,k)=vztdq_avn(i,k,6)
         vr%qrvln(i,k)=vztdq_avn(i,k,7)
         vr%qsvln(i,k)=vztdq_avn(i,k,8)
         vr%ozvln(i,k)=vztdq_avn(i,k,9)
         vr%cvln(i,k) =vztdq_avn(i,k,10)

       end do
     end do

     do m=1,nsig
       do k=1,nsig
         do i=1,nlat
           vr%tcon(i,k,m)=agv_avn(i,k,m)
         end do
       end do
     end do
     do k=1,nsig
       do i=1,nlat
         vr%pscon(i,k)=wgv_avn(i,k)
         vr%vpcon(i,k)=bv_avn(i,k)
       end do
     end do

  endif ! m2c

  open(luout,file=trim(ofname),form='unformatted')
     rewind luout
     write(luout) nsig,nlat,nlon
     write(luout) vr%tcon,vr%vpcon,vr%pscon
     write(luout) var(1),nsig
     write(luout) vr%sfvar
     write(luout) vr%sfhln
     write(luout) vr%sfvln
     write(luout) var(2),nsig
     write(luout) vr%vpvar
     write(luout) vr%vphln
     write(luout) vr%vpvln
     write(luout) var(3),nsig
     write(luout) vr%tvar
     write(luout) vr%thln
     write(luout) vr%tvln
     write(luout) var(4),nsig
     write(luout) vr%qvar,vr%nrhvar
     write(luout) vr%qhln
     write(luout) vr%qvln
     if (hydromet) then
     write(luout) var(5),nsig
     write(luout) vr%qivar
     write(luout) vr%qihln
     write(luout) vr%qivln
     write(luout) var(6),nsig
     write(luout) vr%qlvar
     write(luout) vr%qlhln
     write(luout) vr%qlvln
     write(luout) var(7),nsig
     write(luout) vr%qrvar
     write(luout) vr%qrhln
     write(luout) vr%qrvln
     write(luout) var(8),nsig
     write(luout) vr%qsvar
     write(luout) vr%qshln
     write(luout) vr%qsvln
     endif
     write(luout) var(9),nsig
     write(luout) vr%ozvar
     write(luout) vr%ozhln
     write(luout) vr%ozvln
     write(luout) var(10),nsig
     write(luout) vr%cvar
     write(luout) vr%chln
     write(luout) vr%cvln
     m=1
     write(luout) var(11),m
     write(luout) vr%psvar
     write(luout) vr%pshln
     write(luout) var(12),m
     write(luout) vr%varsst
     write(luout) vr%corlsst
   close(luout)
  end subroutine berror_write_
  subroutine berror_write_grads_(vars)

   use sstmod, only: write_grads_ctl
   type(nc_berror_vars) vars
   integer j,nsig,nlat,nlon,iret
   real(4),allocatable,dimension(:,:) :: aux

   nlat=vars%nlat 
   nlon=vars%nlon 
   nsig=vars%nsig 
   
   call baopenwt(lugrd,'bgstats_sp.grd',iret)

   call wryte(lugrd,4*nlat*nsig,vars%sfvar)
   call wryte(lugrd,4*nlat*nsig,vars%vpvar)
   call wryte(lugrd,4*nlat*nsig,vars%tvar)
   call wryte(lugrd,4*nlat*nsig,vars%qvar)
   call wryte(lugrd,4*nlat*nsig,vars%nrhvar)
   if (hydromet) then
   call wryte(lugrd,4*nlat*nsig,vars%qivar)
   call wryte(lugrd,4*nlat*nsig,vars%qlvar)
   call wryte(lugrd,4*nlat*nsig,vars%qrvar)
   call wryte(lugrd,4*nlat*nsig,vars%qsvar)
   endif
   call wryte(lugrd,4*nlat*nsig,vars%ozvar)
   call wryte(lugrd,4*nlat*nsig,vars%cvar)
   call wryte(lugrd,4*nlat     ,vars%psvar)
   call wryte(lugrd,4*nlat*nsig,vars%sfhln)
   call wryte(lugrd,4*nlat*nsig,vars%vphln)
   call wryte(lugrd,4*nlat*nsig,vars%thln)
   call wryte(lugrd,4*nlat*nsig,vars%qhln)
   if (hydromet) then
   call wryte(lugrd,4*nlat*nsig,vars%qihln)
   call wryte(lugrd,4*nlat*nsig,vars%qlhln)
   call wryte(lugrd,4*nlat*nsig,vars%qrhln)
   call wryte(lugrd,4*nlat*nsig,vars%qshln)
   endif
   call wryte(lugrd,4*nlat*nsig,vars%ozhln)
   call wryte(lugrd,4*nlat*nsig,vars%chln)
   call wryte(lugrd,4*nlat     ,vars%pshln)
   call wryte(lugrd,4*nlat*nsig,1./vars%sfvln)
   call wryte(lugrd,4*nlat*nsig,1./vars%vpvln)
   call wryte(lugrd,4*nlat*nsig,1./vars%tvln)
   call wryte(lugrd,4*nlat*nsig,1./vars%qvln)
   if (hydromet) then
   call wryte(lugrd,4*nlat*nsig,1./vars%qivln)
   call wryte(lugrd,4*nlat*nsig,1./vars%qlvln)
   call wryte(lugrd,4*nlat*nsig,1./vars%qrvln)
   call wryte(lugrd,4*nlat*nsig,1./vars%qsvln)
   endif
   call wryte(lugrd,4*nlat*nsig,1./vars%ozvln)
   call wryte(lugrd,4*nlat*nsig,1./vars%cvln)
   call wryte(lugrd,4*nlat*nsig*nsig,vars%tcon)
   call wryte(lugrd,4*nlat*nsig,vars%vpcon)
   call wryte(lugrd,4*nlat*nsig,vars%pscon)

   call baclose(lugrd,iret)

  open(luout,file='tcon.bin',form='unformatted',convert='little_endian')
  do j=1,nlat
     write(luout) vars%tcon(j,:,:)
  enddo 
  close(luout)

! Put out SST info to on a separate grads file
  allocate(aux(nlon,nlat))
  call baopenwt(lugrd,'sst.grd',iret)

  aux = transpose(vars%varsst)
  call wryte(lugrd,4*nlat*nlon,aux)

  aux = transpose(vars%corlsst)
  call wryte(lugrd,4*nlat*nlon,aux)

  call baclose(lugrd,iret)
  deallocate(aux)
  call write_grads_ctl('sst',lugrd,nlon,nlat)

  end subroutine berror_write_grads_

  subroutine vinterp_berror_vars_(ivars,ovars)

  use m_spline, only: spline
  use m_set_eta, only: set_eta
  use m_set_eta, only: get_ref_plevs
  use m_const, only: pstd
  implicit none

  type(nc_berror_vars) ivars
  type(nc_berror_vars) ovars

  real(4),allocatable,dimension(:,:) :: aux
  real(4),allocatable,dimension(:) :: plevi,plevo
  real(4),allocatable,dimension(:) :: ak,bk
  real(4) ptop, pint
  integer k,ks

  if( ivars%nlat/=ovars%nlat .or. &
      ivars%nlon/=ovars%nlon      ) then
      print *, 'vinterp_berror_vars_: error, nlat/nlon must equal'
      call exit(1)
  endif 

! Input levels
! ------------
  allocate(plevi(ivars%nsig))
  allocate(ak(ivars%nsig+1),bk(ivars%nsig+1))
  call set_eta ( ivars%nsig, ks, ptop, pint, ak, bk )
  call get_ref_plevs ( ak, bk, ptop, plevi )
  deallocate(ak,bk)

! Output levels
! -------------
  allocate(plevo(ovars%nsig))
  allocate(ak(ovars%nsig+1),bk(ovars%nsig+1))
  call set_eta ( ovars%nsig, ks, ptop, pint, ak, bk )
  call get_ref_plevs ( ak, bk, ptop, plevo )
  deallocate(ak,bk)

! Log of normalized levels (perhaps log(p^kappa)?)
! ------------------------
! plevi = log(plevi/pstd)
! plevo = log(plevo/pstd)

! re-orient levels for GSI compliance
! -----------------------------------
  plevi = plevi(ivars%nsig:1:-1)
  plevo = plevo(ovars%nsig:1:-1)

! allocate(vr%tcon(nlat,nsig,nsig))
! TBD ovars%tcon    = ivars%tcon

   
  allocate(aux(ivars%nsig,ovars%nsig))
  do j=1,ivars%nlat ! very, very parallelizable

     ! tcon is a covariance matrix, so we must be carefull with its
     ! interpolation. It should be done as in 
     !             Bnew = T Bold T', 
     !                  = T ( T Bold )'
     ! where T is the interpolation matrix and T' its transpose.
     ! Not all interpolants will preserve covariance properties. 
     ! Interpolate columns first ...
     aux=0.0
     do k=1,ivars%nsig
        call spline( plevi, plevo, ivars%tcon(j,k,:), aux(k,:) )
     enddo
!    now interpolate rows of previous resulting matrix
     do k=1,ovars%nsig
        call spline( plevi, plevo, aux(:,k), ovars%tcon(j,k,:) )
     enddo
     ! symmetrize is by force
     ovars%tcon(j,:,:) = 0.5*(ovars%tcon(j,:,:) + transpose(ovars%tcon(j,:,:)))

     call spline( plevi, plevo, ivars%vpcon (j,:), ovars%vpcon (j,:) )
     call spline( plevi, plevo, ivars%pscon (j,:), ovars%pscon (j,:) )
     call spline( plevi, plevo, ivars%sfvar (j,:), ovars%sfvar (j,:) )
     call spline( plevi, plevo, ivars%sfhln (j,:), ovars%sfhln (j,:) )
     call spline( plevi, plevo, ivars%sfvln (j,:), ovars%sfvln (j,:) )
     call spline( plevi, plevo, ivars%vpvar (j,:), ovars%vpvar (j,:) )
     call spline( plevi, plevo, ivars%vphln (j,:), ovars%vphln (j,:) )
     call spline( plevi, plevo, ivars%vpvln (j,:), ovars%vpvln (j,:) )
     call spline( plevi, plevo, ivars%tvar  (j,:), ovars%tvar  (j,:) )
     call spline( plevi, plevo, ivars%thln  (j,:), ovars%thln  (j,:) )
     call spline( plevi, plevo, ivars%tvln  (j,:), ovars%tvln  (j,:) )
     call spline( plevi, plevo, ivars%qvar  (j,:), ovars%qvar  (j,:) )
     call spline( plevi, plevo, ivars%nrhvar(j,:), ovars%nrhvar(j,:) )
     call spline( plevi, plevo, ivars%qhln  (j,:), ovars%qhln  (j,:) )
     call spline( plevi, plevo, ivars%qvln  (j,:), ovars%qvln  (j,:) )
     call spline( plevi, plevo, ivars%qivar (j,:), ovars%qivar (j,:) )
     call spline( plevi, plevo, ivars%qihln (j,:), ovars%qihln (j,:) )
     call spline( plevi, plevo, ivars%qivln (j,:), ovars%qivln (j,:) )
     call spline( plevi, plevo, ivars%qlvar (j,:), ovars%qlvar (j,:) )
     call spline( plevi, plevo, ivars%qlhln (j,:), ovars%qlhln (j,:) )
     call spline( plevi, plevo, ivars%qlvln (j,:), ovars%qlvln (j,:) )
     call spline( plevi, plevo, ivars%qrvar (j,:), ovars%qrvar (j,:) )
     call spline( plevi, plevo, ivars%qrhln (j,:), ovars%qrhln (j,:) )
     call spline( plevi, plevo, ivars%qrvln (j,:), ovars%qrvln (j,:) )
     call spline( plevi, plevo, ivars%qsvar (j,:), ovars%qsvar (j,:) )
     call spline( plevi, plevo, ivars%qshln (j,:), ovars%qshln (j,:) )
     call spline( plevi, plevo, ivars%qsvln (j,:), ovars%qsvln (j,:) )
     call spline( plevi, plevo, ivars%ozvar (j,:), ovars%ozvar (j,:) )
     call spline( plevi, plevo, ivars%ozhln (j,:), ovars%ozhln (j,:) )
     call spline( plevi, plevo, ivars%ozvln (j,:), ovars%ozvln (j,:) )
     call spline( plevi, plevo, ivars%cvar  (j,:), ovars%cvar  (j,:) )
     call spline( plevi, plevo, ivars%chln  (j,:), ovars%chln  (j,:) )
     call spline( plevi, plevo, ivars%cvln  (j,:), ovars%cvln  (j,:) )
  enddo
  deallocate(aux)
  deallocate(plevi)
  deallocate(plevo)

  end subroutine vinterp_berror_vars_

  subroutine hinterp_berror_vars_(ivars,ovars)

  use m_spline, only: spline
  use m_set_eta, only: set_eta
  use m_set_eta, only: get_ref_plevs
  use m_const, only: pstd
  implicit none

  type(nc_berror_vars) ivars
  type(nc_berror_vars) ovars

  real(4),allocatable,dimension(:,:) :: aux
  real(4),allocatable,dimension(:) :: lati,lato
  real(4),allocatable,dimension(:) :: loni,lono
  integer i,j,k,k2
  real dlon,dlat

  if( ivars%nsig/=ovars%nsig ) then
      print *, 'hinterp_berror_vars_: error, nsig must equal'
      call exit(1)
  endif 

! Input levels
! ------------
  allocate(lati(ivars%nlat))
  allocate(loni(ivars%nlon))
  dlat = 180./(ivars%nlat-1)
  do j=1,ivars%nlat
     lati(j) = -90.0 + (j-1.0)*dlat 
  enddo
  dlon = 360./ivars%nlon
  do i=1,ivars%nlon
     loni(i) = i*dlon ! GSI def
  enddo

! Output levels
! -------------
  allocate(lato(ovars%nlat))
  allocate(lono(ovars%nlon))
  dlat = 180./(ovars%nlat-1)
  do j=1,ovars%nlat
     lato(j) = -90.0 + (j-1.0)*dlat 
  enddo
  dlon = 360./ovars%nlon
  do i=1,ovars%nlon
     lono(i) = i*dlon ! GSI def
  enddo


  do k=1,ivars%nsig ! very, very parallelizable

     do k2=1,ivars%nsig
        call spline( lati, lato, ivars%tcon(:,k2,k), ovars%tcon(:,k2,k) )
     enddo

     call spline( lati, lato, ivars%vpcon (:,k), ovars%vpcon (:,k) )
     call spline( lati, lato, ivars%pscon (:,k), ovars%pscon (:,k) )
     call spline( lati, lato, ivars%sfvar (:,k), ovars%sfvar (:,k) )
     call spline( lati, lato, ivars%sfhln (:,k), ovars%sfhln (:,k) )
     call spline( lati, lato, ivars%sfvln (:,k), ovars%sfvln (:,k) )
     call spline( lati, lato, ivars%vpvar (:,k), ovars%vpvar (:,k) )
     call spline( lati, lato, ivars%vphln (:,k), ovars%vphln (:,k) )
     call spline( lati, lato, ivars%vpvln (:,k), ovars%vpvln (:,k) )
     call spline( lati, lato, ivars%tvar  (:,k), ovars%tvar  (:,k) )
     call spline( lati, lato, ivars%thln  (:,k), ovars%thln  (:,k) )
     call spline( lati, lato, ivars%tvln  (:,k), ovars%tvln  (:,k) )
     call spline( lati, lato, ivars%qvar  (:,k), ovars%qvar  (:,k) )
     call spline( lati, lato, ivars%nrhvar(:,k), ovars%nrhvar(:,k) )
     call spline( lati, lato, ivars%qhln  (:,k), ovars%qhln  (:,k) )
     call spline( lati, lato, ivars%qvln  (:,k), ovars%qvln  (:,k) )
     call spline( lati, lato, ivars%qivar (:,k), ovars%qivar (:,k) )
     call spline( lati, lato, ivars%qihln (:,k), ovars%qihln (:,k) )
     call spline( lati, lato, ivars%qivln (:,k), ovars%qivln (:,k) )
     call spline( lati, lato, ivars%qlvar (:,k), ovars%qlvar (:,k) )
     call spline( lati, lato, ivars%qlhln (:,k), ovars%qlhln (:,k) )
     call spline( lati, lato, ivars%qlvln (:,k), ovars%qlvln (:,k) )
     call spline( lati, lato, ivars%qrvar (:,k), ovars%qrvar (:,k) )
     call spline( lati, lato, ivars%qrhln (:,k), ovars%qrhln (:,k) )
     call spline( lati, lato, ivars%qrvln (:,k), ovars%qrvln (:,k) )
     call spline( lati, lato, ivars%qsvar (:,k), ovars%qsvar (:,k) )
     call spline( lati, lato, ivars%qshln (:,k), ovars%qshln (:,k) )
     call spline( lati, lato, ivars%qsvln (:,k), ovars%qsvln (:,k) )
     call spline( lati, lato, ivars%ozvar (:,k), ovars%ozvar (:,k) )
     call spline( lati, lato, ivars%ozhln (:,k), ovars%ozhln (:,k) )
     call spline( lati, lato, ivars%ozvln (:,k), ovars%ozvln (:,k) )
     call spline( lati, lato, ivars%cvar  (:,k), ovars%cvar  (:,k) )
     call spline( lati, lato, ivars%chln  (:,k), ovars%chln  (:,k) )
     call spline( lati, lato, ivars%cvln  (:,k), ovars%cvln  (:,k) )
  enddo
  call spline( lati, lato, ivars%psvar, ovars%psvar )
  call spline( lati, lato, ivars%pshln, ovars%pshln )

! Now handle horizontal 2d fields
  allocate(aux(ovars%nlat,ivars%nlon))

  ! varsst ...
  do i=1,ivars%nlon 
     call spline( lati, lato, ivars%varsst(:,i), aux(:,i) )
  enddo
  do j=1,ovars%nlat 
     call spline( loni, lono, aux(j,:), ovars%varsst(j,:) )
  enddo
  ! corlsst
  do i=1,ivars%nlon 
     call spline( lati, lato, ivars%corlsst(:,i), aux(:,i) )
  enddo
  do j=1,ovars%nlat 
     call spline( loni, lono, aux(j,:), ovars%corlsst(j,:) )
  enddo

  deallocate(aux)

  deallocate(lati,loni)
  deallocate(lato,lono)


  end subroutine hinterp_berror_vars_

  subroutine be_write_nc_(fname,ivars)

  use m_set_eta, only: set_eta
  use m_set_eta, only: get_ref_plevs
  implicit none

  character(len=*),     intent(in) :: fname
  type(nc_berror_vars), intent(in) :: ivars

  real(4),allocatable,dimension(:,:) :: aux
  real(4),allocatable,dimension(:) :: lats,lons
  real(4),allocatable,dimension(:) :: plevs
  real(4),allocatable,dimension(:) :: ak,bk
  real(4) ptop, pint, dlon, dlat
  integer :: nlat,nlon
  integer ii,jj,k,ks,status

  allocate(plevs(ivars%nsig))
  allocate(ak(ivars%nsig+1),bk(ivars%nsig+1))
  call set_eta ( ivars%nsig, ks, ptop, pint, ak, bk )
  call get_ref_plevs ( ak, bk, ptop, plevs )
  plevs = plevs(ivars%nsig:1:-1) ! reorient GEOS-5 levs to be consistent w/ GSI(Berror)

! The following defines lat/lon per GSI orientation
  nlon=ivars%nlon; nlat=ivars%nlat
  allocate(lons(nlon),lats(nlat))
  dlat=180./(nlat-1.0)
  do jj = nlat,1,-1
     lats(jj) = -90.0 + (jj-1.0)*dlat 
  enddo
  dlon=360./nlon
  do ii = 1, nlon
     lons(ii) = (ii-1.0)*dlon 
  enddo
  
  call nc_berror_write(trim(fname),ivars,plevs,lats,lons,status)

  deallocate(ak,bk)
  deallocate(plevs)
  deallocate(lons,lats)

  end subroutine be_write_nc_

  end program write_berror_global
