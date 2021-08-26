!  HISTORY:
!
!    ?????2010 Zhu, Y.     - Original code
!    18Apr2010 Todling     - add implicit none
!                          - add command line input/output: resolution
!    ????????? El Akkraoui - changes to make it look more like NMC-code itself
!    10May2018 Todling     - a little more strealined in preparation for interp
!                          - add ability to vertically interpolate berror
!
!   Declare local variables

  program write_berror_global

  use m_nc_berror, only: berror_vars
  use m_nc_berror, only: write_nc_berror
  implicit none

! type berror_vars
!    integer :: nlon,nlat,nsig
!    real(4),allocatable,dimension(:,:,:):: tcon
!    real(4),allocatable,dimension(:,:)  :: sfvar,vpvar,tvar,qvar,cvar,nrhvar,ozvar
!    real(4),allocatable,dimension(:,:)  :: qivar,qlvar,qrvar,qsvar
!    real(4),allocatable,dimension(:,:)  :: sfhln,vphln,thln,qhln,chln,ozhln
!    real(4),allocatable,dimension(:,:)  :: qihln,qlhln,qrhln,qshln
!    real(4),allocatable,dimension(:,:)  :: sfvln,vpvln,tvln,qvln,cvln,ozvln
!    real(4),allocatable,dimension(:,:)  :: qivln,qlvln,qrvln,qsvln
!    real(4),allocatable,dimension(:,:)  :: vpcon,pscon,varsst,corlsst
!    real(4),allocatable,dimension(:)    :: psvar,pshln
! end type berror_vars

  real(4),allocatable,dimension(:)::  corp_avn,hwllp_avn
  real(4),allocatable,dimension(:,:):: corsst_avn,hwllsst_avn
  real(4),allocatable,dimension(:,:)::  bv_avn,wgv_avn,corqq_avn,pput_avn
  real(4),allocatable,dimension(:,:,:):: corz_avn,hwll_avn,vztdq_avn,agv_avn
  real(4),allocatable,dimension(:,:):: corz,corzq,hwll,vztdq

  type(berror_vars) ivars
  type(berror_vars) xvars

  integer, parameter :: luin =22
  integer, parameter :: luout=45
  integer, parameter :: lugrd=46
  integer isig,ilat,ilon  ! dims in file
  integer msig,mlat,mlon  ! user dims
  integer i,j,k,m,ncfggg,iret,kindex

  character(len=256)  argv, ifname, ofname
  character(255) grdfile
  character*5 var(40)
  logical merra2current ! convert older format to current format
  logical hydromet

  hydromet = .true.
  merra2current =.false.

  call init_()

  call get_berror_dims_(ilon,ilat,isig)

  call init_berror_vars_(ivars,ilon,ilat,isig)

  if (merra2current) then
     call berror_old_read_(mlon,mlat,msig)
     deallocate ( corp_avn,hwllp_avn,corsst_avn,hwllsst_avn )
     deallocate ( corz_avn,corqq_avn,hwll_avn,vztdq_avn,agv_avn )
     deallocate ( bv_avn,wgv_avn,pput_avn )
     deallocate ( corz,corzq,hwll,vztdq )
  else
     call berror_read_(ivars)
  endif

  if (mlat/=ivars%nlat.or.mlon/=ivars%nlon) then
     print *, 'cannot interpolate yet ...'
     call exit(1)
  endif
  if (msig/=ivars%nsig) then
     write(6,'(a)') ' Interpolating error covariance fields ...'
     call init_berror_vars_(xvars,ilon,ilat,isig)
     call copy_berror_vars_(ivars,xvars)
     call final_berror_vars_(ivars)
     call init_berror_vars_(ivars,ilon,ilat,msig)
     call copy_berror_vars_(xvars,ivars)
     call vinterp_berror_vars_(xvars,ivars)
     write(6,'(a)') ' Finish interpolation.'
  endif
  call berror_write_(ivars,merra2current)
  call be_write_nc_(ivars)
  call berror_write_grads_(ivars)

  call final_berror_vars_(ivars)



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
     print *, "   -nohyro  -  handles case w/o hydrometeors"
     print *
     stop
  end if

  fixargs = 5
  iarg = 0; ncount=0
  do i = 1, 32767
     iarg = iarg + 1
     if ( iarg .gt. argc ) exit
     call GetArg ( iarg, argv )
     select case (trim(argv))
       case('-nohydro')
          hydromet = .false.
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

  print * , "input  filename: ", trim(ifname)
  print * , "output filename: ", trim(ofname)
  print * , "desired output resolution: ", mlon, ' x ', mlat, ' x ', msig

  end subroutine init_

  subroutine berror_old_read_(ilon,ilat,isig)

  integer, intent(in) :: ilon,ilat,isig
  integer :: nlon,nlat,nsig

  open(luin,file=trim(ifname),form='unformatted')

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
  open(luin,file=trim(ifname),form='unformatted')
  read(luin) nsig,nlat,nlon
  close(luin)
  end subroutine get_berror_dims_
  
  subroutine init_berror_vars_(vr,nlon,nlat,nsig)

  integer,intent(in) :: nlon,nlat,nsig
  type(berror_vars) vr

  vr%nlon=nlon 
  vr%nlat=nlat
  vr%nsig=nsig

! allocate single precision arrays
  allocate(vr%sfvar(nlat,nsig),vr%vpvar(nlat,nsig),vr%tvar(nlat,nsig),vr%qvar(nlat,nsig),  &  
           vr%qivar(nlat,nsig),vr%qlvar(nlat,nsig),vr%qrvar(nlat,nsig),vr%qsvar(nlat,nsig),&
           vr%cvar(nlat,nsig),vr%nrhvar(nlat,nsig),vr%ozvar(nlat,nsig))
  allocate(vr%sfhln(nlat,nsig),vr%vphln(nlat,nsig),vr%thln(nlat,nsig),vr%qhln(nlat,nsig),  &
           vr%qihln(nlat,nsig),vr%qlhln(nlat,nsig),vr%qrhln(nlat,nsig),vr%qshln(nlat,nsig),&
           vr%chln(nlat,nsig), vr%ozhln(nlat,nsig))
  allocate(vr%sfvln(nlat,nsig),vr%vpvln(nlat,nsig),vr%tvln(nlat,nsig),vr%qvln(nlat,nsig),  &
           vr%qivln(nlat,nsig),vr%qlvln(nlat,nsig),vr%qrvln(nlat,nsig),vr%qsvln(nlat,nsig),&
           vr%cvln(nlat,nsig), vr%ozvln(nlat,nsig))
  allocate(vr%pscon(nlat,nsig),vr%vpcon(nlat,nsig))
  allocate(vr%varsst(nlat,nlon),vr%corlsst(nlat,nlon))
  allocate(vr%tcon(nlat,nsig,nsig))
  allocate(vr%psvar(nlat),vr%pshln(nlat))
  end subroutine init_berror_vars_

  subroutine berror_read_(vr)

  type(berror_vars) vr
  integer nlat,nlon,nsig

  var=' '

  open(luin,file=trim(ifname),form='unformatted')
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

  type(berror_vars) vr
  logical, intent(in) :: m2c
  integer  nlon,nlat,nsig

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

   type(berror_vars) vars
   integer j,nsig,nlat,iret

   nlat=vars%nlat 
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

  end subroutine berror_write_grads_
  subroutine final_berror_vars_(vr)
  type(berror_vars) vr
  deallocate(vr%tcon)
  deallocate(vr%sfvar,vr%vpvar,vr%tvar,vr%qvar,vr%qivar,vr%qlvar,vr%qsvar,vr%qrvar,vr%cvar,vr%nrhvar,vr%sfhln,&
             vr%vphln,vr%thln,vr%qhln,vr%qihln,vr%qlhln,vr%qrhln,vr%qshln,vr%chln,vr%sfvln,vr%vpvln,vr%tvln,&
             vr%qvln,vr%qivln,vr%qlvln,vr%qrvln,vr%qsvln,vr%cvln,vr%vpcon,vr%pscon,vr%varsst,vr%corlsst, &
             vr%ozvar,vr%ozhln,vr%ozvln)
  deallocate(vr%psvar,vr%pshln)
  end subroutine final_berror_vars_
  subroutine copy_berror_vars_(ivars,ovars)
  type(berror_vars) ivars
  type(berror_vars) ovars

  logical wrtall

  wrtall=.true.
  if (ovars%nlon/=ivars%nlon .or. &
      ovars%nlat/=ivars%nlat      ) then
      print*, 'copy_berror_vars_: Trying to copy inconsistent vectors, aborting ...'
      call exit(1)
  endif
  if ( ovars%nsig/=ivars%nsig ) then
     wrtall=.false.
  endif

  if (wrtall) then
     ovars%tcon    = ivars%tcon
     ovars%vpcon   = ivars%vpcon
     ovars%pscon   = ivars%pscon
     ovars%sfvar   = ivars%sfvar
     ovars%sfhln   = ivars%sfhln
     ovars%sfvln   = ivars%sfvln
     ovars%vpvar   = ivars%vpvar
     ovars%vphln   = ivars%vphln
     ovars%vpvln   = ivars%vpvln
     ovars%tvar    = ivars%tvar
     ovars%thln    = ivars%thln
     ovars%tvln    = ivars%tvln
     ovars%qvar    = ivars%qvar
     ovars%nrhvar  = ivars%nrhvar
     ovars%qhln    = ivars%qhln
     ovars%qvln    = ivars%qvln
     ovars%qivar   = ivars%qivar
     ovars%qihln   = ivars%qihln
     ovars%qivln   = ivars%qivln
     ovars%qlvar   = ivars%qlvar
     ovars%qlhln   = ivars%qlhln
     ovars%qlvln   = ivars%qlvln
     ovars%qrvar   = ivars%qrvar
     ovars%qrhln   = ivars%qrhln
     ovars%qrvln   = ivars%qrvln
     ovars%qsvar   = ivars%qsvar
     ovars%qshln   = ivars%qshln
     ovars%qsvln   = ivars%qsvln
     ovars%ozvar   = ivars%ozvar
     ovars%ozhln   = ivars%ozhln
     ovars%ozvln   = ivars%ozvln
     ovars%cvar    = ivars%cvar
     ovars%chln    = ivars%chln
     ovars%cvln    = ivars%cvln
  endif

  ovars%psvar   = ivars%psvar
  ovars%pshln   = ivars%pshln
  ovars%varsst  = ivars%varsst
  ovars%corlsst = ivars%corlsst

  end subroutine copy_berror_vars_

  subroutine vinterp_berror_vars_(ivars,ovars)

  use m_spline, only: spline
  use m_set_eta, only: set_eta
  use m_set_eta, only: get_ref_plevs
  use m_const, only: pstd
  implicit none

  type(berror_vars) ivars
  type(berror_vars) ovars

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
     ! interpolation. It shuld be done as in 
     !             Bnew = T Bold T', 
     ! where T is the interpolation matrix and T' its transpose.
     ! No all interpolants will preserve covariance properties. 
     aux=0.0
     do k=1,ivars%nsig
        call spline( plevi, plevo, ivars%tcon(j,k,:), aux(k,:) )
     enddo
!    the following should really be the adjoint of vinterp ...
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


  end subroutine vinterp_berror_vars_

  subroutine be_write_nc_(ivars)

  use m_set_eta, only: set_eta
  use m_set_eta, only: get_ref_plevs
  implicit none

  type(berror_vars), intent(in) :: ivars

  real(4),allocatable,dimension(:,:) :: aux
  real(4),allocatable,dimension(:) :: plevs
  real(4),allocatable,dimension(:) :: ak,bk
  real(4) ptop, pint
  integer k,ks

  allocate(plevs(ivars%nsig))
  allocate(ak(ivars%nsig+1),bk(ivars%nsig+1))
  call set_eta ( ivars%nsig, ks, ptop, pint, ak, bk )
  call get_ref_plevs ( ak, bk, ptop, plevs )
  plevs = plevs(ivars%nsig:1:-1) ! reorient GEOS-5 levs to be consistent w/ GSI(Berror)

  call write_nc_berror('try.nc',ivars,plevs)

  deallocate(ak,bk)
  deallocate(plevs)

  end subroutine be_write_nc_

  end program write_berror_global
