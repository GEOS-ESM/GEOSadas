 module gridio
!$$$  module documentation block
!
! module: gridio                     subroutines for reading and writing
!                                    ensemble members files using
!                                    EnKF internal format.  A separate
!                                    program must be run before and
!                                    after the EnKF analysis to convert
!                                    to and from the native model format.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: I/O for ensemble member files.
!
! Public Functions:
!  readgriddata, writegriddata
!
! this version reads and writes NCEP GFS sigma files.
!
! Public Variables: None
!
! Modules Used: constants (must be pre-initialized).
!
! program history log:
!   2009-02-23  Todling  Initial version.
!   2014-04-11  Todling  Add ts.
!   2014-04-14  Todling  Replace (possible) var from QCTOT to qi/ql
!   2015-11-12  MJ Kim   Bug fix in (analysis) update to ql/qi/qr/qs
!   2015-11-13  Todling  genqsat to genqsat1 (absurd conding!)
!   2016-02-28  Todling  knobs to handle multiple time levels (not active yet)
!   2017-11-20  MJ Kim   Bounds on hydro-meteors to preserve positiveness
!
! attributes:
!   language: f95
!
!$$$
 use constants, only: zero,one,cp,rd,grav,half
 use params, only: nlons,nlats,ndim,reducedgrid,nvars,nlevs,pseudo_rh, &
                   cliptracers,datestring,datapath,nsfcvars,&
                   nhr_anal,nbackgrounds,expid,evalft,fso_flag,&
                   fso_have_ferr,fso_have_fsen
 use kinds, only: i_kind,r_double,r_kind,r_single
 use gridinfo, only: npts, ncep2gmao, gmao2ncep
 use gridinfo, only: nvarhumid,nvarozone
 use gridinfo, only: fso_inc_test
 use gridinfo, only: anatype4fso,vertype4fso
 use mpisetup
 use m_dyn, only: dyn_vect
 use m_dyn, only: dyn_get
 use m_dyn, only: dyn_put
 use m_dyn, only: dyn_clean
 use m_tick, only: tick
 use m_const, only: tref=>tstd
 use m_const, only: pref=>pstd
 use m_const, only: hvap=>alhl
 use params,  only: wmoist
 implicit none
 private
 public :: readgriddata, writegriddata
 public :: get_weight, destroy_weight, divide_weight

 integer(i_kind) :: freq
 integer(i_kind) :: nstep
 integer(i_kind), parameter :: prec=0 ! 0=32 bit file; 1=64 bit file

 real(r_kind), allocatable, dimension(:,:), save :: weight
 real(r_kind), allocatable, dimension(:), save :: grweight

 contains

 subroutine readgriddata(nanal,ft,mode,grdin,qsat,infilename)
  implicit none

  integer, intent(in) :: nanal                                         !  ensemble member counter
  integer, intent(in) :: ft
  integer, intent(in) :: mode
  real(r_single), dimension(npts,nvars*nlevs+nsfcvars,nbackgrounds), intent(out) :: grdin  !  all fields of interest lined up
                                                                       !    ps must be last
  real(r_double), dimension(npts,nlevs,nbackgrounds), intent(out), optional :: qsat !  saturation q
  character(len=*), intent(in), optional :: infilename

  character(len=*), parameter:: myname_='readgriddata'
  character(len=120) :: filename
  character(len=3) charnanal
  character(len=14) geosdate, geosdate0, fcstdate

  real(r_single) kap,kapr,kap1
  real(r_kind) cptr,qweight,rdtrpr

  real(r_single), dimension(nlons*nlats,nlevs+1) :: pressi
  real(r_single), dimension(npts,nlevs) :: pslg
  real(r_single), dimension(nlons*nlats) :: ug,vg

  type(dyn_vect) :: x_b
  integer(i_kind) nlevsin,nsfc,nb
  integer(i_kind) k,nt,iunitsig,iret
  integer(i_kind) fnymd(2),fnhms(2) ! intial/final date/time of forecast
  integer(i_kind) nymd0,nhms0       ! initial date/time of background
  integer(i_kind) anymd,anhms       ! analysis date/time
  integer(i_kind) nymd,nhms         ! current time
  logical ice
  logical scaleit
  logical weighit
 
  if(ndim/=nvars*nlevs+nsfcvars) then
    print *,'incorrect dim for grdin, aborting ... '
    call stop2(99)
  endif
  scaleit=.true.
  weighit=.false.

! Tick clock to initial time of background/forecast
! -------------------------------------------------
  read(datestring,'(i8,i2)') nymd0, nhms0; nhms0 = 100 * nhms0 ! nhms0 has hhmn, w/ mn=00
  write(geosdate0,'(i8.8,a,i4.4,a)') nymd0, '_', nhms0, 'z'
  nhms0 = nhms0 * 100
  anymd = nymd0; anhms = nhms0 
  call tick (nymd0,nhms0,-6*3600)

! When applicable, Get initial time of forecast
! ---------------------------------------------
  if (fso_flag) then
     fnymd(2)=anymd;fnhms(2)=anhms
     call tick (fnymd(2),fnhms(2),evalft*3600)
     fnymd(1)=fnymd(2);fnhms(1)=fnhms(2)
     if (nanal==0) then
        call tick (fnymd(1),fnhms(1),-(ft+3)*3600)
     else
!       call tick (fnymd(1),fnhms(1),-(evalft+3)*3600)
        call tick (fnymd(1),fnhms(1),-(ft+3)*3600)
     endif
  endif


  ice = .false. ! calculate qsat w/resp to ice?
  kap = rd/cp
  kapr = cp/rd
  kap1 = kap+one

! Loop over number of background time slots
! -----------------------------------------
  nb = 1
  do while (nhr_anal(nb) > 0)
  nymd=nymd0;nhms=nhms0
  call tick (nymd,nhms,nhr_anal(nb)*3600)

  if (fso_flag) then
     weighit=.true.
     if(fso_have_ferr .or. fso_have_fsen) weighit=.false.
!    fnymd(2)=nymd;fnhms(2)=nhms
!    call tick (fnymd(2),fnhms(2),evalft*3600)
     write(fcstdate,'(i8.8,a,i4.4,a)') fnymd(1), '_', fnhms(1)/100, 'z'
     write(geosdate,'(i8.8,a,i4.4,a)') fnymd(2), '_', fnhms(2)/100, 'z'
     if (nanal==0) then
        if (ft==0) then ! read initial non-inflated analysis 
           if(present(infilename)) then
              if (mode==1) then
                 if (fso_inc_test) then
                    read(datestring,'(i8,i2)') nymd, nhms; nhms = 100*nhms
                    write(geosdate,'(i8.8,a,i4.4,a)') nymd, '_', nhms, 'z'
                    filename = trim(adjustl(datapath))//"ensmean"//"/"//trim(expid)//".bkg.eta."//geosdate//".nc4"
                 else
                    nymd=fnymd(2);nhms=fnhms(2)
                    write(geosdate,'(i8.8,a,i4.4,a)') nymd, '_', nhms/100, 'z'
                    filename = trim(adjustl(datapath))//"prog/fcsterr"//"/"//trim(expid)//"."//trim(vertype4fso)//".eta."//geosdate//".nc4"
                 endif
              endif
              if (mode==0) then
                 write(geosdate,'(i8.8,a,i4.4,a)') nymd, '_', nhms/100, 'z'
                 filename = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//"."//trim(anatype4fso)//".eta."//geosdate//".nc4"
              endif
           else
              write(geosdate,'(i8.8,a,i4.4,a)') nymd, '_', nhms/100, 'z'
              filename = trim(adjustl(datapath))//"ensmean"//"/"//trim(expid)//"."//trim(anatype4fso)//".eta."//geosdate//".nc4"
           endif
        else            ! otherwise read forecast 
           if (fso_have_ferr) then
              filename = trim(adjustl(datapath))//"prog/fcsterr/Jgradf_twe.eta.nc4"
              scaleit=.false.
              nymd=fnymd(2); nhms=fnhms(2) ! hack for now - only works for 3d
           else if (fso_have_fsen) then
              read(datestring,'(i8,i2)') nymd, nhms; nhms = 100*nhms ! hack for now - only works for 3d
              write(geosdate,'(i8.8,a,i4.4,a)') nymd, '_', nhms, 'z'
              filename = trim(adjustl(datapath))//"prog/fcsterr/fsens.eta."//geosdate//".nc4"
              scaleit=.false.
           else
              if (fso_inc_test) then
                 read(datestring,'(i8,i2)') nymd, nhms; nhms = 100*nhms
                 write(geosdate,'(i8.8,a,i4.4,a)') nymd, '_', nhms, 'z'
                 filename = trim(adjustl(datapath))//"ensmean"//"/"//trim(expid)//"."//trim(anatype4fso)//".eta."//geosdate//".nc4"
              else
                 filename = trim(adjustl(datapath))//"prog/fcsterr"//"/"//trim(expid)//".prog.eta."//fcstdate//"+"//geosdate//".nc4"
                 nymd=fnymd(2); nhms=fnhms(2) ! hack for now - only works for 3d
              endif
           endif
        endif
     else
        if((.not.fso_have_ferr).and.(.not.fso_have_fsen)) then
           weighit=.not.present(qsat)
        endif
        write(charnanal,'(i3.3)') nanal ! ensemble member identifier
        if (fso_inc_test.or.fso_have_fsen) then
            read(datestring,'(i8,i2)') nymd, nhms; nhms = 100*nhms
            write(geosdate,'(i8.8,a,i4.4,a)') nymd, '_', nhms, 'z'
            filename = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//"."//trim(anatype4fso)//".eta."//geosdate//".nc4"
        else
           filename = trim(adjustl(datapath))//"prog/"//fcstdate//"/mem"//charnanal//"/"//trim(expid)//".prog.eta."//geosdate//".nc4"
           nymd=fnymd(2); nhms=fnhms(2) ! hack for now - only works for 3d
        endif
     endif
  else
     write(geosdate,'(i8.8,a,i4.4,a)') nymd, '_', nhms/100, 'z'
     write(charnanal,'(i3.3)') nanal ! ensemble member identifier
     if(present(infilename)) then
        filename = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//"."//trim(anatype4fso)//".eta."//geosdate//".nc4"
     else
        filename = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".bkg.eta."//geosdate//".nc4"
     endif
  endif
  print *, trim(filename)

  call dyn_get ( filename, nymd, nhms, x_b, iret, timidx=0, freq=freq, nstep=nstep, vectype=5 )
  if (iret /= 0) then
     print *,'error reading file in gridio ',trim(filename)
     call stop2(99)
  end if

  nlevsin = x_b%grid%km
  if (nlevs /= nlevsin) then
    print *,'error, inconsistent input file - nlevs != ',nlevsin
    call stop2(99)
  end if
  if (npts /= x_b%grid%im*x_b%grid%jm) then ! original code more general than this
    print *,'error, inconsistent input file - im*jm != ',x_b%grid%im*x_b%grid%jm
    call stop2(99)
  end if

  if (weighit) then
     cptr = sqrt(cp/tref)
     if(wmoist<tiny(one)) then
        qweight = zero
     else
        qweight = sqrt(wmoist/(cp*tref))*hvap
     endif
     rdtrpr = sqrt(rd*tref)/pref
  endif

! convert GMAO units to NCEP units
  call gmao2ncep (x_b,scaleit=scaleit)

!_$omp parallel do private(k,nt) shared(grdin,nlevs,nvars,npts)
  do k=1,nlevs
     grdin(:,        k,nb) = reshape(x_b%u   (:,:,k),(/npts/))
     grdin(:,  nlevs+k,nb) = reshape(x_b%v   (:,:,k),(/npts/))
     grdin(:,2*nlevs+k,nb) = reshape(x_b%pt  (:,:,k),(/npts/))
     if(weighit) then
        grdin(:,        k,nb) =        weight(:,k) * grdin(:,        k,nb)
        grdin(:,  nlevs+k,nb) =        weight(:,k) * grdin(:,  nlevs+k,nb)
        grdin(:,2*nlevs+k,nb) = cptr * weight(:,k) * grdin(:,2*nlevs+k,nb)
     endif
     if (nvars .gt. 3) then
         nt=1 ! first sphu (wired)
         if(nvarhumid/=4)then
            print *,'error, humidy slot inconsistent with wired-in value'
            call stop2(99)
         endif
         grdin(:,(2+nt)*nlevs+k,nb) = reshape(x_b%q(:,:,k,nt),(/npts/))
         if (weighit) then
            grdin(:,(2+nt)*nlevs+k,nb) = qweight * weight(:,k) * grdin(:,(2+nt)*nlevs+k,nb)
         endif
     endif
     if (nvars .gt. 4) then
         nt=2 ! next ozone (wired)
         if(nvarozone/=5)then
            print *,'error, ozone slot inconsistent with wired-in value'
            call stop2(99)
         endif
         if (.not.present(qsat)) then
            grdin(:,(2+nt)*nlevs+k,nb) = zero
         else
            grdin(:,(2+nt)*nlevs+k,nb) = reshape(x_b%q(:,:,k,nt),(/npts/))
         endif
     endif
     if (nvars .gt. 6) then
         if (.not.present(qsat)) then
            nt=3 ! next qitot (wired)
            grdin(:,(2+nt)*nlevs+k,nb) = zero
            nt=4 ! next qltot (wired)
            grdin(:,(2+nt)*nlevs+k,nb) = zero
         else
            nt=3 ! next qitot (wired)
            grdin(:,(2+nt)*nlevs+k,nb) = reshape(x_b%q(:,:,k,nt),(/npts/))
            nt=4 ! next qltot (wired)
            grdin(:,(2+nt)*nlevs+k,nb) = reshape(x_b%q(:,:,k,nt),(/npts/))
         endif
     endif
     if (nvars .gt. 8) then
         if (.not.present(qsat)) then
            nt=5 ! next qrtot (wired)
            grdin(:,(2+nt)*nlevs+k,nb) = zero
            nt=6 ! next qstot (wired)
            grdin(:,(2+nt)*nlevs+k,nb) = zero
         else
            nt=5 ! next qrtot (wired)
            grdin(:,(2+nt)*nlevs+k,nb) = reshape(x_b%q(:,:,k,nt),(/npts/))
            nt=6 ! next qstot (wired)
            grdin(:,(2+nt)*nlevs+k,nb) = reshape(x_b%q(:,:,k,nt),(/npts/))
         endif
     endif
  enddo
!_$omp end parallel do
  ! surface pressure is last grid.
  nsfc=nsfcvars
  grdin(:,nvars*nlevs+nsfc,nb) = reshape(x_b%ps,(/npts/))
  if (weighit) then
     grdin(:,nvars*nlevs+nsfc,nb) = rdtrpr * grweight(:) * grdin(:,nvars*nlevs+nsfc,nb)
  endif
  ! skin temperature is next ...
  if(nsfcvars>1) then
    nsfc=nsfc-1
    if (.not.present(qsat)) then
       grdin(:,nvars*nlevs+nsfc,nb) = zero
    else
       grdin(:,nvars*nlevs+nsfc,nb) = reshape(x_b%ts,(/npts/))
    endif
  endif

  if (scaleit) then
     !==> pressure at interfaces.
     do k=1,nlevs+1 ! _RT: we have x_b%delp so we can do better than this
        pressi(:,k)=x_b%grid%ak(k)+x_b%grid%bk(k)*grdin(:,nvars*nlevs+nsfcvars,nb)
     enddo
     ! compute saturation q.
     do k=1,nlevs
       ! layer pressure from phillips vertical interolation
       ug(:) = ((pressi(:,k)**kap1-pressi(:,k+1)**kap1)/&
               (kap1*(pressi(:,k)-pressi(:,k+1))))**kapr
       pslg(:,k) = ug
     end do
     if (present(qsat)) then
        if (nvarhumid/=0.and.pseudo_rh) then
           call genqsat1(grdin(:,3*nlevs+1:4*nlevs,nb),qsat(:,:,nb),pslg,grdin(:,2*nlevs+1:3*nlevs,nb),ice,npts,nlevs)
        else
           qsat(:,:,nb) = 1._r_double
        end if
     endif
  endif ! have_ferr
  call dyn_clean ( x_b )

  nb = nb + 1
  enddo ! loop over backgrounds

 end subroutine readgriddata

 subroutine writegriddata(nanal,grdin,no_inflate_flag)
  implicit none

  integer, intent(in) :: nanal
  real(r_single), dimension(npts,nvars*nlevs+nsfcvars,nbackgrounds), intent(inout) :: grdin
  logical,intent(in) :: no_inflate_flag

  character(len=*), parameter:: myname_='writegriddata'
  type (dyn_vect) :: x_a
  character(len=3) charnanal
  character(len=14) geosdate
  character(len=120):: filename
  real(r_single) kap,kapr,kap1,clip
  integer nymd0,nhms0
  integer nymd,nhms
  integer im,jm,k,nt,iret,kk,nsfc,nb

! if (.not. constants_initialized) then
!     print *,'constants not initialized (with init_constants, init_constants_derived)'
!     call stop2(23)
! end if

  kapr = cp/rd
  kap = rd/cp
  kap1 = kap+one

! Tick clock to initial time of background/forecast
! -------------------------------------------------
  read(datestring,'(i8,i2)') nymd0, nhms0
  nhms0 = nhms0 * 10000
  call tick (nymd0,nhms0,-6*3600)

! Loop over number of background time slots
! -----------------------------------------
  nb = 1
  do while (nhr_anal(nb) > 0)
  nymd=nymd0;nhms=nhms0
  call tick (nymd,nhms,nhr_anal(nb)*3600)

  write(geosdate,'(i8.8,a,i4.4,a)') nymd, '_', nhms/100, 'z'
  write(charnanal,'(i3.3)') nanal ! ensemble member identifier
  filename = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".bkg.eta."//geosdate//".nc4"
! filename = trim(adjustl(datapath))//"sfg_"//datestring//"_fhr06_mem"//charnanal

  ! read first-guess fields in array that will end up holding analysis
  call dyn_get ( filename, nymd, nhms, x_a, iret, timidx=0, vectype=5 )
  if (iret/=0) then
     print *,'error(2) reading file in gridio ',trim(filename)
     call stop2(99)
  end if
  im = x_a%grid%im
  jm = x_a%grid%jm

  ! convert background to NCEP units
  call gmao2ncep(x_a)

  ! add increments to background field
!_$omp parallel do private(k,nt,ug,vg) shared(sigdata,grdin,nlevs,nvars,reducedgrid) 
  do k=1,nlevs
     x_a%u(:,:,k)    = x_a%u(:,:,k)  + reshape(grdin(:,k,nb),(/im,jm/))
     x_a%v(:,:,k)    = x_a%v(:,:,k)  + reshape(grdin(:,nlevs+k,nb),(/im,jm/))
     x_a%pt(:,:,k)   = x_a%pt(:,:,k) + reshape(grdin(:,2*nlevs+k,nb),(/im,jm/))
     if (nvars>3) then ! sphu
        nt=1
        x_a%q(:,:,k,nt) = x_a%q(:,:,k,nt) + reshape(grdin(:,(2+nt)*nlevs+k,nb),(/im,jm/))
     endif
     if (nvars>4) then
        nt=2
        x_a%q(:,:,k,nt) = x_a%q(:,:,k,nt) + reshape(grdin(:,(2+nt)*nlevs+k,nb),(/im,jm/))
     endif
     if (nvars>6) then ! qitot & qltot
        nt=3
        x_a%q(:,:,k,nt) = x_a%q(:,:,k,nt) + reshape(grdin(:,(2+nt)*nlevs+k,nb),(/im,jm/))
        x_a%q(:,:,k,nt) = max(zero,x_a%q(:,:,k,nt))
        nt=4
        x_a%q(:,:,k,nt) = x_a%q(:,:,k,nt) + reshape(grdin(:,(2+nt)*nlevs+k,nb),(/im,jm/))
        x_a%q(:,:,k,nt) = max(zero,x_a%q(:,:,k,nt))
     endif
     if (nvars>8) then ! qrtot & qstot
        nt=5
        x_a%q(:,:,k,nt) = x_a%q(:,:,k,nt) + reshape(grdin(:,(2+nt)*nlevs+k,nb),(/im,jm/))
        x_a%q(:,:,k,nt) = max(zero,x_a%q(:,:,k,nt))
        nt=6
        x_a%q(:,:,k,nt) = x_a%q(:,:,k,nt) + reshape(grdin(:,(2+nt)*nlevs+k,nb),(/im,jm/))
        x_a%q(:,:,k,nt) = max(zero,x_a%q(:,:,k,nt))
     endif
  enddo
  nsfc=nsfcvars
  x_a%ps = x_a%ps + reshape(grdin(:,nvars*nlevs+nsfc,nb),(/im,jm/))
  if(nsfcvars>1) then
     nsfc=nsfc-1 
     x_a%ts = x_a%ts + reshape(grdin(:,nvars*nlevs+nsfc,nb),(/im,jm/))
  endif

! convert analysis to GMAO units
  call ncep2gmao(x_a)

! construct delp (assume eta-coordinates)
  do k=1,nlevs
     x_a%delp(:,:,k)=(x_a%grid%ak(k+1)-x_a%grid%ak(k)) +  &
                     (x_a%grid%bk(k+1)-x_a%grid%bk(k))*x_a%ps(:,:)
  end do

  ! clip tracers.
! if (cliptracers .and. nvars .gt. 3) then
!    clip = tiny(vg(1))
!_$omp parallel do private(k,nt,vg) shared(sigdata,nlevs,nvars,clip) 
!    do k=1,nlevs
!    do nt=1,nvars-3
!       call sptez_s(sigdata%q(:,k,nt),vg,1)
!       where (vg < clip) vg = clip
!       call sptez_s(sigdata%q(:,k,nt),vg,-1)
!    enddo
!    enddo
!_$omp end parallel do
!  end if

  if (no_inflate_flag) then
     filename = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".niana.eta."//geosdate//".mem"//charnanal//".nc4"
  else
     filename = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".ana.eta."//geosdate//".mem"//charnanal//".nc4"
  endif
  print*, myname_, ': output file name =', trim(filename)
! x_a%grid%lm = nvars-1 !  why do I need this? Something is messed in m_dyn
  call dyn_put ( filename, nymd, nhms, prec, x_a, iret, &
                 freq=freq, nstep=nstep, vectype=5 )
  if (iret/=0) then
       call dyn_clean ( x_a )
       write(6,*) trim(myname_), ': cannot write final analysis file'
       call stop2(99)
  endif   

! clean up
  call dyn_clean ( x_a )
 
 nb = nb + 1
 enddo  ! loop over backgrounds

 end subroutine writegriddata

 ! --------------------------------!
 ! get_weight:
 ! EFSO routine to assign weight   !
 ! 3d grid points based on latitude!
 ! and pressure level difference   !
 ! --------------------------------!
 subroutine get_weight
  use gridinfo, only: latsgrd
  implicit none
  character(len=500) :: filename
  real(r_kind), allocatable, dimension(:,:) :: pressi
  real(r_kind), allocatable, dimension(:) :: tmpgrd
  real(r_kind) :: sumcoslat

  type(dyn_vect) x_m
  integer(i_kind) nlevsin
  integer(i_kind) i,j,k,iunitsig,iret
  integer(i_kind) nymd,nhms
  character(len=14) geosdate

! if(fso_have_ferr.or.fso_have_fsen) return

  allocate(weight(npts,nlevs))
  allocate(grweight(npts))
  ! Read analysis data
  read(datestring,'(i8,i2)') nymd, nhms
  write(geosdate,'(i8.8,a,i4.4,a)') nymd, '_', nhms*100, 'z'
  filename = trim(adjustl(datapath))//"ensmean"//"/"//trim(expid)//"."//trim(anatype4fso)//".eta."//geosdate//".nc4"
  call dyn_get ( filename, nymd, nhms, x_m, iret, timidx=0, freq=freq, nstep=nstep, vectype=5 )
  if (iret /= 0) then
     print *,'error reading file in gridio ',trim(filename)
     call stop2(23)
  end if
  nlevsin = x_m%grid%km
  if (nlevs /= nlevsin) then
    print *,'error reading input file - nlevs != ',nlevsin
  end if
  call gmao2ncep (x_m)

  ! calculate weight on the grid point
  sumcoslat = zero
  do i=1,nlons*nlats
     grweight(i) = abs(cos(latsgrd(i)))
     sumcoslat = sumcoslat + grweight(i)
  end do
! where(grweight<zero) grweight=tiny(one) ! fix for roundoff
  sumcoslat = 1.0_r_kind / sumcoslat
  grweight(:) = sqrt(grweight(:)*sumcoslat)
  ! calculate half level pressures
  !==> pressure at interfaces.
  allocate(pressi(npts,nlevs+1))
  allocate(tmpgrd(npts))
  tmpgrd(:) = reshape(x_m%ps,(/npts/))
  do k=1,nlevs+1
     pressi(:,k)=x_m%grid%ak(k)+x_m%grid%bk(k)*tmpgrd
  end do
!$omp parallel do private(k) shared(weight,pressi,grweight,nlevs)
  do k=1,nlevs
     ! sqrt(dp)*sqrt(area)
     weight(:,k)=sqrt((pressi(:,k)-pressi(:,k+1))/tmpgrd(:))*grweight(:)
  end do
!$omp end parallel do
! clean up
  deallocate(tmpgrd)
  deallocate(pressi)
  call dyn_clean ( x_m )
  return
 end subroutine get_weight

 ! EFSO routine to destroy impact weight arrays
 subroutine destroy_weight
  implicit none
! if(fso_have_ferr.or.fso_have_fsen) return
  if(allocated(weight)) deallocate(weight)
  if(allocated(grweight)) deallocate(grweight)
 end subroutine destroy_weight

 ! EFSO routine to account for mass
 ! when specifying advection
 ! for localization 
 subroutine divide_weight(grdin)
  use mpisetup, only: nproc
  use loadbal, only: npts_max,indxproc,numptsperproc
  implicit none
  real(r_single), dimension(npts_max,ndim), intent(inout) :: grdin
  real(r_kind) cptr,qweight,rdtrpr
  integer(i_kind) :: k,npt

  if(fso_have_ferr.or.fso_have_fsen) return

  cptr = real(sqrt(tref/cp),r_kind)
  if(wmoist<tiny(one)) then
     qweight = zero
  else
     qweight = real(sqrt(cp*tref/wmoist)/hvap,r_kind)
  endif
  rdtrpr = real(sqrt(pref/(rd*tref)),r_kind)
  do npt=1,numptsperproc(nproc+1)
     do k=1,nlevs
        grdin(npt,k) = grdin(npt,k) / weight(indxproc(nproc+1,npt),k)
        grdin(npt,nlevs+k) = grdin(npt,nlevs+k) / weight(indxproc(nproc+1,npt),k)
        grdin(npt,2*nlevs+k) = grdin(npt,2*nlevs+k) * cptr / weight(indxproc(nproc+1,npt),k)
        if (nvars .gt. 3) then
           grdin(npt,3*nlevs+k) = grdin(npt,3*nlevs+k) * qweight / weight(indxproc(nproc+1,npt),k)
        end if
     end do
     grdin(npt,nvars*nlevs+nsfcvars) = grdin(npt,nvars*nlevs+nsfcvars) &
          & * rdtrpr / grweight(indxproc(nproc+1,npt))
  end do
  return
 end subroutine divide_weight

 end module gridio


