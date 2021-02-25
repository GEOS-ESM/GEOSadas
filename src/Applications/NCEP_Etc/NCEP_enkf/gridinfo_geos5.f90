module gridinfo
!$$$  module documentation block
!
! module: gridinfo                     read horizontal (lons, lats) and 
!                                      vertical (pressure) information from
!                                      ensemble mean first guess file.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: This module reads gfg_YYYYMMDDHH_fhr06_ensmean, and
! extracts information about the analysis grid, including the 
! longitudes and latitudes of the analysis grid points and
! the pressure on each grid point/vertical level.
!
! Public Subroutines:
!   getgridinfo: read latitudes, longitudes, pressures and orography for analysis grid,
!    broadcast to each task. Compute spherical cartesian coordinate values
!    for each analysis horizontal grid point.
!   gridinfo_cleanup: deallocate allocated module variables. 
!
! Public Variables:
!   npts: number of analysis grid points in the horizontal (from module params).
!   nlevs: number of analysis vertical levels (from module params).
!   ntrac: number of 'tracer' model state variables (3 for GFS,
!    specific humidity, ozone and cloud condensate).
!   nvars: number of 'non-tracer' model state variables (usually 4 
!    for hydrostatic models).  See readgriddata in gridio for a description
!    of how these variables must be laid out in input/output files.
!   ptop: (real scalar) pressure (hPa) at top model layer interface.
!   lonsgrd(npts): real array of analysis grid longitudes (radians).
!   latsgrd(npts): real array of analysis grid latitudes (radians).
!   logp(npts,ndim):  -log(press) for all 2d analysis grids.
!   gridloc(3,npts): spherical cartesian coordinates (x,y,z) for analysis grid.
!   
! Modules Used: mpisetup, params, kinds
!
! program history log:
!   2009-02-23  Initial version.
!
! attributes:
!   language: f95
!
!$$$

use mpisetup
use params, only: datapath,nsfcvars,nlevs,nvars,ndim,datestring,&
                  nlons,nlats,readin_localization,reducedgrid,&
                  expid,fso_flag,gdatestring
use kinds, only: r_kind, i_kind, r_double
use constants, only: one,zero,pi,cp,rd,grav,rearth
use reducedgrid_mod, only: reducedgrid_init, regtoreduced, reducedtoreg,&
                           nptsred, lonsred, latsred
use m_chars, only: lowercase
implicit none
private
public :: getgridinfo, gridinfo_cleanup
public :: ncep2gmao, gmao2ncep
integer(i_kind),public :: nlevs_pres
integer(i_kind),public, allocatable,dimension(:):: index_pres
real(r_single),public :: ptop
real(r_single),public, allocatable, dimension(:) :: aodfreq
real(r_single),public, allocatable, dimension(:) :: lonsgrd, latsgrd
real(r_single),public, allocatable, dimension(:,:) :: logp, gridloc
integer(i_kind),public, allocatable,dimension(:) :: id_u, id_v, id_t, id_q
integer(i_kind),public :: id_ps
integer,public :: npts
integer,public :: nvarhumid ! spec hum is the nvarhumid'th var
integer,public :: nvarozone ! ozone is the nvarozone'th var
real, parameter:: PPMV2GpG = 1.6571E-6 ! from ppmv to g/g
real, parameter:: mbar_per_Pa = 0.01
integer(i_kind), public :: ntrunc
real(r_single),  public :: logaodeps
logical,public :: fso_inc_test
character(len=32), public :: anatype4fso
character(len=32), public :: vertype4fso
character(len=*), parameter :: defRCfile = 'enkf.nml' ! default RC filename
contains

subroutine getgridinfo()
! read latitudes, longitudes and pressures for analysis grid,
! broadcast to each task.
use m_dyn, only: dyn_vect
use m_dyn, only: dyn_get
use m_dyn, only: dyn_stat
use m_dyn, only: dyn_clean
implicit none

character(len=*), parameter :: myname ="gridinfo"
integer(i_kind) nlevsin, ierr, nvar, k, nn, i, j
character(len=120) filename
integer(i_kind) nymd,nhms,freq,nstep
real(r_kind), allocatable, dimension(:,:) :: pressimn,presslmn
real(r_kind) kap,kapr,kap1
type(dyn_vect) x_m
character(len=14) geosdate

kap = rd/cp
kapr = cp/rd
kap1 = kap + one

! initialize any GMAO-specific setting
! ------------------------------------
call init_(defRCfile,nproc,0,ierr)
 if(ierr/=0) then
   print *, myname, ' Failed reading RC file '
   call stop2(999)
 endif

nlevs_pres=nlevs+1
nvarhumid=0 ! should not be wired, but is
nvarozone=0 ! should not be wired, but is
if (nvars>=4) nvarhumid=4 ! should not be wired, but is
if (nvars>=5) nvarozone=5 ! should not be wired, but is
if (nproc .eq. 0) then
   ! get pressure, lat/lon information from ensemble mean file.
   allocate(presslmn(nlons*nlats,nlevs))
   allocate(pressimn(nlons*nlats,nlevs+1))
! get pressure from ensemble mean,
! distribute to all processors.
   read(datestring,'(i8,i2)') nymd, nhms; nhms=100*nhms ! nymd hhmn
   write(geosdate,'(i8.8,a,i4.4,a)') nymd, '_', nhms, 'z'
   nhms = nhms * 100 ! hhmnss
   if (fso_flag) then
      filename = trim(adjustl(datapath))//"ensmean/"//trim(expid)//"."//trim(anatype4fso)//".eta."//geosdate//".nc4"
   else
      filename = trim(adjustl(datapath))//"ensmean/"//trim(expid)//".bkg.eta."//geosdate//".nc4"
   endif
   call dyn_get ( filename, nymd, nhms, x_m, ierr, timidx=0, freq=freq, &
                  nstep=nstep, vectype=5 )
   if (ierr /= 0) then
      print *,'error reading file in gridinfo',trim(filename)
      call stop2(99)
   end if
! convert GMAO units to NCEP units 
!  call dyn_stat (6,x_m,rc=ierr)
   call gmao2ncep(x_m) 

   nlevsin = x_m%grid%km
   if (nlevs .ne. nlevsin) then
     print *,'warning: reading input file in gridinfo - nlevs != ',nlevsin,nlevs
!    call stop2(99)
   end if
   if (reducedgrid) then
      print *,'error, reduced-grid option not yet ready'
      call stop2(99)
      npts = nptsred
   else
      npts = nlons*nlats
   end if
   allocate(latsgrd(npts),lonsgrd(npts))
   allocate(logp(npts,nlevs_pres)) ! log(ens mean first guess press) on mid-layers
   allocate(gridloc(3,npts))
   ptop = x_m%grid%ak(nlevs+1)
   print *,'top interface pressure (mb) = ',ptop
   !==> pressure at interfaces.
   if (reducedgrid) then
      lonsgrd(:) = lonsred(:)
      latsgrd(:) = latsred(:)
   else
      nn = 0
      do j=1,nlats
      do i=1,nlons
         nn = nn + 1
         lonsgrd(nn) = pi*x_m%grid%lon(i)/180._r_kind
         latsgrd(nn) = pi*x_m%grid%lat(j)/180._r_kind
      enddo
      enddo
   endif
   do k=1,nlevs+1
      nn=0
      do j=1,nlats
         do i=1,nlons
            nn=nn+1
            pressimn(nn,k) = x_m%grid%ak(k)+x_m%grid%bk(k)*x_m%ps(i,j)
         enddo
      enddo
   enddo
   do k=1,nlevs
     ! layer pressure from Phillips vertical interpolation.
     presslmn(:,k) = ((pressimn(:,k)**kap1-pressimn(:,k+1)**kap1)/&
                (kap1*(pressimn(:,k)-pressimn(:,k+1))))**kapr
   end do
   print *,'ensemble mean first guess surface pressure:'
   print *,minval(x_m%ps),maxval(x_m%ps)
   !do k=1,nlevs
   !   print *,'min/max ens mean press level',&
   !   k,'=',minval(presslmn(:,k)),maxval(presslmn(:,k))
   !   print *,'min/max ens mean press interface',&
   !   k,'=',minval(pressimn(:,k)),maxval(pressimn(:,k))
   !enddo
   ! logp holds log(pressure) or pseudo-height on grid, for each level/variable.
   do k=1,nlevs
      ! all variables to be updated are on mid-layers, not layer interfaces.
      if (reducedgrid) then
         call regtoreduced(presslmn(:,k),logp(:,k))
         logp(:,k) = -log(logp(:,k))
      else
         logp(:,k) = -log(presslmn(:,k))
      endif
   end do
   if (reducedgrid) then
!_RT  call regtoreduced(reshape(x_m%ps,(/x_m%grid%im*x_m%grid%jm/)),logp(:,nlevs_pres))
      logp(:,nlevs_pres) = -log(logp(:,nlevs_pres))
   else
      logp(:,nlevs_pres) = -log(reshape(x_m%ps,(/x_m%grid%im*x_m%grid%jm/)))
   endif
   call dyn_clean(x_m)
   deallocate(presslmn,pressimn)
end if
call mpi_bcast(npts,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
if (nproc .ne. 0) then
   ! allocate arrays on other (non-root) tasks
   allocate(latsgrd(npts),lonsgrd(npts))
   allocate(logp(npts,nlevs_pres)) ! log(ens mean first guess press) on mid-layers
   allocate(gridloc(3,npts))
   ! initialize reducedgrid_mod on other tasks.
   if (reducedgrid) then
!_RT      call init_spec_vars(nlons,nlats,ntrunc,4)
!_RT      call reducedgrid_init(nlons,nlats,asin(gaulats))
   end if
endif
call mpi_bcast(logp,npts*nlevs_pres,MPI_REAL,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(lonsgrd,npts,MPI_REAL,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(latsgrd,npts,MPI_REAL,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(ptop,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)

allocate(index_pres(ndim))
nn=0
do nvar=1,nvars
  do k=1,nlevs
    nn = nn + 1
    index_pres(nn)=k
  end do
end do
do nvar=1,nsfcvars
   nn=nn+1
   index_pres(nn)=nlevs+1
end do
if(nn/=ndim) then
  print *,'error: inconsistent dims, nn,ndim ',nn,ndim
  call stop2(99)
endif
 
!==> precompute cartesian coords of analysis grid points.
do nn=1,npts
   gridloc(1,nn) = cos(latsgrd(nn))*cos(lonsgrd(nn))
   gridloc(2,nn) = cos(latsgrd(nn))*sin(lonsgrd(nn))
   gridloc(3,nn) = sin(latsgrd(nn))
end do

! Index of each elements
allocate(id_u(nlevs))
allocate(id_v(nlevs))
allocate(id_t(nlevs))
allocate(id_q(nlevs))
do k=1,nlevs
   id_u(k) = k
   id_v(k) = nlevs + k
   id_t(k) = 2*nlevs + k
   id_q(k) = 3*nlevs + k
end do
id_ps = nvars*nlevs + nsfcvars

! read in vertical profile of horizontal and vertical localization length
! scales, set values for each ob.
if (readin_localization) call read_locinfo()

contains
  subroutine init_(rcfile,myID,ROOT,stat)
  use m_stdio, only: stdout, stderr
  use m_inpak90
  use m_die, only: mp_die
  implicit none
  character(len=*), intent(in) :: rcfile
  integer, intent(in)  :: myID, ROOT
  integer, intent(out) :: stat
 
  character(len=*), parameter :: myname_=myname//':init_'
  character(len=256) :: token
  integer ic,naodfreq
  integer iret

  stat=0

! load resource file:
! -------------------
  call i90_loadf (trim(rcfile), iret)
     if( iret .ne. 0) then
          write(stderr,'(2a,i5)') myname_,': I90_loadf error, iret =',iret
          stat = 2 ! missing RC file
          return
      end if
      if(myID==ROOT) then
         write(stdout,'( a  )') '---------------------------------'
         write(stdout,'(2a  )') myname_, ': Reading resource file'
         write(stdout,'( a,/)') '---------------------------------'
      end if

      ntrunc=-1
      call I90_label('spectral_truncation:', iret)
      if (iret .ne. 0) then
         write(stderr,'(2a,i5)') myname_, ': I90_label(1) warning, iret =',iret
      else
        ntrunc = I90_GInt(iret)
        if( iret .ne. 0) then
           write(stderr,'(3a,i5)') myname_,': I90_GInt(1) warning, ', ' iret =',iret
        end if
      endif
      if(myID==ROOT) write(stdout,'(2a,i7)') myname_,':: Spectral truncation: ',ntrunc

      logaodeps=-1.0_r_single
      call I90_label('eps_for_log_transform_aod:', iret)
      if (iret .ne. 0) then
         write(stderr,'(2a,i5)') myname_, ': I90_label(2) warning, iret =',iret
      else
        logaodeps = I90_GFloat(iret)
        if( iret .ne. 0) then
           write(stderr,'(3a,i5)') myname_,': I90_GInt(2) warning, ', ' iret =',iret
        end if
      endif
      if(myID==ROOT) write(stdout,'(2a,f7.2)') myname_,':: EPS for Log(AOD+EPS) transform: ',logaodeps

      fso_inc_test=.false.
      call I90_label('fso_ana_increment_test:', iret)
      if (iret .ne. 0) then
         write(stderr,'(2a,i5)') myname_, ': I90_label(3) warning, iret =',iret
      else
        call I90_Gtoken ( token, iret )
        if( iret .ne. 0) then
           write(stderr,'(3a,i5)') myname_,': I90_Gtoken(3) warning, ', ' iret =',iret
        else
           if(lowercase(trim(token))=='yes') fso_inc_test = .true.
        end if
      endif
      if (fso_inc_test) then
         if(myID==ROOT) write(stdout,'(3a)') myname_,':: performing ana increment test: ',lowercase(trim(token))
      endif

      anatype4fso='niana'
      call I90_label('analysis_type4fso:', iret)
      if (iret .ne. 0) then
         write(stderr,'(2a,i5)') myname_, ': I90_label(4) warning, iret =',iret
      else
        call I90_Gtoken ( token, iret )
        if( iret .ne. 0) then
           write(stderr,'(3a,i5)') myname_,': I90_Gtoken(4) warning, ', ' iret =',iret
        else
           anatype4fso = trim(token)
        end if
      endif
      if(myID==ROOT) write(stdout,'(3a)') myname_,':: ana type used for EFSO: ',trim(anatype4fso)

      vertype4fso='niana'
      call I90_label('verification_type4fso:', iret)
      if (iret .ne. 0) then
         write(stderr,'(2a,i5)') myname_, ': I90_label(5) warning, iret =',iret
      else
        call I90_Gtoken ( token, iret )
        if( iret .ne. 0) then
           write(stderr,'(3a,i5)') myname_,': I90_Gtoken(5) warning, ', ' iret =',iret
        else
           vertype4fso = trim(token)
        end if
      endif
      if(myID==ROOT) write(stdout,'(3a)') myname_,':: verification type used for EFSO: ',trim(vertype4fso)

      ! first ... figure out number of AOD frequencies to handle
      ! --------------------------------------------------------
      call I90_label('aod_frequencies_to_analyze:', iret)
      naodfreq=0
      if (iret .eq. 0) then
          do while ( iret==0 )
             call I90_Gtoken ( token, iret )
             if ( iret==0 ) then
                  naodfreq = naodfreq + 1
             endif
          enddo
      endif
      ! if so, read in AOD frequencies to analyze
      ! -----------------------------------------
      if (naodfreq>0) then
         allocate(aodfreq(naodfreq))
         ic=0
         call I90_label('aod_frequencies_to_analyze:', iret)
         if (iret .eq. 0) then
             do while ( iret==0 )
                call I90_Gtoken ( token, iret )
                if ( iret==0 ) then
                     ic = ic + 1
                     if (ic>naodfreq) then
                         call mp_die(myname_,'Alloc(aod-freq...)',99)
                     else
                        read(token,*) aodfreq(ic)
                        if(myID==ROOT) write(stdout,'(3a)') myname_,':: will analyze AOD freq: ',aodfreq(ic)
                     endif
                endif
             enddo
         endif
      endif


! release resource file:
! ---------------------
  call I90_release()

  end subroutine init_
end subroutine getgridinfo

subroutine gridinfo_cleanup()
if (allocated(lonsgrd)) deallocate(lonsgrd)
if (allocated(latsgrd)) deallocate(latsgrd)
if (allocated(logp)) deallocate(logp)
if (allocated(gridloc)) deallocate(gridloc)
if (allocated(index_pres)) deallocate(index_pres)
if (allocated(id_u)) deallocate(id_u)
if (allocated(id_v)) deallocate(id_v)
if (allocated(id_t)) deallocate(id_t)
if (allocated(id_q)) deallocate(id_q)
end subroutine gridinfo_cleanup

subroutine gmao2ncep (x,scaleit)
  use m_dyn, only: dyn_vect
  use m_dyn, only: dyn_flip
  implicit none
  type(dyn_vect) x
  logical,optional,intent(in) :: scaleit 
  logical scaleit_

  scaleit_=.true.
  if(present(scaleit)) then
     scaleit_=scaleit
  endif
  !==> input ps in Pa - convert to hPa(mb)
  x%grid%ak = x%grid%ak * mbar_per_Pa
  if (scaleit_) then
     x%ps         = x%ps         * mbar_per_Pa
     x%delp       = x%delp       * mbar_per_Pa
     x%q(:,:,:,2) = x%q(:,:,:,2) * PPMV2GpG
  endif
  ! need flip so localization function applies equaly to EnKF and Hybrid-GSI
  call dyn_flip(x)
  ! flip N-S ?
end subroutine gmao2ncep

subroutine ncep2gmao (x)
  use m_dyn, only: dyn_vect
  use m_dyn, only: dyn_flip
  implicit none
  type(dyn_vect) x

  !==> input ps in mbar - convert to Pa
  x%grid%ak    = x%grid%ak    / mbar_per_Pa
  x%ps         = x%ps         / mbar_per_Pa
  x%delp       = x%delp       / mbar_per_Pa
  x%q(:,:,:,2) = x%q(:,:,:,2) / PPMV2GpG
  ! need flip so localization function applies equaly to EnKF and Hybrid-GSI
  call dyn_flip(x)
  ! flip N-S ?
end subroutine ncep2gmao

end module gridinfo
