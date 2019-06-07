module readaerobs
! module: readaerobs
!
!abstract: read aerosol data from  files.
! AOD data
!
!program history log:
! 2015-10 Buchard Initial version

use kinds, only: r_double,r_single,i_kind,r_kind
use params, only: expid,nhr_anal,nbackgrounds
use m_odsmeta, only: ktAOD,ktLogAOD
use m_tick, only: tick
use m_ods
use m_die
use Chem_RegistryMod
use Chem_BundleMod

integer,parameter :: my_nobs=-1
logical,parameter :: showjob=.false.

contains

subroutine get_num_aerobs(obspath,datestring,num_obs_tot,id)
    implicit none
    character (len=120), intent(in) :: obspath
    character (len=10), intent(in) :: datestring
    character(len=8), intent(in) :: id
    integer(i_kind), intent(out) :: num_obs_tot
    character(len=120) obsfile
    character(len=20) :: ftype

    character(len=*), parameter:: myname='get_num_aerobs'

    character(10) :: obstype
    character(12) :: geosdate
    character(14) :: geosdate2
    integer(i_kind) iunit, nhms, nymd, nymd0, nhms0, nsel, nb, rc
  
    type(ods_vect):: ods

! For now in the ods file, only observations data (AOD) after buddy check are available. 

   read(datestring,'(i8,i2)') nymd0, nhms0
   nhms0 = nhms0 * 10000
   call tick (nymd0,nhms0,-6*3600)

   num_obs_tot=0; nb=1
   do while (nhr_anal(nb) > 0)
      nymd=nymd0;nhms=nhms0
      call tick (nymd,nhms,nhr_anal(nb)*3600)
      write(geosdate ,'(i8.8,a,i2.2,a)') nymd, '_', nhms/10000, 'z'
      write(geosdate2,'(i8.8,a,i4.4,a)') nymd, '_', nhms/100, 'z'

      obsfile = trim(adjustl(obspath))//"ensmean/"//trim(expid)//".ensmean_aod.obs."//geosdate//".ods"
      print *,'[r] reading ensmean aerosol file: ', trim(obsfile)

      call ods_get (obsfile, nymd, nhms, ftype, ods, rc)
      if ( rc /= 0 ) then
           num_obs_tot = 0
      end if

      call ODS_Select ( ods, ods%data%nobs, nsel, rc, &
                        kt=ktAOD, qcexcl=0 )
  
      if (ANY(ods%data%kt /= ktAOD)) then
         call die(myname, 'expecting only log(AOD) in observation files')
      endif
      if (my_nobs>0) then
        num_obs_tot = my_nobs
      else
        num_obs_tot = num_obs_tot + nsel
     endif

     call ODS_Clean(ods,rc)  

     nb=nb+1
   end do
   write(6,'(a,2(1x,i7))') 'AOD observation count (total,qc-cleared): ', ods%data%nobs, num_obs_tot
    

end subroutine get_num_aerobs



subroutine get_aerobs_data(obspath, datestring, nobs_max,h_x_ensmean, h_xnobc, x_obs, x_err, &
           x_lon, x_lat, x_press, x_time, x_code, x_errorig, x_type,id, id2)

  implicit none
  character(len=120), intent(in) :: obspath
  character(len=10), intent(in)  :: datestring
  character(len=10), intent(in)  :: id2   ! member number format mem001 for ex
  character(len=10), intent(in)  :: id    ! m

  integer, intent(in) :: nobs_max
  
  real(r_single), dimension(nobs_max), intent(out) :: h_x_ensmean
  real(r_single), dimension(nobs_max), intent(out) :: h_xnobc
  real(r_single), dimension(nobs_max), intent(out) :: x_obs
  real(r_single), dimension(nobs_max), intent(out) :: x_err
  real(r_single), dimension(nobs_max), intent(out) :: x_lon
  real(r_single), dimension(nobs_max), intent(out) :: x_lat
  real(r_single), dimension(nobs_max), intent(out) :: x_press
  real(r_single), dimension(nobs_max), intent(out) :: x_time
  real(r_single), dimension(nobs_max), intent(out) :: x_errorig

  integer(i_kind), dimension(nobs_max), intent(out) :: x_code
  character(len=20), dimension(nobs_max), intent(out) ::  x_type

  character(len=*), parameter:: myname='get_aerobs_data'

  type(ods_vect):: mods ! mean
  type(ods_vect):: eods ! member e
  type(ods_vect),allocatable:: odsm(:)
  type(ods_vect),allocatable:: odse(:)
  type(ods_vect):: odsx         ! auxiliar

  logical :: twofiles

  real(r_single), allocatable, dimension(:) :: press, errorig
  real,parameter :: eps=0.01
  integer, allocatable,dimension(:) :: code
  character, allocatable, dimension(:) :: obstype
  integer :: nhms, nymd, nymd0, nhms0, rc, nobs_aer, nsel, nb                         
 
  character(len=120) :: fname
  character(len=120) :: obsfile
  character(len=12)  :: geosdate 
  character(len=14)  :: geosdate2
  character(len=3) :: obtype
  character(len=20) :: ftype

  real,allocatable,dimension(:):: job
   
  twofiles = id /= id2

  allocate(odsm(nbackgrounds))
  allocate(odse(nbackgrounds))

  read(datestring,'(i8,i2)') nymd0, nhms0
  nhms0 = nhms0 * 10000
  call tick (nymd0,nhms0,-6*3600)

  nobs_aer=0; nb=1
  do while (nhr_anal(nb) > 0)
     nymd=nymd0;nhms=nhms0
     call tick (nymd,nhms,nhr_anal(nb)*3600)
     write(geosdate ,'(i8.8,a,i2.2,a)') nymd, '_', nhms/10000, 'z'
     write(geosdate2,'(i8.8,a,i4.4,a)') nymd, '_', nhms/100, 'z'

!    Read ods file containing AOD observations after buddy check and omf for each member and ensmean
!    -----------------------------------------------------------------------------------------------
     obsfile = trim(adjustl(obspath))//"ensmean/"//trim(expid)//".ensmean_aod.obs."//geosdate//".ods"
     if(trim(id2)=="mem001") print *,'[r] reading omf aerosol file:',id, obsfile

     call ods_get (obsfile, nymd, nhms, ftype, odsx, rc)
     if ( rc /= 0 ) then
            call die(myname, 'could not read ODS aerosol files')
     end if
     call ODS_Select ( odsx, odsx%data%nobs, nsel, rc, &
                       odss=odsm(nb), &
                       kt=ktAOD, qcexcl=0 )
!    odsm(nb)%data%time = nhms/10000!+odsm(nb)%data%time/60
     call ods_clean (odsx,rc)
     if (ANY(odsm(nb)%data%kt /= ktAOD)) then
        call die(myname, 'expecting only AOD in observation files')
     endif
   
     if(twofiles) then

       obsfile = trim(adjustl(obspath))//trim(id2)//"/"//"aer_omf.ext_Nc."//geosdate//".ods"
       print *,'[r] reading omf aerosol file twofile:', id2, obsfile

       call ods_get (obsfile, nymd, nhms, ftype, odsx, rc)
       if ( rc /= 0 ) then
          call die(myname, 'could not read ODS aerosol files')
       end if   
       call ODS_Select ( odsx, odsx%data%nobs, nsel, rc, &
                         odss=odse(nb), &
                         kt=ktAOD, qcexcl=0 )
!      odse(nb)%data%time = nhms/10000!+odse(nb)%data%time/60
       call ods_clean (odsx,rc)
       if (ANY(odse(nb)%data%kt /= ktAOD)) then
          call die(myname, 'expecting only log(AOD) in observation files')
       endif
     endif  

     ! Consistency check
     ! ------------------
     if(twofiles) then
       if (odsm(nb)%data%nobs /= odse(nb)%data%nobs) then
          print*, '[w], warning: the 2 ods files do not have same number of obs:', odsm(nb)%data%nobs, odse(nb)%data%nobs
          call die(myname, 'Aborting ...')
       endif
       if (ANY(odsm(nb)%data%qcexcl/=odse(nb)%data%qcexcl) ) then
          call die(myname, ': ensmean and members have inconsistent QC excl marks, aborting ...')
       endif
     endif  

     nobs_aer = nobs_aer + odsm(nb)%data%nobs 
     if(my_nobs>0) nobs_aer=my_nobs
     nb = nb + 1
  end do ! nb

  if ( nobs_aer /= nobs_max ) then
      call die(myname, 'inconsistent aerosol obs count, aborting ...')
  endif 

  call ODS_Merge ( odsm, nbackgrounds, mods, rc )
  do nb=nbackgrounds,1,-1
     call ODS_Clean ( odsm(nb), rc )
  enddo
  deallocate(odsm)
  if(twofiles) then
     call ODS_Merge ( odse, nbackgrounds, eods, rc )
     do nb=nbackgrounds,1,-1
        call ODS_Clean ( odse(nb), rc )
     enddo
     deallocate(odse)
  endif

  allocate(press(nobs_aer), errorig(nobs_aer), obstype(nobs_aer), code(nobs_aer))
  code   (1:nobs_aer) = 1_i_kind
  press  (1:nobs_aer) = 1.e15_r_single ! total column quantities require oblev to be undef
  errorig(1:nobs_aer) = 0.20_r_single

! These are log of AOD ...
  h_x_ensmean = log(max(eps,eps+mods%data%obs-mods%data%omf))
  x_obs       = log(max(eps,eps+mods%data%obs))
  if(twofiles) then 
    h_xnobc = log(max(eps,eps+eods%data%obs-eods%data%omf))
    if (showjob) then
       allocate(job(nobs_aer))
       job = ( log(max(eps,eps+eods%data%obs))-log(max(eps,eps+eods%data%obs-eods%data%omf)) )/errorig(1:nobs_aer)
       job = job*job
       print *, 'Member Jo(omb)/n = ', sum(job)/nobs_aer
       deallocate(job)
    endif
  else
    h_xnobc = h_x_ensmean
  endif

  ! output needed  
  ! --------
  x_type = 'aod'
  x_code = code
  x_lat  = mods%data%lat(1:nobs_aer)
  x_lon  = mods%data%lon(1:nobs_aer)
  x_time = mods%data%time(1:nobs_aer)/60 ! hours relative to analysis time
  x_press= press
!  if (( mods%data%kx == 301).AND.(mods%data%kx == 302 )) then      ! modo modl   ! is there ods%data%index ????
!       x_errorig(i:) = 0.18        
!  else if (( mods%data%kx == 311).AND.(mods%data%kx == 312 )) then ! mydo mydl
!       x_errorig(:) = 0.20
!  else if ( mods%data%kx == 313 ) then ! misr
!       x_errorig(:) = 0.15
!  else if ( mods%data%kx == 323 ) then ! aeronet
!       x_errorig(:) = 0.15
!  endif 
  x_errorig = errorig*errorig
  x_err     = errorig*errorig

!  301  MODO  MODIS/TERRA-Ocean
!  302  MODL  MODIS/TERRA-Land
!  311  MYDO  MODIS/AQUA-Ocean
!  312  MYDL  MODIS/AQUA-Land
!  313  MISR  MISR
!  323  ANET  AERONET  

  if (showjob) then
     allocate(job(nobs_aer))
     job = ( log(max(eps,eps+mods%data%obs))-log(max(eps,eps+mods%data%obs-mods%data%omf)) )/errorig(1:nobs_aer)
     job = job*job
     print *, 'Mean Jo(omb)/n = ', sum(job)/nobs_aer
     deallocate(job)
  endif
  
  call ODS_Clean(mods,rc)
  if(twofiles) then
     call ODS_Clean(eods,rc)
  endif
  deallocate(obstype, code, press, errorig)
  
end subroutine get_aerobs_data

end module readaerobs
