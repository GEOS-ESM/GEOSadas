!
! utilities and m_inpak90 supplemental routines to be eventually mergerd.
!


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: i90_LoadRC - Load resources
! 
! !INTERFACE:
!
    subroutine I90_LoadRC ( rcenvar, defname, rcfname, rc )

! !USES:

    use m_inpak90, only: i90_loadf

    IMPLICIT NONE

!
! !INPUT PARAMETERS: 
!
    character(len=*), intent(in) :: rcenvar ! env var for new rc file name
    character(len=*), intent(in) :: defname ! default rc file name

!
! !OUTPUT PARAMETERS: 
!
    character(len=*), intent(out) :: rcfname ! resource file name
    integer,          intent(out) :: rc      ! error code 


! !DESCRIPTION: Determine resource file name and load it.
!
!
! !REVISION HISTORY: 
!
!  12nov1999 da Silva First crack.
!  12sep2016 Todling  Replaced peanut extinctions with total extinction.
!  01oct2016 Todling  Backward-compatible read for extinctions.
!
!EOP
!--------------------------------------------------------------------------

    rcfname = ' '
    call getenv ( rcenvar, rcfname )

    if ( rcfname .eq. ' ' ) rcfname = defname

    call i90_loadf ( rcfname, rc )

    return

  end subroutine I90_LoadRC


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: i90_gtab - read a multi-level parameter table from a file 
! 
! !INTERFACE:
!
   subroutine i90_gtab ( rc_Pars, ParType, ParDesc, mlev, nlev, plev, &
                         mPar, nPar, Pars, istat)

!
! !USES: 

   use m_inpak90
   use m_die,     only: warn
   implicit NONE

! !INPUT PARAMETERS: 
!
   character(len=*), intent(in)	:: rc_Pars	! the resource name
   integer,          intent(in)	:: mlev		! possible level count
   integer,          intent(in)	:: mPar		! max. no. of Pars
!   
!
! !OUTPUT PARAMETERS:
!


  character(len=*), intent(out)	:: ParType	! model type
  character(len=*), intent(out)	:: ParDesc	! model description

  integer, intent(out)		:: nlev		! actual level count
  real,    intent(out)		:: plev(mlev)	! level values
  integer, intent(out)		:: nPar		! actual number of Pars
  real,    intent(out)		:: Pars(mPar,mlev)	! parameters
  integer, intent(out)		:: istat	! input status
!
! !DESCRIPTION: This routine reads a standard resource file table.
!
!
! !TO DO: Update from i77 to i90 API (lablin() -> i90_label, etc)
!
! !REVISION HISTORY: 
!
!  06oct1999  da Silva  First crack based on Guo's rdPars().
!  11mar2017  Todling   Add Nitrates check when handling extinctions.
!
!EOP
!-------------------------------------------------------------------------


!   Local vars.
  integer ios,ln,ll,ipar
  character(len=len(ParType)) snum,slev
  real*8 val
!-----------------------------------------------------------------------
!   Parameters
  character(len=*), parameter	:: myname='i90_gtab'
!_______________________________________________________________________
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  istat=0			! default return if everything OK
  npar=mpar			! most possible number of paras.

!-----------------------------------------------------------------------
!  Look for the parameter table resource
  call lablin(trim(rc_Pars))	! locate the resource block
  call rdnext(ios)	! get the next line
  if(ios.eq.2) then
    ln=max(len_trim(rc_Pars),1)
    call warn(myname, 'table not found, ' // rc_Pars(1:ln) )
    istat=-1
    return	! bad news to the parent
  endif

!-----------------------------------------------------------------------
!   Process the parameter table line by line
  ParType=' '
  nlev=0
  do while(ios.eq.0.and.nlev.lt.mlev)

	! token #1 is either a level (pressure) or the class

    call getwrd(ios,slev)
    if(ios /= 0) then
      ln=max(len_trim(slev),1)
      call warn ( myname, 'unexpected error from getwrd(), ' &
                  // slev(1:ln) )
      istat=1
      return	! bad news to the parent
    endif

	! rest tokens are dependent on the type of the first one

    ll=max(len_trim(slev),1)
    ios=verify(slev(1:ll),'0123456789.')	! a f90 function
    if(ios /= 0) then
		! it can not be a valid level value, therefore,
		! possiblly a correlation class name (ParType)

      if(ParType /= ' ') then	! ParType is already defined
        call warn ( myname, 'unexpected string found, ' &
	            // slev(1:ll) )
	istat=2
	return
      endif

		! the rest is treated as a string

      ParType=slev		! it is a class name
      call getstr(ios,ParDesc)	! get the description
      if(ios /= 0) ParDesc='?'	! ignore error

    else
		! it might be a number, therefore, a pressure level
		! value

      snum=slev

      val=str2rn(snum,ios)
      if(ios /= 0) then		! a format problem
        call warn ( myname, 'level ' // slev(1:ll) //	&
	  ', expected a number but found ' // snum(1:ll) )
	istat=3
	return
      endif

      nlev=nlev+1		! Count as a new level
      plev(nlev)=val

      ipar=0
      call getwrd(ios,snum)
      do while(ios == 0 .and. ipar < npar)

	val=str2rn(snum,ios)
	if(ios /= 0) then	! there is a format problem
	  ln=max(len_trim(snum),1)
          call warn ( myname, 'level ' // slev(1:ll) //	&
	  ', expected a number but found ' // snum(1:ll) )
	  istat=3
	  return
	endif

	ipar=ipar+1
        Pars(ipar,nlev)=val

	call getwrd(ios,snum)

      end do	! while(ios==0 .and. ipar < npar)

	! Verify the line of input

      if(ios == 0) then
		! There is more token than expected (npar)
	ln=max(len_trim(snum),1)
	call warn ( myname, 'level ' //  slev(1:ll) // &
                    ', more tokens than expected, ' // snum(1:ln) )

      else
		! it might be alright, except ...
	if(nlev > 1 .and. ipar < npar) then
		! that means the number of tokens is not the same as
		! the first line.
	  ln=max(len_trim(snum),1)
  	  call warn ( myname, 'level ' //  slev(1:ll) // &
                    ', fewer tokens than expected, ' // snum(1:ln) )
	  istat=4
	  return
	endif
      endif

      npar=ipar		! the actual number of tokens
    endif

    call rdnext(ios)
  end do	! ios == 0 .and. nlev < mlev
!
		! If the table is not finished, tell the parent.
  if(ios.eq.0) then
    call warn ( myname, 'buffer is full' )
    istat=5
    return
  endif

		! If the table is empty, tell the parent.
!  if(nlev.eq.0) then
!    ln=max(len_trim(rc_Pars),1)
!   call warn ( myname, 'empty table' )
!    istat=6
!    return
!  endif

end subroutine i90_gtab
!.

!..........................................................................

  subroutine vect_stat ( myname, label, f, n )

      use              m_mpout
      implicit         none

      character*(*)    myname, label
      integer          n
      real*8             f(n)

!
!     Prints out min, max values using warn. 
!

      integer          i
      real*8             fmax
      real*8             fmin
      real*8             stdv
      real*8 mean
      real*8 big
      parameter (big = 1.e14)
      integer count
      logical hasmiss 

      character*512 msg

      hasmiss = .false.
      fmax = - big
      fmin = + big
      mean = 0.
      count = 0
      stdv = 0.0
      do i = 1, n
            if( abs(f(i)) .lt. big ) then
                fmax = max(fmax,f(i))
                fmin = min(fmin,f(i))
                mean = mean + f(i)
                stdv = stdv + f(i)**2 
                count = count + 1
            else
                hasmiss = .true.
            endif
      end do

      if( count .ne. 0 ) then 
          mean = mean / count
          stdv = sqrt( stdv/count - mean*mean )
      end if

      if ( count .eq. 0 ) then
           write(msg,9) label//' max/min/mean/stdv = ', &
                        fmax, fmin, mean, stdv, ' X'
      else if ( hasmiss ) then
           write(msg,9) label//' max/min/mean/stdv = ', &
                        fmax, fmin, mean, stdv, ' M'
      else
           write(msg,9) label//' max/min/mean/stdv = ',  &
                        fmax, fmin, mean, stdv 
      endif

  9   format(a30,4(F9.4),a)

!!!      call mpout_log ( myname, trim(msg) )

      write(*,'(a)') trim(myname) // ': ' // trim(msg) 

      return

   end subroutine vect_stat

!.............................................................................................

   subroutine AOD_SumSpecies ( w_f, im, jm, km )
      use Chem_BundleMod
      use Chem_RegistryMod
      use m_die
      implicit NONE
      integer,              intent(in) :: im, jm, km 
      type(Chem_Bundle), intent(inout) :: w_f
!
!     On input, the Chem Bundle w_f is assumed to have AOD for individual species.
!     On putput, w_f is modified so as to have a single tracer: the AOD summed
!     over all species.

!                                              ---

      character(len=*), parameter :: myname = 'AOD_SumSpecies'
      integer :: n, iq, ios, rc
      real*8 :: tau(im,jm,km)

      if ( w_f%reg%nq .eq. 0 ) return ! no tracer, nothing to do

!     Bundle already has total AOD
!     ----------------------------
      if ( (w_f%reg%nq .eq. 1)             .and. &
           w_f%reg%doing_XX ) return

!     Sum AOD over species
!     --------------------
      tau = 0.0
      n = im * jm * km
      if ( w_f%reg%doing_DU ) then
           do iq = w_f%reg%i_DU, w_f%reg%j_DU
              tau = tau + w_f%qa(iq)%data3d
              call vect_stat ( myname, trim(w_f%reg%vname(iq)), tau(:,:,:), n )
           end do
      end if
      if ( w_f%reg%doing_SS ) then
           do iq = w_f%reg%i_SS, w_f%reg%j_SS
              tau = tau + w_f%qa(iq)%data3d
              call vect_stat ( myname, trim(w_f%reg%vname(iq)), tau(:,:,:), n )
           end do
      end if
      if ( w_f%reg%doing_BC ) then
           do iq = w_f%reg%i_BC, w_f%reg%j_BC
              tau = tau + w_f%qa(iq)%data3d
              call vect_stat ( myname, trim(w_f%reg%vname(iq)), tau(:,:,:), n )
           end do
      end if
      if ( w_f%reg%doing_OC ) then
           do iq = w_f%reg%i_OC, w_f%reg%j_OC
              tau = tau + w_f%qa(iq)%data3d
              call vect_stat ( myname, trim(w_f%reg%vname(iq)), tau(:,:,:), n )
           end do
      end if
      if ( w_f%reg%doing_SU ) then
           do iq = w_f%reg%i_SU, w_f%reg%j_SU
              if ( trim(w_f%reg%vname(iq)) .eq. 'SO4' ) then
                 tau = tau + w_f%qa(iq)%data3d
                 call vect_stat ( myname, trim(w_f%reg%vname(iq)), tau(:,:,:), n )
              end if
           end do
      end if
      if ( w_f%reg%doing_NI ) then
           do iq = w_f%reg%i_NI, w_f%reg%j_NI
              if ( trim(w_f%reg%vname(iq)) .eq. 'NO3an1' .or. &
                   trim(w_f%reg%vname(iq)) .eq. 'NO3an2' .or. &
                   trim(w_f%reg%vname(iq)) .eq. 'NO3an3'      &
                 ) then
                 tau = tau + w_f%qa(iq)%data3d
                 call vect_stat ( myname, trim(w_f%reg%vname(iq)), tau(:,:,:), n )
              end if
           end do
      end if

!     Deallocate all tracers but the first
!     ------------------------------------
      do iq = 2, w_f%reg%nq
         deallocate(w_f%qa(iq)%data3d, stat=ios)
         if ( ios .ne. 0 ) call die(myname,'cannot allocate memory for vname, etc.')
      end do

!     First tracer takes the total AOD
!     --------------------------------
      w_f%qa(1)%data3d = tau

!     Update the registry to show only 1 tracer with proper name
!     ----------------------------------------------------------
      call Chem_RegistryDestroy(w_f%reg, rc ) ! clean slate
      if ( rc .ne.0 ) call die(myname,'cannot destroy chem registry')
      allocate ( w_f%reg%vname(1), w_f%reg%vtitle(1), w_f%reg%vunits(1), &
                 w_f%reg%fscav(1), stat=ios )
      if ( ios .ne. 0 ) call die(myname,'cannot allocate memory for vname, etc.')
      w_f%reg%nq = 1
      w_f%reg%doing_XX = .true.
      w_f%reg%n_XX = 1
      w_f%reg%i_XX = 1
      w_f%reg%j_XX = 1
      w_f%reg%vname(1)  = 'AOD'
      w_f%reg%vtitle(1) = 'Total Aerosol Optical Depth'
      w_f%reg%vunits(1) = '1'
      w_f%reg%fscav(1)  = 0.0

    end subroutine AOD_SumSpecies

!.............................................................................................

subroutine AOD_GetDims (filename, im, jm, km, rc)
!
! Return dimensions on GFIO-compatible file.
!
  implicit NONE
  character(len=*), intent(in)  :: filename ! input GFIO compatible file name
  integer,          intent(out) :: im ! number of longitudes on file
  integer,          intent(out) :: jm ! number of latitudes on file
  integer,          intent(out) :: km ! number of levels on file
  integer,          intent(out) :: rc ! return code
!                   ---
  integer, parameter :: READ_ONLY=1
  integer :: fid, tm, nvars, ngatts
  call GFIO_Open ( filename, READ_ONLY, fid, rc )
  if (rc/=0) then
     print *,'cannot open '//trim(filename)
     return
  end if
  call GFIO_DimInquire ( fid, im, jm, km, tm, nvars, ngatts, rc)
  if (rc/=0) then
     print *,'cannot get dimensions of file '//trim(filename)
     return
  end if
  call GFIO_Close ( fid, rc )
  if (rc/=0) then
     print *,'cannot close file '//trim(filename)
     return
  end if
end subroutine AOD_GetDims


!.............................................................................................

subroutine AOD_GetTau ( filename, im, jm, km, nymd, nhms, verbose, w_tau, rc )
!
! Given a Chem Bundle file name with aerosol mixing ratio, returns the aerosol 
! optical depth (tau), at the channels given in the "GAAS_Mie.rc" file. It requires
! that the following resource files be present in the current directory:
!
! GAAS_AerRegistry.rc --- used to decide which variable ro read from 
!                          mixing ratio Chem Bundle
! GAAS_AodRegistry.rc --- Specifies 1 tracer to hold AOD summed over species.
! GAAS_Mie.rc --- used to specify channels, location of Mie tables, etc.
!
  use Chem_MieMod
  use Chem_RegistryMod
  use Chem_BundleMod
  use m_die

  implicit NONE
  character(len=*), intent(in)  :: filename ! Chem bundle with aerosol
                                            ! mixing ratio
  integer,          intent(in)  :: im   ! number of longitudes on file
  integer,          intent(in)  :: jm   ! number of latitudes on file
  integer,          intent(in)  :: km   ! number of levels on file
  integer,          intent(inout)  :: nymd ! date, eg., 20080629
  integer,          intent(inout)  :: nhms ! time, eg., 120000
                                        ! wavelengths [nm] 
  logical,          intent(in)  :: verbose
  type(Chem_Bundle), intent(out) :: w_tau ! aerosol optical depth

  integer,          intent(out) :: rc ! return code

!                               ---

  if ( km > 1 ) then

     call AOD_GetTauMch ( filename, im, jm, km, nymd, nhms, verbose, w_tau, rc )

  else

     call AOD_GetTau1ch ( filename, im, jm, km, nymd, nhms, verbose, w_tau, rc )

  end if

end subroutine AOD_GetTau

subroutine AOD_GetTau1ch ( filename, im, jm, km, nymd, nhms, verbose, w_tau, rc )
!
! Given a GEOS-5 hyperwall style file name with 550nm AOD for each species, returns 
! the total aerosol optical depth (tau), summed over all species. It requires
! that the following resource files be present in the current directory:
!
! GAAS_AodRegistry.rc --- Specifies 1 tracer to hold AOD summed over species.
! GAAS_Mie.rc --- used to specify channels, location of Mie tables, etc.
!                 for consistency only 1 channel (550 nm) is allowed.
!
  use Chem_MieMod
  use Chem_RegistryMod
  use Chem_BundleMod

  implicit NONE
  character(len=*), intent(in)  :: filename ! Chem bundle with aerosol
                                            ! mixing ratio
  integer,          intent(in)  :: im   ! number of longitudes on file
  integer,          intent(in)  :: jm   ! number of latitudes on file
  integer,          intent(in)  :: km   ! number of levels on file
  integer,          intent(inout)  :: nymd ! date, eg., 20080629
  integer,          intent(inout)  :: nhms ! time, eg., 120000
                                        ! wavelengths [nm] 
  logical,          intent(in)  :: verbose
  type(Chem_Bundle), intent(out) :: w_tau ! aerosol optical depth

  integer,          intent(out) :: rc ! return code
!                               ---
  type(Chem_Registry) :: aodReg
  integer             :: n, fid, incSecs
  real                :: tau(im,jm)

  integer :: nch = 1
  real :: channels(1) = (/ 550. /) ! this is hardwired for hyperwall files

  integer, parameter :: NQ = 1
  character(len=9) :: vname(NQ) = (/ 'TOTEXTTAU' /)

  integer, parameter :: NQori = 5
  character(len=8) :: vname_ori(NQori) = (/ 'DUEXTTAU', 'SSEXTTAU', 'SUEXTTAU', &
                                            'BCEXTTAU', 'OCEXTTAU' /)


  integer, parameter  :: READ_ONLY=1

! Open the hyperwall file
! -----------------------
  if (verbose) &
       write(*,*), ' Reading ', trim(filename)
  call GFIO_Open ( filename, READ_ONLY, fid, rc )
  if (rc/=0) then
     print *,'cannot open '//trim(filename)
     return
  end if

  if ( nymd<0 .OR. nhms<0 ) then
     call GetBegDateTime ( fid, nymd, nhms, incSecs, rc )
     if (rc/=0) then
        print *,'cannot get first time in  '//trim(filename)
        return
     end if
  end if

! Load the AOD Chem Registry
! --------------------------
  aodReg = Chem_RegistryCreate(rc,'GAAS_AodRegistry.rc')
  if(rc/=0) then
     print *, 'cannot create AOD Registry'
     return
  else
     if (verbose) &
          call Chem_RegistryPrint(aodReg)
  end if

!  Create output bundle
!  --------------------
   call Chem_BundleCreate ( aodReg, 1, im, 0, im, 1, jm, 0, jm, nch, &
                            w_tau, rc, lev=channels, levUnits="nm")
   if ( rc /= 0 ) then
      print *, 'cannot create AOD Chem_Bundle'
      return
   end if

   if (verbose) &
        write(*,'(a,(10f6.0))'), ' [o] Wavenumbers:', w_tau%grid%lev 

! Initialize output arrays to zero
! --------------------------------
  w_tau%delp = 1.0
  w_tau%rh = 0.0
  w_tau%qa(1)%data3d = 0.0

! read tau for individual species and sum them up
! -----------------------------------------------
  do n = 1, nq
     call GFIO_GetVar ( fid, vname(n), nymd, nhms, im, jm, 0, 1, tau, rc )
     if ( rc == 0 ) then
        if (verbose) &
             print *, '[+] Adding '//trim(vname(n))//' contribution at ', nymd, nhms
        w_tau%qa(1)%data3d(:,:,1) = w_tau%qa(1)%data3d(:,:,1) + tau
     else
        print *, 'warning cannot read hyperwall variable ', vname(n)
        print *, 'will try reading old wired-in specifies ...'
     end if
  end do

! try reading originally wired-in species
! ---------------------------------------
  if ( rc/=0 ) then
     rc=0 ! reset error code
     do n = 1, nqori
        call GFIO_GetVar ( fid, vname_ori(n), nymd, nhms, im, jm, 0, 1, tau, rc )
        if ( rc /= 0 ) then
           print *, 'warning cannot read hyperwall variable ', vname_ori(n)
           return
        end if
        if (verbose) &
             print *, '[+] Adding '//trim(vname(n))//' contribution at ', nymd, nhms
        w_tau%qa(1)%data3d(:,:,1) = w_tau%qa(1)%data3d(:,:,1) + tau
     end do
  endif

! All done
! --------
  call GFIO_Close ( fid, rc )
  if (rc/=0) then
     print *,'cannot close file '//trim(filename)
     return
  end if

end subroutine AOD_GetTau1ch

subroutine AOD_GetTauMch ( filename, im, jm, km, nymd, nhms, verbose, w_tau, rc )
!
! Given a Chem Bundle file name with aerosol mixing ratio, returns the aerosol 
! optical depth (tau), at the channels given in the "GAAS_Mie.rc" file. It requires
! that the following resource files be present in the current directory:
!
! GAAS_AerRegistry.rc --- used to decide which variable ro read from 
!                          mixing ratio Chem Bundle
! GAAS_AodRegistry.rc --- Specifies 1 tracer to hold AOD summed over species.
! GAAS_Mie.rc --- used to specify channels, location of Mie tables, etc.
!
  use Chem_MieMod
  use Chem_RegistryMod
  use Chem_BundleMod

  implicit NONE
  character(len=*), intent(in)  :: filename ! Chem bundle with aerosol
                                            ! mixing ratio
  integer,          intent(in)  :: im   ! number of longitudes on file
  integer,          intent(in)  :: jm   ! number of latitudes on file
  integer,          intent(in)  :: km   ! number of levels on file
  integer,          intent(inout)  :: nymd ! date, eg., 20080629
  integer,          intent(inout)  :: nhms ! time, eg., 120000
                                        ! wavelengths [nm] 
  logical,          intent(in)  :: verbose
  type(Chem_Bundle), intent(out) :: w_tau ! aerosol optical depth

  integer,          intent(out) :: rc ! return code
!                               ---
  type(Chem_Registry) :: chemReg, aodReg
  type(Chem_Mie)      :: mieTables
  type(Chem_Bundle)   :: w_c 
  integer             :: i, j, k, m, n, iq, nymd_, nhms_, idxTable, fid
  real                :: tau_

  integer, parameter  :: READ_ONLY=1

  real, parameter     :: grav = 9.80616


! Load the mixing ratio Chem Registry
! -----------------------------------
  chemReg = Chem_RegistryCreate(rc,'GAAS_AerRegistry.rc')
  if(rc/=0) then
     print *, 'cannot create Chem Registry'
     return
  else
     if (verbose) &
          call Chem_RegistryPrint(chemReg)
  end if

! Load the AOD Chem Registry
! --------------------------
  aodReg = Chem_RegistryCreate(rc,'GAAS_AodRegistry.rc')
  if(rc/=0) then
     print *, 'cannot create AOD Registry'
     return
  else
     if (verbose) &
          call Chem_RegistryPrint(aodReg)
  end if

! Read in Chem Bundle
! -------------------
  if ( nymd<0 ) then ! get last time on file
        call Chem_BundleRead(filename, nymd_, nhms_, w_c, rc, & 
                             ChemReg=chemReg )
        nymd = nymd_
        nhms = nhms_
  else ! get specified date/time
        nymd_ = nymd
        nhms_ = nhms
        call Chem_BundleRead(filename, nymd_, nhms_, w_c, rc, & 
                             ChemReg=chemReg, timidx=0 )
  end if

  if (rc==0) then
     if (verbose) &
         print *, '[r] read Chem Bundle '//trim(filename)
  else
     print *, '[x] cannot read Chem Bundle '//trim(filename)//', rc = ', rc
     return
  end if

! Create the Mie Tables
! ---------------------
  mieTables = Chem_MieCreate('GAAS_Mie.rc',rc,chemReg=chemReg)
  if ( rc /= 0 ) then
     print *, 'Cannot create Mie tables from GAAS_Mie.rc'
     return
  end if

! Check consistency of dimensions
! -------------------------------
  if ( im /= w_c%grid%im .OR. &
       jm /= w_c%grid%jm .OR. &
       km /= w_c%grid%km  ) then
     print *, 'expecting im, jm, km = ', im, jm, km
     print *, 'but found im, jm, km = ', w_c%grid%im, w_c%grid%jm, w_c%grid%km
     rc = 97
     return
  end if

!  Create output bundle
!  --------------------
   call Chem_BundleCreate ( aodReg, 1, im, 0, im, 1, jm, 0, jm, mieTables%nch, &
                            w_tau, rc, lev=1.e9*mieTables%channels, levUnits="nm")
   if ( rc /= 0 ) then
      print *, 'cannot create AOD Chem_Bundle'
      return
   end if

   if (verbose) &
        write(*,'(a,(10f6.0))'), ' [o] Wavenumbers:', w_tau%grid%lev 

! Initialize output arrays to zero
! --------------------------------
  w_tau%qa(1)%data3d = 0.0

! Loop over aerosol species
! -------------------------
  do iq = 1, chemReg%nq

     idxTable = Chem_MieQueryIdx(mieTables,chemReg%vname(iq),rc)
     if(idxTable == -1) cycle
     if ( rc/=0 ) then
        print *, 'cannot get Mie index for '//chemReg%vname(iq)
        return
     end if

     if (verbose) &
          print *, '[+] Adding '//trim(chemReg%vname(iq))//' contribution'

!    Loop over channel, x, y, z
!    --------------------------
     do n = 1, mieTables%nch
        do k = 1, km
           do j = 1, jm
              do i = 1, im

              call Chem_MieQuery(mieTables, idxTable, float(n), &
                         w_c%qa(iq)%data3d(i,j,k)*w_c%delp(i,j,k)/grav, &
                         w_c%rh(i,j,k), tau=tau_ )

              w_tau%qa(1)%data3d(i,j,n) = w_tau%qa(1)%data3d(i,j,n) + tau_

             end do ! longitudes
          end do ! latitudes
       end do ! levels
    end do ! channels

  end do ! aerosol tracers

! Chem bundle with mixing ratio no longer needed
! ----------------------------------------------
  call Chem_BundleDestroy(w_c, rc)
  if ( rc /= 0 ) then
     print *, 'Cannot destroy Chem_Bundle'
     return
  end if

  if (verbose) &
       print *, '[x] All done!'
        
end subroutine AOD_GetTauMch

!.............................................................................................

subroutine AOD_StandardChannels (channels, nobs)
  implicit NONE
  integer, intent(in) :: nobs
  real, intent(inout) :: channels(nobs)

!
!  Adjust channels to standard values; very adhoc for EOS instruments.
!

   where(channels >= 460. .AND. channels <= 480. ) channels = 470.
   where(channels >= 540. .AND. channels <= 560. ) channels = 550. 
   where(channels >= 650. .AND. channels <= 680. ) channels = 660. 
   where(channels >= 860. .AND. channels <= 880. ) channels = 870. 


end subroutine AOD_StandardChannels

!.............................................................................................

subroutine fix_odsmeta(ods)
  use m_ODS
  implicit NONE
  type(ods_vect), intent(inout) :: ods
  
!
! Note: kt/kx lists are hardwired for now. Better solution
!       is to have this code fragmented created from kx/kt lists
!       on the fly during build.
!
  ods%meta%kt_names(1) = 'Surface (10m) zonal wind'
  ods%meta%kt_names(2) = 'Surface (10m) meridional wind'
  ods%meta%kt_names(3) = 'Sea level pressure'
  ods%meta%kt_names(4) = 'Upper-air zonal wind'
  ods%meta%kt_names(5) = 'Upper-air meridional wind'
  ods%meta%kt_names(6) = 'Upper-air geopotential height'
  ods%meta%kt_names(7) = 'Upper-air water vapor mixing'
  ods%meta%kt_names(8) = 'Upper-air temperature'
  ods%meta%kt_names(9) = 'Upper-air dew-point temperature'
  ods%meta%kt_names(10) = 'Upper-air relative humidity'
  ods%meta%kt_names(11) = 'Upper-air specific humidity'
  ods%meta%kt_names(12) = 'Surface (10m) wind speed'
  ods%meta%kt_names(13) = 'Surface (10m) temperature Kelvin'
  ods%meta%kt_names(14) = 'Surface (10m) dew-point temperature'
  ods%meta%kt_names(15) = 'Surface (10m) relative humidity'
  ods%meta%kt_names(16) = 'Surface (10m) specific humidity'
  ods%meta%kt_names(17) = 'Precipitation rate'
  ods%meta%kt_names(18) = 'Total precipitable water'
  ods%meta%kt_names(19) = 'Total cloud liquid water'
  ods%meta%kt_names(20) = 'Fractional cloud cover'
  ods%meta%kt_names(21) = 'Column ozone'
  ods%meta%kt_names(22) = 'Ozone layer'
  ods%meta%kt_names(23) = 'Upper-air thickness'
  ods%meta%kt_names(24) = 'Surface (2m) wind speed'
  ods%meta%kt_names(25) = 'Surface (2m) maximum wind'
  ods%meta%kt_names(26) = 'Surface (2m) maximum wind'
  ods%meta%kt_names(27) = 'Surface (2m) temperature'
  ods%meta%kt_names(28) = 'Surface (2m) maximum temperature'
  ods%meta%kt_names(29) = 'Surface (2m) minimum temperature'
  ods%meta%kt_names(30) = 'Surface (2m) dew-point temperature'
  ods%meta%kt_names(31) = 'Surface (2m) relative humidity'
  ods%meta%kt_names(32) = 'Surface (2m) specific humidity'
  ods%meta%kt_names(33) = 'Surface (2m) pressure'
  ods%meta%kt_names(34) = 'Visibility'
  ods%meta%kt_names(35) = 'Snow depth'
  ods%meta%kt_names(36) = 'Weather condition'
  ods%meta%kt_names(37) = 'Upper-air potential temperature (ps=1000hPa)'
  ods%meta%kt_names(38) = 'Surface skin temperature'
  ods%meta%kt_names(39) = 'Sea Surface temperature'
  ods%meta%kt_names(40) = 'Brightness temperature'
  ods%meta%kt_names(41) = 'Layer-mean virtual temperature'
  ods%meta%kt_names(42) = 'Line-of-Sight Wind (LOS)'
  ods%meta%kt_names(43) = 'Log-transformed AOD'
  ods%meta%kt_names(44) = 'Upper-air virtual temperature'
  ods%meta%kt_names(45) = 'Aerosol Optical Depth'
  ods%meta%kt_names(46) = 'Angstrom Exponent'
  ods%meta%kt_names(47) = 'Surface Albedo'
  ods%meta%kt_names(48) = 'Reflectance'
  ods%meta%kt_names(49) = 'Aerosol Absorption Optical Depth'
  ods%meta%kt_names(50) = 'Single Scattering Albedo'
  ods%meta%kt_names(51) = 'CO mixing ratio'
  ods%meta%kt_names(52) = 'NO2 mixing ratio'
  ods%meta%kt_names(53) = 'unknown'
  ods%meta%kt_names(54) = 'Solar Zenith Angle'
  ods%meta%kt_names(55) = 'Solar Azimuth Angle'
  ods%meta%kt_names(56) = 'Sensor Zenith Angle'
  ods%meta%kt_names(57) = 'Sensor Azimuth Angle'
  ods%meta%kt_names(58) = 'Aerosol Scattering Angle'
  ods%meta%kt_names(59) = 'unknown'
  ods%meta%kt_names(60) = 'Aerosol Type'
  ods%meta%kt_names(61) = 'Fine model Optical thickness'
  ods%meta%kt_names(62) = 'Fine model Angstrom exponent'
  ods%meta%kt_names(63) = 'unknown'
  ods%meta%kt_names(64) = 'unknown'
  ods%meta%kt_names(65) = 'unknown'
  ods%meta%kt_names(66) = 'unknown'
  ods%meta%kt_names(67) = 'unknown'
  ods%meta%kt_names(68) = 'unknown'
  ods%meta%kt_names(69) = 'unknown'
  ods%meta%kt_names(70) = 'Aerosol Optical Depth Ratio'
  ods%meta%kt_names(71) = 'unknown'
  ods%meta%kt_names(72) = 'unknown'
  ods%meta%kt_names(73) = 'unknown'
  ods%meta%kt_names(74) = 'Quality Assurance Flag'
  ods%meta%kt_names(75) = 'unknown'
  ods%meta%kt_names(76) = 'Aerosol Refractive index (Real)'
  ods%meta%kt_names(77) = 'Aerosol Refractive index (Imaginary)'
  ods%meta%kt_names(78) = 'Reflectivity'
  ods%meta%kt_names(79) = 'Height of aerosol layer'
  ods%meta%kt_names(80) = 'Radiance'
  ods%meta%kt_names(81) = 'Aerosol Index'
  ods%meta%kt_names(82) = 'Fire Radiative Power Megawatts'
  ods%meta%kt_names(83) = 'Aerosol Asymmetry Factor'
  ods%meta%kt_names(84) = 'Aerosol Mass Concentration'
  ods%meta%kt_names(85) = 'Aerosol Effective Radius'
  ods%meta%kt_names(86) = 'GSI precip rate ln(1+rain_rate)'
  ods%meta%kt_names(87) = 'Ozone mixing ratio'

  ods%meta%kx_names(1) = 'Surface Land Obs - 1  '
  ods%meta%kx_names(2) = 'Surface Land Obs - 2  '
  ods%meta%kx_names(3) = 'Surface Ship Obs - 1  '
  ods%meta%kx_names(4) = 'Surface Ship Obs - 2  '
  ods%meta%kx_names(5) = 'Environment Buoy     '
  ods%meta%kx_names(6) = 'Drifting Buoy     '
  ods%meta%kx_names(7) = 'Rawinsonde      '
  ods%meta%kx_names(8) = 'Pilot Wind     '
  ods%meta%kx_names(9) = 'Ship Released Rawinsonde    '
  ods%meta%kx_names(10) = 'Dropwinsonde      '
  ods%meta%kx_names(11) = 'Radar-tracked Rawinsonde     '
  ods%meta%kx_names(12) = 'Rocketsonde      '
  ods%meta%kx_names(13) = 'Balloon      '
  ods%meta%kx_names(14) = 'Aircraft - Air/Sat Relay   '
  ods%meta%kx_names(15) = 'Aircraft - Int. Data Sys  '
  ods%meta%kx_names(16) = 'Aircraft Report     '
  ods%meta%kx_names(17) = 'Aircraft Coded Report    '
  ods%meta%kx_names(18) = 'Aircraft - ALPX    '
  ods%meta%kx_names(19) = 'Cloud Track Wind - Wisc E1 '
  ods%meta%kx_names(20) = 'Cloud Track Wind - Wisc E2 '
  ods%meta%kx_names(21) = 'Cloud Track Wind - Wisc W '
  ods%meta%kx_names(22) = 'Cloud Track Wind - Wisc Ocean '
  ods%meta%kx_names(23) = 'Cloud Track Wind - Repr. Jap. '
  ods%meta%kx_names(24) = 'Cloud Track Wind - US1 Picture triplet'
  ods%meta%kx_names(25) = 'Cloud Track Wind - US2 Picture triplet'
  ods%meta%kx_names(26) = 'Cloud Track Wind - European IR automated'
  ods%meta%kx_names(27) = 'Cloud Track Wind - Japanese IR automated'
  ods%meta%kx_names(28) = 'SCATTEROMETER Research Mode    '
  ods%meta%kx_names(29) = 'WSAT 55     '
  ods%meta%kx_names(30) = 'WSAT 57     '
  ods%meta%kx_names(31) = 'Limb Infrared Monitor - Strat  '
  ods%meta%kx_names(32) = 'Aircraft EU experiment    '
  ods%meta%kx_names(33) = 'NESDIS NH Land AM type A '
  ods%meta%kx_names(34) = 'NESDIS SH Land AM type A '
  ods%meta%kx_names(35) = 'NESDIS NH Land AM type B /'
  ods%meta%kx_names(36) = 'NESDIS SH Land AM type B /'
  ods%meta%kx_names(37) = 'NESDIS NH Land AM type C '
  ods%meta%kx_names(38) = 'NESDIS SH Land AM type C '
  ods%meta%kx_names(39) = 'NESDIS NH Ocean AM type A '
  ods%meta%kx_names(40) = 'NESDIS SH Ocean AM type A '
  ods%meta%kx_names(41) = 'NESDIS NH Ocean AM type B /'
  ods%meta%kx_names(42) = 'NESDIS SH Ocean AM type B /'
  ods%meta%kx_names(43) = 'NESDIS NH Ocean AM type C '
  ods%meta%kx_names(44) = 'NESDIS SH Ocean AM type C '
  ods%meta%kx_names(45) = 'NESDIS NH Land PM type A '
  ods%meta%kx_names(46) = 'NESDIS SH Land PM type A '
  ods%meta%kx_names(47) = 'NESDIS NH Land PM type B /'
  ods%meta%kx_names(48) = 'NESDIS SH Land PM type B /'
  ods%meta%kx_names(49) = 'NESDIS NH Land PM type C '
  ods%meta%kx_names(50) = 'NESDIS SH Land PM type C '
  ods%meta%kx_names(51) = 'NESDIS NH Ocean PM type A '
  ods%meta%kx_names(52) = 'NESDIS SH Ocean PM type A '
  ods%meta%kx_names(53) = 'NESDIS NH Ocean PM type B /'
  ods%meta%kx_names(54) = 'NESDIS SH Ocean PM type B /'
  ods%meta%kx_names(55) = 'NESDIS NH Ocean PM type C '
  ods%meta%kx_names(56) = 'NESDIS SH Ocean PM type C '
  ods%meta%kx_names(57) = 'Special Sat NH - Ocn A '
  ods%meta%kx_names(58) = 'Special Sat SH - Ocn A '
  ods%meta%kx_names(59) = 'Special Sat NH - Ocn B '
  ods%meta%kx_names(60) = 'Special Sat SH - Ocn B '
  ods%meta%kx_names(61) = 'Special Sat NH - Ocn C '
  ods%meta%kx_names(62) = 'Special Sat SH - Ocn C '
  ods%meta%kx_names(63) = 'VAS NH Land - type A '
  ods%meta%kx_names(64) = 'VAS SH Land - type A '
  ods%meta%kx_names(65) = 'VAS NH Land - type B '
  ods%meta%kx_names(66) = 'VAS SH Land - type B '
  ods%meta%kx_names(67) = 'VAS NH Ocean - type A '
  ods%meta%kx_names(68) = 'VAS SH Ocean - type A '
  ods%meta%kx_names(69) = 'VAS NH Ocean - type B '
  ods%meta%kx_names(70) = 'VAS SH Ocean - type B '
  ods%meta%kx_names(71) = 'NASA-GLA NH Land type A  '
  ods%meta%kx_names(72) = 'NASA-GLA SH Land type A  '
  ods%meta%kx_names(73) = 'NASA-GLA NH Land type B  '
  ods%meta%kx_names(74) = 'NASA-GLA SH Land type B  '
  ods%meta%kx_names(75) = 'NASA-GLA NH Land type C  '
  ods%meta%kx_names(76) = 'NASA-GLA SH Land type C  '
  ods%meta%kx_names(77) = 'NASA-GLA NH Land type D  '
  ods%meta%kx_names(78) = 'NASA-GLA SH Land type D  '
  ods%meta%kx_names(79) = 'NASA-GLA NH Oceantype A   '
  ods%meta%kx_names(80) = 'NASA-GLA SH Ocean type A  '
  ods%meta%kx_names(81) = 'NASA-GLA NH Ocean type B  '
  ods%meta%kx_names(82) = 'NASA-GLA SH Ocean type B  '
  ods%meta%kx_names(83) = 'NASA-GLA NH Ocean type C  '
  ods%meta%kx_names(84) = 'NASA-GLA SH Ocean type C  '
  ods%meta%kx_names(85) = 'NASA-GLA NH Ocean type D  '
  ods%meta%kx_names(86) = 'NASA-GLA SH Ocean type D  '
  ods%meta%kx_names(87) = 'Pseudo-1000 mb Heights    '
  ods%meta%kx_names(88) = 'ER-2 Aircraft / MMS Data  '
  ods%meta%kx_names(89) = 'Aircraft reports (ACARS)    '
  ods%meta%kx_names(90) = 'Surface METAR     '
  ods%meta%kx_names(91) = 'UARS / MLS 1   '
  ods%meta%kx_names(92) = 'UARS / MLS 2   '
  ods%meta%kx_names(93) = 'DAOTOVS Land AM Type: Clr HIRS/MSU/SSU '
  ods%meta%kx_names(94) = 'DAOTOVS Land AM Type: Clr HIRS/MSU '
  ods%meta%kx_names(95) = 'DAOTOVS Land AM Type: MSU/SSU  '
  ods%meta%kx_names(96) = 'DAOTOVS Land AM Type: MSU  '
  ods%meta%kx_names(97) = 'DAOTOVS Land AM Type: SSU  '
  ods%meta%kx_names(98) = 'DAOTOVS Ocean AM Type: Clr HIRS/MSU/SSU '
  ods%meta%kx_names(99) = 'DAOTOVS Ocean AM Type: Clr HIRS/MSU '
  ods%meta%kx_names(100) = 'DAOTOVS Ocean AM Type: MSU/SSU  '
  ods%meta%kx_names(101) = 'DAOTOVS Ocean AM Type: MSU  '
  ods%meta%kx_names(102) = 'DAOTOVS Ocean AM Type: SSU  '
  ods%meta%kx_names(103) = 'DAOTOVS Ice AM Type: Clr HIRS/MSU/SSU '
  ods%meta%kx_names(104) = 'DAOTOVS Ice AM Type: Clr HIRS/MSU '
  ods%meta%kx_names(105) = 'DAOTOVS Ice AM Type: MSU/SSU  '
  ods%meta%kx_names(106) = 'DAOTOVS Ice AM Type: MSU  '
  ods%meta%kx_names(107) = 'DAOTOVS Ice AM Type: SSU  '
  ods%meta%kx_names(108) = 'DAOTOVS Land AM Type: Cld Clr HIRS/MSU/SSU'
  ods%meta%kx_names(109) = 'DAOTOVS Land AM Type: Cld Clr HIRS/MSU'
  ods%meta%kx_names(110) = 'DAOTOVS Ocean AM Type: Cld Clr HIRS/MSU/SSU'
  ods%meta%kx_names(111) = 'DAOTOVS Ocean AM Type: Cld Clr HIRS/MSU'
  ods%meta%kx_names(112) = 'DAOTOVS Ice AM Type: Cld Clr HIRS/MSU/SSU'
  ods%meta%kx_names(113) = 'DAOTOVS Ice AM Type: Cld Clr HIRS/MSU'
  ods%meta%kx_names(114) = 'Cloud Track Wind - INDIAN OCEAN '
  ods%meta%kx_names(115) = 'Cloud Track Wind - GOES 9 (04/04/97-03/11/98)'
  ods%meta%kx_names(116) = 'SSM/I NESDIS Precipitation    '
  ods%meta%kx_names(117) = 'TOMS      '
  ods%meta%kx_names(118) = 'SBUV/2      '
  ods%meta%kx_names(119) = 'Cloud Track Wind - US1 IR automated'
  ods%meta%kx_names(120) = 'Cloud Track Wind - US1 Water Vapor,Deep-Layer'
  ods%meta%kx_names(121) = 'Cloud Track Wind - US1 Water Vapor,Cloud-Top'
  ods%meta%kx_names(122) = 'Cloud Track Wind - US2 IR automated'
  ods%meta%kx_names(123) = 'Cloud Track Wind - US2 Water Vapor,Deep-Layer'
  ods%meta%kx_names(124) = 'Cloud Track Wind - US2 Water Vapor,Cloud-Top'
  ods%meta%kx_names(125) = 'DAOTOVS Land PM Type: Clr HIRS/MSU/SSU '
  ods%meta%kx_names(126) = 'DAOTOVS Land PM Type: Clr HIRS/MSU '
  ods%meta%kx_names(127) = 'DAOTOVS Land PM Type: MSU/SSU  '
  ods%meta%kx_names(128) = 'DAOTOVS Land PM Type: MSU  '
  ods%meta%kx_names(129) = 'DAOTOVS Land PM Type: SSU  '
  ods%meta%kx_names(130) = 'DAOTOVS Ocean PM Type: Clr HIRS/MSU/SSU '
  ods%meta%kx_names(131) = 'DAOTOVS Ocean PM Type: Clr HIRS/MSU '
  ods%meta%kx_names(132) = 'DAOTOVS Ocean PM Type: MSU/SSU  '
  ods%meta%kx_names(133) = 'DAOTOVS Ocean PM Type: MSU  '
  ods%meta%kx_names(134) = 'DAOTOVS Ocean PM Type: SSU  '
  ods%meta%kx_names(135) = 'DAOTOVS Ice PM Type: Clr HIRS/MSU/SSU '
  ods%meta%kx_names(136) = 'DAOTOVS Ice PM Type: Clr HIRS/MSU '
  ods%meta%kx_names(137) = 'DAOTOVS Ice PM Type: MSU/SSU  '
  ods%meta%kx_names(138) = 'DAOTOVS Ice PM Type: MSU  '
  ods%meta%kx_names(139) = 'DAOTOVS Ice PM Type: SSU  '
  ods%meta%kx_names(140) = 'DAOTOVS Land PM Type: Cld Clr HIRS/MSU/SSU'
  ods%meta%kx_names(141) = 'DAOTOVS Land PM Type: Cld Clr HIRS/MSU'
  ods%meta%kx_names(142) = 'DAOTOVS Ocean PM Type: Cld Clr HIRS/MSU/SSU'
  ods%meta%kx_names(143) = 'DAOTOVS Ocean PM Type: Cld Clr HIRS/MSU'
  ods%meta%kx_names(144) = 'DAOTOVS Ice PM Type: Cld Clr HIRS/MSU/SSU'
  ods%meta%kx_names(145) = 'DAOTOVS Ice PM Type: Cld Clr HIRS/MSU'
  ods%meta%kx_names(146) = 'Cloud Track Wind - US1 visible automated'
  ods%meta%kx_names(147) = 'Cloud Track Wind - US2 visible automated'
  ods%meta%kx_names(148) = 'SSM/I WENTZ Speed only   '
  ods%meta%kx_names(149) = 'SSM/I NESDIS Speed only   '
  ods%meta%kx_names(150) = 'SSM/I WENTZ F8/F14    '
  ods%meta%kx_names(151) = 'SCATTEROMETER ERS1     '
  ods%meta%kx_names(152) = 'SCATTEROMETER ERS2     '
  ods%meta%kx_names(153) = 'SCATTEROMETER NSCAT     '
  ods%meta%kx_names(154) = 'SCATTEROMETER QuikSCAT     '
  ods%meta%kx_names(155) = 'SCATTEROMETER SEAWINDS     '
  ods%meta%kx_names(156) = 'TRMM      '
  ods%meta%kx_names(157) = 'TMI      '
  ods%meta%kx_names(158) = 'GADS(BRITISH AIRWAYS)     '
  ods%meta%kx_names(159) = 'SAT am, WATER, CLFRC<=.10   '
  ods%meta%kx_names(160) = 'SAT am, WATER, CLFRC<=.40   '
  ods%meta%kx_names(161) = 'SAT am, WATER, CLFRC> .40  '
  ods%meta%kx_names(162) = 'SAT am, Other, CLFRC<=.10   '
  ods%meta%kx_names(163) = 'SAT am, Other, CLFRC<=.40   '
  ods%meta%kx_names(164) = 'SAT am, Other, CLFRC> .40  '
  ods%meta%kx_names(165) = 'SAT pm, WATER, CLFRC<=.10   '
  ods%meta%kx_names(166) = 'SAT pm, WATER, CLFRC<=.40   '
  ods%meta%kx_names(167) = 'SAT pm, WATER, CLFRC> .40  '
  ods%meta%kx_names(168) = 'SAT pm, Other, CLFRC<=.10   '
  ods%meta%kx_names(169) = 'SAT pm, Other, CLFRC<=.40   '
  ods%meta%kx_names(170) = 'SAT pm, Other, CLFRC> .40  '
  ods%meta%kx_names(171) = 'GPS DAO Refractive Retrieval   '
  ods%meta%kx_names(172) = 'GPS DAO Bending Angle Retrieval  '
  ods%meta%kx_names(173) = 'GPS DAO Observation Retrieval   '
  ods%meta%kx_names(174) = 'GPS JPL Refractive Retrieval   '
  ods%meta%kx_names(175) = 'GPS JPL Bending Angle Retrieval  '
  ods%meta%kx_names(176) = 'GPS JPL Observation Retrieval   '
  ods%meta%kx_names(177) = 'GPS NCAR Refractive Retrieval   '
  ods%meta%kx_names(178) = 'GPS NCAR Bending Angle Retrieval  '
  ods%meta%kx_names(179) = 'GPS NCAR Observation Retrieval   '
  ods%meta%kx_names(180) = 'GPS INST Refractive Retrieval   '
  ods%meta%kx_names(181) = 'GPS INST Bending Angle Retrieval  '
  ods%meta%kx_names(182) = 'GPS INST Observation Retrieval   '
  ods%meta%kx_names(183) = 'SSM/I WENTZ F10/F15    '
  ods%meta%kx_names(184) = 'SSM/I WENTZ F11    '
  ods%meta%kx_names(185) = 'SSM/I WENTZ F13    '
  ods%meta%kx_names(186) = 'DAOTOVS Land AM Type: Clr HIRS/AMSUA/AMSUB '
  ods%meta%kx_names(187) = 'DAOTOVS Land AM Type: Clr HIRS/AMSUA '
  ods%meta%kx_names(188) = 'DAOTOVS Land AM Type: AMSUA/AMSUB  '
  ods%meta%kx_names(189) = 'DAOTOVS Land AM Type: AMSUA  '
  ods%meta%kx_names(190) = 'DAOTOVS Land AM Type: AMSUB  '
  ods%meta%kx_names(191) = 'DAOTOVS Ocean AM Type: Clr HIRS/AMSUA/AMSUB '
  ods%meta%kx_names(192) = 'DAOTOVS Ocean AM Type: Clr HIRS/AMSUA '
  ods%meta%kx_names(193) = 'DAOTOVS Ocean AM Type: AMSUA/AMSUB  '
  ods%meta%kx_names(194) = 'DAOTOVS Ocean AM Type: AMSUA  '
  ods%meta%kx_names(195) = 'DAOTOVS Ocean AM Type: AMSUB  '
  ods%meta%kx_names(196) = 'DAOTOVS Ice AM Type: Clr HIRS/AMSUA/AMSUB '
  ods%meta%kx_names(197) = 'DAOTOVS Ice AM Type: Clr HIRS/AMSUA '
  ods%meta%kx_names(198) = 'DAOTOVS Ice AM Type: AMSUA/AMSUB  '
  ods%meta%kx_names(199) = 'DAOTOVS Ice AM Type: AMSUA  '
  ods%meta%kx_names(200) = 'DAOTOVS Ice AM Type: AMSUB  '
  ods%meta%kx_names(201) = 'DAOTOVS Land AM Type: Cld Clr HIRS/AMSUA/AMSUB'
  ods%meta%kx_names(202) = 'DAOTOVS Land AM Type: Cld Clr HIRS/AMSUA'
  ods%meta%kx_names(203) = 'DAOTOVS Ocean AM Type: Cld Clr HIRS/AMSUA/AMSUB'
  ods%meta%kx_names(204) = 'DAOTOVS Ocean AM Type: Cld Clr HIRS/AMSUA'
  ods%meta%kx_names(205) = 'DAOTOVS Ice AM Type: Cld Clr HIRS/AMSUA/AMSUB'
  ods%meta%kx_names(206) = 'DAOTOVS Ice AM Type: Cld Clr HIRS/AMSUA'
  ods%meta%kx_names(207) = 'DAOTOVS Land PM Type: Clr HIRS/AMSUA/AMSUB '
  ods%meta%kx_names(208) = 'DAOTOVS Land PM Type: Clr HIRS/AMSUA '
  ods%meta%kx_names(209) = 'DAOTOVS Land PM Type: AMSUA/AMSUB  '
  ods%meta%kx_names(210) = 'DAOTOVS Land PM Type: AMSUA  '
  ods%meta%kx_names(211) = 'DAOTOVS Land PM Type: AMSUB  '
  ods%meta%kx_names(212) = 'DAOTOVS Ocean PM Type: Clr HIRS/AMSUA/AMSUB '
  ods%meta%kx_names(213) = 'DAOTOVS Ocean PM Type: Clr HIRS/AMSUA '
  ods%meta%kx_names(214) = 'DAOTOVS Ocean PM Type: AMSUA/AMSUB  '
  ods%meta%kx_names(215) = 'DAOTOVS Ocean PM Type: AMSUA  '
  ods%meta%kx_names(216) = 'DAOTOVS Ocean PM Type: AMSUB  '
  ods%meta%kx_names(217) = 'DAOTOVS Ice PM Type: Clr HIRS/AMSUA/AMSUB '
  ods%meta%kx_names(218) = 'DAOTOVS Ice PM Type: Clr HIRS/AMSUA '
  ods%meta%kx_names(219) = 'DAOTOVS Ice PM Type: AMSUA/AMSUB  '
  ods%meta%kx_names(220) = 'DAOTOVS Ice PM Type: AMSUA  '
  ods%meta%kx_names(221) = 'DAOTOVS Ice PM Type: AMSUB  '
  ods%meta%kx_names(222) = 'DAOTOVS Land PM Type: Cld Clr HIRS/AMSUA/AMSUB'
  ods%meta%kx_names(223) = 'DAOTOVS Land PM Type: Cld Clr HIRS/AMSUA'
  ods%meta%kx_names(224) = 'DAOTOVS Ocean PM Type: Cld Clr HIRS/AMSUA/AMSUB'
  ods%meta%kx_names(225) = 'DAOTOVS Ocean PM Type: Cld Clr HIRS/AMSUA'
  ods%meta%kx_names(226) = 'DAOTOVS Ice PM Type: Cld Clr HIRS/AMSUA/AMSUB'
  ods%meta%kx_names(227) = 'DAOTOVS Ice PM Type: Cld Clr HIRS/AMSUA'
  ods%meta%kx_names(228) = 'NESTAT AIRS Land PM type A '
  ods%meta%kx_names(229) = 'NESTAT AIRS Land PM type B '
  ods%meta%kx_names(230) = 'NESTAT AIRS Land PM type C '
  ods%meta%kx_names(231) = 'NESTAT AIRS Ocean PM type A '
  ods%meta%kx_names(232) = 'NESTAT AIRS Ocean PM type B '
  ods%meta%kx_names(233) = 'NESTAT AIRS Ocean PM type C '
  ods%meta%kx_names(234) = 'NESPHY AIRS Land PM type A '
  ods%meta%kx_names(235) = 'NESPHY AIRS Land PM type B '
  ods%meta%kx_names(236) = 'NESPHY AIRS Land PM type C '
  ods%meta%kx_names(237) = 'NESPHY AIRS Ocean PM type A '
  ods%meta%kx_names(238) = 'NESPHY AIRS Ocean PM type B '
  ods%meta%kx_names(239) = 'NESPHY AIRS Ocean PM type C '
  ods%meta%kx_names(240) = 'DAOAIRS Land Type: Clr AIRS/AMSU/HSB  '
  ods%meta%kx_names(241) = 'DAOAIRS Land Type: Clr AIRS/AMSU  '
  ods%meta%kx_names(242) = 'DAOAIRS Land Type: Clr AIRS/HSB  '
  ods%meta%kx_names(243) = 'DAOAIRS Land Type: AMSU/HSB   '
  ods%meta%kx_names(244) = 'DAOAIRS Land Type: AMSU   '
  ods%meta%kx_names(245) = 'DAOAIRS Land Type: HSB   '
  ods%meta%kx_names(246) = 'DAOAIRS Ocean Type: Clr AIRS/AMSU/HSB  '
  ods%meta%kx_names(247) = 'DAOAIRS Ocean Type: Clr AIRS/AMSU  '
  ods%meta%kx_names(248) = 'DAOAIRS Ocean Type: Clr AIRS/HSB  '
  ods%meta%kx_names(249) = 'DAOAIRS Ocean Type: AMSU/HSB   '
  ods%meta%kx_names(250) = 'DAOAIRS Ocean Type: AMSU   '
  ods%meta%kx_names(251) = 'DAOAIRS Ocean Type: HSB   '
  ods%meta%kx_names(252) = 'DAOAIRS Ice Type: Clr AIRS/AMSU/HSB  '
  ods%meta%kx_names(253) = 'DAOAIRS Ice Type: Clr AIRS/AMSU  '
  ods%meta%kx_names(254) = 'DAOAIRS Ice Type: Clr AIRS/HSB  '
  ods%meta%kx_names(255) = 'DAOAIRS Ice Type: AMSU/HSB   '
  ods%meta%kx_names(256) = 'DAOAIRS Ice Type: AMSU   '
  ods%meta%kx_names(257) = 'DAOAIRS Ice Type: HSB   '
  ods%meta%kx_names(258) = 'DAOAIRS Land Type: Cld Clr AIRS/AMSU/HSB '
  ods%meta%kx_names(259) = 'DAOAIRS Land Type: Cld Clr AIRS/AMSU '
  ods%meta%kx_names(260) = 'DAOAIRS Land Type: Cld Clr AIRS/HSB '
  ods%meta%kx_names(261) = 'DAOAIRS Ocean Type: Cld Clr AIRS/AMSU/HSB '
  ods%meta%kx_names(262) = 'DAOAIRS Ocean Type: Cld Clr AIRS/AMSU '
  ods%meta%kx_names(263) = 'DAOAIRS Ocean Type: Cld Clr AIRS/HSB '
  ods%meta%kx_names(264) = 'DAOAIRS Ice Type: Cld Clr AIRS/AMSU/HSB '
  ods%meta%kx_names(265) = 'DAOAIRS Ice Type: Cld Clr AIRS/AMSU '
  ods%meta%kx_names(266) = 'DAOAIRS Ice Type: Cld Clr AIRS/HSB '
  ods%meta%kx_names(267) = 'Cloud Track Wind - European Water Vapor'
  ods%meta%kx_names(268) = 'Cloud Track Wind - European visible automated'
  ods%meta%kx_names(269) = 'Cloud Track Wind - Japanese Water Vapor'
  ods%meta%kx_names(270) = 'Cloud Track Wind - Japanese visible automated'
  ods%meta%kx_names(271) = 'Microwave Limb Sounder (MLS) at 205 GHz'
  ods%meta%kx_names(272) = 'Microwave Limb Sounder (MLS) at 183 GHz'
  ods%meta%kx_names(273) = 'Global Ozone Monitoring Experiment (GOME)  '
  ods%meta%kx_names(274) = 'Cloud Track Wind - MISR  '
  ods%meta%kx_names(275) = 'Cloud Track Wind - European ELW***) IR'
  ods%meta%kx_names(276) = 'Cloud Track Wind - European ELW***) visible'
  ods%meta%kx_names(277) = 'Cloud Track Wind - European ELW***) Water'
  ods%meta%kx_names(278) = 'Cloud Track Wind - European High-resolution Visible'
  ods%meta%kx_names(279) = 'Cloud Track Wind - European Clear Sky'
  ods%meta%kx_names(280) = 'TIDE GAUGE STATION    '
  ods%meta%kx_names(281) = 'Cloud Track Wind - TERRA Modis Water'
  ods%meta%kx_names(282) = 'Cloud Track Wind - TERRA Modis IR-CIMSS'
  ods%meta%kx_names(283) = 'Super-pressure balloon     '
  ods%meta%kx_names(284) = 'VAD (NEXRAD) WINDS    '
  ods%meta%kx_names(285) = 'WIND PROFILER     '
  ods%meta%kx_names(286) = 'Cloud Track Wind-European High-resolution Water Vapor Wind(HWW)'
  ods%meta%kx_names(287) = 'GOES IRET - US1 Land Type: Clr'
  ods%meta%kx_names(288) = 'GOES IRET - US1 Ocean Type: Clr'
  ods%meta%kx_names(289) = 'GOES IRET - US2 Land Type: Clr'
  ods%meta%kx_names(290) = 'GOES IRET - US2 Ocean Type: Clr'
  ods%meta%kx_names(291) = 'GOES IRET - US3 Land Type: Clr'
  ods%meta%kx_names(292) = 'GOES IRET - US3 Ocean Type: Clr'
  ods%meta%kx_names(293) = 'HRDI - High Resolution Doppler Imager '
  ods%meta%kx_names(294) = 'SABER - Sounding of the Atmosphere Broadband'
  ods%meta%kx_names(295) = 'Cloud Track Wind - AQUA Modis Water'
  ods%meta%kx_names(296) = 'Cloud Track Wind - AQUA Modis IR-CIMSS'
  ods%meta%kx_names(297) = 'Cloud Track Wind - TERRA Modis Water'
  ods%meta%kx_names(298) = 'Cloud Track Wind - TERRA Modis IR'
  ods%meta%kx_names(299) = 'Cloud Track Wind - AQUA Modis Water'
  ods%meta%kx_names(300) = 'Cloud Track Wind - AQUA Modis IR'
  ods%meta%kx_names(301) = 'TERRA MODIS Aerosol (Dark Target Ocean Algorithm)  '
  ods%meta%kx_names(302) = 'TERRA MODIS Aerosol (Dark Target Land Algorithm)  '
  ods%meta%kx_names(303) = 'EPTOMS Aerosol (Ocean Algorithm)   '
  ods%meta%kx_names(304) = 'EPTOMS Aerosol (Land Algorithm)   '
  ods%meta%kx_names(305) = 'MSG Cloud track wind - Infrared channel'
  ods%meta%kx_names(306) = 'MSG Cloud track wind - Water vapor'
  ods%meta%kx_names(307) = 'MSG Cloud track wind - Visible channel'
  ods%meta%kx_names(308) = 'MSG Clear sky Water Vapor channel wind'
  ods%meta%kx_names(309) = 'TERRA MODIS Aerosol (Deep Blue Ocean Algorithm)'
  ods%meta%kx_names(310) = 'TERRA MODIS Aerosol (Deep Blue Land Algorithm)'
  ods%meta%kx_names(311) = 'AQUA MODIS Aerosol (Dark Target Ocean Algorithm)  '
  ods%meta%kx_names(312) = 'AQUA MODIS Aerosol (Dark Target Land Algorithm)  '
  ods%meta%kx_names(313) = 'MISR (Multi-angle Imaging SpectroRadiometer)   '
  ods%meta%kx_names(314) = 'OMI (Ozone Monitoring Instrument)   '
  ods%meta%kx_names(315) = 'Mozaic Aircraft Data    '
  ods%meta%kx_names(316) = 'Parasol (Ocean Algorithm)    '
  ods%meta%kx_names(317) = 'Parasol (Land Algorithm)    '
  ods%meta%kx_names(318) = 'MOPITT (Measurements Of Pollution In The Troposphere)'
  ods%meta%kx_names(319) = 'AQUA MODIS Aerosol (Deep Blue Ocean Algorithm)'
  ods%meta%kx_names(320) = 'AQUA MODIS Aerosol (Deep Blue Land Algorithm)'
  ods%meta%kx_names(321) = 'TERRA MODIS pixFire    '
  ods%meta%kx_names(322) = 'AQUA MODIS Pixfire    '
  ods%meta%kx_names(323) = 'AERONET'

end subroutine fix_odsmeta



