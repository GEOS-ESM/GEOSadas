program ene_adpatinf

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 610.1, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: ene_adaptinf: Total-energy-based adaptive inflation for AtmEns
!
! !USAGE: see the routine usage() below
!
! !USES:
!
   use kinds, only: r_double
   use m_stdio, only: stdout, stderr
   use m_strTemplate, only : strTemplate
   use m_inpak90
   implicit NONE

! !DESCRIPTION: 
!
! !REVISION HISTORY:
!
! !SEE ALSO:
!     dyn_recenter
!
!-------------------------------------------------------------------------
!EOP

  character(len=*), parameter :: myname = 'ene_adaptinf'
  character(len=256) outfile, RCfile, expid, enefname
  integer im,jm,km,ntime,nvars,ngatts,rc,id,ks
  integer nymdusr, nhmsusr, nymd, nhms, nfiles, ifile
  integer,parameter :: nfiles_max=500
  real(r_double),allocatable :: twe(:,:,:)
  real ene_threshold, factinf
  character(len=256) infile(nfiles_max)
  logical verbose, minecho
   
  call init_ ( infile, nfiles_max, nfiles, RCfile, expid, nymdusr, nhmsusr, outfile, verbose, minecho )

  call set_( RCfile, infile(1), ene_threshold, factinf, ks, rc )
    if(rc/=0) then
       print *, myname, ': Error, aborting ...', rc
       call exit(rc)
    endif

  do ifile = 1,nfiles

     call gfio_open       ( trim(infile(ifile)),1,id,rc )
       if(rc/=0) then
          print *, myname, ': Error, aborting ...', rc
          call exit(rc)
       endif
     call gfio_diminquire ( id,im,jm,km,ntime,nvars,ngatts,rc)
       if(rc/=0) then
          print *, myname, ': Error, aborting ...', rc
          call exit(rc)
       endif

     call get_metadata_

     allocate(twe(im,jm,km))
     call get_energy_
     call total_energy_(ene_threshold,factinf,ks)
     deallocate(twe)

     call gfio_close  ( id,rc )
       if(rc/=0) then
          print *, myname, ': Error, aborting ...', rc
          call exit(rc)
       endif

     enddo ! ifile

! Successful completion
  open (999,file='ENE_ADAPTINF_EGRESS',form='formatted')
  close(999)
  contains
  subroutine get_metadata_
    real(r_double),    allocatable ::    lat(:)
    real(r_double),    allocatable ::    lon(:)
    real(r_double),    allocatable ::    lev(:)
    real(r_double),    allocatable :: vrange(:,:)
    real(r_double),    allocatable :: prange(:,:)
    integer, allocatable :: yymmdd(:)
    integer, allocatable :: hhmmss(:)
    integer, allocatable ::  kmvar(:)
    integer timinc
    integer k
    real(r_double)  undef
    character*256  title
    character*256  source
    character*256  contact
    character*256  levunits
    character*256, allocatable ::  vname(:)
    character*256, allocatable :: vtitle(:)
    character*256, allocatable :: vunits(:)
    allocate ( lon(im) )
    allocate ( lat(jm) )
    allocate ( lev(km) )
    allocate ( yymmdd(ntime) )
    allocate ( hhmmss(ntime) )
    allocate (  vname(nvars) )
    allocate ( vtitle(nvars) )
    allocate ( vunits(nvars) )
    allocate (  kmvar(nvars) )
    allocate ( vrange(2,nvars) )
    allocate ( prange(2,nvars) )

    call gfio_inquire ( id,im,jm,km,ntime,nvars, &
                        title,source,contact,undef, &
                        lon,lat,lev,levunits, &
                        yymmdd,hhmmss,timinc, &
                        vname,vtitle,vunits,kmvar, &
                        vrange,prange,rc )

    if (ntime>1) then
       print *, "error: cannot handle file w/ multiple time slots"
       call exit(1)
    else
       nymd = yymmdd(1)
       nhms = hhmmss(1)
    endif

    deallocate ( lon )
    deallocate ( lat )
    deallocate ( lev )
    deallocate ( yymmdd )
    deallocate ( hhmmss )
    deallocate (  vname )
    deallocate ( vtitle )
    deallocate ( vunits )
    deallocate (  kmvar )
    deallocate ( vrange )
    deallocate ( prange )

  end subroutine get_metadata_
  subroutine get_energy_
    call gfio_getvar ( id,'twe',nymd,nhms,im,jm,1,km,twe,rc )
  end subroutine get_energy_
  subroutine total_energy_(ene_threshold,factinf,ks)
    integer,intent(in) :: ks
    real,intent(in) :: ene_threshold
    real,intent(in) :: factinf
    integer i,j,k
    real totene
    real rinflate

!   Note: energy in file expected to be fully consistently scaled
!   (w/ cos(lat) and all)
!   -------------------------------------------------------------
    totene = 0.0
    do k=km-ks,km
       totene = totene + sum(twe(:,:,k))
    enddo
    rinflate = factinf
    if(abs(totene)>1.e-5) then 
       rinflate = factinf * ene_threshold / totene
    endif

    if(minecho) then
       write(stdout,'(f7.3)') rinflate
    else
       write(stdout,'(a,f9.6,a,i8.8,2x,i6.6,a,f7.3)') &
           'Total (tropo) energy: ', totene, ' on ', nymd, nhms, ' Infl: ',rinflate
    endif
  end subroutine total_energy_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 610.1, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !ROUTINE: init: initialize this package
!
! !DESCRIPTION:
!
! !INTERFACE:
!
  subroutine init_ ( infile, nfiles_max, nfiles, RCfile, expid, nymdusr, nhmsusr, &
                     outfile, verbose, minecho )

! !USES:

! !INPUT PARAMETERS:
!
      implicit NONE
      integer,       intent(in)  :: nfiles_max

! !OUTPUT PARAMETERS:

      character(len=*), intent(out) :: RCfile
      character(len=*), intent(out) :: expid
      character(len=*), intent(out) :: infile(nfiles_max)
      integer,          intent(out) :: nfiles
      integer,          intent(out) :: nymdusr,nhmsusr
      character(len=*), intent(out) :: outfile
      logical,          intent(out) :: verbose
      logical,          intent(out) :: minecho
!
!
! !REVISION HISTORY:
!     26Jun2014 Todling - Initial code (stripped off somewhere else)
!
!EOP
!BOC

      character*4, parameter :: myname_ = myname//'*init_'

      integer iret, i, ic, lt, lv, iarg, argc, iargc
      character*255 argv
      character*10 SS

      RCfile  = 'ene_adaptinf.rc'
      outfile = 'ene_adaptinf.txt'
      verbose = .false.
      minecho = .false.
      expid   = 'UNKNOWN'
      nymdusr = -1
      nhmsusr = -1

!     Parse command line
!     ------------------
      argc =  iargc()
      if ( argc .lt. 1 ) call usage_()
      nfiles = 0
      iarg = 0
      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) go to 111
         call GetArg ( iArg, argv )
         if (index(argv,'-o' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, outfile )
         else if (index(argv,'-verbose' ) .gt. 0 ) then
            verbose = .true.
         else if (index(argv,'-minecho' ) .gt. 0 ) then
            minecho = .true.
         else if (index(argv,'-expid' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, expid )
         else if (index(argv,'-pick' ) .gt. 0 ) then
            if ( iarg+2 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iarg, argv )
            read(argv,*) nymdusr
            iarg = iarg + 1
            call GetArg ( iarg, argv )
            read(argv,*) nhmsusr
         else if (index(argv,'-rc' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, RCfile )
         else
            nfiles = nfiles + 1
            if ( nfiles .gt. nfiles_max ) then
               print *, 'Maximum number of input files = ', nfiles_max
               print *, myname, ': Error, aborting ...', 99
               call exit(99)
            end if
            infile(nfiles) = argv
         end if
      end do
 111  continue
      if ( nfiles .lt. 1 ) call usage_()

      if(minecho) verbose = .false. ! regardless of command line choices

      if(.not.minecho) then
         print *
         print *, 'Input files: ', nfiles
         print *
         do i = 1, nfiles
            lv = len_trim(infile(i))
            print *, ' o ', infile(i)(1:lv)
         end do
         print *
         print *, 'Output filename: ', trim(outfile)
      endif

      return

  end subroutine init_

!EOC
!-------------------------------------------------------------------------

!-------------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 610.1, GEOS/DAS        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: set_ --- paramater setup via rc file
!
! !INTERFACE:
!
  subroutine set_ ( rcfile, fname, ene_threshold, factinf, ks, rc )

      Implicit None
                                                                                                 
! !INPUT PARAMETERS:
!
      character(len=*), intent(in) :: rcfile  ! resource filename
                                                                                                 
! !OUTPUT PARAMETERS:

      real,    intent(out) :: ene_threshold ! pre-set energy threshold
      real,    intent(out) :: factinf       ! desirable inflation factor
      integer, intent(out) :: ks            ! threshold level index
      integer, intent(out) :: rc            ! return status code

! !INPUT/OUTPUT PARAMETERS:
      character(len=*), intent(inout) :: fname  !  input: filename template for energy density field
                                                ! output: resolved filename
! !FILES USED:  ene_adapt_infl.rc
!
! !DESCRIPTION:  Reads resource file, and set parameters need by this package.
!
! !REVISION HISTORY:
!
!   26Jun2014  Todling     Initial code.
!
!EOP
!-------------------------------------------------------------------------
                                                                                                 
      character(len=*), parameter :: myname_ = myname//'*set_'

      character(len=255) token,fntmpl
      character(len=255) thisrc

      integer     iret, ierr
      integer     myID
   
      rc=0
      if (rcfile=='null') return 

!     Load resources from pert_energy.rc
!     ----------------------------------
      thisrc = ' '
      call getenv('ENE_ADAPTINF_RC',thisrc)           ! Unix binding
      if(thisrc.eq.' ') thisrc=rcfile                 ! default name
      call i90_loadf (trim(thisrc), iret)
      if( iret .ne. 0) then
          write(stdout,'( a  )') '----------------------------------'
          write(stdout,'(2a  )') myname_, ': No resource file found'
          write(stdout,'( a,/)') '----------------------------------'
          return
      end if
      if(verbose) then
         write(stdout,'( a  )') '---------------------------------'
         write(stdout,'(2a  )') myname_, ': Reading resource file'
         write(stdout,'( a,/)') '---------------------------------'
      endif


!     Level encompassing troposphere/low stratosphere
!     -----------------------------------------------
      call I90_label('ene_threshold_level_index:', iret)
      if (iret .ne. 0) then
        write(stderr,'(2a,i5)') myname_,': I90_label error(ene_threshold), iret =',iret
         rc=1
         call I90_release()
         return
      end if
      ks = I90_GInt(iret)
      if( iret .ne. 0) then
         write(stderr,'(2a,i5)') myname_,': I90_GInt error(ibak), iret =',iret
         rc=2
         call I90_release()
         return
      end if
      if(verbose) write(stdout,'(a,i4)') 'Level-index encompassing tropo/low strat: ', ks

!     Preset energy threshold
!     -----------------------
      call I90_label('ene_threshold:', iret)
      if (iret .ne. 0) then
        write(stderr,'(2a,i5)') myname_,': I90_label error(ene_threshold), iret =',iret
         rc=3
         call I90_release()
         return
      end if
      ene_threshold = I90_GFloat(iret)
      if( iret .ne. 0) then
         write(stderr,'(2a,i5)') myname_,': I90_GFloat error(ibak), iret =',iret
         rc=4
         call I90_release()
         return
      end if
      if(verbose) write(stdout,'(a,e13.6)') 'Pre-set energy threshold (J/Kg): ', ene_threshold

!     Desirable inflation factor
!     --------------------------
      call I90_label('desirable_inflation_factor:', iret)
      if (iret .ne. 0) then
        write(stderr,'(2a,i5)') myname_, ': I90_label error, iret =',iret
      else
        factinf = I90_GFloat(iret)
        if( iret .ne. 0) then
           write(stderr,'(3a,i5)') myname_,': I90_GFloat error, ', ' iret =',iret
           rc=5
           call I90_release()
           return
        end if
      end if
      if(verbose) write(stdout,'(a,1p,e13.6)') 'Desirable inflation factor: ',factinf

!     In case input filename is TEMPLATE, then read in template filename from
!     RCfile and resolve template with other user-provided information
!     -----------------------------------------------------------------------
      if(trim(fname) == 'TEMPLATE' .and. (nymdusr>0.and.nhmsusr>0) .and. & 
         trim(expid) /= 'UNKNOWN') then
        call I90_label('ene_fname_template:', iret)
        if (iret .eq. 0) then
          call I90_Gtoken ( token, iret )
          if (iret .ne. 0) then
            write(stderr,'(2a,i5)') myname_, ': I90_Gtoken error, iret =',iret
            rc=6
            call I90_release()
            return
          else
            fntmpl= trim(token)
          end if
        end if
        call strTemplate ( fname, fntmpl, nymd=nymdusr, nhms=nhmsusr, xid=trim(expid) )
        if(verbose) write(stdout,'(2a)') 'Energy density filename: ',trim(fname)
      endif

!     release resource file:
!     ---------------------
      call I90_release()

      return
  end subroutine set_

  subroutine usage_()
      print *
      print *, 'Usage:'
      print *
      print *, 'ene_adpatinf.x [-o ID] [-rc RCfile] enefile(s)'
      print *
      print *, 'where'
      print *
      print *,'-o  ID           use ID for naming output files'
      print *,'                  (default: ene_adaptinf.txt)'
      print *,'-expid  EXPID    experiment identification name '
      print *,'-pick nymd nhms  date and time (YYYYMMDD HHMMSS)'
      print *,'-verbose         sets verbose on (default: off)'
      print *,'-rc RCfile        resource file'
      print *,'                  (default: ene_adaptinf.rc)'
      print *,'-minecho         minimal echo; echoes only resulting inflation factor'
      print *,'                  (default: echo all)'
      print *
      print *
      print *,' enefile(s)    NetCDF file(s) with energy density fields'
      print *
      print *, 'NOTES: '
      print *, '-----  '
      print *
      print *, ' Initial version: 26 June 2014; R. Todling '
      print *, ' Last updated: 26 June 2014; R. Todling '
      print *
      stop
 end subroutine usage_
end program ene_adpatinf
