
      program odsqc

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: odsqc:  Unit Tester for SQC
!
!
! !USAGE: see the routine usage() below
!
! !USES:
!
      use  m_odsmeta
      use  m_ods
      use  m_sqc
      use  m_die
      use  m_zeit
      use  m_inpak90
      use  m_psas    ! for Taft's version only

      implicit NONE

! !DESCRIPTION:
!
!     Unit test for the Quality Control
!
! !REVISION HISTORY:
!
!     19oct1999  da Silva  Derived from ods_qc.f
!     10may2002  Dee       Replaced m_soqcs by m_sqc
!     22oct2002  Sawyer    Upgraded for SPMD, free format
!     05dec2002  Sawyer    Removed lat_max, use default partitioning
!
!EOP

      character(len=*), parameter :: myname = 'odsqc'

      integer, parameter :: NFILES_MAX = 100 ! Max number of input files


!     Local variables
!     ---------------
      integer nfiles, ifile, isyn, nymd, nhms, jday, jbeg, jend
      integer i, ier, rc, nobs, nobs_good, synhour
      integer, external :: ods_caldat

      character*255 infile (NFILES_MAX) ! input filenames
      character*255 prefix              ! output filename prefix
      character*255 outfile             ! output filename
      character*255 ftype               ! ods file type


!     storage for ODS:
!     ---------------
      type (ods_vect) :: ods

      call zeit_ci ( 'odsqc' )

!     Option flags:
!     ------------
      prefix  = 'odsqc'    ! DEFAULT: output file names prefixed with 'odsqc'
      synhour = -1         ! DEFAULT: process all synoptic times

!     Parse command line and load resources
!     -------------------------------------
      call Init_ ( infile, NFILES_MAX, nfiles, prefix, synhour )

!     Initialize PSAS
!     ---------------
      call psas_init()

!     Loop over input files
!     ---------------------
      do ifile = 1, nfiles

!       Find days on file
!       ------------------
        call Days_ ( trim(infile(ifile)), jbeg, jend )

!       Loop over days/synoptic times on this file
!       -------------------------------------------
        do jday = jbeg, jend

           nymd = ODS_CalDat ( jday )

         do isyn = 1, 4

           print *, 'Working on ... ', trim(infile(ifile)), nymd, nhms

!          6-hourly files
!          --------------
           nhms = (isyn-1) * 060000

!         Read all data for this synoptic time
!         ------------------------------------
                         call zeit_ci ( 'ods_get' )
          call ODS_Get ( trim(infile(ifile)), nymd, nhms, ftype, ods, ier )
                         call zeit_co ( 'ods_get' )
#if 0
          if ( ier .eq. 0 .and. trim(ftype) .ne. 'post_analysis' ) then
             call warn ( myname, 'expected post analysis ODS but found '  &
                         // trim(ftype) )
             ier = -1
          endif
#endif
          if ( ier .ne. 0 ) call die ( myname, 'could not read ods file ' &
                                     // trim(infile(ifile)) )
!         Decide whether to skip this synoptic hour
!         -----------------------------------------
          nobs = ods%data%nobs

          if ( synhour .ge. 0 .and. nhms .ne. synhour*10000 ) then
               call warn ( myname, 'Skipping this synoptic time' )
          else
             print *, '   [] Read nobs, date, time: ', nobs,       &
                             nymd, nhms, ods%meta%nkt

!            Apply SQC
!            ---------
                         call zeit_ci ( 'SQC' )
             call SQC ( ods%data, nobs, nobs_good )
                         call zeit_co ( 'SQC' )

!            Print out input ODS summary
!            ---------------------------
             call ODS_Tally ( 6, ods, nobs, rc )

!            Write to ods file
!            -----------------
             outfile = trim(prefix) // '.' // trim(infile(ifile))
                         call zeit_ci ( 'ods_put' )
             call ODS_Put ( trim(outfile), ftype, nymd, nhms, ods, ier,       &
                            new = .true. )
                            call zeit_co ( 'ods_put' )
             if ( ier .eq. 0 ) then
                print *, '   [] Wrote nobs, date, time: ', nobs, nymd, nhms
             else
                call die ( myname, 'could not write '//trim(outfile) )
             endif
          endif

          call ods_clean( ods, ier )
          if ( ier .ne. 0 ) then
             print *, 'Error releasing ods ', 'rc = ', rc
          end if

         end do  ! loop over synoptic times
        end do  ! loop over julian days

      end do  ! loop over files

!     Finalize PSAS
!     -------------
!!!!!      call psas_end()   ! this is only mpi_finalize

!     All done
!     --------
      call zeit_co ( 'odsqc' )
      call zeit_flush ( 6 )

      CONTAINS

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: Init_ --- Initialize odsqc
!
! !DESCRIPTION:
!
! !INTERFACE:
!
      subroutine Init_ ( infile, NFILES_MAX, nfiles, prefix, synhour )

! !INPUT PARAMETERS:
!
      implicit NONE
      integer,       intent(in)  :: nfiles_max
!
! !OUTPUT PARAMETERS:

      character*255, intent(out) :: infile(nfiles_max)
      integer,       intent(out) :: nfiles

! !INPUT/OUTPUT PARAMETERS:

      character*255, intent(inout) :: prefix
      integer,       intent(inout) :: synhour
!
!
! !REVISION HISTORY:
!       10Oct97 - D.Dee   Initial code
!       04Nov97 - D.Dee   Added qccopy
!       04Mar98 - D.Dee   Completely revised
!       10Mar98 - D.Dee   Minor changes
!       11Mar98 - D.Dee   Added reorder option
!       14Sep98 - D.Dee   See revision history of main program
!       21Sep98 - D.Dee   Removed listods, shuffle options
!       08Nov98 - D.Dee   Added -daily option
!       18Nov99 - da Silva removed 0daily
!
!EOP
!BOC

      character*4, parameter :: myname = 'init'

      integer iret, i, lv, iarg, argc, iargc
      character*255 argv
      character*2 HH

!     Parse command line
!     ------------------

      argc =  iargc()
      if ( argc .lt. 1 ) call usage()
      nfiles = 0
      iarg = 0
      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) go to 111
         call GetArg ( iArg, argv )
         if (index(argv,'-prefix' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage()
            iarg = iarg + 1
            call GetArg ( iArg, prefix )
         elseif (index(argv,'-synhour') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage()
            iarg = iarg + 1
            call GetArg ( iArg, HH )
            read(HH,*) synhour
         else
            nfiles = nfiles + 1
            if ( nfiles .gt. nfiles_max ) then
               print *, 'Maximum number of input files = ', nfiles_max
               stop
            end if
            infile(nfiles) = argv
         end if
      end do
 111  continue
      if ( nfiles .lt. 1 ) call usage()

!     Echo the parameters
!     -------------------
      print *
      if (synhour .ge. 0) print *, 'Will process synoptic hour ', &
           synhour, ' only'
      print *
      print *, 'Input files: ', nfiles
      print *
      do i = 1, nfiles
         lv = len_trim(infile(i))
         print *, ' o ', infile(i)(1:lv)
      end do
      print *
      lv = len_trim(prefix)
      print *, 'Output filename prefix: ', prefix(1:lv)

      return

      end subroutine Init_

!.................................................................

      Subroutine Days_ ( fname, jbeg, jend )

      character(len=*) fname
      integer jbeg, jend

!
!     Find beginning and ending Julian days on file.
!
      integer ncid, ier

      call ODS_Open ( ncid, trim(fname), 'r', ier ) ! open the file
      if ( ier .ne. 0 ) call die ( myname, 'could not open ods file ' &
                                     // trim(fname) )

       call ODS_IGet ( ncid, 'syn_beg:first_julian_day',  jbeg, ier )
       call ODS_IGet ( ncid, 'syn_beg:latest_julian_day', jend, ier )

      if ( ier .ne. 0 ) call die ( myname, 'could not read ods file ' &
                                    // trim(fname) )

       call ODS_close ( ncid, myname, ier )

      end Subroutine Days_

!.................................................................

      end program odsqc

      subroutine usage()
      print *
      print *, 'Usage:  odsqc.x [-prefix ID] [-synhour HH] odsfile(s)'
      print *
      print *, 'where'
      print *
      print *,   '-prefix ID       use ID for naming output files'
      print *,   '                (default: odsqc)'
      print *,   '-synhour HH      process synoptic hour HH only'
      print *,   '                (default: all)'
      print *,   ' odsfile(s)      post-analysis ODS file(s)'
      print *
      stop
      end

