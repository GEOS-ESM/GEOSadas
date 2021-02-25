!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  m_sqc --- Statistical Quality Control
!
! !INTERFACE:
!
   MODULE m_sqc

! !USES:

   use m_odsmeta                         ! ODS metadata: qc flags, etc
   use m_ods                             ! ODS vector class
   use m_MergeSorts                      ! sorting tools
   use m_Spherical_Partition, only : NumberOfRegions, Initialize, Clean
   use m_Spherical_Triangle,  only : Separation, SEPANG_MIN
   use m_Spherical_Partition, only : Spherical_Partition, GetRegion
   use m_Spherical_Partition, only : xyz2reg
   use m_geometry,            only : ll2xyz
   use m_const, only: radius_earth
   use m_die                             ! error messages
   use m_stdio                           ! i/o definitions
   use m_inpak90                         ! rc input handler
   use m_zeit                            ! timer

   implicit NONE

!
! !PUBLIC MEMBER FUNCTIONS:
!
   private
   public sqc             ! main interface

!
! !DESCRIPTION: This module contains statistical quality control checks
!               on the observations.
!
! !REVISION HISTORY:
!
!  15May2002  Dee     Initial code, derived from m_soqcs.f and psas_qc.f
!  07Jun2002  Todling Merged w/ mods from T. Clune for MPI-PSAS compatib.
!  31Mar2003 (Dee/Rukh) Minor changes to the buddy check (merge w/fvDAS 1.4)
!
!EOP
!-------------------------------------------------------------------------

   logical, parameter :: verbose = .true. ! until we fix stdout issues

!  default resource file name
!  --------------------------
   character(len=*), parameter :: SQCRC = 'sqc.rc'

!  QC flags written by this module:
!  -------------------------------
   integer, dimension(1), parameter :: SQCH = (/ H_BACKG /)
   integer, dimension(6), parameter :: SQCX = (/ X_NOSTATS, X_BACKG, &
                                                 X_BUDDY,   X_WIND,  &
                                                 X_PROFILE, X_BADXM  /)

!  profile check excludes profiles with these exclusion flags:
!  ----------------------------------------------------------
   integer, parameter :: nqcxp = 4
   integer, dimension(nqcxp), parameter :: QCXP = (/ X_BACKG, X_BUDDY, &
                                                     X_WIND , X_BADXM  /)

!  data type info:
!  --------------
   character(len=16)  scaling(ktmax)   ! O-F scaling method
   character(len=16)  sigsrc (ktmax)   ! source for sigO, sigF
   real*8               ls_h   (ktmax)   ! horizontal length scales
   real*8               ls_v   (ktmax)   ! vertical length scales

   real*8, parameter :: Z_scale = 8e3    ! scale height for vertical length scales

!  relative tolerance for various real-real comparisons:
!  ----------------------------------------------------
   real*8, parameter :: tol_rel = 1e-5


   CONTAINS

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SQC --- Performs Statistical Quality Control Checks
!
! !INTERFACE:
!
      subroutine SQC ( y, ntotal, npassed )

!
! !INPUT PARAMETERS:
!
      integer,          intent(in)    :: ntotal  ! number of observations
!
! !INPUT/OUTPUT PARAMETERS:
!
      type(obs_vect),   intent(inout) :: y       ! observation vector

!
! !OUTPUT PARAMETERS:
!
      integer, intent(out)            :: npassed ! number of obs passing QC;
                                                 !  these are now at
                                                 !  the front of the list
!
! !DESCRIPTION: This driver routine implements the following quality checks
! on the observations:
!
! \begin{tabbing}
! xxx\=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\=\kill
! \>  background check:     \> marks outliers as suspect or extreme\\
! \>  iterated buddy check: \> compares suspects with neighboring data\\
! \>  pairwise check:       \> eliminates unpaired wind components\\
! \>  profile check:        \> eliminates profiles containing bad data
! \end{tabbing}
!
! The main driver routine {\tt SQC} requires an OBS vector (type obs_vect)
! as input. Apart from the order of the data, only the quality control
! attributes {\tt qcexcl} and {\tt qchist} will be changed on output.
! Data that passed all quality control checks will have {\tt qcexcl}$=0$
! and will be located in the top section of the ODS structure.
! \\[1ex]
! Numerical values and descriptions of all quality control marks produced
! by {\tt SQC}, and by any other quality control procedures, are defined
! in the module {\tt m_odsmeta}.
! \\[1ex]
! {\bf Rules pertaining to quality control marks:}
!
! \bv
!
! 1. Once a quality mark is set to a non-zero value it cannot be changed.
!
! 2. All quality control marks written by SQC are defined in the integer
!    arrays SQCX and SQCH.
!
! 3. Input data with non-zero exclusion marks are not used for any
!    of the quality control checks, and therefore do not affect the
!    SQC decisions.
!
! \ev
!
! !REVISION HISTORY:
!
!  15May2002  Dee       Created this module from psas_qc.f version 1.61
!  31Mar2003 (Dee/Rukh) Minor changes to the buddy check
!  13Oct2020 (Todling)  Store sigO in xvec
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'SQC'
      character(len=*), parameter :: version = '2.1.0 (25Mar2010)'

!     Prescribed obs and fcst error variances
!     ---------------------------------------
      real*8, allocatable :: varO(:)
      real*8, allocatable :: varF(:)

!     Local declarations
!     --------------------
      integer rc, nexcl, nobs, nh, nx, nc

      if (verbose) then
          write(stdout,'(a)') '--------------------------------------------------'
          write(stdout,'(4a)') 'Entering ', myname, ': version ', version
          write(stdout,'(14x,a,i8,a)') 'with ', ntotal, ' observations'
          write(stdout,'(a)') '--------------------------------------------------'
      end if

      npassed = 0
      if ( ntotal .eq. 0 ) then
           call warn ( myname, 'no observations, nothing to do.' )
           return
      end if

!     Reset some qc marks, move already excluded data out of the way
!     --------------------------------------------------------------
      call Init_ ( y, ntotal, nobs )

      if (verbose) write(stdout,'(2a,i8,a)') myname, ': ', &
                        ntotal-nobs, ' observations previously excluded'

      if (nobs==0) return   ! nothing to do

      nexcl = 0
      if (verbose) write(stdout,'(2a,i8,a)') myname, ': ', &
                        nobs-nexcl, ' observations remain'

!     Get datatype information from the resource file
!     -----------------------------------------------
      call RC_ktinfo_ ( )

!     Scale O-Fs for selected datatypes
!     ---------------------------------
      call OmF_scaling_ ( y, nobs, nx, 'scale' )
      nexcl = nexcl + nx
      if (verbose) write(stdout,'(2a,i8,a)') myname, ': excluded ', nx, &
                                ' data with non-positive scaling parameters'
      if (verbose) write(stdout,'(2a,i8,a)') myname, ': ', &
                        nobs-nexcl, ' observations remain'

!     Get varO, varF at data locations
!     --------------------------------
      allocate ( varF(nobs), varO(nobs), stat=rc )
      if (rc/=0) then
          write(stderr,'(2a,i5)') myname,': allocate() error, stat = ', rc
          call die(myname)
      end if

      call Get_ErrVar_ ( nobs, nx, y, varF, varO )
      nexcl = nexcl + nx
      if (verbose) write(stdout,'(2a,i8,a)') myname, ': excluded ', nx, &
                                ' data with undefined error statistics'
      if (verbose) write(stdout,'(2a,i8,a)') myname, ': ', &
                        nobs-nexcl, ' observations remain'

!     Background check
!     ----------------
      call Backg_Check_ ( nobs, nh, nx, y, varF, varO )
      nexcl = nexcl + nx
      if (verbose) write(stdout,'(3a,i8,a)') myname, ': ',       &
                  'background check excluded ', nx, ' extreme outliers'
      if (verbose) write(stdout,'(3a,i8,a)') myname, ': ',       &
                  'background check flagged  ', nh, ' suspect outliers'
      if (verbose) write(stdout,'(2a,i8,a)') myname, ': ', &
                        nobs-nexcl, ' observations remain'

!     Buddy check
!     -----------
      call Buddy_Check_ ( nobs, nx, y, varF, varO )
      nexcl = nexcl + nx
      if (verbose) write(stdout,'(3a,i8,a)') myname, ': ',    &
                  'buddy check excluded ', nx, ' observations'
      if (verbose) write(stdout,'(2a,i8,a)') myname, ': ', &
                        nobs-nexcl, ' observations remain'

!     Store sigO in xvec for diagnostic purposes
!     ------------------------------------------
      where (varO>0.0 .and. y%qcexcl>0)
         y%xvec = sqrt(varO)
      endwhere

!     Done with statistical tests:
!     ---------------------------
      deallocate ( varF, varO )

!     Un-scale O-Fs for selected datatypes
!     ------------------------------------
      call OmF_scaling_ ( y, nobs, nx, 'unscale' )

!     Pairwise check
!     --------------
      call Pairw_Check_ ( nobs, nx, y )
      nexcl = nexcl + nx
      if (verbose) write(stdout,'(3a,i8,a)') myname, ': ',    &
                  'pairwise check excluded ', nx, ' observations'
      if (verbose) write(stdout,'(2a,i8,a)') myname, ': ', &
                        nobs-nexcl, ' observations remain'

!     Profile check
!     -------------
      call Profl_Check_ ( nobs, nx, y )
      nexcl = nexcl + nx
      if (verbose) write(stdout,'(3a,i8,a)') myname, ': ',    &
                  'profile check excluded ', nx, ' observations'
      if (verbose) write(stdout,'(2a,i8,a)') myname, ': ', &
                        nobs-nexcl, ' observations remain'

!     Move clear obs to the top
!     -------------------------
      npassed = nobs - nexcl
      call OBS_MoveUp ( y, nobs, nc, rc, 'qcexcl', (/ 0 /) )
      if (rc/=0) then
          write(stderr,'(2a,i5)') myname,': OBS_MoveUp() error, rc = ', rc
          call die(myname)
      end if

      if (nc/=npassed) then
          write(stderr,'(2a)') myname, ': inconsistent data counts'
          call die(myname)
      end if

      if (verbose) then
          write(stdout,'(a)') '------------------------------------------------------'
          write(stdout,'(4a)') 'Leaving ', myname, ' version ', version
          write(stdout,'(a,i8,a)') 'Returning ', npassed, &
                                   ' quality-controlled observations'
          write(stdout,'(a,i8,a)') '          ', nexcl, &
                                   ' observations rejected by SQC checks'
          write(stdout,'(a,i8,a)') '          ', ntotal-nobs, &
                                   ' previously rejected observations'
          write(stdout,'(a)') '------------------------------------------------------'
      end if

      end subroutine SQC
!     ------------------

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Init_ () --- Initialize SQC
!
! !INTERFACE:
!
      subroutine Init_ ( y, ntotal, nobs )


! !INPUT PARAMETERS:
!
      integer,          intent(in) :: ntotal    ! number of observations


! !INPUT/OUTPUT PARAMETERS:
!
      type(obs_vect),   intent(inout) :: y      ! observation vector


! !OUTPUT PARAMETERS:
!
      integer, intent (out) ::  nobs  ! number of obs not yet excluded

!
! !DESCRIPTION: This routine checks the input data, optionally resets
!               certain QC marks, and moves data that have not yet
!               been excluded to the front.
!
! !REVISION HISTORY:
!
!  15May02 (Dee)  Initial code
!  06Jan03 (Todling)  Put bounds when applying where statement
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'Init_'

!     data selection:
!     --------------
      logical reset_allqc   ! reset all nonzero qc marks on input
      logical reset_sqc     ! reset all sqc marks on input
      logical reset_passive ! reset all passive qc marks on input

      integer nch, ncx, i, rc

!     Pointers to data attributes:
!     ---------------------------
      integer, pointer :: qcx(:)      ! exclusion mark
      integer, pointer :: qch(:)      ! history mark

      qcx    => y%qcexcl
      qch    => y%qchist

!     Get reset parameters from the resource file
!     -------------------------------------------
      call RC_resets_ ( )

      if (reset_allqc) then  ! reset all qc marks

          ncx = count( qcx(1:ntotal)/=0 )
          qcx(1:ntotal) = 0
          if ( ncx>0 .AND. verbose ) &
                      write(stdout,'(3a,i8,a)') myname, ': ',     &
                             'reset ', ncx, ' exclusion marks'

          nch = count( qch(1:ntotal)/=0 )
          qch(1:ntotal) = 0
          if ( nch>0 .AND. verbose ) &
                      write(stdout,'(3a,i8,a)') myname, ': ',     &
                             'reset ', nch, ' history marks'

          nobs = ntotal
          return

      end if

      if (reset_sqc) then    ! reset all sqc marks

          nch = 0
          do i = 1, size(SQCH)
             nch = nch + count( qch(1:ntotal)==SQCH(i) )
             where ( qch(1:ntotal)==SQCH(i) ) qch(1:ntotal) = 0
          end do
          if ( nch>0 .AND. verbose ) &
                      write(stdout,'(3a,i8,a)') myname, ': ',     &
                             'reset ', nch, ' sqc history marks'

          ncx = 0
          do i = 1, size(SQCX)
             ncx = ncx + count( qcx(1:ntotal)==SQCX(i) )
             where ( qcx(1:ntotal)==SQCX(i) ) qcx(1:ntotal) = 0
          end do
          if ( ncx>0 .AND. verbose ) &
                      write(stdout,'(3a,i8,a)') myname, ': ',     &
                             'reset ', ncx, ' sqc exclusion marks'

      end if

      if (reset_passive) then  ! reset all passive marks

          ncx = count( qcx(1:ntotal)==X_PASSIVE )
          where ( qcx(1:ntotal)==X_PASSIVE ) qcx(1:ntotal) = 0
          if ( ncx>0 .AND. verbose ) &
                      write(stdout,'(3a,i8,a)') myname, ': ',    &
                             'reset ', ncx, ' passive data marks'

      end if

!     Move data that have not yet been excluded to front of list:
!     ----------------------------------------------------------
      call OBS_MoveUp ( y, ntotal, nobs, rc, 'qcexcl', (/ 0 /) )
      if (rc/=0) then
          write(stderr,'(2a,i5)') myname,': OBS_MoveUp() error, rc = ', rc
          call die(myname)
      end if

      CONTAINS   !................ internals follow ........................

         subroutine RC_resets_ ()

!        Get reset parameters from the rc file
!        -------------------------------------

         character(len=255) token
         integer iret

!        Load resources
!        --------------
         call i90_loadf (SQCRC, iret)
         if (iret/=0) then
            write(stderr,'(2a,i5,2a)') myname, ': I90_loadf error, iret =', iret, &
                                               ': trying to load ', SQCRC
            call die(myname)
         end if

         reset_allqc = .FALSE.
         call I90_label('reset_allqc:', iret)
         if (iret/=0) then
            write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                            ': trying to read ', 'reset_allqc:'
            call die(myname)
         end if
         call I90_GToken(token, iret )
         if (iret/=0) then
            write(stderr,'(2a,i5)') myname, ': I90_GToken error, iret=', iret
            call die(myname)
         end if
         if (token(1:1)=='y') reset_allqc = .TRUE.
         write(stdout,'(2a,L1)') myname, ': reset_allqc = ', reset_allqc

         reset_sqc = .TRUE.
         call I90_label('reset_sqc:', iret)
         if (iret/=0) then
            write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                            ': trying to read ', 'reset_sqc:'
            call die(myname)
         end if
         call I90_GToken(token, iret )
         if (iret/=0) then
            write(stderr,'(2a,i5)') myname, ': I90_GToken error, iret=', iret
            call die(myname)
         end if
         if (token(1:1)=='n') reset_allqc = .FALSE.
         write(stdout,'(2a,L1)') myname, ': reset_sqc = ', reset_sqc

         reset_passive = .FALSE.
         call I90_label('reset_passive:', iret)
         if (iret/=0) then
            write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                            ': trying to read ', 'reset_passive:'
            call die(myname)
         end if
         call I90_GToken(token, iret )
         if (iret/=0) then
            write(stderr,'(2a,i5)') myname, ': I90_GToken error, iret=', iret
            call die(myname)
         end if
         if (token(1:1)=='y') reset_allqc = .TRUE.
         write(stdout,'(2a,L1)') myname, ': reset_passive = ', reset_passive

!        release resource file:
!        ---------------------
         call I90_release()

         end subroutine RC_resets_
!        -------------------------

     end subroutine Init_
!    --------------------


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: RC_ktinfo_() --- Read 'allowable_kts::' table from the
!                             resource file
!
! !INTERFACE:
!
      subroutine RC_ktinfo_ ()

!
! !REVISION HISTORY:
!
!  15May02 (Dee)  Initial code
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'RC_ktinfo_'
      character(len=*), parameter :: tablename = 'allowable_kts::'

      character(len=255) token
      integer iret, kta

!     Initialize table entries
!     ------------------------
      scaling(1:ktmax) = 'NONE'  ! O-F scaling method
      sigsrc (1:ktmax) = 'NONE'  ! source for varO, varF
      ls_h   (1:ktmax) = 0.0     ! horizontal length scales
      ls_v   (1:ktmax) = 0.0     ! vertical length scales

!     Load resources
!     --------------
      call i90_loadf (SQCRC, iret)
      if (iret/=0) then
         write(stderr,'(2a,i5,2a)') myname, ': I90_loadf error, iret =', iret, &
                                            ': trying to load ', SQCRC
      end if

!     Read table with datatypes and omf statistics:
!     --------------------------------------------
      call I90_label(tablename, iret)
      if (iret/=0) then
         write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                         ': trying to read ', tablename
         call die(myname)
      end if
      do while (iret==0)     ! read table entries
         call I90_GLine ( iret ) ! iret=-1: end of file; +1: end of table
         if (iret==0) then   ! OK, we have next row of table

             kta = I90_GInt( iret )    ! allowable kt
             if (iret/=0) then
                 write(stderr,'(2a,i5)') myname, ': I90_GInt error, iret=', iret
                 call die(myname)
             end if
             if (kta < 0 .OR. kta > ktmax) then
                 write(stderr,'(2a,i5)') myname, ': invalid kt value: kt = ', kta
                 call die(myname)
             end if

             if (verbose) write(stdout,'(2a,i3)') myname, &
                                       ': allowable kt = ', kta

             call I90_GToken(scaling(kta), iret )   ! scaling method
             if (iret/=0) then
                 write(stderr,'(2a,i5)') myname, ': I90_GToken error, iret=', iret
                 call die(myname)
             end if

             if (verbose) write(stdout,'(3a)') myname, &
                                       ':    scaling method: ', scaling(kta)

             call I90_GToken(sigsrc(kta), iret )   ! source for sigs
             if (iret/=0) then
                 write(stderr,'(2a,i5)') myname, ': I90_GToken error, iret=', iret
                 call die(myname)
             end if

             if (verbose) write(stdout,'(3a)') myname, &
                                       ':    error std dev from: ', sigsrc(kta)

             ls_h(kta) = I90_GFloat( iret )     ! horizontal length scale
             if (iret/=0) then
                 write(stderr,'(2a,i5)') myname, ': I90_GFloat error, iret=', iret
                 call die(myname)
             end if

             if (verbose) write(stdout,'(2a,e10.2)') myname, &
                                       ':    horizontal length scale = ', ls_h(kta)

             ls_v(kta) = I90_GFloat( iret )     ! vertical length scale
             if (iret/=0) then
                 write(stderr,'(2a,i5)') myname, ': I90_GFloat error, iret=', iret
                 call die(myname)
             end if

             if (verbose) write(stdout,'(2a,e10.2)') myname, &
                                       ':    vertical length scale = ', ls_v(kta)

         end if
      end do

!     release resource file:
!     ---------------------
      call I90_release()

      end subroutine RC_ktinfo_
!     ----------------------


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: OmF_scaling_() --- Scale O-F for selected datatypes
!
! !INTERFACE:
!
      subroutine OmF_scaling_( y, nobs, nexcl, which )

!
! !INPUT PARAMETERS:
!
      integer,          intent(in) :: nobs      ! number of observations
      character(len=*), intent(in) :: which     ! 'scale' or 'unscale'
!
! !INPUT/OUTPUT PARAMETERS:
!
      type(obs_vect),   intent(inout) :: y      ! observation vector
!
! !OUTPUT PARAMETERS:
!
      integer, intent (out) ::  nexcl    ! number of exclusions due to
                                         !   bad (non-positive) xm value
!
! !DESCRIPTION: This routine (un)scales the OmF attribute by the xm value,
!         making sure that xm is numerically positive.
!
!
! !REVISION HISTORY:
!
!  15May02 (Dee)  Initial code
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'OmF_scaling_'

      integer i, kta, ns, rc

!     Pointers to data attributes:
!     --------------------------
      integer, pointer :: kt(:)       ! data type index
      real*8           :: OmF(nobs)   ! obs-minus-fcst (O-F)
      real*8           :: xm(nobs)    ! metadata
      integer, pointer :: qcx(:)      ! exclusion mark

      kt     => y%kt
      OmF(1:nobs) = y%OmF(1:nobs)
      xm(1:nobs)  = y%xm(1:nobs)
      qcx    => y%qcexcl

      nexcl = 0

!     Scale or unscale omf's according to 'scaling' resource:
!     ------------------------------------------------------
      do kta = 1, ktmax

         if ( index(scaling(kta),'xm')==1 ) then

            select case (which)

            case ('scale')

               ns = 0
               do i = 1, nobs
                  if ( kt(i) == kta ) then
                     if ( xm(i) > tol_rel*abs(omf(i)) ) then
                          omf(i) = omf(i) / xm(i)
                          ns     = ns + 1
                     else
                          qcx(i) = X_BADXM
                          nexcl  = nexcl + 1
                     end if
                  end if
               end do

               if (verbose) write(stdout,'(2a,i8,a,20i4)') myname,    &
                   ': scaled O-F by xm value for ', ns,   &
                   ' observations with kt=', kta

            case ('unscale')

               ns = 0
               do i = 1, nobs
                  if ( kt(i) == kta ) then
                     if ( qcx(i) /= X_BADXM ) then
                          omf(i) = omf(i) * xm(i)
                          ns     = ns + 1
                     end if
                  end if
               end do

               if (verbose) write(stdout,'(2a,i8,a,20i4)') myname,    &
                   ': unscaled O-F by xm value for ', ns,   &
                   ' observations with kt=', kta

            end select

         end if

      end do


      end subroutine OmF_scaling_
!     ---------------------------


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get_ErrVar_() --- Get error variances at obs locations
!
! !INTERFACE:
!
      subroutine Get_ErrVar_( nobs, nexcl, y, varF, varO )

!
! !INPUT PARAMETERS:
!
      integer,          intent(in) :: nobs   ! number of observations
!
! !INPUT/OUTPUT PARAMETERS:
!
      type(obs_vect),   intent(inout) :: y   ! observation vector
!
! !OUTPUT PARAMETERS:
!
      real*8,    intent (out) ::  varF(nobs)   ! prescribed fcst error variances
      real*8,    intent (out) ::  varO(nobs)   ! prescribed obs  error variances
      integer, intent (out) ::  nexcl        ! number of exclusions due to
                                             !   undefined error statistics
!
! !REVISION HISTORY:
!
!  15May02 (Dee)  Initial code
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'Get_ErrVar_'

      real*8,    allocatable :: sigOc(:)
      integer, allocatable :: indx(:)

      integer ktPSAS(ktmax)
      integer rc, kta, np, nmoved
      integer i, j, jmin
      real*8 dlnp, dmin, lnp

!     lev, varO, varF tables
!     ----------------------
      integer, parameter :: ntblmax = 30  ! max table length
      real*8 tbl_lev (ntblmax)
      real*8 tbl_sigO(ntblmax)
      real*8 tbl_sigF(ntblmax)
      integer ntbl                        ! actual table length

!     Pointers to obs attributes:
!     --------------------------
      integer, pointer :: kt(:)       ! data type index
      integer, pointer :: kx(:)       ! data source index
      real*8  :: lon(nobs)      ! longitude of obs (degrees)
      real*8  :: lat(nobs)      ! latitute of obs (degrees)
      real*8  :: lev(nobs)      ! pressure level of obs (hPa)
      integer, pointer :: qcx(:)      ! exclusion mark

      kt     => y%kt
      kx     => y%kx
      lon(1:nobs) = y%lon(1:nobs)
      lat(1:nobs) = y%lat(1:nobs)
      lev(1:nobs) = y%lev(1:nobs)
      qcx    => y%qcexcl

      call zeit_ci ( myname )   ! timing call

!     Initialize to zero
!     ------------------
      varF = 0.0
      varO = 0.0

!     First handle all kts with sigmas defined by PSAS:
!     ------------------------------------------------
      np = 0
      do kta = 1, ktmax
         if ( index(sigsrc(kta),'PSAS') > 0 ) then
              np = np + 1
              ktPSAS(np) = kta
         end if
      end do

!     Move those data up...
!     ---------------------
      nmoved = 0
      if ( np > 0 ) then

           call OBS_MoveUp ( y, nobs, nmoved, rc, 'kt', ktPSAS(1:np) )
           if (rc /= 0) then
               write(stderr,'(2a,i5)') myname,': OBS_MoveUp() error, rc = ', rc
               call die(myname)
           end if

      end if

!     ...and get the sigmas from PSAS:
!     -------------------------------
      if ( nmoved > 0 ) then

           allocate ( sigOc(nmoved), stat=rc )
           if (rc/=0) then
               write(stderr,'(2a,i5)') myname,': allocate() error, stat = ', rc
               call die(myname)
           end if

!          Note: PSAS_sigs returns sigF (in varF), sigOc, sigO (in VarO)
!          -------------------------------------------------------------
           call PSAS_sigs ( nmoved, lat, lon, lev, kx, kt, varF, sigOc, varO )

!          Compute variances:
!          -----------------
           do i = 1, nmoved
              if ( varF(i)>0.0 .AND. varO(i)>0.0 )  then
                   varF(i) = varF(i)**2
                   if ( sigOc(i)>0.0 ) then
                        varO(i) = varO(i)**2 + sigOc(i)**2
                   else
                        varO(i) = varO(i)**2
                   end if
              end if
           end do

           deallocate ( sigOc )

           if (verbose) write(stdout,'(2a,i8,a,20i4)') myname,    &
                ': obtained varO, varF from   PSAS for ', nmoved,   &
                ' observations with kt=', ktPSAS(1:np)

      end if

!     Next handle each kt with sigmas defined in SQC resource file:
!     ------------------------------------------------------------
      allocate ( indx(nobs), stat=rc )
      if (rc/=0) then
          write(stderr,'(2a,i5)') myname,': allocate() error, stat = ', rc
          call die(myname)
      end if

      do kta = 1, ktmax

         nmoved = 0
         if ( index(sigsrc(kta),'NONE') == 0 .AND. &
              index(sigsrc(kta),'PSAS') == 0       ) then

!           Move up any obs with this kt...
!           -------------------------------
            call OBS_MoveUp ( y, nobs, nmoved, rc, 'kt', (/ kta /), indx )
            if (rc /= 0) then
                write(stderr,'(2a,i5)') myname,': OBS_MoveUp() error, rc = ', rc
                call die(myname)
            end if

         end if

         if ( nmoved > 0 ) then

!           ...move variances along with the other attributes...
!           ----------------------------------------------------
            varO = varO( (/ (indx(i), i=1,nobs) /) )
            varF = varF( (/ (indx(i), i=1,nobs) /) )

!           ...get sig table from resource file for this kt...
!           --------------------------------------------------
            call RC_sigs_ ( trim(sigsrc(kta))//'_sigs::' )

!           ...perform 'nearest in log(p)' table lookup
!           -------------------------------------------
            if ( ntbl == 1 ) then  ! special case: single level

               varF(1:nmoved) = tbl_sigF(1)**2
               varO(1:nmoved) = tbl_sigO(1)**2

            else                   ! multiple levels

               do i = 1,nmoved

                  dmin = 1e6 ! practically infinite
                  lnp = log ( lev(i) )
                  do j = 1, ntbl
                     dlnp = abs(lnp - log(tbl_lev(j)))
                     if (dlnp<dmin) then
                         jmin = j
                         dmin = dlnp
                     end if
                  end do

                  varF(i) = tbl_sigF(jmin)**2
                  varO(i) = tbl_sigO(jmin)**2

               end do

            end if

            if (verbose) write(stdout,'(4a,i8,a,i3)') myname,            &
                 ': obtained varO, varF from ', SQCRC, ' for ', nmoved,  &
                 ' observations with kt=', kta

         end if

      end do

      deallocate ( indx )

!     Exclude any data with undefined error statistics:
!     ------------------------------------------------
      nexcl = 0
      do i = 1, nobs
         if ( varF(i)<=0.0 .OR. varO(i)<=0.0 ) then
              qcx(i) = X_NOSTATS
              nexcl = nexcl + 1
         end if
      end do

      call zeit_co ( myname )   ! timing call

      CONTAINS   !................ internals follow ........................

         subroutine RC_sigs_ ( tablename )

!        Get sigO, sigF table from resource file

         character(len=*), intent (in) :: tablename

         integer iret

!        Load resources
!        --------------
         call i90_loadf (SQCRC, iret)
         if (iret/=0) then
            write(stderr,'(2a,i5,2a)') myname, ': I90_loadf error, iret =', iret, &
                                               ': trying to load ', SQCRC
            call die(myname)
         end if

!        Read table with datatypes and omf statistics:
!        --------------------------------------------
         call I90_label(tablename, iret)
         if (iret/=0) then
            write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                            ': trying to read ', tablename
            call die(myname)
         end if
         ntbl = 0
         do while (iret==0)     ! read table entries
            call I90_GLine ( iret ) ! iret=-1: end of file; +1: end of table
            if (iret==0) then   ! OK, we have next row of table

                ntbl = ntbl + 1
                if ( ntbl > ntblmax ) then
                     write(stderr,'(4a)') myname, ': increase ntblmax', &
                                  ': not enough space for ', tablename
                     call die(myname)
                end if

                tbl_lev(ntbl)  = I90_GFloat( iret )     ! level
                if (iret/=0) then
                    write(stderr,'(2a,i5)') myname, ': I90_GFloat error, iret=', iret
                    call die(myname)
                end if

                tbl_sigO(ntbl) = I90_GFloat( iret )     ! sigO
                if (iret/=0) then
                    write(stderr,'(2a,i5)') myname, ': I90_GFloat error, iret=', iret
                    call die(myname)
                end if

                tbl_sigF(ntbl) = I90_GFloat( iret )     ! sigF
                if (iret/=0) then
                    write(stderr,'(2a,i5)') myname, ': I90_GFloat error, iret=', iret
                    call die(myname)
                end if

            end if
         end do

!        release resource file:
!        ---------------------
         call I90_release()

         end subroutine RC_sigs_
!        -----------------------

      end subroutine Get_ErrVar_
!     ------------------------


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Backg_Check_ () --- Checks for statistical outliers
!
! !INTERFACE:
!
      subroutine Backg_Check_ ( nobs, nsuspect, nexcl, y, varF, varO )

!
! !INPUT PARAMETERS:
!
      integer, intent(in)    ::  nobs       ! number of observations
      real*8, intent (in)      ::  varF(nobs) ! prescribed fcs error variance
      real*8, intent (in)      ::  varO(nobs) ! prescribed obs error variance
!
! !INPUT/OUTPUT PARAMETERS:
!
      type(obs_vect),   intent(inout) :: y       ! observation vector
!
! !OUTPUT PARAMETERS:
!
      integer , intent (out) ::    nsuspect ! number of obs marked suspect
      integer , intent (out) ::    nexcl    ! number of obs rejected
!
! !DESCRIPTION: Marks observations for which the squared
!       observed-minus-forecast residual exceeds a pre-defined
!       multiple of the presumed variance of the residual, viz.
! $$
!    \delta^2 \equiv ( w_o - w_f )^2 \ge \tau^2 (\sigma_o^2 + \sigma_f^2 )
! $$
! where $w_{o,f}$ are the (observation,background) vectors, and
! $\sigma_{o,f}$ their presumed standard deviations.
! Sets history mark if the inequality holds with $\tau$ = $\tau$\_bgh;
! exclusion mark if the inequality holds with $\tau$ = $\tau$\_bgx.
!
! !REVISION HISTORY:
!
!  04Feb99 (Dee)  See v1.1 revision history for previous revisions
!  01Mar00 (Dee)  Minor changes for v1.5
!  15May02 (Dee)  Upgraded for SQC v2.00
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'Backg_Check_'

!     Background check resource parameters:
!     ------------------------------------
      real*8 tau_bgx  ! tolerance for extreme outliers
      real*8 tau_bgh  ! tolerance for suspect outliers

      integer i
      real*8 tau, var, zero

!     Pointers to obs attributes:
!     --------------------------
      real*8 :: OmF(nobs)      ! obs-minus-fcst (O-F)
      integer, pointer :: qcx(:)      ! exclusion mark
      integer, pointer :: qch(:)      ! history mark

      OmF(1:nobs) =  y%omf(1:nobs)
      qcx    => y%qcexcl
      qch    => y%qchist

      nsuspect = 0
      nexcl = 0

      if (nobs==0) return  ! nothing to do

      call zeit_ci ( myname )   ! timing call

!     Get resource parameters
!     -----------------------
      call RC_bkg_ ( )

      zero = tol_rel**2

      do i = 1, nobs

         if ( qch(i)==0 .AND. qcx(i)==0 ) then  ! test unmarked data only

!           check for near-zero variances:

            var = varF(i) + varO(i)
            if ( var<zero ) then
                write(stderr,'(2a)') myname,      &
                   ': Prescribed O-F variance is numerically zero.'
                call die(myname)
            end if

            tau = sqrt(OmF(i)**2/var)

!           extreme outlier test:

            if     ( tau>tau_bgx ) then

                     qcx(i)   = X_BACKG
                     nexcl    = nexcl + 1

!           suspect outlier test:

            elseif ( tau>tau_bgh ) then

                     qch(i)   = H_BACKG
                     nsuspect = nsuspect + 1

            end if

         end if

      end do

      call zeit_co ( myname )   ! timing call

      CONTAINS   !................ internals follow ........................

         subroutine RC_bkg_ ( )

!        Get varO, varF table from resource file

         integer iret

!        Load resources
!        --------------
         call i90_loadf (SQCRC, iret)
         if (iret/=0) then
             write(stderr,'(2a,i5,2a)') myname, ': I90_loadf error, iret =', iret, &
                                                ': trying to load ', SQCRC
             call die(myname)
         end if

         call I90_label('tau_bgh:', iret)
         if (iret/=0) then
             write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                                ': trying to read ', 'tau_bgh:'
             call die(myname)
         end if
         tau_bgh = I90_GFloat(iret)
         if (iret/=0) then
             write(stderr,'(2a,i5)') myname, ': I90_GFloat error, iret=', iret
             call die(myname)
         end if
         write(stdout,'(2a,f6.2)') myname, ': tau_bgh = ',tau_bgh

         call I90_label('tau_bgx:', iret)
         if (iret/=0) then
             write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                                ': trying to read ', 'tau_bgx:'
             call die(myname)
         end if
         tau_bgx = I90_GFloat(iret)
         if (iret/=0) then
             write(stderr,'(2a,i5)') myname, ': I90_GFloat error, iret=', iret
             call die(myname)
         end if
         write(stdout,'(2a,f6.2)') myname, ': tau_bgx = ',tau_bgx

!        Release resource file:
!        ---------------------
         call I90_release()

         end subroutine RC_bkg_
!        ----------------------

      end subroutine Backg_Check_
!     --------------------------------

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Buddy_Check_() --- Iterative, adaptive buddy check
!
! !INTERFACE:
!
      subroutine Buddy_Check_ ( nobs, nfailed, y, varF, varO )

!
! !INPUT PARAMETERS:
!
      integer, intent(in)    ::  nobs       ! number of observations
!
! !INPUT/OUTPUT PARAMETERS:
!
      type(obs_vect), intent (inout) :: y   ! observation vector
      real*8, intent (inout)   ::  varF(nobs) ! prescribed fcs error variance
      real*8, intent (inout)   ::  varO(nobs) ! prescribed obs error variance
!
! !OUTPUT PARAMETERS:
!
      integer , intent (out) ::    nfailed  ! number of obs rejected
!
! !DESCRIPTION: Checks each suspect observation (with any nonzero history mark)
!  against its "buddies" (nearby observations of the same variable with a zero
!  history mark). If a suspect observation passes the buddy check, it joins the
!  pool of buddies for the next round.
!  This procedure is iterated until the pool stabilizes, or until a maximum
!  number of iterations is reached.
!
!  The buddy check for each suspect observation comprises two steps. First,
!  the suspect O-F is predicted from its buddies. In the current implementation
!  this is simply done by taking a weighted average of the buddies, using
!  weights that decrease exponentially with distance (horizontal as well as
!  vertical).
!
!  Second, the suspect O-F is compared to the predicted O-F and passes the
!  buddy check when it is sufficiently close; i.e. when
! $$
!  ( \delta - \delta^* )^2 \le \tau^2 \alpha (\sigma_o^2 + \sigma_f^2 )
! $$
!  where $\delta^*$ is the predicted O-F, $\tau$ is a trimming
!  parameter, and $\alpha$ is a scale factor computed from the buddies (see
!  Dee etal 2001). The scale factor adjusts the prescribed O-F variance
!  in order to account for local variability of the actual forecast errors,
!  and this renders the procedure adaptive.
!
! !REVISION HISTORY:
!
!  04Feb99 (Dee)     See v1.1 revision history for previous revisions
!  01Mar00 (Dee)     All observations processed in one single call
!  12Feb01 (Kokron)  Added open MP directives
!  15May02 (Dee)     Upgraded for SQC v2.00
!  07Jun02 (Todling) - Replaced sepang with Separation (per T. Clune)
!                    - Added initialization/clean calls to Partitioner
!  31Mar03 (Dee/Rukh) Make less adaptive when few buddies;
!                     introduce search_rad as resource parameter
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'Buddy_Check_'

!     Buddy check resource parameters:
!     -------------------------------
      real*8 tau_buddy            ! tolerance parameter
      real*8 seplim               ! look for buddies in regions separated
                                !      by seplim (deg) or less
      real*8 search_rad           ! look for buddies within search_rad length scales
      integer nstar             ! damping parameter for variance scaling
      integer nbuddy_max        ! maximum number of buddies
      integer niter_max         ! max number of iterations

!     Cartesian coordinates on unit sphere
!     ------------------------------------
      real*8, allocatable :: xobs(:)
      real*8, allocatable :: yobs(:)
      real*8, allocatable :: zobs(:)

!     Region pointers
!     ---------------
      Type (Spherical_Partition) :: partition
      integer, allocatable :: iregbeg(:)     ! Pointer to first ob in region
      integer, allocatable :: ireglen(:)     ! No. of obs in region

!     Index vector for sorts (cannot be allocatable because of OMP)
!     -------------------------------------------------------------
      integer indx(nobs)

!     List of suspect observations
!     ----------------------------
      integer, allocatable :: ki_susp(:)  ! index of suspect obs
      integer, allocatable :: kr_susp(:)  ! region to which it belongs

!     List of buddies
!     ---------------
      integer :: ki_buddy(nobs) ! index of buddy
      real*8    :: wt_buddy(nobs) ! weight of buddy

!     Suspect and reaccept flags
!     --------------------------
      logical, allocatable :: issuspect(:)  ! .TRUE. if suspect
      logical, allocatable :: reaccept(:)   ! .TRUE. if reaccepted

!     Accumulators
!     ------------
      real*8 :: accum_del ! sum O-F's
      real*8 :: accum_wgt ! sum weights
      real*8 :: accum_de2 ! sum O-F squares
      real*8 :: accum_var ! sum O-F variances

!     Counters
!     --------
      integer   n_susp                    ! no. suspect obs.
      integer   nbuddy                    ! no. of buddies found
      integer   n_reacc                   ! no. obs re-accepted

      real*8      del_star                ! O-F predicted from buddies
      real*8      alpha                   ! Dee-Cats variance scaling

!     Minor locals
!     ------------
      integer   rc, iter, ireg, ibeg, iend, ilen, i, is, ib, ibb, j
      integer   kis, kts, krs, niter
      integer   maxreg                ! number of PSAS regions
      real*8      tol2, exponent, scgain

      real*8      dist2, z_dist2

      integer, pointer :: kt(:)       ! data type index
      integer, pointer :: kx(:)       ! data source index
      integer, pointer :: ks(:)       ! sounding index
      real*8   :: lon(nobs)      ! longitude of obs (degrees)
      real*8   :: lat(nobs)      ! latitute of obs (degrees)
      real*8   :: lev(nobs)      ! pressure level of obs (hPa)
      real*8   :: OmF(nobs)      ! obs-minus-fcst (O-F)
      integer, pointer :: qcx(:)      ! exclusion mark
      integer, pointer :: qch(:)      ! history mark

      logical :: single_level = .FALSE.  ! search only obs at same level
      real*8  :: lvs 

!     Statement function: chordal distance squared
!     --------------------------------------------
      dist2(i,j) = (radius_earth**2) * ( (xobs(i)-xobs(j))**2 +   &
                                         (yobs(i)-yobs(j))**2 +   &
                                         (zobs(i)-zobs(j))**2   )

!     Statement function: vertical distance squared
!     ---------------------------------------------
      z_dist2(i,j) = ( Z_scale * log(lev(i)/lev(j)) )**2

!     Pointers to obs attributes:
!     --------------------------
      kt     => y%kt
      kx     => y%kx
      ks     => y%ks
      lon(1:nobs)    = y%lon(1:nobs)
      lat(1:nobs)    = y%lat(1:nobs)
      lev(1:nobs)    = y%lev(1:nobs)
      OmF(1:nobs)    = y%OmF(1:nobs)
      qcx    => y%qcexcl
      qch    => y%qchist

!     If analyzing AOD, do single level buddy check (for now)
!     Note: this speeds things up
!     -------------------------------------------------------
      single_level = any(qcx==0 .AND. (kt==ktAOD .OR. kt==ktLogAOD) )
      if (verbose .and. single_level) &
          write(stdout,'(a)') myname//': running in SINGLE LEVEL mode'

!     Let's get started...
!     -----------------
      nfailed = 0
      if ( nobs==0 )  return    ! nothing to do

      call zeit_ci ( myname )   ! timing call

!     Get resource parameters
!     -----------------------
      call RC_buddy_ ( )

!     Initialize Partitioner
!     ----------------------
      call Initialize ( n_levels=0, partition=partition, compress=.true. )
      maxreg = NumberOfRegions(partition)

!     Allocate region arrays
!     ----------------------
      allocate ( iregbeg(maxreg), ireglen(maxreg), &
                 stat=rc )
      if ( rc/=0 ) then
          write(stderr,'(2a,i5)') myname,': allocate(reg) error, stat =', rc
          call die(myname)
      end if

!     Sort data (including varO, varF) by region:
!     ------------------------------------------
      allocate ( xobs(nobs), yobs(nobs), zobs(nobs), &
                 stat=rc )
      if (rc/=0) then
          write(stderr,'(2a,i5)') myname,': allocate(xyz) error, stat = ', rc
          call die(myname)
      end if

      call Region_Sort_ ( ) 

      if (verbose) write(stdout,'(2a)') myname, ': sorted data by region'

!     Allocate local memory
!     ---------------------
      allocate ( ki_susp(nobs), kr_susp(nobs), issuspect(nobs),  &
                 reaccept(nobs), stat=rc )
      if ( rc/=0 ) then
          write(stderr,'(2a,i5)') myname,': allocate(susp) error, stat =', rc
          call die(myname)
      end if

!     Initialize suspect and reaccept flags. The suspect flag will be
!     be reset at the beginning of each iteration to indicate which
!     data are still suspect. The reaccept flag may be set during an
!     iteration to indicate that a suspect flag will be changed at
!     the beginning of the next iteration. Note that reaccept flags
!     only apply to data which are currently suspect.
!     ---------------------------------------------------------------
      issuspect = .FALSE.
      where ( qch(1:nobs)>0 .AND. qcx(1:nobs)==0 ) issuspect(1:nobs) = .TRUE.
      reaccept  = .FALSE.

!     Iterate buddy check...
!     ----------------------
      do iter = 1, niter_max

         niter = iter

!        Prepare list of suspect data
!        ----------------------------
         n_susp = 0
         do ireg = 1, maxreg

            if ( ireglen(ireg)>0 ) then  ! only if data in region

!              Look for suspect data in this region
!              ------------------------------------
               ibeg = iregbeg(ireg)
               iend = ibeg + ireglen(ireg) - 1
               do i = ibeg, iend
                  if ( issuspect(i) ) then
                     n_susp = n_susp + 1
                    ki_susp(n_susp) = i
                    kr_susp(n_susp) = ireg
                  end if
               end do

            end if

         end do

!        Nothing to do (no suspect obs found)
!        ------------------------------------
         if ( n_susp==0 ) goto 900         ! sorry ...

!        Now do buddy check for each suspect obs
!        ---------------------------------------
         if (verbose) write(stdout,'(3a,i2,a,i8,a)') myname, ': ',  &
                               'starting iteration ',iter,         &
                               ' with ', n_susp, ' suspect observations'

         n_reacc = 0

!$omp  parallel do                                                &
!$omp  default(shared),                                           &
!$omp  private(is,kis,kts,krs,nbuddy,ireg,ibeg,iend,i,exponent,   &
!$omp          scgain,ki_buddy,wt_buddy,indx,accum_del,accum_de2, &
!$omp          accum_wgt,accum_var,ib,ibb,del_star,alpha,tol2),   &
!$omp  reduction(+:n_reacc)

         do is = 1, n_susp      ! for each suspect obs

            kis = ki_susp(is)   ! this suspect's index
            kts = kt(kis)       ! this suspect's kt
            krs = kr_susp(is)   ! this suspect's region
            lvs = lev(kis)      ! this suspect's level

!           Find all candidate buddies
!           --------------------------
            nbuddy = 0
            do ireg = 1, maxreg

               if ((Separation(GetRegion(ireg,partition),GetRegion(krs,partition),SEPANG_MIN) <= seplim) &
                    .and. (ireglen(ireg)   >0     ) ) then  ! nearby region with data

                  ibeg = iregbeg(ireg)
                  iend = ibeg + ireglen(ireg) - 1

                  do i = ibeg, iend        ! look within a region

!                    Skip other levels if doing single level
                     if ( single_level .AND. (abs(lvs-lev(i))>tol_rel*lvs) ) cycle

                     ! only if same data type, not suspect, not excluded:

                     if ( .not. issuspect(i) .AND. qcx(i)==0 .AND.  &
                                                  kt(i)==kts) then

                        exponent = dist2(kis,i)/ls_h(kts)**2

                        if ( ls_v(kts)>tol_rel ) then     ! upper-air data
                             exponent = exponent + z_dist2(kis,i)/ls_v(kts)**2
                        end if

                        ! only if not too far (search_rad length scales):

                        if ( exponent<search_rad**2 ) then

                        ! found a candidate buddy:

                             nbuddy = nbuddy + 1
                             ki_buddy(nbuddy) = i

                        ! associate weight with this candidate:

                             scgain = varF(i) / (varF(i) + varO(i))
                             wt_buddy(nbuddy) =  scgain * exp(-0.5*exponent)

                        end if      ! not too far

                     end if      ! same data type, not suspect

                  end do      ! within a region

               end if      ! nearby regions

            end do      ! over regions

            if ( nbuddy>0 ) then

!              Find the best buddies
!              ---------------------
               call IndexSet  ( nbuddy, indx )
               call IndexSort ( nbuddy, indx, wt_buddy, descend=.true. )

               nbuddy = min(nbuddy, nbuddy_max)      ! pick highest weights

!              Do buddy check
!              --------------
               accum_del = 0.0
               accum_de2 = 0.0
               accum_wgt = 0.0
               accum_var = 0.0

               do ib = 1, nbuddy       ! accumulate weights, variances, etc.

                  ibb = indx(ib)       ! index in ki_buddy of best buddy
                  i = ki_buddy(ibb)    ! index in OmF of best buddy

                  accum_del = accum_del + OmF(i) * wt_buddy(ibb)
                  accum_wgt = accum_wgt + wt_buddy(ibb)
                  accum_de2 = accum_de2 + (OmF(i))**2
                  accum_var = accum_var + varO(i) + varF(i)

               end do

               del_star = sngl(accum_del/accum_wgt)    ! prediction

               alpha = sngl(accum_de2/accum_var)    ! variance scale factor

               if (nbuddy < nstar) then   ! handle cases with few buddies
                   alpha = (nstar + nbuddy*alpha)/(nstar + nbuddy)  ! damping
                   alpha = min(max(alpha,0.25),4.0)  ! 1/4<=alpha<=4.0
               end if

               tol2 = tau_buddy**2 * alpha * (varO(kis) + varF(kis)) ! tolerance**2

               if ( (OmF(kis)-del_star)**2<tol2 ) then
                    reaccept(kis) = .TRUE.
                    n_reacc = n_reacc + 1
               end if

            end if

         end do

!$omp end parallel do

         if (verbose) then
             write(stdout,'(2a,i8,a,i2)') myname, ': ',   &
                     n_reacc,' observations reaccepted after iteration', iter
         end if

!        End buddy check if no additional obs have been re-accepted
!        ----------------------------------------------------------
         if ( n_reacc==0 ) goto 900        ! sorry ...

!        Reset suspect flags
!        -------------------
         where ( issuspect .AND. reaccept ) issuspect = .FALSE.

      end do    ! Buddy check iteration ends here

  900 continue

!     All obs which have not been reaccepted by now, fail:
!     ---------------------------------------------------
      nfailed = 0
      do i = 1, nobs
         if ( issuspect(i) .AND. qcx(i)==0 ) then
              qcx(i) = X_BUDDY
              nfailed = nfailed + 1
         end if
      end do

!     Wrap up 
!     -------
      call Clean ( partition )

!     Release memory
!     --------------
      deallocate ( ki_susp, kr_susp, issuspect, reaccept )
      deallocate ( iregbeg, ireglen )
      deallocate ( xobs, yobs, zobs )

      call zeit_co ( myname )   ! timing call

      CONTAINS   !................ internals follow ........................

         subroutine Region_Sort_ ( )

!     Sorts data in obs vector by PSAS-defined regions.
!     Also returns sort index, Cartesian data coordinates on unit sphere,
!     and region pointers.

         integer  ksofar
         integer, allocatable :: kr(:)

!        Compute cartesian (x,y,z) coord. on unit sphere:
!        ------------------------------------------------
         call LL2XYZ ( lon, lat, nobs, xobs, yobs, zobs, rc )
         if (rc/=0) then
            write(stderr,'(2a)') myname, ': on error from LL2XYZ'
            call die(myname)
         end if

!        Allocate memory
!        ---------------
         allocate ( kr(nobs), stat=rc )
         if ( rc/=0 ) then
             write(stderr,'(2a)') myname,': allocate() error'
             call die(myname)
         end if

!        Compute iregn from (x,y,z)
!        -------------------------
         call XYZ2REG ( nobs, xobs, yobs, zobs, kr, partition )

!        Sort by region
!        --------------
         call IndexSet  ( nobs, indx )
         call IndexSort ( nobs, indx, kr, descend=.false. )
         call OBS_Reorder ( nobs, y, indx )
         xobs = xobs( (/ (indx(i), i=1,nobs) /) )
         yobs = yobs( (/ (indx(i), i=1,nobs) /) )
         zobs = zobs( (/ (indx(i), i=1,nobs) /) )
         varF = varF( (/ (indx(i), i=1,nobs) /) )
         varO = varO( (/ (indx(i), i=1,nobs) /) )

!        Also re-sort local arrays
!        -------------------------
         lon = lon( (/ (indx(i), i=1,nobs) /) )
         lat = lat( (/ (indx(i), i=1,nobs) /) )
         OmF = OmF( (/ (indx(i), i=1,nobs) /) )

!        Set integer region pointers
!        ---------------------------
         do ireg = 1, maxreg
            iregbeg(ireg) = 0
            ireglen(ireg) = 0
         end do

         do i = 1, nobs
            ireglen(kr(i)) = ireglen(kr(i)) + 1
         end do

         ksofar = 0
         do ireg = 1, maxreg
            if ( ireglen(ireg).gt.0 ) then
               iregbeg(ireg) = ksofar + 1
               ksofar = ksofar + ireglen(ireg)
            end if
         end do

!        Deallocate memory
!        -----------------
         deallocate ( kr )

         end subroutine Region_Sort_
!        ---------------------------

         subroutine RC_buddy_ ( )

!        Get buddy check resource parameters from resource file

         integer iret

!        Load resources
!        --------------
         call i90_loadf (SQCRC, iret)
         if (iret/=0) then
             write(stderr,'(2a,i5,2a)') myname, ': I90_loadf error, iret =', iret, &
                                                ': trying to load ', SQCRC
             call die(myname)
         end if

!        tau_buddy:

         call I90_label('tau_buddy:', iret)
         if (iret/=0) then
             write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                                ': trying to read ', 'tau_buddy:'
             call die(myname)
         end if
         tau_buddy = I90_GFloat(iret)
         if (iret/=0) then
             write(stderr,'(2a,i5)') myname, ': I90_GFloat error, iret=', iret
             call die(myname)
         end if
         write(stdout,'(2a,f6.2)') myname, ': tau_buddy = ',tau_buddy

!        maximum number of iterations:

         call I90_label('niter_max:', iret)
         if (iret/=0) then
             write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                                ': trying to read ', 'niter_max:'
             call die(myname)
         end if
         niter_max = I90_GInt(iret)
         if (iret/=0) then
             write(stderr,'(2a,i5)') myname, ': I90_GInt error, iret=', iret
             call die(myname)
         end if
         write(stdout,'(2a,i5)') myname, ': niter_max = ',niter_max

!        maximum number of buddies:

         call I90_label('nbuddy_max:', iret)
         if (iret/=0) then
             write(stderr,'(2a,i5)') myname,': I90_label error, iret =', iret
             call die(myname)
         end if
         nbuddy_max = I90_GInt(iret)
         if (iret/=0) then
             write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                                ': trying to read ', 'nbuddy_max:'
             call die(myname)
         end if
         write(stdout,'(2a,i5)') myname, ': nbuddy_max = ', nbuddy_max

!        damping parameter for the adaptive scheme:

         call I90_label('nstar:', iret)
         if (iret/=0) then
             write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                                ': trying to read ', 'nstar:'
             call die(myname)
         end if
         nstar = min(99999,I90_GInt(iret))
         if (iret/=0) then
             write(stderr,'(2a,i5)') myname,': I90_GInt error, iret =', iret
             call die(myname)
         end if
         write(stdout,'(2a,i5)') myname, ': nstar = ', nstar

!        search radius (in units of horizontal length scale)

         call I90_label('search_rad:', iret)
         if (iret/=0) then
             write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                                ': trying to read ', 'search_rad:'
             call die(myname)
         end if
         search_rad = I90_Gfloat(iret)
         if (iret/=0) then
             write(stderr,'(2a,i5)') myname,': I90_GFloat error, iret=', iret
             call die(myname)
         end if
         write(stdout,'(2a,e10.3)') myname, ': search_rad = ', search_rad

!        maximum separation angle for buddy search

         call I90_label('seplim:', iret)
         if (iret/=0) then
             write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                                ': trying to read ', 'seplim:'
             call die(myname)
         end if
         seplim = I90_Gfloat(iret)
         if (iret/=0) then
             write(stderr,'(2a,i5)') myname,': I90_GFloat error, iret=', iret
             call die(myname)
         end if
         write(stdout,'(2a,e10.3)') myname, ': seplim = ', seplim

!        Release resource file:
!        ---------------------
         call I90_release()

         end subroutine RC_buddy_
!        ------------------------

      end subroutine Buddy_Check_
!     ---------------------------


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Pairw_Check_ () --- Test for matched components in
!                                        paired datatypes
!
! !INTERFACE:
!
      subroutine Pairw_Check_ ( nobs, nexcl, y )
!
! !INPUT PARAMETERS:
!
      integer,          intent(in) :: nobs      ! number of observations
!
! !INPUT/OUTPUT PARAMETERS:
!
      type(obs_vect),   intent(inout) :: y      ! observation vector
!
! !OUTPUT PARAMETERS:
!
      integer, intent (out) ::  nexcl    ! number of exclusions due to
                                         !   unpaired components
!
! !REVISION HISTORY:
!
!  15May02 (Dee)  Initial code
!  24Apr03 (Meta) add zeit_co call before return if nwind==0
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'Pairw_Check_'


!     Pairwise check resource parameters:
!     ----------------------------------
      integer kt_u(ktmax)   ! u-components
      integer kt_v(ktmax)   ! v-components

!     Data type flags
!     ---------------
      logical, allocatable :: isuwnd(:)
      logical, allocatable :: isvwnd(:)
      logical, allocatable :: issrfc(:)

!     Sorting
!     -------
      integer, allocatable :: indx(:)

      integer i, j, nwind, npairs, rc

!     Pointers to data attributes:
!     --------------------------
      integer, pointer :: kt(:)       ! data type index
      integer, pointer :: kx(:)       ! data source index
      integer, pointer :: ks(:)       ! sounding index
      real*8  :: lon(nobs)      ! longitude of obs (degrees)
      real*8  :: lat(nobs)      ! latitute of obs (degrees)
      real*8  :: lev(nobs)      ! pressure level of obs (hPa)
      integer, pointer :: qcx(:)      ! exclusion mark

      kt     => y%kt
      kx     => y%kx
      ks     => y%ks
      lon(1:nobs)    = y%lon(1:nobs)
      lat(1:nobs)    = y%lat(1:nobs)
      lev(1:nobs)    = y%lev(1:nobs)
      qcx    => y%qcexcl

      call zeit_ci ( myname )   ! timing call

!     Get resource parameters
!     -----------------------
      call RC_pairwise_ ( )

!     Move all wind data to front of list:
!     -----------------------------------
      npairs = count ( kt_u > 0 )
      call OBS_MoveUp ( y, nobs, nwind, rc, 'kt', (/ kt_u(1:npairs), kt_v(1:npairs) /) )
      if (rc/=0) then
          write(stderr,'(2a,i5)') myname,': OBS_MoveUp() error, rc = ', rc
          call die(myname)
      end if

      if (verbose) write(stdout,'(3a,i8,a)') myname, ': ',      &
                                'found ', nwind, ' wind observations'

      nexcl = 0
      if (nwind==0) then
          call zeit_co ( myname )   ! timing call
          return
      endif

!     Sort by kx, ks, lon, lat, lev:
!     -----------------------------
      allocate ( indx(nobs), stat=rc )
      if (rc/=0) then
          write(stderr,'(2a,i5)') myname,': allocate() error, stat = ', rc
          call die(myname)
      end if
      call IndexSet  ( nwind, indx )
      call IndexSort ( nwind, indx, lev(1:nwind), descend=.false. )
      call IndexSort ( nwind, indx, lat(1:nwind), descend=.false. )
      call IndexSort ( nwind, indx, lon(1:nwind), descend=.false. )
      call IndexSort ( nwind, indx,  ks(1:nwind), descend=.false. )
      call IndexSort ( nwind, indx,  kx(1:nwind), descend=.false. )
      call OBS_Reorder ( nwind, y, indx )
      deallocate ( indx )

!     Flag u-wind, v-wind, and surface data:
!     -------------------------------------
      allocate ( isuwnd(nwind), isvwnd(nwind), issrfc(nwind), stat=rc )
      if (rc/=0) then
          write(stderr,'(2a,i5)') myname,': allocate() error, stat = ', rc
          call die(myname)
      end if

      isuwnd = .FALSE.
      isvwnd = .FALSE.
      issrfc = .FALSE.
      do i = 1, nwind
         do j = 1, npairs
            if (kt(i)==kt_u(j)) isuwnd(i) = .TRUE.
            if (kt(i)==kt_v(j)) isvwnd(i) = .TRUE.
            if (ls_v(kt(i))<tol_rel) issrfc(i) = .TRUE.
         end do
      end do

!     Check for matching pairs
!     ------------------------
      call Wind_Check_ ( nwind, nexcl, kt, kx, ks, lon, lat, lev, qcx, &
                                                   issrfc, isuwnd, isvwnd )

      call zeit_co ( myname )   ! timing call

      deallocate ( issrfc, isuwnd, isvwnd )


      CONTAINS   !................ internals follow ........................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Wind_Check_() -- Reject unpaired and/or duplicate
!                                    wind components.
!
! !INTERFACE:

      subroutine Wind_Check_ ( nobs, nfailed,                    &
                                     kt, kx, ks, lon, lat, lev, qcx,   &
                                     issrfc, isuwnd, isvwnd )


! !INPUT PARAMETERS:
!
      integer , intent(in) ::  nobs            ! number of observations
      integer , intent(in) ::  kt(nobs)        ! data type index
      integer , intent(in) ::  kx(nobs)        ! data source index
      integer , intent(in) ::  ks(nobs)        ! sounding index
      real*8    , intent(in) ::  lon(nobs)       ! longitude
      real*8    , intent(in) ::  lat(nobs)       ! latitude
      real*8    , intent(in) ::  lev(nobs)       ! pressure level
      logical , intent(in) ::  issrfc(nobs)    ! true for surface data
      logical , intent(in) ::  isuwnd(nobs)    ! true for u-wind data
      logical , intent(in) ::  isvwnd(nobs)    ! true for v-wind data
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer , intent(inout) ::  qcx(nobs)    ! exclusion mark
!
! !OUTPUT PARAMETERS:
!
      integer , intent(out)   ::  nfailed      ! number of failed obs
!
! !DESCRIPTION: Excludes unmatched (and extra) wind components. Wind component
!               data are assumed to belong to a single report if they have the
!               same kx, ks, lon, lat (and lev, for upper-air data). On input,
!               data are assumed to be sorted by kx, ks, lon, lat (lev).
!
! !REVISION HISTORY:
!
!  01Mar00 (Dee) - Completely redone using ks. Note slightly different
!                  behavior from previous version: duplicate wind obs are
!                  now removed as well.
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::  myname = 'Wind_Check_'

      integer i, j, ibeg, iend, iob, kxp, ksp, ktu, ktv, iu, iv
      real*8 lonp, latp, lnp
      logical newreport

      nfailed = 0

      if (nobs==0) return   ! nothing to do

      latp = -1e10   ! forces condition of first IF

!     Loop through all data:
!     ---------------------
      do i = 1, nobs

         ! check if this obs belongs to a new wind report:
         ! ----------------------------------------------
         if      (abs(lat(i)-latp)>tol_rel)     then
                                                newreport = .TRUE.
         else if (abs(lon(i)-lonp)>tol_rel)     then
                                                newreport = .TRUE.
         else if (ks(i)/=ksp)                   then
                                                newreport = .TRUE.
         else if (kx(i)/=kxp)                   then
                                                newreport = .TRUE.
         else if (.not. issrfc(i))              then
              if (abs(log(lev(i))-lnp)>tol_rel) newreport = .TRUE.
         end if

         ! check for u or v with qcx=0 in current report:
         ! ---------------------------------------------
         if (.not. newreport) then
             iend = i    ! last obs (so far) in current wind report
             if (isuwnd(i) .AND. qcx(i)==0 .AND. iu==0) iu = i ! first good u
             if (isvwnd(i) .AND. qcx(i)==0 .AND. iv==0) iv = i ! first good v
         end if

         ! perform wind check on the data in the previous report:
         ! -----------------------------------------------------
         if ((i/=1 .AND. newreport) .OR. i==nobs) then

             ! check for single uv-pair with qcx=0:

             if (iu==0 .OR. iv==0) then
                 iu =0
                 iv =0
             end if

             ! iu, iv now point to first u, v with qcx=0, but only if the
             ! report contains both. Otherwise iu=iv=0. Any remaining data
             ! in the report with qcx=0 fail the wind check.

             do iob = ibeg, iend
                if (qcx(iob)==0 .AND. iob/=iu .AND. iob/=iv) then
                    qcx(iob) = X_WIND
                    nfailed = nfailed + 1
                end if
             end do

         end if

         ! initialize for the new report:
         ! -----------------------------
         if (newreport) then

             ! save kx, ks, lon, lat, lev for this report:

             kxp  = kx(i)   ! current kx
             ksp  = ks(i)   ! current ks
             lonp = lon(i)  ! current lon
             latp = lat(i)  ! current lat
             if (.not. issrfc(i)) lnp = log(lev(i)) ! current level

             ! set pointers:

             ibeg = i    ! first obs in this report
             iend = i    ! last obs in this report
             iu   = 0    ! index of first u-wind with qcx=0
             iv   = 0    ! index of first v-wind with qcx=0

             ! check for u or v with qcx=0:

             if (isuwnd(i) .AND. qcx(i)==0) iu = i ! first good u
             if (isvwnd(i) .AND. qcx(i)==0) iv = i ! first good v

             ! assume next obs belongs to this report:

             newreport = .false.

         end if

      end do

      i = count ( qcx==0 .AND. isuwnd )
      j = count ( qcx==0 .AND. isvwnd )

      write(stderr,'(2a,2i10)') myname, ': i and j value =',i,j

      if ( i .ne. j ) then
           write(stderr,'(2a)') myname, ': incompatible nobs_u, nobs_v '
           call die(myname)
      end if

      end subroutine Wind_Check_
!     -------------------------

      subroutine RC_pairwise_ ( )

!     Get pairwise check resource parameters from resource file

      character(len=*), parameter :: tablename = 'paired_kts::'
      integer iret, ipair, kta

!     Initialize
!     ----------
      kt_u = 0
      kt_v = 0

!     Load resources
!     --------------
      call i90_loadf (SQCRC, iret)
      if (iret/=0) then
          write(stderr,'(2a,i5,2a)') myname, ': I90_loadf error, iret =', iret, &
                                             ': trying to load ', SQCRC
          call die(myname)
      end if

!     Read table with paired datatypes:
!     --------------------------------
      call I90_label(tablename, iret)
      if (iret/=0) then
         write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                            ': trying to read ', tablename
         call die(myname)
      end if
      ipair = 0
      do while (iret==0)     ! read table entries
         call I90_GLine ( iret ) ! iret=-1: end of file; +1: end of table
         if (iret==0) then   ! OK, we have next row of table

             ipair = ipair + 1

             kta = I90_GInt( iret )    ! u-component
             if (iret/=0) then
                 write(stderr,'(2a,i5)') myname, ': I90_GInt error, iret=', iret
                 call die(myname)
             end if
             if (kta < 0 .OR. kta > ktmax) then
                 write(stderr,'(2a,i5)') myname, ': invalid kt value: kt = ', kta
                 call die(myname)
             end if

             kt_u(ipair) = kta

             kta = I90_GInt( iret )    ! v-component
             if (iret/=0) then
                 write(stderr,'(2a,i5)') myname, ': I90_GInt error, iret=', iret
                 call die(myname)
             end if
             if (kta < 0 .OR. kta > ktmax) then
                 write(stderr,'(2a,i5)') myname, ': invalid kt value: kt = ', kta
                 call die(myname)
             end if

             kt_v(ipair) = kta

             if (verbose) write(stdout,'(2a,i3,x,i3)') myname, &
                                       ': paired datatypes: ', &
                                              kt_u(ipair), kt_v(ipair)

         end if
      end do


!     Release resource file:
!     ---------------------
      call I90_release()

      end subroutine RC_pairwise_
!     ---------------------------

     end subroutine Pairw_Check_
!    ------------------------------

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Profl_Check_ () --- Test for incomplete profiles
!
!
! !INTERFACE:
!
      subroutine Profl_Check_ ( nobs, nexcl, y )
!
! !INPUT PARAMETERS:
!
      integer,          intent(in) :: nobs      ! number of observations
!
! !INPUT/OUTPUT PARAMETERS:
!
      type(obs_vect),   intent(inout) :: y       ! observation vector
!
! !OUTPUT PARAMETERS:
!
      integer, intent (out) ::  nexcl    ! number of exclusions due to
                                         !   incomplete profiles
!
! !DESCRIPTION: Excludes an entire profile if any of the data from that
!               profile were previously marked for exclusion.  Data are
!               assumed to belong to the same (univariate) profile if
!               they have the same kx, kt, ks.
!
! !REVISION HISTORY:
!
!  01Mar00 (Dee) - Completely redone using ks.
!  15May02 (Dee) - Upgraded for version 2.00
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'Profl_Check_'

      integer, allocatable :: indx(:)  ! index vector for sort

      integer, parameter :: n_kxprmax = 100  ! max for n_kxpr
      integer kx_prfl(n_kxprmax)      ! list of kx's for profile check
      integer n_kxpr            ! number of kx's for profile check

      integer rc, i, ibeg, iend, iob, iq, nprof, kxp, ktp, ksp
      logical newprofile, exclude

!     Pointers to data attributes:
!     --------------------------
      integer, pointer :: kt(:)       ! data type index
      integer, pointer :: kx(:)       ! data source index
      integer, pointer :: ks(:)       ! sounding index
      integer, pointer :: qcx(:)      ! exclusion mark

      kt     => y%kt
      kx     => y%kx
      ks     => y%ks
      qcx    => y%qcexcl

      nexcl = 0
      if ( nobs==0 ) return     ! nothing to do

      call zeit_ci ( myname )   ! timing call

!     Get resource parameters
!     -----------------------
      call RC_profile_ ( )

!     Move data for profile check to front of list:
!     --------------------------------------------
      call OBS_MoveUp ( y, nobs, nprof, rc, 'kx', kx_prfl )
      if (rc/=0) then
          write(stderr,'(2a,i5)') myname,': OBS_MoveUp() error, rc = ', rc
          call die(myname)
      end if

      if (verbose) write(stdout,'(3a,i8,2a)') myname, ': ',   &
                                'found ', nprof, ' observations subject to ', &
                                'a profile check'

      if ( nprof==0 ) then
           call zeit_co ( myname )   ! timing call
           return  ! nothing to do
      end if

      allocate ( indx(nprof), stat=rc )
      if (rc/=0) then
          write(stderr,'(2a,i5)') myname,': allocate() error, stat = ', rc
          call die(myname)
      end if

!     Sort obs by kx, kt, ks:
!     ----------------------
      call IndexSet  ( nprof, indx )
      call IndexSort ( nprof, indx, ks(1:nprof), descend=.false. )
      call IndexSort ( nprof, indx, kt(1:nprof), descend=.false. )
      call IndexSort ( nprof, indx, kx(1:nprof), descend=.false. )
      call OBS_Reorder ( nprof, y, indx )

      deallocate ( indx )

      ksp = -1  ! forces condition of first IF

!     Loop through all data:
!     ---------------------
      do i = 1, nprof

         ! check if this obs belongs to a new profile:
         ! ------------------------------------------
         if      (ks(i)/=ksp) then
                              newprofile = .TRUE.
         else if (kx(i)/=kxp) then
                              newprofile = .TRUE.
         else if (kt(i)/=ktp) then
                              newprofile = .TRUE.
         end if

         ! check for data with qcx/=0 in current profile:
         ! ---------------------------------------------
         if (.not. newprofile) then
             iend = i    ! last obs (so far) in current wind report
             do iq = 1, nqcxp
                if (qcx(i)==QCXP(iq)) exclude = .TRUE. ! bad obs in profile
             end do
         end if

         ! perform profile check on data in the previous profile:
         ! -----------------------------------------------------
         if ((i/=1 .AND. newprofile) .OR. i==nobs) then

             if (exclude) then ! mark entire profile:
                 do iob = ibeg, iend
                    if (qcx(iob)==0) then
                        qcx(iob) = X_PROFILE
                        nexcl = nexcl + 1
                    end if
                 end do
             end if

         end if

         ! initialize for the new profile:
         ! ------------------------------
         if (newprofile) then

             ! save kx, ks, kt for this profile:

             ktp = kt(i)   ! current kt
             kxp = kx(i)   ! current kx
             ksp = ks(i)   ! current ks

             ! set pointers:

             ibeg = i    ! first obs in this profile
             iend = i    ! last obs in this profile

             ! check for data with qcx/=0:

             exclude = .FALSE.
             if (qcx(i)/=0) exclude = .TRUE.

             ! assume next obs belongs to this profile:

             newprofile = .FALSE.

         end if

      end do

      call zeit_co ( myname )   ! timing call

      CONTAINS   !................ internals follow ........................

         subroutine RC_profile_ ( )

!        Get profile check resource parameters from resource file

         character*255 token
         integer iret, kx1, kx2, kxnext, i, lt

!        Load resources
!        --------------
         call i90_loadf (SQCRC, iret)
         if (iret/=0) then
             write(stderr,'(2a,i5,2a)') myname, ': I90_loadf error, iret =', iret, &
                                                ': trying to load ', SQCRC
             call die(myname)
         end if

!        table of kx's:

         call I90_label('profile_kxs::', iret)
         if (iret/=0) then
             write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                                ': trying to read ', 'profile_kxs::'
             call die(myname)
         end if
         write(stdout,'(2a)') myname, ': profile_kxs::'
         n_kxpr = 0
         do while (iret==0)     ! read table entries
            call I90_GLine ( iret ) ! iret=-1: end of file; +1: end of table
            if (iret==0) then   ! OK, we have next row of table
                call I90_GToken(token, iret )
                if (iret/=0) then      ! read error
                    write(stderr,'(2a,i5)') myname, ': I90_GToken error, iret=', iret
                    call die(myname)
                end if
                i  = index(token,':') ! token is single kx or range of kx's
                lt = len_trim(token)
                if (i==0) then    ! no colon, therefore single kx
                    read(token,*) kx1
                    kx2 = kx1
                    write(stdout,'(2a,i3)') myname, ':   ', kx1
                else                  ! colon, therefore kx1:kx2
                    read(token(1:i-1),*) kx1
                    read(token(i+1:lt),*) kx2
                    write(stdout,'(2a,i3,a1,i3)') myname, ':   ', kx1, ':', kx2
                end if
                if (kx1>kx2) then      ! range error
                    write(stderr,'(2a,i5)') myname, ': Invalid range: ', token
                    call die(myname)
                end if
                do kxnext = kx1, kx2
                   if (n_kxpr==n_kxprmax) then    ! check space
                       write(stderr,'(2a,i5)') myname,': increase n_kxprmax'
                       call die(myname)
                   else if (iret==0) then
                       n_kxpr = n_kxpr + 1
                       kx_prfl(n_kxpr) = kxnext
                   end if
                end do
            end if
         end do

!        Release resource file:
!        ---------------------
         call I90_release()

         end subroutine RC_profile_
!        --------------------------

      end subroutine Profl_Check_
!     ----------------------------

   end module m_sqc
