!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !PROGRAM:  saber2ods.x --- Reads a SABER data files and writes ODS files
! 
! !DESCRIPTION: 
!  This module ingests SABER data files and writes ODS files.  It uses 
!  the m_saber.f module.
! 
! !USAGE: saber2ods.x [-o odstmpl] saber_ncfile(s)
!
!         where:  odstmpl             template for ODS output file(s)
!                 saber_ncfile(s)     Input netcdf SABER file(s)"
!
!
! !REVISION HISTORY: 
!
! 18Mar2003  T. King      First crack.
! 29Oct2023  R. Todling   - Add support for v2.0(7) - not too different
!                           than original; indeed original code should
!                           work for this except that errors are not
!                           available.
!                         - Pulled code into own directory (out of ODS)
!                           (need to link to bufr lib)
!                         - Output ODS now 6-hrly instead of daily files
!                         - Fixed timing getting into ODS time slot
! 
! To do: 
!    1. generate corresponding prepbufr file
!
!EOP
!-------------------------------------------------------------------------

      program saber2ods

      use m_saber
      use m_ods
      use m_StrTemplate
      use m_pbutil

      implicit none

      integer, parameter :: nfiles_max=200
      integer :: i,j,k,l
      integer :: num_options,nhms(5),nymd,iargc,pos
      integer :: rc_ods_clean,rc_ods_init, &
           rc_ods_put,rc_ods_open,rc_ods_close
      integer :: iarg,argc,ierr_write,ierr_app,rc
      integer :: ncid
      integer :: fyear,fdoy,fjul_day,fcal_day,fjul_day_p1,fcal_day_p1, &
           jul_day, hh
      integer :: n_events, n_levels
      integer, external :: ODS_Julian,ODS_Caldat
      character(len=180) fnames(nfiles_max),tempfname,arg,fn,odsname,bfrfname
      character(len=180) odstmpl,bfrtmpl
      character(len=255) cmd
      character(len=8)   cnymd
      character(len=4)   cfyear
      character(len=3)   cfdoy
      character(len=9)   ftype
      character(len=10) s_string
      character(len=3) version
      character(len=255) argv
      logical :: found,append,hit
      logical :: verbose,bexist
      logical :: syn(5)
      type(ods_vect)  ::  ods_struct(2), odsm ! ODS vector
      integer :: kxtyp,tnobs,nfiles
 
      logical :: debug=.false.

! Initialize variables
      version = "2.0"
      verbose = .false.
      tnobs=450000
      ncid=11
      ftype='pre_anal'
      nhms(1)=000000
      nhms(2)=060000
      nhms(3)=120000
      nhms(4)=180000
      nhms(5)=000000
      kxtyp = 394
      i=1
      nfiles=0
      s_string='SABER_L2A_'
      bfrtmpl='NULL'
      bfrtmpl='saber.l2a.obs.%y4%m2%d2_%h2z.bfr'
      odstmpl='saber.l2a.obs.%y4%m2%d2_%h2z.ods'

      ! Process argument list
      argc =  iargc()              
      if ( argc .lt. 1 ) call usage()
      nfiles = 0
      iarg = 0
      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) go to 111
         call GetArg ( iArg, argv )
         if (index(argv,'-o' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage()
            iarg = iarg + 1
            call GetArg ( iArg, odstmpl )
         else if (index(argv,'-verbose' ) .gt. 0 ) then
            verbose= .true.
         else if (index(argv,'-version' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage()
            iarg = iarg + 1
            call GetArg ( iArg, version )
         else
            nfiles = nfiles + 1
            if ( nfiles .gt. nfiles_max ) then
               print *, 'Maximum number of input files = ', nfiles_max
               stop
            end if
            fnames(nfiles) = argv
         end if
      end do
 111  continue
      if ( nfiles .lt. 1 ) call usage()

      print *
      print *, "Processing the following files:"
      do j=1,nfiles
         write(*,'(1x,a)') trim(fnames(j))
      enddo
! Begin reading files.
      do j=1,nfiles
         
! Search for date string in the filename.
!        pos=index(fnames(j),s_string)
!        tempfname=fnames(j)
!        cfyear=tempfname(pos+10:pos+13)
!        cfdoy=tempfname(pos+14:pos+16)
!        read(cfyear,204)fyear
!204     format(i4)
!        read(cfdoy,204)fdoy
!205     format(i3)
!        fyear=(10000*fyear)+101

!        fjul_day=ODS_Julian(fyear)+fdoy-1
!        fjul_day_p1=ODS_Julian(fyear)+fdoy

!        fcal_day=ODS_Caldat(fjul_day)
!        fcal_day_p1=ODS_Caldat(fjul_day_p1)
!        hit=.false.

         call SABER_Get(fnames(j), nymd, fdoy, n_events, n_levels)
         call SABER_Get(fnames(j), n_events, n_levels, syn)
        
         do k=1,5 ! Cycle through the possible synoptic times in the file
            if(.not.syn(k)) cycle
            if(k==5 .and. syn(5) ) then
               fyear=nymd/10000
               fyear=(10000*fyear)+101
               fjul_day_p1=ODS_Julian(fyear)+fdoy
               nymd=ODS_Caldat(fjul_day_p1)
            endif 
            if(debug) cycle
            found=.false.
            append=.false.

            ! Create ods file name from template
            call StrTemplate ( odsname, odstmpl, 'GRADS', xid="saber", &
                               nymd=nymd, nhms=nhms(k), stat=rc )               

            ! Initialize ODS vector
            call ODS_Init(ods_struct(1),tnobs,rc_ods_init)

            ! Generate/update ODS file w/ SABER data
            call saberods_(odsname)

            ! generate bufr file if desired
            call StrTemplate ( bfrfname, bfrtmpl, 'GRADS', xid="saber", &
                               nymd=nymd, nhms=nhms(k), stat=rc )               
            if ( trim(bfrfname) /= "NULL" ) then

               inquire(file=bfrfname,exist=bexist)
               if (bexist) then
                  if(verbose) print *,'Append to output bfr file ',trim(bfrfname)
                  call init_bufr(bfrfname,append=.true.)
               else
                  if(verbose) print *,'Create output bfr file ',trim(bfrfname)
                  call init_bufr(bfrfname,tablefile='saber_prepbufr_table',append=.false.)
               endif
 
               call ods2bfr_ (nymd,nhms(k),ods_struct(1))

               call end_bufr

            endif

            ! Release the ODS vector
            call ODS_Clean(ods_struct(1),rc_ods_clean)

         enddo 
!        if(.not.hit)then
!           print*, 'Warning! No data matching the files date and times were found.'
!        endif
      enddo
      
contains
      subroutine saberods_ (odsname)
 
      character(len=*), intent(in) :: odsname

      integer :: ks

!     Check existence of ODS file
      ks = 1
      inquire(file=odsname,exist=found)
      if (found) then
          call ODS_Get( odsname, nymd, nhms(k), ftype, ods_struct(2), rc )
          ks=maxval(ods_struct(2)%data%ks) + 1
      endif

!     Initialize ods vector
      call SABER_Get(version,fnames(j),nymd,nhms(k),ks,kxtyp,ods_struct(1),rc)

!     If data matching the selected synoptic time were found, continue.
       if(rc.eq.0)then ! If something was read from the file, continue
!         hit=.true.

! Check to see if ods file already exists.
          if(found)then
             call ODS_Merge ( ods_struct, 2, odsm, rc )
             write(cmd,'(2a)') '/bin/rm -r ', trim(odsname)
             call system(cmd)
             call ODS_Put( odsname, ftype, nymd, nhms(k), odsm, rc )
             if( maxval(abs(odsm%data%time)) > 180 ) then
                write(6,'(a,1x,i6.6,1x,a,2x,2i5)') &
                        'At nhms= ', nhms(k), 'min/max time ', minval(odsm%data%time), maxval(odsm%data%time)
                call exit(98)
             endif
             write(6,'(a,1x,i6.6,1x,a,3(2x,i6))') &
                     'At nhms: ', nhms(k), ' nobs (init,new,merge): ', ods_struct(1)%data%nobs, ods_struct(2)%data%nobs, odsm%data%nobs
             call ODS_Clean( ods_struct(2), rc )
             call ODS_Clean( odsm, rc )
          else
             if( maxval(abs(ods_struct(1)%data%time)) > 180 ) then
                write(6,'(a,1x,i6.6,1x,a,2x,2i5)') &
                        'At nhms= ', nhms(k), 'min/max time ', minval(ods_struct(1)%data%time), maxval(ods_struct(1)%data%time)
                call exit(99)
             endif
             write(6,'(a,1x,i6.6,1x,a,2x,i6)') &
                     'At nhms: ', nhms(k), ' nobs: ', ods_struct(1)%data%nobs
             call ODS_Put(odsname,ftype,nymd,nhms(k),ods_struct(1),rc)
          endif
       else
          if(verbose) print*,'No data from SABER at ',nymd,nhms(k) 
       endif

      return
      end subroutine saberods_

      subroutine ods2bfr_(nymd,nhms,ods)

      use m_odsmeta, only: ktTT, kto3mx, ktww

      implicit none
 
      integer :: nymd, nhms
      type(ods_vect)  ::  ods

      integer idate
      real xob, yob, dhr, typ, rstat, qual, conv
      real,allocatable :: pob(:), tob(:), tqm(:), tprec(:)
      character(len=8) stnid, subset

      integer ii, ib

      subset = 'SABER'
      idate=nymd*100 + nhms/10000
      ib=1
      allocate (pob(ib), tob(ib), tqm(ib), tprec(ib))
      do ii=1,ods%data%nobs,3
         xob = ods%data%lon(ii)
         if (xob .lt. 0.) xob = xob + 360.
         yob = ods%data%lat(ii)
         pob(1) = ods%data%lev(ii)
         tprec(1) = 1.03
         if ( ods%data%kt(ii)  ==ktTT ) then
             tob(1) = ods%data%obs(ii)- 273.15 ! Celsius
             tprec(1) = ods%data%xvec(ii)
         endif
         if ( ods%data%kt(ii+1)==ktww ) tqm(1) = ods%data%obs(ii+1) ! units?
         rstat = ods%data%qcexcl(ii)
         dhr = ods%data%time(ii)/60. ! seconds
         typ = ods%data%kx(ii)
         write(stnid,'(''S'',i7.7)') ods%data%ks(ii)
         call write_bfr(stnid, xob, yob, dhr, typ, ib, &
                        rstat, 9., 1.0, &
                        pob, tob, tqm, tprec, subset, idate)
      enddo
      deallocate (pob, tob, tqm, tprec)

      end subroutine ods2bfr_
      subroutine write_bfr(stnid, xob, yob, dhr, typ, ib, &
                           rstat, qual, conv, &
                           pob, tob, tqm, tprec, subset, idate)

      implicit none
      integer idate, ib
      character(len=8) stnid, subset
      real xob, yob, dhr, typ, rstat, qual, conv
      real pob(ib), tob(ib), tqm(ib), tprec(ib)

      real(8), dimension(8) ::    hdr    !  observation header
      real(8)               ::    rid
      real(8), dimension(8,255) :: pobs, tobs

      integer l 
      integer, parameter :: MXBLVL = 255 ! max no. of report levels allowed
      integer(i_bfr), parameter :: iarr = 8    ! size of bfr arrays
      integer(i_bfr), parameter :: i1   = 1    ! single level
      integer(i_bfr), parameter :: ilv = MXBLVL  ! multiple level
      integer(i_bfr)  ibdate
      integer(i_bfr)  ilevs
      integer(i_bfr)  iret

      character(len=40) hdstr, pobstr, tobstr
      data hdstr /'SID XOB YOB DHR TYP SABERT SABERQ SABERC'/
      data pobstr /'POB PQM PPC PRC CAT'/
      data tobstr /'TOB TQM TPC TRC SABERPT'/

      hdr(1) = transfer(stnid, rid)
      hdr(2) = xob
      hdr(3) = yob
      hdr(4) = dhr
      hdr(5) = typ
      hdr(6) = rstat
      hdr(7) = qual
      hdr(8) = conv

      pobs = missing
      tobs = missing

      do l = 1,ib
         pobs(1,l) = pob(l)
         pobs(2,l) = tqm(l)    ! use same as T
         pobs(3,l) = 1.
         pobs(4,l) = 1.
         pobs(5,l) = 2.
         tobs(1,l) = tob(l)
         tobs(2,l) = tqm(l)
         tobs(3,l) = 1.
         tobs(4,l) = 1.
         tobs(5,l) = tprec(l)
      end do

      ibdate = idate
      ilevs = ib
      call openmb(lu_b, subset, ibdate)
      call ufbint(lu_b, hdr,    iarr, i1   , iret, hdstr)
      call ufbint(lu_b, pobs,   iarr, ilevs, iret, pobstr)
      call ufbint(lu_b, tobs,   iarr, ilevs, iret, tobstr)

      call writsb(lu_b)

      return
      end subroutine write_bfr
      subroutine usage
         print *, "usage: saber2ods.x [-o odstmpl] saber_ncfile(s)"
         print *, "odstmpl          template for ODS output file(s)"
         print *, "saber_ncfile(s)  Input netcdf SABER file(s)"
         stop
      end subroutine usage

      end program saber2ods

