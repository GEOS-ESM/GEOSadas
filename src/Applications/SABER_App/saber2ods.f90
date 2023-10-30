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

      integer :: i,j,k,l
      integer :: num_options,nhms(5),nymd,iargc,pos
      integer :: rc_saber_get,rc_ods_clean,rc_ods_init, &
           rc_ods_put,rc_ods_get,rc_ods_open,rc_ods_close
      integer :: ierr_write,ierr_app,rc
      integer :: ncid
      integer :: fyear,fdoy,fjul_day,fcal_day,fjul_day_p1,fcal_day_p1, &
           jul_day, hh
      integer :: ODS_Julian,ODS_Caldat
      character(len=180) fnames(200),tempfname,arg,fn,odsname,bfrfname
      character(len=180) odstmpl,bfrtmpl
      character(len=8)   cnymd
      character(len=4)   cfyear
      character(len=3)   cfdoy
      character(len=9)   ftype
      character(len=10) s_string
      character(len=3) version
      logical :: found,append,hit
      logical :: verbose,bexist
      type(ods_vect)  ::  ods_struct,ods_old ! ODS vector
      integer :: tnobs,fc

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
      i=1
      fc=1
      s_string='SABER_L2A_'
      bfrtmpl='saber.l2a.obs.%y4%m2%d2_%h2z.bfr'
      bfrtmpl='NULL'
! Process argument list
      num_options = iargc()
      if (num_options .eq. 0 ) then
         print *, "usage: saber2ods.x [-o odstmpl] saber_ncfile(s)"
         print *, "odstmpl          template for ODS output file(s)"
         print *, "saber_ncfile(s)  Input netcdf SABER file(s)"
         stop
      endif
      odstmpl=''
      do while (i .le. num_options)
         call getarg (i,arg)
         pos=index(arg,'-o')
         if (i.eq.1.and.pos.gt.0)then
            call getarg (2,arg)
            odstmpl=arg
            i=3
         else
            if(len(trim(odstmpl)).eq.0)then
               odstmpl='saber.l2a.obs.%y4%m2%d2_%h2z.ods'
            endif
            fnames(fc)=trim(arg)
            fc=fc+1
            i=i+1
         endif
      enddo

! Begin reading files.
      do j=1,fc-1
         
! Search for date string in the filename.
         pos=index(fnames(j),s_string)
         tempfname=fnames(j)
         cfyear=tempfname(pos+10:pos+13)
         cfdoy=tempfname(pos+14:pos+16)
         read(cfyear,204)fyear
 204     format(i4)
         read(cfdoy,204)fdoy
 205     format(i3)
         fyear=(10000*fyear)+101

         fjul_day=ODS_Julian(fyear)+fdoy-1
         fjul_day_p1=ODS_Julian(fyear)+fdoy

         fcal_day=ODS_Caldat(fjul_day)
         fcal_day_p1=ODS_Caldat(fjul_day_p1)
         hit=.false.
         do k=1,5 ! Cycle through the possible synoptic times in the file
            found=.false.
            append=.false.
            nymd=fcal_day
            if(k.eq.5)then
               nymd=fcal_day_p1
            endif

            ! Create ods file name from template
            call StrTemplate ( odsname, odstmpl, 'GRADS', xid="saber", &
                               nymd=nymd, nhms=nhms(k), stat=rc )               

            ! generate/update ODS file w/ SABER data
            call saberods_(odsname)

            ! generate bufr file if desired
            call StrTemplate ( bfrfname, bfrtmpl, 'GRADS', xid="saber", &
                               nymd=nymd, nhms=nhms(k), stat=rc )               
            if ( trim(bfrfname) /= "NULL" ) then
               if(verbose) print*,'Writing data at ',nymd,nhms(k),'Z to ',bfrfname 
               inquire(file=bfrfname,exist=bexist)
               if (bexist) then
                  print *,'Append to output bfr file ',trim(bfrfname)
                  call init_bufr(bfrfname,append=.true.)
               else
                  print *,'Create output bfr file ',trim(bfrfname)
                  call init_bufr(bfrfname,tablefile='saber_prepbufr_table',append=.false.)
               endif
 
!              call write_bfr(stnid, xob, yob, dhr, typ, ib, &
!                             rstat, quality(jtm), convergence(jtm), &
!                             pob, tob, tqm, tprec, subset, idate)
            endif


         enddo 
         if(.not.hit)then
            print*, 'Warning! No data matching the files date and times were found.'
         endif
      enddo
      
contains
      subroutine saberods_ (odsname)
 
      character(len=*), intent(in) :: odsname
! Initialize ods vector
       call ODS_Init(ods_struct,tnobs,rc_ods_init)
       call SABER_Get(version,fnames(j),nymd,nhms(k),ods_struct, &
            rc_saber_get)

! If data matching the selected synoptic time were found, continue.
       if(rc_saber_get.eq.0)then ! If something was read from the file, continue
          hit=.true.

! Check to see if ods file already exists.
          inquire(file=odsname,exist=found)
          if(found)then

! If the file is present read it and get the number of obs
! and then add those nobs to the ks values so ks is unique
! throughout the synoptic time.  We also need to know if 
! there are already obs written for the current synoptic 
! time.  If so, we have to set append=.true.

             call ODS_Get(odsname,nymd,nhms(k),ftype,ods_old, &
                         rc_ods_get)
             if(verbose) print*, 'rc_ods_get = ',rc_ods_get
             if(verbose) print*,'number of obs found for ',nhms(k),'Z is ', &
                    ods_old%data%nobs
             if(ods_old%data%nobs.eq.0)then
                print*,'Writing data at ',nymd,nhms(k),'Z to ',trim(odsname) 
                call ODS_Put(odsname,ftype,nymd,nhms(k),ods_struct, &
                     rc_ods_put)
                if(verbose) print*,'rc_ods_put = ',rc_ods_put
             else
                print*,'Appending data at ',nymd,nhms(k),'Z to ',trim(odsname) 
                do l=1,ods_struct%data%nobs
                   ods_struct%data%ks(l)=ods_old%data%ks(ods_old%data%nobs) &
                        +ods_struct%data%ks(l)
                enddo
! Append data to the current synoptic time.
                call ODS_Open(ncid,odsname,'w',rc_ods_open)
                if(verbose) print*,'rc_ods_open=',rc_ods_open
                call ODS_Append(ncid,ods_struct%data%nobs,ierr_app)
                if(verbose) print*,'ierr_app=',ierr_app
                jul_day=ODS_Julian(nymd)
                call ODS_PutR(ncid,'lat',jul_day,nhms(k)/10000,ods_struct%data%nobs, &
                              ods_struct%data%lat,ierr_write)
                if(verbose) print*,'ierr_write=',ierr_write
                call ODS_PutR(ncid,'lon',jul_day,nhms(k)/10000,ods_struct%data%nobs, &
                              ods_struct%data%lon,ierr_write)
                if(verbose) print*,'ierr_write=',ierr_write
                call ODS_PutR(ncid,'lev',jul_day,nhms(k)/10000,ods_struct%data%nobs, &
                              ods_struct%data%lev,ierr_write)
                if(verbose) print*,'ierr_write=',ierr_write
                call ODS_PutI(ncid,'time',jul_day,nhms(k)/10000,ods_struct%data%nobs, &
                              ods_struct%data%time,ierr_write)
                if(verbose) print*,'ierr_write=',ierr_write
                call ODS_PutI(ncid,'kt',jul_day,nhms(k)/10000,ods_struct%data%nobs, &
                              ods_struct%data%kt,ierr_write)
                if(verbose) print*,'ierr_write=',ierr_write
                call ODS_PutI(ncid,'kx',jul_day,nhms(k)/10000,ods_struct%data%nobs, &
                              ods_struct%data%kx,ierr_write)
                if(verbose) print*,'ierr_write=',ierr_write
                call ODS_PutI(ncid,'ks',jul_day,nhms(k)/10000,ods_struct%data%nobs, &
                              ods_struct%data%ks,ierr_write)
                if(verbose) print*,'ierr_write=',ierr_write
                call ODS_PutR(ncid,'xm',jul_day,nhms(k)/10000,ods_struct%data%nobs, &
                              ods_struct%data%xm,ierr_write)
                if(verbose) print*,'ierr_write=',ierr_write
                call ODS_PutI(ncid,'qcexcl',jul_day,nhms(k)/10000,ods_struct%data%nobs, &
                              ods_struct%data%qcexcl,ierr_write)
                if(verbose) print*,'ierr_write=',ierr_write
                call ODS_PutI(ncid,'qchist',jul_day,nhms(k)/10000,ods_struct%data%nobs, &
                              ods_struct%data%qchist,ierr_write)
                if(verbose) print*,'ierr_write=',ierr_write
                call ODS_PutR(ncid,'obs',jul_day,nhms(k)/10000,ods_struct%data%nobs, &
                              ods_struct%data%obs,ierr_write)
                if(verbose) print*,'ierr_write=',ierr_write

                call ODS_Close(ncid,'SABER',rc_ods_close)
                if(verbose) print*,'rc_ods_close=',rc_ods_close
             endif
          else
! Write data to an entirely NEW ods file
             call ODS_Put(odsname,ftype,nymd,nhms(k),ods_struct,rc_ods_put)
             if(verbose) print*,'rc_ods_put = ',rc_ods_put
          endif
       else
          if(verbose) print*,'No data at ',nymd,nhms(k) 
       endif
! Deallocate the ods vector
       call ODS_Clean(ods_struct,rc_ods_clean)

      return
      end subroutine saberods_

      subroutine write_bfr(stnid, xob, yob, dhr, typ, ib, &
                           rstat, qual, conv, &
                           pob, tob, tqm, tprec, subset, idate)

      integer idate, ib, l
      character(len=8) stnid, subset
      real xob, yob, dhr, typ, rstat, qual, conv
      real pob(ib), tob(ib), tqm(ib), tprec(ib)

      real(8), dimension(8) ::    hdr    !  observation header
      real(8)               ::    rid
      real(8), dimension(8,255) :: pobs, tobs

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
      data tobstr /'TOB TQM TPC TRC SABERRT'/

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

      end program saber2ods

