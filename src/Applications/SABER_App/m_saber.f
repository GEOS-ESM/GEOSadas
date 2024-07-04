!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  m_saber --- Reads SABER data and writes it into an ODS vector
! 
! !INTERFACE:
!

      module m_saber
         
      use m_ods
      use m_odsmeta, only: H_DESCEND, H_ASCEND
      use m_odsmeta, only: ktTT, kto3mx, ktww

      use m_saber_err, only: saber_err_get

      implicit none

!
! !PUBLIC MEMBER FUNCTIONS:
!
      PUBLIC  saber_get

!
! !DESCRIPTION:
! \label{SABER:Mod}
!  This module ingests SABER data and writes it into ODS vectors
!
! !REVISION HISTORY: 
!
! 12Mar2003  T. King      First crack.
!
!EOP
!-------------------------------------------------------------------------

!  Overloaded Interfaces
!  ---------------------
      Interface SABER_Get
         module procedure SABER_Dims_
         module procedure SABER_GetM_
         module procedure SABER_range_
      end Interface

      integer, parameter :: minutes1day = 24 * 60
      integer, parameter :: minutes3hrs =  3 * 60
!     integer, parameter :: mflush = 320 ! not clear what this is about
      integer, parameter :: mflush = 400 ! not clear what this is about
      integer, parameter :: std_levels = 400
      integer, parameter :: max_allevents = 2500
!     integer, parameter :: std_levels = 500
!     integer, parameter :: max_allevents = 2200
      integer, dimension(max_allevents) :: tdate
      real, dimension(std_levels,max_allevents)     :: lat, lon
      integer, dimension(std_levels,max_allevents)  :: time, date
      integer, dimension(max_allevents)    :: tpAD
      real, dimension(std_levels,max_allevents)     :: pres, temp, 
     $     density, h2o, o3_96
      real, dimension(std_levels,max_allevents)     :: pres_e, 
     $     temp_e, density_e, h2o_e
      logical :: intime(std_levels,max_allevents)
      integer start_pt

      CONTAINS

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SABER_GetM_ --- Reads data from a multiple SABER files and 
!                             returns an ODS vector.
! 
! !INTERFACE:

      subroutine SABER_GetM_ ( version, fname, nymd, nhms, kscnt,
     $                         kxtyp, ods, rc)
      implicit none

! !INPUT PARAMETERS: 
!
      character(len=*), intent(in)   :: version   ! SABER version
      character(len=*), intent(in)   :: fname     ! SABER file name
      integer, intent(in)            :: nymd      ! year-month-day, e.g., 19990701
      integer, intent(in)            :: nhms      ! hour-min-sec,   e.g., 120000
      integer, intent(in)            :: kscnt     ! ks counter
      integer, intent(in)            :: kxtyp     ! kx indicator

! !OUTPUT PARAMETERS:
!

      type(ods_vect), intent(inout)  ::  ods ! ODS vector

      integer, intent(out)           ::  rc ! Error return code:
                                              ! = 0    - all is well

! !DESCRIPTION:
! \label{SABER:GetM}
!  This routine reads data from 1 SABER HDF files, allocates the necessary 
!  memory for the ODS vector, and loads the data for the synoptic time 
!  (nymd,nhms).
!
! !REVISION HISTORY: 
!
!  12Mar2003  T. King  Initial code is conceptually based on that of the
!                      m_roms.90 module for reading in ODS files.
!
!EOP
!-------------------------------------------------------------------------

      integer :: s_julian,s_date    ! Selected date and synoptic time
      integer :: n,m,i,k,oc,nfiles
      integer :: min_bnd(2) 
      integer :: min_tods, max_tods,tgap
      integer :: n_levels,n_events
      integer :: timediff,year,jan01,jul_day,doy,ksnum,iminutes
      integer, parameter :: dist_tol = 0.001
      real :: minutes,ob_array(4)
      logical :: levflag,skipflag,skipall
      integer, external :: ODS_Julian

! Set meta data attributes
      call SABER_meta_(ods%meta%kt_names,ods%meta%kt_units,
     $  ods%meta%kx_names,ods%meta%kx_meta,ods%meta%qcx_names)

      rc=0
      start_pt=1
      intime=.false.

      call read_ ( version, fname, n_events, n_levels )

      call get_range_ (std_levels,start_pt-1,nhms,intime)

!_RT  print *, 'nhms = ', nhms
      min_tods = -180
      max_tods =  180
      if(nhms.eq.000000)then
         min_bnd(1)= -180
         min_bnd(2)=  0
         tgap = 0
      elseif(nhms.eq.060000)then
         min_bnd(1)= 180
         min_bnd(2)= 6*60
         tgap = 360
      elseif(nhms.eq.120000)then
         min_bnd(1)= 540
         min_bnd(2)= 12*60
         tgap = 720
      elseif(nhms.eq.180000)then
         min_bnd(1)= 900
         min_bnd(2)= 18*60
         tgap = 1080
      else
         print *,'Synoptic time ',nhms,'Z is not a valid time.'
         stop
      endif
!        min_bnd(1)= -180
!        min_bnd(2)=  180
      s_julian=ODS_Julian(nymd)

!      print *, 'Total number of events=',start_pt-1

! Begin writing to ODS vector. 

      oc=1
      ksnum=kscnt 
      ! Cycle through the events.
      do n=1,n_events !start_pt-1   
         levflag=.false.  ! This flag is activated when processing reaches
                          ! somewhere near the end of the "level" portion of
                          ! this input data array.  This allows the program
                          ! to search for old unflushed data.

         skipflag=.false. ! This allows the processing to skip past all 
                          ! unflushed values in the level portion of the 
                          ! input data array.

         skipall=.false.  ! This flag allows for the skipping over sets
                          ! of obs with impossible locations.

         ! Cycle through the levels.
         do m=1,n_levels !std_levels

            if(.not.intime(m,n)) cycle

            ! Find the end of the number of levels for which there are data.
            if(abs(lat(m,n)).ge.dist_tol.and.
     $         abs(lon(m,n)).ge.dist_tol.and.
     $             time(m,n).ne.0)then

               ! Use only data below 0.01 mb
               if(pres(m,n).ge.0.01)then

                  ! Because netcdf arrays were not flushed, removed junk
                  ! from the ends.
                  if(m.gt.mflush)then
                     levflag=.true.
                     timediff=time(m,n)-time(m-1,n)
                  else
                     timediff=0
                  endif

                  ! If the current observation time is not significantly 
                  ! less than the previous time we continue onward, 
                  ! otherwise we've hit unflushed data and it's time to
                  ! skip the rest of the array go to the next data event.
                  if(.not.skipflag.and.timediff.ge.-15000)then

! Start to get the internal time information out of the file for comparison
! with what was specified in the argument list.

                     ! This strips off the "doy" and gives you the year.
                     year=date(m,n)/1000

                     ! This gives you the doy.
                     doy=date(m,n)-year*1000

                     ! Get the date of Jan 1st of the current year
                     jan01=(year*10000)+101

                     ! Then we get the Julian day and add back on the doy

                     jul_day=ODS_Julian(jan01)+doy-1
                     ! Because minutes can be greater than 1440 in the 
                     ! last file of a day, subtract 1440 from 
                     ! these numbers and increment the julian day.

                     ! Get time into minutes
                     minutes=time(m,n)/60000.0
                     if(minutes.ge.minutes1day-minutes3hrs)then
                        minutes=minutes-minutes1day
                        jul_day=jul_day+1
                     endif
                     iminutes=anint(minutes)

                     ! If the times in the file match our temporal search 
                     ! criteria write them to the ods arrays.
                     if(jul_day.eq.s_julian)then !.and.iminutes.ge.min_bnd(1)
!    $                                     .and.iminutes.lt.min_bnd(2))then

                        ! Obs types to be written to ods.
                        ! Do some quick quality checks on the data
!                       if(temp(m,n).lt.0.and.temp(m,n).gt.1000)then
                          ob_array(1)=temp(m,n)
!                       else
!                         ob_array(1)=1.0e+15
!                       endif
                        
                        ob_array(2)=h2o(m,n)*0.001 ! pmmv to g/kg
                        ob_array(3)=O3_96(m,n)     ! pmmv
!                       ob_array(4)=density(m,n)

                        ! Convert lon from 0 to 360 into -180 to 180
                        if(lon(m,n).gt.180)then
                           lon(m,n)=lon(m,n)-360
                        endif

                        ! Do a quick range check of lat and lon
                        if(abs(lon(m,n)).gt.180)then
                           print*,'Longitude out of range',lon(m,n)
                           skipall=.true.
                        elseif(abs(lat(m,n)).gt.90)then
                           print*,'Latitude out of range',lat(m,n)
                           skipall=.true.
                        endif

                        ! If location is possible, continue onward.
                        if(.not.skipall)then

                           do k=1,3

                              ! If data are not out of range, write them
                              ! to the ods structure.
                              if(ob_array(k).gt.0)then
                                 ods%data%lat(oc)=lat(m,n)
                                 ods%data%lon(oc)=lon(m,n)
                                 ods%data%lev(oc)=pres(m,n)
                                 ods%data%xm(oc)=0.0
                                 ods%data%obs(oc)=ob_array(k)
                                 ods%data%time(oc)=iminutes-min_bnd(2)
                                 ods%data%ks(oc)=ksnum
                                 if(k.eq.1)then
                                    ods%data%kt(oc)=ktTT
                                    call saber_err_get(ods%data%lev(oc),ods%data%xvec(oc))
                                 elseif (k==2) then
                                    ods%data%kt(oc)=ktww
                                 elseif (k==3) then
                                    ods%data%kt(oc)=kto3mx
!                                elseif(k.eq.4)then
!                                   ods%data%kt(oc)=43
                                 endif
                                 ods%data%kx(oc)=kxtyp
                                 ods%data%qcexcl(oc)=0
                                 select case (tpAD(n))
                                 case (0)
                                    ods%data%qchist(oc)=H_ASCEND
                                 case (1)
                                    ods%data%qchist(oc)=H_DESCEND
                                 case default
                                    ods%data%qchist(oc)=0
                                 end select
                                 oc=oc+1
                              endif ! Good data to write to ods structure.
                           enddo ! Cycle through obs loop.
                           ksnum=ksnum+1
                        endif ! Location check.
                        skipall=.false.
                     endif ! Meet time criteria.
                  else
                     skipflag=.true.
                  endif ! Skip over unflushed data.
               endif    ! Pressure greater than 0.01.

            endif ! Check current level for valid data.
         enddo ! Reading level array.
         if(.not.levflag)then
!_RT        print*,"Warning: current array has less than ", mflush, " levels."
!_RT        print*,"File may be incomplete."
         endif
!        ksnum=ksnum+1
      enddo ! Reading event array.

      if(oc.eq.1)then
         rc=1 ! No data were found matching the requested time and date.
      else
         ods%data%nobs=oc-1
      endif
      return
      end subroutine SABER_GetM_

      subroutine saber_dims_ ( fname,
     $                         nymd,
     $                         doy, n_events, n_levels )
      implicit none      
      include 'netcdf.inc'

      character(len=*), intent(in) :: fname
      integer, intent(out) :: nymd
      integer, optional, intent(out) :: doy, n_events, n_levels

      character(len=100) varname
      integer ndims, nvars, ngatts, dimid, year, doy_
      integer beg(1), fin(1)
      integer prev, now, nthis, jan01, julday
      integer fid, id, ii, rc
      integer, external :: ODS_Julian
      integer, external :: ODS_Caldat

!     Open file
      fid = ncopn (fname, NCNOWRIT, rc)

!     Inquire dims
      call ncinq (fid, ndims, nvars, ngatts, dimid, rc)

      id = ncdid (fid, "event", rc)
      call ncdinq (fid, id, varname, nthis, rc)
      if(present(n_events)) then
        n_events = nthis
      endif

      beg(1)=1
      fin(1)=nthis
      id = ncvid (fid, "date", rc)
      call ncvgt (fid,id,beg,fin,tdate,rc)

!     Make sure all data in file is for a single day
      prev=-999
      do ii=1,nthis
         if(tdate(ii)<197801) cycle
         now=tdate(ii)
         if (prev==-999) then
            prev=now
            cycle
         endif
         if(now/=prev) then
           print *, 'Not all data in file at same day'
           call exit (1)
         endif 
      enddo

      ! This strips off the "doy" and gives you the year.
      year=now/1000
     
      ! This gives you the doy.
      doy_=now-year*1000
      if(present(doy)) then
        doy=doy_     
      endif

      ! Get the date of Jan 1st of the current year
      jan01=(year*10000)+101
      
      ! Then we get the Julian day and add back on the doy
      JulDay=ODS_Julian(jan01)+doy_-1
      nymd = ODS_CalDat ( JulDay )

      if (present(n_levels)) then
        id = ncdid (fid, "altitude", rc)
        call ncdinq (fid, id, varname, n_levels, rc)
      endif

!     Close file
      call ncclos (fid, rc)

      end subroutine saber_dims_

      subroutine read_ ( version, fname, n_events, n_levels )
      implicit none      
      include 'netcdf.inc'

!
! !INPUT PARAMETERS:
!

      character(len=*), intent(in) :: version
      character(len=*)   fname         ! File name
!
! !OUTPUT PARAMETERS:
!
      integer, intent(out) :: n_events, n_levels

! Variables for ncopn

      integer fid
      integer rc

! Variables for ncinq

      integer ndims, nvars, ngatts, dimid

! Variables for ncdid
  
      integer event_id, date_id
      integer alt_id
      integer varid
      character(len=100) varname
      character(len=100) dimname
      integer dimsize

      integer vartype, nvdims, vdims(MAXVDIMS), nvatts

! Variables for data

      real, allocatable :: buf (:,:)
      integer, allocatable :: ibuf (:,:)
      integer*2 cbuf (1,100)
      integer start1D(1), edge1D(1), start2D(2), edge2D(2)

! Other vars

      integer i,j,end_pt

      allocate(buf(std_levels,100))
      allocate(ibuf(std_levels,100))

!     call ncpopt(NCVERBOS)

      fid = ncopn (fname, NCNOWRIT, rc)
      call ncinq (fid, ndims, nvars, ngatts, dimid, rc)

      event_id = ncdid (fid, "event", rc)
      call ncdinq (fid, event_id, varname, n_events, rc)
      end_pt = start_pt + n_events - 1
      if (end_pt .GT. max_allevents) then
         print *, end_pt,' events is too many.  Increase 
     $        size of max_allevents.'
         stop
      endif

      alt_id = ncdid (fid, "altitude", rc)
      call ncdinq (fid, alt_id, varname, n_levels, rc)
 
! Get date

      start1D(1)=1
      edge1D (1)=n_events
      date_id = ncvid (fid, "date", rc)
      call ncvgt (fid,date_id,start1D,edge1D,tdate,rc)
      if ( rc .ne. 0 ) then
         print *, 'get date rc=',rc
      endif
!     print *, TRIM(fname), tdate(1:n_events)
!     print *, 'Number of events =',n_events
!     print *, 'Number of levels=',n_levels
      do i=1,size(date,1)
         date(i,start_pt:end_pt)=tdate
      enddo
    

! Set hyperslab dimensions

      start2D(1)=1
      start2D(2)=1
      edge2D(1)=n_levels
      edge2D(2)=n_events

! Get ascending/descending info
 
      varid = ncvid (fid, "tpAD", rc)
      call ncvgt (fid,varid,start2D(2),edge2D(2),cbuf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get tpAD rc=',rc
      endif
      tpAD(start_pt:end_pt)=cbuf(1,1:n_events)

! Get pressure & error
 
      varid = ncvid (fid, "pressure", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get pres rc=',rc
      endif
      pres(:,start_pt:end_pt)=buf(:,1:n_events)

      if (trim(version) == "1.0") then
         varid = ncvid (fid, "pressure_error", rc)
         call ncvgt (fid,varid,start2D,edge2D,buf,rc)
         if ( rc .ne. 0 ) then
            print *, 'get pres_e rc=',rc
         endif
         pres_e(:,start_pt:end_pt)=buf(:,1:n_events)
      else
         pres_e(:,start_pt:end_pt)=1.e15
      endif

! Get temperature & error
      
      varid = ncvid (fid, "ktemp", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get temp rc=',rc
      endif
      temp(:,start_pt:end_pt)=buf(:,1:n_events)

      if (trim(version) == "1.0") then
         varid = ncvid (fid, "ktemp_error", rc)
         call ncvgt (fid,varid,start2D,edge2D,buf,rc)
         if ( rc .ne. 0 ) then
            print *, 'get temp_e rc=',rc
         endif
         temp_e(:,start_pt:end_pt)=buf(:,1:n_events)
      else
         temp_e(:,start_pt:end_pt)=1.e15
      endif

! Get density and error
      
      varid = ncvid (fid, "density", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get density rc=',rc
      endif
      density(:,start_pt:end_pt)=buf(:,1:n_events)

      if (trim(version) == "1.0") then
         varid = ncvid (fid, "density_error", rc)
         call ncvgt (fid,varid,start2D,edge2D,buf,rc)
         if ( rc .ne. 0 ) then
            print *, 'get density_e rc=',rc
         endif
         density_e(:,start_pt:end_pt)=buf(:,1:n_events)
      else
         density_e(:,start_pt:end_pt)=1.e15
      endif

! Get o3_96 and error
      
      varid = ncvid (fid, "O3_96", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get o3_96 rc=',rc
      endif
      o3_96(:,start_pt:end_pt)=buf(:,1:n_events)

! Get h2o and error
      
      varid = ncvid (fid, "H2O", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get h2o rc=',rc
      endif
      h2o(:,start_pt:end_pt)=buf(:,1:n_events)

      if (trim(version) == "1.0") then
         varid = ncvid (fid, "H2O_error", rc)
         call ncvgt (fid,varid,start2D,edge2D,buf,rc)
         if ( rc .ne. 0 ) then
            print *, 'get h2o_e rc=',rc
         endif
         h2o_e(:,start_pt:end_pt)=buf(:,1:n_events)
      else
         h2o_e(:,start_pt:end_pt)=1.e15
      endif

! Get latitude

      varid = ncvid (fid, "tplatitude", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get lat rc=',rc
      endif
      lat(:,start_pt:end_pt)=buf(:,1:n_events)

! Get longitude

      varid = ncvid (fid, "tplongitude", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get lon rc=',rc
      endif
      lon(:,start_pt:end_pt)=buf(:,1:n_events)

! Get time

      varid = ncvid (fid, "time", rc)
      call ncvgt (fid,varid,start2D,edge2D,ibuf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get time rc=',rc
      endif
      time(:,start_pt:end_pt)=ibuf(:,1:n_events)

      call ncclos (fid, rc)
      start_pt = start_pt + n_events

      deallocate(ibuf)
      deallocate(buf)

      return
      end subroutine read_

      subroutine SABER_meta_(ktnames,ktunits,kxnames,kxmeta,qcxnames)

      implicit none

      character (len=*), intent(out), dimension(:) :: ktnames
      character (len=*), intent(out), dimension(:) :: ktunits
      character (len=*), intent(out), dimension(:) :: kxnames
      character (len=*), intent(out), dimension(:) :: kxmeta 
      character (len=*), intent(out), dimension(:) :: qcxnames

      qcxnames = ' '
      kxnames = ' '
      kxmeta = ' '
      ktnames = ' '
      ktunits = ' '

      kxnames(394)=
     $'SABER - Sounding of the Atmosphere Broadband Emission Radiometer'

      ktnames(ktww)='Upper-air water vapor mixing ratio'
      ktunits(ktww)='g/kg'
      ktnames(ktTT)='Upper-air temperature'
      ktunits(ktTT)='Kelvin'
      ktnames(kto3mx)='Ozone mixing ratio'
      ktunits(kto3mx)='ppmv'
!     ktnames(43)='Density of atmosphere'
!     ktunits(43)='1/cm*3'
      
      qcxnames(1)='clear'
      qcxnames(2)='unspecified preprocessing flag'
      qcxnames(3)='impossible location'
      qcxnames(4)='gcm deep underground'
      qcxnames(5)='observation value undefined'
      qcxnames(6)='forecast value undefined'
      qcxnames(7)='observation level too high'
      qcxnames(8)='passive data type'
      qcxnames(9)='outside active time window'
      qcxnames(10)='not an analysis variable'
      qcxnames(11)='NCEP CQC bad observation'
      qcxnames(12)='NCEP PREPDATA bad observation'
      qcxnames(13)='NCEP CQC bad pressure'
      qcxnames(14)='NCEP PREPDATA bad pressure'
      qcxnames(15)='DAO range check failed'
      qcxnames(16)='DAO duplicate obs. (>1 in 6 hr)'
      qcxnames(17)='DAO failed hydrostatic check'
      qcxnames(18)='SQC: buddy check'
      qcxnames(19)='SQC: wind check'
      qcxnames(20)='SQC: invalid error stats'
      qcxnames(21)='SQC: profile check'
      qcxnames(22)='SQC: background check'
      qcxnames(23)='SQC: obsolete'
      qcxnames(24)='SQC: obsolete'
      qcxnames(25)='SQC: obsolete'
      qcxnames(26)='SQC: obsolete'
      qcxnames(27)='SQC: obsolete'
      qcxnames(28)='invalid qsat'
      qcxnames(29)='moisture from bad temp'
      qcxnames(30)='thinned'
      qcxnames(31)='unphysical value'
      qcxnames(32)='Red list'
      qcxnames(33)='obs could not be simulated'
      qcxnames(34)='excluded by PSAS'

      return
      end subroutine SABER_meta_

      subroutine get_range_ (mm,nn,nhms,intime)
      integer, intent(in) :: mm,nn,nhms
      logical, intent(inout) :: intime(:,:)
      integer n,m,iminutes,synmin,synmina,synminb
      integer trange(2)
      real minutes

      trange(1)= 99999
      trange(2)=-trange(1)
      synmin  = (nhms/10000)*60
      synmina = synmin-180
      synminb = synmin+180
      intime=.false.
      do m=1,mm
         do n=1,nn
           minutes=time(m,n)/60000.0
           if(minutes.ge.minutes1day-minutes3hrs)then
              minutes=minutes-minutes1day
           endif 
           iminutes=anint(minutes)
           if (iminutes>=synmina .and. iminutes<synminb) then
              if (iminutes<trange(1)) trange(1)=iminutes
              if (iminutes>trange(2)) trange(2)=iminutes
               intime(m,n)=.true.
           endif
      
         enddo
      enddo
      !print*, 'nhms, time range: ', mm, nn, nhms, trange
      end subroutine get_range_

      subroutine SABER_range_ (fname, n_events, n_levels, syn)

      implicit none      
      include 'netcdf.inc'

      character(len=*), intent(in) :: fname
      integer, intent(in) :: n_events, n_levels
      logical, intent(inout) :: syn(:)

      integer fid,id,varid,rc
      integer m,n,iminutes
      integer start2D(2),edge2D(2)
      integer, allocatable, dimension(:,:) :: mytime
      real minutes

!     Open file
      fid = ncopn (fname, NCNOWRIT, rc)

      start2D(1)=1
      start2D(2)=1
      edge2D(1)=n_levels
      edge2D(2)=n_events

      allocate(mytime(n_levels,n_events))

      varid = ncvid (fid, "time", rc)
      call ncvgt (fid,varid,start2D,edge2D,mytime,rc)
      if ( rc .ne. 0 ) then
         print *, 'get_range2_: get time rc=',rc
         call exit (1)
      endif

      syn=.false.
      do m=1,n_levels
         do n=1,n_events
           minutes=mytime(m,n)/60000.0
!          if(minutes.ge.minutes1day-minutes3hrs)then
!             minutes=minutes-minutes1day
!          endif 
           iminutes=anint(minutes)
!          if (iminutes<trange(1)) trange(1)=iminutes
!          if (iminutes>trange(2)) trange(2)=iminutes
!          if (iminutes>=synmina .and. iminutes<synminb) intime(m,n)=.true.
           if (iminutes>    0 .and. iminutes < 180) syn(1)=.true. !  disregard data at midnight
           if (iminutes>= 180 .and. iminutes < 540) syn(2)=.true.
           if (iminutes>= 540 .and. iminutes < 900) syn(3)=.true.
           if (iminutes>= 900 .and. iminutes <1080) syn(4)=.true.
           if (iminutes>=1080                     ) syn(5)=.true.
         enddo
      enddo

      deallocate(mytime)

!     Close file
      call ncclos (fid, rc)

      end subroutine SABER_range_

      end module m_saber



