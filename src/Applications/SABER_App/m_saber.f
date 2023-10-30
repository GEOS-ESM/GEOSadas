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
         module procedure SABER_Get1_
         module procedure SABER_GetM_
      end Interface

      integer, parameter :: std_levels = 400
      integer, parameter :: max_allevents = 2500
      real, dimension(std_levels,max_allevents)     :: lat, lon
      integer, dimension(std_levels,max_allevents)  :: time, date
      integer, dimension(max_allevents)    :: tpAD
      real, dimension(std_levels,max_allevents)     :: pres, temp, 
     $     density, h2o
      real, dimension(std_levels,max_allevents)     :: pres_e, 
     $     temp_e, density_e, h2o_e
      integer start_pt

      CONTAINS

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SABER_Get1_ --- Reads data from a single SABER file and returns
!                             an ODS vector.
! 
! !INTERFACE:

      subroutine SABER_Get1_ ( version, fname, nymd, nhms, ods, rc)
      implicit none

! !INPUT PARAMETERS: 
!
      character(len=*), intent(in)   :: version ! SABER version
      character(len=*), intent(in)   :: fname ! SABER file name
      integer, intent(in)            :: nymd ! year-month-day, e.g., 19990701
      integer, intent(in)            :: nhms ! hour-min-sec,   e.g., 120000

! !OUTPUT PARAMETERS:
!

      type(ods_vect), intent(out)    ::  ods ! ODS vector

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
      integer :: n,m,i,k,oc
      integer :: min_bnd(2) 
      integer :: timediff,year,jan01,jul_day,doy,ksnum,iminutes
      integer, parameter :: dist_tol = 0.001
      real :: minutes,ob_array(3)
      logical :: levflag,skipflag,skipall
      integer, external :: ODS_Julian
      integer, external :: ODS_Caldat

! Set meta data attributes
      call SABER_meta_(ods%meta%kt_names,ods%meta%kt_units,
     $  ods%meta%kx_names,ods%meta%kx_meta,ods%meta%qcx_names)

      rc=0
      start_pt=1

      call readfile ( version, fname ) 

      if(nhms.eq.000000)then
         min_bnd(1)=-180
         min_bnd(2)=180
      elseif(nhms.eq.060000)then
         min_bnd(1)=180
         min_bnd(2)=540
      elseif(nhms.eq.120000)then
         min_bnd(1)=540
         min_bnd(2)=900
      elseif(nhms.eq.180000)then
         min_bnd(1)=900
         min_bnd(2)=1260
      else
         print *,'Synoptic time ',nhms,'Z is not a valid time.'
         stop
      endif
      s_julian=ODS_Julian(nymd)

!      print *, 'Total number of events=',start_pt-1

! Begin writing to ODS vector. 

      oc=1
      ksnum=1 
      ! Cycle through the events.
      do n=1,start_pt-1   
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
         do m=1,std_levels

            ! Find the end of the number of levels for which there are data.
            if(abs(lat(m,n)).ge.dist_tol.and.abs(lon(m,n)).ge.
     $           dist_tol.and.time(m,n).ne.0)then

               ! Use only data below 0.01 mb
               if(pres(m,n).ge.0.01)then

                  ! Because netcdf arrays were not flushed, removed junk
                  ! from then ends.
                  if(m.gt.320)then
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
                     ! last file of a day, make the subtract 1440 from 
                     ! these numbers and increment the julian day.

                     ! Get time into minutes
                     minutes=real(time(m,n))/60000.0
                     if(minutes.ge.1260)then
                        minutes=minutes-1440.0
                        jul_day=jul_day+1
                     endif
                     iminutes=anint(minutes) 

                     ! If the times in the file match our temporal search 
                     ! criteria write them to the ods arrays.
                     if(jul_day.eq.s_julian.and.iminutes.ge.min_bnd(1).
     $                    and.iminutes.lt.min_bnd(2))then

                        ! Obs types to be written to ods.
                        ! Do some quick quality checks on the data
!                        if(temp(m,n).lt.0.and.temp(m,n).gt.1000)then
                           ob_array(1)=temp(m,n)
!                        else
!                           ob_array(1)=1.0e+15
!                        endif
                        
                        ob_array(2)=density(m,n)
                        ob_array(3)=h2o(m,n)

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
                                 ods%data%time(oc)=iminutes
                                 ods%data%ks(oc)=ksnum
                                 if(k.eq.1)then
                                    ods%data%kt(oc)=8
                                 elseif(k.eq.2)then
                                    ods%data%kt(oc)=43
                                 else
                                    ods%data%kt(oc)=7
                                 endif
                                 ods%data%kx(oc)=294
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
            print*,"Warning: current array has less than 320 levels."
            print*,"File may be incomplete."
         endif
      enddo ! Reading event array.

      if(oc.eq.1)then
         rc=1 ! No data were found matching the requested time and date.
      else
         ods%data%nobs=oc-1
      endif
      return
      end subroutine SABER_Get1_


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SABER_GetM_ --- Reads data from a multiple SABER files and 
!                             returns an ODS vector.
! 
! !INTERFACE:

      subroutine SABER_GetM_ ( version, nfiles, fnames, nymd, nhms, 
     $                         ods, rc)
      implicit none

! !INPUT PARAMETERS: 
!
      character(len=*), intent(in)   :: version ! SABER version
      integer, intent(in)            :: nfiles ! number of input files
      character(len=*), intent(in)   :: fnames(nfiles) ! SABER file name
      integer, intent(in)            :: nymd ! year-month-day, e.g., 19990701
      integer, intent(in)            :: nhms ! hour-min-sec,   e.g., 120000

! !OUTPUT PARAMETERS:
!

      type(ods_vect), intent(out)    ::  ods ! ODS vector

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

      character*255 fname
      integer :: s_julian,s_date    ! Selected date and synoptic time
      integer :: n,m,i,k,oc
      integer :: min_bnd(2) 
      integer :: timediff,year,jan01,jul_day,doy,ksnum,iminutes
      integer, parameter :: dist_tol = 0.001
      real :: minutes,ob_array(3)
      logical :: levflag,skipflag,skipall
      integer, external :: ODS_Julian
      integer, external :: ODS_Caldat

! Set meta data attributes
      call SABER_meta_(ods%meta%kt_names,ods%meta%kt_units,
     $  ods%meta%kx_names,ods%meta%kx_meta,ods%meta%qcx_names)

      rc=0
      start_pt=1

      do i=1,nfiles
         fname=fnames(i)
         call readfile ( version, fname ) 
      enddo

      if(nhms.eq.000000)then
         min_bnd(1)=-180
         min_bnd(2)=180
      elseif(nhms.eq.060000)then
         min_bnd(1)=180
         min_bnd(2)=540
      elseif(nhms.eq.120000)then
         min_bnd(1)=540
         min_bnd(2)=900
      elseif(nhms.eq.180000)then
         min_bnd(1)=900
         min_bnd(2)=1260
      else
         print *,'Synoptic time ',nhms,'Z is not a valid time.'
         stop
      endif
      s_julian=ODS_Julian(nymd)

!      print *, 'Total number of events=',start_pt-1

! Begin writing to ODS vector. 

      oc=1
      ksnum=1 
      ! Cycle through the events.
      do n=1,start_pt-1   
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
         do m=1,std_levels

            ! Find the end of the number of levels for which there are data.
            if(abs(lat(m,n)).ge.dist_tol.and.abs(lon(m,n)).ge.
     $           dist_tol.and.time(m,n).ne.0)then

               ! Use only data below 0.01 mb
               if(pres(m,n).ge.0.01)then

                  ! Because netcdf arrays were not flushed, removed junk
                  ! from then ends.
                  if(m.gt.320)then
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
                     ! last file of a day, make the subtract 1440 from 
                     ! these numbers and increment the julian day.

                     ! Get time into minutes
                     minutes=real(time(m,n))/60000.0
                     if(minutes.ge.1260)then
                        minutes=minutes-1440.0
                        jul_day=jul_day+1
                     endif
                     iminutes=anint(minutes) 

                     ! If the times in the file match our temporal search 
                     ! criteria write them to the ods arrays.
                     if(jul_day.eq.s_julian.and.iminutes.ge.min_bnd(1).
     $                    and.iminutes.lt.min_bnd(2))then

                        ! Obs types to be written to ods.
                        ! Do some quick quality checks on the data
!                        if(temp(m,n).lt.0.and.temp(m,n).gt.1000)then
                           ob_array(1)=temp(m,n)
!                        else
!                           ob_array(1)=1.0e+15
!                        endif
                        
                        ob_array(2)=density(m,n)
                        ob_array(3)=h2o(m,n)

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
                                 ods%data%time(oc)=iminutes
                                 ods%data%ks(oc)=ksnum
                                 if(k.eq.1)then
                                    ods%data%kt(oc)=8
                                 elseif(k.eq.2)then
                                    ods%data%kt(oc)=43
                                 else
                                    ods%data%kt(oc)=7
                                 endif
                                 ods%data%kx(oc)=294
                                 ods%data%qcexcl(oc)=0
                                 ods%data%qchist(oc)=0
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
            print*,"Warning: current array has less than 320 levels."
            print*,"File may be incomplete."
         endif
      enddo ! Reading event array.

      if(oc.eq.1)then
         rc=1 ! No data were found matching the requested time and date.
      else
         ods%data%nobs=oc-1
      endif
      return
      end subroutine SABER_GetM_


      subroutine readfile ( version, fname )
      character(len=*), intent(in) :: version
      character(len=*), intent(in) :: fname
      select case (trim(version))
      case ("1.0")
         call read_v1_(fname)
      case ("2.0")
         call read_v2_(fname)
      case default
         call read_v1_(fname)
      end select 

      endsubroutine readfile

      subroutine read_v1_ ( fname )
      implicit none      
      include 'netcdf.inc'

!
! !INPUT PARAMETERS:
!

      character(len=*)   fname         ! File name

! Variables for ncopn

      integer fid
      integer rc

! Variables for ncinq

      integer ndims, nvars, ngatts, dimid

! Variables for ncdid
  
      integer event_id, n_events, date_id
      integer alt_id, n_levels
      integer varid
      character*100 varname
      character*100 dimname
      integer dimsize

      integer vartype, nvdims, vdims(MAXVDIMS), nvatts

! Variables for data

      integer tdate
      real buf (400,100)
      integer ibuf (400,100)
      integer start1D, start2D(2), edge2D(2)

! Other vars

      integer i,j,end_pt

!     call ncpopt(NCVERBOS)

      fid = ncopn (fname, NCNOWRIT, rc)
      call ncinq (fid, ndims, nvars, ngatts, dimid, rc)
      event_id = ncdid (fid, "event", rc)

      alt_id = ncdid (fid, "altitude", rc)

      call ncdinq (fid, event_id, varname, n_events, rc)
      end_pt = start_pt + n_events - 1
      if (end_pt .GT. max_allevents) then
         print *, end_pt,' events is too many.  Increase 
     $        size of max_allevents.'
         stop
      endif

      call ncdinq (fid, alt_id, varname, n_levels, rc)
 
! Get date

      date_id = ncvid (fid, "date", rc)
      call ncvgt (fid,date_id,1,1,tdate,rc)
      if ( rc .ne. 0 ) then
         print *, 'get date rc=',rc
      endif
!      print *, TRIM(fname), tdate
!      print *, 'Number of events =',n_events
!      print *, 'Number of levels=',n_levels
      date(:,:)=tdate
    

! Set hyperslab dimensions

      start2D(1)=1
      start2D(2)=1
      edge2D(1)=n_levels
      edge2D(2)=n_events

! Get pressure & error
 
      varid = ncvid (fid, "pressure", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get pres rc=',rc
      endif
      pres(:,start_pt:end_pt)=buf(:,1:n_events)

      varid = ncvid (fid, "pressure_error", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get pres_e rc=',rc
      endif
      pres_e(:,start_pt:end_pt)=buf(:,1:n_events)

! Get temperature & error
      
      varid = ncvid (fid, "ktemp", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get temp rc=',rc
      endif
      temp(:,start_pt:end_pt)=buf(:,1:n_events)

      varid = ncvid (fid, "ktemp_error", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get temp_e rc=',rc
      endif
      temp_e(:,start_pt:end_pt)=buf(:,1:n_events)

! Get density and error
      
      varid = ncvid (fid, "density", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get density rc=',rc
      endif
      density(:,start_pt:end_pt)=buf(:,1:n_events)

      varid = ncvid (fid, "density_error", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get density_e rc=',rc
      endif
      density_e(:,start_pt:end_pt)=buf(:,1:n_events)

! Get h2o and error
      
      varid = ncvid (fid, "H2O", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get h2o rc=',rc
      endif
      h2o(:,start_pt:end_pt)=buf(:,1:n_events)

      varid = ncvid (fid, "H2O_error", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get h2o_e rc=',rc
      endif
      h2o_e(:,start_pt:end_pt)=buf(:,1:n_events)

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

      return
      end subroutine read_v1_

      subroutine read_v2_ ( fname )
      implicit none      
      include 'netcdf.inc'

!
! !INPUT PARAMETERS:
!

      character(len=*)   fname         ! File name

! Variables for ncopn

      integer fid
      integer rc

! Variables for ncinq

      integer ndims, nvars, ngatts, dimid

! Variables for ncdid
  
      integer event_id, n_events, date_id
      integer alt_id, n_levels
      integer varid
      character*100 varname
      character*100 dimname
      integer dimsize

      integer vartype, nvdims, vdims(MAXVDIMS), nvatts

! Variables for data

      integer tdate
      real buf (400,100)
      integer ibuf (400,100)
      integer*2 cbuf (1,100)
      integer start1D, start2D(2), edge2D(2)

! Other vars

      integer i,j,end_pt

!     call ncpopt(NCVERBOS)

      fid = ncopn (fname, NCNOWRIT, rc)
      call ncinq (fid, ndims, nvars, ngatts, dimid, rc)
      event_id = ncdid (fid, "event", rc)

      alt_id = ncdid (fid, "altitude", rc)

      call ncdinq (fid, event_id, varname, n_events, rc)
      end_pt = start_pt + n_events - 1
      if (end_pt .GT. max_allevents) then
         print *, end_pt,' events is too many.  Increase 
     $        size of max_allevents.'
         stop
      endif

      call ncdinq (fid, alt_id, varname, n_levels, rc)
 
! Get date

      date_id = ncvid (fid, "date", rc)
      call ncvgt (fid,date_id,1,1,tdate,rc)
      if ( rc .ne. 0 ) then
         print *, 'get date rc=',rc
      endif
!      print *, TRIM(fname), tdate
!      print *, 'Number of events =',n_events
!      print *, 'Number of levels=',n_levels
      date(:,:)=tdate
    

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

!_RT  varid = ncvid (fid, "pressure_error", rc)
!_RT  call ncvgt (fid,varid,start2D,edge2D,buf,rc)
!_RT  if ( rc .ne. 0 ) then
!_RT     print *, 'get pres_e rc=',rc
!_RT  endif
!_RT  pres_e(:,start_pt:end_pt)=buf(:,1:n_events)
      pres_e(:,start_pt:end_pt)=99999.

! Get temperature & error
      
      varid = ncvid (fid, "ktemp", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get temp rc=',rc
      endif
      temp(:,start_pt:end_pt)=buf(:,1:n_events)

!_RT  varid = ncvid (fid, "ktemp_error", rc)
!_RT  call ncvgt (fid,varid,start2D,edge2D,buf,rc)
!_RT  if ( rc .ne. 0 ) then
!_RT     print *, 'get temp_e rc=',rc
!_RT  endif
!_RT  temp_e(:,start_pt:end_pt)=buf(:,1:n_events)
      temp_e(:,start_pt:end_pt)=1.0

! Get density and error
      
      varid = ncvid (fid, "density", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get density rc=',rc
      endif
      density(:,start_pt:end_pt)=buf(:,1:n_events)

!_RT  varid = ncvid (fid, "density_error", rc)
!_RT  call ncvgt (fid,varid,start2D,edge2D,buf,rc)
!_RT  if ( rc .ne. 0 ) then
!_RT     print *, 'get density_e rc=',rc
!_RT  endif
!_RT  density_e(:,start_pt:end_pt)=buf(:,1:n_events)
      density_e(:,start_pt:end_pt)=1.e15

! Get h2o and error
      
      varid = ncvid (fid, "H2O", rc)
      call ncvgt (fid,varid,start2D,edge2D,buf,rc)
      if ( rc .ne. 0 ) then
         print *, 'get h2o rc=',rc
      endif
      h2o(:,start_pt:end_pt)=buf(:,1:n_events)

!_RT  varid = ncvid (fid, "H2O_error", rc)
!_RT  call ncvgt (fid,varid,start2D,edge2D,buf,rc)
!_RT  if ( rc .ne. 0 ) then
!_RT     print *, 'get h2o_e rc=',rc
!_RT  endif
!_RT  h2o_e(:,start_pt:end_pt)=buf(:,1:n_events)
      h2o_e(:,start_pt:end_pt)=1.e15

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

      return
      end subroutine read_v2_

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

      kxnames(294)=
     $'SABER - Sounding of the Atmosphere Broadband Emission Radiometer'

      ktnames(7)='Upper-air water vapor mixing ratio'
      ktunits(7)='g/kg'
      ktnames(8)='Upper-air temperature'
      ktunits(8)='Kelvin'
      ktnames(43)='Density of atmosphere'
      ktunits(43)='1/cm*3'
      
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


      end module m_saber



