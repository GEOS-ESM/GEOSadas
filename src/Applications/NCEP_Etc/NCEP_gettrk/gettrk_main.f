      program trakmain
c
c$$$  MAIN PROGRAM DOCUMENTATION BLOCK
c
c Main Program: GETTRK       Track model vortices   
C   PRGMMR: MARCHOK          ORG: NP22        DATE: 2002-05-20
c
c ABSTRACT: This program tracks the average of the max or min 
c   of several parameters in the vicinity of an input
c   first guess (lat,lon) position of a vortex in order to give  
c   forecast position estimates for that vortex for a given numerical
c   model.  For the levels 700 & 850 mb, the tracked parameters are:
c   Relative vorticity (max), wind magnitude (min), and geopotential
c   height (min).  Also tracked is the min in the MSLP.  So many
c   parameters are tracked in order to provide more accurate position
c   estimates for weaker storms, which often have poorly defined
c   structures/centers.  Currently, the system is set up to be able
c   to process GRIB input data files from the AVN, MRF, UKMET, GDAS,
c   ECMWF, NGM, Eta and FNMOC/NOGAPS models.  Two 1-line files
c   are  output from this program, both containing the forecast fix
c   positions that the  tracker has obtained.  One of these  output 
c   files contains the positions at every 12 hours from forecast 
c   hour 0 to the end of the forecast. The other file is in ATCF 
c   format, which is the particular format needed by the Tropical
c   Prediction Center, and provides the positions at forecast hours
c   12, 24, 36, 48 and 72, plus the maximum wind near the storm center
c   at each of those forecast hours.
c
c Program history log:
c   98-03-16  Marchok - Original operational version.
c   98-07-15  Marchok - Added code to calculate radii of gale-, storm-,
c                       and hurricane-force winds in each quadrant.
c   99-04-01  Marchok - Added code to be able to read in 4-digit years
c                       off of the TC Vitals records.
c                       Added code, including subroutine  is_it_a_storm,
c                       to make a better determination of whether or 
c                       not the center that was found at each time is
c                       the center of a storm, and not just a passing
c                       vort max, etc.
c   99-06-15  Marchok - Fixed a bug in calcdist that was triggered by a
c                       rounding error sending a number just above 1 
c                       into ACOS to get the distance between 2 
c                       identical points (which, obviously, is 0).
c   00-06-20  Marchok - Added GDAS option for vortex relocation work.
c                       Changed nhalf from 3 to 5.  Relaxed the 
c                       requirements for pthresh and vthresh.
c   00-11-30  Marchok - Added ability to handle GFDL and NCEP Ensemble
c                       model data.  Extended time range to be able to
c                       handle 5-day capability.  Forecast hours are 
c                       now input via a namelist (easiest way to account
c                       for Eta, AVN and GFDL having different forecast
c                       lengths at 00/12z and 06/18z).  Model ID's are 
c                       now input via a namelist (makes it easier, for
c                       example, to run for many different ensemble 
c                       members).  Added new output, the atcfunix 
c                       format, needed for 5-day forecasts.
c   01-08-24  Marchok   Fixed a bug in rvcal and getgridinfo.  When a 
c                       grid that was south-->north is flipped in 
c                       conv1d2d_real to be north-->south, the scanning 
c                       mode flag remains 64 and what we would consider
c                       the max and min latitudes are reversed, so I 
c                       added code to correct this in both routines.
c   02-05-20  Marchok   Weakened the mslp gradient threshold and v850
c                       threshold in is_it_a_storm to cut down on the
c                       number of dropped storms.
c   03-03-18  Marchok   Fixed a bug in get_ij_bounds that was allowing
c                       a cos(90) and cos(-90), which then led to a
c                       divide by zero.
c   05-10-24  Terry     added model namelist
c
c Input files:
c   unit   11    Unblocked GRIB1 file containing model data
c   unit   12    Text file containing TC Vitals card for current time
c   unit   31    Unblocked GRIB index file
c
c Output files:
c   unit   61    Output file with forecast positions every 12h from 
c                vt=00h to the end of the forecast
c   unit   62    Output file in ATCF format, with forecast positions
c                at vt = 12, 24, 36, 48 and 72h, plus wind speeds.
c   unit   63    Output file with forecast wind radii for 34, 50 and
c                64 knot thresholds in each quadrant of each storm.
c
c Subprograms called:
c   read_nlists  Read input namelists for input date & storm number info
c   read_tcv_card Read TC vitals file to get initial storm position
c   getgridinfo  Read GRIB file to get basic grid information
c   tracker      Begin main part of tracking algorithm
c
c Attributes:
c   Language: Standard Fortran_90
c
c$$$
c
c-------
c
c     LOCAL:
c
c     ifhours:   Integer array holding numerical forecast times for
c                the input model (99 = no more times available).
c                These values are read in via a namelist.
c     Model numbers used: (1) AVN, (2) MRF, (3) UKMET, (4) ECMWF,
c                (5) NGM, (6) Early Eta, (7) NOGAPS, (8) GDAS,
c                (9) GFDL, (10) AVN Ensemble (11) AVN Ensemble
c                Relocation system (12) AVN ensemble control
c                analysis (initial time ONLY)
c     stormswitch:  This switch tells how to handle each storm in 
c                the TCV file:
c                1 = process this storm for this forecast hour.
c                2 = Storm was requested to be tracked, but either
c                    the storm went off the grid (regional models),
c                    the storm dissipated, or the program was
c                    unable to track it.
c                3 = Storm was NOT requested to be tracked at all.
c     storm:     An array of type tcvcard.  Each member of storm 
c                contains a separate TC Vitals card.
c     maxstorm:  Maximum number of storms the system is set up to 
c                handle at any 1 time.
c     slonfg,slatfg:  Holds first guess positions for storms.  The 
c                very first, first guess position is read from the
c                TC vitals card. (maxstorm,maxtime)
c     clon,clat: Holds the coordinates for the center positions for
c                all storms at all times for all parameters.
c                (max_#_storms, max_fcst_times, max_#_parms)
c
      USE def_vitals; USE inparms; USE set_max_parms; USE level_parms
      USE trig_vals; USE atcf; USE trkrparms
c
      implicit none 
c
      integer lugb,lugi,lucard,lout,numtcv
      integer itret,iggret,iicret,igcret,imax,jmax,iret
      integer ifhours(maxtime)
      integer igetpds(200),igetgds(200)
      integer maxstorm
      integer nquant
      real    grdspc

      parameter (lugb=11,lugi=31,lucard=12,lout=51)
c
      type (datecard) inp
      type (trackstuff) trkrinfo

c     --------------------------------------------------------
 
c-origcall w3tagb('GETTRK  ',1999,0104,0058,'NP22   ')

      pi = 4. * atan(1.)   ! Both pi and dtr were declared in module 
      dtr = pi/180.0       ! trig_vals, but were not yet defined.
c

      call read_nlists (inp,ifhours,trkrinfo,nquant,grdspc)

      if (ifhours(2) == 99) then  ! Only 1 time listed (24h ens reloc)
        interval_fhr = ifhours(1)
        if (interval_fhr == 0) then
          interval_fhr = 6
        endif
      else
        interval_fhr = ifhours(2) - ifhours(1)
      endif

      call read_tcv_card (lucard,maxstorm,trkrinfo,numtcv,iret)

      if (iret /= 0) then
        print '(/,a50,i4,/)','!!! ERROR: in read_tcv_card, rc= ',iret
        goto 890
      endif

      call open_grib_files (lugb,lugi,iret)

      if (iret /= 0) then
        print '(/,a50,i4,/)','!!! ERROR: in open_grib_files, rc= ',iret
        goto 890
      endif

      call getgridinfo (imax,jmax,grdspc,igetpds,igetgds,trkrinfo
     &                 ,iggret)
      if (iggret /= 0) then 
        print '(/,a50,i4,/)','!!! ERROR: in getgridinfo, rc= ',iggret
        goto 890
      endif

      call tracker (imax,jmax,grdspc,nquant,inp,maxstorm,numtcv
     &         ,ifhours,igetpds,igetgds,trkrinfo,itret)
c
890   continue
      call baclose(lugb,igcret)
      call baclose(lugi,iicret)
      print *,'baclose: igcret= ',igcret,' iicret= ',iicret
c-origcall w3tage('GETTRK  ')
      stop
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine tracker (imax,jmax,grdspc,nquant,inp,maxstorm,numtcv
     &          ,ifhours,igetpds,igetgds,trkrinfo,itret)
c
c     ABSTRACT: This subroutine is the core of the program.  It contains
c     the main loop for looping through all the forecast hours and all
c     the storms.  Basically, the way it works is that it has an outer 
c     loop that loops on the forecast hour.  At the beginning of this 
c     loop, the data are read in for all parameters and levels needed
c     for tracking.  The full regional or global grid is read in. 
c     If vorticity was not read in (some of the centers do not send us
c     vorticity), then vorticity calculations are done on the whole 
c     grid at both 850 and 700 mb.  Then the program goes into the inner
c     loop, which loops on storm number (program originally set up to 
c     handle a max of 15 storms).  For each storm, subroutine 
c     find_maxmin is called for the following parameters: Rel Vort and  
c     geopotential hgt at 700 & 850 mb, and MSLP.  Within find_maxmin,
c     a barnes analysis is performed over the guess position of the 
c     storm to find the max or min value, and then iteratively, the 
c     grid size is cut in half several times and the  barnes analysis
c     rerun to refine the positioning of the max or min location.  After
c     the center positions for these parameters have been obtained, 
c     subroutine  get_uv_center is called to get a center fix for the 
c     minimum in the wind field, specifically, a minimum in the
c     magnitude of the wind speed (vmag).  The calculation of the vmag
c     minimum is done differently than the calculation for the other
c     parameters;  for vmag, the grid near the storm center guess 
c     position is interpolated down to a very fine grid, and then 
c     find_maxmin is called and a barnes analysis is done on that 
c     smaller grid.  For vmag, there are no further calls made to barnes
c     with a smaller grid, since the grid has already been interpolated 
c     down to a smaller grid.  Once all of the parameter center fixes 
c     have been made, subroutine  fixcenter is called to average these 
c     positions together to get a best guess fix position.  Then a check
c     is done with a call to subroutine  is_it_a_storm to make sure that
c     the center that we have found does indeed resemble a tropical 
c     cyclone.  Finally, subroutine  get_next_ges is called to make a 
c     guess position for the next forecast time for this storm.
c
c     INPUT:
c     imax       i dimension of input model data grid 
c     jmax       j dimension of input model data grid 
c     grdspc     grid spacing of input model data grid
c     inp        contains input date and model number information
c     maxstorm   maximum # of storms to be handled
c     numtcv     number of storms read off of the tcvitals file
c     ifhours    array containing forecast hours for the input model
c     igetpds    pds info obtained from getgridinfo
c     igetgds    gds info obtained from getgridinfo
c     trkrinfo   derived type that holds/describes various tracker parms
c
c     OUTPUT:
c     itret      return code from this subroutine
c 
c     LOCAL PARAMETERS:
c     storm      contains the tcvitals for the storms
c     stormswitch 1,2 or 3 (see more description under Main pgm section)
c     slonfg     first guess array for longitude
c     slatfg     first guess array for latitude
c     maxtime    Max number of forecast times program can track
c     maxtp      Max number of tracked parameters program will track.
c                Currently (7/97), this maxtp is 9, and these 9 are
c                listed just a few lines below.
c     readflag   L  Indicates status of read for each of 13 parms:
c                1: 850 mb absolute vorticity
c                2: 700 mb absolute vorticity
c                3: 850 mb u-comp
c                4: 850 mb v-comp
c                5: 700 mb u-comp
c                6: 700 mb v-comp
c                7: 850 mb gp hgt
c                8: 700 mb gp hgt
c                9: MSLP
c                10: 500 mb u-comp
c                11: 500 mb v-comp
c                12: near-surface u-comp
c                13: near-surface v-comp
c
c     calcparm   L  indicates which parms to track and which not to.
c                Array positions are defined exactly as for clon
c                and clat, listed next, except that, in general, when
c                flag 3 is set to a value, flag 4 is set to the same 
c                value as 3, and when flag 5 is set to a value, flag
c                6 is set to the same value as 5.  This is because 
c                3 & 4 are for the 850 mb winds, and if either u or
c                v is missing, we obviously can't calculate the 
c                magnitude of the wind.  The same applies for 5 & 6,
c                which are for the 700 mb winds.
c     clon,clat: Holds the coordinates for the center positions for
c                all storms at all times for all parameters.
c                (max_#_storms, max_fcst_times, max_#_parms).
c                For the third position (max_#_parms), here they are:
c                1: Relative vorticity at 850 mb
c                2: Relative vorticity at 700 mb
c                3: Vector wind magnitude at 850 mb
c                4: NOT CURRENTLY USED
c                5: Vector wind magnitude at 700 mb
c                6: NOT CURRENTLY USED
c                7: Geopotential height at 850 mb
c                8: Geopotential height at 700 mb
c                9: Mean Sea Level Pressure
c     xmaxwind   Contains maximum near-surface wind near the storm
c                center for each storm at each forecast hour.
c                Currently, will not have this for ECMWF, since they
c                do not send us near-surface winds.
c     stderr     Standard deviation of the position "errors" of the 
c                different parameters for each storm at each time.
c     fixlat,fixlon: Contain the final coordinates for each storm at
c                each forecast hour.  These coordinates are a 
c                weighted average of all the individual parameter
c                positions (hgt, zeta, mslp, vmag).
c     cvort_maxmin: Contains the characters 'max' or 'min', and is 
c                used when calling the  find_maxmin routine for the
c                relative vorticity (Look for max in NH, min in SH).
c     vradius    Contains the distance from the storm fix position to
c                each of the various near-surface wind threshhold 
c                distances in each quadrant. 
c                (3,4) ==> (# of threshholds, # of quadrants)
c                Currently, will not have this for ECMWF, since they
c                do not send us near-surface winds.  See subroutine
c                getradii for further details.
c     isastorm   Character array used in the call to is_it_a_storm, 
c                tells whether the minimum requirement for an MSLP 
c                gradient was met (isastorm(1)), whether for the midlat
c                and tcgen cases if a closed mslp contour was found
c                (isastorm(2)), and if a circulation exists at 850 mb 
c                (isastorm(3)).  Can have a value of 'Y' (requirement 
c                met), 'N' (requirement not met) or 'U' (requirement 
c                undetermined, due to the fact that no center location 
c                was found for this parameter).
c     maxmini    These 2 arrays contain the i and j indeces for the
c     maxminj    max/min centers that are found using the rough check
c                in first_ges_ctr and subsequent routines.  Only needed
c                for a midlatitude or a genesis run, NOT needed for a
c                TC tracker run.
c     stormct    Integer: keeps and increments a running tab of the 
c                number of storms that have been tracked at any time 
c                across all forecast hours.  Used only for midlat or
c                tcgen runs.
c     gridprs    This contains the actual value of the minimum pressure
c                at a gridpoint.  The barnes analysis will return an 
c                area-averaged value of pressure; this variable will 
c                contain the actual minimum value at a gridpoint near
c                the lat/lon found by the barnes analysis.
c-----
c
      USE def_vitals; USE inparms; USE tracked_parms; USE error_parms
      USE set_max_parms; USE level_parms; USE grid_bounds; USE trkrparms
      USE contours
c
      implicit none
c
      type (datecard) inp
      type (trackstuff) trkrinfo
      type (cint_stuff) contour_info
c
      real, allocatable :: prstemp(:),iwork(:)
      integer, allocatable :: prsindex(:)
      integer   imax,jmax,ifh,ist,irf,jj,istmp,ifhtemp,itret
      integer   isiret1,isiret2,isiret3,idum,m,iix,jjx,imode,numtcv
      integer   iha,isa,iua,iva,iza,maxstorm,ivort,ifix,jfix,issret
      integer   ioaret,ioaxret,ifgcret,ifmret,igugret,isoiret,icccret
      integer   igrret,igmwret,iorret,ignret,iovret,icbret,igucret
      integer   ifilret,ifret,iaret,isret,iotmret,iwa,iisa,sl_counter
      logical(1) valid_pt(imax,jmax),readflag(13),masked_out(imax,jmax)
      logical(1) masked_outc(imax,jmax),lb(imax,jmax)
      logical(1) calcparm(maxtp,maxstorm)
      logical(1) tracking_previous_known_storms
      character cvort_maxmin*3,isastorm(3)*1,ccflag*1,gotten_avg_value*1
      character cmaxmin*3
      integer   ifhours(maxtime),igetpds(200),igetgds(200)
      integer   kpds(200),kgds(200)
      integer   vradius(3,4)
      integer   maxmini(maxstorm),maxminj(maxstorm)
      integer   ifcsthour,stormct,prevstormct,kf
      integer   nquant
      real      fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real      gridprs(maxstorm,maxtime)
      real      clon(maxstorm,maxtime,maxtp),xmask(imax,jmax)
      real      clat(maxstorm,maxtime,maxtp)
      real      xmaxwind(maxstorm,maxtime)
      real      stderr(maxstorm,maxtime),xval(maxtp)
      real      gridpoint_maxmin,dist,distnm,xknots,xmaxspeed,grdspc
      real      uvgeslon,uvgeslat,xavg,stdv,search_cutoff


c     First, allocate the working data arrays....

      allocate (zeta(imax,jmax,nlevm1),stat=iza)
      allocate (u(imax,jmax,nlevs),stat=iua)
      allocate (hgt(imax,jmax,nlevm1),stat=iha)
      allocate (v(imax,jmax,nlevs),stat=iva)
      allocate (slp(imax,jmax),stat=isa)

      if (iza /= 0 .or. iua /= 0 .or. iha /= 0 .or. 
     &    iva /= 0 .or. isa /= 0) then
        print *,' '
        print *,'!!! ERROR in sub tracker allocating arrays.'
        print *,'!!! iza = ',iza,' iua= ',iua,' iha= ',iha
        print *,'!!! iva = ',iva,' isa= ',isa
        itret = 94
        return
      endif

      allocate (prsindex(maxstorm),stat=iisa)
      allocate (prstemp(maxstorm),stat=iva)
      allocate (iwork(maxstorm),stat=iwa)
      if (iisa /= 0 .or. iva /= 0 .or. iwa /= 0) then
        print *,' '
        print *,'!!! ERROR in sub tracker allocating prsindex, prstemp'
        print *,'!!! or iwork array for storms: iisa = ',iisa
        print *,'!!! iva= ',iva,' iwa= ',iwa
        itret = 94
        return    
      endif

      clon = 0.0
      clat = 0.0
      stderr = stermn    ! initialize stderr to 0.1 (error_parms)
      itret = 0
      xmaxwind = 0.0
      stormct = 0

      ! It is critical to initialize the gridprs array to something
      ! greater than normal atmospheric pressures (I've chosen 9999.99 
      ! mb).  This is so that in the  sort on pressure before stormloop,
      ! the top of the  sorting index array will be filled with pressure
      ! values from active storms, while those inactive 9999 storms 
      ! will fill the bottom of the  sorting index array (prsindex).

      gridprs =  999999.0
      fixlon  =    -999.0
      fixlat  =    -999.0

      ifh = 1
      ifhloop: do while (ifhours(ifh) /= 99 .and. 
     &                   ifh <= maxtime)

        gotten_avg_value = 'n'

        if (ifhours(2) == 99) then   ! Only 1 time (24h ens reloc)
          ifcsthour = ifhours(1)
        else
          ifcsthour = (ifh-1)*interval_fhr
        endif

        masked_out  = .false.   ! Initialize all pts to false at each hr
        masked_outc = .false.   ! Initialize all pts to false at each hr

        print *,' '
        print *,'*-------------------------------------------*'
        print *,'*   New forecast hour -->  fhr = ',ifcsthour
        print *,'*-------------------------------------------*'

        print *,'in beginning of tracker, imax= ',imax,' jmax= ',jmax

c       Initialize all readflags to NOT FOUND for this forecast time,
c       then call subroutine to read data for this forecast time.

        zeta = 0.0; u = 0.0; hgt = 0.0; v = 0.0; slp = 0.0 
        readflag = .FALSE.
        call getdata (readflag,valid_pt,imax,jmax
     &               ,ifhours(ifh),inp,kpds,kgds,lb,nquant)
 
c       Count how many parms were successfully read for this fcst time.
c       Also, for right now, put the value of readflag into all of the
c       calcparms for parameters 3 through 9.  Note that in getdata we
c       read in 13 parms, but in this next loop we only check the 
c       readflags up to maxtp (= 9 at original writing).  That's because
c       parms 10 & 11 are for 500 mb u & v, which are not used for 
c       tracking, only for calculating the deep layer mean wind for
c       the next guess.  And parms 12 & 13 are for the near-surface 
c       winds, which are used in estimating surface winds near the storm
 
        idum = 0
        do irf = 1,maxtp
          if (readflag(irf)) idum = idum + 1
          if (irf > 2) then 
            do jj=1,maxstorm
              calcparm(irf,jj) = readflag(irf)
            enddo
          endif
        enddo          

        print *,' '
        print *,'Of ',maxtp,' trackable parms, you read in ',idum
        print *,'parms for this forecast hour from the input grib file.'

c       If not enough tracked parms were read in, exit the program....

        if (idum == 0) then
          print *,' '
          print *,'!!! ERROR in subroutine  tracker'
          print *,'!!! Not enough tracked parms read in from getdata.'
          print *,'!!! Check for a problem with the input GRIB file.'
          print *,'!!! Model identifier = ',inp%model
          print *,'!!! STOPPING EXECUTION FOR THIS MODEL'
          itret = 99
          ifhtemp = ifh
          do while (ifhtemp <= maxtime)
            do istmp=1,maxstorm
              fixlon (istmp,ifhtemp) = -999.0
              fixlat (istmp,ifhtemp) = -999.0
            enddo
            ifhtemp = ifhtemp + 1
          enddo
          call output_all (fixlon,fixlat,inp,ifhours,maxstorm,ioaret)
          call output_atcf (fixlon,fixlat,inp,ifhours,xmaxwind
     &                     ,maxstorm,ioaret)
          if (ifh == 1) then
            ! Per Jim Gross (1/01), if the  tracker ran but was unable
            ! to get an initial fix (or, in this case, unable to get 
            ! the data needed to run), write out zeroes for the 00h 
            ! fixes to indicate that the  tracker ran unsuccessfully, 
            ! but don't write out any subsequent forecast times
            ! with zeroes....
            vradius = 0
            do istmp = 1,maxstorm
              if (stormswitch(istmp) /= 3) then
                call output_atcfunix (-999.0,-999.0,inp,istmp
     &                   ,ifcsthour,0.0,0.0,vradius,maxstorm
     &                   ,trkrinfo,ioaxret)
              endif
            enddo
          endif
          return
        endif

c       Parameters 1 & 2 are abs vorticity at 850 & 700.  If the data 
c       files had this parm at 850 & 700 (ECMWF & UKMET do NOT), then 
c       we don't need to re-calculate relative vorticity, we just need 
c       to subtract out the Coriolis component.  If the files did not
c       have vorticity, then we need to calculate relative vorticity.
c       If we're able to read vorticity or calculate it, then set the
c       vorticity calcparms to TRUE for all storms for now.

        do ivort=1,2

          if (readflag(ivort)) then

            call subtract_cor (imax,jmax,igetgds,ivort)

            do jj=1,maxstorm
              calcparm(ivort,jj) = .TRUE.
            enddo
          else
            if (ivort == 1) then
              if (readflag(3) .and. readflag(4)) then
                call rvcal (imax,jmax,igetgds,ivort,valid_pt)
                do jj=1,maxstorm
                  calcparm(1,jj) = .TRUE.
                enddo
              else
                do jj=1,maxstorm
                  calcparm(1,jj) = .FALSE.
                enddo
              endif
            else
              if (readflag(5) .and. readflag(6)) then
                call rvcal (imax,jmax,igetgds,ivort,valid_pt)
                do jj=1,maxstorm
                  calcparm(2,jj) = .TRUE.
                enddo
              else
                do jj=1,maxstorm
                  calcparm(2,jj) = .FALSE.
                enddo
              endif
            endif
          endif

        enddo
 
c       ---------------------------------------------------------------
c       Now call  find_maxmin for the variables zeta, hgt and slp. Only
c       process those storms for which stormswitch is set to 1.  If a
c       storm is selected to be processed, we still have to check the
c       calcparm for each parameter, to make sure that the particular
c       parm exists at that level and is able to be processed.
c
c       The following commented-out data statements are just included 
c       as a reference so you can see the array positioning of the 
c       different parameters and levels:
c
c       data igparm   /41,41,33,34,33,34,7,7,2,33,34/
c       data iglevtyp /100,100,100,100,100,100,100,100,102,100,100/
c       data iglev    /850,700,850,850,700,700,850,700,0,500,500/
c
c       NOTE: For mid-latitude cases, we will track ONLY mslp, which
c       is why we set all the other calcparms to 'false' just below.

        if (trkrinfo%type == 'midlat') then
          do m = 1,maxstorm
            calcparm(1,m) = .false.
            calcparm(2,m) = .false.
            calcparm(3,m) = .false.
            calcparm(4,m) = .false.
            calcparm(5,m) = .false.
            calcparm(6,m) = .false.
            calcparm(7,m) = .false.
            calcparm(8,m) = .false.
          enddo
        endif

        if (trkrinfo%type == 'midlat' .or. trkrinfo%type == 'tcgen')
     &  then
          call sort_storms_by_pressure (gridprs,ifh,maxstorm,prsindex
     &                                 ,issret)
          if (ifh == 1) then
            stormct = numtcv
          endif
        endif

        prevstormct = stormct
        tracking_previous_known_storms = .true.

        stormloop: do sl_counter = 1,maxstorm

         if (trkrinfo%type == 'midlat' .or. trkrinfo%type == 'tcgen') 
     &   then
           ist = prsindex(sl_counter)
         else
           ist = sl_counter
         endif

         if (trkrinfo%type == 'midlat' .or. trkrinfo%type == 'tcgen') 
     &   then

           if (ist == (prevstormct + 1)) then

             ! For the mid-latitude and tropical cyclogenesis cases, we
             ! need to scan the mslp field to find new storms.  If we 
             ! are at this point inside the if statement in stormloop,
             ! then that means we have looped through and attempted to 
             ! track all storms that have already been found up to this 
             ! point in the forecast, and we need to scan the field for
             ! any new storms at this forecast hour.  If this is for 
             ! forecast hour = 0, then right off the bat we may be 
             ! scanning the field (if there were no tcvitals records
             ! read in for this forecast), since ist = 1 and 
             ! (prevstormct + 1) = 0 + 1 = 1.  All that the call just 
             ! below to first_ges_center does is return a rough idea 
             ! of the location of new lows; more specific locations are
             ! obtained through the barnes analysis tracking algorithm 
             ! further below.

             if (readflag(9)) then
               if (ifh > 1) then
                 ! We need the use of 2 different masks.  One 
                 ! (masked_out) is to be used when looking for new lows,
                 ! so that after we find a new low, we mask out the 
                 ! surrounding area so we don't find it on a subsequent 
                 ! search for this forecast hour.  The other 
                 ! (masked_outc) is used in the routine to check for a 
                 ! closed contour.  If checking for a closed contour
                 ! at, say 70W/25N, this and surrounding points may have
                 ! already been masked out in first_ges_center, so "N"
                 ! would misleadingly/incorrectly be returned from 
                 ! check_closed_contour, so that is why we need 2 masks.
                 ! But now after the first forecast hour (t=0), the way 
                 ! we have this set up is that we track previously known
                 ! storms first, and once we're done with them, we 
                 ! search for new storms at that same forecast hour.  
                 ! But when looking for new storms, we need to know the 
                 ! positions of the previously tracked storms at this 
                 ! current forecast hour, so we copy the masked_outc 
                 ! array to masked_out in this case....
                 masked_out = masked_outc
               endif
               call first_ges_center (imax,jmax,grdspc,'mslp',slp
     &                ,'min',trkrinfo,ifh,valid_pt,maxstorm,masked_out
     &                ,stormct,contour_info,maxmini,maxminj,ifgcret)
               tracking_previous_known_storms = .false.
             else
               print *,' '
               print *,'!!! ERROR: In subroutine  tracker, readflag for'
               print *,'!!!    mslp indicates that the mslp data is not'
               print *,'!!!    available for this forecast hour, and it'
               print *,'!!!    is needed for a "midlat" or "tcgen" run'
               print *,'!!!    of the  tracker.  We will exit....'
               print *,'!!!    readflag(9) = ',readflag(9)
               print *,'!!!    ifh= ',ifh
               print *,' '
               itret = 98
               return
             endif
           endif
         endif

         xval = 0.0     ! initialize entire xval array to 0
         isastorm = 'U' ! re-initialize flag for each time, each storm
 
         select case (stormswitch(ist))

          case (1)

            vradius = 0

            print *,' '
            print *,'   ---------------------------------------------'
            print *,'   | Beginning of storm loop in tracker for'
            print *,'   | Storm number ',ist,' fcst hour = '
     &                  ,ifcsthour
            print *,'   | Storm name = ',storm(ist)%tcv_storm_name
            print *,'   | Storm ID   = ',storm(ist)%tcv_storm_id
            print *,'   ---------------------------------------------'
            print *,' '

c           First, make sure storm is within the grid boundaries...
 
            call check_bounds (slonfg(ist,ifh),slatfg(ist,ifh),ist,ifh
     &                        ,icbret)
            if (icbret == 95) then   ! Out of regional grid bounds
              fixlon (ist,ifh) = -999.0
              fixlat (ist,ifh) = -999.0
              stormswitch(ist) = 2
              cycle stormloop
            endif

            if (slatfg(ist,ifh) > 0.0) then
              cvort_maxmin = 'max'
            else
              cvort_maxmin = 'min'
            endif

            if (calcparm(1,ist)) then
              print *,' '
              print *,'         ---    ---    ---'
              print *,'Now calling find_maxmin for zeta at 850 mb'
              call find_maxmin (imax,jmax,grdspc,'zeta'
     &           ,zeta(1,1,1),cvort_maxmin,ist,slonfg(ist,ifh)
     &           ,slatfg(ist,ifh),glon,glat,valid_pt,calcparm(1,ist)
     &           ,clon(ist,ifh,1),clat(ist,ifh,1),xval(1),ifmret)
              if (ifmret /= 0) then   ! Out of regional grid bounds
                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
                cycle stormloop
              endif 
            endif

            if (calcparm(2,ist)) then
              print *,' '
              print *,'         ---    ---    ---'
              print *,'Now calling find_maxmin for zeta at 700 mb'
              call find_maxmin (imax,jmax,grdspc,'zeta'
     &           ,zeta(1,1,2),cvort_maxmin,ist,slonfg(ist,ifh)
     &           ,slatfg(ist,ifh),glon,glat,valid_pt,calcparm(2,ist)
     &           ,clon(ist,ifh,2),clat(ist,ifh,2),xval(2),ifmret)
              if (ifmret /= 0) then   ! Out of regional grid bounds
                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
                cycle stormloop
              endif
            endif

            if (calcparm(7,ist)) then
              print *,' '
              print *,'         ---    ---    ---'
              print *,'Now calling find_maxmin for hgt at 850 mb'
              call find_maxmin (imax,jmax,grdspc,'hgt'
     &           ,hgt(1,1,1),'min',ist,slonfg(ist,ifh),slatfg(ist,ifh)
     &           ,glon,glat,valid_pt,calcparm(7,ist)
     &           ,clon(ist,ifh,7),clat(ist,ifh,7),xval(7),ifmret)
              if (ifmret /= 0) then   ! Out of regional grid bounds
                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
                cycle stormloop
              endif
            endif

            if (calcparm(8,ist)) then
              print *,' '
              print *,'         ---    ---    ---'
              print *,'Now calling find_maxmin for hgt at 700 mb'
              call find_maxmin (imax,jmax,grdspc,'hgt'
     &           ,hgt(1,1,2),'min',ist,slonfg(ist,ifh),slatfg(ist,ifh)
     &           ,glon,glat,valid_pt,calcparm(8,ist)
     &           ,clon(ist,ifh,8),clat(ist,ifh,8),xval(8),ifmret)
              if (ifmret /= 0) then   ! Out of regional grid bounds
                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
                cycle stormloop
              endif
            endif

            if (calcparm(9,ist)) then
              print *,' '
              print *,'         ---    ---    ---'
              print *,'Now calling find_maxmin for mslp'
              call find_maxmin (imax,jmax,grdspc,'slp'
     &           ,slp,'min',ist,slonfg(ist,ifh),slatfg(ist,ifh)
     &           ,glon,glat,valid_pt,calcparm(9,ist)
     &           ,clon(ist,ifh,9),clat(ist,ifh,9),xval(9),ifmret)
              if (ifmret /= 0) then   ! Out of regional grid bounds
                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
                cycle stormloop
              endif
            endif

c           Now get centers for V magnitude at 700 & 850 mb.  First,
c           get a modified guess lat/lon position for V magnitude.
c           Do this because it's more crucial to have a better first 
c           guess position for the wind minimum than it is for the 
c           other parms, since in addition to the wind minimum at the
c           center of the storm, you can also have many more wind 
c           minima outside the RMW (this is more of a concern in 
c           smaller and weaker storms).  This modified guess position
c           will be an average of the first guess position for this 
c           time and the  fix positions for this time from some of the
c           other parameters.

            if (calcparm(3,ist) .and. calcparm(4,ist)) then
              call get_uv_guess (slonfg(ist,ifh),slatfg(ist,ifh)
     &                          ,clon,clat,calcparm,ist,ifh,maxstorm
     &                          ,uvgeslon,uvgeslat,igugret)
              if (igugret == 0) then
                print *,' '
                print *,'          ---    ---    ---'
                print *,'Now calling get_uv_center for 850 mb '
                call get_uv_center (uvgeslon,uvgeslat,imax,jmax,grdspc
     &               ,ist,850,valid_pt,calcparm(3,ist)
     &               ,clon(ist,ifh,3),clat(ist,ifh,3),xval(3),igucret)
                if (igucret /= 0) then
                  calcparm(3,ist) = .FALSE.
                  calcparm(4,ist) = .FALSE.
                endif
              else
                calcparm(3,ist) = .FALSE.
                calcparm(4,ist) = .FALSE.
                clon(ist,ifh,3) = 0.0
                clat(ist,ifh,3) = 0.0
              endif 
            endif
  
            if (calcparm(5,ist).and. calcparm(6,ist)) then
              call get_uv_guess (slonfg(ist,ifh),slatfg(ist,ifh)
     &                          ,clon,clat,calcparm,ist,ifh,maxstorm
     &                          ,uvgeslon,uvgeslat,igugret)
              if (igugret == 0) then
                print *,' '
                print *,'          ---    ---    ---'
                print *,'Now calling get_uv_center for 700 mb '
                call get_uv_center (uvgeslon,uvgeslat,imax,jmax,grdspc
     &               ,ist,700,valid_pt,calcparm(5,ist)
     &               ,clon(ist,ifh,5),clat(ist,ifh,5),xval(5),igucret)
                if (igucret /= 0) then
                  calcparm(5,ist) = .FALSE.
                  calcparm(6,ist) = .FALSE.
                endif
              else 
                calcparm(5,ist) = .FALSE.
                calcparm(6,ist) = .FALSE.
                clon(ist,ifh,5) = 0.0
                clat(ist,ifh,5) = 0.0
              endif
            endif
  
c           ------------------------------------------------------
c           All of the parameter center fixes have been done.  Now 
c           average those positions together to get the best guess
c           fix position.  If a center fix is able to be made, then
c           call subroutine  get_max_wind to get the maximum near-
c           surface wind near the center, and then call  get_next_ges
c           to get a guess position for the next forecast hour.

            if (stormswitch(ist) == 1) then

              call fixcenter (clon,clat,ist,ifh,calcparm
     &             ,slonfg(ist,ifh),slatfg(ist,ifh),inp,ifcsthour
     &             ,stderr,fixlon,fixlat,xval,maxstorm,ifret)

              if (ifret == 0) then
                if (trkrinfo%type == 'midlat' .or.
     &              trkrinfo%type == 'tcgen') then
                  if (fixlon(ist,ifh) > (trkrinfo%eastbd + 7.0) .or.
     &                fixlon(ist,ifh) < (trkrinfo%westbd - 7.0) .or.
     &                fixlat(ist,ifh) > (trkrinfo%northbd + 7.0) .or.
     &                fixlat(ist,ifh) < (trkrinfo%southbd - 7.0)) then
                    print *,' '      
                    print *,'!!! For a midlat or tcgen case, a fix '
                    print *,'!!! will NOT be made for this time due'
                    print *,'!!! the storm being more than 7 degrees'
                    print *,'!!! outside the user-specified lat/lon'
                    print *,'!!! bounds for this run.  We will stop'
                    print *,'!!! tracking this storm.'
                    print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
                    print *,'!!! Storm    = ',storm(ist)%tcv_storm_name
                    print *,'!!! Fcst hr  = ',ifcsthour
                    print *,'!!! fixlat= ',fixlat(ist,ifh)
                    print *,'!!! fixlon= ',fixlon(ist,ifh)
                    print *,'!!! User East  Bound = ',trkrinfo%eastbd
                    print *,'!!! User West  Bound = ',trkrinfo%westbd
                    print *,'!!! User North Bound = ',trkrinfo%northbd
                    print *,'!!! User South Bound = ',trkrinfo%southbd
                    fixlon (ist,ifh) = -999.0
                    fixlat (ist,ifh) = -999.0
                    stormswitch(ist) = 2
                    if (ifh == 1) then
                      vradius = 0     
                      call output_atcfunix (-999.0
     &                      ,-999.0,inp,ist
     &                      ,ifcsthour,0.0
     &                      ,0.0,vradius,maxstorm,trkrinfo,ioaxret)
                    endif
                    cycle stormloop     
                  endif
                endif
              else
                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
              endif

c             Just because we've found a center doesn't mean there is
c             actually a storm there.  I noticed in the first year that
c             for some decaying or just weak storms, the  tracker would
c             identify a center to follow, but it may have only been
c             a weak trough passing by, or something else that's not
c             our storm.  This next subroutine checks to see that the 
c             surface pressure gradient and/or tangential winds at 
c             850 mb resemble a storm.  It is called twice; the first
c             time for MSLP, the 2nd time for 850 mb winds.  We will
c             apply these storm-checking criteria if either the mslp
c             or v850 check come back negative.  Remember, there
c             is the possibility that centers could not be found for 
c             1 or both of these parameters, in which case the isastorm
c             flag will have a value of 'U', for "undetermined".

              isiret1 = 0; isiret2 = 0; isiret3 = 0

              if (ifret == 0) then

                if (calcparm(9,ist)) then

                  ! Do a check of the mslp gradient....

                  call is_it_a_storm (imax,jmax,grdspc,'slp',ist
     &                 ,valid_pt,clon(ist,ifh,9),clat(ist,ifh,9)
     &                 ,xval(9),isastorm(1),isiret1)

                  ! If we have found a valid mslp gradient, then make
                  ! a call to fix_latlon_to_ij to (1) get the actual
                  ! gridpoint value of the mslp (the value previously
                  ! stored in xval(9) is an area-averaged value coming
                  ! from the barnes analysis), and (2) to get the 
                  ! (i,j) indeces for this gridpoint to be used in the
                  ! call to check_closed_contour below.

                  if (isiret1 == 0 .and. isastorm(1) == 'Y') then
                    call fix_latlon_to_ij (imax,jmax,grdspc,slp,'min'
     &                 ,valid_pt,clon(ist,ifh,9),clat(ist,ifh,9)
     &                 ,xval(9),ifix,jfix,gridpoint_maxmin,ifilret)
                    if (ifilret == 0) then  
                      gridprs(ist,ifh) = gridpoint_maxmin
                    else          
                      ! Search went out of regional grid bounds....
                      fixlon (ist,ifh) = -999.0
                      fixlat (ist,ifh) = -999.0
                      stormswitch(ist) = 2
                      cycle stormloop     
                    endif
                  endif

                  ! For the midlat & tcgen cases, do a check to see if
                  ! there is a closed mslp contour.  The ifix and jfix
                  ! values passed into check_closed_contour are the 
                  ! values for the (i,j) at the gridpoint minimum, 
                  ! which was obtained just above from the call to
                  ! fix_latlon_to_ij.

                  if (isastorm(1) == 'Y' .and. isiret1 == 0 .and.
     &                (trkrinfo%type == 'midlat' .or.
     &                 trkrinfo%type == 'tcgen')) then
                    print *,' '
                    print *,'Before call to check_closed_contour, '
                    print *,'ifix= ',ifix,' jfix= ',jfix
                    print *,'clon= ',clon(ist,ifh,9),'E   ('
     &                     ,360-clon(ist,ifh,9),'W)'
                    print *,'clat= ',clat(ist,ifh,9)
                    print *,'xval= ',xval(9)
                    if (contour_info%numcont == 0) then
                      contour_info%numcont = maxconts
                    endif
                    call check_closed_contour (imax,jmax,ifix,jfix,slp
     &                  ,valid_pt,masked_outc,ccflag,'min',trkrinfo
     &                  ,999,contour_info,icccret)
                    print *,' '
                    print *,'After call to check_closed_contour, '
                    print *,'ccflag= ',ccflag
                    print *,' '
                    if (ccflag == 'y') then
                      isastorm(2) = 'Y'
                    else if (ccflag == 'n') then
                      isastorm(2) = 'N'
                    endif
                  endif

                else

                  if (trkrinfo%type == 'midlat' .or.
     &                trkrinfo%type == 'tcgen') then
                    isastorm(1) = 'N'
                    print *,' '
                    print *,'!!! For a midlat or tcgen case, a fix '
                    print *,'!!! could not be made for mslp, '
                    print *,'!!! therefore we will stop tracking '
                    print *,'!!! for this storm.'
                    print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
                    print *,'!!! Storm    = ',storm(ist)%tcv_storm_name
                    print *,'!!! Fcst hr  = ',ifcsthour
                    fixlon (ist,ifh) = -999.0
                    fixlat (ist,ifh) = -999.0
                    stormswitch(ist) = 2
                    cycle stormloop
                  endif

                endif

                ! Check the avg 850 mb tangential windspeed....

                if (calcparm(3,ist)) then
                  call is_it_a_storm (imax,jmax,grdspc,'v850',ist
     &                 ,valid_pt,clon(ist,ifh,3),clat(ist,ifh,3)
     &                 ,xval(3),isastorm(3),isiret3)
                endif

                if (isiret1 /= 0 .or. isiret2 /= 0 .or. isiret3 /= 0) 
     &          then 
                  print *,' '
                  print *,'!!! ERROR: One of the calls to '
                  print *,'!!! is_it_a_storm produced an error.'
                  print *,'!!! Chances are this is from a call to '
                  print *,'!!! get_ij_bounds, meaning we are too close'
                  print *,'!!! to a regional grid boundary to do this '
                  print *,'!!! analysis.  Processing will continue....'
                  print *,'!!! isiret1= ',isiret1,' isiret2= ',isiret2
                  print *,'!!! isiret3= ',isiret3
                endif

                if (isastorm(1) == 'N' .or. isastorm(2) == 'N' .or.
     &              isastorm(3) == 'N') then
                  print *,' '
                  print *,'!!! At least one of the isastorm flags from'
                  print *,'!!! subroutine  is_it_a_storm is "N", so '
                  print *,'!!! either we were unable to find a good '
                  print *,'!!! mslp gradient and/or a valid 850 mb '
                  print *,'!!! circulation for the storm at this time,'
                  print *,'!!! or, for the cases of midlat or tcgen '
                  print *,'!!! tracking, a closed mslp contour could '
                  print *,'!!! not be found, thus we will stop tracking'
                  print *,'!!! this storm.'
                  print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
                  print *,'!!! Storm    = ',storm(ist)%tcv_storm_name
                  print *,'!!! Fcst hr  = ',ifcsthour
                  print *,'!!! mslp gradient flag = ',isastorm(1)
                  print *,'!!! closed contour flag = ',isastorm(2)
                  print *,'!!! 850 mb winds flag = ',isastorm(3)
                  print *,' '

                  fixlon (ist,ifh) = -999.0
                  fixlat (ist,ifh) = -999.0
                  stormswitch(ist) = 2
                endif

                ! Now do another check for the  tracker and tcgen cases.
                ! If the isastorm flags for mslp gradient and v850 BOTH
                ! came back positive AND you have been able to locate an
                ! 850 mb vort center, just do a check to make sure that
                ! the distance between the 850 vort center and the mslp
                ! center is not too great.

                if (trkrinfo%type == 'tracker' .or. 
     &              trkrinfo%type == 'tcgen') then
                  if (isastorm(1) == 'Y' .and. isastorm(3) == 'Y' .and.
     &                calcparm(1,ist) .and. stormswitch(ist) == 1) then
                    call calcdist (clon(ist,ifh,9),clat(ist,ifh,9)
     &                            ,clon(ist,ifh,1),clat(ist,ifh,1),dist)
                    if (dist > max_mslp_850) then
                      print *,' '
                      print *,'!!! In routine  tracker, the dist betw'
                      print *,'!!! the mslp center & the 850 zeta '
                      print *,'!!! center is too great, thus we will'
                      print *,'!!! stop tracking this storm.'
                      print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
                      print *,'!!! Storm    = '
     &                       ,storm(ist)%tcv_storm_name
                      print *,'!!! Fcst hr  = ',ifcsthour
                      print *,'!!! Max dist allowed (km) = '
     &                       ,max_mslp_850
                      print *,'!!! Actual distance  (km) = ',dist
                      print *,' '
  
                      fixlon (ist,ifh) = -999.0
                      fixlat (ist,ifh) = -999.0
                      stormswitch(ist) = 2
                    else
                      print *,' '
                      print *,'Distance between the parm centers for'
                      print *,'850 zeta and mslp is ',dist,' (km)'
                    endif
                  endif
                endif

                ! Do one final check.  Check the new fix position and 
                ! the old fix position and calculate the speed that the
                ! storm would have had to travel to get to this point.
                ! If that speed exceeds a certain threshold (~60 kt), 
                ! assume you're tracking the wrong thing and quit.
                ! Obviously, only do this for times > 00h.  The check
                ! in the if statement to see if the previous hour's 
                ! lats and lons were > -999 is for the midlat and 
                ! tcgen cases -- remember, they can have genesis at
                ! any hour of the forecast, in which case the previous
                ! forecast hour's lat & lon would be -999.

                if (ifh > 1 .and. stormswitch(ist) .eq. 1 .and. 
     &              fixlon(ist,ifh-1) > -999.0 .and.
     &              fixlat(ist,ifh-1) > -999.0 ) then

                  if (trkrinfo%type == 'midlat') then
                    xmaxspeed = maxspeed_ml
                  else
                    xmaxspeed = maxspeed_tc
                  endif

                  call calcdist (fixlon(ist,ifh-1),fixlat(ist,ifh-1)
     &                          ,fixlon(ist,ifh),fixlat(ist,ifh),dist)

                  ! convert distance from km to nm and get speed.

                  distnm = dist * 0.539638
                  xknots = distnm / interval_fhr

                  if (xknots > xmaxspeed) then
                    print *,' '
                    print *,'!!! In routine  tracker, calculated speed'
                    print *,'!!! of the storm from the last position to'
                    print *,'!!! the current position is too great, '
                    print *,'!!! thus we will stop tracking this storm'
                    print *,'!!! (For fear that we are not actually '
                    print *,'!!! tracking our storm, but have instead'
                    print *,'!!! locked onto some other feature....)'
                    print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
                    print *,'!!! Storm    = ',storm(ist)%tcv_storm_name
                    print *,'!!! Fcst hr  = ',ifcsthour
                    print *,'!!! Max speed allowed (kt) = ',xmaxspeed
                    print *,'!!! Actual speed      (kt) = ',xknots
                    print *,' '

                    fixlon (ist,ifh) = -999.0
                    fixlat (ist,ifh) = -999.0
                    stormswitch(ist) = 2
                  else
                    print *,' '
                    print *,'The average speed that the storm moved at'
                    print *,'since the previous forecast time is '
     &                     ,xknots,' knots.'
                  endif

                endif
 
              endif
 
c             Now get the maximum near-surface wind speed near the storm
c             center (get_max_wind).  Also, call  getradii to get the 
c             radii in each storm quadrant of gale-force, storm-force 
c             and hurricane force winds.

              if (readflag(12) .and. readflag(13) .and. ifret == 0
     &            .and. stormswitch(ist) == 1) then
                call get_max_wind (fixlon(ist,ifh),fixlat(ist,ifh)
     &                            ,imax,jmax,grdspc,valid_pt,levsfc
     &                            ,xmaxwind(ist,ifh),igmwret)
                vradius = 0
                call getradii (fixlon(ist,ifh),fixlat(ist,ifh),imax,jmax
     &                        ,grdspc,valid_pt,storm(ist)%tcv_storm_id
     &                        ,ifcsthour,vradius,igrret)
                if (igrret == 0) then
                  call output_radii (maxstorm,vradius,inp
     &                               ,ifcsthour,ist,iorret)
                endif
              endif

c             Now print out the current fix position and intensity
c             (in knots) to standard output.  Conversion for m/s to
c             knots (1.9427) is explained in output_atcf.

              print *,' '
              print *,'After call to fixcenter, fix positions at '
              print *,'forecast hour= ',ifcsthour,' follow:'
              print *,' '

              if (ifret == 0 .and. stormswitch(ist) == 1) then
                write (6,73) storm(ist)%tcv_storm_id
     &                      ,ifcsthour,fixlon(ist,ifh)
     &                      ,360.-fixlon(ist,ifh),fixlat(ist,ifh)
     &                      ,int((xmaxwind(ist,ifh)*1.9427) + 0.5)
                print *,' '
                if (mod(ifcsthour,6) == 0) then
                  print *,'TTT calling atcfunix, ist= ',ist,' ifh= ',ifh
                  call output_atcfunix (fixlon(ist,ifh)
     &                      ,fixlat(ist,ifh),inp,ist
     &                      ,ifcsthour,xmaxwind(ist,ifh)
     &                      ,gridprs(ist,ifh),vradius,maxstorm
     &                      ,trkrinfo,ioaxret)
                  if (inp%model == 12 .and. ifcsthour == 0) then
                    ! Write vitals for AVN ens control analysis
                    call output_tcvitals (fixlon(ist,ifh)
     &                      ,fixlat(ist,ifh),inp,ist,iovret)
                  endif
                endif
              else
                print *,'fixpos ',storm(ist)%tcv_storm_id
     &                 ,' fhr= ',ifcsthour
     &                 ,' Fix not made for this forecast hour'
                print *,' '
                print *,'!!! RETURN CODE from fixcenter not equal to 0,'
                print *,'!!! or output from is_it_a_storm indicated the'
                print *,'!!! system found was not our storm, or the '
                print *,'!!! speed calculated indicated we may have '
                print *,'!!! locked onto a different center, thus a fix'
                print *,'!!! was not made for this storm at this '
                print *,'!!! forecast hour.'
                print *,'!!! mslp gradient check       = ',isastorm(1)
                print *,'!!! mslp closed contour check = ',isastorm(2)
                print *,'!!! 850 mb winds check      = ',isastorm(3)
                print *,'!!! fixcenter return code = ifret = ',ifret
                print *,' '
                if (ifh == 1) then
                  vradius = 0
                  call output_atcfunix (-999.0
     &                      ,-999.0,inp,ist
     &                      ,ifcsthour,0.0
     &                      ,0.0,vradius,maxstorm,trkrinfo,ioaxret)
                endif
                cycle stormloop
              endif


c             Now get first guess for next forecast time's position

              if (ifh < maxtime) then
                if (ifhours(ifh+1) /= 99) then
                  call get_next_ges (fixlon,fixlat,ist,ifh,ifhours
     &              ,imax,jmax,grdspc,inp%model,valid_pt,readflag
     &              ,maxstorm,ignret)
                  if (ignret /= 0) then
                    fixlon (ist,ifh) = -999.0
                    fixlat (ist,ifh) = -999.0
                    stormswitch(ist) = 2
                    cycle stormloop
                  endif
                endif
              endif
 
            endif

          case (2)
            fixlon (ist,ifh) = -999.0
            fixlat (ist,ifh) = -999.0
            print *,' '
            print *,'!!! Case 2 in tracker for stormswitch'
            print *,'!!! Storm name = ',storm(ist)%tcv_storm_name
            print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
            if (ifh == 1) then
              vradius = 0
              call output_atcfunix (-999.0
     &                  ,-999.0,inp,ist
     &                  ,ifcsthour,0.0
     &                  ,0.0,vradius,maxstorm,trkrinfo,ioaxret)
            endif

          case (3)
            continue
c            print *,' '
c            print *,'!!! Case 3 in tracker for stormswitch'
c            print *,'!!! Storm name = ',storm(ist)%tcv_storm_name
c            print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id

          end select
 
        enddo stormloop

        if (trkrinfo%type == 'midlat' .or.
     &      trkrinfo%type == 'tcgen') then
          call output_tracker_mask (masked_outc,kpds,kgds,lb,ifh
     &                ,imax,jmax,iotmret)
        endif

        ifh = ifh + 1
        if (ifh > maxtime) exit ifhloop

      enddo ifhloop

      call output_all (fixlon,fixlat,inp,ifhours,maxstorm,ioaret)
 
      call output_atcf (fixlon,fixlat,inp,ifhours,xmaxwind,maxstorm
     &                 ,ioaret)
c
  73  format ('fixpos  ',a3,'  fhr= ',i3,'   Fix position=  '
     &       ,f7.2,'E  (',f6.2,'W)',2x,f7.2,'   Max Wind= ',i3,' kts')

      deallocate (prstemp); deallocate (prsindex); deallocate(iwork)
c
      return 
      end   

c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine open_grib_files (lugb,lugi,iret)

C     ABSTRACT: This subroutine must be called before any attempt is 
C     made to read from the input GRIB files.  The GRIB and index files
C     are opened with a call to baopenr.  This call to baopenr was not 
C     needed in the cray version of this program (the files could be 
C     opened with a simple Cray assign statement), but the GRIB-reading 
C     utilities on the SP do require calls to this subroutine (it has 
C     something to do with the GRIB I/O being done in C on the SP, and 
C     the C I/O package needs an explicit open statement).
C
C     INPUT:
C     lugb     The Fortran unit number for the GRIB data file
C     lugi     The Fortran unit number for the GRIB index file
C
C     OUTPUT:
C     iret     The return code from this subroutine

      character fnameg*7,fnamei*7

      fnameg(1:5) = "fort."
      fnamei(1:5) = "fort."
      write(fnameg(6:7),'(I2)') lugb
      write(fnamei(6:7),'(I2)') lugi
      call baopenr (lugb,fnameg,igoret)
      call baopenr (lugi,fnamei,iioret)

      print *,' '
      print *,'baopen: igoret= ',igoret,' iioret= ',iioret

      if (igoret /= 0 .or. iioret /= 0) then
        print *,' '
        print *,'!!! ERROR in sub open_grib_files opening grib file'
        print *,'!!! or grib index file.  baopen return codes:'
        print *,'!!! grib  file return code = igoret = ',igoret
        print *,'!!! index file return code = iioret = ',iioret
        iret = 93
        return
      endif

      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine is_it_a_storm (imax,jmax,grdspc,cparm,ist
     &                          ,defined_pt,parmlon,parmlat
     &                          ,parmval,stormcheck,isiret)

c     ABSTRACT: This subroutine is called after the center of the storm
c     has been fixed.  Its purpose is to determine whether or not 
c     the center that was found is actually a storm, and not just some
c     passing trough (this has happened in the case of decaying or weak
c     storms).  It's called twice -- once to check for a minimum MSLP
c     gradient, and once to check for a circulation at 850 mb.  The 
c     subroutine input parameter "cparm" determines which parameter to
c     check for.
c
c     INPUT:
c     imax     Num pts in i direction on input grid
c     jmax     Num pts in j direction on input grid
c     grdspc   Grid spacing on input grid
c     cparm    Char string indicating what parm is to be checked:
c              slp  = mslp, for a check of mslp gradient
c              v850 = tangential winds at 850 mb
c     ist      integer storm number (internal to the  tracker)
c     defined_pt Logical; bitmap indicating if valid data at that pt.
c     parmlon  Longitude of the max/min value for the input parameter
c     parmlat  Latitude  of the max/min value for the input parameter
c     parmval  Data value at parm's max/min point (used for mslp call)
c
c     OUTPUT:
c     stormcheck Character; set to 'Y' if mslp gradient or 850 mb 
c                tangential winds check okay.
c     isiret   Return code for this subroutine.
c
      USE radii; USE grid_bounds; USE set_max_parms; USE level_parms
      USE trig_vals; USE tracked_parms; USE atcf
c
      real         vt,vtavg,vr,grdspc,parmlat,parmlon,parmval
      real         pthresh,vthresh
      character(*) cparm
      logical(1)   defined_pt(imax,jmax)
      character*1  stormcheck

      isiret = 0
      stormcheck = 'N'

      dell = grdspc

c     First define the radius of influence, which depends on the
c     grid spacing of the model data being used.  The ceiling statement
c     for npts in the first if statement is needed in case the
c     resolution of the grib files eventually goes very low, down to
c     say a half degree or less, in order to cover enough points in
c     the search.

      if (dell < 1.24) then      ! AVN, MRF, Eta, NGM, NOGAPS, GDAS,
                                 ! GFDL, NCEP Ensemble & Ensemble
                                 ! Relocation
        ri     = ritrk_most
        if (cparm == 'slp') then
          radinf = 300.0
        else
          radinf = 225.0
        endif
        npts   = ceiling(radinf/(dtk*grdspc))
      else if (dell >= 1.24 .and. dell < 2.49) then     ! UKMET
        ri     = ritrk_most     
        radinf = 275.0
        npts   = 2
      else                       ! ECMWF
        ri     = ritrk_coarse
        radinf = 350.0
        npts   = 1
      endif

      if (atcfname(1:3) == 'GDA' .or. atcfname(1:2) == 'YP' .or.
     &    atcfname(1:2) == 'YN' .or. atcfname(1:2) == 'ZP' .or.
     &    atcfname(1:2) == 'ZN' .or. atcfname == 'AEAR') then
                    ! Different for relocation in GDAS and AVN
                    ! Ensemble, by Qingfu Liu's request...
        pthresh = 0.0020
        vthresh = 2.0000
      else                ! For all other models, weaker...
        pthresh = 0.0015
        vthresh = 1.5000
      endif

      call get_ij_bounds (npts,0,ri,imax,jmax,grdspc
     &           ,glatmax,glatmin,glonmax,glonmin,parmlon,parmlat
     &           ,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)

      if (igiret /= 0) then
        print *,' '
        print *,'!!! ERROR in is_it_a_storm from call to get_ij_bounds,'
        print *,'!!! stopping processing for storm number ',ist
        isiret = 92
        return
      endif

c     If the input cparm is slp, then check to see that the MSLP 
c     gradient in any direction from the MSLP center is at least 
c     1mb / 200km, or 0.005mb/km.  This is based on discussions with 
c     Morris & Bob, who have had good results using a 2mb/200km 
c     requirement.  Since their model has a much finer resolution than
c     all of the models we run the  tracker on AND a much better 
c     depiction of the hurricane vortex, we do not use a requirement
c     as strict as theirs, and so make the requirement only half as
c     strong as theirs.
c
c     If the input cparm is v850, then check to see that there is
c     a circulation at 850 mb.  We will do this by calculating the
c     tangential wind of all points within a specified radius of 
c     the 850 minimum wind center, and seeing if there is a net
c     average tangential wind speed of at least 5 m/s.
c
c     UPDATE APRIL 2000: I've relaxed the thresholds slightly from
c     0.005 mb/km to 0.003 mb/km, and the wind threshold from 
c     5 m/s to 3 m/s.  Also, note that a special case for GDAS has
c     been hardwired in that is weaker (0.002 mb/km and 2 m/s).
c     That weaker GDAS requirement is for Qingfu's relocation stuff.
c
c     UPDATE JULY 2001: The relaxed requirement put in place in
c     April 2000 for the GDAS relocation has also been put in place
c     for the AVN ensemble relocation.

      print *,' '
      print *,'In is_it_a_storm, ilonfix= ',ilonfix
     &       ,' jlatfix= ',jlatfix
      print *,'ibeg jbeg iend jend = ',ibeg,jbeg,iend,jend
      print *,'cparm= ',cparm,'  parmlon parmlat = ',parmlon,parmlat
      print *,'parmval= ',parmval
      print *,' '

      vtavg = 0.0
      ivt   = 0

      xmaxpgrad = -999.0

      jloop: do j = jbeg,jend
        iloop: do i = ibeg,iend
  
          call calcdist(parmlon,parmlat,glon(i),glat(j),dist)

          if (dist > radinf .or. dist == 0.0) cycle

          if (defined_pt(i,j)) then

            if (cparm == 'slp') then
              pgradient = ((slp(i,j) - parmval)/100.0) / dist
              if (pgradient > xmaxpgrad) xmaxpgrad = pgradient
              write (6,93) i,j,glon(i),glat(j),dist,slp(i,j),pgradient
              if (pgradient > pthresh) then
                print *,' '
                print *,'In is_it_a_storm, valid pgradient found.'
                print '(a23,f8.5)',' pgradient threshold = ',pthresh
                print '(a23,f8.5)',' pgradient found     = ',pgradient
                print *,'mslp center = ',parmlon,parmlat,parmval
                print *,'pgrad loc   = ',glon(i),glat(j),slp(i,j)
                stormcheck = 'Y'
                exit jloop
              endif
            endif

            if (cparm == 'v850') then
              call getvrvt (parmlon,parmlat,glon(i),glat(j)
     &             ,u(i,j,nlev850),v(i,j,nlev850),vr,vt,igvtret)
              write (6,91) i,j,glon(i),glat(j),u(i,j,nlev850)
     &                    ,v(i,j,nlev850),vr,vt

              vtavg = vtavg + vt
              ivt   = ivt + 1
            endif

          endif
              
        enddo iloop
      enddo jloop

  91  format (1x,'i= ',i3,' j= ',i3,' glon= ',f7.2,' glat= ',f6.2
     &       ,' u= ',f8.4,' v= ',f8.4,' vr= ',f9.5,' vt= ',f9.5)

  93  format (1x,'i= ',i3,' j= ',i3,' glon= ',f7.2,' glat= ',f6.2
     &       ,' dist= ',f8.2,' slp= ',f10.2,' pgradient= ',f8.5)

      if (stormcheck /= 'Y' .and. cparm == 'slp') then
        print *,' '
        print *,'!!! In is_it_a_storm, valid pgradient NOT FOUND.'
        print *,'!!! (Max pgradient less than ',pthresh,' mb/km)'
        write (6,95) '!!! Max pgradient (mb/km) found = ',xmaxpgrad
  95    format (1x,a34,f8.5)
        print *,' '
      endif

      if (cparm == 'v850') then

        if (ivt > 0) then
          vtavg = vtavg / float(ivt)
        else
          vtavg = 0.0
        endif

        if (parmlat > 0) then
          if (vtavg >= vthresh) then
            stormcheck = 'Y'
            print *,' '
            print *,' In is_it_a_storm, average 850 tangential'
     &       ,' winds are OKAY (>= +',vthresh,' m/s for a NH storm).'
            print *,' Avg 850 tangential winds = ',vtavg,' m/s'
            print *,' '
          else
            print *,' '
            print *,'!!! In is_it_a_storm, average 850 tangential'
            print *,'!!! winds did NOT exceed +',vthresh
     &             ,' m/s (NH storm).'
            print *,'!!! Avg 850 tangential winds = ',vtavg,' m/s'
            print *,' '
          endif
        else
          if (vtavg <= -vthresh) then
            stormcheck = 'Y'
            print *,' '
            print *,' In is_it_a_storm, average 850 tangential'
     &       ,' winds are OKAY (<= -',vthresh,' m/s for a SH storm).'
            print *,' Avg 850 tangential winds = ',vtavg,' m/s'
            print *,' '
          else
            print *,' '
            print *,'!!! In is_it_a_storm, average 850 tangential'
            print *,'!!! winds did NOT exceed -',vthresh
     &             ,' m/s (SH storm).'
            print *,'!!! Avg 850 tangential winds = ',vtavg,' m/s'
            print *,' '
          endif
        endif

      endif
c
      return
      end

c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine sort_storms_by_pressure (gridprs,ifh,maxstorm,sortindex
     &                                   ,issret)
c    
c     ABSTRACT: This subroutine  sorts storms by mslp.  It is called by
c     subroutine  tracker just before the loop for "stormloop" is done
c     for all the storms at a particular forecast hour.  It is only 
c     called for the "midlat" and "tcgen" cases.  The end result of
c     this sort is an array (prsindex) that contains the indeces of 
c     the storms, arranged from lowest pressure to highest (and note
c     that the "undefined" storms have a pressure of 9999.99 mb and
c     thus get sorted to the bottom of the array).  The purpose of 
c     doing this is so that we track the most intense storms first.
c     Why go to the trouble?  Imagine a scenario in which we are 
c     tracking a complex system in which there are 2 low pressure 
c     centers.  Let's say that one is becoming dominant and 
c     intensifying, while the other is weakening.  Now, let's assume
c     that the weakening one eventually gets absorbed into the 
c     stronger, more dominant low.  Now we only have 1 low, but if in
c     the tracker stormloop, we first process the data for the 
c     weakening low, we will attribute the track to that storm, and
c     then when we get to the point in the loop where we are trying
c     to get the track for the stronger storm, we will (erroneously)
c     stop the tracking for that storm since the storm center has 
c     already been attributed to the weaker storm.  But by using this
c     subroutine, we will track the stronger storm first, and thus 
c     avoid this problem.
c
c     NOTE: The pressures used in the  sort are those obtained at the
c     previous forecast hour.  At forecast hour = 0, just use the 
c     values as they were input to this routine, since they were 
c     found in first_ges_center from strongest to weakest already.
c
c     INPUT:
c     gridprs  real array of storm mslp values
c     ifh      integer index for the current forecast hour
c     maxstorm max num of storms that can be handled in this run
c
c     OUTPUT: 
c     sortindex contains a sorted array of indeces.  The orders 
c               sort routine does NOT rearrange the data.  Rather, it
c               returns this array of sorted indeces which point to 
c               the correct order of data values in the data array.
c     issret    return code from this subroutine
c
      USE set_max_parms
c
      real, allocatable :: prstemp(:),iwork(:)
      real      gridprs(maxstorm,maxtime)                        
      integer   ifh,maxstorm
      integer   sortindex(maxstorm)
c
      allocate (prstemp(maxstorm),stat=iva)
      allocate (iwork(maxstorm),stat=iwa)
      if (iva /= 0 .or. iwa /= 0) then
        print *,' '
        print *,'!!! ERROR in sub sort_storms_by_pressure allocating' 
        print *,'!!! prstemp or iwork arrays: '
        print *,'!!! iva= ',iva,' iwa= ',iwa
        STOP 94
        return
      endif

      if (ifh > 1) then

        print *,' '
        print *,'--- Before  sort, original prs values follow:'
        print *,' '

        do ist = 1,maxstorm
          prstemp(ist) = gridprs(ist,ifh-1)
          write (6,81) prstemp(ist)/100.0
        enddo

        imode = 2
        sortindex = 0
c-orig  call orders (imode,iwork,prstemp,sortindex,maxstorm,1,8,1)

        print *,' '
        print *,'+++ Pressure-sorted storm list:'
        print *,' '

        do ist = 1,maxstorm
          write (6,82) sortindex(ist),prstemp(sortindex(ist))/100.0
        enddo

  81    format (' Original (unsorted) prstemp= ',f7.2)
  82    format (' sortindex(ist)= ',i3,' prstemp= ',f7.2)

      else
        do ist = 1,maxstorm
          sortindex(ist) = ist
        enddo
      endif  

      deallocate (prstemp); deallocate (iwork)
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine getvrvt (centlon,centlat,xlon,xlat
     &                   ,udat,vdat,vr,vt,igvtret)
c
c     ABSTRACT: This subroutine takes as input a u-wind and v-wind value
c     at an input (xlon,xlat) location and returns the tangential and
c     radial wind components relative to the input center lat/lon 
c     position (centlon,centlat).  The only trick to this whole 
c     subroutine is figuring out the angle from the center point to the
c     data point, and we do this by creating a triangle with the leg 
c     from the center point to the data point being the hypotenuse.
c
c     NOTE: All longitudes must be in positive degrees east (0-360) !!!
c
c     INPUT:
c     centlon  Longitude of center point
c     centlat  Latitude  of center point
c     xlon     Longitude of pt at which vr & vt will be computed
c     xlat     Latitude  of pt at which vr & vt will be computed
c     udat     u-value of wind at the point (xlon,xlat) 
c     vdat     v-value of wind at the point (xlon,xlat) 
c
c     OUTPUT:
c     vr      Radial wind component at (xlon,xlat) wrt (centlon,centlat)
c     vt      Tang   wind component at (xlon,xlat) wrt (centlon,centlat)
c     igvtret Return code from this subroutine

      USE trig_vals

      real centlon,centlat,xlon,xlat,udat,vdat,vr,vt
c
      call calcdist(centlon,centlat,xlon,xlat,hyp_dist)

      xlatdiff = abs(centlat - xlat)
      xlondiff = abs(centlon - xlon)

      if (xlondiff == 0 .and. xlatdiff > 0) then

        if (centlat > xlat) angle = 180   ! pt directly south of ctr
        if (centlat < xlat) angle = 0     ! pt directly north of ctr

      else if (xlondiff > 0 .and. xlatdiff == 0) then

        if (centlon > xlon) angle = 270   ! pt directly west of ctr
        if (centlon < xlon) angle = 90    ! pt directly east of ctr

      else

        ! This next part figures out the angle from the center point
        ! (centlon,centlat) to the data point (xlon,xlat).  It does 
        ! this by setting up a triangle and then using inverse trig
        ! functions to get the angle.  Since this is a kludgy way to
        ! do it that doesn't account for the curvature of the earth,
        ! we'll do it 2 ways, using asin and then acos, then take the
        ! average of those 2 for the angle.  hyp_dist, calculated just
        ! above, is the distance from the center pt to the data pt.

        opp_dist  = xlatdiff/360. * ecircum
        sin_value = opp_dist / hyp_dist
        if (sin_value > 1.0) then
          print *,' '
          print *,'!!! In is_it_a_storm, sin_value > 1, setting to 1.'
          print *,'!!! opp_dist= ',opp_dist,' hyp_dist= ',hyp_dist
          print *,'!!! sin_value = ',sin_value
          print *,'!!! centlon= ',centlon,' centlat= ',centlat
          print *,'!!! xlon=    ',xlon,' xlat=    ',xlat
          print *,' '
          sin_value = 0.99999
        endif
        sin_angle = asin(sin_value) / dtr

        call calcdist(centlon,centlat,xlon,centlat,adj_dist)
        cos_value = adj_dist / hyp_dist
        if (cos_value > 1.0) then
          print *,' '
          print *,'!!! In is_it_a_storm, cos_value > 1, setting to 1.'
          print *,'!!! adj_dist= ',adj_dist,' hyp_dist= ',hyp_dist
          print *,'!!! cos_value = ',cos_value
          print *,'!!! centlon= ',centlon,' centlat= ',centlat
          print *,'!!! xlon=    ',xlon,' xlat=    ',xlat
          print *,' '
          cos_value = 0.99999
        endif
        cos_angle = acos(cos_value) / dtr

        tmpangle = 0.5 * (sin_angle + cos_angle)

        ! The previous lines of code just calculated an angle between
        ! 0 and 90.  This next if structure adjusts that angle to 
        ! instead be between 0 and 360.

        if      (centlat <= xlat .and. centlon <= xlon) then
          angle = 90 - tmpangle
        else if (centlat >  xlat .and. centlon <= xlon) then
          angle = 90 + tmpangle
        else if (centlat >= xlat .and. centlon >= xlon) then
          angle = 270 - tmpangle
        else if (centlat <  xlat .and. centlon >= xlon) then
          angle = 270 + tmpangle
        endif

      endif

      uvrcomp = udat * sin(angle * dtr)
      vvrcomp = vdat * cos(angle * dtr)
      vr      = uvrcomp + vvrcomp

      uvtcomp = (-udat) * cos(angle * dtr)
      vvtcomp = vdat    * sin(angle * dtr)
      vt      = uvtcomp + vvtcomp

      return 
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine output_radii (maxstorm,vradius,inp,ifcsthr,ist,iorret)
c
c     ABSTRACT: This subroutine  outputs a 1-line message for a
c     storm at a given forecast hour.  This message contains the model 
c     identifier, the forecast initial time, the forecast hour, and
c     the radii of gale force (34kt|17.5m/s), storm force 
c     (50kt|25.7m/s), and hurricane force (64kt|32.9m/s) winds in each
c     quadrant of the storm.  Whereas the  output_all and output_atcf 
c     subroutines are called only once (after the last forecast hour
c     position is computed), this subroutine is called after each 
c     forecast hour.
c
c     INPUT:
c     maxstorm  Max # of storms that can be handled
c     vradius   Contains the distance from the storm fix position to
c               each of the various wind threshhold distances in each
c               quadrant. (3,4) ==> (# of threshholds, # of quadrants)
c     inp       Contains the input namelist.  Use this for the dates
c     ifcsthr   The current forecast hour being output 
c     ist       The number storm that we're processing (can be 1-15)
c
c     OUTPUT:
c     iorret    Return code from this subroutine
c    
c     OTHER:
c     storm     An array of type tcvcard.  Use this for the storm ID.
c               Defined in def_vitals module.
 
      USE def_vitals; USE inparms; USE set_max_parms; USE atcf
c
      type (datecard) inp
c
      integer    vradius(3,4)
      character  basinid*4
c
      basinid = '    '
      select case (storm(ist)%tcv_storm_id(3:3))
        case ('L','l');  basinid(1:2) = 'AL'
        case ('E','e');  basinid(1:2) = 'EP'
        case ('C','c');  basinid(1:2) = 'CP'
        case ('W','w');  basinid(1:2) = 'WP'
        case ('O','o');  basinid(1:2) = 'SC'
        case ('T','t');  basinid(1:2) = 'EC'
        case ('U','u');  basinid(1:2) = 'AU'
        case ('P','p');  basinid(1:2) = 'SP'
        case ('S','s');  basinid(1:2) = 'SI'
        case ('B','b');  basinid(1:2) = 'BB'
        case ('A','a');  basinid(1:2) = 'NA'
        case default;    basinid(1:2) = '**'
      end select
      basinid(3:4) = storm(ist)%tcv_storm_id(1:2)

      write (63,81) atcfnum,atcfname
     &,inp%byy,inp%bmm,inp%bdd,inp%bhh,ifcsthr
     &,vradius(1,1),vradius(1,2),vradius(1,3),vradius(1,4)
     &,vradius(2,1),vradius(2,2),vradius(2,3),vradius(2,4)
     &,vradius(3,1),vradius(3,2),vradius(3,3),vradius(3,4)
     &,basinid,inp%byy

      write (6,*) ' '
      write (6,*) 'Surface wind radii information follows: '
      write (6,81) atcfnum,atcfname
     &,inp%byy,inp%bmm,inp%bdd,inp%bhh,ifcsthr
     &,vradius(1,1),vradius(1,2),vradius(1,3),vradius(1,4)
     &,vradius(2,1),vradius(2,2),vradius(2,3),vradius(2,4)
     &,vradius(3,1),vradius(3,2),vradius(3,3),vradius(3,4)
     &,basinid,inp%byy

  81  format (i2,a4,4i2.2,i3.3,1x,3(4i3.3,1x),13x,a4,i2.2)

      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine output_all (fixlon,fixlat,inp,ifhours,maxstorm,ioaret)
c
c     ABSTRACT: This subroutine  outputs a 1-line message for each 
c     storm.  This message contains the model identifier, the forecast 
c     initial date, and the positions for 0, 12, 24, 36, 48, 60 and 72 
c     hours.  In the case of the regional models (NGM, Eta), which 
c     only go out to 48h, zeroes are included for forecast hours 
c     60 and 72.
c
c     NOTE: The longitudes that are passed into this subroutine are 
c     given in 0 - 360, increasing eastward.  The  output of this 
c     subroutine is used by Steve Lord for plotting purposes, and his
c     plotting routines need the longitudes in 0 - 360, increasing 
c     westward.  Thus, a necessary adjustment is made.
c
      USE def_vitals; USE inparms; USE set_max_parms; USE atcf
c
      type (datecard) inp
c
      real    fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      integer intlon(maxtime),intlat(maxtime),ifhours(maxtime)
      character  modelchar(maxmodel)*4

c     First convert all of the lat/lon values from reals into integers.
c     These integer values must be 10x their real value (eg. 125.4 will
c     be written out as 1254).  Convert the lon values so that they go
c     from 0 - 360, increasing westward.

      stormloop: do ist = 1,maxstorm

        if (stormswitch(ist) == 3) cycle stormloop
        intlon = 0; intlat = 0

        ifhloop: do ifh = 1,maxtime

          if (ifhours(ifh) == 99) then
            intlon(ifh) = 0
            intlat(ifh) = 0
            cycle ifhloop
          endif

          if (fixlon(ist,ifh) < -998.0 .or. fixlat(ist,ifh) < -998.0)
     &    then
            intlon(ifh) = 0
            intlat(ifh) = 0
          else
            intlon(ifh) = 3600 - int(fixlon(ist,ifh) * 10. + 0.5)
            intlat(ifh) = int(abs(fixlat(ist,ifh)) * 10. + 0.5)
            if (fixlat(ist,ifh) < 0.0) then
              intlat(ifh) = intlat(ifh) * (-1)
            endif
          endif
 
        enddo ifhloop

        select case (atcfname(1:3))

          case ('AVN','NGM','ETA','GFD','AP0','AN0','AP1','AN1','AC0',
     &          'NAS','FV4','FV5','FG5')
            write (61,81) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(1),intlon(1)
     &           ,intlat(3),intlon(3),intlat(5),intlon(5),intlat(7)
     &           ,intlon(7),intlat(9),intlon(9),intlat(11),intlon(11)
     &           ,intlat(13),intlon(13),storm(ist)%tcv_storm_id

          case ('MRF','UKX','NGX')        ! MRF, UKMET, NOGAPS
            write (61,81) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(1),intlon(1)
     &           ,intlat(2),intlon(2),intlat(3),intlon(3),intlat(4)
     &           ,intlon(4),intlat(5),intlon(5),intlat(6),intlon(6)
     &           ,intlat(7),intlon(7),storm(ist)%tcv_storm_id

          case ('EMX')       ! ECMWF
            write (61,81) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(1),intlon(1)
     &           ,0,0,intlat(2),intlon(2),0,0,intlat(3),intlon(3)
     &           ,0,0,intlat(4),intlon(4),storm(ist)%tcv_storm_id
            
          case ('GDA')       ! GDAS relocation
            write (61,81) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(1),intlon(1)
     &           ,intlat(2),intlon(2),intlat(3),intlon(3)
     &           ,intlat(4),intlon(4),0,0,0,0,0,0
     &           ,storm(ist)%tcv_storm_id

          case ('YP0','YP1','YN0','YN1')  ! AVN ensemble relocation
                                          ! for 6h breeding cycle....
            write (61,81) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(1),intlon(1)
     &           ,intlat(2),intlon(2),intlat(2),intlon(2)
     &           ,intlat(2),intlon(2),0,0,0,0,0,0
     &           ,storm(ist)%tcv_storm_id
     
          case ('ZP0','ZP1','ZN0','ZN1','AEA')  ! AVN ensemble
                               ! relocation for 24h breeding cycle, and
                               ! AVN ens control analysis
            write (61,81) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(1),intlon(1)
     &           ,intlat(1),intlon(1),intlat(1),intlon(1)
     &           ,intlat(1),intlon(1),0,0,0,0,0,0
     &           ,storm(ist)%tcv_storm_id
            
          case default
            print *,' '
            print *,'!!! ERROR in subroutine  output_all. '
            print *,'!!! Model name is not identified.'
            print *,'!!! Model name = ',atcfname
            print *,'!!! ist = ',ist,' Model number = ',atcfnum

        end select

      enddo stormloop

  81  format (i2,a4,4i2.2,14i4,1x,a3)
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine output_atcf (fixlon,fixlat,inp,ifhours,xmaxwind
     &                       ,maxstorm,ioaret)
c
c     ABSTRACT: This subroutine  outputs a 1-line message for each storm
c     in ATCF format.  This message contains the model identifier, the 
c     forecast initial date, and the positions for 0, 12, 24, 36, 48, 
c     60 and 72 hours.  This message also contains the intensity 
c     estimates (in knots) for those same hours.  The  conversion for
c     m/s to knots is to multiply m/s by 1.9427 (3.281 ft/m, 
c     1 naut mile/6080 ft, 3600s/h).
c
c     NOTE: The longitudes that are passed into this subroutine are
c     given in 0 - 360, increasing eastward.  The  output of this
c     subroutine is used by the atcf system at TPC for plotting 
c     purposes, and the atcf plotting routines need the longitudes in 
c     0 - 360, increasing westward.  Thus, a necessary adjustment is 
c     made.
c
      USE def_vitals; USE inparms; USE set_max_parms; USE atcf
c
      type (datecard) inp
c
      real    fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real    xmaxwind(maxstorm,maxtime)
      real    conv_ms_knots
      integer intlon(maxtime),intlat(maxtime),ifhours(maxtime)
      character  modelchar(maxmodel)*4,basinid*4

c     First convert all of the lat/lon values from reals into integers.
c     These integer values must be 10x their real value (eg. 125.4 will
c     be written out as 1254).  Convert the lon values so that they go
c     from 0 - 360, increasing westward.

      conv_ms_knots = 1.9427

      stormloop: do ist = 1,maxstorm

        if (stormswitch(ist) == 3) cycle stormloop
        intlon = 0; intlat = 0

        ifhloop: do ifh = 1,maxtime

          if (ifhours(ifh) == 99) then
            intlon(ifh) = 0
            intlat(ifh) = 0
            cycle ifhloop
          endif

          if (fixlon(ist,ifh) < -998.0 .or. fixlat(ist,ifh) < -998.0)
     &    then

            intlon(ifh) = 0
            intlat(ifh) = 0

          else
            intlon(ifh) = 3600 - int(fixlon(ist,ifh) * 10. + 0.5)
            intlat(ifh) = int(abs(fixlat(ist,ifh)) * 10. + 0.5)
            if (fixlat(ist,ifh) < 0.0) then
              intlat(ifh) = intlat(ifh) * (-1)
            endif

          endif

        enddo ifhloop

        basinid = '    '
        select case (storm(ist)%tcv_storm_id(3:3))
          case ('L','l');  basinid(1:2) = 'AL'
          case ('E','e');  basinid(1:2) = 'EP'
          case ('C','c');  basinid(1:2) = 'CP'
          case ('W','w');  basinid(1:2) = 'WP'
          case ('O','o');  basinid(1:2) = 'SC'
          case ('T','t');  basinid(1:2) = 'EC'
          case ('U','u');  basinid(1:2) = 'AU'
          case ('P','p');  basinid(1:2) = 'SP'
          case ('S','s');  basinid(1:2) = 'SI'
          case ('B','b');  basinid(1:2) = 'BB'
          case ('A','a');  basinid(1:2) = 'NA'
          case default;    basinid(1:2) = '**'
        end select
        basinid(3:4) = storm(ist)%tcv_storm_id(1:2)


        select case (atcfname(1:3))

          case ('AVN','NGM','ETA','GFD','AP0','AN0','AP1','AN1','AC0',
     &          'NAS','FV4','FV5','FG5')
            write (62,82) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(3),intlon(3)
     &           ,intlat(5),intlon(5),intlat(7),intlon(7),intlat(9)
     &           ,intlon(9),intlat(13),intlon(13)
     &           ,int((xmaxwind(ist,3)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,5)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,7)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,9)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,13)*conv_ms_knots) + 0.5)
     &           ,basinid,inp%byy

          case ('MRF','UKX','NGX')     ! MRF, UKMET, NOGAPS
            write (62,82) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(2),intlon(2)
     &           ,intlat(3),intlon(3),intlat(4),intlon(4),intlat(5)
     &           ,intlon(5),intlat(7),intlon(7)
     &           ,int((xmaxwind(ist,2)*conv_ms_knots) + 0.5) 
     &           ,int((xmaxwind(ist,3)*conv_ms_knots) + 0.5) 
     &           ,int((xmaxwind(ist,4)*conv_ms_knots) + 0.5) 
     &           ,int((xmaxwind(ist,5)*conv_ms_knots) + 0.5) 
     &           ,int((xmaxwind(ist,7)*conv_ms_knots) + 0.5) 
     &           ,basinid,inp%byy

          case ('EMX')        ! ECMWF
            write (62,82) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,0,0,intlat(2)
     &           ,intlon(2),0,0,intlat(3),intlon(3),intlat(4)
     &           ,intlon(4)
     &           ,0
     &           ,int((xmaxwind(ist,2)*conv_ms_knots) + 0.5)
     &           ,0
     &           ,int((xmaxwind(ist,3)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,4)*conv_ms_knots) + 0.5)
     &           ,basinid,inp%byy

          case ('GDA')        ! GDAS relocation
            write (62,82) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh
     &           ,intlon(1),intlat(1),intlat(2),intlon(2)
     &           ,intlat(3),intlon(3),intlat(4),intlon(4)
     &           ,0,0
     &           ,int((xmaxwind(ist,2)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,3)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,4)*conv_ms_knots) + 0.5)
     &           ,0,0,basinid,inp%byy

          case ('YP0','YP1','YN0','YN1')  ! AVN ensemble relocation
                                          ! for 6h breeding cycle....
            write (62,82) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh
     &           ,intlon(1),intlat(1),intlat(2),intlon(2)
     &           ,intlat(2),intlon(2),intlat(2),intlon(2)
     &           ,0,0
     &           ,int((xmaxwind(ist,2)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,3)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,4)*conv_ms_knots) + 0.5)
     &           ,0,0,basinid,inp%byy

          case ('ZP0','ZP1','ZN0','ZN1','AEA')  ! AVN ensemble
                              ! relocation for 24h breeding cycle,
                              ! and AVN ens control analysis
            write (62,82) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh
     &           ,intlon(1),intlat(1),intlat(1),intlon(1)
     &           ,intlat(1),intlon(1),intlat(1),intlon(1)
     &           ,0,0
     &           ,int((xmaxwind(ist,2)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,3)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,4)*conv_ms_knots) + 0.5)
     &           ,0,0,basinid,inp%byy

          case default
            print *,' '
            print *,'!!! ERROR in subroutine  output_atcf. '
            print *,'!!! Model name is not identified.'
            print *,'!!! Model name = ',atcfname
            print *,'!!! ist = ',ist,' Model number = ',atcfnum

        end select

      enddo stormloop

  82  format (i2,a4,4i2.2,10i4,5i3,1x,a4,i2.2)
c
      return
      end

c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine output_atcfunix (outlon,outlat,inp,ist
     &         ,ifcsthour,vmaxwind,xminmslp,vradius,maxstorm
     &         ,trkrinfo,ioaxret)

c     ABSTRACT: This subroutine  outputs a 1-line message for a given 
c     storm at an input forecast hour in the new ATCF UNIX format.  
c     Unlike the old atcf DOS format in which you waited until the 
c     whole tracking was over to write the  output for all forecast 
c     hours, with this atcfunix format, each time we are calling this
c     subroutine, it is to only write out 1 record, which will be the
c     fix info for a particular storm at a given time.  Also, even 
c     though we have some data (AVN, Eta) at 6-hour intervals, Jim 
c     Gross informed me that TPC does not need the positions at such
c     frequency, and keeping the reporting at 12 hour intervals is fine.
c
c     While this new atcfunix format contains much more information than
c     the old 1-line atcf dos message, for our purposes we will use the
c     slots for mslp and wind radii.  An example set of output records
c     will look like the following:
c
c     AL, 13, 2000092500, 03, AVNO, 036, 243N, 675W, 42, 995, XX,  34,
c             NEQ,  242,  163,  124,  208
c     AL, 13, 2000092500, 03, AVNO, 036, 243N, 675W, 42, 995, XX,  50,
c             NEQ,  155,  000,  000,  000
c     AL, 13, 2000092500, 03, AVNO, 036, 243N, 675W, 42, 995, XX,  64,
c             NEQ,  000,  000,  000,  000
c
c     (NOTE: Each of the above lines beginning with "AL" is output as 
c            a single line of text.)
c
c     Note that in this example, for this 36h forecast hour, there are 
c     3 entries.  This is so that we can include the radii for the 
c     3 different wind thresholds (34kt, 50kt and 64kt).  So the only
c     thing different in each entry is the wind radii info;  all the
c     other info is identical for each entry.
c
c     This message also contains the intensity estimates (in knots) 
c     for every forecast hours  The  conversion for m/s to knots is 
c     to multiply m/s by 1.9427 (3.281 ft/m, 1 naut mile/6080 ft, 
c     3600s/h).
c
c     NOTE: The longitudes that are passed into this subroutine are
c     given in 0 - 360, increasing eastward.  The format for the 
c     atcfunix system requires that the  output be 0-180E or
c     0-180W, so we must adjust the values, if needed.  Also, the
c     values for southern latitudes must be positive (use 'N' and 
c     'S' to distinguish Northern/Southern Hemispheres).
c
c     INPUT:
c     outlon    longitude  fix position for this storm at this time 
c               which is to be written out to the  output file
c     outlat    latitude  fix position for this storm at this time 
c               which is to be written out to the  output file
c     inp       contains input date and model number information
c     ist       the number storm that we're processing (can be 1-15)
c     ifcsthr   the current forecast hour being output
c     vmaxwind  the max surface wind for this storm at this fcst hour
c     xminmslp  the min mslp for this storm at this fcst hour
c     vradius   Contains the distance from the storm fix position to
c               each of the various wind threshhold distances in each
c               quadrant. (3,4) ==> (# of threshholds, # of quadrants)
c     maxstorm  max # of storms that can be handled
c 
c     OUTPUT:
c     ioaxret   integer return code from this subroutine
c     
c     LOCAL:
c     intlon    integer that holds the value of outlon*10
c     intlat    integer that holds the value of outlat*10
c     storm     An array of type tcvcard.  Use this for the storm ID
c

      USE def_vitals; USE inparms; USE set_max_parms; USE atcf
      USE trkrparms
c
      type (datecard) inp
      type (trackstuff) trkrinfo
c
      real    outlon,outlat
      real    vmaxwind,conv_ms_knots,xminmslp
      integer intlon,intlat
      integer vradius(3,4)
      character  basinid*2,clatns*1,clonew*1

      print *,'TTT top of atcfunix, ist= ',ist,' ifh= ',ifcsthour

      if (xminmslp == 999999.0) xminmslp = 0.0

c     First convert all of the lat/lon values from reals into integers.
c     These integer values must be 10x their real value (eg. 125.4 will
c     be written out as 1254).  Convert the lon values so that they go
c     from 0-180E or 0-180W, and convert the lat values so that they are
c     positive and use 'N' or 'S' to differentiate hemispheres.

      conv_ms_knots = 1.9427

      if (outlon < -998.0 .or. outlat < -998.0) then
        intlon = 0
        intlat = 0
        clonew = ' '
        clatns = ' '
      else
        if (outlon >= 180.0) then
          intlon = 3600 - int(outlon * 10. + 0.5)
          clonew = 'W'
        else
          intlon = int(outlon * 10. + 0.5)
          clonew = 'E'
        endif
        intlat = int(abs(outlat) * 10. + 0.5)
        if (outlat < 0.0) then
          clatns = 'S'
        else
          clatns = 'N'
        endif
      endif

      select case (storm(ist)%tcv_storm_id(3:3))
        case ('L','l');  basinid = 'AL'
        case ('E','e');  basinid = 'EP'
        case ('C','c');  basinid = 'CP'
        case ('W','w');  basinid = 'WP'
        case ('O','o');  basinid = 'SC'
        case ('T','t');  basinid = 'EC'
        case ('U','u');  basinid = 'AU'
        case ('P','p');  basinid = 'SP'
        case ('S','s');  basinid = 'SI'
        case ('B','b');  basinid = 'BB'
        case ('A','a');  basinid = 'NA'
        case default;    basinid = '**'
      end select

      if (trkrinfo%type == 'midlat' .or. trkrinfo%type == 'tcgen') then

        if (stcvtype(ist) == 'FOF') then
          ! If this is a TC vitals-described storm (i.e., one that is
          ! numbered by JTWC or NHC), then leave the basinid as is.
          ! Otherwise, we want to use the "basinid" location as a 
          ! label to identify what type of run this is.
          if (trkrinfo%type == 'midlat') basinid = 'ML'
          if (trkrinfo%type == 'tcgen')  basinid = 'TG'
        endif

        write (64,91) basinid,storm(ist)%tcv_storm_id
     &        ,inp%bcc,inp%byy,inp%bmm,inp%bdd,inp%bhh
     &        ,adjustr(atcfname),ifcsthour,intlat,clatns,intlon,clonew
     &        ,int((vmaxwind*conv_ms_knots) + 0.5)
     &        ,int(xminmslp/100.0 + 0.5)
     &        ,'XX,  34, NEQ'
     &        ,vradius(1,1),vradius(1,2),vradius(1,3),vradius(1,4)
     &        ,0,0,0,0,0,stcvtype(ist)

        if (vradius(2,1) > 0 .or. vradius(2,2) > 0 .or.
     &      vradius(2,3) > 0 .or. vradius(2,4) > 0) then
          write (64,91) basinid,storm(ist)%tcv_storm_id
     &          ,inp%bcc,inp%byy,inp%bmm,inp%bdd,inp%bhh
     &          ,adjustr(atcfname),ifcsthour,intlat,clatns,intlon,clonew
     &          ,int((vmaxwind*conv_ms_knots) + 0.5)
     &          ,int(xminmslp/100.0 + 0.5)
     &          ,'XX,  50, NEQ'
     &          ,vradius(2,1),vradius(2,2),vradius(2,3),vradius(2,4)
     &          ,0,0,0,0,0,stcvtype(ist)
        endif

        if (vradius(3,1) > 0 .or. vradius(3,2) > 0 .or.
     &      vradius(3,3) > 0 .or. vradius(3,4) > 0) then
          write (64,91) basinid,storm(ist)%tcv_storm_id
     &          ,inp%bcc,inp%byy,inp%bmm,inp%bdd,inp%bhh
     &          ,adjustr(atcfname),ifcsthour,intlat,clatns,intlon,clonew
     &          ,int((vmaxwind*conv_ms_knots) + 0.5)
     &          ,int(xminmslp/100.0 + 0.5)
     &          ,'XX,  64, NEQ'
     &          ,vradius(3,1),vradius(3,2),vradius(3,3),vradius(3,4)
     &          ,0,0,0,0,0,stcvtype(ist)
        endif

      else

        write (64,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &        ,inp%bcc,inp%byy,inp%bmm,inp%bdd,inp%bhh
     &        ,adjustr(atcfname),ifcsthour,intlat,clatns,intlon,clonew
     &        ,int((vmaxwind*conv_ms_knots) + 0.5)
     &        ,int(xminmslp/100.0 + 0.5)
     &        ,'XX,  34, NEQ'
     &        ,vradius(1,1),vradius(1,2),vradius(1,3),vradius(1,4)

        if (vradius(2,1) > 0 .or. vradius(2,2) > 0 .or.
     &      vradius(2,3) > 0 .or. vradius(2,4) > 0) then
          write (64,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &          ,inp%bcc,inp%byy,inp%bmm,inp%bdd,inp%bhh
     &          ,adjustr(atcfname),ifcsthour,intlat,clatns,intlon,clonew
     &          ,int((vmaxwind*conv_ms_knots) + 0.5)                   
     &          ,int(xminmslp/100.0 + 0.5)         
     &          ,'XX,  50, NEQ'          
     &          ,vradius(2,1),vradius(2,2),vradius(2,3),vradius(2,4)
        endif

        if (vradius(3,1) > 0 .or. vradius(3,2) > 0 .or.
     &      vradius(3,3) > 0 .or. vradius(3,4) > 0) then
          write (64,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &          ,inp%bcc,inp%byy,inp%bmm,inp%bdd,inp%bhh
     &          ,adjustr(atcfname),ifcsthour,intlat,clatns,intlon,clonew
     &          ,int((vmaxwind*conv_ms_knots) + 0.5)
     &          ,int(xminmslp/100.0 + 0.5)
     &          ,'XX,  64, NEQ'
     &          ,vradius(3,1),vradius(3,2),vradius(3,3),vradius(3,4)
        endif

      endif

   81 format (a2,', ',a2,', ',5i2.2,', 03, ',a4,', ',i3.3,', ',i3,a1
     &       ,', ',i4,a1,', ',i3,', ',i4,', ',a12,4(', ',i4.4))
   91 format (a2,',',a3,', ',5i2.2,', 03, ',a4,', ',i3.3,', ',i3,a1
     &       ,', ',i4,a1,', ',i3,', ',i4,', ',a12,4(', ',i4.4)
     &       ,2(', ',i4),3(', ',i3),', ',a3)

      return
      end

c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine output_tcvitals (xlon,xlat,inp,ist,iovret)
c
c     ABSTRACT: This subroutine  outputs a tcvitals record.  The
c     lat/lon location is given by the xlon and xlat that are
c     input to this subroutine.
c
c     INPUT:
c     xlon   longitude of storm position to be  output
c     xlat   latitude of storm position to be  output
c     inp    contains input date and model number information
c     ist    the number storm that we're processing (can be 1-15)
c
c     OUTPUT:
c     iovret return code from this subroutine
c
c     OTHER:
c     storm  contains the tcvitals info (from module def_vitals)
c
      USE def_vitals; USE inparms; USE set_max_parms
c
      type (tcvcard) stm
      type (datecard) inp
      real       xlon,xlat
c
      iovret = 0

c     Initially, set all "stm" components equal to the input "storm"
c     components for this storm, then we will change the specific
c     components that we need to.

      stm = storm(ist)

      stm%tcv_center = 'AEAR'

      stm%tcv_lat = int(abs(xlat) * 10. + 0.5)
      if (xlat < 0.0) then
        stm%tcv_latns = 'S'
      else
        stm%tcv_latns = 'N'
      endif

      if (xlon >= 180.) then
        stm%tcv_lon = 3600 - int(xlon * 10. + 0.5)
        stm%tcv_lonew = 'W'
      else
        stm%tcv_lon = int(xlon * 10. + 0.5)
        stm%tcv_lonew = 'E'
      endif
      
      write (6,*) ' '
      write (6,21) stm
      write (65,21) stm
      
   21 format (a4,1x,a3,1x,a9,1x,i2.2,i6.6,1x,i4.4,1x,i3,a1,1x,i4,a1,1x
     &       ,i3,1x,i3,3(1x,i4),1x,i2,1x,i3,1x,4(i4,1x),a1)
      
c     
      return
      end

c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine output_tracker_mask (masked_outc,kpds,kgds,lb,ifh
     &                ,imax,jmax,iotmret)
c
c     ABSTRACT: This subroutine outputs a GRIB record that contains the
c     "mask" used to mask out areas surrounding low pressure centers 
c     that have been found during the search at each forecast hour. This
c     mask is written out purely for diagnostic purposes.  The GRIB 
c     identifier given to the mask in the pds is 850 mb height (you can 
c     make it anything you want).  This is only done for the "midlat"
c     and "tcgen" cases, since the runs for those cases use a mask while
c     the regular "tracker" run (that is, the run which strictly tracks
c     only those storms in the TC vitals file) does not.
c
c     INPUT:
c     masked_outc logical array containing mask
c     kpds     GRIB pds array
c     kgds     GRIB gds array
c     ifh      integer counter index for current forecast hour
c     imax     num points is i-direction of input grid
c     jmax     num points is j-direction of input grid
c
c     OUTPUT:
c     iotmret  return code from this subroutine
c
      implicit none
c
      integer   ifh,imax,jmax,iotmret,kf,igoret,iix,jjx,ipret
      integer   kpds(200),kgds(200)
      logical(1) masked_outc(imax,jmax),lb(imax,jmax)
      real      xmask(imax,jmax)
c
      if (ifh == 1) then
        call baopenw (72,"fort.72",igoret)
        print *,'baopenw: igoret= ',igoret

        if (igoret /= 0) then
          print *,' '
          print *,'!!! ERROR in sub output_tracker_mask opening'
          print *,'!!! **OUTPUT** grib files.  baopenw return codes:'
          print *,'!!! grib file 1 return code = igoret = ',igoret
          STOP 95
          return    
        endif   
      endif  

      xmask = 0.0
      do jjx = 1,jmax
        do iix = 1,imax
          if (masked_outc(iix,jjx)) then
            xmask(iix,jjx) = 1.0
          else
            xmask(iix,jjx) = 0.0
          endif
        enddo  
      enddo  

      kf = imax * jmax

c      kpds(1)  =       7  ;    kpds(2)  =      80
c      kpds(3)  =       2  ;    kpds(4)  =     192

      kpds(5)  =       7  
      kpds(6)  =     100
      kpds(7)  =     850
      kpds(22) =       0

c      kpds(7)  =     850  ;    kpds(8)  =      02
c      kpds(9)  =      11  ;    kpds(10) =       4
c      kpds(11) =       0  ;    kpds(12) =       0
c      kpds(13) =       1  ;    kpds(14) =  ifcsthour
c      kpds(15) =       0  ;    kpds(16) =      10
c      kpds(17) =       0  ;    kpds(18) =       1
c      kpds(19) =       2  ;    kpds(20) =       0
c      kpds(21) =      21  ;    kpds(22) =       0
c      kpds(23) =       0  ;    kpds(24) =       0
c      kpds(25) =       0
c      kgds(1)  =       0  ;    kgds(2)  =    imax
c      kgds(3)  =    jmax  ;    kgds(4)  =   90000
c      kgds(5)  =       0  ;    kgds(6)  =     128
c      kgds(7)  =  -90000  ;    kgds(8)  =   -1000
c      kgds(9)  =    1000  ;    kgds(10) =    1000
c      kgds(11) =       0  ;    kgds(12) =       0
c      kgds(13) =       0  ;    kgds(14) =       0
c      kgds(15) =       0  ;    kgds(16) =       0
c      kgds(17) =       0  ;    kgds(18) =       0
c      kgds(19) =       0  ;    kgds(20) =     255

      write(*,980) kpds(1),kpds(2)
      write(*,981) kpds(3),kpds(4)
      write(*,982) kpds(5),kpds(6)
      write(*,983) kpds(7),kpds(8)
      write(*,984) kpds(9),kpds(10)
      write(*,985) kpds(11),kpds(12)
      write(*,986) kpds(13),kpds(14)
      write(*,987) kpds(15),kpds(16)
      write(*,988) kpds(17),kpds(18)
      write(*,989) kpds(19),kpds(20)
      write(*,990) kpds(21),kpds(22)
      write(*,991) kpds(23),kpds(24)
      write(*,992) kpds(25)
      write(*,880) kgds(1),kgds(2)
      write(*,881) kgds(3),kgds(4)
      write(*,882) kgds(5),kgds(6)
      write(*,883) kgds(7),kgds(8)
      write(*,884) kgds(9),kgds(10)
      write(*,885) kgds(11),kgds(12)
      write(*,886) kgds(13),kgds(14)
      write(*,887) kgds(15),kgds(16)
      write(*,888) kgds(17),kgds(18)
      write(*,889) kgds(19),kgds(20)
      write(*,890) kgds(21),kgds(22)
c
  980 format('    kpds(1)  = ',i7,'  kpds(2)  = ',i7)
  981 format('    kpds(3)  = ',i7,'  kpds(4)  = ',i7)
  982 format('    kpds(5)  = ',i7,'  kpds(6)  = ',i7)
  983 format('    kpds(7)  = ',i7,'  kpds(8)  = ',i7)
  984 format('    kpds(9)  = ',i7,'  kpds(10) = ',i7)
  985 format('    kpds(11) = ',i7,'  kpds(12) = ',i7)
  986 format('    kpds(13) = ',i7,'  kpds(14) = ',i7)
  987 format('    kpds(15) = ',i7,'  kpds(16) = ',i7)
  988 format('    kpds(17) = ',i7,'  kpds(18) = ',i7)
  989 format('    kpds(19) = ',i7,'  kpds(20) = ',i7)
  990 format('    kpds(21) = ',i7,'  kpds(22) = ',i7)
  991 format('    kpds(23) = ',i7,'  kpds(24) = ',i7)
  992 format('    kpds(25) = ',i7)
  880 format('    kgds(1)  = ',i7,'  kgds(2)  = ',i7)
  881 format('    kgds(3)  = ',i7,'  kgds(4)  = ',i7)
  882 format('    kgds(5)  = ',i7,'  kgds(6)  = ',i7)
  883 format('    kgds(7)  = ',i7,'  kgds(8)  = ',i7)
  884 format('    kgds(9)  = ',i7,'  kgds(10) = ',i7)
  885 format('    kgds(11) = ',i7,'  kgds(12) = ',i7)
  886 format('    kgds(13) = ',i7,'  kgds(14) = ',i7)
  887 format('    kgds(15) = ',i7,'  kgds(16) = ',i7)
  888 format('    kgds(17) = ',i7,'  kgds(18) = ',i7)
  889 format('    kgds(19) = ',i7,'  kgds(20) = ',i7)
  890 format('    kgds(20) = ',i7,'  kgds(22) = ',i7)

      print *,'just before call to putgb, kf= ',kf
      call putgb (72,kf,kpds,kgds,lb,xmask,ipret)
      print *,'just after call to putgb, kf= ',kf
      if (ipret == 0) then
        print *,' '
        print *,'+++ IPRET = 0 after call to putgb'
        print *,' '
      else
        print *,' '
        print *,'!!!!!! ERROR: IPRET NE 0 AFTER CALL TO PUTGB !!!'
        print *,' '
      endif
c
      return
      end
      
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_next_ges (fixlon,fixlat,ist,ifh,ifhours,imax,jmax
     &          ,grdspc,modelid,valid_pt,readflag,maxstorm,ignret)
c
c     ABSTRACT: This subroutine calculates a guess position for the next
c               forecast time.  It does this by using two different 
c               methods and averaging the results from those two.  The
c               first method is a simple linear extrapolation made by
c               basically drawing a line from the previous position 
c               through the current fix position and assuming straight
c               line motion.  The second method is to do a barnes 
c               smoothing of u & v in the vicinity of the storm at 850, 
c               700 & 500 mb to get an average environmental wind 
c               vector at each level, and then move the storm according 
c               to the vector at each level.  Then a weighted average is
c               taken of all these positions from methods 1 & 2 to get 
c               the consensus for the guess position.  NOTE: For a 
c               regional model and a storm that is relatively close to
c               the model boundary, there is a strong possibility that
c               the  barnes analysis subroutine will fail due to trying
c               to access grid points beyond the model's lateral  
c               boundary.  In this case, the redlm & ridlm are halved
c               and barnes is called again.  If it still fails, then 
c               just use the result from method 1 as a default.
c
c     INPUT:
c     fixlon  Array with longitudes of fix positions
c     fixlat  Array with latitudes of fix positions
c     ist     Storm number currently being processed
c     ifh     Forecast hour currently being processed
c     ifhours Array containing the integer forecast hours for this model
c     imax    Max number of pts in x-direction for this grid
c     jmax    Max number of pts in y-direction for this grid
c     grdspc  grid-spacing of the model being processed
c     modelid Integer indicating what model's data is being processed
c     valid_pt Logical; bitmap indicating if valid data at that pt.
c     readflag Logical; Tells whether or not a variable was read in
c              for this model
c     maxstorm Max # of storms that can be handled in this run
c
c     LOCAL:
c     dt      Number of seconds between successive forecast times
c             for this particular model.
c     dtkm    Distance in meters of 1 degree latitude
c     icutmax Max number of times to cut the ridlm and redlm in half,
c             for use in calling barnes.  If you're using a regional
c             model and on the first call to barnes you try to access
c             a point that's outside the model grid boundary, we'll
c             call  barnes again, but not before cutting the redlm and
c             ridlm in half.  icutmax says how many times to allow 
c             this cutting in half before giving up and just going
c             with the extrapolation method.  At first writing, we'll
c             set icutmax to 2, so that it will allow the ridlm to 
c             get down to 500 km (originally 2000 km) and the redlm 
c             to 125 km (originally 500 km).
c         *** NOTE: After testing the system, it was found that if
c             we cut these radii, the u and v values that are 
c             calculated from barnes are too strongly influenced by
c             the near-storm environment and, especially for asymmetric
c             systems, resulted in u and v values being much too strong.
c             As such, we will not allow these values to be cut, and if
c             we hit the boundaries in barnes, we'll just use the 
c             extrapolation method, which has seemd to work just fine.
c
c     OTHER:  (slonfg, slatfg & storm defined in module def_vitals)
c     slonfg  Array containing first guess longitude positions
c     slatfg  Array containing first guess latitude positions
c     storm   Contains tcvitals information
c
      USE radii; USE def_vitals; USE set_max_parms; USE grid_bounds
      USE tracked_parms; USE level_parms; USE trig_vals; USE trkrparms
c
      type (trackstuff) trkrinfo
      integer   ifhours(maxtime)
      integer   icutmax
      real      fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      character*1 :: in_grid, extrap_flag, barnes_flag
      logical(1) valid_pt(imax,jmax),readflag(11)
c
      in_grid = 'n'
      extrap_flag = 'y'
c
c     For updating the first guess, if Method 1 and Method 2 are both 
c     able to be done, give the following weights to the 2 methods.
c      
      data barneswt /0.50/, extrapwt /0.50/
c
c     -------------------------------
c     METHOD 1: LINEAR EXTRAPOLATION
c     -------------------------------
c     First, just do a simple linear extrapolation from the previous
c     fix position through the current fix position.  If it's the 
c     first time (vt=0), then use the storm motion vector and storm 
c     speed information from the TC Vitals card.
c
      dtkm = dtk * 1000.
      ifcsthr = (ifh-1) * interval_fhr
      dt   = (ifhours(2) - ifhours(1)) * 3600.0
c
      if (ifh == 1) then
        if (storm(ist)%tcv_stdir == -99 .or.
     &      storm(ist)%tcv_stspd == -99) then
          print *,' '
          print *,'!!! IN GET_NEXT_GES, at fcst hour = 0, either '
          print *,'!!! storm motion or storm speed = -99 on TCV card.'
          print *,'!!! ist= ',ist,' ifh= ',ifh
          print *,'!!! Storm name = ',storm(ist)%tcv_storm_name
          print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
          print *,'!!! storm motion vector= ',storm(ist)%tcv_stdir
          print *,'!!! storm motion speed= ',storm(ist)%tcv_stspd
          print *,'!!! CANNOT USE LINEAR EXTRAP TO GET NEXT GUESS !!!'
          extrap_flag = 'n'
        else
          ucomp = sin(float(storm(ist)%tcv_stdir) * dtr) *
     &                float(storm(ist)%tcv_stspd)/10.0
          vcomp = cos(float(storm(ist)%tcv_stdir) * dtr) *
     &                float(storm(ist)%tcv_stspd)/10.0
          xdist = ucomp * dt
          ydist = vcomp * dt
          ydeg = ydist / dtkm
          extraplat = fixlat(ist,ifh) + ydeg
          avglat = 0.5 * (extraplat + fixlat(ist,ifh))
          if (avglat > 89.5)  avglat =  89.0
          if (avglat < -89.5) avglat = -89.0
          cosfac = cos(avglat * dtr)
          xdeg = xdist / (dtkm*cosfac)
          extraplon = fixlon(ist,ifh) + xdeg
        endif
      else

c       Do a simple linear extrapolation of the current motion of the
c       storm.  Follow a line from the  fix position from the last fix
c       through the current fix and extrapolate out.  To figure out the
c       new latitude, just see how many deg lat the storm moved since
c       last time and add it to the current fix latitude.  To calculate
c       the new fix longitude, though, we need to see how many deg lon
c       the storm moved since the last time, convert that to the 
c       distance (km) the storm travelled in the x-direction (at an
c       average latitude between the current and previous latitudes),
c       and then add that distance on to the current longitude and 
c       convert that distance to the num of degrees the storm has 
c       travelled in the x-direction (at an average latitude between
c       the current and next(extrap) latitudes).


        if (fixlat(ist,ifh-1) > -900.0 .and.
     &      fixlon(ist,ifh-1) > -900.0) then

          ylatdegmove = fixlat(ist,ifh) - fixlat(ist,ifh-1)
          xlondegmove = fixlon(ist,ifh) - fixlon(ist,ifh-1)

          extraplat = fixlat(ist,ifh) + ylatdegmove

          yoldavglat = 0.5 * (fixlat(ist,ifh) + fixlat(ist,ifh-1))
          yoldcosfac = cos (dtr * yoldavglat)
          xdistmove  = xlondegmove * dtk * yoldcosfac

          ynewavglat = 0.5 * (extraplat + fixlat(ist,ifh))
          ynewcosfac = cos(dtr * ynewavglat)
          xdegnew    = xdistmove / (dtk * ynewcosfac)
          extraplon  = fixlon(ist,ifh) + xdegnew

        else 

          print *,' '
          print *,'!!! IN GET_NEXT_GES, at fcst hour = ',ifcsthr
          print *,'!!! the lon and lat positions for the previous'
          print *,'!!! forecast hour are -999, meaning that this is a'
          print *,'!!! new storm, so we cannot use the extrap method.'
          print *,'!!! Storm name = ',storm(ist)%tcv_storm_name
          print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
          print *,'!!! CANNOT USE LINEAR EXTRAP TO GET NEXT GUESS !!!'
          extrap_flag = 'n'

        endif

      endif

c     -------------------------------
c     METHOD 2: Barnes analysis
c     -------------------------------
c     Do a barnes analysis on the u & v components of the wind near the
c     storm to get an average u & v, then advect the storm according to
c     the average wind vector obtained.  The call to get_ij_bounds is 
c     needed in order to restrict the number of grid points that are 
c     searched in the  barnes subroutine.  See Abstract from this 
c     subroutine for further details.
 
      npts = ceiling(ridlm/(dtk*grdspc))
 
      call get_ij_bounds (npts,0,ridlm,imax,jmax,grdspc
     & ,glatmax,glatmin,glonmax,glonmin,fixlon(ist,ifh),fixlat(ist,ifh)
     & ,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)

      if (igiret /= 0) then
        print *,' '
        print *,'!!! ERROR in get_next_ges from call to get_ij_bounds,'
        print *,'!!! stopping processing for storm number ',ist
        ignret = 92
        return
      endif

c     Calculate average wind at each level (currently: 850, 700 & 500)

      re = redlm
      ri = ridlm
      icut = 0

      if (trkrinfo%type == 'midlat') then
        icutmax = 2
      else
        icutmax = 1
      endif

      radmaxloop:  do while (icut <= icutmax .and. in_grid == 'n')

        ubar  = 0.0; vbar  = 0.0
        iuret = 0; ivret = 0
        wgttot = 0.0
        ibarnct = 0
        barnes_flag = 'n'

        levelloop: do n=1,nlevg

          select case (n)
           case (1); ix1=3; ix2=4    ! For 850 mb readflags
           case (2); ix1=5; ix2=6    ! For 700 mb readflags
           case (3); ix1=10; ix2=11  ! For 500 mb readflags
          end select

          if (readflag(ix1) .and. readflag(ix2)) then

            call barnes (fixlon(ist,ifh),fixlat(ist,ifh),glon,glat
     &           ,imax,jmax,ibeg,jbeg,iend,jend,u(1,1,n),valid_pt
     &           ,re,ri,uavg,icount,'test',iuret)

            call barnes (fixlon(ist,ifh),fixlat(ist,ifh),glon,glat
     &           ,imax,jmax,ibeg,jbeg,iend,jend,v(1,1,n),valid_pt
     &           ,re,ri,vavg,icount,'test',ivret)
      
            if (iuret /= 0 .or. ivret /= 0) then
 
c             ...barnes probably tried to access a pt outside the grid
c             domain.  So, reduce by half the distance from the center
c             of the farthest pt that barnes tries to access, exit this
c             loop, and try it again with the smaller re and ri.
 
              iuret = 96; ivret = 96
              reold = re
              riold = ri
              re = 0.5 * re
              ri = 0.5 * ri
              print *,' ' 
              print *,'NOTE: While attempting to use the  barnes method'
              print *,'to update the first guess, the algorithm tried ' 
              print *,'to access a grid point that does not have valid'
              print *,'data, meaning that too large a radius is being'
              print *,'searched.  So, the 2 radii, re and ri, are being'
              print *,'halved and, if the value of icutmax > 0,  the '
              print *,'algorithm will be run again.  Otherwise, if '
              print *,'icutmax = 0, only the extrapolation method will'
              print *,'be used.'
              print *,'iuret= ',iuret,' ivret= ',ivret,' icut= ',icut
              print *,'Old re = ',reold,' New re = ',re
              print *,'Old ri = ',riold,' New ri = ',ri

              exit levelloop

            else
              ubar = ubar + wgts(n) * uavg
              vbar = vbar + wgts(n) * vavg
              wgttot = wgttot + wgts(n)
              ibarnct = ibarnct + 1
            endif

          endif
              
        enddo levelloop

        if (ibarnct > 0 .and. wgttot > 0.0) then
          barnes_flag = 'y'
          in_grid = 'y'    
          ubar = ubar / wgttot
          vbar = vbar / wgttot
          barnlat = fixlat(ist,ifh) + (vbar * dt)/dtkm
          cosfac = cos (dtr * 0.5 * (fixlat(ist,ifh) + barnlat))
          barnlon = fixlon(ist,ifh) + (ubar * dt)/(dtkm * cosfac)

c         This next if statement says that if we've had to reduce the
c         size of the  barnes analysis domain twice already, then we've
c         only done the analysis on a much smaller area, and this 
c         doesn't give us as good a picture of the average winds in the
c         area of the storm, so reduce the emphasis we place on the 
c         barnes method.

          if (icut >= 2) barneswt = barneswt / 2.

        else
          barnes_flag = 'n'
        endif

        icut = icut + 1

      enddo radmaxloop

c     ---------------------
c     Average the results
c     ---------------------
c     Now do a weighted average of the positions obtained from the 
c     linear extrapolation and the  barnes analysis methods.

      if (extrap_flag == 'y' .and. barnes_flag == 'y') then
        wt_total = barneswt + extrapwt
        slatfg(ist,ifh+1) = (barneswt * barnlat + extrapwt * extraplat)
     &                      / wt_total
        slonfg(ist,ifh+1) = (barneswt * barnlon + extrapwt * extraplon)
     &                      / wt_total
        write (6,*) ' '
        write (6,41) 360.-barnlon,barnlat        
        write (6,43) 360.-extraplon,extraplat        
        ignret = 0
      else if (extrap_flag == 'y' .and. barnes_flag == 'n') then
        print *,' '
        print *,'!!! NOTE: In get_next_ges, barnes method was not done'
        print *,'!!! for updating the first guess for this storm.'
        print *,'!!! Only the linear extrapolation method was used.'
        print *,'!!! ist= ',ist,' ifh= ',ifh
        print *,'!!! Storm Name = ',storm(ist)%tcv_storm_name
        print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
        slatfg(ist,ifh+1) = extraplat
        slonfg(ist,ifh+1) = extraplon
        write (6,*) ' '
        write (6,41) 0.0,0.0
        write (6,43) 360.-extraplon,extraplat
        ignret = 0
      else if (extrap_flag == 'n' .and. barnes_flag == 'y') then
        slatfg(ist,ifh+1) = barnlat
        slonfg(ist,ifh+1) = barnlon
        write (6,*) ' '
        write (6,41) 360.-barnlon,barnlat
        write (6,43) 0.0,0.0
        ignret = 0
      else
        print *,' '
        print *,'!!! ERROR in get_next_ges, new position guess not'
        print *,'!!! made.  Could not get guess using either barnes'
        print *,'!!! method or extrapolation method.'
        print *,'!!! extrap_flag = ',extrap_flag
        print *,'!!! barnes_flag = ',barnes_flag
        print *,'!!! Storm number = ',ist,' ifh = ',ifh
        print *,'!!! Storm Name = ',storm(ist)%tcv_storm_name
        print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
        write (6,41) 0.0,0.0
        write (6,43) 0.0,0.0
        ignret = 95 
      endif
c
      print *,' '
      print *,'-------------------------------------------------- '
      print *,'|      Current fix & updated fix positions       |'
      print *,'-------------------------------------------------- '
      print *,'| In get_next_ges, current fcst hour    = ',ifcsthr
      print *,'|                 current storm number  = ',ist
      print *,'| Return code from get_next_ges = ',ignret
      print *,'| Storm Name = ',storm(ist)%tcv_storm_name
      print *,'| Storm ID = ',storm(ist)%tcv_storm_id
      write (6,21) fixlat(ist,ifh)
      write (6,23) 360.-fixlon(ist,ifh),fixlon(ist,ifh)
      write (6,25) slatfg(ist,ifh+1)
      write (6,27) 360.-slonfg(ist,ifh+1),slonfg(ist,ifh+1)
      print *,'-------------------------------------------------'
      print *,' '

 21   format (' | Current fix lat is ',f7.2)
 23   format (' | Current fix lon is ',f7.2,'W   (',f7.2,'E)')
 25   format (' | Updated guess lat for next fcst hour is ',f7.2)
 27   format (' | Updated guess lon for next fcst hour is ',f7.2
     &       ,'W   (',f7.2,'E)')
 41   format (' --- barnlon=   ',f7.2,'W    barnlat=   ',f7.2)
 43   format (' --- extraplon= ',f7.2,'W    extraplat= ',f7.2)
c      
      return
      end       
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine getradii (xcenlon,xcenlat,imax,jmax,grdspc,valid_pt
     &                    ,cstormid,ifcsthr,vradius,igrret) 
c
c     ABSTRACT: This subroutine looks through the wind data near an
c     input storm center (fixlon,fixlat) and gets the radii of various
c     surface winds in each of the 4 storm quadrants (NE,NW,SE,SW).  
c     The wind thresholds that are sought are gale force (34kt|17.5m/s),
c     storm force (50kt|25.7m/s), and hurricane force (64kt|32.9m/s). 
c     This subroutine calls the Cray subroutine orders, which is a 
c     Cray-optimized sort routine.
c
c     UPDATE (AUG 2001): The Cray subroutine orders was ported to the
c     SP by NCEP personnel.  On the SP version, some changes were 
c     apparently made so that the size of the arrays for calling 
c     arguments 2, 3 and 4 (iwork, vtemp and isortix in my calling 
c     routine) must be the same.  This was not the case on the Crays,
c     and this was causing the  tracker to crash for cases far north
c     on fine grids (GFDL 1/3 grid).
c
c     INPUT:
c
c     xcenlon   fix longitude of storm center for current forecast hour
c     xcenlat   fix latitude of storm center for current forecast hour
c     imax      max i dimension of model grid
c     jmax      max j dimension of model grid
c     grdspc    grid spacing of model grid
c     valid_pt  logical bitmap for valid data at a grid point
c     cstormid  3-character storm ATCF ID (e.g., 03L, 11E, etc)
c     ifcsthr   integer value for current forecast hour
c
c     OUTPUT:
c   
c     igrret    return code from this subroutine
c     vradius   Contains the distance from the storm fix position to
c               each of the various wind threshhold distances in each
c               quadrant. (3,4) ==> (# of threshholds, # of quadrants)
c
c     LOCAL:
c
c     radmax    the maximum radius to look for winds for the various
c               thresholds.
c     quadinfo  This array contains the magnitude of the near-surface 
c               winds and the distance from the gridpoint to the  fix 
c               position for each point in each quadrant that is within
c               the maximum allowed radius, radmax.  quadinfo is 
c               allocated within this subroutine, and is allocated as
c               (quadrant, num_pts_in_quadrant, data_type), where 
c               data_type is either windspeed(1) or distance(2) from 
c               storm center to grid point.
c     quadmax   This array contains the max surface wind in each 
c               quadrant, plus the location of it and the distance from
c               the storm center.  This information is critical to 
c               identifying when this subroutine is malfunctioning.

      USE grid_bounds; USE tracked_parms; USE trig_vals; USE level_parms
c
      logical(1) valid_pt(imax,jmax)     
c      dimension iwork(257)
      real, allocatable :: vtemp(:),quadinfo(:,:,:),iwork(:)
      real      quadmax(4,4)
      real      exactdistnm,exactdistkm,radmax
      integer, allocatable :: isortix(:)
      integer   iwindix,ipoint,ifcsthr
      integer   quadct(4),vradius(3,4)
      real ::   windthresh(3) = (/17.5,25.7,32.9/)
      character cstormid*3
c
      print *,' '
      print *,' ****************************** '
      print *,' AT BEGINNING OF GETRADII'
      print *,' ****************************** '
      print *,' '
      print *,'xcenlon= ',xcenlon,' xcenlat= ',xcenlat
      print *,'imax= ',imax,' jmax= ',jmax,' grdspc= ',grdspc

      igrret  = 0
      vradius = 0
      
c     -----------------------------------------------------------
c     PART 1: Define the maximum radius for which you'll search
c     for the wind values, and then get the beginning and ending 
c     i and j points for that sub-region to search.  Define this
c     maximum radius (radmax) in terms of km.
c     -----------------------------------------------------------
 
      radmax = 650.0

c     Roughly fix xcenlat to the grid point just poleward of xcenlat,
c     and fix xcenlon to the grid point just EASTward of xcenlon.

      if (xcenlat >= 0.0) then
        jlatfix = int((glatmax - xcenlat)/grdspc + 1.)
      else
        jlatfix = ceiling((glatmax - xcenlat)/grdspc + 1.)
      endif

      ilonfix = int((xcenlon - glonmin)/grdspc + 2.)

c     Calculate number of grid points to have surrounding the storm so
c     that we are sure radmax is within those points.

      cosfac  = cos (xcenlat * dtr)
      numipts = ceiling((radmax/(dtk*grdspc))/cosfac)
      numjpts = ceiling(radmax/(dtk*grdspc))

      jbeg = jlatfix - numjpts
      jend = jlatfix + numjpts + 1
      ibeg = ilonfix - (numipts + 1)
      iend = ilonfix + numipts

      if (ibeg > imax .or. jbeg > jmax .or. ibeg < 1 .or. jbeg < 1 .or.
     &    iend < 1 .or. jend < 1) then
        print *,' '
        print *,'ERROR in getradii calculating ibeg, iend, jbeg or'
        print *,'jend.  ibeg= ',ibeg,' iend= ',iend,' jbeg= ',jbeg
        print *,'jend= ',jend
        print *,'Wind radii will not be calculated for this time.'
        igrret = 99
        return
      endif

      if (iend > imax) iend = imax
      if (jend > jmax) jend = jmax
 
      print *,' '
      print *,'In getradii, ibeg= ',ibeg,' iend= ',iend
      print *,'             jbeg= ',jbeg,' jend= ',jend
      print *,'  ilonfix= ',ilonfix,' jlatfix= ',jlatfix
 
c     -----------------------------------------------------------
c     PART 2: Within the area of grid points defined by jbeg, 
c     jend, ibeg and iend, (1) calculate all the wind speeds at 
c     each grid point, (2) calculate all of the distances from 
c     each grid point to the storm center, (3) assign each grid 
c     point to one of the 4 quadrants (NE,NW,SE,SW), (4) in each 
c     quadrant, sort the points, based on windspeed (use the Cray
c     subroutine, orders, to sort).
c     -----------------------------------------------------------

      jnum = jend - jbeg + 1
      inum = iend - ibeg + 1
      numalloc = ((jnum * inum) / 2) + inum/2 + jnum/2

      print *,'in getradii, numalloc= ',numalloc,' radmax= ',radmax

      allocate (quadinfo(4,numalloc,2),stat=iqa)

      if (iqa /= 0) then
        print *,' '
        print *,'!!! ERROR in sub getradii allocating quadinfo array.'
        print *,'!!! iqa = ',iqa
        igrret = 94
        return
      endif

      quadct = 0

c     Calculate the distances and wind speeds at each grid point.  If
c     the distance is < radmax, include that wind info in the 
c     appropriate quadinfo array location for that quadrant.

      quadmax = 0.0

      jloop: do j=jbeg,jend
        iloop: do i=ibeg,iend

          call calcdist (xcenlon,xcenlat,glon(i),glat(j),dist)
          if (dist > radmax) cycle iloop

          if (valid_pt(i,j)) then

            vmag = sqrt (u(i,j,levsfc)**2  + v(i,j,levsfc)**2)

cc            print *,'i= ',i,' j= ',j,' dist= ',dist,' vmag= ',vmag

            if (glon(i) >= xcenlon .and. glat(j) >= xcenlat) then
              ! NE quadrant
              quadct(1) = quadct(1) + 1
              quadinfo(1,quadct(1),1) = vmag
              quadinfo(1,quadct(1),2) = dist
              if (vmag > quadmax(1,4)) then
                quadmax(1,1) = glon(i)
                quadmax(1,2) = glat(j)
                quadmax(1,3) = dist
                quadmax(1,4) = vmag
              endif
            else if (glon(i) >= xcenlon .and. glat(j) < xcenlat) then
              ! SE quadrant
              quadct(2) = quadct(2) + 1
              quadinfo(2,quadct(2),1) = vmag
              quadinfo(2,quadct(2),2) = dist
              if (vmag > quadmax(2,4)) then
                quadmax(2,1) = glon(i)
                quadmax(2,2) = glat(j)
                quadmax(2,3) = dist
                quadmax(2,4) = vmag
              endif
            else if (glon(i) < xcenlon .and. glat(j) < xcenlat) then
              ! SW quadrant
              quadct(3) = quadct(3) + 1
              quadinfo(3,quadct(3),1) = vmag
              quadinfo(3,quadct(3),2) = dist
              if (vmag > quadmax(3,4)) then
                quadmax(3,1) = glon(i)
                quadmax(3,2) = glat(j)
                quadmax(3,3) = dist
                quadmax(3,4) = vmag
              endif
            else if (glon(i) < xcenlon .and. glat(j) >= xcenlat) then
              ! NW quadrant
              quadct(4) = quadct(4) + 1
              quadinfo(4,quadct(4),1) = vmag
              quadinfo(4,quadct(4),2) = dist
              if (vmag > quadmax(4,4)) then
                quadmax(4,1) = glon(i)
                quadmax(4,2) = glat(j)
                quadmax(4,3) = dist
                quadmax(4,4) = vmag
              endif
            endif

          endif

        enddo iloop
      enddo jloop

      print *,' '
      print *,'After loop, quadct(1)= ',quadct(1),' quadct(2)= '
     &       ,quadct(2)
      print *,'            quadct(3)= ',quadct(3),' quadct(4)= '
     &       ,quadct(4)
      print *,' '

      write (6,110) cstormid,ifcsthr,'NE',quadmax(1,1),quadmax(1,2)
     &             ,quadmax(1,3)*0.539638,quadmax(1,4)*1.9427
      write (6,110) cstormid,ifcsthr,'SE',quadmax(2,1),quadmax(2,2)
     &             ,quadmax(2,3)*0.539638,quadmax(2,4)*1.9427
      write (6,110) cstormid,ifcsthr,'SW',quadmax(3,1),quadmax(3,2)
     &             ,quadmax(3,3)*0.539638,quadmax(3,4)*1.9427
      write (6,110) cstormid,ifcsthr,'NW',quadmax(4,1),quadmax(4,2)
     &             ,quadmax(4,3)*0.539638,quadmax(4,4)*1.9427
      print *,' '

  110 format (' quadmax: ',a3,1x,i3.3,1x,a2,1x,' lon: ',f6.2,'E',1x
     &       ,' lat: ',f6.2,' radius: ',f7.2,' nm',2x,' vmag: '
     &       ,f6.2,' kts')

c     Now go through each quadrant and put the wind speed info into a 
c     temporary array (vtemp), sort that array, and then scan through
c     that array to find the various thresholds.  

      quadrantloop: do k=1,4

        allocate (isortix(quadct(k)),stat=iisa)
        allocate (vtemp(quadct(k)),stat=iva)
        allocate (iwork(quadct(k)),stat=iwa)
        if (iisa /= 0 .or. iva /= 0 .or. iwa /= 0) then
          print *,' '
          print *,'!!! ERROR in getradii allocating isortix or vtemp'
          print *,'!!! array for quadrant= ',k,' iisa = ',iisa
          print *,'!!! iva= ',iva,' iwa= ',iwa
          itret = 94
          return
        endif

        do m=1,quadct(k)
          vtemp(m) = quadinfo(k,m,1)
        enddo

        imode = 2
        isortix = 0
c-orig  call orders (imode,iwork,vtemp,isortix,quadct(k),1,8,1)

        print *,' '
        print *,' vtemp(isortix(1)) = ',vtemp(isortix(1))
        print *,' vtemp(isortix(quadct(k)))= ',vtemp(isortix(quadct(k)))
        print *,' isortix(1) = ',isortix(1)
        print *,' isortix(quadct(k)) = ',isortix(quadct(k))

        if (quadct(k) < 2) then   ! not enough members in array
          print *,' '
          print *,'!!! IN GETRADII, NOT ENOUGH MEMBERS IN ARRAY FOR'
          print *,'!!! QUADRANT #',k,' .... # members = quadct(k)= '
     &           ,quadct(k)
          print *,'!!! SETTING ALL VRADII = 0 for quadrant = ',k
          vradius(1,k) = 0
          vradius(2,k) = 0
          vradius(3,k) = 0
          cycle quadrantloop
        endif

c       Within this quadrant, go through the  sorted array of wind
c       magnitudes and compare those wind values against the set
c       wind thresholds to get the wind radii. 

        iwindix = 1
        ipoint = 0

        threshloop: do while (iwindix <= 3 .and. ipoint < quadct(k))

          ipoint = ipoint + 1

          if (vtemp(isortix(ipoint)) < windthresh(iwindix)) then
            cycle threshloop
          else
            if (ipoint == 1) then
              print *,' ' 
              print *,'!!! NOTE: In getradii, a max wind radius was' 
              print *,'!!! found at the maximum radius checked, so ' 
              print *,'!!! you may want to make sure that you are'
              print *,'!!! checking at a far enough distance from  ' 
              print *,'!!! the  fix position, that is, you may want to' 
              print *,'!!! increase the value of radmax in subroutine'
              print *,'!!! getradii.  Currently, radmax (km) = ',radmax
              vradius(iwindix,k) = int( (quadinfo(k,isortix(1),2) / 5.0)
     &                                + 0.5) * 5
            else 

c             Interpolate between the 2 closest distances to each wind
c             threshold to get "exact" distance to that wind threshold
c             radius, convert from km to nm, and then round to the 
c             nearest 5 nm (since TPC uses this precision). 
c             7/23/98 UPDATE: Jim Gross has asked that values not be
c             rounded to the nearest 5 nm, but rather only to the 
c             nearest 1 nm.

              exactdistkm = quadinfo(k,isortix(ipoint),2) + 
     &        ( (quadinfo(k,isortix(ipoint),1) - windthresh(iwindix)) /
     &          (quadinfo(k,isortix(ipoint),1) -
     &           quadinfo(k,isortix(ipoint-1),1)) *
     &          ( abs(quadinfo(k,isortix(ipoint),2) -
     &                quadinfo(k,isortix(ipoint-1),2)) ) )

              exactdistnm = exactdistkm * 0.5396   ! Convert km to nm
              vradius(iwindix,k) = int(exactdistnm + 0.5)

cc             vradius(iwindix,k) = int( (exactdistnm / 5.0) + 0.5) * 5


              print *,'iwindix= ',iwindix,' exactdistnm = ',exactdistnm
              print *,'vradius(iwindix,k) =',vradius(iwindix,k)

            endif

c           The possibility exists, especially for coarse  output 
c           grids, that there could be a jump over more than 1 wind-
c           thresh category when going from 1 grid point to the next, so
c           we need to account for this.  For example, if 1 point has
c           vmag = 15 m/s and the next point closer in has vmag = 28 
c           m/s, then between those 2 points you have the thresholds
c           for gale force AND storm force winds, so to be safe, we
c           actually need to subtract 1 from ipoint and re-check the
c           current point, if the wind value at that point is found 
c           to be greater than a wind threshold value.

            ipoint = ipoint - 1

            iwindix = iwindix + 1

          endif

        enddo threshloop

        deallocate (vtemp); deallocate(isortix); deallocate(iwork)

      enddo quadrantloop

      deallocate (quadinfo)

      return
      end

c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_max_wind (xcenlon,xcenlat,imax,jmax,grdspc
     &                        ,valid_pt,levsfc,vmax,igmwret)
c
c     ABSTRACT: This subroutine looks for the maximum near-surface wind
c     near the storm center.  Because different fcst Centers give us 
c     different parms, we will look at: 10m winds for AVN, MRF, GDAS, 
c     NGM and Eta; 10 m winds for NOGAPS; and surface winds for UKMET.
c     ECMWF does not send us any near-surface wind parameters.  By the
c     way, this subroutine is only concerned with the value of the max
c     wind, NOT where it's located radially with respect to the center.
c     The value that's returned in vmax is the max wind speed in m/s,
c     which are the units the data are stored in.  However, when the
c     max wind values are  output in output_atcf, they will be 
c     converted from m/s to knots.
c
c     INPUT:
c
c     xcenlon   fix longitude of storm center for current forecast hour
c     xcenlat   fix latitude of storm center for current forecast hour
c     imax      max i dimension of model grid
c     jmax      max j dimension of model grid
c     grdspc    grid spacing of model grid
c     valid_pt  logical bitmap for valid data at a grid point
c     levsfc    integer holding the value of the array member that holds
c               the near-surface winds in the u and v arrays (at orig
c               writing, it's = 4).
c
c     OUTPUT:
c    
c     vmax      value of maximum near-surface wind near the storm ctr
c     igmwret   return code from this subroutine
c
c     LOCAL:
c
c     radmaxwind the maximum radius to look for a max wind near the 
c                storm center.  You have to allow this to be bigger for
c                model grids with coarse resolution (ECMWF 2.5 degree).

      USE grid_bounds; USE tracked_parms; USE trig_vals

      real      radmaxwind
      logical(1) valid_pt(imax,jmax)
c
      igmwret = 0

      if (grdspc <= 1.25) then
        radmaxwind = 300.0
      else
        radmaxwind = 500.0
      endif

c     Roughly fix xcenlat to the grid point just poleward of xcenlat,
c     and fix xcenlon to the grid point just EASTward of xcenlon.

      if (xcenlat >= 0.0) then
        jlatfix = int((glatmax - xcenlat)/grdspc + 1.)
      else
        jlatfix = ceiling((glatmax - xcenlat)/grdspc + 1.)
      endif

      ilonfix = int((xcenlon - glonmin)/grdspc + 2.)

c     Calculate number of grid points to have surrounding the storm so
c     that we are sure radmaxwind is within those points.

      cosfac  = cos (xcenlat * dtr)
      numipts = ceiling((radmaxwind/(dtk*grdspc))/cosfac)
      numjpts = ceiling(radmaxwind/(dtk*grdspc))
 
      jbeg = jlatfix - numjpts
      jend = jlatfix + numjpts + 1
      ibeg = ilonfix - (numipts + 1)
      iend = ilonfix + numipts

      if (ibeg > imax .or. jbeg > jmax .or. ibeg < 1 .or. jbeg < 1 .or.
     &    iend < 1 .or. jend < 1) then
        print *,' '
        print *,'ERROR in get_max_wind calculating ibeg, iend, jbeg or'
        print *,'jend.  ibeg= ',ibeg,' iend= ',iend,' jbeg= ',jbeg
        print *,'jend= ',jend
        print *,'Value of vmax will be set to 0 for this time.'
        vmax = 0.0
        igmwret = 99
        return
      endif

      if (iend > imax) iend = imax
      if (jend > jmax) jend = jmax
c
      print *,' '
      print *,'In get_max_wind, ibeg= ',ibeg,' iend= ',iend
      print *,'                 jbeg= ',jbeg,' jend= ',jend
      print *,'        ilonfix= ',ilonfix,' jlatfix= ',jlatfix
c
      vmax = 0.0
      do j=jbeg,jend
        do i=ibeg,iend

          call calcdist (xcenlon,xcenlat,glon(i),glat(j),dist)

          if (dist > radmaxwind) cycle

          if (valid_pt(i,j)) then
            vmag = sqrt (u(i,j,levsfc)**2  + v(i,j,levsfc)**2)
            if (vmag > vmax) vmax = vmag
          endif

        enddo
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine fixcenter (clon,clat,ist,ifh,calcparm,geslon,geslat
     &      ,inp,ifcsthour,stderr,fixlon,fixlat,xvalues,maxstorm,ifret)
c
c     ABSTRACT: This subroutine loops through the different parameters
c               for the input storm number (ist) and calculates the 
c               center position of the storm by taking an average of
c               the center positions obtained for those parameters.
c               First we check to see which parameters are within a 
c               max error range (errmax), and we discard those that are
c               not within that range.  Of the remaining parms, we get 
c               a mean position, and then we re-calculate the position
c               by giving more weight to those estimates that are closer
c               to this mean first-guess position estimate.
c
c     INPUT:
c     clon     Center longitudes of tracked parms for this storm & ifh
c     clat     Center latitudes of tracked parms for this storm & ifh
c     ist      Storm number
c     ifh      Index for forecast hour
c     calcparm Logical; Use this parm's location for this storm or not
c     geslon   Initial guess longitude for this storm at this fcst hour
c     geslat   Initial guess latitude for this storm at this fcst hour
c     inp      contains the input date and model number information
c     ifcsthour integer containing the current forecast hour
c     xvalues  The actual max or min data values for each parameter
c     maxstorm max # of storms to be handled in this run
c
c     INPUT/OUTPUT:
c     stderr   Standard deviation of the position "error" of the parms
c              relative to the guess storm position.  As long as the 
c              distance of a parm center to the guess center is <=
c              errpmax, it is included in the std dev calculation.
c
c     OUTPUT:
c     fixlon   Best approximation of storm center's longitude
c     fixlat   Best approximation of storm center's latitude
c     ifret    Return code from this subroutine
c
c     LOCAL:
c     storm       Contains tcvitals info for the storms (def_vitals)
c     trkerr_avg  Sum/avg of the track errors for all parms for this
c                 fcst hour, regardless of whether or not the error was
c                 > errmax.  It's used for getting the std deviation of
c                 the position error for this forecast time, to be used
c                 as part of the errmax calculation for the next fcst 
c                 time.
c     iclose      Number of parameters whose position estimates are 
c                 found to be within a distance errmax of the guess pos
c     wtpos       The weight given to each position estimate.  It's 
c                 based on the distance from the average position.
c     errdist     The "error" of the parameter center position relative
c                 to the storm's guess position.
c     avgerr      Average "error" of the parameter center positions
c                 relative to the storm's guess position.
c     use4next    Logical; If a parm center has been calculated but its
c                 distance from the guess position is > errmax, we don't
c                 use this center in calculating the new guess position,
c                 however we will use this position in calculating the 
c                 standard deviation of the current time's guess 
c                 positions, to be used in calculating the new errmax 
c                 for the next forecast time.  So in this subroutine,
c                 calcparm may be set to FALSE if errdist > errmax, but
c                 use4next will not be set to FALSE (Actually, it is 
c                 only set to FALSE if errdist > errpmax, which is 
c                 defined in error_parms and is roughly 600km).
c     stderr_close  Standard deviation of position errors for parms that
c                 have center estimates that are within a distance 
c                 errmax of the guess position.
c     clon_fguess These are the first-guess mean position estimates, 
c     clat_fguess which are the means of the position estimates that
c                 are within a distance errmax.  These first-guess mean
c                 positions will be refined by giving more weight to
c                 individual parameter estimates that are closer to 
c                 this first-guess mean position.
c     dist_from_mean Contains the "error" distance of each parameter
c                 from the first-guess mean position (clon_fguess,
c                 clat_fguess).  NOTE: If a parameter is not within
c                 a distance errmax of the guess position for this
c                 time (geslon,geslat), then there will be NO 
c                 dist_from_mean calculated for that parm.
c
      USE error_parms; USE set_max_parms; USE inparms; USE def_vitals
      USE atcf
c
      type (datecard) inp
c
      real      clon(maxstorm,maxtime,maxtp),temp_clon(maxtp)
      real      clat(maxstorm,maxtime,maxtp),temp_clat(maxtp)
      real      fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real      trkerr(maxtp),errdist(maxtp),xvalues(maxtp)
      real      stderr(maxstorm,maxtime),devia(maxtp),wtpos(maxtp)
      real      dist_from_mean(maxtp)
      logical(1) calcparm(maxtp,maxstorm),use4next(maxtp)
      character charparm(9)*8,charmaxmin(9)*8
c
      data charparm /'zeta 850','zeta 700','vmag 850','NOT USED'
     &   ,'vmag 700','NOT USED',' gph 850',' gph 700','    MSLP'/
      data charmaxmin /'  Max   ','  Max   ','  Min   ','NOT USED'
     &   ,'  Min   ','NOT USED','  Min   ','  Min   ','  Min   '/
c
      ifret=0
c
c     We need to judge whether each parameter position is reasonable,
c     so we'll check to make sure that the dist from each parameter's 
c     estimate to the guess position is less than a maximum allowable 
c     error. If it's the first forecast time, use the initial error max 
c     (defined as errinit in error_parms) as errmax.  Otherwise, the 
c     max error criterion is that the distance error must not exceed 3 
c     times the previous forecast time's standard deviation (after a 
c     small growth factor has been applied).
c     UPDATE 3/5/98: During testing, it was found that just using the
c     previous time's stdev made errmax too "jumpy" (i.e., at vt=48h,
c     errmax could = 380, and then at vt=54h, errmax could jump down
c     to 190, so we've changed it so that it uses an average of the 
c     stdev's from the 3 previous forecast times to maintain some
c     continuity between successive forecast times).
c
      if (ifh == 1) then
        if (atcfname == 'AVNO' .or. atcfname == 'MRFO' .or. 
     &      atcfname == 'GDAS' .or. atcfname == 'GFDT' .or.
     &      atcfname(1:3) == 'AP0' .or. atcfname(1:3) == 'AN0' .or.
     &      atcfname(1:3) == 'AP1' .or. atcfname(1:3) == 'AN1' .or.
     &      atcfname(1:3) == 'AC0' .or. atcfname == 'AEAR' ) then
          errmax  = err_avn_init
          errinit = err_avn_init
        else if (atcfname == 'EMX ') then
          errmax  = err_ecm_max
          errinit = err_ecm_max
        else
          errmax  = err_reg_init
          errinit = err_reg_init
        endif
      else
        if (atcfname == 'AVNO' .or. atcfname == 'MRFO' .or.
     &      atcfname == 'GDAS' .or. atcfname == 'GFDT' .or.
     &      atcfname(1:3) == 'AP0' .or. atcfname(1:3) == 'AN0' .or.
     &      atcfname(1:3) == 'AP1' .or. atcfname(1:3) == 'AN1' .or.
     &      atcfname(1:3) == 'AC0' .or. atcfname == 'AEAR') then
          errinit = err_avn_init
        else if (atcfname == 'EMX ') then
          errinit = err_ecm_max
        else
          errinit = err_reg_max
        endif

        if (ifh >= 4) then
          xavg_stderr = (stderr(ist,ifh-3) + stderr(ist,ifh-2)
     &                +  stderr(ist,ifh-1)) / 3.0
        else if (ifh == 3) then
          xavg_stderr = (stderr(ist,ifh-2) + stderr(ist,ifh-1)) / 2.0
        else if (ifh == 2) then
          xavg_stderr = stderr(ist,ifh-1)
        endif

        errmax = amin1(amax1(3.0*xavg_stderr*errpgro,errinit)
     &                ,errpmax)
      endif
c
      print *,' '
      if (ifh > 1) then
        print *, '(a42,f8.2,a15,f8.2)'
     &         ,' At beg of fixcenter, stderr(ist,ifh-1) = '
     &         ,stderr(ist,ifh-1),'  xavg_stderr= ',xavg_stderr 
      else
        print *, '(a42,f8.2,a15,f8.2)'
     &         ,' At beg of fixcenter, stderr(ist,ifh-1) = N/A'
     &         ,'  xavg_stderr= ',xavg_stderr 
      endif
      print *,'At beg of fixcenter, errpgro = ',errpgro
      print *,'At beg of fixcenter, errinit = ',errinit
      print *,'At beg of fixcenter, errpmax = ',errpmax
      print *,'At beg of fixcenter, ifh= ',ifh,' errmax= ',errmax
c
      trkerr_avg = 0.0
      iclose = 0; itot4next = 0 
      clonsum = 0.0; clatsum = 0.0
      errdist = 0.0
      use4next = .FALSE.
 
c     For each parm, check to see if the estimated center is within
c     distance errmax of the guess center.  If it's within errmax,
c     then use that parm for locating the center.  If it's NOT
c     within errmax, but IS within errpmax, then we still use this
c     in calculating the standard deviation of the parameters for
c     helping to determine the errmax for the next forecast hour.
c     NOTE: For calculating the std dev to be used for the next
c     forecast hour, do NOT use vmag 850 or vmag 700, since those
c     parms are always guaranteed to be within a short range of 
c     the guess, due to the nature of the algorithm (see subroutine
c     get_uv_center for further details on that).

      do ip=1,maxtp

        if (ip == 4 .or. ip == 6) then   ! Parms 4 & 6 not defined.
          calcparm(ip,ist) = .FALSE.   
          cycle  
        endif
        if (calcparm(ip,ist)) then
          call calcdist (geslon,geslat,clon(ist,ifh,ip)
     &                    ,clat(ist,ifh,ip),dist)
          errdist(ip) = dist
          if (dist <= errpmax) then
            if (ip == 3 .or. ip == 5) then
              use4next(ip) = .FALSE.
            else
              use4next(ip) = .TRUE.
              trkerr_avg = trkerr_avg + dist
              itot4next = itot4next + 1
            endif
          endif
          if (dist <= errmax) then
            iclose = iclose + 1
            clonsum = clonsum + clon(ist,ifh,ip)
            clatsum = clatsum + clat(ist,ifh,ip)
          else 
            calcparm(ip,ist) = .FALSE.
          endif
        endif

      enddo

      if (iclose > 0) then
        clon_fguess = clonsum / float(iclose)
        clat_fguess = clatsum / float(iclose)
      endif

c     Print out a table listing of the locations of the  fixes for 
c     the individual parameters.

      print *,' '
      print *,'--------------------------------------------------'
      print *,'Individual fixes follow..., fhr= ',ifcsthour
     &       ,'  ',storm(ist)%tcv_storm_id,' ',storm(ist)%tcv_storm_name
      print *,'Model name = ',atcfname
      print *,'Values of -99.99 indicate that a fix was unable to be '
      print *,'made for that paramater.  Parameters 4 & 6 are not used.'
      print *,'Vorticity data values are scaled by 1e5.  errdist is the'
      print *,'distance that the position estimate is from the guess'
      print *,'position for this time.  MSLP value here may differ from'
      print *,'that in the atcfunix file since the one here is that '
      print *,'derived from the area-averaged barnes analysis, while '
      print *,'that in the atcfunix file is from a specific gridpoint.'
      write (6,21) geslon,360.-geslon,geslat
      write (6,*)  ' '
      write (6,23) 
      write (6,25)

      if (geslat > 0.0) then
        charmaxmin(1) = '  Max   '
        charmaxmin(2) = '  Max   '
      else 
        charmaxmin(1) = '  Min   '
        charmaxmin(2) = '  Min   '
      endif

      do ip=1,maxtp
        if (ip == 1 .or. ip == 2) then 
          if (clon(ist,ifh,ip) < 0.001 .and. 
     &        clon(ist,ifh,ip) > -0.001) then
            write (6,27) ip,charparm(ip),charmaxmin(ip),0.0
     &         ,0.0,clat(ist,ifh,ip),xvalues(ip)*1e5
     &         ,calcparm(ip,ist),errdist(ip)
          else
            write (6,27) ip,charparm(ip),charmaxmin(ip),clon(ist,ifh,ip)
     &         ,360.-clon(ist,ifh,ip),clat(ist,ifh,ip),xvalues(ip)*1e5
     &         ,calcparm(ip,ist),errdist(ip)
          endif
        else
          if (clon(ist,ifh,ip) < 0.001 .and.
     &        clon(ist,ifh,ip) > -0.001) then
            write (6,27) ip,charparm(ip),charmaxmin(ip),0.0
     &         ,0.0,clat(ist,ifh,ip),xvalues(ip)
     &         ,calcparm(ip,ist),errdist(ip)
          else
            write (6,27) ip,charparm(ip),charmaxmin(ip),clon(ist,ifh,ip)
     &         ,360.-clon(ist,ifh,ip),clat(ist,ifh,ip),xvalues(ip)
     &         ,calcparm(ip,ist),errdist(ip)
          endif
        endif
      enddo

 21   format (' Guess location for this time: ',f7.2,'E  (',f6.2,'W)'
     &       ,2x,f7.2)
 23   format (' parm#    parm    Max/Min   Lon_fix(E)  Lon_fix(W)'
     &       ,'   Lat_fix   Max/Min_value   calcparm   errdist(km)')
 25   format (' -----    ----    -------   ----------  ----------'
     &       ,'   -------   -------------   --------   ----------')
 27   format (2x,i2,4x,a8,2x,a8,3x,f7.2,5x,f7.2,4x,f7.2,7x,f9.2
     &       ,6x,L2,7x,f7.2)


c     If number of parameter centers close enough (iclose) > 0, then
c     calculate the center by taking an average of all the parameter
c     center positions that are within distance errmax from the guess
c     position (geslon,geslat).  Get a first-guess mean position, and
c     then re-calculate the position estimate by giving more weight
c     to those positions that are closer to the first-guess mean
c     position.

      dist_from_mean = 0.0

      if (iclose > 0) then

c       Get distances from first-guess mean position....

        do ip=1,maxtp
          if (calcparm(ip,ist)) then
            call calcdist (clon_fguess,clat_fguess,clon(ist,ifh,ip)
     &                    ,clat(ist,ifh,ip),dist)
            dist_from_mean(ip) = dist
          endif
        enddo
           
c       Get the mean distance of each parameter estimate from 
c       the first-guess mean position

        call avgcalc (dist_from_mean,maxtp,calcparm(1,ist)
     &               ,xmn_dist_from_mean,iaret)

        if (iaret == 0) then

          call stdevcalc (dist_from_mean,maxtp,calcparm(1,ist)
     &                   ,xmn_dist_from_mean,stderr_close,isret)

          print *,' '
          print *,'After stdevcalc, xmn_dist_from_mean= '
     &           ,xmn_dist_from_mean,' stderr_close= '
     &           ,stderr_close,' isret= ',isret

        endif
        if (iaret /= 0 .or. isret /= 0) then
          print *,' '
          print *,'!!! ERROR IN FIXCENTER -- Error occurred in either'
          print *,'!!! avgcalc or stdevcalc.  Storm number = ',ist
          print *,'!!! RCC from avgcalc = ',iaret
          print *,'!!! RCC from stdevcalc = ',isret
          print *,'!!! Center fix will NOT be made, and processing for'
          print *,'!!! this storm is ending.  The probable cause is '
          print *,'!!! that no calcparms were valid for this storm at' 
          print *,'!!! this forecast hour.'
          fixlon(ist,ifh) = -999.0
          fixlat(ist,ifh) = -999.0
          ifret = 95
          return
        endif

        if (calcparm(1,ist) .or. calcparm(2,ist) .or. calcparm(7,ist)
     &      .or. calcparm(8,ist) .or. calcparm(9,ist)) then
          continue
        else
          print *,' '
          print *,'!!! In fixcenter, STOPPING PROCESSING for this'
          print *,'!!! storm.  The reason is that none of the  fix'
          print *,'!!! locations for parms z850, z700, zeta 850,'
          print *,'!!! zeta 700 or MSLP were within a reasonable'
          print *,'!!! distance of the guess location.  As such,'
          print *,'!!! no attempt will be made to fix the vmag 850'
          print *,'!!! or vmag 700 minima since, by the nature of'
          print *,'!!! the algorithm for these 2 parms, a fix '
          print *,'!!! location WILL ALWAYS be returned that is '
          print *,'!!! within a reasonable distance of the center'
          print *,'!!! guess position (the other 5 parameters may'
          print *,'!!! or may not do so).  So if the other 5 parms'
          print *,'!!! insist that the guess is too far away, we '
          print *,'!!! do not want to grab a false center with '
          print *,'!!! the vmag minima.'
          print *,'!!! ist= ',ist,' fhr= ',ifcsthour
          fixlon(ist,ifh) = -999.0
          fixlat(ist,ifh) = -999.0
          ifret = 95
          return
        endif

c       Now re-calculate the mean position by giving more weight 
c       to those position estimates that are closer to the first
c       guess mean position.  Note that if stderr_close < 5.0, we
c       force it to be 5.0; we do this to avoid getting very 
c       large numbers for devia values, which could make the 
c       weights (wtpos) equal to 0.  This occurred during testing
c       when only 2 parameters were valid, and so, of course, the
c       standard deviation from the mean of those 2 parameters 
c       was close to 0, which gave devia values around 6000, and
c       then wtpos values of 0, leading to a divide by 0 crash
c       later on in subroutine wtavrg.

        kprm=0

        if (stderr_close > 0.0) then
          if (stderr_close < 5.0) then
            print *,' '
            print *,'NOTE: Since stderr_close had a value less than'
            print *,'5, stderr_close has been forced to be equal'
            print *,'to 5 in order to avoid dividing by zero later'
            print *,'on in subroutine wtavrg.'
            stderr_close = 5.0
          endif
          do ip=1,maxtp
            if (calcparm(ip,ist)) then
              kprm = kprm + 1
              devia(kprm) = dist_from_mean(ip) / stderr_close
              wtpos(kprm) = exp(-devia(kprm))
              temp_clon(kprm) = clon(ist,ifh,ip)
              temp_clat(kprm) = clat(ist,ifh,ip)
            endif
          enddo
        else
c     
c         This next if statement is for the case in which only 1
c         parameter is valid, for which the stderr_close will = 0
c         (obviously), but as long as we have 1 valid parameter,
c         continue processing, and set the weight for that parm = 1.
c         The else portion is for the case in which stderr_close
c         = 0 with NO parms being close.
c
          if (iclose == 1) then
            do ip=1,maxtp
              if (calcparm(ip,ist)) then
                kprm = kprm + 1
                wtpos(kprm) = 1
                temp_clon(kprm) = clon(ist,ifh,ip)
                temp_clat(kprm) = clat(ist,ifh,ip)
              endif
            enddo
          else
            print *,' '
            print *,'!!! ERROR IN FIXCENTER, stderr_close not > 0'
            print *,'!!! stderr_close = ',stderr_close
            print *,'!!! The probable cause is that no calcparms were'
            print *,'!!! valid for this storm at this forecast hour.'
            fixlon(ist,ifh) = -999.0
            fixlat(ist,ifh) = -999.0
            ifret = 95
            return
          endif
        endif
c
        if (kprm > 0) then
          call wtavrg (temp_clon,wtpos,kprm,fixlon(ist,ifh),iwtret1)
          call wtavrg (temp_clat,wtpos,kprm,fixlat(ist,ifh),iwtret2)
          if (iwtret1 > 0 .or. iwtret2 > 0) then
            print *,' '
            print *,'!!! ERROR IN FIXCENTER in call to wtavrg.'
            print *,'!!! Return Codes from wtavrg calls follow: '
            print *,'!!!   RCC from wtavrg for long fix: ',iwtret1
            print *,'!!!   RCC from wtavrg for lat  fix: ',iwtret2
            print *,'!!! This means a divide by zero would have '
            print *,'!!! been attempted, which means that the '
            print *,'!!! weights in wtpos are not > 0.  Check in'
            print *,'!!! subroutine  fixcenter where devia values'
            print *,'!!! are calculated to see if something is '
            print *,'!!! wrong there.  Values of wtpos array follow:'
            print *,'!!! ',wtpos
            print *,'!!! ist= ',ist,' ifh= ',ifh,' iclose= ',iclose
            print *,'!!! errmax= ',errmax,' kprm= ',kprm
            print *,' '
            fixlon(ist,ifh) = -999.0
            fixlat(ist,ifh) = -999.0
            ifret = 95
            return
          endif
        else
          print *,' '
          print *,'!!! ERROR IN FIXCENTER, kprm NOT > 0'
          print *,'!!! This means that, for whatever reason, the '
          print *,'!!! calcparm logical flag was set to .FALSE. for'
          print *,'!!! all of the parameters.  Thus, a center position'
          print *,'!!! could not be obtained for this storm'
          print *,'!!! ist= ',ist,' ifh= ',ifh,' iclose= ',iclose
          print *,'!!! errmax= ',errmax,' kprm= ',kprm
          fixlon(ist,ifh) = -999.0
          fixlat(ist,ifh) = -999.0
          ifret = 95
          return
        endif

      else
        print *,' '
        print *,'!!! NOTE: IN FIXCENTER, No storms are within errmax OR'
        print *,'!!! the calcparm logical flag was set to .FALSE. for'
        print *,'!!! all of the parameters.  Thus, a center position'
        print *,'!!! could not be obtained for this storm'
        print *,'!!! ist= ',ist,' ifh= ',ifh,' iclose= ',iclose
        print *,'!!! errmax= ',errmax
        fixlon(ist,ifh) = -999.0
        fixlat(ist,ifh) = -999.0
        ifret = 95
        return
      endif 
 
c     Now calculate the average error of all the parms that are within
c     a radius errpmax (defined in error_parms, ~600km), and the std
c     dev of those errors.  This standard deviation will be used in
c     calculating the maximum allowable error for the next forecast 
c     time.
 
      if (itot4next > 0 .and. ifret /= 95) then
        trkerr_avg = trkerr_avg / float(itot4next)
        call stdevcalc (errdist,maxtp,use4next,trkerr_avg
     &                 ,stderr(ist,ifh),isret)
        if (isret /= 0) then
          print *,' '
          print *,'!!! ERROR in FIXCENTER calculating std deviation '
          print *,'!!! for use in next forecast hours errmax.'
          print *,'!!! ist= ',ist,' ifh= ',ifh,' itot4next= ',itot4next
          ifret = 95
        endif
      endif
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine avgcalc (xdat,kmax,valid,xavg,iaret)
c
c     ABSTRACT: This subroutine just calculates a straight average of
c     the parameters in the input array (xdat).  The logical array 
c     (valid) indicates whether or not to include a particular array
c     member or not in the calculation.
 
      real      xdat(kmax)
      logical(1) valid(kmax)
c
      iaret = 0
c
      xsum = 0.0
      ict = 0
      do i=1,kmax
        if (valid(i)) then
          xsum = xsum + xdat(i)
          ict = ict + 1
        endif
      enddo 
c
      if (ict > 0) then
        xavg = xsum / float(ict)
      else
        print *,' '
        print *,'!!! ERROR in avgcalc, ict NOT > 0'
        xavg = xdat(1)
        iaret = 95
      endif
c 
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine wtavrg (xdat,wt,kmax,xwtavg,iwtret)
c
c     ABSTRACT: This subroutine calculates a weighted average of the 
c     parameters in the input array (xdat) using the input weights
c     in the input array (wt).  It is used to calculate the center lat
c     and lon fix positions.
c
      real     xdat(kmax),wt(kmax)
c
      iwtret = 0
c
      xwtavg = 0.0
      wtot = 0.0
      do i=1,kmax
        xwtavg = xwtavg + xdat(i)*wt(i)
        wtot = wtot + wt(i)
      enddo
c
      if (wtot > 0.0) then
        xwtavg = xwtavg / wtot
      else
        print *,' '
        print *,'!!! ERROR in wtavrg, wtot NOT > 0'
        iwtret = 95
      endif
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine stdevcalc (xdat,kmax,valid,xavg,stdx,isret)
c
      real      xdat(kmax)
      logical(1) valid(kmax)
c
      isret = 0
c
      stdx = 0.0
      ict = 0
      do i=1,kmax
        if (valid(i)) then
          stdx = stdx + (xdat(i) - xavg)**2
          ict = ict + 1
        endif
      enddo
 
      if (ict > 0) then
        stdx = sqrt(stdx/float(ict))
        if (stdx == 0.0) then
c         This can happen if you have just 2 points; The mean position
c         will be exactly in the middle of the 2 points and so the
c         standard deviation around that mean point will be 0.  And
c         since the calling routine will quit if the returned standard
c         deviation is 0, we must force it to be 1 so the program
c         continues running.  Theoretically, it could also happen with
c         3 or more points, but the likelihood of the distances working
c         out to exactly equidistant for 3 points is not that good.
          stdx = 1.0
        endif
      else
        print *,' '
        print *,'!!! ERROR in stdevcalc, ict NOT > 0'
        isret = 95
      endif
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_uv_center (uvgeslon,uvgeslat,imax,jmax,grdspc
     &                     ,ist,level,valid_pt,cflag
     &                     ,ctlon,ctlat,xval,igucret)
c
c     ABSTRACT: This subroutine calculates the center fix position for
c     the minimum in the wind speed near the storm center.  This center
c     fix is done differently than for the other parms.  With this fix,
c     we severely limit the area that is searched, because we do not 
c     want to confuse a wind minimum out on the periphery of a storm 
c     with the center wind minimum.  Therefore, this subroutine is not
c     called until center fixes have been made for the 5 other parms
c     (z850, z700, zeta850, zeta700, mslp).  Once those  fixes have been
c     made, a modified first guess is made of the average of the guess
c     position for this time and the 5 other parm fixes.  That modified
c     guess position is passed into this subroutine as uvgeslon and 
c     uvgeslat, and that's where the searching for the wind minimum 
c     is done.  To get the wind minimum, the u and v data are first 
c     interpolated down to a fine grid (see details below for exact
c     figures), and then a single-pass barnes analysis is done on that
c     fine grid.  The reason that we first interpolate the data (which
c     is different from how we do the other parms) is that if we just 
c     use the original grid resolution, we may not be able to 
c     accurately pick out a minimum in the wind field at the center.
c
      USE radii; USE grid_bounds; USE tracked_parms; USE trig_vals
      USE level_parms
c
      real, allocatable ::  uold(:,:),vold(:,:),unew(:,:),vnew(:,:)
      real, allocatable ::  rlonold(:),rlatold(:),rlonnew(:),rlatnew(:)
      real, allocatable ::  vmag(:,:)
      character*1 ::   gotlat
      logical(1)        cflag, valid_pt(imax,jmax)
      logical(1), allocatable :: lbi(:,:)
c
      gotlat = 'n'
c
c     -----------------------------------------------------------------
c     INTERPOLATE INPUT GRID TO SMALLER GRID
c     -----------------------------------------------------------------
c
c     Get beginning and ending j points (on the input grid) for a 
c     smaller array that surrounds the storm.  It is this smaller array
c     that we will interpolate to a finer grid.
c
c     Calculate number of pts to either side of this j to search
c
      npts = ceiling(rads_vmag/(dtk*grdspc))
c
      call get_ij_bounds (npts,0,ritrk_vmag,imax,jmax,grdspc
     &             ,glatmax,glatmin,glonmax,glonmin,uvgeslon,uvgeslat
     &             ,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)

      if (igiret /= 0) then
        print *,' '
        print *,'!!! ERROR in get_uv_center from call to get_ij_bounds,'
        print *,'!!! stopping processing for storm number ',ist
        igucret = 92
        return
      endif
c
      select case (level)
        case (850);  nlev = nlev850  ! check module level_parms for
        case (700);  nlev = nlev700  ! the values of these....
        case (500);  nlev = nlev500
      end select

c     This next if statement determines how many times to interpolate
c     the input grid to a smaller grid.  Here are the grid sizes for 
c     some of the typical grids that will be used:
c
c      Original grid size     # of interps        Final grid size
c     --------------------    ------------     ---------------------
c     1.00 deg (111.19 km)        3             0.125 deg (13.9 km)
c     1.25 deg (138.99 km)        3             0.156 deg (17.4 km)
c     2.50 deg (277.99 km)        4             0.156 deg (17.4 km)

      if (grdspc > 1.5) then
        numinterp = 4
      else
        numinterp = 3
      endif
  
      dell = grdspc
      imxold = iend - ibeg + 1
      jmxold = jend - jbeg + 1

c     --------------------------------------------------------------
c     Before interpolating, make sure that all the original
c     points have valid data.  If they don't then exit the
c     subroutine.  NOTE: This is NOT checking to see if ALL the pts
c     on the complete & full input grid have valid data; it only
c     checks those points that are within the box returned from
c     get_ij_bounds.

      do i=ibeg,iend
        do j=jbeg,jend
          if (.not. valid_pt(i,j)) goto 975
        enddo
      enddo
c
c     ------------------------------------
c     Now begin the interpolation process
c
      allocate (uold(imxold,jmxold),stat=iuo)
      allocate (vold(imxold,jmxold),stat=ivo)
      allocate (rlonold(imxold),stat=iloo)
      allocate (rlatold(jmxold),stat=ilao)
      if (iuo /= 0 .or. ivo /= 0 .or. iloo /= 0 .or. ilao /= 0) goto 970
 
      do intnum = 1,numinterp

        if (intnum == 1) then

          do i=ibeg,iend
            rlonold(i-ibeg+1) = glon(i)
            do j=jbeg,jend
              uold(i-ibeg+1,j-jbeg+1) = u(i,j,nlev)
              vold(i-ibeg+1,j-jbeg+1) = v(i,j,nlev)
              if (gotlat == 'n') then
                rlatold(j-jbeg+1) = glat(j)
              endif
            enddo
            gotlat = 'y'    ! Only need to fill rlatold once
          enddo

        else

          deallocate (uold); deallocate (vold)
          deallocate (rlonold); deallocate (rlatold)
          allocate (uold(imxnew,jmxnew),stat=iuo)
          allocate (vold(imxnew,jmxnew),stat=ivo)
          allocate (rlonold(imxnew),stat=iloo)
          allocate (rlatold(jmxnew),stat=ilao)
          if (iuo /= 0 .or. ivo /= 0 .or.
     &        iloo /= 0 .or. ilao /= 0) goto 970

          gotlat = 'n'
          do i=1,imxnew
            rlonold(i) = rlonnew(i)
            do j=1,jmxnew
              uold(i,j) = unew(i,j)
              vold(i,j) = vnew(i,j)
              if (gotlat == 'n') then
                rlatold(j) = rlatnew(j)
              endif
            enddo
            gotlat = 'y'
          enddo

          imxold = imxnew
          jmxold = jmxnew
          deallocate (unew); deallocate (vnew)
          deallocate (rlonnew); deallocate (rlatnew)

        endif
 
        dell = 0.5 * dell
        imxnew = 2 * imxold - 1
        jmxnew = 2 * jmxold - 1

        allocate (unew(imxnew,jmxnew),stat=iuo)
        allocate (vnew(imxnew,jmxnew),stat=ivo)
        allocate (rlonnew(imxnew),stat=iloo)
        allocate (rlatnew(jmxnew),stat=ilao)
        if (iuo /= 0 .or. ivo /= 0 .or. 
     &      iloo /= 0 .or. ilao /= 0) goto 971

        call bilin_int (imxold,jmxold,uold
     &                 ,imxnew,jmxnew,unew,ibiret)
        call bilin_int (imxold,jmxold,vold
     &                 ,imxnew,jmxnew,vnew,ibiret)
        call lin_int (imxold,imxnew,rlonold,rlonnew,iliret)
        call lin_int (jmxold,jmxnew,rlatold,rlatnew,iliret)
 
        chk_lonspc_old = rlonold(imxold) - rlonold(imxold - 1)
        chk_latspc_old = rlatold(jmxold) - rlatold(jmxold - 1)
        chk_lonspc_new = rlonnew(imxnew) - rlonnew(imxnew - 1)
        chk_latspc_new = rlatnew(jmxnew) - rlatnew(jmxnew - 1)
        
        print *,' '
        print *,'In get_uv_center, intnum= ',intnum
        print *,'imxold= ',imxold,' imxnew= ',imxnew
        print *,'jmxold= ',jmxold,' jmxnew= ',jmxnew
 
      enddo
 
c     ------------------

      deallocate (uold); deallocate (vold)
      deallocate (rlonold); deallocate(rlatold)

      allocate (vmag(imxnew,jmxnew),stat=ivm)
      allocate (lbi(imxnew,jmxnew),stat=ilb)
      if (ivm /= 0 .or. ilb /= 0) goto 972
      call calc_vmag (unew,vnew,imxnew,jmxnew,vmag,icvret)
      deallocate (unew); deallocate (vnew)
 
      lbi = .TRUE.

      print *,' '
      print *,'Before call to find_maxmin, imxnew= ',imxnew
     &       ,'jmxnew= ',jmxnew,' ist= ',ist
      write (6,171) dell,uvgeslon,360.-uvgeslon,uvgeslat
 171  format (' dell= ',f7.3,' uvgeslon= ',f8.3,'E  (',f8.3,'W)'
     &       ,'  uvgeslat= ',f8.3)

      call find_maxmin (imxnew,jmxnew,dell,'vmag'
     &   ,vmag,'min',ist,uvgeslon,uvgeslat,rlonnew,rlatnew,lbi,cflag
     &   ,ctlon,ctlat,xval,ifmret)
      deallocate (vmag); deallocate (lbi)
      deallocate (rlonnew); deallocate (rlatnew)
c
      if (ifmret == 0) then
        goto 995
      else
        igucret = ifmret
        print *,' '
        print *,'!!! ERROR in get_uv_center in call to find_maxmin'
        print *,'!!! storm num = ',ist,' igucret = ',igucret
        goto 998
      endif
c
  970 print *,' '
      print *,'!!! ERROR ALLOCATING either uold, vold,'
      print *,'!!! rlonold or rlatold in get_uv_center'
      print *,'!!! Storm number = ',ist
      print *,'!!! intnum= ',intnum
      print *,'!!! imxnew= ',imxnew,' jmxnew= ',jmxnew
      print *,'!!! imxold= ',imxold,' jmxold= ',jmxold
      print *,'!!! iuo= ',iuo,' ivo= ',ivo
      print *,'!!! iloo= ',iloo,' ilao= ',ilao
      igucret = 97
      goto 998
c
  971 print *,' '
      print *,'!!! ERROR ALLOCATING either unew, vnew,'
      print *,'!!! rlonnew or rlatnew in get_uv_center'
      print *,'!!! Storm number = ',ist
      print *,'!!! intnum= ',intnum
      print *,'!!! imxnew= ',imxnew,' jmxnew= ',jmxnew
      print *,'!!! imxold= ',imxold,' jmxold= ',jmxold
      print *,'!!! iuo= ',iuo,' ivo= ',ivo
      print *,'!!! iloo= ',iloo,' ilao= ',ilao
      igucret = 97
      goto 998
c
  972 print *,' '
      print *,'!!! ERROR ALLOCATING either vmag or lbi in '
      print *,'!!! subroutine get_uv_center'
      print *,'!!! Storm number = ',ist
      print *,'!!! imxnew= ',imxnew,' jmxnew= ',jmxnew
      print *,'!!! ivm= ',ivm,' ilb= ',ilb
      igucret = 97
      goto 998
c
  975 print *,' '
      print *,'!!! Inside get_uv_center, at least one of the points'
      print *,'!!! is not a valid data point.  This point may be '
      print *,'!!! outside the valid data bounds of a regional grid'
      print *,'!!! i= ',i,' j= ',j
      print *,'!!! Storm number = ',ist
      igucret = 98
      goto 998
c
  995 continue
      igucret = 0 
c
  998 continue
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_uv_guess (guesslon,guesslat,clon,clat
     &                       ,calcparm,ist,ifh,maxstorm
     &                       ,uvgeslon,uvgeslat,igugret)
c
c     ABSTRACT: The purpose of this subroutine is to get a modified 
c               first guess lat/lon position before searching for the 
c               minimum in the wind field.  The reason for doing this is
c               to better refine the guess and avoid picking up a wind
c               wind minimum far away from the center.  So, use the 
c               first guess position (and give it strong weighting), and
c               then also use the  fix positions for the current time
c               (give the vorticity centers stronger weighting as well),
c               and then take the average of these positions.
c
c     INPUT:
c     guesslon  guess longitude for this forecast time 
c     guesslat  guess latitude for this forecast time 
c     clon      array with center longitude  fixes for the various parms
c     clat      array with center latitude  fixes for the various parms
c     calcparm  logical; tells whether or not a parm has a valid fix
c                   at this forecast hour
c     ist       index for current storm
c     ifh       index for current forecast hour
c     maxstorm  max # of storms that can be handled
c
c     OUTPUT:
c     uvgeslon  contains modified guess longitude position at which to
c                   look for the wind minimum
c     uvgeslat  contains modified guess latitude position at which to
c                   look for the wind minimum
c     igugret   return code for this subroutine (0=normal)
c----
c
      USE set_max_parms; USE level_parms; USE error_parms
c
      logical(1) calcparm(maxtp,maxstorm)
      real      clon(maxstorm,maxtime,maxtp)
      real      clat(maxstorm,maxtime,maxtp)
      real      uvgeslon, uvgeslat
      real      guesslon,guesslat
c
      sumlon = 0.0
      sumlat = 0.0
      ict = 0
c
c     Weight the uv guess position by counting the storm's guess 
c     position twice
c
      sumlon = sumlon + 2.*guesslon
      sumlat = sumlat + 2.*guesslat
      ict = ict + 2
c
      do ip = 1,maxtp
        if (ip > 2 .and. ip < 7) then
          cycle   ! because 3-6 are for u & v
        else
          if (calcparm(ip,ist)) then
            call calcdist (guesslon,guesslat,clon(ist,ifh,ip)
     &                    ,clat(ist,ifh,ip),dist)
 
            if (dist < uverrmax) then
c
c             Give the vorticity centers 2x weighting as well
c 
              if (ip == 1 .or. ip == 2) then
                sumlon = sumlon + 2.*clon(ist,ifh,ip)
                sumlat = sumlat + 2.*clat(ist,ifh,ip)
                ict = ict + 2
              else
                sumlon = sumlon + clon(ist,ifh,ip)
                sumlat = sumlat + clat(ist,ifh,ip)
                ict = ict + 1
              endif

            endif

          endif
        endif
      enddo
c 
      if (ict > 0) then
        uvgeslon = sumlon / ict
        uvgeslat = sumlat / ict
        igugret = 0
      else
        print *,' '
        print *,'!!! ERROR in get_uv_guess, ict not > 0, ict= ',ict
        print *,'!!! vmag center will not be calculated for this storm'
        print *,'!!! -- at least not at this level'
        print *,'!!! Storm number = ',ist
        igugret = 91
      endif
c
      return
      end      
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine calc_vmag (xu,xv,imx,jmx,wspeed,icvret)
c
c     ABSTRACT: This subroutine calculates the magnitude of the wind
c     speed for an array of points, given real u and real v arrays.
c
      real    xu(imx,jmx),xv(imx,jmx),wspeed(imx,jmx)
c
      do i=1,imx
        do j=1,jmx
          wspeed(i,j) = sqrt( xu(i,j)*xu(i,j) + xv(i,j)*xv(i,j) )
        enddo
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine bilin_int (imxold,jmxold,xold
     &                     ,imxnew,jmxnew,xnew,ibiret)
c
c     ABSTRACT: This subroutine does a bilinear interpolation on a 
c     grid of evenly spaced data.  Do NOT attempt to use this subroutine
c     with data that are not evenly spaced or you will get unpredictable
c     results.
c
      real      xold(imxold,jmxold), xnew(imxnew,jmxnew)
c
c
c  ---------------------------------------------------------------------
c         Latitude ---->          |
c                                 |
c  L   O  e  O  e  O  e  O  e  O  | O: original point from input array
c  o                              | 
c  n   e  1  2  1  2  1  2  1  e  | 1: interpolated, primary inter. pt
c  g                              |
c  i   O  2  O  2  O  2  O  2  O  | e: interpolated edge point
c  t                              |
c  u   e  1  2  1  2  1  2  1  e  | 2: interpolated, secondary inter. pt
c  d                              |
c  e   O  2  O  2  O  2  O  2  O  | Interpolations are done in the order
c                                 | as indicated above; First, the input
c  |   e  1  2  1  2  1  2  1  e  | 'O' pts are placed onto the new, 
c  |                              | larger grid. From that, the '1' pts
c  |   O  2  O  2  O  2  O  2  O  | can be interpolated.  Next, the edge
c  |                              | (e) pts are interpolated using an
c  v   e  1  2  1  2  1  2  1  e  | interpolation of two 'O' pts and one
c                                 | '1' pt.  Finally, the '2' pts are
c      O  e  O  e  O  e  O  e  O  | done using the 2 surrounding '0' and
c                                 | '1' pts.  Bilinear interpolation is
c                                 | made incredibly easier by the fact
c                                 | that the grid is evenly spaced.
c  ---------------------------------------------------------------------
c     NOTE: Remember that the arrays that are read in are indexed as
c     (lon,lat), so that in the diagram above, pt (1,1) is at the upper
c     left and pt (imax,jmax) is at the lower right, and each column is
c     a new latitude and each row is a new longitude.
c
c     -----------------------------------------------------------------
c     Put original (O) values from input array into new, expanded array
c     -----------------------------------------------------------------
c
      do i=1,imxold
        do j=1,jmxold
          xnew(2*i-1,2*j-1) = xold(i,j) 
        enddo
      enddo
c
c     ----------------------------------------------
c     Interpolate to get primary interior (1) points
c     ----------------------------------------------
c
      do i=1,imxold-1
        do j=1,jmxold-1
          xnew(2*i,2*j) = 0.25 * (xnew(2*i-1,2*j-1) + xnew(2*i+1,2*j-1)
     &                        +  xnew(2*i+1,2*j+1) + xnew(2*i-1,2*j+1))
        enddo
      enddo
c
c     ---------------------------
c     Interpolate edge (e) points
c     ---------------------------
c
c     ... Northernmost 'e' points ...
c
      j=1
      do i=1,imxold-1
        xnew(2*i,j) = 0.3333 * (xnew(2*i-1,j) + xnew(2*i+1,j) 
     &                                        + xnew(2*i,2))
      enddo
c
c     ... Southernmost 'e' points ...
c
      j = 2*jmxold - 1
      do i=1,imxold-1
        xnew(2*i,j) = 0.3333 * (xnew(2*i-1,j) + xnew(2*i+1,j)
     &                                        + xnew(2*i,j-1))
      enddo
c
c     ... Westernmost 'e' points ...
c
      i=1
      do j=1,jmxold-1
        xnew(i,2*j) = 0.3333 * (xnew(i,2*j-1) + xnew(i,2*j+1)
     &                                        + xnew(2,2*j))
      enddo
c
c     ... Easternmost 'e' points ...
c
      i = 2*imxold - 1
      do j=1,jmxold-1
        xnew(i,2*j) = 0.3333 * (xnew(i,2*j-1) + xnew(i,2*j+1)
     &                                        + xnew(i-1,2*j))
      enddo
c
c     ------------------------------------------------
c     Interpolate to get secondary interior (2) points
c     ------------------------------------------------
c
      do j=2,2*jmxold-2
        istep = mod(j+1,2)
        do i=istep+2,2*imxold-2,2
          xnew(i,j) = 0.25 * (xnew(i-1,j) + xnew(i,j-1) + xnew(i+1,j)
     &                     +  xnew(i,j+1))
        enddo
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine lin_int (ioldmax,inewmax,xold,xnew,iliret)
c
c     ABSTRACT: This subroutine linearly interpolates evenly spaced
c               data from one grid to another.
c 
      real      xold(ioldmax), xnew(inewmax)
c
c     First just copy points from old grid onto new, larger grid
c
      do i=1,ioldmax
        xnew(2*i-1) = xold(i)
      enddo
c
c     Now interpolate to get the in-between points
c
      do i=1,ioldmax-1
        xnew(2*i) = 0.5 * (xnew(2*i-1) + xnew(2*i+1))
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine find_maxmin (imax,jmax,grdspc,cparm,fxy,maxmin,ist
     &             ,guesslon,guesslat,rlonv,rlatv,valid_pt,compflag
     &             ,ctlon,ctlat,xval,ifmret)
c
c     This routine  finds the location (clon,clat) of and value of the
c     the max or min of fxy in the vicinity of slon,slat.  The value of
c     the input flag maxmin determines whether to look for a max or a
c     min value.  The max/min is determined by finding the point which 
c     gives the max/min value of a single point barnes analysis of fxy 
c     with e-folding radius re (km) and influence radius ri (km). The 
c     initial search is restricted to a radius rads around the point 
c     (slon,slat) on a grid with lon,lat spacing grdspc. The location is
c     refined by reducing the spacing of the search grid by a factor of
c     two, nhalf times.
c
c     INPUT:
c     imax     Num pts in i direction on input grid
c     jmax     Num pts in j direction on input grid
c     grdspc   Grid spacing on input grid
c     cparm    Char string indicating what parm is being passed in
c     fxy      Real array of data values
c     maxmin   Char string indicating whether to search for a max or min
c     ist      Number of the storm being processed
c     guesslon Guess longitude of the storm
c     guesslat Guess latitude of the storm
c     rlonv    Array containing longitude values of input grid points
c     rlatv    Array containing latitude values of input grid points
c     valid_pt Logical bitmap masking non-valid grid points.  This is a
c              concern for the regional models, which are interpolated 
c              from Lam-Conf or NPS grids onto lat/lon grids, leaving 
c              grid points around the edges which have no valid data.
c
c     INPUT/OUTPUT:
c     compflag Logical; continue processing this storm or not (would be
c              set to FALSE if, for example, the guess position is 
c              outside the domain of a regional grid)
c
c     OUTPUT:
c     ctlon    Center longitude of storm found for this parameter
c     ctlat    Center latitude of storm found for this parameter
c     xval     Max or Min value found at the (ctlon,ctlat)
c     ifmret   Return code from this subroutine
c
      USE radii; USE grid_bounds; USE set_max_parms; USE level_parms
      USE trig_vals
c
      real    fxy(imax,jmax),rlonv(imax),rlatv(jmax)
      real    ctlon,ctlat
c
      character(*)  maxmin,cparm
      logical(1)    compflag, valid_pt(imax,jmax)
      integer       igetgds(200)
c 
c
      ifmret = 0
c
c     -----------------------------------------------------------
c     Set initial parms for use in find_maxmin.
c     Different radii used for V magntitude than for other parms, 
c     see discussion in module radii for more details.
c
      if (cparm == 'vmag') then
        rads = rads_vmag; re = retrk_vmag; ri = ritrk_vmag
        re = (float(maxvgrid)/4) * (grdspc * dtk) ! Basically, this
c               sets re equal to half the distance from the gridpoint
c               in question to the farthest point that will be
c               sampled.  Thus, just ignore the parameter retrk_vmag,
c               and use this instead.
      else if (grdspc < 1.26) then
        rads = rads_most; re = retrk_most; ri = ritrk_most
      else 
        rads = rads_coarse; re = retrk_coarse; ri = ritrk_coarse
      endif

      print *,' '
      print *,'At beg of find_maxmin, rads= ',rads,' re= ',re 
     &       ,' ri= ',ri,' cparm= ',cparm,' grdspc= ',grdspc

      dell = grdspc
      npts = rads/(dtk*dell)
      fmax  = -1.0e+15; fmin  =  1.0e+15
      ctlon = 0.0; ctlat = 0.0

      if (npts == 0) npts = 1

c     If input parm is vmag, we've already done the minimizing by 
c     interpolating to the fine mesh grid, so we'll simply send the 
c     bounds that were input to this subroutine to barnes
c     as boundaries for the array to search.  For all other parms, 
c     however, no minimizing has been done yet, so we need to call 
c     get_ij_bounds to set the boundaries for a much smaller grid that
c     surrounds the storm (as opposed to having subroutine  barnes 
c     search the entire global grid).
 
      if (cparm == 'vmag') then

        ibeg=1; jbeg=1; iend=imax; jend=jmax
        vmag_latmax = rlatv(1)    ! N-most lat of vmag subgrid
        vmag_latmin = rlatv(jmax) ! S-most lat of vmag subgrid
        vmag_lonmin = rlonv(1)    ! W-most lon of vmag subgrid
        vmag_lonmax = rlonv(imax) ! E-most lon of vmag subgrid

        write (6,44) vmag_latmax,vmag_lonmin,360.-vmag_lonmin,imax,jmax
        write (6,46) vmag_latmin,vmag_lonmax,360.-vmag_lonmax
 44     format (' vmag_latmax= ',f8.3,' vmag_lonmin= ',f8.3
     &         ,'E  (',f8.3,'W)  imax= ',i4,' jmax= ',i4)
 46     format (' vmag_latmin= ',f8.3,' vmag_lonmax= ',f8.3
     &         ,'E  (',f8.3,'W)')

        npts = ceiling(rads/(dtk*dell))

      else

        call get_ij_bounds (npts,0,ri,imax,jmax,grdspc
     &             ,glatmax,glatmin,glonmax,glonmin,guesslon,guesslat
     &             ,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)

        if (igiret /= 0) then
          print *,' '
          print *,'!!! ERROR in find_maxmin from call to get_ij_bounds,'
          print *,'!!! stopping processing for storm number ',ist
          ifmret = 92
          return
        endif

      endif

c
c     ---------------------------------------------------------------
c
      print *,' '
      write (6,39) guesslon,360.-guesslon,guesslat
  39  format (' guesslon= ',f8.3,'E  (',f8.3,'W)   guesslat= ',f8.3)
      print *,'ilonfix= ',ilonfix,' jlatfix= ',jlatfix
     &       ,' npts= ',npts
      if (cparm == 'vmag') then
        print *,'ilonfix and jlatfix are meaningless for computing'
        print *,'vmag, so ignore the large values you see for them.'
      endif
      print *,'ibeg= ',ibeg,' jbeg= ',jbeg,' imax= ',imax
      print *,'iend= ',iend,' jend= ',jend,' jmax= ',jmax
c
      ibct=0
      ibarnes_loopct = 0

      jix = 0

      jloop: do j=-npts,npts

        jix = jix + 1
        rlatt = guesslat + dell*float(j)

        iix = 0

c        vlat(jix) = rlatt

        iloop: do i=-npts,npts

          iix = iix + 1
          rlont = guesslon + dell*float(i)

c          print '(a12,i2,a4,i2,2(a8,f8.3))','in find_max, i= ',i,' j= '
c     &           ,j,' rlatt= ',rlatt,' rlont= ',rlont
c
c         If any points in the search grid would extend beyond the grid
c         boundaries, then just set the input calcparm to FALSE and 
c         return to calling subroutine without trying to get max/min
c
          if (rlatt > glatmax .or. rlatt < glatmin .or.
     &        rlont > glonmax .or. rlont < glonmin) then
            print *,' '
            print *,'!!! Lat/Lon value outside grid boundary in'
            print *,'!!! subroutine  find_maxmin'
            print *,'!!! rlatt= ',rlatt,' rlont= ',rlont
            print *,'!!! guesslat= ',guesslat,' guesslon= ',guesslon
            print *,'!!! Storm number = ',ist
            print *,'!!! Processing will NOT continue for this storm'
            compflag = .FALSE.
            ifmret = 94
            return
          endif
c
          call calcdist(rlont,rlatt,guesslon,guesslat,dist)
          if (dist .gt. rads) cycle iloop


          if (cparm == 'vmag') then

c           This next bit of code gets the ij coordinates for an 8x8 
c           box around the current point under consideration. These ij
c           coordinates are sent to barnes so that barnes only loops 
c           64 times, as opposed to nearly 10,000 if the whole 97x97
c           array were sent.  So, fix rlatt to the grid point just 
c           northward of rlatt and fix rlont to the grid point just 
c           eastward of rlont.  Note that this makes for a modified 
c           barnes analysis in that we're  sort of specifying ahead of
c           time exactly which grid points will be included and we'll
c           be excluding some points that would be near the periphery
c           of each (rlont,rlatt)'s range, but as long as we're consis-
c           tent and do it this way for each point, it's well worth the
c           trade-off in cpu time.  Parameter maxvgrid determines what 
c           size array to send to barnes (maxvgrid=8 means 8x8)

            jvlatfix = int((vmag_latmax - rlatt)/grdspc + 1.)
            ivlonfix = int((rlont - vmag_lonmin)/grdspc + 2.)

            ibeg = ivlonfix - (maxvgrid/2)
            iend = ivlonfix + (maxvgrid/2 - 1)
            jbeg = jvlatfix - (maxvgrid/2 - 1)
            jend = jvlatfix + (maxvgrid/2)

            if (ibeg < 1 .or. jbeg < 1 .or. 
     &          iend > imax .or. jend > jmax) then
              print *,' '
              print *,'!!! Gridpoint in vmag subgrid would be outside'
              print *,'!!! the boundaries in subroutine find_maxmin.'
              print *,'!!! rlatt= ',rlatt,' rlont= ',rlont
              print *,'!!! guesslat= ',guesslat,' guesslon= ',guesslon
              print *,'!!! Storm number = ',ist,' maxvgrid= ',maxvgrid
              print *,'!!! ibeg= ',ibeg,' iend= ',iend,' imax= ',imax
              print *,'!!! jbeg= ',jbeg,' jend= ',jend,' jmax= ',jmax
              compflag = .FALSE.
              ifmret = 94
              return
            endif

          endif

          ibct = ibct + 1
          call barnes(rlont,rlatt,rlonv,rlatv,imax,jmax,ibeg,jbeg
     &          ,iend,jend,fxy,valid_pt,re,ri,ftemp,icount,cparm,iret)

          ibarnes_loopct = ibarnes_loopct + icount

          if (iret /= 0) then
            print *,' '
            print *,'!!! Non-zero RCC from barnes...Exiting find_maxmin'
            compflag = .FALSE.
            ifmret = iret
            return
          endif
c
          if (maxmin == 'max') then
            if (ftemp > fmax) then
              fmax = ftemp
              ctlon = rlont
              ctlat = rlatt
            endif
          else
            if (ftemp < fmin) then
              fmin = ftemp
              ctlon = rlont
              ctlat = rlatt
            endif
          endif

        enddo iloop
      enddo jloop
 
      print *,' '
      print *,'After 1st findmax loop, # calls to barnes = ',ibct
      print *,'Total # of barnes loop iterations = ',ibarnes_loopct
c
  55  format ('i= ',i3,' j= ',i3,'  rln= ',f7.3,'  rlt= ',f7.3
     &       ,'  barnval= ',f11.5)
  56  format ('k= ',i3,' i= ',i3,' j= ',i3,'  rln= ',f7.3,'  rlt= '
     &       ,f7.3,'  barnval= ',f11.5)
c
      if (cparm == 'zeta') then
        print *,' !!! Zeta check, fmax= ',fmax,' fmin= ',fmin
        write (6,61) 360.-ctlon,ctlat,fmax*100000.,fmin*100000.
      else
        write (6,63) 360.-ctlon,ctlat,fmin
      endif
  61  format (' After first run, ctlon= ',f8.3,'W  ctlat= ',f8.3
     &       ,' fmax (x10e5) = ',f10.3,' fmin (x10e5) = ',f10.3)
  63  format (' After first run, ctlon= ',f8.3,'W  ctlat= ',f8.3
     &       ,' fmin = ',f10.3)
 111  format (i2,'  rlont= ',f7.2,'W   rlatt= ',f7.2,'  zeta= ',f13.8)
c
c     Since through interpolation the grid for vmag has already been
c     minimized considerably, we don't need to go through the 2nd part
c     of this subroutine, which halves the grid spacing.
c
      if (nhalf < 1 .or. cparm == 'vmag') then
        if (maxmin == 'max') then
          xval = fmax
        else
          xval = fmin
        endif
        return
      endif
c
c     ---------------------------------------------------------------
c     ---------------------------------------------------------------
c     Halve the grid spacing to refine the location and value of the
c     max/min value, but restrict the area of the new search grid.
c
      npts = 3
c
c     -------------------------------------------------------------
c     First, recalculate the i and j beginning and ending points to
c     be used in the  barnes analysis subroutine.  Only
c     do this once for this grid-refinement (even though the grid is
c     redefined 3 times in this subroutine), but make sure to have the
c     possible search grid be big enough to allow the possibility of
c     the grid shifting way right or way left each time through the
c     loop (get_ij_bounds takes care of this).
c

      call get_ij_bounds (npts,nhalf,ri,imax,jmax,grdspc
     &              ,glatmax,glatmin,glonmax,glonmin,ctlon,ctlat
     &              ,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)

      if (igiret /= 0) then
        print *,' '
        print *,'!!! ERROR in find_maxmin from call to get_ij_bounds'
        print *,'!!! just before nhalf loop.  Stopping processing'
        print *,'!!! for storm number ',ist
        ifmret = 92
        return
      endif
c
c     --------------------------------------------------------------
c     Now do the actual searching for the max/min value 
c

      print *,' '
      if (grdspc <= 1.25 .and. ri >= 300 .and. re >= 150) then
        retmp = re
        ritmp = ri
        re = re * 0.5
        ri = ri * 0.5
        print *,'After first pass through barnes, re has been reduced'
        print *,'from ',retmp,' to ',re,', and ri has been reduced '
        print *,'from ',ritmp,' to ',ri
      else
        print *,'After first pass through barnes, re and ri have NOT '
        print *,'been changed.  re = ',re,' ri = ',ri
      endif

      ibct=0
      ibarnes_loopct = 0
      do k=1,nhalf

        dell = 0.5*dell
        tlon = ctlon
        tlat = ctlat
        fmax = -1.0e+15; fmin = 1.0e+15

        print *,' '
        print *,'find_maxmin nhalf loop, cparm= ',cparm,' k= ',k
        write (6,161) tlon,360.-tlon,tlat
        print *,'ilonfix= ',ilonfix,' jlatfix= ',jlatfix
     &         ,' npts= ',npts
        print *,'ibeg= ',ibeg,' jbeg= ',jbeg,' imax= ',imax
        print *,'iend= ',iend,' jend= ',jend,' jmax= ',jmax

        do j=-npts,npts

          rlatt = tlat + dell*float(j)

          do i=-npts,npts

            rlont = tlon + dell*float(i)
c
            if (rlatt > glatmax .or. rlatt < glatmin .or.
     &          rlont > glonmax .or. rlont < glonmin) then
              print *,' '
              print *,'!!! Lat/Lon value outside grid boundary in'
              print *,'!!! subr find_maxmin, in the nhalf loop.'
              print *,'!!! In nhalf loop, k currently = ',k
              print *,'!!! rlatt= ',rlatt,' rlont= ',rlont
              print *,'!!! tlat= ',tlat,' tlon= ',tlon
              print *,'!!! Storm number = ',ist
              compflag = .FALSE.
              ifmret = 94
              return
            endif

            ibct = ibct + 1
            call barnes(rlont,rlatt,glon,glat,imax,jmax,ibeg,jbeg
     &         ,iend,jend,fxy,valid_pt,re,ri,ftemp,icount,cparm,iret)

            ibarnes_loopct = ibarnes_loopct + icount

            if (iret /= 0) then
              print *,' '
              print *,'!!! Non-zero RCC from barnes, k= ',k
              print *,'!!! Exiting find_maxmin'
              compflag = .FALSE.
              ifmret = iret
              return
            endif

            if (maxmin == 'max') then
              if (ftemp > fmax) then
                fmax = ftemp
                ctlon = rlont
                ctlat = rlatt
              endif
            else
              if (ftemp < fmin) then
                fmin = ftemp
                ctlon = rlont
                ctlat = rlatt
              endif
            endif

          enddo
        enddo

        if (cparm == 'zeta') then
          write (6,71) k,360.-ctlon,ctlat,fmax*100000.,fmin*100000.
        else
          write (6,73) k,360.-ctlon,ctlat,fmin
        endif

      enddo

  71  format (' nhalf findmax, k= ',i2,' ctlon= ',f8.3,'W  ctlat= '
     &       ,f8.3,' fmax (x10e5) = ',f10.3,' fmin (x10e5) = ',f10.3)
  73  format (' nhalf findmax, k= ',i2,' ctlon= ',f8.3,'W  ctlat= '
     &       ,f8.3,' fmin = ',f10.3)

 161  format (' guesslon= ',f8.3,'E  (',f8.3,'W)   guesslat= ',f8.3)
 
      print *,' '
      print *,'ppp after 2nd findmax loop, # calls to barnes =  ',ibct
      print *,'ppp Total # of barnes loop iterations = ',ibarnes_loopct
 
      if (maxmin == 'max') then
        xval = fmax
      else
        xval = fmin
      endif
c
      return
      end
c
c----------------------------------------------------------------------
c
c----------------------------------------------------------------------
      subroutine barnes(flon,flat,rlon,rlat,iimax,jjmax,iibeg,jjbeg
     &        ,iiend,jjend,fxy,defined_pt,re,ri,favg,icount,cparm,iret)
c
c     ABSTRACT: This routine performs a single-pass barnes anaylsis
c     of fxy at the point (flon,flat). The e-folding radius (km)
c     and influence radius (km) are re and ri, respectively.
c
c     NOTE:  The input grid that is searched in this subroutine is most
c     likely NOT the model's full, original grid.  Instead, a smaller
c     subgrid of the original grid is searched.  The upper left and 
c     lower right grid point indices are passed into this subroutine 
c     (iibeg, jjbeg, iiend, jjend) for this subgrid.  These indices are
c     determined in the subroutine  get_ij_bounds, and the purpose of 
c     doing it this way is to limit the number of points for which the
c     subroutine has to calculate distances (for a global 1 deg grid,
c     the number of loop iterations is reduced from 65160 to somewhere
c     around 600).
c
c     NOTE: This subroutine will immediately exit with a non-zero
c     return code if it tries to access a grid point that does not have
c     valid data.  This would happen in the case of a regional grid, if
c     you try to access a point near the edge of the grid (remember that
c     because of the interpolation for the regional grids, there will be
c     areas around the edges that have no valid data).
c
c     INPUT:
c     flon    Lon value for center point about which barnes anl is done
c     flat    Lat value for center point about which barnes anl is done
c     rlon    Array of lon values for each grid point
c     rlat    Array of lat values for each grid point
c     iimax   Max number of pts in x-direction on input grid
c     jjmax   Max number of pts in y-direction on input grid
c     iibeg   i index for grid point to start barnes anlysis (upp left)
c     jjbeg   j index for grid point to start barnes anlysis (upp left)
c     iiend   i index for last grid point in barnes anlysis (low right)
c     jjend   j index for last grid point in barnes anlysis (low right)
c     fxy     Real array of data on which to perform barnes analysis
c     defined_pt Logical; bitmap array used for regional grids
c     re      input e-folding radius for barnes analysis
c     ri      input influence radius for searching for min/max
c
c     OUTPUT:
c     favg    Average value about the point (flon,flat)
c     iret    Return code from this subroutine
c
      real      fxy(iimax,jjmax), rlon(iimax), rlat(jjmax)
      logical(1) defined_pt(iimax,jjmax)
      character(*) cparm
c
c     --------------------------
c
      res = re*re
      wts = 0.0
      favg = 0.0

      icount = 0

      do j=jjbeg,jjend
        do i=iibeg,iiend

          icount = icount + 1

          call calcdist(flon,flat,rlon(i),rlat(j),dist)

          if (dist .gt. ri) cycle

          if (defined_pt(i,j)) then
            wt   = exp(-1.0*dist*dist/res)
            wts  = wts + wt
            favg = favg + wt*fxy(i,j)
          else
            print *,' '
            print *,'!!! UNDEFINED PT OUTSIDE OF GRID IN BARNES....'
            print *,'!!! i= ',i,' j= ',j
            print *,'!!! flon= ',flon,' flat= ',flat
            print *,'!!! rlon= ',rlon(i),' rlat= ',rlat(j)
            print *,'!!! re= ',re,' ri= ',ri
            print *,'!!! EXITING BARNES....'
            print *,' '
            iret = 95
            return
          endif
 
        enddo
      enddo
 
      if (wts .gt. 1.0E-5) then
         favg = favg/wts
      else
         favg = 0.0
      endif
      iret = 0
c
      return
      end
c
c----------------------------------------------------------------------
c
c----------------------------------------------------------------------
      subroutine get_ij_bounds (npts,nhalf,ri,imax,jmax,grdspc
     &             ,rglatmax,rglatmin,rglonmax,rglonmin,geslon,geslat
     &             ,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)
c
c     -----------------------------------------------------------
c     ABSTRACT: This subroutine figures out, based on ri, grdspc and
c     the guess latitude and longitude positions, the farthest reaching
c     grid points that are searchable by an analysis subroutine.  The
c     purpose is to return indices for a subgrid that is much smaller 
c     than the original, full grid.  This smaller subgrid can then be 
c     passed to a subsequent analysis or interpolation subroutine, and 
c     work can be done on this smaller array.  This can help save time, 
c     especially in the  barnes analysis subroutine, as work will only 
c     be done on, say, a 20 x 20 array (400 pts) instead of on a 
c     360 x 181 array (65160 pts).  It's crucial, however, to make sure 
c     that the ibeg, jbeg, iend and jend are extended far enough out to 
c     fully encompass any search that would be done.  Below is a 
c     diagram showing the different grids....
c
c Full Global or Regional Model Grid  (Grid F) ----------->
c     ----------------------------------------------------------------
c  |  |                            (ibeg,jbeg)                       |
c  |  | x = ij position that the        |      (Grid R)              |
c  |  |     geslat/geslon is fixed to.  ._______________.            |
c  |  |                                 |               |            |
c  |  | Even though only the points     |    (Grid B)   |            |
c  |  | within Grid B will be checked   |   . . . . k   |            |
c  v  | later on for a max/min (in the  |   . . . . .   |            |
c     | case of a subsequent call to    |   . . x . .   |            |
c     | find_maxmin), the  barnes anal- |   . . . . .   |            |
c     | ysis will include all pts sur-  |   . . . . .   |            |
c     | rounding these Grid B points    |               |            |
c     | that are within a radius of ri. ._______________.            |
c     | So in the case of pt. k, that ri                             |
c     | radius may extend all the way to the Grid R     |            |
c     | boundary, thus we need to include those    (iend,jend)       |
c     | points within our ibeg-jbeg-iend-jend bounds.                |
c     |                                                              |
c     ----------------------------------------------------------------
c
c     Remember that the grids we deal with start north and increase 
c     south, so the northernmost latitude on the input grid will have 
c     a j index of 1.
c
c     INPUT:
c     npts     Num pts from x to edge of max/min search grid (Grid B)
c              (i.e., You define the size of Grid B by the value of
c               npts that you pass into this subroutine).
c     nhalf    Number of times the grid spacing will be halved
c     ri       Radius of influence (for use in barnes analysis)
c     imax     Number of points in x-direction on original grid
c     jmax     Number of points in y-direction on original grid
c     grdspc   Input grid spacing for the original grid
c     rglatmax Value of northern-most latitude on original grid
c     rglatmin Value of southern-most latitude on original grid
c     rglonmax Value of eastern-most longitude on original grid
c     rglonmin Value of western-most longitude on original grid
c     geslat   Value of latitude of guess position of storm
c     geslon   Value of longitude of guess position of storm
c
c     OUTPUT:
c     ilonfix  i index on full, input grid that storm is fixed to
c     jlatfix  j index on full, input grid that storm is fixed to
c     ibeg     i index for top left of sub-array (Grid R) of input grid
c     iend     i index for bot right of sub-array (Grid R) of input grid
c     jbeg     j index for top left of sub-array (Grid R) of input grid
c     jend     j index for bot right of sub-array (Grid R) of input grid
c     igiret   Return code from this subroutine
c
      USE trig_vals
c
      igiret = 0
c
c     --------------------------------------
c     GET BEGINNING AND ENDING J POINTS....
c
c     (1) Calculate number of searchable, max/min pts, that is, the pts 
c         from x to the edge of Grid B.
c     (2) Calculate number of pts beyond the last search point in Grid 
c         B, but are within the bounds of Grid R and thus can be 
c         included in the  barnes analysis.
c     (3) Add (1) and (2) to get the max number of pts to subtract/add
c         to x to get jbeg and jend.
c
c     If nhalf > 0: This occurs in the case of a call from fmax, when
c     the grid spacing is halved nhalf times.  In this case, we have to
c     do extra work to figure out the maximum possible grid point.  For
c     this case:
c       jhlatpts = # of grid pts to last possible search pt (from x to
c                  edge of Grid B in above diagram), plus a cushion.
c       jripts   = # of searchable grid points within radius ri of last
c                  possible search pt (num pts between edge of Grid B
c                  and edge of Grid R in above diagram), plus a cushion
c       jbmaxlatpts = # of pts (in j direction) from x to the edge of
c                     Grid R to include in this subgrid. 
c
c     If nhalf = 0: In this case, the grid spacing will not be reduced,
c     so the number of pts in j direction from x to the edge of Grid
c     B will be the input parameter npts, and just multiply it by 2,
c     and add 2 for a cushion to get jmaxlatpts.  Typically, this sub
c     is called from find_maxmin, and in that sub, the first time that
c     this sub is called, nhalf will = 0.  Then, after a first-shot
c     center is found, the grid spacing will be cut in order to rerun 
c     barnes on a smaller grid, and that's when nhalf will be sent 
c     here as 3.
c
      if (nhalf > 0) then
        rdeg = 0.0
        do i = 1,nhalf
          rdeg = rdeg + float(npts) * (1./(float(i)*2)) * grdspc 
        enddo
        jhlatpts = ceiling(rdeg/grdspc) + 1
        jripts   = ceiling((ri + 1.)/(dtk*grdspc)) + 1
        jbmaxlatpts = jhlatpts + jripts
      else
        jbmaxlatpts = npts * 2 + 2
      endif
c
c
c     Roughly fix geslat to the grid point just poleward of geslat.
c
      if (geslat >= 0.0) then
        jlatfix = int((rglatmax - geslat)/grdspc + 1.)
      else
        jlatfix = ceiling((rglatmax - geslat)/grdspc + 1.)
      endif
      jbeg = jlatfix - jbmaxlatpts
      jend = jlatfix + jbmaxlatpts
      if (jbeg > jmax ) then
        print *,'!!! ERROR in get_ij_bounds, jbeg > jmax'
        print *,'!!! jbeg = ',jbeg,' jmax= ',jmax
        igiret = igiret + 1
        return
      endif
      if (jend < 1) then
        print *,'!!! ERROR in get_ij_bounds, jend < 1, jend = ',jend
        igiret = igiret + 1
        return
      endif
      if (jbeg < 1) jbeg = 1
      if (jend > jmax) jend = jmax

      ! If using a global grid, avoid using the pole points, or else
      ! you'll get a cosfac = 0 and then a divide by zero!!!

      if (jend == jmax .and. rglatmin == -90.0) then
        jend = jmax - 2
      endif
      if (jbeg == 1    .and. rglatmax == 90.0) then
        jbeg = 3
      endif

c     -----------------------------------------
c     NOW GET BEGINNING AND ENDING I POINTS....
c
c     Figure out what the most poleward latitude is that we could have
c     in this smaller search grid, based on jbeg (NH) or jend (SH).

      if (geslat >= 0.0) then
        platmax = rglatmax - ( float(jbeg - 1) * grdspc )
      else
        platmax = rglatmax - ( float(jend - 1) * grdspc )
      endif

      if (platmax > 89.0)  platmax =  89.0
      if (platmax < -89.0) platmax = -89.0

c     Now, using the map factor (cos lat), figure out, based on ri, the
c     max distance beyond the last search point in x-direction (in
c     degrees) that could be searched at this most poleward latitude
c     (i.e., in the diagram above, the max num pts from pt. k eastward
c     to the edge of Grid R).  Calculate how many grid points that is,
c     add 2 to it for a cushion, & add the number of points (npts) 
c     within the defined search grid (Grid B) to get ibmaxlonpts.

      cosfac = cos (platmax * dtr)
      dlon   = ri / (cosfac * dtk)
c
      if (nhalf > 0) then
        ihlonpts    = ceiling(rdeg/grdspc) + 1
        ibmaxlonpts = ihlonpts + ceiling(dlon/grdspc) + 2
      else
        ibmaxlonpts = npts + ceiling(dlon/grdspc) + 2
      endif
c
c     Roughly fix geslon to the grid point just EASTward of geslon.
c
      ilonfix = int((geslon - rglonmin)/grdspc + 2.)
c
      ibeg = ilonfix - ibmaxlonpts
      iend = ilonfix + ibmaxlonpts
      if (ibeg > imax ) then
        print *,'!!! ERROR in get_ij_bounds, ibeg > imax'
        print *,'!!! ibeg = ',ibeg,' imax= ',imax
        igiret = igiret + 1
        return
      endif
      if (iend < 1) then
        print *,'!!! ERROR in get_ij_bounds, iend < 1, iend = ',iend
        igiret = igiret + 1
        return
      endif
      if (ibeg < 1) ibeg = 1
      if (iend > imax) iend = imax 
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine check_bounds (guesslon,guesslat,ist,ifh,icbret)
c
c     ABSTRACT:  This subroutine checks to make sure that the requested
c                storm is in fact within the model's grid boundaries;
c                this is only a concern for the regional models.
c
      USE def_vitals; USE grid_bounds; USE set_max_parms 
c
      if (guesslon > glonmax .or. guesslon < glonmin .or.
     &    guesslat > glatmax .or. guesslat < glatmin) then
        print *,' '
        print *,'!!! IN check_bounds, Storm is outside of grid'
        print *,'!!! Storm ID =   ',storm(ist)%tcv_storm_id
        print *,'!!! Storm Name = ',storm(ist)%tcv_storm_name
        print *,'!!! ist= ',ist,' ifh= ',ifh
        print *,'!!! guess storm lon= ',guesslon
        print *,'!!! guess storm lat= ',guesslat
        icbret = 95
      else
        icbret = 0
      endif
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine calcdist(rlonb,rlatb,rlonc,rlatc,xdist)
c
c     ABSTRACT: This subroutine computes the distance between two 
c               lat/lon points by using spherical coordinates to 
c               calculate the great circle distance between the points.
c                       Figure out the angle (a) between pt.B and pt.C,
c             N. Pole   then figure out how much of a % of a great 
c               x       circle distance that angle represents.
c              / \
c            b/   \     cos(a) = (cos b)(cos c) + (sin b)(sin c)(cos A)
c            /     \                                             
c        pt./<--A-->\c     NOTE: The latitude arguments passed to the
c        B /         \           subr are the actual lat vals, but in
c                     \          the calculation we use 90-lat.
c               a      \                                      
c                       \pt.  NOTE: You may get strange results if you:
c                         C    (1) use positive values for SH lats AND
c                              you try computing distances across the 
c                              equator, or (2) use lon values of 0 to
c                              -180 for WH lons AND you try computing
c                              distances across the 180E meridian.
c    
c     NOTE: In the diagram above, (a) is the angle between pt. B and
c     pt. C (with pt. x as the vertex), and (A) is the difference in
c     longitude (in degrees, absolute value) between pt. B and pt. C.
c
c     !!! NOTE !!! -- THE PARAMETER ecircum IS DEFINED (AS OF THE 
c     ORIGINAL WRITING OF THIS SYSTEM) IN KM, NOT M, SO BE AWARE THAT
c     THE DISTANCE RETURNED FROM THIS SUBROUTINE IS ALSO IN KM.
c
      USE trig_vals
c
      if (rlatb < 0.0 .or. rlatc < 0.0) then
        pole = -90.
      else
        pole = 90.
      endif
c
      distlatb = (pole - rlatb) * dtr
      distlatc = (pole - rlatc) * dtr
      difflon  = abs( (rlonb - rlonc)*dtr )
c
      cosanga = ( cos(distlatb) * cos(distlatc) + 
     &            sin(distlatb) * sin(distlatc) * cos(difflon))
 
c     This next check of cosanga is needed since I have had ACOS crash
c     when calculating the distance between 2 identical points (should
c     = 0), but the input for ACOS was just slightly over 1
c     (e.g., 1.00000000007), due to (I'm guessing) rounding errors.

      if (cosanga > 1.0) then
        cosanga = 1.0
      endif

      degrees    = acos(cosanga) / dtr
      circ_fract = degrees / 360.
      xdist      = circ_fract * ecircum
c
c     NOTE: whether this subroutine returns the value of the distance
c           in km or m depends on the scale of the parameter ecircum. 
c           At the original writing of this subroutine (7/97), ecircum
c           was given in km.
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine subtract_cor (imax,jmax,igetgds,level)
c
c     ABSTRACT: This subroutine subtracts out the coriolis parameter
c     from the vorticity values.  It is needed because at the original
c     writing of this system, all of the forecast centers who included
c     vorticity included only absolute vorticity.
c
      USE tracked_parms; USE trig_vals
      integer igetgds(200)
c
      rmaxlat = float(igetgds(4)) / 1000.0
      grdspc  = float(igetgds(10)) / 1000.0
c
      do j=1,jmax
        rlat = rmaxlat - ((j-1) * grdspc)
        coriolis = 2. * omega * sin(rlat*dtr) 
        do i=1,imax
          zeta(i,j,level) = zeta(i,j,level) - coriolis
        enddo
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine getdata (readflag,valid_pt,imax,jmax,ifhr,inp
     &                   ,kpds,kgds,lb,nquant)
c
c     ABSTRACT: This subroutine reads the input GRIB file for the 
c     tracked parameters.  It then calls subroutines to convert the
c     data from a 1-d array into a 2-d array if the read was successful
c
      USE tracked_parms; USE level_parms; USE inparms; USE set_max_parms
c
      parameter (jf=1440*721,lugb=11,lugi=31) 
      real      f(jf)
      integer   igparm(13),iglev(13),iglevtyp(13)
      integer   jpds(200),jgds(200),kpds(200),kgds(200)
      integer   nquant 
      logical(1) lb(jf),valid_pt(imax,jmax),readflag(13)
      character*1 :: lbrdflag
c
      type (datecard) inp
c
      lbrdflag = 'n'
c
c     The following data statements contain the parameters that will be 
c     used to search the grib files.  The first 9 parameters will all be
c     used to locate the storm position.  The last 4 parameters (500 mb
c     u- and v-components and 10 m u- and v- components) will not be 
c     used for tracking, but only for helping to estimate the next first
c     guess position (500 mb winds) and for estimating the max near-
c     surface wind speeds in the vicinity of the storm (10 m winds).
c
c     ** NOTE: iglevtyp(12 & 13) and iglev(12 & 13) are initialized to
c              0 just to satisfy the IBM xlf compiler, which barks about
c              there being too few initial values in the list when I 
c              only had 11 values there -- even though the real 
c              initialization for these variables is done just about
c              10 lines below.

      data igparm   /41,41,33,34,33,34,7,7,2,33,34,33,34/
      data iglevtyp /100,100,100,100,100,100,100,100,102,100,100,0,0/
      data iglev    /850,700,850,850,700,700,850,700,0,500,500,0,0/
 
c     This next bit is needed because we need to read the near-surface
c     winds, and while several models provide us with 10m winds, the 
c     UKMET gives us surface winds, while nogaps gives us 10m winds.
c     Currently, ECMWF doesn't give us any sfc winds, but throw them
c     in with the first group just so there's something for it to 
c     search for and come up empty for.  For GFDL, we have 35m winds.

      if (inp%model == 1 .or. inp%model == 2 .or. inp%model == 4 .or.
     &    inp%model == 5 .or. inp%model == 6 .or. inp%model == 8 .or.
     &    inp%model == 10) then
        iglevtyp(12) = 105
        iglevtyp(13) = 105
        iglev(12)    = 10
        iglev(13)    = 10
      else if (inp%model == 3) then    ! UKMET: "surface" winds
        iglevtyp(12) = 1
        iglevtyp(13) = 1
        iglev(12)    = 0
        iglev(13)    = 0
      else if (inp%model == 7) then    ! NOGAPS: 10m winds
        iglevtyp(12) = 105
        iglevtyp(13) = 105
        iglev(12)    = 10
        iglev(13)    = 10
      else if (inp%model == 9) then   ! GFDL: 35m winds
        iglevtyp(12) = 105
        iglevtyp(13) = 105
        iglev(12)    = 35
        iglev(13)    = 35
      endif
c
      print *,' '
      print *,'NOTE: Program is now in subroutine  getdata.  A return '
      print *,'code (iret) not equal to zero indicates that subroutine'
      print *,'getgb was unable to find the requested parameter.  This'
      print *,'could be simply because the parm is not included in the'
      print *,'grib file (this is likely for ECMWF data, as they limit'
      print *,'what they send us), or it could indicate a problem with'
      print *,'the grib index file.'

      do ip = 1,13
c
        jpds = -1
        jgds = -1
        j = ifhr/interval_fhr * nquant
        print *, ' Number of quantities = ', j
c
        jpds(5)  = igparm(ip)
        jpds(6)  = iglevtyp(ip)
        jpds(7)  = iglev(ip)
c       jpds(14) = ifhr
C          (8)   - YEAR INCLUDING (CENTURY-1)
C          (9)   - MONTH OF YEAR
C          (10)  - DAY OF MONTH
C          (11)  - HOUR OF DAY
c       jpds(8) = 2000 + inp%byy
c       jpds(9) = inp%bmm
c       jpds(10) = inp%bdd
c       jpds(11) = 12 + ifhr
c       jpds(12) = 0
c       jpds(13) = 1
c
        call getgb (lugb,0,jf,j,jpds,jgds,
     &                        kf,k,kpds,kgds,lb,f,iret) 
CJT/JA: hardcoded scan mode to properly recognize input grid orientation.
        kgds(11) = 64

        print *, 'kpds = ',kpds(8:11)
c
        print *,' '
        print *,'After getgb call, j= ',j,' k= ',k,' ifhr= ',ifhr
     &         ,' parm # (ip) = ',ip,' iret= ',iret
c
        if (iret == 0) then
c
          readflag(ip) = .TRUE.
          call bitmapchk(kf,lb,f,dmin,dmax)
c
          write (6,31)
  31      format (' rec#  parm# levt lev  byy   bmm  bdd  bhh  fhr   np'
     &           ,'ts  minval       maxval')
          print '(i4,2x,8i5,i8,2g12.4)',
     &         k,(kpds(i),i=5,11),kpds(14),kf,dmin,dmax
c
c         Convert logical bitmap to 2-d array (only need to do this 
c         once since using same model for all variables).
c
          if (lbrdflag .eq. 'n') then
            call conv1d2d_logic (imax,jmax,lb,valid_pt,kgds(11))
            lbrdflag = 'y'
          endif
c
          select case (igparm(ip))
            case (41)
              if (iglev(ip) == 850) then
                call conv1d2d_real (imax,jmax,f,zeta(1,1,1),kgds(11))
              else
                call conv1d2d_real (imax,jmax,f,zeta(1,1,2),kgds(11))
              endif
            case (33)
              if (iglev(ip) == 850) then
                call conv1d2d_real (imax,jmax,f,u(1,1,1),kgds(11))
              else if (iglev(ip) == 700) then
                call conv1d2d_real (imax,jmax,f,u(1,1,2),kgds(11))
              else if (iglev(ip) == 500) then
                call conv1d2d_real (imax,jmax,f,u(1,1,3),kgds(11))
              else
                call conv1d2d_real (imax,jmax,f,u(1,1,4),kgds(11))
              endif
            case (34)
              if (iglev(ip) == 850) then
                call conv1d2d_real (imax,jmax,f,v(1,1,1),kgds(11))
              else if (iglev(ip) == 700) then
                call conv1d2d_real (imax,jmax,f,v(1,1,2),kgds(11))
              else if (iglev(ip) == 500) then
                call conv1d2d_real (imax,jmax,f,v(1,1,3),kgds(11))
              else
                call conv1d2d_real (imax,jmax,f,v(1,1,4),kgds(11))
              endif
            case (7)
              if (iglev(ip) == 850) then
                call conv1d2d_real (imax,jmax,f,hgt(1,1,1),kgds(11))
              else
                call conv1d2d_real (imax,jmax,f,hgt(1,1,2),kgds(11))
              endif
            case (2)
              call conv1d2d_real (imax,jmax,f,slp,kgds(11))
            case default
              print *,'!!! ERROR: BAD IGPARM IN GETDATA = ',igparm(ip)
          end select
c
        else
c
          print *,'!!! NOTE: getgb could not find parm # ',igparm(ip)
          print *,'!!!       at level = ',iglev(ip)
          print *,'!!!       Forecast time = ',ifhr
c
        endif
c
      enddo 

c     open(90,file='/tmp/terry/slp.dat',access='direct',
c    &     form='unformatted',status='new',recl=imax*jmax)
c     write(90,rec=1) slp
c     write(90,rec=2) ((u(i,j,4),i=1,imax),j=1,jmax)
c     write(90,rec=3) ((v(i,j,4),i=1,imax),j=1,jmax)
c     close(90)
c     stop 13
 
      return
      end
c
c-------------------------------------------------------------------
c                                                   
c-------------------------------------------------------------------
      subroutine bitmapchk (n,ld,d,dmin,dmax)
c
c     This subroutine checks the bitmap for non-existent data values.
c     Since the data from the regional models have been interpolated
c     from either a polar stereographic or lambert conformal grid
c     onto a lat/lon grid, there will be some gridpoints around the
c     edges of this lat/lon grid that have no data; these grid 
c     points have been bitmapped out by Mark Iredell's interpolater.
c     To provide another means of checking for invalid data points
c     later in the program, set these bitmapped data values to a 
c     value of -999.0.  The min and max of this array are also 
c     returned if a user wants to check for reasonable values.
c
      logical(1) ld
      dimension  ld(n),d(n)
c
      dmin=1.E15
      dmax=-1.E15
c
      do i=1,n
        if (ld(i)) then
          dmin=min(dmin,d(i))
          dmax=max(dmax,d(i))
        else
          d(i) = -999.0
        endif
      enddo
c
      return
      end
c
c------------------------------------------------------------------
c
c------------------------------------------------------------------
      subroutine conv1d2d_logic (imax,jmax,lb1d,lb2d,iscanflag)
c
c     ABSTRACT: This subroutine converts a 1-dimensional input 
c     array of logical data (lb1d) into a 2-dimensional output
c     array (dimension imax,jmax) of logical data (lb2d).
c
c     This subroutine was updated in 6/2000 to add the scanning mode
c     flag (iscanflag) as an input.  This is in order to handle grids
c     that are flipped.  Most grids -- NCEP, UKMET, ECMWF -- have
c     point (1,1) as the uppermost left point on the grid, and the
c     data goes from north to south.  Some grids -- GFDL and the new
c     NOGAPS grid -- are flipped; their point (1,1) is the lowermost
c     left point, and their data goes from south to north.  So if
c     the scanning mode flag indicates northward scanning data
c     (bit 2 in the flag is turned on), we catch it in this
c     subroutine and flip the data ourselves for our own arrays,
c     since this whole program is structured around the data going
c     from north to south.  As of the writing of this, only the
c     first 3 bits of the scanning flag are used, which is why we
c     can use the mod statement in the code below.
c
c     PARAMETERS:
c
c     INPUT:
c     imax     Number of gridpoints in i direction in input box
c     jmax     Number of gridpoints in j direction in input box
c     lb1d     1-d array containing logical bitmap values
c     iscanflag This is kgds(11), an integer value in the GDS,
c              which holds the scanning mode for the data values
c
c     OUTPUT:
c     lb2d     2-d array containing logical bitmap values
c
      logical(1) lb1d(imax*jmax),lb2d(imax,jmax)
c
      if (mod(iscanflag,128) >= 64) then

        ! Input data is south to north; flip the data while
        ! converting to 2-d grid....

        do ilat=1,jmax
          ilatix = jmax - ilat + 1
          do ilon=1,imax
            lb2d(ilon,ilatix) = lb1d(ilon+(ilat-1)*imax)
          enddo
        enddo

      else

        ! Input data is north to south.  Just convert the
        ! data onto a 2-d grid, do not flip it....

        do ilat=1,jmax
          do ilon=1,imax
            lb2d(ilon,ilat) = lb1d(ilon+(ilat-1)*imax)
          enddo
        enddo

      endif
c
      return
      end
c
c------------------------------------------------------------------
c
c------------------------------------------------------------------
      subroutine conv1d2d_real (imax,jmax,dat1d,dat2d,iscanflag)
c
c     ABSTRACT: This subroutine converts a 1-dimensional input 
c     array of real data (dat1d) into a 2-dimensional output
c     array (dimension imax,jmax) of real data (dat2d).
c
c     This subroutine was updated in 6/2000 to add the scanning mode
c     flag (iscanflag) as an input.  This is in order to handle grids
c     that are flipped.  Most grids -- NCEP, UKMET, ECMWF -- have
c     point (1,1) as the uppermost left point on the grid, and the
c     data goes from north to south.  Some grids -- GFDL and the new
c     NOGAPS grid -- are flipped; their point (1,1) is the lowermost
c     left point, and their data goes from south to north.  So if
c     the scanning mode flag indicates northward scanning data
c     (bit 2 in the flag is turned on), we catch it in this
c     subroutine and flip the data ourselves for our own arrays,
c     since this whole program is structured around the data going
c     from north to south.  As of the writing of this, only the
c     first 3 bits of the scanning flag are used, which is why we
c     can use the mod statement in the code below.
c
c     INPUT:
c     imax     Number of gridpoints in i direction in input box
c     jmax     Number of gridpoints in j direction in input box
c     dat1d    1-d real array of data
c     iscanflag This is kgds(11), an integer value in the GDS,
c              which holds the scanning mode for the data values
c
c     OUTPUT:
c     dat2d    2-d real array of data
c
      real    dat1d(imax*jmax),dat2d(imax,jmax)
c
      if (mod(iscanflag,128) >= 64) then

        ! Input data is south to north; flip the data while
        ! converting to 2-d grid....

        do ilat=1,jmax
          ilatix = jmax - ilat + 1
          do ilon=1,imax
            dat2d(ilon,ilatix) = dat1d(ilon+(ilat-1)*imax)
          enddo
        enddo

      else

        ! Input data is north to south.  Just convert the
        ! data onto a 2-d grid, do not flip it....

        do ilat=1,jmax
          do ilon=1,imax
            dat2d(ilon,ilat) = dat1d(ilon+(ilat-1)*imax)
          enddo
        enddo

      endif
c
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine read_nlists (inp,ifhours,trkrinfo,nquant,grdspc)
c
c     ABSTRACT: This subroutine simply reads in the namelists that are
c     created in the shell script.  Namelist datein contains the 
c     starting date information, plus the model identifier.  Namelist
c     stswitch contains the flags for processing for each storm.
c
      USE inparms; USE set_max_parms; USE atcf; USE trkrparms
c
      implicit none
c
      integer itmphrs(maxtime),ifhours(maxtime)
      integer ifh
      integer nquant
      real grdspc
      type (datecard) inp
      type (trackstuff) trkrinfo
c
      namelist/model/nquant,grdspc
      namelist/datein/inp
      namelist/fhlist/itmphrs
      namelist/atcfinfo/atcfnum,atcfname
      namelist/trackerinfo/trkrinfo
c
      read (5,NML=model,END=800)
  800 continue
      read (5,NML=datein,END=801)
  801 continue
      read (5,NML=fhlist,END=805)
  805 continue
      read (5,NML=atcfinfo,END=807)
  807 continue
      read (5,NML=trackerinfo,END=809)
  809 continue

      print*
      print*, ' Model namelist parms: '
      print*, ' # of quantities = ', nquant
      print*, ' grid spacing = ', grdspc
c
      print *,' '
      print *,'After datein namelist in trak.f, namelist parms follow:'
      print *,'Forecast initial year  = byy = ',inp%byy
      print *,'Forecast initial month = bmm = ',inp%bmm
      print *,'Forecast initial day   = bdd = ',inp%bdd
      print *,'Forecast initial hour  = bhh = ',inp%bhh
      print *,'Forecast model identifier = model= ',inp%model
c
      print *,' '
      print *,'Values read in from fhlist (forecast hours) follow. '
      print *,'Values of 99 mean undefined: '
      write (6,87) itmphrs
  87  format ('Forecast hours= ',22i4)
c
      print *,' '
      print *,'Values read in from atcfinfo namelist: '
      write (6,89) atcfnum,atcfname
  89  format ('ATCF ID = ',i2,'  ATCF Name = ',a4)
c
      print *,' '
      print *,'Values read in from trackerinfo namelist follow: '
      write (6,101) ' western boundary  = westbd  = ',trkrinfo%westbd
      write (6,101) ' eastern boundary  = eastbd  = ',trkrinfo%eastbd
      write (6,101) ' northern boundary = northbd = ',trkrinfo%northbd
      write (6,101) ' southern boundary = southbd = ',trkrinfo%southbd
      write (6,102) ' tracker type = ',trkrinfo%type
      write (6,103) ' mslp threshold = mslpthresh = '
     &               ,trkrinfo%mslpthresh
      write (6,103) ' v850 threshold = v850thresh = '
     &               ,trkrinfo%v850thresh
      write (6,104) ' model grid type = ',trkrinfo%gridtype
      write (6,101) ' Contour interval to be used = ',trkrinfo%contint
  101 format (a31,f7.2)
  102 format (a16,a7)
  103 format (a31,f7.4)
  104 format (a19,a8)

      do ifh=1,maxtime
        ifhours(ifh) = itmphrs(ifh)
      enddo
c
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine read_tcv_card (lucard,maxstorm,trkrinfo,numtcv,iret)
c
c     ABSTRACT: This subroutine reads in the updated TC Vitals file
c               for the current time and prints out those cards (storms)
c               that have been selected to be processed.  It also 
c               takes the initial positions from the tcv card for each
c               storm and puts them into the slonfg & slatfg arrays.
c               If this is a mid-latitude or tc genesis case, the 
c               first (and only) vitals record that is read in will have
c               MIDLAT as its name.
c
c     INPUT:    
c     lucard    integer unit number for tcvitals card
c
c     OUTPUT:   
c     maxstorm  max # of storms to be handled for this case
c     numtcv    number of storms read off of the input tcvitals file
c     iret      return code from this subroutine
c
c     OTHER:
c     stormswitch 1,2 or 3 (see more description under Main pgm section)
c     slonfg     first guess array for longitude
c     slatfg     first guess array for latitude
c     storm      contains the tcvitals info
c     (storm, stormswitch, slonfg and slatfg are allocatable and are 
c      defined in module def_vitals)

      USE def_vitals; USE set_max_parms; USE trkrparms

      implicit none

      type (tcvcard) tmpstorm(maxstorm_tc)
      type (trackstuff) trkrinfo
      integer    isa,issa,ioa,iaa,ita,iret,ict,maxstorm
      integer    i,ii,lucard,numtcv
c------
      ii=1
      do while (.true. .and. ii <= maxstorm_tc)
        read (lucard,21,END=801,ERR=891) tmpstorm(ii)
        ii = ii + 1
      enddo
   21 format (a4,1x,a3,1x,a9,1x,i2,i6,1x,i4,1x,i3,a1,1x,i4,a1,1x,i3,1x
     &       ,i3,3(1x,i4),1x,i2,1x,i3,1x,4(i4,1x),a1)

  801 continue

      numtcv = ii - 1

      if (trkrinfo%type == 'midlat' .or. trkrinfo%type == 'tcgen') then
        ! For the mid-latitude or tc genesis cases, the max number
        ! of storms (maxstorm) allowed to be tracked throughout a
        ! forecast is defined in module set_max_parms.
        print *,' '
        print *,'In read_tcv_card, tracker type of "midlat" or "tcgen"'
        print *,'indicates that this run of the  tracker is for a '
        print *,'midlat or a tcgen case....'
        maxstorm = maxstorm_mg
        allocate (stormswitch(maxstorm),stat=isa)
        allocate (storm(maxstorm),stat=issa)
        allocate (slonfg(maxstorm,maxtime),stat=ioa)
        allocate (slatfg(maxstorm,maxtime),stat=iaa)
        allocate (stcvtype(maxstorm),stat=ita)
        if (isa /= 0 .or. ioa /= 0 .or. iaa /= 0 .or. issa /= 0 .or.
     &      ita /= 0) then
          print *,' '
          print *,'!!! ERROR in read_tcv_card allocating stormswitch,'
          print *,'!!! slonfg, storm, slatfg or stcvtype arrays.  '
          print *,'!!! isa = ',isa,' ioa= ',ioa,' iaa= ',iaa,' issa= '
          print *,'!!! ',issa,' ita= ',ita
          iret = 97
          return
        endif
        slonfg = 0.0; slatfg = 0.0
        stcvtype = 'FOF' ! Found On the Fly by tracker (not on tcvitals)
        stormswitch = 3    ! Initialize whole array to case of '3'
        if (numtcv > 0) then
          print *,' '
          print *,'Following are the already-existing storms that were'
          print *,'read in from the tc vitals file: '
          print *,' '       
          ict = 0
          do i=1,numtcv
            stormswitch(i) = 1
            storm(i) = tmpstorm(i)
            ict = ict + 1
            write (*,31) storm(i)
          
            if (storm(i)%tcv_lonew == 'W') then
              slonfg(i,1) =  360. - float(storm(i)%tcv_lon)/10.0
            else
              slonfg(i,1) = float(storm(i)%tcv_lon)/10.0
            endif
            if (storm(i)%tcv_latns == 'S') then
              slatfg(i,1) = -1. * float(storm(i)%tcv_lat)/10.0
            else
              slatfg(i,1) = float(storm(i)%tcv_lat)/10.0
            endif
            stcvtype(i) = 'TCV' ! Storm listed on tcvitals

c            if (trkrinfo%type == 'midlat') then
c              storm(i)%tcv_center = 'MIDL'
c            else if (trkrinfo%type == 'tcgen') then
c              storm(i)%tcv_center = 'TCG '
c            endif
c            write (storm(i)%tcv_storm_id,'(i3.3)') i
c            write (storm(i)%tcv_storm_name,'(i3.3)') i

          enddo
        endif
        return
      else
        ! For the  tracker cases, the max number of storms (maxstorm) 
        ! allowed to be tracked throughout a forecast is defined by 
        ! the number of vitals read in above.
        maxstorm = numtcv
        allocate (stormswitch(maxstorm),stat=isa)
        allocate (storm(maxstorm),stat=issa)
        allocate (slonfg(maxstorm,maxtime),stat=ioa)
        allocate (slatfg(maxstorm,maxtime),stat=iaa)
        allocate (stcvtype(maxstorm),stat=ita)
        if (isa /= 0 .or. ioa /= 0 .or. iaa /= 0 .or. issa /= 0 .or.
     &      ita /= 0) then
          print *,' '
          print *,'!!! ERROR in read_tcv_card allocating stormswitch,'
          print *,'!!! slonfg, storm, slatfg or stcvtype arrays.  '
          print *,'!!! isa = ',isa,' ioa= ',ioa,' iaa= ',iaa,' issa= '
          print *,'!!! ',issa,' ita= ',ita
          iret = 97
          return   
        endif
        print *,' '
        print *,'Following are the storms to be processed: '
        print *,' '
        slonfg = 0.0; slatfg = 0.0
        stcvtype = '   '  ! Not needed for regular tracker run....
        ict=0
        do i=1,maxstorm
          stormswitch(i) = 1
          storm(i) = tmpstorm(i)
          ict = ict + 1
          write (*,31) storm(i)

          if (storm(i)%tcv_lonew == 'W') then
            slonfg(i,1) =  360. - float(storm(i)%tcv_lon)/10.0
          else
            slonfg(i,1) = float(storm(i)%tcv_lon)/10.0
          endif
          if (storm(i)%tcv_latns == 'S') then
            slatfg(i,1) = -1. * float(storm(i)%tcv_lat)/10.0
          else
            slatfg(i,1) = float(storm(i)%tcv_lat)/10.0
          endif
        enddo
 
        if (ict.gt.0) then
          iret = 0
          return
        else
          print *,' '
          print *,'!!! ERROR in read_tcv_card, num storms to be '
          print *,'!!! processed is not greater than 0 for a tracker'
          print *,'!!! case.  Check to see that you have the Fortran'
          print *,'!!! unit assigned right in your script.'
          iret = 99
          return
        endif

      endif

   31 format (a4,1x,a3,1x,a9,1x,i2,i6.6,1x,i4.4,1x,i3,a1,1x,i4,a1,1x
     &       ,i3,1x,i3,3(1x,i4),1x,i2,1x,i3,1x,4(i4,1x),a1)

  891 print *,'!!! ERROR in read_tcv_card reading unit ',lucard
      iret = 98
c
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine getgridinfo (imax,jmax,grdspc,igetpds,igetgds,trkrinfo
     &                       ,iggret)
c
c     ABSTRACT: The purpose of this subroutine is just to get the max
c     values of i and j and the grid interval for the grid to be used 
c     in the rest of the program.  So just read the first record you 
c     come across and return the needed values.  Also, from the gds 
c     for the record that's read, get the information for the data 
c     grid's boundaries.  This boundary information will be used later 
c     in the tracking algorithm, and is accessed via Module grid_bounds.
c
c     NOTE: If the input grib file contains records that may have a 
c     grid that's NOT lat/lon, we want to make sure we read far enough
c     into the file to read one that IS a lat/lon grid.  That's why we
c     set jgds(1) = 0 before reading.
c
      USE grid_bounds; USE trkrparms
c
      type (trackstuff) trkrinfo

      parameter (lugb=11,lugi=31,jf=1440*721)
      logical(1), allocatable :: lb(:)
      real      grdspc,xhold
      real grdint
      real, allocatable    :: f(:)
      integer   jpds(200),jgds(200),igetpds(200),igetgds(200)
      integer   iscanflag
c
      allocate (lb(jf),stat=ila); allocate (f(jf),stat=ifa)
      if (ila /= 0 .or. ifa /= 0) then
        print *,' '
        print *,'!!! ERROR in getgridinfo allocating either lb or f'
        print *,'!!! ila = ',ila,' ifa= ',ifa
        iggret = 97
        return
      endif
c
      jpds = -1
      jgds = -1
c
      jgds(1) = 0   ! Request a record that's on a lat/lon grid
      j=0
      call getgb(lugb,0,jf,j,jpds,jgds,
     &                     kf,k,igetpds,igetgds,lb,f,iret)
CJT/JA: hardcoded scan mode to properly recognize input grid orientation.
      igetgds(11) = 64
c
      if (iret.ne.0) then
        print *,' '
        print *,'!!! ERROR in getgridinfo calling getgb'
        print *,'!!! Return code from getgb = iret = ',iret
        iggret = iret
      else
        iggret=0
        imax = igetgds(2)
        jmax = igetgds(3)
        grdint = float(igetgds(9))/1000.
        print*, ' Using grid spacing from namelist rather than grib'
      endif

      print *,' '
      print *,'In getgridinfo, grid dimensions follow:'
      print *,'imax= ',imax,' jmax= ',jmax

c     ------------------------------------------------------------------
c     Get boundaries of the data grid.  NOTE: gds(4) is referred to in
c     GRIB documenatation as the "Latitude of origin", which might imply
c     "minimum Latitude".  However, for the grids that we'll be using in
c     this program, the "Latitude of origin" will be listed under gds(4)
c     as the northernmost point (eg., in MRF, gds(4) = 90), so for this
c     program, use gds(4) as your max lat, and gds(7) as your min lat.
c     However, in case NCEP, UKMET or ECMWF change their convention and
c     begin flipping their grids, a check is made to make sure that the
c     max lat is not less than the min lat.
c
c     BUGFIX (August, 2001): It is possible to have an input grid which
c     goes from south to north (such as NOGAPS).  In this case, we flip
c     the data in subroutine conv1d2d_real.  However, the max and min
c     latitudes listed in the GRIB GDS will be confused, so we need to 
c     check the value of the GRIB scanning mode flag here.

      iscanflag = igetgds(11)
      if (mod(iscanflag,128) >= 64) then
        ! Input data is south to north...
        glatmin = float(igetgds(4))/1000.
        glatmax = float(igetgds(7))/1000.
      else
        ! Input data is north to south...
        glatmin = float(igetgds(7))/1000.
        glatmax = float(igetgds(4))/1000.
      endif

      glonmin = float(igetgds(5))/1000.
      glonmax = float(igetgds(8))/1000.

c
      if (glonmin < 0.0) glonmin = 360. - abs(glonmin)
      if (glonmax < 0.0) glonmax = 360. - abs(glonmax)
c
      if (glatmax < glatmin) then
        temp    = glatmax
        glatmax = glatmin
        glatmin = temp
      endif
c
      print *,' '
      print *,'Data Grid Lat/Lon boundaries follow:'
      write (6,81) glatmin,glonmin
  81  format (' Min Lat: ',f8.3,'  Min Lon: ',f8.3)
      write (6,83) glatmax,glonmax
  83  format (' Max Lat: ',f8.3,'  Max Lon: ',f8.3)
      print *,' '
      print *,'NOTE: For regional grids, valid data points might NOT'
      print *,'extend all the way to the gds-defined grid boundary, due'
      print *,'to the fact that data have been interpolated from a NPS'
      print *,'or Lamb-Conf grid onto a lat/lon grid.  This program' 
      print *,'checks the logical bitmap for valid data points, but '
      print *,'just keep this in mind if trying to debug errors that'
      print *,'occur near the grid boundaries for regional models.'

c     ----------------------------------------------------------------
c     Fill glat and glon with the lat & lon values for the grid.  This
c     info will be used in subroutine  barnes

      allocate (glat(jmax),stat=ija); allocate (glon(imax),stat=iia)
      if (ija /= 0 .or. iia /= 0) then
        print *,' '
        print *,'!!! ERROR in getgridinfo allocating glon or glat'
        print *,'!!! ija = ',ija,' iia= ',iia
        iggret = 96
        return
      endif
c
      do j=1,jmax
        glat(j) = glatmax - (j-1)*grdspc
      enddo
      do i=1,imax
        glon(i) = glonmin + (i-1)*grdspc
      enddo

c     ---------------------------------------------------------------
c     Finally, check to see if the requested boundary limits that
c     the user input are contained within this grid (for example, 
c     someone running this tracker on the Eta may have forgotten
c     to change the input grid bounds from a GFS run).  Modify 
c     the user-input bounds as needed....

      if (trkrinfo%eastbd > glonmax) then
        xhold = trkrinfo%eastbd
        trkrinfo%eastbd = glonmax - 5.0
        write (6,90)
        write (6,91)
        write (6,92)
        write (6,93)
        write (6,94)
        write (6,95)
        write (6,96)
        write (6,97) 'EASTERN LONGITUDE'
        write (6,98) xhold
        write (6,99) trkrinfo%eastbd
        write (6,91)
      endif

      if (trkrinfo%westbd < glonmin) then
        xhold = trkrinfo%westbd
        trkrinfo%westbd = glonmin + 5.0
        write (6,90)
        write (6,91)
        write (6,92)
        write (6,93)
        write (6,94)
        write (6,95)
        write (6,96)
        write (6,97) 'WESTERN LONGITUDE'
        write (6,98) xhold
        write (6,99) trkrinfo%westbd
        write (6,91)
      endif

      if (trkrinfo%northbd > glatmax) then
        xhold = trkrinfo%northbd
        trkrinfo%northbd = glatmax - 5.0
        write (6,90)            
        write (6,91)
        write (6,92)
        write (6,93)
        write (6,94)
        write (6,95)
        write (6,96)
        write (6,97) 'NORTHERN LATITUDE'
        write (6,98) xhold            
        write (6,99) trkrinfo%northbd
        write (6,91)
      endif

      if (trkrinfo%southbd < glatmin) then
        xhold = trkrinfo%southbd
        trkrinfo%southbd = glatmin + 5.0
        write (6,90)            
        write (6,91)
        write (6,92)
        write (6,93)
        write (6,94)
        write (6,95)
        write (6,96)
        write (6,97) 'SOUTHERN LATITUDE'
        write (6,98) xhold            
        write (6,99) trkrinfo%southbd
        write (6,91)
      endif

  90  format (///)
  91  format (' *********************************************')
  92  format (' WARNING: A USER-REQUESTED BOUNDARY IS BEYOND')
  93  format (' THE BOUNDARY OF THE DATA, AS DEFINED IN THE ')
  94  format (' GRIB FILE.  THE USER BOUNDARY WILL BE MODIFIED')
  95  format (' TO MATCH THE BOUNDARY OF THE DATA FILE.')
  96  format (' ')
  97  format (' USER-INPUT BOUNDARY AT FAULT: ',A20)
  98  format (' USER-INPUT BOUNDARY VALUE: ',f8.2)
  99  format (' NEW BOUNDARY VALUE: ',f8.2)

c
      deallocate (lb); deallocate(f)
c
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine rvcal (imax,jmax,igetgds,z,vp)
c
c     ABSTRACT: This routine calculates the relative vorticity (zeta)
c     from u,v on an evenly-spaced lat/lon grid. Centered finite 
c     differences are used on the interior points and one-sided 
c     differences are used on the boundaries.
c
c     LOCAL VARIABLES:
c
      USE tracked_parms; USE trig_vals; USE grid_bounds
c
      dimension cosfac(jmax),tanfac(jmax)
      real      tmpzeta(imax,jmax)
      integer   z,iscanflag
      integer   igetgds(200)
      logical(1) vp(imax,jmax)

c     --------------------------

c     NOTE: In the GRIB GDS, all longitude and latitude values are 
c     stored as integer * 1000, so need to convert these values.

      dlon = float(igetgds(9)) / 1000.0   ! Di grid increment in degrees
      dlat = float(igetgds(10)) / 1000.0  ! Dj grid increment in degrees

c     Calculate grid increments for interior and edge points.

c     IMPORTANT: If dtk is defined in module trig_vals in km, then
c     we need to multiply by 1000 here to get meters.  If it's defined
c     as meters, just let it be.  Since the wind values are given in 
c     meters, that's why we need the dlon values to be in meters.

      if (dtk < 750.) then     ! chances are, dtk was defined as km
        dfix = 1000.0
      else                     ! dtk was already defined as meters
        dfix = 1.0
      endif

      dlon_edge = dtk * dfix * dlon          ! Di dist over 1 grid pt
      dlat_edge = dtk * dfix * dlat          ! Dj dist over 1 grid pt
      dlon_inter = dtk * dfix * 2.0 * dlon   ! Di dist over 2 grid pts
      dlat_inter = dtk * dfix * 2.0 * dlat   ! Dj dist over 2 grid pts


c     Calculate required trig functions.  These are functions of 
c     latitude.  We need to check the value of the GRIB scanning mode 
c     flag to see if the data goes north to south (NCEP standard) or 
c     south to north (NOGAPS).  In the case of NOGAPS, the data values
c     have already been flipped in subroutine conv1d2d_real, so that 
c     now they go from north to south.  However, in the GRIB GDS 
c     section, the starting latitude is still listed as the southern
c     point and the last latitude as the northern point.  So we need
c     to check for this and make an adjustment, if necessary.

      iscanflag = igetgds(11)
      if (mod(iscanflag,128) >= 64) then
        ! Input data was originally south to north, so the northern
        ! most lat needs to be taken from igetgds(7)
        rmaxlat = float(igetgds(7)) / 1000.0
        print *,'iscanflag in rvcal > 64: !!! SOUTH TO NORTH'
      else
        ! Input data was originally north to south, so the northern
        ! most lat needs to be taken from igetgds(4)
        rmaxlat = float(igetgds(4)) / 1000.0
        print *,'iscanflag in rvcal = 0: +++ north to south'
      endif

      do j=2,jmax-1
         rlat = rmaxlat - ((j-1) * dlat)
         cosfac(j) = cos(dtr*rlat)
         tanfac(j) = (tan(dtr*rlat))/erad
      enddo

c     Set trig factors at end points to closest interior point
c     to avoid a singularity if the domain includes the poles,
c     which it will for the global grids (MRF, GDAS, AVN, UKMET,NCE)

      cosfac(1) = cosfac(2)
      tanfac(1) = tanfac(2)
      cosfac(jmax) = cosfac(jmax-1)
      tanfac(jmax) = tanfac(jmax-1)
c
c     NOTE: These next bits of vorticity calculation code assume that 
c           the input grid is oriented so that point (1,1) is the upper
c           left-most (NW) and point (imax,jmax) is the lower right-
c           most point.  Any other grids will probably crash the 
c           program due to array out of bounds errors.
c     NOTE: Before each calculation is done, the logical array is 
c           checked to make sure that all the data points in this 
c           calculation have valid data (ie., that the points are not
c           outside a regional model's boundaries).
c
c !!! IMPORTANT NOTE: While testing this, I uncovered a bug, which was
c     that I had the "j+1" and "j-1" reversed.  Just from a physical 
c     understanding, the du/dy term at a point is calculated by taking 
c     the u value north of the point minus the u value south of the 
c     point. Intuitively, this is u(j+1) - u(j-1).  However, all of the
c     input GRIB files for this program have the northernmost point as
c     the beginning of the grid (i.e., for the global grids, j=1 at 90N,
c     and j increases southward).  Thus, if you would do u(j+1) -
c     u(j-1), you would actually be taking the u value south of the 
c     point minus the u value north of the point, EXACTLY THE OPPOSITE
c     OF WHAT YOU WANT.  Therefore, the vorticity calculations have
c     been changed so that we now have u(j-1) - u(j+1).
c
c     ---------------
c     Interior points
c     ---------------
    
      print *,'Just before inter rvcalc, dlon_inter = ',dlon_inter
     &       ,' dlat_inter = ',dlat_inter
      do j=2,jmax-1
       do i=2,imax-1
c
        if (vp(i,j) .and. vp(i+1,j) .and. vp(i-1,j) .and. 
     &      vp(i,j+1) .and. vp(i,j-1)) then
c 
         zeta(i,j,z)= (v(i+1,j,z) - v(i-1,j,z))/(dlon_inter * cosfac(j))
     &               - (u(i,j-1,z) - u(i,j+1,z))/(dlat_inter)
     &               + tanfac(j)*u(i,j,z)

        else
         zeta(i,j,z)= -999.
        endif
c
       enddo
      enddo
c
c     -----------------------------
c     Bottom (Southernmost) points
c     -----------------------------
c
      j=jmax
      do i=2,imax-1
c
       if (vp(i,j) .and. vp(i+1,j) .and. vp(i-1,j) .and. 
     &     vp(i,j-1)) then
c
         zeta(i,j,z)= (v(i+1,j,z) - v(i-1,j,z))/(dlon_inter * cosfac(j))
     &              - (u(i,j-1,z) - u(i,j,z))/(dlat_edge) 
     &              + tanfac(j)*u(i,j,z)
       else
         zeta(i,j,z)= -999.
       endif
c
      enddo
c
c     --------------------------
c     Top (Northernmost) points
c     --------------------------
c
      j=1
      do i=2,imax-1
c
       if (vp(i,j) .and. vp(i+1,j) .and. vp(i-1,j) .and.  
     &     vp(i,j+1)) then
c
         zeta(i,j,z)= (v(i+1,j,z) - v(i-1,j,z))/(dlon_inter * cosfac(j))
     &              - (u(i,j,z) - u(i,j+1,z))/(dlat_edge)
     &              + tanfac(j)*u(i,j,z)
       else
         zeta(i,j,z)= -999.
       endif
c
      enddo
c
c     -------------------------------
c     Left edge (Westernmost) points
c     -------------------------------
c
      i=1
      do j=2,jmax-1
c
       if (vp(i,j) .and. vp(i+1,j) .and. vp(i,j+1) .and.  
     &     vp(i,j-1)) then
c
         zeta(i,j,z) = (v(i+1,j,z) - v(i,j,z))/(dlon_edge * cosfac(j))
     &               - (u(i,j-1,z) - u(i,j+1,z))/(dlat_inter)
     &               + tanfac(j)*u(i,j,z)
       else
         zeta(i,j,z)= -999.
       endif
c
      enddo
c
c     --------------------------------
c     Right edge (Easternmost) points
c     --------------------------------
c
      i=imax
      do j=2,jmax-1
c
       if (vp(i,j) .and. vp(i-1,j) .and. vp(i,j+1) .and.  
     &     vp(i,j-1)) then
c
         zeta(i,j,z) = (v(i,j,z) - v(i-1,j,z))/(dlon_edge * cosfac(j))
     &               - (u(i,j-1,z) - u(i,j+1,z))/(dlat_inter)
     &               + tanfac(j)*u(i,j,z)
       else 
         zeta(i,j,z)= -999.
       endif
c
      enddo
c
c     ---------
c     SW corner
c     ---------
      i=1
      j=jmax
      if (vp(i,j) .and. vp(i+1,j) .and. vp(i,j-1) ) then 
c
        zeta(i,j,z) = (v(i+1,j,z)-v(i,j,z))/(dlon_edge * cosfac(j))
     &              - (u(i,j-1,z)-u(i,j,z))/(dlat_edge)
     &              + tanfac(j)*u(i,j,z)
      else
        zeta(i,j,z)= -999.
      endif
c
c     ---------
c     NW corner
c     ---------
      i=1
      j=1
      if (vp(i,j) .and. vp(i+1,j) .and. vp(i,j+1) ) then
c
        zeta(i,j,z) = (v(i+1,j,z) - v(i,j,z))/(dlon_edge * cosfac(j))
     &              - (u(i,j,z) - u(i,j+1,z))/(dlat_edge)
     &              + tanfac(j)*u(i,j,z)
      else
        zeta(i,j,z)= -999.
      endif
c
c     ---------
c     NE corner
c     ---------
      i=imax
      j=1
      if (vp(i,j) .and. vp(i-1,j) .and. vp(i,j+1) ) then
c
        zeta(i,j,z) = (v(i,j,z) - v(i-1,j,z))/(dlon_edge * cosfac(j))
     &              - (u(i,j,z) - u(i,j+1,z))/(dlat_edge)
     &              + tanfac(j)*u(i,j,z)
      else
        zeta(i,j,z)= -999.
      endif
c
c     ---------
c     SE corner
c     ---------
      i=imax
      j=jmax
      if (vp(i,j) .and. vp(i-1,j) .and. vp(i,j-1) ) then
c
        zeta(i,j,z) = (v(i,j,z)-v(i-1,j,z))/(dlon_edge * cosfac(j))
     &              - (u(i,j-1,z)-u(i,j,z))/(dlat_edge)
     &              + tanfac(j)*u(i,j,z)
      else
        zeta(i,j,z)= -999.
      endif
c
      do ii=1,imax
        do jj=1,jmax
          tmpzeta(ii,jj) = zeta(ii,jj,z) * 1.e5
        enddo
      enddo

      return
      end

c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine first_ges_center (imax,jmax,grdspc,cparm,fxy
     &            ,cmaxmin,trkrinfo,ifh,valid_pt,maxstorm,masked_out
     &            ,stormct,contour_info,maxmini,maxminj,ifgcret)
c
c     ABSTRACT: This subroutine scans an array and picks out areas of 
c     max or min, then loads those center positions into the first-
c     guess lat & lon arrays to be used by subroutine  tracker for 
c     locating the very specific low center positions.
c
c     INPUT:
c     imax     Number of gridpoints in i direction in input grid
c     jmax     Number of gridpoints in j direction in input grid
c     grdspc   Grid spacing for the input grid
c     cparm    Char string indicating what parm is being passed in
c     fxy      Real array of data values
c     finf     Logical. Field of influence.  Dimension same as fxy
c     cmaxmin  Char string to indicate if search is for a max or a min
c     trkrinfo Derived type that holds/describes various tracker parms,
c              including the contour interval to be used
c     ifh      Index for the forecast hour
c     valid_pt Logical bitmap masking non-valid grid points.  This is a
c              concern for the regional models, which are interpolated
c              from Lam-Conf or NPS grids onto lat/lon grids, leaving
c              grid points around the edges which have no valid data.
c     maxstorm max # of storms that can be handled in this run
c
c     INPUT/OUTPUT:
c     masked_out Logical. T = data point is already accounted for, under
c                the influence of another nearby max or min center, 
c                F = data point is available to be scanned by this 
c                subroutine for max or min centers.
c     stormct  Integer: keeps and increments a running tab of the number
c              of storms that have been tracked at any time across all
c              forecast hours
c     contour_info Type cint_stuff from module contours.  Contains 
c                  contour information
c
c     OUTPUT:
c     maxmini  Integer array containing i-indeces of max/min locations
c     maxminj  Integer array containing j-indeces of max/min locations
c     ifgcret  return code from this subroutine
c
c     OTHER:
c     storm    Contains the tcvitals for the storms (module def_vitals)

      USE trkrparms; USE grid_bounds; USE set_max_parms; USE def_vitals
      USE contours; USE tracked_parms

      implicit none

      type (trackstuff) trkrinfo
      type (cint_stuff) contour_info

      integer       i,j,n,isstart,ifamret,ibeg,jbeg,iend,jend
      integer       ifh,maxstorm,imax,jmax,itemp,ifgcret
      integer       stormct,oldstormct
      logical(1)    valid_pt(imax,jmax),masked_out(imax,jmax)
      character(*)  cparm,cmaxmin
      integer       maxmini(maxstorm),maxminj(maxstorm)
      real          fxy(imax,jmax)
      real          dmax,dmin,grdspc,dbuffer

c     First check the user-supplied grid boundaries to see if we will 
c     scan the entire array or just a portion of it.

      if (trkrinfo%northbd < -998.0 .or. trkrinfo%southbd < -998.0 .or.
     &    trkrinfo%westbd < -998.0  .or. trkrinfo%eastbd < -998.0) then
        ! User did not specify a subgrid, so scan the whole domain
        ibeg = 1
        iend = imax
        jbeg = 1
        jend = jmax
      else
        if (trkrinfo%westbd > 360.0 .or. trkrinfo%eastbd < 0.0 .or.
     &      trkrinfo%westbd <   0.0 .or.
     &      trkrinfo%northbd > 90.0 .or. trkrinfo%northbd <-90.0 .or.
     &      trkrinfo%southbd > 90.0 .or. trkrinfo%southbd <-90.0 .or.
     &      trkrinfo%westbd  >= trkrinfo%eastbd .or.
     &      trkrinfo%southbd >= trkrinfo%northbd) then
          print *,' '
          print *,'!!! ERROR: Error in first_ges_center.  There is a '
          print *,'!!!        problem with the user-supplied grid '
          print *,'!!!        boundaries.  Please check them and '
          print *,'!!!        resubmit the program.'
          print *,'!!!'
          print *,'!!!        Western boundary: ',trkrinfo%westbd
          print *,'!!!        Eastern boundary: ',trkrinfo%eastbd
          print *,'!!!        Northern boundary: ',trkrinfo%northbd
          print *,'!!!        Southern boundary: ',trkrinfo%southbd
          print *,' '
          ifgcret = 91
          return
        else
          ! Calculate the beginning and ending i and j points....
          jbeg = (glatmax + grdspc - trkrinfo%northbd) / grdspc 
          jend = (glatmax + grdspc - trkrinfo%southbd) / grdspc 
          ibeg = (trkrinfo%westbd - glonmin + grdspc)  / grdspc
          iend = (trkrinfo%eastbd - glonmin + grdspc)  / grdspc
        endif
      endif

c     Scan the requested portion of the grid and pick out the max and
c     min data values, figure out what the max and min contour levels
c     will be, and fill an array with the values of the various 
c     intermediate, incremental contour levels.

      if (trkrinfo%contint <= 0) then
        print *,' '
        print *,'!!! ERROR: Error in first_ges_center.  For a midlat'
        print *,'!!!        or tcgen run of the  tracker, the contour'
        print *,'!!!        interval supplied by the user is not '
        print *,'!!!        greater than 0.'
        print *,'!!! '
        print *,'!!! User-supplied contint = ',trkrinfo%contint
        print *,' '
        ifgcret = 91
        return
      endif

      dmin =  9.99e20
      dmax = -9.99e20

      do j = jbeg,jend
        do i = ibeg,iend
          if (i > imax) then
            itemp = i - imax   ! If wrapping past GM
          else
            itemp = i
          endif
          if (valid_pt(itemp,j)) then
            if (fxy(itemp,j) < dmin) dmin = fxy(itemp,j)    
            if (fxy(itemp,j) > dmax) dmax = fxy(itemp,j)    
          endif
        enddo
      enddo

      print *,' '
      print *,'*--------------------------------------------*'
      print *,'In first_ges_center, dmin= ',dmin,' dmax= ',dmax

c     We want to allow for storms moving out of the sub-region,
c     in which case we might hit slightly lower or higher 
c     contours than were found in the sub-region, so allow for 
c     an extra buffer and modify dmin and dmax....

      dbuffer = (dmax - dmin) / 2.0
      dmax = dmax + dbuffer
      dmin = dmin - dbuffer
      print *,'after adjustment, dmin= ',dmin,' dmax= ',dmax

      contour_info%xmaxcont = dmax - amod(dmax,trkrinfo%contint)
      contour_info%xmincont = dmin - amod(dmin,trkrinfo%contint)

      print *,'A1 contour_info%xmaxcont= ',contour_info%xmaxcont
      print *,'A1 contour_info%xmincont= ',contour_info%xmincont

      if (contour_info%xmincont > contour_info%xmaxcont) then
        contour_info%xmincont = contour_info%xmaxcont
      endif

c      if (dmin > contour_info%xmincont) then 
c        contour_info%xmincont=contour_info%xmincont + trkrinfo%contint
c      endif
c      if (dmax < contour_info%xmaxcont) then 
c        contour_info%xmaxcont=contour_info%xmaxcont - trkrinfo%contint
c      endif

      print *,'A2 contour_info%xmaxcont= ',contour_info%xmaxcont
      print *,'A2 contour_info%xmincont= ',contour_info%xmincont
      print *,'maxconts= ',maxconts

c     NOTE: In the loop below, the contour_info%contvals array is now
c     (5/2003) no longer used in subsequent subroutines.  But we still
c     need to figure out the value of the contvals as we iterate the 
c     loop so we can know when we've surpassed dmax and can stop 
c     incrementing contour_info%numcont, which we do need in subsequent
c     subroutines.

      contour_info%numcont = 0
      do n = 1,maxconts
        contour_info%numcont = contour_info%numcont + 1
        contour_info%contvals(n) = contour_info%xmincont + 
     &                             float(n-1)*trkrinfo%contint
c        print *,'n= ',n,' contour_info%contvals(n)= '
c     &                 ,contour_info%contvals(n)
        if (contour_info%contvals(n) >= dmax) exit
      enddo
    
      oldstormct = stormct
      call find_all_maxmins (imax,jmax,ibeg,iend,jbeg,jend,fxy
     &        ,valid_pt,masked_out,contour_info
     &        ,trkrinfo,cmaxmin,maxstorm,stormct,maxmini
     &        ,maxminj,ifamret)

      if (stormct > 0) then
        continue
      else 
        print *,' '
        print *,' '
        print *,'!!! ************************************************ '
        print *,'!!! '
        print *,'!!! NOTE: In first_ges_center, the value of stormct'
        print *,'!!! returned from find_all_maxmins is not greater'
        print *,'!!! than 0.  This means that there are no new centers'
        print *,'!!! to track, which is not likely.  Perhaps you are '
        print *,'!!! searching over too small of an area??'
        print *,'!!! '
        print *,'!!! ************************************************ '
        print *,' '
      endif

      if (stormct > oldstormct .and. stormct > 0) then
        isstart = oldstormct + 1
        write (6,*) ' '
        write (6,*) 'New search: '
        write (6,*) 'Possible new max/min locations at ifh= ',ifh
        write (6,*) '--------------------------------------------'
        do n = isstart,stormct
          if (trkrinfo%type == 'midlat') then
            storm(n)%tcv_center = 'MIDL'
          else if (trkrinfo%type == 'tcgen') then
            storm(n)%tcv_center = 'TCG '
          endif
          slonfg(n,ifh) = glonmin + (maxmini(n)-1)*grdspc
          slatfg(n,ifh) = glatmax - (maxminj(n)-1)*grdspc
          storm(n)%tcv_stspd = -99
          storm(n)%tcv_stdir = -99
          write (storm(n)%tcv_storm_id,'(i3.3)') n
          write (storm(n)%tcv_storm_name,'(i3.3)') n
          stormswitch(n) = 1
          if (cparm == 'mslp') then
            write (6,71) maxmini(n),maxminj(n),slonfg(n,ifh)
     &                  ,360.-slonfg(n,ifh),slatfg(n,ifh)
     &                  ,slp(maxmini(n),maxminj(n))/100.0
          endif
        enddo
      else
        print *,' '
        print *,'    New search: '
        print *,'!!! NOTE: No new storms found in find_all_maxmins'
        print *,'!!! at ifh = ',ifh,'  stormct= ',stormct
        print *,'!!! oldstormct= ',oldstormct
        print *,' '
      endif

  71  format (1x,'i= ',i3,'  j= ',i3,'   lon: ',f7.2,'E  (',f6.2,'W)'
     &       ,2x,' lat: ',f6.2,'    mslp: ',f6.1,' mb')
c
      return
      end

c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine find_all_maxmins (imax,jmax,ibeg,iend,jbeg,jend,fxy
     &          ,valid_pt,masked_out,contour_info
     &          ,trkrinfo,cmaxmin,maxstorm,stormct,maxmini
     &          ,maxminj,ifamret)
c
c     ABSTRACT: This subroutine will search an area delineated by  
c     input i and j indeces in order to find all local maxes or mins 
c     in that area.  The (i,j) locations of the maxes/mins are returned
c     in the maxmini and maxminj arrays.  The input 3-character string
c     cmaxmin will tell the subroutine to look for a "max" or a "min".
c
c     INPUT:
c     imax     Number of gridpoints in i direction in input grid
c     jmax     Number of gridpoints in j direction in input grid
c     ibeg     i-index for upper left location of grid to search
c     iend     i-index for lower right location of grid to search
c     jbeg     j-index for upper left location of grid to search
c     jend     j-index for lower right location of grid to search
c     fxy      Real array of data values
c     valid_pt Logical bitmap masking non-valid grid points.  This is a
c              concern for the regional models, which are interpolated
c              from Lam-Conf or NPS grids onto lat/lon grids, leaving
c              grid points around the edges which have no valid data.
c     masked_out Logical. T = data point is already accounted for, under
c                the influence of another nearby max or min center,
c                F = data point is available to be scanned by this
c                subroutine for max or min centers.
c     contour_info Type cint_stuff from module contours containing the
c                  the following 4 variables:
c     1. xmincont Real value for min contour level in the fxy data array
c     2. xmaxcont Real value for max contour level in the fxy data array
c     3. contvals Real array holding values of cont levels at this time
c     4. numcont  Number of contour intervals found at this time
c     trkrinfo derived type containing various user-input tracker parms
c     cmaxmin  String that declares if "min" or "max" is being searched
c     maxstorm max # of storms that can be handled in this run
c
c     INPUT/OUTPUT:
c     stormct  Integer: keeps and increments a running tab of the number
c              of storms that have been tracked at any time across all 
c              forecast hours
c
c     OUTPUT:
c     maxmini  integer array containing i-indeces of the max/min points
c     maxminj  integer array containing j-indeces of the max/min points
c     ifamret  return code from this subroutine

      USE trkrparms; USE set_max_parms; USE contours

      implicit none

      type (trackstuff) trkrinfo
      type (cint_stuff) contour_info
      integer    stormct,i,j,ibeg,iend,jbeg,jend,ix,jx,ixp1,ixm1
      integer    ip,jp,maxstorm,jxp1,jxm1,ifamret,isret,iaret
      integer    isoiret,icccret,igicwret,imax,jmax
      character ccflag*1
      character(*) cmaxmin
      logical(1) still_finding_valid_maxmins,rough_gradient_check_okay
      logical(1) valid_pt(imax,jmax),masked_out(imax,jmax)
      integer    maxmini(maxstorm),maxminj(maxstorm)
      real       fxy(imax,jmax)
      real       xavg,stdv,search_cutoff,dmin,dmax,sphere_cutoff
c-----
      still_finding_valid_maxmins = .true.


c      print *,'ctm beg of find_all_maxmins, maxstorm= ',maxstorm


c     First, we want to get the mean and standard deviation of the input
c     field to be searched.  We can use the standard deviation info as
c     part of our guideline for when to stop searching for maxes & mins.
c     We will set the search cut-off threshold at 1/2 standard deviation
c     above the mean for min searches.  So, for the example of mslp, if
c     the mean pressure over the whole domain is 1010 mb and the 
c     standard deviation is 12 mb, then when we are searching, if the
c     lowest available (i.e., hasn't been found in a previous iteration
c     of this loop) pressure is 1016, then it's time to stop searching.

      call avgcalc   (fxy,imax*jmax,valid_pt,xavg,iaret)
      call stdevcalc (fxy,imax*jmax,valid_pt,xavg,stdv,isret)
      if (iaret /= 0 .or. isret /= 0) then
        print *,' '
        print *,'!!! ERROR: In find_all_maxmins, the calls to avgcalc'
        print *,'!!!        and/or stdevcalc returned an error.'
        print *,'!!!    iaret= ',iaret,'  isret= ',iaret
        print *,' '
        ifamret = 98
        return
      endif

      if (cmaxmin == 'min') then
        search_cutoff = xavg + stdv*0.5
      else
        search_cutoff = xavg - stdv*0.5
      endif

      print *,' '
      print *,'In find_all_maxmins, search_cutoff= ',search_cutoff
      print *,' '

c     Now begin to search the domain.  We do a simple gridpoint scan,
c     and once we find the max/min value, we pass the (i,j) coordinates
c     at that point to a routine to check for a closed contour.  Then
c     we mask out those points in the contour (or, if there is not a 
c     closed contour, just the 8 points immediately surrounding the low
c     center) and we do another iteration of search_loop to look for 
c     more lows.  We mask out points we've found so that on subsequent
c     iterations of search_loop, we don't find the same old center 
c     again and again and again.....

      search_loop: do while (still_finding_valid_maxmins)

        dmin =  9.99e20
        dmax = -9.99e20

        jloop: do j = jbeg,jend
          iloop: do i = ibeg,iend

            ip = i
            jp = j

            if (ip > imax) then 
              if (trkrinfo%gridtype == 'global') then
                ip = i - imax   ! If wrapping past GM
              else
                print *,' '
                print *,'!!! WARNING: In find_all_maxmins, the '
                print *,'!!!    user-requested eastern search boundary'
                print *,'!!!    is beyond the eastern bounds of '
                print *,'!!!    this regional grid.  The search'
                print *,'!!!    will not extend to the user-requested'
                print *,'!!!    grid boundary.'
                print *,'!!!         '
                print *,'!!!   imax of regional grid    = ',imax
                print *,'!!!   User-requested eastern i = ',ip
                print *,' '
                exit iloop
              endif
            endif

            if (valid_pt(ip,jp) .and..not. masked_out(ip,jp)) then
              if (cmaxmin == 'min') then
                if (fxy(ip,jp) < dmin) then
                  dmin = fxy(ip,jp)
                  ix = ip
                  jx = jp
                endif
              else
                if (fxy(ip,jp) > dmax) then
                  dmax = fxy(ip,jp)
                  ix = ip
                  jx = jp
                endif
              endif
            endif

          enddo iloop
        enddo jloop
 
        if (cmaxmin == 'min') then
          if (dmin < search_cutoff) then
            continue
          else
            still_finding_valid_maxmins = .false.
            exit search_loop
          endif
        else
          if (dmax > search_cutoff) then
            continue
          else
            still_finding_valid_maxmins = .false.
            exit search_loop
          endif
        endif

c       As a rough first check, see if the neighboring points on all
c       4 sides have a gradient sloping down into the found min point,
c       or at least that there is a flat field not having a gradient 
c       sloping away from the center point.

        call get_ijplus1_check_wrap (imax,jmax,ix,jx,ixp1,jxp1,ixm1
     &                              ,jxm1,trkrinfo,igicwret)

        if (igicwret /= 0) then
          print *,' '
          print *,'!!! NOTE: In find_all_maxmins, the center we found'
          print *,'!!!       is too close to the grid boundary and will'
          print *,'!!!       NOT be checked for a closed contour.'
          print *,'!!!  ix= ',ix,' jx= ',jx,'  fxy= ',fxy(ix,jx) 
          print *,'!!! '
          print *,' '
          masked_out(ix,jx) = .true. 
          cycle search_loop
        endif

        if (cmaxmin == 'min') then
          if (fxy(ix,jx) <= fxy(ixp1,jx) .and. 
     &        fxy(ix,jx) <= fxy(ix,jxm1) .and.
     &        fxy(ix,jx) <= fxy(ixm1,jx) .and.
     &        fxy(ix,jx) <= fxy(ix,jxp1)) then
            rough_gradient_check_okay = .true.
          else
            rough_gradient_check_okay = .false.
          endif
        else
          if (fxy(ix,jx) >= fxy(ixp1,jx) .and.
     &        fxy(ix,jx) >= fxy(ix,jxm1) .and.
     &        fxy(ix,jx) >= fxy(ixm1,jx) .and.
     &        fxy(ix,jx) >= fxy(ix,jxp1)) then
            rough_gradient_check_okay = .true.
          else
            rough_gradient_check_okay = .false.
          endif
        endif
 
        if (rough_gradient_check_okay) then

          print *,'Found a possible max/min at ix= ',ix,' jx= ',jx

c         From this rough check, we appear to have a gradient sloping
c         in towards the center point.  Now call the subroutine to 
c         check whether or not there is in fact a closed contour
c         surrounding this local maximum or minimum.

          ccflag = 'n'
          call check_closed_contour (imax,jmax,ix,jx,fxy,valid_pt
     &             ,masked_out,ccflag,cmaxmin,trkrinfo
     &             ,1,contour_info,icccret)

          if (ccflag == 'y') then
            stormct = stormct + 1
            print *,'AAA stormct= ',stormct,'  ix= ',ix,' jx= ',jx
            maxmini(stormct) = ix
            maxminj(stormct) = jx
          else
            print *,'--- contour check negative, ccflag= ',ccflag
          endif

        endif

c       Regardless of whether or not the found point turns out to have
c       a closed contour, we don't want to find this local minimum or
c       its 8 surrounding points again in a search on a subsequent 
c       iteration of this loop.

        masked_out(ix,jx)     = .true.
        masked_out(ix,jxp1)   = .true.
        masked_out(ixp1,jxp1) = .true.
        masked_out(ixp1,jx)   = .true.
        masked_out(ixp1,jxm1) = .true.
        masked_out(ix,jxm1)   = .true.
        masked_out(ixm1,jxm1) = .true.
        masked_out(ixm1,jx)   = .true.
        masked_out(ixm1,jxp1) = .true.

      enddo search_loop

      return
      end

c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine fix_latlon_to_ij (imax,jmax,grdspc,fxy,cmaxmin
     &                 ,valid_pt,parmlon,parmlat,xdataval
     &                 ,ifix,jfix,gridpoint_maxmin,ifilret)
c
c     ABSTRACT: This subroutine takes an input lat/lon position and
c     assigns it to a nearby (i,j) gridpoint.  If this is being used
c     before the call to check_closed_contour after the barnes analysis
c     to see if we have a storm or not, then the lat/lon position that
c     is input into this subroutine is one which was obtained from a
c     barnes analysis, so it is essentially an area-weighted average
c     of nearby points.  What we need to do in this subroutine is find
c     the actual nearby gridpoint which does have the actual raw max or
c     min value.  Then we return the (i,j) coordinates of that point as
c     well as that raw data value at that point.
c
c     INPUT:
c     imax     Num pts in i-direction on grid
c     jmax     Num pts in j-direction on grid
c     grdspc   grid spacing of the data grid
c     fxy      real array of input data values
c     cmaxmin  character that tells if searching for max or min
c     valid_pt Logical; bitmap indicating if valid data at that pt
c     parmlon  lon at which input parameter center was found
c     parmlat  lat at which input parameter center was found
c     xdataval barnes-obtained value of parameter at (parmlon,parmlat)
c
c     OUTPUT:
c     ifix     i-index for gridpoint to which the max or min is assigned
c     jfix     j-index for gridpoint to which the max or min is assigned
c     gridpoint_maxmin  value of fxy at (ifix,jfix).  This will be 
c              different from the input value xdataval, which came from 
c              the barnes averaging.  This is the raw value at the 
c              gridpoint.
              
      USE grid_bounds

      implicit none

      integer    imax,jmax,istart,iend,jstart,jend,ifix,jfix
      integer    ipfix,jpfix,i,j,ifilret
      character(*)  cmaxmin
      logical(1) valid_pt(imax,jmax)
      real       fxy(imax,jmax)
      real       parmlon,parmlat,xdataval,gridpoint_maxmin
      real       xplon,yplat,grdspc,dmin,dmax

      ifilret = 0

c     Fix parmlat to the *nearest* j-point (i.e., round it....)

      if (parmlat >= 0.0) then    ! N. Hemisphere
        jpfix = int((glatmax - parmlat)/grdspc + 1.0 + 0.5)
      else                        ! S. Hemisphere
        jpfix = ceiling((glatmax - parmlat)/grdspc + 1.0 - 0.5)
      endif

c     Fix parmlon to the *nearest* i-point (i.e., round it....)

      ipfix = int((parmlon - glonmin)/grdspc + 1.0 + 0.5)

c     Calculate the longitude and latitude of these ipfix and
c     jpfix points....

      xplon = glonmin + (ipfix-1)*grdspc
      yplat = glatmax - (jpfix-1)*grdspc

c     We want to do a simple search in the very few points around
c     this (ipfix,jpfix) point to find the raw max or min data 
c     value.  First we need to set up a 4x4 box to search:
c
c               o     o     o     o                             
c                                                               
c                                                               
c               o     a     b     o                             
c                      +                                        
c                                                               
c               o     c     d     o                             
c                                                               
c                                                               
c               o     o     o     o                             
c                                                               
c     In the above diagram, if "+" is the lat/lon location of our
c     barnes-found point (i.e., the input (parmlon,parmlat)), and
c     a-b-c-d is the square of points surrounding "+", we only want
c     to look out 1 layer of points further.  So first we need to 
c     know, for each case we're looking at, if "+" got assigned to
c     a or b or c or d.  By the way, if the parmlon falls directly
c     on a gridpoint in either the i or j direction, we will only 
c     look at the 2 gridpoints on either side of that point, as 
c     opposed to having 4 points set up as in the box above.

      if (xplon < parmlon) then         !(ipfix is at either a or c)
        istart = ipfix - 1
        iend   = ipfix + 2
      else if (xplon > parmlon) then    !(ipfix is at either b or d)
        istart = ipfix - 2
        iend   = ipfix + 1
      else if (xplon == parmlon) then   !(parmlon is exactly ipfix)
        istart = ipfix - 1
        iend   = ipfix + 1
      endif

      if (yplat < parmlat) then         !(jpfix is at either c or d)
        jstart = jpfix - 2
        jend   = jpfix + 1
      else if (yplat > parmlat) then    !(jpfix is at either a or b)
        jstart = jpfix - 1
        jend   = jpfix + 2
      else if (yplat == parmlat) then   !(parmlat is exactly jpfix)
        jstart = jpfix - 1
        jend   = jpfix + 1
      endif

c     Make sure the edges of our box are within the grid bounds...

      if (jstart > jmax ) then
        print *,'!!! ERROR in fix_latlon_to_ij, jstart > jmax'
        print *,'!!! jstart = ',jstart,' jmax= ',jmax
        ifilret = 99
        return
      endif
      if (jend < 1) then
        print *,'!!! ERROR in fix_latlon_to_ij, jend < 1, jend = ',jend
        ifilret = 99
        return
      endif
      if (jstart < 1) jstart = 1
      if (jend > jmax) jend = jmax

      if (istart > imax ) then
        print *,'!!! ERROR in fix_latlon_to_ij, istart > imax'
        print *,'!!! istart = ',istart,' imax= ',imax
        ifilret = 99
        return
      endif
      if (iend < 1) then
        print *,'!!! ERROR in fix_latlon_to_ij, iend < 1, iend = ',iend
        ifilret = 99
        return
      endif
      if (istart < 1) istart = 1
      if (iend > imax) iend = imax

c     Now look for the max or min value....

      dmax = -9.99e12
      dmin =  9.99e12
      ifix = ipfix
      jfix = jpfix

      do i = istart,iend
        do j = jstart,jend

          if (valid_pt(i,j)) then
            continue
          else
            print *,' '
            print *,'!!! ERROR: In fix_latlon_to_ij, we tried to '
            print *,'!!! access an invalid data point.'
            print *,'!!! i= ',i,' j= ',j
            print *,'!!! ipfix= ',ipfix,' jpfix= ',jpfix
            print *,'!!! parmlon= ',parmlon,' parmlat= ',parmlat
            print *,' '
            ifilret = 98
            return
          endif

          if (cmaxmin == 'min') then
            if (fxy(i,j) < dmin) then
              dmin = fxy(i,j)
              ifix = i
              jfix = j
            endif
          else
            if (fxy(i,j) > dmax) then
              dmax = fxy(i,j)
              ifix = i
              jfix = j
            endif
          endif
        enddo
      enddo

      if (cmaxmin == 'min') then
        gridpoint_maxmin = dmin
      else
        gridpoint_maxmin = dmax
      endif

      return
      end

c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine check_closed_contour (imax,jmax,ix,jx,fxy,valid_pt
     &           ,masked_out,closed_contour,cmaxmin,trkrinfo
     &           ,num_requested_contours,contour_info,icccret)
c
c     ABSTRACT: This subroutine checks a field of data around an input
c     (ix,jx) data point to see if a closed contour exists around 
c     that data point.  It can check for a closed contour on a max or a 
c     min field, depending on the value of the input variable 'cmaxmin'.
c     The algorithm works by examining rings of the 8 data points 
c     surrounding a data point that is in the contour interval.  For
c     example, in the diagram below, the X represents the location of
c     the local minimum value which was passed into this routine with
c     the coordinates (ix,jx), let's say it's 985 mb.  And let's assume
c     that the data values at points A-I are all in the 4 mb contour 
c     interval of 985-989 mb, and that all the surrounding points have
c     data values >= 989.  To test for a closed contour, we first check
c     the ring of 8 points immediately around point X to see what their
c     data values are.  If a data value is found that is below the 
c     lower limit of this contour interval (985 mb) or lower than the
c     local minimum value at the X point that we initially targeted 
c     (985 mb), then we do NOT have a closed contour, and we exit this
c     subroutine.  But in our example, that's not the case, and we have
c     5 points (B,D,E,F,G) that are in the interval.  So in our next 
c     iteration of the loop, we set up 5 rings, each one set up around 
c     the points found in the first iteration (B,D,E,F,G), and we check 
c     the 8 points around each of those points.  A logical array is 
c     used so that as soon as a point is found, it is flagged as being 
c     found.  In this way, when we look at the ring around point D, for
c     example, we won't pick point X again and set up another ring 
c     around it in the next ring iteration and end up in an infinite 
c     loop, going back and forth between point X and point D.  While 
c     checking the 8 points in a ring, if a found data value is above 
c     our contour interval (i.e., >= 989 mb), we just ignore the 
c     point; we only mark points that are in our contour interval, 
c     and again, if we find a point below our contour interval, we 
c     exit the subroutine with a flag indicating a closed contour was 
c     NOT found.  So in this method, we keep spreading out from the 
c     initial local minimum and creating and checking new rings until 
c     we either: (a) Hit the edge of the regional grid, in which case 
c     we consider a closed contour NOT found, (b) Run into a data 
c     point that has been marked as being under the influence of 
c     another nearby low, in which case we consider a closed contour 
c     NOT found, (c) Run into a point which is below (above) our 
c     contour interval for a min (max) check, in which case we 
c     consider a closed contour NOT found, or (d) we run out of 
c     points to keep searching, we have no rings left to create and 
c     check because all of the surrounding points are above (below) 
c     our contour interval for a min (max) check, and by default we 
c     consider this a closed contour and return to the calling 
c     subroutine a flag indicating such.
c
c               + + + + + + + + + + 
c               + + + + + + + + + + 
c               + + A B + + + + + + 
c               + + C D X E + + + + 
c               + + + + F G + + + + 
c               + + + + + H I + + + 
c               + + + + + + + + + + 
c               + + + + + + + + + + 
c
c     UPDATE: This subroutine was updated to keep searching for 
c     multiple closed contours until it can't find anymore.  The 
c     input parameter num_requested_contours dictates how many 
c     contours to search for.  In the case of just trying to roughly
c     locate new centers and establish that there is a closed 
c     circulation, num_requested_contours will = 1, and we will exit
c     after finding that 1 contour.  But for a check after making a
c     full center fix, we set num_requested_contours = 999 so that 
c     we can keep searching for all closed contours around the low.
c     In this 999 case, you will eventually get to a point where
c     there is no closed contour.  In that case, in the standard
c     output you will see a message telling you that you hit a point
c     that is not in the contour and that there is no closed contour,
c     but you will also notice that the ccflag = y, meaning there is
c     a closed contour (because you have found at least 1 closed 
c     contour along the way).  The reason to keep searching for more
c     closed contours is that we can then return the value of the 
c     outermost closed isobar.
c
c     INPUT:
c     imax     Num pts in i-direction on grid
c     jmax     Num pts in j-direction on grid
c     ix       i index for location of local max or min
c     jx       j index for location of local max or min
c     fxy      input data array
c     valid_pt Logical; bitmap indicating if valid data at that pt
c     masked_out Logical. T = data point is already accounted for, under
c                the influence of another nearby max or min center,
c                F = data point is available to be scanned by this
c                subroutine for max or min centers.
c     cmaxmin  character string ('max' or 'min') that tells this 
c              routine what we're looking for.
c     trkrinfo   derived type that holds/describes various tracker parms
c     contour_info Type cint_stuff from module contours.  Contains
c                  contour information
c     num_requested_contours  For the simple first_ges_center check, 
c              this will be 1 (we just want to know if there's at
c              least 1 closed contour).  For the verifying check after
c              we've found a center, this will be 9999 (i.e., just keep
c              searching for more contours)
c
c     OUTPUT:
c     closed_contour character; A returned value of 'y' indicates that
c              this routine was able to find a closed contour. 
c
c     LOCAL:
c     num_pts_in_all_contours Counter for the number of pts inside of 
c              the contour we're looking at
   
      USE set_max_parms; USE trkrparms; USE contours

      implicit none

      type (trackstuff) trkrinfo
      type (cint_stuff) contour_info

      integer    i,j,ir,iria,irja,irx,jrx,ix,jx,imax,jmax
      integer    nb,ibx,jby,nct,iflip
      integer    mr,ringct,ixp1,ixm1,jxp1,jxm1,nring,iter
      integer    icenx,jcenx,icccret,next_ring_ct,igicwret
      integer    num_pts_in_all_contours,next_contour_ct
      integer    beyond_contour_ct
      integer    num_pts_in_one_contour
      integer    num_requested_contours,num_found_contours
      integer    nm,im,jm,inall,insingle,isc_count
      character  found_a_point_in_our_contour*1,closed_contour*1
      character  found_a_point_below_contour*1
      character  found_a_point_above_contour*1
      character(*) cmaxmin
      logical(1) still_scanning
      logical(1) valid_pt(imax,jmax),masked_out(imax,jmax)
      logical(1) point_already_in_our_contour(imax,jmax)
      logical(1) point_already_in_next_contour(imax,jmax)
      integer    search_next_i((imax*jmax)/4)
      integer    search_next_j((imax*jmax)/4)
      integer    next_contour_i((imax*jmax)/4)
      integer    next_contour_j((imax*jmax)/4)
      integer    beyond_contour_i((imax*jmax)/10)
      integer    beyond_contour_j((imax*jmax)/10)
      integer    hold_mask_i_loc((imax*jmax)/4)
      integer    hold_mask_j_loc((imax*jmax)/4)
      integer    temp_mask_i_loc((imax*jmax)/4)
      integer    temp_mask_j_loc((imax*jmax)/4)
      integer, allocatable :: ringposi(:),ringposj(:)
      real       contlo,conthi,xcentval,contlo_next,conthi_next
      real       fxy(imax,jmax),contvals(maxconts)
      real,allocatable :: ringpos(:,:)
c
      closed_contour = 'n'
      xcentval = fxy(ix,jx)
      num_found_contours = 0
      next_contour_ct = 0
      beyond_contour_ct = 0
      num_pts_in_all_contours = 0
      hold_mask_i_loc = 0
      hold_mask_j_loc = 0
      point_already_in_our_contour  = .false.
      icccret = 0
      isc_count = 0

      print *,' '
      print *,'In check_closed_contour, ix= ',ix,' jx= ',jx
      print *,'fxy(ix,jx)= ',fxy(ix,jx),' xcentval= ',xcentval

c     First, set up the contour intervals that will be used.  In
c     the original version of this code, we used preset 
c     standard intervals (984,988,992,996,1000,1004....).  But upon
c     further review, it was decided that this was too arbitrary.
c     So instead, we consider the found min (max) value to be the 
c     bottom (top) of the list of contour intervals.  In this way,
c     we can clearly specify and screen storms based on the "depth" 
c     of the pressure field as compared to the surroundings.

      i = 1
      do while (i <= contour_info%numcont)
        if (cmaxmin == 'min') then
          contvals(i) = xcentval + float(i-1)*trkrinfo%contint 
          i = i + 1
        else
          iflip = contour_info%numcont - i + 1
          contvals(iflip) = xcentval - float(i-1)*trkrinfo%contint
          i = i + 1
        endif
      enddo

c     This successive_contours loop is the master loop....

      successive_contours_loop: do while (num_found_contours <
     &     num_requested_contours)

c       Find the contour interval in which the center value resides.
c       Note that the lower bound is included for a min check, while
c       the upper bound is included for a max check.  Note also that
c       this subroutine can be used to find the last closed contour,
c       and part of that functionality shows up in the next while 
c       statement where we reference "num_found_contours" in the 
c       array indeces for the contour values.  Basically, the way we
c       do this is, for example, if our central value is 990.4 mb and 
c       our contour interval is 4 mb, then in the first run through 
c       successive_contours_loop we see if we have a closed contour in
c       the interval 990.4-994.4.  If yes, then the next time through 
c       this loop, we see if we have a closed contour in the interval 
c       994.4-998.4.  If yes, then the next loop check is for 998.4-
c       1002.4, and so on....  We stop searching if we find a value 
c       that is either below the xcentval input into this subroutine
c       or below the lower value of the current contour interval (this
c       would mean a change in the gradient and would indicate that, 
c       in the case of mslp, we are heading down towards another,
c       different low).

        isc_count = isc_count + 1

        point_already_in_next_contour = .false.

        i = 1
        do while (i < contour_info%numcont)
          if (cmaxmin == 'min') then
            if (contvals(i) <= xcentval .and. xcentval < contvals(i+1)) 
     &      then
              contlo = contvals(i+num_found_contours)
              conthi = contvals(i+1+num_found_contours)
              exit
            endif
          else
            if (contvals(i) < xcentval .and. xcentval <= contvals(i+1)) 
     &      then
              contlo = contvals(i-num_found_contours)
              conthi = contvals(i-num_found_contours+1)
              exit
            endif
          endif
          i = i + 1
        enddo

        print *,' '
        print *,'num_found_contours= ',num_found_contours
        print *,'contlo= ',contlo,' conthi= ',conthi
        print *,'xcentval= ',xcentval

c       This single_contour_scan_loop is the main loop for searching 
c       for one individual contour.  If it is determined that a contour
c       exists, control is returned to the successive_contours_loop,
c       and if more contours were requested to be found, then the 
c       search continues onward & outward....

        temp_mask_i_loc = 0
        temp_mask_j_loc = 0

        iter  = 1
        num_pts_in_one_contour = 0
        still_scanning = .true.

        single_contour_scan_loop: do while (still_scanning)

ctm       
c          print *,' '
c          print *,'    top of single contour scan loop'
c          print *,'+++ iter= ',iter
c          print *,'    N1: next_contour_ct= ',next_contour_ct

          if (iter == 1 .and. num_found_contours == 0) then 
            ! For the first iteration, we have only the first ring, 
            ! which is centered on the input minimum/maximum point.
            ringct = 1
            search_next_i(1) = ix 
            search_next_j(1) = jx 
C            point_already_in_our_contour(ix,jx) = .true.
C            num_pts_in_one_contour = num_pts_in_one_contour + 1
C            temp_mask_i_loc(num_pts_in_one_contour) = ix
C            temp_mask_j_loc(num_pts_in_one_contour) = jx
          else if (iter == 1 .and. num_found_contours > 0) then
            ! This is the first iteration in a *new* contour.
            ! That is, we have already found 1 or more previous
            ! contours while in previous iterations of 
            ! successive_contours_loop and we are now beginning 
            ! to look for the next contour.
ctm
c            print *,'    N2: next_contour_ct= ',next_contour_ct

            if (next_contour_ct == 0) then
              ! This would be for the special case in which, for
              ! example, you've got a very intense, compact storm
              ! that "skips" a contour.  That is, suppose the 
              ! min pressure of a storm is 982 mb, but all 
              ! surrounding data points are, say, 987 mb or 
              ! higher.  Then, next_contour_ct would be 0 since no
              ! data points were found in the next contour interval,
              ! but we can continue searching since the gradient is
              ! still sloping the correct way.  The code in this if
              ! statement handles this special case.
              print *,' '
              print *,'ALERT: next_contour_ct = 0 '
              if (cmaxmin == 'min') then
                contlo_next = conthi
                conthi_next = conthi + trkrinfo%contint
                do nb = 1,beyond_contour_ct
                  ibx = beyond_contour_i(nb)
                  jby = beyond_contour_j(nb)
                  if (fxy(ibx,jby) >= contlo_next .and.
     &                fxy(ibx,jby) <  conthi_next) then
                    next_contour_ct = next_contour_ct + 1
                    next_contour_i(next_contour_ct) = ibx
                    next_contour_j(next_contour_ct) = jby
                    print *,' +++ BEYOND: i= ',ibx,' j= ',jby
     &                     ,' fxy= ',fxy(ibx,jby)
                  endif
                enddo
              else
                contlo_next = contlo - trkrinfo%contint
                conthi_next = contlo
                do nb = 1,beyond_contour_ct
                  ibx = beyond_contour_i(nb)
                  jby = beyond_contour_j(nb)
                  if (fxy(ibx,jby) >  contlo_next .and.
     &                fxy(ibx,jby) <= conthi_next) then
                    next_contour_ct = next_contour_ct + 1
                    next_contour_i(next_contour_ct) = ibx
                    next_contour_j(next_contour_ct) = jby
                  endif
                enddo
              endif

              if (next_contour_ct > 0) then
                ringct = next_contour_ct
                beyond_contour_ct = 0
                beyond_contour_i  = 0
                beyond_contour_j  = 0
              endif

            else
              ringct = next_contour_ct
            endif

            do nring = 1,ringct
              search_next_i(nring) = next_contour_i(nring)
              search_next_j(nring) = next_contour_j(nring)
            enddo
            next_contour_ct = 0
          else
            ringct = next_ring_ct 
          endif

          if (allocated(ringposi)) deallocate (ringposi)
          if (allocated(ringposj)) deallocate (ringposj)
          allocate (ringposi(ringct),stat=iria)
          allocate (ringposj(ringct),stat=irja)
          if (iria /= 0 .or. irja /= 0) then
            print *,' '
            print *,'!!! ERROR in check_closed_contour allocating'
            print *,'!!! various ring arrays.  iria = ',iria
            print *,'!!! irja = ',irja
            print *,' '
            STOP 98
          endif

ctm
c          print *,' '
c          print *,'ringct= ',ringct

          do nring = 1,ringct
            ringposi(nring) = search_next_i(nring)
            ringposj(nring) = search_next_j(nring)
ctm
c            print *,'nring= ',nring,' ringposi= ',ringposi(nring)
c     &                             ,' ringposj= ',ringposj(nring)
          enddo

          next_ring_ct = 0

c         In each iteration of single_contour_scan_loop, we can have a
c         different number of rings to analyze.  In the first
c         iteration, we only have 1 ring, the initial ring around the
c         local max/min that was input to this subroutine.  Subsequent
c         iterations will have a variable number of rings, depending on
c         how many new data points within our contour interval were 
c         found in the previous iteration.

          print *,' '
          multiple_ring_loop: do mr = 1,ringct

            icenx = ringposi(mr)
            jcenx = ringposj(mr)

ctm
c            print *,'  --- iter= ',iter,' mr= ',mr,' icenx= ',icenx
c     &             ,' jcenx= ',jcenx

            call get_ijplus1_check_wrap (imax,jmax,icenx,jcenx,ixp1,jxp1
     &                                  ,ixm1,jxm1,trkrinfo,igicwret)

            if (igicwret /= 0) then
              print *,' '
              print *,'!!! NO CLOSED CONTOUR: The call to '
              print *,'!!! get_ijplus1_check_wrap indicates the max/min'
              print *,'!!! contour extends past the edge of our '
              print *,'!!! regional grid. '
              print *,' '
              print *,' '

              do nm = 1,num_pts_in_all_contours
                im = hold_mask_i_loc(nm)
                jm = hold_mask_j_loc(nm)
                masked_out(im,jm) = .true.
              enddo

              deallocate (ringposi); deallocate (ringposj)
              icccret = 0
              return
            endif

c           For each individual ring, we check all 8 points surrounding
c           the center point.  The points are numbered for each ring as
c           shown in the diagram to the right of the "select case" 
c           statement just below.  REMEMBER: The j in our grids 
c           increases from north to south, so that for a global grid,
c           j=1 is at 90N and j=jmax is at 90S.

            individual_ring_loop: do ir = 1,9

              select case (ir)
                case (1); irx=ixm1; jrx=jcenx;!     2       3       4 
                case (2); irx=ixm1; jrx=jxm1; !                      
                case (3); irx=icenx;jrx=jxm1; !                     
                case (4); irx=ixp1; jrx=jxm1; !     1 (icenx,jcenx) 5
                case (5); irx=ixp1; jrx=jcenx;!                     
                case (6); irx=ixp1; jrx=jxp1; !                     
                case (7); irx=icenx;jrx=jxp1; !     8       7       6
                case (8); irx=ixm1; jrx=jxp1; !                     
                case (9); irx=icenx; jrx=jcenx; ! = center pt of ring
              end select

c             Make sure the point we are looking at has valid data.  
c             This is an issue only on regional grids, where we have a
c             buffer of bitmapped (null) data points surrounding the 
c             real grid.

              if (.not. valid_pt(irx,jrx)) then
                print *,' '
                print *,'!!! NOTE: In check_closed_contour, while '
                print *,'!!!     checking points around (icenx,jcenx)='
                print *,'!!!     (',icenx,',',jcenx,'), we hit a non-'
                print *,'!!!     valid point, meaning we are near the '
                print *,'!!!     bounds of the grid.  We will skip the'
                print *,'!!!     search for this center.'
                print *,'!!! '
                print *,'!!! (i,j) of non-valid pt = (',irx,',',jrx,')'
                print *,'!!! '

                do nm = 1,num_pts_in_all_contours
                  im = hold_mask_i_loc(nm)
                  jm = hold_mask_j_loc(nm)
                  masked_out(im,jm) = .true.
                enddo

                deallocate (ringposi); deallocate (ringposj)
                icccret = 0
                return
              endif

c             Check to make sure that the point we are looking at is
c             not considered under the influence of another nearby low.

              if (masked_out(irx,jrx)) then
                print *,' '
                print *,'!!! NOTE: In check_closed_contour, while '
                print *,'!!!     checking points around (icenx,jcenx)='
                print *,'!!!     (',icenx,',',jcenx,'), we hit a point'
                print *,'!!!     that has been masked out, meaning it'
                print *,'!!!     belongs under the influence of '
                print *,'!!!     another nearby low, so we will skip'
                print *,'!!!     the search for this center....'
                print *,'!!!  '
                print *,'!!!  Min central value      = ',xcentval
                print *,'!!!  (i,j) of central value = (',ix,',',jx,')'
                print *,'!!!  '
                print *,'!!!  Masked-out value found = ',fxy(irx,jrx)
                print *,'!!!  (i,j) of masked value  = (',irx,','
     &                 ,jrx,')'
                print *,'!!!  '
                print *,'!!!  Lower bound of contour interval = ',contlo
                print *,'!!!  Upper bound of contour interval = ',conthi
                print *,'!!!  Contour interval = ',trkrinfo%contint

                do nm = 1,num_pts_in_all_contours
                  im = hold_mask_i_loc(nm)
                  jm = hold_mask_j_loc(nm)
                  masked_out(im,jm) = .true.
                enddo

                deallocate (ringposi); deallocate (ringposj)
                icccret = 0
                return
              endif

c             If we have already hit this point on a previous ring 
c             check, then just ignore this point and cycle past it.

              if (point_already_in_our_contour(irx,jrx)) then
ctm
c                print *,' '
c                print *,'Pt. AAA, already-in-contour.....'
c                print *,'irx= ',irx,' jrx= ',jrx
                cycle individual_ring_loop
              endif

c             For a MIN check, check to see if the data point is below 
c             the contour interval or is below the local minimum value 
c             passed into this subroutine.  In either case, exit and 
c             consider this to NOT be a closed contour.
c             For a MAX check, check to see if the data point is above 
c             the contour interval or is above the local maximum value 
c             passed into this subroutine.  In either case, exit and 
c             consider this to NOT be a closed contour.
c             
c             For example, for mslp, this would be as we're moving 
c             outward away from lower pressures to higher pressures,
c             and then all of a sudden we come upon a lower pressure.
c             This probably means we're heading toward another low
c             pressure area, so mark the point and return to the 
c             calling routine.

              found_a_point_below_contour = 'n'
              found_a_point_above_contour = 'n'
              if (cmaxmin == 'min') then
                if (fxy(irx,jrx) < xcentval .or. fxy(irx,jrx) < contlo)
     &          then
                  found_a_point_below_contour = 'y'
                endif
              else 
                if (fxy(irx,jrx) > xcentval .or. fxy(irx,jrx) > conthi)
     &          then
                  found_a_point_above_contour = 'y'
                endif
              endif

              if (found_a_point_below_contour == 'y' .or.
     &            found_a_point_above_contour == 'y') then
                print *,' '
                print *,'!!! NOTE: In check_closed_contour, while '
                print *,'!!!     checking points around (icenx,jcenx)='
                print *,'!!!     (',icenx,',',jcenx,'), we hit a data'
                print *,'!!!     value that is less (greater) than the'
                print *,'!!!     current contour interval bound for a'
                print *,'!!!     min (max) and/or is less (greater) '
                print *,'!!!     than the minimum (maximum) central '
                print *,'!!!     value that we are centering the '
                print *,'!!!     search on.'
                print *,'!!!    '
                print *,'!!!  Central value      = ',xcentval
                print *,'!!!  (i,j) of central value = (',ix,',',jx,')'
                print *,'!!!   '
                print *,'!!!  Flagged value found    = ',fxy(irx,jrx)
                print *,'!!!  (i,j) of flagged value = (',irx,','
     &                 ,jrx,')'
                print *,'!!!   '
                print *,'!!!  Lower bound of contour interval = ',contlo
                print *,'!!!  Upper bound of contour interval = ',conthi
                print *,'!!!  Contour interval = ',trkrinfo%contint
                print *,'!!! ' 

                do nm = 1,num_pts_in_all_contours
                  im = hold_mask_i_loc(nm)
                  jm = hold_mask_j_loc(nm)
                  masked_out(im,jm) = .true.
                enddo

                deallocate (ringposi); deallocate (ringposj)
                icccret = 0
                return
              endif 

c             If we've made it this far, then we at least know that the
c             gradient is still heading in the right direction.  Do the
c             check now to see if the value at this point is within our
c             specific contour interval (there is the possibility that
c             the value is beyond our interval, which will be checked
c             for just below, and if that's the case, then that point
c             will be processed in a subsequent iteration of this loop
c             that encompasses that correct contour interval).

              found_a_point_in_our_contour = 'n'
              if (cmaxmin == 'min') then
                if (fxy(irx,jrx) >= contlo .and. fxy(irx,jrx) < conthi)
     &          then
                  found_a_point_in_our_contour = 'y'
                endif
              else
                if (fxy(irx,jrx) > contlo .and. fxy(irx,jrx) <= conthi) 
     &          then
                  found_a_point_in_our_contour = 'y'
                endif
              endif

              if (found_a_point_in_our_contour == 'y') then
                ! We've found a data point in our interval, something 
                ! that is inside the closed contour, and it hasn't been
                ! marked as being found in a previous iteration of this 
                ! loop, so mark it now and store the (i,j) location so 
                ! that we can scan a ring around this point in a 
                ! successive iteration of this loop for more potential 
                ! points within this interval...

                point_already_in_our_contour(irx,jrx) = .true.

                next_ring_ct = next_ring_ct + 1
                search_next_i(next_ring_ct) = irx
                search_next_j(next_ring_ct) = jrx

                num_pts_in_one_contour = num_pts_in_one_contour + 1
                temp_mask_i_loc(num_pts_in_one_contour) = irx
                temp_mask_j_loc(num_pts_in_one_contour) = jrx

ctm
c                print *,' '
c                print *,' PT IN! irx= ',irx,' jrx= ',jrx,' xval= '
c     &                 ,fxy(irx,jrx)
c                print *,'next_ring_ct= ',next_ring_ct
c                print *,'num_pts_in_one_contour= '
c     &                 ,num_pts_in_one_contour
              endif

c             If we've made it this far AND the 
c             found_a_point_in_our_contour flag indicates that this
c             point is not in our contour interval, then by default that
c             means that this point is for a contour interval beyond 
c             what we're currently looking at.  E.g., if we're looking 
c             at the contours around a 972 mb low and we're moving 
c             outward and currently checking the 984-988 mb contour 
c             interval, it means that we found, say, a gridpoint with 
c             991 mb.  So we want to mark that point for a future 
c             iteration of this loop that would be checking the 
c             988-992 mb contour interval.

              if (found_a_point_in_our_contour /= 'y' .and. 
     &            .not. point_already_in_next_contour(irx,jrx)) then
                ! We've found a data point that is beyond our interval,
                ! so this is not a concern for finding the bounds of 
                ! our current contour interval, but we want to mark 
                ! these points and remember them for the next iteration
                ! of successive_scan_loop.  (For example, suppose we 
                ! are currently searching for points in the 984-988 mb
                ! range, and we find a point that is 990 -- mark it 
                ! here to be remembered when we scan for 988-992 mb).
                if (cmaxmin == 'min') then
                  contlo_next = conthi
                  conthi_next = conthi + trkrinfo%contint
                  if (fxy(irx,jrx) >= contlo_next .and. 
     &                fxy(irx,jrx) <  conthi_next) then
                    ! "NEXT_CONTOUR" Comment:
                    ! We've found a point that is in the very next
                    ! contour interval....
                    next_contour_ct = next_contour_ct + 1
                    next_contour_i(next_contour_ct) = irx
                    next_contour_j(next_contour_ct) = jrx
                    point_already_in_next_contour(irx,jrx) = .true.
                  else if (fxy(irx,jrx) >= conthi_next) then
                    ! "BEYOND_CONTOUR" Comment:
                    ! This point is at least 1 contour interval beyond
                    ! the next contour interval.  Dump the info into 
                    ! these i and j arrays.  This info will be used if
                    ! in the next iteration of single_contour_scan_loop,
                    ! next_contour_ct = 0.  That would mean that we 
                    ! have, e.g., an intensely deep low with a sharp
                    ! mslp gradient that essentially "skips" over a 
                    ! contour interval.  E.g., if using a 4 mb interval,
                    ! we go from 947 to 953 AND there are NO
                    ! intervening gridpoints in the 948-952 interval.
                    beyond_contour_ct = beyond_contour_ct + 1
                    beyond_contour_i(beyond_contour_ct) = irx
                    beyond_contour_j(beyond_contour_ct) = jrx
                  endif
                else
                  contlo_next = contlo - trkrinfo%contint
                  conthi_next = contlo
                  if (fxy(irx,jrx) >  contlo_next .and. 
     &                fxy(irx,jrx) <= conthi_next) then
                    ! See "NEXT_CONTOUR" comment just above....
                    next_contour_ct = next_contour_ct + 1
                    next_contour_i(next_contour_ct) = irx
                    next_contour_j(next_contour_ct) = jrx
                    point_already_in_next_contour(irx,jrx) = .true.
                  else if (fxy(irx,jrx) <= contlo_next) then
                    ! See "BEYOND_CONTOUR" comment just above....
                    beyond_contour_ct = beyond_contour_ct + 1
                    beyond_contour_i(beyond_contour_ct) = irx
                    beyond_contour_j(beyond_contour_ct) = jrx
                  endif
                endif
              endif 
               
            enddo individual_ring_loop

          enddo multiple_ring_loop

          if (next_ring_ct > 0) then
            iter = iter + 1
          else
            icccret = 0
            still_scanning = .false.
            if (allocated(ringposi)) deallocate (ringposi)
            if (allocated(ringposj)) deallocate (ringposj)
            num_found_contours = num_found_contours + 1
            closed_contour = 'y'
          endif

        enddo single_contour_scan_loop

        do insingle = 1,num_pts_in_one_contour
          num_pts_in_all_contours = num_pts_in_all_contours + 1
          inall = num_pts_in_all_contours
          hold_mask_i_loc(inall) = temp_mask_i_loc(insingle) 
          hold_mask_j_loc(inall) = temp_mask_j_loc(insingle) 
        enddo

      enddo successive_contours_loop

      print *,' '
      print *,'END SUM: num of iterations = ',isc_count

      do nm = 1,num_pts_in_all_contours
        im = hold_mask_i_loc(nm)
        jm = hold_mask_j_loc(nm)
        masked_out(im,jm) = .true.
      enddo
c
      return
      end

c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine sphere_of_influence (imax,jmax,ix,jx,fxy,valid_pt
     &        ,masked_out,cmaxmin,trkrinfo,search_cutoff,isoiret)
c
c     ABSTRACT: The purpose of this subroutine is to mask out points
c     surrounding a targeted max or min so that subsequent calls to 
c     the calling subroutine will not attempt to pick this low as a
c     prospective center.  For example, if find_all_maxmins picks out 
c     a 968 mb low, a subsequent loop in that subroutine may pick out
c     a gridpoint only a couple degrees away that is 984 mb.  Of 
c     course, the  check_closed_contour would return a negative for 
c     that 984 mb point since it would eventually start down towards
c     our 968 mb low, but using this routine helps to eliminate 
c     points and get us to the end of the loop faster.
c
c     The algorithm works as follows.  We check in octants all around
c     the center (ix,jx) position beginning at the center position 
c     for each octant and continuing out from the center until we 
c     find a row/column that begins to show the gradient tilting away 
c     from our center (even if it's at just one point).  For example, 
c     with pressure, as we move away from the low center, the pressure
c     should keep going up, but at some point the pressure will start
c     to decrease as we approach another nearby low, and it is at 
c     that point that the algorithm marks the end of the influence 
c     for our targeted low in that octant.  The octants are arranged 
c     as shown in the diagram below.  Note that the edge points from 
c     one octant do overlap with those of each adjoining octant, but 
c     that redundancy is no big deal.
c
c               + + + + + + + + + + + + + + + +
c               + 7 + + + + + + + + + + + + + +
c               + 7 7 8 8 8 8 1 1 1 1 1 1 + + +
c               + 7 7 7 8 8 8 1 1 1 1 1 + + + +
c               + 7 7 7 7 8 8 1 1 1 1 2 + + + +
c               + 7 7 7 7 7 8 1 1 1 2 2 + + + +
c               + 7 7 7 7 7 7 1 1 2 2 2 + + + +
c               + 7 6 6 6 6 6 X 2 2 2 2 3 3 + +
c               + + 6 6 6 6 5 4 3 3 3 3 3 3 + +
c               + + 6 6 6 5 5 4 4 3 3 3 3 3 + +
c               + + 6 6 5 5 5 4 4 4 3 3 3 3 + +
c               + + 6 5 5 5 5 4 4 4 4 3 3 3 + +
c               + + 5 5 5 5 5 4 4 4 4 4 3 3 + +
c               + + + + + + + 4 4 4 4 4 4 3 + +
c               + + + + + + + + + + + + + + + +
c               + + + + + + + + + + + + + + + +
c
c     !!! IMPORTANT NOTE: Remember that our grids go from north to 
c     south, so that, on a global grid, j=1 is at 90N and j=jmax is
c     at 90S (j increases going southward).

      USE trkrparms

      implicit none

      type (trackstuff) trkrinfo
      integer    iprevct,imax,jmax,ix,jx,is,js,iinc,jinc,ip,jp
      integer    istart,iend,jstart,jend,ig,jg,ioct,ngct
      integer    i,ii,jj,n,ica,icia,icja,isoiret,ipa,ict
      character(*) cmaxmin
      logical(1) masked_out(imax,jmax),valid_pt(imax,jmax)
      logical(1) gradient_still_okay
      real       fxy(imax,jmax)
      real,allocatable :: previous(:),current(:)
      real       prevmax,prevmin,search_cutoff
      integer,allocatable :: currenti(:),currentj(:)

c     First, mask out the input data point itself.  The remaining code
c     below this line then determines how many points around this point
c     should also be masked out as being within this point's sphere
c     of influence.

      masked_out(ix,jx) = .true.

c     Here is an explanation for the 6 elements in each line of 
c     the "select case (ioct)" statement below:
c
c     is:   The increment you must go in the i-direction to get to the
c           starting point for the next row or column in the octant.
c     js:   The increment you must go in the j-direction to get to the
c           starting point for the next row or column in the octant.
c     iinc: Once you are at the starting point for this next row or 
c           column, this increment tells you how to move in the 
c           i-direction.
c     jinc: Once you are at the starting point for this next row or 
c           column, this increment tells you how to move in the 
c           j-direction.
c     ip,jp: These are the unit indeces that are used in a do loop
c           statement to indicate if the loop should increment upwards
c           or decrement downwards.

      octant_loop: do ioct = 1,8

        select case (ioct)
          case (1); is=  0; js= -1; iinc=  1; jinc=  0; ip=  1; jp=  1;
          case (2); is=  1; js=  0; iinc=  0; jinc= -1; ip=  1; jp= -1;
          case (3); is=  1; js=  0; iinc=  0; jinc=  1; ip=  1; jp=  1;
          case (4); is=  0; js=  1; iinc=  1; jinc=  0; ip=  1; jp=  1;
          case (5); is=  0; js=  1; iinc= -1; jinc=  0; ip= -1; jp=  1;
          case (6); is= -1; js=  0; iinc=  0; jinc=  1; ip=  1; jp=  1;
          case (7); is= -1; js=  0; iinc=  0; jinc= -1; ip=  1; jp= -1;
          case (8); is=  0; js= -1; iinc= -1; jinc=  0; ip= -1; jp=  1;
        end select

c       In this next "gradient_loop", we are only looking at the 
c       points surrounding our max/min center location.  In each 
c       iteration of gradient_loop, we analyze the next row or column
c       out from the max/min center (see the diagram in the Abstract 
c       above to determine if it's a row or a column for the given 
c       octant) and compare the values in that row/column with the 
c       values from the previous row/column to see if the gradient 
c       is continuing in the right direction or not.

        gradient_still_okay = .true.
        ict = 1

        gradient_loop: do while (gradient_still_okay)

          ! Allocate "previous" with a size of ict (it makes the
          ! programming easier), even though the actual number of items
          ! to be entered into this array may only be ngct, which will
          ! always be <= ict.

          if (allocated(previous)) deallocate (previous)
          allocate (previous(ict),stat=ipa)
          if (ipa /= 0) then
            print *,' '
            print *,'!!! ERROR in sphere_of_influence allocating'
            print *,'!!! the "previous" array.  ipa = ',ipa
            print *,' '
            STOP 98
          endif

          if (ict == 1) then
            previous(1) = fxy(ix,jx)
            iprevct = 1
          else
            iprevct = ngct
            do n = 1,iprevct
              previous(n) = current(n)
              masked_out(currenti(n),currentj(n)) = .true.
            enddo
          endif

          if (allocated(current))  deallocate (current)
          if (allocated(currenti)) deallocate (currenti)
          if (allocated(currentj)) deallocate (currentj)
          allocate (current(ict+1),stat=ica)
          allocate (currenti(ict+1),stat=icia)
          allocate (currentj(ict+1),stat=icja)
          if (ica /= 0 .or. icia /= 0 .or. icja /= 0) then
            print *,' '
            print *,'!!! ERROR in sphere_of_influence allocating'
            print *,'!!! various "current" arrays.  ica = ',ica
            print *,'!!! icia= ',icia,' icja= ',icja
            print *,' '
            STOP 98
          endif

          istart = ix + (ict * is)
          iend   = istart + (ict * iinc)
          jstart = jx + (ict * js)
          jend   = jstart + (ict * jinc)

          ngct = 0
          iloop: do ii = istart,iend,ip
            jloop: do jj = jstart,jend,jp

              ig = ii
              jg = jj

              if (ig > imax) then     ! If wrapping east of GM
                if (trkrinfo%gridtype == 'global') then
                  ig = ig - imax   
                else
                  ! We have hit the grid boundary of a regional grid,
                  ! so we will just consider the  sphere of influence
                  ! to end here at the last row/column we checked...
                  cycle octant_loop
                endif
              endif

              if (ig < 1) then        ! If wrapping west of GM
                if (trkrinfo%gridtype == 'global') then
                  ig = imax + ig
                else
                  ! We have hit the grid boundary of a regional grid,
                  ! so we will just consider the  sphere of influence
                  ! to end here at the last row/column we checked...
                  cycle octant_loop
                endif
              endif

              if (jg > jmax .or. jg < 1) then
                ! We have hit the northern or southern boundary of the
                ! grid, so we'll just consider the  sphere of influence
                ! to end here at the last row/column we checked...
                  cycle octant_loop
              endif

              if (valid_pt(ig,jg)) then
                ngct = ngct + 1
                current(ngct)  = fxy(ig,jg)
                currenti(ngct) = ig
                currentj(ngct) = jg
              endif

            enddo jloop
          enddo iloop

c         Make sure gradient continues:
c         As we spread out in each new row or column from the center
c         max/min, we want to verify if our gradient is still true.
c         So in the case of min pressure, we find the min pressure from
c         the previous row/column, and then we search through the 
c         pressure values in the current row/column and make sure that
c         there are none which are lower than the min from the previous
c         row/column.  If there were, that would indicate that the 
c         gradient is changing and decreasing again down towards perhaps
c         another nearby low, so stop searching.

          ! First, find the max or min value in the previous row/column 
          ! in this octant.

          if (cmaxmin == 'min') then
            prevmin = 9.99e20
            do i = 1,iprevct
              if (previous(i) < prevmin) prevmin = previous(i)
            enddo
          else
            prevmax = -9.99e20
            do i = 1,iprevct
              if (previous(i) > prevmax) prevmax = previous(i)
            enddo
          endif

          ! Now check the current row/column and make sure that none 
          ! of the values is less than (greater than) the minimum 
          ! (maximum) value from the previous row/column found in 
          ! the previous "if" statement just executed.

          if (cmaxmin == 'min') then
            minloop: do i = 1,ngct
              if (current(i) < prevmin .or. 
     &            current(i) > search_cutoff) then
                ! The gradient has changed, or we've found a value 
                ! greater than the search cutoff, stop searching....
                gradient_still_okay = .false.
                exit minloop
              endif
            enddo minloop
          else
            maxloop: do i = 1,ngct
              if (current(i) > prevmax .or.
     &            current(i) < search_cutoff) then
                ! The gradient has changed, or we've found a value
                ! less than the search cutoff,, stop searching....
                gradient_still_okay = .false.
                exit maxloop
              endif
            enddo maxloop
          endif

          ict = ict + 1

        enddo gradient_loop

      enddo octant_loop

      if (allocated(current))  deallocate (current)
      if (allocated(previous)) deallocate (previous)
      if (allocated(currenti)) deallocate (currenti)
      if (allocated(currentj)) deallocate (currentj)
c
      return
      end

c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine get_ijplus1_check_wrap (imax,jmax,i,j,iplus1,jplus1
     &                            ,iminus1,jminus1,trkrinfo,igicwret)
c
c     ABSTRACT: This subroutine takes an (i,j) position input and 
c     returns the four neighboring (i,j) points to the east, south, 
c     west and north.  The routine checks for wrap around the GM, so 
c     that if, for example, you are on a global 360x181 grid and you
c     are at point i=360, then i+1 = 361, so you need something to 
c     adjust that back to i = 1.  Likewise, if you are at i=1 and 
c     looking for point i-1, it will adjust it to be point 360 
c     instead of the meaningless point 0 (i=0).

      USE trkrparms

      implicit none

      type (trackstuff) trkrinfo
      integer   i,j,imax,jmax,iplus1,jplus1,iminus1,jminus1,igicwret

      igicwret = 0

      jplus1  = j + 1
      jminus1 = j - 1
      iplus1  = i + 1
      iminus1 = i - 1

      if (iplus1 > imax) then
        if (trkrinfo%gridtype == 'global') then
          iplus1 = iplus1 - imax   ! If wrapping east of GM
        else
          print *,' '
          print *,'!!! ERROR: Error in get_ijplus1_check_wrap.  The'
          print *,'!!!        user-requested eastern search boundary'
          print *,'!!!        is too close to the eastern bounds of'
          print *,'!!!        this regional grid.  When we check '
          print *,'!!!        neighboring points, we are going past'
          print *,'!!!        the edge of the grid by one point. '
          print *,'!!!        Cut back your requested eastern  '
          print *,'!!!        boundary by a degree or 2 in the  '
          print *,'!!!        script and resubmit....'
          print *,'!!!         '
          print *,'!!!   imax of regional grid    = ',imax
          print *,'!!!   User-requested eastern i = ',iplus1
          print *,' '
          igicwret = 98
          return
        endif
      endif

      if (iminus1 < 1) then
        if (trkrinfo%gridtype == 'global') then
          iminus1 = imax + iminus1  ! If wrapping west of GM
        else
          print *,' '
          print *,'!!! ERROR: Error in get_ijplus1_check_wrap.  The'
          print *,'!!!        user-requested western search boundary'
          print *,'!!!        is too close to the western bounds of'
          print *,'!!!        this regional grid.  When we check '
          print *,'!!!        neighboring points, we are going past'
          print *,'!!!        the edge of the grid by one point. '
          print *,'!!!        Cut back your requested western  '
          print *,'!!!        boundary by a degree or 2 in the  '
          print *,'!!!        script and resubmit....'
          print *,'!!!         '
          print *,'!!!   User-requested western i = ',iminus1
          print *,' '
          igicwret = 98
          return
        endif
      endif

      if (jplus1 > jmax .or. jminus1 < 1) then
        print *,' '
        print *,'!!! ERROR: Error in get_ijplus1_check_wrap.  The user-'
        print *,'!!!        requested northern or southern search'
        print *,'!!!        boundary is too close to the bounds of the'
        print *,'!!!        grid.  Cut back your requested northern or'
        print *,'!!!        southern boundary by a degree or 2 in the'
        print *,'!!!        script and resubmit....'
        print *,'!!! '
        print *,'!!!   User-requested northern j = ',jminus1
        print *,'!!!   User-requested southern j = ',jplus1
        print *,'!!!   jmax of grid              = ',jmax
        print *,'    '
        igicwret = 91
        return
      endif
    
      return
      end
