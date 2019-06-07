!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: BUFR_TRANAVHRR
!   PRGMMR: KEYSER           ORG: NP22        DATE: 2015-09-09
!
! ABSTRACT: Reads in raw AVHRR GAC 1B data file, Reformats and packs into
!   BUFR files.  Two output BUFR files are possible, one containing clear and
!   oceanic data and one containing everything else (i.e., cloudy or overland
!   data).
!
! PROGRAM HISTORY LOG:
! 2003-01-29  Xu Li   Original author
! 2004-10-01  Xu Li   Update the 3rd infra-red calibration scale fator (6 -> 7)
!                     with KLM to N.  Add one cloud test based on SST
!                     climatology.  Save data records into land & detected
!                     cloudy and others seperately into two bufr files.  Assign
!                     time to each data record (to scan line only before).
!                     Increase the time precision upto 0.01 miliseconds.  Add
!                     sat id for NOAA-18 processing.  Use operational
!                     calibration for visible channels (rather than pre-launch
!                     as before). Change Satellite ID: 16 => 207; 17 => 208; 18
!                     => 209. Some re-organization of the code
! 2006-01-20, Xu Li   Remove the read of SST climatology (tranavhrr.f90).
!                     Remove cloud detection with SST climatology (avhrr.f90).
!                     Add the decode of CLAVR cloud flag and save data records
!                     with flag 0 & 1 (as clear) and 2 & 3 seperately (as
!                     cloudy) (avhrr.f90).  Add one parameter (CLAVR) in
!                     BUFR table for CLAVR cloud flags.  Use new precision for
!                     SECO and CLATH/CLONH.
! 2006-10-19  Keyser  Modified to integrate into general format for codes which
!                     process data into BUFR under satellite ingest scripts.
!                     Improved docblocks and comments.
! 2009-07-31  Keyser  Modified to handle METOP-2 and NOAA-19 satellites.
! 2012-11-13  Keyser  Changes to run on WCOSS. Modified to handle METOP-1
!                     satellite. Do not encode BUFR dx table messages into top
!                     of output file(s).
! 2014-01-20  Keyser  Minor changes.
! 2015-09-09  Stokes  Prescribe bufr missing value. Preserve precision of real*8
!                     data passed to bufrlib routines and other minor corrections
!                     to reduce risk of memory corruption.
!
! 2017-11-07  J Kim   Install inside GEOSadas-5_17_0 
!                     some input file name change and accurate control on time bound
!
! USAGE:
!   INPUT FILES:
!     UNIT 05  - Standard input. W3TRNARG parses arguments from standard input
!     UNIT 11  - NESDIS binary AVHRR GAC file containing 1B radiance data
!     UNIT 20  - BUFR table file containing BUFR tables A, B, and D
!     UNIT 37  - Land-sea mask on 1/16'th degree grid
!
!   OUTPUT FILES:
!     UNIT 06  - Standard output print
!     UNIT 51  - Output BUFR file containing clear and oceanic AVHRR GAC 1B
!                radiances (TRANJB will place the BUFR messages into the proper
!                tanks)
!     UNIT 52  - Output BUFR file containing cloudy or overland AVHRR GAC 1B
!                radiances (TRANJB will place the BUFR messages into the proper
!                tanks)
!
!   SUBPROGRAMS CALLED:
!     UNIQUE:  - GAC_1B_PROC (module)  AVHRR    BUFR1B   GAC_LBC  AVH_ICON
!                LAG      DATTIM
!                
!     LIBRARY:
!       W3NCO  - W3TAGB   W3TRNARG W3TAGE   ERREXIT  W3MOVDAT W3FS26
!     BUFRLIB  - DATELEN  OPENBF   MAXOUT   CLOSMG   OPENMB   UFBINT
!                UFBREP   WRITCP
!
!   EXIT STATES:
!     COND =   0 - Successful run
!          =   1 - Unable to parse input arguments in W3TRNARG
!          =   2 - Error opening raw 1B AVHRR file
!          =   3 - Invalid satellite id read in
!          =   4 - Input data file does not contain AVHRR/GAC data
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 (free format)
!   MACHINE:  NCEP WCOSS
!
!$$$


      PROGRAM BUFR_TRANAVHRR

!  Module gac_1b_proc contains include files to set up parameters and to
!   define raw AVHRR GAC 1B file header and data record
!  ---------------------------------------------------------------------

      use gac_1b_proc

      implicit none

! local variables
      integer, parameter :: file_id_lsMask = 37 ! land-sea-mask 
      real(8), dimension(4) :: beg_date, end_date

      open(unit=101,file='infile')
      read(101,*) 
      read(101,*) 
      read(101,*) 
      read(101,*) 
      read(101,*) beg_date(1),beg_date(2),beg_date(3),beg_date(4),&
                  end_date(1),end_date(2),end_date(3),end_date(4)
      close(101)
      time_min =(beg_date(1)*1E8 + beg_date(2)*1E6 + beg_date(3)*1E4 + beg_date(4)*1E2)*1E2
      time_max =(end_date(1)*1E8 + end_date(2)*1E6 + end_date(3)*1E4 + end_date(4)*1E2)*1E2
!-----------------------------------------------------------------------
      call w3tagb('BUFR_TRANAVHRR',2016,0106,1450,'NP22')
      print *, ' '
      print *, '  WELCOME TO BUFR_TRANAVHRR - VERSION 04-15-2016'
      print *, ' '

      call datelen(10)
      call setbmiss(10e8_8)

      call w3trnarg(subdir,lsubdr,tankid,ltnkid,appchr,lapchr,tlflag,jdate, &
                    kdate,ierr)
      
!.......................................................................
      if(ierr.ne.0) then
         write(6,&
 '('' UNABLE TO PARSE ARGS TO TRANSLATION ROUTINE - RETURN CODE = '',i5)') ierr
         call w3tage('BUFR_TRANAVHRR')
         call errexit(ierr)
      endif
!.......................................................................
      subset = 'NC'//subdir(lsubdr-2:lsubdr)//tankid(ltnkid-2:ltnkid)
!!!!! call openbf (wbf_sea_a_lcr,'OUT',lundx) ! Open new BUFR file for writing
      call openbf (wbf_sea_a_lcr,'NODX',lundx)! Open new BUFR file for writing
                                              !  clear and oceanic reports

      call w3trnarg(subdirp,lsubdrp,tankidp,ltnkidp,appchrp,lapchrp,tlflagp,&
                    jdate,kdate,ierr)
!.......................................................................
      if(ierr.ne.0) then
         write(6,&
 '('' UNABLE TO PARSE ARGS TO TRANSLATION ROUTINE - RETURN CODE = '',i5)') ierr
         call w3tage('BUFR_TRANAVHRR')
         call errexit(ierr)
      endif
!.......................................................................
      subsetp = 'NC'//subdirp(lsubdrp-2:lsubdrp)//tankidp(ltnkidp-2:ltnkidp)
!!!!! call openbf (wbf_lnd_o_cld,'OUT',lundx) ! Open new BUFR file for writing
      call openbf (wbf_lnd_o_cld,'NODX',lundx)! Open new BUFR file for writing
                                              !  cloudy or overland reports

!  Read Land Sea Mask data (1/16 degree)
!  -------------------------------------

      open(file_id_lsMask,file="lnd_sea_mask_dat",form="formatted",status="old")

      read(37,'(5761I1)') lst

      call maxout(20000)                      ! Set BUFR message length to 20K
                                              !  (default is 10K)

!  Open raw 1B AVHRR GAC data file
!  -------------------------------

      open(lu1b,file="input.bufr",recl=reclavh/rfac,access='direct',IOSTAT=ios,status='old') ! <--- NEEDS clean up too for scripting

      if(ios.ne.0) then
         print *,'*****ERROR opening raw 1b AVHRR GAC file',ios
         print *,'*****STOP 2'
         call w3tage('BUFR_TRANAVHRR')
         call errexit(2)
      endif

      write(stdout,*)' Begin decoding AVHRR GAC 1B data'

!  Write header record to standard output
!  --------------------------------------

      write(stdout,*)' '
      write(stdout,*)'Header information below'
      write(stdout,*)'nreal,mch = ',nreal,mch
      write(stdout,*)'ntot      = ',ntot

!  Read GAC data header and check primary modes
!  --------------------------------------------

      nri = 1                                 ! Counting the all record number
!!!!! read(lu1b,rec=nri,err=1900) avh_hd ! (does not work right on WCOSS)
      read(lu1b,rec=nri,err=1900) &
       avh_h_siteid , avh_h_blank , avh_h_l1bversnb , &
       avh_h_l1bversyr , avh_h_l1bversdy , avh_h_reclg , &
       avh_h_blksz , avh_h_hdrcnt , avh_h_filler0 , &
       avh_h_dataname , avh_h_prblkid , avh_h_satid , &
       avh_h_instid , avh_h_datatyp , avh_h_tipsrc , &
       avh_h_startdatajd , avh_h_startdatayr , &
       avh_h_startdatady , avh_h_startdatatime , &
       avh_h_enddatajd , avh_h_enddatayr , avh_h_enddatady , &
       avh_h_enddatatime , avh_h_cpidsyr , avh_h_cpidsdy , &
       avh_h_filler1 , avh_h_inststat1 , avh_h_filler2 , &
       avh_h_statchrecnb , avh_h_inststat2 , avh_h_scnlin , &
       avh_h_callocscnlin , avh_h_misscnlin , avh_h_datagaps , &
       avh_h_okdatafr , avh_h_pacsparityerr , &
       avh_h_auxsyncerrsum , avh_h_timeseqerr , &
       avh_h_timeseqerrcode , avh_h_socclockupind , &
       avh_h_locerrind , avh_h_locerrcode , &
       avh_h_pacsstatfield  , avh_h_pacsdatasrc , &
       avh_h_filler3 , avh_h_spare1 , avh_h_spare2 , &
       avh_h_filler4 , avh_h_racalind , avh_h_solarcalyr , &
       avh_h_solarcaldy , avh_h_pcalalgind, avh_h_pcalalgopt , &
       avh_h_scalalgind , avh_h_scalalgopt , avh_h_irttcoef , &
       avh_h_filler5 , avh_h_albcnv , avh_h_radtempcnv , &
       avh_h_filler6 , avh_h_modelid , avh_h_nadloctol , &
       avh_h_locbit, avh_h_filler7 , avh_h_rollerr , &
       avh_h_pitcherr , avh_h_yawerr , avh_h_epoyr , &
       avh_h_epody , avh_h_epotime , avh_h_smaxis , &
       avh_h_eccen , avh_h_incli , avh_h_argper , &
       avh_h_rascnod , avh_h_manom , avh_h_xpos , avh_h_ypos , &
       avh_h_zpos , avh_h_xvel , avh_h_yvel , avh_h_zvel , &
       avh_h_earthsun , avh_h_filler8 , avh_h_pchtemp , &
       avh_h_reserved1 , avh_h_pchtempext , avh_h_reserved2 , &
       avh_h_pchpow , avh_h_reserved3 , avh_h_rdtemp , &
       avh_h_reserved4 , avh_h_bbtemp1 , avh_h_reserved5 , &
       avh_h_bbtemp2 , avh_h_reserved6 , avh_h_bbtemp3 , &
       avh_h_reserved7 , avh_h_bbtemp4 , avh_h_reserved8 , &
       avh_h_eleccur , avh_h_reserved9 , avh_h_motorcur , &
       avh_h_reserved10 , avh_h_earthpos , avh_h_reserved11 , &
       avh_h_electemp , avh_h_reserved12 , avh_h_chtemp , &
       avh_h_reserved13  , avh_h_bptemp , avh_h_reserved14 , &
       avh_h_mhtemp , avh_h_reserved15 , avh_h_adcontemp , &
       avh_h_reserved16 , avh_h_d4bvolt , avh_h_reserved17 , &
       avh_h_d5bvolt , avh_h_reserved18 , avh_h_bbtempchn3B , &
       avh_h_reserved19 , avh_h_bbtempchn4 , &
       avh_h_reserved20 , avh_h_bbtempchn5 , &
       avh_h_reserved21 , avh_h_refvolt , avh_h_reserved22 , &
       avh_h_filler9


!  See if valid satellite, if so convert to BUFR code figure value
!  ---------------------------------------------------------------

      jsat  = avh_h_satid                     ! NOAA spacecraft id code
      if (jsat.eq.4) then                     ! NOAA-15 (K)
         jsat0 = jsat
         jsat  = 206
         write(stdout,*) '***WARNING:   reset satellite id from ', jsat0,&
                         ' to ',jsat
      elseif (jsat.eq.2) then                 ! NOAA-16 (L)
         jsat0 = jsat
         jsat  = 207
         write(stdout,*) '***WARNING:   reset satellite id from ', jsat0,&
                         ' to ',jsat
      elseif (jsat.eq.6) then                 ! NOAA-17 (M)
         jsat0 = jsat
         jsat  = 208
         write(stdout,*) '***WARNING:   reset satellite id from ', jsat0,&
                         ' to ',jsat
      elseif (jsat.eq.7) then                 ! NOAA-18 (N)
         jsat0 = jsat
         jsat  = 209
         write(stdout,*) '***WARNING:   reset satellite id from ', jsat0,&
                         ' to ',jsat
      elseif (jsat.eq.8) then                 ! NOAA-19 (P)
         jsat0 = jsat
         jsat  = 223
         write(stdout,*) '***WARNING:   reset satellite id from ', jsat0,&
                         ' to ',jsat
      elseif (jsat.eq.12) then                ! METOP-2 (M2)
         jsat0 = jsat
         jsat  =   4
         write(stdout,*) '***WARNING:   reset satellite id from ', jsat0,&
                         ' to ',jsat
      elseif (jsat.eq.11) then                ! METOP-1 (M1)
         jsat0 = jsat
         jsat  =   3
         write(stdout,*) '***WARNING:   reset satellite id from ', jsat0,&
                         ' to ',jsat
      else
         print *,'*****INVALID satellite id read in ',jsat
         print *,'*****STOP 3'
         call w3tage('BUFR_TRANAVHRR')
         call errexit(3)
      endif

!  If data type is not that for AVHRR GAC, exit program
!  ----------------------------------------------------

      jtype = avh_h_datatyp                   ! data type (2 = AVHRR GAC )

      if (jtype.ne.2) then
         print *,'*****ERROR: Input data file does not contain AVHRR/GAC ',&
                 'data (type=2) - data type = ',jtype
         print *,'*****STOP 4'
         call w3tage('BUFR_TRANAVHRR')
         call errexit(4)
      endif

      write(stdout,*) 'Data and satellite type = ',jtype,jsat

!  Extract number of data records in data set
!  ------------------------------------------

      nrecs = avh_h_scnlin                    ! no. of data records in data set
      nscan = avh_h_callocscnlin              ! count of calibrated, earth
                                              !  located scans

!  ===================================
!  Main loop over number of scan lines
!  ===================================

      nlo = 0                                 ! Initialize no. of scan lines (0)

 1200 continue

         nri = nri + 1                        ! Increment all record counter

                                              ! Read in data record
!!!!!!!! read(lu1b,rec=nri,err=1600) avh_dt ! (does not work right on WCOSS)
         read(lu1b,rec=nri,err=1600) &
          avh_scnlin , avh_scnlinyr , avh_scnlindy , &
          avh_clockdrift , avh_scnlintime , avh_scnlinbit , &
          avh_filler0 , avh_qualind , avh_scnlinqual , &
          avh_calqual , avh_cbiterr , avh_filler1 , avh_calvis , &
          avh_calir , avh_filler2 , avh_navstat , &
          avh_attangtime , avh_rollang , avh_pitchang , &
          avh_yawang , avh_scalti , avh_ang , avh_filler3 , &
          avh_pos , avh_filler4 ,  avh_telem , avh_filler5 , &
          avh_hrpt , avh_filler6 , avh_bitflag1, &
          avh_dbdata,avh_filler7, avh_bitflag2,avh_instemp, &
          avh_filler8,  clv_bit, clv_rev, ccmc, avh_filler9


         nrec = nrec + 1                      ! Increment good record counter
         nlo  = nlo + 1                       ! Increment scan line counter
         line = nlo                           ! Number of scan line

!  Decode and then write the scan lines of record into BUFR file
!  -------------------------------------------------------------

         call avhrr(subset,subsetp)
!
!     Goto top of loop to read next scan line
!   
      if( nri < maxread ) go to 1200

      go to 200

  100 continue

      print*,'END OF FILE REACHED FOR UNIT ',lu1b
!     print*,'IOSTAT=',ierr_r
      print*,' nri = ',nri
      print*,' nlo = ',nlo

  200 continue

!     Done reading from 1b files.  Close unit.
 1600 continue
      write(stdout,*)' '
      write(stdout,*)'Done reading raw 1b file'
      write(stdout,*)' '
      write(stdout,*)'avhrr (GAC) ingest stats'
      write(stdout,*)' no. scan lines     = ',nlo,nrecs,nscan
      write(stdout,*)' no. records written= ',nrec
!
      write(stdout,*)' '
      write(stdout,*)'bad radiance/temperature counts per channel'
      write(stdout,1020)
 1020 format(t1,'channel',t10,'num_good',t20,'num_bad ')
      do j = 1,mch+1
         write(stdout,1030) chn_name(j),good_lines(j),bad_lines(j)
 1030    format(t1,a5,t10,I8,t18,I6)
      end do
      goto 2000
!
!
!     Error reading 1b file.
 1900 write(stdout,*)' *** error reading hdr record of rawavhrr file '
      close(lu1b)

!     call w3tage('BUFR_TRANAVHRR')
      call errexit(3)
!
!
!
!     End of GAC 1b ingest.  Close units.
 2000 continue

      close(lu1b)
      write(stdout,*)' avhrr 1b decode completed'
      write(stdout,*)' '

      call closbf(wbf_sea_a_lcr)
      call closbf(wbf_lnd_o_cld)

!DAK
      print *, 'A total of ',ilimb,' spots were skipped due to their being ',&
      'on the limb of scans'
!DAK

!
      if(nrec.eq.0) then
        write(stdout,1003)
 1003   format(/' NO RECORDS WRITTEN -- DISABLING ALL SUBSEQUENT ',  &
               'PROCESSING.'/)
        call w3tage('BUFR_TRANAVHRR')
        call errexit(253)
      endiF

      call w3tage('BUFR_TRANAVHRR')

      stop

 end program BUFR_TRANAVHRR
