      program cp_2ssi
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: cp_2ssi : read GSI obs vars from BUFR file, write to PREPBUFR
!
! !INTERFACE:
!
!      Usage:  cp_2ssi.x  nymd nhms inbfrfile outbfrfile
!
!      where
!
!      nymd      year-month-day in YYYYMMDD format
!      nhms      hour-minute-second in HHMMSS format
!      inbfrfile   input bufr file
!      outbfrfile   output PREPBUFR formatted file
!
!
! !USES:
!
      use  m_die
      
      implicit NONE
      
! !DESCRIPTION:  Sample program to read in obs from a bufr file and
!                write out in prepbufr.  This routine could be used
!                to modify the BUFR table used to encode the data, 
!                for example, or as a basis for a program to modify
!                the conventional data.  Note that the routine does
!                not handle the GOES sounder data,and includes only
!                mnemonics used in 'read_prepbufr' in the GSI.
!
!                to-do:  also add SST handling.
!
! !REVISION HISTORY:
!
!  21May2004    Meta   Initial version
!  26Jan2005    Meta   Remove i*8 coding (now should use r4i4 or r8i4)
!                      add handling of balloon drift information, add
!                      note about GOES sounder, SST not yet handled
!
!EOP
!-----------------------------------------------------------------------
      

      real(8) HDR(9)                       ! header
      real(8) OBS(8,255)                   ! observations
      real(8) QMS(7,255)                   ! quality marks
      real(8) OES(7,255)                   ! observation errors
      real(8) DRF(8,255)                   ! balloon drift information

      CHARACTER*80 HDSTR, QMSTR, OBSTR, OESTR, drift
      CHARACTER*8  SUBSET, stnid
      integer*4 nymd, nhms
      real*8 bmiss, hbmiss
      
      integer  idate, mydate
      
      integer  ilev, jlev, klev, llev, nlev
      integer  iret, jret, kret, nret , lret
      
      real poe, qoe, toe, woe, pwe
      
      integer is, i
      character*255 argv, outfile
      character*80 bufrtable
      
      data bufrtable /'prepobs_prep.bufrtable'/
      
      DATA HDSTR/'SID XOB YOB DHR TYP ELV         '/
      DATA OBSTR/'POB QOB TOB ZOB UOB VOB PWO CAT '/
      DATA QMSTR/'PQM QQM TQM ZQM WQM NUL PWQ     '/
      DATA OESTR/'POE QOE TOE NUL WOE NUL PWE     '/
      data drift/'XDR YDR HRDR                    '/
      
      integer lubfi, lubfo
      data lubfi /10/, lubfo /20/
      
      integer*4 iargc
      
      equivalence(stnid, hdr(1))
      
      is = iargc()
      if (is .lt. 4) then
         print *, 'Usage: cp_2ssi.x  nymd nhms inbfrfile outbfrfile '
         stop
      endif
      call getarg(1_4, argv)
      read(argv,*) nymd
      call getarg(2_4, argv)
      read(argv,*) nhms
      
      print *,'Requested data for date', nymd, nhms
      
      call getarg( 3_4, argv)
      open( unit=lubfi,file=argv,form='unformatted')
      
      call getarg(4_4,outfile)
      
      
      bmiss = 10.e10
      hbmiss = 0.5 * bmiss
      
      mydate = nymd * 100 + nhms / 10000
      print *,'Will try to write data for date: ',mydate
      
      call datelen(10)
      
      CALL OPENBF(LUBFI,'IN',LUBFI)
      
      subset = ''
      idate = -999
      iret = 0
      do while ( iret .eq. 0 .and. idate .ne. mydate)
         CALL READMG(LUBFI,SUBSET,IDATE,IRET)
      enddo
      
      if  (iret .eq. 0 ) then
      

!   date was found, so open output file
!   -----------------------------------
         open(lubfo,file=trim(outfile),form='unformatted')
         call openbf(lubfo,'OUT', lubfi)
          
      else
         print *,'Desired date was not found in input file'
         stop
      endif      
      
C  LOOP THROUGH THE INPUT MESSAGES - READ THE NEXT SUBSET
C  ------------------------------------------------------

      print *, 'start reading and writing data'
      lret = 0
      
      do while (lret .eq. 0) 
         CALL READSB(LUBFI,LRET)
         IF(LRET .eq. 0) then
            call openmb(lubfo, subset, idate)
            
            CALL UFBINT(LUBFI,HDR,9,  1,ilev,HDSTR)
            call ufbint(lubfo,hdr,9,  1,iret,hdstr)
            
            CALL UFBINT(LUBFI,OBS,8,  255,klev,OBSTR)
            call ufbint(lubfo,obs,8,  klev, kret, obstr)
            
            CALL UFBINT(LUBFI,QMS,7,  255,nlev,QMSTR)
            call ufbint(lubfo,qms,7, nlev,nret, qmstr)
            
            CALL UFBINT(LUBFI,OES,7,  255,jlev,OESTR)
            call ufbint(lubfo,oes,7, jlev,jret, oestr)
            
            if ( subset .eq. 'ADPUPA' ) then
               call ufbint(lubfi,drf,8,  255,llev,drift)
               if ( llev .gt. 0 )
     &              call ufbint(lubfo,drf,8, llev,jret,drift)
            endif
            call writsb(lubfo)
            
         else if (lret .eq. -1) then
            idate = -999
            iret = 0
            do while ( iret .eq. 0 .and. idate .ne. mydate)
               CALL READMG(LUBFI,SUBSET,IDATE,IRET)
            enddo
            lret = iret
         else
            print *,'lret has a strange value of ',lret
         endif
      end do
      call closbf(lubfi)
      call closbf(lubfo)
      stop
      end program cp_2ssi
      
