      program saber_obserr
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!     
! !ROUTINE: saber_obserr: write updated obs errors to PREPBUFR file
!     
! !INTERFACE:
!     
!     Usage: saber_obserr [-b] input_prepbufr output_prepbufr
!     
! !USES:
!     
      use m_saber_err, only: saber_err_init
      use m_saber_err, only: saber_err_final
      use m_saber_err, only: saber_err_set
      use m_saber_err, only: saber_err_get
      use m_saber_err, only: saber_err_get_bounds
      implicit NONE
!     
!     link to libbfr_r4i4.a library

! !DESCRIPTION:  simple routine to read prepbufr and update
!                  observation errors - will also reset QM for
!                  observations with pressures less than min-pressure
!
! !REVISION HISTORY:
!     
!     20Mar2007    Meta   Initial version
!     07Feb2008    Meta   New version for MLS temperature
!     11Feb2008    Meta   add command line flag for options - use
!                           precision or precision+bias (-b)
!     04Jan2024    Todling Hacked for SABER
!     
!EOP
!-----------------------------------------------------------------------      

      real*8    OBS(3,255), QMS(2,255),OES(2,255)
      CHARACTER*80 QMSTR, OBSTR, OESTR
      CHARACTER*8  SUBSET, stnid
      integer*4 nymd, nhms
      real*8 bmiss, hbmiss
      real*8 error_factor
      
      integer idate, mydate
      
      integer ilev, jlev, klev, nlev
      integer iret, jret, kret, nret , lret

      integer is, i, k
      character*255 argv, outfile

      integer ip
      real berr(-24:30), bterm, plevm
      real pobs, sigo
      logical do_bterm
      logical first_param

      data berr/ 1.5, 1.45, 1.42, 1.38, 1.33, 1.29, 1.25, 1.21, 1.17,
     &   1.13, 1.08, 1.04, 41*1.0, 2.0, 2.5/ 
      
      data plevm/0.04/
      data do_bterm/.false./

      DATA OBSTR/'POB TOB SABERPT'/
      DATA QMSTR/'PQM TQM '/
      DATA OESTR/'POE TOE '/

      integer lubfi, lubfo
      data lubfi /10/, lubfo /20/
      
      integer*4 iargc

      first_param=.false.
      error_factor = 1.0 ! Jan06-

      is = iargc()
      if (is .lt. 2) then
        print *, 'Usage: saber_obserr.x [-b|-p1] inbfrfile outbfrfile'
        stop
      endif

      i = 0

      call getarg( 1, argv)
      if (index(argv,'-b') > 0) then
         do_bterm = .true.
         i = i + 1
      else if (index(argv,'-p1') > 0) then
         first_param =.true.
         print *, 'First attempt at setting sigo ...'
         i = i + 1
      endif

      if (i>0) call getarg(1+i,argv)
      open( unit=lubfi,file=argv,form='unformatted')

      call getarg(2+i,outfile)
      
      
      bmiss = 10.d10
      hbmiss = 0.5 * bmiss
      
      call datelen(10)
      
      CALL OPENBF(LUBFI,'IN',LUBFI)
      
      subset = ''
      idate = -999
      iret = 0

      CALL READMG(LUBFI,SUBSET,IDATE,IRET)
      if (trim(subset) .ne. 'SABER') then
         print *,'input file not SABER data, exiting. subset=',subset
         stop
      endif

      if  (iret .eq. 0 ) then
      

!   data was found, so open output file
!   -----------------------------------
          open(lubfo,file=trim(outfile),form='unformatted')
          call openbf(lubfo,'OUT', lubfi)
          
      else
          print *,'Desired data was not found in input file'
          stop
      endif      

      if (first_param) then
         call saber_err_init
         call saber_err_set
      endif

C  LOOP THROUGH THE INPUT MESSAGES - READ THE NEXT SUBSET
C  ------------------------------------------------------

      print *, 'Writing new dataset ',trim(outfile)
      lret = 0

      do while (lret .eq. 0) 
         CALL READSB(LUBFI,LRET)
         IF(LRET .eq. 0) then
            call openmb(lubfo, subset, idate)

            call ufbcpy(lubfi, lubfo)

            CALL UFBINT(LUBFI,OBS,3, 255,jlev,OBSTR)
            CALL UFBINT(LUBFI,QMS,2, 255,klev,QMSTR)

            if (jlev .ne. klev) print *, 'inconsistent levels'
            
            oes = bmiss
            
            if (jlev .gt. 0) then
               do k = 1,jlev

! set obs error based on precision, or combination of precision and 'bias'
!  accuracy from table in quality document.

                  if (do_bterm) then                     
! use precision + bias term
!                    ip = nint(alog10(obs(1,k))*12.)
                     ip = nint(dlog10(obs(1,k))*12.)
                     if (ip < -24) then
                        bterm  = 1.5
                     else if (ip > 30) then
                        bterm = 2.5
                     else
                        bterm = berr(ip)
                     endif
                     
                     if(obs(3,k) .lt. bmiss) then
                        oes(2,k) = sqrt(bterm + obs(3,k)*obs(3,k))
                     endif
                  else if (first_param) then                     
                     pobs = obs(1,k)
                     call saber_err_get(pobs,sigo)
                     oes(2,k) = sigo * error_factor
                     print *, 'saber_obserr: lev = ', pobs, 'error: ', oes(2,k)
                  else
! only copy precision value
                     oes(2,k) = obs(3,k)
                  endif
               enddo

               call ufbint(lubfo,oes,2, jlev,jret, oestr)
               if (jlev .ne. jret) print *,'inconsistent 3'

            endif
            
            call writsb(lubfo)
            
         else if (lret .eq. -1) then
            idate = -999
            iret = 0
              CALL READMG(LUBFI,SUBSET,IDATE,IRET)
            lret = iret
         else
            print *,'lret has a strange value of ',lret
         endif
      end do
      if ( first_param ) then
         call saber_err_final
      endif
      call closbf(lubfi)
      call closbf(lubfo)
      stop
      end program saber_obserr
