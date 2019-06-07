      program scanbuf

      implicit none

!
!  Scan NCEP BUFR files and write out how many reports of each type
!
      character*8 subset, saved         ! subset names (current & prev.)
      character*55 descr               ! Table A description of subset
      character*255 cfile, argv        ! file name from command line
      integer idat10                   ! 10 digit date/time from subset
      integer iret                     ! return code
      integer lunit                    ! input unit number
      integer lprint                   ! output unit number
      integer argc                     ! used to get args from command line
      integer knt                      ! count of records in current subset
      integer kmsg, ksub
      integer nmsub

      integer*4 iargc

      lunit = 35                       !  input data file

      

      argc = iargc()
      if ( argc < 1 ) then
         print *, 'program needs a BUFR filename as input'
         stop
      endif
      call GetArg ( 1_4, argv )
      cfile = argv
      open( unit=lunit, file=cfile,form='unformatted')
      if (argc .ge. 2) then
! second arg is output print file
        call GetArg ( 2_4, argv )
        lprint = 36
        open( unit=lprint,file=argv,form='formatted')
      else
        lprint = 6
      endif
      call datelen(10)
      CALL OPENBF(LUNIT,'IN',LUNIT)
      call readmg(lunit,subset,idat10,iret)
      write(lprint,*) 'date:',idat10, iret
      saved = subset
      knt = 0
      do while (iret .eq. 0)
        ksub = nmsub(lunit)
        knt = knt + ksub
        call readmg(lunit,subset,idat10,iret)
        if (subset .ne. saved) then 
           if (knt .gt. 0) write(lprint,'(a8,i8)') saved,knt
           knt = 0
           saved = subset
        endif
      enddo
      if (knt .gt. 0) write(lprint,'(a8,i8)') saved,knt
      stop
      end

