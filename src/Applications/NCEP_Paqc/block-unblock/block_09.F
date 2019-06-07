C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C  
C MAIN PROGRAM:  CWORDSH
C   PRGMMR: J. ATOR          ORG: NP12        DATE: 2005-11-29
C
C ABSTRACT: CONVERTS BINARY BYTE STREAM BUFR FILES BACK AND FORTH
C   FROM A FORTRAN BLOCKED FORMAT.
C
C PROGRAM HISTORY LOG:
C 1999-08-19  J. WOOLLEN  ORIGINAL VERSION FOR IMPLEMENTATION
C 2004-03-19  D. KEYSER   INCREASED SIZE OF ARRAY MBAY FROM 3000 WORDS
C                         TO 5000 WORDS TO ALLOW IT TO PROCESS BUFR
C                         MESSAGES WITH UP TO 20K BYTES
C 2005-11-29  J. ATOR     REWRITTEN USING BUFRLIB C I/O LOGIC TO HANDLE
C                         ANY INPUT BUFR FILES (INCLUDING FILES WHICH
C                         CONTAIN EXTRANEOUS CHARACTERS (E.G. BULLETIN
C                         HEADERS) AND/OR WHICH PREVIOUSLY REQUIRED THE
C                         USE OF APPLICATION PROGRAM GRABBUFR) AND TO
C                         REMOVE DIRECT LINKS TO BUFRLIB COMMON BLOCKS
C 2009-05-18  Sienkiewicz  Modify to get file names for input and output
C                          from command line.  Select 'block' or unblock
C                          through compile time definition
C 2009-05-26  Sienkiewicz  Add error exit codes - if error return from
C                          'crbmg' is non-zero, use that (last one) as
C                           Fortran exit code.
C 2016-10-17  Sienkiewicz  Bump up size of maximum bufr record to match bufrlib
C                          value.
C
C     Error codes from 'crbmg':
C                  1 = overflow of BMG array
C                  2 = "7777" indicator not found in expected location
C                 -1 = end-of-file encountered while reading
C                 -2 = I/O error encountered while reading
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT (OPERATION TYPE, INPUT FILENAME,
C                OUTPUT FILENAME)
C
C   OUTPUT FILES: 
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 51  - BLOCKED FORTRAN FILE OUTPUT FOR BLOCKING OPERATION
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       BUFRLIB  - CCBFL    COBFL    CRBMG    CWBMG    PADMSG
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  IBM SP
C
C$$$

      program cwordsh

      implicit NONE

      integer,parameter :: mxbufr=600000      ! bufrlib variable: MXMSGL
      integer,parameter :: mxbufrd4=mxbufr/4

      character*256 argv0
      integer nargs, iargc
      integer ierw
      integer ierr, iersv

      character(len=256) bfile,ufile
      character(len=8)  cword
      character(len=1)  bufr(mxbufr)
      integer    mbay(mxbufrd4)
      equivalence  (bufr(1),mbay(1))

      integer nbyt, npbyt, ntbyt
      integer  i


#ifdef UNBLOCK
      cword = 'unblock'
#else
      cword = 'block'
#endif
 
c     Read and process the input arguments.
      nargs = iargc()
      call getarg(0,argv0)
      if (nargs .ne. 2) then
         print *,'usage: ',trim(argv0),' input-file output-file'
         stop
      endif
      if(cword.eq.'block') then
         call getarg(1, ufile)
         call getarg(2, bfile)
      elseif(cword.eq.'unblock') then
         call getarg(1, bfile)
         call getarg(2, ufile)
      endif

      iersv = 0
 
c     Open the input and output files.

      if(cword.eq.'block') then
         print*,'blocking from: ',trim(ufile)
         print*,'           to: ',trim(bfile)
         call cobfl(ufile,'r')
         open(51,file=bfile,form='unformatted')
      else
         print*,'unblocking from: ',trim(bfile)
         print*,'             to: ',trim(ufile)
         call cobfl(bfile,'r')
         call cobfl(ufile,'w')
      endif

c     Read the next message from the input file.

      call crbmg(bufr,mxbufr,nbyt,ierr)
      do while(ierr.ge.0)
        if(ierr.eq.0) then

c         Pad the end of the message with zeroed-out bytes up to the
c         next 8-byte boundary.

          call padmsg(mbay,mxbufrd4,npbyt)
          ntbyt = nbyt + npbyt

c         Write the message plus padding to the output file...

          if(cword.eq.'block') then

c           using a FORTRAN write.

            write(51) (bufr(i),i=1,ntbyt)
          else

c           using a C write.

            call cwbmg(bufr,ntbyt,ierw)
            if(ierw.ne.0) print*,'return value from cwbmg was ',ierw
          endif
        else
          print*,'return value from crbmg was ',ierr,
     .           '; message not written to output'
          iersv = ierr
        endif
        call crbmg(bufr,mxbufr,nbyt,ierr)
      enddo

c     Close the input and output files.

      call ccbfl
      if(cword.eq.'block') close(51)

      if (iersv .ne. 0) call exit(iersv)

      stop
      end
