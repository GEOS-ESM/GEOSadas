 MODULE GAC_1B_PROC

      implicit none

!     Include machine dependent parameters.
      include 'gac1b.inc'                  ! Header and data record definition
      include 'rfac.inc'                   ! Record length divisor
      
!     Declare/set parameters.
!      avh_mxpix = total number of bytes (4608) in AVHRR GAC data record
!      cavh_size = number of 4-byte words in avh_mxpix bytes (4608/4=1152)
!      nid,nnd= number of arguments in arrays in DATTIM call.
!      mch    = number of channels
!      mpos   = number of spots (positions) on a scan line
!
 integer, parameter :: real_32=selected_real_kind(6,37)
 integer, parameter :: real_64=selected_real_kind(15,307)
 integer, parameter :: mch=5,mpos=409,maxread=200000
 integer, parameter :: nreal=22,ntot=nreal+mch,bit_qc=0
 integer, parameter :: stdout=6,lu1b=11,wbf_sea_a_lcr=51,wbf_lnd_o_cld=52
 integer, parameter :: lundx=20,nprint=500,nmon=12
 integer, parameter :: nlat_msk=2881,nlon_msk=5761,nlat_sst=360,nlon_sst=720

 real, parameter :: lon1_msk=-180.0,lat1_msk=90.0,lon2_msk=180.0,lat2_msk=-90.0
 real, parameter :: lon1_sst=0.25,lat1_sst=-89.75,lon2_sst=359.75,lat2_sst=89.75
 real, parameter :: tlo=100.,thi=400.,vislo=0.01,vishi=100.
 real, parameter :: dxy=0.5,dtr=3.141593,ttp=271.5
 real(8), parameter :: bmiss=10e8_8

! Variables
!
 integer :: lsubdrp,ierr,lapchrp,ltnkidp,ltnkid,lsubdr,lapchr,kdate,jdate
 integer :: cld_bit,clavr_cld,id_ncep,itime
 integer :: pyear0,pddd0,pyear,pddd,yyyy,mm,dd,hh,atime,nbf,ilat,ilon,iret
 integer :: idate,rtgsst,navysst,iline,iyear,iddd,jyd,jtype,j3ab,nrecs,nscan
 integer :: nri,nrec,jmd1,jmd2,jmd3,sathgt,jsat,jsat0,nlo,line,ioff,i,j,k,n,ios
 integer :: imon,jmon,im,idum,nlev,flag,missing,lu_ncepsst,iy_ncep,im_ncep
!DAK
 integer :: ilimb
!DAK

 integer*1 :: land_sea_tag

 real(real_64) scale,scale5,scale6,scale7,scale9,scale12,sctime

 logical :: ex,lfirst

 character(len=80) :: APPCHR,APPCHRP
 character(len=12) :: SUBDIR,TANKID,SUBDIRP,TANKIDP
 character(len=10) :: catime
 character(len=8) :: cncepsst_date,statid,cid,TLFLAG,TLFLAGP,SUBSET,SUBSETP
 character(len=4) :: cyyyy
 character(len=2) :: cmm,cdd,chh

! Arrays
!
 integer, dimension(mch,mpos) :: counts
 integer, dimension(mch) :: ichan
 integer, dimension(6) :: good_lines,bad_lines
 integer, dimension(-1:10), public :: lu_bufr
 integer, dimension(mpos) :: cld
 integer, dimension(-1:0) :: pyyyyddd,pyyyy,pyy,pmm,pdd
 integer, dimension(-1:0) :: sst_date,lu_sst

 integer idt(3),ndt(7),intime(8),outime(8)

 integer*1, dimension(nlon_msk,nlat_msk) :: lst

 real, dimension(nmon,nlat_sst,nlon_sst) :: clima
 real, dimension(mpos) :: solzen,satzen,rlocaz,alat,alon,avh_qc
 real, dimension(mch,mpos) :: avh_rad,avh_bt

 real(8), dimension(ntot) :: rdata

 real    rinc(5)

 character(len=2), dimension(-1:100), public :: cpmm,cpdd
 character(len=2), dimension(6) :: chn_name
 character(len=4), dimension(-1:100), public :: cpyyyy
 character(len=10),dimension(20) :: cbufr_time

 real(8) :: time_min, time_max, r_time
 logical :: do_bufrize

! Data assignment
!
 data chn_name/'01','02','3B','04','05','3A'/
 data good_lines/6*0/
 data  bad_lines/6*0/
 data ichan / 1, 2, 3, 4, 5/
 data nrec/0/
 data lfirst /.false./



contains

      SUBROUTINE AVHRR(SUBSET,SUBSETP)
!$$$  subprogram documentation block                                    
!
! SUBPROGRAM:    AVHRR
!   PRGMMR: KEYSER           ORG: NP22        DATE: 2006-10-19
!                                                                       
! ABSTRACT: Decodes a single scan line from a raw AVHRR 1B format file and
!    stores report (selected spot) observations into array for later encoding
!    into output BUFR file.
!                                                                       
! PROGRAM HISTORY LOG:
! 2003-01-29  Xu Li   Original author
! 2006-10-19  Keyser  Improved docblocks and comments
!
! USAGE:    CALL AVHRR(SUBSET,SUBSETP)
!   INPUT ARGUMENT LIST:
!     SUBSET  - BUFR message type for clear and oceanic reports
!     SUBSETP - BUFR message type for cloudy or overland reports
!
! REMARKS: None
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 90 (free format)
!   MACHINE:  NCEP WCOSS
!                                                                       
!$$$

      implicit none

      character(len=8) :: SUBSET,SUBSETP

!  Extract scan line number, date/time, position, and type
!  -------------------------------------------------------

      iline  = avh_scnlin                  ! scan line number
      iyear  = avh_scnlinyr                ! scan line year
      iddd   = avh_scnlindy                ! scan line day of year
      itime  = avh_scnlintime              ! scan line UTC time of day in
                                           !  milliseconds
      sctime = 1.d-3*itime
      jyd    = 1000*iyear + iddd

!  Get year  : ndt(1)
!     month  : ndt(2)
!     day    : ndt(3)
!     hour   : ndt(4)
!     minute : ndt(5)
!     second : ndt(6)
!     milisecond : ndt(7)
!
!!!!! call dattim(idt,ndt)

      im = ndt(2)

!  Determine the selection mode for Channel 3 (A or B)
!  ---------------------------------------------------

      j3ab = ibits(avh_scnlinbit,0,1)      ! 0 = 3B; 1 = 3A   

!  Extract navigation data
!  -----------------------

      scale  = 1.d-1
      sathgt = avh_scalti*scale            ! spacecraft altitude (km)

!  Extract earth location data
!  ---------------------------

      scale = 1.d-4
      do i = 5,mpos,8
         alat(i) =  avh_pos(1,(i-5)/8+1)*scale 
         alon(i) =  avh_pos(2,(i-5)/8+1)*scale
      end do

!  Extract angular relationships
!  -----------------------------

      scale = 1.d-2
      do i = 5,mpos,8
         solzen(i)  = avh_ang(1,(i-5)/8+1)*scale
         satzen(i)  = avh_ang(2,(i-5)/8+1)*scale 
         rlocaz(i)  = avh_ang(3,(i-5)/8+1)*scale
      end do

!  Angles and lat/lon are only available for 51 pixels in one AVHRR GAC scan
!   line (mpos pixels), interpolation is needed to get the lat/lon for every
!   pixel
!  -------------------------------------------------------------------------

      call lag(solzen,0)
      call lag(satzen,0)
      call lag(rlocaz,0)

      call lag(alat,0)
      call lag(alon,1)

!  Extract AVHRR counts
!  --------------------

      ioff = -4
      do i = 1, 408, 3
         ioff = ioff + 5

!  Unpack channel 1
!  ----------------

         counts(1,i+0) = ibits(avh_hrpt(ioff+0),20,10)
         counts(1,i+1) = ibits(avh_hrpt(ioff+1), 0,10)
         counts(1,i+2) = ibits(avh_hrpt(ioff+3),10,10)

!  Unpack channel 2
!  ----------------

         counts(2,i+0) = ibits(avh_hrpt(ioff+0),10,10)
         counts(2,i+1) = ibits(avh_hrpt(ioff+2),20,10)
         counts(2,i+2) = ibits(avh_hrpt(ioff+3), 0,10)

!  Unpack channel 3 (A or B)
!  -------------------------

         counts(3,i+0) = ibits(avh_hrpt(ioff+0), 0,10)
         counts(3,i+1) = ibits(avh_hrpt(ioff+2),10,10)
         counts(3,i+2) = ibits(avh_hrpt(ioff+4),20,10)

!  Unpack channel 4
!  ----------------

         counts(4,i+0) = ibits(avh_hrpt(ioff+1),20,10)
         counts(4,i+1) = ibits(avh_hrpt(ioff+2), 0,10)
         counts(4,i+2) = ibits(avh_hrpt(ioff+4),10,10)

!  Unpack channel 5
!  ----------------

         counts(5,i+0) = ibits(avh_hrpt(ioff+1),10,10)
         counts(5,i+1) = ibits(avh_hrpt(ioff+3),20,10)
         counts(5,i+2) = ibits(avh_hrpt(ioff+4), 0,10)

      enddo

      ioff = ioff + 5                      ! ioff = 681

      counts(1,mpos) = ibits(avh_hrpt(ioff+0),20,10)
      counts(2,mpos) = ibits(avh_hrpt(ioff+0),10,10)
      counts(3,mpos) = ibits(avh_hrpt(ioff+0), 0,10)
      counts(4,mpos) = ibits(avh_hrpt(ioff+1),20,10)
      counts(5,mpos) = ibits(avh_hrpt(ioff+1),10,10)

! (1) Convert counts into radiances and albedo/Tb 
!     (may contain missing values: bad calibration ...); 
! (2) Save the quality indicator for each pixel : avh_qc(i),i = 1, mpos
! (3) Save the statistics of the number of good/bad lines:
!      good_lines(i) and bad_lines(i), i = 1, 6
!  --------------------------------------------------------------------

      call gac_lbc(iline)

!  Decode CLAVR cloud detection information (flags)
!  ------------------------------------------------

      cld = 7                              ! 7 = missing
      cld_bit = ibits(clv_bit,0,1)

      if ( cld_bit == 1 ) then
         do i = 1, 401, 8
            n = (i-1)/8 + 1                ! 1 2 3 4 ... 50 51
            do j = 0, 7
                                ! i + j = 1 2 ... 8 9 10 ... 16 ... 401 ... 408
               cld(i+j) = ibits(ccmc(n),14-2*j,2)
            enddo
         enddo
         cld(mpos) = ibits(ccmc(52),14,2)
      endif

!  Exclude the pixels overland and definitely cloudy
!  -------------------------------------------------

      do i = 1,mpos

         if (avh_qc(i) == 0.0) then
            ilat = nint((nlat_msk-1)*(alat(i)-lat1_msk)/(lat2_msk-lat1_msk)) + 1
            ilon = nint((nlon_msk-1)*(alon(i)-lon1_msk)/(lon2_msk-lon1_msk)) + 1
            ilat = max(1,min(ilat,nlat_msk))
            ilon = max(1,min(ilon,nlon_msk))
            land_sea_tag = lst(ilon,ilat)

            if ( land_sea_tag == 1 ) then
               avh_qc(i) = 2.0             ! exclude overland pixels
            elseif ( land_sea_tag == 0 ) then
               if ( cld(i) == 3 .or. cld(i) == 2 ) then
                  clavr_cld = clavr_cld + 1
                  avh_qc(i) = 2.0          ! cloudy 
               endif
            endif
         endif
      enddo

!  Write AVHRR data for each spot position on current scan line
!  ------------------------------------------------------------

      do i = 1,mpos

!DAK
!  Temporary fix by DAK to exclude any "spots" on the limb of the scans
!   because the extrapolations may not be done properly in subroutine LAG
!   (e.g., longitude which can vary greatly from one reported spot to the
!   next at very high latitudes) - Xu Li is working on a permanent fix
!  ----------------------------------------------------------------------
         if(i.lt.5.or.i.gt.(mpos-4))  then
            ilimb = ilimb + 1
!ppppppp
!!!!!       print *, 'On scan ',iline,'; toss this spot - on limb, pos = ',i
!ppppppp
            cycle
         endif
!DAK

         if (avh_qc(i) == 0.0 .or. avh_qc(i) == 2.0) then

            idt(1) = itime/1000
            idt(2) = mod(jyd,1000)
            idt(3) = jyd/1000

            call dattim(idt,ndt)
            intime(1:3) = ndt(1:3)
            intime(4) = 0
            intime(5:7) = ndt(4:6)
            intime(8) = mod(itime,1000)
 
            rinc(1:4) = 0.0
            rinc(5) = real(500000 + 125*i) / 1000.
            call w3movdat(rinc,intime,outime)

!  Assign array for BUFR
!  ---------------------

            rdata( 1) = outime(1)          ! 4-digit year
            rdata( 2) = outime(2)          ! month of a year
            rdata( 3) = outime(3)          ! day of a month
            rdata( 4) = outime(5)          ! hour of a day
            rdata( 5) = outime(6)          ! minute of a hour
                                           ! second of a minute (high-res)
            rdata( 6) = 1.0_8 * outime(7) + 0.001_8 * &
                        (real(outime(8)) + rinc(5) - int(rinc(5)))

!!!!!       print *,'nrec, rdata( 6) : ',nrec, rdata(6)

            rdata( 7) = alat(i)            ! latitude 
            rdata( 8) = alon(i)            ! longitude 
            rdata( 9) = jsat               ! Satellite ID
            rdata(10) = jtype              ! Instrument ID (2 = AVHRR)
            rdata(11) = iline              ! Number of scan line
            rdata(12) = i                  ! Number of the field of view
            rdata(13) = sathgt*1000        ! Satellite altitude
            rdata(14) = satzen(i)          ! satellite zenith angle
            rdata(15) = solzen(i)          ! solar zenith angle
            rdata(16) = j3ab               ! Mode of 3A/3B: 0 = 3B, 1 = 3A
            rdata(17) = cld(i)             ! CLAVR cloud flag:
                                           !    0 = clear,
                                           !    1 = probably clear,
                                           !    2 = probably cloudy,
                                           !    3 = cloudy
            rdata(18) = avh_bt(1,i)        ! AVHRR (GAC) for Ch-1 albedo
            rdata(19) = avh_bt(2,i)        ! AVHRR (GAC) for Ch-2 albedo
            rdata(20) = avh_bt(3,i)        ! AVHRR (GAC) for Ch-3 alb/Tb
            rdata(21) = avh_bt(4,i)        ! AVHRR (GAC) for Ch-4 Tb
            rdata(22) = avh_bt(5,i)        ! AVHRR (GAC) for Ch-5 Tb

            nrec = nrec + 1

!!!!!       write(*,'(2I8,5f16.2)') clavr_cld,cld(i),avh_bt(1,i),avh_bt(2,i), &
!!!!!                               avh_bt(3,i),avh_bt(4,i),avh_bt(5,i)
            
            do_bufrize = .false.
            r_time =(rdata(1)*1E8+rdata(2)*1E6+rdata(3)*1E4+rdata(4)*1E2+rdata(5))*1E2&
                +rdata(6)
            if (r_time .gt. time_min .and. r_time .le. time_max) then
               do_bufrize = .true.
            endif

            if (do_bufrize) call bufr1b(avh_qc(i),subset,subsetp)     ! Encode data in BUFR

         endif                           
      end do

      END SUBROUTINE AVHRR


      SUBROUTINE BUFR1B(MQC,SUBSET,SUBSETP)
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BUFR1B
!   PRGMMR: KEYSER           ORG: NP22        DATE: 2006-10-19
!
! ABSTRACT: Generates a single BUFR subset (report) from AVHRR GAC 1B data on
!   a single "spot" (on a scan line) and encodes it into output BUFR file.
!
! PROGRAM HISTORY LOG:
! 2003-01-29  Xu Li   Original author
! 2006-10-19  Keyser  Improved docblocks and comments
! 2015-09-09  Stokes  Minor corrections, including those to preserve precision 
!                     when handling real*8 data
! 2016-04-15  Stokes  Force a call to openmb before data values are added to a new
!                     bufr subset for output. This ensures the subset is written
!                     to a bufr message with an appropriate section 1 date.
!
! USAGE: CALL BUFR1B(MQC,SUBSET,SUBSETP)
!
!   INPUT ARGUMENTS:
!     MQC     - Real indicator for either clear and oceanic (=0.0) or cloudy or
!               overland (=2.0)
!     SUBSET  - BUFR message type for clear and oceanic reports
!     SUBSETP - BUFR message type for cloudy or overland reports
!
! REMARKS: None
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 (free format)
!   MACHINE:  NCEP WCOSS
!
!$$$
 
      CHARACTER*80  HEADR
      CHARACTER*8   SUBSET,SUBSETP, SUBSETO
      REAL(8)       INCN(5), TMBR(5), ALBD(5)
      REAL(8)       HDR(13),BUFRF(3,5)
      real :: mqc

      DATA  HEADR/&
         'YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAID FOVN SAZA SOZA CLAVR'/

!  TRANSLATE ONE AVHRR(GAC) SPOT TO BUFR FORMAT
!  ----------------------------------------------------------
!  NC021xxx | YEAR     MNTH     DAYS     HOUR     MINU
!  NC021xxx ! 202129 201132 SECO 201000 202000
!  NC021xxx | CLATH    CLONH    SAID     201129 FOVN 201000
!  NC021xxx | SAZA     SOZA     CLAVR "AVCSEQ"5
!           |
!   AVCSEQ  | INCN    ALBD    TMBR
!  -----------------------------------------------------------

      ALBD = bmiss
      TMBR = bmiss

      if ( rdata(16) == 0. ) then          ! 3B

         ALBD(1) = rdata(18)               ! Ch-1 albedo
         ALBD(2) = rdata(19)               ! Ch-2 albedo
         TMBR(3) = rdata(20)               ! Ch-3 BT
         TMBR(4) = rdata(21)               ! Ch-4 BT
         TMBR(5) = rdata(22)               ! Ch-5 BT

      elseif ( rdata(16) == 1. ) then      ! 3A

         ALBD(1) = rdata(18)               ! Ch-1 albedo
         ALBD(2) = rdata(19)               ! Ch-2 albedo
         ALBD(3) = rdata(20)               ! Ch-3 albedo
         TMBR(4) = rdata(21)               ! Ch-4 BT
         TMBR(5) = rdata(22)               ! Ch-5 BT

      endif

      HDR( 1) = rdata( 1)                  ! 4-digit year
      HDR( 2) = rdata( 2)                  ! month of a year
      HDR( 3) = rdata( 3)                  ! day of a month
      HDR( 4) = rdata( 4)                  ! hour of a day
      HDR( 5) = rdata( 5)                  ! minute of a hour
      HDR( 6) = rdata( 6)                  ! second of a minute
      HDR( 7) = rdata( 7)                  ! latitude
      HDR( 8) = rdata( 8)                  ! longitude
      HDR( 9) = rdata( 9)                  ! Satellite ID
      HDR(10) = rdata(12)                  ! IFOV
      HDR(11) = rdata(14)                  ! satellite zenith angle
      HDR(12) = rdata(15)                  ! solar zenith angle
      HDR(13) = rdata(17)                  ! CLAVR cloud flag

      DO N=1,5
         BUFRF(1,N) = N + 47
         BUFRF(2,N) = ALBD(N)
         BUFRF(3,N) = TMBR(N)
      ENDDO

      idate = &
  idnint(rdata(1))*1000000+idnint(rdata(2))*10000+idnint(rdata(3))*100+idnint(rdata(4))

      if (mqc == 0.0 ) then               ! Clear and oceanic reports
         if ( nbf /= wbf_sea_a_lcr ) then
            CALL CLOSMG(wbf_lnd_o_cld)
            nbf = wbf_sea_a_lcr
            SUBSETO=SUBSET
         endif
!               write(99,"(I5,',',4(I3,','),5(f10.4,','),I3)") &
!               int(rdata(1)),int(rdata(2)),int(rdata(3)),int(rdata(4)),int(rdata(5)),&
!               rdata( 8), rdata( 7), rdata(20), rdata(21),rdata(22),int(rdata(17))
      elseif (mqc == 2.0 ) then           ! Cloudy or overland reports
         if ( nbf /= wbf_lnd_o_cld ) then
            CALL CLOSMG(wbf_sea_a_lcr)
            nbf = wbf_lnd_o_cld 
            SUBSETO=SUBSETP
         endif
      else
            write(*,*)'***'
            write(*,*)'*** Exit 7: Unexpected value for mqc: ',mqc,' ***'
            write(*,*)'***'
            call errexit(7)
      endif

      CALL OPENMB(nbf,SUBSETO,idate)
      CALL UFBINT(nbf,HDR,  13,1,IRET,HEADR)
      CALL UFBREP(nbf,BUFRF, 3,5,IRET,'INCN ALBD TMBR')
      CALL WRITCP(nbf)                   ! Write subset (report) into
                                         !  compressed BUFR message

      END SUBROUTINE BUFR1B


      SUBROUTINE GAC_LBC(LINE)
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: GAC_LBC
!   PRGMMR: KEYSER           ORG: NP22        DATE: 2006-10-19
!
! ABSTRACT: For each Level 1B AVHRR scan line, converts numerical counts into
!   radiances and brightness temperatures (GAC Level 1C).
!
! PROGRAM HISTORY LOG:
! 2003-01-14  Xu Li   Original author
! 2006-10-19  Keyser  Improved docblocks and comments
!
! USAGE: CALL GAC_LBC(LINE)
!
!   INPUT ARGUMENTS:
!     LINE   - Scan line number
!
! REMARKS:
!       Needs include files "gac1b.inc" and "cavh_cpi.inc"
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 (free format)
!   MACHINE:  NCEP WCOSS
!
!$$$
 
      implicit none

!---- DECLARATIONS -----------------------------------------------------

      character*62 sccid
      data sccid/'%Z% %M% version %I% on %G%'/ 

!---- ARGUMENTS --------------------------------------------------------

      integer line		           ! Scan line number

      include 'gac1b.inc'                  ! Header and data record definition
      include 'cavh_cpi.inc'               ! AVHRC1 "CPIDS" related parameters

!---- LOCAL VARIABLES --------------------------------------------------

      integer p,c		           ! Pixel, channel
      real rcount		           ! Earth viewed numerical count
      integer ncount		           ! Earth viewed numerical count

      real slope		           ! Coeff. for visible calibration
      real intercept		           ! ....
      real tabalb(0:1023,3)	           ! Table for count to albedo

      real k1,k2,k3		           ! Coeff. conversion count ->radiance
      double precision nu	           ! Central wavenumber
      real radiance		           ! Radiance  mW/cm2.st.cm-1
      integer expon3                       ! Exponent of scaling factor for 1st
                                           !  IR cal. coeff
      integer expon4                       ! Exponent of scaling factor for 2nd
                                           !  IR cal. coeff
      integer expon5                       ! Exponent of scaling factor for 3rd
                                           !  IR cal. coeff
      real scale3                          ! Scaling factor for k1
      real scale4                          ! Scaling factor for k2
      real scale5                          ! Scaling factor for k3
!!!!! data scale3 /1.e-06/
!!!!! data scale4 /1.e-06/
!!!!! data scale5 /1.e-06/

      integer good_qc		           ! Good quality word
      integer bad_qc	                   ! Bad quality word
      integer*4 status_channel             ! avh_h_inststat2 or avh_h_inststat1
      logical first /.true./
      integer vcoefset	                   ! Visible coeff. set

      save                                 ! Save local tables and logical first

!---- EQUIVALENCES -----------------------------------------------------

!---- FUNCTIONS --------------------------------------------------------

      real avh_cir_r                       ! Radiance to brightness temperature
                                           !  conversion (real)

!---- CODE ------------------------------------------------------------- 


      if (first ) then

! FIRST TIME IN HERE ...

!  Modification to correct lack of accuracy of scaling factors proposed by NOAA
!   ( M. Derrien 26/01/1999)
!   -- must be considered only for a transition, because we do not change the
!      l1bversnb number due to CMS consideration and we don't have the new
!      NESDIS scaling factors.
!      This will change scaling factors only for 1B files whose data are newer
!      than 27 January 1999,12 UTC date where changed at CMS.
!                  
!       Now avh_filler2(3) stores the 3 exponents of the scaling coefficients.
!       Subroutine avh_lbc will be able to read the new exponents and will
!       suppose exponent 6 for all archived files.  Beware of reading these new
!       files without taking into account the filler2 array.
!  ----------------------------------------------------------------------------

!  Xu Li, 08/16/2005: From NESDIS people (Chao), NOAA satellite data format was
!                     updated on April 28, 2005.
!                     For AVHRR Ch-4 & Ch-5, IR calibration exponent of scaling
!                     coefficient 3 (expon5) has been changed (6 -> 7).
!  ----------------------------------------------------------------------------

         expon3=6
         expon4=6
         expon5=7

         scale3 = 10. ** (-1 *expon3)
         scale4 = 10. ** (-1 *expon4)
         scale5 = 10. ** (-1 *expon5)

!!!!!$        if (avh_h_startdatajd.ge.17923.5) then
!!!!!$          scale3 = 1.e-09
!!!!!$        endif      

!  We use the prelaunch coefficients  (indice 3)
!   operationnal set is 1 
!   test set is 2
!  ---------------------------------------------

!!!!!!!  vcoefset=3

!  Xu Li, 09/08/2005: change to use operational set (1)
!  ----------------------------------------------------

         vcoefset = 1
         do c = 1, avh_mxvischn
!!!!!!!     slope     = avh_calvis(1,vcoefset,c)*1.e-10
!!!!!!!     intercept = avh_calvis(2,vcoefset,c)*1.e-7

            slope     = avh_calvis(1,vcoefset,c)*1.e-7
            intercept = avh_calvis(2,vcoefset,c)*1.e-6

            do ncount = 0 , avh_calvis(5,vcoefset,c),1
               tabalb(ncount,c) = (slope * float(ncount) + intercept)

               if(tabalb(ncount,c).lt.0.) tabalb(ncount,c)=0.
            enddo
!!!!!!!     slope     = avh_calvis(3,vcoefset,c)*1.e-10
!!!!!!!     intercept = avh_calvis(4,vcoefset,c)*1.e-7

            slope     = avh_calvis(3,vcoefset,c)*1.e-7
            intercept = avh_calvis(4,vcoefset,c)*1.e-6

            do ncount = avh_calvis(5,vcoefset,c), 1023
               tabalb(ncount,c) = (slope * float(ncount) + intercept)

               if(tabalb(ncount,c).lt.0.) tabalb(ncount,c)=0.
            enddo
         enddo

!!!!!    write(*,*) 'tabalb(ncount,1) : '
!!!!!    write(*,'(10F9.2)') (tabalb(ncount,1),ncount=0,1023)

!!!!!    write(*,*) 'tabalb(ncount,2) : '
!!!!!    write(*,'(10F9.2)') (tabalb(ncount,2),ncount=0,1023)

!!!!!    write(*,*) 'tabalb(ncount,3) : '
!!!!!    write(*,'(10F9.2)') (tabalb(ncount,3),ncount=0,1023)

!!!!!    write(*,*) 'counts(1,ncount) : '
!!!!!    write(*,'(15I6)') (counts(1,ncount),ncount=1,mpos)

!!!!!    write(*,*) 'counts(2,ncount) : '
!!!!!    write(*,'(15I6)') (counts(2,ncount),ncount=1,mpos)

!!!!!    write(*,*) 'counts(3,ncount) : '
!!!!!    write(*,'(15I6)') (counts(3,ncount),ncount=1,mpos)

!!!!!    write(*,*) 'counts(4,ncount) : '
!!!!!    write(*,'(15I6)') (counts(4,ncount),ncount=1,mpos)

!!!!!    write(*,*) 'counts(5,ncount) : '
!!!!!    write(*,'(15I6)') (counts(5,ncount),ncount=1,mpos)


!  Infrared channels
!  -----------------

!  C1 and C2 are defined here because the common cavh_cpi has net been
!   initialized (call avh_cpar)
!
!  planck1 is C1 * nu3
!  planck2 is C2 * nu
!  avh_bandcor(1,c) is nu (cm-1)
!  avh_bandcor(2,c) is intercept (K)
!  avh_bandcor(3,c) is slope (K/K)
!  -------------------------------------------------------------------

!  Xu Li: update C1, C2 
!
!!!!     C1 = 0.000011910659d0
!!!!     C2 = 1.438833d0

         C1 = 0.000011910427d0
         C2 = 1.4387752d0
	
!  Planck coefficients channel 3B
!  ------------------------------

         c=1  
         nu=avh_h_radtempcnv(1,c)*1.e-02
         planck1(c)=C1*nu*nu*nu
         planck2(c)=C2*nu
         avh_bandcor(1,c)=nu
         avh_bandcor(2,c)=avh_h_radtempcnv(2,c)*1.e-05
         avh_bandcor(3,c)=avh_h_radtempcnv(3,c)*1.e-06

!  Planck coefficients channel 4
!  -----------------------------

         c=2
         nu=avh_h_radtempcnv(1,c)*1.e-03
         planck1(c)=C1*nu*nu*nu
         planck2(c)=C2*nu
         avh_bandcor(1,c)=nu
         avh_bandcor(2,c)=avh_h_radtempcnv(2,c)*1.e-05
         avh_bandcor(3,c)=avh_h_radtempcnv(3,c)*1.e-06

!  Planck coefficients channel 5
!  -----------------------------

         c=3
         nu=avh_h_radtempcnv(1,c)*1.e-03
         planck1(c)=C1*nu*nu*nu
         planck2(c)=C2*nu
         avh_bandcor(1,c)=nu
         avh_bandcor(2,c)=avh_h_radtempcnv(2,c)*1.e-05
         avh_bandcor(3,c)=avh_h_radtempcnv(3,c)*1.e-06

!  Radiance to temperature tables, all IR channels (common temp_conv)
!  ------------------------------------------------------------------

         call avh_icon

         first = .false.
      endif

!  Channel status for the line
!  --------------------------- 

      status_channel = avh_h_inststat1

      if (avh_h_statchrecnb.eq.0) then
         status_channel = avh_h_inststat1
      else if(line.ge.avh_h_statchrecnb) then
         status_channel = avh_h_inststat2
      endif

!  MISSING LINE TEST
!  -----------------

      if(ibits(avh_qualind,31,1) == 1 .or. ibits(avh_qualind,30,1) == 1 .or. &
         ibits(avh_qualind,28,1) == 1 .or. ibits(avh_qualind,27,1) == 1 )then          

!  Missing line, 0 ==> 5 channels 
!  ------------------------------

         do p=1,mpos
            do c=1,5
               avh_bt(c,p)=bmiss
            enddo
         enddo
         do p=1,mpos
            do c=1,5
               avh_rad(c,p)=bmiss
            enddo
         enddo
         missing = 1
         bad_lines(1) = bad_lines(1) + 1
         bad_lines(2) = bad_lines(2) + 1
         bad_lines(3) = bad_lines(3) + 1
         bad_lines(4) = bad_lines(4) + 1
         bad_lines(5) = bad_lines(5) + 1
         bad_lines(6) = bad_lines(6) + 1

      else

!  GOOD LINE
!  ---------

         missing = 0

!  Channel VIS 1
!  =============

         c = 1

!  Testing if the channel is enabled (1)
!  -------------------------------------

         if(ibits(status_channel,13,1).eq.1)then

            good_lines(1) = good_lines(1)+1

!  Loop on mpos pixels
!  -------------------

            do p = 1, mpos
               avh_bt(c,p) = tabalb(counts(c,p),c)
               avh_rad(c,p)= bmiss
            enddo


         else

!  Channel is disabled
!  -------------------

            bad_lines(1) = bad_lines(1) + 1
            do p=1,mpos
               avh_bt(c,p) = bmiss
               avh_rad(c,p) = bmiss
            enddo
         endif

!  Channel VIS 2
!  =============

         c = 2

!  Testing if the channel is enabled (1)
!  -------------------------------------

         if(ibits(status_channel,12,1).eq.1)then

            good_lines(2) = good_lines(2)+1

!  Loop on mpos pixels
!  -------------------

            do p=1,mpos
               avh_bt(c,p)  = tabalb(counts(c,p),c)
                avh_rad(c,p) = bmiss
            enddo

         else

!  Channel is disabled
!  -------------------

            bad_lines(2) = bad_lines(2) + 1
            do p=1,mpos
               avh_bt(c,p) = bmiss
               avh_rad(c,p) = bmiss
            enddo
         endif

!  Channel IR 3B or Channel Vis 3A
!  ===============================

         c = 3

!  Bits 1-0 of avh_scnlinbit : channel 3 select (0 = 3b; 1 = 3a; 2 = transition)
!  -----------------------------------------------------------------------------

         if(ibits(avh_scnlinbit,0,1) == 0)then

!  Testing if the channel 3B is enabled (1)
!  ----------------------------------------

            if(ibits(status_channel,10,1).eq.1)then

!  Testing the calibration quality flag
!  ------------------------------------

               if(ibits(avh_calqual(1),7,1).eq.0)then  
                  k1=avh_calir(1,1,1)*scale3
                  k2=avh_calir(2,1,1)*scale4
                  k3=avh_calir(3,1,1)*scale5

                  good_lines(3) = good_lines(3)+1

!  Loop on mpos pixels
!  -------------------

                  do p=1,mpos
                     rcount = float(counts(c,p))
!!!!!!               radiance = k1*rcount*rcount + k2*rcount + k3
                     radiance = k1 + k2*rcount + k3*rcount*rcount
                     avh_rad(c,p) = radiance
                     avh_bt(c,p)=avh_cir_r(radiance,1)
                  enddo

               else

!  Calibration not usable for channel 3B
!  -------------------------------------

                  bad_lines(3) = bad_lines(3) + 1
                  do p=1,mpos
                     avh_rad(c,p) = bmiss
                     avh_bt(c,p)  = bmiss
                  enddo

               endif

            else

!  Channel is disabled
!  -------------------

               bad_lines(3) = bad_lines(3) + 1
               do p=1,mpos
                  avh_rad(c,p) = bmiss
                  avh_bt(c,p)  = bmiss
               enddo
            endif

         elseif ( ibits(avh_scnlinbit,0,1) == 1) then 

!  Bits 1-0 of avh_scnlinbit : channel 3 select (0 = 3b; 1 = 3a; 2 = transition)
!  -----------------------------------------------------------------------------

!  Channel IR 3A
!  =============

!  Testing if the channel 3A is enabled (1)
!  ----------------------------------------

            if(ibits(status_channel,11,1).eq.1)then

               good_lines(6) = good_lines(6)+1

!  Loop on mpos pixels
!  -------------------

               do p=1,mpos
                  avh_bt(c,p)  = tabalb(counts(c,p),3)
                  avh_rad(c,p) = bmiss
               enddo

            else 

!  Channel is disabled
!  -------------------

               bad_lines(6) = bad_lines(6) + 1
               do p=1,mpos
                  avh_bt(c,p)  = bmiss
                  avh_rad(c,p) = bmiss
               enddo
            endif

         endif

!  Channel IR 4
!  ============

         c = 4

!  Testing if the channel is enabled (1)
!  -------------------------------------

         if(ibits(status_channel,9,1).eq.1)then

!  Testing the calibration quality flag
!  ------------------------------------

            if(ibits(avh_calqual(2),7,1).eq.0)then
               k1=avh_calir(1,1,2)*scale3
               k2=avh_calir(2,1,2)*scale4
               k3=avh_calir(3,1,2)*scale5

               good_lines(4) = good_lines(4)+1

!  Loop on mpos pixels
!  -------------------

               do p=1,mpos
                  rcount = float(counts(c,p))
!!!!!             radiance = k1*rcount*rcount + k2*rcount + k3
                  radiance = k1 + k2*rcount + k3*rcount*rcount
                  avh_rad(c,p) = radiance
                  avh_bt(c,p)=avh_cir_r(radiance,2)

               enddo
            else

!  Calibration not usable 
!  ----------------------

               bad_lines(4) = bad_lines(4) + 1
               do p=1,mpos
                  avh_rad(c,p) = bmiss
                  avh_bt(c,p)  = bmiss
               enddo
            endif
         else

!  Channel is disabled
!  -------------------

            bad_lines(4) = bad_lines(4) + 1
            do p=1,mpos
               avh_rad(c,p) = bmiss
               avh_bt(c,p)  = bmiss
            enddo
         endif

!  Channel IR 5
!  ============

         c = 5

!  Testing if the channel is enabled (1)
!  -------------------------------------

         if(ibits(status_channel,8,1).eq.1)then

!  Testing the calibration quality flag
!  ------------------------------------

            if(ibits(avh_calqual(3),7,1).eq.0)then

               k1=avh_calir(1,1,3)*scale3
               k2=avh_calir(2,1,3)*scale4
               k3=avh_calir(3,1,3)*scale5

               good_lines(5) = good_lines(5)+1

!  Loop on mpos pixels
!  -------------------

               do p=1,mpos
                  rcount = float(counts(c,p))
!!!!!             radiance = k1*rcount*rcount + k2*rcount + k3
                  radiance = k1 + k2*rcount + k3*rcount*rcount

                  avh_rad(c,p) = radiance
                  avh_bt(c,p)=avh_cir_r(radiance,3)

               enddo
            else

!  Calibration not usable 
!  ----------------------

               bad_lines(5) = bad_lines(5) + 1
               do p=1,mpos
                  avh_rad(c,p) = bmiss
                  avh_bt(c,p)  = bmiss
               enddo
            endif
         else

!  Channel is disabled
!  -------------------

            bad_lines(5) = bad_lines(5) + 1
            do p=1,mpos
               avh_rad(c,p) = bmiss
               avh_bt(c,p)  = bmiss
            enddo
         endif

      endif

      good_qc = 0
      if(missing == 0) then
         do p=1,mpos
            avh_qc(p) = good_qc		! good line
         end do
      else

         bad_qc = ibset(good_qc,bit_qc)

         print*,'missing AVHRR line',line 

         do p = 1, mpos
            avh_qc(p) = bad_qc   	! missing line
         end do
      endif

      END SUBROUTINE GAC_LBC


      SUBROUTINE AVH_ICON
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: AVH_ICON
!   PRGMMR: KEYSER           ORG: NP22        DATE: 2006-10-19
!
! ABSTRACT: Initializes lookup tables to convert radiances into temperature for
!   the 3 AVHRR IR channels.
!
! PROGRAM HISTORY LOG:
! 1995-09-01  Marcel Derrien   Original author
! 2003-01-14  Xu Li            ????
! 2006-10-19  Keyser           Improved docblocks and comments
!
! USAGE: CALL AVH_ICON
!
! REMARKS: None
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 (free format)
!   MACHINE:  NCEP WCOSS
!
!$$$
      implicit none                                                     

!  DECLARATIONS
!  ------------

      character*62 sccsid 

      data sccsid/'@(#) avh_icon.F version 1.1 on 3/24/98'/

!  COMMONS
!  -------

      include 'cavh_cpi.inc'
      real rbeg(3),rstep(3),rend(3)
      real tconv(4000,3)
      common/temp_conv/rbeg,rstep,rend,tconv

!  Local variables
!  ---------------

      real r
      integer i
      integer c
      integer ios

!  FUNCTIONS
!  ---------

      real avh_brig

      rbeg(1)=1.e-4

      rbeg(2)=1.
      rbeg(3)=1.
      rstep(1)=5.e-04
      rstep(2)=0.1
      rstep(3)=0.1
      do i = 1 , 3
         rend(i)=rbeg(i) + 3999.* rstep(i)
      enddo
      do i = 1,4000
         do c = 1,3
            r = rbeg(c) + (i-1)* rstep(c)
            tconv(i,c) = avh_brig(r,c,ios)
         enddo
      enddo

      END SUBROUTINE AVH_ICON


      SUBROUTINE LAG(Y,IMODE)
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: LAG
!   PRGMMR: KEYSER           ORG: NP22        DATE: 2006-10-19
!
! ABSTRACT: Given selected parameter (solar zenith angle, satellite zenith
!   angle, relative azuimuth angle,lat,lon) on 51 spaced-apart pixels in one
!   AVHRR GAC scan, inteprolates parameter to all pixels in scan.
!
! PROGRAM HISTORY LOG:
! 2003-01-14  Xu Li            Original author
! 2006-10-19  Keyser           Improved docblocks and comments
!
! USAGE: CALL LAG(Y,IMODE)
!   INPUT ARGUMENTS:
!     Y      - Parameter with values on 51 spaced-apart pixels
!     IMODE  - Identifies case where parameter passed in (Y) is longitude (=1)
!              since extra logic is invoked (this is set to 0 for all other
!              parameters)
!
!   OUTPUT ARGUMENTS:
!     Y      - Parameter with values on all pixels (via interpolation)
!
! REMARKS: NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 (free format)
!   MACHINE:  NCEP WCOSS
!
!$$$

      implicit none

      integer, parameter :: n1=5, dn=8
      real, dimension(mpos) :: y0,y

      integer :: n0,n,imode,domd
      real :: L,max

!  Lagrangian interpolation for the pixels between n1 + 1 = 6 and mpos/2  = 204 
!  ----------------------------------------------------------------------------

                         ! n0 = 13, 29, 45, ... , 173,189
      do n0 = n1 + dn , mpos/2, 2*dn
                         ! n  = (6...11 12 14...19 20)... (182...188,190...196)
         do n = n0 - (dn - 1), n0 + (dn - 1)
            if ( n /= n0 ) then
               y(n) = 0.0
               max = -99999.0
               do i = n0 - dn , n0 + dn, dn
                  if ( y(i) >= max ) then
                     max = y(i)
                  endif
               enddo
               do i = n0 - dn , n0 + dn, dn
                  L = 1.0
                  do j = n0 - dn , n0 + dn, dn
                     if (j /= i ) then
                        L = L*real(n-j)/real(i-j)
                     endif
                  enddo

!  Handle transition across dateline {abs(slon) close to 180 and sign changes}
!  ---------------------------------------------------------------------------

                  y0(i) = y(i)
                  if ( y0(i) < -50.0 .and. imode == 1 .and. max > 0.0 ) then 
                     y0(i) = y0(i) + 360.0
                  endif
                  y(n) = y(n) + L*y0(i)
               enddo
               if ( imode == 1 ) then
                  if ( y(n) > 180.0 ) then 
                     y(n) = y(n) - 360.0
                  elseif ( y(n) < -180.0 ) then
                     y(n) = y(n) + 360.0
                  endif
               endif
            endif
         enddo
      enddo

!  Lagrangian interpolation for the pixels between mpos/2 - 1 = 404 and
!   mpos/2 + dn  = 214 
!  --------------------------------------------------------------------

                         ! n0 = 397, 381,  ..., 237,221
                         ! n = (404...398 396... 390)... (228...222,220...214)
      do n0 = mpos -dn/2 - dn , mpos/2 + 2, -2*dn
         do n = n0 + (dn - 1), n0 - (dn - 1), -1
            if ( n /= n0 ) then
               y(n) = 0.0
               max = -99999.0
               do i = n0 + dn , n0 - dn, -dn
                  if ( y(i) >= max ) then
                     max = y(i)
                  endif
               enddo
               do i = n0 + dn , n0 - dn, -dn
                  L = 1.0
                  do j = n0 + dn , n0 - dn, -dn
                     if (j /= i ) then
                        L = L*real(n-j)/real(i-j)
                      endif
                  enddo

!  Handle transition across dateline {abs(slon) close to 180 and sign changes}
!  ---------------------------------------------------------------------------

                  y0(i) = y(i)
                  if ( y0(i) < -50.0 .and. imode == 1 .and. max > 0.0 ) then 
                     y0(i) = y0(i) + 360.0
                  endif
                  y(n) = y(n) + L*y0(i)
               enddo
               if ( imode == 1 ) then
                  if ( y(n) > 180.0 ) then 
                     y(n) = y(n) - 360.0
                  elseif ( y(n) < -180.0 ) then
                     y(n) = y(n) + 360.0
                  endif
               endif
            endif
         enddo
      enddo

!  5-point Lagrangian extrapolation for the begining pixels n = 1 - 4 
!  ------------------------------------------------------------------

      do n = 1, n1 - 1
         y(n) = 0.0
         max = -99999.0
         do i = n1, n1 + 4*dn, dn                     ! 5 13 21 29 37 45
            if ( y(i) >= max ) then
               max = y(i)
            endif
         enddo
         do i = n1, n1 + 4*dn, dn                     ! 5 13 21 29 37 45
            L = 1.0
            do j = n1, n1 + 4*dn, dn
               if (j /= i ) then
                  L = L*real(n-j)/real(i-j)
               endif
            enddo

!  Handle transition across dateline {abs(slon) close to 180 and sign changes}
!  ---------------------------------------------------------------------------

            y0(i) = y(i)
            if ( y0(i) < -50.0 .and. imode == 1 .and. max > 0.0 ) then 
               y0(i) = y0(i) + 360.0
            endif
            y(n) = y(n) + L*y0(i)
         enddo
         if ( imode == 1 ) then
            if ( y(n) > 180.0 ) then 
               y(n) = y(n) - 360.0
            elseif ( y(n) < -180.0 ) then
               y(n) = y(n) + 360.0
            endif
         endif
      enddo

!  3-point Lagrangian interpolation for the pixels (198, ..., 204)
!  ---------------------------------------------------------------

      do n = mpos/2 - dn + 2, mpos/2                  ! 198 - 204
         y(n) = 0.0
         max = -99999.0
         do i = mpos/2 + 1 - 2*dn, mpos/2 + 1, dn     ! 189,197,205
            if ( y(i) >= max ) then
               max = y(i)
            endif
         enddo
         do i = mpos/2 + 1 - 2*dn, mpos/2 + 1, dn     ! 189,197,205
            L = 1.0
            do j = mpos/2 + 1 - 2*dn, mpos/2 + 1, dn  ! 189,197,205 
               if (j /= i ) then
                  L = L*real(n-j)/real(i-j)
               endif
            enddo

!  Handle transition across dateline {abs(slon) close to 180 and sign changes}
!  ---------------------------------------------------------------------------

            y0(i) = y(i)
            if ( y0(i) < -50.0 .and. imode == 1 .and. max > 0.0 ) then 
               y0(i) = y0(i) + 360.0
            endif
            y(n) = y(n) + L*y0(i)
         enddo
         if ( imode == 1 ) then
            if ( y(n) > 180.0 ) then 
               y(n) = y(n) - 360.0
            elseif ( y(n) < -180.0 ) then
               y(n) = y(n) + 360.0
            endif
         endif
      enddo

!  3-point Lagrangian interpolation for the pixels (206, ..., 212)
!  ---------------------------------------------------------------

      do n = mpos/2 + 2, mpos/2 + dn                  ! 206 - 212
         y(n) = 0.0
         max = -99999.0
         do i = mpos/2 + 1, mpos/2 + 1 + 2*dn, dn     ! 205,213,221
            if ( y(i) >= max ) then
               max = y(i)
            endif
         enddo
         do i = mpos/2 + 1, mpos/2 + 1 + 2*dn, dn     ! 205,213,221
            L = 1.0
            do j = mpos/2 + 1, mpos/2 + 1 + 2*dn, dn  ! 205,213,221
               if (j /= i ) then
                  L = L*real(n-j)/real(i-j)
               endif
            enddo

!  Handle transition across dateline {abs(slon) close to 180 and sign changes}
!  ---------------------------------------------------------------------------

           y0(i) = y(i)
           if ( y0(i) < -50.0 .and. imode == 1 .and. max > 0.0 ) then 
              y0(i) = y0(i) + 360.0
           endif
           y(n) = y(n) + L*y0(i)
        enddo
        if ( imode == 1 ) then
           if ( y(n) > 180.0 ) then 
              y(n) = y(n) - 360.0
           elseif ( y(n) < -180.0 ) then
              y(n) = y(n) + 360.0
           endif
        endif
     enddo

!  5-point Lagrangian extrapolation for the ending pixels (406-mpos)
!  -----------------------------------------------------------------

     do n = mpos - dn/2 + 1, mpos
        y(n) = 0.0
        max = -99999.0
        do i = mpos - dn/2 - 4*dn , mpos - dn/2 , dn
           if ( y(i) >= max ) then
              max = y(i)
           endif
        enddo
        do i = mpos - dn/2 - 4*dn , mpos - dn/2 , dn
           L = 1.0
           do j = mpos - dn/2 - 4*dn , mpos - dn/2 , dn
              if (j /= i ) then
                 L = L*real(n-j)/real(i-j)
              endif
           enddo

!  Handle transition across dateline {abs(slon) close to 180 and sign changes}
!  ---------------------------------------------------------------------------

           y0(i) = y(i)
           if ( y0(i) < -50.0 .and. imode == 1 .and. max > 0.0 ) then 
              y0(i) = y0(i) + 360.0
           endif
           y(n) = y(n) + L*y0(i)
        enddo
        if ( imode == 1 ) then
           if ( y(n) > 180.0 ) then 
              y(n) = y(n) - 360.0
           elseif ( y(n) < -180.0 ) then
              y(n) = y(n) + 360.0
           endif
        endif
     enddo

     END SUBROUTINE LAG


     SUBROUTINE DATTIM(IDT,NDT)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
! SUBPROGRAM:    DATTIM 
!   PRGMMR: Katz             ORG: NP2        DATE: 1997-11-06             
!                                                                       
! ABSTRACT: Converts year, day-of-year, and second-of-day into year,
!   month-of-year, day-of-month, hour-of-day, minute-of-hour, and
!   second-of-minute.
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1997-11-06  Katz  Original author
!                                                                       
! USAGE:    CALL DATTIM(IDT,NDT)
!   INPUT ARGUMENT LIST:                                               
!     IDT      - Integer array argument containing three members:
!                year, day-of-year, and second-of-day
!
!   OUTPUT ARGUMENT LIST:
!     KDT      - Integer array argument containing six members:
!                year, month-of-year, day-of-month, hour-of-day, 
!                minute-of-hour, and second-of-minute
!
! Remarks:                                                              
!                                                                       
! Attributes:                                                           
!   Language: FORTRAN 90 (free format)
!   Machine:  NCEP WCOSS
!                                                                       
!$$$                                                                    

!  Array arguments ..
!  ------------------

      integer idt(3),ndt(6)

      integer :: julian

!  Local scalars ..
!  ----------------

      integer :: iday,imon,iyr,jday,jda,idyr,idaywk,idayyr
      integer :: kyr,idy,jdn,jmo

!  External subroutines ..
!  -----------------------

      external w3fs26

!  Intrinsic functions ..
!  ----------------------

      intrinsic mod

      julian(IYR,IDYR) = -31739 + 1461 * (IYR + 4799) / 4   &
                          -3 * ((IYR + 4899) / 100) / 4 + IDYR

      iyr  = idt(3)
      jday = idt(2)

!  If year is two digits, convert to 4 digits
!  ------------------------------------------

      if (iyr.ge.0.and.iyr.le.99) then
         if (iyr.lt.21) then
            kyr = iyr + 2000
         else
            kyr = iyr + 1900
         endif
      else
         kyr = iyr
      endif

!  Compute Julian day number as number days after 4713 BC
!  ------------------------------------------------------

      idy = jday
      jdn = julian(kyr,idy)

      call w3fs26(jdn,iyear,jmo,jda,idaywk,idayyr)

      ndt(1) = iyr
      ndt(2) = jmo
      ndt(3) = jda
      ndt(4) = idt(1)/3600
      ndt(5) = mod(idt(1),3600)/60
      ndt(6) = idt(1) - 3600*ndt(4) - 60*ndt(5)

      END SUBROUTINE DATTIM

END MODULE GAC_1B_PROC

