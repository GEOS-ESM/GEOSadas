c$$$  Subprogram Documentation Block
c   BEST VIEWED WITH 94-CHARACTER WIDTH WINDOW
c
c Subprogram: tranQCflags
c   Programmer: D. Keyser       Org: NP22       Date: 2012-05-08
c
c Abstract:  Translates quality information from NRL standards to equivalent NCEP PREPBUFR
c   quality marks.  Also generates the NCEP PREPBUFR reason codes based on the NRL quality
c   information.  This is read in for one observation at a time for each report (e.g.,
c   pressure reads it, then altitude, then temperature, then moisture, then wind).
c
c Program History Log:
c 2010-11-15  S. Bender -- Original Author
c 2012-05-08  D. Keyser -- Prepared for operational implementation
c
c Usage: call tranQCflags(NRLQCstg,type,NCEPqm,NCEPrc,l_badrpt,l_duprpt)
c
c   Input argument list:
c     NRLQCstg     - NRLACQC quality information (11 char. string) for this complete report
c     type         - Type of variable being considered in this call (e.g., 'p', 'z', 't',
c                    'q', 'w')
c
c   Output argument list:
c     NCEPqm       - Equivalent NCEP PREPBUFR quality mark for this variable
c     NCEPrc       - Generated NCEP PREPBUFR reason code for this variable
c     l_badrpt     - Logical indicating if the entire report should be marked as "bad"
c     l_duprpt     - Logical indicating if the entire report should be marked as a duplicate
c
c   Output files:
c     Unit 06     - Standard output print
c
c   Subprograms called:
c     Library:
c       W3NCO:   ERREXIT    W3TAGE     MOVA2I
c
c   Exit States:
c     Cond =  0 - successful run
c            69 - row number for input data matrix is outside range of 1-34
c
c Remarks: Called by subroutines output_acqc_noprof and sub2mem_um.
c
c Attributes:
c   Language: FORTRAN 90
c   Machine:  NCEP WCOSS
c
c$$$
      subroutine tranQCflags(NRLQCstg,type,NCEPqm,NCEPrc,l_badrpt,
     +                       l_duprpt)

      implicit none

      character*11 NRLQCstg
      character*1 type

      logical l_badrpt,l_duprpt

      integer iii(45:118)               ! the "row number" (RN) corresponding to the single
                                        !  character NRLACQC  quality flag pulled out of
                                        !  NRLQCstg, this is the second dimension of table
                                        !  w2d
     +,         mova2i

      integer   cp                      ! character position in c_qc string (1-11), this
                                        !  value minus 1 is the first dimension of table w2d
     +,         RN                      ! row number in w2d/action data table/numerical 
                                        !  equivalent of character value (example: a=1,
                                        !  F=10), this is obtained via integer conversion of
                                        !  the single ASCII character NRLACQC quality flag
                                        !  pulled out of NRLQCstg, obtained from iii

      character*2 w2d(0:10,34)
c             0 = overall report
c              |--- 1 = time
c              |--- |--- 2 = latitude
c              |--- |--- |--- 3 = longitude
c              |--- |--- |--- |--- 4 = pressure/altitude
c              |--- |--- |--- |--- |--- 5 = temperature
c              |--- |--- |--- |--- |--- |--- 6 = wind direction
c              |--- |--- |--- |--- |--- |--- |--- 7 = wind speed
c              |--- |--- |--- |--- |--- |--- |--- |--- 8 = moisture
c              |--- |--- |--- |--- |--- |--- |--- |--- |--- 9 = reject (black) list
c              |--- |--- |--- |--- |--- |--- |--- |--- |--- |--- 10 = flight phase
c              |--- |--- |--- |--- |--- |--- |--- |--- |--- |--- |---
c       i: 0:10 (cp-1) -------------->
      data w2d/'ND','ND','ND','ND','ND','ND','ND','ND','ND','ND','IO', ! a  j:1-34 (RN) 1 |
     +         'RR','ND','ND','ND','ND','ND','ND','RW','ND','ND','IO', ! A	        2 |
     +         'RR','RR','RR','RR','RR','RR','RW','RW','RM','ND','ND', ! B	        3 |
     +         'ND','ND','ND','ND','ND','RT','ND','ND','ND','ND','ND', ! b	        4 V
     +	       'ND','ND','ND','ND','ND','ND','ND','ND','ND','IO','ND', ! C              5
     +	       'DR','ND','ND','ND','ND','ND','ND','ND','ND','ND','IO', ! D              6
     +	       'DR','ND','ND','ND','ND','ND','ND','ND','ND','ND','IO', ! d              7
     +	       'RR','ND','ND','ND','ND','RT','RW','RW','ND','ND','ND', ! E              8
     +	       'RR','ND','ND','ND','ND','ND','ND','ND','ND','ND','ND', ! e              9
     +	       'ND','ND','ND','ND','ND','ND','ND','ND','ND','ND','ND', ! F             10
     +	       'ND','RR','RR','RR','RR','RT','RW','RW','ND','ND','ND', ! I             11
     +	       'ND','ND','ND','ND','RR','ND','ND','ND','ND','ND','IO', ! i             12
     +	       'ND','RR','RR','RR','RR','CW','CT','CT','RM','ND','ND', ! K             13
     +	       'ND','ND','ND','ND','ND','ND','ND','ND','ND','ND','IO', ! L             14
     +	       'ND','RR','RR','RR','RR','CW','CT','CT','RM','IO','IO', ! M             15
     +	       'NU','NU','NU','NU','ND','NU','ND','ND','NU','ND','IO', ! N             16
     +         'RR','ND','ND','ND','ND','ND','ND','ND','ND','RR','ND', ! O             17
     +         'RR','ND','ND','ND','ND','ND','ND','ND','ND','ND','ND', ! P             18
     +         'RR','ND','ND','ND','ND','ND','ND','ND','ND','ND','ND', ! p             19
     +	       'ND','IO','IO','IO','IO','IO','ND','ND','ND','ND','ND', ! R             20
     +	       'RR','ND','ND','ND','IO','ND','ND','ND','ND','ND','ND', ! r             21
     +	       'RR','RR','RR','RR','RR','ND','ND','SW','SM','ND','ND', ! S             22
     +	       'RR','ND','ND','ND','ND','ND','SW','SW','ND','ND','ND', ! s             23
     +	       'ND','ND','ND','ND','ND','ND','ND','ND','ND','RT','ND', ! T             24
     +	       'RR','ND','ND','ND','ND','ND','ND','ND','ND','ND','ND', ! t             25
     +	       'ND','ND','ND','ND','ND','ND','ND','ND','ND','ND','IO', ! U             26
     +	       'RR','ND','ND','ND','ND','ND','ND','ND','ND','ND','ND', ! V             27
     +	       'RR','ND','ND','ND','ND','ND','ND','ND','ND','ND','ND', ! v             28
     +	       'RR','ND','ND','ND','ND','ND','ND','ND','ND','RW','ND', ! W             29
     +	       'RR','ND','ND','ND','ND','ND','ND','ND','ND','ND','ND', ! X             30
     +	       'GR','IO','IO','IO','GV','GT','GW','GW','GM','IO','ND', ! .             31
     +	       'NU','ND','ND','ND','NU','NU','NU','NU','NU','ND','ND', ! -             32
     +	       'IO','ND','ND','ND','ND','ND','ND','ND','SM','ND','ND', ! 2             33
     +	       'ND','ND','ND','ND','ND','ND','ND','ND','SM','ND','ND'  ! 3             34
     +	      /

c  'CT' --  check temperature -> reject wind, or will reject report if temperature also bad
c  'CW' --  check wind -> reject temperature, or will reject report if wind also bad
c  'DR' --  duplicate report
c  'GM' --  good moisture
c  'GR' --  good report
c  'GT' --  good temperature`
c  'GV' --  good pressure/altitude
c  'GW' --  good wind
c  'IO' --  inconclusive (?)
c  'ND' --  not defined
c  'NU' --  neutral report (not checked)
c  'RM' --  reject moisture
c  'RR' --  reject entire report
c  'RT' --  reject temperature
c  'RW' --  reject wind
c  'SM' --  suspect moisture
c  'SW' --  suspect wind
c  'XX' --  initialized value (not yet set)

      character*2 action                ! action value to be passed back to the calling
                                        !  routine (RR,DR,GR,RT,RM,RW,SW,NU,ND,IO)
     +,           bl_action             ! reject (black) list action (c_qc(10:10))
     +,	          pres_action           ! pressure/altitude action (c_qc(5:5))
     +,           temp_action 	        ! temperature action (c_qc(6:6))
     +,           moist_action          ! moisture action (c_qc(9:9))
     +,	          wdir_action           ! wind direction action (c_qc(7:7))
     +,           wspd_action           ! wind speed action (c_qc(8:8))
     +,           lat_action            ! latitude action (c_qc(3:3))
     +,           lon_action            ! longitude action (c_qc(4:4))
     +,	          time_action           ! time action (c_qc(2:2))
     +,           overall_action        ! action per c_qc(1:1)

      integer     NCEPqm                ! value of NCEP quality mark to be passed back to
                                        !  calling routine
     +,           NCEPrc                ! value of NCEP reason code to be passed back to
                                        !  calling routine
     +,           NCEPrc_t              ! intermediate value for temperature quality mark
     +,           NCEPrc_q              ! intermediate value for moisture quality mark
     +,           NCEPrc_w              ! intermediate value for wind quality mark


c Misc.
c -----

c decimal -->     45  46              50  51
c character -->  '-' '.'             '2' '3'
      data iii  / 32, 31,  0,  0,  0, 33, 34,  0,  0,  0,  0,  0,  0,

c decimal -->                                 65  66  67  68  69  70
c character -->                              'A' 'B' 'C' 'D' 'E' 'F'
     +             0,  0,  0,  0,  0,  0,  0,  2,  3,  5,  6,  8, 10,

c decimal -->             73      75  76  77  78  79  80      82  83
c character -->          'I'     'K' 'L' 'M' 'N' 'O' 'P'     'R' 'S'
     +             0,  0, 11,  0, 13, 14, 15, 16, 17, 18,  0, 20, 22,

c decimal -->     84  85  86  87  88
c character -->  'T' 'U' 'V' 'W' 'X'
     +            24, 26, 27, 29, 30,  0,  0,  0,  0,  0,  0,  0,  0,

c decimal -->     97  98     100 101             105
c character -->  'a' 'b'     'd' 'e'             'i'
     +             1,  4,  0,  7,  9,  0,  0,  0, 12,  0,  0,  0,  0,

c decimal -->            112     114 115 116     118
c character -->          'p'     'r' 's' 't'     'v'
     +             0,  0, 19,  0, 21, 23, 25,  0, 28 /

c -----------------------------------------------------------

c Initialize variables
c --------------------
      l_badrpt = .false.
      l_duprpt = .false.

      bl_action      = 'XX'
      pres_action    = 'XX'
      temp_action    = 'XX'
      moist_action   = 'XX'
      wdir_action    = 'XX'
      wspd_action    = 'XX'
      lat_action     = 'XX'
      lon_action     = 'XX'
      time_action    = 'XX'
      overall_action = 'XX'
      action         = 'XX'

      NCEPqm   = 99999
      NCEPrc   = 99999
      NCEPrc_t = 99999
      NCEPrc_q = 99999
      NCEPrc_w = 99999
c -----------------------------------------------------------

C *************************************************************************
c FIRST CHECK FOR UNILATERAL REJECT REPORT - APPLIES TO ALL VARIABLES
C *************************************************************************

c ---------------------------------------------------------------------
c First sub-check is on OVERALL REPORT (first character of c_qc string)
c ---------------------------------------------------------------------
      cp = 1

c iii represents the "row number" corresponding to the single ASCII character NRLACQC
c   quality flag NRLQCstg(cp:cp) and is the second dimension of table w2d (Note: this comment
c   is not repeated for each instance where iii is obtained below)
c -------------------------------------------------------------------------------------------
      RN = iii(mova2i(NRLQCstg(cp:cp)))
      if(RN.lt.0 .or. RN.gt.34) go to 999
      action = w2d(cp-1,RN) ! either 'ND', 'RR', 'IO', 'NU', 'GR', 'DR'
      overall_action = action
      if(action.eq.'RR' .or. action.eq.'DR') then
        NCEPrc = (cp-1)*100 + RN ! RC range 001-034
        go to 2000 ! reject entire report here means we don't need to do anymore testing
      endif

c ------------------------------------------------------------------------
C If we make it to here ...
c Second sub-check is on PRESSURE/ALTITUDE (fifth character of c_qc string)
c -------------------------------------------------------------------------
      cp = 5
      RN = iii(mova2i(NRLQCstg(cp:cp)))
      if(RN.lt.0 .or. RN.gt.34) go to 999
      action = w2d(cp-1,RN) ! either 'ND', 'RR', 'IO', 'NU', 'GV'
      pres_action = action
      if(action.eq.'RR') then
        NCEPrc = (cp-1)*100 + RN ! RC range 401-434
        go to 2000 ! reject entire report here means we don't need to do anymore testing
      endif

c ------------------------------------------------------------------
C If we make it to here ...
c Third sub-check is on TEMPERATURE (sixth character of c_qc string)
c ------------------------------------------------------------------
      cp = 6
      RN = iii(mova2i(NRLQCstg(cp:cp)))
      if(RN.lt.0 .or. RN.gt.34) go to 999
      action = w2d(cp-1,RN) ! either 'ND', 'RR', 'IO', 'NU', 'RT', 'GT', 'CW'
      temp_action = action
      if(temp_action.eq.'RT') then
c If temperature action is reject temperature ('RT'), change reject (black) list value in
c  tenth character of c_qc string from 'O' (reject entire report) to 'W' (reject wind only) -
c  this prevents eighth sub-check below from masking QM (13) & RC associated with this code's
c  reject of temperature {instead it would receive reject (black) list QM (14) and RC}
        if(NRLQCstg(10:10).eq.'O')  NRLQCstg(10:10) = 'W'
      endif
      if(action.eq.'RR') then
        NCEPrc = (cp-1)*100 + RN ! RC range 501-534
        go to 2000 ! reject entire report here means we don't need to do anymore testing
      endif

c -------------------------------------------------------------
C If we make it to here ...
c Fourth sub-check is on TIME (second character of c_qc string)
c -------------------------------------------------------------
      cp = 2
      RN = iii(mova2i(NRLQCstg(cp:cp)))
      if(RN.lt.0 .or. RN.gt.34) go to 999
      action = w2d(cp-1,RN) ! either 'ND', 'RR', 'IO', 'NU'
      time_action = action
      if(action.eq.'RR') then
        NCEPrc = (cp-1)*100 + RN ! RC range 101-134
        go to 2000 ! reject entire report here means we don't need to do anymore testing
      endif

c ---------------------------------------------------------------
C If we make it to here ...
c Fifth sub-check is on LATITUDE (third character of c_qc string)
c ---------------------------------------------------------------
      cp = 3
      RN = iii(mova2i(NRLQCstg(cp:cp)))
      if(RN.lt.0 .or. RN.gt.34) go to 999
      action = w2d(cp-1,RN) ! either 'ND', 'RR', 'IO', 'NU'
      lat_action = action
      if(action.eq.'RR') then
        NCEPrc = (cp-1)*100 + RN ! RC range 201-234
        go to 2000 ! reject entire report here means we don't need to do anymore testing
      endif

c -----------------------------------------------------------------
C If we make it to here ...
c Sixth sub-check is on LONGITUDE (fourth character of c_qc string)
c -----------------------------------------------------------------
      cp = 4
      RN = iii(mova2i(NRLQCstg(cp:cp)))
      if(RN.lt.0 .or. RN.gt.34) go to 999
      action = w2d(cp-1,RN) ! either 'ND', 'RR', 'IO', 'NU'
      lon_action = action
      if(action.eq.'RR') then
        NCEPrc = (cp-1)*100 + RN ! RC range 301-334
        go to 2000 ! reject entire report here means we don't need to do anymore testing
      endif

c -------------------------------------------------------------------------------------------
C If we make it to here ...
c Seventh sub-check is on TEMPERATURE/WIND COMBINATION (sixth thru tenth char of c_qc string)
c -------------------------------------------------------------------------------------------
c We already know temperature action (from sixth character of c_qc string) from above
c  {temp_action, either 'ND', 'IO', 'NU', 'RT', 'GT', 'CW' ('RR' already considered)}

c Obtain wind direction action from seventh character of c_qc string (wdir_action)
      cp = 7
      RN = iii(mova2i(NRLQCstg(cp:cp)))
      if(RN.lt.0 .or. RN.gt.34) go to 999
      wdir_action = w2d(cp-1,RN) ! either 'ND', 'NU', 'RW', 'SW', 'GW', 'CT'
                                 !                              (Note: 'RR' not a choice here)
      if(wdir_action.eq.'RW') then
c If wind direction action is reject wind ('RW'), change reject (black) list value in tenth
c  character of c_qc string from 'O' (reject entire report) to 'T' (reject temperature only)
c  - this prevents eighth sub-check below from masking QM (13) & RC associated with this
c  code's reject of wind {instead it would receive reject (black) list QM (14) & RC}
        if(NRLQCstg(10:10).eq.'O')  NRLQCstg(10:10) = 'T'
      endif

c Obtain wind speed action from eighth character of c_qc string (wspd_action)
      cp = 8
      RN = iii(mova2i(NRLQCstg(cp:cp)))
      if(RN.lt.0 .or. RN.gt.34) go to 999
      wspd_action = w2d(cp-1,RN) ! either 'ND', 'NU', 'RW', 'SW', 'GW', 'CT'
                                 !                              (Note: 'RR' not a choice here)
      if(wspd_action.eq.'RW') then
c If wind speed action is reject wind ('RW'), change reject (black) list value in tenth
c  character of c_qc string from 'O' (reject entire report) to 'T' (reject temperature only)
c  - this prevents eighth sub-check below from masking QM (13) & RC associated with this
c   code's reject of wind {instead it would receive reject (black) list QM (14) and RC}
        if(NRLQCstg(10:10).eq.'O')  NRLQCstg(10:10) = 'T'
      endif

c Obtain moisture action from ninth character of c_qc string (wspd_action)
      cp = 9
      RN = iii(mova2i(NRLQCstg(cp:cp)))
      if(RN.lt.0 .or. RN.gt.34) go to 999
      moist_action = w2d(cp-1,RN) ! either 'ND', 'NU', 'RM', 'SM', 'GM'
                                  !                             (Note: 'RR' not a choice here)

c Obtain reject (black) list action from tenth character of c_qc string (bl_action)
      cp = 10
      RN = iii(mova2i(NRLQCstg(cp:cp)))
      if(RN.lt.0 .or. RN.gt.34) go to 999
      bl_action = w2d(cp-1,RN) ! either 'ND', 'RR', 'IO', 'RT', 'RW'

c If temperature action is to check wind ('CW'), then a "bad" wind will result in the entire
c  report being rejected
      if(temp_action.eq.'CW') then
        action = 'RR'
        cp = 6
        RN = iii(mova2i(NRLQCstg(cp:cp)))
        if(RN.lt.0 .or. RN.gt.34) go to 999
        NCEPrc_t = (cp-1)*100 + RN ! temperature RC range 501-534
c .... first check wind direction to see if it is "bad"
        if(wdir_action.eq.'CT'.or.wdir_action.eq.'RW') then
          cp = 7
          RN = iii(mova2i(NRLQCstg(cp:cp)))
          if(RN.lt.0 .or. RN.gt.34) go to 999
	  NCEPrc_w = (cp-1)*100 + RN ! wind RC range 601-634
c ........ a bad wind direction rejects entire report
c ........ set RC for moisture to moisture action if moisture rejected, otherwise set it to
c           RC for temperature
          if(moist_action.eq.'RM') then
            cp = 9
            RN = iii(mova2i(NRLQCstg(cp:cp)))
            if(RN.lt.0 .or. RN.gt.34) go to 999
	    NCEPrc_q = (cp-1)*100 + RN ! moisture RC range 801-834
          else
	    NCEPrc_q = NCEPrc_t ! moisture RC range 501-534
          endif
          go to 2000 ! reject entire report here means we don't need to do anymore testing
        elseif(wspd_action.eq.'CT'.or.wspd_action.eq.'RW') then
c .... next check wind speed to see if it is "bad"
	  cp = 8
          RN = iii(mova2i(NRLQCstg(cp:cp)))
          if(RN.lt.0 .or. RN.gt.34) go to 999
	  NCEPrc_w = (cp-1)*100 + RN ! wind RC range 701-734
c ........ a bad wind speed rejects entire report
c ........ set RC for moisture to moisture action if moisture rejected, otherwise set it to
c           RC for temperature
          if(moist_action.eq.'RM') then
            cp = 9
            RN = iii(mova2i(NRLQCstg(cp:cp)))
            if(RN.lt.0 .or. RN.gt.34) go to 999
	    NCEPrc_q = (cp-1)*100 + RN ! moisture RC range 801-834
          else
	    NCEPrc_q = NCEPrc_t ! moisture RC range 501-534
          endif
          go to 2000 ! reject entire report here means we don't need to do anymore testing
        elseif(bl_action.eq.'RW' ) then
c .... finally check reject (black) list to see if wind (direction/speed) is rejected ("bad")
c       (Note: bl_action = 'RR' will be considered separately in eighth sub-check below)
	  cp = 10
          RN = iii(mova2i(NRLQCstg(cp:cp)))
          if(RN.lt.0 .or. RN.gt.34) go to 999
	  NCEPrc_w = (cp-1)*100 + RN ! wind RC range 901-934
c ........ wind on reject (black) list rejects entire report
c ........ set RC for moisture to moisture action if moisture rejected, otherwise set it to
c           RC for temperature
          if(moist_action.eq.'RM') then
            cp = 9
            RN = iii(mova2i(NRLQCstg(cp:cp)))
            if(RN.lt.0 .or. RN.gt.34) go to 999
	    NCEPrc_q = (cp-1)*100 + RN ! moisture RC range 801-834
          else
	    NCEPrc_q = NCEPrc_t ! moisture RC range 501-534
          endif
          go to 2000 ! reject entire report here means we don't need to do anymore testing
        else
c .... even though temperature action is to check wind ('CW'), wind is not "bad", so
c      temperature (and wind and moisture if they are present) will be tested later as a
c      single variable unless eighth sub-check below yields a reject entire report
	  action = 'XX' ! reset action back to initialized value
	  NCEPrc_t = 99999  ! reset temperature reason code back to initialized value
        endif
      else
c Temperature action is something other than check wind ('CW') {or, for that matter, reject
c  report ('RR')}, so temperature (and wind and moisture if present) will be tested later as
c  a single variable unless eighth sub-check below yields a reject entire report
        temp_action = temp_action ! dummy statement to allow else branch here
      endif

c If wind direction action is to check temperature ('CT'), then a "bad" temperature will
c  result in the entire report being rejected
      if(wdir_action.eq.'CT') then
        action = 'RR'
        cp = 7
        RN = iii(mova2i(NRLQCstg(cp:cp)))
        if(RN.lt.0 .or. RN.gt.34) go to 999
	NCEPrc_w = (cp-1)*100 + RN ! wind RC range 601-634
c .... first check temperature to see if it is "bad"
        if(temp_action.eq.'CW'.or.temp_action.eq.'RT') then
	  cp = 6
          RN = iii(mova2i(NRLQCstg(cp:cp)))
          if(RN.lt.0 .or. RN.gt.34) go to 999
	  NCEPrc_t = (cp-1)*100 + RN ! temperature RC range 501-534
c ........ a bad temperature rejects entire report
c ........ set RC for moisture to moisture action if moisture rejected, otherwise set it to
c           RC for temperature
          if(moist_action.eq.'RM') then
            cp = 9
            RN = iii(mova2i(NRLQCstg(cp:cp)))
            if(RN.lt.0 .or. RN.gt.34) go to 999
	    NCEPrc_q = (cp-1)*100 + RN ! moisture RC range 801-834
          else
	    NCEPrc_q = NCEPrc_t ! moisture RC range 501-534
          endif
          go to 2000 ! reject entire report here means we don't need to do anymore testing
        elseif(bl_action.eq.'RT' ) then
c .... finally check reject (black) list to see if temperature is rejected ("bad")
c       (Note: bl_action = 'RR' will be considered separately in eighth sub-check below)
	  cp = 10
          RN = iii(mova2i(NRLQCstg(cp:cp)))
          if(RN.lt.0 .or. RN.gt.34) go to 999
	  NCEPrc_t = (cp-1)*100 + RN ! temperature RC range 901-934
c ........ temperature on reject (black) list rejects entire report
c ........ set RC for moisture to moisture action if moisture rejected, otherwise set it to
c           RC for temperature
          if(moist_action.eq.'RM') then
            cp = 9
            RN = iii(mova2i(NRLQCstg(cp:cp)))
            if(RN.lt.0 .or. RN.gt.34) go to 999
	    NCEPrc_q = (cp-1)*100 + RN ! moisture RC range 801-834
          else
	    NCEPrc_q = NCEPrc_t ! moisture RC range 501-534
          endif
          go to 2000 ! reject entire report here means we don't need to do anymore testing
        else
c .... even though wind direction action is to check temperature ('CT'), temperature is not
c      "bad", so wind (and temperature and moisture if present) will be tested later as a
c      single variable unless either wind speed check (action 'CT' checking against "bad"
c      temperature) just below yields a reject entire report, or eighth sub-check below
c      yields a reject entire report
	  action = 'XX'     ! reset action back to initialized value
	  NCEPrc_w = 99999  ! reset wind reason code back to initialized value
        endif
      else
c Wind direction action is something other than check temperature ('CT') {or, for that
c  matter, reject report ('RR')}, so wind (and temperature and moisture if present) will be
c  tested later as a single variable unless either wind speed check (action 'CT' checking
c  against "bad" temperature) just below yields a reject entire report, or eighth sub-check
c  below yields a reject entire report
        wdir_action = wdir_action ! dummy statement to allow else branch here
      endif

c If wind speed action is to check temperature ('CT'), then a "bad" temperature will result
c  in the entire report being rejected
      if(wspd_action.eq.'CT') then
        action = 'RR'
        cp = 8
        RN = iii(mova2i(NRLQCstg(cp:cp)))
        if(RN.lt.0 .or. RN.gt.34) go to 999
        NCEPrc_w = (cp-1)*100 + RN ! wind RC range 701-734
c .... first check temperature to see if it is "bad"
        if(temp_action.eq.'CW'.or.temp_action.eq.'RT') then
          cp = 6
          RN = iii(mova2i(NRLQCstg(cp:cp)))
          if(RN.lt.0 .or. RN.gt.34) go to 999
	  NCEPrc_t = (cp-1)*100 + RN ! temperature RC range 501-534
c ........ a bad temperature rejects entire report
c ........ set RC for moisture to moisture action if moisture rejected, otherwise set it to
c           RC for temperature
          if(moist_action.eq.'RM') then
            cp = 9
            RN = iii(mova2i(NRLQCstg(cp:cp)))
            if(RN.lt.0 .or. RN.gt.34) go to 999
	    NCEPrc_q = (cp-1)*100 + RN ! moisture RC range 801-834
          else
	    NCEPrc_q = NCEPrc_t ! moisture RC range 501-534
          endif
          go to 2000 ! reject entire report here means we don't need to do anymore testing
        elseif(bl_action.eq.'RT' ) then
c .... finally check reject (black) list to see if temperature is rejected ("bad")
c       (Note: bl_action = 'RR' will be considered separately in eighth sub-check below)
	  cp = 10
          RN = iii(mova2i(NRLQCstg(cp:cp)))
          if(RN.lt.0 .or. RN.gt.34) go to 999
	  NCEPrc_t = (cp-1)*100 + RN ! temperature RC range 901-934
c ........ temperature on reject (black) list rejects entire report
c ........ set RC for moisture to moisture action if moisture rejected, otherwise set it to
c           RC for temperature
          if(moist_action.eq.'RM') then
            cp = 9
            RN = iii(mova2i(NRLQCstg(cp:cp)))
            if(RN.lt.0 .or. RN.gt.34) go to 999
            NCEPrc_q = (cp-1)*100 + RN ! moisture RC range 801-834
          else
            NCEPrc_q = NCEPrc_t ! moisture RC range 501-534
          endif
          go to 2000 ! reject entire report here means we don't need to do anymore testing
        else
c .... even though wind speed action is to check temperature ('CT'), temperature is not"bad",
c      so wind (and temperature and moisture if present) will be tested later as a single
c      variable unless eighth sub-check below yields a reject entire report
	  action = 'XX'     ! reset action back to initialized value
	  NCEPrc_w = 99999  ! reset wind reason code back to initialized value
        endif
      else
c Wind speed action is something other than check temperature ('CT') {or, for that matter,
c  reject report ('RR')}, so wind (and temperature and moisture if present) will be tested
c  later as a single variable unless eighth sub-check below yields a reject entire report
        wspd_action = wspd_action ! dummy statement to allow else branch here
      endif

c ---------------------------------------------------------------------------
C If we make it to here ...
c Eighth sub-check is on REJECT (BLACK) LIST (tenth character of c_qc string)
c ---------------------------------------------------------------------------
c We already know reject (black) list action (from tenth character of c_qc string) from above
c  {bl_action, either 'ND', 'RR', 'IO', 'RT', 'RW'}
      action = bl_action
      if(action.eq.'RR') then
        cp = 10
        RN = iii(mova2i(NRLQCstg(cp:cp)))
        if(RN.lt.0 .or. RN.gt.34) go to 999
        NCEPrc = (cp-1)*100 + RN ! RC range 901-934
        go to 2000 ! reject entire report here means we don't need to do anymore testing
      endif

C *************************************************************************
C If we make it to here ...
c NEXT CHECK SINGLE VARIABLES
C *************************************************************************

      if(type.eq.'p' .or. type.eq.'z') then ! check 5
c *****************
c PRESSURE/ALTITUDE
c *****************
        cp = 5
        RN = iii(mova2i(NRLQCstg(cp:cp)))
        if(RN.lt.0 .or. RN.gt.34) go to 999
        action = w2d(cp-1,RN) ! either 'ND', 'IO', 'NU', 'GV' ('RR' already considered)
        NCEPrc = (cp-1)*100 + RN ! pressure/altitude RC range 401-434
        pres_action = action

      elseif(type.eq.'t') then ! check 6, 10
c ***********
c TEMPERATURE
c ***********
        cp = 6
        RN = iii(mova2i(NRLQCstg(cp:cp)))
        if(RN.lt.0 .or. RN.gt.34) go to 999
        action = w2d(cp-1,RN) ! either 'ND', 'IO', 'NU', 'RT', 'GT', 'CW'
                              !                                      ('RR' already considered)
        NCEPrc_t = (cp-1)*100 + RN ! temperature RC range 501-534

c A temperature action of check wind ('CW') is treated as a reject temperature ('RT') - test
c  for unilateral reject of entire report above has already tested cases where temperature
c  action of 'CW' is combined with a "bad" wind (resulting in a reject of the entire report),
c  so we know here that wind is not bad and only temperature should be rejected
c -------------------------------------------------------------------------------------------
        if(action.eq.'CW') action = 'RT'

c Check reject (black) list flag to see if temperature should be rejected {but ONLY if
c  temperature action is not already set to reject temperature ('RT')}
c ------------------------------------------------------------------------------------
        if(action.ne.'RT') then
	  cp = 10
          RN = iii(mova2i(NRLQCstg(cp:cp)))
          if(RN.lt.0 .or. RN.gt.34) go to 999
          bl_action = w2d(cp-1,RN) ! either 'ND', 'IO', 'RT', 'RW' ('RR' already considered)
	  if(bl_action.eq.'RT') then
	    action = 'RT' ! reject temperature
	    NCEPrc_t = (cp-1)*100 + RN ! temperature RC range 901-934
	  endif
	endif

      elseif(type.eq.'q') then ! check 9
c ********	
c MOISTURE
c ********	
        cp = 9
        RN = iii(mova2i(NRLQCstg(cp:cp)))
        if(RN.lt.0 .or. RN.gt.34) go to 999
        action = w2d(cp-1,RN) ! either 'ND', 'NU', 'RM', 'SM', 'GM'
                              !                                 (Note: 'RR' not a choice here)
        NCEPrc_q = (cp-1)*100 + RN ! moisture RC range 801-834
        moist_action = action

        if(action.ne.'RM') then

c A reject of temperature forces a reject of moisture (if moisture not already rejected)
c --------------------------------------------------------------------------------------
          cp = 6
          RN = iii(mova2i(NRLQCstg(cp:cp)))
          if(RN.lt.0 .or. RN.gt.34) go to 999
          temp_action = w2d(cp-1,RN) !  either 'ND', 'IO', 'NU', 'RT', 'GT', 'CW'
                                     !                               ('RR' already considered)
          if(temp_action.eq.'RT') then
            action = 'RM'
            NCEPrc_q = (cp-1)*100 + RN ! moisture RC range 501-534
          endif
        endif

      elseif(type.eq.'w') then ! check 7, 8, 10
c ****
c WIND
c ****

c First, check Wind direction action
c ----------------------------------
        cp = 7
        RN = iii(mova2i(NRLQCstg(cp:cp)))
        if(RN.lt.0 .or. RN.gt.34) go to 999
        wdir_action = w2d(cp-1,RN) ! either 'ND', 'NU', 'RW', 'SW', 'GW', 'CT'
                                   !                            (Note: 'RR' not a choice here)
     	NCEPrc_w = (cp-1)*100 + RN ! initially set overall wind RC to reflect wind direction
                               ! status, wind RC range 601-634 {this may be overwritten later
                               !  by wind speed status if it is inferior to wind direction
                               !  status (quality-wise)})

        action = wdir_action   ! initially set overall wind action to wind direction
                               !  action {this may be overwritten later by wind speed action
                               !  if it is inferior to wind direction action (quality-wise)}

c A wind direction action of check temperature ('CT') is treated as a reject wind ('RW') -
c  test for unilateral reject of entire report above has already tested cases where wind
c  direction action of 'CT' is combined with a "bad" temperature (resulting in a reject of
c  the entire report), so we know here that temperature is not bad and only wind should be
c  rejected
c ----------------------------------------------------------------------------------------
        if(wdir_action.eq.'CT') wdir_action = 'RW'

        if(wdir_action.eq.'RW') then

c If wind direction action is reject wind ('RW') then set overall wind action to 'RW' - no
c  need to examine wind speed action in this case
c ----------------------------------------------------------------------------------------
          action = 'RW'
        else

c Otherwise, check wind speed action to see if it is inferior to wind direction action
C  (quality-wise)
c ------------------------------------------------------------------------------------

          cp = 8
          RN = iii(mova2i(NRLQCstg(cp:cp)))
          if(RN.lt.0 .or. RN.gt.34) go to 999
          wspd_action = w2d(cp-1,RN) ! either 'ND', 'NU', 'RW', 'SW', 'GW', 'CT'
                                     !                          (Note: 'RR' not a choice here)

c A wind speed action of check temperature ('CT') is treated as a reject wind ('RW') - test
c  for unilateral reject of entire report above has already tested cases where wind speed
c  action of 'CT' is combined with a "bad" temperature (resulting in a reject of the entire
c  report), so we know here that temperature is not bad and only wind should be rejected
c -----------------------------------------------------------------------------------------
          if(wspd_action.eq.'CT') wspd_action = 'RW'

          if(wspd_action.eq.'RW') then

c For cases when wind direction action is not 'RW' but wind speed action is 'RW', use wind
c  speed's RC as overall wind RC value since it is inferior to wind direction action
C  (quality-wise)
c ----------------------------------------------------------------------------------------
            NCEPrc_w = (cp-1)*100 + RN !  wind RC range 701-734
            action = 'RW'
          elseif (wdir_action.eq.'SW') then

c For cases when wind direction action is suspect wind ('SW') and wind speed action is not
c  'RW', set overall wind action to 'SW' and use wind direction's RC as overall wind RC value
c  since it is inferior to wind speed action (quality-wise)
c -------------------------------------------------------------------------------------------
            action = 'SW'
          elseif (wspd_action.eq.'SW') then

c For cases when wind direction action is neither 'RW' nor 'SW' but wind speed action is
c  'SW', set overall wind action to 'SW' and use wind speed's RC as overall wind RC value
c  since it is inferior to wind direction action (quality-wise)
c ---------------------------------------------------------------------------------------
            NCEPrc_w = (cp-1)*100 + RN !  wind RC range 701-734
            action = 'SW'
          endif
        endif

c Check reject (black) list flag to see if wind should be rejected {but ONLY if overall wind
c  action is not already set to reject wind ('RW')}
c ------------------------------------------------------------------------------------------
        if(action.ne.'RW') then
          cp = 10
          RN = iii(mova2i(NRLQCstg(cp:cp)))
          if(RN.lt.0 .or. RN.gt.34) go to 999
          bl_action = w2d(cp-1,RN) ! either 'ND', 'IO', 'RT', 'RW' ('RR' already considered)
          if(bl_action.eq.'RW') then
            action = 'RW' ! reject wind
            NCEPrc_w = (cp-1)*100 + RN ! wind RC range 901-934
          endif
        endif

      endif

C *************************************************************************

 2000 continue

c -------------------------------------------
c Translate actions into NCEP QUALITY MARKERS
c -------------------------------------------
      if(action.eq.'RR') then
        l_badrpt = .true. 		 

      elseif(action.eq.'DR') then 
        l_duprpt = .true.

      elseif(action.eq.'GV'.or.action.eq.'GT' .or.
     +       action.eq.'GM'.or.action.eq.'GW') then
        NCEPqm = 1 ! good (RC already set)

      elseif(action.eq.'NU') then 
        NCEPqm = 2 ! neutral/not checked (RC already set)

      elseif(action.eq.'ND') then ! not defined
        NCEPqm = 2   ! QM -> neutral
        NCEPrc = 099 ! RC -> 099 - override any RCs already set

        print *, 'type: ',type
        print *, 'overall_action: ',overall_action
        print *, 'time_action: ',time_action
        print *, 'lat/lon_action: ',lat_action,'/',lon_action
        print *, 'pres_action: ',pres_action
        print *, 'temp_action: ',temp_action
        print *, 'moist_action: ',moist_action
        print *, 'wdir_action: ',wdir_action
        print *, 'wspd_action: ',wspd_action
        print *, 'bl_action: ',bl_action
        print *, 'c_qc: "',NRLQCstg,'"'

      elseif(action.eq.'RT'.or.action.eq.'RM'.or.action.eq.'RW') then  
        NCEPqm = 13 ! QM -> bad (RC already set)

      elseif(action.eq.'SM'.or.action.eq.'SW') then
        NCEPqm = 3  ! QM -> suspect (RC already set)

      else
        cp = 99 ! this is just a dummy statement
                !  leave QM as is (IO,GR)
      endif

c -------------------------------------------------------------
c Set QM, RC info into arrays to be returned to calling routine
c -------------------------------------------------------------
      if(type.eq.'t' .and. NCEPrc_t.ne.99999) then
        NCEPrc = NCEPrc_t
      elseif(type.eq.'q' .and. NCEPrc_q.ne.99999) then
        NCEPrc = NCEPrc_q
      elseif(type.eq.'w' .and. NCEPrc_w.ne.99999) then
        NCEPrc = NCEPrc_w
      endif

      return

  999 continue
 
      write(*,*) '*** Warning! RN is out of range 1-34, here = ',RN
      call w3tage('PREPOBS_PREPACQC')
      call errexit(69)

      end
