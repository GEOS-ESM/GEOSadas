!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_RadBufr -- Utilities for extracting raob data from buffer files
!
! !DESCRIPTION:
!
! !INTERFACE:
!
      module m_RadBufr
      use m_RadData, only  : radcor_profiles
      use m_convert, only  : Air_Temperature, Specific_Humidity, VT_NCEP
      use m_humidity, only : wEqtn_NCEP
      implicit    NONE
      private   ! except

      public ::
     .   Get_Raobs,          ! Get raob data
     .   Put_Raobs,          ! Put raob data to buffer file
     .   Clean_Raobs,        ! Clean up data structures
     .   Obs_Reorder,        ! Reorder radcor and prep data structures
     .   Bufr_Inq,           ! Inquire about buffer parameters
     .   Set_Options,        ! Set options for reading/writing
     .   prep_info,          ! Prep buffer information
     .   prep_vector,        ! Data structure within prep_info
     .   prep_options,       ! Data structure for options for read/write
     .   radcor_profiles,    ! Radcor data structure for all profiles
     .   Set_Reason          ! Set reason code to be used for writing BUFR
      interface Get_Raobs
         module procedure
     .      Get_Raobs_,
     .      Get_Raobs_Tag,
     .      Get_Raobs_DT
      end interface
      interface Put_Raobs
         module procedure
     .      Put_Raobs_,
     .      Put_Raobs_Tag,
     .      Put_Raobs_DT
      end interface
      interface Clean_Raobs
      module procedure
     .      Prep_Clean
      end interface
      interface Bufr_Inq
         module procedure
     .      Bufr_Inq_,
     .      Bufr_Inq_byFile
      end interface
      interface Set_Options
         module procedure
     .      Init_Options,
     .      Copy_Options
      end interface
!
! !REVISION HISTORY:
!     01Dec2003  C. Redder  Original code
!     03Feb2004  C. Redder  Renamed the components, Lev, LevM and LevE to
!                           P, P_m and P_e in the data structures,
!                           pbufr_mass and pbufr_wind.
!     11Feb2004  C. Redder  Added the tables RType2kx and RType2kx_file.
!     20Feb2004  C. Redder  Added the components, P_QM and ObQM to the
!                           data structures, prep_vect, pbufr_mass and
!                           pbufr_wind.
!     11Mar2004  C. Redder  Added the component ProcN to the data
!                           structure, pbufr_meta
!     20Apr2004  C. Redder  Added the generic interface, Set_Options.
!                           and the data structure, prep_options.
!                           Added prep_options to the internal data
!                           structures, pbufr_mass and pbufr_wind.
!     16Jun2004  C. Redder  Added the component, write_drift, to the
!                           type, prep_options.
!     02Sep2004  C. Redder  Added the component, QC_Local, to the
!                           types, prep_vector, pbufr_mass and
!                           pbufr_wind.
!     02Nov2004  C. Redder  Added the component, kt_humidity to the
!                           type pbufr_mass
!     04Dec2006  C. Redder  Changed value of ReasonCode from 1 to 100
!     21Jul2007  C. Redder  Removed the components TCor and ZCor from
!                           the data structure of type, pbufr_mass
!                           Add the component Cat to the data structure
!                           of type prep_vector.
!     09Aug2007  C. Redder  Added the native real parameter RT0 (which
!                           is set to T0) to correct a complition error
!                           for the parameter array MassM.
!     23Jan2014  Meta       Add 'Set_Reason' to allow different programs
!                           (hradcor and raobcore) to use different
!                           progam codes
!     16May2017  Meta       Adjust maximum BUFR record size (call MAXOUT)
!                           to avoid losing sounding records that exceed
!                           max record size when raobcor changes are added
!     28Dec2022  Meta       Add option to use environment variable to increase
!                           the maximum BUFR record size
!
!EOP
!-----------------------------------------------------------------

!     Constants for ...
!     -----------------
      integer, parameter ::
     .   IMiss        = -10000,    ! Missing integer attribute
     .   LenID        =  8,        ! String lengh of station ID
     .   BKind        =  8,        ! Size of real
c     .   ReasonCode   =  1,        ! Reason code for all corrected obs
     .   DeflReasonCode   =  100,      ! Reason code for all corrected obs
     .   NLev_Max     =  255,      ! Max no. of levels in a buffer sounding
     .   Nks_Max      =  1000,     !        ... soundings (in first file scan)
     .   NPrepEvn_Max =  10        !        ... events in prep buffer file.

      integer, parameter ::
     .   VT_Eqtn      = VT_NCEP,   ! Set formula for virtual temperature
     .   SVP_Eqtn     = wEqtn_NCEP ! ... satuation vapor pressure (Teton's)
                                   !   to that implemented at NCEP
      real (kind = BKind), parameter ::
     .   BMiss        =  10.0e10,  ! Missing buffer data
     .   Tol          =  1.0e-5    ! Tolerance for comparisons of floats
      real (kind = BKind), parameter ::
     .   TolBMiss     =          BMiss * Tol,
     .   BMiss_down   =  BMiss - BMiss * Tol,
     .   BMiss_up     =  BMiss + BMiss * Tol
      real (kind = BKind), parameter ::
     .   T0           =  273.15,   ! Temperature in deg K at 0 deg C
     .   QM_max       =  3.0 + Tol
      real,                parameter ::
     .   RT0          =  T0        ! T0 in native real
      real (kind = BKind), parameter ::
     .   TD_min       =  T0  - 103.15, ! Minimum and ...
     .   TD_max       =  T0  +  46.83, ! ... maximum dew pt allowed at NCEP
     .   P_max        =  1100.0,       ! Minimum and ...
     .   P_min        =  0.1           ! ... maximum pres   allowed at NCEP
      character (len=*), parameter ::
     .   Prep_Type_   = 'Prep'    ! File type for use in radcor data stucture

      character (len=*), parameter :: MyModule = 'm_RadBufr'

!     Parameters used to read from / write to the buffer file to process ...
!     ----------------------------------------------------------------------

!     ...the mass data
!     ----------------
      integer, parameter ::
     .   Cat_Sfc      = 0,
     .   MassReport   = 1,
     .   MassNVar     = 3,
     .!   MassNVar     = 4,
     .   MassNObMne   = 4,
     .   MassNFCMne   = 1,
     .   MassNPMne    = 4,
     .   MassNDrMne   = 3,
     .   iVarHeight   = 1,
     .   iVarTemp     = 2,
     .   iVarSH       = 3,
     .   iVarHum      = 3,
     .!   iVarDewPt    = 4,
     .   iMassMneOb   = 1,
     .   iMassMneFC   = 1,
     .   iMassMneQM   = 2,
     .   iMassMnePC   = 3,
     .   iMassMneRC   = 4,
     .   kt_Height    = 6,
     .   kt_AirTemp   = Air_Temperature,
     .!   kt_DewPoint  = 9,
     .   kt_SpecHum   = Specific_Humidity,
     .   iMassMneDT   = 1,
     .   iMassMneDLat = 2,
     .   iMassMneDLon = 3
      integer, parameter ::
     .   Masskt_List ( MassNVar ) = (/ kt_Height,    ! in m
     .                                 kt_AirTemp,   ! .. deg K
     .                                 kt_SpecHum /) ! .. g / kg
!     .   Masskt_List ( MassNVar ) = (/ kt_Height,
!     .                                 kt_AirTemp,
!     .                                 kt_SpecHum,
!     .                                 kt_DewPoint /)
      real,    parameter ::
     .   MassM       ( MassNVar ) = (/ 1.0, 1.0,  1.0e-3 /),
     .   MassB       ( MassNVar ) = (/ 0.0, RT0,  0.0    /)
     .!   MassM       ( MassNVar ) = (/ 1.0, 1.0,  1.0e-3, 1.0 /),
     .!   MassB       ( MassNVar ) = (/ 0.0, RT0,  0.0,    RT0 /)
      real,    parameter ::
     .   HumMin = 0.01 * MassM ( iVarSH )
      character (len=*), parameter ::
     .   MassPStr                 =    'POB PQM PPC PRC',
     .   MassObStr   ( MassNVar ) = (/ 'ZOB ZQM ZPC ZRC',
     .                                 'TOB TQM TPC TRC',
     .                                 'QOB QQM QPC QRC' /),
     .   MassFCStr   ( MassNVar ) = (/ 'ZFC',
     .                                 'TFC',
     .                                 'QFC' /),
     .!   MassObStr   ( MassNVar ) = (/ 'ZOB ZQM ZPC ZRC',
     .!                                 'TOB TQM TPC TRC',
     .!                                 'QOB QQM QPC QRC',
     .!                                 'TDO QQM QPC QRC' /),
     .   MassDrStr                =   'HRDR YDR XDR'

!     ... the wind data
!     -----------------
      integer, parameter ::
     .   WindReport   = 2,
     .   WindNVar     = 2,
     .    WindNObMne  = 5,
     .    WindNFCMne  = 2,
     .    WindNPMne   =  MassNPMne,
     .    WindNDrMne  =  MassNDrMne,
     .   iWindMnePOb  = 1,
     .   iWindMneUOb  = 1,
     .   iWindMneVOb  = 2,
     .   iWindMneUFC  = 1,
     .   iWindMneVFC  = 2,
     .   iWindMneQM   = 3,
     .   iWindMnePC   = 4,
     .   iWindMneRC   = 5,
     .   iWindMnePQM  = 2,
     .   iWindMnePPC  = 3,
     .   iWindMnePRC  = 4,
     .   kt_UWind     = 4,
     .   kt_VWind     = 5,
     .   iWindMneDT   = iMassMneDT,
     .   iWindMneDLat = iMassMneDLat,
     .   iWindMneDLon = iMassMneDLon
      integer, parameter ::
     .   Windkt_List ( WindNVar ) = (/ kt_UWind, kt_VWind /)
      character (len=*), parameter ::
     .   WindPStr     =  MassPStr,
     .   WindObStr    = 'UOB VOB WQM WPC WRC',
     .   WindFCStr    = 'UFC VFC',
     .   WindDrStr    =  MassDrStr

!     Array for mapping buffer report type to GMAO data source index
!     --------------------------------------------------------------
      integer, parameter ::
     .   RType_Min   = 101,
     .   RType_Max   = 299,
     .   RType_Size  = 299 - 101 + 1
      integer, parameter, dimension ( RType_Size ) ::
     .   kx_Def      = 299,
     .   kx_file_Def = 199
      integer, parameter, dimension ( RType_Min : RType_Max ) ::
     .   RType2kx       = (/
     .      kx_Def      ( : 19 ),   7,
     .      kx_Def      ( :  9 ),  16,  14,  10,  89,
     .      kx_Def      ( :  7 ), 114, 270, 268, 146, 119, 121, 120,
     .      kx_Def      ( :  2 ), 116,
     .      kx_Def      ( :  1 ), 116,
     .      kx_Def      ( : 27 ),   3,   2,   3,   4,
     .      kx_Def      ( :  3 ),   2,
     .      kx_Def      ( : 22 ), 298,
     .      kx_Def      ( :  9 ),   7,   8,
     .      kx_Def      ( :  1 ), 285, 284,
     .      kx_Def      ( :  5 ),  16,  14,  10,  89,
     .      kx_Def      ( :  7 ), 114, 270, 268,
     .      kx_Def      ( :  1 ), 119, 121, 120,
     .      kx_Def      ( :  2 ), 269, 146,  27,  26, 267,  24, 114,
     .      kx_Def      ( : 23 ),   3,   2,   6, 149,   4, 154, 152,
     .                              2,
     .      kx_Def      ( : 12 ) /)

!     ... and to NCEP data source index
!     ---------------------------------
      integer, parameter, dimension ( RType_Min : RType_Max ) ::
     .   RType2kx_file  = (/
     .      kx_file_Def ( :  1 ),   2,
     .      kx_file_Def ( :  8 ),  11,
     .      kx_file_Def ( :  8 ),  20,
     .      kx_file_Def ( :  1 ),  22,
     .      kx_file_Def ( :  7 ),  30,  31,  32,  33,
     .      kx_file_Def ( :  7 ),  41, 142, 143,  44,  45,  46,  47,
     .                             48,  49, 150, 151, 152,
     .      kx_file_Def ( :  3 ), 156, 157, 158, 159,
     .      kx_file_Def ( :  4 ),  64,  65,
     .      kx_file_Def ( :  8 ),  74,  75,
     .      kx_file_Def ( :  4 ),  80,  81, 182, 183,
     .      kx_file_Def ( :  3 ),  87,
     .      kx_file_Def ( :  2 ),  90,  91,
     .      kx_file_Def ( : 18 ),  10,
     .      kx_file_Def ( :  9 ),  20,  21,  22,  23,  24,  25,
     .      kx_file_Def ( :  4 ),  30,  31,  32,  33,
     .      kx_file_Def ( :  7 ),  41,  42,  43,
     .      kx_file_Def ( :  1 ),  45,  46,  47,  48,  49,  50,  51,
     .                             52,  53,  54,  55,  56,
     .      kx_file_Def ( : 23 ),  80,  81,  82,  83,  84,  85,  86,
     .                             87,
     .      kx_file_Def ( : 12 ) /)

!     List of data source indices (kx) as defined by NCEP
!     ---------------------------------------------------
      integer, parameter ::
     .   nkx_file      = 183,
     .   kx_len        =  50,
     .   kx_RaobListSz =   3
      integer, dimension ( kx_RaobListSz ),             parameter ::
     .   kx_RaobList   = (/ 20, 22, 32 /)
      character   (len = kx_len), dimension (nkx_file), parameter ::
     .   kx_blanks     = ' '
      character   (len = kx_len), dimension (nkx_file), parameter ::
     .   kx_names_file = (/
     .   kx_blanks (   1 :   1 ),
     .  'SSM/I brightness temperatures                     ',  ! 2
     .   kx_blanks (   3 :   9 ),
     .  'Synthetic tropical cyclone winds                  ',  ! 10
     .  'SYNTHETIC TROPICAL CYCLONE STORM CENTER           ',  ! 11
     .   kx_blanks (  12 :  19 ),
     .  'Rawinsonde                                        ',  ! 20
     .  'Pibal wind                                        ',  ! 21
     .  'CLASS sounding                                    ',  ! 22
     .  'Wind profiler                                     ',  ! 23
     .  'NEXRAD VAD wind                                   ',  ! 24
     .  'NEXRAD radial wind                                ',  ! 25
     .   kx_blanks (  26 :  29 ),
     .  'AIREP/PIREP                                       ',  ! 30
     .  'ASDAR                                             ',  ! 31
     .  'Recon/Dropwisonde                                 ',  ! 32
     .  'MDCRS                                             ',  ! 33
     .   kx_blanks (  34 :  40 ),
     .  'INDIAN IR/VIS CTW                                 ',  ! 41
     .  'JMA IR/VIS CTW below 850 mb                       ',  ! 42
     .  'EUMETSAT IR/VIS CTW below 850 mb                  ',  ! 43
     .  'NESDIS VIS                                        ',  ! 44
     .  'NESDIS IR                                         ',  ! 45
     .  'NESDIS imager cloud top                           ',  ! 46
     .  'NESDIS imager deep layer                          ',  ! 47
     .  'NESDIS sounder cloud top                          ',  ! 48
     .  'NESDIS sounder deep layer                         ',  ! 49
     .  'JMA WV CTW                                        ',  ! 50
     .  'NESDIS Vis CTW                                    ',  ! 51
     .  'JMA IR/vis CTW above 850                          ',  ! 52
     .  'EUMETSAT IR/vis CTW above 850                     ',  ! 53
     .  'EUMETSAT WV wind                                  ',  ! 54
     .  'NESDIS picture triplet                            ',  ! 55
     .  'Indian WV wind                                    ',  ! 56
     .   kx_blanks (  57 :  63 ),
     .  'GOES land radiances - clear                       ',  ! 64
     .  'GOES land radiances - cloudcorr                   ',  ! 65
     .   kx_blanks (  66 :  73 ),
     .  'GOES ocean radiances - clear                      ',  ! 74
     .  'GOES ocean radiances - cloudcorr                  ',  ! 75
     .   kx_blanks (  76 :  79 ),
     .  'Surface marine w/station pressure                 ',  ! 80
     .  'Surface land/METAR w/station pressure             ',  ! 81
     .  'Atlas buoy wind                                   ',  ! 82
     .  'SSM/I neural net superobed wind speed             ',  ! 83
     .  'Surface marine/land METAR wind w/o sfcP           ',  ! 84
     .  'Quikscat scatterometer wind                       ',  ! 85
     .  'ERS scatterometer wind                            ',  ! 86
     .  'Sfc METAR w/altimeter setting                     ',  ! 87
     .   kx_blanks (  88 :  89 ),
     .  'OPC/NOS POINT MEAN SEA-LEVEL PRESSURE BOGUS       ',  ! 90
     .  'AUSTRALIAN PAOB MEAN SEA-LEVEL PRESSURE BOGUS     ',  ! 91
     .   kx_blanks (  92 : 141 ),
     .  'JMA IR/VIS cld top temperature                    ',  ! 142
     .  'EUMETSAT IR/VIS cld top temperature               ',  ! 143
     .   kx_blanks ( 144 : 149 ),
     .  'SSM/I SUPEROBED FNOC RAIN RATE                    ',  ! 150
     .  'NESDIS SFOV CLOUD TOP PRESSURE AND TEMPERATURE    ',  ! 151
     .  'SSM/I SUPEROBED NEURAL NET 3 TPW                  ',  ! 152
     .   kx_blanks ( 153 : 155 ),
     .  'NESDIS Precip water land clear                    ',  ! 156
     .  'NESDIS Precip water land cloudcorr                ',  ! 157
     .  'NESDIS Precip water ocean clear                   ',  ! 158
     .  'NESDIS Precip water ocean cloudcorr               ',  ! 159
     .   kx_blanks ( 160 : 181 ),
     .  'Splash level (sfc) dropwinsonde                   ',  ! 182
     .  'Sfc land w/o station pressure                     '/) ! 183


!     List of NCEP's quality marks
!     ----------------------------
      integer, parameter ::
     .   OK_ListSz      = 3,
     .   Suspect_ListSz = 5,
     .   OIQC_ListSz    = 4
      integer, parameter ::
     .   OK_List      ( OK_ListSz )      = (/ 0, 1, 2 /),
     .   Suspect_List ( Suspect_ListSz ) = (/ 3, 4, 5, 6, 7 /),
     .   OIQC_List    ( OIQC_ListSz    ) = (/    4, 5, 6, 7 /),
     .   QM_Unknown     = 16,
     .   QM_Missing     = 17
      integer, parameter ::
     .   nqcx_file      = 17,
     .   qcx_len        = 32
      character (len = qcx_len), dimension (nqcx_file), parameter ::
     .   qcx_names_file = (/
     .  'not used                        ',   !  1
     .  'not used                        ',   !  2
     .  'not used                        ',   !  3
     .  'OIQC - prev QM was 0            ',   !  4
     .  'OIQC - prev QM was 1            ',   !  5
     .  'OIQC - prev QM was 2            ',   !  6
     .  'OIQC - prev QM was 3            ',   !  7
     .  'PREVENT                         ',   !  8
     .  'PREVENT                         ',   !  9
     .  'ACQC superob; PROFL failed chks ',   ! 10
     .  'PROFL no pass median/shear chk  ',   ! 11
     .  'Reject /  PROFL failed shear chk',   ! 12
     .  'Fail Auto QC; PROFL faild median',   ! 13
     .  'SDM purge flag                  ',   ! 14
     .  'PREPRO flag for non-use by analy',   ! 15
     .  'Unknown NCEP QM                 ',   ! 16
     .  'Missing NCEP QM                 ' /) ! 17

!     List of valid program names (consistent with prep buffer)
!     ---------------------------------------------------------
      integer, parameter ::
     .   NProgNames   = 10,
     .   ProgCode_Max = 13
      character (len=*), parameter ::
     .   ProgNames ( NProgNames ) = (/
     .      'PREPRO  ',
     .      'SYNDATA ',
     .      'CLIMO   ',
     .      'PREVENT ',
     .      'CQCHT   ',
     .      'RADCOR  ',
     .      'PREPACQC',
     .      'VIRTMP  ',
     .      'OIQC    ',
     .      'SSI     ' /)

!     Status codes
!     ------------
      integer, parameter ::
     .   No_Error        =   0, ! Valid error status; no error
     .   No_LUAvail      =  -1, ! No FORTRAN logical unit number
     .   Inquire_Error   =  -2, ! Error in inquiring about a file
     .   Open_Error      =  -3, ! Error in opening a file
     .   Read_Error      =  -4, ! Error in reading a file
     .   Alloc_Error     =  -5, ! Allocation error
     .   Close_Error     =  -6, ! Error in closing a file
     .   Conv_Error      =  -7, ! Conversion error
     .   MPI_Error       =  -8, ! Error from message passing interface routine
     .   Bad_BType       =  -9, ! Invalid buffer type
     .   Dealloc_Error   = -10, ! Deallocation error
     .   Varying_SDate   = -11, ! Varying synoptic date in bufr data
     .   Index_Error     = -12, ! Array/Char index error
     .   Bad_ProgName    = -13  ! Bad program name

!     Data structure for  option flags for reading/writing Prep buffer files
!     ----------------------------------------------------------------------
      type prep_options
         character (len=255) ::
     .      LastProgName,     ! Name of last program generating input file
     .      ThisProgName      ! Name of current driver program
         logical ::
     .      get_latest_QM,    ! = .true. to get QM for latest event regard-
     .                        !   less of the event specified in LastProgName
     .      reverse_RadCor,   ! = .true. to remove the bias corrections
     .                        !   applied by NCEP's RadCor scheme.
     .      write_drift,      ! = .true. to write drift infomation into
     .                        !   to output buffer file
     .      reject_OIQC       ! = .true. to reject observations that failed
                              !   the OIQC check
      end type

!     Private, internal data structures for the file parameters
!     ---------------------------------------------------------
      type pbufr_file
         integer ::
     .      LastPCode,  ! Program code for last program
     .      ThisPCode,  ! ... this program
     .      CQC_PCode,  ! ... for complex quality control
     .      Rad_PCode,  ! ... for radcor
     .       VT_PCode   ! ... for calculating virtual temperature
         character ( len = len ( MassObStr )),
     .      dimension ( MassNVar, MassNObMne ) ::
     .      WrMne       ! Write mnemonic.  See type prep_vector for details.
      end type

!     ... for each raob report meta information
!     -----------------------------------------
      type pbufr_meta
         character (len=LenID) ::
     .      StnID       ! WMO station ID
         real ::
     .      Lon,        ! Station latitude (degrees, -90 = 90S)
     .      Lat,        ! ... longitude    (degrees, -90 = 90W)
     .      Elev        ! ... elevation (m)
         integer ::
     .      SeqN,       ! Sequence number
     .      ProcN,      ! Processor number
     .      IType,      ! WMO instrument type
     .      RType,      ! NCEP report type
     .      RadCode,    ! WMO radiation correction code (missing = IMiss)
     .      ObTime,     ! Observation time (which may have been corrected)
     .      RObTime,    ! Uncorrected (reported) observation time
     .      TCor        ! Ob time correction code (mnemonic TCOR)
      end type

!     ... for information concerning NCEP radcor
!     ------------------------------------------
      type NCEP_radcor_BInfo
         real ( kind = BKind ), dimension ( NLev_Max ) ::
     .      ZCor_RADCOR,! Height correction by RADCOR
     .      TCor_preVT, ! Temperature correction prior to VIRTMP program
     .      TCor_RADCOR,! ... by RADCOR
     .      HCor_VIRTMP,! Humidity correction by the VIRTMP program
     .      HCor_after  ! ... after the VIRTMP program
      end type
      type NCEP_radcor_Info
         real ( kind = BKind ), dimension ( NLev_Max ) ::
     .      ZCor_RADCOR,! Height correction by RADCOR
     .      TCor_preVT, ! Temperature correction prior to VIRTMP program
     .      TCor_RADCOR,! ... by RADCOR
     .      HCor_VIRTMP,! Humidity correction by the VIRTMP program
     .      HCor_after  ! ... after the VIRTMP program
      end type

!     ... for the mass portion of the report
!     --------------------------------------
      type pbufr_mass
         type ( pbufr_file   ) ::
     .      File        ! File data
         type ( pbufr_meta   ) ::
     .      Meta        ! Meta data
         type ( prep_options ) ::
     .      Options     ! Flag options for reading/writing
         logical ::
     .      loaded,     ! = .true. if data is loaded into data structure
     .      drift_avail ! = .true. if prep buffer file contains balloon
         integer ::     !   drift data
     .      NLev,       ! Number of levels
     .      P_PC_Max,   ! Maximum value for the program codes for pressure
     .      ObPC_Max,   ! ... and ob values.
     .      kt_humidity ! Current data type for humidity variable
         integer, dimension ( NLev_Max ) ::
     .      P_m,        ! Mantessa of pressure
     .      P_e,        ! Exponent of pressure
     .      Cat,        ! Category
     .      Indx        ! Indices for sorting by pressure
         real,    dimension ( NLev_Max ) ::
     .      DLat,       ! Drift latitude
     .      DLon,       ! ... and logitude (-90 = 90W)
     .      P,          ! Pressure (hPa)
     .      P_QM,       ! ... and its quality mark
     .      P_PC,       ! ... and program code
     .      ETime       ! Elapsed time since synoptic time (s)
         logical, dimension ( NLev_Max ) ::
     .      P_Missing,  ! Logical flags for missing pressure
     .      T_VIRTMP,   ! = .true. if virtual temperature was computed
     .      H_VIRTMP    ! ... or humidity was updated by VIRTMP
         real,    dimension ( MassNVar, NLev_Max ) ::
     .      Obs,        ! Observation values
     .      ObFC,       ! ... its first guess (or forecast)
     .      ObQM,       ! ... its quality marks
     .      ObPC,       ! ... its program code
     .      QM          ! Quality marks for pressure and observation
         integer, dimension ( MassNVar, NLev_Max ) ::
     .      QC,         ! Radcor quality control marks
     .      QC_Local    ! ... for use in this module.
         logical, dimension ( MassNVar, NLev_Max ) ::
     .      ObMissing,  ! Logical flags for missing observation.
     .      FCMissing   ! ... first guess (or forecast)
      end type

!     ... for the wind portion of the report
!     --------------------------------------
      type pbufr_wind
         type ( pbufr_file   ) ::
     .      File        ! File data
         type ( pbufr_meta   ) ::
     .      Meta        ! Meta data
         type ( prep_options ) ::
     .      Options     ! Flag options for reading/writing
         logical ::
     .      loaded,     ! = .true.  if data is loaded into data structure
     .      drift_avail ! = .true.  if prep buffer file contains balloon
         integer ::     !   drift data
     .      NLev,       ! Number of levels
     .      P_PC_Max,   ! Maximum value for the program codes for pressure
     .      ObPC_Max    ! ... and ob values.
         integer, dimension ( NLev_Max ) ::
     .      P_m,        ! Mantessa of pressure
     .      P_e,        ! Exponent of pressure
     .      Cat,        ! Category
     .      Indx        ! Sorting indices
         real,    dimension ( NLev_Max ) ::
     .      DLat,       ! Drift latitude
     .      DLon,       ! ... and logitude (-90 = 90W)
     .      P,          ! Pressure (hPa)
     .      P_QM,       ! ... for pressure only
     .      P_PC,       ! ... and program code
     .      ETime       ! Elapsed time since synoptic time (s)
         logical, dimension ( NLev_Max ) ::
     .      P_Missing   ! Logical flags for missing pressure
         real,    dimension ( WindNVar, NLev_Max ) ::
     .      Obs,        ! Observation values
     .      ObFC,       ! ... its first guess
     .      ObQM,       ! ... its quality mark
     .      ObPC,       ! ... its program code
     .      QM          ! Quality marks for observation and pressure
         logical, dimension ( WindNVar, NLev_Max ) ::
     .      ObMissing,  ! Logical flags for missing observation
     .      FCMissing   ! ... for first guess
         integer, dimension ( WindNVar, NLev_Max ) ::
     .      QC,         ! Radcor quality control marks
     .      QC_Local    ! ... for use in this module
      end type

!     Data structure for information about the prep data file
!     -------------------------------------------------------
      type prep_vector
         integer,  dimension (:), pointer ::
     .      RepType     ! Report type.
         real,     dimension (:), pointer ::
     .      P_QM,       ! Quality markers for pressure
     .      ObQM,       ! ... and observations
     .      P_PC,       ! Program codes for pressure
     .      ObPC        ! ... and observations
         integer,  dimension (:), pointer ::
     .      QC_Local,   ! QC for use in this module.
     .      Cat         ! Category
         logical,  dimension (:), pointer ::
     .      T_VIRTMP,   ! = .true. if virtual temperature was computed
     .      H_VIRTMP    ! ... or humidity was updated by VIRTMP
      end type

      type prep_info
         logical ::
     .      drift_avail ! = .true. if prep buffer file contains balloon drift
         integer ::     !   data.
     .      LastPCode,  ! Buffer code for last program
     .      CQC_PCode,  ! ... for complex quality control
     .      Rad_PCode,  ! ... for radcor
     .      VT_PCode,   ! ... for calculating virtual temperature
     .      P_PC_Max,   ! Maximum value for the program codes for pressure
     .      ObPC_Max    ! ... and ob values.
         character ( len = len ( MassObStr )),
     .      dimension ( MassNVar, MassNObMne ) ::
     .      WrMne       ! Mnemonics for writing data.  Used to prevent
                        !   writing ob data twice.  Set in routine, Prep_Init.
         type ( prep_vector ) ::
     .      Vector      ! Mass data from buffer file not in radcor data
      end type          !    structure

      integer :: ReasonCode =  DeflReasonCode

      contains

!.................................................................

!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Set_Reason () --- set 'reason code' for writing prepbufr
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Set_Reason( RCcode )
!
! !INPUT PARAMETERS:
      implicit   NONE
      integer, intent(in) ::   RCcode    ! reason code to use
! !SEE ALSO:
!
! !REVISION HISTORY:
!     23Jan2014  Meta     Original code
! EOP
!-------------------------------------------------------------------------
      ReasonCode = RCcode
      return
      end subroutine Set_Reason

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Test_RMiss () --- Test for missing values in native reals
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Test_RMiss ( X, Missing )
!
! !INPUT PARAMETERS:
      implicit   NONE
      real,          intent(in)  ::
     .   X       (:) ! Values to be tested
!
! !OUTPUT PARAMETERS:
      logical,       intent(out) ::
     .   Missing (:) ! = .true. if corresponding value is missing.
!                    ! = .false. otherwise
! !SEE ALSO:
!
! !REVISION HISTORY:
!     21Apr2004  C. Redder   Original code
! EOP
!-------------------------------------------------------------------------
      real, parameter ::
     .   RMiss_down = BMiss_down,
     .   RMiss_up   = BMiss_up
      real    :: XX
      integer :: iVal, NVal

      NVal = min ( size ( X ), size ( Missing ))
      do iVal = 1, NVal
         XX               = X ( iVal )
         Missing ( iVal ) = XX .gt. RMiss_down .and.
     .                      XX .lt. RMiss_up
      end do

      return
      end subroutine Test_RMiss

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Test_BMiss () --- Test for missing values in buffer reals
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Test_BMiss ( X, Missing )
!
! !INPUT PARAMETERS:
      implicit   NONE
      real ( kind = BKind ), intent(in)  ::
     .   X       (:)       ! Values to be tested
!
! !OUTPUT PARAMETERS:
      logical,               intent(out) ::
     .   Missing (:)       ! = .true. if corresponding value is missing.
!                          ! = .false. otherwise
! !SEE ALSO:
!
! !REVISION HISTORY:
!     21Apr2004  C. Redder   Original code
! EOP
!-------------------------------------------------------------------------
      real ( kind = BKind ) :: XX
      integer :: iVal, NVal

      NVal = min ( size ( X ), size ( Missing ))
      do iVal = 1, NVal
         XX               = X ( iVal )
         Missing ( iVal ) = XX .gt. BMiss_down .and.
     .                      XX .lt. BMiss_up
      end do

      return
      end subroutine Test_BMiss

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Bufr_Inq_ () --- Inquire about buffer file parameters
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Bufr_Inq_ ( Prep_Type )
      use m_TextUtil, only : LowerCase
!
! !INPUT PARAMETERS:
      implicit   NONE
!
! !OUTPUT PARAMETERS:
      character (len=*), optional, intent (out) ::
     .   Prep_Type        ! File type string for use in radcor data structure
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     03Jul2003  C. Redder   Original code
! EOP
!-------------------------------------------------------------------------

      if ( present ( Prep_Type )) Prep_Type = LowerCase ( Prep_Type_ )

      return
      end subroutine Bufr_Inq_

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Bufr_Inq_byFile () --- Inquire about buffer file parameters
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Bufr_Inq_byFile ( File, stat, PName,
     .                             PCode,
     .                             CQC_PCode,
     .                             Rad_PCode,
     .                             VT_PCode,
     .                             SynTag,
     .                             adpupa_exist )
      use m_AdvError, only : WPErr, PErr, ItoA
      use m_SysIO,    only : LUAvail
!
! !INPUT PARAMETERS:
      implicit   NONE
      character (len=*),    intent (in)  ::
     .   File             ! Name of buffer file
      character (len=*),    intent (in), optional  ::
     .   PName            ! Progam name.  Default: PName = ' '
!
! !OUTPUT PARAMETERS:
      integer,              intent (out) ::
     .   stat             ! Returned status code.
      integer,              intent (out), optional ::
     .   PCode,           ! Program code (= 0 if PName = ' ')
     .   CQC_PCode,       ! ... for complex quality control,
     .   Rad_PCode,       ! ... for radcor
     .   VT_PCode,        ! ... for virtual temperature calculations
     .   SynTag           ! Synoptic date and time tag in YYYYMMDDHH format
      logical,              intent (out), optional ::
     .   adpupa_exist     ! = .true. if adpupa data exists
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     03Jul2003  C. Redder   Original code
!     31Jan2004  C. Redder   Added error handling for read error.
!     03Mar2004  C. Redder   Add optional input argument, PName.
!                            Changed meaning of output argument, PCode.
!     26Apr2004  C. Redder   Added the optional, argument CQC_PCode,
!                            Rad_PCode, and VT_PCode.
!     14Jun2007  C. Redder   Added the optional output argument,
!                            adpupa_exist.  Removed error handling
!                            for the case when adpupa data does not
!                            exist (i.e. iret_mg .ne. 0)
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .           MyName = MyModule // '::Bufr_Inq_byFile'
      character ( len = 8 ) :: SubSet
      real    :: RR, CQCRR, RadRR, VTRR
      integer :: lu, iret_mg, STag, SynopticTag, ProgIndx, ProgCode
      real, parameter ::
     .   IMin = -huge ( 1 ),
     .   IMax =  huge ( 1 )

!     Determine an available logical unit
!     -----------------------------------
      lu = LUAvail()
      if ( lu .lt. 0 ) then
         stat = No_LUAvail
         call WPErr ( MyName, 'No logical units available for the '
     .                     // 'file, ' // trim ( File ) )
         return
      end if

!     Determine program index
!     -----------------------
      ProgIndx = 0
      if ( present ( PName )) then
         if ( PName .ne. ' ' ) then
            ProgIndx = ProgIndex ( PName )
            if ( ProgIndx .eq. 0 .and. present ( PCode )) then
               stat = Bad_ProgName
               call WPErr ( MyName, 'Invalid progame name, '
     .                            // trim ( PName ))
               return
            end if
         end if
      end if

!     Open buffer file
!     ----------------
      open ( unit   =  lu,
     .       file   =  File,
     .       status = 'old',
     .       form   = 'unformatted',
     .       iostat =  stat )
      if ( stat .ne. 0 ) then
         call PErr ( MyName, ' Open error (iostat = '
     .                    //   trim ( ItoA ( stat ) ) // ') \n'
     .                    // ' File = ' // trim ( File ) )
         stat = Open_Error
         return

      end if
      call OpenBF ( lu, 'IN', lu )

      call DateLen ( 10 ) ! Set up for Y2K compliant dates

!     Search for the first MG block of raob (ADPUPA) reports
!     ------------------------------------------------------
      iret_mg     =  0
      SubSet      = ' '
      SynopticTag = 1900010100
      do while ( iret_mg    .eq.  0      .and.
     .              SubSet  .ne. 'ADPUPA' )
         call ReadMG ( lu, SubSet, STag, iret_mg )
         if ( iret_mg .eq. 0 ) SynopticTag = STag
      end do

!     Extract some file parameters including the ...
!     ----------------------------------------------
      if ( iret_mg .eq. 0 .and. ProgIndx .ne. 0 ) then
         call UFBQCD ( lu, trim ( ProgNames ( ProgIndx )), RR )
         ProgCode = nint ( max ( min ( IMax, RR ), IMin ))
      else
         ProgCode = 0
      end if
      if ( iret_mg .eq. 0 ) then
         call UFBQCD ( lu, 'CQCHT',  CQCRR )
         call UFBQCD ( lu, 'RADCOR', RadRR )
         call UFBQCD ( lu, 'VIRTMP', VTRR )
      else
         CQCRR = 0
         RadRR = 0
         VTRR  = 0
      end if
      call ClosBF ( lu )

      if ( present ( PCode        )) PCode        = ProgCode
      if ( present ( CQC_PCode    )) CQC_PCode    = nint ( CQCRR )
      if ( present ( Rad_PCode    )) Rad_PCode    = nint ( RadRR )
      if ( present ( VT_PCode     ))  VT_PCode    = nint ( VTRR  )
      if ( present ( SynTag       )) SynTag       = SynopticTag
      if ( present ( adpupa_exist )) adpupa_exist = iret_mg .eq. 0


!     All is well
!     -----------
      stat = 0

      return
      end subroutine Bufr_Inq_byFile

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  ProgIndex - Return program index number for the given name
!
! !INTERFACE:
      function ProgIndex ( Name )
!
! !USES:
      implicit NONE
!
! !INPUT PARAMETERS:
      character (len=*), intent (in)  ::
     .   Name      ! Program name
!
! !OUTPUT PARAMETERS:
      integer                         ::
     .   ProgIndex ! Prog index
!
! !DESCRIPTION:
!     This function returns the program index for the given name or
!     zero if the name is not valid.
!
! !REVISION HISTORY:
!     04Mar2004  C. Redder  Initial code
!EOP
!.................................................................

      integer :: iList, ListSz

      ProgIndex = 0
      ListSz    = size ( ProgNames )
      do iList = 1, ListSz
         if ( Name .eq. ProgNames ( iList )) then
            ProgIndex = iList
            exit
         end if
      end do
      return
      end function ProgIndex
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Get_Raobs_DT() --- Get raob data from buffer file for given date and time
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Get_Raobs_DT ( File, NYMD, NHMS, Prep, Obs, stat,
     .                          Options )
      use m_RadData,  only : radcor_profiles
      use m_AdvError, only : WPErr, ErrStat
      use m_SunAlt,   only : Julian, CalDate
!
! !INPUT PARAMETERS:
      implicit   NONE
      character (len=*),        intent (in)    ::
     .   File                 ! Input buffer file
      integer,                  intent (in)    ::
     .   NYMD,                ! Synoptic date and time for the desired
     .   NHMS                 !   data, in YYYYMMDD and HHMMSS format.
                              !  (Time rounded to nearest hour.)  Set
                              !   date to zero or less to retrieve all
                              !   data in buffer file. (But non-zero
                              !   error status is returned if date and
                              !   time tags vary within the file.)
      type ( prep_options ),    intent (in), optional ::
     .   Options              ! Flag options for reading/writing
!
! !OUTPUT PARAMETERS:
      type ( prep_info ),       intent (inout) ::
     .   Prep                 ! Information about the prep buffer file
                              !   which is used to write the data to
                              !   the buffer file
      type ( radcor_profiles ), intent (inout) ::
     .   Obs                  ! Obs vector for both raobs and non-raobs
      integer,                  intent (out)   ::
     .   stat                 ! Returned status code
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     16Apr2003  C. Redder   Origional code
!     09Jul2003  C. Redder   Fixed bug regarding the calculation of NYMDH
!     12Nov2003  C. Redder   Replaced the data sturcture, radcor, with
!                            radcor_profiles.
!     04Mar2004  C. Redder   Added optional arguments, PName and
!                            updated_QM.
!     01Apr2004  C. Redder   Removed the optional arguments, PName and
!                            updated_QM and added the optional argument,
!                            Options.
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .           MyName = MyModule // '::Get_Raobs_DT'
      integer :: NYMDH, HH, MMSS, JHr

      if ( NYMD .le. 0 ) then
         NYMDH = 0
      else
         HH    =       NHMS / 10000
         MMSS  = mod ( NHMS,  10000 )
         if ( MMSS .ge. 3000 ) HH = HH + 1
c         JHr   = ( Julian ( NYMD ) - 1 ) * 24 + HH
         JHr   = Julian ( NYMD, HH )
         NYMDH = CalDate ( JHr / 24 + 1 ) * 100 + mod ( JHr, 24 )
      end if

      call Get_Raobs_Tag ( File, NYMDH, Prep, Obs, stat,
     .                     Options    = Options )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Get_Raobs_Tag' ))
         return
      end if

      return
      end subroutine Get_Raobs_DT

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Get_Raobs_() --- Get all raob data from buffer file
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Get_Raobs_ ( File,  Prep, Obs, stat, Options )
      use m_RadData,  only : radcor_profiles
      use m_AdvError, only : WPErr, ErrStat
!
! !INPUT PARAMETERS:
      implicit   NONE
      character (len=*),        intent (in)    ::
     .   File                 ! Input buffer file
      type ( prep_options ),    intent (in), optional ::
     .   Options              ! Options for reading/writing
!
! !OUTPUT PARAMETERS:
      type ( prep_info ),       intent (inout) ::
     .   Prep                 ! Information about the prep buffer file
                              !   which is used to write the data to the
                              !   buffer file
      type ( radcor_profiles ), intent (inout) ::
     .   Obs                  ! Obs vector for both raobs and non-raobs
      integer,                  intent (inout) ::
     .   stat                 ! Returned status code
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     16Apr2003  C. Redder   Origional code
!     12Nov2003  C. Redder   Replaced the data sturcture, radcor, with
!                            radcor_profiles.
!     04Mar2004  C. Redder   Added optional arguments, PName and
!                            updated_QM.
!     01Apr2004  C. Redder   Removed the optional arguments, PName and
!                            updated_QM and added the optional argument,
!                            Options.
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .           MyName = MyModule // '::Get_Raobs_'
      integer, parameter :: NYMDH = 0

      call Get_Raobs_Tag ( File, NYMDH, Prep, Obs, stat,
     .                     Options    = Options )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Get_Raobs_Tag' ))
         return
      end if

      return
      end subroutine Get_Raobs_
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Get_Raobs_Tag() --- Get raob data from buffer file.
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Get_Raobs_Tag ( File,  NYMDH, Prep, Obs, stat,
     .                           Options )
      use m_RadData,  only : radcor_profiles, Rad_IndexSet, Rad_LevSort
      use m_AdvError, only : PErr, WPErr, ErrStat, ItoA
      use m_SysIO,    only : LUAvail
!
! !INPUT PARAMETERS:
      implicit   NONE
      character (len=*),        intent (in)    ::
     .   File                   ! Input buffer file
      integer,                  intent (in)    ::
     .   NYMDH                  ! Synoptic date and hour for the desired
                                !   data, in YYYYMMDDHH format.  Set tag
                                !   to zero or less to retrieve all data
                                !   in buffer file.  (But non-zero error
                                !   status is returned if date and time
                                !   tags vary within the file.)
      type ( prep_options ),    intent (in), optional ::
     .   Options                ! Options for reading/writing
!
! !OUTPUT PARAMETERS:
      type ( prep_info ),       intent (inout) ::
     .   Prep                   ! Information about the prep buffer file
                                !   which is used to write the data to the
                                !   buffer file
      type ( radcor_profiles ), intent (inout) ::
     .   Obs                    ! Obs vector for both raobs and non-raobs
      integer,                  intent (out)   ::
     .   stat                   ! Returned status code
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     16Apr2003  C. Redder   Origional code
!     09Jul2003  C. Redder   Fixed bug by initializing SynBeg.
!     12Nov2003  C. Redder   Replaced the data sturcture, radcor, with
!                            radcor_profiles.
!     03Mar2004  C. Redder   Added code to set the components, ProgCode
!                            and latest_QM in the data structures,
!                            pbufr_mass and pbufr_wind.  Added optional
!                            arguments, PName and updated_QM.  Added
!                            code to handle processor number as well as
!                            the sequence number.
!     01Apr2004  C. Redder   Removed the optional arguments, PName and
!                            updated_QM and added the optional argument,
!                            Options.
!     27Aug2006  C. Redder   Changed the argument, Options, to LocOptions
!                            in call to the routine Bufr_Inq_byFile
!     04Feb2007  C. Redder   Fixed bug in determining the number of
!                            observations used for setting the array
!                            sizes in the data substructure, Vector,
!                            in radcor_profiles.
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .           MyName = MyModule // '::Get_Raobs_Tag'
      integer, parameter :: NTObs_Max = NLev_Max * Nks_Max

      type ( pbufr_meta   ) :: Meta
      type ( pbufr_mass   ) :: Mass
      type ( pbufr_wind   ) :: Wind
      type ( prep_options ) :: LocOptions
      logical :: accept_meta, add_sonde, mass_report, wind_report,
     .           get_all_data, varying_dates, adpupa_processed,
     .           sonde_changed
      integer :: lu,  iScan, iOb,  iSQN1, iSQN2, iPrN1, iPrN2,
     .           Nks, NTObs, NObs, iret_mg, iret_sb,
     .           SynTag, SynTagF, NYMDH_file, NMObs, NWObs
      real    :: Lat, Lon,  Elev, PC
      character ( len = 8 ) :: SubSet
      equivalence ( PC, SubSet )
      character ( len = 255 ) :: Prep_Type

!     Set the data stucture that contain the option switches
!     ------------------------------------------------------
      call    Init_Options ( LocOptions )
      if ( present ( Options ))
     .   call Copy_Options ( Options,    LocOptions )
      call    Copy_Options ( LocOptions, Mass % Options )
      call    Copy_Options ( LocOptions, Wind % Options )

!     Implement option to get all data
!     --------------------------------
      get_all_data = NYMDH .le. 0

!     Determine an available logical unit
!     -----------------------------------
      lu = LUAvail()
      if ( lu .lt. 0 ) then
         stat = No_LUAvail
         call WPErr ( MyName, 'No logical units available for the '
     .                     // 'file, ' // trim ( File ) )
         return
      end if

!     Assume a maximum number of soundings and total number of obs
!     during the first scan and save the observations.  If more space
!     is needed then the required space can be determined in the first
!     scan without saving all observations in the file.  During a
!     second scan, the observations can be read and saved.
!     -----------------------------------------------------------------
      Nks   = Nks_Max
      NTObs = NTObs_Max
      ScanLoop : do iScan = 1, 2

!        Allocate space in radcor data structure
!        ---------------------------------------
         call Prep_Init  ( Nks, NTObs, Prep, Obs, stat )
         if ( stat .ne. 0 ) then
            call WPErr ( MyName, ErrStat ( 'Prep_Init' ))
            stat = Alloc_Error
            return
         end if

!        Get the radcor program code
!        ---------------------------
         call Bufr_Inq_byFile (File, stat,
     .                         PName     = LocOptions % LastProgName,
     .                         SynTag    = SynTagF,
     .                         CQC_PCode = Prep       % CQC_PCode,
     .                         Rad_PCode = Prep       % Rad_PCode,
     .                         VT_PCode  = Prep       %  VT_PCode,
     .                         PCode     = Prep       % LastPCode )
         if ( stat .ne. 0 ) then
            call WPErr ( MyName, ErrStat ( 'Bufr_Inq_byFile' ))
            return
         end if

         call Prep2File ( Prep, Mass % File )  ! Copy file info to
         call Prep2File ( Prep, Wind % File )  ! ... local storage

!        Open buffer file
!        ----------------
         open ( unit   =  lu,
     .          file   =  File,
     .          status = 'old',
     .          form   = 'unformatted',
     .          iostat =  stat )
         if ( stat .ne. 0 ) then
            call PErr ( MyName, ' Open error (iostat = '
     .                       //   trim ( ItoA ( stat ) ) // ') \n'
     .                       // ' File = ' // trim ( File ) )
            if ( iScan .eq. 2 ) call Prep_Clean ( Prep, Obs )
            stat = Open_Error
            return

         end if

!        Set up the Bufr software to read the data
!        -----------------------------------------
         call OpenBF ( lu, 'IN', lu )

!        Set up for Y2k compliant dates
!        ------------------------------
         call DateLen ( 10 )

!        Read through the file looking for upperair (ADPUPA) subsets
!        -----------------------------------------------------------
         Nks              =  0      ! Number of soundings
         NTObs            =  0      ! Total number of observations
         NMObs            =  0      ! Number of mass observations
         NWObs            =  0      ! ... and wind observations
         SynTag           =  0      ! Synoptic date/time tag
         SubSet           = ''      ! Type of subset in MG block
         NYMDH_file       =  0      ! Syn date/time tag in the file
         iret_mg          =  0      ! Returned status from call to ReadMG
         iret_sb          = -1      ! ... and ReadSB
         iSQN1            = -1      ! Sequence  number from last sb
         iPrN1            = -1      ! Processor number from last sb
         adpupa_processed = .false. ! = .true. if an ADPUPA report has
                                    !    already been processed
         varying_dates    = .false. ! = .true. if extracting data from more
                                    !    than one synoptic date and time.
         call Init_pBufr ( Mass = Mass, Wind = Wind )

!        Search for the first MG block of raob (ADPUPA) reports
!        ------------------------------------------------------
         if ( get_all_data ) then
            do while ( iret_mg    .eq.  0      .and.
     .                    SubSet  .ne. 'ADPUPA' )
               call ReadMG ( lu, SubSet, SynTag, iret_mg )
            end do
         else
            do while ( iret_mg    .eq.  0      .and.
     .                  ( SubSet  .ne. 'ADPUPA' .or.
     .                    SynTag  .ne.  NYMDH ))
               call ReadMG ( lu, SubSet, SynTag, iret_mg )
            end do
         end if

         if ( iret_mg .eq. 0 ) then
            call ReadSB ( lu, iret_sb )
            if ( NYMDH_file .eq. 0 ) NYMDH_file = SynTag
            if ( SynTag  .ne. NYMDH_file ) varying_dates = .true.
         end if

!        As long as there are more mg blocks ...
!        ---------------------------------------
         do while ( iret_mg .eq. 0 )

!           If the next report exist ...
!           ----------------------------
            if ( iret_sb .eq. 0 ) then

!              Get header information
!              ----------------------
               call Get_PrepMeta ( lu, Meta, accept_meta )

!              ... and if accepted
!              -------------------
               if ( accept_meta ) then
                  iSQN2         =  Meta %  SeqN
                  iPrN2         =  Meta % ProcN
                  sonde_changed =  iSQN2 .ne. iSQN1 .or.
     .                             iPrN2 .ne. iPrN1
                  add_sonde     =  sonde_changed .and.
     .                             adpupa_processed
                  mass_report   =  Meta % RType / 100 .eq. 1
                  wind_report   =  Meta % RType / 100 .eq. 2

!                 ... and is the header for a new sonde
!                 -------------------------------------
                  if ( add_sonde ) then
                     Nks   = Nks + 1
                     NTObs = NTObs + NMObs + NWObs
                     if ( Nks   .le.   Nks_Max .and.
     .                    NTObs .le. NTObs_Max ) then
                        call pbufr_2Rad ( Mass, Wind, Nks, Prep, Obs )
                        call Init_pBufr ( Mass, Wind )
                     end if
                     NMObs = 0
                     NWObs = 0
                  end if

!                 Save the mass obs
!                 -----------------
                  if      ( mass_report ) then
c                     print *, 'Nks = ', Nks
                     call Meta2Meta ( Meta, Mass % Meta )
                     call Get_PrepMass ( lu, NMObs, Mass )

!                 ... or the wind obs
!                 -------------------
                  else if ( wind_report ) then
                     call Meta2Meta ( Meta, Wind % Meta )
                     call Get_PrepWind ( lu, NWObs, Wind )
                  end if
                  iSQN1            =  iSQN2
                  iPrN1            =  iPrN2
                  adpupa_processed = .true.

               end if
               call ReadSB ( lu, iret_sb )  ! Move to next report
                                            ! -------------------

            else ! ... Otherwise, search for next MG block of raob reports
                 ! -------------------------------------------------------
               SubSet  = ''
               SynTag  =  0
               iret_mg =  0
               iret_sb = -1
               if ( get_all_data ) then
                  do while ( iret_mg   .eq.  0      .and.
     .                         SubSet  .ne. 'ADPUPA' )
                     call ReadMG ( lu, SubSet, SynTag, iret_mg )
                  end do
               else
                  do while ( iret_mg   .eq.  0      .and.
     .                        ( SubSet .ne. 'ADPUPA' .or.
     .                          SynTag .ne.  NYMDH ))
                     call ReadMG ( lu, SubSet, SynTag, iret_mg )
                  end do
               end if
               if ( iret_mg .eq. 0 ) then
                  call ReadSB ( lu, iret_sb )
                  if ( SynTag  .ne. NYMDH_file ) varying_dates = .true.
               end if
            end if
         end do

!        Close the file
!        --------------
         call ClosBF ( lu )

!        Error handling for the case when syn date/time changes
!        ------------------------------------------------------
         if ( varying_dates ) then
            call WPErr ( MyName, ' Attempt to retrieve data from the '
     .                       //  'entire file with varying synoptic '
     .                       //  'dates and times \n\C'
     .                       //  ' File = ' // trim ( File ) )
            call Prep_Clean ( Prep, Obs )
            stat = Varying_SDate
            return
         end if

!        Save navigators for the last sonde
!        ----------------------------------
         if ( adpupa_processed ) then
            Nks   = Nks + 1
            NTObs = NTObs + NMObs + NWObs
            if ( Nks   .le.   Nks_Max .and.
     .           NTObs .le. NTObs_Max ) then
               call pbufr_2Rad ( Mass, Wind, Nks, Prep, Obs )
            end if
            NMObs = 0
            NWObs = 0
         end if

!        Check to determine if all data was saved (i.e, exit loop)
!        ---------------------------------------------------------
         if (   Nks .le.   Nks_Max .and.
     .        NTObs .le. NTObs_Max ) then
            exit ScanLoop

         else ! ... or if a second scan is necessary
              ! ------------------------------------
            call Prep_Clean ( Prep, Obs )
         end if
      end do ScanLoop

!     Add final bookkeeping information to the radcor data structure
!     --------------------------------------------------------------
      call Bufr_Inq_ ( Prep_Type = Prep_Type )
      Obs % FileType =  Prep_Type
      Obs % ObsFile  =  File
      Obs % MetaFile = ' '
      Obs % Meta   % Date = SynTagF / 100
      Obs % Meta   % Time = mod ( SynTagF, 100 ) * 10000
      Obs % Meta   % Nks  = Nks
      Obs % Vector % NObs = NTObs

!     All is well
!     -----------
      stat = 0

      return
      end subroutine Get_Raobs_Tag
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: pbufr_2Rad() --- Store the data into the radcor structure
!
! !DESCRIPTION:
!     This routine stores the data in the pbufr structure into the
!     radcor structure and updates the bookkeeping
!
! !INTERFACE:
      subroutine pbufr_2Rad ( Mass, Wind, Nks, Prep, RadData )
      use m_RadData,   only : radcor_profiles, AMiss, QC_Rejected,
     .                        LTHist_IMiss, LTHist_RObTime, LTHist_File,
     .                        ETHist_File,  ETHist_IMiss
      use m_soundings, only : logP2Y, ExtrapM
      use m_stdatm,    only : StdAtmP
!
! !INPUT PARAMETERS:
      implicit   NONE
      type ( pbufr_mass ),      intent (in)    ::
     .   Mass                 ! Mass data
      type ( pbufr_wind ),      intent (in)    ::
     .   Wind                 ! Wind data
      integer,                  intent (in)    ::
     .   Nks                  ! Number of soundings
!
! !INPUT/OUTPUT PARAMETERS:
      type ( prep_info ),       intent (inout) ::
     .   Prep                 ! Prep-data information
      type ( radcor_profiles ), intent (inout) ::
     .   RadData              ! Radcor data structure
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     11Aug2003  C. Redder   Origional code
!     31Oct2003  C. Redder   Set default of and LTHist and ETHist to missing
!     13Nov2003  C. Redder   Replaced the data structure radcor with
!                            radcor_profiles.
!     29Jan2004  C. Redder   Added code to process drift data
!     17Feb2004  C. Redder   Renamed the component, QM to QC_file in the
!                            data structure, radcor_profiles.
!     20Feb2004  C. Redder   Added the component, P_QM and ObQM, to the
!                            sub-data structure, prep_vector, in the
!                            data structure, prep_info.
!     21Apr2004  C. Redder   Added the capability of aquiring the
!                            data for the components, P_PC, ObPC and
!                            P_Missing and ObMissing, in the structures,
!                            pbufr_mass and pbufr_wind.
!     02Jun2004  C. Redder   Added the capability of aquiring the
!                            data for the components, ObFC and
!                            FCMissing, in the structures pubf_mass and
!                            pbufr_wind.
!     02Sep2004  C. Redder   Added the component, QC_Local, to the
!                            type (i.e. data structures), prep_vector
!                            (in prep_infor), pbufr_mass and pbufr_wind
!     21Jul2007  C. Redder   Added the component, Cat, to the sub-data
!                            structure of type, prep_vector.  Added the
!                            component, Cat, to the data structure of
!                            type, prep_vector.
!EOP
!-------------------------------------------------------------------------

      integer, parameter :: NVar = MassNVar
      logical :: choose_mass,  drift_avail, reject_OIQC
      real    :: LTime, STime, BTime, DelTime, ET, StnElev
      integer :: iPBeg, iPEnd, NPObs, MassNLev, WindNLev,
     .           iSfcLev, iLev, TNLev, iVar, kt_iVar, iOb, NZ2,
     .           iMLev, iWLev, iBeg, rkx, rkx_max
      real,    dimension (:), pointer :: DLat, DLon, P, Obs, ETime, QM,
     .                                   P_QM, ObQM, P_PC, ObPC, FG
      integer, dimension (:), pointer :: Indx, P_e,  P_m, kt, QC,
     .                                   RepType,    QC_Local, Cat
      logical, dimension (:), pointer :: P_Missing,  ObMissing,
     .                                   FGMissing,
     .                                   T_VIRTMP,   H_VIRTMP
      real,    dimension ( 2 * NLev_Max ) :: P2, ETime2, Z2
      logical, dimension ( 2 * NLev_Max ) :: Mask
      integer, dimension     ( NLev_Max ) :: MIndx, MLLoc, WIndx, WLLoc

!     Get the number of levels
!     ------------------------
      MassNLev = 0
      WindNLev = 0
      if ( Mass % loaded ) MassNLev = Mass % NLev
      if ( Wind % loaded ) WindNLev = Wind % NLev

!     Determine the navagators for the current profile (or sounding)
!     --------------------------------------------------------------
      iPBeg = 1
      if ( Nks .gt. 1 ) iPBeg = RadData % Meta % ksLoc ( Nks - 1 )
     .                        + RadData % Meta % ksLen ( Nks - 1 )
      NPObs = MassNLev * MassNVar + WindNLev * WindNVar
      iPEnd = iPBeg + NPObs  - 1

!     Check bounds
!     ------------
      if ( Nks   .gt. RadData % Meta   % NVct .or.
     .     iPEnd .gt. RadData % Vector % NVct ) return

!     Initialize if first sounding and update maximums
!     ------------------------------------------------
      if ( Nks .le. 1 ) then
         Prep % P_PC_Max = 0
         Prep % ObPC_Max = 0
      end if
      Prep % P_PC_Max = max ( Prep % P_PC_Max,
     .                        Mass % P_PC_Max,
     .                        Wind % P_PC_Max )
      Prep % ObPC_Max = max ( Prep % ObPC_Max,
     .                        Mass % ObPC_Max,
     .                        Wind % ObPC_Max )

!     Save navigators
!     ---------------
      RadData % Meta % ksLoc ( Nks ) = iPBeg
      RadData % Meta % ksLen ( Nks ) = NPObs

!     Merge data from mass and wind reports into a single profile
!     -----------------------------------------------------------
      iMLev   = 0
      iWLev   = 0
      iBeg    = 1
      TNLev   = MassNLev + WindNLev
      do iLev = 1, TNLev
         if ( iMLev .lt. MassNLev .and.
     .        iWLev .lt. WindNLev ) then
            choose_mass = Mass % P_e ( iMLev + 1 ) .gt.
     .                    Wind % P_e ( iWLev + 1 ) .or.
     .                    Mass % P_e ( iMLev + 1 ) .eq.
     .                    Wind % P_e ( iWLev + 1 ) .and.
     .                    Mass % P_m ( iMLev + 1 ) .ge.
     .                    Wind % P_m ( iWLev + 1 )

         else
            choose_mass = iMLev .lt. MassNLev

         end if
         if ( choose_mass ) then
            iMLev = iMLev + 1
            MIndx ( iMLev ) = iLev
            MLLoc ( iMLev ) = iBeg
            iBeg  = iBeg  + MassNVar
         else
            iWLev = iWLev + 1
            WIndx ( iWLev ) = iLev
            WLLoc ( iWLev ) = iBeg
            iBeg  = iBeg  + WindNVar
         end if
      end do

      iSfcLev  = 0
      do iLev = 1, MassNLev
         iMLev = MIndx ( iLev )
         if ( Mass % Cat ( iLev ) .eq. 0 ) iSfcLev = iMLev
         Z2     ( iMLev ) =  Mass % Obs      ( iVarHeight, iLev )
         P2     ( iMLev ) =  Mass % P        ( iLev )
         ETime2 ( iMLev ) =  Mass % ETime    ( iLev )
         Mask   ( iMLev ) =  Mass % QC_Local ( iVarHeight, iLev )
     .                      .ne. QC_Rejected
      end do
      do iLev = 1, WindNLev
         iWLev = WIndx ( iLev )
         P2     ( iWLev ) =  Wind % P     ( iLev )
         ETime2 ( iWLev ) =  Wind % ETime ( iLev )
         Mask   ( iWLev ) = .false.
      end do

!     ... and determine if drift data is available
!     --------------------------------------------
      drift_avail = .true.
      if ( Mass % loaded )
     .   drift_avail = drift_avail .and. Mass % drift_avail
      if ( Wind % loaded )
     .   drift_avail = drift_avail .and. Wind % drift_avail

!     ... and obtain the launch time
!     ------------------------------
      StnElev     = Mass % Meta % Elev
      if ( drift_avail ) then
         call logP2Y  ( P2   ( : TNLev ), Mask, (Z2),
     .                  P2   ( : TNLev ), Z2 )
         call ExtrapM ( P2   ( : TNLev ), Mask, (Z2),
     .                  P2   ( : TNLev ), Z2 )

!        ... and obtain the balloon drift time at the surface
!        ----------------------------------------------------
         if ( iSfcLev .ne. 0 ) then
            STime = ETime2  ( iSfcLev ) ! ... with
         else                           ! ... or without cat 0 obs.
            STime = SfcTime    ( StnElev, Z2 ( : TNLev ), ETime2, Mask )
            if ( STime .eq. IMiss ) then
               call StdAtmP ( P2 ( :TNLev ), Z2 )
               do iLev = 1, TNLev
                  Mask ( iLev ) = .true.
               end do
               STime = SfcTime ( StnElev, Z2 ( : TNLev ), ETime2, Mask )
            end if
         end if
c      if ( TNLev .gt. 0 ) STime = ETime2 ( 1 )
         LTime = STime
         if ( TNLev .gt. 0 ) LTime = ETime2 ( 1 )
         DelTime  = LTime - STime

         do iLev  = 1, TNLev
            ETime2 ( iLev ) =  ETime2 ( iLev ) + DelTime - LTime
            if ( Z2 ( iLev ) .lt. StnElev ) ETime2 ( iLev ) = 0
         end do
      end if

!     Store into the radcor data structure, the meta data ...
!     -------------------------------------------------------
      if ( Mass % loaded ) then
         call Save_PrepMeta ( Mass % Meta, Nks, Prep, RadData % Meta )
      else
         call Save_PrepMeta ( Wind % Meta, Nks, Prep, RadData % Meta )
      end if
      RadData % Meta % LTime          ( Nks ) = LTime
      RadData % Meta % LTHist         ( Nks ) = LTHist_IMiss
      if (  Mass % Meta % TCor .eq. 0 )
     .   RadData % Meta % LTHist      ( Nks ) = LTHist_RObTime
      if (  Mass % Meta % TCor .eq. 1 )
     .   RadData % Meta % LTHist      ( Nks ) = LTHist_File
      RadData    % Meta % ETHist      ( Nks ) = ETHist_IMiss
      if (  drift_avail )
     .   RadData % Meta % ETHist      ( Nks ) = ETHist_File
      rkx     = RadData % Meta  % rkx ( Nks )
      rkx_max = RadData % Lists % rkx_max
      if ( rkx .eq. 0 )  ! WMO code of 0 is equivalent to the max
     .   RadData % Meta % rkx         ( Nks ) = rkx_max

!     ... mass data
!     -------------
      DLat      => RadData % Vector % DLat      ( iPBeg : iPEnd )
      DLon      => RadData % Vector % DLon      ( iPBeg : iPEnd )
      P         => RadData % Vector % P         ( iPBeg : iPEnd )
      P_m       => RadData % Vector % P_m       ( iPBeg : iPEnd )
      P_e       => RadData % Vector % P_e       ( iPBeg : iPEnd )
      ETime     => RadData % Vector % ETime     ( iPBeg : iPEnd )
      Obs       => RadData % Vector % Obs       ( iPBeg : iPEnd )
      FG        => RadData % Vector % FG        ( iPBeg : iPEnd )

      P_Missing => RadData % Vector % P_Missing ( iPBeg : iPEnd )
      ObMissing => RadData % Vector % ObMissing ( iPBeg : iPEnd )
      FGMissing => RadData % Vector % FGMissing ( iPBeg : iPEnd )

      Indx      => RadData % Vector % Indx      ( iPBeg : iPEnd )
      kt        => RadData % Vector % kt        ( iPBeg : iPEnd )
      QC        => RadData % Vector % QC        ( iPBeg : iPEnd )
      QM        => RadData % Vector % QC_file   ( iPBeg : iPEnd )
      RepType   => Prep    % Vector % RepType   ( iPBeg : iPEnd )
      P_QM      => Prep    % Vector % P_QM      ( iPBeg : iPEnd )
      ObQM      => Prep    % Vector % ObQM      ( iPBeg : iPEnd )
      P_PC      => Prep    % Vector % P_PC      ( iPBeg : iPEnd )
      ObPC      => Prep    % Vector % ObPC      ( iPBeg : iPEnd )
      QC_Local  => Prep    % Vector % QC_Local  ( iPBeg : iPEnd )
      Cat       => Prep    % Vector % Cat       ( iPBeg : iPEnd )

      T_VIRTMP  => Prep    % Vector % T_VIRTMP  ( iPBeg : iPEnd )
      H_VIRTMP  => Prep    % Vector % H_VIRTMP  ( iPBeg : iPEnd )

      do iVar = 1, MassNVar
         kt_iVar = Masskt_List ( iVar )
         if ( iVar .eq. iVarHum ) kt_iVar = Mass % kt_humidity
         do iLev = 1, MassNLev
            iOb   = MLLoc ( iLev ) + iVar - 1
            iMLev = MIndx ( iLev )
            DLat      ( iOb ) = Mass % DLat            ( iLev )
            DLon      ( iOb ) = Mass % DLon            ( iLev )
            P         ( iOb ) = Mass % P               ( iLev )
            P_m       ( iOb ) = Mass % P_m             ( iLev )
            P_e       ( iOb ) = Mass % P_e             ( iLev )
            P_QM      ( iOb ) = Mass % P_QM            ( iLev )
            P_PC      ( iOb ) = Mass % P_PC            ( iLev )
            P_Missing ( iOb ) = Mass % P_Missing       ( iLev )
            ETime     ( iOb ) = ETime2                ( iMLev )
            Indx      ( iOb ) = Mass % Indx            ( iLev )
            T_VIRTMP  ( iOb ) = Mass % T_VIRTMP        ( iLev )
            H_VIRTMP  ( iOb ) = Mass % H_VIRTMP        ( iLev )
            Obs       ( iOb ) = Mass % Obs       ( iVar, iLev )
            FG        ( iOb ) = Mass % ObFC      ( iVar, iLev )
            ObQM      ( iOb ) = Mass % ObQM      ( iVar, iLev )
            ObPC      ( iOb ) = Mass % ObPC      ( iVar, iLev )
            ObMissing ( iOb ) = Mass % ObMissing ( iVar, iLev )
            FGMissing ( iOb ) = Mass % FCMissing ( iVar, iLev )
            kt        ( iOb ) = kt_iVar
            QC        ( iOb ) = Mass % QC        ( iVar, iLev )
            QC_Local  ( iOb ) = Mass % QC_Local  ( iVar, iLev )
            Cat       ( iOb ) = Mass % Cat             ( iLev )
            QM        ( iOb ) = Mass % QM        ( iVar, iLev )

            RepType   ( iOb ) = MassReport

         end do
      end do

!     ... and wind data
!     -----------------
      do iVar = 1, WindNVar
         kt_iVar = Windkt_List ( iVar )
         do iLev = 1, WindNLev
            iOb   = WLLoc ( iLev ) + iVar - 1
            iWLev = WIndx ( iLev )
            DLat      ( iOb ) = Wind % DLat           (  iLev )
            DLon      ( iOb ) = Wind % DLon           (  iLev )
            P         ( iOb ) = Wind % P              (  iLev )
            P_m       ( iOb ) = Wind % P_m            (  iLev )
            P_e       ( iOb ) = Wind % P_e            (  iLev )
            P_QM      ( iOb ) = Wind % P_QM           (  iLev )
            P_PC      ( iOb ) = Wind % P_PC           (  iLev )
            P_Missing ( iOb ) = Wind % P_Missing      (  iLev )
            ETime     ( iOb ) = ETime2                ( iWLev )
            Indx      ( iOb ) = Wind % Indx           (  iLev )
            T_VIRTMP  ( iOb ) = .false.
            H_VIRTMP  ( iOb ) = .false.
            Obs       ( iOb ) = Wind % Obs       ( iVar, iLev )
            FG        ( iOb ) = Wind % ObFC      ( iVar, iLev )
            ObQM      ( iOb ) = Wind % ObQM      ( iVar, iLev )
            ObPC      ( iOb ) = Wind % ObPC      ( iVar, iLev )
            ObMissing ( iOb ) = Wind % ObMissing ( iVar, iLev )
            FGMissing ( iOb ) = Wind % FCMissing ( iVar, iLev )
            kt        ( iOb ) = kt_iVar
            QC        ( iOb ) = Wind % QC        ( iVar, iLev )
            QC_Local  ( iOb ) = Wind % QC_Local  ( iVar, iLev )
            Cat       ( iOb ) = Wind % Cat             ( iLev )
            QM        ( iOb ) = Wind % QM        ( iVar, iLev )
            RepType   ( iOb ) = WindReport
         end do
      end do

!     Drift data is assumed to be available
!     only if available in all profiles
!     -------------------------------------
      Prep % drift_avail = Prep % drift_avail .and. drift_avail

      return
      end subroutine pbufr_2Rad
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Rad_2pbufr() --- Put radcor data into the data structure pbufr
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Rad_2pbufr ( Prep, RadData, Nks, Mass, Wind )
      use m_RadData, only : radcor_profiles, AMiss,
     .                      kt_HumRadcor => kt_Humidity
!
! !INPUT PARAMETERS:
      implicit   NONE
      integer,                  intent (in)    ::
     .   Nks
      type ( prep_info ),       intent (in)    ::
     .   Prep                 ! Prep-data information
      type ( radcor_profiles ), intent (in)    ::
     .   RadData              ! Radcor data structure
!
! !INPUT/OUTPUT PARAMETERS:
      type ( pbufr_mass ),      intent (inout) ::
     .   Mass                 ! Mass data
      type ( pbufr_wind ),      intent (inout) ::
     .   Wind                 ! Wind data
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     11Aug2003  C. Redder   Origional code
!     13Nov2003  C. Redder   Replaced the data structure radcor with
!                            radcor_profiles.
!     29Jan2004  C. Redder   Added code to process drift data
!     17Feb2004  C. Redder   Renamed the component, QM to QC_file in the
!                            data structure, radcor_profiles.
!     20Feb2004  C. Redder   Added the component, P_QM and ObQM, to the
!                            sub-data structure, prep_vector, in the
!                            data structure, prep_info.
!     21Apr2004  C. Redder   Added the capability of aquiring the
!                            data from the components, P_PC, ObPC and
!                            P_Missing and ObMissing, in the structures,
!                            pbufr_mass and pbufr_wind.
!     02Jun2004  C. Redder   Added the capability of aquiring the
!                            data from the components, ObFC and
!                            FCMissing in the structures pubf_mass and
!                            pbufr_wind.
!     02Sep2004  C. Redder   Added the component, QC_Local, to the
!                            type (i.e. data structures), prep_vector
!                            (in prep_infor), pbufr_mass and pbufr_wind
!     02Nov2006  C. Redder   Set the component kt_humidity in the
!                            variable Mass of type pbufr_mass.
!     21Jan2007  C. Redder   Set P_PC_Max and ObPC_Max in the data
!                            structures, pbufr_max and pbufr_wind.
!                            Added the component, Cat, to the data
!                            structure of type, prep_vector
! EOP
!-------------------------------------------------------------------------

      logical :: choose_mass,  ETime_Avail
      real    :: LTime, STime, BTime, DelTime, ET, StnElev
      integer :: iPBeg, iPEnd, NPObs, iType,
     .           MassNLev, MassNObs, WindNLev, WindNObs,
     .           iSfcLev, iLev, TNLev, iVar, kt_iVar, iOb
      real,    dimension (:), pointer :: DLat, DLon, P,    ETime,
     .                                   Obs,  QM,   P_QM, ObQM,
     .                                               P_PC, ObPC, FG
      integer, dimension (:), pointer :: Indx, P_e,  P_m, kt, QC,
     .                                   RepType,    QC_Local, Cat
      logical, dimension (:), pointer :: P_Missing,  ObMissing,
     .                                   FGMissing,
     .                                   T_VIRTMP,   H_VIRTMP
      real,    dimension ( 2 * NLev_Max ) :: P2,  ETime2
      integer :: MIndx ( NLev_Max * MassNVar ),
     .           WIndx ( NLev_Max * WindNVar )

!     Determine the navagators for the current profile (or sounding)
!     --------------------------------------------------------------
      iPBeg = RadData % Meta % ksLoc ( Nks )
      NPObs = RadData % Meta % ksLen ( Nks )
      iPEnd = iPBeg + NPObs  - 1

!     Get list of ...
!     ---------------
      RepType  => Prep % Vector % RepType ( iPBeg : iPEnd )
      MassNObs =  0
      WindNObs =  0
      do iOb = 1, NPObs
         iType = RepType ( iOb )
         if ( iType .eq. MassReport ) then  ! ... mass observations
            MassNObs = MassNObs + 1
            MIndx ( MassNObs ) = iOb
         end if
         if ( iType .eq. WindReport ) then  ! ... wind observations
            WindNObs = WindNObs + 1
            WIndx ( WindNObs ) = iOb
         end if
      end do

!     Get the number of levels
!     ------------------------
      MassNLev = MassNObs / MassNVar
      WindNLev = WindNObs / WindNVar

!     Check bounds
!     ------------
      if ( Nks  .gt. RadData % Meta   % NVct .or.
     .    iPEnd .gt. RadData % Vector % NVct ) return

!     Store radcor data into the mass data structure ...
!     --------------------------------------------------
      LTime     =  RadData % Meta   % LTime     ( Nks )
      DLat      => RadData % Vector % DLat      ( iPBeg : iPEnd )
      DLon      => RadData % Vector % DLon      ( iPBeg : iPEnd )
      kt        => RadData % Vector % kt        ( iPBeg : iPEnd )
      P         => RadData % Vector % P         ( iPBeg : iPEnd )
      P_m       => RadData % Vector % P_m       ( iPBeg : iPEnd )
      P_e       => RadData % Vector % P_e       ( iPBeg : iPEnd )
      ETime     => RadData % Vector % ETime     ( iPBeg : iPEnd )
      Obs       => RadData % Vector % Obs       ( iPBeg : iPEnd )
      FG        => RadData % Vector % FG        ( iPBeg : iPEnd )

      P_Missing => RadData % Vector % P_Missing ( iPBeg : iPEnd )
      ObMissing => RadData % Vector % ObMissing ( iPBeg : iPEnd )
      FGMissing => RadData % Vector % FGMissing ( iPBeg : iPEnd )

      Indx      => RadData % Vector % Indx      ( iPBeg : iPEnd )
      QC        => RadData % Vector % QC        ( iPBeg : iPEnd )
      QM        => RadData % Vector % QC_file   ( iPBeg : iPEnd )
      P_QM      => Prep    % Vector % P_QM      ( iPBeg : iPEnd )
      ObQM      => Prep    % Vector % ObQM      ( iPBeg : iPEnd )
      P_PC      => Prep    % Vector % P_PC      ( iPBeg : iPEnd )
      ObPC      => Prep    % Vector % ObPC      ( iPBeg : iPEnd )
      QC_Local  => Prep    % Vector % QC_Local  ( iPBeg : iPEnd )
      Cat       => Prep    % Vector % Cat       ( iPBeg : iPEnd )

      T_VIRTMP  => Prep    % Vector % T_VIRTMP  ( iPBeg : iPEnd )
      H_VIRTMP  => Prep    % Vector % H_VIRTMP  ( iPBeg : iPEnd )

      do iVar = 1, MassNVar
         do iLev = 1, MassNLev
            iOb  = MIndx ( MassNVar * ( iLev - 1 ) + iVar )
            Mass % DLat            ( iLev ) = DLat      ( iOb )
            Mass % DLon            ( iLev ) = DLon      ( iOb )
            Mass % P               ( iLev ) = P         ( iOb )
            Mass % P_m             ( iLev ) = P_m       ( iOb )
            Mass % P_e             ( iLev ) = P_e       ( iOb )
            Mass % P_QM            ( iLev ) = P_QM      ( iOb )
            Mass % P_PC            ( iLev ) = P_PC      ( iOb )
            Mass % P_Missing       ( iLev ) = P_Missing ( iOb )
            Mass % Indx            ( iLev ) = Indx      ( iOb )
            Mass % T_VIRTMP        ( iLev ) = T_VIRTMP  ( iOb )
            Mass % H_VIRTMP        ( iLev ) = H_VIRTMP  ( iOb )
            Mass % ETime           ( iLev ) = ETime     ( iOb ) + LTime
            Mass % Obs       ( iVar, iLev ) = Obs       ( iOb )
            Mass % ObFC      ( iVar, iLev ) = FG        ( iOb )
            Mass % ObQM      ( iVar, iLev ) = ObQM      ( iOb )
            Mass % ObPC      ( iVar, iLev ) = ObPC      ( iOb )
            Mass % ObMissing ( iVar, iLev ) = ObMissing ( iOb )
            Mass % FCMissing ( iVar, iLev ) = FGMissing ( iOb )
            Mass % QC        ( iVar, iLev ) = QC        ( iOb )
            Mass % QC_Local  ( iVar, iLev ) = QC_Local  ( iOb )
            Mass % Cat             ( iLev ) = Cat       ( iOb )
            Mass % QM        ( iVar, iLev ) = QM        ( iOb )
         end do
      end do
      Mass % NLev        = MassNLev
      Mass % drift_avail = Prep % drift_avail
      Mass % kt_humidity = kt_HumRadcor
      Mass % P_PC_Max    = Prep % P_PC_Max
      Mass % ObPC_Max    = Prep % ObPC_Max
      if ( MassNLev .gt. 0 ) Mass % kt_humidity = kt ( iVarHum )

!     ... and wind data
!     -----------------
      do iVar = 1, WindNVar
         do iLev = 1, WindNLev
            iOb  = WIndx ( WindNVar * ( iLev - 1 ) + iVar )
            Wind % DLat            ( iLev ) = DLat      ( iOb )
            Wind % DLon            ( iLev ) = DLon      ( iOb )
            Wind % P               ( iLev ) = P         ( iOb )
            Wind % P_m             ( iLev ) = P_m       ( iOb )
            Wind % P_e             ( iLev ) = P_e       ( iOb )
            Wind % P_QM            ( iLev ) = P_QM      ( iOb )
            Wind % P_PC            ( iLev ) = P_PC      ( iOb )
            Wind % P_Missing       ( iLev ) = P_Missing ( iOb )
            Wind % ETime           ( iLev ) = ETime     ( iOb ) + LTime
            Wind % Indx            ( iLev ) = Indx      ( iOb )
            Wind % Obs       ( iVar, iLev ) = Obs       ( iOb )
            Wind % ObFC      ( iVar, iLev ) = FG        ( iOb )
            Wind % ObQM      ( iVar, iLev ) = ObQM      ( iOb )
            Wind % ObPC      ( iVar, iLev ) = ObPC      ( iOb )
            Wind % ObMissing ( iVar, iLev ) = ObMissing ( iOb )
            Wind % QC        ( iVar, iLev ) = QC        ( iOb )
            Wind % QC_Local  ( iVar, iLev ) = QC_Local  ( iOb )
            Wind % Cat             ( iLev ) = Cat       ( iOb )
            Wind % QM        ( iVar, iLev ) = QM        ( iOb )
         end do
      end do
      Wind % NLev        = WindNLev
      Wind % drift_avail = Prep % drift_avail
      Wind % P_PC_Max    = Prep % P_PC_Max
      Wind % ObPC_Max    = Prep % ObPC_Max


      return
      end subroutine Rad_2pbufr

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Put_Raobs_DT() --- Put raob data into the buffer file for given date and time
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Put_Raobs_DT ( File, NYMD, NHMS, Prep, Obs, stat,
     .                          Options, write_report, cleanup )
      use m_RadData,  only : radcor_profiles
      use m_AdvError, only : WPErr, ErrStat
      use m_SunAlt,   only : Julian, CalDate
!
! !INPUT PARAMETERS:
      implicit   NONE
      character (len=*),        intent (in)    ::
     .   File                   ! Output buffer file
      integer,                  intent (in)    ::
     .   NYMD,                  ! Synoptic date and time for the desired
     .   NHMS                   !   data, in YYYYMMDD and HHMMSS format.
                                !  (Time rounded to nearest hour.)  Set
                                !   date to zero or less to retrieve all
                                !   data in buffer file. (But non-zero
                                !   error status is returned if date and
                                !   time tags vary within the file.)
      type ( prep_options ),    intent (in), optional ::
     .   Options                ! Options for writing to buffer file.
      logical, optional,        intent (in), dimension (:) ::
     .   write_report           ! = .true. to write corresponding report
                                !   to file
      logical, optional,        intent (in)    ::
     .   cleanup                ! = .true. to deallocate and reset input
                                !   data structure.  Default:
!                               !   cleanup = .false.
! !INPUT/OUTPUT PARAMETERS:
      type ( prep_info ),       intent (inout) ::
     .   Prep                   ! Information about the prep buffer file
                                !   which is used to write the data to
!                               !   the buffer file
      type ( radcor_profiles ), intent (inout) ::
     .   Obs                    ! Obs vector for both raobs and non-raobs
                                !   On output, both data structures are
                                !   reset if the optional arguent,
!                               !   cleanup = .true.
! !OUTPUT PARAMETERS:
      integer,                  intent (out)   ::
     .   stat                   ! Returned status code
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     16Apr2003  C. Redder   Origional code
!     09Jul2003  C. Redder   Added optional argument, cleanup, and fixed
!                            bug regarding the calculation of NYMDH
!     12Nov2003  C. Redder   Replaced the data sturcture, radcor, with
!                            radcor_profiles.
!     20Apr2004  C. Redder   Added input optional argument, Options.
!     26Jan2007  C. Redder   Added the optional argument, write_report.
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .           MyName = MyModule // '::Put_Raobs_DT'
      integer :: NYMDH, HH, MMSS, JHr

      if ( NYMD .le. 0 ) then
         NYMDH = 0
      else
         HH    =       NHMS / 10000
         MMSS  = mod ( NHMS,  10000 )
         if ( MMSS .ge. 3000 ) HH = HH + 1
         JHr   = ( Julian ( NYMD ) - 1 ) * 24 + HH
         NYMDH =   CalDate ( JHr / 24 + 1 ) * 100 + mod ( JHr, 24 )
      end if

      call Put_Raobs_Tag ( File, NYMDH, Prep, Obs, stat,
     .                     Options, write_report, cleanup )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Put_Raobs_Tag' ))
         return
      end if

      return
      end subroutine Put_Raobs_DT

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Put_Raobs_() --- Put raob data into buffer file
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Put_Raobs_ ( File, Prep, Obs, stat,
     .                        Options, write_report, cleanup )
      use m_RadData,  only : radcor_profiles
      use m_AdvError, only : WPErr, ErrStat
!
! !INPUT PARAMETERS:
      implicit   NONE
      character (len=*),        intent (in)    ::
     .   File                   ! Output buffer file
      type ( prep_options ),    intent (in), optional ::
     .   Options                ! Options for writing to buffer file.
      logical, optional,        intent (in), dimension (:) ::
     .   write_report           ! = .true. to write corresponding report
                                !   to file
      logical, optional,        intent (in)     ::
     .   cleanup                ! = .true. to deallocate and reset input
                                !   data structure.  Default:
!                               !   cleanup = .false.
! !INPUT/OUTPUT PARAMETERS:
      type ( prep_info ),       intent (inout) ::
     .   Prep                   ! Information about the prep buffer file
                                !   which is used to write the data to the
                                !   buffer file
      type ( radcor_profiles ), intent (inout) ::
     .   Obs                    ! Obs vector for both raobs and non-raobs
                                !   On output, both data structures are
                                !   reset if the optional arguent,
!                               !   cleanup = .true.
! !OUTPUT PARAMETERS:
      integer,                  intent (out) ::
     .   stat                   ! Returned status code
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     16Apr2003  C. Redder   Origional code
!     09Jul2003  C. Redder   Added optional argument, cleanup
!     12Nov2003  C. Redder   Replaced the data sturcture, radcor, with
!                            radcor_profiles.
!     20Apr2004  C. Redder   Added input optional argument, Options.
!     26Jan2007  C. Redder   Added the optional argument, write_report.
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .           MyName = MyModule // '::Put_Raobs_'
      integer, parameter :: NYMDH = 0

      call Put_Raobs_Tag( File, NYMDH, Prep, Obs, stat,
     .                    Options, write_report, cleanup )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Put_Raobs_Tag' ))
         return
      end if

      return
      end subroutine Put_Raobs_
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Put_Raobs_Tag() --- Put raob data into buffer file.
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Put_Raobs_Tag ( File, NYMDH, Prep, Obs, stat,
     .                           Options, write_report, cleanup )
      use m_RadData,  only : radcor_profiles, Rad_Clean
      use m_AdvError, only : PErr, WPErr, ErrStat, ItoA
      use m_SysIO,    only : LUAvail
!
! !INPUT PARAMETERS:
      implicit   NONE
      character (len=*),        intent (in)    ::
     .   File                   ! Input buffer file
      integer,                  intent (in)    ::
     .   NYMDH                  ! Synoptic date and hour for the desired
                                !   data, in YYYYMMDDHH format.  Set tag
                                !   to zero or less to retrieve all data
                                !   in buffer file.  (But non-zero error
                                !   status is returned if date and time
                                !   tags vary within the file.)
      type ( prep_options ),    intent (in), optional ::
     .   Options                ! Options for writing to buffer file.
      logical, optional,        intent (in), dimension (:) ::
     .   write_report           ! = .true. to write corresponding report
                                !   to file
      logical, optional,        intent (in)     ::
     .   cleanup                ! = .true. to deallocate and reset input
                                !   data structure.  Default:
!                               !   cleanup = .false.
! !INPUT/OUTPUT PARAMETERS:
      type ( prep_info ),       intent (inout) ::
     .   Prep                   ! Information about the prep buffer file
                                !   which is used to write the data to the
                                !   buffer file
      type ( radcor_profiles ), intent (inout) ::
     .   Obs                    ! Obs vector for both raobs and non-raobs
                                !   On output, both data structures are
                                !   reset if the optional arguent,
!                               !   cleanup = .true.
! !OUTPUT PARAMETERS:
      integer,                  intent (out) ::
     .   stat                   ! Returned status code
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     16Apr2003  C. Redder   Origional code
!     09Jul2003  C. Redder   Added optional argument, cleanup
!     12Nov2003  C. Redder   Replaced the data sturcture, radcor, with
!                            radcor_profiles.
!     11Mar2004  C. Redder   Added code to handle processor number as
!                            well as the sequence number.
!     20Apr2004  C. Redder   Added input optional argument, Options.
!     06Apr2004  C. Redder   Fixed bug that check sequence and processor
!                            numbers that determines the logical
!                            variable, next_sonde.
!     27Aug2006  C. Redder   Changed the argument, Options, to LocOptions
!                            in call to the routine Bufr_Inq_byFile
!     26Jan2007  C. Redder   Added the optional argument, write_report.
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .           MyName = MyModule // '::Put_Raobs_Tag'

      type ( pbufr_mass   ) :: Mass
      type ( pbufr_wind   ) :: Wind
      type ( pbufr_meta   ) :: Meta
      type ( prep_options ) :: LocOptions
      logical :: accept_meta, next_sonde, mass_report, wind_report,
     .           get_all_data, goto_next_mg, clean_up, adpupa_processed,
     .           write_rep
      integer :: lu_out,    lu_in, iOb, iSQN1, iSQN2, iPrN1, iPrN2,
     .           ks, NTObs, NObs,  iret_mg,    iret_sb, SynTag, Nks,
     .           ThisPCode, LastPCode
      real    :: Lat, Lon, Elev
      character ( len = 8 ) :: SubSet
      character ( len = 5 ) :: cmaxout
      integer :: imaxout, length

!     Implement options
!     -----------------
      clean_up  = .false.
      if ( present ( cleanup )) clean_up  = cleanup

!     Set the data stucture that contain the option switches
!     ------------------------------------------------------
      call    Init_Options ( LocOptions )
      if ( present ( Options ))
     .   call Copy_Options ( Options,    LocOptions     )
      call    Copy_Options ( LocOptions, Mass % Options )
      call    Copy_Options ( LocOptions, Wind % Options )

!     Determine the output program code
!     ---------------------------------
      call Bufr_Inq_byFile ( Obs % ObsFile, stat,
     .                       PName = LocOptions % ThisProgName,
     .                       PCode = ThisPCode )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Bufr_Inq_byFile' ))
         return
      end if
      if ( ThisPCode .eq. 0 ) then
         LastPCode = Prep % LastPCode
         if ( LastPCode .eq. 0 ) then
            ThisPCode = min ( max ( Prep % P_PC_Max,
     .                              Prep % ObPC_Max ) + 1,
     .                        ProgCode_Max )
         else
            ThisPCode = min ( LastPCode + 1, ProgCode_Max )
         end if
      end if
      Mass % File % ThisPCode = ThisPCode
      Wind % File % ThisPCode = ThisPCode

!     Implement option to get all data
!     --------------------------------
      get_all_data = NYMDH .le. 0

!     Determine an available logical unit in order to ...
!     ---------------------------------------------------
      lu_out = LUAvail()
      if ( lu_out .lt. 0 ) then
         stat = No_LUAvail
         call WPErr ( MyName, 'No logical units available for the '
     .                     // 'output file, ' // trim ( File ) )
         return
      end if

!     ... open output buffer file
!     ---------------------------
      open ( unit   =  lu_out,
     .       file   =  File,
     .       status = 'new',
     .       form   = 'unformatted',
     .       iostat =  stat )
      if ( stat .ne. 0 ) then
         call PErr ( MyName, ' Open error (iostat = '
     .                    //   trim ( ItoA ( stat ) ) // ') \n'
     .                    // ' Output file = ' // trim ( File ) )
         stat = Open_Error
         return

      end if

!     Determine another available logical unit in order to ...
!     --------------------------------------------------------
      lu_in = LUAvail()
      if ( lu_in .lt. 0 ) then
         stat = No_LUAvail
         call WPErr ( MyName, 'No logical units available for the '
     .                     // 'output file, ' // trim ( File ) )
         return
      end if

!     ... open input buffer file
!     --------------------------
      open ( unit   =  lu_in,
     .       file   =  Obs % ObsFile,
     .       status = 'old',
     .       form   = 'unformatted',
     .       iostat =  stat )
      if ( stat .ne. 0 ) then
         call PErr ( MyName, ' Open error (iostat = '
     .                    //   trim ( ItoA ( stat ) ) // ') \n'
     .                    // ' Input file = ' // trim ( File ) )
         close ( lu_out )
         stat = Open_Error
         return

      end if

!     Set up the Bufr software ...
!     ----------------------------
      call OpenBF ( lu_in,  'IN',  lu_in ) ! ... to read
      call OpenBF ( lu_out, 'OUT', lu_in ) ! ... and write the data

      CALL GET_ENVIRONMENT_VARIABLE('BUFR_MAXOUT',cmaxout,length)
      if (length > 0) then
        read(cmaxout,*) imaxout
        if (imaxout > 15000) then
          call maxout(imaxout)
        else
          call maxout(15000)
        end if
      else
        call maxout(15000)
      end if

!     Set up for Y2k compliant dates
!     ------------------------------
      call DateLen ( 10 )

!     Read through the file looking for upperair (ADPUPA) subsets
!     -----------------------------------------------------------
      Nks              =  0      ! Number of soundings
      goto_next_mg     = .true.  ! Flag set to go to next mg
      iret_sb          = -1      ! ... and ReadSB
      iSQN1            = -1      ! Sequence  number from last sb
      iPrN1            = -1      ! Processor number from last sb
      adpupa_processed = .false. ! = .true. if a ADPUPA report has
                                 !    already been processed

!     Copy file info to local storage
!     -------------------------------
      call Prep2File ( Prep, Mass % File )
      call Prep2File ( Prep, Wind % File )

!     Search for the first MG block of raob (ADPUPA) reports
!     ------------------------------------------------------
      if ( get_all_data ) then
         do while ( goto_next_mg )
            call ReadMG ( lu_in, SubSet, SynTag, iret_mg )
            goto_next_mg = iret_mg .eq. 0 .and.
     .                     SubSet  .ne. 'ADPUPA'
            if ( goto_next_mg ) then
               call ClosMG ( lu_out )
               call CopyMG ( lu_in, lu_out )
            end if
         end do
      else
         do while ( goto_next_mg )
            call ReadMG ( lu_in, SubSet, SynTag, iret_mg )
            goto_next_mg = iret_mg .eq.  0 .and.
     .                   ( SubSet  .ne. 'ADPUPA' .or.
     .                     SynTag  .ne.  NYMDH )
            if ( goto_next_mg ) then
               call ClosMG ( lu_out )
               call CopyMG ( lu_in, lu_out )
            end if
         end do
      end if

      if ( iret_mg .eq. 0 ) then
         call OpenMB ( lu_out, SubSet, SynTag )
         call ReadSB ( lu_in,  iret_sb )
      end if

!     As long as there are more mg blocks ...
!     ---------------------------------------
      do while ( iret_mg .eq. 0 )

!        If the next report exist ...
!        ----------------------------
         if ( iret_sb .eq. 0 ) then

!           Copy SB from input file to output file
!           --------------------------------------
            call UFBCPY ( lu_in, lu_out )

!           Get header information
!           ----------------------
            call Get_PrepMeta ( lu_in, Meta, accept_meta )

!           ... and if accepted
!           -------------------
            if ( accept_meta ) then
               iSQN2       =  Meta %  SeqN
               iPrN2       =  Meta % ProcN
               next_sonde  =  iSQN2 .ne. iSQN1 .or.
     .                        iPrn2 .ne. iPrN1 .or.
     .                       .not. adpupa_processed
               mass_report =  Meta % RType / 100 .eq. 1
               wind_report =  Meta % RType / 100 .eq. 2

!              ... and is the header for a new sonde
!              -------------------------------------
               if ( next_sonde ) then
                  Nks = Nks + 1
                  call Init_pBufr ( Mass, Wind )
                  call Rad_2pbufr ( Prep, Obs, Nks, Mass, Wind )
                  write_rep = .true.
                  if ( present ( write_report ) )
     .               write_rep = write_report ( Nks )
               end if

!              Save the mass obs for the report (if desired)
!              ---------------------------------------------
               if      ( write_rep    ) then
               if      (  mass_report ) then
c             print *, nks
                  call Meta2Meta    ( Meta,   Mass % Meta )
                  call Put_PrepMass ( lu_out, Mass )

!              ... or the wind obs
!              -------------------
               else if (  wind_report ) then
                  call Meta2Meta    ( Meta,   Wind % Meta )
                  call Put_PrepWind ( lu_out, Wind )

               end if
               end if
               iSQN1            =  iSQN2
               iPrN1            =  iPrN2
               adpupa_processed = .true.

            end if

            call WritSB ( lu_out )          ! Write updated report to mg
            call ReadSB ( lu_in, iret_sb )  ! ... and move to next report
                                            ! ----------------------------

         else ! ... Otherwise, search for next MG block of raob reports
              ! -------------------------------------------------------
            goto_next_mg = .true.
            iret_sb      = -1
            if ( get_all_data ) then
               do while ( goto_next_mg )
                  call ReadMG ( lu_in, SubSet, SynTag, iret_mg )
                  goto_next_mg = iret_mg .eq. 0 .and.
     .                           SubSet  .ne. 'ADPUPA'
                  if ( goto_next_mg ) then
                     call ClosMG ( lu_out )
                     call CopyMG ( lu_in, lu_out )
                  end if
               end do
            else
               do while ( goto_next_mg )
                  call ReadMG ( lu_in, SubSet, SynTag, iret_mg )
                  goto_next_mg = iret_mg .eq.  0 .and.
     .                         ( SubSet  .ne. 'ADPUPA' .or.
     .                           SynTag  .ne.  NYMDH )
                  if ( goto_next_mg ) then
                     call ClosMG ( lu_out )
                     call CopyMG ( lu_in, lu_out )
                  end if
               end do
            end if
            if ( iret_mg .eq. 0 ) then
               call OpenMB ( lu_out, SubSet, SynTag )
               call ReadSB ( lu_in,  iret_sb )
            end if
         end if
      end do

!     Close the files
!     ---------------
      call ClosBF ( lu_in  )
      call ClosBF ( lu_out )

!     and clean up radcor data structure (if desired)
!     -----------------------------------------------
      if ( clean_up ) then
         call Prep_Clean ( Prep, Obs, stat )
         if (  stat .ne. 0 ) then
            call WPErr ( MyName, ErrStat ( 'Prep_Clean', stat ))
            return
         end if
      end if

!     All is well
!     -----------
      stat = 0

      return
      end subroutine Put_Raobs_Tag

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Obs_Reorder --- Reorder prep data structure
!
! !INTERFACE:
      subroutine Obs_Reorder ( Prep, Obs, reverse )
!
! !USES
      use m_RadData,  only : Rad_Reorder, radcor_profiles
      implicit NONE
!
! !INPUT PARAMETERS:
      logical, optional,        intent (in)    ::
     .   reverse              ! = .true. to reorder in reverse.
!                             !   Default: reverse = .false.
! !INPUT/OUTPUT PARAMETERS:
      type ( prep_info       ), intent (inout) ::
     .   Prep                 ! Prep data structure
      type ( radcor_profiles ), intent (inout) ::
     .   Obs                  ! data structure
!
! !DESCRIPTION:
!     This routine reorders the mass compoment of the radcor and the
!     prep data structure except the sorting indices (i.e. the
!     components Indx in the radcor data structure)
!
! !REVISION HISTORY:
!     21Jul2003  C. Redder  Original code
!     01Dec2003  C. Redder  Removed the component, QM, from the data
!                           structure, prep_info.
!     20Feb2004  C. Redder  Added the component, P_QM and ObQM, to the
!                           sub-data structure, prep_vector, in the
!                           data structure, prep_info.
!EOP
!-------------------------------------------------------------------------

      integer :: NObs, iOb
      logical :: reverse_
      integer, dimension (:), pointer :: Indx

      call Rad_Reorder ( Obs, reverse )

      NObs     =  Obs % Vector % NObs
      reverse_ = .false.
      if ( present ( reverse )) reverse_ = reverse

      Indx     => Obs % Vector % Indx
      if ( reverse_ ) then
         Indx ( Indx ( : NObs )) = (/( iOb, iOb = 1, NObs )/)
      end if

      Prep % Vector % RepType  ( 1 : NObs ) =
     .   Prep % Vector % RepType  ((/( Indx ( iOb ), iOb = 1, NObs )/))
      Prep % Vector % P_QM     ( 1 : NObs ) =
     .   Prep % Vector % P_QM     ((/( Indx ( iOb ), iOb = 1, NObs )/))
      Prep % Vector % ObQM     ( 1 : NObs ) =
     .   Prep % Vector % ObQM     ((/( Indx ( iOb ), iOb = 1, NObs )/))
      Prep % Vector % P_PC     ( 1 : NObs ) =
     .   Prep % Vector % P_PC     ((/( Indx ( iOb ), iOb = 1, NObs )/))
      Prep % Vector % ObPC     ( 1 : NObs ) =
     .   Prep % Vector % ObPC     ((/( Indx ( iOb ), iOb = 1, NObs )/))
      Prep % Vector % QC_Local ( 1 : NObs ) =
     .   Prep % Vector % QC_Local ((/( Indx ( iOb ), iOb = 1, NObs )/))
      Prep % Vector % Cat      ( 1 : NObs ) =
     .   Prep % Vector % Cat      ((/( Indx ( iOb ), iOb = 1, NObs )/))
      Prep % Vector % T_VIRTMP ( 1 : NObs ) =
     .   Prep % Vector % T_VIRTMP ((/( Indx ( iOb ), iOb = 1, NObs )/))
      Prep % Vector % H_VIRTMP ( 1 : NObs ) =
     .   Prep % Vector % H_VIRTMP ((/( Indx ( iOb ), iOb = 1, NObs )/))

      if ( reverse_ ) then
         Indx ( Indx ( : NObs )) = (/( iOb, iOb = 1, NObs )/)
      end if

      return
      end subroutine Obs_Reorder

!....................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Init_Options () --- Initialize option flags for reading/writing prep buffer files
!
! !DESCRIPTION:
!     The routine initializes the contents of the data structure,
!     prep_options.
!
! !INTERFACE:
      subroutine Init_Options ( this )
      implicit NONE
!
! !OUTPUT PARAMETERS:
      type ( prep_options ), intent (out) :: this
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     20Apr2004  C. Redder  Origional code
!     16Jun2004  C. Redder  Added the component, write_drift, to the
!                           type, prep_options.
!
! EOP
!-------------------------------------------------------------------------

      this % get_latest_QM    = .false.
      this % reverse_RadCor   = .false.
      this % write_drift      = .false.
      this % reject_OIQC      = .false.
      this % LastProgName     = ' '
      this % ThisProgName     = ' '

      return
      end subroutine Init_Options

!....................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Copy_Options () --- Copy option flags for reading/writing prep buffer files
!
! !DESCRIPTION:
!     The routine copies the contents from the input data structure to
!     the output data structure if it is present.
!
! !INTERFACE:
      subroutine Copy_Options ( this_in, this_out )
      implicit   NONE
!
! !INPUT PARAMETERS:
      type ( prep_options ), intent (in)  ::
     .   this_in
!
! !OUTPUT PARAMETERS:
      type ( prep_options ), intent (out) ::
     .   this_out            ! Output data structure.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     20Apr2004  C. Redder  Origional code
!     16Jun2004  C. Redder  Added the component, write_drift, to the
!                           type, prep_options.
! EOP
!-------------------------------------------------------------------------

      this_out % LastProgName     = this_in % LastProgName
      this_out % ThisProgName     = this_in % ThisProgName
      this_out % get_latest_QM    = this_in % get_latest_QM
      this_out % reverse_RadCor   = this_in % reverse_RadCor
      this_out % write_drift      = this_in % write_drift
      this_out % reject_OIQC      = this_in % reject_OIQC

      return
      end subroutine Copy_Options

!....................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Prep_Init () --- Initializes the data structures, prep_info and radcor
!
! !DESCRIPTION:
!     This routine initializes the data structures, prep_info and radcor.
!
! !INTERFACE:
      subroutine Prep_Init ( NSondes, NTObs, Prep, Obs, stat )
      use m_AdvError, only : WPErr, ErrStat, Alloc, ItoA
      use m_RadData,  only : Rad_Init, radcor_profiles
      use m_RadLists, only : MetaStr_Raobs
!
! !INPUT PARAMETERS:
      implicit   NONE
      integer,                  intent (in)    ::
     .   NSondes,             ! Number of soundings
     .   NTObs                ! Total number of mass observations
!
! !INPUT/OUTPUT PARAMETERS:
      type ( prep_info ),       intent (inout) ::
     .   Prep                 ! Data structure for info on prep buffer
                              !   file
      type ( radcor_profiles ), intent (inout) ::
     .   Obs                  ! Radcor observation data structure
!
! !OUTPUT PARAMETER:
      integer,                  intent (out)   ::
     .   stat                 ! Return status from allocation statements
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Origional code
!     12Nov2003  C. Redder   Replaced the data sturcture, radcor, with
!                            radcor_profiles.
!     20Feb2004  C. Redder   Added the component, P_QM and ObQM, to the
!                            sub-data structure, prep_vector, in the
!                            data structure, prep_info.
!     26Apr2004  C. Redder   Added the components, P_PC and ObPC, to
!                            the data structure, prep_vector
!     14Jun2004  C. Redder   Fixed index bug for the array,
!                            qcx_names_file in the data stucture,
!                            radcor_profiles
!     21Nov2006  C. Redder   Change modules for accessing MetaStr_Raobs
!                            from m_RadData to m_RadLists.
!     21Jul2007  C. Redder   Added the component, Cat to the data
!                            structure, prep_vector.
! EOP
!-------------------------------------------------------------------------

      character ( len = * ), parameter ::
     .           MyName = MyModule // '::Prep_Init'
      integer, parameter ::
     .   NVar     = MassNVar,
     .   NMne_Max = MassNObMne,
     .   LEvn     = len ( MassObStr )
      integer :: NLev, iVar, iBegOld, iEndOld, iBegNew, iEndNew,
     .          iBegPrev, iEndPrev, LMne, NBlks, NMne, iCh, iList
      logical :: write_data ( NMne_Max ), found
      character ( len = NVar * ( LEvn + 1 ) + 1 ) :: PrevMne
      character ( len = LEvn ) :: OldEvn

!     Initialize radcor data structure
!     --------------------------------
      call Rad_Init  ( NSondes, NTObs, nkx_file, nqcx_file, Obs, stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Rad_Init' ))
         return

      end if

!     Set lists for the prep buffer file
!     ----------------------------------
      Obs % Lists %  kx_names_file ( : nkx_file  ) = kx_names_file
      Obs % Lists %  kx_meta_file  ( : nkx_file  ) = ' '
      do iList = 1, kx_RaobListSz
         Obs % Lists %  kx_meta_file  ( kx_RaobList ( iList ))
     .       = MetaStr_Raobs
      end do
      Obs % Lists % qcx_names_file ( : nqcx_file ) = qcx_names_file

!     Allocate memory for prep info
!     -----------------------------
      allocate ( Prep % Vector % RepType  ( NTObs ),
     .           Prep % Vector % P_QM     ( NTObs ),
     .           Prep % Vector % ObQM     ( NTObs ),
     .           Prep % Vector % P_PC     ( NTObs ),
     .           Prep % Vector % ObPC     ( NTObs ),
     .           Prep % Vector % QC_Local ( NTObs ),
     .           Prep % Vector % Cat      ( NTObs ),
     .           Prep % Vector % T_VIRTMP ( NTObs ),
     .           Prep % Vector % H_VIRTMP ( NTObs ),
     .           stat = stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName,
     .                Alloc ( stat, 'Prep%Vector%RepType,'
     .                           // 'Prep%Vector%P_QM,'
     .                           // 'Prep%Vector%ObQM,'
     .                           // 'Prep%Vector%P_PC,'
     .                           // 'Prep%Vector%ObPC,'
     .                           // 'Prep%Vector%QC_Local,'
     .                           // 'Prep%Vector%Cat,'
     .                           // 'Prep%Vector%T_VIRTMP,'
     .                           // 'Prep%Vector%H_VIRTMP',
     .                         NTObs ))
         return
      end if

!     Initialize program code
!     -----------------------
      Prep % LastPCode   =  0
      Prep % P_PC_Max    =  0
      Prep % ObPC_Max    =  0

!     ... and flag for drift data availability
!     ----------------------------------------
      Prep % drift_avail = .true.

!     Edit event strings so that the same data is not written twice
!     -------------------------------------------------------------
      Prep % WrMne = ' '
      PrevMne   = ' '
      iBegPrev  = 2
      do iVar   = 1, NVar
         OldEvn =  MassObStr ( iVar )

!        Find the beginning of first mnemonic in old event string
!        --------------------------------------------------------
         iBegOld    =  verify ( OldEvn, ' ' )
         if ( iBegOld .eq. 0 ) iBegOld = LEvn + 1
         iBegNew    =  1
         NMne       =  0
         do iCh = 1, LEvn

!           Exit loop if no more mnemonics are in old event string
!           ------------------------------------------------------
            if ( iBegOld .gt. LEvn ) exit

!           Find the end of the current mnemonic
!           ------------------------------------
            LMne    = scan ( OldEvn ( iBegOld : ), ' ' ) - 1
            if ( LMne .lt. 0 ) LMne = LEvn - iBegOld + 1
            iEndOld = iBegOld + LMne - 1
            NMne    = NMne    + 1

!           Search for current mnemonic in list
!           of previously encountered mneumonics
!           ------------------------------------
            found = index ( PrevMne,
     .               ' ' // OldEvn ( iBegOld : iEndOld ) // ' ' )
     .            .gt. 0

!           If not found, then add current mnemonic to ...
!           ----------------------------------------------
            if ( .not. found .and. NMne .le. NMne_Max ) then

!              ... new event string
!              --------------------
               iEndNew  = min ( iBegNew + LMne, LEvn )
               Prep % WrMne ( iVar, NMne )
     .                  = OldEvn ( iBegOld : iEndOld )
               iBegNew  = iBegNew  + LMne + 1

!              ... and list of previously encountered mnemonics
!              ------------------------------------------------
               iEndPrev = iBegPrev + LMne
               PrevMne ( iBegPrev : iEndPrev )
     .            = OldEvn ( iBegOld : iEndOld )
               iBegPrev = iBegPrev + LMne + 1
            end if

!           Point to the beginning of the next mnemonic
!           -------------------------------------------
            NBlks = verify ( OldEvn ( iEndOld + 1 : ), ' ' ) - 1
            iBegOld = iBegOld + LMne + NBlks
            if ( NBlks .lt. 0 ) iBegOld = LEvn + 1
         end do

!        If the number of mneumonics does not exceed the maximum
!        -------------------------------------------------------
         if ( NMne .gt. NMne_Max ) then

!           ... return with error status
!           ----------------------------
            call WPErr ( MyName,
     .                 'The number of mnemonics (= '
     .              //  trim ( ItoA ( NMne ))
     .              // ') exceeds the allowed maximum (= '
     .              //  trim ( ItoA ( NMne_Max ))
     .              // ')')
            stat = Index_Error
            return
         end if
      end do
      stat = 0

      return
      end subroutine Prep_Init
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Prep_Clean () --- Deallocates the data structures, prep_info and radcor
!
! !DESCRIPTION:
!     This routine deallocates the data structures, prep_info and radcor.
!
! !INTERFACE:
      subroutine Prep_Clean ( Prep, Obs, stat )
      use m_AdvError, only : WPErr, Dealloc, ErrStat
      use m_RadData,  only : Rad_Clean, radcor_profiles
!
! !INPUT PARAMETERS:
      implicit   NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( prep_info ),       intent (inout) ::
     .   Prep                 ! Data structure for info on prep buffer file
      type ( radcor_profiles ), intent (inout) ::
     .   Obs                  ! Radcor observation data structure
!
! !INPUT PARAMETER:
      integer,                  intent (out), optional ::
     .   stat                 ! Return status from allocation statements
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Origional code
!     09Jul2003  C. Redder   Fixed bug by initializing istat to 0
!     12Nov2003  C. Redder   Replaced the data sturcture, radcor, with
!                            radcor_profiles.
!     20Feb2004  C. Redder   Added the component, P_QM and ObQM, to the
!                            sub-data structure, prep_vector, in the
!                            data structure, prep_info.
!     26Apr2004  C. Redder   Added the components, P_PC and ObPC to
!                            the data structure, prep_vector.
!     21Jul2007  C. Redder   Added the component, Cat, to the data
!                            structure, prep_vector.
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .           MyName = MyModule // '::Prep_Clean'
      integer :: istat
      logical :: check_stat

      check_stat = present ( stat )
      istat      = 0

!     Clean up radcor data structure
!     ------------------------------
      call Rad_Clean ( Obs, stat )
      if ( check_stat ) then
         if ( stat .ne. 0 ) then
            call WPErr ( MyName, ErrStat ( 'Rad_Init' ))
            return

         end if
      end if

!     ... and prep_info data structure
!     --------------------------------
      deallocate ( Prep % Vector % RepType,
     .             Prep % Vector % P_QM,
     .             Prep % Vector % ObQM,
     .             Prep % Vector % P_PC,
     .             Prep % Vector % ObPC,
     .             Prep % Vector % QC_Local,
     .             Prep % Vector % Cat,
     .             Prep % Vector % T_VIRTMP,
     .             Prep % Vector % H_VIRTMP,
     .             stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call WPErr ( MyName,
     .                Dealloc ( istat, 'Prep%Vector%RepType,'
     .                              // 'Prep%Vector%P_QM,'
     .                              // 'Prep%Vector%ObQM,'
     .                              // 'Prep%Vector%P_PC,'
     .                              // 'Prep%Vector%ObPC,'
     .                              // 'Prep%Vector%QC_Local,'
     .                              // 'Prep%Vector%Cat,'
     .                              // 'Prep%Vector%T_VIRTMP,'
     .                              // 'Prep%Vector%H_VIRTMP'
     .                            ) // '\W' )
         stat = Dealloc_Error
         return
      end if
      Prep % WrMne       = ' '

      Prep % LastPCode   =  0
      Prep % P_PC_Max    =  0
      Prep % ObPC_Max    =  0
      Prep % drift_avail = .false.
      if ( check_stat ) stat = 0

      return
      end subroutine Prep_Clean
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Init_pBufr () --- Initialize data structure
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Init_pBufr ( Mass, Wind )
!
! !INPUT/OUTPUT PARAMETERS:
      implicit   NONE
      type ( pbufr_mass ),   intent (inout),  optional ::
     .   Mass              ! Input local meta values
      type ( pbufr_wind ),   intent (inout),  optional ::
     .   Wind              ! Input local meta values
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     13Aug2003  C. Redder   Original code
! EOP
!-------------------------------------------------------------------------

      if ( present ( Mass )) then
         Mass % loaded = .false.
      end if
      if ( present ( Wind )) then
         Wind % loaded = .false.
      end if
      return
      end subroutine Init_pBufr
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Prep2File () --- Copy contents of prep data to data structure pbufr_file
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Prep2File ( Prep, File )
!
! !INPUT PARAMETERS:
      implicit   NONE
      type ( prep_info ),    intent (in)    ::
     .   Prep              ! Input local meta values
!
! !INPUT/OUTPUT PARAMETERS:
      type ( pbufr_file ),   intent (inout) ::
     .   File              ! Output local meta values
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     03Sep2003  C. Redder   Original code
! EOP
!.................................................................

      File % LastPCode = Prep % LastPCode
      File % CQC_PCode = Prep % CQC_PCode
      File % Rad_PCode = Prep % Rad_PCode
      File %  VT_PCode = Prep %  VT_PCode
      File % WrMne     = Prep % WrMne

      return
      end subroutine Prep2File
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: File2File () --- Copy contents of file data local to this module
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine File2File ( File1, File2 )
!
! !INPUT PARAMETERS:
      implicit   NONE
      type ( pbufr_file ),   intent (in)  ::
     .   File1             ! Input local data structure
!
! !OUTPUT PARAMETERS:
      type ( pbufr_file ),   intent (out) ::
     .   File2             ! Output local data structure
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     21Jul2007  C. Redder   Original code
! EOP
!.................................................................

      File2 % LastPCode = File1 % LastPCode
      File2 % ThisPCode = File1 % ThisPCode
      File2 % CQC_PCode = File1 % CQC_PCode
      File2 % Rad_PCode = File1 % Rad_PCode
      File2 %  VT_PCode = File1 %  VT_PCode
      File2 % WrMne     = File1 % WrMne

      return
      end subroutine File2File
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Meta2Meta () --- Copy contents of meta data local to this module
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Meta2Meta ( Meta1, Meta2 )
!
! !INPUT PARAMETERS:
      implicit   NONE
      type ( pbufr_meta ),   intent (in)  ::
     .   Meta1             ! Input local meta values
!
! !OUTPUT PARAMETERS:
      type ( pbufr_meta ),   intent (out) ::
     .   Meta2             ! Output local meta values
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Original code
!     11Mar2004  C. Redder   Added the component, ProcN, to the data
!                            structure, pbufr_meta
! EOP
!-------------------------------------------------------------------------

      Meta2 % StnID    = Meta1 % StnID
      Meta2 % Lon      = Meta1 % Lon
      Meta2 % Lat      = Meta1 % Lat
      Meta2 % Elev     = Meta1 % Elev
      Meta2 %  SeqN    = Meta1 %  SeqN
      Meta2 % ProcN    = Meta1 % ProcN
      Meta2 % IType    = Meta1 % IType
      Meta2 % RType    = Meta1 % RType
      Meta2 % ObTime   = Meta1 % ObTime
      Meta2 % RObTime  = Meta1 % RObTime
      Meta2 % TCor     = Meta1 % TCor
      Meta2 % RadCode  = Meta1 % RadCode

      return
      end subroutine Meta2Meta

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Save_PrepMeta () --- Save meta values for a sounding in the data structures, prep_info and radcor_meta
!
! !DESCRIPTION:
!     This routine saves the meta values for one sounding in the data
!     structure, radcor_meta.  The meta value are extracted from the
!     data structure local to this module.
!
! !INTERFACE:
      subroutine Save_PrepMeta ( Meta, iSonde, Prep, RadMeta )
      use m_RadData, only : radcor_meta, LTHist_IMiss, ETHist_IMiss
!
! !INPUT PARAMETERS:
      implicit   NONE
      type ( pbufr_meta ),  intent (in) ::
     .   Meta             ! Local meta values
      integer,              intent (in) ::
     .   iSonde           ! Array index number for the selected sonde
!
! !INPUT/OUTPUT PARAMETERS:
      type ( prep_info   ), intent (inout), optional ::
     .   Prep             ! Information about the prep buffer file
      type ( radcor_meta ), intent (inout), optional ::
     .   RadMeta          ! Meta data values in the target data-structure
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Original code
!     04Aug2003  C. Redder   Changed input/output argument, NSondes, to
!                            the input argument, iSonde.
!     11Feb2004  C. Redder   Added the components, kx and kx_file, to
!                            the data structure, radcor_meta.
! EOP
!-------------------------------------------------------------------------
      logical :: set_radcor_meta, set_prep_info
      integer :: RType

      set_prep_info   = present ( Prep    )
      set_radcor_meta = present ( RadMeta )

      RType = Meta % RType
      if ( set_radcor_meta ) then
         RadMeta    % StnID    ( iSonde ) = Meta % StnID
         RadMeta    % Lat      ( iSonde ) = Meta % Lat
         RadMeta    % Lon      ( iSonde ) = Meta % Lon
         RadMeta    % Elev     ( iSonde ) = Meta % Elev
         if ( RType .ge. RType_Min .and.
     .        RType .le. RType_Max ) then
            RadMeta %  kx      ( iSonde ) = RType2kx      ( RType )
            RadMeta %  kx_file ( iSonde ) = RType2kx_file ( RType )
         else
            RadMeta %  kx      ( iSonde ) = kx_Def        ( 1 )
            RadMeta %  kx_file ( iSonde ) = kx_file_Def   ( 1 )
         end if
         RadMeta    % rkx      ( iSonde ) = Meta % IType
         RadMeta    %  ObTime  ( iSonde ) = Meta % RObTime
         RadMeta    %   LTime  ( iSonde ) = Meta %  ObTime
         RadMeta    %  LTHist  ( iSonde ) = LTHist_IMiss
         RadMeta    %  ETHist  ( iSonde ) = ETHist_IMiss
         RadMeta    % RadCode  ( iSonde ) = Meta % RadCode
      end if

      return
      end subroutine Save_PrepMeta
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Get_PrepMeta () --- Get meta data for a raob sounding from Prep buffer file
!
! !DESCRIPTION:
!     This routine extracts the meta-data from the buffer file and stores
!     it into the data stucture local to this module.
!
! !INTERFACE:
      subroutine Get_PrepMeta ( lu, Meta, accept )
      use m_RadData, only : RCode_AMiss => RadCode_AMiss
!
! !INPUT PARAMETERS:
      implicit   NONE
      integer,             intent (in)  ::
     .   lu                ! logical unit number for Prep buffer file
!
! !OUTPUT PARAMETERS:
      type ( pbufr_meta ), intent (out) ::
     .   Meta              ! station meta data
      logical,             intent (out) ::
     .   accept            ! = .true. to accept meta data
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Origional code
!     05Sep2003  C. Redder   Added code to accept soundings based on the
!                            report type and office note 29 report type
!
! EOP
!-------------------------------------------------------------------------

      real ( kind = BKind )  :: HDR ( 11 ), X
      logical :: Missing ( 11 )
      integer :: TypeMax, NVal, RType
      logical :: get_prep
      character (len=*), parameter ::
     .   HDStr  = 'SID XOB YOB DHR ELV ITP TYP T29 SIRC SQN PROCN',
     .!   HDStr  = 'SID XOB YOB DHR ELV ITP TYP T29 SIRC SQN',
     .   HDStr2 = 'RPT TCOR'
      character ( len = LenID ) :: CStnID
      equivalence ( CStnID, HDR )

!     Get the maximum number for the instrument type
!     ----------------------------------------------
      call Raob_Types ( TypeMax )

!     Get header information including ...
!     ------------------------------------
      call UFBINT ( lu, HDR, 11, 1, NVal, HDStr )
      call Test_BMiss ( HDR, Missing )

      accept       = .true.
      Meta % StnID =  CStnID                  ! ... WMO station ID
      if ( .not. Missing (  3 )) then         ! ... latitude
         Meta % Lat       = HDR (  3 )        ! required info
      else
         accept = .false.
      end if
      if ( .not. Missing (  2 )) then         ! ... longitude
         Meta % Lon       = HDR (  2 )        ! required info
         if ( Meta % Lon .gt. 180.0 )
     .        Meta % Lon  = Meta % Lon - 360.0
      else
         accept = .false.
      end if
      if ( .not. Missing (  5 )) then         ! ... elevation
         Meta % Elev      = HDR (  5 )        ! required info
      else
         accept = .false.
      end if
      if ( .not. Missing (  4 )) then         ! ... sampling hr
         Meta % ObTime    = nint ( HDR (  4 ) * 3600.0 )
      else                                    ! required info
         accept = .false.
      end if
      if ( .not. Missing (  6 )) then         ! ... instr type
         Meta % IType     = nint ( HDR (  6 ))
      else
         Meta % IType     = TypeMax - 1
      end if
      if ( .not. Missing (  7 )) then         ! ... report type
         Meta % RType     = nint ( HDR (  7 ))
      else                                    ! required info
         accept = .false.
      end if
      if ( .not. Missing (  9 )) then         ! ... report type
         Meta % RadCode   = nint ( HDR (  9 ))
      else
         Meta % RadCode   = RCode_AMiss
      end if

      if ( .not. Missing ( 10 )) then         ! ... sequence number
         Meta % SeqN      = nint ( HDR ( 10 ))
      else
         accept = .false.
      end if

      if ( .not. Missing  ( 11 )) then        ! ... processor number
         Meta % ProcN     = nint ( HDR ( 11 ))
      else
         Meta % ProcN     = 0
      end if

!     Get reported ob time and its associated correction code
!     -------------------------------------------------------
      call UFBINT ( lu, HDR, 2, 1, NVal, HDStr2 )
      if ( NVal .lt. 2 ) then
         Meta % RObTime   = Meta % ObTime
         Meta % TCor      = 0
      else
         Meta % RObTime   = nint ( HDR ( 1 ) * 3600.0 ) ! Convert to seconds
         Meta % TCor      = nint ( HDR ( 2 ))
      end if

      if ( accept ) then
         RType   = nint ( HDR ( 7 ))
         accept  = mod ( RType, 100 ) .eq. 20 ! Accept only rawinsondes in
                                              ! ADPUPA reports.  Skip
      end if                                  ! dropsonde, pibal and
                                              ! "class sounding" reports.
      return
      end subroutine Get_PrepMeta

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Get_PrepMass () --- Get mass reports from Prep buffer file
!
! !DESCRIPTION:
!     This routine extracts a mass report from a Prep buffer file and
!     stores the data into the structure, Mass, local to this module.
!
! !INTERFACE:
      subroutine Get_PrepMass ( lu, NObs, Mass )
      use m_RadSort,  only : R2ManExp, IndexSet
      use m_RadData,  only : QC_Rejected, NPresDigits, AMiss, QC_OK,
     .                       kt_HumBufr => kt_Humidity, QC_NotTested
!
! !INPUT PARAMETERS:
      implicit   NONE
      integer,              intent (in)    ::
     .   lu               ! Logical unit number for Prep buffer file
!
! !INPUT/OUTPUT PARAMETERS:
      type ( pbufr_mass ),  intent (inout), optional ::
     .   Mass             ! Data structure for mass data
!
! !OUTPUT PARAMETERS:
      integer,              intent (out)   ::
     .   NObs             ! Number of observations
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Original code
!     05Aug2003  C. Redder   Added optional argument, Meta
!     13Aug2003  C. Redder   Major changes to the interface and internal
!                            coding to put data into a local data structure
!                            rather than radcor.
!     28Jan2004  C. Redder   Added code to extract the drift lat/lon
!     26Apr2004  C. Redder   Added code to retreive program codes
!     09Jun2004  C. Redder   Fixed compilation bug by changing the
!                            attribute from integer to real for internal
!                            variable, PC_Max.
!     01Sep2004  C. Redder   Condensed code that assigns QC to a single
!                            call to the routine, Check_PrepQM, which has
!                            a revised interface.
!     02Nov2006  C. Redder   Set the component kt_humidity in the
!                            variable Mass of type pbufr_mass.  Added
!                            code to convert humidity from specific
!                            humidity to dew point temperature.
!     07Dec2006  C. Redder   Renamed kt_HumRadcor to kt_HumBufr
!     22Jun2007  C. Redder   Added code to ensure that first guess
!                            humidity value does not become zero.
!                            Added code to reject all humidity obs
!                            if corresponding temperature is also rejected
!     20Jul2007  C. Redder   Split call to the routine, ConvertH_Prep,
!                            into two.  The first now performs a range
!                            check and the second a conversion to dew
!                            point.  This was done as part of a bug
!                            fix to the computation of air temperature
!                            from virtual temperature and vice versa.
!                            A additional call to the routine,
!                            ATtoVT_PrepMass was added to convert the
!                            temperature observations.
! EOP
!-------------------------------------------------------------------------

      type ( NCEP_radcor_Info  ) ::  RadInfo
      type ( NCEP_radcor_BInfo ) :: BRadInfo
      integer, parameter ::
     .   NVar     = MassNVar,
     .   NStr     = NVar,
     .   NMne     = MassNObMne,
     .   NFMne    = MassNFCMne,
     .   NDMne    = MassNDrMne
      integer :: NLev, NL, iLev, iiLev, iVar, kt_iVar, iMne,
     .           ProgCode, kt_out, QC_
      real    :: M, B, Elev, DLat_, DLon_, ET_, ObQM_, ObPC_,
     .           ObFC_, Obs_, PC_Max
      logical :: nobs_only, drift_avail, latest_QM, reject_OIQC,
     .           ObMissing_,  reverse_RadCor, local_QC_set,
     .           FCMissing_,
     .           P_Missing  ( NLev_Max ),
     .           ObMissing  ( NLev_Max ),
     .           FCMissing  ( NLev_Max ),
     .           PCMissing  ( NLev_Max ),
     .           T_VIRTMP   ( NLev_Max ),
     .           H_VIRTMP   ( NLev_Max )
      real ( kind = BKind ), target ::
     .    Bufr ( NMne,  NLev_Max ),
     .   FBufr ( NFMne, NLev_Max ),
     .   DBufr ( NDMne, NLev_Max ),
     .   BZCor        ( NLev_Max ),
     .   BTCor        ( NLev_Max )
      real,    dimension ( NLev_Max ) :: Obs,   DLat,  DLon, P, T, ET,
     .                                   ObQM,  P_QM,  TObs, Hum, HumFG,
     .                                   P_PC,  ObPC,  ZCor, TCor, ObFC
      integer, dimension ( NLev_Max ) :: Cat,   EIndx, P_m,  P_e,  Indx,
     .                                   HumQC
      character ( len = len(MassDrStr)),  parameter ::
     .   DrStr = MassDrStr
      character ( len = len(MassFCStr)) :: FCStr

!     Set flags to get optional output arguments
!     ------------------------------------------
      nobs_only = .not. present ( Mass )

!     Initial data type for humidity variable
!     ---------------------------------------
      Mass % kt_humidity = Masskt_List ( iVarHum )

!     Get category and number of levels
!     ---------------------------------
      call UFBINT ( lu, Bufr ( : 1, : ), 1, NLev_Max, NLev, 'CAT' )
      NLev = min ( NLev_Max, NLev )

!     7 - PREPBUFR data level category (local descriptor 0-01-194) defined as:
!               0 - Surface level (mass reports only)
!               1 - Mandatory level (upper-air profile reports)
!               2 - Significant temperature level (upper-air profile reports)
!               3 - Winds-by-pressure level (upper-air profile reports)
!               4 - Winds-by-height level (upper-air profile reports)
!               5 - Tropopause level (upper-air profile reports)
!               6 - Reports on a single level (e.g., aircraft, satellite-wind, surface wind, precipitable water retrievals, etc.) {default}
!               9 - Auxiliary levels generated via interpolation from spanning levels (upper-air profile reports)

!     Return only if NObs is desired
!     ------------------------------
      NObs = NLev * MassNVar

      if ( nobs_only ) return

!     Set options
!     -----------
      reverse_RadCor = Mass % Options % reverse_RadCor
      reject_OIQC    = Mass % Options % reject_OIQC

!     Set the program code.
!     ---------------------
      ProgCode = Mass % File % LastPCode

!     Convert category to integer format
!     ----------------------------------
      call B2DtoI1D   ( Bufr ( : 1, : NLev ), Cat )

!     Get pressure levels and their QM marks
!     --------------------------------------
      call Get_BObs    ( lu, Mass % File,
     .                       Mass % Options, NLev, Bufr )
      iMne = iMassMneQM
      call B2DtoR1D    ( Bufr ( iMne : iMne, : NLev ), P_QM      )
      iMne = iMassMnePC
      call B2DtoR1D    ( Bufr ( iMne : iMne, : NLev ), P_PC      )
      call Test_RMiss  ( P_PC              ( : NLev ), PCMissing )
      iMne = iMassMneOb
      call B2DtoR1D    ( Bufr ( iMne : iMne, : NLev ), P         )
      call Test_RMiss  ( P                 ( : NLev ), P_Missing )

!     Determine maximum value for the program code
!     --------------------------------------------
      PC_Max = 0.0
      do iLev = 1, NLev
         if ( .not. PCMissing ( iLev ))
     .      PC_Max = max ( PC_Max, P_PC ( iLev ))
      end do
      Mass % P_PC_Max = nint ( PC_Max )

!     Ensure that data are sorted by pressure level (descending order)
!     ----------------------------------------------------------------
      call R2ManExp   ( NPresDigits, P ( : NLev ), P_m, P_e )
      call IndexSet   ( Indx ( : NLev ))
      call Sort_byLev ( Indx ( : NLev ),           P_m, P_e )

!     Get the drift data (if available)
!     ---------------------------------
      call UFBINT  ( lu, DBufr, NDMne, NLev_Max, NL, DrStr )
      drift_avail =  NL .eq. NLev  ! Determine if drift data is available
      iMne = iMassMneDT
      call B2DtoR1D    ( DBufr ( iMne : iMne, : NL   ), ET   )
      iMne = iMassMneDLat
      call B2DtoR1D    ( DBufr ( iMne : iMne, : NL   ), DLat )
      iMne = iMassMneDLon
      call B2DtoR1D    ( DBufr ( iMne : iMne, : NL   ), DLon )

!     Perform the necessary calculations for data already obtained
!     ------------------------------------------------------------
      do iLev = 1, NLev
         DLat_         = DLat ( iLev )
         DLon_         = DLon ( iLev )
         ET_           = ET   ( iLev )
         DLat ( iLev ) = AMiss
         DLon ( iLev ) = AMiss
         ET   ( iLev ) = AMiss
         if ( drift_avail ) then
            DLat ( iLev ) = DLat_
            DLon ( iLev ) = DLon_
            ET   ( iLev ) = ET_ * 3600.0 ! Convert elapsed to seconds
         end if
      end do

!     Store non-observation values
!     ----------------------------
      Mass % NLev = NLev
      do iLev = 1, NLev
         iiLev = Indx     ( iLev )
         Mass % DLat      ( iLev ) = DLat      ( iiLev )
         Mass % DLon      ( iLev ) = DLon      ( iiLev )
         Mass % P         ( iLev ) = P         ( iiLev )
         Mass % P_m       ( iLev ) = P_m       ( iiLev )
         Mass % P_e       ( iLev ) = P_e       ( iiLev )
         Mass % Cat       ( iLev ) = Cat       ( iiLev )
         Mass % Indx      ( iLev ) =             iiLev
         Mass % ETime     ( iLev ) = ET        ( iiLev )
         Mass % P_QM      ( iLev ) = P_QM      ( iiLev )
         Mass % P_PC      ( iLev ) = P_PC      ( iiLev )
         Mass % P_Missing ( iLev ) = P_Missing ( iiLev )
      end do

!     Initialize some variables
!     -------------------------
      PC_Max = 0.0
      do iLev = 1, NLev
         ZCor ( iLev ) =  0.0
         TCor ( iLev ) =  0.0
      end do

!     For each variable ...
!     ---------------------
      do iVar = 1, NVar

!        Get the observations
!        --------------------
         if ( reverse_RadCor ) then
            call Get_BObs    ( lu, Mass % File,
     .                             Mass % Options,
     .                             NLev, Bufr, BRadInfo,
     .                             T_VIRTMP,   H_VIRTMP,
     .                             DataType = iVar )
         else
            call Get_BObs    ( lu, Mass % File,
     .                             Mass % Options,
     .                             NLev, Bufr,
     .                             T_VIRTMP =  T_VIRTMP,
     .                             H_VIRTMP =  H_VIRTMP,
     .                             DataType = iVar )
         end if

         iMne = iMassMneQM
         call B2DtoR1D   (  Bufr ( iMne : iMne, : NLev ), ObQM )
         iMne = iMassMnePC
         call B2DtoR1D   (  Bufr ( iMne : iMne, : NLev ), ObPC )
         call Test_RMiss (  ObPC              ( : NLev ), PCMissing )
         iMne = iMassMneOb
         call B2DtoR1D   (  Bufr ( iMne : iMne, : NLev ), Obs  )
         call Test_RMiss (  Obs               ( : NLev ), ObMissing )

!        ... and forecast value
!        ----------------------
         FCStr = MassFCStr ( iVar )
         call UFBINT ( lu, FBufr, NFMne, NLev_Max, NLev, FCStr )
         iMne = iMassMneFC
         call B2DtoR1D   ( FBufr ( iMne : iMne, : NLev ), ObFC )
         call Test_RMiss ( ObFC               ( : NLev ), FCMissing )

!        Determine maximum value for the program code
!        --------------------------------------------
         do iLev = 1, NLev
            if ( .not. PCMissing ( iLev ))
     .         PC_Max = max ( PC_Max, ObPC ( iLev ))
         end do

!        Save the mass data
!        ------------------
         M       = MassM ( iVar )
         B       = MassB ( iVar )
         do  iLev = 1, NLev
            iiLev = Indx ( iLev )

            ObQM_      = ObQM      ( iiLev )
            ObPC_      = ObPC      ( iiLev )
            ObFC_      = ObFC      ( iiLev )
            Obs_       = Obs       ( iiLev )
            ObMissing_ = ObMissing ( iiLev )
            FCMissing_ = FCMissing ( iiLev )

!           ... and convert the units of the obs (e.g. deg C to deg K)
!           ----------------------------------------------------------
            if ( .not. ObMissing_ )  Obs_  = M * Obs_  + B
            if ( .not. FCMissing_ )  ObFC_ = M * ObFC_ + B

!           ... and save the values
!           -----------------------
            Mass % ObQM      ( iVar, iLev ) = ObQM_
            Mass % ObPC      ( iVar, iLev ) = ObPC_
            Mass % ObFC      ( iVar, iLev ) = ObFC_
            Mass % Obs       ( iVar, iLev ) = Obs_
            Mass % ObMissing ( iVar, iLev ) = ObMissing_
            Mass % FCMissing ( iVar, iLev ) = FCMissing_
         end do
      end do

!     Set quality control flags
!     -------------------------
      call Check_PrepQM    ( Mass % P_Missing ( : NLev ),
     .                       Mass % P_QM,
     .                       Mass % ObMissing,
     .                       Mass % ObQM,
     .                       QM   = Mass % QM,
     .                       QC   = Mass % QC,
     .                       reject_OIQC = reject_OIQC,
     .                       local_QC    = local_QC_set )

!     Set quality control flags for internal use
!     ------------------------------------------
      if ( .not. local_QC_set ) then
         call Check_PrepQM ( Mass % P_Missing ( : NLev ),
     .                       Mass % P_QM,
     .                       Mass % ObMissing,
     .                       Mass % ObQM,
     .                       QC   = Mass % QC_Local )

!        Reject humidity obs if temperature obs is rejected
!        --------------------------------------------------
         do iLev = 1, NLev
            Mass % QC_Local ( iVarHum, iLev )
     .         = max ( Mass % QC_Local ( iVarHum,  iLev ),
     .                 Mass % QC_Local ( iVarTemp, iLev ))
         end do

      else
         do iVar = 1, NVar
         do iLev = 1, NLev
            Mass % QC_Local ( iVar, iLev ) = Mass % QC ( iVar, iLev )
         end do
         end do
      end if

!     Reject humidity obs if temperature obs is rejected
!     --------------------------------------------------
      do iLev = 1, NLev
         Mass % QC       ( iVarHum, iLev )
     .      = max ( Mass % QC       ( iVarHum,  iLev ),
     .              Mass % QC       ( iVarTemp, iLev ))
         Mass % QC_Local ( iVarHum, iLev )
     .      = max ( Mass % QC_Local ( iVarHum,  iLev ),
     .              Mass % QC_Local ( iVarTemp, iLev ))
      end do

!     Perform a range check for humidity observations
!     -----------------------------------------------
      kt_out = kt_HumBufr
      call ConvertH_Prep ( kt_out, Mass, HumQC = HumQC )
      do iLev = 1, NLev
         if ( HumQC ( iLev ) .eq. QC_NotTested ) cycle
         if ( HumQC ( iLev ) .eq. QC_Rejected  ) then
            Mass % QC       ( iVarHum, iLev ) = QC_Rejected
            Mass % QC_Local ( iVarHum, iLev ) = QC_Rejected
         end if
      end do

!     Save VIRTMP flags
!     -----------------
      do iLev = 1, NLev
         iiLev = Indx        ( iLev )
         Mass %  T_VIRTMP    ( iLev ) = T_VIRTMP ( iiLev )
         Mass %  H_VIRTMP    ( iLev ) = H_VIRTMP ( iiLev )
      end do

!     If necessary convert observed and ...
!     -------------------------------------
c      call ATtoVT_PrepMass ( Mass, T, toAT = .true. )
c      do iLev = 1, NLev
c         Mass % Obs  ( iVarTemp, iLev ) = T ( iLev )
c      end do

!     If necessary convert forecast virtual tempto air temp
!     -----------------------------------------------------
      call ATtoVT_PrepMass ( Mass, T, toAT = .true., fcst = .true. )
      do iLev = 1, NLev
         Mass % ObFC ( iVarTemp, iLev ) = T ( iLev )
      end do

!     Reverse NCEP's RadCor corrections (if desired)
!     ----------------------------------------------
      if ( reverse_RadCor ) then
         call BRad2Rad    ( Mass,    BRadInfo, RadInfo )
         call Undo_RadCor ( RadInfo, Mass   )
      end if

!     Ensure that the first guess humidity
!     values are semi-realistic (i.e. > 0)
!     ------------------------------------
      do iLev = 1, NLev
         if ( .not. Mass % FCMissing ( iVarHum,  iLev ))
     .      Mass % ObFC ( iVarHum, iLev )
     .          = max ( Mass % ObFC ( iVarHum, iLev ), HumMin )
      end do

!     Convert humidity to dew point with quality control checks
!     ---------------------------------------------------------
      kt_out = kt_HumBufr
      call ConvertH_Prep ( kt_out, Mass, Hum, HumFG )
      do iLev = 1, NLev
         QC_ = Mass % QC_Local ( iVarHum, iLev )
         if ( QC_ .ne. QC_Rejected  ) then
            Mass % Obs  ( iVarHum, iLev ) = Hum   ( iLev )
            Mass % ObFC ( iVarHum, iLev ) = HumFG ( iLev )
         end if
      end do
      Mass % kt_humidity =  kt_out

      Mass % ObPC_Max    =  nint ( PC_Max )

      Mass % loaded      = .true.
      Mass % drift_avail =  drift_avail

      return
      end subroutine Get_PrepMass

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: BRad2Rad () --- Post-processor for NCEP's radcor info.
!
! !DESCRIPTION:
!     This routine converts NCEP's radcor info from buffer to native
!     reals.
!
! !INTERFACE:
      subroutine BRad2Rad ( Mass, BRadInfo, RadInfo )
!
! !INPUT PARAMETERS:
      implicit   NONE
      type ( pbufr_mass ),        intent (in) ::
     .   Mass                   ! Data structure for mass data
      type ( NCEP_radcor_BInfo ), intent (in) ::
     .   BRadInfo               ! Radcor info in buffer format
!
! !OUTPUT PARAMETERS:
      type ( NCEP_radcor_Info  ), intent (out) ::
     .    RadInfo               ! ... native format
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17May2004  C. Redder   Original code
! EOP
!-------------------------------------------------------------------------

      integer :: NLev, iLev, iiLev
      real :: M_Temp, M_Hum, M_Z
      real, dimension ( NLev_Max ) :: ZCor_RADCOR, TCor_RADCOR,
     .                                HCor_VIRTMP,
     .                                TCor_preVT, HCor_after,
     .                                TCor, ZCor, HCor, TCor_bef,
     .                                HCor_aft
      NLev = Mass % NLev
      call B1DtoR1D   ( BRadInfo % ZCor_RADCOR ( : NLev ), ZCor_RADCOR )
      call B1DtoR1D   ( BRadInfo % TCor_RADCOR ( : NLev ), TCor_RADCOR )
      call B1DtoR1D   ( BRadInfo % TCor_preVT  ( : NLev ), TCor_preVT  )
      call B1DtoR1D   ( BRadInfo % HCor_VIRTMP ( : NLev ), HCor_VIRTMP )
      call B1DtoR1D   ( BRadInfo % HCor_after  ( : NLev ), HCor_after  )

      M_Z    = MassM ( iVarHeight )
      M_Temp = MassM ( iVarTemp   )
      M_Hum  = MassM ( iVarHum    )
      do iLev = 1, NLev
         iiLev = Mass % Indx   ( iLev )
         RadInfo % ZCor_RADCOR ( iLev ) = ZCor_RADCOR ( iiLev ) * M_Z
         RadInfo % TCor_RADCOR ( iLev ) = TCor_RADCOR ( iiLev ) * M_Temp
         RadInfo % TCor_preVT  ( iLev ) = TCor_preVT  ( iiLev ) * M_Temp
         RadInfo % HCor_VIRTMP ( iLev ) = HCor_VIRTMP ( iiLev ) * M_Hum
         RadInfo % HCor_after  ( iLev ) = HCor_after  ( iiLev ) * M_Hum

      end do

      return
      end subroutine BRad2Rad
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Undo_RadCor () --- Undo the corrections applied by radcor
!
! !DESCRIPTION:
!     This routine reverse the corrections applied by radcor.
!
! !INTERFACE:
      subroutine Undo_RadCor ( RadInfo, Mass )
      use m_convert,  only : ConvertH, CheckH, Dew_Point
!
! !INPUT PARAMETERS:
      implicit   NONE
      type ( NCEP_radcor_Info ),  intent (in)     ::
     .   RadInfo                ! Radcor info in buffer format
!
! !INPUT/OUTPUT PARAMETERS:
      type ( pbufr_mass ),        intent (inout)  ::
     .   Mass                   ! Data structure for mass data
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     03May2004  C. Redder   Original code
! EOP
!-------------------------------------------------------------------------

      real, parameter :: TCor_Thresh = 10.0 * T0 * Tol
      integer :: iLev, NLev, Var, iHVar, iList, List1Sz, List2Sz,
     .           kt_in, kt_out
      logical :: Missing_Z, Missing_T, Missing_H,
     .           valid, recompute_Hum, in_range
      real ::  HCor_Thresh, TCor_RADCOR_, TCor_diff, HCor_after_
      real,    dimension ( NLev_Max ) :: P, Hum,
     .                                   T_before, T_after, H_after
      integer, dimension ( NLev_Max ) :: List1, List2
      logical, dimension ( NLev_Max ) :: InRange

!     Adjust height and temperatures
!     ------------------------------
      NLev = Mass % NLev
      do iLev = 1, NLev
         Missing_Z = Mass     % ObMissing     ( iVarHeight, iLev )
         Missing_T = Mass     % ObMissing     ( iVarTemp,   iLev )
         Missing_H = Mass     % ObMissing     ( iVarHum,    iLev )
         if ( .not. Missing_Z )
     .      Mass % Obs                        ( iVarHeight, iLev )
     .             = Mass     % Obs           ( iVarHeight, iLev )
     .             + RadInfo  % ZCor_RADCOR               ( iLev )
         if ( .not. Missing_T ) then
            T_after  ( iLev ) = Mass    % Obs ( iVarTemp,   iLev )
            T_before ( iLev ) = T_after                   ( iLev )
     .                        + RadInfo % TCor_RADCOR     ( iLev )
            Mass % Obs                        ( iVarTemp,   iLev )
     .                        = T_before                  ( iLev )
         else
            T_before ( iLev ) = Mass    % Obs ( iVarTemp,   iLev )
            T_after  ( iLev ) = Mass    % Obs ( iVarTemp,   iLev )
         end if
         if ( .not. Missing_H ) then
            H_after  ( iLev ) = Mass    % Obs ( iVarHum,    iLev )
            Mass % Obs                        ( iVarHum,    iLev )
     .                        = H_after                   ( iLev )
     .                        + RadInfo % HCor_VIRTMP     ( iLev )
         else
            H_after  ( iLev ) = Mass    % Obs ( iVarHum,    iLev )
         end if

      end do
      if ( iVarHum .le. 0 ) return

!     Generate list of humidity obs that must be adjusted
!     ---------------------------------------------------
      List1Sz = 0
      do iLev = 1, NLev
         P   ( iLev ) = Mass % P                       ( iLev )
         Hum ( iLev ) = H_after                        ( iLev )
         valid =     .not. Mass % P_Missing            ( iLev ) .and.
     .               .not. Mass % ObMissing ( iVarTemp,  iLev ) .and.
     .               .not. Mass % ObMissing ( iVarHum,   iLev )
         TCor_RADCOR_  = abs ( RadInfo % TCor_RADCOR   ( iLev ))
         TCor_diff     = abs ( RadInfo % TCor_RADCOR   ( iLev )
     .                       - RadInfo % TCor_preVT    ( iLev ))
         HCor_after_   = abs ( RadInfo % HCor_after    ( iLev ))
         HCor_Thresh   = 10.0 * Tol * Hum              ( iLev )
         recompute_Hum = valid .and. Mass % H_VIRTMP   ( iLev ) .and.
     .                   TCor_RADCOR_ .gt. TCor_Thresh  .and.
     .                 ( TCor_diff    .gt. TCor_Thresh  .or.
     .                   HCor_after_  .gt. HCor_Thresh )

         if ( recompute_Hum ) then
            List1Sz = List1Sz + 1
            List1 ( List1Sz ) = iLev
         end if
      end do
      if ( List1Sz .le. 0 ) return

!     Check humidity ...
!     ------------------
      kt_in  =  Mass % kt_humidity
      call CheckH   ( List1 ( : List1Sz ), P, T_after,
     .                kt_in    = kt_in,
     .                Obs      = Hum,
     .                Valid    = InRange,
     .                SVP_Eqtn = SVP_Eqtn )

!     ... and update the list
!     -----------------------
      List2Sz  = 0
      do iList = 1, List1Sz
         iLev  = List1 ( iList )
         if ( InRange ( iLev )) then
            List2Sz = List2Sz + 1
            List2 ( List2Sz ) = iLev
         end if
      end do

!     ... and convert to dew point
!     ----------------------------
      kt_out =  Dew_Point
      call ConvertH ( List2 ( : List2Sz ), P, T_before,
     .                kt_in    = kt_in,
     .                kt_out   = kt_out,
     .                Obs      = Hum,
     .                SVP_Eqtn = SVP_Eqtn )

!     ... adjust by conserving dew point depression
!     ---------------------------------------------
      do iList = 1, List2Sz
         iLev = List2 ( iList )
         Hum ( iLev ) = T_after  ( iLev )
     .                + Hum      ( iLev )
     .                - T_before ( iLev )
      end do

!     ... and then convert back to the original variable
!     --------------------------------------------------
      call ConvertH ( List2 ( : List2Sz ), P, T_after,
     .                kt_in    = kt_out,
     .                kt_out   = kt_in,
     .                Obs      = Hum,
     .                SVP_Eqtn = SVP_Eqtn )

!     Save the adjusted humidity values.
!     ----------------------------------
      do iList = 1, List2Sz
         iLev = List2 ( iList )
         Mass % Obs  ( iVarHum, iLev ) = Hum ( iLev )
      end do

      return
      end subroutine Undo_RadCor

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: ConvertH_Prep () --- Convert humidity variables
!
! !DESCRIPTION:
!     The routine converts humidity variables according to the formulae for
!     the saturation vapor pressure used in prep files..
!
! !INTERFACE:
      subroutine ConvertH_Prep ( kt_out, Mass, Hum, HumFG, HumQC )

      use m_RadData,  only : QC_OK, QC_Rejected, QC_NotTested
      use m_convert,  only : ConvertH, CheckH, Dew_Point
!
! !INPUT PARAMETERS:
      implicit   NONE
      integer,             intent (in)  ::
     .   kt_out          ! Data type index of output humidity variable
      type ( pbufr_mass ), intent (in)  ::
     .   Mass            ! Data structure for mass data
!
! !OUTPUT PARAMETERS:
      real,    optional,   intent (out) ::
     .   Hum     (:)     ! Output humidity
      real,    optional,   intent (out) ::
     .   HumFG   (:)     ! Output first guess humidity
      integer, optional,   intent (out) ::
     .   HumQC   (:)     ! QC of humidity variable.  If present, range
                         !   checks and checks for missing data are performed
!
! !INPUT/OUTPUT PARAMETERS:
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     06Nov2006  C. Redder   Original code
!     22Dec2006  C. Redder   Fixed bug in generating first guess values
!                            to be converted.
!     20Jul2007  C. Redder   Made the output argument, Hum, optional
!
! EOP
!-------------------------------------------------------------------------

      logical :: perform_QC, valid, get_outlist, convert_ob, convert_fg
      integer :: HList1Sz, HList2Sz, iList, NLev, iLev, QC, kt_in
      logical, dimension ( NLev_Max ) :: InRange
      integer, dimension ( NLev_Max ) :: HList1, HList2
      real,    dimension ( NLev_Max ) :: P, T, H, T_fg, H_fg

!     Get data type index for input humidity
!     --------------------------------------
      kt_in = Mass % kt_humidity

!     Implement options
!     -----------------
      convert_ob = present ( Hum   )
      convert_fg = present ( HumFG )
      perform_QC = present ( HumQC )

!     Copy the desired data to local storage
!     --------------------------------------
      NLev = Mass % NLev
      HList1Sz = 0
      do iLev = 1, NLev
         QC = Mass % QC_Local ( iVarHum, iLev )
         valid = QC .ne. QC_Rejected
         if ( valid ) then
            HList1Sz = HList1Sz + 1
            HList1 ( HList1Sz ) = iLev
         end if
         P         ( iLev )  = Mass % P              ( iLev )
         T         ( iLev )  = Mass % Obs  ( iVarTemp, iLev )
         H         ( iLev )  = Mass % Obs  ( iVarHum,  iLev )
         if ( convert_ob )
     .      Hum    ( iLev )  = H ( iLev )
      end do

!     Perform QC (if desired) with a ...
!     ----------------------------------
      if ( perform_QC ) then

!        ... check for missing data
!        --------------------------
         HList2Sz = 0
         do iList = 1, HList1Sz
            iLev = HList1 ( iList )
            valid =  .not. Mass % P_Missing            ( iLev ) .and.
     .               .not. Mass % ObMissing ( iVarTemp,  iLev ) .and.
     .               .not. Mass % ObMissing ( iVarHum,   iLev )
            if ( valid ) then
               HList2Sz = HList2Sz + 1
               HList2 ( HList2Sz ) = iLev
            end if
         end do

!        ... range check
!        ---------------
         call CheckH   ( HList2 ( : HList2Sz ), P, T,
     .                   kt_in    = kt_in,
     .                   Obs      = H,
     .                   Valid    = InRange,
     .                   SVP_Eqtn = SVP_Eqtn )

!        Set the output qc flags
!        -----------------------
         HList1Sz  = 0
         do iLev = 1, NLev
            HumQC ( iLev ) = QC_NotTested
         end do
         do iList = 1, HList2Sz
            iLev  = HList2 ( iList )
            if ( InRange ( iLev )) then
               HList1Sz = HList1Sz + 1
               HList1 ( HList1Sz ) = iLev
               HumQC  ( iLev )     = QC_OK
            else
               HumQC  ( iLev )     = QC_Rejected
            end if
         end do

      end if

!     Convert obs to the desired humidity variable
!     --------------------------------------------
      call ConvertH ( HList1 ( : HList1Sz ), P, T,
     .                kt_in    = kt_in,
     .                kt_out   = kt_out,
     .                Obs      = H,
     .                SVP_Eqtn = SVP_Eqtn )

!     Copy obs to output
!     ------------------
      if ( convert_ob ) then
         do iList = 1, HList1Sz
            iLev = HList1 ( iList )
            Hum ( iLev ) = H ( iLev )
         end do
      end if

!     Convert first guess (if desired)
!     --------------------------------
      if ( convert_fg ) then
         do iLev = 1, NLev
            T     ( iLev ) = Mass % ObFC ( iVarTemp, iLev )
            H     ( iLev ) = Mass % ObFC ( iVarHum,  iLev )
            HumFG ( iLev ) = H ( iLev )
         end do

         HList2Sz = HList1Sz
         do iList = 1, HList1Sz
            HList2 ( iList ) = HList1 ( iList )
         end do

!        Check for missing first guess values
!        ------------------------------------
         HList1Sz = 0
         do iList = 1, HList2Sz
            iLev  =  HList2 ( iList )
            valid = .not. Mass % P_Missing            ( iLev ) .and.
     .              .not. Mass % FCMissing ( iVarTemp,  iLev ) .and.
     .              .not. Mass % FCMissing ( iVarHum,   iLev )
            if ( valid ) then
               HList1Sz = HList1Sz + 1
               HList1 ( HList1Sz ) = iLev
            end if

         end do

         call ConvertH ( HList1 ( : HList1Sz ), P, T,
     .                   kt_in    = kt_in,
     .                   kt_out   = kt_out,
     .                   Obs      = H,
     .                   SVP_Eqtn = SVP_Eqtn )

         do iList = 1, HList1Sz
            iLev = HList1 ( iList )
            HumFG ( iLev ) = H ( iLev )
         end do
      end if

      return
      end subroutine ConvertH_Prep

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: ATtoVT_PrepMass () --- Convert air temperature to virtual temperature or vice versa
!
! !DESCRIPTION:
!     This routine converts air temperature (AT) to virtual temperature
!     (VT) or vice-versa using algorithm equivalent to that implemented
!     at NCEP.
!
! !INTERFACE:
      subroutine ATtoVT_PrepMass ( Mass, T, toAT, fcst )
      use m_convert,  only : ComputeVT, ConvertH, Air_Temperature,
     .                       Virtual_Temperature, Vapor_Pressure,
     .                       Dew_Point, Mixing_Ratio, Specific_Humidity
      use m_humidity, only : DP2VP, wEqtn_NCEP
      use m_RadData,  only : QC_OK, QC_Rejected
!
! !INPUT PARAMETERS:
      implicit   NONE
      type ( pbufr_mass ),   intent (in)  ::
     .   Mass              ! Data structure for mass data
      logical,               intent (in), optional ::
     .   toAT,             ! = .true. to convert back to AT.
     .                     !   Default: toAT = .false.
     .   fcst              ! = .true. to convert forecast values.
!                          !   Default: fcst = .false.
! !OUTPUT PARAMETERS:
      real,                  intent (out) ::
     .   T (:)             ! Temperature (AT or VT, fcst or ob, deg K)
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     06May2004  C. Redder   Original code
!     03Jun2004  C. Redder   Added the optional argument, fcst.
!     20Jul2007  C. Redder   Added check to local QC flag during
!                            the conversion.
! EOP
!-------------------------------------------------------------------------

      real, parameter ::   ! Calculation are restricted to levels in the
     .   P_top = 80.0      !   troposphere or near the tropopause.
     .
      integer, parameter ::
     .   NMne  = MassNObMne
      real,    parameter ::
     .   RMiss_down = BMiss_down,
     .   RMiss_up   = BMiss_up

      integer :: iLev, NLev, iList, ListSz, kt_in, kt_out, QC_Local
      real    :: P_, T_, Hum_
      real,    dimension ( NLev_Max ) :: P, SH, Hum, T2
      integer, dimension ( NLev_Max ) :: List
      logical :: valid, to_AT, P_Missing, T_Missing, Hum_Missing,
     .           T_VIRTMP_, convert_fcst

!     Set options
!     -----------
      to_AT        = .false.
      if ( present  ( toAT )) to_AT        = toAT
      convert_fcst = .false.
      if ( present  ( fcst )) convert_fcst = fcst

!     Set default to previous temperature
!     ----------------------------------
      NLev    = Mass % NLev
      if ( convert_fcst ) then
         do iLev = 1, NLev
            T ( iLev ) = Mass % ObFC ( iVarTemp, iLev )
         end do
      else
         do iLev = 1, NLev
            T ( iLev ) = Mass % Obs  ( iVarTemp, iLev )
         end do
      end if

!     Set up list of levels at which calculations will be performed.
!     --------------------------------------------------------------
      ListSz  = 0
      do iLev = 1, NLev
         P_             =   Mass % P                    ( iLev )
         P_Missing      =   Mass % P_Missing            ( iLev )
         if ( convert_fcst ) then
              T_        =   Mass % ObFC      ( iVarTemp,  iLev )
              T_Missing =   Mass % FCMissing ( iVarTemp,  iLev )
            Hum_        =   Mass % ObFC      ( iVarHum,   iLev )
            Hum_Missing =   Mass % FCMissing ( iVarHum,   iLev )
         else
              T_        =   Mass % Obs       ( iVarTemp,  iLev )
              T_Missing =   Mass % ObMissing ( iVarTemp,  iLev )
            Hum_        =   Mass % Obs       ( iVarHum,   iLev )
            Hum_Missing =   Mass % ObMissing ( iVarHum,   iLev )
         end if
         valid          =  .not. (    P_Missing .or.
     .                                T_Missing .or.
     .                             Hum_Missing )
         T_VIRTMP_      =   Mass % T_VIRTMP             ( iLev )
!         QC_Local       =   Mass % QC_Local  ( iVarHum,   iLev )
         QC_Local       =   QC_OK
         if ( valid   .and. T_VIRTMP_   .and.
     .        QC_Local .ne. QC_Rejected .and.
     .        P_       .ge. P_top ) then
            ListSz          = ListSz + 1
            List ( ListSz ) = iLev
         end if
         P   ( iLev ) = P_
         T   ( iLev ) = T_
         Hum ( iLev ) = Hum_
      end do

!     Convert to mixing ratio
!     -----------------------
      kt_in  = Mass % kt_humidity
!      kt_out = Mixing_Ratio
      kt_out = Specific_Humidity
      call ConvertH  ( List ( : ListSz ), P, T,
     .                 kt_in    = kt_in,
     .                 kt_out   = kt_out,
     .                 Obs      = Hum,
     .                 SVP_Eqtn = SVP_Eqtn )

!     Determine if VT or AT is to be computed
!     ---------------------------------------
      if ( to_AT ) then
         kt_in  = Virtual_Temperature
         kt_out =     Air_Temperature
      else
         kt_in  =     Air_Temperature
         kt_out = Virtual_Temperature
      end if

!     Compute VT to AT
!     ----------------
      call ComputeVT ( List ( : ListSz ), P, Hum,
     .                 kt_in    = kt_in,
     .                 kt_out   = kt_out,
     .                 Obs      = T,
     .                 VT_Eqtn  = VT_Eqtn )

      return
      end subroutine ATtoVT_PrepMass
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Put_PrepMass () --- Put a segment of mass reports from Prep buffer file
!
! !DESCRIPTION:
!     This routine writes a segment of mass reports to a Prep buffer file.
!
! !INTERFACE:
      subroutine Put_PrepMass ( lu, Mass )
      use m_RadSort,  only : R2ManExp, IndexSet
      use m_RadData,  only : QC_Rejected
!
! !INPUT PARAMETERS:
      implicit   NONE
      integer,              intent (in) ::
     .   lu               ! Logical unit number for Prep buffer file
      type ( pbufr_mass ),  intent (in) ::
     .   Mass             ! Data structure for mass data
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Origional code
!     05Aug2003  C. Redder   Added argument, Meta
!     29Jan2004  C. Redder   Added code to process drift data.
!     22Feb2004  C. Redder   Added code to process pressure data.
!     11Mar2004  C. Redder   Added code to prevent obs with bad QC from
!                            being written to the buffer file.  Removed
!                            code to process pressure data.  Added code
!                            to ensure that DLon is written to file with
!                            the value in the range [0,360)
!     19Apr2004  C. Redder   Made the argument, Mass, required.
!     19May2004  C. Redder   Made changes on handling missing data.
!     16Jun2004  C. Redder   Added the component, write_drift, to the
!                            type, prep_options.
!     02Nov2006  C. Redder   Added code to convert humidity from dew
!                            point temperature to specific humidity.
!     21Jul2007  C. Redder   Added code to transfer data structure to
!                            local space
! EOP
!-------------------------------------------------------------------------

      integer, parameter ::
     .   NVar     = MassNVar,
     .   NStr     = NVar,
     .   NMne_Max = MassNObMne,
     .   NDMne    = MassNDrMne
      logical :: drift_avail, ObMissing_, ObMissing ( NLev_Max ),
     .           write_drift, FCMissing_, FCMissing ( NLev_Max )
      integer :: NLev, NL, iLev, iVar, NMne, iMne, ProgCode, iiLev,
     .           iMneBeg, iMneEnd, QC_, QC ( NLev_Max ),
     .           kt_out, kt_humidity, HumQC  ( NLev_Max )
      real    :: M, B, AMiss
      real ( kind = BKind ) :: Bufr (  NMne_Max, NLev_Max ),
     .                        DBufr ( NDMne,     NLev_Max )
      real ( kind = BKind ), dimension ( NLev_Max ) ::
     .   BufObs, BufObFC, BufObQM, BufRC, BufPC
      real,  dimension ( NLev_Max ) :: Obs,  ObFC, ObQM, RC, PC,
     .                                 DLat, DLon, ET,   TObs,
     .                                 QC_Local,   Hum,  HumFG
      character (len = *),  parameter ::
     .   ObStr   ( NStr ) = MassObStr,
     .   DrStr            = MassDrStr
      character (len = len ( ObStr )) :: ThisEvn
      character (len = len ( ObStr ) + NMne_Max )  TheseMne ( NMne_Max )
      type ( pbufr_mass ) :: Mass2

!     Copy to local space
!     -------------------
      call Mass2Mass ( Mass, Mass2 )

!     Get prep buffer file info
!     -------------------------
      ProgCode = Mass2 % File % ThisPCode

!     Get number of levels
!     --------------------
      NLev     = Mass2 % NLev

!     Convert humidity to dew point with quality control checks
!     ---------------------------------------------------------
      kt_out  = Masskt_List ( iVarHum )
      call ConvertH_Prep ( kt_out, Mass2, Hum, HumFG )
      do iLev = 1, NLev
         Mass2 % Obs  ( iVarHum, iLev ) = Hum   ( iLev )
         Mass2 % ObFC ( iVarHum, iLev ) = HumFG ( iLev )
      end do
      Mass2 % kt_humidity = kt_out

!     Compute the virtual temperature from
!     the air temperature (if necessary)
!     ------------------------------------
      call ATtoVT_PrepMass ( Mass2, TObs )
      do iLev = 1, NLev
         Mass2 % Obs ( iVarTemp, iLev ) = TObs ( iLev )
      end do

!     For each ob ...
!     ---------------
      do iVar = 1, NVar
         M = MassM ( iVar )
         B = MassB ( iVar )
         do iLev = 1, NLev
            iiLev = Mass % Indx ( iLev )

!           Convert units in observation and
!           first guess (e.g. deg K to deg C)
!           ---------------------------------
            Obs       ( iiLev ) =   Mass2 % Obs       ( iVar, iLev )
            ObFC      ( iiLev ) =   Mass2 % ObFC      ( iVar, iLev )
            ObMissing_          =   Mass2 % ObMissing ( iVar, iLev )
            FCMissing_          =   Mass2 % FCMissing ( iVar, iLev )
            if ( .not. ObMissing_ )
     .         Obs    ( iiLev ) = ( Obs  ( iiLev ) - B ) / M
            if ( .not. FCMissing_ )
     .         ObFC   ( iiLev ) = ( ObFC ( iiLev ) - B ) / M
            ObQM      ( iiLev ) =   Mass2 % ObQM      ( iVar, iLev )
            RC        ( iiLev ) =   ReasonCode
            PC        ( iiLev ) =     ProgCode
            QC        ( iiLev ) =   Mass2 % QC        ( iVar, iLev )
            ObMissing ( iiLev ) =   ObMissing_
            FCMissing ( iiLev ) =   FCMissing_
         end do

!        Store data into buffer variables
!        --------------------------------
         call R1DtoB1D ( Obs  ( : NLev ), BufObs  )
         call R1DtoB1D ( ObFC ( : NLev ), BufObFC )
         call R1DtoB1D ( ObQM ( : NLev ), BufObQM )
         call R1DtoB1D ( RC   ( : NLev ), BufRC   )
         call R1DtoB1D ( PC   ( : NLev ), BufPC   )

!        ... and then into one large buffer array
!        ----------------------------------------
         do iLev = 1, NLev
            Bufr    ( iMassMneOb, iLev ) = BufObs  ( iLev )
            Bufr    ( iMassMneQM, iLev ) = BufObQM ( iLev )
            Bufr    ( iMassMneRC, iLev ) = BufRC   ( iLev )
            Bufr    ( iMassMnePC, iLev ) = BufPC   ( iLev )

!           ... prevent data with bad QC from being written to file
!           -------------------------------------------------------
            ObMissing_ = ObMissing ( iLev )
            if ( ObMissing_ ) then
c            if ( QC_ .eq. QC_Rejected ) then
               Bufr ( iMassMneOb, iLev ) = BMiss
               Bufr ( iMassMneQM, iLev ) = BMiss
               Bufr ( iMassMneRC, iLev ) = BMiss
               Bufr ( iMassMnePC, iLev ) = BMiss
            end if
         end do

!        ... without the unwanted data
!        -----------------------------
         NMne    = 0
         TheseMne   ( : NMne_Max )
     .           = Mass2 % File % WrMne ( iVar, : NMne_Max )
         ThisEvn = ' '
         iMneBeg = 1
         do iMne = 1, NMne_Max
            if ( TheseMne ( iMne ) .ne. ' ' ) then
               NMne    = NMne + 1
               iMneEnd = iMneBeg + len_trim ( TheseMne ( iMne )) - 1
               ThisEvn ( iMneBeg : iMneEnd ) = TheseMne ( iMne )
               do iLev = 1, NLev
                  Bufr ( NMne, iLev ) = Bufr  ( iMne, iLev )
               end do
               iMneBeg = iMneEnd + 2
            end if
         end do

!        ... before writing to file
!        --------------------------
         if ( NMne .gt. 0 .and. NLev .gt. 0 ) then
            call UFBINT   ( lu,    Bufr ( : NMne, : NLev ),
     .                      NMne,  NLev, NL, ThisEvn )
         end if
      end do

!     Store balloon drift information into local scratch
!     --------------------------------------------------
      do iLev = 1, NLev
         iiLev          = Mass2 % Indx  ( iLev )
         DLat ( iiLev ) = Mass2 % DLat  ( iLev )
         DLon ( iiLev ) = Mass2 % DLon  ( iLev )
         ET   ( iiLev ) = Mass2 % ETime ( iLev )
      end do

!     ... and perform necessary conversion
!     ------------------------------------
      do iLev = 1, NLev                       ! Ensure that DLon is in the
         DLon ( iLev ) = modulo ( DLon ( iLev ),   360.0 ) !   range [0,360)
         ET   ( iLev ) =            ET ( iLev ) / 3600.0   ! Convert to hours
      end do

!     If mneumonics for drift data exists ...
!     ---------------------------------------
      drift_avail = Mass2 % drift_avail
      write_drift = Mass2 % Options % write_drift
      if ( drift_avail .and. write_drift ) then

         iMne = iMassMneDT
         call R1DtoB2D ( ET   ( : NLev ), DBufr ( iMne : iMne, : NLev ))
         iMne = iMassMneDLat
         call R1DtoB2D ( DLat ( : NLev ), DBufr ( iMne : iMne, : NLev ))
         iMne = iMassMneDLon
         call R1DtoB2D ( DLon ( : NLev ), DBufr ( iMne : iMne, : NLev ))

!        ... write the data to file
!        --------------------------
         NMne = NDMne
         call UFBINT   ( lu,    DBufr ( : NMne, : NLev ),
     .                   NDMne, NLev, NL, DrStr )
      end if

      return
      end subroutine Put_PrepMass

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Mass2Mass --- Copy data structure of type
!
! !INTERFACE:
      subroutine Mass2Mass ( Mass1, Mass2 )
!
! !USES
      implicit NONE
!
! !INPUT PARAMETERS:
      type ( pbufr_mass ),  intent (in)  ::
     .   Mass1
!
! !OUTPUT PARAMETERS:
      type ( pbufr_mass ),  intent (out) ::
     .   Mass2
!
! !DESCRIPTION:
!
! !REVISION HISTORY:
!     21Jul2007  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------
      integer iLev, iVar, NVar, NLev

      call File2File    ( Mass1 % File,    Mass2 % File    )
      call Copy_Options ( Mass1 % Options, Mass2 % Options )
      call Meta2Meta    ( Mass1 % Meta,    Mass2 % Meta    )

      NVar = MassNVar
      NLev = Mass1 % NLev

      Mass2 % loaded      = Mass1 % loaded
      Mass2 % drift_avail = Mass1 % drift_avail
      Mass2 % NLev        = NLev
      Mass2 % P_PC_Max    = Mass1 % P_PC_Max
      Mass2 % ObPC_Max    = Mass1 % ObPC_Max
      Mass2 % kt_humidity = Mass1 % kt_humidity
      do iLev = 1, NLev
         Mass2 % P_m       ( iLev ) = Mass1 % P_m       ( iLev )
         Mass2 % P_e       ( iLev ) = Mass1 % P_e       ( iLev )
         Mass2 % Cat       ( iLev ) = Mass1 % Cat       ( iLev )
         Mass2 % Indx      ( iLev ) = Mass1 % Indx      ( iLev )
         Mass2 % DLat      ( iLev ) = Mass1 % DLat      ( iLev )
         Mass2 % DLon      ( iLev ) = Mass1 % DLon      ( iLev )
         Mass2 % P         ( iLev ) = Mass1 % P         ( iLev )
         Mass2 % P_QM      ( iLev ) = Mass1 % P_QM      ( iLev )
         Mass2 % P_PC      ( iLev ) = Mass1 % P_PC      ( iLev )
         Mass2 % ETime     ( iLev ) = Mass1 % ETime     ( iLev )
         Mass2 % P_Missing ( iLev ) = Mass1 % P_Missing ( iLev )
         Mass2 % T_VIRTMP  ( iLev ) = Mass1 % T_VIRTMP  ( iLev )
         Mass2 % H_VIRTMP  ( iLev ) = Mass1 % H_VIRTMP  ( iLev )
      end do

      do iVar = 1, NVar
      do iLev = 1, NLev
         Mass2 % Obs      ( iVar,iLev ) = Mass1 % Obs      ( iVar,iLev )
         Mass2 % ObFC     ( iVar,iLev ) = Mass1 % ObFC     ( iVar,iLev )
         Mass2 % ObQM     ( iVar,iLev ) = Mass1 % ObQM     ( iVar,iLev )
         Mass2 % ObPC     ( iVar,iLev ) = Mass1 % ObPC     ( iVar,iLev )
         Mass2 % QM       ( iVar,iLev ) = Mass1 % QM       ( iVar,iLev )
         Mass2 % QC       ( iVar,iLev ) = Mass1 % QC       ( iVar,iLev )
         Mass2 % QC_Local ( iVar,iLev ) = Mass1 % QC_Local ( iVar,iLev )
         Mass2 % ObMissing( iVar,iLev ) = Mass1 % ObMissing( iVar,iLev )
         Mass2 % FCMissing( iVar,iLev ) = Mass1 % FCMissing( iVar,iLev )
      end do
      end do

      return
      end subroutine Mass2Mass
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Get_PrepWind () --- Get wind reports from Prep buffer file
!
! !DESCRIPTION:
!     This routine extracts a wind report from a Prep buffer file and
!     stores the data into the structure, Winds, local to this module.
!
!
! !INTERFACE:
      subroutine Get_PrepWind ( lu, NObs, Wind )
      use m_RadSort,  only : R2ManExp, IndexSet
      use m_RadData,  only : QC_Rejected, NPresDigits, AMiss, QC_OK
!
! !INPUT PARAMETERS:
      implicit   NONE
      integer,              intent (in)    ::
     .   lu               ! Logical unit number for Prep buffer file
!
! !INPUT/OUTPUT PARAMETERS:
      type ( pbufr_wind ),  intent (inout), optional ::
     .   Wind             ! Data structure for wind data
!
! !OUTPUT PARAMETERS:
      integer,              intent (out)   ::
     .   NObs             ! Number of observations
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Origional code
!     05Aug2003  C. Redder   Added optional argument, Meta
!     13Aug2003  C. Redder   Major changes to the interface and internal
!                            coding to put data into a local data structure
!                            rather than radcor.
!     28Jan2004  C. Redder   Added code to extract the drift lat/lon
!     22Feb2004  C. Redder   Changes in the routine calls in the buffer
!                            library.
!     26Apr2004  C. Redder   Added code to retreive program codes
!     03Jun2004  C. Redder   Added code to process forecast data.
!     09Jun2004  C. Redder   Fixed compilation bug by changing the
!                            attribute from integer to real for internal
!                            variable, PC_Max.
!     01Sep2004  C. Redder   Condensed code that assigns QC to a single
!                            call to the routine, Check_PrepQM, which has
!                            a revised interface.
! EOP
!-------------------------------------------------------------------------

      integer, parameter ::
     .   NMne  =  WindNObMne,
     .   NFMne =  WindNFCMne,
     .   NPMne =  WindNPMne,
     .   NDMne =  WindNDrMne,
     .   iWind = -2
      real    :: DLat_, DLon_, ET_, PC_Max
      integer :: NLev, NL, iLev, iiLev, iMne, ProgCode
      logical :: nobs_only, drift_avail, latest_QM, reject_OIQC,
     .           PCMissing ( NLev_Max ), local_QC_set,
     .           P_Missing ( NLev_Max ),
     .           U_Missing ( NLev_Max ),
     .           UFMissing ( NLev_Max ),
     .           V_Missing ( NLev_Max ),
     .           VFMissing ( NLev_Max )
      real ( kind = BKind ) :: Bufr (  NMne, NLev_Max ),
     .                        FBufr ( NFMne, NLev_Max ),
     .                        PBufr ( NPMne, NLev_Max ),
     .                        DBufr ( NDMne, NLev_Max )
      integer, dimension ( NLev_Max ) :: Cat,  Indx, P_m,  P_e
      real,    dimension ( NLev_Max ) :: DLat, DLon, P,    UObs, VObs,
     .                                               P_QM, ObQM,
     .                                               P_PC, ObPC, ET,
     .                                                     UFC,  VFC
      character (len= len(WindFCStr)) :: FCStr
      character (len=*), parameter ::
     .   DrStr  =   WindDrStr

!     Set flags to get optional output arguments
!     ------------------------------------------
      nobs_only =  .not. present ( Wind )

!     Get category
!     ------------
      call UFBINT ( lu, Bufr ( : 1, : ), 1, NLev_Max, NLev, 'CAT' )
      NLev = min ( NLev_Max, NLev )

!     Return only if NObs is desired
!     ------------------------------
      NObs = 2 * NLev
      if ( nobs_only ) return

!     Set optins for reading
!     ----------------------
      latest_QM   = Wind % Options % get_latest_QM
      reject_OIQC = Wind % Options % reject_OIQC

!     Set the select the program code.
!     --------------------------------
      ProgCode  = Wind % File % LastPCode

!     Convert category to integer format
!     ----------------------------------
      call B2DtoI1D    ( Bufr ( : 1, : NLev ), Cat )

!     Get pressure levels and their QC marks
!     --------------------------------------
      call Get_BObs    ( lu, Wind % File,
     .                       Wind % Options, NLev, PBufr )
c      call UFBINT      ( lu, PBufr, NPMne, NLev_Max, NLev, PStr )
      iMne = iWindMnePQM
      call B2DtoR1D    ( PBufr ( iMne : iMne, : NLev ), P_QM )
      iMne = iWindMnePPC
      call B2DtoR1D    ( PBufr ( iMne : iMne, : NLev ), P_PC )
      call Test_RMiss  ( P_PC               ( : NLev ), PCMissing )
      iMne = iWindMnePOb
      call B2DtoR1D    ( PBufr ( iMne : iMne, : NLev ), P    )
      call Test_RMiss  ( P                  ( : NLev ), P_Missing )

!     Determine maximum value for the program code
!     --------------------------------------------
      PC_Max = 0.0
      do iLev = 1, NLev
         if ( .not. PCMissing ( iLev ))
     .      PC_Max = max ( PC_Max, P_PC ( iLev ))
      end do
      Wind % P_PC_Max = nint ( PC_Max )

!     Ensure that data are sorted by pressure level (descending order)
!     ----------------------------------------------------------------
      call IndexSet   ( Indx ( : NLev ))
      call R2ManExp   ( NPresDigits, P ( : NLev ), P_m, P_e )
      call Sort_byLev ( Indx ( : NLev ),           P_m, P_e )

!     Get the drift data (if available)
!     ---------------------------------
      call UFBINT  ( lu, DBufr, NDMne, NLev_Max, NL, DrStr )
      drift_avail =  NL .eq. NLev  ! Determine if elapsed time is available
      iMne = iWindMneDT
      call B2DtoR1D    ( DBufr ( iMne : iMne, : NL   ), ET   )
      iMne = iWindMneDLat
      call B2DtoR1D    ( DBufr ( iMne : iMne, : NL   ), DLat )
      iMne = iWindMneDLon
      call B2DtoR1D    ( DBufr ( iMne : iMne, : NL   ), DLon )

!     Perform the necessary calculations for data already obtained
!     ------------------------------------------------------------
      do iLev = 1, NLev
         DLat_         = DLat ( iLev )
         DLon_         = DLon ( iLev )
         ET_           = ET   ( iLev )
         DLat ( iLev ) = AMiss
         DLon ( iLev ) = AMiss
         ET   ( iLev ) = AMiss
         if ( drift_avail ) then
            DLat ( iLev ) = DLat_
            DLon ( iLev ) = DLon_
            ET   ( iLev ) = ET_ * 3600.0 ! Convert elapsed to seconds
         end if
      end do

!     Wind observations
!     -----------------
      call Get_BObs    ( lu, Wind % File,
     .                       Wind % Options, NLev, Bufr,
     .                       DataType = iWind )
!      call UFBINT      ( lu, Bufr, NMne, NLev_Max, NLev, ObStr )
      iMne = iWindMneQM
      call B2DtoR1D    ( Bufr ( iMne : iMne, : NLev ), ObQM )
      iMne = iWindMnePC
      call B2DtoR1D    ( Bufr ( iMne : iMne, : NLev ), ObPC )
      call Test_RMiss  ( ObPC              ( : NLev ), PCMissing )
      iMne = iWindMneUOb
      call B2DtoR1D    ( Bufr ( iMne : iMne, : NLev ), UObs )
      call Test_RMiss  ( UObs              ( : NLev ), U_Missing )
      iMne = iWindMneVOb
      call B2DtoR1D    ( Bufr ( iMne : iMne, : NLev ), VObs )
      call Test_RMiss  ( VObs              ( : NLev ), V_Missing )

!     ... and forecast values
!     -----------------------
      FCStr = WindFCStr
      call UFBINT ( lu,  FBufr, NFMne, NLev_Max, NLev, FCStr )
      iMne = iWindMneUFC
      call B2DtoR1D    ( FBufr ( iMne : iMne, : NLev ), UFC  )
      call Test_RMiss  ( UFC                ( : NLev ), UFMissing )
      iMne = iWindMneVFC
      call B2DtoR1D    ( FBufr ( iMne : iMne, : NLev ), VFC  )
      call Test_RMiss  ( VFC                ( : NLev ), VFMissing )

!     Determine maximum value for the program code
!     --------------------------------------------
      PC_Max  = 0.0
      do iLev = 1, NLev
         if ( .not. PCMissing ( iLev ))
     .      PC_Max = max ( PC_Max, ObPC ( iLev ))
      end do
      Wind % ObPC_Max = nint ( PC_Max )

!     Store data into storage local to this module
!     --------------------------------------------
      Wind % NLev = NLev
      do iLev = 1, NLev
         iiLev = Indx        ( iLev )
         Wind % DLat         ( iLev ) = DLat      ( iiLev )
         Wind % DLon         ( iLev ) = DLon      ( iiLev )
         Wind % P            ( iLev ) = P         ( iiLev )
         Wind % P_m          ( iLev ) = P_m       ( iiLev )
         Wind % P_e          ( iLev ) = P_e       ( iiLev )
         Wind % P_QM         ( iLev ) = P_QM      ( iiLev )
         Wind % P_PC         ( iLev ) = P_PC      ( iiLev )
         Wind % P_Missing    ( iLev ) = P_Missing ( iiLev )
         Wind % Cat          ( iLev ) = Cat       ( iiLev )
         Wind % Indx         ( iLev ) =             iiLev
         Wind % ETime        ( iLev ) = ET        ( iiLev )
         Wind % Obs       ( 1, iLev ) = UObs      ( iiLev )
         Wind % Obs       ( 2, iLev ) = VObs      ( iiLev )
         Wind % ObFC      ( 1, iLev ) = UFC       ( iiLev )
         Wind % ObFC      ( 2, iLev ) = VFC       ( iiLev )
         Wind % ObQM      ( 1, iLev ) = ObQM      ( iiLev )
         Wind % ObQM      ( 2, iLev ) = ObQM      ( iiLev )
         Wind % ObPC      ( 1, iLev ) = ObPC      ( iiLev )
         Wind % ObPC      ( 2, iLev ) = ObPC      ( iiLev )
         Wind % ObMissing ( 1, iLev ) = U_Missing ( iiLev )
         Wind % ObMissing ( 2, iLev ) = V_Missing ( iiLev )
         Wind % FCMissing ( 1, iLev ) = UFMissing ( iiLev )
         Wind % FCMissing ( 2, iLev ) = VFMissing ( iiLev )
      end do
      Wind % loaded      = .true.
      Wind % drift_avail =  drift_avail

!     Set quality control flags
!     -------------------------
      call Check_PrepQM    ( Wind % P_Missing ( : NLev ),
     .                       Wind % P_QM,
     .                       Wind % ObMissing,
     .                       Wind % ObQM,
     .                       QM   = Wind % QM,
     .                       QC   = Wind % QC,
     .                       reject_OIQC = reject_OIQC,
     .                       local_QC    = local_QC_set )

!     Set quality control flags for internal use
!     ------------------------------------------
      if ( .not. local_QC_set ) then
         call Check_PrepQM ( Wind % P_Missing ( : NLev ),
     .                       Wind % P_QM,
     .                       Wind % ObMissing,
     .                       Wind % ObQM,
     .                       QC   = Wind % QC_Local )
      else
         do iLev = 1, NLev
            Wind % QC_Local ( 1, iLev ) = Wind % QC ( 1, iLev )
         end do
         do iLev = 1, NLev
            Wind % QC_Local ( 2, iLev ) = Wind % QC ( 2, iLev )
         end do

      end if

c      if ( DBufr (3,1) .gt. 180.0 ) then
c         print *, 'Wind', P(NL), DBufr(2,1),
c     .                           DBufr(3,1) - 360.0,
c     .                           ET (NLev) - ET(1),
c     .                           DBufr(2,NLev),
c     .                           DBufr(3,NLev)-360.0
c      else
c         print *, 'Wind', P(NL), DBufr(2,1),
c     .                           DBufr(3,1),
c     .                           ET (NLev) - ET(1),
c     .                           DBufr(2,NLev),
c     .                           DBufr(3,NLev)
c      end if
c      if ( nint(DBufr(2,1)*100.0).eq. 4269 .and.
c     .     nint(DBufr(3,1)*100.0).eq.28617) then
c         print *, NLev
c         do iLev=1, NLev
c            iiLev = Indx ( iLev )
c            print *, iLev, P(iLev), ET (iiLev) - ET(Indx(1)),
c     .                              DBufr(1,iiLev),
c     .                              DBufr(2,iiLev),
c     .                              DBufr(3,iiLev)-360.0,
c     .                              UObs (iiLev),
c     .                              VObs (iiLev),
c     .                              ObQC (iiLev)
c         end do
c      end if

      return
      end subroutine Get_PrepWind

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Put_PrepWind () --- Put a segment of wind reports from Prep buffer file
!
! !DESCRIPTION:
!     This routine writes a segment of wind reports to a Prep buffer file.
!
! !INTERFACE:
      subroutine Put_PrepWind ( lu, Wind )
      use m_RadSort,  only : R2ManExp, IndexSet
      use m_RadData,  only : QC_Rejected
!
! !INPUT PARAMETERS:
      implicit   NONE
      integer,              intent (in) ::
     .   lu               ! Logical unit number for Prep buffer file
      type ( pbufr_wind ),  intent (in) ::
     .   Wind             ! Data structure for wind data
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     29Jan2004  C. Redder   Origional code
!     29Jan2004  C. Redder   Added code to process drift data.
!     22Feb2004  C. Redder   Added code to process pressure data.
!     11Mar2004  C. Redder   Removed code to process pressure data and
!                            observation data.  Added code to ensure that
!                            DLon is written to file with the value in
!                            the range [0,360)
!     19Apr2004  C. Redder   Made the argument, Wind, required.
!     16Jun2004  C. Redder   Added the component, write_drift, to the
!                            type, prep_options.
! EOP
!-------------------------------------------------------------------------

      integer, parameter ::
     .   NDMne = WindNDrMne
      real    :: DLat_, DLon_, ET_
      integer :: NLev, NL, iLev, iiLev, iMne
      logical :: drift_avail, write_drift
      real ( kind = BKind ) ::DBufr ( NDMne, NLev_Max )
      real,    dimension ( NLev_Max ) :: DLat, DLon, ET
      integer, dimension ( NLev_Max ) :: QC_Local
      character (len = *),  parameter ::
     .   DrStr  = WindDrStr

!     Get number of levels
!     --------------------
      NLev     = Wind % NLev

!     Assign quality control markers consistent
!     with those in the routine Get_PrepWind
!     (routine not needed for this version)
!     -----------------------------------------
c      call Check_PrepQM ( Wind % P_Missing ( : NLev )
c     .                    Wind % P_QM,
c     .                    Wind % ObMissing,
c     .                    Wind % ObQM,
c     .                    QC   = QC_Local )

!     Store balloon drift-data into local scratch
!     -------------------------------------------
      do iLev = 1, NLev
         iiLev          = Wind % Indx  ( iLev )
         DLat ( iiLev ) = Wind % DLat  ( iLev )
         DLon ( iiLev ) = Wind % DLon  ( iLev )
         ET   ( iiLev ) = Wind % ETime ( iLev )
      end do

!     ... and perform necessary conversion
!     ------------------------------------
      do iLev = 1, NLev                       ! Ensure that dlon is in the
         DLon ( iLev ) = modulo ( DLon ( iLev ),   360.0 ) !   range [0,360)
         ET   ( iLev ) =            ET ( iLev ) / 3600.0   ! Convert to hours
      end do

!     If mneumonics for drift data exists ...
!     ---------------------------------------
      drift_avail = Wind % drift_avail
      write_drift = Wind % Options % write_drift
      if ( drift_avail .and. write_drift ) then

         iMne = iWindMneDT
         call R1DtoB2D ( ET   ( : NLev ), DBufr ( iMne : iMne, : NLev ))
         iMne = iWindMneDLat
         call R1DtoB2D ( DLat ( : NLev ), DBufr ( iMne : iMne, : NLev ))
         iMne = iWindMneDLon
         call R1DtoB2D ( DLon ( : NLev ), DBufr ( iMne : iMne, : NLev ))

!        ... write the data to file
!        --------------------------
         call UFBINT   ( lu,    DBufr ( : NDMne, : NLev ),
     .                   NDMne, NLev, NL, DrStr )
      end if

      return
      end subroutine Put_PrepWind

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Sort_byLev --- Sort each sounding by level
!
! !INTERFACE:
      subroutine Sort_byLev ( Indx, Man, Exp )
!
! !USES
      use m_RadSort, only : IndexSort
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,  intent (in),    dimension (:) ::
     .   Man,   ! Pressure levels expressed as mantessas
     .   Exp    ! ... and exponents
!
! !INPUT/OUTPUT PARAMETERS:
      integer,  intent (inout), dimension (:) ::
     .   Indx   ! Sorting indices
!
! !DESCRIPTION:
!     This routine sorts by level the data for a given sounding.  The
!     levels are expressed as mantessa and exponents to eliminate
!     uncertainty due to round-off error.
!
! !REVISION HISTORY:
!     12Aug2003  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------
      integer :: iLev, NLev, iiLev, ThisMan, ThisExp, LastMan, LastExp
      logical :: sorted
      integer, dimension ( NLev_Max ) :: Indx2

!     Determine whether it is sorted by level
!     ---------------------------------------
      sorted  = .true.
      LastMan =  Man  ( Indx ( 1 ))
      LastExp =  Exp  ( Indx ( 1 ))
      NLev    =  size ( Indx )
      do iLev =  2, NLev
         iiLev   = Indx ( iLev )
         ThisMan = Man ( iiLev )
         ThisExp = Exp ( iiLev )
         sorted  = ThisExp .le. LastExp
         if ( ThisExp .eq. LastExp )
     .      sorted = sorted .and. ThisMan .le. LastMan
         if ( .not. sorted ) exit
         LastMan = ThisMan
         LastExp = ThisExp
      end do

!     ... and sort if necessary
!     -------------------------
      if ( .not. sorted ) then
         call IndexSort ( Indx, Indx2, Man, descend = .true. )
         call IndexSort ( Indx, Indx2, Exp, descend = .true. )
      end if

      end subroutine Sort_byLev

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: SfcTime () --- Estimate the observation time at the surface
!
! !DESCRIPTION:
!     Given the station elevation, this routine estimates the ob time
!     at the surface from height and elapsed time data.  If no valid data
!     is close enough to the surface, then this function returns the
!     missing value, IMiss, as defined in the module, m_RadData.
!
! !INTERFACE:
      function SfcTime ( Elev, Z, ET, Mask )
      use m_RadData, only : IMiss
!
! !INPUT PARAMETERS:
      implicit   NONE
      real,             intent (in)  ::
     .   Elev,        ! Station elevation ( m )
     .   Z    (:),    ! Heights      ( m )
     .   ET   (:)     ! Elapsed time ( s )
      logical,          intent (in)  ::
     .   Mask (:)     ! = .true./.false. to mask in/out (i.e. accept/reject)
!
! !OUTPUT PARAMETERS:
      real :: SfcTime ! Time at the surface (s)
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     05Aug2003  C. Redder   Origional code
!
! EOP
!-------------------------------------------------------------------------

      real    :: Z_, ET_, Del_Above, Del, ET1, Z1, ET2, Z2, RRate
      integer :: iLev_Above, NLev, iLev, iLev1, iLev2
      logical :: mask_in
      real, parameter ::
     .   DelZ_Min  =   -0.5,
     .   DelZ_Max  = 3000.0,
     .   DelZ_Max2 = 1500.0,
     .   DelET_Min =  600.0,
     .   DelET_Max = 1800.0,
     .   RRate_Def =    5.0,
     .   RRate_Min =    0.5

!     Set the launch time to default
!     ------------------------------
      SfcTime  = IMiss

      NLev     = min ( size ( Z ), size ( ET ), size ( Mask ))

!     Find the nearest point above the surface.
!     -----------------------------------------
      Del_Above   = DelZ_Max
      iLev_Above  = 0
      do iLev = 1, NLev
         mask_in = Mask ( iLev )
         if ( mask_in ) then               ! ... that is acceptable
            Del = Z ( iLev ) - Elev
            if ( Del .gt. DelZ_Min .and.
     .           Del .le. Del_Above ) then ! ... below a maximum height
               iLev_Above = iLev
                Del_Above = Del
            end if
         end if
      end do
      iLev1 = iLev_Above

!     Return if no point is found
!     ---------------------------
      if ( iLev1 .eq. 0 ) return

!     Find a second point above the first one
!     ---------------------------------------
       ET1        = ET ( iLev1 )
       Del_Above  = DelET_Min
      iLev_Above  = 0
      do iLev = 1, NLev
         mask_in = Mask ( iLev )
         if ( mask_in ) then               ! ... that is acceptable
            Del = ET ( iLev ) - ET1
            if ( Del .lt. DelET_Max .and.  ! ... with enough distance above
     .           Del .ge. Del_Above ) then ! ... but below a maximum height
               iLev_Above = iLev           !   in order to obtain a robust
                Del_Above = Del            !   estimate for the rise rate.
            end if
         end if
      end do
      iLev2  = iLev_Above

!     Obtain the estimate for the rise rate
!     -------------------------------------
      ET1    = ET ( iLev1 )
      Z1     = Z  ( iLev1 )
      if      ( iLev2 .ne. 0 ) then
         ET2   = ET ( iLev2 )
         Z2    = Z  ( iLev2 )
         RRate = ( Z2 - Z1 ) / ( ET2 - ET1 )

      else if ( Z1 - Elev .le. DelZ_Max2 ) then
         RRate = RRate_Def
      else
         return

      end if

!     Return if rise rate is too small
!     --------------------------------
      if ( RRate .lt. RRate_Min ) return

!     Estimate the ob time
!     --------------------
      SfcTime = ET1 - ( Z1 - Elev ) / RRate

      return
      end function SfcTime
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: B1DtoI1D () --- Copy 1-D buffer array to 1-D, native integer array
!
! !DESCRIPTION:
!     This routine copies a 1-dimensional, buffer array to a 1-dimension,
!     native integer array.
!
! !INTERFACE:
      subroutine B1DtoI1D ( B1D, I1D )
!
! !INPUT PARAMETERS:
      implicit   NONE
      real ( kind = BKind ), intent (in)    ::
     .   B1D (:)           ! 1-D input buffer array
!
! !OUTPUT PARAMETERS:
      integer,               intent (inout) ::
     .   I1D (:)           ! 1-D output array of native integers
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Origional code
!     29Jan2004  C. Redder   Made argument, I1D, input/output.
! EOP
!-------------------------------------------------------------------------

      real, parameter ::
     .   IMin = -Huge ( 1 ),
     .   IMax =  Huge ( 1 )
      integer :: Sz, i1
      real ( kind = BKind ) :: BVal

      Sz = min ( size ( B1D ), size ( I1D ))

      do i1 = 1, Sz
         BVal       = B1D ( i1 )
         I1D ( i1 ) = nint ( min ( max ( BVal, IMin ), IMax ))
      end do

      return
      end subroutine B1DtoI1D

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: B1DtoR1D () --- Copy 1-D buffer array to 1-D, native real array
!
! !DESCRIPTION:
!     This routine copies a 1-dimensional, buffer array to a 1-dimension,
!     native real array.
!
! !INTERFACE:
      subroutine B1DtoR1D ( B1D, R1D )
!
! !INPUT PARAMETERS:
      implicit   NONE
      real ( kind = BKind ), intent (in)    ::
     .   B1D (:)           ! 1-D input buffer array
!
! !OUTPUT PARAMETERS:
      real,                  intent (inout) ::
     .   R1D (:)           ! 1-D output array of native reals
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Original code
!     10Dec2003  C. Redder   Removed bug by removing call to intrinsic
!                            function, nint.
!     29Jan2004  C. Redder   Made argument, R1D, input/output.
! EOP
!-------------------------------------------------------------------------

      integer, parameter ::
     .   RangeR  = range ( 1.0 ),
     .   RangeB  = range ( 1.0_BKind )
      integer :: Sz, i1
      real ( kind = BKind ) :: BVal, BMin, BMax

      if ( RangeB .ge. RangeR ) then
         BMin = -huge ( 1.0 )
         BMax =  huge ( 1.0 )
      else
         BMin = -huge ( 1.0_BKind )
         BMax =  huge ( 1.0_BKind )
      end if

      Sz = min ( size ( B1D ), size ( R1D ))

      do i1 = 1, Sz
         BVal       = B1D  ( i1 )
         R1D ( i1 ) = min ( max ( BVal, BMin ), BMax )
      end do

      return
      end subroutine B1DtoR1D

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: B2DtoI1D () --- Copy 2-D buffer array to 1-D, native integer array
!
! !DESCRIPTION:
!     This routine copies a 2-dimensional, buffer array to a 1-dimension,
!     native integer array.  The first (leftmost) dimension is assumed to
!     correspond to the fastest running index.
!
! !INTERFACE:
      subroutine B2DtoI1D ( B2D, I1D )
!
! !INPUT PARAMETERS:
      implicit   NONE
      real ( kind = BKind ), intent (in)    ::
     .   B2D (:,:)         ! 2-D input buffer array
!
! !OUTPUT PARAMETERS:
      integer,               intent (inout) ::
     .   I1D   (:)         ! 1-D output array of native integers
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Origional code
!     29Jan2004  C. Redder   Made argument, I1D, input/output.
! EOP
!-------------------------------------------------------------------------

      real ( kind = BKind ), parameter ::
     .   IMin = -Huge ( 1 ),
     .   IMax =  Huge ( 1 )
      integer :: Sz1, Sz2, ii1, ii2, i1, SzN
      real ( kind = BKind ) :: BVal

      Sz1 = size ( B2D, dim = 1 )
      Sz2 = size ( B2D, dim = 2 )
      SzN = min  ( Sz1 * Sz2, size ( I1D ))

      ii1 = 1
      ii2 = 1
      do i1 = 1, SzN
         BVal       = B2D ( ii1, ii2 )
         I1D ( i1 ) = nint ( min ( max ( BVal, IMin ), IMax ))
         if ( ii1 .lt. Sz1 ) then
            ii1 = ii1 + 1
         else
            ii1 = 1
            ii2 = ii2 + 1
         end if
      end do

      return
      end subroutine B2DtoI1D

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: B2DtoR1D () --- Copy 2-D buffer array to 1-D, native real array
!
! !DESCRIPTION:
!     This routine copies a 2-dimensional, buffer array to a 1-dimension,
!     native real array.  The first (leftmost) dimension is assumed to
!     correspond to the fastest running index.
!
! !INTERFACE:
      subroutine B2DtoR1D ( B2D, R1D )
!
! !INPUT PARAMETERS:
      implicit   NONE
      real ( kind = BKind ), intent (in)    ::
     .   B2D (:,:)         ! 2-D input buffer array
!
! !OUTPUT PARAMETERS:
      real,                  intent (inout) ::
     .   R1D   (:)         ! 1-D output array of native reals
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Origional code
!     29Jan2004  C. Redder   Made argument, R1D, input/output.
! EOP
!-------------------------------------------------------------------------

      integer, parameter ::
     .   RangeR  = range ( 1.0 ),
     .   RangeB  = range ( 1.0_BKind )
      integer :: Sz1, Sz2, ii1, ii2, i1, SzN
      real ( kind = BKind ) :: BVal, BMin, BMax

      Sz1 = size ( B2D, dim = 1 )
      Sz2 = size ( B2D, dim = 2 )
      SzN = min  ( Sz1 * Sz2, size ( R1D ))

      if ( RangeB .ge. RangeR ) then
         BMin = -huge ( 1.0 )
         BMax =  huge ( 1.0 )
      else
         BMin = -huge ( 1.0_BKind )
         BMax =  huge ( 1.0_BKind )
      end if

      ii1 = 1
      ii2 = 1
      do i1 = 1, SzN
         BVal       = B2D ( ii1, ii2 )
         R1D ( i1 ) = min ( max ( BVal, BMin ), BMax )
         if ( ii1 .lt. Sz1 ) then
            ii1 = ii1 + 1
         else
            ii1 = 1
            ii2 = ii2 + 1
         end if
      end do

      return
      end subroutine B2DtoR1D

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: I1DtoB1D () --- Copy 1-D, native integer array to 1-D buffer array
!
! !DESCRIPTION:
!     This routine copies a 1-dimension, native integer array to a
!     1-dimension buffer array
!
! !INTERFACE:
      subroutine I1DtoB1D ( I1D, B1D )
!
! !INPUT PARAMETERS:
      implicit   NONE
      integer,               intent (in)    ::
     .   I1D (:)           ! 1-D output array of native integers
!
! !OUTPUT PARAMETERS:
      real ( kind = BKind ), intent (inout) ::
     .   B1D (:)           ! 1-D input buffer array
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Origional code
!     29Jan2004  C. Redder   Made argument, B1D, input/output.
! EOP
!-------------------------------------------------------------------------

      real, parameter ::
     .   IMin = -Huge ( 1 ),
     .   IMax =  Huge ( 1 )
      integer :: Sz, i1
      real ( kind = BKind ) :: BVal

      Sz = min ( size ( I1D ), size ( B1D ))

      do i1 = 1, Sz
         BVal       = real ( B1D ( i1 ), kind = BKind )
         B1D ( i1 ) = BVal
      end do

      return
      end subroutine I1DtoB1D

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: R1DtoB1D () --- Copy 1-D, native real array to 1-D buffer array
!
! !DESCRIPTION:
!     This routine copies a 1-dimension, native real array to a 1-dimension
!     buffer array
!
! !INTERFACE:
      subroutine R1DtoB1D ( R1D, B1D )
!
! !INPUT PARAMETERS:
      implicit   NONE
      real,                  intent (in)    ::
     .   R1D (:)           ! 1-D output array of native reals
!
! !OUTPUT PARAMETERS:
      real ( kind = BKind ), intent (inout) ::
     .   B1D (:)           ! 1-D input buffer array
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Origional code
!     29Jan2004  C. Redder   Made argument, B1D, input/output.
! EOP
!-------------------------------------------------------------------------

      integer, parameter ::
     .   RangeR  = range ( 1.0 ),
     .   RangeB  = range ( 1.0_BKind )
      integer :: Sz, i1
      real :: RVal, RMin, RMax

      Sz = min ( size ( R1D ), size ( B1D ))
      if ( RangeR .ge. RangeB ) then
         RMin = -huge ( 1.0_BKind )
         RMax =  huge ( 1.0_BKind )
      else
         RMin = -huge ( 1.0 )
         RMax =  huge ( 1.0 )
      end if

      do i1 = 1, Sz
         RVal       = min ( max ( R1D ( i1 ), RMin ), RMax )
         B1D ( i1 ) = RVal
      end do

      return
      end subroutine R1DtoB1D

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: I1DtoB2D () --- Copy 1-D native integer array to 2-D buffer array to
!
! !DESCRIPTION:
!     This routine copies a 1-dimension, native integer array.  The first
!     (leftmost) dimension is assumed to correspond to the fastest running
!     index.
!
! !INTERFACE:
      subroutine I1DtoB2D ( I1D, B2D )
!
! !INPUT PARAMETERS:
      implicit   NONE
      integer,               intent (in)    ::
     .   I1D   (:)         ! 1-D output array of native integers
!
! !OUTPUT PARAMETERS:
      real ( kind = BKind ), intent (inout) ::
     .   B2D (:,:)         ! 2-D input buffer array
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Origional code
!     29Jan2004  C. Redder   Made argument, B2D, input/output.
!
! EOP
!-------------------------------------------------------------------------

      integer :: Sz1, Sz2, ii1, ii2, i1, SzN
      real ( kind = BKind ) :: BVal

      Sz1 = size ( B2D, dim = 1 )
      Sz2 = size ( B2D, dim = 2 )
      SzN = min  ( Sz1 * Sz2, size ( I1D ))

      ii1 = 1
      ii2 = 1
      do i1 = 1, SzN
         BVal   = real ( I1D ( i1 ), kind = BKind )
         B2D ( ii1, ii2 ) = BVal
         if ( ii1 .lt. Sz1 ) then
            ii1 = ii1 + 1
         else
            ii1 = 1
            ii2 = ii2 + 1
         end if
      end do

      return
      end subroutine I1DtoB2D

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: R1DtoB2D () --- Copy 1-D, native real array to 2-D, buffer array
!
! !DESCRIPTION:
!     This routine copies a 1-dimension, native real array to a
!     2-dimensional, buffer array.  The first (leftmost) dimension is
!     assumed to correspond to the fastest running index.
!
! !INTERFACE:
      subroutine R1DtoB2D ( R1D, B2D )
!
! !INPUT PARAMETERS:
      implicit   NONE
      real,                  intent (in)    ::
     .   R1D   (:)         ! 1-D output array of native reals
!
! !OUTPUT PARAMETERS:
      real ( kind = BKind ), intent (inout) ::
     .   B2D (:,:)         ! 2-D input buffer array
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Origional code
!     29Jan2004  C. Redder   Made argument, B2D, input/output.
!
! EOP
!-------------------------------------------------------------------------

      integer, parameter ::
     .   RangeR  = range ( 1.0 ),
     .   RangeB  = range ( 1.0_BKind )
      integer :: Sz1, Sz2, SzN, ii1, ii2, i1
      real    :: RVal, RMin, RMax

      Sz1 = size ( B2D, dim = 1 )
      Sz2 = size ( B2D, dim = 2 )
      SzN = min  ( Sz1 * Sz2, size ( R1D ))

      if ( RangeR .ge. RangeB ) then
         RMin = -huge ( 1.0_BKind )
         RMax =  huge ( 1.0_BKind )
      else
         RMin = -huge ( 1.0 )
         RMax =  huge ( 1.0 )
      end if

      ii1 = 1
      ii2 = 1
      do i1 = 1, SzN
         RVal = min ( max ( R1D ( i1 ), RMin ), RMax )
         B2D ( ii1, ii2 ) = RVal
         if ( ii1 .lt. Sz1 ) then
            ii1 = ii1 + 1
         else
            ii1 = 1
            ii2 = ii2 + 1
         end if
      end do

      return
      end subroutine R1DtoB2D

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Check_PrepQM () --- Set quality control (QC) flags
!
! !DESCRIPTION:
!     This routine sets assign quality control marks based on quality
!     marks from the prep buffer files and logical flags denoting
!     whether the correcting observation has missing (or fill value).
!     See the routine, PrepQMtoQC, for more details.
!
! !INTERFACE:
      subroutine Check_PrepQM ( P_Missing, P_QM,
     .                          ObMissing, ObQM, QM,
     .                                     P_QC,
     .                                     ObQC, QC,
     .                          reject_OIQC,
     .                          local_QC )
      use m_RadData,  only : QC_Rejected, QC_OK
!
! !INPUT  PARAMETERS:
      implicit   NONE
      logical, intent (in) ::
     .   P_Missing   (:), ! = .true. if pressure ob is missing
     .   ObMissing (:,:)  ! "   "    if ob value is missing
      real,    intent (in) ::
     .   P_QM        (:), ! quality mark for pressure ob
     .   ObQM      (:,:)  ! ... for observed value
      logical, intent (in),  optional ::
     .   reject_OIQC      ! = .true. to reject observations that failed
                          !   IOQC.  Default: QIQC = .false. to classify
                          !   obs that failed IOQC as suspect.
!
! !OUTPUT PARAMETERS:
      real,    intent (out), optional ::
     .     QM      (:,:)  ! combined quality mark
      integer, intent (out), optional ::
     .   P_QC        (:), ! quality control flag for pressure ob
     .   ObQC      (:,:), ! ... for observed value
     .     QC      (:,:)  ! ... combined
      logical, intent (out), optional ::
     .   local_QC         ! = .true. if the options are consistent with
                          !   those for local QC
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     01Sep2004  C. Redder   Original code
! EOP
!-------------------------------------------------------------------------

      integer :: iLev, NLev, iVar, NVar
      logical :: get_QM, get_P_QC, get_ObQC, get_QC, combine_QM,
     .           mass_rep
      real,    dimension ( NLev_Max ) ::        tObQM
      integer, dimension ( NLev_Max ) :: tP_QC, tObQC, tQC

      get_QM     = present (   QM )
      get_P_QC   = present ( P_QC )
      get_ObQC   = present ( ObQC )
      get_QC     = present (   QC )
      combine_QM = get_QM .or. get_QC

      if ( present ( local_QC )) then
         local_QC = .true.
         if ( present ( reject_OIQC )) local_QC = .not. reject_OIQC
      end if

!     Get the dimension sizes (i.e number of levels and variables)
!     ------------------------------------------------------------
      NLev = min ( size ( P_Missing ),
     .             size ( P_QM ),
     .             size ( ObMissing, dim = 2 ),
     .             size ( ObQM,      dim = 2 ))
      NVar = min ( size ( ObMissing, dim = 1 ),
     .             size ( ObQM,      dim = 1 ))
      if ( NLev .le. 0 .or. NVar .le. 0 ) return

      if ( get_QM   ) then
         NLev = min ( NLev, size (   QM, dim = 2 ))
         NVar = min ( NVar, size (   QM, dim = 1 ))
      end if
      if ( get_P_QC ) then
         NLev = min ( NLev, size ( P_QC ))
      end if
      if ( get_ObQC ) then
         NLev = min ( NLev, size ( ObQC, dim = 2 ))
         NVar = min ( NVar, size ( ObQC, dim = 1 ))
      end if
      if ( get_QC   ) then
         NLev = min ( NLev, size (   QC, dim = 2 ))
         NVar = min ( NVar, size (   QC, dim = 1 ))
      end if
      if ( NLev .le. 0 .or. NVar .le. 0 ) return

!     Set quality control flags for pressure obs
!     ------------------------------------------
      if ( combine_QM .or. get_P_QC ) then
         do iLev = 1, NLev
            tP_QC ( iLev ) = QC_OK      ! Initialize QC to valid
            if ( P_Missing ( iLev )) tP_QC ( iLev ) = QC_Rejected
         end do
         call PrepQMtoQC ( P_QM ( : NLev ), tP_QC,
     .                     reject_OIQC = reject_OIQC )
         if ( get_P_QC ) then
            do iLev = 1, NLev
               P_QC ( iLev ) = tP_QC ( iLev )
            end do
         end if
      end if

!     ... observed values
!     -------------------
      if ( get_ObQC ) then
         do iVar = 1, NVar
            do iLev = 1, NLev
               tObQM ( iLev )  =  ObQM  ( iVar, iLev )
               tObQC ( iLev )  =  QC_OK ! Initialize QC to valid.
               if ( ObMissing ( iVar, iLev ))
     .            tObQC ( iLev ) = QC_Rejected
            end do
            call PrepQMtoQC ( tObQM ( : NLev ), tObQC,
     .                        reject_OIQC = reject_OIQC )
            if ( get_ObQC ) then
               do iLev = 1, NLev
                  ObQC ( iVar, iLev ) = tObQC ( iLev )
               end do
            end if
         end do
      end if

!     If the resulting QC or QM marks are to be derived
!     from both pressure ob and observed value ...
!     -------------------------------------------------
      if ( combine_QM ) then
         do iVar = 1, NVar

!           ... initialize QC to the QC of the pressure ob
!           ----------------------------------------------
            do iLev = 1, NLev
               tObQM ( iLev )  =  ObQM  ( iVar, iLev )
                 tQC ( iLev )  =  tP_QC       ( iLev )
               if ( ObMissing ( iVar, iLev )) tQC ( iLev ) = QC_Rejected
            end do

!           ... convert the QM to the QC for ...
!           ------------------------------------
            call PrepQMtoQC ( tObQM ( : NLev ), tQC,
     .                        reject_OIQC = reject_OIQC )

!           ... the QC and/or ...
!           ---------------------
            if ( get_QC ) then
               do iLev = 1, NLev
                  QC ( iVar, iLev ) = tQC ( iLev )
               end do
            end if

!           ... the QM for the observed value
!           ---------------------------------
            if ( get_QM ) then
               do iLev = 1, NLev
                  QM ( iVar, iLev ) = P_QM ( iLev )
                  if ( tQC ( iLev ) .gt. tP_QC ( iLev ))
     .               QM ( iVar, iLev ) = tObQM ( iLev )
               end do
            end if
         end do
      end if

      return
      end subroutine Check_PrepQM

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: PrepQMtoQC () --- Convert prep quality marks (QM) to quality control (QC) flags
!
! !DESCRIPTION:
!     This converts QM from the prep buffer file to QC flags.  The
!     returned flag to OK if the Prep QM is 0 or 2, suspect if QM is 1
!     or 3, and rejected for all other values.  The flags are modified
!     only if the input value is less than the new value.  QM is also
!     modified and set to 17 if it is missing and 16 if it is invalid.
!
! !INTERFACE:
      subroutine PrepQMtoQC ( QM, Flag, OK, Suspect, Rejected,
     .                        reject_OIQC )
      use m_RadData, only : QC_OK, QC_Suspect, QC_Rejected
!
! !INPUT PARAMETERS:
      implicit   NONE
      real,                 intent (in) ::
     .   QM   (:)         ! Quality marks
      logical, optional,    intent (in) ::
     .   reject_OIQC      ! = .true. to reject observations that failed
                          !   IOQC.  Default: QIQC = .false. to classify
                          !   obs that failed IOQC as suspect.
!
! !INPUT/OUTPUT PARAMETERS:
      integer, optional,    intent (inout) ::
     .   Flag (:)         ! Modifified flags
!
! !OUTPUT PARAMETERS:
      integer, optional,    intent (out)   ::
     .   OK,              ! Returned codes for OK
     .   Suspect,         ! ... suspect
     .   Rejected         ! ... rejected observations
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Original code
!     20Aug2003  C. Redder   Changed type of input argument, QM, from
!                            integer, to real.
!     01Nov2003  C. Redder   Changed output parameter, Flag, to have the
!                            intent attribute, inout.  Output parameter,
!                            is modified only if new flag has a higher
!                            value than the previous value.
!     20Feb2004  C. Redder   Change the intent of QM from "in" to "inout"
!                            Added code to modify QM if QM is missing
!                            or is an invalid value.
!     22Apr2004  C. Redder   Added call to Test_RMiss
!     19May2004  C. Redder   Reversed modifications made on 20Feb2004.
!     30Aug2004  C. Redder   Added the optional input argument, reject_OIQC.
!                            Changed the name from Check_PrepQM to
!                            PrepQMtoQC.
! EOP
!-------------------------------------------------------------------------

      integer, parameter ::
     .   iQM_Min = 0,
     .   iQM_Max = nqcx_file
      real,    parameter ::
     .   Tol     =  0.00001
      real    :: QM_Min, QM_Max, QM_, Del, X
      integer :: Table ( iQM_Min : iQM_Max ), NObs, iOb, iList, iQM_,
     .           OutFlag
      logical :: Missing, in_valid_range, reject_OIQC_
c      Missing ( X ) = X .ge. BMiss_down .and.
c     .                X .le. BMiss_up

      if ( present ( OK       )) OK       = QC_OK
      if ( present ( Suspect  )) Suspect  = QC_Suspect
      if ( present ( Rejected )) Rejected = QC_Rejected

!     Return if return flag values are not desired
!     --------------------------------------------
      NObs = size ( QM )
      if ( NObs .le. 0 ) return

      reject_OIQC_ = .false.
      if ( present ( reject_OIQC )) reject_OIQC_ = reject_OIQC

!     Set up Flag table
!     -----------------
      Table = QC_Rejected
      do iList = 1, OK_ListSz
         Table ( OK_List      ( iList )) = QC_OK
      end do

      do iList = 1, Suspect_ListSz
         Table ( Suspect_List ( iList )) = QC_Suspect
      end do

      reject_OIQC_ = .false.
      if ( present ( reject_OIQC )) reject_OIQC_ = reject_OIQC
      if ( reject_OIQC_ ) then
         do iList = 1, OIQC_ListSz
            Table ( OIQC_List ( iList )) = QC_Rejected
         end do
      end if

!     Return the flag values for the given QM
!     ---------------------------------------
      Del    = real ( iQM_Max   - iQM_Min )
      QM_Min = real ( iQM_Min ) -  Tol * Del
      QM_Max = real ( iQM_Max ) +  Tol * Del
      do iOb = 1, NObs
         QM_            = QM ( iOb )
         in_valid_range = QM_ .ge. QM_Min .and.
     .                    QM_ .lt. QM_Max
         if ( .not. in_valid_range ) then
            Missing = QM_ .ge. BMiss_down .and.
     .                QM_ .le. BMiss_up
            if ( Missing ) then
               QM_ = QM_Missing
            else
               QM_ = QM_Unknown
            end if
         end if
         OutFlag = Table ( nint ( QM_ ))

c         QM ( iOb ) = QM_
         if ( present ( Flag ))
     .      Flag ( iOb ) = max ( Flag ( iOb ), OutFlag )
      end do

      return
      end subroutine PrepQMtoQC

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE  Get_BObs () --- Get observation from buffer file for latest and speciefied event
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Get_BObs ( lu, File, Options, NLev, Bufr,
     .                      RadInfo, T_VIRTMP, H_VIRTMP, DataType )
!
! !INPUT PARAMETERS:
      implicit   NONE
      integer,                intent (in) ::
     .   lu                 ! Logical unit number for Prep buffer file
      type ( prep_options ),  intent (in) ::
     .   Options            ! File options
      type ( pbufr_file ),    intent (in) ::
     .   File               ! File information.
      integer, optional,      intent (in) ::
     .   DataType           ! Data type.
                            !   =  0 for pressure  (default)
                            !   = -2 for winds
                            !   >  0 for the corresponding mass variable
!
!           Note: Any invalid value for ProgCode (in data structure,
!                 Options) and DataType is set to default.
!
! !INPUT/OUTPUT PARAMETERS:
      type ( NCEP_radcor_Binfo ), intent (inout), optional ::
     .   RadInfo            ! Radcor infor
      logical,                intent (inout), optional ::
     .   T_VIRTMP    ( : ), ! = .true. if virtual temperature was computed
     .   H_VIRTMP    ( : )  ! ... and humidity was re-computed by VIRTMP
!           Note: The routine does not change any value unless the
!                 appropriate data type is specified
!
! !OUTPUT PARAMETERS:
      integer,                intent (out) ::
     .   NLev               ! Number of levels of data returned
      real ( kind = BKind ),  intent (out) ::
     .    Bufr    ( :, : )  ! Data from the latest event
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     01Mar2004  C. Redder  Original code
!     03May2004  C. Redder  Added the arguments, Options, File, HCor and
!                           ZCor and H_VIRTMP.  Removed the optional
!                           input arguments, latest_QM and EvnCode.
!
! EOP
!-------------------------------------------------------------------------

      integer, parameter ::
     .   Wind     = -2,
     .   Pressure =  0,
     .   Latest   =  0
      real ( kind = BKind ), parameter ::
     .   BM_Temp  = MassB ( iVarTemp ) / MassM ( iVarTemp ),
     .   TK_min   = 1.0_BKind  ! Set (in deg K) to prevent division by zero
      real ( kind = BKind ), parameter ::
     .   BM_min   = TK_min             / MassM ( iVarTemp )

      integer :: DType, iLev, iMne, iMneOb, iMnePC, iMneQM,
     .   NMne, NMne_Out, PCode, PCode_Max, PCode0,
     .   Indx_, Indx1_, Indx2_, NEvn, iEvn, Indx ( NLev_Max ),
     .   Indx_first ( NLev_Max ), Indx_last ( NLev_Max ),
     .   Rad_PCode, VT_PCode, CQC_PCode
      real ( kind = BKind ) :: B_PCode_Max, B_PCode, B_PCode0, BVal,
     .                         Diff, Diff_VIRTMP,    Diff_after,
     .                         Ob_RADCOR, Ob_before, Ob_first, Ob_last,
     .                         Ob_VIRTMP, Ob_after,
     .                         T_RADCOR,  VT_VIRTMP, VT_after
      real ( kind = BKind ), target  ::
     .   MBufr  ( MassNObMne, NLev_Max, NPrepEvn_Max ),
     .   WBufr  ( WindNObMne, NLev_Max, NPrepEvn_Max ),
     .   PCBufr             ( NLev_Max, NPrepEvn_Max )
      real ( kind = BKind ), pointer ::
     .   XBufr ( :, :, : )
      character (len = max ( len (MassObStr),
     .                       len (MassPStr),
     .                       len (WindObStr),
     .                       len (WindPStr))) :: ObStr
      logical :: Missing ( NLev_Max ), no_more_events, scan,
     .           get_latest_QM, get_ZCor, get_TCor, get_HCor,
     .           get_AirTemp, get_T_VIRTMP, get_H_VIRTMP, get_event,
     .           get_all_events
!
!     Impliment options
!     -----------------
      PCode0      =  File % LastPCode
      if ( PCode0 .gt. NPrepEvn_Max .or.
     .     PCode0 .lt. Latest )  PCode0 = Latest
      B_PCode0    =  real ( PCode0, kind = BKind )

      get_latest_QM =  Options % get_latest_QM

      DType       = Pressure
      if ( present ( DataType )) DType  = DataType

      if ( DType .eq. Wind ) then
         NMne     =  WindNObMne
         iMneOb   = iWindMneUOb
         iMnePC   = iWindMnePC
         iMneQM   = iWindMneQM
         XBufr    => WBufr
         ObStr    =  WindObStr
      else
         NMne     =  MassNObMne
         iMneOb   = iMassMneOb
         iMnePC   = iMassMnePC
         iMneQM   = iMassMneQM
         XBufr    => MBufr
         if ( DType .eq. Pressure ) then
            ObStr =  MassPStr
         else
            ObStr =  MassObStr ( DType )
         end if
      end if

!     Get file info
!     -------------
      Rad_PCode   = File % Rad_PCode
       VT_PCode   = File %  VT_PCode

!     Get buffer data from buffer
!     ---------------------------
      call UFBINT    ( lu, XBufr, NMne, NLev_Max, NLev, ObStr )

      NMne_Out = min ( NMne, size ( Bufr, dim = 1 ))
      do iMne = 1, NMne_Out
      do iLev = 1, NLev
          Bufr ( iMne, iLev ) = XBufr ( iMne, iLev, 1 )
      end do
      end do

!     Initialize output arguments for reversing radcor
!     ------------------------------------------------
      get_ZCor = present ( RadInfo )     .and.
     .           DType   .eq. iVarHeight
      get_TCor = present ( RadInfo )     .and.
     .           DType   .eq. iVarTemp
      get_HCor = present ( RadInfo )     .and.
     .           DType   .eq. iVarHum
      if ( get_ZCor ) then
         do iLev = 1, NLev
            RadInfo % ZCor_RADCOR ( iLev ) = 0.0
         end do
      end if
      if ( get_TCor ) then
         do iLev = 1, NLev
            RadInfo % TCor_preVT  ( iLev ) = 0.0
            RadInfo % TCor_RADCOR ( iLev ) = 0.0
         end do
      end if
      if ( get_HCor ) then
         do iLev = 1, NLev
            RadInfo % HCor_VIRTMP ( iLev ) = 0.0
            RadInfo % HCor_after  ( iLev ) = 0.0
         end do
      end if

!     ... for setting flags for the program VIRTMP
!     --------------------------------------------
      get_T_VIRTMP   = present ( T_VIRTMP )    .and.
     .                 DType .eq. iVarTemp
      if ( get_T_VIRTMP ) then
         do iLev = 1, NLev
            T_VIRTMP ( iLev ) = .false.
         end do
      end if

      get_H_VIRTMP   = present ( H_VIRTMP )    .and.
     .                 DType .eq. iVarHum
      if ( get_H_VIRTMP ) then
         do iLev = 1, NLev
            H_VIRTMP ( iLev ) = .false.
         end do
      end if

!     Read all events from the buffer file only
!     if certain restricted situations are met.
!     -----------------------------------------
      get_event    = PCode0  .ne.  0
      get_ZCor     = get_ZCor                .and.
     .             ( PCode0  .ge.  Rad_PCode .or.
     .               PCode0  .eq.  0 )
      get_TCor     = get_TCor                .and.
     .             ( PCode0  .ge.  Rad_PCode .or.
     .               PCode0  .eq.  0 )
      get_HCor     = get_HCor                .and.
     .             ( PCode0  .ge.   VT_PCode .or.
     .               PCode0  .eq.  0 )
      get_AirTemp  = DType   .eq. iVarTemp   .and.
     .             ( PCode0  .ge.   VT_PCode .or.
     .               PCode0  .eq.  0 )

      if ( .not. get_event    .and.
     .     .not. get_ZCor     .and.
     .     .not. get_TCor     .and.
     .     .not. get_AirTemp  .and.
     .     .not. get_T_VIRTMP .and.
     .     .not. get_H_VIRTMP ) return

!     Scan the program codes ...
!     --------------------------
      B_PCode_Max = 0.0_BKind
      do iLev = 1, NLev
         B_PCode = Bufr ( iMnePC, iLev )
         if ( B_PCode .lt. BMiss_down .or.
     .        B_PCode .gt. BMiss_up )
     .      B_PCode_Max =  max ( B_PCode_Max, B_PCode )
      end do
      PCode_Max = nint ( B_PCode_Max )

!     ... and to determine if further processing is necessary
!     -------------------------------------------------------
      get_event      = get_event    .and.
     .                 PCode_Max    .ge.     PCode0

      get_ZCor       = get_ZCor     .and.
     .                 PCode_Max    .ge. Rad_PCode

      get_TCor       = get_TCor     .and.
     .                 PCode_Max    .ge. Rad_PCode

      get_HCor       = get_HCor     .and.
     .                 PCode_Max    .ge. Rad_PCode

      get_AirTemp    = get_AirTemp  .and.
     .                 PCode_Max    .ge.  VT_PCode

      get_T_VIRTMP   = get_T_VIRTMP .and.
     .                 PCode_Max    .ge.  VT_PCode

      get_H_VIRTMP   = get_H_VIRTMP .and.
     .                 PCode_Max    .ge.  VT_PCode

      get_all_events = get_event    .or.
     .                 get_ZCor     .or.
     .                 get_TCor     .or.
     .                 get_HCor     .or.
     .                 get_AirTemp  .or.
     .                 get_T_VIRTMP .or.
     .                 get_H_VIRTMP
      if ( .not. get_all_events ) return ! Nothing more to do

!     Get all relevant events events from buffer file
!     -----------------------------------------------
      if ( get_all_events ) then
         NEvn = NPrepEvn_Max
         call UFBEVN  ( lu, XBufr, NMne, NLev_Max, NEvn, NLev, ObStr )
         do iEvn = 1, NEvn
         do iLev = 1, NLev
            PCBufr ( iLev, iEvn ) = XBufr ( iMnePC, iLev, iEvn )
         end do
         end do
      else
         return
      end if

!     If data for a specified event is desired then
!     ---------------------------------------------
      if ( get_event ) then

!        Locate the last data at or before the specified event
!        -----------------------------------------------------
         call IndexEvn ( PCode0, PCBufr, Indx ( : NLev ),
     .                   before_Evn = .true. )

!        Store data for specified event into output buffer
!        -------------------------------------------------
         do iMne = 1, NMne_Out
         do iLev = 1, NLev
            Indx_ = Indx ( iLev )
            BVal = BMiss
            if ( Indx_ .ne. 0 ) BVal = XBufr ( iMne, iLev, Indx_ )
            Bufr ( iMne, iLev ) = BVal
         end do
         end do
      end if

!     If it is necessary to convert virtual
!     temperature to air temperature then
!     -------------------------------------
      if ( get_AirTemp  .or.
     .     get_T_VIRTMP ) then

!        Locate the data at the specified event
!        --------------------------------------
         call IndexEvn ( VT_PCode, PCBufr, Indx ( : NLev ))

!        Compute the air temperature
!        ---------------------------
         do iLev = 1, NLev
            Indx_= Indx ( iLev )
            if ( Indx_ .gt. 0 ) then
               T_RADCOR = XBufr ( iMneOb, iLev, Indx_ + 1 )
              VT_VIRTMP = XBufr ( iMneOb, iLev, Indx_ )
              VT_after  =  Bufr ( iMneOb, iLev )
               if ((  T_RADCOR .lt. BMiss_down .or.
     .                T_RADCOR .gt. BMiss_up ) .and.
     .             ( VT_VIRTMP .lt. BMiss_down .or.
     .               VT_VIRTMP .gt. BMiss_up ) .and.
     .             ( VT_after  .lt. BMiss_down .or.
     .               VT_after  .gt. BMiss_up )) then
                   T_RADCOR =  T_RADCOR + BM_Temp
                  VT_VIRTMP = VT_VIRTMP + BM_Temp
                  VT_after  = VT_after  + BM_Temp
                  if ( get_AirTemp )
     .               Bufr ( iMneOb, iLev ) = ( T_RADCOR / VT_VIRTMP )
     .                                       * VT_after - BM_Temp
                  if ( get_T_VIRTMP )
     .               T_VIRTMP     ( iLev ) = .true.
               end if
            end if
         end do
      end if

!     If radcor corrections are desired then
!     --------------------------------------
      if ( get_ZCor .or. get_TCor ) then

!        Locate the data at the RADCOR event
!        -----------------------------------
         call IndexEvn ( Rad_PCode, PCBufr, Indx  ( : NLev ))

!        ... compute the corrections by radcor
!        -------------------------------------
         do iLev = 1, NLev
            Indx_  = Indx ( iLev )
            if ( Indx_ .gt. 0 ) then
               Ob_before = XBufr ( iMneOb, iLev, Indx_ + 1 )
               Ob_RADCOR = XBufr ( iMneOb, iLev, Indx_ )
               if (( Ob_before .lt. BMiss_down .or.
     .               Ob_before .gt. BMiss_up ) .and.
     .             ( Ob_RADCOR .lt. BMiss_down .or.
     .               Ob_RADCOR .gt. BMiss_up )) then
                  Diff = Ob_before - Ob_RADCOR
                  if ( get_ZCor ) RadInfo % ZCor_RADCOR ( iLev ) = Diff
                  if ( get_TCor ) RadInfo % TCor_RADCOR ( iLev ) = Diff
               end if
            end if
         end do

!        ... and by all events
!        ---------------------
         if ( get_TCor ) then

!           ... from the beginning
!           ----------------------
            call IndexEvn ( 0,        PCBufr, Indx_first ( : NLev ))

!           ... to the last event prior to VIRTMP
!           -------------------------------------
            call IndexEvn ( VT_PCode, PCBufr, Indx_last  ( : NLev ),
     .                      before_Evn = .true.,
     .                      not_at_Evn = .true. )


            do iLev = 1, NLev
               Indx1_ = Indx_first ( iLev )
               Indx2_ = Indx_last  ( iLev )
               Diff   = 0.0
               if ( Indx1_ .ne. 0 .and.
     .              Indx2_ .ne. 0 ) then
                  Ob_first =  XBufr ( iMneOb, iLev, Indx1_ )
                  Ob_last  =  XBufr ( iMneOb, iLev, Indx2_ )
                  if (( Ob_first   .lt. BMiss_down .or.
     .                  Ob_first   .gt. BMiss_up ) .and.
     .                ( Ob_last    .lt. BMiss_down .or.
     .                  Ob_last    .gt. BMiss_up ))
     .               Diff = Ob_first - Ob_last
               end if
               RadInfo % TCor_preVT ( iLev ) = Diff
            end do
         end if
      end if

!     If flags for the VIRTMP program are to be returned, then ...
!     ------------------------------------------------------------
      if ( get_H_VIRTMP .or. get_HCor ) then

!        ... locate the data at the specified event
!        ------------------------------------------
         call IndexEvn ( VT_PCode, PCBufr, Indx ( : NLev ))

!        ... and determine if the VIRTMP program was executed
!        ----------------------------------------------------
         do iLev = 1, NLev
            H_VIRTMP ( iLev ) = Indx ( iLev ) .ne. 0
         end do

!        ... and obtain the humidity corrections
!        ---------------------------------------
         if ( get_HCor ) then
            do iLev = 1, NLev
               Indx_       = Indx ( iLev )
               Diff_VIRTMP = 0.0
               Diff_after  = 0.0
               if ( Indx_ .ne. 0 ) then
                  Ob_before =  XBufr ( iMneOb, iLev, Indx_ + 1 )
                  Ob_VIRTMP =  XBufr ( iMneOb, iLev, Indx_ )
                  Ob_after  =  XBufr ( iMneOb, iLev, 1     )
                  if    (( Ob_VIRTMP .lt. BMiss_down .or.
     .                     Ob_VIRTMP .gt. BMiss_up )) then
                     if (( Ob_before .lt. BMiss_down .or.
     .                     Ob_before .gt. BMiss_up ))
     .                  Diff_VIRTMP = Ob_before - Ob_VIRTMP
                     if (( Ob_after  .lt. BMiss_down .or.
     .                     Ob_after  .gt. BMiss_up ))
     .                  Diff_after  = Ob_VIRTMP - Ob_after
                  end if
               end if
               RadInfo % HCor_VIRTMP ( iLev ) = Diff_VIRTMP
               RadInfo % HCor_after  ( iLev ) = Diff_after
            end do
         end if
      end if

!     If desired, get QM for the latest event
!     ---------------------------------------
      if ( get_latest_QM ) then
         do iLev = 1, NLev
            Bufr ( iMneQM, iLev ) = XBufr ( iMneQM, iLev, 1 )
         end do
      end if

      return
      end subroutine Get_BObs
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE  IndexEvn () --- Search for the last prep buffer event(s) with the specified program code.
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine IndexEvn ( PCode, PCodes, Indx,
     .                      before_Evn, not_at_Evn )
!
! !INPUT PARAMETERS:
      implicit   NONE
      integer,               intent (in)  ::
     .   PCode               ! Program code for specified event.  For
                             !   earliest event, set PCode = 0.
      real ( kind = BKind ), intent (in),  dimension (:,:) ::
     .   PCodes              ! Program codes
      logical, optional,     intent (in)  ::
     .   before_Evn,         ! = .true. select all before the specified
     .                       !   event.  Default:  before_Evn = .false.
     .   not_at_Evn          ! = .true. to exclude the specified event.
                             !   Default: before_Evn = .false.
! !OUTPUT PARAMETERS:
      integer,               intent (out), dimension (:)   ::
     .   Indx                ! Index for selected event.  If the event
                             !   is not found, then zero is returned.
!
!     note: The number of observations is determine by the minimum size
!           of the first dimension of all arrays.  The number of events
!           is determined by size of the second dimension of the array
!           PCodes.
! !SEE ALSO:
!
! !REVISION HISTORY:
!     28Apr2004  C. Redder   Origional code
! EOP
!-------------------------------------------------------------------------

      integer :: iEvn, NEvn, iOb, NObs, Indx_
      real ( kind = BKind ) :: PCode_, PCode_down, PCode_up
      logical :: no_more_events, scan, find_earliest_event

!     Get the number of events and obs
!     --------------------------------
      NEvn =       size ( PCodes, dim = 2 )
      NObs = min ( size ( PCodes, dim = 1 ), size ( Indx ))

!     Set upper and lower range of program codes for selected
!     event which will be used for comparisons of reals numbers
!     ---------------------------------------------------------
      PCode_down  =       real (  PCode, kind = BKind )
     .            - Tol * real (  NEvn,  kind = BKind )
      PCode_up    =       real (  PCode, kind = BKind )
     .            + Tol * real (  NEvn,  kind = BKind )

!     Impliment options
!     -----------------
      if ( present ( before_Evn )) then
         if ( before_Evn )
     .      PCode_down  = 1.0_BKind
     .                  - Tol * real (  NEvn,  kind = BKind )
      end if
      if ( present ( not_at_Evn )) then
         if ( not_at_Evn )
     .      PCode_up    =       real (  PCode, kind = BKind )
     .                  - Tol * real (  NEvn,  kind = BKind )
      end if

      find_earliest_event = PCode .eq. 0

!     Initialize indices
!     ------------------
      do iOb = 1, NObs
         Indx ( iOb ) = 0
      end do

!     Find the ...
!     ------------
      do iEvn = 1, NEvn
         no_more_events = .true.

!        ... earliest event
!        ------------------
         if ( find_earliest_event ) then
            do iOb  = 1, NObs
               PCode_ = PCodes ( iOb, iEvn )
               Indx_  = Indx   ( iOb )
               scan   = Indx_ .ge. 0  ! Continue to scan data that is
               if ( scan ) then       !   non-missing
                  if ( PCode_ .lt. BMiss_down  .or.
     .                 PCode_ .gt. BMiss_up   ) then
                     Indx_ =  iEvn    ! ... as long as valid event are
                  else                !   detected or ...
                     Indx_ = -Indx_ - 1 ! ... until first missing data for
                  end if              !   the level is detected
                  if ( no_more_events ) no_more_events = Indx_ .lt. 0
                  Indx ( iOb ) = Indx_
               end if
            end do

            if ( no_more_events ) then
               do iOb = 1, NObs
                  Indx_ = Indx ( iOb )
                  if ( Indx_ .lt. 0 ) Indx ( iOb ) = 0 - Indx_ - 1
               end do
               exit
            end if
         else

!           ... desired event
!           -----------------
            do iOb  = 1, NObs
               PCode_ = PCodes ( iOb, iEvn )
               Indx_  = Indx   ( iOb )
               scan   = Indx_ .eq. 0  ! Restrict scans to those sections
               if ( scan ) then       !   that may have the unlocated event
                  if      ( PCode_ .ge. PCode_down  .and.
     .                      PCode_ .le. PCode_up   ) then
                     Indx_  = iEvn    ! Event found
                  else if ( PCode_ .lt. PCode_down ) then
                     Indx_  = -1      ! Event earlier than selected event.
                                      !   No more events need to be searched.
                  else if ( PCode_ .ge. BMiss_down  .and.
     .                      PCode_ .le. BMiss_up   ) then
                     Indx_  = -1      ! Missing value indicates no more
                  end  if             !   events for the corresponding ob
                  if ( no_more_events ) no_more_events = Indx_ .ne. 0
                  Indx ( iOb ) = Indx_
               end if
            end do
            if ( no_more_events ) then
               do iOb = 1, NObs
                  Indx_ = Indx ( iOb )
                  if ( Indx_ .eq. -1 ) Indx ( iOb ) = 0
               end do
               exit
            end if
         end if
      end do

!     Reset all negative indices to zero
!     ----------------------------------

      return
      end subroutine IndexEvn

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE  SelectEvn () --- Select lastest 2-D data before selected event
!
! !DESCRIPTION:
!
! !INTERFACE:
!      subroutine SelectEvn ( iPCode, rPCodes, Indx )
!
! !INPUT PARAMETERS:
!      implicit   NONE
!      integer,               intent (in)  ::
!     .   iPCode              ! Program code for selected event
!      real ( kind = BKind ), intent (in),  dimension (:,:) ::
!     .   rPCodes             ! Program codes
!
! !OUTPUT PARAMETERS:
!      integer,               intent (out), dimension (:)   ::
!     .   Indx                ! Event index for selected data.  If no
!                             !   suitable event is found, then zero is
!!                            !   returned for that Ob
! !SEE ALSO:
!
! !REVISION HISTORY:
!     24Feb2004  C. Redder   Origional code
!
! EOP
!-------------------------------------------------------------------------
!
!      real (kind = BKind) :: rPCode_down, X, rPCode, NEvn_up
!      integer :: NEvn, NObs, iEvn, iOb, Indx_
!      logical :: Missing, scan
!      Missing ( X ) = X .ge. BMiss_down .and.
!     .                X .le. BMiss_up
!
!     Initialize
!     ----------
!      NEvn =       size ( rPCodes, dim = 2 )
!      NObs = min ( size ( rPCodes, dim = 1 ), size ( Indx ))

!      rPCode_down = ( 1.0_BKind - Tol ) * real ( iPCode )
!       NEvn_up    = ( 1.0       + Tol ) * real (  NEvn  )

!      do iOb = 1, NObs
!         Indx ( iOb ) = 0
!      end do
!
!     Get appropriate event
!     ---------------------
!      do iEvn = 1, NEvn
!      do iOb  = 1, NObs
!         rPCode = rPCodes ( iOb, iEvn )
!         Indx_  =   Indx  ( iOb )
!         scan   =   Indx_ .eq. 0
!         if ( scan ) then
!            if      ( rPCode .le. rPCode_down ) then
!               Indx_ = iEvn
!            else if ( rPCode .lt. NEvn_up    ) then
!               if ( Missing ( rPCode )) Indx_ = -1
!            end if
!            Indx ( iOb ) = Indx_
!         end if
!      end do
!      end do

!     Set Indx to zero if event is not found
!     --------------------------------------
!      do iOb = 1, NObs
!         Indx_ = Indx ( iOb )
!         if ( Indx_ .eq. -1 ) Indx ( iOb ) = 0
!      end do
!      end subroutine SelectEvn

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Get_UPAMeta () --- Get meta data for a raob sounding from Prep buffer file
!
! !DESCRIPTION:
!
! !INTERFACE:
!      subroutine Get_UPAMeta ( lu, SynJHr, Meta, accept )
!      use m_SunAlt,   only : Julian
!
! !INPUT PARAMETERS:
!      implicit   NONE
!      integer,             intent (in)  ::
!     .   lu,               ! Logical unit number for Prep buffer file
!     .   SynJHr            ! Synoptic Julian hour
!
! !OUTPUT PARAMETERS:
!      type ( pbufr_meta ), intent (out) ::
!     .   Meta              ! station meta data
!      logical,             intent (out) ::
!     .   accept            ! = .true. to accept meta data
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     17Apr2003  C. Redder   Origional code
!
! EOP
!-------------------------------------------------------------------------
!
!      real ( kind = BKind ) :: HDR1 ( 8 ), HDR2 ( 3 ), HDR3 ( 1 ), X
!      integer :: TypeMax
!      character ( len = * ), parameter ::
!     .    HDStr1  = 'RPID YEAR MNTH DAYS HOUR UALNHR UALNMN SIRC',
!     .    HDStr2  = 'CLAT CLON SELV',
!     .    HDStr3  = 'RATP'
!      character ( len = LenID ) :: CStnID
!      integer :: SynMin, RepYMD, DHr, LMin
!      integer :: Type,   LTime,  RadCode, NLev, ObTime
!      logical :: Missing
!      Missing ( X ) = X .ge. BMiss_down .and.
!     .                X .le. BMiss_up
!      equivalence ( CStnID, HDR1 )
!
!     Get synoptic minutes
!     --------------------
!      SynMin = ( SynJHr - ( SynJHr / 24 ) * 24 ) * 60
!
!     Get the maximum number for the instrument type
!     ----------------------------------------------
!      call Raob_Types ( TypeMax )
!
!     Get header information including ...
!     ------------------------------------
!      call UFBINT ( lu, HDR1, 8, 1, NLev, HDStr1 )
!      call UFBINT ( lu, HDR2, 3, 1, NLev, HDStr2 )
!      call UFBINT ( lu, HDR3, 1, 1, NLev, HDStr3 )
!
!      if ( .not. Missing ( HDR2 ( 1 ))) then  ! ... latitude
!         Meta % Lat      = HDR2 ( 1 )         ! required info
!      else
!         accept = .false.
!      end if
!      if ( .not. Missing ( HDR2 ( 2 ))) then  ! ... longitude
!         Meta % Lon      = HDR2 ( 2 )         ! required info
!      else
!         accept = .false.
!      end if
!      if ( .not. Missing ( HDR2 ( 3 ))) then  ! ... longitude
!         Meta % Elev     = HDR2 ( 3 )         ! required info
!      else
!         accept = .false.
!      end if
!      if ( .not. Missing ( HDR1 ( 2 ))  .and. ! ... sampling hr
!     .     .not. Missing ( HDR1 ( 3 ))  .and. ! required info
!     .     .not. Missing ( HDR1 ( 4 ))  .and.
!     .     .not. Missing ( HDR1 ( 5 ))) then
!         RepYMD        = nint ( HDR1 ( 2 )) * 10000
!     .                 + nint ( HDR1 ( 3 )) * 100
!     .                 + nint ( HDR1 ( 4 ))
!         DHr           = ( Julian ( RepYMD ) - 1 ) * 24
!     .                 + nint ( HDR1 ( 5 ))
!     .                 - SynJHr
!         Meta % ObTime = DHr * 60 * 60        ! Convert to seconds
!      else
!         accept = .false.
!      end if
!      if ( .not. Missing ( HDR3 ( 1 ))) then  ! ... instr type
!         Meta % IType = nint ( HDR3 ( 1 ))
!      else
!         Meta % IType = TypeMax - 1
!      end if
!      LTime   = IMiss                         ! ... launch time
!      if ( accept                      .and.
!     .    .not. Missing ( HDR1 ( 6 ))  .and.
!     .    .not. Missing ( HDR1 ( 7 ))) then
!         LMin = nint ( HDR1 ( 6 ) * 60.0 + HDR1 ( 7 ))
!         if       ( LMin - SynMin .gt.  12 * 60 ) then
!            LTime = LMin - SynMin - 24 * 60
!         else if  ( LMin - SynMin .le. -12 * 60 ) then
!            LTime = LMin - SynMin + 24 * 60
!         else
!            LTime = LMin - SynMin
!         endif
!         ObTime = Meta % ObTime / 60
!         if ( LTime .lt. ObTime - 60 .or.     ! ... range check.
!     .        LTime .gt. ObTime + 60 )        !   LTime must be
!     .      LTime = IMiss                     !   within 1 hr of
!      end if                                  !   Time.
!!      Meta % LTime = LTime
!!      if ( LTime .ne. IMiss )                 ! Convert to seconds
!!     .   Meta % LTime = Meta % LTime * 60
!
!      RadCode = IMiss                         ! ... radiation
!      if ( .not. Missing ( HDR1 ( 8 )))       !   bias correction
!     .   RadCode = nint  ( HDR1 ( 8 ))        !   code
!      Meta % RadCode = RadCode
!
!      Meta % SeqN    = IMiss                  ! ... set sequence number
!                                              !   to missing
!      Meta % ProcN   = IMiss                  ! ... set sequence number
!                                              !   to missing
!      return
!      end subroutine Get_UPAMeta
!
!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Raob_Types - Get table of radiosonde instrument types
!
! !INTERFACE:
!
      subroutine Raob_Types ( NTypes, Table )
      use m_RadLists, only : rkx_names
! !USES:

      implicit none

! !OUTPUT PARAMETERS:
      integer,           intent (out), optional ::
     .   NTypes          ! Number of entries in the table
      character (len=*), intent (out), dimension (:), optional ::
     .   Table           ! Discription of each radiosonde instrument type

! !DESCRIPTION:
!    \label{MODS:RaobTypes}
!     Returns the number of radiosonde instrument types and, if, desired
!     the description of each type.
!
!    !REVISION HISTORY:
!     21Sep2001  C Redder  Orininal code
!     21Nov2006  C.Redder  Modification implemented to use list from
!                          module, m_RadLists
!EOP
!-------------------------------------------------------------------------

      integer, parameter :: TypeMax = size ( rkx_names )
      character (len=*), parameter, dimension (TypeMax) ::
     .    TypeList = rkx_names

      NTypes = size ( TypeList )
      if ( present ( Table )) Table = TypeList

      return
      end subroutine Raob_Types
!.................................................................

      end module m_RadBufr
!====================================================================
