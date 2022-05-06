program init_coeffs
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: init_coeffs:  generate radiance bias correction coefficients
!
! !INTERFACE:
!
!     Usage:  init_coeffs.x [option-flags] expid yyyymmddhh 
!
! !USES:
!
  use kinds, only : r_kind,i_kind,r_quad,r_single
  use m_sischnTable, only : sischnTable
  use m_sischnTable, only : sischnTable_build
  use m_sischnTable, only : sischnTable_verify
  use m_sischnTable, only : sischnTable_locate
  use m_sischnTable, only : sischnTable_clean
  use m_FileResolv,  only : FileResolv
  use read_diag
  use mpeu_util, only: gettablesize
  use mpeu_util, only: gettable
  implicit NONE

!
! !DESCRIPTION:  Read radiance diag files to create initial set of
!                polynomial cross-track bias coefficients for 
!                radiance bias correction.  Based on Yanqiu Zhu's
!                code in GSI - but run offline before initial analysis.
!                
!
! !REVISION HISTORY:
!
!     12Mar2015    Meta   Initial version of offline code loosely 
!                          based on 'sac.x'
!     summer2015   Meta   various updates and bug fixes
!     30Jan2017    Meta   spawn from original offline program.  Get
!                          tlapmean from gmao_global_tlapmean.rc
!     24Mar2017    Meta   remove namelist dependence, add command
!                         line switches. 
!      5Apr2017    Meta   clean old (commented-out) vestiges of MPI  
!      5May2017    Meta   modify for changes in satinfo (airs and iasi
!                          names and seviri channel numbering)
!      6Oct2017    Meta   initialize cbiasx to zero for each entry;
!                         try -not- zeroing ostats3, to carry old
!                         values from input file to output file
!                         (if coeffs are not being set/reset)
!     14Oct2017    Meta   access array of sis/chn ranking, obtain array of unique
!                         sis names to use when accessing tables
!     13Oct2017    Meta   read dtype/dplat from gsi.rc.tmpl instead of using
!                         the file name components from gsidiags.rc.tmpl
!     16Nov2017    Meta   Changes to use gsi.rc.tmpl in place of gsidiags.rc
!                         to get info for diag file names (dtype/dplat).  
!                         Restructuring to avoid opening every diag file.
!     21Nov2017    Meta   Modified to skip channels from the input diag file
!                         missing from satinfo (instead of error exit) 
!     20Apr2018    Meta   Modified to write changes out only for channels
!                         that have modified coefficients 
!     25Apr2018    Meta   Modify to write updated tlapmean values even
!                         when coefficients are not changed.
!     27Apr2018    Meta   Added mode '-2', read in satbias coefficient
!                         file and write satbias_pc file with background
!                         error set to 1.e4  (give large error to coefs
!                         to force adjustment as in e.g. reusing
!                         coefficents from a different date/time/experiment
!     30Apr2018    Meta   only process 'coeff' files from satellite/insts
!                         that have been marked as updated.  (Avoids using
!                         any stray 'coeff' files left in work area.)
!      9May2018    Meta   Add '-qc' flag to use the QC decisions from the 
!                         input diag file in choosing which obs to use
!                         when fitting the coefficents.  Using errinv<1e-6
!                         because data_chan(j)%qcmark may not always be set
!     31May2018    Meta   Add 'avhrr' to the list of mean_only instruments
!                         to avoid any problem with fixed-angle fitting
!      6Jun2018    Meta   Extra check for '999' in ntl field for tlapmean
!                         with tlapmean value == 0.0.  Set these 999 to 0
!                         so that tlapmean can be initialized.
!EOP
!-----------------------------------------------------------------------

  type(sischnTable):: satinfo_table
  type(sischnTable):: scantable

! Globally used variables
   real(r_kind),allocatable,dimension(:):: radstart    ! starting scan angle
   real(r_kind),allocatable,dimension(:):: radstep     ! step of scan angle
   integer(i_kind),allocatable,dimension(:):: radnstep    ! nstep of scan angle
   
   integer(i_kind),allocatable,dimension(:):: radedge1    ! cut-off of edge removal
   integer(i_kind),allocatable,dimension(:):: radedge2    ! cut-off of edge removal
   character(len=20),allocatable,dimension(:):: radsis    ! satellite/instrument/sensor
                                                          ! for radinfo
   integer(i_kind),allocatable,dimension(:):: ones

   integer(i_kind) angord        ! order of polynomial for angle bias correction
   integer(i_kind) npred         ! number of radiance biases predictors to write out
   real(r_kind),allocatable,dimension(:,:):: predx           ! coefficients
                                                             ! for predictor
                                                             ! part of bias
                                                             ! correction
   real(r_kind),allocatable,dimension(:,:):: coef2,coef3     ! coefficient storage


   integer(i_kind) :: maxscan

! Environment variables
    character(len=250) :: fvhome
    character(len=250) :: fvroot

! file names
    character(len=220) :: scanfile
    character(len=300) :: gsirc
    character(len=300) :: satinfo
    character(len=300) :: tlapfile
    character(len=300) :: satbias_in
    character(len=300) :: satbias_pc


! Declare local parameters
  integer(i_kind),parameter:: lninfo = 11
  integer(i_kind),parameter:: lncoef = 14
  integer(i_kind),parameter:: lnberr = 16
  integer(i_kind),parameter:: lndiag = 21
  integer(i_kind),parameter:: lntemp = 51 
  integer(i_kind),parameter:: lnupdt = 52
  integer(i_kind),parameter:: lngtbl = 53

  integer(i_kind),parameter:: maxchn = 3000
  integer(i_kind),parameter:: maxdat = 200
  integer(i_kind),parameter:: nthreshold = 100

  real(r_kind),parameter:: zero = 0.0_r_kind
  real(r_kind),parameter:: one = 1.0_r_kind
  real(r_kind),parameter::  r10       = 10.0_r_kind
  real(r_kind),parameter::  atiny     = 1.0e-10_r_kind
  real(r_quad),parameter::  zero_quad = 0.0_r_quad
  real(r_kind),parameter::  two       = 2.0_r_kind
  real(r_kind),parameter::  three     = 3.0_r_kind


! Declare local variables
  logical lexist,pexist,done,update_coeff,retrieval
  logical use_edges
  logical ntlupd
  logical,allocatable,dimension(:):: update_tlapmean, lnew, lupdt
  logical update_file
  logical mean_only
  logical verbose, wrinit
  logical use_iuse, use_qc

  character(6) :: word
  character(15):: obsname         ! obstype_platid
  character(15):: obstype
  character(11):: dstring
  character(20):: satsens,satsens_save
  character(50):: fstring
  character(15):: string
  character(200):: diag_rad,dname,sname,fname
  character(20),allocatable,dimension(:):: satsensor0,satsensor2,satsensor3,sislist
  character(120) :: prefix
  character(160):: ftemplate
  character(40) :: dtemplate
  character(20):: expid
  character(10) :: datestr
  character(10),allocatable,dimension(:):: dtype,dplat

  integer(i_kind):: ix,ii,iii,iiii,irdind,ndat,ich,ici,ndatppe
  integer(i_kind):: nstep = 90 ! Default value which may be over-written by namelist
                               ! and by the satang file itself
  integer(i_kind):: i,j,k,n_chan,np,nc,n,istr,lsis, irj, nchans
  integer(i_kind):: jpch,ierror,jj,ntime,it,ierror_code, ier
  integer(i_kind):: istatus,ispot
  integer(i_kind),dimension(maxchn):: io_chan
  integer(i_kind):: mxchn
  integer(i_kind),dimension(maxdat):: ipoint
  integer(i_kind),allocatable,dimension(:):: jchanum0,jchanum2,jchanum3
  integer(i_kind),allocatable,dimension(:):: inew, iobs, iorder
  integer(i_kind),allocatable,dimension(:):: ntl,ntl2,ntl3
  integer(i_kind),allocatable,dimension(:):: rankarray
  integer(i_kind),allocatable,dimension(:):: sisind, sischn
  integer(i_kind),allocatable,dimension(:):: chan_ptr
  integer(i_kind) :: radedge_min, radedge_max
  integer(i_kind) :: ntlapthresh
  integer(i_kind) :: nymd, nhms, ihh
  integer :: mode
  integer(i_kind) :: iptr_ch_start
  integer(i_kind) :: iuse_rad

  integer ngsircf

  type :: gsi_diag_type
     character(len=20) ::  sis      ! satellite/instrument/sensor string
     character(len=10) ::  dtype    ! instrument type
     character(len=10) ::  dplat    ! platform
     integer(i_kind)   ::  sisptr   ! pointer to sis in the sislist
     logical           ::  update   ! indicates type is updated 
  end type gsi_diag_type

  type (gsi_diag_type), allocatable, dimension(:) :: gsi_files



  character(len=20):: satsensor0_j
  integer(i_kind):: jchanum0_j
  integer(i_kind):: ntl_j
  real(r_kind):: tlap0_j, tlbe_j
  real(r_kind),allocatable,dimension(:):: varx, pred, predr
  real(r_kind),allocatable,dimension(:,:):: varA, var3

  real(r_kind):: scan,dtmax,resid,x,y,ratio,errinv,clw
  real(r_kind):: tlaptmp, rnad, pi, deg2rad, ostatsx
  real(r_kind),allocatable,dimension(:):: tlapmean,tlbe,tlap0,tlap1,tlap2,tlap3, tlapm
  real(r_kind),allocatable,dimension(:):: tsum0,tsum,tsum2,tsum3,tcnt
  real(r_quad),allocatable,dimension(:,:,:) :: A
  real(r_quad),allocatable,dimension(:,:) :: b
  real(r_kind),allocatable,dimension(:,:):: AA
  real(r_kind),allocatable,dimension(:):: be
  real(r_kind),allocatable,dimension(:)::cbiasx
  real(r_single) dth    !  r_single needed for call to determine_time_levels

  logical,allocatable,dimension(:):: inew_rad  ! indicator if it needs initialized for satellite radiance data

  integer(i_kind) nlines, nsis
  character(len=1)   :: cflg
  character(len=120) :: crecord

  real(r_kind),allocatable,dimension(:):: ostats,ostats3
  real(r_quad),allocatable,dimension(:,:):: rstats

! Declare types used for reading satellite data
  type(diag_header_fix_list )         :: header_fix
  type(diag_header_chan_list),allocatable :: header_chan(:)
  type(diag_data_name_list  )         :: data_name
  type(diag_data_fix_list   )         :: data_fix
  type(diag_data_chan_list  ),allocatable :: data_chan(:)
  type(diag_data_extra_list ),allocatable :: data_extra(:,:)

  integer(i_kind) :: max_pos

!************************************************************************

  retrieval=.false.     ! .true. if bisst present

  verbose = .false.
  dtemplate=''
  expid = ''
  mode = 1

  angord = 4
  npred = 8 + angord    

  ntlapthresh = 100

  use_edges = .false.
  pi      = acos(-one)
  deg2rad = pi/180.0_r_kind

  call init()


  write(6,*) 'angord, npred =',angord,npred

! Check for satinfo file
  inquire(file=satinfo,exist=lexist)
  if (.not. lexist) then
     write(6,'(''file '',a,'' does not exist'')') trim(satinfo)
     write(6,'(''***ERROR*** no satinfo file - exiting'')')
     ierror_code=95
!!$     call mpi_abort(mpi_comm_world,ierror_code,ierror)
     stop 95
  end if
!
!  read sis/chn from satinfo file
!  first get the number of entries in the file
  write(6,'(''reading file '',a)')trim(satinfo)
  open(lninfo,file=satinfo,form='formatted')
  j=0
  nlines=0
  read1: do
     read(lninfo,'(a1,a120)',iostat=istatus) cflg,crecord
     if (istatus /=0) exit
     nlines = nlines + 1
     if (cflg == '!') cycle
     j = j + 1
  end do read1
  if (istatus > 0) then
     close(lninfo)
     write(6,'(''***ERROR*** reading satinfo file, istatus='',i5)') istatus
     ierror_code=94
!!$     call mpi_abort(mpi_comm_world,ierror_code,ierror)
     stop 94
  end if

  jpch = j

  write(6,'(i6,a)') jpch,' entries found in the satinfo file'

! satsensor = sis  jchanum0 = channel number
! tlapmean = mean lapse rate  tlbe = bkg err for tlapmean
! ntl = no. of tlapmean estimates

  allocate(satsensor0(jpch), jchanum0(jpch))
  allocate(tlapmean(jpch), tlbe(jpch), ntl(jpch), update_tlapmean(jpch))
  allocate(coef3(npred,jpch),ostats3(jpch),var3(npred,jpch),varx(npred))
  allocate(inew_rad(jpch))

  inew_rad = .true.
  
  
  rewind(lninfo)
  j=0
  nsis = 0
  satsens_save = ''
  do jj = 1,nlines
     read(lninfo,'(a1,a120)') cflg,crecord
     if (cflg == '!') cycle
     j = j + 1
     read(crecord,*,iostat=istatus)satsensor0_j,jchanum0_j,iuse_rad
     if (istatus/=0) then
        write(6,*) '***ERROR*** reading satinfo file'
        ierror_code = 99
!!$        call mpi_abort(mpi_comm_world,ierror_code,ierror)
        stop 99
     end if
     satsensor0(j) = satsensor0_j
     jchanum0(j) = jchanum0_j
     if (satsensor0_j /= satsens_save) then
        satsens_save = satsensor0_j
        nsis = nsis + 1
     end if
     if (use_iuse) then
!!$        if (iuse_rad <= -2 .or. iuse_rad == 4) inew_rad(j) = .false.
        if (iuse_rad <= -2) inew_rad(j) = .false.
     end if
  end do

  print *, 'build and verify satinfo table'
  call sischnTable_build(satinfo_table,satsensor0(:jpch),jchanum0(:jpch))
  call sischnTable_verify(satinfo_table)
  
  allocate(sischn(nsis),sisind(nsis),sislist(nsis))
  allocate(dtype(nsis), dplat(nsis))
  allocate(rankarray(jpch))
 
! now get the index array for ranked sis/chn and find the unique sis & nchan
! sisind = index in 'rankarray' of start of sis
! sischn = number of channels of sis
  mxchn = 0
  satsens_save = ''
  lsis = 0
  jj = 0
  do j = 1,jpch       !  loop through all entries in the sorted array
     irj = sischnTable_locate(satinfo_table,j)
     rankarray(j) = irj
     if (satsens_save /= satsensor0(irj)) then
        satsens_save = satsensor0(irj)
        if (lsis > 0) then             ! save nchannel count from last sis
           sischn(lsis) = jj
           if (jj > mxchn) mxchn = jj
        end if
        lsis = lsis + 1    ! increment pointer in sis array
        sislist(lsis) = satsensor0(irj)
        sisind(lsis) = j   ! index of sis start in sorted array
        jj = 1             ! reset nchannel count
     else
        jj = jj + 1        ! increment nchannel count
     end if
        
  end do
  sischn(lsis) = jj
  if (jj > mxchn) mxchn = jj

  allocate(chan_ptr(mxchn))

  print *,'number of unique satellite instruments = ',lsis

  if (verbose) then
     do j = 1,lsis
        print *,j,sislist(j),sischn(j),sisind(j)
     end do
  end if

  tlapmean = 0.0
  tlbe     = 0.0
  ntl      = 0
  update_tlapmean = .true.
  
  var3       = 0.0
  
  close(lninfo)

  inquire(file=tlapfile,exist=lexist)
  if (.not. lexist) then
     write(6,*) 'did not find ',trim(tlapfile)
     ierror_code = 95
!!$     call mpi_abort(mpi_comm_world,ierror_code,ierror)
     stop 95
  end if
     
  open(lninfo,file=tlapfile,form='formatted')

!  read the tlapmean table
!  locate the sis/chn pair in the satinfo table
!  if not found, skip.  If found, store the tlapmean
  do 
     read(lninfo,'(1x,a20,1x,i5,e15.7)',iostat=istatus) satsensor0_j,  &
          jchanum0_j, tlap0_j
     if (istatus /= 0) exit
     iiii = sischnTable_locate(satinfo_table,satsensor0_j,jchanum0_j)
     if (iiii <=0 .or. iiii > jpch) cycle         ! entry not in satinfo
     tlapmean(iiii) = tlap0_j*100.
     tlbe(iiii) = 1.e6
     ntl(iiii) = 999
     update_tlapmean(iiii) = .false.
  end do

  close(lninfo)

! read in prior satbias coeff files if requested (predx contains the coefficients)
  allocate(predx(npred,jpch), varA(npred,jpch),predr(npred),ostats(jpch))
  predx = zero
  varA = zero
  ostats = zero

  if (trim(satbias_in) /= '') then
     inquire(file=satbias_in,exist=lexist)
     if (.not. lexist) then
        write(6,'(a,a,a)') 'Did not find satbias file ',satbias_in,' skipping file read'
     else
        print *,'reading from satbias_in file ',trim(satbias_in)
        open(lncoef,file=trim(satbias_in),form='formatted')
        istatus = 0
        do while ( istatus == 0 )
           read(lncoef,110,iostat=istatus) ich, satsensor0_j,jchanum0_j,  &
                tlap0_j, tlbe_j, ntl_j, (predr(i),i=1,npred)
           
           if (istatus /= 0) exit

!  special check for coeffs from x0029_bc and prePP_rt wrong ntl value

           if (tlap0_j == zero .and. tlbe_j .eq. zero .and. ntl_j == 999 ) ntl_j = 0

! check which location in table
           iiii = sischnTable_locate(satinfo_table,satsensor0_j,jchanum0_j)
           if (iiii <=0 .or.  iiii > jpch) cycle     ! entry not in satinfo list
           predx(:,iiii) = predr(:)
           if (ntl_j /= zero) then   ! if nonzero ntl, use tlapmean,ntl from satbias_in 
              ntl(iiii) = ntl_j         
              tlapmean(iiii) = tlap0_j
              tlbe(iiii) = tlbe_j
              if (ntl_j <= ntlapthresh) update_tlapmean(iiii) = .true.
           end if

           if (any(predr/=zero)) inew_rad(iiii) = .false.
        end do
        
        close(lncoef)

        if (trim(satbias_pc) /= '') then
           inquire(file=satbias_pc,exist=lexist)
           if (.not. lexist) then
              print *,'Did not find satbias_pc file ',satbias_pc,' skipping file read'
           else
              print *,'reading from satbias_pc file ',trim(satbias_pc)
              open(lnberr,file=trim(satbias_pc),form='formatted')
              istatus = 0
              do while (istatus == 0)
                 read(lnberr,111,iostat=istatus) ich, satsensor0_j,jchanum0_j,ostatsx, &
                      (varx(i),i=1,npred)
                 if (istatus /= 0) exit
                 iiii = sischnTable_locate(satinfo_table,satsensor0_j,jchanum0_j)
                 if (iiii < 0 .or. iiii > jpch) cycle
                 varA(:,iiii) = varx
                 ostats(iiii) = ostatsx
                 if (any(varx/=0)) inew_rad(iiii) = .false.
              end do
              close(lnberr)
          end if
        end if
     end if
  endif



  if (mode == 0 .or. mode == -1) then
!    Write zeroed satbias coeffs and error to output file
!
!  satsensor0  = satellite/instrument   e.g. amsua_n15
!  jchanum0    = channel #
!  tlapmean    = mean used for lapse rate term            (from 'satbias_in')
!  tlbe        = 'background error' for lapse rate term   (from 'satbias_in')
!  ntl         = number of estimates used for tlapmean    (from 'satbias_in')
!  coef3       = satbias coeffs                           ( = zero          )
!  ostats3     = data count from prior analysis           ( = zero          )
!  var3        = background error var. for bias coeffs    ( = zero or 1.e4  )

     write(6,*) 'mode == 0 or -1, write zeroed coeff files'
     coef3 = zero
     select case (mode)
     case (0)
        varx = zero
     case (-1)
        varx = 1.e4
     end select

     open(lnupdt,file='satbias_out',form='formatted')
     do j=1,jpch
        write(lnupdt,110) j,satsensor0(j),jchanum0(j),tlapmean(j),tlbe(j),ntl(j), &
             (coef3(i,j),i=1,npred)
     end do
     close(lnupdt)
     open(lnupdt,file='satbias_pc.out',form='formatted')
     do j = 1,jpch
        write(lnupdt,111) j,satsensor0(j),jchanum0(j),ostats3(j), &
             (varx(i),i=1,npred)
     end do
     close(lnupdt)

     deallocate(satsensor0, jchanum0)
     deallocate(tlapmean, tlbe, ntl, update_tlapmean)
     deallocate(coef3,ostats3,var3,varx)

     stop     !---------- End for nsteps 0, -1  --------------------------------
  end if

  if (mode == -2) then
     open(lnupdt,file='satbias_pc.out',form='formatted')
     var3 = zero
     do j = 1,jpch
        if (any(predx(:,j) /= zero )) then
           varx = 1.e4
           write(lnupdt,111) j, satsensor0(j), jchanum0(j),zero, varx
        else
           varx = zero
           write(lnupdt,111) j, satsensor0(j), jchanum0(j),zero, varx
        end if
     end do
     close(lnupdt)

     deallocate(satsensor0, jchanum0)
     deallocate(tlapmean, tlbe, ntl, update_tlapmean)
     deallocate(coef3,ostats3,var3,varx)

     stop   !---------- End for mode =-2  --------------------------------
  end if

  allocate(radstart(maxdat),radstep(maxdat),radnstep(maxdat))
  allocate(radedge1(maxdat),radedge2(maxdat))
  allocate(radsis(maxdat),ones(maxdat))

  
! Read gsi.rc.tmpl for file names

  print *,'read gsi.rc.tmpl obs table'
  call read_gsirctbl(ier)
  if (ier /= 0) then
     print *,'Error reading ',gsirc,' - stopping program '
     stop
  end if
  print *,'found ',ngsircf,' entries in the gsi template file'


! Read scaninfo data
  print *,'read scaninfo, build and verify table'
  call read_scaninfo2(ndat, nstep)
  call sischnTable_build(scantable,radsis(1:ndat),ones(1:ndat))
  call sischnTable_verify(scantable)

  allocate(tsum(jpch),tsum0(jpch),tsum2(jpch),tsum3(jpch))
  allocate(tcnt(jpch),tlap1(jpch),tlap2(jpch),tlap3(jpch),tlapm(jpch))
  allocate(coef2(npred,jpch),tlap0(jpch))
  allocate(ntl2(jpch),ntl3(jpch))
  allocate(satsensor3(jpch),jchanum3(jpch))
  allocate(satsensor2(jpch),jchanum2(jpch))
  allocate(lnew(jpch),lupdt(jpch))
  

!!!!!!!!!!!!!!
! Initialize arrays used for computation, save copies of values read in from files

! initialize arrays
  tsum   = zero
  tcnt   = zero
  tlap1  = zero
  tlap2  = zero
  coef2  = zero
  ntl2   = 0
  tlap0  = tlapmean
  tlapm = zero

! Fill arrays (for later output) with original values
  tlap3  = tlapmean
  tsum0  = tlbe
  tsum3  = tlbe
  ntl3   = ntl
  coef3  = predx
  var3   = varA
  satsensor3=satsensor0
  jchanum3=jchanum0
  ostats3=ostats


! Loop over satellite/sensors
  taskloop: do iii = 1,ngsircf
!!$     obsname=satstr(iii)
     obsname = trim(gsi_files(iii)%dtype) // '_' // trim(gsi_files(iii)%dplat)

     fstring = 'diag_' // trim(obsname)
     istr = index(obsname,'_')           ! separator for instrument_satellite
!     obstype = obsname(1:istr-1)
     obstype = gsi_files(iii)%dtype

     select case (trim(obstype))
     case ('sndr','sndrd1','sndrd2','sndrd3','sndrd4','ssmi','ssmis',         &
               'seviri','tmi','gmi','avhrr','geoirs')
        mean_only = .true.
        np = 1
     case default
        mean_only = .false.
        np = angord + 1
     end select

     update_file = .false.

! since we have the sis name for this file (read from gsi.rc.tmpl) we can check all
! the inew_rad values to see if it needs updating.  
!
     iptr_ch_start = sisind(gsi_files(iii)%sisptr)
     nchans = sischn(gsi_files(iii)%sisptr)
     chan_ptr(1:nchans) = rankarray(iptr_ch_start:iptr_ch_start+nchans-1)
     if (any(inew_rad(chan_ptr(1:nchans))) .or. any(update_tlapmean(chan_ptr(1:nchans)))) then      

     ftemplate= trim(prefix) // trim(fstring) // trim(dtemplate)
     call FileResolv(expid,nymd,nhms,ftemplate,diag_rad,stat=istatus)
     lexist = istatus .eq. 0

     if (.not.lexist) then
        if (verbose) write(*,'(a,a)') 'could not find file ',trim(diag_rad)
        cycle taskloop
     end if

     write(6,'(a,a)') ' Processing ',trim(diag_rad)

     if (verbose) then
        dname = 'sample_' // trim(obsname)
        open(lntemp,file=dname,form='formatted')

        do j = 1,nchans
           jj = chan_ptr(j)
           write(lntemp,*) satsensor0(jj),jchanum0(jj),inew_rad(jj)
        end do
     end if
     
!    Open file and read header

     open(lndiag,file=diag_rad,form='unformatted',status='old',iostat=istatus)
     if (istatus/=0) then
        write(6,'('' Problem opening file '',a,'' iostat='',i4)') &
             trim(diag_rad),istatus
        close(lndiag)
        cycle taskloop
     endif
     
     call read_radiag_header(lndiag,0,retrieval,header_fix,header_chan,data_name,istatus)
     if (istatus/=0) then
        write(6,'('' Problem reading header for file '',a,'' iostat='',i4)') &
             trim(diag_rad),istatus
        close(lndiag)
        cycle taskloop
     endif
     
!       Process file
     satsens = header_fix%isis
     n_chan  = header_fix%nchan

! change channel numbering for seviri if necessary
     if (satsens(1:6) == 'seviri') then
        if (header_chan(1)%nuchan < 4) then
           do j = 1,n_chan
              header_chan(j)%nuchan = header_chan(j)%nuchan+3
           end do
        end if
     end if
!
!  revision for latest satinfo format
     select case (satsens(1:4))
        case ('airs')
           satsens = 'airs_aqua'
        case('iasi')
           if (index(satsens,'metop-a') /= 0) satsens='iasi_metop-a'
           if (index(satsens,'metop-b') /= 0) satsens='iasi_metop-b'
           if (index(satsens,'metop-c') /= 0) satsens='iasi_metop-c'
        end select
     
!  check that the sis from the file matches the one that we expect

        if (trim(satsens) /= trim(gsi_files(iii)%sis)) then
           print *,'Error. mismatch in sis names '
           print *,'Expected (from gsi.rc.tmpl) ', trim(gsi_files(iii)%sis)
           print *,'Found (from diag file) ',trim(satsens)
           stop
        end if
        
!       Extract satinfo relative index

!!$!  we use the satinfo_table since we have aligned everything as in the satinfo
!!$!  file
     do j=1,n_chan
        io_chan(j) = sischnTable_locate(satinfo_table,satsens,header_chan(j)%nuchan)   
           if (verbose) then
              write(lntemp,'(4i5)') j,header_chan(j)%nuchan,io_chan(j),chan_ptr(j)
           end if
     end do

     if(any(io_chan(1:n_chan)<=0) .or. any(io_chan(1:n_chan)>jpch)) then
        write(6,*)'**WARNING** mismatching channel numbers, will skip unknown channels'
     end if


!!!! start of checks to determine if pre-processing needed for un/under-initialized coefficients
     allocate(inew(n_chan))
     inew = 0
     

!  check if any tlapmean needs to be updated
     ntlupd = .false.
     do j=1,n_chan
        jj = io_chan(j)
        if (jj <=0 .or. jj > jpch) cycle
        update_tlapmean(jj)  = (ntl(jj) <= ntlapthresh)
        ntlupd = update_tlapmean(jj) .or. ntlupd
     end do
        
! Check for uninitialized bias coefficients as in radinfo - we want to try to fit the
! angle coeffs using only the gross check QC and the errinv=exp(-(data_chan(j)%omgnbc/3.0_r_kind)**2)
     update_coeff = .false.
     ich = 0
     lnew = .false.
     lupdt = .false.
     do j = 1,n_chan
        jj = io_chan(j)
        if (jj <=0 .or. jj > jpch) cycle
        if (all(varA(:,jj) == zero) .and. all(predx(:,jj) == zero)) then
           update_coeff = .true.
           ich = ich + 1
           lnew(jj) = .true.
           inew(ich) = jj
!!$              varA(:,jj) = 1.e4      ! fill in varA to write out after coeff. fit zzzz
        end if
     end do

     if (ich > 0) then
        if (verbose) then
           print *,'ich is ',ich
           print *,'Fitting coeffs for ',satsensor0(inew(1)),' channels ',(jchanum0(inew(j)),j=1,ich)
        end if
        allocate(A(np,np,ich),b(np,ich))
        allocate(iobs(ich),pred(np))
        iobs = zero
        b = zero_quad
        A = zero_quad
     end if

     if ( update_coeff .or. ntlupd ) then
        
        radedge_min = 0
        radedge_max = 1000
        irdind = sischnTable_locate(scantable,satsens,1)
        if (irdind < 1 .or. irdind > ndat) then
           irdind = ndat        ! use dummy entry values for sis not in table
        end if

        if (radedge1(irdind)/=-1 .and. radedge2(irdind)/=-1) then
           radedge_min = radedge1(irdind)
           radedge_max = radedge2(irdind)
        end if
        
        write(6,'('' Initial angle processing for '',a,'' np='',i3)') &
             trim(diag_rad),np

! Loop to read diagnostic file to fill in values for un-/under-initialized coeffs.
        istatus = 0
        loopd1:  do while (istatus == 0)
           
! Read a record.  If read flag, istatus does not equal zero, exit loopd
           call read_radiag_data( lndiag, header_fix, retrieval, data_fix, data_chan, data_extra, istatus )
           if( istatus /= 0 ) exit loopd1
           
! Extract scan angle, lat, lon
           scan   = data_fix%senscn_pos
           ispot  = nint(scan)

! Exclude data on edges
           if (.not. use_edges .and. (&
                ispot < radedge_min .OR. ispot > radedge_max )) cycle loopd1

! Channel loop
           nc = 0
           loopc1:  do j = 1,n_chan
              
              jj = io_chan(j)
              if (jj <=0 .or. jj > jpch) cycle

              if(lnew(jj)) nc = nc + 1
              
              if (.not. update_tlapmean(jj) .and. .not. lnew(jj)) cycle

!     Check for reasonable obs-ges and observed Tb.

!     If use_qc = .true. check the errinv from the input file for quality control
!     If errinv= (1 /(obs error)) is small (small = less than 1.e-6)
!     the observation did not pass quality control.  In this case do not use the
!     observation in computing the update to the angle dependent bias.
              if (use_qc) then
                 if (data_chan(j)%errinv<1.e-6 ) then
                    cycle loopc1
                 end if

!     Gross check (if use_qc = .false.)
!     If the o-g difference is too large (> 200 K, very generous!)
!     of the observation is too cold (<50 K) or too warm (>500 K),
!     do not use this observation in computing the update to the
!     angle dependent bias.
              else
                 if( ( abs(data_chan(j)%omgnbc) > 200. .or. &
                      data_chan(j)%tbobs < 50. .or. &
                      data_chan(j)%tbobs > 500. ) ) then
                    cycle loopc1
                 end if
              end if

              errinv=exp(-(data_chan(j)%omgnbc/3.0_r_kind)**2)
              if (errinv < atiny) cycle loopc1
              
              if (ntlupd .and.update_tlapmean(jj)) then
                 tlaptmp=data_chan(j)%tlap
                 if (header_fix%inewpc==0) tlaptmp=100.0_r_kind*tlaptmp
                 tlap1(jj)=tlap1(jj)+(tlaptmp-tlap0(jj))*errinv
                 tsum(jj) =tsum(jj)+errinv
                 tcnt(jj) =tcnt(jj)+one
                 tlapm(jj)=tlapm(jj)+tlaptmp
              end if

              if (lnew(jj)) then
                 pred = zero
                 pred(1) = one
                 if (.not. mean_only) then
                    rnad = rnad_pos(satsens,ispot,irdind)*deg2rad
                    do i = 1,angord
                       pred(i+1) = rnad**i
                    end do
                 end if

                 iobs(nc) = iobs(nc) + 1
                 resid = data_chan(j)%omgnbc
                 do i = 1,np
                    b(i,nc) = b(i,nc) + resid*pred(i)*errinv**2
                 end do
                 do k = 1,np
                    do i = 1,np
                       A(i,k,nc) = A(i,k,nc) + pred(i)*pred(k)*errinv**2
                    end do
                 end do

              end if


           end do loopc1 ! channel loop

!       End of loop over diagnostic file
        end do loopd1

! Calculate updated tlapmean if necessary
        if (ntlupd) then
           do j = 1,n_chan
              jj = io_chan(j)
              if (jj <=0 .or. jj > jpch) cycle

              if (update_tlapmean(jj)) then
                 if (tcnt(jj) > 0 ) tlapm(jj) = tlapm(jj) / tcnt(jj)
                 if(tcnt(jj) >= nthreshold)  then
                    tsum(jj)=tsum(jj)+tsum0(jj)
                    tlap2(jj) = tlap0(jj) + tlap1(jj)/tsum(jj)
                    ntl(jj)=ntl(jj)+one
                 elseif (tcnt(jj)>0) then
                    ratio = max(zero,min(tcnt(jj)/float(nthreshold),one))
                    tsum(jj)=ratio*tsum(jj)+tsum0(jj)
                    tlap2(jj) = tlap0(jj) + ratio*tlap1(jj)/tsum(jj)
                    ntl(jj)=ntl(jj)+one
                 else
                    tsum(jj)=tsum0(jj)
                    tlap2(jj) = tlap0(jj)
                    ntl(jj)=ntl(jj)
                 endif
                 tlbe(jj)=tsum(jj)
                 tlapmean(jj)=tlap2(jj)
                 update_file = .true.
              end if
           end do
        end if

        if (verbose) then
           print *,'Updated tlap info'
           print *,'update_file = ',update_file
           do j = 1,n_chan
              jj = io_chan(j)
              if (jj <=0 .or. jj > jpch) cycle
              write(lntemp,'(I5,1x,A20,1x,I5,4e15.6,1x,I5)') jj, satsens, &
                   header_chan(j)%nuchan,tlapm(jj),tcnt(jj),tlapmean(jj),tlbe(jj),ntl(jj)
           end do
        end if

! solve linear system for angle bias
        if (ich > 0) then
           if (any(iobs>=nthreshold)) then
              allocate(AA(np,np),be(np))
              do i = 1,ich
                 if (iobs(i)< nthreshold) cycle
                 AA(:,:) = A(:,:,i)
                 be(:) = b(:,i)
                 if (all(abs(AA)<atiny)) cycle
                 if (all(abs(be)<atiny)) cycle
                 if (verbose) then
                    write(lntemp,*) i,iobs(i),be
                    write(lntemp,*) AA
                 end if
                 call linmm(AA,be,np,1,np,np)
                 
                 predx(1,inew(i)) = be(1)
                 if (.not. mean_only) then
                    do j = 1,angord
                       predx(npred-j+1,inew(i))=be(j+1)
                    end do
                 end if
                 lnew(inew(i)) = .false.
                 lupdt(inew(i)) = .true.
                 if (verbose) then
                    write(lntemp,*) i,tlapmean(inew(i))
                    write(lntemp,*)(predx(j,inew(i)),j=1,npred)
                 end if
              end do
              deallocate(AA,be)
           end if

           update_file = .true.
           
           deallocate(A,b)
           deallocate(iobs,pred)
        end if
        
     end if    ! if ( update_coeff .or. ntlupd)

     if (verbose) close(lntemp)
! if there are any updates write out bias file fragments for this obstype/platid/sis

     if (update_file) then
        gsi_files(iii)%update = .true.
        dname = 'coef_' // trim(obsname)
        open(lntemp,file=dname,form='formatted')
        do j = 1,n_chan
           jj = io_chan(j)
           if (jj <=0 .or. jj > jpch) cycle
           if (lupdt(jj) .or. update_tlapmean(jj))    &
                write(lntemp,112) jj,lupdt(jj),update_tlapmean(jj),  &
                satsensor0(jj),jchanum0(jj), &
                tlapmean(jj),tlbe(jj),ntl(jj),  &
                (predx(i,jj),i=1,npred)
        end do
        close(lntemp)
        if (wrinit) then
           dname = 'init_' // trim(obsname)
           open(lntemp,file=dname,form='formatted')
           do j = 1,n_chan
              jj = io_chan(j)
              if (jj <=0 .or. jj > jpch) cycle
              write(lntemp,'(I5,1x,5e13.6)')  &
                   jj,predx(1,jj),(predx(npred-i+1,jj),i=1,angord)
           end do
           close(lntemp)
        end if
     end if

     deallocate(inew)

  end if

! End of loop over satellite/sensor types
  end do taskloop

  deallocate(chan_ptr)

! combine the satellite/sensor specific
! update files together

  print *,'Combine satellite updates'

! reset all ostats (obs counts) to zero - non-missing instruments/channels
! will overwrite with current stats and missing channels will remain zero
!  ostats3 = zero

!    Loop over the satellite/sensors.  Read in each update
!    scratch file and load into proper location in output
!    arrays. 
!
  do iii=1,ngsircf
     if (.not. gsi_files(iii)%update ) cycle
     fname = 'coef_' // trim(gsi_files(iii)%dtype) // '_' // trim(gsi_files(iii)%dplat)
     inquire(file=fname,exist=lexist)
     if (lexist) then
        write(6,*) 'processing update file i=',iii,' with fname=', &
             trim(fname),' ',lexist,' ',gsi_files(iii)%update
     end if
     
!   Process the scratch update file
!
     if (lexist) then
        io_chan=0
        satsensor2=' '
        jchanum2=0
        tlap2=zero
        tsum2=zero
        coef2=zero
        ntl2=0
        
!  Read data from scratch file
        open(lntemp,file=fname,form='formatted')
        done=.false.
        j=1
        do while (.not.done)
           read(lntemp,112,end=160) io_chan(j),update_coeff,ntlupd, &
                satsensor2(j),jchanum2(j),&
                tlap2(j),tsum2(j),ntl2(j),(coef2(i,j),i=1,npred)
           j=j+1
           goto 170
160        continue
           done=.true.
170        continue
        end do
        n_chan=j-1
        close(lntemp)
        
! Transfer to output arrays

        do j=1,n_chan
           jj=io_chan(j)
           satsensor3(jj)=satsensor2(j)
           jchanum3(jj)  =jchanum2(j)
           if (ntlupd) then
              tlap3(jj)     =tlap2(j)
              tsum3(jj)     =tsum2(j)
              ntl3(jj)      =ntl2(j)
           endif
           if (update_coeff) then
              coef3(:,jj) = coef2(:,j)
              var3(:,jj) = 1.e4     ! newly fitted coeffs get large bkg err 
              ostats3(jj) = zero    !   and zero value for ostats
           endif
        end do
        
           
!  End of lexist block

     endif

!  End of loop over satellite/sensor data types
  end do


!  Write updated satbias coeffs and error to output file

  print *,'write updated satbias coefficients'

  open(lnupdt,file='satbias_out',form='formatted')
  do j=1,jpch
     write(lnupdt,110) j,satsensor3(j),jchanum3(j),tlap3(j),tsum3(j), &
          ntl3(j), (coef3(i,j),i=1,npred)
  end do
  close(lnupdt)
  open(lnupdt,file='satbias_pc.out',form='formatted')
  do j = 1,jpch
     write(lnupdt,111) j,satsensor3(j),jchanum3(j),ostats3(j), &
          (var3(i,j),i=1,npred)
  end do
  close(lnupdt)

110 format(I5,1x,A20,1x,I5,2e15.6,1x,I5/2(4x,10f12.6/))
111 format(I5,1x,A20,1x,I5,e15.7/2(4x,10e15.7/))
112 format(I5,2L2,1x,A20,1x,I5,2e15.6,1x,I5/2(4x,10f12.6/))
  
  allocate(cbiasx(nstep))
  open(lnupdt,file='satbias_ang.out',form='formatted')
  if (nstep /= 90) write(lnupdt,'(''nscan='',I8)') nstep
  satsensor0_j = ''
  do j=1,jpch
     ii = sischnTable_locate(scantable,satsensor3(j),1)
     if (ii < 1 .or. ii > ndat) ii = ndat    ! use dummy entry if sis not found
     cbiasx = 0.0
     call angle_cbias1(satsensor3(j),ii,coef3(1:npred,j),cbiasx)
     write(lnupdt,'(I5,1x,A20,2x,I4,e15.6/100(4x,10f7.3/))') &
          j,satsensor3(j),jchanum3(j),tlap3(j),(cbiasx(i),i=1,nstep)
  end do
  close(lnupdt)
  
  deallocate(cbiasx)
  
  print *,'Finalizing program, deallocate arrays'

  call sischnTable_clean(scantable)
  call sischnTable_clean(satinfo_table)

! Deallocate data arrays
  deallocate(tsum0, tsum, tlap0, tlap1, tcnt)
  deallocate(satsensor0,jchanum0)
  deallocate (tlapmean,tlbe,ntl)
  deallocate(predx)
  deallocate(coef2,coef3)
  deallocate(varA,var3,varx)
  deallocate(satsensor2,jchanum2,tlap2,tsum2,ntl2)
  deallocate(satsensor3,jchanum3,tlap3,tsum3,ntl3)
  deallocate(ostats3)
  deallocate(update_tlapmean,lnew, lupdt)
  deallocate(inew_rad)
  deallocate(gsi_files)

! Deallocate arrays from read_scaninfo

  deallocate(radstart, radstep, radnstep, radedge1, radedge2, radsis, ones)

  stop

CONTAINS


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE:  read_scaninfo2
!
! !INTERFACE:
!
  subroutine read_scaninfo2(ndat,nscan)

! !USES:
    implicit none

! !INPUT PARAMETERS:

    integer,intent(in) :: nscan       ! number of scan positions to use in
                                      ! modelling
! !OUTPUT PARAMETERS:

    integer, intent(out) :: ndat      ! size of scan info arrays

! !DESCRIPTION:   read the gmao_global_scaninfo file, allocate
!                 scaninfo arrays and fill with data read in from
!                 the scaninfo file.  Main routine will search in
!                 the dsis array.  Fill in default value at end of
!                 array for satellite/sensor not in the list
!
!
! !REVISION HISTORY:
!   08Mar2016 Sienkiewicz Initial code based on GSI/offline.x routine
!   04Apr2016 Sienkiewicz Add default line to table with values to use for
!                           instruments missing from the table
!
!EOP
!-------------------------------------------------------------------------

    integer, parameter :: lunin = 49
    logical pcexist
    character(len=20) :: satscan_sis
    real(r_kind) start,step
    integer(i_kind) nstep, edge1, edge2
    integer(i_kind) j
    integer istat
    character(len=1):: cflg
    maxscan = nscan
    j = 0

    inquire(file=scanfile,exist=pcexist)
    if (pcexist) then
       open(lunin,file=trim(scanfile),form='formatted')
       do
          read(lunin,1000,IOSTAT=istat,end=1222) cflg,satscan_sis,start,step,nstep,edge1,edge2
          if (istat /= 0) exit
          if (cflg == '!') cycle
          j = j +1
       end do
    else
       write(6,*) 'Error, can not find file "scaninfo"'
       ndat = 0
       return
    end if

    ndat = j+1

    if (ndat > maxdat) then
       print *,'ERROR: maxdat < ndat, need to increase maxdat'
       print *,'maxdat = ',maxdat,' ndat = ',ndat
       stop
    end if
    
    ones = 1
    radstart=zero
    radstep =one
    radnstep=maxscan
    radedge1=-1
    radedge2=-1
    j = 0
    rewind lunin
    do
       read(lunin,1000,IOSTAT=istat,end=1222) cflg,satscan_sis,start,step,nstep,edge1,edge2
       if (istat /= 0) exit
       if (cflg == '!') cycle
       j = j + 1
       
       radstart(j)=start
       radstep(j)=step
       radnstep(j)=nstep
       radedge1(j)=edge1
       radedge2(j)=edge2
       radsis(j) = satscan_sis
    end do
    radsis(ndat) = 'missing'
1000 format(a1,a20,2f11.3,i10,2i6)
1222 continue
    close(lunin)
    
  end subroutine read_scaninfo2
  
  
  real(r_kind) function rnad_pos(isis,iscan,jch)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    function rnad_pos
!
!   prgrmmr:     zhu      org: np23                date: 2010-05-06
!
! abstract:  For a given satellite/sensor produce the scan angle
!
! program history log:
!   2010-05-06  zhu
!
!   return:
!             - scan angle
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block


    character(len=20),intent(in):: isis
    integer(i_kind),intent(in):: iscan,jch
    
    integer(i_kind) ifov
    real(r_kind) piece
    
    if (index(isis,'iasi')/=0) then
       
       piece=-0.625_r_kind
       if (mod(iscan,2) == 1) piece = 0.625_r_kind
       rnad_pos=radstart(jch)+radstep(jch)*float((iscan-1)/2)+piece
       
    else
       
       if (index(isis,'hirs')/=0 .and. (index(isis,'n16')/=0 .or. &
            index(isis,'n17')/=0)) then
          ifov=iscan+1
       else if (index(isis,'atms') /= 0 .AND. maxscan < 96) then
          ifov=iscan+3
       else
          ifov=iscan
       end if
       rnad_pos=radstart(jch)+radstep(jch)*float(ifov-1)
       
    end if
    
    return
  end function rnad_pos
  
  subroutine angle_cbias1(isis,jspot,coefs,cbiasj)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    angle_cbias
!
!   prgrmmr:     zhu      org: np23                date: 2010-05-06
!
! abstract:  For a given satellite/sensor produce angle bias correction
!
! program history log:
!   2010-05-06  zhu
!   2010-12-16  treadon - recode cbiasj to be consistent with setuprad
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:
    implicit none
    
    character(len=20),intent(in):: isis     ! satellite/instrument/sensor
    integer(i_kind),intent(in):: jspot      ! index for radstart/radstep array
    real(r_kind),intent(in):: coefs(:)
    real(r_kind),dimension(maxscan),intent(inout):: cbiasj
    
    integer(i_kind) i,k
    real(r_kind),dimension(npred):: pred
    
    pred=zero
    do i=1,min(radnstep(jspot),maxscan)
       pred(npred)=rnad_pos(isis,i,jspot)*deg2rad
       do k=2,angord
          pred(npred-k+1)=pred(npred)**k
       end do
       cbiasj(i)=zero
       do k=1,angord
          cbiasj(i) = cbiasj(i)+ coefs(npred-k+1)*pred(npred-k+1)
       end do
       
    end do
    return
  end subroutine angle_cbias1
  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE:  read_gsirctbl
!
! !INTERFACE:
!
  subroutine read_gsirctbl(ier)
    
! USES:
    use mpeu_util, only: gettablesize
    use mpeu_util, only: gettable
    implicit none
    
! !INPUT PARAMETERS:
!
    integer ier
    
! !DESCRIPTION:  Reads dtype, dplat, and dsis values from gsi.rc.tmpl
!                
!
! !REVISION HISTORY:
!   14Nov2017 Sienkiewicz  Original routine
!   15Nov2017 Sienkieiwcz  check sislist to restrict saving info only for
!                          existing radiance sis types (defined by satinfo)
!
!EOP
!-------------------------------------------------------------------------
    
    character(len=256),allocatable,dimension(:):: utable
    character(20) :: sfile, ssis
    character(10) :: stype, splat
    integer nrows, ii,i, j
    integer ntot
    
    
    ier = 0

! Read dtype and dplat from gsi.rc and construct strings for diag file names
    open(unit=30,file=gsirc,form='formatted')
    call gettablesize('OBS_INPUT::',30,ntot,nrows)
    if (nrows == 0) then
       ier = 1
       close (30)
       return
    end if
    
    allocate(utable(nrows),gsi_files(nrows))
    call gettable('OBS_INPUT::',30,ntot, nrows, utable)
    
    close (30)
    
    j=0
    do ii = 1,nrows
       read(utable(ii),*)  sfile,&
            stype    ,& ! character string identifying type of observatio
            splat    ,& ! currently contains satellite id (no meaning for non-sat data)
            ssis        ! sensor/instrument/satellite identifier for info files
    
       select case (ssis(1:4))
       case ('airs')
          ssis='airs_aqua'
       case ('iasi')
          if (index(ssis,'metop-a') /= 0) ssis='iasi_metop-a'
          if (index(ssis,'metop-b') /= 0) ssis='iasi_metop-b'
          if (index(ssis,'metop-c') /= 0) ssis='iasi_metop-c'
       end select
       do i = 1,lsis          ! check if it's a sis type in the satinfo
          if (sislist(i) == ssis) then
             j = j + 1
             gsi_files(j)%dtype  = stype
             gsi_files(j)%dplat  = splat
             gsi_files(j)%sis    = ssis
             gsi_files(j)%sisptr = i
             gsi_files(j)%update = .false.    ! set true later if updated
             if (verbose) print *, stype, splat, ssis, i
             exit
          end if
       end do
    end do

    ngsircf = j  

    deallocate(utable)
    return
  end subroutine read_gsirctbl

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE:  init
!
! !INTERFACE:
!
  subroutine init()

! !REVISION HISTORY:
!   Spring2017 Sienkiewicz Initial version of init subroutine
!   04May2017  Sienkiewicz Fix missing initialization of FVROOT from
!                             environment variable
!   16Nov2017  Sienkiewicz Replaced 'gsidiags.rc' with 'gsi.rc.template' 
!                          Add '-s' to specify satinfo file input
!    9May2018  Sienkiewicz Add '-qc' flag to use the QC decisions from the
!                          input diag file in choosing which obs to use
!                          when fitting the coefficents. 
!
!-------------------------------------------------------------------------

    integer nargs, iargc, iarg
    character(len=120) argv, argv2
    
    nstep = 90
    satbias_in = ''
    satbias_pc = ''
    gsirc = ''
    satinfo = ''
!    fvroot = ''
    fvhome = ''
    dtemplate = '_ges.%y4%m2%d2_%h2z.bin'
    prefix = '%s.'
    verbose = .false.
    wrinit = .false.       ! flag to write Yanqiu 'init' style file for testing
    use_iuse = .false.
    use_qc = .false.

    nargs = iargc()

    if (nargs < 2) call usage

    call getarg(nargs,datestr)
    read(datestr,'(i8,i2)') nymd,ihh
    nhms = ihh *10000
    call getarg(nargs-1,argv)
    expid = trim(argv)

    iarg = 0
    do while (iarg < nargs-2)
       iarg = iarg + 1
       call getarg(iarg,argv)
       if (index(argv,'-') == 1) then
          select case (trim(argv))
          case('-b')                              ! name of 'satbias_in' file to read
             iarg = iarg + 1
             if (iarg > nargs-2) then
                print *,'Error in -b satbias_in argument'
                call usage
             end if
             call getarg(iarg,satbias_in)
          case ('-d')                             ! specify alternate date template
             iarg = iarg + 1
             if (iarg > nargs-2) then
                print *,'Error in -d template argument'
                call usage
             end if
             call getarg(iarg,dtemplate)
          case ('-g')                             ! alternate location of gsi.rc.tmpl file
             iarg = iarg + 1
             if (iarg > nargs-2) then
                print *,'Error in -g (gsi_rc.tmpl) argument'
                call usage
             end if
             call getarg(iarg,argv)
             read(argv,*) gsirc
          case ('-h')
             call usage
          case ('-i')                             ! write 'init' files
             wrinit = .true.
             print *,'wrinit = .true.'
          case ('-iuse')                          ! use 'iuse' from satinfo
             use_iuse = .true.
          case ('-m')                             ! set mode - fit coeffs or write uninitialized file
             iarg = iarg + 1                      ! (use to write inflated satbias_pc to use with 
             if (iarg > nargs-2) then             ! satbias from prior experiment)
                print *,'Error in -m argument'
                call usage
             end if
             call getarg(iarg,argv)
             read(argv,*) mode
          case ('-nstep')                         ! specify number of cross-track positions
             iarg = iarg + 1
             if (iarg > nargs-2) then
                print *,'Error in -nstep argument'
                call usage
             end if
             call getarg(iarg,argv)
             read(argv,*) nstep
          case ('-p')                             ! name of 'satbias_pc' file to read
             iarg = iarg + 1
             if (iarg > nargs-2) then
                print *,'Error in -p satbias_pc argument'
                call usage
             end if
             call getarg(iarg,satbias_pc)
          case ('-qc')
             use_qc = .true.
          case ('-s')                             ! name of 'satinfo' file to read
             iarg = iarg + 1
             if (iarg > nargs-2) then
                print *,'Error in -s (satinfo) argument'
                call usage
             end if
             call getarg(iarg,argv)
             read(argv,*) satinfo
          case ('-v')                             ! set 'verbose' flag
             verbose = .true.
             print *,'verbose = .true.'
          case ('-H')                             ! FVHOME value to use (override/replace environ. variable)
             iarg = iarg + 1
             if (iarg > nargs-2) then
                print *,'Error in -H (FVHOME) argument'
                call usage
             end if
             call getarg(iarg,fvhome)
             write(6,'(a,a,a)') 'using ',trim(fvhome), ' for FVHOME'
          case ('-P')                             ! prefix for input diag files (if not in current directory)
             iarg = iarg + 1
             if (iarg > nargs-2) then
                print *,'Error in -P prefix argument'
                call usage
             end if
             call getarg(iarg,prefix)
          case default
             print *,'undefined argument ',trim(argv),'; skipping'
             iarg = iarg + 1
          end select
       else
          write(6,'(a,a,a)') 'found argument ',trim(argv),' instead of flag'
          call usage
       end if
    end do

    if (satbias_in == '' .and. satbias_pc /= '') then
       print *,'satbias_in not specifed, not reading preconditioner file'
       satbias_pc = ''
    end if
    
    if (fvhome == '') then
       call getenv('FVHOME',fvhome)
       if (fvhome == '') then
          print *,'FVHOME not set (need for scaninfo,satinfo,tlapmean), exiting'
          stop
       end if
       print *,'FVHOME:  ',trim(fvhome)
    end if
    scanfile = trim(fvhome) // '/run/gmao_global_scaninfo.rc'
    tlapfile = trim(fvhome) // '/run/gmao_global_tlapmean.rc'

    if (satinfo == '') then
       use_iuse = .false.
       satinfo  = trim(fvhome) // '/run/gmao_global_satinfo.rc'
    end if
    
    if (use_iuse)   print *,'use_iuse = .true.'

    if ( gsirc == '') then
       gsirc = trim(fvhome) // '/run/gsi.rc.tmpl'
    end if
    
  end subroutine init

!-------------------------------------------------------------------------
!
subroutine usage()
  character(len=250) progname

  call GetArg(0,progname)

  print *,'Usage:  ',trim(progname),' [ flags ] expid yyyymmddhh      '
  print *,' '
  print *,' expid    - Experiment ID of input diag files'
  print *,' yyyymmddhh - year-month-day-hour of input diag files'
  print *,' '
  print *,'Flags:  '
  print *,'  -h              print this usage information'
  print *,'  -d template     diag file template (default _ges.%y4%m2%d2_%h2z.bin) '
  print *,'  -g filename     location of "gsi.rc.tmpl" used to build diag file names'
  print *,'  -m mode         bias correction mode (default: 1)'
  print *,'                                     -2  var=1e4 for nonzero bias coeff'
  print *,'                                     -1  zero bias coeff, var=1e4'
  print *,'                                      0  zero bias coeff, var=0  '
  print *,'                                      1  mean/scanangle fit      '
  print *,'  -nstep steps    number of cross-track positions (default: 90)'
  print *,'  -s filename     satinfo input file name (default=gmao_global_satinfo.rc)'
  print *,'      -iuse       if satinfo file set, use "iuse" values from the file    '
  print *,'  -v              set verbose = .true.'
  print *,'  -H directory    location of FVHOME '
  print *,'  -P prefix       prefix template for diag files (e.g. directory path)'
  print *,'  -qc             use qc marks as in diag file to screen obs'
  print *,''
  print *,'If adding entries for new instrument to prior satbias/satbias_pc file:'
  print *,'  -b filename     satbias_in input file name (optional)'
  print *,'  -p filename     satbias_pc input file name (optional)'
  print *,''
  print *,'ENVIRONMENT VARIABLES (if not specified on command line):'
  print *,'   FVHOME -  needed for satinfo, scaninfo, tlapmean resource files'
  
  stop
end subroutine usage
!-------------------------------------------------------------------------
!
!  if no input satbias is read in
!     fitting angle and mean bias for initial use of all satellite instruments
!     look for tlapmean in heritage table, if missing will try to fit
!     write out fitted satbias and maxed out satbias_pc values
!
!  if input satbias is read in
!     fitting only for satellite instruments without information
!     copy tlapmean if non-zero value of ntl
!     (if you want to refit tlapmean, at zero step remove old tlapmean value from table)
!  if satbias_pc is also read in, will use input error values from existing satbias_pc
!  This allows missing channels/instruments to be read in without changing existing info
!
!  sislist lists the unique sis combinations in the satinfo file.
!  gsi_files contains information for eligble diag files for fitting
!   
!

end program init_coeffs
