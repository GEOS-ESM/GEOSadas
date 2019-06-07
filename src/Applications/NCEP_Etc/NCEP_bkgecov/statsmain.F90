program statsmain
!$$$  main program documentation block
!                .      .    .                                       .
! main program: statsmain
!   prgrmmr: kleist           org: np20                date: 2005-01-15
!
! program history log:
!   2005-01-xx  kleist   initial version of stats code
!   2005-04-18  kleist   use sp library calls, add ability to easily run
!                        in single or double precision, modify post module
!                        to create byte addressable files for viewing using
!                        GRADS, compute variances for pseudo-RH and normalized
!                        RH, use IO on gridded fields to reduce memory usage
!   2005-04-xx  todling  read of namelist fix for most machines
!   2005-04-25  wgu      use LAPACK library calls to do SVD
!                        different buffers are specified in mpi_allreduce 
!                        (no aliasing) for code use in Halem(OSF)
!   2005-08-01  wgu      routines: m_GsiGrided.f90, m_fvHeader.f90, 
!                        m_rdgrids.f90, m_stvp.f90, m_calap.f90,
!                        m_getcases.f90, m_readpairs.f90, m_vordiv.f90
!                        m_speclap.f90 added to handle the forecast fields
!                        at regular lat-long grids, and to generate the 
!                        background stats at lat-lon grids by setting 
!                        lgaus=.false. or at Gaussian grids with lgaus=.true.
!  03May2015    todling  add opt to add hydrometeors
!
! abstract:
!   This code computes background error statistics to be used with the
!   GSI analysis code.  The NMC method, utilizing 24/48 hour forecast 
!   pairs valid at the same time, is used to compute variance, length
!   scale, and linear balance projection estimates.
!
! input files:
!   fort.10      - listing of NCEP global model forecast files
!   berror_sst   - fixed, global sst statistics file
!
! output files:
!   gsi.berror_stats - double precision stats output (serves as input to GSI)
!   bgstats_sp.grd   - single precision byte-addressable file for latidude 
!                      dependent variables
!   sststats_sp.grd  - single precision byte-addressable file SST statistics
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use type_kinds, only: fp_kind
  use variables, only: nsig,nlat,nlon,fnm0
  use variables, only: lgaus,lsmver,biasrm,laddoz
  use variables, only: nlatfv,nlonfv,latfv,lonfv
  use variables, only: maxcases,hybrid,smoothdeg
  use variables, only: smooth_vert_variances
  use variables, only: init_vars
  use variables, only: create_vars,create_grids
  use variables, only: destroy_grids,destroy_vars
  use variables, only: create_bias_g
  use variables, only: destroy_bias_g
  use variables, only: create_intvar,destroy_intvar
  use variables, only: hydromet
  use variables, only: init_smooth_vars
  use variables, only: hcoeffs,vcoeffs
  use specgrid,  only: jcap,nc,slat,jb,je
  use specgrid,  only: init_spec
  use specgrid,  only: init_spec_vars,destroy_spec_vars
  use postmod,   only: writefiles
  use m_GsiGrided, only: vectype
#ifndef ibm_sp
  use m_mpif
#endif
  implicit none
#ifdef ibm_sp
  include 'mpif.h'
#endif

  integer k,n,npes,total,mype,numcases,tskcases,mycases,ierror

! define namelist
! NAMSTAT
!   jcap      - spectral resolution of forecast pairs
!   nsig      - number of vertical levels
!   nlat      - number of latitudes
!   nlon      - number of longitudes
!   maxcases  - maximum number of forecast pairs to process
!   hybrid    - logical for hybrid vertical coordinate
!   smoothdeg - degree of horizontal smoothing to apply in latitudinal direction
!   hydromet  - controls addition of hydrometeors
!   smooth_vert_variances  - smooth variances vertically

  namelist/namstat/jcap,lgaus,lsmver,nsig,nlat,nlon,nlatfv,nlonfv,&
                   maxcases,hybrid,smoothdeg,fnm0,vectype,biasrm,laddoz,hydromet,&
                   smooth_vert_variances,hcoeffs,vcoeffs
  namelist/smoothvars/hcoeffs,vcoeffs
!   hcoeffs   - coeffs for horizontal smoothing
!   vcoeffs   - coeffs for vertical   smoothing

! MPI initial setup
  call mpi_init(ierror)
  call mpi_comm_size(mpi_comm_world,npes,ierror)
  call mpi_comm_rank(mpi_comm_world,mype,ierror)

! Initialize defaults for namelist variables
  call init_vars
  call init_spec

! Read in namelist
#ifdef ibm_sp
  read(5,namstat)
#else
  open(11,file='stats.parm')
  read(11,namstat)
  close(11)
#endif

  if(mype==0) then
    write(6,*)'starting computation of background stats with ',npes,' tasks'
    write(6,namstat)
  endif

  if(mype==0) write(6,*) 'INITIALIZE VARIABLES'
  call create_vars(nlat,nlon,nsig,mype)
  call create_grids(nlat,nlon,jcap,lgaus)

  call init_smooth_vars(nsig)

! Read in namelist
#ifdef ibm_sp
  read(5,smoothvars)
#else
  open(11,file='stats.parm')
  read(11,smoothvars)
  close(11)
#endif
 
! Make call to see how many available files there are
  if(mype==0) write(6,*) 'COUNT NUMBER OF AVAILABLE CASES'
#ifdef gmao_intf
    call m_getcases(numcases,mype)
    if(mype==0) write(6,*)latfv
    if(mype==0) write(6,*)lonfv
#else
    call getcases(numcases,mype)
#endif

  tskcases=numcases/npes
  if (mod(numcases,npes).NE.0) tskcases=tskcases+1

  if (mype==0) then 
     write(6,*) 'tskcases == ', tskcases
  endif

  call create_intvar(nlat,nlon,nsig)

  if(mype==0) write(6,*) 'READING PAIRS'
#ifdef gmao_intf
    call create_bias_g(nlat,nlon,nsig)
    call m_readpairs(tskcases,npes,mype,numcases,mycases)
#endif

  if(mype==0) write(6,*) 'CALCULATING THE BALANCED PROJECTIONS'
! get balance projection matrices, and unbal variables
  call calcbal(mycases,numcases,npes,mype)

! get variances
  call variances(mycases,numcases,npes,mype)

! get vertical length scale estimates
  call vertlength(mycases,numcases,npes,mype)

! get horizontal length scale estimates
  call horlength(mycases,numcases,npes,mype)

  call destroy_intvar

#ifdef gmao_intf
    call destroy_bias_g
#endif

! Replace Ozone field
   if ( laddoz ) then 
      call addoz
   end if 
! post Processing
  if (mype==0) then
    write(6,*) 'WRITE OUT BACKGROUND ERROR STATISTICS'
    call writefiles
  end if

  call destroy_grids
  call destroy_vars

  if (mype==0) write(6,*) '*** STATS CODE COMPLETE! ***'
  call mpi_finalize(ierror)

end program statsmain 
