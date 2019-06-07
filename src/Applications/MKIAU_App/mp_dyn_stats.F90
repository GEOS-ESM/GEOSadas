program effread

  use m_mpif90,only: MP_init
  use m_mpif90,only: MP_finalize
  use m_mpif90,only: MP_comm_rank
  use m_mpif90,only: MP_comm_size
  use m_mpif90,only: MP_real4
  use m_mpif90,only: MP_integer
  use m_mpif90,only: mp_logical
  use m_mpif90,only: mp_character
  use m_mpif90,only: MP_sum
  use m_mpif90,only: MP_type
  use m_mpif90,only: MP_comm_world

  use m_zeit, only: zeit_ci,zeit_co,zeit_flush
  use m_zeit, only: zeit_allflush

  use m_dyn, only: dyn_getdim

  use m_ioutil, only: luavail
  use m_die, only: MP_die

  implicit none
  integer,parameter :: r_single=4
  integer,parameter :: r_double=8
  integer,parameter :: i_kind=4
  integer,parameter :: maxchar=256
  real(r_single),allocatable, dimension(:,:,:) :: anal_chunk
  real(r_single),allocatable, dimension(:,:) :: ensspread_chunk
  real(r_single),allocatable, dimension(:,:) :: grdin
  real(r_single),allocatable, dimension(:,:) :: ensmean
  real(r_single),allocatable, dimension(:,:) :: ensspread
  integer(i_kind), allocatable, dimension(:) :: scounts, displs, rcounts

  integer(i_kind) :: ndim,nanals
  integer(i_kind) :: npts_max,npts_min
  integer(i_kind),allocatable,dimension(:,:) :: indxproc
  integer(i_kind),allocatable,dimension(:) :: numptsperproc
  integer(i_kind), allocatable, dimension(:) :: rtmp,numobs,numobs1
  integer(i_kind) :: nlon,nlat,nlevs,LM
  integer(i_kind) :: npts
  integer(i_kind) :: numproc_def
  integer(i_kind) :: nanals_def

  integer nymd,nhms

  integer,parameter :: maxfiles=100
  character(len=maxchar) :: files(maxfiles)
  character(len=maxchar) :: fnout_mean
  character(len=maxchar) :: fnout_sprd
  character(len=maxchar) :: eigtype

  integer(i_kind),parameter :: ROOT=0
  integer(i_kind) myPE,ierr
  integer(i_kind) :: icases = 2
  character(len=*),parameter::myname='effread'

  logical,parameter:: forreal = .true.
  logical :: esa_only
  logical :: rmse
  logical :: verbose

  call MP_init(ierr)
        if(ierr/=0) call MP_die(myname,'MP_init()',ierr)

  call MP_comm_rank(MP_comm_world,myPE,ierr)
        if(ierr/=0) call MP_die(myname,'MP_comm_rank()',ierr)
  call MP_comm_size(MP_comm_world,numproc_def,ierr)

! 
  call init_()
!
  call zeit_ci('read_ens')
  call load_balance(numproc_def)
  call read_ensemble(numproc_def,myPE,nanals_def)
  call zeit_co('read_ens')
!
  call ens_stats(numproc_def,myPE,nanals_def)
!
  call esa(numproc_def,myPE,nanals_def)
!
  call write_stats(myPE,ensmean,trim(fnout_mean),ierr)
  if(icases>1) &
  call write_stats(myPE,ensspread,trim(fnout_sprd),ierr)

  if(myPE==ROOT) call zeit_flush(6,subname_at_end=.true.)

contains

subroutine init_()

  integer nymd_,nhms_,nmem_
  integer iret, i, iarg, argc, iargc
  integer ires,nfiles
  character(len=255) :: argv,res

  integer, dimension(6), parameter :: IMS5 = (/ 72, 144, 288, 576, 1152, 2304 /)
  integer, dimension(6), parameter :: JMSG = (/ 46,  91, 181, 361,  721, 1441 /)

  esa_only = .false.
  fnout_mean = 'fast_mean.nc4'
  fnout_sprd = 'fast_spread.nc4'
  verbose = .false.
  eigtype = 'none'
  rmse = .false.

  nymd  = -1 
  nhms  = -1
  ires  = -1

  if (myPE==Root) then

!     Parse command line
!     ------------------
      argc =  iargc()
      if ( argc .lt. 1 ) call usage_()

      iarg = 0
      nfiles = 0

      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) exit
         call GetArg ( iarg, argv )

         select case (argv)
           case ("-pick")
             if ( iarg+2 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iarg, argv )
             read(argv,*) nymd_
             iarg = iarg + 1
             call GetArg ( iarg, argv )
             read(argv,*) nhms_
           case ("-esa_only")
             if ( iarg+1 .gt. argc ) call usage_()
             esa_only = .true.
           case ("-eigtype")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iArg, eigtype )
           case ("-h")
             if ( iarg+1 .gt. argc ) call usage_()
           case ("-o")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iArg, fnout_mean )
           case ("-res")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iarg, res )
             select case (trim(res))
               case ("a")
                     ires=1
               case ("b")
                     ires=2
               case ("c")
                     ires=3
               case ("d")
                     ires=4
               case ("e")
                     ires=5
               case ("f")
                     ires=6
               case default
                     print *, 'Sorry this resolution not supported'
                     call usage_()
             end select
           case ("-rmse")
             if ( iarg+1 .gt. argc ) call usage_()
             rmse = .true.
           case ("-sprd")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iArg, fnout_sprd )
           case ("-verbose")
             verbose = .true.
           case default
             nfiles = nfiles + 1
             if ( nfiles .gt. maxfiles ) call usage_()
             files(nfiles) = trim(argv)

         end select
      end do
      print *
      print *, 'Files to handle: '
      print *
      do i=1,nfiles
         print*, 'input from file:              ', trim(files(i))
      enddo
      print *

      if(ires==-1) then
        call dyn_getdim ( files(1), nlon, nlat, nlevs, LM, ierr )
      endif
      write(6,'(4(a,i5,1x))') 'Dimension of fields: ', nlon, ' x ', nlat, ' x ', nlevs, ' x ', LM

      nmem_=nfiles
  endif ! root

  call mpi_bcast(nfiles,        1,mp_integer  ,ROOT,MP_comm_world,iret)
  call mpi_bcast(nymd_ ,        1,mp_integer  ,ROOT,MP_comm_world,iret)
  call mpi_bcast(nhms_ ,        1,mp_integer  ,ROOT,MP_comm_world,iret)
  call mpi_bcast(nmem_ ,        1,mp_integer  ,ROOT,MP_comm_world,iret)
  call mpi_bcast(esa_only,      1,mp_logical  ,ROOT,MP_comm_world,iret)
  call mpi_bcast(rmse,          1,mp_logical  ,ROOT,MP_comm_world,iret)
  call mpi_bcast(verbose,       1,mp_logical  ,ROOT,MP_comm_world,iret)
  call mpi_bcast(eigtype,     maxchar,mp_character,ROOT,MP_comm_world,iret)
  call mpi_bcast(fnout_mean,  maxchar,mp_character,ROOT,MP_comm_world,iret)
  call mpi_bcast(fnout_sprd,  maxchar,mp_character,ROOT,MP_comm_world,iret)
  do i = 1,nfiles
     call mpi_bcast(files(i),maxchar,mp_character,ROOT,MP_comm_world,iret)
  enddo
  call mpi_bcast(ires  ,        1,mp_integer  ,ROOT,MP_comm_world,iret)
  if(ires<=0)then
    call mpi_bcast(nlon,        1,mp_integer  ,ROOT,MP_comm_world,iret)
    call mpi_bcast(nlat,        1,mp_integer  ,ROOT,MP_comm_world,iret)
    call mpi_bcast(nlevs,       1,mp_integer  ,ROOT,MP_comm_world,iret)
  else
    nlon = ims5(ires)
    nlat = jmsg(ires)
    nlevs = 72
 endif

  nanals_def = nmem_
  nymd = nymd_
  nhms = nhms_

  if (trim(eigtype) == "all" .or. trim(eigtype) == "none" ) then
!    ndim = 10*nlevs+9  ! nvars3d*nlevs+nvars2d
     ndim =  4*nlevs+1  ! nvars3d*nlevs+nvars2d
  else
     if (trim(eigtype) == "ps" ) then
        ndim = 1
     else
        ndim = nlevs
     endif
  endif
  npts = nlon*nlat
  if (rmse) then
     icases=1
  endif

end subroutine init_

subroutine load_balance(numproc)

integer(i_kind),intent(in) :: numproc

integer(i_kind) np,n

allocate(numobs(npts),numobs1(npts))
allocate(numptsperproc(numproc))
allocate(rtmp(numproc))
numobs = 1 ! one grid-point per grid-cell
call mpi_allreduce(numobs,numobs1,npts,mp_integer,mp_sum,mp_comm_world,ierr)
rtmp = 0
numptsperproc=0
do n=1,npts
   np = minloc(rtmp,dim=1)
   ! np is processor with the fewest number of obs to process
   ! add this grid point to list for nmin
   rtmp(np) = rtmp(np)+numobs1(n)
   numptsperproc(np) = numptsperproc(np)+1
end do
npts_max = maxval(numptsperproc)
npts_min = minval(numptsperproc)
allocate(indxproc(numproc,npts_max))
! indxproc(np,i) is i''th horiz grid index for processor np.
! there are numptsperpoc(np) i values for processor np
rtmp = 0
numptsperproc = 0
do n=1,npts
   np = minloc(rtmp,dim=1)
   rtmp(np) = rtmp(np)+numobs1(n)
   numptsperproc(np) = numptsperproc(np)+1 ! recalculate
   indxproc(np,numptsperproc(np)) = n
end do
if (myPE == ROOT) then
    print *,'npts = ',npts
    print *,'min/max number of points per proc = ',npts_min,npts_max
end if

end subroutine load_balance

subroutine read_ensemble(numproc,nproc,nanals)
! read ensemble members on IO tasks,
! distribute pieces (defined by module loadbal) to each task.
! for now, first nanals tasks are IO tasks.
implicit none
integer(i_kind),intent(in) :: numproc
integer(i_kind),intent(in) :: nproc
integer(i_kind),intent(in) :: nanals
real(r_single), allocatable, dimension(:) :: sendbuf,recvbuf
real(r_double) t1,t2
integer(i_kind) nanal,nn,i,n
! npts,ntrac arrays
integer(i_kind) ierr, np

! must at least nanals tasks allocated.
if (numproc < nanals) then
  print *,'need at least nanals =',nanals,'MPI tasks, exiting ...'
  call mp_finalize(ierr)
end if
if (npts < numproc) then
  print *,'cannot allocate more than npts =',npts,'MPI tasks, exiting ...'
  call mp_finalize(ierr)
end if

allocate(scounts(0:numproc-1))
allocate(displs(0:numproc-1))
allocate(rcounts(0:numproc-1))
! only IO tasks send any data.
! scounts is number of data elements to send to processor np.
! rcounts is number of data elements to recv from processor np.
! displs is displacement into send array for data to go to proc np
do np=0,numproc-1
   displs(np) = np*npts_max*ndim
enddo
if (nproc <= nanals-1) then
   scounts = npts_max*ndim
else
   scounts = 0
endif
! displs is also the displacement into recv array for data to go into anal_chunk on
! task np.
do np=0,numproc-1
   if (np <= nanals-1) then
      rcounts(np) = npts_max*ndim
   else
      rcounts(np) = 0
   end if
enddo

! allocate array to hold pieces of state vector on each proc.
allocate(anal_chunk(nanals,npts_max,ndim))
if (nproc == ROOT) print *,'anal_chunk size = ',size(anal_chunk)
! send and receive buffers.
allocate(sendbuf(numproc*npts_max*ndim))
allocate(recvbuf(nanals*npts_max*ndim))

! read in whole state vector on i/o procs - keep in memory 
if (nproc <= nanals-1) then
   allocate(grdin(npts,ndim))
   nanal = nproc + 1
   call readgriddata(nanal,grdin,ierr)
        if(ierr/=0) call MP_die(myname,'readgriddata()',ierr)
   ! fill up send buffer.
   do np=1,numproc
    do nn=1,ndim
     do i=1,numptsperproc(np)
      n = ((np-1)*ndim + (nn-1))*npts_max + i
      sendbuf(n) = grdin(indxproc(np,i),nn)
     enddo
    enddo
   enddo
end if
call mpi_alltoallv(sendbuf, scounts, displs, mp_real4, recvbuf, rcounts, displs,&
                   mp_real4, mp_comm_world, ierr)
if (ierr/=0) then
   call MP_die(myname,'failed in alltoall, aborting ...',ierr)
endif
deallocate(sendbuf)

!==> compute ensemble mean
!_$omp parallel do schedule(dynamic,1)  private(nn,i,nanal,n)
do nn=1,ndim 
   do i=1,numptsperproc(nproc+1)

      do nanal=1,nanals
         n = ((nanal-1)*ndim + (nn-1))*npts_max + i
         anal_chunk(nanal,i,nn) = recvbuf(n)
      enddo

   end do
end do
deallocate(recvbuf)

end subroutine read_ensemble

subroutine ens_stats(numproc,nproc,nanals)

implicit none
integer(i_kind),intent(in):: numproc
integer(i_kind),intent(in):: nproc
integer(i_kind),intent(in):: nanals

real(r_single),allocatable, dimension(:,:) :: ensmean_chunk
real(r_single), allocatable, dimension(:) :: sendbuf,recvbuf
real(r_single)  cin, cm1, smom
integer(i_kind) i,n,ii,np,nn,icase

if (esa_only) return
call zeit_ci('ens_stats')

allocate(ensmean_chunk(npts_max,ndim))
if(icases>1)allocate(ensspread_chunk(npts_max,ndim))
ensmean_chunk = 0.
if(icases>1)ensspread_chunk = 0.
!==> compute ensemble mean
!_$omp parallel do schedule(dynamic,1)  private(nn,i,nanal,n)
do nn=1,ndim 
   if(icases==1) then ! non-recursive mean
     if (rmse) then
        do i=1,numptsperproc(nproc+1)
           ensmean_chunk(i,nn) = sqrt(sum(anal_chunk(:,i,nn)*anal_chunk(:,i,nn))/float(nanals-1))
        end do
     else
        do i=1,numptsperproc(nproc+1)
           ensmean_chunk(i,nn) = sum(anal_chunk(:,i,nn))/float(nanals)
        end do
     endif
   endif
   if(icases>1) then ! recursive mean and spread to equal mp_stats
     do i=1,numptsperproc(nproc+1)
        do ii=1,nanals
           cin = 1.0/ii
           cm1 = ii-1.0
           smom = anal_chunk(ii,i,nn) - ensmean_chunk(i,nn)
           ensspread_chunk(i,nn) = ensspread_chunk(i,nn) + cin*cm1* smom*smom
           ensmean_chunk(i,nn) = cm1*ensmean_chunk(i,nn) + anal_chunk(ii,i,nn)
           ensmean_chunk(i,nn) = cin*ensmean_chunk(i,nn)

        end do
        ensspread_chunk(i,nn) = sqrt(ensspread_chunk(i,nn)/nanals)
     end do
   endif
end do
! calculate mean on ROOT
if (forreal) then
  allocate(sendbuf(npts_max*ndim))
  allocate(recvbuf(npts*ndim))
  do icase = 1,icases ! mean and spread
!    gather ens. mean anal. increment on root, print out max/mins.
     n = 0
     do nn=1,ndim
      do i=1,numptsperproc(nproc+1)
        n = n + 1
        if(icase==1) sendbuf(n) = ensmean_chunk  (i,nn)
        if(icase==2) sendbuf(n) = ensspread_chunk(i,nn)
      enddo
     enddo
     do np=0,numproc-1
        scounts(np) = numptsperproc(np+1)*ndim
        n = 0
        do nn=1,np
           n = n + numptsperproc(nn)*ndim
        enddo
        displs(np) = n
     enddo
     call mpi_gatherv(sendbuf, numptsperproc(nproc+1)*ndim, mp_real4, recvbuf, &
                      scounts, displs, mp_real4, ROOT, mp_comm_world, ierr)
     if (nproc == ROOT) then
        if(icase==1) allocate(ensmean  (npts,ndim))
        if(icase==2) allocate(ensspread(npts,ndim))
        n = 0
        do np=1,numproc
           do nn=1,ndim
            do i=1,numptsperproc(np)
              n = n + 1
              if(icase==1) ensmean  (indxproc(np,i),nn) = recvbuf(n)
              if(icase==2) ensspread(indxproc(np,i),nn) = recvbuf(n)
            enddo
           enddo
        enddo
     end if
  enddo ! icase
  deallocate(recvbuf)
  deallocate(sendbuf)
endif ! forreal
deallocate(ensmean_chunk)

call zeit_co('ens_stats')

end subroutine ens_stats

subroutine esa(numproc,nproc,nanals)

implicit none
integer(i_kind),intent(in):: numproc
integer(i_kind),intent(in):: nproc
integer(i_kind),intent(in):: nanals

real(r_single),allocatable, dimension(:,:) :: ensmean_chunk
real(r_single), allocatable, dimension(:) :: sendbuf,recvbuf
real(r_single), allocatable, dimension(:,:,:) :: err_chunk
real(r_single), allocatable, dimension(:,:,:) :: matrix_chunk
real(r_single), allocatable, dimension(:,:) :: matrix
real(r_single), allocatable, dimension(:,:) :: Evecs
real(r_single), allocatable, dimension(:)   :: Evals
integer(i_kind) i,j,n,ii,np,nn,icase,nvals,m1,m2,luout

if(trim(eigtype)=="none") return
call zeit_ci('esa')

allocate(ensmean_chunk(npts_max,ndim))
ensmean_chunk = 0.
!==> compute ensemble mean
!_$omp parallel do schedule(dynamic,1)  private(nn,i,nanal,n)
do nn=1,ndim 
   do i=1,numptsperproc(nproc+1)
      ensmean_chunk(i,nn) = sum(anal_chunk(:,i,nn))/float(nanals)
   end do
end do
! compute deviations from the mean
allocate(err_chunk(nanals,npts_max,ndim))
do nn=1,ndim 
   do i=1,numptsperproc(nproc+1)
      err_chunk(:,i,nn) = anal_chunk(:,i,nn) - ensmean_chunk(i,nn)
   end do
end do
deallocate(ensmean_chunk)

allocate(matrix_chunk(nanals,nanals,npts_max))
matrix_chunk=0.0
do i=1,numptsperproc(nproc+1)
   do m1 = 1,nanals
     do m2 = 1,nanals
        matrix_chunk(m1,m2,i) = matrix_chunk(m1,m2,i) + sum(err_chunk(m1,i,:) * err_chunk(m2,i,:)) ! recall that for this to make sense
                                                                                                   ! err has to be composed of fields
                                                                                                   ! properly scaled, such as, E-normlzd
     enddo
  enddo
enddo

! calculate matrix and its eigendecomposion on ROOT
if (forreal) then
  if(myPE==ROOT) then
    allocate(matrix  (nanals,nanals))
  else
    allocate(matrix  (0,0))
  endif
  allocate(sendbuf(npts_max))
  allocate(recvbuf(npts))
! gather ens. mean anal. increment on root, print out max/mins.
  matrix=0.0
  do m2=1,nanals
  do m1=1,nanals
  n = 0
  do i=1,numptsperproc(nproc+1)
    n = n + 1
    sendbuf(n) = matrix_chunk  (m1,m2,i)
  enddo
  do np=0,numproc-1
     scounts(np) = numptsperproc(np+1)
     n = 0
     do nn=1,np
        n = n + numptsperproc(nn)
     enddo
     displs(np) = n
  enddo
  call mpi_gatherv(sendbuf, numptsperproc(nproc+1), mp_real4, recvbuf, &
                   scounts, displs, mp_real4, ROOT, mp_comm_world, ierr)
  if (nproc == ROOT) then
     n = 0
     do np=1,numproc
         do i=1,numptsperproc(np)
           n = n + 1
           matrix(m1,m2) =  matrix(m1,m2) + recvbuf(n)
         enddo
     enddo
  end if
  enddo ! m1
  enddo ! m2
  deallocate(recvbuf)
  deallocate(sendbuf)
!---------------------------------

! Solver eigenproblem
  if (nproc == ROOT) then
     allocate(Evecs(nanals,nanals),Evals(nanals))
     Evecs=matrix
     call solver1_ (Evecs,Evals,nvals)
     write(6,'(a,i5)') 'Converged number of eigenvalues: ', nvals
     do nn=nanals-nvals+1,nanals
        write(6,'(i5,1p,e13.6)') nn, Evals(nn)
     enddo
!    Compose Evecs Evals Evecs^T into matrix
!    do i=1,nanals
!       Evecs(i,:) = Evecs(i,:)*(1./sqrt(Evals(i)))
!    enddo
!    matrix=0.0
!    do j=1,nanals
!       do i=1,nanals
!          matrix(i,j) = 0.0
!          do nn=1,nanals
!             matrix(i,j) = matrix(i,j) + Evecs(i,nn)*Evecs(j,nn)
!          enddo
!       enddo
!    enddo
!    Write out matrix to binary file
     luout=luavail()
     open(luout,file='eigen_decomp.bin',form='unformatted')
     write(luout) nanals, nvals
     write(luout) Evals
     do i=1,nanals
        write(luout) Evecs(:,i)
     enddo
     close(luout)
!    Clean up
     deallocate(Evecs)
     deallocate(Evals)
     deallocate(matrix)
   endif
endif ! forreal
deallocate(err_chunk)

call zeit_co('esa')

end subroutine esa

subroutine solver1_ (A,W,NC)
  implicit none
  real, intent(inout) :: A(:,:)
  real, intent(inout) :: W(:)
  integer, intent(out) :: NC ! converged evals

  integer :: N, LDA, LWORK, INFO
  real, allocatable :: WORK(:)
  
  LDA = size(A,1)
  N   = size(A,2)
  LWORK = 3*N-1
  allocate(WORK(LWORK))

  call SSYEV( 'V', 'U', N, A, LDA, W, WORK, LWORK, INFO )

  if (INFO/=0) then
     write(6,'(a,i5)') 'Error in eigensolver, info = ', info
  endif
  deallocate(WORK)
  NC = count(W>0.0,1) ! For now, simple criterium of acceptance 

end subroutine solver1_

subroutine readgriddata(nanal,grdin,rc)
use m_dyn, only: dyn_init
use m_dyn, only: dyn_vect
use m_dyn, only: dyn_get
use m_dyn, only: dyn_clean
use m_maph, only: h_map
implicit none
integer(i_kind),intent(in) :: nanal
real(r_single),intent(inout), dimension(:,:) :: grdin
integer(i_kind),intent(out) :: rc

character(len=*),parameter:: myname_ = myname//'readgriddata'
character(len=maxchar) :: fname
integer freq,nstep,ier
integer im,jm,km,lm
integer k,nt
type(dyn_vect) x_b
type(dyn_vect) x_x

   rc=0
   fname=trim(files(nanal))
   print *, 'reading: ', trim(fname)
   call dyn_getdim ( fname, im, jm, km, lm, ierr )
   if (nlon/=im .or. nlat/=jm) then ! need to make sure dyn utils are thread-safe

      print *, 'Interpolating to intermediate grid: ', nlon, nlat

!     Requires interpolation: read member 
!     -----------------------------------
      call dyn_get ( fname, nymd, nhms, x_x, ier, timidx=1, freq=freq, nstep=nstep, vectype=5 )

!     Initialize dimension of output (interpolated) vector
!     ----------------------------------------------------
      call dyn_init ( nlon, nlat, x_x%grid%km, x_x%grid%lm, x_b, ier, &
                                  x_x%grid%ptop, x_x%grid%ks, x_x%grid%ak, x_x%grid%bk, vectype=5 )
           if ( ier/=0 ) then
              print *, trim(myname_), ': Error initializing dyn vector(x_b), ier=', ier
              rc = 1
              return
          endif

!     Map to desired resolution
!     -------------------------
      call h_map ( x_x, x_b, ier, lwifile='NONE', dgrid=.false. )
           if ( ier/=0 ) then
              call dyn_clean ( x_x )
              call dyn_clean ( x_b )
              print *, trim(myname_), ': Error in horizontal interpolation'
              rc = 2
              return
           endif

!     Clean up
!     --------
      call dyn_clean ( x_x )

   else

!     No need to interpolate: read member 
!     -----------------------------------
      call dyn_get ( fname, nymd, nhms, x_b, ier, timidx=1, freq=freq, nstep=nstep, vectype=5 )

   endif
 
!  Copy input fields over to long vector
!  -------------------------------------
   ! This following belows to m_dyn
   do k=1,km
         if (trim(eigtype) == "all" .or. trim(eigtype) == "none" ) then
             grdin(:,     k) = reshape(x_b%u   (:,:,k),(/npts/))
             grdin(:,  km+k) = reshape(x_b%v   (:,:,k),(/npts/))
             grdin(:,2*km+k) = reshape(x_b%pt  (:,:,k),(/npts/))
!_RT         grdin(:,3*km+k) = reshape(x_b%delp(:,:,k),(/npts/))
             do nt=1,1!_RT 6
                grdin(:,(2+nt)*km+k) = reshape(x_b%q(:,:,k,nt),(/npts/))
!_RT            grdin(:,(3+nt)*km+k) = reshape(x_b%q(:,:,k,nt),(/npts/))
             enddo
!_RT         grdin(:,10*km+1) = reshape(x_b%frland(:,:),(/npts/))
!_RT         grdin(:,10*km+2) = reshape(x_b%frlandice(:,:),(/npts/))
!_RT         grdin(:,10*km+3) = reshape(x_b%frlake(:,:),(/npts/))
!_RT         grdin(:,10*km+4) = reshape(x_b%frocean(:,:),(/npts/))
!_RT         grdin(:,10*km+4) = reshape(x_b%frseaice(:,:),(/npts/))
!_RT         grdin(:,10*km+6) = reshape(x_b%hs_stdv(:,:),(/npts/))
!_RT         grdin(:,10*km+7) = reshape(x_b%phis(:,:),(/npts/))
!_RT         grdin(:,10*km+8) = reshape(x_b%ts(:,:),(/npts/))
!_RT         grdin(:,10*km+9) = reshape(x_b%ps(:,:),(/npts/))
             grdin(:, 4*km+1) = reshape(x_b%ps(:,:),(/npts/))
         else
             if (trim(eigtype) == "u" ) then
                grdin(:,k) = reshape(x_b%u (:,:,k),(/npts/))
             endif
             if (trim(eigtype) == "v" ) then
                grdin(:,k) = reshape(x_b%v (:,:,k),(/npts/))
             endif
             if (trim(eigtype) == "pt" ) then
                grdin(:,k) = reshape(x_b%pt (:,:,k),(/npts/))
             endif
             if (trim(eigtype) == "q" ) then
                grdin(:,k) = reshape(x_b%q(:,:,k,1),(/npts/))
             endif
             if (trim(eigtype) == "ps" ) then
                grdin(:,1) = reshape(x_b%ps(:,:),(/npts/))
             endif
         endif
      enddo
   call dyn_clean (x_b)
   
end subroutine readgriddata

subroutine write_stats(nproc,mean,fname,rc)
use m_dyn, only: dyn_getdim
use m_dyn, only: dyn_init
use m_dyn, only: dyn_vect
use m_dyn, only: dyn_get
use m_dyn, only: dyn_put
use m_dyn, only: dyn_clean
use m_maph, only: h_map
implicit none
integer(i_kind),intent(in) :: nproc
real(r_single),intent(inout), dimension(:,:) :: mean
character(len=*),intent(in):: fname
integer(i_kind),intent(out):: rc

character(len=*),parameter:: myname_ = myname//'write_stats'
character(len=maxchar) :: tmplfname
integer freq,nstep,ier
integer im,jm,km,lm
integer k,nt
type(dyn_vect) x_m
type(dyn_vect) x_x


   rc=0
   if(nproc/=ROOT) return 
   if(esa_only) return

   call zeit_ci('write_stats')

   tmplfname=trim(files(1))
   print *, 'reading: ', trim(tmplfname)
   call dyn_get ( tmplfname, nymd, nhms, x_m, ier, timidx=1, freq=freq, nstep=nstep, vectype=5 )
   im=x_m%grid%im
   jm=x_m%grid%jm
   km=x_m%grid%km
   if (nlon/=im .or. nlat/=jm) then
 
!     Initialize dimension of output (interpolated) vector
!     ----------------------------------------------------
      call dyn_init ( nlon, nlat, x_m%grid%km, x_m%grid%lm, x_x, ier, &
                                  x_m%grid%ptop, x_m%grid%ks, x_m%grid%ak, x_m%grid%bk, vectype=5 )
           if ( ier/=0 ) then
              print *, trim(myname_), ': Error initializing dyn vector(x_b), ier=', ier
              rc = 1
              return
          endif

      if (forreal) then
         ! This following belows to m_dyn
         do k=1,km
            x_x%u   (:,:,k)  = reshape(mean(:,     k),(/nlon,nlat/))
            x_x%v   (:,:,k)  = reshape(mean(:,  km+k),(/nlon,nlat/))
            x_x%pt  (:,:,k)  = reshape(mean(:,2*km+k),(/nlon,nlat/))
!_RT        x_x%delp(:,:,k)  = reshape(mean(:,3*km+k),(/nlon,nlat/))
            do nt=1,1!_RT 6
!_RT           x_x%q(:,:,k,nt) = reshape(mean(:,(3+nt)*km+k),(/nlon,nlat/))
               x_x%q(:,:,k,nt) = reshape(mean(:,(2+nt)*km+k),(/nlon,nlat/))
            enddo
!_RT        x_x%frland(:,:)    = reshape(mean(:,10*km+1),(/nlon,nlat/))
!_RT        x_x%frlandice(:,:) = reshape(mean(:,10*km+2),(/nlon,nlat/))
!_RT        x_x%frlake (:,:)   = reshape(mean(:,10*km+3),(/nlon,nlat/))
!_RT        x_x%frocean(:,:)   = reshape(mean(:,10*km+4),(/nlon,nlat/))
!_RT        x_x%frseaice(:,:)  = reshape(mean(:,10*km+5),(/nlon,nlat/))
!_RT        x_x%hs_stdv(:,:)   = reshape(mean(:,10*km+6),(/nlon,nlat/))
!_RT        x_x%phis   (:,:)   = reshape(mean(:,10*km+7),(/nlon,nlat/))
!_RT        x_x%ts  (:,:)      = reshape(mean(:,10*km+8),(/nlon,nlat/))
!_RT        x_x%ps  (:,:)      = reshape(mean(:,10*km+9),(/nlon,nlat/))
            x_x%ps  (:,:)      = reshape(mean(:, 4*km+1),(/nlon,nlat/))
         enddo
      endif

!     Map to desired resolution
!     -------------------------
      call h_map ( x_x, x_m, ier, lwifile='NONE', dgrid=.false. )
           if ( ier/=0 ) then
              call dyn_clean ( x_x )
              call dyn_clean ( x_m )
              print *, trim(myname_), ': Error in horizontal interpolation'
              rc = 2
              return
           endif

!     Clean up
!     --------
      call dyn_clean ( x_x )

   else
   
      if (forreal) then
         ! This following belows to m_dyn
         do k=1,km
            x_m%u   (:,:,k)  = reshape(mean(:,     k),(/im,jm/))
            x_m%v   (:,:,k)  = reshape(mean(:,  km+k),(/im,jm/))
            x_m%pt  (:,:,k)  = reshape(mean(:,2*km+k),(/im,jm/))
!_RT        x_m%delp(:,:,k)  = reshape(mean(:,3*km+k),(/im,jm/))
            do nt=1,1!_RT 6
!_RT           x_m%q(:,:,k,nt) = reshape(mean(:,(3+nt)*km+k),(/im,jm/))
               x_m%q(:,:,k,nt) = reshape(mean(:,(2+nt)*km+k),(/im,jm/))
            enddo
!_RT        x_x%frland(:,:)    = reshape(mean(:,10*km+1),(/im,jm/))
!_RT        x_x%frlandice(:,:) = reshape(mean(:,10*km+2),(/im,jm/))
!_RT        x_x%frlake (:,:)   = reshape(mean(:,10*km+3),(/im,jm/))
!_RT        x_x%frocean(:,:)   = reshape(mean(:,10*km+4),(/im,jm/))
!_RT        x_x%frseaice(:,:)  = reshape(mean(:,10*km+5),(/im,jm/))
!_RT        x_x%hs_stdv(:,:)   = reshape(mean(:,10*km+6),(/im,jm/))
!_RT        x_x%phis   (:,:)   = reshape(mean(:,10*km+7),(/im,jm/))
!_RT        x_x%ts  (:,:)      = reshape(mean(:,10*km+8),(/im,jm/))
!_RT        x_m%ps  (:,:)      = reshape(mean(:,10*km+9),(/im,jm/))
            x_m%ps  (:,:)      = reshape(mean(:, 4*km+1),(/im,jm/))
         enddo
      endif
   endif

!  Write out 
!  ---------
   print *, 'write: ', trim(fname)
   call dyn_put ( fname, nymd, nhms, 0, x_m, ier, freq=freq, nstep=nstep, vectype=5 )
   call dyn_clean (x_m)

   call zeit_co('write_stats')
   
end subroutine write_stats

subroutine usage_

   implicit none
   if( myPE==ROOT ) then
       write(6,*)
       write(6,'(a)') 'Usage: mp_dyn_stats.x [options] files'
       write(6,*)
       write(6,'(a)') 'options:'
       write(6,*)
       write(6,'(a)') '-h                echo these usage lines'
       write(6,'(a)') '-esa_only         perform and output results of ESA only'
       write(6,'(a)') '-o     oFILE      specify output filename of projected fields'
       write(6,'(a)') '-rmse             presumes input is error vector and calcuates'
       write(6,'(a)') '                      sqrt(sum(err*err)/(M-1))  '
       write(6,'(a)') '-sprd  oSPRD      filename for deviation from the mean'
       write(6,'(a)') '-pick  NYMD NHMS  date/time of output file(s)'
       write(6,'(a)') '                    (when absent use date of last file read)'
       write(6,*)
       write(6,*)
   endif
   call mp_Finalize(ierr)
   call exit(1)
end subroutine usage_

end program effread
