module letkf
!$$$  module documentation block
!
! module: letkf                        Update model state variables and
!                                      bias coefficients with the LETKF.
!
! prgmmr: ota              org: np23                   date: 2011-06-01
!         updates, optimizations by whitaker
!
! abstract: Updates the model state using the LETKF (Hunt et al 2007,
!  Physica D, 112-126).
!
!  Covariance localization is used in the state update to limit the impact 
!  of observations to a specified distance from the observation in the
!  horizontal and vertical.  These distances can be set separately in the
!  NH, tropics and SH, and in the horizontal, vertical and time dimensions,
!  using the namelist parameters  corrlengthnh, corrlengthtr, corrlengthsh,
!  lnsigcutoffnh, lnsigcutofftr, lnsigcutoffsh (lnsigcutoffsatnh,
!  lnsigcutoffsattr, lnsigcutoffsatsh for satellite obs, similar for ps obs)
!  obtimelnh, obtimeltr, obtimelsh. The length scales should be given in km for the
!  horizontal, hours for time, and 'scale heights' (units of -log(p/pref)) in the
!  vertical. Note however, that time localization *not used in LETKF*. 
!  The function used for localization (function taper)
!  is imported from module covlocal. Localization requires that
!  every observation have an associated horizontal, vertical and temporal location.
!  For satellite radiance observations the vertical location is given by
!  the maximum in the weighting function associated with that sensor/channel/
!  background state (this computation, along with the rest of the forward
!  operator calcuation, is performed by a separate program using the GSI
!  forward operator code).  Although all the observation variable ensemble
!  members sometimes cannot fit in memory, they are necessary before LETKF core
!  process. So they are saved in all processors.
!
!  Adaptive observation thinning implemented in the serial EnSRF is not 
!  implemented here in the current version.

!  Updating the state in observation space is not supported in the EnKF - 
!  use lupd_obspace_serial=.true. to perform the observation space update
!  using the serial EnSRF.
!
! Public Subroutines:
!  letkf_update: performs the LETKF update (calls update_biascorr to perform
!   the bias coefficient update).  
!
! Public Variables: None
!
! Modules Used: kinds, constants, params, covlocal, mpisetup, loadbal, statevec,
!               enkf_obsmod, radinfo, radbias, gridinfo
!
! program history log:
!   2011-06-01  ota: Created from Whitaker's serial EnSRF core module.
!   2015-07-25  whitaker: Optimization for case when no vertical localization
!               is used. Fixed missing openmp private declarations in obsloop and grdloop.
!               Use openmp reductions for profiling openmp loops. Use kdtree
!               for range search instead of original box routine. Modify
!               ob space update to use weights computed at nearest grid point.
!   2016-02-01  whitaker: Use MPI-3 shared memory pointers to reduce memory
!               footprint by only allocated observation prior ensemble
!               array on one MPI task per node. Also ensure posterior
!               perturbation mean is zero.
!   2016-05-02  shlyaeva: Modification for reading state vector from table.
!   2016-07-05  whitaker: remove buggy code for observation space update.
!               Rely on serial EnSRF to perform observation space update
!               using logical lupd_obspace_serial.
!
! attributes:
!   language: f95
!
!$$$

use mpisetup
use random_normal, only : rnorm, set_random_seed
use, intrinsic :: iso_c_binding
use omp_lib, only: omp_get_num_threads
use covlocal, only:  taper, latval
use kinds, only: r_double,i_kind,r_kind,r_single,num_bytes_for_r_single
use loadbal, only: numptsperproc, npts_max, &
                   indxproc, lnp_chunk, &
                   grdloc_chunk, kdtree_obs2
use statevec, only: ensmean_chunk, anal_chunk
use gridinfo, only: index_pres
use enkf_obsmod, only: oberrvar, ob, ensmean_ob, obloc, oblnp, &
                  nobstot, nobs_conv, nobs_oz, nobs_sat,&
                  obfit_prior, obfit_post, obsprd_prior, obsprd_post,&
                  numobspersat, deltapredx, biaspreds, corrlengthsq,&
                  biasprednorm, probgrosserr, prpgerr, obtype, obpress,&
                  lnsigl, anal_ob, obloclat, obloclon, stattype, iused
use constants, only: pi, one, zero, rad2deg, deg2rad
use params, only: sprd_tol, datapath, nanals, iseed_perturbed_obs,&
                  iassim_order,sortinc,deterministic,nlevs,&
                  zhuberleft,zhuberright,varqc,lupd_satbiasc,huber,letkf_novlocal,&
                  lupd_obspace_serial,corrlengthnh,corrlengthtr,corrlengthsh,&
                  nbackgrounds,nobsl_max,ndim,ensmean_roundoff_fix
use radinfo, only: npred,nusis,nuchan,jpch_rad,predx
use radbias, only: apply_biascorr, update_biascorr
use gridinfo, only: nlevs_pres,lonsgrd,latsgrd,logp,npts,gridloc
use kdtree2_module, only: kdtree2, kdtree2_create, kdtree2_destroy, &
                          kdtree2_result, kdtree2_n_nearest, kdtree2_r_nearest

implicit none

private
public :: letkf_update

contains

subroutine letkf_update()
implicit none
! LETKF update.

! local variables.
integer(i_kind) nob,nf,nanal,&
                i,nrej,npt,nn,nnmax,ierr
integer(i_kind) nobsl, ngrd1, nobsl2, nthreads, nb, &
                nobslocal_min,nobslocal_max, &
                nobslocal_minall,nobslocal_maxall
integer(i_kind),allocatable,dimension(:) :: oindex
real(r_single) :: deglat, dist, corrsq
real(r_double) :: t1,t2,t3,t4,t5,tbegin,tend,tmin,tmax,tmean
real(r_kind) r_nanals,r_nanalsm1,r_scalefact
real(r_kind) normdepart, pnge, width
real(r_kind),dimension(nobstot):: oberrvaruse
real(r_kind) vdist
real(r_kind) corrlength
real(r_kind) sqrtoberr
logical vlocal
! For LETKF core processes
real(r_kind),allocatable,dimension(:,:) :: hxens
real(r_single),allocatable,dimension(:,:) :: obperts,obens
real(r_single),allocatable,dimension(:) :: kfgain
real(r_kind),allocatable,dimension(:) :: rdiag,dep,rloc
real(r_kind),dimension(nanals,nanals) :: trans
real(r_kind),dimension(nanals) :: work,work2
! kdtree stuff
type(kdtree2_result),dimension(:),allocatable :: sresults
#ifdef MPI3
! pointers used for MPI-3 shared memory manipulations.
real(r_single), pointer, dimension(:,:) :: anal_ob_fp ! Fortran pointer
type(c_ptr)                             :: anal_ob_cp ! C pointer
real(r_single), pointer, dimension(:,:) :: obperts_fp ! Fortran pointer
type(c_ptr)                             :: obperts_cp ! C pointer
integer disp_unit, shm_win, shm_win2
integer(MPI_ADDRESS_KIND) :: win_size, nsize
integer(MPI_ADDRESS_KIND) :: segment_size
#endif
real(r_single), allocatable, dimension(:) :: buffer

integer(i_kind), allocatable, dimension(:) :: iused_letkf
allocate(iused_letkf(nobstot))
iused_letkf = 0

!$omp parallel
nthreads = omp_get_num_threads()
!$omp end parallel
if (nproc == 0) print *,'using',nthreads,' openmp threads'

! define a few frequently used parameters
r_nanals=one/float(nanals)
r_nanalsm1=one/float(nanals-1)
r_scalefact = sqrt(float(nanals)/float(nanals-1))

! create random numbers for perturbed obs on root task.
if (.not. deterministic .and. nproc .eq. 0) then
   call set_random_seed(iseed_perturbed_obs,nproc)
   allocate(obperts(nanals, nobstot))
   do nob=1,nobstot
      sqrtoberr=sqrt(oberrvar(nob))
      do nanal=1,nanals
         obperts(nanal,nob) = sqrtoberr*rnorm()
      enddo
      ! make mean/variance are exact.
      obperts(1:nanals,nob) = obperts(1:nanals,nob) - &
                              sum(obperts(:,nob))*r_nanals
      obperts(1:nanals,nob) = obperts(1:nanals,nob)*sqrtoberr/(sqrt(sum(obperts(:,nob)**2)*r_nanalsm1))
   enddo
endif

t1 = mpi_wtime()
#ifdef MPI3
! setup shared memory segment on each node that points to
! observation prior ensemble.
! shared window size will be zero except on root task of
! shared memory group on each node.
disp_unit = num_bytes_for_r_single ! anal_ob is r_single
nsize = nobstot*nanals
if (nproc_shm == 0) then
   win_size = nsize*disp_unit
else
   win_size = 0
endif
call MPI_Win_allocate_shared(win_size, disp_unit, MPI_INFO_NULL,&
                             mpi_comm_shmem, anal_ob_cp, shm_win, ierr)
if (.not. deterministic) then
   call MPI_Win_allocate_shared(win_size, disp_unit, MPI_INFO_NULL,&
                                mpi_comm_shmem, obperts_cp, shm_win2, ierr)
endif
if (nproc_shm == 0) then
   ! create shared memory segment on each shared mem comm
   call MPI_Win_lock(MPI_LOCK_EXCLUSIVE,0,MPI_MODE_NOCHECK,shm_win,ierr)
   call c_f_pointer(anal_ob_cp, anal_ob_fp, [nanals, nobstot])
   ! bcast entire obs prior ensemble from root task 
   ! to a single task on each node, assign to shared memory window.
   ! send one ensemble member at a time.
   allocate(buffer(nobstot))
   do nanal=1,nanals
      if (nproc == 0) buffer(1:nobstot) = anal_ob(nanal,1:nobstot)
      if (nproc_shm == 0) then
         call mpi_bcast(buffer,nobstot,mpi_real4,0,mpi_comm_shmemroot,ierr)
         anal_ob_fp(nanal,1:nobstot) = buffer(1:nobstot)
      end if 
   end do
   if (.not. deterministic) then
      call MPI_Win_lock(MPI_LOCK_EXCLUSIVE,0,MPI_MODE_NOCHECK,shm_win2,ierr)
      call c_f_pointer(obperts_cp, obperts_fp, [nanals, nobstot])
      do nanal=1,nanals
         if (nproc == 0) buffer(1:nobstot) = obperts(nanal,1:nobstot)
         if (nproc_shm == 0) then
            call mpi_bcast(buffer,nobstot,mpi_real4,0,mpi_comm_shmemroot,ierr)
            obperts_fp(nanal,1:nobstot) = buffer(1:nobstot)
         end if 
      end do
   endif
   deallocate(buffer)
   call MPI_Win_unlock(0, shm_win, ierr)
   nullify(anal_ob_fp)
   ! don't need anal_ob anymore
   if (allocated(anal_ob)) deallocate(anal_ob)
   if (.not. deterministic) then
      ! don't need obperts anymore
      call MPI_Win_unlock(0, shm_win2, ierr)
      nullify(obperts_fp)
      if (allocated(obperts)) deallocate(obperts)
   endif
endif
! barrier here to make sure no tasks try to access shared
! memory segment before it is created.
call mpi_barrier(mpi_comm_world, ierr)
! associate fortran pointer with c pointer to shared memory 
! segment (containing observation prior ensemble) on each task.
call MPI_Win_shared_query(shm_win, 0, segment_size, disp_unit, anal_ob_cp, ierr)
call c_f_pointer(anal_ob_cp, anal_ob_fp, [nanals, nobstot])
if (.not. deterministic) then
   call MPI_Win_shared_query(shm_win2, 0, segment_size, disp_unit, obperts_cp, ierr)
   call c_f_pointer(obperts_cp, obperts_fp, [nanals, nobstot])
endif
#else
! if MPI3 not available, need anal_ob on every MPI task
! broadcast observation prior ensemble from root one ensemble member at a time.
allocate(buffer(nobstot))
! allocate anal_ob on non-root tasks
if (nproc .ne. 0) allocate(anal_ob(nanals,nobstot))
! bcast anal_ob from root one member at a time.
do nanal=1,nanals
   buffer(1:nobstot) = anal_ob(nanal,1:nobstot)
   call mpi_bcast(buffer,nobstot,mpi_real4,0,mpi_comm_world,ierr)
   if (nproc .ne. 0) anal_ob(nanal,1:nobstot) = buffer(1:nobstot)
end do
if (.not. deterministic) then
   if (nproc .ne. 0) allocate(obperts(nanals,nobstot))
   do nanal=1,nanals
      buffer(1:nobstot) = obperts(nanal,1:nobstot)
      call mpi_bcast(buffer,nobstot,mpi_real4,0,mpi_comm_world,ierr)
      if (nproc .ne. 0) obperts(nanal,1:nobstot) = buffer(1:nobstot)
   end do
endif
deallocate(buffer)
#endif
t2 = mpi_wtime()
if (nproc .eq. 0) print *,'time to broadcast ob prior ensemble = ',t2-t1

if (nproc .eq. 0 .and. .not. deterministic) then
   print *,'perturbed obs LETKF'
endif
if (minval(lnsigl) > 1.e3 .or. letkf_novlocal) then
   vlocal = .false.
   if (nproc == 0) print *,'no vertical localization in LETKF'
   ! if no vertical localization, weights
   ! need only be computed once for each column.
   nnmax = 1
else
   vlocal = .true.
   ! if vertical localization on, analysis weights
   ! need to be computed for every vertical level.
   nnmax = nlevs_pres
endif

! apply bias correction with latest estimate of bias coeffs
! (if bias correction update in ob space turned on).
if (nobs_sat > 0 .and. lupd_satbiasc .and. lupd_obspace_serial) call apply_biascorr()

nrej=0
! reset ob error to account for gross errors 
if (varqc .and. lupd_obspace_serial) then
    if (huber) then ! "huber norm" QC
      do nob=1,nobstot
        ! observation space update performed in serial filter 
        ! using lupd_obspace_serial
        normdepart = obfit_post(nob)/sqrt(oberrvar(nob))
        ! depends of 2 parameters: zhuberright, zhuberleft.
        if (normdepart < -zhuberleft) then
           pnge = zhuberleft/abs(normdepart)
        else if (normdepart > zhuberright) then
           pnge = zhuberright/abs(normdepart)
        else
           pnge = one
        end if
        ! eqn 17 in Dharssi, Lorenc and Inglesby
        ! divide ob error by prob of gross error not occurring.
        oberrvaruse(nob) = oberrvar(nob)/pnge
        ! pnge is the prob that the ob *does not* contain a gross error.
        ! assume rejected if prob of gross err > 50%.
        probgrosserr(nob) = one-pnge
        if (probgrosserr(nob) > 0.5_r_single) then 
           nrej=nrej+1
        endif
      end do
    else ! "flat-tail" QC.
      do nob=1,nobstot
        ! original form, gross error cutoff a multiple of ob error st dev.
        ! here gross err cutoff proportional to ensemble spread plus ob error
        ! Dharssi, Lorenc and Inglesby eqn (1) a = grosserrw*sqrt(S+R) 
        width = sprd_tol*sqrt(obsprd_prior(nob)+oberrvar(nob))
        pnge = prpgerr(nob)*sqrt(2.*pi*oberrvar(nob))/((one-prpgerr(nob))*(2.*width))
        normdepart = obfit_post(nob)/sqrt(oberrvar(nob))
        pnge = one - (pnge/(pnge+exp(-normdepart**2/2._r_single)))
        ! eqn 17 in Dharssi, Lorenc and Inglesby
        ! divide ob error by prob of gross error not occurring.
        oberrvaruse(nob) = oberrvar(nob)/pnge
        ! pnge is the prob that the ob *does not* contain a gross error.
        ! assume rejected if prob of gross err > 50%.
        probgrosserr(nob) = one-pnge
        if (probgrosserr(nob) > 0.5_r_single) then 
           nrej=nrej+1
        endif
      end do
    endif
else
     oberrvaruse(1:nobstot) = oberrvar(1:nobstot)
end if

tbegin = mpi_wtime()

t2 = zero
t3 = zero
t4 = zero
t5 = zero
tbegin = mpi_wtime()
nobslocal_max = -999
nobslocal_min = nobstot

! Update ensemble on model grid.
! Loop for each horizontal grid points on this task.
!$omp parallel do schedule(dynamic) private(npt,nob,nobsl, &
!$omp                  nobsl2,ngrd1,corrlength, &
!$omp                  nf,vdist,kfgain,obens, &
!$omp                  nn,hxens,rdiag,dep,rloc,i,work,work2,trans, &
!$omp                  oindex,deglat,dist,corrsq,nb,sresults) &
!$omp  reduction(+:t1,t2,t3,t4,t5) &
!$omp  reduction(max:nobslocal_max) &
!$omp  reduction(min:nobslocal_min) 
grdloop: do npt=1,numptsperproc(nproc+1)

   t1 = mpi_wtime()

   ! find obs close to this grid point (using kdtree)
   ngrd1=indxproc(nproc+1,npt)
   deglat = latsgrd(ngrd1)*rad2deg
   corrlength=latval(deglat,corrlengthnh,corrlengthtr,corrlengthsh)
   corrsq = corrlength**2
   ! kd-tree fixed range search
   if (allocated(sresults)) deallocate(sresults)
   if (nobsl_max > 0) then ! only use nobsl_max nearest obs (sorted by distance).
       allocate(sresults(nobsl_max))
       call kdtree2_n_nearest(tp=kdtree_obs2,qv=grdloc_chunk(:,npt),nn=nobsl_max,&
            results=sresults)
       nobsl = nobsl_max
   else ! find all obs within localization radius (sorted by distance).
       allocate(sresults(nobstot))
       call kdtree2_r_nearest(tp=kdtree_obs2,qv=grdloc_chunk(:,npt),r2=corrsq,&
            nfound=nobsl,nalloc=nobstot,results=sresults)
   endif

   t2 = t2 + mpi_wtime() - t1
   t1 = mpi_wtime()

   ! Skip when no observations in local area
   if(nobsl == 0) cycle grdloop

   ! Loop through vertical levels (nnmax=1 if no vertical localization)
   verloop: do nn=1,nnmax

      ! Pick up variables passed to LETKF core process
      allocate(rloc(nobsl))
      allocate(oindex(nobsl))
      nobsl2=1
      do nob=1,nobsl
         nf = sresults(nob)%idx
         ! skip 'screened' obs.
         if (oberrvaruse(nf) > 1.e10_r_single) cycle
         if (vlocal) then
            vdist=(lnp_chunk(npt,nn)-oblnp(nf))/lnsigl(nf)
            if(abs(vdist) >= one) cycle
         else
            vdist = zero
         endif
         dist = sqrt(sresults(nob)%dis/corrlengthsq(sresults(nob)%idx)+vdist*vdist)
         if (dist >= one) cycle
         rloc(nobsl2)=taper(dist)
         oindex(nobsl2)=nf
         if(rloc(nobsl2) > tiny(rloc(nobsl2))) nobsl2=nobsl2+1
      end do
      nobsl2=nobsl2-1
      if (nobsl2 > nobslocal_max) nobslocal_max=nobsl2
      if (nobsl2 < nobslocal_min) nobslocal_min=nobsl2
      if(nobsl2 == 0) then
         deallocate(rloc,oindex)
         cycle verloop
      end if
      allocate(hxens(nanals,nobsl2))
      allocate(rdiag(nobsl2))
      allocate(dep(nobsl2))
      do nob=1,nobsl2
         nf=oindex(nob)
#ifdef MPI3
         hxens(1:nanals,nob)=anal_ob_fp(1:nanals,nf) 
#else
         hxens(1:nanals,nob)=anal_ob(1:nanals,nf) 
#endif
         rdiag(nob)=one/oberrvaruse(nf)
         dep(nob)=ob(nf)-ensmean_ob(nf)
	 iused_letkf(nf) = 1
      end do

      t3 = t3 + mpi_wtime() - t1
      t1 = mpi_wtime()

      if (.not. deterministic) then
         allocate(kfgain(nobsl2),obens(nobsl2,nanals))
         ! add ob perts to observation priors
         do nob=1,nobsl2
            nf = oindex(nob)
            obens(nob,1:nanals) = &
#ifdef MPI3
            obperts_fp(1:nanals,nf) + anal_ob_fp(1:nanals,nf) 
#else
            obperts(1:nanals,nf) + anal_ob(1:nanals,nf) 
#endif
         enddo
      endif
      deallocate(oindex)
  
      ! Compute transformation matrix of LETKF
      call letkf_core(nobsl2,hxens,rdiag,dep,rloc(1:nobsl2),trans)
      deallocate(rloc,rdiag)
      ! if perturbed obs not used, these arrays no longer needed.
      if (deterministic) then
         deallocate(hxens,dep)
      endif
      

      t4 = t4 + mpi_wtime() - t1
      t1 = mpi_wtime()

      ! Update analysis ensembles (all time levels)
      do nb=1,nbackgrounds
      do i=1,ndim
         ! if not vlocal, update all state variables in column.
         if(vlocal .and. index_pres(i) /= nn) cycle
         if (deterministic) then
            work(1:nanals) = anal_chunk(1:nanals,npt,i,nb)
            work2(1:nanals) = ensmean_chunk(npt,i,nb)
            if(r_kind == kind(1.d0)) then
               call dgemv('t',nanals,nanals,1.d0,trans,nanals,work,1,1.d0, &
                    & work2,1)
            else
               call sgemv('t',nanals,nanals,1.e0,trans,nanals,work,1,1.e0, &
                    & work2,1)
            end if
            ensmean_chunk(npt,i,nb) = sum(work2(1:nanals)) * r_nanals
            anal_chunk(1:nanals,npt,i,nb) = work2(1:nanals)-ensmean_chunk(npt,i,nb)
         else ! perturbed obs using LETKF gain.
            do nob=1,nobsl2
               kfgain(nob) = sum(hxens(:,nob)*anal_chunk(:,npt,i,nb))
            enddo
            ensmean_chunk(npt,i,nb) = ensmean_chunk(npt,i,nb) + sum(kfgain*dep)
            do nanal=1,nanals
               anal_chunk(nanal,npt,i,nb) = anal_chunk(nanal,npt,i,nb) - &
               sum(kfgain*obens(:,nanal))
            enddo
         endif
      enddo
      enddo
      ! deallocate arrays needed for perturbed obs LETKF
      if (.not. deterministic) then
         deallocate(hxens,kfgain,dep,obens)
      endif

      t5 = t5 + mpi_wtime() - t1
      t1 = mpi_wtime()

   end do verloop
   if (allocated(sresults)) deallocate(sresults)
end do grdloop
!$omp end parallel do

! make sure posterior perturbations still have zero mean.
! (roundoff errors can accumulate)
if (ensmean_roundoff_fix) then
!$omp parallel do schedule(dynamic) private(npt,nb,i)
  do npt=1,npts_max
     do nb=1,nbackgrounds
        do i=1,ndim
           anal_chunk(1:nanals,npt,i,nb) = anal_chunk(1:nanals,npt,i,nb)-&
             sum(anal_chunk(1:nanals,npt,i,nb),1)*r_nanals
        end do
     end do
  enddo
!$omp end parallel do
endif

tend = mpi_wtime()
call mpi_reduce(tend-tbegin,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
tmean = tmean/numproc
call mpi_reduce(tend-tbegin,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(tend-tbegin,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
if (nproc .eq. 0) print *,'min/max/mean time to do letkf update ',tmin,tmax,tmean
t2 = t2/nthreads; t3 = t3/nthreads; t4 = t4/nthreads; t5 = t5/nthreads
if (nproc == 0) print *,'time to process analysis on gridpoint = ',t2,t3,t4,t5,' secs on task',nproc
call mpi_reduce(t2,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
tmean = tmean/numproc
call mpi_reduce(t2,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(t2,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
if (nproc .eq. 0) print *,',min/max/mean t2 = ',tmin,tmax,tmean
call mpi_reduce(t3,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
tmean = tmean/numproc
call mpi_reduce(t3,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(t3,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
if (nproc .eq. 0) print *,',min/max/mean t3 = ',tmin,tmax,tmean
call mpi_reduce(t4,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
tmean = tmean/numproc
call mpi_reduce(t4,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(t4,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
if (nproc .eq. 0) print *,',min/max/mean t4 = ',tmin,tmax,tmean
call mpi_reduce(t5,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
tmean = tmean/numproc
call mpi_reduce(t5,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(t5,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
if (nproc .eq. 0) print *,',min/max/mean t5 = ',tmin,tmax,tmean
call mpi_reduce(nobslocal_max,nobslocal_maxall,1,mpi_integer,mpi_max,0,mpi_comm_world,ierr)
call mpi_reduce(nobslocal_min,nobslocal_minall,1,mpi_integer,mpi_max,0,mpi_comm_world,ierr)
if (nproc == 0) print *,'min/max number of obs in local volume',nobslocal_minall,nobslocal_maxall
if (nrej > 0 .and. nproc == 0) print *, nrej,' obs rejected by varqc'
  
! free shared memory segement, fortran pointer to that memory.
#ifdef MPI3
nullify(anal_ob_fp)
call MPI_Win_free(shm_win, ierr)
if (.not. deterministic) then
   nullify(obperts_fp)
   call MPI_Win_free(shm_win2, ierr)
endif
#endif
! deallocate anal_ob on non-root tasks.
if (nproc .ne. 0 .and. allocated(anal_ob)) deallocate(anal_ob)
if (allocated(obperts)) deallocate(obperts)

call mpi_allreduce(iused_letkf,iused_letkf,nobstot,mpi_integer,mpi_sum,mpi_comm_world,ierr)
if (nproc .eq. 0) iused = iused_letkf

deallocate(iused_letkf)

return

end subroutine letkf_update

subroutine letkf_core(nobsl,hxens,rdiaginv,dep,rloc,trans)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    letkf_core
!
!   prgmmr: ota
!
! abstract:  LETKF core subroutine computing transform matrix. BLAS subroutines
!            are used for the computational efficiency.
!
! program history log:
!   2011-06-03  ota: created from miyoshi's LETKF core subroutine
!   2014-06-20  whitaker: optimization for case when no vertical localization
!               is used. Fixed missing openmp private declarations in obsloop and grdloop.
!               Use openmp reductions for profiling openmp loops. Use LAPACK
!               routine dsyev for eigenanalysis.
!   2016-02-01  whitaker: Use LAPACK dsyevr for eigenanalysis (faster
!               than dsyev in most cases).
!
!   input argument list:
!     nobsl    - number of observations in the local patch
!     hxens     - first-guess ensembles on observation space
!     rdiaginv - inverse of diagonal element of observation error covariance
!     dep      - observation departure from first guess mean
!     rloc     - localization function to each observations
!
!   output argument list:
!     trans    - transform matrix for this point.
!                On output, hxens is over-written
!                with matrix that can be used to compute Kalman Gain.
!
! attributes:
!   language:  f95
!   machine:
!
!$$$ end documentation block
implicit none
integer(i_kind)                      ,intent(in ) :: nobsl
real(r_kind),dimension(nanals,nobsl ),intent(inout) :: hxens
real(r_kind),dimension(nobsl        ),intent(in ) :: rdiaginv
real(r_kind),dimension(nobsl        ),intent(in ) :: dep
real(r_kind),dimension(nobsl        ),intent(in ) :: rloc
real(r_kind),dimension(nanals,nanals),intent(out) :: trans
real(r_kind), allocatable, dimension(:,:) :: work1,work2,eivec,pa
real(r_kind), allocatable, dimension(:) :: rrloc,eival,work3
real(r_kind) :: rho
integer(i_kind) :: i,j,nob,nanal,ierr,lwork
!for dsyevr
integer(i_kind) iwork(10*nanals),isuppz(2*nanals)
real(r_kind) vl,vu,work(70*nanals)
!for dsyevd
!integer(i_kind) iwork(5*nanals+3)
!real(r_kind) work(2*nanals*nanals+6*nanals+1)
allocate(work3(nanals),work2(nanals,nobsl))
allocate(eivec(nanals,nanals),pa(nanals,nanals))
allocate(work1(nanals,nanals),eival(nanals),rrloc(nobsl))
! hxens sqrt(Rinv)
rrloc(1:nobsl) = rdiaginv(1:nobsl) * rloc(1:nobsl)
rho = tiny(rrloc)
where (rrloc < rho) rrloc = rho
rrloc = sqrt(rrloc)
do nanal=1,nanals
   hxens(nanal,1:nobsl) = hxens(nanal,1:nobsl) * rrloc(1:nobsl)
end do
! hxens^T Rinv hxens
!do j=1,nanals
!   do i=1,nanals
!      work1(i,j) = hxens(i,1) * hxens(j,1)
!      do nob=2,nobsl
!         work1(i,j) = work1(i,j) + hxens(i,nob) * hxens(j,nob)
!      end do
!   end do
!end do
if(r_kind == kind(1.d0)) then
   call dgemm('n','t',nanals,nanals,nobsl,1.d0,hxens,nanals, &
        hxens,nanals,0.d0,work1,nanals)
else
   call sgemm('n','t',nanals,nanals,nobsl,1.e0,hxens,nanals, &
        hxens,nanals,0.e0,work1,nanals)
end if
! hdxb^T Rinv hdxb + (m-1) I
do nanal=1,nanals
   work1(nanal,nanal) = work1(nanal,nanal) + real(nanals-1,r_kind)
end do
! eigenvalues and eigenvectors of [ hdxb^T Rinv hdxb + (m-1) I ]
! use LAPACK dsyev
eivec(:,:) = work1(:,:); lwork = -1
call dsyev('V','L',nanals,eivec,nanals,eival,work1(1,1),lwork,ierr)
lwork = min(nanals*nanals, int(work1(1,1)))
call dsyev('V','L',nanals,eivec,nanals,eival,work1(1,1),lwork,ierr)
! use LAPACK dsyevd
!call dsyevd('V','L',nanals,eivec,nanals,eival,work,size(work),iwork,size(iwork),ierr)
! use LAPACK dsyevr 
!call dsyevr('V','A','L',nanals,work1,nanals,vl,vu,1,nanals,-1.d0,nanals,eival,eivec, &
!            nanals,isuppz,work,size(work),iwork,size(iwork),ierr)
if (ierr .ne. 0) print *,'warning: dsyev* failed, ierr=',ierr
! Pa = [ hdxb^T Rinv hdxb + (m-1) I ]inv
do j=1,nanals
   do i=1,nanals
      work1(i,j) = eivec(i,j) / eival(j)
   end do
end do
!do j=1,nanals
!   do i=1,nanals
!      pa(i,j) = work1(i,1) * eivec(j,1)
!      do k=2,nanals
!         pa(i,j) = pa(i,j) + work1(i,k) * eivec(j,k)
!      end do
!   end do
!end do
if(r_kind == kind(1.d0)) then
   call dgemm('n','t',nanals,nanals,nanals,1.d0,work1,nanals,eivec,&
        nanals,0.d0,pa,nanals)
else
   call sgemm('n','t',nanals,nanals,nanals,1.e0,work1,nanals,eivec,&
        nanals,0.e0,pa,nanals)
end if
! convert hxens * Rinv^T from hxens * sqrt(Rinv)^T
do nanal=1,nanals
   hxens(nanal,1:nobsl) = hxens(nanal,1:nobsl) * rrloc(1:nobsl)
end do
! Pa hdxb_rinv^T
!do nob=1,nobsl
!   do nanal=1,nanals
!      work2(nanal,nob) = pa(nanal,1) * hxens(1,nob)
!      do k=2,nanals
!         work2(nanal,nob) = work2(nanal,nob) + pa(nanal,k) * hxens(k,nob)
!      end do
!   end do
!end do
if(r_kind == kind(1.d0)) then
   call dgemm('n','n',nanals,nobsl,nanals,1.d0,pa,nanals,hxens,&
        nanals,0.d0,work2,nanals)
else
   call sgemm('n','n',nanals,nobsl,nanals,1.e0,pa,nanals,hxens,&
        nanals,0.e0,work2,nanals)
end if
! over-write hxens with Pa hdxb_rinv
! (pre-multiply with ensemble perts to compute Kalman gain - 
!  eqns 20-23 in Hunt et al 2007 paper)
hxens = work2
! work3 = Pa hdxb_rinv^T dep
do nanal=1,nanals
   work3(nanal) = work2(nanal,1) * dep(1)
   do nob=2,nobsl
      work3(nanal) = work3(nanal) + work2(nanal,nob) * dep(nob)
   end do
end do
! T = sqrt[(m-1)Pa]
do j=1,nanals
   rho = sqrt( real(nanals-1,r_kind) / eival(j) )
   do i=1,nanals
      work1(i,j) = eivec(i,j) * rho
   end do
end do
if(r_kind == kind(1.d0)) then
   call dgemm('n','t',nanals,nanals,nanals,1.d0,work1,nanals,eivec,&
        & nanals,0.d0,trans,nanals)
else
   call sgemm('n','t',nanals,nanals,nanals,1.e0,work1,nanals,eivec,&
        & nanals,0.e0,trans,nanals)
end if
!do j=1,nanals
!   do i=1,nanals
!      trans(i,j) = work1(i,1) * eivec(j,1)
!      do k=2,nanals
!         trans(i,j) = trans(i,j) + work1(i,k) * eivec(j,k)
!      end do
!   end do
!end do
! T + Pa hdxb_rinv^T dep
do j=1,nanals
   do i=1,nanals
      trans(i,j) = trans(i,j) + work3(i)
   end do
end do
deallocate(work2,eivec,pa,work1,rrloc,eival,work3)

return
end subroutine letkf_core

end module letkf
