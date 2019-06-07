module smooth_mod

! model dependent smoothing of inflation estimate.
! This version is for GEOS, expects data to be on global lat-lon grids
! Isotropic spectral smoothing (gaussian) is used.

use mpisetup
use params, only:  ndim, nlons, nlats, smoothparm
use kinds, only:  r_kind, i_kind, r_single
use gridinfo, only: npts, ntrunc
use constants, only: zero

implicit none

private
public :: smooth

contains 

subroutine smooth(grids)
! horizontal smoothing of 2d grids.
! version for equally-spaced lat/lon grids with poles.
! when called, grids contains unsmoothed grids on
! root task.  on return, grids on root will contain
! smoothed grids.
! smoothing controlled by parameter smoothparm.
use specmod, only: sptez_s, init_spec_vars, jcap, isinitialized
implicit none
integer(i_kind) np,ierr,m,nmdim,nm,nn,n,delta,npmax
real(r_single), intent(inout) :: grids(npts,ndim) ! there are ndim 2d grids.
real(r_single) smoothfact ! smoothing parameter.
real(r_kind) reggrd(nlons*nlats)
real(r_kind), allocatable, dimension(:) :: specdat
integer(i_kind) n1(0:numproc-1),n2(0:numproc-1)
if(ntrunc<=0) return
delta = ndim/numproc
if (delta*numproc < ndim) delta = delta + 1
npmax = 0
do np=0,numproc-1
   n1(np) = 1 + np*delta
   n2(np) = (np+1)*delta
   if (n2(np) > ndim) n2(np) = ndim
   if (n1(np) > ndim .and. npmax == 0) npmax = np-1
enddo
! spectrally smooth the grids
! bcast out to all procs.
if (nproc <= npmax) then
  if (.not. isinitialized) call init_spec_vars(nlons,nlats,ntrunc,0)
  do nn=1,ndim
    if (nn < n1(nproc) .or. nn > n2(nproc)) grids(:,nn)=zero
  enddo
  nmdim = (ntrunc+1)*(ntrunc+2)/2
  allocate(specdat(2*nmdim))
  do nn=n1(nproc),n2(nproc)
     reggrd = grids(:,nn) 
     call sptez_s(specdat,reggrd,-1)
     nm = 1
     do m=0,ntrunc
        do n=m,ntrunc
           smoothfact = exp(-(float(n)/smoothparm)**2)
           specdat(nm) = smoothfact*specdat(nm)
           specdat(nm+1) = smoothfact*specdat(nm+1)
           nm = nm + 2
        enddo
     enddo
     call sptez_s(specdat,reggrd,1)
     grids(:,nn) = reggrd
  enddo !nn=1,ndim
  deallocate(specdat)
else ! np > npmax
  grids = zero
end if 
do nn=1,ndim
  call mpi_allreduce(mpi_in_place,grids(1,nn),npts,mpi_real4,mpi_sum,mpi_comm_world,ierr)
enddo
end subroutine smooth
end module smooth_mod
