module m_saber_err
private
   public :: saber_err_init
   public :: saber_err_set
   public :: saber_err_get
   public :: saber_err_get_bounds
   public :: saber_err_final

   interface saber_err_init
      module procedure init_
   end interface
   interface saber_err_set
      module procedure parameterized_sigo_
   end interface
   interface saber_err_get_bounds
      module procedure get_bounds_
   end interface
   interface saber_err_get
      module procedure get_
   end interface
   interface saber_err_final
      module procedure final_
   end interface

   real,allocatable :: plevs(:)
   real,allocatable :: sigo(:)

   integer, parameter :: nlevs=72   ! should be optional
!  real, parameter :: alpha  = -12.0 ! empirical
   real, parameter :: alpha  = -16.0 ! empirical
!  real, parameter :: errormin = 0.3 ! empirical
!  real, parameter :: errormax = 1.4 ! empirical
!  real, parameter :: alpha = -20.0  ! empirical - Jan06-
   real, parameter :: errormin = 0.8 ! empirical - Jan06-
   real, parameter :: errormax = 2.2 ! empirical - Jan06-
!  real, parameter :: pabove = 0.1
!  real, parameter :: pbelow = 5.
   real, parameter :: pabove = 0.05
   real, parameter :: pbelow = 6.

   character(len=*),parameter :: myname = 'm_saber_err'
   logical :: initialized_ = .false.
   logical :: set_ = .false.
contains
   subroutine init_
   character(len=*), parameter :: myname_ = myname//'*init_'
   if(initialized_) return
   allocate ( plevs(nlevs), sigo(nlevs) )
   initialized_=.true.
   print *, trim(myname_), ': done'
   end subroutine init_
   subroutine parameterized_sigo_

   use m_set_eta, only: set_eta
   use m_set_eta, only: get_ref_plevs

   implicit none

   real(8) :: ptop8, pint8
   real(8),allocatable :: ak8(:),bk8(:)
   real(8),allocatable :: pk(:),err(:)
   integer ii,ks

   if(set_) return
   allocate(ak8(nlevs+1),bk8(nlevs+1),pk(nlevs),err(nlevs))

   call set_eta ( nlevs, ks, ptop8, pint8, ak8, bk8 ) 
   call get_ref_plevs ( ak8, bk8, ptop8, pk )

   err = 0.0
   do ii=1,nlevs
      if (pk(ii)>pbelow) then
        err(ii) = errormin
      elseif (pk(ii)<pabove) then
        err(ii) = errormax
      else
        err(ii) = errormin * exp(alpha * (pk(ii)-pbelow)/100)
      endif
   enddo

   if(size(plevs)<=nlevs .and. size(sigo)<=nlevs) then
     if (.not. initialized_ ) call init_()
     plevs = pk(1:nlevs)
     sigo  = err(1:nlevs)
     do ii=1,nlevs
        print *, plevs(ii), sigo(ii)
     enddo
   else
     print *, "parameterized_sigo: error setting obs error"       
     stop (1)
   endif
   deallocate(ak8,bk8,pk,err)
   set_=.true.
   end subroutine parameterized_sigo_

   subroutine get_bounds_ ( emin, emax )
   real, intent(out) :: emin, emax
   emin=errormin
   emax=errormax
   end subroutine get_bounds_
   
   subroutine get_ ( oblev, error )
   implicit none
   real, intent(in)  :: oblev
   real, intent(out) :: error
   real deno
   integer ia,ib,ii

   call parameterized_sigo_()
   if (oblev>=pbelow) then
      error = errormin
    elseif (oblev<=pabove) then
      error = errormax
    else
      ia = -999
      ib = -999
      do ii=1,nlevs
         if (oblev <= plevs(ii)) then
            ib = ii
            exit
         endif
      enddo
      ia=min(ib+1,nlevs-1)
      if (ia /= ib) then
         deno = log(plevs(ib))-log(plevs(ia))
         error = (log(plevs(ib))-log(oblev))*sigo(ib) + (log(oblev)-log(plevs(ia)))*sigo(ia)
         error = error/deno
!        print *, 'a,b,a,b:  ', oblev,plevs(ia),plevs(ib), sigo(ia),sigo(ib)
      else
!        print *, 'b:  ', sigo(ib)
         error = sigo(ib)
      endif
    endif
    if ( error > errormax ) then
       print *, 'get_: DEBUG, error insanity (err/max): ', error, errormax
       stop(1)
    endif

   end subroutine get_

   subroutine final_
   if (.not. initialized_) return
   deallocate ( plevs, sigo )
   initialized_=.false.
   end subroutine final_

end module m_saber_err
