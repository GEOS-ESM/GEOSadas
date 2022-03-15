module m_GsiGrided

   use type_kinds, only: fp_kind,double
   use m_die, only: die

   implicit none
   private ! except

   public :: GsiGrided
   public :: GsiGrided_set
   public :: GsiGrided_init
   public :: GsiGrided_read
   public :: GsiGrided_clean
   public :: vectype
   public :: input_vpsf

   type GsiGrided
!     private
     real(fp_kind),allocatable,dimension(:)   :: ak,bk
     real(fp_kind),allocatable,dimension(:,:) :: ps
     real(fp_kind),allocatable,dimension(:,:,:) :: sf
     real(fp_kind),allocatable,dimension(:,:,:) :: vp
     real(fp_kind),allocatable,dimension(:,:,:) :: vt
     real(fp_kind),allocatable,dimension(:,:,:) :: q
     real(fp_kind),allocatable,dimension(:,:,:) :: qi
     real(fp_kind),allocatable,dimension(:,:,:) :: ql
     real(fp_kind),allocatable,dimension(:,:,:) :: qr
     real(fp_kind),allocatable,dimension(:,:,:) :: qs
     real(fp_kind),allocatable,dimension(:,:,:) :: cw
     real(fp_kind),allocatable,dimension(:,:,:) :: oz
     real(fp_kind),allocatable,dimension(:,:,:) :: rh
     real(fp_kind),allocatable,dimension(:,:,:) :: mrh
   end type GsiGrided

   interface GsiGrided_init ; module procedure    &
      init_ ; end interface
   interface GsiGrided_set ; module procedure    &
      set_ ; end interface
   interface GsiGrided_read ; module procedure    &
      read_ ; end interface
   interface GsiGrided_clean; module procedure    &
      clean_; end interface

   character(len=*),parameter :: myname='m_GsiGrided'
   real(fp_kind),parameter :: kPa_per_Pa=.001
   real(fp_kind),parameter :: PPMV2GG=1.6571e-6
   real(fp_kind),parameter :: CPD    = 1004.64
   real(fp_kind),parameter :: RGAS   = 287.04
   real(fp_kind),parameter :: KAPPA = RGAS/CPD

   integer :: vectype
   logical :: input_vpsf

contains

   subroutine init_(ob)

   use variables, only : nlat, nlon, nsig

   implicit none
   type(GsiGrided),intent(out) :: ob

     allocate(ob%ak(nsig+1))
     allocate(ob%bk(nsig+1))

     allocate(ob%ps  (nlat,nlon)     )
     allocate(ob%sf  (nlat,nlon,nsig))
     allocate(ob%vp  (nlat,nlon,nsig))
     allocate(ob%vt  (nlat,nlon,nsig))
     allocate(ob%q   (nlat,nlon,nsig))
     allocate(ob%qi  (nlat,nlon,nsig))
     allocate(ob%ql  (nlat,nlon,nsig))
     allocate(ob%qr  (nlat,nlon,nsig))
     allocate(ob%qs  (nlat,nlon,nsig))
     allocate(ob%cw  (nlat,nlon,nsig))
     allocate(ob%oz  (nlat,nlon,nsig))
     allocate(ob%rh  (nlat,nlon,nsig))
     allocate(ob%mrh (nlat,nlon,nsig))

     ob%ak  = 0
     ob%bk  = 0
     ob%ps  = 0
     ob%sf  = 0
     ob%vp  = 0
     ob%vt  = 0
     ob%q   = 0
     ob%qi  = 0
     ob%ql  = 0
     ob%qr  = 0
     ob%qs  = 0
     ob%cw  = 0
     ob%oz  = 0
     ob%rh  = 0
     ob%mrh = 0

   end subroutine init_

   subroutine set_
   implicit none

   vectype = 5
   input_vpsf = .false.

   end subroutine set_

   subroutine read_(fid, nymd, nhms, ob, mype)

   use variables, only : nlat, nlon, nsig, filename
   use variables, only : lgaus,rlats,pi
   use variables, only : nGlat, nGlon, glats
   use variables, only : readperts

   use m_dyn,only : dyn_vect
   use m_dyn,only : dyn_get
   use m_dyn,only : dyn_clean
   use m_dyn,only : dyn_flip

   use m_llInterp, only : llInterp
   use m_llInterp, only : llInterp_init
   use m_llInterp, only : llInterp_clean
   use m_llInterp, only : llInterp_atog

   use m_daInterp, only : daInterp_vdtoa

   use m_set_eta,  only: set_eta

   implicit none 

   integer,intent(in) :: fid, mype
   integer,intent(out) :: nymd, nhms
   type(GsiGrided),intent(inout) :: ob

! local variables

   integer :: nAlon, nAlat, nAlev
   integer :: nstep, ier, ierror, k, i, j
   integer :: nslat,nslon,i1,j1
   integer :: ks
   real(double) :: ptop, pint
   real(fp_kind),allocatable,dimension(:) :: var1s
   real(fp_kind),allocatable,dimension(:,:,:) :: var3su,var3sv
   real(double),allocatable,dimension(:) :: pl,plk
   real(double),allocatable,dimension(:,:)    :: var2d
   real(double),allocatable,dimension(:,:,:)  :: var3du,var3dv
   type(dyn_vect) :: w_fv
   type(llInterp) :: obll,obl
   character(len=3),pointer::xtrnames(:)
   logical dointerp
   logical dovecwnds
   

   if (readperts) then
     allocate(xtrnames(2))
     xtrnames = (/'rh ','mrh'/)
     call dyn_get(filename(fid),nymd,nhms,w_fv, &
                  ier,timidx=1,vectype=vectype,xtrnames=xtrnames)
     deallocate(xtrnames)
   else
     call dyn_get(filename(fid),nymd,nhms,w_fv, &
                  ier,timidx=1,vectype=vectype)
   endif
   call dyn_flip(w_fv,dover=.false.) ! This is required since the calc 
                                     ! of div/vor; sf/vp before assume
                                     ! the lon [0,360]; see inisph (RT)

!  Atmospheric model dimensions
     nAlon = w_fv%grid%im
     nAlat = w_fv%grid%jm  
     nAlev = w_fv%grid%km

     if(nsig .ne. nAlev) then
       write(6,*)"STOP: nsig .ne. nAlev",nsig,nAlev,vectype
       call mpi_finalize(ierror)
       stop
     end if 

     dointerp = lgaus.or.(mod(nlat,2)==0)
     if( .not. dointerp ) then
       nslat=(nAlat-1)/(nlat-1)
       nslon=nAlon/nlon
       if(  nslat ==0 .or. nslon==0   .or.   &
          ( nslat*(nlat-1) .ne. (nAlat-1) ) .or.   &
          ( nslon*nlon .ne. nAlon ) ) then
         write(6,*)"STOP: DIMENSION NOT MATCHED"
         call mpi_finalize(ierror)
         stop
       end if
     end if 

!wgutest_b11p2_d1
     call set_eta(nsig, ks, ptop, pint, w_fv%grid%ak, w_fv%grid%bk)
!wgutest_b11p2_d1
     ob%ak = w_fv%grid%ak
     ob%bk = w_fv%grid%bk
   
     if(vectype == 4 )then
!      transform from scaled virtual potential temperature
!      to virtual temperature 
       allocate ( pl(nAlev+1) )
       allocate ( plk(nAlev+1) )
       do j=1,nAlat
         do i=1,nAlon
           do k= nAlev+1,1,-1
             pl(k) = ob%ak(k)+ob%bk(k)*w_fv%ps(i,j)
           end do
           plk = pl ** kappa
           do k = 1, nAlev
             w_fv%pt(i,j,k) = w_fv%pt(i,j,k)    &
                      * (plk(k+1)-plk(k))       &
                      / (log(plk(k+1))-log(plk(k)))
           end do
         end do
       end do
       deallocate ( pl )
       deallocate ( plk )
     endif

     if( dointerp ) then
       call llInterp_init(obll,nAlon,nAlat,nlon,nlat)
     endif

!  Surface Pressure
     allocate( var2d(nlon,nlat) )
     if(dointerp)then
       call llInterp_atog(obll,w_fv%ps,var2d)
     else
       do j=1,nAlat,nslat
        do i=1,nAlon,nslon
          j1=1+(j-1)/nslat
          i1=1+(i-1)/nslon 
          var2d(i1,j1)=w_fv%ps(i,j)
        enddo
       enddo
     endif
     call swapij2d_(var2d,ob%ps,nlat,nlon)
     deallocate( var2d )

!  Divergence and Vorticity
     if(vectype == 4 )then
       allocate( var3du(nAlon, nAlat, nAlev) ) 
       allocate( var3dv(nAlon, nAlat, nAlev) ) 
       call daInterp_vdtoa(w_fv%u,w_fv%v,var3du,var3dv)
       w_fv%u = var3du
       w_fv%v = var3dv
       deallocate ( var3du, var3dv )
     endif
     dovecwnds=.not.input_vpsf

     if(dointerp)then
       allocate( var3su(nlat,nlon,nsig) )
       allocate( var3sv(nlat,nlon,nsig) )
       allocate( var3du(nlon,nlat,nsig) ) 
       call llInterp_atog(obll,w_fv%u, var3du, vector=dovecwnds)
       call swapij3d_(var3du,var3su,nlat,nlon,nsig)
       call llInterp_atog(obll,w_fv%v, var3du, vector=dovecwnds)
       call swapij3d_(var3du,var3sv,nlat,nlon,nsig)
     else
       call llInterp_init(obl,nAlon,nAlat,nGlon,nGlat)
       allocate( var3su(nGlat,nGlon,nsig) )
       allocate( var3sv(nGlat,nGlon,nsig) )
       allocate( var3du(nGlon,nGlat,nsig) ) 
       call llInterp_atog(obl,w_fv%u, var3du, vector=dovecwnds)
       call swapij3d_(var3du,var3su,nGlat,nGlon,nsig)
       call llInterp_atog(obl,w_fv%v, var3du, vector=dovecwnds)
       call swapij3d_(var3du,var3sv,nGlat,nGlon,nsig)
       call llInterp_clean(obl)
     endif
     deallocate( var3du ) 
     if (input_vpsf) then
        ob%vp=var3su
        ob%sf=var3sv
     else
        call m_vordiv(var3su,var3sv)
        call m_stvp(ob%sf,ob%vp,var3su,var3sv) 
     endif
     deallocate( var3su )
     deallocate( var3sv )

!  Virtual Temperature
   
     allocate( var3du(nlon, nlat, nsig) ) 
     if(dointerp) then
       call llInterp_atog(obll,w_fv%pt,var3du, vector=.false.)
     else
       do j=1,nAlat,nslat
        do i=1,nAlon,nslon
          j1=1+(j-1)/nslat
          i1=1+(i-1)/nslon 
          var3du(i1,j1,1:nAlev)=w_fv%pt(i,j,1:nAlev)
        enddo
       enddo
     endif
     call swapij3d_(var3du, ob%vt, nlat, nlon, nsig)

!  Specific Humidity
     if(dointerp) then
       call llInterp_atog(obll,w_fv%q(:,:,:,1),var3du, &
                      vector=.false.,norder=1)
     else
       do j=1,nAlat,nslat
        do i=1,nAlon,nslon
          j1=1+(j-1)/nslat
          i1=1+(i-1)/nslon 
          var3du(i1,j1,1:nAlev)=w_fv%q(i,j,1:nAlev,1)
        enddo
       enddo
     endif 
     call swapij3d_(var3du, ob%q, nlat, nlon, nsig)

!  Ozone
     if(dointerp) then
       call llInterp_atog(obll,w_fv%q(:,:,:,2),var3du, &
                      vector=.false.,norder=1)
     else
       do j=1,nAlat,nslat
        do i=1,nAlon,nslon
          j1=1+(j-1)/nslat
          i1=1+(i-1)/nslon 
          var3du(i1,j1,1:nAlev)=w_fv%q(i,j,1:nAlev,2)
        enddo
       enddo
     endif 
     call swapij3d_(var3du, ob%oz, nlat, nlon, nsig)

! qi,ql,qr,qs
     if(dointerp) then
       call llInterp_atog(obll,w_fv%q(:,:,:,3),var3du, &
                      vector=.false.,norder=1)
     else
       do j=1,nAlat,nslat
        do i=1,nAlon,nslon
          j1=1+(j-1)/nslat
          i1=1+(i-1)/nslon 
          var3du(i1,j1,1:nAlev)=w_fv%q(i,j,1:nAlev,3)
        enddo
       enddo
     endif 
     call swapij3d_(var3du, ob%qi, nlat, nlon, nsig)

     if(dointerp) then
       call llInterp_atog(obll,w_fv%q(:,:,:,4),var3du, &
                      vector=.false.,norder=1)
     else
       do j=1,nAlat,nslat
        do i=1,nAlon,nslon
          j1=1+(j-1)/nslat
          i1=1+(i-1)/nslon 
          var3du(i1,j1,1:nAlev)=w_fv%q(i,j,1:nAlev,4)
        enddo
       enddo
     endif 
     call swapij3d_(var3du, ob%ql, nlat, nlon, nsig)

     if(dointerp) then
       call llInterp_atog(obll,w_fv%q(:,:,:,5),var3du, &
                      vector=.false.,norder=1)
     else
       do j=1,nAlat,nslat
        do i=1,nAlon,nslon
          j1=1+(j-1)/nslat
          i1=1+(i-1)/nslon 
          var3du(i1,j1,1:nAlev)=w_fv%q(i,j,1:nAlev,5)
        enddo
       enddo
     endif 
     call swapij3d_(var3du, ob%qr, nlat, nlon, nsig)

     if(dointerp) then
       call llInterp_atog(obll,w_fv%q(:,:,:,6),var3du, &
                      vector=.false.,norder=1)
     else
       do j=1,nAlat,nslat
        do i=1,nAlon,nslon
          j1=1+(j-1)/nslat
          i1=1+(i-1)/nslon 
          var3du(i1,j1,1:nAlev)=w_fv%q(i,j,1:nAlev,6)
        enddo
       enddo
     endif 
     call swapij3d_(var3du, ob%qs, nlat, nlon, nsig)

     if (readperts) then
! rh diff
        if(dointerp) then
          call llInterp_atog(obll,w_fv%q(:,:,:,7),var3du, &
                         vector=.false.,norder=1)
        else
          do j=1,nAlat,nslat
           do i=1,nAlon,nslon
             j1=1+(j-1)/nslat
             i1=1+(i-1)/nslon 
             var3du(i1,j1,1:nAlev)=w_fv%q(i,j,1:nAlev,7)
           enddo
          enddo
        endif 
        call swapij3d_(var3du, ob%rh, nlat, nlon, nsig)

! mean rh
        if(dointerp) then
          call llInterp_atog(obll,w_fv%q(:,:,:,8),var3du, &
                         vector=.false.,norder=1)
        else
          do j=1,nAlat,nslat
           do i=1,nAlon,nslon
             j1=1+(j-1)/nslat
             i1=1+(i-1)/nslon 
             var3du(i1,j1,1:nAlev)=w_fv%q(i,j,1:nAlev,8)
           enddo
          enddo
        endif 
        call swapij3d_(var3du, ob%mrh, nlat, nlon, nsig)
     endif

!  Cloud Liquid Water

     ob%cw = 0


     if(dointerp)call llInterp_clean(obll)
     call dyn_clean(w_fv) 
     deallocate ( var3du )

!  Changing Units 
     ob%ps = ob%ps * kPa_per_Pa
     ob%ak = ob%ak * kPa_per_Pa
     ob%oz = ob%oz * PPMV2GG
 
!  Vertical Swap 
     allocate ( var1s(nsig+1) )
     var1s(1:nsig+1) = ob%ak(nsig+1:1:-1)
     ob%ak = var1s
     var1s(1:nsig+1) = ob%bk(nsig+1:1:-1)
     ob%bk = var1s
     deallocate ( var1s )

     allocate( var3su(nlat, nlon, nsig) )

     var3su(:,:,1:nsig) = ob%vp (:, :, nsig:1:-1)
     ob%vp = var3su
     var3su(:,:,1:nsig) = ob%sf (:, :, nsig:1:-1)
     ob%sf = var3su
     var3su(:,:,1:nsig) = ob%vt (:, :, nsig:1:-1)
     ob%vt = var3su
     var3su(:,:,1:nsig) = ob%q  (:, :, nsig:1:-1)
     ob%q = var3su
     var3su(:,:,1:nsig) = ob%qi (:, :, nsig:1:-1)
     ob%qi = var3su
     var3su(:,:,1:nsig) = ob%ql (:, :, nsig:1:-1)
     ob%ql = var3su
     var3su(:,:,1:nsig) = ob%qr (:, :, nsig:1:-1)
     ob%qr = var3su
     var3su(:,:,1:nsig) = ob%qs (:, :, nsig:1:-1)
     ob%qs = var3su
     var3su(:,:,1:nsig) = ob%cw (:, :, nsig:1:-1)
     ob%cw = var3su
     var3su(:,:,1:nsig) = ob%oz (:, :, nsig:1:-1)
     ob%oz = var3su
     if (readperts) then
        var3su(:,:,1:nsig) = ob%rh (:, :, nsig:1:-1)
        ob%rh= var3su
        var3su(:,:,1:nsig) = ob%mrh(:, :, nsig:1:-1)
        ob%mrh= var3su
     else
        ob%rh= 0.0
        ob%mrh= 0.0
     endif

     deallocate ( var3su )

   end subroutine read_

   subroutine clean_(ob)

     implicit none
     type(GsiGrided),intent(inout) :: ob
  
     deallocate(ob%mrh )
     deallocate(ob%rh  )
     deallocate(ob%oz  )
     deallocate(ob%cw  )
     deallocate(ob%qs  )
     deallocate(ob%qr  )
     deallocate(ob%ql  )
     deallocate(ob%qi  )
     deallocate(ob%q   )
     deallocate(ob%vt  )
     deallocate(ob%sf  )
     deallocate(ob%vp  )

     deallocate(ob%ps  )

     deallocate(ob%bk  )
     deallocate(ob%ak  )

   end subroutine clean_

end module m_GsiGrided

subroutine swapij2d_(aij,aji,nlat,nlon)
  use type_kinds, only : fp_kind, double
  implicit none
  integer,intent(in) :: nlat,nlon
  real(double),dimension(nlon,nlat),intent(in ) :: aij
  real(fp_kind),dimension(nlat,nlon),intent(out) :: aji
                
  integer :: i,j
  do i=1,nlon
    aji(1:nlat,i)=aij(i,1:nlat)
  end do
end subroutine swapij2d_


subroutine swapij3d_(aij,aji,nlat,nlon,nsig)

  use type_kinds, only : fp_kind, double
  implicit none
  integer,intent(in) :: nlat,nlon,nsig
  real(double), dimension(nlon,nlat,nsig),intent(in ) :: aij
  real(fp_kind),dimension(nlat,nlon,nsig),intent(out) :: aji
  integer :: i,j,k
              
  do k=1,nsig
    do i=1,nlon
      aji(1:nlat,i,k)=aij(i,1:nlat,k)
    end do
  end do

end subroutine swapij3d_


