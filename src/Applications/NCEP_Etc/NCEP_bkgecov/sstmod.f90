module sstmod
!   def varsst   - 2d variances for sea surface temperature
!   def corlsst  - 2d horizontal length scales for sea surface temperature  
!
! $$$
  use type_kinds, only: fp_kind
  use m_spline, only: spline
  implicit none

  real(fp_kind),allocatable,dimension(:,:):: varsst,corlsst

  logical :: do_spline=.false.

contains

 subroutine create_sstvars(nlat,nlon)

  integer,intent(in):: nlat,nlon
  allocate(varsst(nlat,nlon),corlsst(nlat,nlon))

 end subroutine create_sstvars


 subroutine destroy_sstvars
  implicit none

  deallocate(varsst,corlsst)

 end subroutine destroy_sstvars


 subroutine sst_stats
   use type_kinds, only: fp_kind,single
   use variables, only: nlat,nlon,rlats,rlons,deg2rad
   implicit none

   integer i,j,k,mype,ilt,iln,idx

   real*4,dimension(720,360):: sstintmp
   real*4,dimension(720,360,9):: sst2in

   real(fp_kind),dimension(360,720):: sstvin,sstcin
   real(fp_kind) linlat(360)
   real(fp_kind) linlon(720)
   real(fp_kind),allocatable,dimension(:):: rlatint,rlonint
   real(fp_kind),allocatable,dimension(:):: rlatbig,rlonbig,sstv1,sstc1
   real(4),allocatable,dimension(:,:):: var_out,cor_out

   integer, parameter :: lui=23
   integer, parameter :: luo=24

   allocate(rlatint(nlat),rlonint(nlon))
   allocate(rlatbig(nlat*nlon),rlonbig(nlat*nlon))
   allocate(sstv1(nlat*nlon),sstc1(nlat*nlon))

   ! these fields are transposed by oriented as in GEOS: [-180,180] and [-90,90]
   ilt=360
   iln=720
   open(lui,file='berror_sst',access='direct',&
       recl=720*360*4,form='unformatted')

   read (lui,rec=1) sstintmp
   do k=2,9
     read (lui,rec=k) ((sst2in(i,j,k-1),i=1,iln),j=1,ilt)
   end do
   do j=1,iln
     do i=1,ilt
       sstcin(i,j)=sst2in(j,i,8)
       sstvin(i,j)=sstintmp(j,i)
     end do
   end do

   call sst_grads_(luo,'origsst',real(sstvin,4),real(sstcin,4),.true.)

! the sst variances has missing values in it, which need to be filled 
! with more realistic values
  
   do i=1,200 !RT: this is real bad hack
     call fillsstv(sstvin,ilt,iln)
   end do

   call sst_grads_(luo,'nofillsst',real(sstvin,4),real(sstcin,4),.true.)

! load the lats/lons of the 0.5 x 0.5 linear grid
   do j=1,ilt
     linlat(j)=deg2rad*(0.5**2.-90.+(j-1)*0.5)
   end do
   do i=1,iln
     linlon(i)=deg2rad*((i-1)*0.5)
   end do

! load that lats/lons of the desired Gaussian grid for getting 
! grid coordinates on the linear grid
   do i=1,nlat
     rlatint(i)=rlats(i)
   end do
   do j=1,nlon
     rlonint(j)=rlons(j)
   end do


   if (do_spline) then
      allocate(var_out(nlat,iln),cor_out(nlat,iln))
      do i=1,iln
         call spline( linlat, rlatint, sstvin(:,i), var_out(:,i) )
         call spline( linlat, rlatint, sstcin(:,i), cor_out(:,i) )
      enddo
      do j=1,nlat
         call spline( linlon, rlonint, var_out(j,:), varsst (j,:) )
         call spline( linlon, rlonint, cor_out(j,:), corlsst(j,:) )
      enddo
      deallocate(var_out,cor_out)
   else
!     get linear grid coordinate numbers of gaussian points
      call gdcrdp(rlatint,nlat,linlat,ilt)
      call gdcrdp(rlonint,nlon,linlon,iln)

!     load nlat*nlon arrays for 2d interpolation
      idx=0
      do j=1,nlon
        do i=1,nlat
          idx=idx+1
          rlatbig(idx)=rlatint(i)
          rlonbig(idx)=rlonint(j)
        end do
      end do 

!     perform interpolation of linear grid fields to Gaussian
      call intrp2(sstvin,sstv1,rlatbig,rlonbig,ilt,iln,nlat*nlon)
      call intrp2(sstcin,sstc1,rlatbig,rlonbig,ilt,iln,nlat*nlon)

      idx=0
      do j=1,nlon
        do i=1,nlat
          idx=idx+1
          varsst(i,j)=sstv1(idx)
          corlsst(i,j)=sstc1(idx)
        end do
      end do 

   endif

   call sst_grads_(luo,'sst4gsi',real(varsst,4),real(corlsst,4),.true.)

   deallocate(sstv1,sstc1)
   deallocate(rlatbig,rlonbig)
   deallocate(rlatint,rlonint)

   return
 contains

   subroutine sst_grads_(lu,prefix_fname,var,clen,trans)
   integer, intent(in) :: lu
   real(4), intent(in) :: var(:,:),clen(:,:)
   character(len=*),intent(in) :: prefix_fname
   logical,intent(in) :: trans
   real(4),allocatable:: aux(:,:)
   integer :: im,jm
   im=size(var,1); jm=size(var,2) 
   allocate(aux(im,jm))
   open(lu,file=trim(prefix_fname)//'.grd',form='unformatted',convert='little_endian',access='sequential')
   if(trans) then
     aux=transpose(var)
   else
     aux=var
   endif
   write(lu) aux
   if(trans) then
      aux=transpose(clen)
   else
     aux=clen
   endif
   write(lu) aux
   close(lu)
   deallocate(aux)
   if (trans) then
     call write_grads_ctl(prefix_fname,lu,jm,im)
   else
     call write_grads_ctl(prefix_fname,lu,im,jm)
   endif
   end subroutine sst_grads_

 end subroutine sst_stats

 subroutine write_grads_ctl (fname, lu,im,jm)
   implicit none
   character(len=*), intent(in) :: fname
   integer, intent(in) :: lu,im,jm

   open(lu,file=trim(fname)//'.ctl',form='formatted')
   write(lu,'(2a)') 'dset  ^', trim(fname)//'.grd'
   write(lu,'(2a)') 'title ', 'sst berror variances/corlength'
   write(lu,'(a)')  'options little_endian'
   write(lu,'(a,2x,f6.1)') 'undef', -999.0 ! any other preference for this?
   write(lu,'(a,2x,i4,2x,a,2x,f5.1,2x,f9.6)') 'xdef',im, 'linear',   0.0, 360./im
   write(lu,'(a,2x,i4,2x,a,2x,f5.1,2x,f9.6)') 'ydef',jm, 'linear', -90.0, 180./(jm-1.)
   write(lu,'(a)')      'zdef 1 linear 1 1'
   write(lu,'(a,2x,i4,2x,a)')   'tdef', 1, 'LINEAR 12:00Z04JUL1776 6hr' ! any date suffices
   write(lu,'(a,2x,i4)')        'vars', 2
   write(lu,'(a,1x,2(i4,1x),a)') 'sst',   1,0, 'sst'
   write(lu,'(a,1x,2(i4,1x),a)') 'sstcorl',   1,0, 'sstcorl'
   write(lu,'(a)') 'endvars'
   close(lu)

   
 end subroutine write_grads_ctl

 subroutine gdcrdp(d,nd,x,nx)
   use type_kinds, only: fp_kind
   implicit none

   integer id,nd,ix,nx,isrchfge
   real(fp_kind),dimension(nd):: d
   real(fp_kind),dimension(nx):: x

   do id=1,nd
     if(d(id)<=x(1)) then
       ix=1
     else
       ix=isrchfge(nx-1,x,d(id))-1
     end if
     if(ix==nx) ix=ix-1
     d(id)=float(ix)+(d(id)-x(ix))/(x(ix+1)-x(ix))
   end do

   return
 end subroutine gdcrdp


 subroutine intrp2(fin,gout,dx,dy,nxin,nyin,isize)
! perform horizontal interpolation
   use type_kinds, only: fp_kind
   implicit none

   integer ione,m1,mype,i,ix1,iy1,ix,iy,ixp,iyp
   integer isize,k,nxin,nyin
   real(fp_kind) delx,dely,delxp,delyp
   real(fp_kind),dimension(nxin,nyin):: fin
   real(fp_kind),dimension(isize):: dx,dy,gout

   ione=1

   do i=ione,isize
     ix1=int(dx(i))
     iy1=int(dy(i))
     ix1=max(ione,min(ix1,nxin))
     iy1=max(ione,min(iy1,nyin))

     delx=dx(i)-float(ix1)
     dely=dy(i)-float(iy1)
     delx=max(0._fp_kind,min(delx,1._fp_kind))
     dely=max(0._fp_kind,min(dely,1._fp_kind))

     ix=ix1
     iy=iy1
     ixp=ix+ione; iyp=iy+ione 

     if(ix1==nxin) then
       ixp=ix
     end if
     if(iy1==nyin) then
       iyp=iy
     end if

     delxp=1-delx; delyp=1-dely 

     gout(i)=fin(ix,iy)*delxp*delyp+fin(ixp,iy)*delx*delyp&
            +fin(ix,iyp)*delxp*dely+fin(ixp,iyp)*delx*dely

   end do
   return
 end subroutine intrp2

 subroutine fillsstv(sst,nx,ny)
   use type_kinds, only: fp_kind
   implicit none

   integer,intent(in):: nx,ny
   integer i,j
   real(fp_kind),dimension(nx,ny):: sst

! search for -999.000 and fill in with alternate/near gridpoint
! value if possible

    do j=1,ny
      do i=1,nx
        if(sst(i,j).eq.(-999.0)) then
          if((i.ne.nx).AND.(sst(i+1,j).ne.(-999.0))) then
            sst(i,j)=sst(i+1,j)
          else if ((i.ne.1).AND.(sst(i-1,j).ne.(-999.0))) then
            sst(i,j)=sst(i-1,j)
          else if ((j.ne.ny).AND.(sst(i,j+1).ne.(-999.0))) then
            sst(i,j)=sst(i,j+1)
          else if ((j.ne.1).AND.(sst(i,j-1).ne.(-999.0))) then
            sst(i,j)=sst(i,j-1)
          else
            continue
          end if
        end if
      end do
    end do 
  
   return
 end subroutine fillsstv

end module sstmod
