 subroutine addoz

   use type_kinds,only: single,fp_kind
   use variables, only: ozvar,ozhln,ozvln
   use variables, only: nlat,nlon,nsig

   implicit none
   integer :: nlat0,nsig0

   real(single),allocatable,dimension(:)   :: tmp1d
   real(single),allocatable,dimension(:,:) :: tmp2d
   real(single),allocatable,dimension(:,:) :: ozvar0,ozhln0,ozvln0
   real(single),allocatable,dimension(:,:) :: ozvar1,ozhln1,ozvln1

   integer j,outf,ier

!  Single precision variables for visualization

   ozvar=0
   ozhln=0
   ozvln=0
   outf=45

   open(outf,file='oz.dat',form='unformatted')
   rewind outf
   read(outf) nsig0,nlat0
   allocate(tmp1d(nlat0),tmp2d(nlat0,nsig0))
   allocate(ozvar0(nlat0,nsig0),ozhln0(nlat0,nsig0),ozvln0(nlat0,nsig0))
   rewind outf
   read(outf) nsig0,nlat0,&
              tmp2d,tmp2d,tmp2d,tmp2d,tmp2d,ozvar0,tmp2d,tmp1d,&
              tmp2d,tmp2d,tmp2d,tmp2d,ozhln0,tmp2d,tmp1d,&
              tmp2d,tmp2d,tmp2d,tmp2d,ozvln0,tmp2d
   close(outf)

   if(nsig0/=nsig.and.nlat==nlat0)then  ! interpolate vertically only
     do j=1,nlat
        call mapz1d_(ozvar0(j,:),ozvar(j,:))
        call mapz1d_(ozhln0(j,:),ozhln(j,:))
        call mapz1d_(ozvln0(j,:),ozvln(j,:))
     enddo
   endif
   if(nsig0==nsig.and.nlat0/=nlat)then ! horizontal interpolation only
     call lat_interp_(ozvar0,ozvar)
     call lat_interp_(ozhln0,ozhln)
     call lat_interp_(ozvln0,ozvln)
   endif
   if(nsig0/=nsig.and.nlat0/=nlat)then ! both horz and vert interps
     ! first do horizontal ...
     allocate(ozvar1(nlat,nsig0),ozhln1(nlat,nsig0),ozvln1(nlat,nsig0))
     call lat_interp_(ozvar0,ozvar1)
     call lat_interp_(ozhln0,ozhln1)
     call lat_interp_(ozvln0,ozvln1)
     ! ... then do vertical
     do j=1,nlat
        call mapz1d_(ozvar1(j,:),ozvar(j,:))
        call mapz1d_(ozhln1(j,:),ozhln(j,:))
        call mapz1d_(ozvln1(j,:),ozvln(j,:))
     enddo
     deallocate(ozvar1,ozhln1,ozvln1)
   endif

   deallocate(ozvar0,ozhln0,ozvln0)
   deallocate(tmp1d,tmp2d)

contains

  subroutine lat_interp_(fldi,fldo)
  use m_interpack, only: interpack_terpv
  implicit none
  real(fp_kind) fldi(:,:), fldo(:,:)

  integer jmi,jmo,km
  integer jj,kk
  real(4), allocatable :: auxi(:,:), auxo(:,:)

! transpose fields on way in
  jmi=size(fldi,1)
  jmo=size(fldo,1)
  km =size(fldi,2)
  if(size(fldo,2)/=km) then
    print *, 'lat_interp: incompatible dims, aborting'
    stop
  endif

  allocate(auxi(1,jmi),auxo(1,jmo))
  do kk=1,km

!    from GSI(lat,lon) to GEOS(lon,lat)
!    from GSI(np,sp) to GEOS(sp,np)
     call gsi2geos(1,jmi,fldi(:,kk),auxi)
    
     call interpack_terpv ( 1, jmi, auxi, 1, jmo, auxo, &
                            lon0=-180.d0, fill=1.d15 ) ! fill is not relevant here

!    transpose fields on way out
     call geos2gsi(1,jmo,auxo,fldo(:,kk))
  enddo
  deallocate(auxi,auxo)

  end subroutine lat_interp_
  subroutine mapz1d_ (fldi, fldo)
    use m_mapz, only: map1_ppm
    use m_set_eta, only: set_eta
    use m_const, only: kappa,pstd
    implicit none
    real(fp_kind) :: fldi(:), fldo(:) ! caution w/ this map1_ppm applies differently to diff lats

    real(8),allocatable,dimension(:,:,:):: f_m, f_n
    real(8),allocatable,dimension(:):: akm,bkm
    real(8),allocatable,dimension(:):: akn,bkn
    real(8),allocatable,dimension(:):: plem,plen
    real(8),allocatable,dimension(:,:):: pkem
    real(8),allocatable,dimension(:,:):: pken
    real(8) ptopm,pintm
    real(8) ptopn,pintn
    real ps
    integer k
    integer km,ksm
    integer kn,ksn

    km=size(fldi)
    kn=size(fldo)

    allocate(akm(km+1),bkm(km+1))
    allocate(akn(kn+1),bkn(kn+1))
    allocate(plem(km+1),plen(kn+1))
    allocate(pkem(1,km+1),pken(1,kn+1))
    allocate(f_m(1,1,km),f_n(1,1,kn))

    call set_eta ( km,ksm,ptopm,pintm,akm,bkm )
    call set_eta ( kn,ksn,ptopn,pintn,akn,bkn )

    ps=pstd*100.
    plem = akm + ps*bkm
    plen = akn + ps*bkn
    pkem(1,:) = plem**kappa
    pken(1,:) = plen**kappa
    
    do k=1,km ! from gsi vertical to geos vertical
       f_m(1,1,k) = fldi(km-k+1)
    enddo
    call map1_ppm ( km,   pkem,   f_m, &
                    kn,   pken,   f_n, &
                    1, 1, 1, 1, 1, 1, 1, 3, 1.d15)
    do k=1,kn ! from geos vertical to gsi vertical
       fldo(k)=f_n(1,1,kn-k+1)
    enddo

    deallocate(f_m,f_n)
    deallocate(pkem,pken)
    deallocate(akn,bkn)
    deallocate(akm,bkm)

    end subroutine mapz1d_
!
  subroutine gsi2geos (im,jm,fldi,fldo)
  implicit none
  integer im,jm
  real(fp_kind) fldi(jm,im)
  real(fp_kind) fldo(im,jm)
  integer jj,jgeos
  do jj=1,jm
     jgeos=jj!jm-jj+1
     fldo(:,jgeos) = fldi(jj,:)
  enddo
  end subroutine gsi2geos
  subroutine geos2gsi (im,jm,fldi,fldo)
  implicit none
  integer im,jm
  real(fp_kind) fldi(im,jm)
  real(fp_kind) fldo(jm,im)
  integer jj,jgeos
  do jj=1,jm
     jgeos=jj!jm-jj+1
     fldo(jj,:)=fldi(:,jgeos)
  enddo
  end subroutine geos2gsi

end subroutine addoz

