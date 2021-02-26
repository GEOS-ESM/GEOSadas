module specgrid

  use type_kinds, only: fp_kind,single,double
  use m_die, only: die
  implicit none

  integer jcap,nc,ncd2
  integer iromb,idrt,imax,jmax,ijmax,jn,js,kw,jb,je,jc,ioffset
  real(single),allocatable,dimension(:):: factsml,factvml
  real(single),allocatable,dimension(:):: eps,epstop,enn1,elonn1,eon,eontop
  real(single),allocatable,dimension(:):: clat,slat,wlat
  real(single),allocatable,dimension(:,:):: pln,plntop
  real(double),allocatable,dimension(:):: afft

contains

  subroutine init_spec
    implicit none
    jcap = 62
    return
  end subroutine init_spec

  subroutine init_spec_vars(jcap0)
    implicit none

    integer,intent(in) :: jcap0

    integer ii,ii1,l,m
    real(single) zero1

!   Set constants
    jcap=jcap0
    nc=(jcap0+1)*(jcap0+2)
    ncd2=nc/2

!   Allocate more arrays related to transforms
    allocate(factsml(nc),factvml(nc))
!   Set up factsml and factvml
    ii=-1; ii1=0
    do l=0,jcap0
       zero1=float(min(1,l))
       do m=0,jcap0-l
          ii=ii+2; ii1=ii1+2
          factsml(ii)=1.; factsml(ii1)=zero1
          factvml(ii)=1.; factvml(ii1)=zero1
       end do
    end do
    factvml(1)=0.

    return
  end subroutine init_spec_vars

  subroutine init_spec_grid(nlat,nlon,lgaus)
    implicit none

    logical,intent(in) :: lgaus
    integer,intent(in) :: nlat,nlon
    integer ncpus

!   Set other constants used in transforms
    iromb=0
    if(lgaus) then
      idrt=4
      imax=nlon
      jmax=nlat-2
    else
      idrt=0
      imax=nlon
      jmax=nlat
    endif
    ijmax=imax*jmax
    ioffset=imax*(jmax-1)
    jn=imax
    js=-jn
    kw=2*ncd2
    jb=1
    je=(jmax+1)/2
    jc=ncpus()

!   Allocate arrays
    allocate( eps(ncd2) )
    allocate( epstop(jcap+1) )
    allocate( enn1(ncd2) )
    allocate( elonn1(ncd2) )
    allocate( eon(ncd2) )
    allocate( eontop(jcap+1) )
    allocate( afft(50000+4*imax) )
    allocate( clat(jb:je) )
    allocate( slat(jb:je) )
    allocate( wlat(jb:je) )
    allocate( pln(ncd2,jb:je) )
    allocate( plntop(jcap+1,jb:je) )

!   Initialize arrays used in transforms
    call sptranf0(iromb,jcap,idrt,imax,jmax,jb,je, &
       eps,epstop,enn1,elonn1,eon,eontop, &
       afft,clat,slat,wlat,pln,plntop)

    return
  end subroutine init_spec_grid

  subroutine sptez_s(wave,grid,idir)
    use type_kinds, only: fp_kind,single
    implicit none

! Declare passed variables
    integer,intent(in):: idir
    real(single),dimension(nc),intent(inout):: wave
    real(single),dimension(ijmax),intent(inout):: grid

! Declare local variables
    integer i

! Zero appropriate output array based on direction of transform
    if (idir<0) then
      do i=1,nc
        wave(i)=0.
      end do
    elseif (idir>0) then
      do i=1,ijmax
        grid(i)=0.
      end do
    endif

! Call spectral <--> grid transform
    call sptranf_s(wave,grid,grid,idir)

    return
  end subroutine sptez_s

  subroutine destroy_spec_vars
    deallocate(factsml,factvml)
    return
  end subroutine destroy_spec_vars


  subroutine destroy_spec_grid
    deallocate(eps,epstop,enn1,elonn1,eon,eontop,afft,&
       clat,slat,wlat,pln,plntop)
    return
  end subroutine destroy_spec_grid


  subroutine sptranf_s(wave,gridn,grids,idir)
    use type_kinds, only: fp_kind,single
    implicit none

! Declare passed variables
    integer,intent(in):: idir
    real(single),dimension(nc),intent(inout):: wave
    real(single),dimension(ijmax),intent(inout):: gridn
    real(single),dimension(ijmax),intent(inout):: grids

! Declare local variables
    integer i,j,jj,ij,ijn,ijs,mp
    real(single),dimension(2*(jcap+1)):: wtop
    real(single),dimension(imax,2):: g

! Initialize local variables
    mp=0
    
    do i=1,2*(jcap+1)
      wtop(i)=0.
    end do

! Transform wave to grid
    if(idir.gt.0) then
      do j=jb,je
        call sptranf1(iromb,jcap,idrt,imax,jmax,j,j, &
             eps,epstop,enn1,elonn1,eon,eontop, &
             afft,clat(j),slat(j),wlat(j), &
             pln(1,j),plntop(1,j),mp, &
             wave,wtop,g,idir)
        do i=1,imax
          jj  = j-jb
          ijn = i + jj*jn
          ijs = i + jj*js + ioffset
          gridn(ijn)=g(i,1)
          grids(ijs)=g(i,2)
        enddo
      enddo
! Transform grid to wave
    else
      do j=jb,je
        if(wlat(j).gt.0.) then
          do i=1,imax
            jj  = j-jb
            ijn = i + jj*jn
            ijs = i + jj*js + ioffset
            g(i,1)=gridn(ijn)
            g(i,2)=grids(ijs)
          enddo
          call sptranf1(iromb,jcap,idrt,imax,jmax,j,j, &
                eps,epstop,enn1,elonn1,eon,eontop, &
                afft,clat(j),slat(j),wlat(j), &
                pln(1,j),plntop(1,j),mp, &
                wave,wtop,g,idir)
        endif
      enddo
    endif
    return
  end subroutine sptranf_s

  subroutine fill_ns(grid_in,nlat,nlon,grid_out)
    use type_kinds, only: fp_kind,single
    implicit none

    integer,intent(in):: nlat,nlon
    real(single),dimension(imax,jmax),intent(in):: grid_in  ! input grid
    real(single),dimension(nlat,nlon),intent(out):: grid_out  ! output grid
!  Declare local variables
    integer i,j,k,jj,nlatm2
    real(single) rnlon,sumn,sums

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
    if(imax .ne. nlon) then
      call die('fill_ns','Error: imax.ne.nlon',999)
    endif 
    if(idrt==0 .and. jmax.ne.nlat) then
      call die('fill_ns','Error: idrt==0 & jmax.ne.nlat',999)
    endif 
    if(idrt==4 .and. jmax.ne.nlat-2) then
      call die('fill_ns','Error: idrt==4 & jmax.ne.nlat-2',999)
    endif 

    if(idrt==0)then
      do j=1,nlat
        jj=nlat-j+1
        do i=1,nlon
          grid_out(j,i)=grid_in(i,jj)
        end do
      end do
    endif
    if(idrt==4)then
    do j=2,nlat-1
      jj=nlat-j
      do i=1,nlon
        grid_out(j,i)=grid_in(i,jj)
      end do
    end do

!  Compute mean along southern and northern latitudes
    sumn=0.
    sums=0.
    if(idrt==0)then
      nlatm2=nlat
    else if(idrt==4)then
      nlatm2=nlat-2
    endif
    do i=1,nlon
      sumn=sumn+grid_in(i,1)
      sums=sums+grid_in(i,nlatm2)
    end do
    rnlon=1./float(nlon)
    sumn=sumn*rnlon
    sums=sums*rnlon

!  Load means into local work array
    do i=1,nlon
      grid_out(1,i)   =sums
      grid_out(nlat,i)=sumn
    end do
    endif

    return
  end subroutine fill_ns

  subroutine load_grid(grid_in,nlat,nlon,grid_out)
    use type_kinds, only: single
    implicit none

    integer,intent(in):: nlat,nlon
    real(single),dimension(nlat,nlon),intent(in):: grid_in        ! input grid
    real(single),dimension(imax,jmax),intent(out):: grid_out    ! output grid

    integer i,j,k,nlatm1,jj,j2

!  Transfer contents of local array to output array.
    if(imax .ne. nlon) then
      call die('fill_ns','Error: imax.ne.nlon',999)
    endif 
    if(idrt==0 .and. jmax.ne.nlat) then
      call die('fill_ns','Error: idrt==0 & jmax.ne.nlat',999) 
    endif 
    if(idrt==4 .and. jmax.ne.nlat-2) then
      call die('fill_ns','Error: idrt==4 & jmax.ne.nlat-2',999)
    endif 
    nlatm1=nlat-1
    if(idrt==4)then
      do j=2,nlatm1
        j2=nlat-j+1
        jj=j-1
        do i=1,nlon
          grid_out(i,jj)=grid_in(j2,i)
        end do
      end do
    else if(idrt==0)then
      do j=1,nlat
        j2=nlat-j+1
        jj=j
        do i=1,nlon   
          grid_out(i,jj)=grid_in(j2,i)
        enddo
      end do
    endif

    return
 end subroutine load_grid

end module specgrid

