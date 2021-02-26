module variables

  use type_kinds, only: ip_kind, fp_kind, double, single
  implicit none

! These will be made flexible via namelist argument ... easy to do later
  integer, parameter :: n3d = 12
  integer, parameter :: n2d = 1
  character(len=4), parameter :: vars3d(n3d) = (/  &
      'sf  ', 'vp  ', 't   ', 'oz  ', 'q   ',  'cw  ', 'qi  ','ql  ', 'qr  ', 'qs  ', 'rh  ', 'mrh ' /) 
  character(len=4), parameter :: vars2d(n2d) = (/  &
      'ps  ' /)
! general
  integer maxcases
  integer filunit,filunit1,filunit2

! forecast pair file variables
  character(len=100),allocatable,dimension(:):: filename
  integer,allocatable,dimension(:):: na,nb

! from GSI gridmod:
  logical hybrid,db_prec,anaclw,lgaus,lsmver,biasrm,laddoz,lbal
  logical hydromet
  logical readperts
  logical calchrzscl,calcvrtscl
  logical smooth_vert_variances
  integer nlat,nlon,nsig,dimbig,option,lat1,lon1
  integer nGlat,nGlon
  integer nreaders
  real(fp_kind),dimension(2):: rhbounds
  real(fp_kind) smoothdeg,fnm0
  real(fp_kind) :: taj
  real(fp_kind) :: hrzsfactor
  real(fp_kind),allocatable,dimension(:):: sigl,sigi,ak5,bk5
  real(fp_kind),allocatable,dimension(:):: rlats,rlons
  real(fp_kind),allocatable,dimension(:):: glats,glons
  real(fp_kind),allocatable,dimension(:,:):: vsmth,vsmc

! MPI related stuff
  integer mype,npe,iglobal,itotsub
  integer(ip_kind),allocatable,dimension(:):: jstart  ! start lon of the whole array on each pe
  integer(ip_kind),allocatable,dimension(:):: istart  ! start lat of the whole array on each pe
  integer(ip_kind),allocatable,dimension(:):: ilat1   ! no. of lats for each subdomain (no buffer)
  integer(ip_kind),allocatable,dimension(:):: jlon1   ! no. of lons for each subdomain (no buffer)
  integer(ip_kind),allocatable,dimension(:):: ijn_s   ! no. of horiz. points for each subdomain (with buffer)
  integer(ip_kind),allocatable,dimension(:):: ijn     ! no. of horiz. points for each subdomain (no buffer)
  integer(ip_kind),allocatable,dimension(:):: isc_g   ! no. array, count for send to global; size of subdomain

                                                 ! comm. array ...
  integer(ip_kind),allocatable,dimension(:):: irc_s     !   count for receive on subdomain
  integer(ip_kind),allocatable,dimension(:):: ird_s     !   displacement for receive on subdomain
  integer(ip_kind),allocatable,dimension(:):: isd_g     !   displacement for send to global
  integer(ip_kind),allocatable,dimension(:):: displs_s  !   displacement for send from subdomain
  integer(ip_kind),allocatable,dimension(:):: displs_g  !   displacement for receive on global grid

                                                 ! array element indices for location of ...
  integer(ip_kind),allocatable,dimension(:):: ltosi   !   lats in iglobal array excluding buffer
  integer(ip_kind),allocatable,dimension(:):: ltosj   !   lons in iglobal array excluding buffer
  integer(ip_kind),allocatable,dimension(:):: ltosi_s !   lats in itotsub array including buffer
  integer(ip_kind),allocatable,dimension(:):: ltosj_s !   lons in itotsub array including buffer

  integer(ip_kind) :: fcst_origin                     ! specifies fcst field origin
                                                      !   0=GFS (default)
                                                      !   1=GMAO


! allocateable arrays
  real(fp_kind),allocatable,dimension(:):: sweight
  real(fp_kind),allocatable,dimension(:,:  ) :: bbp
  real(fp_kind),allocatable,dimension(:,:,:) :: bbt,bbs,bbv
  real(fp_kind),allocatable,dimension(:,:,:) :: bboz,bbq,bbqi,bbql,bbqr,bbqs

! smoothing coefficients
  integer, parameter :: nlmax=200
  real(fp_kind) :: hcoeffs(nlmax)
  real(fp_kind) :: vcoeffs(nlmax)

! Bias correction arrays
  real(fp_kind),allocatable,dimension(:,:,:):: bbiasz,bbiasd,bbiast
  real(fp_kind),allocatable,dimension(:,:,:):: bcorrz,bcorrd,bcorrt
  real(fp_kind),allocatable,dimension(:,:  ):: bbiasp,bcorrp

! variances
  real(fp_kind),allocatable,dimension(:,:) :: sfvar,vpvar,tvar
  real(fp_kind),allocatable,dimension(:,:) :: qvar,cvar,nrhvar,ozvar
  real(fp_kind),allocatable,dimension(:,:) :: qivar,qlvar,qrvar,qsvar
  real(fp_kind),allocatable,dimension(:  ) :: psvar

! balances
  real(fp_kind),allocatable,dimension(:,:) :: vpbal,tbal
  real(fp_kind),allocatable,dimension(:  ) :: pbal
! horizontal length scales
  real(fp_kind),allocatable,dimension(:,:) :: sfhln,vphln,thln
  real(fp_kind),allocatable,dimension(:,:) :: qhln,chln,ozhln
  real(fp_kind),allocatable,dimension(:,:) :: qihln,qlhln,qrhln,qshln
  real(fp_kind),allocatable,dimension(:  ) :: pshln

! vertical length scales
  real(fp_kind),allocatable,dimension(:,:) :: sfvln,vpvln,tvln
  real(fp_kind),allocatable,dimension(:,:) :: qvln,cvln,ozvln
  real(fp_kind),allocatable,dimension(:,:) :: qivln,qlvln,qrvln,qsvln

! balance constraints
  integer,allocatable,dimension(:,:) :: updex,bmdex
  real(fp_kind),allocatable,dimension(:,:,:) :: tcon
  real(fp_kind),allocatable,dimension(:,:  ) :: pscon,vpcon

! from GSI constants:
  integer izero
  real(fp_kind) rearth,rd,rv,cp,cvap,cliq,zero,one,hvap,&
                psat,ttp,fv,pi,hfus,csol,deg2rad,grav,&
                half,two,omega

! define parms
  parameter(rearth = 6.3712e+6_fp_kind)             !  radius of earth                 (m)
  parameter(omega  = 7.2921e-5_fp_kind)             !  angular velocity of earth       (1/s)
  parameter(rd     = 2.8705e+2_fp_kind)             !  gas constant of dry air         (J/kg/K)
  parameter(rv     = 4.6150e+2_fp_kind)             !  gas constant of h2o vapor       (J/kg/K)
  parameter(cp     = 1.0046e+3_fp_kind)             !  specific heat of air @pressure  (J/kg/K)
  parameter(cvap   = 1.8460e+3_fp_kind)             !  specific heat of h2o vapor      (J/kg/K)
  parameter(cliq   = 4.1855e+3_fp_kind)             !  specific heat of liquid h2o     (J/kg/K)
  parameter(hvap   = 2.5000e+6_fp_kind)             !  latent heat of h2o condensation (J/kg)
  parameter(psat   = 6.1078e+2_fp_kind)             !  pressure at h2o triple point    (Pa)
  parameter(csol   = 2.1060e+3_fp_kind)             !  specific heat of solid h2o (ice)(J/kg/K)
  parameter(ttp    = 2.7316e+2_fp_kind)             !  temperature at h2o triple point (K)
  parameter(hfus   = 3.3358e+5_fp_kind)             !  latent heat of h2o fusion       (J/kg)
  parameter(pi     = 3.141593e+0_fp_kind)           !  pi                              ()
  parameter(grav   = 9.80665_fp_kind)               ! gravity
  parameter(izero  = 0)
  parameter(zero   = 0.0_fp_kind)
  parameter(one    = 1.0_fp_kind)
  parameter(two    = 2.0_fp_kind)
  parameter(half   = one/two)

! Derived constants
  parameter(fv = rv/rd-1._fp_kind)                  ! used in virtual temp. equation   ()

contains 

  subroutine init_vars

    implicit none
    
    maxcases=10
    nsig=72
    nlat=258
    nlon=512
    fnm0=nsig
    lgaus=.true.
    lsmver=.false.
    hybrid=.false.
    smoothdeg=4.0
    dimbig=5000
    anaclw=.false.
    taj=1
    biasrm=.false.
    laddoz=.false.
    lbal=.false.
    hydromet=.true.
    smooth_vert_variances=.false. ! to correspond to Wei''s original code this
                                  ! needs to be set to true; false corresponds
                                  ! to what Amal used in 513 or so.
    nreaders=4
    readperts=.false.
    calchrzscl=.true.             ! it is sometimes useful to bypass calc of horz scales
    calcvrtscl=.true.             ! it is sometimes useful to bypass calc of vert scales
    rhbounds(1) = -0.25           ! lower bound on rh differences
    rhbounds(2) =  1.25           ! upper bound on rh differences
    hrzsfactor=-1.0               ! apply factor to horizontal scales (when >0)

  end subroutine init_vars

  subroutine init_smooth_vars(nsig)
  integer,intent(in) :: nsig
    hcoeffs=0.0
    if ( nsig==72 ) then
!      hcoeffs(1:17)=10.0
!      hcoeffs(18:22)=2.0
!      hcoeffs(23:28)=1.0
!      hcoeffs(29:35)=0.2
!      hcoeffs(36:44)=1.0
!      hcoeffs(45:58)=10.0
!      hcoeffs(59:72)=360.0
       hcoeffs(1:35)= 10.0
       hcoeffs(36:48)=100
       hcoeffs(49:58)=10.0
       hcoeffs(59:72)=360.0
    endif
    if (nsig==132) then
        hcoeffs(1:72)  =10.0   !upto about 100 mb
        hcoeffs(73:103)=100    !upto about  10 mb
        hcoeffs(104:118)=10.0  !upto about   1 mb
        hcoeffs(119:132)=360.0 !upto the top
    endif

    vcoeffs=0.0
    if ( nsig==72 ) then
       vcoeffs(1:17)=10.0
       vcoeffs(18:44)=4.0
       vcoeffs(45:58)=10.0
       vcoeffs(59:72)=360.0
    endif
    if (nsig==132) then
       vcoeffs(1:26)=10.0   ! upto about 700 mb
       vcoeffs(27:96)=4.0   ! upto about  21 mb
       vcoeffs(97:118)=10.0 ! upto about   1 mb
       vcoeffs(119:132)=360.0
    endif
  end subroutine init_smooth_vars

  subroutine create_grids(nlat,nlon,jcap,lgauss)
    implicit none

    logical,intent(in) :: lgauss
    integer,intent(in) :: nlat,nlon,jcap

!  local variables 
    integer i,ii,l,m,i1
    real(fp_kind) dlon,pih
    real(double),allocatable,dimension(:) :: dlats 

    if (lgauss) then
       if (jcap==21) then
          nGlat=32; nGlon=64
       elseif (jcap==42) then
          nGlat=64; nGlon=128
       elseif (jcap==62) then
          nGlat=94; nGlon=192
       elseif (jcap==85) then
          nGlat=128; nGlon=256
       elseif (jcap==254) then
          nGlat=360; nGlon=576
       elseif(jcap==268) then
         nGlat=360; nGlon=540
       elseif(jcap==382) then
         nGlat=384; nGlon=768
       elseif (jcap==574) then
          nGlat=578; nGlon=1152
       elseif (jcap==878) then
          nGlat=880; nGlon=1760
       else 
          nGlat = nlat; nGlon = nlon
       endif 
    else
       nGlat = nlat; nGlon = nlon
    endif
    if (mype==0) then
       write(6,*) 'create_grids:  jcap         ',  jcap
       write(6,*) 'create_grids:  nlat,  nlon= ',  nlat,  nlon
       write(6,*) 'create_grids: nGlat, nGlon= ', nGlat, nGlon
    endif

    allocate(rlats(nlat),rlons(nlon))
    allocate(glats(nGlat),glons(nGlon))

! constant for deg/radians conversion
    deg2rad=acos(-1.0)/180.0

    pih=0.5*pi

    dlon=4*pih/float(nlon)
    do i=1,nlon
      rlons(i)=float(i-1)*dlon
    end do

    if( lgauss ) then
      glons(:)=rlons(:)
    else
      dlon=4*pih/float(nGlon)
      do i=1,nGlon
        glons(i)=float(i-1)*dlon
      end do
    endif 
      
    if(.not. lgauss ) then  ! lat-lon grid
      do i=1, nlat
        rlats(i)=-pih + (i-1)*pi / float(nlat-1)
      end do
      allocate(dlats(nGlat))
      call gauss_lat_nmc(dlats(2:nGlat-1),nGlat-2)
      glats(2:nGlat-1) = dlats(2:nGlat-1) * deg2rad
      glats(1)=-pih
      glats(nGlat)=pih
      deallocate(dlats)
    else
      allocate(dlats(nlat))
      call gauss_lat_nmc(dlats(2:nlat-1),nlat-2)
      rlats(2:nlat-1) = dlats(2:nlat-1) * deg2rad
      rlats(1)=-pih
      rlats(nlat)=pih
      deallocate(dlats)
      glats(:)=rlats(:)
    endif

    return
  end subroutine create_grids

  subroutine create_mapping
    implicit none
    integer(ip_kind) i

    allocate(jstart(npe),istart(npe),&
             ilat1(npe),jlon1(npe),&
             ijn_s(npe),irc_s(npe),ird_s(npe),displs_s(npe),&
             ijn(npe),isc_g(npe),isd_g(npe),displs_g(npe))

    do i=1,npe
      jstart(i)    = 0
      istart(i)    = 0
      ilat1(i)     = 0
      jlon1(i)     = 0
      ijn_s(i)     = 0
      irc_s(i)     = 0
      ird_s(i)     = 0
      displs_s(i)  = 0
      ijn(i)       = 0
      isc_g(i)     = 0
      isd_g(i)     = 0
      displs_g(i)  = 0
    end do

    return
  end subroutine create_mapping


  subroutine create_bias_g(nlat0,nlon0,nsig0)
    implicit none
    integer, intent(in) :: nlat0,nlon0,nsig0

    allocate( bbp (nlat0,nlon0)      )
    allocate( bbt (nlat0,nlon0,nsig0) )
    allocate( bbs (nlat0,nlon0,nsig0) )
    allocate( bbv (nlat0,nlon0,nsig0) )
    allocate( bbq (nlat0,nlon0,nsig0) )
    if (hydromet ) then
       allocate( bbqi (nlat0,nlon0,nsig0) )
       allocate( bbql (nlat0,nlon0,nsig0) )
       allocate( bbqr (nlat0,nlon0,nsig0) )
       allocate( bbqs (nlat0,nlon0,nsig0) )
    endif
    allocate( bboz (nlat0,nlon0,nsig0) )
    bbp=0.
    bbt=0.
    bbs=0.
    bbv=0.
    bboz=0.
    if (hydromet) then
       bbqi=0.
       bbql=0.
       bbqr=0.
       bbqs=0.
    endif

    return
  end subroutine create_bias_g


  subroutine create_vars(nlat0,nlon0,nsig0)

    implicit none
    integer,intent(in) :: nlat0, nlon0, nsig0
    real(fp_kind) onetest
    real(double) onedouble

! test for precision at which code was compiled
    onetest=1.; onedouble=1.
    if(digits(onetest).lt.digits(onedouble)) then
      db_prec=.false.
    else
      db_prec=.true.
    endif
    if(mype==0) write(6,*) 'INITVARS: DB_PREC = ',db_prec

    allocate(filename(dimbig)     )
    allocate(na      (dimbig)     )
    allocate(nb      (dimbig)     )

    allocate(sigi(nsig0+1)        )
    allocate(sigl(nsig0  )        )
    allocate(ak5 (nsig0+1)        )
    allocate(bk5 (nsig0+1)        )
    allocate(vsmth(nsig0, nsig0)  )
    allocate(vsmc(nsig0, nsig0)   )

    allocate(sfvar(nlat0,nsig0)   )
    allocate(vpvar(nlat0,nsig0)   )
    allocate(tvar (nlat0,nsig0)   )
    allocate(qvar (nlat0,nsig0)   )
    if (hydromet ) then
       allocate(qivar (nlat0,nsig0)   )
       allocate(qlvar (nlat0,nsig0)   )
       allocate(qrvar (nlat0,nsig0)   )
       allocate(qsvar (nlat0,nsig0)   )
    endif
    allocate(cvar (nlat0,nsig0)   )
    allocate(ozvar (nlat0,nsig0)  )
    allocate(psvar(nlat0      )   )

    allocate(nrhvar(nlat0,nsig0))

    allocate(sfhln(nlat0,nsig0)   )
    allocate(vphln(nlat0,nsig0)   )
    allocate(thln (nlat0,nsig0)   )
    allocate(qhln (nlat0,nsig0)   )
    if (hydromet ) then
        allocate(qihln (nlat0,nsig0)   )
        allocate(qlhln (nlat0,nsig0)   )
        allocate(qrhln (nlat0,nsig0)   )
        allocate(qshln (nlat0,nsig0)   )
    endif
    allocate(chln (nlat0,nsig0)   )
    allocate(ozhln (nlat0,nsig0)  )
    allocate(pshln(nlat0)         )

    allocate(sfvln(nlat0,nsig0)   )
    allocate(vpvln(nlat0,nsig0)   )
    allocate(tvln (nlat0,nsig0)   )
    allocate(qvln (nlat0,nsig0)   )
    if (hydromet) then
       allocate(qivln (nlat0,nsig0)   )
       allocate(qlvln (nlat0,nsig0)   )
       allocate(qrvln (nlat0,nsig0)   )
       allocate(qsvln (nlat0,nsig0)   )
    endif
    allocate(cvln (nlat0,nsig0)   )
    allocate(ozvln (nlat0,nsig0)  )

    allocate(updex(nlat0,nsig0) )
    allocate(bmdex(nlat0,nsig0) )
    allocate(tcon (nlat0,nsig0,nsig0) )
    allocate(vpcon(nlat0,nsig0      ) )
    allocate(pscon(nlat0,nsig0      ) )

    return

  end subroutine create_vars

  subroutine destroy_vars
    implicit none
 
    deallocate(filename,na,nb)
    deallocate(sigi,sigl,ak5,bk5)
    deallocate(vsmth,vsmc)
    deallocate(sfvar,vpvar,tvar,qvar,cvar,psvar,ozvar)
    deallocate(sfhln,vphln,thln,qhln,chln,pshln,ozhln)
    deallocate(sfvln,vpvln,tvln,qvln,cvln,ozvln)
    if(hydromet) then
      deallocate(qivar,qlvar,qrvar,qsvar)
      deallocate(qihln,qlhln,qrhln,qshln)
      deallocate(qivln,qlvln,qrvln,qsvln)
    endif
    deallocate(updex,bmdex)
    deallocate(tcon,vpcon,pscon)
    deallocate(nrhvar)

   return

  end subroutine destroy_vars

  subroutine destroy_mapping
    implicit none
    deallocate(ltosi,ltosj,ltosi_s,ltosj_s)
    deallocate(jstart,istart,ilat1,jlon1,&
               ijn_s,displs_s,&
               ijn,isc_g,isd_g,displs_g)

    return
  end subroutine destroy_mapping

  subroutine destroy_bias_g
    deallocate(bbt,bbs,bbv)
    deallocate(bboz,bbq)
    if(hydromet) then
      deallocate(bbqi,bbql,bbqr,bbqs)
    endif
    deallocate(bbp)
    return
  end subroutine destroy_bias_g

  subroutine destroy_grids
    deallocate(rlats,rlons)
    deallocate(glats,glons)
    return
  end subroutine destroy_grids

  subroutine setvars
  end subroutine setvars

end module variables
