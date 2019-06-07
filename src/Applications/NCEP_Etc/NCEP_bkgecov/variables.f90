module variables

  use type_kinds, only: fp_kind, double, single
  implicit none

! general
! integer mype
  integer maxcases
  integer filunit,filunit1,filunit2

! forecast pair file variables
  character(len=100),allocatable,dimension(:):: filename
  integer,allocatable,dimension(:):: na,nb

! from GSI gridmod:
  logical hybrid,db_prec,anaclw,lgaus,lsmver,biasrm,laddoz
  logical hydromet
  logical smooth_vert_variances
  integer nlat,nlon,nsig,dimbig,option
  integer nGlat,nGlon
  integer nlatfv,nlonfv
  real(fp_kind) smoothdeg,fnm0
  real(fp_kind) :: taj
  real(fp_kind),allocatable,dimension(:):: sigl,sigi,ak5,bk5
  real(fp_kind),allocatable,dimension(:):: rlats,rlons
  real(fp_kind),allocatable,dimension(:):: glats,glons
  real(fp_kind),allocatable,dimension(:):: latfv,lonfv
  real(fp_kind),allocatable,dimension(:,:):: vsmth,vsmc

! allocateable arrays
  real(fp_kind),allocatable,dimension(:):: sweight
  real(fp_kind),allocatable,dimension(:,:  ) :: bbp
  real(fp_kind),allocatable,dimension(:,:,:) :: bbt,bbs,bbv
  real(fp_kind),allocatable,dimension(:,:,:) :: bboz,bbq,bbqi,bbql,bbqr,bbqs
!  real(fp_kind),allocatable,dimension(:,:) :: bcorrt,bcorrd,bcorrv
!  real(fp_kind),allocatable,dimension(:,:) :: bbiast,bbiasd,bbiasv
!  real(fp_kind),allocatable,dimension(:  ) :: bcorrp,bbiasp

! smoothing coefficients
   integer,parameter :: nlmax=200
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

  real(fp_kind),allocatable,dimension(:,:,:) :: grdsf
  real(fp_kind),allocatable,dimension(:,:,:) :: grdvp
  real(fp_kind),allocatable,dimension(:,:,:) :: gridt
  real(fp_kind),allocatable,dimension(:,:,:) :: gridq
  real(fp_kind),allocatable,dimension(:,:,:) :: gridqi
  real(fp_kind),allocatable,dimension(:,:,:) :: gridql
  real(fp_kind),allocatable,dimension(:,:,:) :: gridqr
  real(fp_kind),allocatable,dimension(:,:,:) :: gridqs
  real(fp_kind),allocatable,dimension(:,:,:) :: grdrh
  real(fp_kind),allocatable,dimension(:,:,:) :: grdc
  real(fp_kind),allocatable,dimension(:,:,:) :: grdoz
  real(fp_kind),allocatable,dimension(:,:)   :: gridp

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
    nlatfv=361
    nlonfv=540
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
    hydromet=.false.
    smooth_vert_variances=.false. ! to correspond to Wei''s original code this
                                  ! needs to be set to true; false corresponds
                                  ! to what Amal used in 513 or so.


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

  subroutine create_grids(nlat,nlon,jcap,lgaus)
    implicit none

    logical,intent(in) :: lgaus
    integer,intent(in) :: nlat,nlon,jcap

!  local variables 
    integer i,ii,l,m,i1
    real(fp_kind) dlon,pih
    real(double),allocatable,dimension(:) :: dlats 


!    if(jcap==46)then
!      nGlat=90; nGlon=144
!    elseif(jcap==58)then
!      nGlat=90; nGlon=180
!    elseif(jcap==62)then
!      nGlat=96; nGlon=192
!    elseif(jcap==88)then  !
!      nGlat=180; nGlon=270
!    elseif(jcap==94)then  !
!      nGlat=180; nGlon=288
!    elseif(jcap==118)then  !
!      nGlat=180; nGlon=360
!    elseif(jcap==254)then
!      nGlat=258; nGlon=512
!    elseif(jcap==268)then  !
!      nGlat=360; nGlon=540
!    elseif(jcap==382)then
!      nGlat=386; nGlon=768
!    elseif(jcap==538)then
!      nGlat=542; nGlon=1080
!    else
!      print*,"stop: this jcap not considered yet"
!      stop
!    endif

!    if( lgaus .and. (nlat .ne. nGlat .or. nlon.ne.nGlon) )then
!      print*,"GAUSSIAN GRID: nGlat .ne. nlat"
!      print*,"GAUSSIAN GRID: nGlon .ne. nlon"
!      stop
!    endif

    if (jcap==254) then 
       nGlat=360; nGlon=576
    else 
       nGlat = nlat
       nGlon = nlon
    endif 

    allocate(rlats(nlat),rlons(nlon))
    allocate(glats(nGlat),glons(nGlon))
    allocate(latfv(nlatfv),lonfv(nlonfv))

! constant for deg/radians conversion
    deg2rad=acos(-1.0)/180.0

    pih=0.5*pi

    dlon=4*pih/float(nlon)
    do i=1,nlon
      rlons(i)=float(i-1)*dlon
    end do

    dlon=4*pih/float(nlonfv)

    if( lgaus ) then
      glons(:)=rlons(:)
    else
      dlon=4*pih/float(nGlon)
      do i=1,nGlon
        glons(i)=float(i-1)*dlon
      end do
    endif 
      
    if(.not. lgaus ) then  ! lat-lon grid
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
  
  subroutine create_bias_g(nlat0,nlon0,nsig0)
    implicit none
    integer, intent(in) :: nlat0,nlon0,nsig0

    allocate( bbp (nlat0,nlon0)       )
    allocate( bbt (nlat0,nlon0,nsig0) )
    allocate( bbs (nlat0,nlon0,nsig0) )
    allocate( bbv (nlat0,nlon0,nsig0) )
    allocate( bbq (nlat0,nlon0,nsig0) )
    if (hydromet) then
       allocate( bbqi (nlat0,nlon0,nsig0))
       allocate( bbql (nlat0,nlon0,nsig0))
       allocate( bbqr (nlat0,nlon0,nsig0))
       allocate( bbqs (nlat0,nlon0,nsig0))
    endif
    allocate( bboz (nlat0,nlon0,nsig0))
    return
  end subroutine create_bias_g

  subroutine create_intvar(nlat0,nlon0,nsig0)
    implicit none
    integer, intent(in) :: nlat0,nlon0,nsig0
    integer k

    allocate( grdsf(nlat0,nlon0,nsig0) )
    allocate( grdvp(nlat0,nlon0,nsig0) )
    allocate( gridt(nlat0,nlon0,nsig0) )
    allocate( gridq(nlat0,nlon0,nsig0) )
    if (hydromet) then
       allocate( gridqi(nlat0,nlon0,nsig0))
       allocate( gridql(nlat0,nlon0,nsig0))
       allocate( gridqr(nlat0,nlon0,nsig0))
       allocate( gridqs(nlat0,nlon0,nsig0))
    endif
    allocate( grdrh(nlat0,nlon0,nsig0) )
    allocate( grdc (nlat0,nlon0,nsig0) )
    allocate( grdoz(nlat0,nlon0,nsig0) )
    allocate( gridp(nlat0,nlon0) )

    return
  end subroutine create_intvar

  subroutine create_vars(nlat0,nlon0,nsig0,mype)

    implicit none
    integer,intent(in) :: nlat0, nlon0, nsig0
    integer,intent(in) :: mype
    real(fp_kind) onetest
    real(double) onedouble

! test for precision at which code was compiled
    onetest=1.; onedouble=1.
    if(digits(onetest).lt.digits(onedouble)) then
      db_prec=.false.
    else
      db_prec=.true.
    endif
    if (mype==0) then
        write(6,*) 'INITVARS: DB_PREC = ',db_prec
    endif

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
    if (hydromet) then
       allocate(qivar (nlat0,nsig0)   )
       allocate(qlvar (nlat0,nsig0)   )
       allocate(qrvar (nlat0,nsig0)   )
       allocate(qsvar (nlat0,nsig0)   )
    endif
    allocate(cvar (nlat0,nsig0)   )
    allocate(ozvar (nlat0,nsig0)   )
    allocate(psvar(nlat0      )   )

    allocate(nrhvar(nlat0,nsig0))

    allocate(sfhln(nlat0,nsig0)   )
    allocate(vphln(nlat0,nsig0)   )
    allocate(thln (nlat0,nsig0)   )
    allocate(qhln (nlat0,nsig0)   )
    if (hydromet) then
       allocate(qihln (nlat0,nsig0)   )
       allocate(qlhln (nlat0,nsig0)   )
       allocate(qrhln (nlat0,nsig0)   )
       allocate(qshln (nlat0,nsig0)   )
    endif
    allocate(chln  (nlat0,nsig0)   )
    allocate(ozhln (nlat0,nsig0)   )
    allocate(pshln(nlat0)         )

    allocate(sfvln(nlat0,nsig0)   )
    allocate(vpvln(nlat0,nsig0)   )
    allocate(tvln (nlat0,nsig0)   )
    allocate(qvln (nlat0,nsig0)   )
    if (hydromet) then
       allocate(qivln(nlat0,nsig0)   )
       allocate(qlvln(nlat0,nsig0)   )
       allocate(qrvln(nlat0,nsig0)   )
       allocate(qsvln(nlat0,nsig0)   )
    endif
    allocate(cvln (nlat0,nsig0)   )
    allocate(ozvln(nlat0,nsig0)   )

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
    if (hydromet) then
       deallocate(qivar,qlvar,qrvar,qsvar)
       deallocate(qihln,qlhln,qrhln,qshln)
       deallocate(qivln,qlvln,qrvln,qsvln)
    endif
    deallocate(updex,bmdex)
    deallocate(tcon,vpcon,pscon)
    deallocate(nrhvar)

   return

  end subroutine destroy_vars

  subroutine destroy_bias_g
    deallocate(bbt,bbs,bbv)
    deallocate(bboz,bbq)
    if (hydromet) deallocate(bbqi,bbql,bbqr,bbqs)
    deallocate(bbp)
    return
  end subroutine destroy_bias_g

  subroutine destroy_intvar
    deallocate( grdsf )
    deallocate( grdvp )
    deallocate( gridt )
    deallocate( gridq )
    if (hydromet) then
       deallocate( gridqi)
       deallocate( gridql)
       deallocate( gridqr)
       deallocate( gridqs)
    endif
    deallocate( grdrh )
    deallocate( grdc  )
    deallocate( grdoz )
    deallocate( gridp )
    return
  end subroutine destroy_intvar


  subroutine destroy_grids
    deallocate(rlats,rlons)
    deallocate(glats,glons)
    deallocate(latfv,lonfv)
    return
  end subroutine destroy_grids

end module variables
