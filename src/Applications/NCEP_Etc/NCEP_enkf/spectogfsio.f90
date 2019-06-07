program spectogfsio
! mpif90 -O3 -xHOST -warn all -implicitnone -traceback -openmp spectogfsio.f90
! constants.o specmod.o sigio_module.o gfsio_rst.o -L../../../../gfsenkf/lib
! -lsp_4s -lgfsio_4 -lw3_4
  USE SIGIO_MODULE
  USE GFSIO_MODULE
  USE GFSIO_RST
  use constants
  use specmod
  implicit none
  real, parameter :: fvirt = 461.50/287.05-1.0
  TYPE(SIGIO_HEAD)  :: SIGHEAD
  TYPE(SIGIO_DATA)  :: SIGDATA
  TYPE(GFSIO_GFILE) :: GFILEO             
  TYPE(GFSIO_HEAD)  :: GFSHEADO
  TYPE(GFSIO_HEADV) :: GFSHEADVO
  TYPE(GFSIO_DATA)  :: GFSDATAO
  INTEGER LEVSO, IMO, JMO, IRET, JREC, idim_gfsio, jdim_gfsio, k, j, i
  INTEGER NTRACO, IUNIT, NREC, nt, n, ii, thermodyn_id_i
  real kap,kapr,kap1
  CHARACTER(120) :: SIGFILE,GRBFILE
  real, dimension(:), allocatable :: ak,bk,tmp
  real, dimension(:,:), allocatable :: psi
  real, dimension(:,:,:), allocatable :: pressi,ui,vi,ti,wi

  call getarg(1,sigfile)
  call getarg(2,grbfile)
  call gfsio_init(iret)
  IF(IRET.NE.0) THEN
    print *,'failed to initialize gfsio'
    stop
  ENDIF
  iunit = 7
  call sigio_srohdc(iunit,trim(sigfile),sighead,sigdata,iret)
  IF(IRET.NE.0) THEN
    PRINT*, ' ERROR AT GFSIO_OPEN chgres.out.grd ',iret
    STOP
  ENDIF
  IMO = sighead%lonb
  JMO = sighead%latb
  LEVSO = sighead%levs
  NTRACO = sighead%ntrac
  allocate(ak(levso+1))
  allocate(bk(levso+1))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ALLOCATE DATA FOR GFSIO OUTPUT
  ALLOCATE(GFSDATAO%ZS(IMO,JMO))
  ALLOCATE(GFSDATAO%PS(IMO,JMO))
  ALLOCATE(GFSDATAO%P(IMO,JMO,LEVSO))
  ALLOCATE(GFSDATAO%DP(IMO,JMO,LEVSO))
  ALLOCATE(GFSDATAO%T(IMO,JMO,LEVSO))
  ALLOCATE(GFSDATAO%U(IMO,JMO,LEVSO))
  ALLOCATE(GFSDATAO%V(IMO,JMO,LEVSO))
  ALLOCATE(GFSDATAO%Q(IMO,JMO,LEVSO,NTRACO))
  ALLOCATE(GFSDATAO%W(IMO,JMO,LEVSO))
  call init_constants(.false.) ! initialize constants.
  call init_constants_derived()
  kap = rd/cp
  kapr = cp/rd
  kap1 = kap+one
  !==> get U,V,temp,q,ps on gaussian grid.
  print *,'lonb,latb,jcap=',imo,jmo,sighead%jcap
  call init_spec_vars(imo, jmo, sighead%jcap, 4)
!$omp parallel do private(k,nt) shared(sigdata,gfsdatao,levso,ntraco)
  do k=1,levso
     call sptezv_s(sigdata%d(:,k),sigdata%z(:,k),gfsdatao%u(:,:,k),gfsdatao%v(:,:,k),1)
     call sptez_s(sigdata%t(:,k),gfsdatao%t(:,:,k),1)
     do nt=1,ntraco
        call sptez_s(sigdata%q(:,k,nt),gfsdatao%q(:,:,k,nt),1)
     enddo
  enddo
!$omp end parallel do
  ! surface pressure is last grid.
  call sptez_s(sigdata%ps,gfsdatao%ps(:,:),1)
  call sptez_s(sigdata%hs,gfsdatao%zs(:,:),1)
  !==> input psg is ln(ps) in centibars - convert to ps in pascals.
  gfsdatao%ps = 1000.*exp(gfsdatao%ps)
! compute pressure vertical velocity.
  allocate(psi(imo,2))
  allocate(ui(imo,2,levso))
  allocate(vi(imo,2,levso))
  allocate(ti(imo,2,levso))
  allocate(wi(imo,2,levso))
  DO J=1,(JMO+1)/2
     psi(:,1) = gfsdatao%ps(:,j)
     psi(:,2) = gfsdatao%ps(:,jmo-j+1)
     ui(:,1,:) = gfsdatao%u(:,j,:)
     ui(:,2,:) = gfsdatao%u(:,jmo-j+1,:)
     vi(:,1,:) = gfsdatao%v(:,j,:)
     vi(:,2,:) = gfsdatao%v(:,jmo-j+1,:)
     ti(:,1,:) = gfsdatao%t(:,j,:)
     ti(:,2,:) = gfsdatao%t(:,jmo-j+1,:)
     ! process two lats at a time.
     call getomega(SIGHEAD%JCAP,SIZE(SIGDATA%T,1),SIGHEAD%LEVS,&
      SIGHEAD%IDVC,SIGHEAD%IDVM,4,SIGHEAD%IDSL,&
      SIGHEAD%NVCOORD,SIGHEAD%VCOORD,&
      IMO,JMO,2*IMO,2*IMO,J,J,1,SIGDATA%D,SIGDATA%PS,&
      PSI,TI,UI,VI,WI)
     gfsdatao%w(:,j,:) = wi(:,1,:)
     gfsdatao%w(:,jmo-j+1,:) = wi(:,2,:)
  enddo
! convert virt temp to temp.
  thermodyn_id_i=mod(SIGHEAD%IDVM/10,10)
  select case( thermodyn_id_i )
     case(0,1)
     gfsdatao%t=gfsdatao%t/(1.0+fvirt*gfsdatao%q(:,:,:,1))
     print *,'virt temp thermodyn var'
     case(2)
     print *,'dry temp thermodyn var'
     case default
     print *,'unknown input thermodyn id =',thermodyn_id_i
  end select
  deallocate(psi,ui,vi,ti,wi)
  call sigio_axdata(sigdata,iret)
  if (sighead%idvc .eq. 0) then ! sigma coordinate, old file format.
      ak = zero
      bk = sighead%si(1:levso+1)
  else if (sighead%idvc == 1) then ! sigma coordinate
      ak = zero
      bk = sighead%vcoord(1:levso+1,2)
  else if (sighead%idvc == 2 .or. sighead%idvc == 3) then ! hybrid coordinate
      bk = sighead%vcoord(1:levso+1,2) 
      ak = 0.01*sighead%vcoord(1:levso+1,1) 
  else
      print *,'unknown vertical coordinate type',sighead%idvc
      stop
  end if
  !==> pressure at interfaces.
  allocate(pressi(imo,jmo,levso+1))
  do k=1,levso+1
     pressi(:,:,k)=100.*(ak(k)+bk(k)*0.01*gfsdatao%ps)
  enddo
  do k=1,levso
     gfsdatao%dp(:,:,k) = pressi(:,:,k)-pressi(:,:,k+1)
  enddo
  do k=1,levso
     ! layer pressure from phillips vertical interolation
     gfsdatao%p(:,:,k) = ((pressi(:,:,k)**kap1-pressi(:,:,k+1)**kap1)/&
                         (kap1*(pressi(:,:,k)-pressi(:,:,k+1))))**kapr
  end do
  deallocate(pressi,ak,bk)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  WRITE NEW GFSIO FILE
!         print*, ' prepare gfsio header '  

       ! --- fill in GFSIO header data  ---
  NREC=2+SIGHEAD%LEVS*(6+NTRACO)     !zs,ps,p,dp,t,u,v,q(ntracer)
  LEVSO=SIGHEAD%LEVS
  ALLOCATE(GFSHEADVO%VCOORD(LEVSO+1,SIGHEAD%NVCOORD))
  ALLOCATE(GFSHEADVO%RECNAME(NREC))
  ALLOCATE(GFSHEADVO%RECLEVTYP(NREC))
  ALLOCATE(GFSHEADVO%RECLEV(NREC))
  ALLOCATE(GFSHEADVO%GLAT1D(JMO))
  ALLOCATE(GFSHEADVO%GLON1D(IMO))
  ALLOCATE(GFSHEADVO%CPI(NTRACO+1))
  ALLOCATE(GFSHEADVO%RI(NTRACO+1))

  GFSHEADVO%VCOORD   =SIGHEAD%VCOORD
  GFSHEADVO%RECNAME(1)='hgt'
  GFSHEADVO%RECNAME(2)='pres'
  GFSHEADVO%RECNAME(3:(2+LEVSO))='pres'
  GFSHEADVO%RECNAME((3+LEVSO):(2+2*LEVSO))='dpres'
  GFSHEADVO%RECNAME((3+2*LEVSO):(2+3*LEVSO))='tmp'
  GFSHEADVO%RECNAME((3+3*LEVSO):(2+4*LEVSO))='ugrd'
  GFSHEADVO%RECNAME((3+4*LEVSO):(2+5*LEVSO))='vgrd'
  GFSHEADVO%RECNAME((3+5*LEVSO):(2+6*LEVSO))='spfh'
  GFSHEADVO%RECNAME((3+6*LEVSO):(2+7*LEVSO))='o3mr'
  GFSHEADVO%RECNAME((3+7*LEVSO):(2+8*LEVSO))='clwmr'
  GFSHEADVO%RECNAME((3+8*LEVSO):(2+9*LEVSO))='vvel'
  GFSHEADVO%RECLEVTYP(1:2)='sfc'
  GFSHEADVO%RECLEVTYP(3:NREC)='layer'
  GFSHEADVO%RECLEV(1:2)=1      
  DO K=1,LEVSO
    GFSHEADVO%RECLEV(2+K)=K
    GFSHEADVO%RECLEV(2+LEVSO+K)=K
    GFSHEADVO%RECLEV(2+2*LEVSO+K)=K
    GFSHEADVO%RECLEV(2+3*LEVSO+K)=K
    GFSHEADVO%RECLEV(2+4*LEVSO+K)=K
    GFSHEADVO%RECLEV(2+5*LEVSO+K)=K
    GFSHEADVO%RECLEV(2+6*LEVSO+K)=K
    GFSHEADVO%RECLEV(2+7*LEVSO+K)=K
    GFSHEADVO%RECLEV(2+8*LEVSO+K)=K
  ENDDO
  if (mod(sighead%idvm/10,10) == 3) then
    GFSHEADVO%CPI=SIGHEAD%CPI
    GFSHEADVO%RI=SIGHEAD%RI
  else
    GFSHEADVO%CPI=0.
    GFSHEADVO%RI=0.
  endif
  
  GFSHEADO%VERSION  =SIGHEAD%IVS
  GFSHEADO%FHOUR    =SIGHEAD%FHOUR
  GFSHEADO%IDATE    =SIGHEAD%IDATE   
  GFSHEADO%NREC     =NREC     
  GFSHEADO%LATB     =SIGHEAD%LATB
  GFSHEADO%LONB     =SIGHEAD%LONB
  GFSHEADO%LEVS     =SIGHEAD%LEVS
  GFSHEADO%JCAP     =SIGHEAD%JCAP
  GFSHEADO%ITRUN    =SIGHEAD%ITRUN
  GFSHEADO%IORDER   =SIGHEAD%IORDER
  GFSHEADO%IREALF   =SIGHEAD%IREALF
  GFSHEADO%IGEN     =SIGHEAD%IGEN  
  GFSHEADO%LATF     =SIGHEAD%LATF  
  GFSHEADO%LONF     =SIGHEAD%LONF  
  GFSHEADO%LATR     =SIGHEAD%LATR  
  GFSHEADO%LONR     =SIGHEAD%LONR  
  GFSHEADO%NTRAC    =SIGHEAD%NTRAC 
  GFSHEADO%ICEN2    =SIGHEAD%ICEN2
  GFSHEADO%IENS     =SIGHEAD%IENS   
  GFSHEADO%IDPP     =SIGHEAD%IDPP
  GFSHEADO%IDSL     =SIGHEAD%IDSL
  GFSHEADO%IDVC     =SIGHEAD%IDVC
  GFSHEADO%IDVM     =SIGHEAD%IDVM
  GFSHEADO%IDVT     =SIGHEAD%IDVT
  GFSHEADO%IDRUN    =SIGHEAD%IDRUN
  GFSHEADO%IDUSR    =SIGHEAD%IDUSR
  GFSHEADO%PDRYINI  =SIGHEAD%PDRYINI
  GFSHEADO%NCLDT    =SIGHEAD%NCLDT  
  GFSHEADO%IXGR     =SIGHEAD%IXGR   
  GFSHEADO%NVCOORD  =SIGHEAD%NVCOORD
  GFSHEADO%IDRT     =4               
  ! --- end GFSIO header data  ---
          
!         print*, ' write out gfsio chgres.out.grd'
  CALL GFSIO_OPEN(GFILEO,TRIM(grbfile),'write',&
       VERSION=GFSHEADO%VERSION,&  
       FHOUR=GFSHEADO%FHOUR,&
       IDATE=GFSHEADO%IDATE,&
       NREC=GFSHEADO%NREC,&     
       LATB=GFSHEADO%LATB,&    
       LONB=GFSHEADO%LONB,&   
       LEVS=GFSHEADO%LEVS,&  
       JCAP=GFSHEADO%JCAP,& 
       ITRUN=GFSHEADO%ITRUN,&
       IORDER=GFSHEADO%IORDER,&
       IREALF=GFSHEADO%IREALF,&
       IGEN=GFSHEADO%IGEN,&
       LATF=GFSHEADO%LATF,& 
       LONF=GFSHEADO%LONF,&     
       LATR=GFSHEADO%LATR          ,&
       LONR=GFSHEADO%LONR   ,&
       NTRAC=GFSHEADO%NTRAC    ,&
       ICEN2=GFSHEADO%ICEN2 ,& 
       IENS=GFSHEADO%IENS,&
       IDPP=GFSHEADO%IDPP ,&    
       IDSL=GFSHEADO%IDSL  ,&  
       IDVC=GFSHEADO%IDVC  ,& 
       IDVM=GFSHEADO%IDVM ,& 
       IDVT=GFSHEADO%IDVT,& 
       IDRUN=GFSHEADO%IDRUN    ,&
       IDUSR=GFSHEADO%IDUSR ,& 
       PDRYINI=GFSHEADO%PDRYINI ,&
       NCLDT=GFSHEADO%NCLDT ,& 
       IXGR=GFSHEADO%IXGR  ,& 
       NVCOORD=GFSHEADO%NVCOORD ,&
       IDRT=GFSHEADO%IDRT,& 
       RECNAME=GFSHEADVO%RECNAME,&
       RECLEVTYP=GFSHEADVO%RECLEVTYP,&
       RECLEV=GFSHEADVO%RECLEV ,& 
       VCOORD=GFSHEADVO%VCOORD,& 
       CPI=GFSHEADVO%CPI,& 
       RI=GFSHEADVO%RI,& 
       IRET=IRET)
  IF(IRET.NE.0) THEN
    PRINT*, ' ERROR AT GFSIO_OPEN chgres.out.grd ',iret
    STOP
  ENDIF

  JREC=1

  idim_gfsio=size(GFSDATAO%ZS,1)
  jdim_gfsio=size(GFSDATAO%ZS,2)
  allocate(tmp(idim_gfsio*jdim_gfsio))
  do j=1,jdim_gfsio
    ii = (j-1)*idim_gfsio
    do i=1,idim_gfsio
      tmp(i+ii) = GFSDATAO%ZS(i,j)
    enddo
  enddo
  CALL GFSIO_WRITERECW34(GFILEO,JREC, &
                            tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
  IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITERECW34 ZS '
  JREC=JREC+1
  do j=1,jdim_gfsio
    ii = (j-1)*idim_gfsio
    do i=1,idim_gfsio
      tmp(i+ii) = GFSDATAO%PS(i,j)
    enddo
  enddo
  CALL GFSIO_WRITERECW34(GFILEO,JREC,&
                      tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
  IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITERECW34 PS '
  DO K=1,LEVSO
    JREC=JREC+1

    do j=1,jdim_gfsio
      ii = (j-1)*idim_gfsio
      do i=1,idim_gfsio
        tmp(i+ii) = GFSDATAO%P(i,j,k)
      enddo
    enddo
    CALL GFSIO_WRITERECW34(GFILEO,JREC,&
                        tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
    IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITERECW34 P '
  ENDDO
  DO K=1,LEVSO
  JREC=JREC+1
    do j=1,jdim_gfsio
      ii = (j-1)*idim_gfsio
      do i=1,idim_gfsio
        tmp(i+ii) = GFSDATAO%DP(i,j,k)
      enddo
    enddo
    CALL GFSIO_WRITERECW34(GFILEO,JREC,&
                        tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
    IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITERECW34 DP '
  ENDDO
  DO K=1,LEVSO
    JREC=JREC+1
    do j=1,jdim_gfsio
      ii = (j-1)*idim_gfsio
      do i=1,idim_gfsio
        tmp(i+ii) = GFSDATAO%T(i,j,k)
      enddo
    enddo
    CALL GFSIO_WRITERECW34(GFILEO,JREC,&
                        tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
    IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITERECW34 T '
  ENDDO
  DO K=1,LEVSO
    JREC=JREC+1
    do j=1,jdim_gfsio
      ii = (j-1)*idim_gfsio
      do i=1,idim_gfsio
        tmp(i+ii) = GFSDATAO%U(i,j,k)
      enddo
    enddo
    CALL GFSIO_WRITERECW34(GFILEO,JREC,&
                        tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
    IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITERECW34 U '
  ENDDO
  DO K=1,LEVSO
    JREC=JREC+1
    do j=1,jdim_gfsio
      ii = (j-1)*idim_gfsio
      do i=1,idim_gfsio
        tmp(i+ii) = GFSDATAO%V(i,j,k)
      enddo
    enddo
    CALL GFSIO_WRITERECW34(GFILEO,JREC,&
                        tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
    IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITERECW34 V '
  ENDDO
  DO N=1,NTRACO
    DO K=1,LEVSO
      JREC=JREC+1
      do j=1,jdim_gfsio
        ii = (j-1)*idim_gfsio
        do i=1,idim_gfsio
          tmp(i+ii) = GFSDATAO%Q(i,j,k,n)
        enddo
      enddo
      CALL GFSIO_WRITERECW34(GFILEO,JREC,&
                          tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
      IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITERECW34 Q '
    ENDDO
  ENDDO
  DO K=1,LEVSO
    JREC=JREC+1
    do j=1,jdim_gfsio
      ii = (j-1)*idim_gfsio
      do i=1,idim_gfsio
        tmp(i+ii) = GFSDATAO%W(i,j,k)
      enddo
    enddo
    CALL GFSIO_WRITERECW34(GFILEO,JREC,&
                        tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
    IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITERECW34 W '
  ENDDO

  print*,' JREC=', JREC, ' NREC=', NREC

  DEALLOCATE(GFSHEADVO%VCOORD)
  DEALLOCATE(GFSHEADVO%RECNAME)
  DEALLOCATE(GFSHEADVO%RECLEVTYP)
  DEALLOCATE(GFSHEADVO%RECLEV)
  DEALLOCATE(GFSHEADVO%GLAT1D)
  DEALLOCATE(GFSHEADVO%GLON1D)
  DEALLOCATE(GFSHEADVO%CPI)
  DEALLOCATE(GFSHEADVO%RI)

  DEALLOCATE(GFSDATAO%ZS)
  DEALLOCATE(GFSDATAO%PS)
  DEALLOCATE(GFSDATAO%P)
  DEALLOCATE(GFSDATAO%DP)
  DEALLOCATE(GFSDATAO%T)
  DEALLOCATE(GFSDATAO%U)
  DEALLOCATE(GFSDATAO%V)
  DEALLOCATE(GFSDATAO%Q)
  DEALLOCATE(GFSDATAO%W)
  DEALLOCATE(tmp)

end program spectogfsio

!----------------------------------------------------------------------------------
       subroutine getomega(jcap,nc,km,idvc,idvm,idrt,idsl,nvcoord,&
     &      vcoord,lonb,latb,ijl,ijn,j1,j2,jc,sd,sps,psi,ti,ui,vi,wi)
!
       use sigio_module, only : sigio_modpr
       implicit none
!
       integer,intent(in):: jcap,nc,km,idvc,idvm,idrt,idsl,nvcoord
       integer,intent(in):: lonb,latb,ijl,j1,j2,jc,ijn
       real,intent(in):: vcoord(km+1,nvcoord)
       real,intent(in):: sd(nc,km),sps(nc)
       real,intent(in):: psi(ijn),ti(ijn,km),ui(ijn,km),vi(ijn,km)
       real,intent(out):: wi(ijn,km)
       real :: pd(ijn,km),pi(ijn,km+1),pm(ijn,km)
       real :: os
       real dpmdps(ijn,km),dpddps(ijn,km),dpidps(ijn,km+1),vgradp,psmean
       real di(ijn,km),psx(ijn),psy(ijn)
       integer k,i,ij,lonb2,in,is,iret
!----1. spectral transform
      lonb2=lonb*2
      ij=lonb2*(j2-j1+1)
      in=1
      is=1+lonb
      call sptrand(0,jcap,idrt,lonb,latb,1,1,1,lonb2,lonb2,nc,ijn,&
           j1,j2,jc,sps,psmean,&
           psx(in),psx(is),psy(in),psy(is),1)
      SELECT CASE(MOD(IDVM,10))
      CASE(0,1)
          continue
      CASE(2)
!$OMP PARALLEL DO DEFAULT(SHARED) private(i)
          do i=1,ijn
           psx(i)=psx(i)/(psi(i)*1.0E-3)
           psy(i)=psy(i)/(psi(i)*1.0E-3)
          enddo
!$OMP END PARALLEL DO
      CASE DEFAULT
!$OMP PARALLEL DO DEFAULT(SHARED) private(i)
          do i=1,ijn
           psx(i)=psx(i)/psi(i)
           psy(i)=psy(i)/psi(i)
          enddo
!$OMP END PARALLEL DO
      END SELECT

!$OMP PARALLEL DO DEFAULT(SHARED) private(k)
      do K=1,km
        call sptran(0,jcap,idrt,lonb,latb,1,1,1,lonb2,lonb2,nc,ijn,&
           j1,j2,jc,sd(1,k),di(in,k),di(is,k),1)
      enddo
!$OMP END PARALLEL DO
       call sigio_modpr(ijl,ijn,km,nvcoord,idvc,idsl,vcoord,iret,&
                   ps=psi,t=ti,pm=pm,pd=pd,dpmdps=dpmdps,dpddps=dpddps)

!----3.omeda from modstuff
!$OMP PARALLEL DO DEFAULT(SHARED) private(i)
      do i=1,ijl
       pi(i,1)=psi(i)
       dpidps(i,1)=1.
       do k=1,km
         pi(i,k+1)=pi(i,k)-pd(i,k)
         dpidps(i,k+1)=dpidps(i,k)-dpddps(i,k)
       enddo
       os=0.
       do k=km,1,-1
        vgradp=ui(i,k)*psx(i)+vi(i,k)*psy(i)
        os=os-vgradp*psi(i)*(dpmdps(i,k)-dpidps(i,k+1))-&
           di(i,k)*(pm(i,k)-pi(i,k+1))
        wi(i,k)=vgradp*psi(i)*dpmdps(i,k)+os
        os=os-vgradp*psi(i)*(dpidps(i,k)-dpmdps(i,k))- &
           di(i,k)*(pi(i,k)-pm(i,k))
       enddo
!
      enddo
!$OMP END PARALLEL DO
       return
       end subroutine 
