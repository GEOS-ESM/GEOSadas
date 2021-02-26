subroutine variances(mycases,numcases,npes,mype)
  use type_kinds, only: fp_kind,single
  use variables,  only: nlat,nlon,nsig
  use variables,  only: sfvar,vpvar,tvar,qvar,nrhvar,cvar,psvar,ozvar
  use variables,  only: qivar,qlvar,qrvar,qsvar
  use variables,  only: lbal,tbal, vpbal,pbal
  use variables,  only: db_prec,anaclw
  use variables,  only: tcon,vpcon,pscon
  use variables,only: lat1,lon1
  use variables, only: filunit,biasrm,bbt,bbs,bbv,bbp
  use variables,only: ijn,displs_g,iglobal,ltosi,ltosj
  use variables,  only: smoothdeg
  use variables, only: one
  use delmod,    only: delvars, hydrobias,delbal  
  use postmod,    only: smoothlat
#ifndef ibm_sp
  use m_mpif
#endif
  implicit none
#ifdef ibm_sp
  include 'mpif.h'
#endif

  character(255) grdfile
  integer ncfggg,iret
  integer i,j,k,m,nn,kk,k2,ibin,ll,ierror,mycases,numcases,mpi_rtype,mype
  integer :: mype_work,mm1,ni1,ni2
  integer npes
  real(fp_kind) norm
  real(fp_kind),dimension(25,nsig):: qcount,qamp
  real(fp_kind),dimension(25):: qcavg
  real(fp_kind),allocatable,dimension(:,:)  :: workgrd
  real(fp_kind),allocatable,dimension(:)  :: work1
  real(fp_kind),allocatable,dimension(:,:):: vptot,ttot
  real(fp_kind),allocatable,dimension(:):: ptot
  real(fp_kind),dimension(lat1,lon1,nsig) :: grdsf,grdvp,gridt,gridq,grdrh,grdoz,grdc
  real(fp_kind),dimension(lat1,lon1,nsig) :: gridqi,gridql,gridqr,gridqs
  real(fp_kind),dimension(lat1,lon1):: gridp
  real(fp_kind),dimension(lat1,lon1,nsig) :: sf,vp,t,q,rh,oz,cw
  real(fp_kind),dimension(lat1,lon1,nsig) :: qi,ql,qr,qs
  real(fp_kind),dimension(lat1,lon1):: ps
  real(fp_kind),dimension(lat1,lon1,nsig) :: tb,vpb,gvpb,gtb,twrk,vpwrk,ghold
  real(fp_kind),dimension(lat1,lon1):: pb,gpb,pwrk
  real(single), dimension(nlat,nsig,3):: tbal4,vpbal4
  real(single), dimension(nlat     ,3):: pbal4
  real(fp_kind) :: r_norm,qdiff

  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

  if (mype==0) write(6,*) 'IN ROUTINE TO CALCULATE VARIANCES'

  mype_work=max(0,npes/2-1)
  mm1=mype+1

  allocate(workgrd(nlat,nlon))
  allocate(work1(iglobal))

  qvar=0.
  qivar=0.
  qlvar=0.
  qrvar=0.
  qsvar=0.
  cvar=0.
  ozvar=0. 
  nrhvar=0.
  sfvar=0.
  vpvar=0.
  tvar=0.
  psvar=0.
  ibin=25
  qcount=0.
  qamp=0.
  oz=0. ; sf=0. ; vp=0. ; t=0. ; q=0. ; cw=0.; ps=0.
  qi=0.;ql=0.;qr=0.;qs=0.

  r_norm=1./float(numcases)

  if(lbal) then 
     allocate(tbal(nlat,nsig))
     allocate(vpbal(nlat,nsig))
     allocate(pbal(nlat))
     allocate(ttot(nlat,nsig))
     allocate(vptot(nlat,nsig))
     allocate(ptot(nlat))
     tbal=0.; vpbal=0.; pbal=0.
     vpb=0. ;  tb=0.  ;pb=0.
     vptot=0.; ttot=0.;ptot=0.
     vpwrk=0.;twrk=0.;pwrk=0.
  end if 

  filunit = 1000*(mype+1)+1
  open(filunit,form='unformatted')
  rewind(filunit)

!  do nn=1,mycases
  do nn=1,numcases

    read(filunit) grdsf,grdvp,gridt,gridp,gridq,grdrh,gridqi,gridql,gridqr,gridqs,grdoz,grdc
    call delvars  (grdsf,grdvp,gridt,gridp,gridq,grdoz,grdc,mype)
    call hydrobias(gridqi,gridql,gridqr,gridqs)

    if(lbal) then  
       gvpb=0.; gtb=0. ;gpb=0.
       call delbal(grdsf,gvpb,gtb,gpb,mype)
       do k=1,nsig
         do j=1,lon1
           do i=1,lat1
              vpb(i,j,k) = vpb(i,j,k) + (gvpb(i,j,k)*gvpb(i,j,k))*r_norm
               tb(i,j,k) =  tb(i,j,k) + (gtb(i,j,k)*gtb(i,j,k))*r_norm
              ghold(i,j,k) = gvpb(i,j,k)  + grdvp(i,j,k)
              vpwrk(i,j,k) = vpwrk(i,j,k) + (ghold(i,j,k)*ghold(i,j,k))*r_norm
              ghold(i,j,k) = gtb(i,j,k) + gridt(i,j,k)
              twrk(i,j,k) =  twrk(i,j,k) + (ghold(i,j,k)*ghold(i,j,k))*r_norm
           end do 
         end do 
       end do 
       do j=1,lon1
         do i=1,lat1
           pb(i,j) = pb(i,j) + (gpb(i,j)*gpb(i,j))*r_norm
           ghold(i,j,1) = gpb(i,j) + gridp(i,j)
           pwrk(i,j) = pwrk(i,j) + (ghold(i,j,1)*ghold(i,j,1))*r_norm
         end do 
       end do 
    end if 
    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          sf(i,j,k) = sf(i,j,k) + ( grdsf(i,j,k)* grdsf(i,j,k))*r_norm
          vp(i,j,k) = vp(i,j,k) + ( grdvp(i,j,k)* grdvp(i,j,k))*r_norm
           t(i,j,k) =  t(i,j,k) + ( gridt(i,j,k)* gridt(i,j,k))*r_norm
           q(i,j,k) =  q(i,j,k) + ( gridq(i,j,k)* gridq(i,j,k))*r_norm
          qi(i,j,k) = qi(i,j,k) + (gridqi(i,j,k)*gridqi(i,j,k))*r_norm
          ql(i,j,k) = ql(i,j,k) + (gridql(i,j,k)*gridql(i,j,k))*r_norm
          qr(i,j,k) = qr(i,j,k) + (gridqr(i,j,k)*gridqr(i,j,k))*r_norm
          qs(i,j,k) = qs(i,j,k) + (gridqs(i,j,k)*gridqs(i,j,k))*r_norm
          oz(i,j,k) = oz(i,j,k) + ( grdoz(i,j,k)* grdoz(i,j,k))*r_norm
          cw(i,j,k) = cw(i,j,k) + (  grdc(i,j,k)*  grdc(i,j,k))*r_norm
        end do
      end do
    end do
    if(.not. anaclw) cw = 0.
    do j=1,lon1
      do i=1,lat1
        ps(i,j) = ps(i,j) + (gridp(i,j)*gridp(i,j))*r_norm
      end do
    end do
! Normalized RH calculations
! this is what GSI will do: varq(i,k)=min(max(corq2x,0.00015_r_kind),one)
    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          ll=grdrh(i,j,k)*20.0+1
          ll=min0(max0(1,ll),25)
          qdiff=min(one,abs(gridq(i,j,k)))**2.
          qamp(ll,k)=qamp(ll,k)+qdiff
          qcount(ll,k)=qcount(ll,k)+1
        end do
      end do
    end do

  end do ! enddo over cases
  close(filunit)

  if (lbal) then
    do k=1,nsig
      call mpi_gatherv(vpb(1,1,k),ijn(mm1),mpi_rtype,&
                       work1,ijn,displs_g,mpi_rtype,&
                       mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            vpbal(i,k) = vpbal(i,k) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do    
    do k=1,nsig
      call mpi_gatherv(vpwrk(1,1,k),ijn(mm1),mpi_rtype,&
                       work1,ijn,displs_g,mpi_rtype,&
                       mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            vptot(i,k) = vptot(i,k) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do    

    do k=1,nsig
      call mpi_gatherv(tb(1,1,k),ijn(mm1),mpi_rtype,&
                       work1,ijn,displs_g,mpi_rtype,&
                       mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            tbal(i,k) = tbal(i,k) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do    
    do k=1,nsig
      call mpi_gatherv(twrk(1,1,k),ijn(mm1),mpi_rtype,&
                       work1,ijn,displs_g,mpi_rtype,&
                       mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            ttot(i,k) = ttot(i,k) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do    

    call mpi_gatherv(pb,ijn(mm1),mpi_rtype,&
                     work1,ijn,displs_g,mpi_rtype,&
                     mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          pbal(i) = pbal(i) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
    call mpi_gatherv(pwrk,ijn(mm1),mpi_rtype,&
                     work1,ijn,displs_g,mpi_rtype,&
                     mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          ptot(i) = ptot(i) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  endif   ! End if lbal



  do k=1,nsig
    call mpi_gatherv(sf(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          sfvar(i,k) = sfvar(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(vp(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          vpvar(i,k) = vpvar(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(t(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          tvar(i,k) = tvar(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(q(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          qvar(i,k) = qvar(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(qi(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          qivar(i,k) = qivar(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(ql(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          qlvar(i,k) = qlvar(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(qr(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          qrvar(i,k) = qrvar(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do


  do k=1,nsig
    call mpi_gatherv(qs(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          qsvar(i,k) = qsvar(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(oz(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          ozvar(i,k) = ozvar(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(cw(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          cvar(i,k) = cvar(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do 
    end if 
  end do 

  call mpi_gatherv(ps,ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
  if (mype==mype_work) then
    do kk=1,iglobal
      ni1=ltosi(kk); ni2=ltosj(kk)
      workgrd(ni1,ni2)=work1(kk)
    end do
    do i=1,nlat
      do j=1,nlon
        psvar(i) = psvar(i) + workgrd(i,j)/float(nlon)
      end do
    end do
  end if

  call mpi_barrier(mpi_comm_world,ierror)

  call mpi_allreduce((qamp),qamp,ibin*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((qcount),qcount,ibin*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)

  if (mype==mype_work) then
    qcavg=0.
    do k=1,nsig
      do ll=1,ibin
        if(qcount(ll,k).ne.0) then
          qamp(ll,k)=qamp(ll,k)/qcount(ll,k)
          nrhvar(ll,k)=qamp(ll,k)
          qcavg(ll)=qcavg(ll)+qcount(ll,k)/float(nsig)
        end if
      end do
    end do
  end if

  if (mype==mype_work) then
    call smoothlat(sfvar,nsig,smoothdeg)
    call smoothlat(vpvar,nsig,smoothdeg)
    call smoothlat(tvar,nsig,smoothdeg)
    call smoothlat(qvar,nsig,smoothdeg)
    call smoothlat(qivar,nsig,smoothdeg)
    call smoothlat(qlvar,nsig,smoothdeg)
    call smoothlat(qrvar,nsig,smoothdeg)
    call smoothlat(qsvar,nsig,smoothdeg)
    call smoothlat(ozvar,nsig,smoothdeg)
    call smoothlat(cvar,nsig,smoothdeg)
    call smoothlat(psvar,1,smoothdeg)
  end if

  call mpi_barrier(mpi_comm_world,ierror)

  call mpi_bcast(sfvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(vpvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(tvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(qvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(qivar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(qlvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(qrvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(qsvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(nrhvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(ozvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(cvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(psvar,nlat,mpi_rtype,mype_work,mpi_comm_world,ierror)

  if(lbal) then 
    call mpi_bcast(vpbal,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
    call mpi_bcast(tbal,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
    call mpi_bcast(pbal,nlat,mpi_rtype,mype_work,mpi_comm_world,ierror)
    call mpi_bcast(vptot,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
    call mpi_bcast(ttot,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
    call mpi_bcast(ptot,nlat,mpi_rtype,mype_work,mpi_comm_world,ierror)
  end if
  call mpi_barrier(mpi_comm_world,ierror)


  if (mype==0 .and. lbal) then
   grdfile='balvar_sp.grd'
   ncfggg=len_trim(grdfile)
   call baopenwt(22,grdfile(1:ncfggg),iret)
!   call wryte(22,4*nlat*nsig*3,expvart)
!   call wryte(22,4*nlat*nsig*3,expvarvp)
!   call wryte(22,4*nlat*3,expvarsp)
   do k=1,nsig
    do i=1,nlat
      tbal4(i,k,1)=ttot(i,k)
      tbal4(i,k,2)=tbal(i,k)
      tbal4(i,k,3)=tvar(i,k)
      vpbal4(i,k,1)=vptot(i,k)
      vpbal4(i,k,2)=vpbal(i,k)
      vpbal4(i,k,3)=vpvar(i,k)
    end do
   end do 
   do i=1,nlat
     pbal4(i,1) = ptot(i)
     pbal4(i,2) = pbal(i)
     pbal4(i,3) = psvar(i)
   end do 
   call wryte(22,4*nlat*nsig*3,tbal4)
   call wryte(22,4*nlat*nsig*3,vpbal4)
   call wryte(22,4*nlat*3,pbal4)
   call baclose(22,iret)
  end if

  if(lbal) deallocate(tbal,vpbal,pbal)
  if(lbal) deallocate(ttot,vptot,ptot)
  deallocate(workgrd,work1)

  return
end subroutine variances
