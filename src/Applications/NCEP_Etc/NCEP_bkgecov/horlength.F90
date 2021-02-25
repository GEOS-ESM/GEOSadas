subroutine horlength(mycases,numcases,npes,mype)

  use type_kinds,only: fp_kind
  use m_zeit, only: zeit_ci,zeit_co
  use m_die, only: die
  use postmod,only: smoothlat,smver
  use variables,only: nlat,nlon,nsig,lgaus,lsmver,vsmth
  use variables,only: sfvar,vpvar,tvar,qvar,qivar,qlvar,qrvar,qsvar,cvar,psvar,ozvar
  use variables,only: sfhln,vphln,thln,qhln,qihln,qlhln,qrhln,qshln,chln,pshln,ozhln
  use variables,only: smoothdeg,db_prec,anaclw
  use variables,only: lat1,lon1,istart
  use variables,only: ijn,displs_g,iglobal,ltosi,ltosj
  use variables,only: filunit
  use variables,only: hydromet
  use variables,only: hrzsfactor
  use comm_mod, only: sub2grid,grid2sub,nsig1o
  use delmod,   only: delvars,hydrobias

#ifndef ibm_sp
  use m_mpif
#endif
  implicit none
#ifdef ibm_sp
  include 'mpif.h'
#endif

  integer i,j,k,m,kk,k2,ierror,mycases,numcases,mpi_rtype,mype,nn
  integer i2,i2m1,npes,nstride,nflds,nc
  integer :: mype_work,mm1,ni1,ni2,ix
  real(fp_kind) r_norm, r_small,r_lim1,r_lim2
  real(fp_kind),allocatable,dimension(:,:):: workgrd
  real(fp_kind),allocatable,dimension(:,:,:):: work
  real(fp_kind),allocatable,dimension(:,:,:) :: grdsf,grdvp,gridt,gridq,grdrh,grdoz,grdc
  real(fp_kind),allocatable,dimension(:,:,:) :: gridqi,gridql,gridqr,gridqs
  real(fp_kind),allocatable,dimension(:,:):: gridp
  real(fp_kind),allocatable,dimension(:,:,:):: sf3,vp3,t3,q3,oz3,cw3
  real(fp_kind),allocatable,dimension(:,:,:):: qi3,ql3,qr3,qs3
  real(fp_kind),allocatable,dimension(:,:):: ps3
  real(fp_kind),allocatable,dimension(:,:,:) :: dummy
  integer,allocatable,dimension(:) :: nprocs
  logical,parameter :: specdecomp = .true. ! never to make it out of here; typically it makes no sense to set to false

  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

  mype_work=npes/2
  nflds=6
  if(anaclw) nflds=nflds+1
  if(hydromet) nflds=nflds+4
  allocate(nprocs(nflds))
  if(npes<=2*nflds) then
     nprocs=mype_work ! as original code, gather works off the same single PE when handling each field
  else
     nstride = npes/nflds-1
     i=0
     do k=1,npes,nstride
        i=i+1
        nprocs(i)=k
!       nprocs(i)=mype_work ! test
     enddo
  endif

  r_small=1.e-8
  r_lim1= 5.e+5
  r_lim2= 2.5e+3

  mm1=mype+1
  r_norm=1./float(numcases)
  if (mype==0) write(6,*) 'IN ROUTINE TO CALCULATE HORIZ LENGTH SCALES'

  allocate(gridp(lat1,lon1))
  allocate(grdsf(lat1,lon1,nsig))
  allocate(grdvp(lat1,lon1,nsig))
  allocate(gridt(lat1,lon1,nsig))
  allocate(gridq(lat1,lon1,nsig))
  allocate(grdrh(lat1,lon1,nsig))
  allocate(grdoz(lat1,lon1,nsig))
  allocate(grdc (lat1,lon1,nsig))
  if (hydromet) then
     allocate(dummy (lat1,lon1,nsig))
     allocate(gridqi(lat1,lon1,nsig))
     allocate(gridql(lat1,lon1,nsig))
     allocate(gridqr(lat1,lon1,nsig))
     allocate(gridqs(lat1,lon1,nsig))
     dummy=0.
  endif

  filunit = 1000*(mype+1)+1
  open(filunit,form='unformatted')
  rewind(filunit)

  allocate(sf3(lat1,lon1,nsig)); sf3=0.
  allocate(vp3(lat1,lon1,nsig)); vp3=0.
  allocate( t3(lat1,lon1,nsig));  t3=0.
  allocate( q3(lat1,lon1,nsig));  q3=0.
  allocate(oz3(lat1,lon1,nsig)); oz3=0.
  allocate(cw3(lat1,lon1,nsig)); cw3=0.
  allocate(ps3(lat1,lon1)); ps3=0.
  if (hydromet) then
     allocate(qi3(lat1,lon1,nsig)); qi3=0.
     allocate(ql3(lat1,lon1,nsig)); ql3=0.
     allocate(qr3(lat1,lon1,nsig)); qr3=0.
     allocate(qs3(lat1,lon1,nsig)); qs3=0.
  endif


  do nn=1,numcases
    if (hydromet) then
       read(filunit) grdsf,grdvp,gridt,gridp,gridq,grdrh,gridqi,gridql,gridqr,gridqs,grdoz,grdc
       call delvars  (grdsf,grdvp,gridt,gridp,gridq,grdoz,grdc,mype)
       call hydrobias(gridqi,gridql,gridqr,gridqs)
    else
       read(filunit) grdsf,grdvp,gridt,gridp,gridq,grdrh,grdoz,grdc
       call delvars(grdsf,grdvp,gridt,gridp,gridq,grdoz,grdc,mype)
    endif
    
!   Normalize by standard deviation
    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          ix=istart(mype+1)+i-1
          grdsf(i,j,k)=grdsf(i,j,k)/sqrt(sfvar(ix,k)) 
          grdvp(i,j,k)=grdvp(i,j,k)/sqrt(vpvar(ix,k))
          gridt(i,j,k)=gridt(i,j,k)/sqrt( tvar(ix,k))
          gridq(i,j,k)=gridq(i,j,k)/sqrt( qvar(ix,k))
          grdoz(i,j,k)=grdoz(i,j,k)/sqrt(ozvar(ix,k))
          if(anaclw)then
            grdc(i,j,k)= grdc(i,j,k)/sqrt( cvar(ix,k))
          endif 
        end do 
      end do 
    end do 
    if (hydromet) then
       do k=1,nsig
         do j=1,lon1
           do i=1,lat1
             ix=istart(mype+1)+i-1
             gridqi(i,j,k)=max(r_small,gridqi(i,j,k))/max(r_small,sqrt(qivar(ix,k)))
             gridql(i,j,k)=max(r_small,gridql(i,j,k))/max(r_small,sqrt(qlvar(ix,k)))
             gridqr(i,j,k)=max(r_small,gridqr(i,j,k))/max(r_small,sqrt(qrvar(ix,k)))
             gridqs(i,j,k)=max(r_small,gridqs(i,j,k))/max(r_small,sqrt(qsvar(ix,k)))
           end do 
         end do 
       end do 
    endif
    do j=1,lon1
      do i=1,lat1
        ix=istart(mype+1)+i-1
        gridp(i,j)=gridp(i,j)/sqrt(psvar(ix))
      end do 
    end do 

    if (specdecomp) then

       call zeit_ci('horspec')

!      Place on evenly distributed horizontal slabs
       allocate(work(nlat,nlon,nsig1o))
       if (hydromet) then
          call sub2grid(work,grdsf,grdvp,gridt,gridq,gridqi,gridql,gridqs,gridql,grdoz,grdc,dummy,dummy,gridp)
       else
          call sub2grid(work,grdsf,grdvp,gridt,gridq,grdoz,grdc,gridp)
       endif

!      Loop over nsigqo levels and apply spectral decomposition
       allocate(workgrd(nlat,nlon))
       do k=1,nsig1o

         do j=1,nlon
           do i=1,nlat
             workgrd(i,j)=work(i,j,k)
           end do
         end do 

         if(lgaus)then
           call m_speclap(workgrd,1)
         else
           call m_calap(workgrd,1)
         endif
         do j=1,nlon
           do i=1,nlat
             work(i,j,k)=workgrd(i,j)
           end do 
         end do 
       end do ! end do nsig10 loop
       deallocate(workgrd)

!      Transform work array back to subdomain    
       if (hydromet) then
          call grid2sub(work,grdsf,grdvp,gridt,gridq,gridqi,gridql,gridqr,gridqs,grdoz,grdc,dummy,dummy,gridp)
       else
          call grid2sub(work,grdsf,grdvp,gridt,gridq,grdoz,grdc,gridp)
       endif
       deallocate(work)

       call zeit_co('horspec')

    endif ! specdecomp

!   Load into average laplacian arrays
    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          sf3(i,j,k) = sf3(i,j,k) + grdsf(i,j,k)*grdsf(i,j,k)*r_norm
          vp3(i,j,k) = vp3(i,j,k) + grdvp(i,j,k)*grdvp(i,j,k)*r_norm
           t3(i,j,k) =  t3(i,j,k) + gridt(i,j,k)*gridt(i,j,k)*r_norm
           q3(i,j,k) =  q3(i,j,k) + gridq(i,j,k)*gridq(i,j,k)*r_norm
          oz3(i,j,k) = oz3(i,j,k) + grdoz(i,j,k)*grdoz(i,j,k)*r_norm
          cw3(i,j,k) = cw3(i,j,k) +  grdc(i,j,k)* grdc(i,j,k)*r_norm
        end do
      end do
    end do
    if (hydromet) then
       do k=1,nsig
         do j=1,lon1
           do i=1,lat1
              qi3(i,j,k)=  qi3(i,j,k)+ gridqi(i,j,k)*gridqi(i,j,k)*r_norm
              ql3(i,j,k)=  ql3(i,j,k)+ gridql(i,j,k)*gridql(i,j,k)*r_norm
              qr3(i,j,k)=  qr3(i,j,k)+ gridqr(i,j,k)*gridqr(i,j,k)*r_norm
              qs3(i,j,k)=  qs3(i,j,k)+ gridqs(i,j,k)*gridqs(i,j,k)*r_norm
           end do
         end do
       end do
    endif
    do j=1,lon1
      do i=1,lat1
         ps3(i,j) = ps3(i,j) + gridp(i,j)*gridp(i,j)*r_norm
      end do 
    end do 
 
  end do !end do cases on mype (n)

  close(filunit)

  if (hydromet) then
    deallocate(gridqs)
    deallocate(gridqr)
    deallocate(gridql)
    deallocate(gridqi)
    deallocate(dummy )
  endif
  deallocate(grdc )
  deallocate(grdoz)
  deallocate(grdrh)
  deallocate(gridq)
  deallocate(gridt)
  deallocate(grdvp)
  deallocate(grdsf)
  deallocate(gridp)


! Convert to zonal mean quantities
  nc=1
  call horlength_gather3d_(sfhln,sf3,nprocs(nc),.false.)
  deallocate(sf3)

  nc=nc+1
  call horlength_gather3d_(vphln,vp3,nprocs(nc),.false.)
  deallocate(vp3)

  nc=nc+1
  call horlength_gather3d_(thln,t3,nprocs(nc),.false.)
  deallocate(t3)

  nc=nc+1
  call horlength_gather3d_(qhln,q3,nprocs(nc),.false.)
  deallocate(q3)

  if (hydromet) then
     nc=nc+1
     where(qi3<1.e-36) qi3=1e-30
     call horlength_gather3d_(qihln,qi3,nprocs(nc),.true.)
     deallocate(qi3)
     nc=nc+1
     where(ql3<1.e-36) ql3=1e-30
     call horlength_gather3d_(qlhln,ql3,nprocs(nc),.true.)
     deallocate(ql3)
     nc=nc+1
     where(qr3<1.e-36) qr3=1e-30
     call horlength_gather3d_(qrhln,qr3,nprocs(nc),.true.)
     deallocate(qr3)
     nc=nc+1
     where(qs3<1.e-36) qs3=1e-30
     call horlength_gather3d_(qshln,qs3,nprocs(nc),.true.)
     deallocate(qs3)
  endif ! hydromet

  if(anaclw)then
     nc=nc+1
     call horlength_gather3d_(chln,cw3,nprocs(nc),.false.)
     deallocate(cw3)
  else
     chln = 0
  endif

  nc=nc+1
  call horlength_gather3d_(ozhln,oz3,nprocs(nc),.false.)
  deallocate(oz3)

  nc=nc+1
  call horlength_gather2d_(pshln,ps3,nprocs(nc))
  deallocate(ps3)

  deallocate(nprocs)

  if(lsmver .and. mype==0)then
    call smver(sfhln,vsmth,nlat,nsig)
    call smver(vphln,vsmth,nlat,nsig)
    call smver(thln,vsmth,nlat,nsig)
    call smver(qhln,vsmth,nlat,nsig)
    if (hydromet) then
       call smver(qihln,vsmth,nlat,nsig)
       call smver(qlhln,vsmth,nlat,nsig)
       call smver(qrhln,vsmth,nlat,nsig)
       call smver(qshln,vsmth,nlat,nsig)
    endif
    call smver(chln,vsmth,nlat,nsig)
    call smver(ozhln,vsmth,nlat,nsig)
  endif

  if (mype==0) write(6,*) 'ENDING THE ROUTINE TO CALCULATE HORIZ LENGTH SCALES'

  return
contains
  subroutine horlength_gather2d_(fldo,fldi,nproc)
  real(fp_kind),intent(in)   :: fldi(:,:) 
  real(fp_kind),intent(inout):: fldo(:) 
  integer,intent(in) :: nproc
  real(fp_kind),allocatable,dimension(:):: fldlap
  real(fp_kind),allocatable,dimension(:):: work1
  if(mype==nproc) then
    allocate(work1(iglobal))
  endif
  call mpi_gatherv(fldi,ijn(mm1),mpi_rtype,&
                   work1,ijn,displs_g,mpi_rtype,&
                   nproc,mpi_comm_world,ierror)
  if (mype==nproc) then
    allocate(workgrd(nlat,nlon))
    do kk=1,iglobal
      ni1=ltosi(kk); ni2=ltosj(kk)
      workgrd(ni1,ni2)=work1(kk)
    end do
    deallocate(work1)
    allocate(fldlap(nlat)); fldlap=0.
    do i=1,nlat
      do j=1,nlon
        fldlap(i) = fldlap(i) + workgrd(i,j)/float(nlon)
      end do
    end do
    deallocate(workgrd)
    do i=1,nlat
       fldo(i)=(8./fldlap(i))**.25
    end do
    call smoothlat(fldo,1,smoothdeg)
  end if
  call mpi_bcast(fldo,nlat,mpi_rtype,nproc,mpi_comm_world,ierror)
  end subroutine horlength_gather2d_ 

  subroutine horlength_gather3d_(fldo,fldi,nproc,applim)
  real(fp_kind),intent(in)   :: fldi(:,:,:) 
  real(fp_kind),intent(inout):: fldo(:,:) 
  logical,intent(in) :: applim
  integer,intent(in) :: nproc
  real(fp_kind),allocatable,dimension(:,:):: fldlap
  real(fp_kind),allocatable,dimension(:):: work1
  if (mype==nproc) then
    allocate(work1(iglobal))
  endif
  do k=1,nsig
     call mpi_gatherv(fldi(1,1,k),ijn(mm1),mpi_rtype,&
                      work1,ijn,displs_g,mpi_rtype,&
                      nproc,mpi_comm_world,ierror)
     if (mype==nproc) then
       allocate(workgrd(nlat,nlon))
       do kk=1,iglobal
         ni1=ltosi(kk); ni2=ltosj(kk)
         workgrd(ni1,ni2)=work1(kk)
       end do
       if (k==1) then
          allocate(fldlap(nlat,nsig)); fldlap=0.
       endif
       do i=1,nlat
         do j=1,nlon
           fldlap(i,k) = fldlap(i,k) + workgrd(i,j)/float(nlon)
         end do
       end do
       deallocate(workgrd)
     end if
  end do
  if (mype==nproc) then
     deallocate(work1)
     do k=1,nsig
       do i=1,nlat
         fldo(i,k)=(8./fldlap(i,k))**.25
       enddo
     enddo
     if(hrzsfactor>0.0) then
       fldo=hrzsfactor*fldo
     endif
     if (applim) then
        do k=1,nsig
          do i=1,nlat
            fldo(i,k)=max(min(r_lim1,fldo(i,k)),r_lim2)
          enddo
        enddo
     endif
     call smoothlat(fldo,nsig,smoothdeg)
     deallocate(fldlap)
  endif
  call mpi_bcast(fldo,nlat*nsig,mpi_rtype,nproc,mpi_comm_world,ierror)
  end subroutine horlength_gather3d_ 
end subroutine horlength
