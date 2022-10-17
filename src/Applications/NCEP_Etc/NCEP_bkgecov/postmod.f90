module postmod

  use type_kinds, only: fp_kind
  implicit none

  integer ndeg,nasm
  parameter(ndeg=6,nasm=560)

contains

 subroutine smver(field,vsmth,nlat,nsig)
   implicit none
   integer nlat,nsig,i,k,kk
   real(fp_kind),dimension(nlat,nsig):: field
   real(fp_kind),dimension(nsig):: field_tmp
   real(fp_kind),dimension(nsig,nsig),intent(in) :: vsmth
   field_tmp=0
   do i=1,nlat
     do kk=1,nsig
       field_tmp(kk)=0
       do k=1,nsig
         field_tmp(kk)=field_tmp(kk)+vsmth(k,kk)*field(i,k)
       enddo
     end do
    field(i,1:nsig)=field_tmp(1:nsig)
   enddo
 return
 end subroutine smver

 subroutine smoothlat(field,nlevs,degs)
   use variables,only: nlat
   implicit none

   integer j,j2,k,nlevs
   real(fp_kind),dimension(nlat,nlevs):: field
   real(fp_kind),dimension(nlat):: field_sm
   real(fp_kind),dimension(nlat,nlat):: weights
   real(fp_kind) degs

! get weights for smoothing in lat direction
   call get_weights(degs,weights)

! smooth the field array based on weights computed 
   do k=1,nlevs
     field_sm=0.0
     do j=2,nlat-1
       do j2=2,nlat-1
         field_sm(j)=field_sm(j)+weights(j,j2)*field(j2,k)
       end do
     end do

! redefine field to be smoothed 
     do j=2,nlat-1
       field(j,k)=field_sm(j)
     end do
     field(1,k)=field(2,k)
     field(nlat,k)=field(nlat-1,k)

   end do   !end do loop over levs
 return

 end subroutine smoothlat


 subroutine get_weights(degs,wsmooth)
   use variables,only: nlat,deg2rad,rlats
   implicit none

   real(fp_kind),dimension(nlat):: rnorm,slat
   real(fp_kind),dimension(nlat,nlat):: wsmooth
   real(fp_kind),dimension(-nlat+4:nlat-2) :: wsmtemp

   real(fp_kind) sum,errmax,degs,arg,denom
   integer j,jj

! use difference in sin(lat) to calculate weighting
   do j=2,nlat-1
     slat(j)=sin(rlats(j))
   end do
   denom=1.0/(deg2rad*degs)

!   do jj=2,nlat-1
!     arg=.5*(denom*(slat(2)-slat(jj)))**2
!     wsmtemp(jj-1)=exp(-arg)
!   end do
!   do jj=-nlat+4,0
!     wsmtemp(jj)=wsmtemp(2-jj)
!   enddo

   rnorm=0.
   do j=2,nlat-1
     do jj=2,nlat-1
       arg=.5*(denom*(slat(j)-slat(jj)))**2
       wsmooth(j,jj)=exp(-arg)
       rnorm(j)=rnorm(j)+wsmooth(j,jj)
     end do
   end do

   rnorm(1)=rnorm(2)
   rnorm(nlat)=rnorm(nlat-1)

   rnorm=1./rnorm
   errmax=0.
   do j=2,nlat-1
     sum=0.0
     do jj=2,nlat-1
       wsmooth(j,jj)=rnorm(j)*wsmooth(j,jj)
       sum=sum+wsmooth(j,jj)
     end do
     errmax=max(abs(sum-1._fp_kind),errmax)
   end do

 return
 end subroutine get_weights

 subroutine writefiles
   use type_kinds,only: double,single,fp_kind
   use variables,only: smooth_vert_variances,vsmth,laddoz
   use variables,only: nlat,nlon,nsig 
   use variables,only: sfvar,vpvar,tvar,qvar,qivar,qlvar,qrvar,qsvar,cvar,psvar,nrhvar,ozvar
   use variables,only: sfhln,vphln,thln,qhln,qihln,qlhln,qrhln,qshln,chln,pshln,ozhln
   use variables,only: sfvln,vpvln,tvln,qvln,qivln,qlvln,qrvln,qsvln,cvln,ozvln
   use variables,only: tcon,vpcon,pscon
   use variables,only: hydromet
   use sstmod
   implicit none

!  Single precision variables for visualization
   real*4,allocatable,dimension(:,:,:):: tcon4
   real*4,allocatable,dimension(:,:):: sfvar4,vpvar4,tvar4,qvar4,cvar4,nrhvar4,ozvar4
   real*4,allocatable,dimension(:,:):: sfhln4,vphln4,thln4,qhln4,chln4,ozhln4
   real*4,allocatable,dimension(:,:):: sfvln4,vpvln4,tvln4,qvln4,cvln4,ozvln4
   real*4,allocatable,dimension(:,:):: qivar4,qihln4,qivln4,qlvar4,qlhln4,qlvln4
   real*4,allocatable,dimension(:,:):: qrvar4,qrhln4,qrvln4,qsvar4,qshln4,qsvln4
   real*4,allocatable,dimension(:,:):: vpcon4,pscon4,varsst4,corlsst4
   real*4,allocatable,dimension(:):: psvar4,pshln4

   real(fp_kind),allocatable,dimension(:,:):: qdummy
   integer i,j,k,m,outf,ncfggg,iret,kindex
   character(255) grdfile
   character*5 var(40)


! Interpolate sst statistics
! go file for use in GSI analysis code
   call create_sstvars(nlat,nlon)
   call sst_stats

   do k=1,nsig
     do i=1,nlat
       sfvar(i,k)=sqrt(sfvar(i,k))
       vpvar(i,k)=sqrt(vpvar(i,k))
       tvar(i,k) =sqrt(tvar(i,k))
       nrhvar(i,k)=sqrt(nrhvar(i,k))
       qvar(i,k) =sqrt(qvar(i,k))
       cvar(i,k) =sqrt(cvar(i,k))
       ozvar(i,k)=sqrt(ozvar(i,k))
     end do
   end do
   if (hydromet) then
      do k=1,nsig
        do i=1,nlat
          qivar(i,k)=sqrt(qivar(i,k))
          qlvar(i,k)=sqrt(qlvar(i,k))
          qrvar(i,k)=sqrt(qrvar(i,k))
          qsvar(i,k)=sqrt(qsvar(i,k))
        end do
      end do
   endif

   do i=1,nlat
     psvar(i)=sqrt(psvar(i))
   end do

   if(smooth_vert_variances)then
    call smver(sfvar,vsmth,nlat,nsig)
    call smver(vpvar,vsmth,nlat,nsig)
    call smver(tvar,vsmth,nlat,nsig)
    call smver(qvar,vsmth,nlat,nsig)
    if (hydromet) then
       call smver(qivar,vsmth,nlat,nsig)
       call smver(qlvar,vsmth,nlat,nsig)
       call smver(qrvar,vsmth,nlat,nsig)
       call smver(qsvar,vsmth,nlat,nsig)
    endif
    call smver(cvar,vsmth,nlat,nsig)
    call smver(ozvar,vsmth,nlat,nsig)
    call smver(nrhvar,vsmth,nlat,nsig)
   endif

! reset stats for cloud (FROM ADDOZ CODE)
!_RT_NON_SENSE   nrhvar(1:nlat,52:nsig)=0.0
   cvar=0
   chln=100*1.e3
   cvln=0.5

! write out files; RT: The file below is useless: it's not a grads file as the
!                      extention implies!
   outf=45
   open(outf,file='berror_stats.grd',form='unformatted')
   rewind outf
   if (hydromet) then
      write(outf) nsig,nlat,&
                  sfvar,vpvar,tvar,qvar,nrhvar,qivar,qlvar,qrvar,qsvar,ozvar,cvar,psvar,&
                  sfhln,vphln,thln,qhln,qihln,qlhln,qrhln,qshln,ozhln,chln,pshln,&
                  sfvln,vpvln,tvln,qvln,qivln,qlvln,qrvln,qsvln,ozvln,cvln,&
                  tcon,vpcon,pscon,&
                  varsst,corlsst
   else ! for file compliance, fill in w/ zeros when not hydrometeors are wanted
      allocate(qdummy(nlat,nsig))
      qdummy=0.0
      write(outf) nsig,nlat,&
                  sfvar,vpvar,tvar,qvar,nrhvar,qdummy,qdummy,qdummy,qdummy,ozvar,cvar,psvar,&
                  sfhln,vphln,thln,qhln,qdummy,qdummy,qdummy,qdummy,ozhln,chln,pshln,&
                  sfvln,vpvln,tvln,qvln,qdummy,qdummy,qdummy,qdummy,ozvln,cvln,&
                  tcon,vpcon,pscon,&
                  varsst,corlsst
      deallocate(qdummy)
   endif
   close(outf)

    var=' '
    var(1)='sf'
    var(2)='vp'
    var(3)='t'
    var(4)='q'
    var(5)='qi'
    var(6)='ql'
    var(7)='qr'
    var(8)='qs'
    var(9)='oz'
    var(10)='cw'
    var(11)='ps'
    var(12)='sst'

   open(outf,file='gsi.berror_stats',form='unformatted')
     rewind outf
     write(outf) nsig,nlat,nlon
     write(outf) tcon,vpcon,pscon
     write(outf) var(1),nsig
     write(outf) sfvar 
     write(outf) sfhln
     write(outf) sfvln
     write(outf) var(2),nsig
     write(outf) vpvar 
     write(outf) vphln
     write(outf) vpvln
     write(outf) var(3),nsig
     write(outf) tvar 
     write(outf) thln
     write(outf) tvln
     write(outf) var(4),nsig
     write(outf) qvar,nrhvar 
     write(outf) qhln
     write(outf) qvln
     write(outf) var(5),nsig
     write(outf) qivar
     write(outf) qihln
     write(outf) qivln
     write(outf) var(6),nsig
     write(outf) qlvar
     write(outf) qlhln
     write(outf) qlvln
     write(outf) var(7),nsig
     write(outf) qrvar
     write(outf) qrhln
     write(outf) qrvln
     write(outf) var(8),nsig
     write(outf) qsvar
     write(outf) qshln
     write(outf) qsvln
     write(outf) var(9),nsig
     write(outf) ozvar 
     write(outf) ozhln
     write(outf) ozvln
     write(outf) var(10),nsig
     write(outf) cvar 
     write(outf) chln
     write(outf) cvln
     m=1
     write(outf) var(11),m
     write(outf) psvar 
     write(outf) pshln
     write(outf) var(12),m
     write(outf) varsst
     write(outf) corlsst
   close(outf)

! allocate single precision arrays
   allocate(sfvar4(nlat,nsig),vpvar4(nlat,nsig),tvar4(nlat,nsig),qvar4(nlat,nsig),& 
            qivar4(nlat,nsig),qlvar4(nlat,nsig),qrvar4(nlat,nsig),qsvar4(nlat,nsig),&
            cvar4(nlat,nsig),nrhvar4(nlat,nsig),ozvar4(nlat,nsig))
   allocate(sfhln4(nlat,nsig),vphln4(nlat,nsig),thln4(nlat,nsig),qhln4(nlat,nsig),&
            qihln4(nlat,nsig),qlhln4(nlat,nsig),qrhln4(nlat,nsig),qshln4(nlat,nsig),& 
            chln4(nlat,nsig), ozhln4(nlat,nsig))
   allocate(sfvln4(nlat,nsig),vpvln4(nlat,nsig),tvln4(nlat,nsig),qvln4(nlat,nsig),&
            qivln4(nlat,nsig),qlvln4(nlat,nsig),qrvln4(nlat,nsig),qsvln4(nlat,nsig),&
            cvln4(nlat,nsig), ozvln4(nlat,nsig))
   allocate(pscon4(nlat,nsig),vpcon4(nlat,nsig))
   allocate(varsst4(nlat,nlon),corlsst4(nlat,nlon))
   allocate(tcon4(nlat,nsig,nsig))
   allocate(psvar4(nlat),pshln4(nlat))

! Load single precision arrays for visualization
   do k=1,nsig
     do i=1,nlat
       sfvar4(i,k)=sfvar(i,k)
       vpvar4(i,k)=vpvar(i,k)
       tvar4(i,k)=tvar(i,k)
       nrhvar4(i,k)=nrhvar(i,k)
       qvar4(i,k)=qvar(i,k)
       if (hydromet) then
          qivar4(i,k)=qivar(i,k)
          qlvar4(i,k)=qlvar(i,k)
          qrvar4(i,k)=qrvar(i,k)
          qsvar4(i,k)=qsvar(i,k)
       else
          qivar4(i,k)=0.0
          qlvar4(i,k)=0.0
          qrvar4(i,k)=0.0
          qsvar4(i,k)=0.0
       endif
       cvar4(i,k)=cvar(i,k)
       ozvar4(i,k)=ozvar(i,k)

       sfhln4(i,k)=sfhln(i,k)
       vphln4(i,k)=vphln(i,k)
       thln4(i,k)=thln(i,k)
       qhln4(i,k)=qhln(i,k)
       if (hydromet) then
          qihln4(i,k)=qihln(i,k)
          qlhln4(i,k)=qlhln(i,k)
          qrhln4(i,k)=qrhln(i,k)
          qshln4(i,k)=qshln(i,k)
       else
          qihln4(i,k)=0.0
          qlhln4(i,k)=0.0
          qrhln4(i,k)=0.0
          qshln4(i,k)=0.0
       endif
       chln4(i,k)=chln(i,k)
       ozhln4(i,k)=ozhln(i,k)

       sfvln4(i,k)=1./sfvln(i,k)
       vpvln4(i,k)=1./vpvln(i,k)
       tvln4(i,k)=1./tvln(i,k)
       qvln4(i,k)=1./qvln(i,k)
       if (hydromet) then
          qivln4(i,k)=1./qivln(i,k)
          qlvln4(i,k)=1./qlvln(i,k)
          qrvln4(i,k)=1./qrvln(i,k)
          qsvln4(i,k)=1./qsvln(i,k)
       else
          qivln4(i,k)=0.0
          qlvln4(i,k)=0.0
          qrvln4(i,k)=0.0
          qsvln4(i,k)=0.0
       endif
       cvln4(i,k)=1./cvln(i,k)
       ozvln4(i,k)=1./ozvln(i,k)

     end do
   end do
   do i=1,nlat
     psvar4(i)=psvar(i)
     pshln4(i)=pshln(i)
   end do
   do j=1,nlon
     do i=1,nlat
       varsst4(i,j)=varsst(i,j)
       corlsst4(i,j)=corlsst(i,j)
     end do
   end do
   do m=1,nsig
     do k=1,nsig
       do i=1,nlat
         tcon4(i,k,m)=tcon(i,k,m)
       end do
     end do
   end do
   do k=1,nsig
     do i=1,nlat
       pscon4(i,k)=pscon(i,k)
       vpcon4(i,k)=vpcon(i,k)
     end do
   end do

! CREATE SINGLE PRECISION BYTE-ADDRESSABLE FILE FOR GRADS
! OF LATIDUDE DEPENDENT VARIABLES
   grdfile='bgstats_sp.grd'
   ncfggg=len_trim(grdfile)
   call baopenwt(22,grdfile(1:ncfggg),iret)
   call wryte(22,4*nlat*nsig,sfvar4)
   call wryte(22,4*nlat*nsig,vpvar4)
   call wryte(22,4*nlat*nsig,tvar4)
   call wryte(22,4*nlat*nsig,qvar4)
   call wryte(22,4*nlat*nsig,nrhvar4)
   call wryte(22,4*nlat*nsig,qivar4)
   call wryte(22,4*nlat*nsig,qlvar4)
   call wryte(22,4*nlat*nsig,qrvar4)
   call wryte(22,4*nlat*nsig,qsvar4)
   call wryte(22,4*nlat*nsig,ozvar4)
   call wryte(22,4*nlat*nsig,cvar4)
   call wryte(22,4*nlat,psvar4)
   call wryte(22,4*nlat*nsig,sfhln4)
   call wryte(22,4*nlat*nsig,vphln4)
   call wryte(22,4*nlat*nsig,thln4)
   call wryte(22,4*nlat*nsig,qhln4)
   call wryte(22,4*nlat*nsig,qihln4)
   call wryte(22,4*nlat*nsig,qlhln4)
   call wryte(22,4*nlat*nsig,qrhln4)
   call wryte(22,4*nlat*nsig,qshln4)
   call wryte(22,4*nlat*nsig,ozhln4)
   call wryte(22,4*nlat*nsig,chln4)
   call wryte(22,4*nlat,pshln4)
   call wryte(22,4*nlat*nsig,sfvln4)
   call wryte(22,4*nlat*nsig,vpvln4)
   call wryte(22,4*nlat*nsig,tvln4)
   call wryte(22,4*nlat*nsig,qvln4)
   call wryte(22,4*nlat*nsig,qivln4)
   call wryte(22,4*nlat*nsig,qlvln4)
   call wryte(22,4*nlat*nsig,qrvln4)
   call wryte(22,4*nlat*nsig,qsvln4)
   call wryte(22,4*nlat*nsig,ozvln4)
   call wryte(22,4*nlat*nsig,cvln4)
   call wryte(22,4*nlat*nsig*nsig,tcon4)
   call wryte(22,4*nlat*nsig,vpcon4)
   call wryte(22,4*nlat*nsig,pscon4)
   call baclose(22,iret)

! CREATE SINGLE PRECISION BYTE-ADDRESSABLE FILE FOR GRADS
! OF SST STATISTICS
   grdfile='sststats_sp.grd'
   ncfggg=len_trim(grdfile)
   call baopenwt(23,grdfile(1:ncfggg),iret)
   call wryte(23,4*nlat*nlon,varsst4)
   call wryte(23,4*nlat*nlon,corlsst4)
   call baclose(23,iret)

   deallocate(tcon4)
   deallocate(sfvar4,vpvar4,tvar4,qvar4,qivar4,qlvar4,qrvar4,qsvar4,cvar4,nrhvar4,sfhln4,&
              vphln4,thln4,qhln4,qihln4,qlhln4,qrhln4,qshln4,chln4,sfvln4,vpvln4,tvln4,&
              qvln4,qivln4,qlvln4,qrvln4,qsvln4,cvln4,vpcon4,pscon4,varsst4,corlsst4, &
              ozvar4,ozhln4,ozvln4)
   deallocate(psvar4,pshln4)

  call destroy_sstvars

 return
 end subroutine writefiles

end module postmod
