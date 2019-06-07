subroutine readpairs(tskcases,npes,mype,numcases,mycases)
  use variables, only: nsig
  use variables, only: sigl,ak5,bk5
  use variables, only: na,nb,filename,hybrid,db_prec
  use variables, only: bcorrt,bcorrd,bcorrv,bcorrp
  use variables, only: bbiast,bbiasd,bbiasv,bbiasp
  use specgrid, only: nc,factvml,factsml
  use type_kinds, only: fp_kind
#ifndef ibm_sp
  use m_mpif
#endif
  implicit none
#ifdef ibm_sp
  include 'mpif.h'
#endif

  integer tskcases,npes,mype,numcases,mycases,ierror,mpi_rtype
  integer i,j,k,inges,inge2,total,n
  real(fp_kind),dimension(nc,nsig,4):: bfactt,bfactd,bfactv
  real(fp_kind),dimension(nc,4):: bfactp

  integer,dimension(4):: idateg
  real(fp_kind),dimension(nc):: z,z2
  real*4,dimension(nc):: z4
  real*4 hourg4
  real*4,dimension(nsig+1):: ak5r4,bk5r4,sigi4
  real*4,dimension(nsig):: sigl4

  logical ice
  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

  inges=50
  inge2=51
  mycases=0
  bfactp=0.
  bfactt=0.
  bfactd=0.
  bfactv=0.
  do n=1,tskcases
    total=npes*(n-1)+mype+1
    if(total.le.numcases)then
      mycases=mycases+1
      write(6,*)'opening=', inges,filename(na(total))
      write(6,*)'opening=', inge2,filename(nb(total))
      open(inges,file=filename(na(total)),form='unformatted',action='read')
      open(inge2,file=filename(nb(total)),form='unformatted',action='read')

      rewind inges
      rewind inge2

!     read in hour/date/vertical coords
      read(inges)
      read(inge2)
      if (hybrid) then
        read(inges) hourg4,idateg,ak5r4,bk5r4
        read(inge2) hourg4,idateg,ak5r4,bk5r4
      else
        read(inges) hourg4,idateg,sigi4,sigl4
        read(inge2) hourg4,idateg,sigi4,sigl4
      end if

!     terrain coefs
      read(inges)z4
      read(inge2)z4

!     surface pressure coefs
      read(inges)z4
      z=z4
      read(inge2)z4
      z2=z4

      do i=1,nc
        bfactp(i,1)=bfactp(i,1)+factsml(i)*(z(i)*z2(i))
        bfactp(i,2)=bfactp(i,2)+factsml(i)*(z2(i)*z2(i))
        bfactp(i,3)=bfactp(i,3)+factsml(i)*z(i)
        bfactp(i,4)=bfactp(i,4)+factsml(i)*z2(i)
      end do

!     temperature coefs
      do k=1,nsig
        read(inges)z4
        z=z4
        read(inge2)z4
        z2=z4

        do i=1,nc
          bfactt(i,k,1)=bfactt(i,k,1)+factsml(i)*(z(i)*z2(i))
          bfactt(i,k,2)=bfactt(i,k,2)+factsml(i)*(z2(i)*z2(i))
          bfactt(i,k,3)=bfactt(i,k,3)+factsml(i)*z(i)
          bfactt(i,k,4)=bfactt(i,k,4)+factsml(i)*z2(i)
        end do
      end do  ! enddo k

!     vorticity and divergence coefs
      do k=1,nsig
        read(inges)z4
        z=z4
        read(inge2)z4
        z2=z4

!       divergence
        do i=1,nc
          bfactd(i,k,1)=bfactd(i,k,1)+factvml(i)*(z(i)*z2(i))
          bfactd(i,k,2)=bfactd(i,k,2)+factvml(i)*(z2(i)*z2(i))
          bfactd(i,k,3)=bfactd(i,k,3)+factvml(i)*z(i)
          bfactd(i,k,4)=bfactd(i,k,4)+factvml(i)*z2(i)

        end do

        read(inges)z4
        z=z4
        read(inge2)z4
        z2=z4

!       vorticity
        do i=1,nc
          bfactv(i,k,1)=bfactv(i,k,1)+factvml(i)*(z(i)*z2(i))
          bfactv(i,k,2)=bfactv(i,k,2)+factvml(i)*(z2(i)*z2(i))
          bfactv(i,k,3)=bfactv(i,k,3)+factvml(i)*z(i)
          bfactv(i,k,4)=bfactv(i,k,4)+factvml(i)*z2(i)
        end do
      end do  ! enddo k
   
      close(inges)
      close(inge2)

    end if
  end do

  bcorrp=1.
  bcorrt=1.
  bcorrd=1.
  bcorrv=1.
  bbiasp=0.
  bbiast=0.
  bbiasd=0.
  bbiasv=0.
  call mpi_allreduce((bfactp),bfactp,nc*4,mpi_rtype,mpi_sum, &
                      mpi_comm_world,ierror)
  call mpi_allreduce((bfactt),bfactt,nc*nsig*4,mpi_rtype,mpi_sum, &
                      mpi_comm_world,ierror)
  call mpi_allreduce((bfactd),bfactd,nc*nsig*4,mpi_rtype,mpi_sum, &
                      mpi_comm_world,ierror)
  call mpi_allreduce((bfactv),bfactv,nc*nsig*4,mpi_rtype,mpi_sum, &
                      mpi_comm_world,ierror)
  do i=1,nc
    do n=1,4
      bfactp(i,n)=bfactp(i,n)/float(numcases)
    end do

    if(abs(bfactp(i,2)-bfactp(i,4)**2) > 1.e-26)then
         bcorrp(i)=(bfactp(i,1)-bfactp(i,3)*bfactp(i,4)) &
                  /(bfactp(i,2)-bfactp(i,4)**2)
    end if
    bbiasp(i)=bfactp(i,3)-bcorrp(i)*bfactp(i,4)

  end do
  do k=1,nsig
    do i=1,nc
      do n=1,4
        bfactt(i,k,n)=bfactt(i,k,n)/float(numcases)
        bfactd(i,k,n)=bfactd(i,k,n)/float(numcases)
        bfactv(i,k,n)=bfactv(i,k,n)/float(numcases)
      end do

      if(abs(bfactt(i,k,2)-bfactt(i,k,4)**2) > 1.e-26)then
        bcorrt(i,k)=(bfactt(i,k,1)-bfactt(i,k,3)*bfactt(i,k,4)) &
                   /(bfactt(i,k,2)-bfactt(i,k,4)**2)
      end if
      bbiast(i,k)=bfactt(i,k,3)-bcorrt(i,k)*bfactt(i,k,4)

      if(abs(bfactd(i,k,2)-bfactd(i,k,4)**2) > 1.e-26)then 
        bcorrd(i,k)=(bfactd(i,k,1)-bfactd(i,k,3)*bfactd(i,k,4)) &
                   /(bfactd(i,k,2)-bfactd(i,k,4)**2)
      end if
      bbiasd(i,k)=bfactd(i,k,3)-bcorrd(i,k)*bfactd(i,k,4)

      if(abs(bfactv(i,k,2)-bfactv(i,k,4)**2) > 1.e-26)then
        bcorrv(i,k)=(bfactv(i,k,1)-bfactv(i,k,3)*bfactv(i,k,4)) &
                   /(bfactv(i,k,2)-bfactv(i,k,4)**2)
      end if
      bbiasv(i,k)=bfactv(i,k,3)-bcorrv(i,k)*bfactv(i,k,4)

    end do
  end do

  return
end subroutine readpairs

