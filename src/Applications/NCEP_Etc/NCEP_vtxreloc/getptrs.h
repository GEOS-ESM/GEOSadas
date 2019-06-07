!!
      interface
        subroutine getptrs(h,w,zs,ps,tv,qw,di,ze,uw,vw)
          implicit none
          real,target,dimension(:,:,:),intent(in) :: h
          real,target,dimension(:,:,:,:),intent(in) :: w
          real,pointer,dimension(:,:) :: zs,ps
          real,pointer,dimension(:,:,:) :: tv,qw,di,ze,uw,vw
        end subroutine getptrs
      end interface
