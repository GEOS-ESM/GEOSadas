!
!                                **********************************************
!                                *             MODULE pmat3                   *
!                                *  R. J. Purser, NOAA/NCEP/EMC    Oct 2012   *
!                                *  jim.purser@noaa.gov                       *
!                                *                                            *
!                                **********************************************
!
! Basic generic matrix routines that can each be expressed conveniently as a
! fortran PURE FUNCTION, or, where matrix inversion is involved, as a
! fortran FUNCTION at least
!
! Single precision real and complex routines are not accommodated here.
! Where it generally makes sense to include an integer version, this is
! included and signified by a function name ending "_i".
! The real functions have names ending "_d".
! The complex functions have names ending "_c".
!
! Last modified: 
!  Keyser (2014-12-12) - print written to unit 41 rather than stdout (for use in
!                        prepobs_prepacqc program - limits amount of stdout)
!
! DIRECT DEPENDENCIES
! Libraries[their Modules]: pmat[pmat,pmat2]
! Additional Modules      : pietc, pkind
!
!=============================================================================
module pmat3
!=============================================================================
use pkind, only: dp,dpc
implicit none
private
public:: norv,mulmd,muldm,diag,inv,mulpp,difp,intp,invp,powp,polps,polpp, &
     copbm,copmb,transposeb,mulbb,mulbd,muldb,mulbv,mulbx,mulby, &
     mulvb,mulxb,mulyb,L1Lb,u1ub,LdLb,udub

interface norv;  module procedure norv_d,norv_c;                  end interface
interface mulmd; module procedure mulmd_i,mulmd_d,mulmd_c;        end interface
interface muldm; module procedure muldm_i,muldm_d,muldm_c;        end interface
interface diag
                 module procedure diagmofd_i,diagmofd_d,diagmofd_c, &
                                  diagdofm_i,diagdofm_d,diagdofm_c 
                                                                  end interface
interface inv;   module procedure invm_d,invm_c,  finvm_d,finvm_c,    &
                                  invmv_d,invmv_c,finvmv_d,finvmv_c
                                                                  end interface
interface mulpp; module procedure mulpp_i,mulpp_d,mulpp_c;        end interface
interface difp;  module procedure difp_d,difp_c,ndifp_d,ndifp_c;  end interface
interface intp;  module procedure intp_d,intp_c,nintp_d,nintp_c;  end interface
interface invp;  module procedure ninvp_d,ninvp_c;                end interface
interface powp;  module procedure npowp_d,npowp_c;                end interface
interface polps; module procedure polps_d,polps_c, &
                                  npolps_d,npolps_c;              end interface
interface polpp; module procedure npolpp_d,npolpp_c;              end interface
!-----------------------------------------------------------------------------
! Banded matrix functions:
interface copbm; module procedure copbm_d,copbm_c;                end interface
interface copmb; module procedure copmb_d,copmb_c;                end interface
interface transposeb; module procedure transposeb_d,transposeb_c; end interface
interface mulbb; module procedure mulbb_d;                        end interface
interface mulbd; module procedure mulbd_d;                        end interface
interface muldb; module procedure muldb_d;                        end interface
interface mulbv; module procedure mulbv_d;                        end interface
interface mulbx; module procedure mulbx_d;                        end interface
interface mulby; module procedure mulby_d;                        end interface
interface mulvb; module procedure mulvb_d;                        end interface
interface mulxb; module procedure mulxb_d;                        end interface
interface mulyb; module procedure mulyb_d;                        end interface
interface L1Lb;  module procedure L1Lb_d,fL1Lb_d;                 end interface
interface u1ub;  module procedure u1ub_d,fu1ub_d;                 end interface
interface LdLb;  module procedure LdLb_d,fLdLb_d;                 end interface
interface udub;  module procedure udub_d,fudub_d;                 end interface

contains
!=============================================================================
pure function norv_d(a)result(b)!                                       [norv]
!=============================================================================
! Norm of vector a
!------------------------------
real(dp),dimension(:),intent(in):: a
real(dp)                        :: b
b=sqrt(dot_product(a,a))
end function norv_d
!=============================================================================
pure function norv_c(a)result(b)!                                       [norv]
!=============================================================================
! Norm of vector a
!------------------------------
complex(dpc),dimension(:),intent(in):: a
real(dp)                            :: b
b=sqrt(real(dot_product(a,a)))
end function norv_c

!=============================================================================
pure function mulmd_i(a,d)result(b)!                                   [mulmd]
!=============================================================================
! matrix times diagonal
!------------------------------
integer,dimension(:,:),     intent(in):: a
integer,dimension(:)  ,     intent(in):: d
integer,dimension(size(a,1),size(a,2)):: b
integer                               :: i
forall(i=1:size(a,2))b(:,i)=a(:,i)*d(i)
end function mulmd_i
!=============================================================================
pure function mulmd_d(a,d)result(b)!                                   [mulmd]
!=============================================================================
! matrix times diagonal
!------------------------------
real(dp),dimension(:,:),     intent(in):: a
real(dp),dimension(:)  ,     intent(in):: d
real(dp),dimension(size(a,1),size(a,2)):: b
integer                                :: i
forall(i=1:size(a,2))b(:,i)=a(:,i)*d(i)
end function mulmd_d
!=============================================================================
pure function mulmd_c(a,d)result(b)!                                   [mulmd]
!=============================================================================
! matrix times diagonal
!------------------------------
complex(dpc),dimension(:,:),     intent(in):: a
complex(dpc),dimension(:)  ,     intent(in):: d
complex(dpc),dimension(size(a,1),size(a,2)):: b
integer                                    :: i
forall(i=1:size(a,2))b(:,i)=a(:,i)*d(i)
end function mulmd_c

!=============================================================================
pure function muldm_i(d,a)result(b)!                                   [muldm]
!=============================================================================
! matrix times diagonal
!------------------------------
integer,dimension(:)  ,     intent(in):: d
integer,dimension(:,:),     intent(in):: a
integer,dimension(size(a,1),size(a,2)):: b
integer                               :: i
forall(i=1:size(a,1))b(i,:)=d(i)*a(i,:)
end function muldm_i
!=============================================================================
pure function muldm_d(d,a)result(b)!                                   [muldm]
!=============================================================================
! matrix times diagonal
!------------------------------
real(dp),dimension(:)  ,     intent(in):: d
real(dp),dimension(:,:),     intent(in):: a
real(dp),dimension(size(a,1),size(a,2)):: b
integer                                :: i
forall(i=1:size(a,1))b(i,:)=d(i)*a(i,:)
end function muldm_d
!=============================================================================
pure function muldm_c(d,a)result(b)!                                   [muldm]
!=============================================================================
! matrix times diagonal
!------------------------------
complex(dpc),dimension(:)  ,     intent(in):: d
complex(dpc),dimension(:,:),     intent(in):: a
complex(dpc),dimension(size(a,1),size(a,2)):: b
integer                                    :: i
forall(i=1:size(a,1))b(i,:)=d(i)*a(i,:)
end function muldm_c

!=============================================================================
pure function diagmofd_i(d)result(a)!                                   [diag]
!=============================================================================
! Diagonal matrix possessing given diagonal elements
!------------------------------
integer,dimension(:),   intent(in):: d
integer,dimension(size(d),size(d)):: a
integer                           :: i
a=0; forall(i=1:size(d))a(i,i)=d(i)
end function diagmofd_i
!=============================================================================
pure function diagmofd_d(d)result(a)!                                   [diag]
!=============================================================================
! Diagonal matrix possessing given diagonal elements
!------------------------------
real(dp),dimension(:),   intent(in):: d
real(dp),dimension(size(d),size(d)):: a
integer                            :: i
a=0; forall(i=1:size(d))a(i,i)=d(i)
end function diagmofd_d
!=============================================================================
pure function diagmofd_c(d)result(a)!                                   [diag]
!=============================================================================
! Diagonal matrix possessing given diagonal elements
!------------------------------
complex(dpc),dimension(:),   intent(in):: d
complex(dpc),dimension(size(d),size(d)):: a
integer                                :: i
a=0; forall(i=1:size(d))a(i,i)=d(i)
end function diagmofd_c

!=============================================================================
pure function diagdofm_i(a)result(d)!                                   [diag]
!=============================================================================
! Diagonal vector of principal diagonal elements of square matrix
!------------------------------
integer,dimension(:,:),intent(in):: a
integer,dimension(size(a,1))     :: d
integer                          :: i
forall(i=1:size(a,1))d(i)=a(i,i)
end function diagdofm_i
!=============================================================================
pure function diagdofm_d(a)result(d)!                                   [diag]
!=============================================================================
! Diagonal vector of principal diagonal elements of square matrix
!------------------------------
real(dp),dimension(:,:),intent(in):: a
real(dp),dimension(size(a,1))     :: d
integer                           :: i
forall(i=1:size(a,1))d(i)=a(i,i)
end function diagdofm_d
!=============================================================================
pure function diagdofm_c(a)result(d)!                                   [diag]
!=============================================================================
! Diagonal vector of principal diagonal elements of square matrix
!------------------------------
complex(dpc),dimension(:,:),intent(in):: a
complex(dpc),dimension(size(a,1))     :: d
integer                               :: i
forall(i=1:size(a,1))d(i)=a(i,i)
end function diagdofm_c

!=============================================================================
function invm_d(a)result(b)!                                             [inv]
!=============================================================================
use pmat, only: sinv=>inv
real(dp),dimension(:,:),     intent(in):: a
real(dp),dimension(size(a,1),size(a,1)):: b
logical                                :: ff
b=a; call sinv(b,ff)
if(ff)stop 'In function invm_d; matrix singular, unable to continue'
end function invm_d
!=============================================================================
function invm_c(a)result(b)!                                             [inv]
!=============================================================================
use pmat, only: sinv=>inv
complex(dpc),dimension(:,:),     intent(in):: a
complex(dpc),dimension(size(a,1),size(a,1)):: b
logical                                    :: ff
b=a; call sinv(b,ff)
if(ff)stop 'In function invm_c; matrix singular, unable to continue'
end function invm_c
!=============================================================================
function finvm_d(a,ff)result(b)!                                         [inv]
!=============================================================================
use pmat, only: sinv=>inv
real(dp),dimension(:,:),     intent(in ):: a
logical,                     intent(out):: ff
real(dp),dimension(size(a,1),size(a,1)) :: b
b=a; call sinv(b,ff)
if(ff) write(41,'("In function finvm_d; singular matrix")')
end function finvm_d
!=============================================================================
function finvm_c(a,ff)result(b)!                                         [inv]
!=============================================================================
use pmat, only: sinv=>inv
complex(dpc),dimension(:,:),     intent(in ):: a
logical,                         intent(out):: ff
complex(dpc),dimension(size(a,1),size(a,1)) :: b
b=a; call sinv(b,ff)
if(ff) write(41,'("In function finvm_c; singular matrix")')
end function finvm_c

!=============================================================================
function invmv_d(a,v)result(u)!                                          [inv]
!=============================================================================
use pmat, only: sinv=>inv
real(dp),dimension(:,:),     intent(in):: a
real(dp),dimension(:)  ,     intent(in):: v
real(dp),dimension(size(a,1),size(a,1)):: b
real(dp),dimension(size(a,1))          :: u
logical                                :: ff
b=a; u=v; call sinv(b,u,ff)
if(ff)stop 'IN function invmv_d; matrix singular, unable to continue'
end function invmv_d
!=============================================================================
function invmv_c(a,v)result(u)!                                          [inv]
!=============================================================================
use pmat, only: sinv=>inv
complex(dpc),dimension(:,:),     intent(in):: a
complex(dpc),dimension(:)  ,     intent(in):: v
complex(dpc),dimension(size(a,1),size(a,1)):: b
complex(dpc),dimension(size(a,1))          :: u
logical                                    :: ff
b=a; u=v; call sinv(b,u,ff)
if(ff)stop 'IN function invmv_c; matrix singular, unable to continue'
end function invmv_c
!=============================================================================
function finvmv_d(a,v,ff)result(u)!                                      [inv]
!=============================================================================
use pmat, only: sinv=>inv
real(dp),dimension(:,:),     intent(in):: a
real(dp),dimension(:)  ,     intent(in):: v
logical,                     intent(out):: ff
real(dp),dimension(size(a,1),size(a,1)) :: b
real(dp),dimension(size(a,1))           :: u
b=a; u=v; call sinv(b,u,ff)
if(ff) write(41,'("In function finvmv_d; singular matrix")')
end function finvmv_d
!=============================================================================
function finvmv_c(a,v,ff)result(u)!                                      [inv]
!=============================================================================
use pmat, only: sinv=>inv
complex(dpc),dimension(:,:),     intent(in ):: a
complex(dpc),dimension(:)  ,     intent(in ):: v
logical,                         intent(out):: ff
complex(dpc),dimension(size(a,1),size(a,1)) :: b
complex(dpc),dimension(size(a,1))           :: u
b=a; u=v; call sinv(b,u,ff)
if(ff) write(41,'("In function finvmv_c; singular matrix")')
end function finvmv_c

!=============================================================================
pure function mulpp_i(a,b)result(c)!                                   [mulpp]
!=============================================================================
! Multiply two polynomials expressed by their coefficients, or convolve
! two one-sided discrete distributions of not necessarily equal size.
!=============================================================================
integer,dimension(0:),intent(in)      :: a,b
integer,dimension(0:size(a)+size(b)-2):: c
integer                               :: i,j
integer                               :: ai
c=0;do i=0,size(a)-1;ai=a(i);forall(j=0:size(b)-1)c(i+j)=c(i+j)+ai*b(j); enddo
end function mulpp_i
!=============================================================================
pure function mulpp_d(a,b)result(c)!                                   [mulpp]
!=============================================================================
real(dp),dimension(0:),intent(in)      :: a,b
real(dp),dimension(0:size(a)+size(b)-2):: c
integer                                :: i,j
real(dp)                               :: ai
c=0;do i=0,size(a)-1;ai=a(i);forall(j=0:size(b)-1)c(i+j)=c(i+j)+ai*b(j); enddo
end function mulpp_d
!=============================================================================
pure function mulpp_c(a,b)result(c)!                                   [mulpp]
!=============================================================================
complex(dpc),dimension(0:),intent(in)      :: a,b
complex(dpc),dimension(0:size(a)+size(b)-2):: c
integer                                    :: i,j
complex(dpc)                               :: ai
c=0;do i=0,size(a)-1;ai=a(i);forall(j=0:size(b)-1)c(i+j)=c(i+j)+ai*b(j); enddo
end function mulpp_c

!=============================================================================
pure function nmulpp_i(n,a,b)result(c)!                                [mulpp]
!=============================================================================
! Multiply two polynomials expressed by their coefficients, or convolve
! two one-sided discrete distributions of fixed size, truncating the result
! to the same size.
!=============================================================================
integer,               intent(in):: n
integer,dimension(0:n),intent(in):: a,b
integer,dimension(0:n)           :: c
integer                          :: i,j
integer                          :: ai
c=0; do i=0,n; ai=a(i);forall(j=0:n-i)c(i+j)=c(i+j)+ai*b(j); enddo
end function nmulpp_i
!=============================================================================
pure function nmulpp_d(n,a,b)result(c)!                                [mulpp]
!=============================================================================
integer,                intent(in):: n
real(dp),dimension(0:n),intent(in):: a,b
real(dp),dimension(0:n)           :: c
integer                           :: i,j
real(dp)                          :: ai
c=0; do i=0,n; ai=a(i);forall(j=0:n-i)c(i+j)=c(i+j)+ai*b(j); enddo
end function nmulpp_d
!=============================================================================
pure function nmulpp_c(n,a,b)result(c)!                                [mulpp]
!=============================================================================
integer,                    intent(in):: n
complex(dpc),dimension(0:n),intent(in):: a,b
complex(dpc),dimension(0:n)           :: c
integer                               :: i,j
complex(dpc)                          :: ai
c=0; do i=0,n; ai=a(i);forall(j=0:n-i)c(i+j)=c(i+j)+ai*b(j); enddo
end function nmulpp_c

!=============================================================================
pure function difp_d(a)result(b)!                                       [difp]
!=============================================================================
! Differentiate the polynomial, result being of degree one less.
!=============================================================================
real(dp),dimension(0:),intent(in):: a
real(dp),dimension(0:size(a)-2)  :: b
integer                          :: i
forall(i=1:size(a)-1)b(i-1)=i*a(i)
end function difp_d
!=============================================================================
pure function difp_c(a)result(b)!                                       [difp]
!=============================================================================
complex(dpc),dimension(0:),intent(in):: a
complex(dpc),dimension(0:size(a)-2)  :: b
integer                              :: i
forall(i=1:size(a)-1)b(i-1)=i*a(i)
end function difp_c
!=============================================================================
pure function ndifp_d(n,a)result(b)!                                    [difp]
!=============================================================================
! Differentiate the polynomial of fixed degree, force result to be same degree
!=============================================================================
integer,                intent(in):: n
real(dp),dimension(0:n),intent(in):: a
real(dp),dimension(0:n)           :: b
integer                           :: i
b(n)=0; forall(i=1:n)b(i-1)=i*a(i)
end function ndifp_d
!=============================================================================
pure function ndifp_c(n,a)result(b)!                                    [difp]
!=============================================================================
integer,                    intent(in):: n
complex(dpc),dimension(0:n),intent(in):: a
complex(dpc),dimension(0:n)           :: b
integer                               :: i
b(n)=0; forall(i=1:n)b(i-1)=i*a(i)
end function ndifp_c

!=============================================================================
pure function intp_d(a)result(b)!                                       [intp]
!=============================================================================
! Integrate the polynomial, result being of degree one greater.
!=============================================================================
real(dp),dimension(0:),intent(in):: a
real(dp),dimension(0:size(a))    :: b
integer                          :: i
b(0)=0; forall(i=1:size(a))b(i)=a(i-1)/i
end function intp_d
!=============================================================================
pure function intp_c(a)result(b)!                                       [intp]
!=============================================================================
complex(dpc),dimension(0:),intent(in):: a
complex(dpc),dimension(0:size(a))    :: b
integer                              :: i
b(0)=0; forall(i=1:size(a))b(i)=a(i-1)/i
end function intp_c
!=============================================================================
pure function nintp_d(n,a)result(b)!                                    [intp]
!=============================================================================
! Integrate the polynomial of fixed degree, force result to be same degree
!=============================================================================
integer,                intent(in):: n
real(dp),dimension(0:n),intent(in):: a
real(dp),dimension(0:n)           :: b
integer                           :: i
b(0)=0; forall(i=1:n)b(i)=a(i-1)/i
end function nintp_d
!=============================================================================
pure function nintp_c(n,a)result(b)!                                    [intp]
!=============================================================================
integer,                    intent(in):: n
complex(dpc),dimension(0:n),intent(in):: a
complex(dpc),dimension(0:n)           :: b
integer                               :: i
b(0)=0; forall(i=1:n)b(i)=a(i-1)/i
end function nintp_c

!=============================================================================
pure function ninvp_d(n,a)result(b)!                                    [invp]
!=============================================================================
integer,                intent(in):: n
real(dp),dimension(0:n),intent(in):: a
real(dp),dimension(0:n)           :: b
integer                           :: i
real(dp)                          :: b0
b0=1/a(0); b(0)=b0; do i=1,n; b(i)=-b0*sum(b(i-1:0:-1)*a(1:i)); enddo
end function ninvp_d
!=============================================================================
pure function ninvp_c(n,a)result(b)!                                    [invp]
!=============================================================================
integer,                    intent(in):: n
complex(dpc),dimension(0:n),intent(in):: a
complex(dpc),dimension(0:n)           :: b
integer                               :: i
complex(dpc)                          :: b0
b0=1/a(0); b(0)=b0; do i=1,n; b(i)=-b0*sum(b(i-1:0:-1)*a(1:i)); enddo
end function ninvp_c

!=============================================================================
pure function npowp_d(n,a,m)result(b)!                                  [powp]
!=============================================================================
integer,                intent(in):: n,m
real(dp),dimension(0:n),intent(in):: a
real(dp),dimension(0:n)           :: b
integer                           :: i
b=0; b(0)=1; do i=1,m; b=nmulpp_d(n,a,b); enddo
end function npowp_d
!=============================================================================
pure function npowp_c(n,a,m)result(b)!                                  [powp]
!=============================================================================
integer,                    intent(in):: n,m
complex(dpc),dimension(0:n),intent(in):: a
complex(dpc),dimension(0:n)           :: b
integer                               :: i
b=0; b(0)=1; do i=1,m; b=nmulpp_c(n,a,b); enddo
end function npowp_c

!=============================================================================
pure function polps_d(a,s1)result(s2) !                                [polps]
!=============================================================================
real(dp),dimension(0:),intent(in):: a
real(dp),              intent(in):: s1
real(dp)                         :: s2
integer                          :: i,n
n=size(a)-1; s2=a(n); do i=n-1,0,-1; s2=s2*s1+a(i); enddo
end function polps_d
!=============================================================================
pure function polps_c(a,s1)result(s2) !                                [polps]
!=============================================================================
complex(dpc),dimension(0:),intent(in):: a
complex(dpc),              intent(in):: s1
complex(dpc)                         :: s2
integer                              :: i,n
n=size(a)-1; s2=a(n); do i=n-1,0,-1; s2=s2*s1+a(i); enddo
end function polps_c
!=============================================================================
pure function npolps_d(n,a,s1)result(s2) !                             [polps]
!=============================================================================
integer,                intent(in):: n
real(dp),dimension(0:n),intent(in):: a
real(dp),               intent(in):: s1
real(dp)                          :: s2
integer                           :: i
s2=a(n); do i=n-1,0,-1; s2=s2*s1+a(i); enddo
end function npolps_d
!=============================================================================
pure function npolps_c(n,a,s1)result(s2) !                             [polps]
!=============================================================================
integer,                    intent(in):: n
complex(dpc),dimension(0:n),intent(in):: a
complex(dpc),               intent(in):: s1
complex(dpc)                          :: s2
integer                               :: i
s2=a(n); do i=n-1,0,-1; s2=s2*s1+a(i); enddo
end function npolps_c

!=============================================================================
pure function npolpp_d(n,a,b)result(c)!                                [polpp]
!=============================================================================
! Up to degree n, get polynomial series c(x)=a(b(x))
!--------------------------------
integer,                intent(in):: n
real(dp),dimension(0:n),intent(in):: a,b
real(dp),dimension(0:n)           :: c
integer                           :: i
c=a(n); do i=n-1,0,-1; c=nmulpp_d(n,c,b)+a(i); enddo
end function npolpp_d
!=============================================================================
pure function npolpp_c(n,a,b)result(c)!                                [polpp]
!=============================================================================
integer,                    intent(in):: n
complex(dpc),dimension(0:n),intent(in):: a,b
complex(dpc),dimension(0:n)           :: c
integer                               :: i
c=a(n); do i=n-1,0,-1; c=nmulpp_c(n,c,b)+a(i); enddo
end function npolpp_c

!-----------------------------------------------------------------------------
! Banded matrix functions begin here:

!=============================================================================
pure function copbm_d(m1,m2,mah1,mah2,aband)result(afull)!            [copbm]
!=============================================================================
integer,                          intent(IN ) :: m1, m2, mah1, mah2
real(dp),dimension(m1,-mah1:mah2),intent(IN ) :: aband
real(dp),dimension(m1,m2)                     :: afull
integer                                       :: i1,i2, i, j
!=============================================================================
afull=0.
do j=1,m1; i1=max(1,1-j); i2=min(m1,m2-j)
   do i=i1,i2; afull(i,j+i)= aband(i,j); enddo
enddo
end function copbm_d
!=============================================================================
pure function copbm_c(m1,m2,mah1,mah2,aband)result(afull)!            [copbm]
!=============================================================================
integer,                              intent(IN ) :: m1, m2, mah1, mah2
complex(dpc),dimension(m1,-mah1:mah2),intent(IN ) :: aband
complex(dpc),dimension(m1,m2)                     :: afull
integer                                           :: i1,i2, i, j
!=============================================================================
afull=0.
do j=1,m1; i1=max(1,1-j); i2=min(m1,m2-j)
   do i=i1,i2; afull(i,j+i)= aband(i,j); enddo
enddo
end function copbm_c

!=============================================================================
pure function copmb_d(m1,m2,mah1,mah2,afull)result(aband)!             [copmb]
!=============================================================================
use pmat2, only: clipb 
integer,                          intent(IN ):: m1, m2, mah1, mah2
real(dp),dimension(m1,m2),        intent(IN ):: afull
real(dp),dimension(m1,-mah1:mah2)            :: aband
integer                                      :: i1,i2, i, j
!=============================================================================
call clipb(m1,m2,mah1,mah2,aband)
do j=1,m1; i1=max(1,1-j); i2=min(m1,m2-j)
   do i=i1,i2; aband(i,j)= afull(i,j+i); enddo
enddo
end function copmb_d
!=============================================================================
pure function copmb_c(m1,m2,mah1,mah2,afull)result(aband)!             [copmb]
!=============================================================================
use pmat2, only: clipb 
integer,                          intent(IN ):: m1, m2, mah1, mah2
complex(dpc),dimension(m1,m2),    intent(IN ):: afull
complex(dpc),dimension(m1,-mah1:mah2)        :: aband
integer                                      :: i1,i2, i, j
!=============================================================================
call clipb(m1,m2,mah1,mah2,aband)
do j=1,m1; i1=max(1,1-j); i2=min(m1,m2-j)
   do i=i1,i2; aband(i,j)= afull(i,j+i); enddo
enddo
end function copmb_c

!=============================================================================
pure function transposeb_d(m1,m2,mah1,mah2,a)result(b)!           [transposeb]
!=============================================================================
use pmat2, only: clipb
integer,                          intent(IN):: m1, m2, mah1, mah2
real(dp),dimension(m1,-mah1:mah2),intent(IN):: a
real(dp),dimension(m2,-mah2:mah1)           :: b
integer                                     :: j, i
!=============================================================================
call CLIPB(m2,m1,mah2,mah1,b)
do j=-mah1,mah2
   forall(i = max(1,1-j) : min(m1,m2-j))b(j+i,-j)=a(i,j)
enddo
end function transposeb_d
!=============================================================================
pure function transposeb_c(m1,m2,mah1,mah2,a)result(b)!           [transposeb]
!=============================================================================
use pmat2, only: clipb
integer,                          intent(IN):: m1, m2, mah1, mah2
complex(dpc),dimension(m1,-mah1:mah2),intent(IN):: a
complex(dpc),dimension(m2,-mah2:mah1)           :: b
integer                                         :: j, i
!=============================================================================
call CLIPB(m2,m1,mah2,mah1,b)
do j=-mah1,mah2
   forall(i = max(1,1-j) : min(m1,m2-j))b(j+i,-j)=a(i,j)
enddo
end function transposeb_c

!=============================================================================
pure function mulbb_d(m1,m2,mah1,mah2,mbh1,mbh2, a,b)result(c)!       [mulbb]
!=============================================================================
integer,                          intent(IN):: m1,m2,mah1,mah2,mbh1,mbh2
real(dp),dimension(m1,-mah1:mah2),intent(in):: a
real(dp),dimension(m2,-mbh1:mbh2),intent(in):: b
real(dp)                                    :: c(m1,-mah1-mbh1:mah2+mbh2)
integer:: j,k,jpk,i1,i2
c=0
do j=-mah1,mah2; do k=-mbh1,mbh2; jpk=j+k; i1=max(1,1-j); i2=min(m1,m2-j)
   c(i1:i2,jpk)=c(i1:i2,jpk)+a(i1:i2,j)*b(j+i1:j+i2,k)
enddo;           enddo
end function mulbb_d

!=============================================================================
pure function mulbd_d(m1,m2,mah1,mah2,a,d)result(b)!                   [mulbd]
!=============================================================================
use pmat2, only: clipb
integer,                          intent(IN):: m1, m2, mah1, mah2
real(dp),dimension(m1,-mah1:mah2),intent(in):: a
real(dp),dimension(m2)           ,intent(in):: d
real(dp),dimension(m1,-mah1:mah2)           :: b
integer                                     :: j, i1,i2
!=============================================================================
call CLIPB(m1,m2,mah1,mah2,b)
do j=-mah1,mah2; i1=max(1,1-j); i2=min(m1,m2-j)
   b(i1:i2,j)=a(i1:i2,j)*d(j+i1:j+i2)
enddo
end function mulbd_d

!=============================================================================
pure function muldb_d(m1,m2,mah1,mah2,d,a)result(b)!                   [muldb]
!=============================================================================
use pmat2, only: clipb
integer,                          intent(IN):: m1, m2, mah1, mah2
real(dp),dimension(m1)           ,intent(in):: d
real(dp),dimension(m1,-mah1:mah2),intent(in):: a
real(dp),dimension(m1,-mah1:mah2)           :: b
integer                                     :: j
call CLIPB(m1,m2,mah1,mah2,b)
do j=-mah1,mah2; b(:,j)=d(:)*a(:,j); enddo
forall(j=-mah1:mah2)b(:,j)=d(:)*a(:,j)
end function muldb_d

!=============================================================================
pure function mulbv_d(m1,m2,mah1,mah2,a,v1)result(v2)!                 [mulbv]
!=============================================================================
integer,  intent(IN   ) :: m1, m2, mah1, mah2
real(dp),dimension(m1,-mah1:mah2),intent(in):: a
real(dp),dimension(m2),           intent(in):: v1
real(dp),dimension(m1)                      :: v2
integer                 :: j, i1,i2 
v2=0
do j=-mah1,mah2; i1=max(1,1-j); i2=min(m1,m2-j)
   v2(i1:i2) = v2(i1:i2) + a(i1:i2,j)*v1(j+i1:j+i2)
enddo
end function mulbv_d

!=============================================================================
pure function mulbx_d(m1,m2,mah1,mah2,my,a,v1)result(v2)!              [mulbx]
!=============================================================================
integer,                          intent(IN) :: m1, m2, mah1, mah2, my
real(dp),dimension(m1,-mah1:mah2),intent(in):: a
real(dp),dimension(m2,my),        intent(in):: v1
real(dp),dimension(m1,my)                   :: v2
integer                                     :: i,j
v2=0
do j=-mah1,mah2
   do i=max(1,1-j),min(m1,m2-j); v2(i,:)=v2(i,:)+a(i,j)*v1(i+j,:); enddo
   forall(i=max(1,1-j):min(m1,m2-j))v2(i,:)=v2(i,:)+a(i,j)*v1(i+j,:)
enddo
end function mulbx_d

!=============================================================================
pure function mulby_d(m1,m2,mah1,mah2,mx,a,v1)result(v2)!              [mulby]
!=============================================================================
integer,  intent(IN   ) :: m1, m2, mah1, mah2, mx
real(dp),dimension(m1,-mah1:mah2),intent(in):: a
real(dp),dimension(mx,m2),        intent(in):: v1
real(dp),dimension(mx,m1)                   :: v2
integer                                     :: i,j
v2=0
do j=-mah1,mah2
   forall(i=max(1,1-j):min(m1,m2-j))v2(:,i)=v2(:,i)+a(i,j)*v1(:,i+j)
enddo
end function mulby_d

!=============================================================================
pure function mulvb_d(m1,m2,mah1,mah2,v1,a)result(v2)!                 [mulvb]
!=============================================================================
integer,                          intent(IN) :: m1, m2, mah1, mah2
real(dp),dimension(m1),           intent(in):: v1
real(dp),dimension(m1,-mah1:mah2),intent(in):: a
real(dp),dimension(m2)                      :: v2
integer                                     :: j, i1,i2
v2=0
do j=-mah1,mah2; i1=max(1,1-j); i2=min(m1,m2-j)
   v2(j+i1:j+i2)=v2(j+i1:j+i2)+v1(i1:i2)*a(i1:i2,j)
enddo
end function mulvb_d

!=============================================================================
pure function mulxb_d(m1,m2,mah1,mah2,my,v1,a)result(v2)!              [mulxb]
!=============================================================================
integer,                          intent(IN) :: m1, m2, mah1, mah2, my
real(dp),dimension(m1,my),        intent(in):: v1
real(dp),dimension(m1,-mah1:mah2),intent(in):: a
real(dp),dimension(m2,my)                   :: v2
integer                                     :: i,j
v2=0
do j=-mah1,mah2
   forall(i=max(1,1-j):min(m1,m2-j))v2(j+i,:)=v2(j+i,:)+v1(i,:)*a(i,j)
enddo
end function mulxb_d

!=============================================================================
pure function mulyb_d(m1,m2,mah1,mah2,mx,v1,a)result(v2)!              [mulyb]
!=============================================================================
integer,                          intent(IN):: m1, m2, mah1, mah2, mx
real(dp),dimension(mx,m1),        intent(in):: v1
real(dp),dimension(m1,-mah1:mah2),intent(in):: a
real(dp),dimension(mx,m2)                   :: v2
integer                                     :: i,j
v2=0
do j=-mah1,mah2
   forall(i=max(1,1-j):min(m1,m2-j))v2(:,j+i)=v2(:,j+i)+v1(:,i)*a(i,j)
enddo
end function mulyb_d

!=============================================================================
function L1Lb_d(m,mah,a)result(b)!                                      [L1Lb]
!=============================================================================
integer,                        intent(IN ):: m, mah
real(dp),dimension(m,-mah:mah), intent(IN ):: a
real(dp),dimension(m,-mah:0)               :: b
logical                                    :: ff
b=fL1Lb_d(m,mah,a,ff)
if(ff)stop 'In L1Lb_d; matrix non-positive, cannot continue'
end function L1Lb_d
!=============================================================================
function fL1Lb_d(m,mah,a,ff)result(b)!                                  [L1Lb]
!=============================================================================
use pietc, only: T,F
use pmat2, only: clipb
integer,                        intent(IN ):: m, mah
real(dp),dimension(m,-mah:mah), intent(IN ):: a
logical,                        intent(out):: ff ! <- failure flag
real(dp),dimension(m,-mah:0)               :: b
integer                                    :: i, j,jmi
real(dp)                                   :: s
!=============================================================================
ff=F
call CLIPB(m,m,mah,0,b)
do j=1,m
   s=a(j,0)-dot_product(b(j,-mah:-1),b(j,-mah:-1))
   if(s <= 0)then
      ff=T
      write(41,'("In fL1LB_d; non-positivity at diagonal index",i5)'),j
      return
   endif
   s=sqrt(s); b(j,0)=s; s=1/s
   do i=j+1,min(m,j+mah); jmi=j-i
      b(i,jmi)=s*(a(i,jmi)-dot_product(b(i,-mah:jmi-1),b(j,-mah-jmi:-1)))
   enddo
enddo
end function fL1Lb_d

!=============================================================================
function u1ub_d(m,mah,a)result(b)!                                      [u1ub]
!=============================================================================
integer,                        intent(IN ):: m, mah
real(dp),dimension(m,-mah:mah), intent(IN ):: a
real(dp),dimension(m,-mah:0)               :: b
real(dp),dimension(m,-mah:mah)             :: at
real(dp),dimension(m,-mah:0  )             :: bt
!=============================================================================
at=a(m:1:-1,mah:-mah:-1); bt=l1lb_d(m,mah,at); b=bt(m:1:-1,0:-mah:-1)
end function u1ub_d
!=============================================================================
function fu1ub_d(m,mah,a,ff)result(b)!                                  [u1ub]
!=============================================================================
integer,                        intent(IN ):: m, mah
real(dp),dimension(m,-mah:mah), intent(IN ):: a
logical,                        intent(out):: ff ! <- failure flag
real(dp),dimension(m,-mah:0)               :: b
real(dp),dimension(m,-mah:mah)             :: at
real(dp),dimension(m,-mah:0  )             :: bt
!=============================================================================
at=a(m:1:-1,mah:-mah:-1); bt=fl1lb_d(m,mah,at,ff)
if(ff)then; write(41,'("In fulub_d; non-positive matrix")'); return; endif
b=bt(m:1:-1,0:-mah:-1)
end function fu1ub_d

!=============================================================================
function LdLb_d(m,mah,a)result(b)!                                      [LdLb]
!=============================================================================
integer,                       intent(IN):: m, mah
real(dp),dimension(m,-mah:mah),intent(IN):: a
real(dp),dimension(m,-mah:0)             :: b
logical                                  :: ff
b=fLdLb_d(m,mah,a,ff)
if(ff)stop 'In LdLb_d; matrix non-positive, unable to continue'
end function LdLb_d
!=============================================================================
function fLdLb_d(m,mah,a,ff)result(b)!                                   [LdLb]
!=============================================================================
use pietc, only: T,F
use pmat2, only: clipb
integer,                       intent(IN ):: m, mah
real(dp),dimension(m,-mah:mah),intent(IN ):: a
logical,                       intent(out):: ff ! <- failure flag
real(dp),dimension(m,-mah:0)              :: b
integer                                   :: i, j,k,jmi,lj,li
real(dp)                                  :: s,te
!=============================================================================
ff=F
call clipb(m,m,mah,0,b); b(:,0)=1
do j=1,m; lj=max(-mah,1-j)
   s=a(j,0)
   do k=lj,-1
      s=s-b(j,k)**2*b(k+j,0)
   enddo
   if(s <= 0)then
      ff=T
      write(41,'(" In fLDLB_d; non-positivity at diagonal index",i5)'),j
      return
   endif
   b(j,0)=s; s=1/s
   do i=j+1,min(m,j+mah); jmi=j-i  
      li=max(-mah,1-i); 
      lj=li-jmi; 
      te=a(i,jmi)
      do k=li,jmi-1
         te=te-b(i,k)*b(j,k-jmi)*b(i+k,0)
      enddo
      b(i,jmi)=s*te
   enddo
enddo
b(:,0)=1/b(:,0)
end function fLdLb_d

!=============================================================================
function udub_d(m,mah,a)result(b)!                                      [udub]
!=============================================================================
integer,                       intent(in):: m, mah
real(dp),dimension(m,-mah:mah),intent(in):: a
real(dp),dimension(m,0:mah)              :: b
real(dp),dimension(m,-mah:mah)           :: at
real(dp),dimension(m,-mah:0)             :: bt
at=a(m:1:-1,mah:-mah:-1);bt=LdLb_d(m,mah,at); b=bt(m:1:-1, 0:-mah:-1)
end function udub_d
!=============================================================================
function fudub_d(m,mah,a,ff)result(b)!                                  [udub]
!=============================================================================
integer,                       intent(in ):: m, mah
real(dp),dimension(m,-mah:mah),intent(in ):: a
logical,                       intent(out):: ff ! <- failure flag
real(dp),dimension(m,0:mah)               :: b
real(dp),dimension(m,-mah:mah)            :: at
real(dp),dimension(m,-mah:0)              :: bt
at=a(m:1:-1,mah:-mah:-1);bt=fLdLb_d(m,mah,at,ff)
if(ff)then; write(41,'("In fudub_d; matrix non-positive")'); return; endif
b=bt(m:1:-1, 0:-mah:-1)
end function fudub_d


end module pmat3
