  subroutine genqsat(temp,qsat,lat2,lon2,ps,icesat,slg,a5,b5)
!   input argument list:
!     t,ps     - temperature, psfc on gaussian grid
!     lat1     - number of gaussian lats in the sub-domain array
!     lon1     - number of gaussian longitudes in the sub-domain array
!     nsig     - number of sigma levels
!     sigl      - sigma values at mid-point of model levels
!     icesat   - logical flag:  T=include ice and ice-water effects,
!                depending on t, in qsat and esat calcuations.
!                otherwise, compute esat and qsat with respect to water surface
!                depending on t, in qsat and esat calcuations.
!                otherwise, compute esat and qsat with respect to water surface
!
!   output argument list:
!     qsat     - specific humidity (input), saturation specific humidity (output)
  use type_kinds, only: fp_kind
  use variables,only: hybrid,nsig,rd,zero,psat,fv,one,rv,cliq,cvap,hvap,csol,&
      hfus,ttp
  implicit none

  real(fp_kind) dldt,xa,xb,hsub,dldti,xai,xbi,tmix
  parameter(dldt=cvap-cliq,xa=-dldt/rv,xb=xa+hvap/(rv*ttp))
  parameter(hsub=hvap+hfus)
  parameter(dldti=cvap-csol,xai=-dldti/rv,xbi=xai+hsub/(rv*ttp))
  parameter (tmix=ttp-20.0_fp_kind)
  real(fp_kind) eps,omeps
  parameter (eps=rd/rv)
  parameter (omeps = 1._fp_kind-eps)
!
  logical icesat
  integer k,j,i,ntsig
  integer lat2,lon2
  real(fp_kind) pw,q,tdry,tr,es,qs,esi,esw
  real(fp_kind) w,onep3,esmax
  real(fp_kind),dimension(lat2,lon2,nsig):: qsat
! real(fp_kind),dimension(lat2,lon2,nsig):: esat
  real(fp_kind),dimension(lat2,lon2,nsig):: temp
  real(fp_kind),dimension(lat2,lon2):: ps
  real(fp_kind),dimension(nsig):: slg
  real(fp_kind),dimension(nsig+1):: a5,b5
!
  onep3 = 1.e3_fp_kind

  if (icesat) then
     do k = 1,nsig
        do j = 1,lon2
           do i = 1,lat2

             if (hybrid) then
               pw = 0.5*(a5(k)+a5(k+1)+(b5(k)+b5(k+1))* &
                          ps(i,j))
             else
               pw = ps(i,j)*slg(k)
             end if
              pw  = onep3*pw
! maximum vapor pressure 5% of atmospheric pressure
              esmax=0.05*pw

              q  = qsat(i,j,k)
              if (q.lt.zero) q=zero

              tdry = temp(i,j,k)/(one+fv*q)
              tr = ttp/tdry
              if (tdry >= ttp) then
                 es = psat * (tr**xa) * exp(xb*(one-tr))
              elseif (tdry < tmix) then
                 es = psat * (tr**xai) * exp(xbi*(one-tr))
              else
                 w  = (tdry - tmix) / (ttp - tmix)
                 es =  w * psat * (tr**xa) * exp(xb*(one-tr)) &
                      + (one-w) * psat * (tr**xai) * exp(xbi*(one-tr))
              endif

              es = min(es,esmax)
              qs = eps * es / (pw - omeps * es)

              if (qs.lt.qsat(i,j,k)) then
                 tdry = temp(i,j,k)/(one+fv*qs)
                 tr = ttp/tdry
                 if (tdry >= ttp) then
                    es = psat * (tr**xa) * exp(xb*(one-tr))
                 elseif (tdry < tmix) then
                    es = psat * (tr**xai) * exp(xbi*(one-tr))
                 else
                    w  = (tdry - tmix) / (ttp - tmix)
                    es =  w * psat * (tr**xa) * exp(xb*(one-tr)) &
                         + (one-w) * psat * (tr**xai) * exp(xbi*(one-tr))
                 endif

                 es = min(es,esmax)
                 qs = eps * es / (pw - omeps * es)

                 tdry = temp(i,j,k)/(one+fv*qs)
                 tr = ttp/tdry
                 if (tdry >= ttp) then
                    es = psat * (tr**xa) * exp(xb*(one-tr))
                 elseif (tdry < tmix) then
                    es = psat * (tr**xai) * exp(xbi*(one-tr))
                 else
                    w  = (tdry - tmix) / (ttp - tmix)
                    es =  w * psat * (tr**xa) * exp(xb*(one-tr)) &
                         + (one-w) * psat * (tr**xai) * exp(xbi*(one-tr))
                 endif
                 es = min(es,esmax)
                 qs = eps * es / (pw - omeps * es)

              end if

              qsat(i,j,k) = qs

           end do
        end do
     end do

!
!     Compute saturation values with respect to water surface
  else
     do k = 1,nsig
        do j = 1,lon2
           do i = 1,lat2

             if (hybrid) then
               pw = 0.5*(a5(k)+a5(k+1)+(b5(k)+b5(k+1))* &
                          ps(i,j))
             else
               pw = ps(i,j)*slg(k)
             end if

              pw  = onep3*pw
! maximum vapor pressure 5% of atmospheric pressure
              esmax=0.05*pw

              q  = qsat(i,j,k)
              if (q.lt.zero) q=zero

              tdry = temp(i,j,k)/(one+fv*q)
              tr = ttp/tdry
              es = psat * (tr**xa) * exp(xb*(one-tr))
              es = min(es,esmax)
              qs = eps * es / (pw - omeps * es)

              if (qs.lt.qsat(i,j,k)) then
                 tdry = temp(i,j,k)/(one+fv*qs)
                 tr = ttp/tdry
                 es = psat * (tr**xa) * exp(xb*(one-tr))
                 es = min(es,esmax)
                 qs = eps * es / (pw - omeps * es)
                 tdry = temp(i,j,k)/(one+fv*qs)
                 tr = ttp/tdry
                 es = psat * (tr**xa) * exp(xb*(one-tr))
                 es = min(es,esmax)
                 qs = eps * es / (pw - omeps * es)

              end if
              qsat(i,j,k) = qs


           end do
        end do
     end do

  endif

  return
  end subroutine genqsat

