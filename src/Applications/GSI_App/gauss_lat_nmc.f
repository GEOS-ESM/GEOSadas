      subroutine gauss_lat_nmc(gaul,k)
      implicit double precision (a-h,o-z)
      dimension a(500)
      real gaul(1)
      save
      esp=1.d-14
      c=(1.d0-(2.d0/3.14159265358979d0)**2)*0.25d0
      fk=k
      kk=k/2
      call bsslz1(a,kk)
      do 30 is=1,kk
      xz=cos(a(is)/sqrt((fk+0.5d0)**2+c))
      iter=0
   10 pkm2=1.d0
      pkm1=xz
      iter=iter+1
      if(iter.gt.10) go to 70
      do 20 n=2,k
      fn=n
      pk=((2.d0*fn-1.d0)*xz*pkm1-(fn-1.d0)*pkm2)/fn
      pkm2=pkm1
   20 pkm1=pk
      pkm1=pkm2
      pkmrk=(fk*(pkm1-xz*pk))/(1.d0-xz**2)
      sp=pk/pkmrk
      xz=xz-sp
      avsp=abs(sp)
      if(avsp.gt.esp) go to 10
      a(is)=xz
   30 continue
      if(k.eq.kk*2) go to 50
      a(kk+1)=0.d0
      pk=2.d0/fk**2
      do 40 n=2,k,2
      fn=n
   40 pk=pk*fn**2/(fn-1.d0)**2
   50 continue
      do 60 n=1,kk
      l=k+1-n
      a(l)=-a(n)
   60 continue
      radi=180./(4.*atan(1.))
      do 211 n=1,k
      gaul(n)=acos(a(n))*radi-90.0
  211 continue
      return
   70 write(6,6000)
 6000 format(//5x,14herror in gauaw//)
      stop
      end

      subroutine bsslz1(bes,n)
      implicit double precision (a-h,o-z)
      dimension bes(n)
      dimension bz(50)
      data pi/3.14159265358979d0/
      data bz  / 2.4048255577d0, 5.5200781103d0,
     $  8.6537279129d0,11.7915344391d0,14.9309177086d0,18.0710639679d0,
     $ 21.2116366299d0,24.3524715308d0,27.4934791320d0,30.6346064684d0,
     $ 33.7758202136d0,36.9170983537d0,40.0584257646d0,43.1997917132d0,
     $ 46.3411883717d0,49.4826098974d0,52.6240518411d0,55.7655107550d0,
     $ 58.9069839261d0,62.0484691902d0,65.1899648002d0,68.3314693299d0,
     $ 71.4729816036d0,74.6145006437d0,77.7560256304d0,80.8975558711d0,
     $ 84.0390907769d0,87.1806298436d0,90.3221726372d0,93.4637187819d0,
     $ 96.6052679510d0,99.7468198587d0,102.888374254d0,106.029930916d0,
     $ 109.171489649d0,112.313050280d0,115.454612653d0,118.596176630d0,
     $ 121.737742088d0,124.879308913d0,128.020877005d0,131.162446275d0,
     $ 134.304016638d0,137.445588020d0,140.587160352d0,143.728733573d0,
     $ 146.870307625d0,150.011882457d0,153.153458019d0,156.295034268d0/
      nn=n
      if(n.le.50) go to 12
      bes(50)=bz(50)
      do 5 j=51,n
    5 bes(j)=bes(j-1)+pi
      nn=49
   12 do 15 j=1,nn
   15 bes(j)=bz(j)
      return
      end
