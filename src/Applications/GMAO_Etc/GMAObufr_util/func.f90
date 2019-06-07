 real function avh_brig(r,l,ier)
!BUT/PURPOSE 
! convert an avhrr ir channel l radiance into brightness temperature
! using band correction coefficients and central wave numbers
! already read from avhrr calibration coefficient file 
! and stored into cavh_cpi.h 
!
! (another way to develop later is to use 1B avhrr header variables  
! from  avh_h_radtempcnv array)
! 
!DESCRIPTION 
!     FORTRAN 77 with UKMO/METEO-FRANCE accepted extensions   
!  
!PROPRIETAIRE/OWNER
!  Marcel Derrien
!
!HISTORIQUE/HISTORY (MODIFICATIONS/CHANGES) 
!(version   date      comment                 <author>)
!  1.0      Nov. 95   initial                 Marcel Derrien
!      
!ARGUMENTS 
!     input real r
!     input integer l ir channel order
!                  1 for channle 3B
!                  2 for channel 4
!                  3 for channel 5  
!     output real brightness temperature
!     output ier exit status 0 =ok
!FUNCTIONS AND SUBROUTINES CALLS  (RETURN CODES FOR FUNCTIONS)
!      
!         
! 
!COPYRIGHT (c) Meteo-France (EVOLUNIX - ATOVS - EUMETSAT project <1994>)
!
!
!DECLARATIONS
!
      implicit none                                                     
! 
! identification variable for "what"
      character*62 sccsid 
!    
      data sccsid/'@(#) avh_brig.F version 1.1 on 3/24/98'/ 
!
!arguments
      real r
      integer l
      integer ier
!commons
      include 'cavh_cpi.inc'
!local variables
      real alge,expn,tt
      integer k
!functions
      intrinsic alog
!-
      ier = 1
      k = l 
      avh_brig = 0.0
      
      if (k.ge.1 .or. k.le.3) then
        if(r.lt.0.) r = 1.e-12
        expn = planck1(k)/r + 1.
        if (expn.ge.1.) then
          alge = alog(expn)
          if (alge.ge.1.) then 
            tt = planck2(k)/alge
!
! Xu Li: 11/03/2005: band correction Tb = (T* - A) / B
! T* is the equavilent black body temperature (brightness temperature) calculated by plank function 
! Tb is the BT after band correction
! How to apply the band corretion depends on the definition of A & B!
! After comparing A & B read from GAC 1b data set with those on NOAA web site, Dr. Chao pointed out
! that the band correction should be done (with the read A & B) as follows:
!           Tb = A + B x T*
!
!           avh_brig = (tt-avh_bandcor(2 ,k))/avh_bandcor(3,k)

            avh_brig = avh_bandcor(2 ,k) + avh_bandcor(3,k)*tt

            ier = 0
          endif
        endif
      endif
 end function avh_brig

 real function avh_cir_r(r,c)
!BUT/PURPOSE 
! returns the  Kelvin corresponding to the radiance r from avhrr
! channel c
!
!DESCRIPTION 
!     FORTRAN 77 with UKMO/METEO-FRANCE accepted extensions   
!  
!PROPRIETAIRE/OWNER
!  Marcel Derrien
!
!HISTORIQUE/HISTORY (MODIFICATIONS/CHANGES) 
!(version   date      comment                 <author>)
!  1.0      Nov. 95   initial                 Marcel Derrien
!      
!ARGUMENTS 
!     input real r
!     input integer c ir channel order
!                  1 for channle 3B
!                  2 for channel 4
!                  3 for channel 5  
!     output ier exit status 0 =ok
!
!
!FUNCTIONS AND SUBROUTINES CALLS  (RETURN CODES FOR FUNCTIONS)
!
!COPYRIGHT (c) Meteo-France (EVOLUNIX - ATOVS - EUMETSAT project <1994>)
!
!DECLARATIONS
!
        implicit none                                                     
! 
! identification variable for "what"
        character*62 sccsid 
!    
        data sccsid/'@(#) avh_cir_r.F version 1.1 on 3/24/98'/
! arguments
	real r
 	integer c
!commons
         real rbeg(3),rstep(3),rend(3)
         real tconv(4000,3)
         common/temp_conv/rbeg,rstep,rend,tconv
! local variables
         integer k 
!functions
!-
         k = int( ( r - rbeg(c) ) / rstep(c) ) + 1
!-saturations ?
         if(k.lt.1) then
            avh_cir_r = tconv(1,c)
         else if(k.ge.4000) then
            avh_cir_r = tconv(4000,c)
         else
!
! interpolation between k and k+1
!
            avh_cir_r = tconv(k,c) + ( tconv(k+1,c) -tconv(k,c) ) * &                   
                 ( r - ( rbeg(c) + (k-1)*rstep(c)) ) / rstep(c)
         endif

 end function avh_cir_r


