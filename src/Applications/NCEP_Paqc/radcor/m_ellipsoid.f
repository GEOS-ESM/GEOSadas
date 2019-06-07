!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_ellipsoid -- Routines for solving geodetic problems on an ellipsoid
!
! !DESCRIPTION:
!     This module contains routines that solve the direct and indirect
!     problems in geodesy on an ellipsoid according to the formulae 
!     described in Vincenty (1975).  The direct problem is the 
!     determination of the latitude and longitude and the reverse azimuth
!     angle given the latitude and longitude at another point, the azimuth
!     angle and angular ellipsoidal distance (with respect to the polar
!     radius).  The inverse problem is the determination of the angular
!     ellipsoidal distance and forward and reverse azimuth angles given
!     the latitude and longitude of two points.  According to Vincenty
!     (1975), "the inverse formulae may give no solution over a line 
!     between two nearly antipodal points."
!
! !REFERENCES:
!     Vincenty, T., 1975: Direct and Inverse Solutions of Geodesics on the 
!          Ellipsoid with Application of Nested Equations.  Survey Review,
!          176, 88-93.
!
! !INTERFACE:
!
      module      m_ellipsoid
      implicit    NONE
      private        ! except

      public ::
     .   PI,         ! pi (in double precision )
     .   Deg2Rad,    ! Factor for convert degrees to radians
     .   REarthEq,   ! Equatorial radius (m, in double precision)
     .   REarthPole, ! Polar radius (m, in double precision)
     .   REarthMean, ! Mean earth radius (m, in double precision)
     .   Direct,     ! Solve the direct problem
     .   Inverse,    ! ... and the inverse problem
     .   InvSph,     ! Solve the inverse problem
     .   DirSph      !  ... and the inverse problem on a sphere

! !REVISION HISTORY:
!     09Nov2001  C. Redder  Original code
!
      interface Direct
         module procedure
     .      Direct_LP,   ! for arguments in low (single) precision
     .      Direct_HP    ! ... and high (double) precision
      end interface
      interface Inverse
         module procedure
     .      Inverse_LP,  ! for arguments in low (single) precision
     .      Inverse_HP   ! ... and high (double) precision
      end interface
!EOP
!-----------------------------------------------------------------

      integer,     parameter ::
     .   LP   = selected_real_kind (6),
     .   HP   = selected_real_kind (12),
     .   MaxP = HP

      real (MaxP), parameter ::
     .   PI         = 3.141592653589793238462643383279502884_MaxP,
     .   flMax      = 0.99_MaxP,        ! Maximum flattening factor
     .   REarthEq   = 6378137.000_MaxP, ! Earth's equatorial ...
     .   REarthPole = 6356752.314_MaxP, ! ... polar ...
     .   REarthMean = ( REarthEq + REarthPole ) / 2.0_MaxP,
     .                                  ! ... and mean radius (m) 
     .   flEarth    = ( REarthEq - REarthPole ) / REarthEq,
     .                                  ! and its flattening factor
     .   Deg2Rad    = PI / 180.0_MaxP   ! Factor for converting degrees to
                                        !   to radians. 
      integer,     parameter ::
     .   Valid      = 0,                ! Valid return status
     .   NIter_max  = 200               ! Maximum number of iterations
      contains

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Direct_LP - Solve the direct problem with low precision arguments
!
! !DESCRIPTION: 
!     This routine solves the direct problem in geodesy.  All input and
!     output arguments are in low (single) precision floating point numbers.
!
! !INTERFACE:
!
      pure subroutine Direct_LP ( lat1, lon1, dist, azim12,
     .                            lat2, lon2,       azim21, fl )
      implicit   NONE
!
! !INPUT PARAMETERS:
      real (LP),     intent (in)  ::
     .   lat1   (:), ! geodetic latitude (deg, -90 = 90S)
     .   lon1   (:), ! ... longitude     (deg, -90 = 90W) for each point
     .   dist   (:), ! angular ellipsoidal distance, arc / b, (radians) where
                     !   arc is the arc distance and b is the minor semi-axis
     .   azim12 (:)  ! azimuth angle on ellipsoid (deg, 90 = due east)
      real (LP),     intent (in), optional ::
     .   fl          ! flatness (a-b)/a where a and b are the major and minor
                     !   semi-axis.  Range: [0,0.99].  Default: Earth's value
! !OUTPUT PARAMETERS: 
      real (LP),     intent (out) ::
     .   lat2   (:), ! latitude (deg)
     .   lon2   (:), ! ... longitude for each second point (deg)
     .   azim21 (:)  ! reverse azimuth angle (deg).  Range: (0,360]
!
!     Note: The number of points is determined by the size of the arrays.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     12Nov2001  C. Redder  Original code.
!     18Apr2002  C. Redder  Modified the way the number of points are
!                           determined.
!
! EOP
!-------------------------------------------------------------------------

      real (LP) :: Deg2R, l1, l2, f1, f2, a12, a21, f, tol, SPI
      real (LP) :: aa, bb,  cc, h,  x, y, xsq, usq, dis, fm1, fMax
      real (LP) :: sina12, cosa12, sig2m, coss2m
      real (LP) :: sinU1,  cosU1,  sina,  cosa
      real (LP) :: sig,    sig1,   dsig,  dsigLast, sins, coss
      logical   :: converged
      integer   :: iPt, NPts, Iter
      real (LP), parameter :: reltol = 10.0_LP ** ( -precision ( tol ))

      fMax   = flMax
      f      = flEarth                     ! flatness of an ellipsoid
      if ( present ( fl )) f = min ( fMax, abs ( fl ))
      fm1    = 1.0_LP - f

      SPI     = PI
      Deg2R   = Deg2Rad
      NPts    = min ( size ( lat1 ), size ( lon1 ), size ( azim12 ),
     .                size ( dist ),
     .                size ( lat2 ), size ( lon2 ), size ( azim21 ))  
      do iPt  = 1, NPts
         f1   = lat1   (iPt) * Deg2R       ! Convert the latitude
         l1   = lon1   (iPt) * Deg2R       ! ... longitude of the first point
         a12  = azim12 (iPt) * Deg2R       ! ... and direction angle to radians
                                           ! Compute the trignometric
                                           !   functions  of the ...
         sina12 = sin ( a12 )              ! ... direction angle
         cosa12 = cos ( a12 )
         y      = sin ( f1 ) * fm1         ! ... reduced latitude, U1,
         x      = cos ( f1 )               !   where tan (U1) = (1-f) tan (f1)
         h      = sqrt ( x ** 2 + y ** 2 )
         sinU1  = y / h
         cosU1  = x / h
         sina   = cosU1 * sina12           ! ... azimuth of the geodesic
         cosa   = sqrt ( 1.0_LP            !   ... at the equator 
     .                 - sina ** 2 )

         sig1   = atan2 ( sinU1, cosU1 * cosa12 )
         usq    = ( 1.0_LP / fm1 ** 2 - 1.0_LP ) * cosa ** 2
         aa     = 1.0_LP + usq * ( 4096.0_LP
     .                   - usq * (  768.0_LP
     .                   - usq * (  320.0_LP - 175.0_LP * usq )))
     .                   / 16384.0_LP
         bb     =          usq * (  256.0_LP
     .                   - usq * (  128.0_LP
     .                   - usq * (   74.0_LP -  47.0_LP * usq )))
     .                   / 1024.0_LP

         dis    = dist (iPt)
         sig    = dis    / aa
         tol    = reltol / aa
         dsigLast  = 0.0_LP
         converged = .false.

         do Iter   = 1, NIter_max          ! Interatively adjust the
            sig2m  = sig1 + sig1 + sig     !   distance between the points
            coss2m = cos ( sig2m )         !   and their trignometric
            sins   = sin ( sig   )         !   functions.
            coss   = cos ( sig   )
            if ( converged ) exit          ! ... until iteration converges
            dsig   = bb * sins * ( coss2m  !   to within machine round-off
     .                  + ( bb / 4.0_LP ) * ( coss
     .                    * ( -1.0_LP + 2.0_LP * coss2m ** 2 )
     .                  - ( bb / 6.0_LP ) *   coss2m
     .                    * ( -3.0_LP + 4.0_LP * sins   ** 2 )
     .                    * ( -3.0_LP + 4.0_LP * coss2m ** 2 )))
            sig       = dis / aa + dsig
            converged = abs ( dsigLast - dsig ) .le. tol ! Test for
            dsigLast  = dsig                             ! convergence
         end do
                                           ! Compute (in radians) ...
         y   = sinU1 * coss                ! ... the latitude
     .       + cosU1 * sins * cosa12
         xsq = sina ** 2 + ( sinU1 * sins - cosU1 * coss * cosa12 ) ** 2
         f2  = atan2 ( y, fm1 * sqrt ( xsq ) )

         y   = sins  * sina12              ! ... and longitude for the
         x   = cosU1 * coss - sinU1 * sins * cosa12     ! second point
         l2  = atan2 ( y, x ) + l1
         cc  = ( f / 16.0_LP ) * cosa ** 2
     .                        * ( 4.0_LP + f
     .                        * ( 4.0_LP - 3.0_LP * cosa ** 2 ))
         l2  = l2 - ( 1.0_LP - cc ) * f * sina * ( sig
     .       + cc * sins * ( coss2m 
     .       + cc * coss * ( -1.0_LP + 2.0_LP * coss2m ** 2 )))

         y    = sina                       ! ... and the reverse angle
         x    = cosU1 * coss * cosa12 - sinU1 * sins
         a21  = atan2 ( y, x )

         lat2   (iPt) =  f2  / Deg2R       ! Convert output to degrees
         lon2   (iPt) =  l2  / Deg2R  
         azim21 (iPt) = modulo ( a21 / Deg2R, 360.0_LP )
         if ( azim21(iPt) .le. 0.0_LP ) azim21(iPt) = 360.0_LP

      end do

      return
      end subroutine Direct_LP
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Direct_HP - Solve the direct problem with hight precision arguments
!
! !DESCRIPTION: 
!     This routine solves the direct problem in geodesy.  All input and
!     output arguments are in hight (double) precision floating point numbers.
!
! !INTERFACE:
!
      pure subroutine Direct_HP ( lat1, lon1, dist, azim12,
     .                            lat2, lon2,       azim21, fl )
      implicit   NONE
!
! !INPUT PARAMETERS:
      real (HP),     intent (in)  ::
     .   lat1   (:), ! geodetic latitude (deg, -90 = 90S)
     .   lon1   (:), ! ... longitude     (deg, -90 = 90W) for each point
     .   dist   (:), ! angular ellipsoidal distance, arc / b, (radians) where
                     !   arc is the arc distance and b is the minor semi-axis
     .   azim12 (:)  ! azimuth angle on ellipsoid (deg, 90 = due east)
      real (HP),     intent (in), optional ::
     .   fl          ! flatness (a-b)/a where a and b are the major and minor
                     !   semi-axis.  Range: [0,0.99].  Default: Earth's value
! !OUTPUT PARAMETERS: 
      real (HP),     intent (out) ::
     .   lat2   (:), ! latitude (deg)
     .   lon2   (:), ! ... longitude for each second point (deg)
     .   azim21 (:)  ! reverse azimuth angle (deg).  Range: (0,360]
!
!     Note: The number of points is determined by the size of the arrays.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     12Nov2001  C. Redder  Original code.
!     18Apr2002  C. Redder  Modified the way the number of points are
!                           determined.
!
! EOP
!-------------------------------------------------------------------------

      real (HP) :: Deg2R, l1, l2, f1, f2, a12, a21, f, tol, SPI
      real (HP) :: aa, bb,  cc, h,  x, y, xsq, usq, dis, fm1, fMax
      real (HP) :: sina12, cosa12, sig2m, coss2m
      real (HP) :: sinU1,  cosU1,  sina,  cosa
      real (HP) :: sig,    sig1,   dsig,  dsigLast, sins, coss
      logical   :: converged
      integer   :: iPt, NPts, Iter
      real (HP), parameter :: reltol = 10.0_HP ** ( -precision ( tol ))

      fMax   = flMax
      f      = flEarth                     ! flatness of an ellipsoid
      if ( present ( fl )) f = min ( fMax, abs ( fl ))
      fm1    = 1.0_HP - f

      SPI     = PI
      Deg2R   = Deg2Rad
      NPts    = min ( size ( lat1 ), size ( lon1 ), size ( azim12 ),
     .                size ( dist ),
     .                size ( lat2 ), size ( lon2 ), size ( azim21 ))  
      do iPt  = 1, NPts
         f1   = lat1   (iPt) * Deg2R       ! Convert the latitude
         l1   = lon1   (iPt) * Deg2R       ! ... longitude of the first point
         a12  = azim12 (iPt) * Deg2R       ! ... and direction angle to radians
                                           ! Compute the trignometric
                                           !   functions  of the ...
         sina12 = sin ( a12 )              ! ... direction angle
         cosa12 = cos ( a12 )
         y      = sin ( f1 ) * fm1         ! ... reduced latitude, U1,
         x      = cos ( f1 )               !   where tan (U1) = (1-f) tan (f1)
         h      = sqrt ( x ** 2 + y ** 2 )
         sinU1  = y / h
         cosU1  = x / h
         sina   = cosU1 * sina12           ! ... azimuth of the geodesic
         cosa   = sqrt ( 1.0_HP            !   ... at the equator 
     .                 - sina ** 2 )

         sig1   = atan2 ( sinU1, cosU1 * cosa12 )
         usq    = ( 1.0_HP / fm1 ** 2 - 1.0_HP ) * cosa ** 2
         aa     = 1.0_HP + usq * ( 4096.0_HP
     .                   - usq * (  768.0_HP
     .                   - usq * (  320.0_HP - 175.0_HP * usq )))
     .                   / 16384.0_HP
         bb     =          usq * (  256.0_HP
     .                   - usq * (  128.0_HP
     .                   - usq * (   74.0_HP -  47.0_HP * usq )))
     .                   / 1024.0_HP

         dis    = dist (iPt)
         sig    = dis    / aa
         tol    = reltol / aa
         dsigLast  = 0.0_HP
         converged = .false.

         do Iter   = 1, NIter_max          ! Interatively adjust the
            sig2m  = sig1 + sig1 + sig     !   distance between the points
            coss2m = cos ( sig2m )         !   and their trignometric
            sins   = sin ( sig   )         !   functions.
            coss   = cos ( sig   )
            if ( converged ) exit          ! ... until iteration converges
            dsig   = bb * sins * ( coss2m  !   to within machine round-off
     .                  + ( bb / 4.0_HP ) * ( coss
     .                    * ( -1.0_HP + 2.0_HP * coss2m ** 2 )
     .                  - ( bb / 6.0_HP ) *   coss2m
     .                    * ( -3.0_HP + 4.0_HP * sins   ** 2 )
     .                    * ( -3.0_HP + 4.0_HP * coss2m ** 2 )))
            sig       = dis / aa + dsig
            converged = abs ( dsigLast - dsig ) .le. tol ! Test for
            dsigLast  = dsig                             ! convergence
         end do
                                           ! Compute (in radians) ...
         y   = sinU1 * coss                ! ... the latitude
     .       + cosU1 * sins * cosa12
         xsq = sina ** 2 + ( sinU1 * sins - cosU1 * coss * cosa12 ) ** 2
         f2  = atan2 ( y, fm1 * sqrt ( xsq ) )

         y   = sins  * sina12              ! ... and longitude for the
         x   = cosU1 * coss - sinU1 * sins * cosa12     ! second point
         l2  = atan2 ( y, x ) + l1
         cc  = ( f / 16.0_HP ) * cosa ** 2
     .                        * ( 4.0_HP + f
     .                        * ( 4.0_HP - 3.0_HP * cosa ** 2 ))
         l2  = l2 - ( 1.0_HP - cc ) * f * sina * ( sig
     .       + cc * sins * ( coss2m 
     .       + cc * coss * ( -1.0_HP + 2.0_HP * coss2m ** 2 )))

         y    = sina                       ! ... and the reverse angle
         x    = cosU1 * coss * cosa12 - sinU1 * sins
         a21  = atan2 ( y, x )

         lat2   (iPt) =  f2  / Deg2R       ! Convert output to degrees
         lon2   (iPt) =  l2  / Deg2R  
         azim21 (iPt) = modulo ( a21 / Deg2R, 360.0_HP )
         if ( azim21(iPt) .le. 0.0_HP ) azim21(iPt) = 360.0_HP

      end do

      return
      end subroutine Direct_HP
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Inverse_LP - Solve the inverse problem with low precision arguments
!
! !DESCRIPTION: 
!     This routine solves the inverse problem in geodesy.  All input and
!     output arguments are in low (single) precision floating point numbers.
!
! !INTERFACE:
!
      pure subroutine Inverse_LP ( lat1, lon1,   lat2,   lon2,
     .                             dist, azim12, azim21, stat, fl )
      implicit   NONE
!
! !INPUT PARAMETERS:
      real (LP),     intent (in)  ::
     .   lat1   (:), ! geodetic latitude (deg, -90 = 90S)
     .   lon1   (:), ! ... longitude     (deg, -90 = 90W) for each point
     .   lat2   (:), ! latitude (deg)
     .   lon2   (:)  ! ... longitude (deg) for each second point
      real (LP),     intent (in), optional ::
     .   fl          ! flatness (a-b)/a where a and b are the major and minor
                     !   semi-axis.  Range: [0,0.99].  Default: Earth's value
! !OUTPUT PARAMETERS: 
      real (LP),     intent (out) ::
     .   dist   (:), ! angular ellipsoidal distance, arc / b, (radians) where
     .               !   arc is the arc distance and b is the minor semi-axis
     .   azim12 (:), ! azimuth angle (deg, 90 = due east) and ...
     .   azim21 (:)  ! ... reverse azimuth angle on the ellipsoid (deg).
                     !   Range: (0,360]
      integer,       intent (out), optional ::
     .   stat   (:)  ! = 0 if algorithm converged properly.  If > 0, then
                     !   stat = number of iterations without converging to a
                     !   a solution (at least to within machine round-off).
!
!     Note: The number of points is determined by the size of the arrays.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!       Jan1976  INGE NESBO  Original code, originally called INVINC
!     12Nov2001  C. Redder   Adapted the routine and renamed it
!     18Apr2002  C. Redder   Modified the way the number of points are
!                            determined.
!
! EOP
!-------------------------------------------------------------------------

      real (LP) :: Deg2R, l1, l2, f1, f2, a12, a21, f, tol, SPI
      real (LP) :: h, x, y, fm1, fMax
      real (LP) :: sina12, cosa12
      real (LP) :: sinU1,  cosU1, sinU2, cosU2, sina, cossqa
      real (LP) :: dl, sindl, cosdl, dl0, ddl, ddlLast
      real (LP) :: aa, bb, cc, dd, ee, ff, usq
      real (LP) :: sig, dsig, sins, coss, coss2m
      logical   :: converged
      integer   :: iPt, NPts, Iter, iret
      real (LP), parameter :: reltol = 10.0_LP ** ( -precision ( tol ))

      fMax    = flMax
      f       = flEarth                    ! flatness of an ellipsoid
      if ( present ( fl )) f = min ( fMax, abs ( fl ))
      fm1     = 1.0_LP - f

      SPI     = PI
      Deg2R   = Deg2Rad
      NPts    = min ( size ( lat1 ), size ( lon1   ),
     .                size ( lat2 ), size ( lon2   ),
     .                size ( dist ), size ( azim12 ), size ( azim21 ))  
      OuterLoop: do iPt  = 1, NPts
         f1   = lat1 (iPt) * Deg2R         ! Convert the latitude
         l1   = lon1 (iPt) * Deg2R         ! ... longitude of the first point
         f2   = lat2 (iPt) * Deg2R         ! ... and of the second point
         l2   = lon2 (iPt) * Deg2R  
                                           ! Compute the trignometric
                                           !   functions  of the ...
         y      = sin ( f1 ) * fm1         ! ... reduced latitude, U1,
         x      = cos ( f1 )               !   where tan (U1) = (1-f) tan (f1)
         h      = sqrt ( x ** 2 + y ** 2 ) !   for the first point
         sinU1  = y / h
         cosU1  = x / h
         y      = sin ( f2 ) * fm1         ! ... and for the second point
         x      = cos ( f2 )
         h      = sqrt ( x ** 2 + y ** 2 )
         sinU2  = y / h
         cosU2  = x / h

         dl     = l2 - l1
         dl0    = dl
         tol    = reltol
         ddl    = 0.0_LP
         converged = .false.

         do Iter = 1, NIter_max            ! Interatively determine the
            ddlLast = ddl                  !   adjusted difference of the
            sindl   = sin ( dl )           !   longitudes.
            cosdl   = cos ( dl )
            sins    = sqrt ( ( cosU2 * sindl ) ** 2
     .                     + ( cosU1 * sinU2
     .                       - sinU1 * cosU2 * cosdl ) ** 2 )
            if ( sins .le. tol ) then      ! If the points are identical ...
               azim12 (iPt) = 0.0_LP       ! ... then set the output 
               azim21 (iPt) = 0.0_LP       !   arguments to zero and ...
               dist   (iPt) = 0.0_LP
               if ( present ( stat ) ) stat (iPt) = Valid
               cycle OuterLoop             ! ... go to next pair of points
            end if

            coss    = sinU1 * sinU2 + cosU1 * cosU2 * cosdl
            sig     = atan2 ( sins, coss )
            sina    = cosU1 * cosU2 * sindl / sins
            cossqa  = 1.0_LP - sina * sina
            coss2m  = coss
            if ( cossqa .gt. tol )
     .         coss2m = coss2m - 2.0_LP * sinU1 * sinU2 / cossqa
            cc      = ( f * cossqa / 16.0_LP ) 
     .              * ( 4.0_LP + f * ( 4.0_LP - 3.0_LP * cossqa ))
            ddl     = ( 1.0_LP - cc ) * f * sina
     .              * ( sig    + cc * sins
     .              * ( coss2m + cc * coss
     .              * ( -1.0_LP + 2.0_LP * coss2m * coss2m )))

            dl = dl0 + ddl
            converged = abs ( ddl - ddlLast ) .le. tol
            if ( converged ) exit          ! If convergence criteria is
                                           !   statisfied, then exit loop.
         end do
         sindl   = sin ( dl )
         cosdl   = cos ( dl )

                                           ! Now compute angular distance
         usq    = ( 1.0_LP / fm1 ** 2 - 1.0_LP ) * cossqa
         aa     = 1.0_LP + usq * ( 4096.0_LP
     .                   - usq * (  768.0_LP
     .                   - usq * (  320.0_LP - 175.0_LP * usq )))
     .                   / 16384.0_LP
         bb     =          usq * (  256.0_LP
     .                   - usq * (  128.0_LP
     .                   - usq * (   74.0_LP -  47.0_LP * usq )))
     .                   / 1024.0_LP
         dsig =  bb * sins * ( coss2m  + bb *  ( coss
     .                     * ( -1.0_LP + 2.0_LP * coss2m * coss2m )
     .                     - bb * coss2m
     .                     * ( -3.0_LP + 4.0_LP *   sins *   sins )
     .                     * ( -3.0_LP + 4.0_LP * coss2m * coss2m )
     .                     / 6.0_LP ) / 24.0_LP )
         sig  =  aa * ( sig - dsig )

         y    =  cosU2 * sindl             ! ... azimuth angle
         x    =  cosU1 * sinU2 - sinU1 * cosU2 * cosdl
         a12  =  atan2 ( y, x )

         y    =  cosU1 * sindl             ! ... and the reverse angle
         x    = -sinU1 * cosU2 + cosU1 * sinU2 * cosdl
         a21  =  atan2 ( y, x )

         iret = NIter_max                  ! ... if desired set the
         if ( converged ) iret = Valid     !     covergence status code.

         dist   (iPt) = sig                ! Convert angles to degrees 
         azim12 (iPt) = modulo ( a12 / Deg2R, 360.0_LP )
         if ( azim12(iPt) .le. 0.0 ) azim12(iPt) = 360.0_LP
         azim21 (iPt) = modulo ( a21 / Deg2R, 360.0_LP )
         if ( azim21(iPt) .le. 0.0 ) azim21(iPt) = 360.0_LP
         if ( present ( stat ) ) stat (iPt) = iret

      end do OuterLoop

      return
      end subroutine Inverse_LP

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Inverse_LP - Solve the inverse problem with high precision arguments
!
! !DESCRIPTION: 
!     This routine solves the inverse problem in geodesy.  All input and
!     output arguments are in high (double) precision floating point numbers.
!
! !INTERFACE:
!
      pure subroutine Inverse_HP ( lat1, lon1,   lat2,   lon2,
     .                             dist, azim12, azim21, stat, fl )
      implicit   NONE
!
! !INPUT PARAMETERS:
      real (HP),     intent (in)  ::
     .   lat1   (:), ! geodetic latitude (deg, -90 = 90S)
     .   lon1   (:), ! ... longitude     (deg, -90 = 90W) for each point
     .   lat2   (:), ! latitude (deg)
     .   lon2   (:)  ! ... longitude (deg) for each second point
      real (HP),     intent (in), optional ::
     .   fl          ! flatness (a-b)/a where a and b are the major and minor
                     !   semi-axis.  Range: [0,0.99].  Default: Earth's value
! !OUTPUT PARAMETERS: 
      real (HP),     intent (out) ::
     .   dist   (:), ! angular ellipsoidal distance, arc / b, (radians) where
     .               !   arc is the arc distance and b is the minor semi-axis
     .   azim12 (:), ! azimuth angle (deg, 90 = due east) and ...
     .   azim21 (:)  ! ... reverse azimuth angle on the ellipsoid (deg).
                     !   Range: (0,360]
      integer,       intent (out), optional ::
     .   stat   (:)  ! = 0 if algorithm converged properly.  If > 0, then
                     !   stat = number of iterations without converging to a
                     !   a solution (at least to within machine round-off).
!
!     Note: The number of points is determined by the size of the arrays.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!       Jan1976  INGE NESBO  Original code, originally called INVINC
!     12Nov2001  C. Redder   Adapted the routine and renamed it
!     18Apr2002  C. Redder   Modified the way the number of points are
!                            determined.
! EOP
!-------------------------------------------------------------------------

      real (HP) :: Deg2R, l1, l2, f1, f2, a12, a21, f, tol, SPI
      real (HP) :: h, x, y, fm1, fMax
      real (HP) :: sina12, cosa12
      real (HP) :: sinU1,  cosU1, sinU2, cosU2, sina, cossqa
      real (HP) :: dl, sindl, cosdl, dl0, ddl, ddlLast
      real (HP) :: aa, bb, cc, dd, ee, ff, usq
      real (HP) :: sig, dsig, sins, coss, coss2m
      logical   :: converged
      integer   :: iPt, NPts, Iter, iret
      real (HP), parameter :: reltol = 10.0_HP ** ( -precision ( tol ))

      fMax    = flMax
      f       = flEarth                    ! flatness of an ellipsoid
      if ( present ( fl )) f = min ( fMax, abs ( fl ))
      fm1     = 1.0_HP - f

      SPI     = PI
      Deg2R   = Deg2Rad
      NPts    = min ( size ( lat1 ), size ( lon1   ),
     .                size ( lat2 ), size ( lon2   ),
     .                size ( dist ), size ( azim12 ), size ( azim21 ))  
      OuterLoop: do iPt  = 1, NPts
         f1   = lat1 (iPt) * Deg2R         ! Convert the latitude
         l1   = lon1 (iPt) * Deg2R         ! ... longitude of the first point
         f2   = lat2 (iPt) * Deg2R         ! ... and of the second point
         l2   = lon2 (iPt) * Deg2R  
                                           ! Compute the trignometric
                                           !   functions  of the ...
         y      = sin ( f1 ) * fm1         ! ... reduced latitude, U1,
         x      = cos ( f1 )               !   where tan (U1) = (1-f) tan (f1)
         h      = sqrt ( x ** 2 + y ** 2 ) !   for the first point
         sinU1  = y / h
         cosU1  = x / h
         y      = sin ( f2 ) * fm1         ! ... and for the second point
         x      = cos ( f2 )
         h      = sqrt ( x ** 2 + y ** 2 )
         sinU2  = y / h
         cosU2  = x / h

         dl     = l2 - l1
         dl0    = dl
         tol    = reltol
         ddl    = 0.0_HP
         converged = .false.

         do Iter = 1, NIter_max            ! Interatively determine the
            ddlLast = ddl                  !   adjusted difference of the
            sindl   = sin ( dl )           !   longitudes.
            cosdl   = cos ( dl )
            sins    = sqrt ( ( cosU2 * sindl ) ** 2
     .                     + ( cosU1 * sinU2
     .                       - sinU1 * cosU2 * cosdl ) ** 2 )
            if ( sins .le. tol ) then      ! If the points are identical ...
               azim12 (iPt) = 0.0_HP       ! ... then set the output 
               azim21 (iPt) = 0.0_HP       !   arguments to zero and ...
               dist   (iPt) = 0.0_HP
               if ( present ( stat ) ) stat (iPt) = Valid
               cycle OuterLoop             ! ... go to next pair of points
            end if

            coss    = sinU1 * sinU2 + cosU1 * cosU2 * cosdl
            sig     = atan2 ( sins, coss )
            sina    = cosU1 * cosU2 * sindl / sins
            cossqa  = 1.0_HP - sina * sina
            coss2m  = coss
            if ( cossqa .gt. tol )
     .         coss2m = coss2m - 2.0_HP * sinU1 * sinU2 / cossqa
            cc      = ( f * cossqa / 16.0_HP ) 
     .              * ( 4.0_HP + f * ( 4.0_HP - 3.0_HP * cossqa ))
            ddl     = ( 1.0_HP - cc ) * f * sina
     .              * ( sig    + cc * sins
     .              * ( coss2m + cc * coss
     .              * ( -1.0_HP + 2.0_HP * coss2m * coss2m )))

            dl = dl0 + ddl
            converged = abs ( ddl - ddlLast ) .le. tol
            if ( converged ) exit          ! If convergence criteria is
                                           !   statisfied, then exit loop.
         end do
         sindl   = sin ( dl )
         cosdl   = cos ( dl )

                                           ! Now compute angular distance
         usq    = ( 1.0_HP / fm1 ** 2 - 1.0_HP ) * cossqa
         aa     = 1.0_HP + usq * ( 4096.0_HP
     .                   - usq * (  768.0_HP
     .                   - usq * (  320.0_HP - 175.0_HP * usq )))
     .                   / 16384.0_HP
         bb     =          usq * (  256.0_HP
     .                   - usq * (  128.0_HP
     .                   - usq * (   74.0_HP -  47.0_HP * usq )))
     .                   / 1024.0_HP
         dsig =  bb * sins * ( coss2m  + bb *  ( coss
     .                     * ( -1.0_HP + 2.0_HP * coss2m * coss2m )
     .                     - bb * coss2m
     .                     * ( -3.0_HP + 4.0_HP *   sins *   sins )
     .                     * ( -3.0_HP + 4.0_HP * coss2m * coss2m )
     .                     / 6.0_HP ) / 24.0_HP )
         sig  =  aa * ( sig - dsig )

         y    =  cosU2 * sindl             ! ... azimuth angle
         x    =  cosU1 * sinU2 - sinU1 * cosU2 * cosdl
         a12  =  atan2 ( y, x )

         y    =  cosU1 * sindl             ! ... and the reverse angle
         x    = -sinU1 * cosU2 + cosU1 * sinU2 * cosdl
         a21  =  atan2 ( y, x )

         iret = NIter_max                  ! ... if desired set the
         if ( converged ) iret = Valid     !     covergence status code.

         dist   (iPt) = sig                ! Convert angles to degrees 
         azim12 (iPt) = modulo ( a12 / Deg2R, 360.0_HP )
         if ( azim12(iPt) .le. 0.0 ) azim12(iPt) = 360.0_HP
         azim21 (iPt) = modulo ( a21 / Deg2R, 360.0_HP )
         if ( azim21(iPt) .le. 0.0 ) azim21(iPt) = 360.0_HP
         if ( present ( stat ) ) stat (iPt) = iret

      end do OuterLoop

      return
      end subroutine Inverse_HP
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: DirSph - Solve the direct problem on a sphere
!
! !DESCRIPTION: 
!     This routine solves the direct problem in geodesy for a sphere.  The
!     algorithm is a special case of the formulae for an ellipsoid as
!     presented by Vincenty (1975).
!
! !INTERFACE:
!
      pure subroutine DirSph ( lat1, lon1, dist, azim12,
     .                         lat2, lon2,       azim21 )
      implicit   NONE
!
! !INPUT PARAMETERS:
      real,          intent (in)  ::
     .   lat1   (:), ! geodetic latitude (deg, -90 = 90S)
     .   lon1   (:), ! ... longitude     (deg, -90 = 90W) for each point
     .   dist   (:), ! angular ellipsoidal distance, arc / b, (radians) where
     .               !   arc is the arc distance and b the spherical radius
     .   azim12 (:)  ! azimuth angle on ellipsoid (deg, 90 = due east)
!
! !OUTPUT PARAMETERS: 
      real,          intent (out) ::
     .   lat2   (:), ! latitude (deg)
     .   lon2   (:), ! ... longitude for each second point (deg)
     .   azim21 (:)  ! reverse azimuth angle (deg).  Range: (0,360]
!
!     Note: The number of points is determined by the size of the arrays.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     15Nov2001  C. Redder   Original code.
!     18Apr2002  C. Redder   Modified the way the number of points are
!                            determined.
!     11Mar2007  C. Redder   Fixed bug for the case when points 1 and 2 
!                            are identical.
! EOP
!-------------------------------------------------------------------------

      real    :: Deg2R, l1, l2, f1, f2, a12, a21, SPI
      real    :: x, y, xsq, sig, sins, coss
      real    :: sina12, cosa12, sinU1, cosU1, sina, cosa
      integer :: iPt, NPts

      SPI     = PI
      Deg2R   = Deg2Rad
      NPts    = min ( size ( lat1 ), size ( lon1 ), size ( azim12 ),
     .                size ( dist ),
     .                size ( lat2 ), size ( lon2 ), size ( azim21 ))  
      do iPt  = 1, NPts                    ! Convert (to radians) ...
         f1     = lat1   (iPt) * Deg2R     ! ... the latitude
         l1     = lon1   (iPt) * Deg2R     ! ... longitude of the first point
         a12    = azim12 (iPt) * Deg2R     ! ... and direction angle
                                           ! Compute the trignometric
                                           !   functions of the ...
         sina12 = sin ( a12 )              ! ... direction angle
         cosa12 = cos ( a12 )
         sinU1  = sin ( f1 )               ! ... latitude
         cosU1  = cos ( f1 )
         sina   = cosU1 * sina12           ! ... azimuth of the geodesic
         cosa   = sqrt ( 1.0 - sina ** 2 ) !     at the equator 

         f2     = f1
         l2     = l1
         a21    = 0.0
         sig    = dist (iPt)
         if ( sig  .ne. 0.0 ) then
            sins = sin ( sig )             ! ... angular distance between the
            coss = cos ( sig )             !   two points
                                           ! For the second point, compute
                                           !   (in radians)
            y    = sinU1 * coss            ! ... the latitude ...
     .           + cosU1 * sins * cosa12
            xsq  = sina ** 2 + ( sinU1 * sins - cosU1 * coss * cosa12 ) ** 2
            f2   = atan2 ( y, sqrt ( xsq ) )

            y    = sins  * sina12          ! ... and longitude for the
            x    = cosU1 * coss - sinU1 * sins * cosa12
            l2   = atan2 ( y, x ) + l1     !   second point in radians

            y    = sina                    ! Compute the reverse angle
            x    = cosU1 * coss * cosa12 - sinU1 * sins
            a21  = atan2 ( y, x )
         end if

         lat2   (iPt) =  f2  / Deg2R       ! Convert output to degrees
         lon2   (iPt) =  l2  / Deg2R  
         azim21 (iPt) = modulo ( a21 / Deg2R, 360.0 )
         if ( azim21(iPt) .le. 0.0 ) azim21(iPt) = 360.0
      end do

      return
      end subroutine DirSph
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: InvSph - Solve the inverse problem for a sphere
!
! !DESCRIPTION: 
!     This routine solves the inverse problem in geodesy for a sphere.  The
!     algorithm is a special case of the formulae for an ellipsoid as
!     presented by Vincenty (1975).
!
! !INTERFACE:
!
      pure subroutine InvSph ( lat1, lon1,   lat2,   lon2,
     .                         dist, azim12, azim21 )
      implicit   NONE
!
! !INPUT PARAMETERS:
      real,          intent (in)  ::
     .   lat1   (:), ! geodetic latitude (deg, -90 = 90S)
     .   lon1   (:), ! ... longitude     (deg, -90 = 90W) for each point
     .   lat2   (:), ! latitude (deg)
     .   lon2   (:)  ! ... longitude (deg) for each second point
!
! !OUTPUT PARAMETERS: 
      real,          intent (out) ::
     .   dist   (:), ! angular ellipsoidal distance, arc / b, (radians) where
     .               !   arc is the arc distance and b the spherical radius
     .   azim12 (:), ! azimuth angle (deg, 90 = due east) and ...
     .   azim21 (:)  ! ... reverse azimuth angle on the ellipsoid (deg).
                     !   Range: (0,360]
!
!     Note: The number of points is determined by the size of the arrays.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     15Nov2001  C. Redder   Original code
!     18Apr2002  C. Redder   Modified the way the number of points are
!                            determined.
!     11Mar2007  C. Redder   Fixed bug for the case when points 1 and 2 
!                            are identical.
! EOP
!-------------------------------------------------------------------------

      real    :: Deg2R, l1, l2, f1, f2, a12, a21, SPI, x, y
      real    :: sinU1, cosU1, sinU2, cosU2
      real    :: dl, sindl, cosdl, sig, sins, coss
      integer :: iPt, NPts

      SPI     = PI
      Deg2R   = Deg2Rad
      NPts    = min ( size ( lat1 ), size ( lon1   ),
     .                size ( lat2 ), size ( lon2   ),
     .                size ( dist ), size ( azim12 ), size ( azim21 ))  
      do iPt  = 1, NPts
         f1    =  lat1 (iPt) * Deg2R       ! Convert the latitude
         l1    =  lon1 (iPt) * Deg2R       ! ... longitude of the first point
         f2    =  lat2 (iPt) * Deg2R       ! ... and of the second point
         l2    =  lon2 (iPt) * Deg2R  
                                           ! Compute the trignometric
                                           !   functions  of the ...
         sinU1 =  sin ( f1 )               ! ... latitude for the first
         cosU1 =  cos ( f1 )
         sinU2 =  sin ( f2 )               ! ... and second point
         cosU2 =  cos ( f2 )

         dl    =  l2 - l1                  ! Compute the difference in 
         sindl =  sin ( dl )               !   longitudes and their
         cosdl =  cos ( dl )               !   trignometric functions
         sins  =  sqrt ( ( cosU2 * sindl ) ** 2
     .                 + ( cosU1 * sinU2
     .                   - sinU1 * cosU2 * cosdl ) ** 2 )
         coss  =  sinU1 * sinU2 + cosU1 * cosU2 * cosdl
                                           ! Now compute (in radians)
         sig   =  atan2 ( sins, coss )     ! ... the angular distance

         y     =  cosU2 * sindl            ! ... azimuth angle
         x     =  cosU1 * sinU2 - sinU1 * cosU2 * cosdl
         a12   = 0.0
         if ( x .ne. 0.0 .or. y .ne. 0 ) a12 = atan2 ( y, x )

         y     =  cosU1 * sindl            ! ... and the reverse angle
         x     = -sinU1 * cosU2 + cosU1 * sinU2 * cosdl
         a21   = 0.0
         if ( x .ne. 0.0 .or. y .ne. 0 ) a21 = atan2 ( y, x )

         dist   (iPt) = sig                ! Convert angles to degrees 
         azim12 (iPt) = modulo ( a12 / Deg2R, 360.0 )
         if ( azim12(iPt) .le. 0.0 ) azim12(iPt) = 360.0
         azim21 (iPt) = modulo ( a21 / Deg2R, 360.0 )
         if ( azim21(iPt) .le. 0.0 ) azim21(iPt) = 360.0

      end do

      return
      end subroutine InvSph
!....................................................................
      end module m_ellipsoid
!====================================================================
