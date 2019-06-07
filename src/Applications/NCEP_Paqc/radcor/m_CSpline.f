!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOI
! !MODULE: m_CSpline -- Utility routines for cubic spline intepolation
!
! !DESCRIPTION:
!     This module contains some utility routines for cubic spline 
!     interpolation.
!
! !INTERFACE:
!
      module      m_CSpline
      implicit    NONE
      private     ! except

      public ::
     .   CSpline, ! Returns tablulated second derivatives for cubic splines
     .   CSplY,   ! Returns the cubic-spline interpolated value
     .   CSplYP,  ! ... its first and
     .   CSplYPP, ! ... its second derivative
     .   FCSplY,  ! Function call for CSplY,
     .   FCSplYP, ! ... CSplYP,
     .   FCSplYPP,! ... and CSplYPP.
     .   BiSect,  ! Performs bi-section search by subroutine
     .   FBiSect, ! ... or function call
     .   TriDiag  ! Solves the tridiagonal matrix equation
!
! !REVISION HISTORY:
!     25Oct2001  C. Redder  Original code
!     23Apr2001  C. Redder  Module renamed from m_RadUtil.f
!     08Aug2002  C. Redder  Made CSpline a generic interface.
!
!EOI
!-----------------------------------------------------------------

      interface CSpline
         module procedure
     .      CSpline_u,    ! without and  
     .      CSpline_      ! ... with scratch space as a
      end interface       !       required argument.

      interface CSplY
         module procedure
     .      CSplY_main,   ! without and  
     .      CSplY_kkxa    ! ... with the required argument kkxa.
      end interface

      interface CSplYP
         module procedure
     .      CSplYP_main,  ! without and  
     .      CSplYP_kkxa   ! ... with the required argument kkxa.
      end interface

      interface CSplYPP
         module procedure
     .      CSplYPP_main, ! without and  
     .      CSplYPP_kkxa  ! ... with the required argument kkxa.
      end interface

      real, parameter :: PrTol = 1.0 / 10.0 ** ( precision ( 1.0 ))
      contains

!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CSpline_u () --- Calculates 2nd derivatives of the cubic spline interpolation
!
! !DESCRIPTION:
!     Given the arrays x(:n) and y (:n) where y(i) = f(x(i)) for some
!     function f and x(i) < x(i+1) for all i in the range [1,n), this
!     routine returns the array ypp (:n) which contains the second
!     derivatives of the natural cubic spline interpolating function at
!     tabulated points x.  This routine has optional input arguments
!     to specify boundary conditions at x(1) and x(n) other than for
!     the natural cubic spline (i.e. zero second derivatives at x(1)
!     and x(n).
!
! !INTERFACE:
      pure subroutine CSpline_u ( x,    y,   u,    ypp,        ! Required and
     .                            stat, yp1, ypp1, ypn, yppn ) ! optional
!                                                              ! arguments
! !INPUT PARAMETERS:
      real,         intent (in)  ::
     .   x     (:), ! The tabulated points and ...
     .   y     (:)  ! ... values for the function f.
      real,         intent (in), optional ::
     .   yp1,       ! The first and ...
     .   ypp1,      ! ... second derivative of f at x(1)
     .   ypn,       ! The first and ...
     .   yppn       ! ... second derivative of f at x(n)
! !OUTPUT PARAMETERS:
      real,         intent (out) ::
     .   u     (:), ! Scratch space
     .   ypp   (:)  ! Output vector
      integer,      intent (out), optional ::
     .   stat       ! return status code
                    !   = 0 if all ok
                    !   > 0 to denote i such that x(i) = x(i+1)
!     Note: The variable n in the above description is determined by
!           the minimum size of all array arguments.
!
! !REFERENCES:
!     Press, W. H., S. A. Teukolsky, W. T. Vetterling and B. P. Flannery,
!          1992: Numerical Recipes.  Cambridge University Press, 
!          New York, NY, 963 pp.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     09Oct2001  C. Redder   Original code.  Adapted from the routine,
!                            spline, in Press et al (1992) with interface
!                            changes and other enhancements.
!     18Apr2002  C. Redder   Changed method of determining the number n
!                            from the size of the input arrays.
!     08Aug2002  C. Redder   Made the scratch space variable a required
!                            argument.  Changed name of subroutine from
!                            CSpline to CSpline_u as a module procedure in
!                            the generic interface CSpline.
! EOP
!-------------------------------------------------------------------------

      integer :: i, n
      real    :: h1, h2, y1, y2, hmin, p, sig
      logical :: increasing

!     Nothing to do if one or no tabulated points exist
!     -------------------------------------------------
      n    = min ( size ( x ), size ( y ), size ( u ), size ( ypp ))
      if ( n .le. 1 ) then
         if ( n .ge. 1 ) ypp (:n) = 0.0
         if ( present ( stat ) ) stat = 0
         return
      endif

!     Set minimum interval size allowed
!     ---------------------------------
      hmin = PrTol * ( max ( maxval (x) - minval (x),
     .                       tiny ( 1.0 ) / PrTol ))

!     ... and,if desired, check the intevals set by x(i)
!     -------------------------------------------------
      if ( present ( stat ) ) then
         stat       = 0
         increasing = all ( maxloc (x) - minloc (x) .gt. 0 )
         do i = n - 1, 1, -1
            if ( x (i+1) - x (i) .le. hmin .eqv. increasing ) stat = i
         end do
      end if

!     Include effects of the boundary at x(1)
!     --------------------------------------
      ypp    (1) =  0.0                ! ... for a natural spline (default)
      u      (1) =  0.0               
      if      ( present ( yp1  )) then ! ... with specified 1st derivative
         ypp (1) = -0.5
         h2      =  max ( abs ( x (2) - x (1) ), hmin )
         y2      =  y (2) - y (1)
         u   (1) =  3.0 * ( y2 / h2 - yp1 ) / h2
      else if ( present ( ypp1 )) then ! ... with specified 2nd derivative
         ypp (1) =  0.0              
         u   (1) =  ypp1
      end if

!     Perform the decomposition of the tridiagonal algrithm.
!     -----------------------------------------------------
      do i = 2, n - 1
         h1  = max ( abs ( x (i  ) - x (i-1) ), hmin )
         h2  = max ( abs ( x (i+1) - x (i  ) ), hmin )
         y1  = y (i  ) - y (i-1)
         y2  = y (i+1) - y (i  )
         sig = h1 / ( h2 + h1 )
         p   = sig * ypp (i-1) + 2.0
         ypp (i) = ( sig - 1.0 ) / p   ! ypp is used as temp storage
         u   (i) = ( 6.0 * ( y2 / h2 - y1 / h1 ) / ( h1 + h2 ) 
     .                   - sig * u (i-1) ) / p
      end do

!     Include effects of the upper boundary
!     -------------------------------------
      ypp (n)    = 0.0                 ! ... for a natural spline (default)
      if      ( present ( ypn  )) then ! ... with specified 1st derivative
         h1 = max ( abs ( x (n) - x (n-1) ), hmin )
         y1 = y (n) - y (n-1)
         ypp (n) = ( 3.0 * ( ypn - y1 / h1 ) /  h1 - 0.5 * u   (n-1) )
     .                                     / ( 1.0 + 0.5 * ypp (n-1) ) 
      else if ( present ( yppn )) then ! ... with specified 2nd derivative
         ypp (n) = yppn
      end if

!     Perform the backsubstitution of the tridiagonal algorithm
!     ---------------------------------------------------------
      do i = n - 1, 1, -1
         ypp (i) = ypp (i) * ypp (i+1) + u (i) 
      end do

      return
      end subroutine CSpline_u

!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CSpline_ () --- Calculates 2nd derivations of the cubic spline interpolation
!
! !DESCRIPTION:
!     This routine is identical to the routine CSpline_u except that scratch
!     is not a required argument.  For more information see the prolouge to
!     the routine, CSpline_u.
!
! !INTERFACE:
      pure subroutine CSpline_ ( xx,   yy,  yypp,             ! Required and
     .                           stat, yp1, ypp1, ypn, yppn ) ! optional
!                                                             ! arguments
! !INPUT PARAMETERS:
      real,         intent (in)  ::
     .   xx    (:), ! The tabulated points and ...
     .   yy    (:)  ! .. values for the function f.
      real,         intent (in), optional ::
     .   yp1,       ! The first and ...
     .   ypp1,      ! ... second derivative of f at x(1)
     .   ypn,       ! The first and ...
     .   yppn       ! ... second derivative of f at x(n)
! !OUTPUT PARAMETERS:
      real,         intent (out) ::
     .   yypp   (:) ! Output vector
      integer,      intent (out), optional ::
     .   stat       ! return status code
                    !   = 0 if all ok
                    !   > 0 to denote i such that x(i) = x(i+1)
!     Note: The variable n in the above description is determined by
!           the minimum size of all array arguments.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     09Aug2002  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real :: u ( size ( xx ))

      call CSpline_u ( xx, yy, u, yypp, stat, yp1, ypp1, ypn, yppn )

      return
      end subroutine CSpline_

!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CSplY_main () --- Calculate values by cubic splines.
!
! !DESCRIPTION:
!     Given the arrays of tabulated points, xa(:na), their function values,
!     ya(:na) and their second derivative, yppa(:na), (from the routine
!     CSpline), this routine returns the cubic spline interpolated values,
!     y(:n), at a given set of points, x(:n), where na and n are the number
!     of tabulated and interpolated points.  If x(i) for any point i is
!     outside the domain of xa, then the y(i) is not defined by this
!     subroutine.
!
! !INTERFACE:
      pure subroutine CSplY_main ( xa, ya, yppa, x, y, kxa )
!
! !INPUT PARAMETERS:
      real,        intent (in) ::
     .   xa   (:), ! The tabulated points (must be increasing or decreasing)
     .   ya   (:), ! ... and their function values
     .   yppa (:), ! ... and their second derivatives
     .   x    (:)  ! The given values at which interpolation is performed
      integer,     intent (in), optional ::
     .   kxa  (:)  ! The interval number for given values, x (i) where
                   !   xa(kx(i)) <= x(i) <= xa(kx(i)+1).
!
! !INPUT/OUTPUT PARAMETERS:
      real,        intent (inout) ::
     .   y    (:)  ! The cubic spline interpolated values.  
!
!     Note: The number of tabulated points, na, and the number of points
!           to calculate, n, are determined by the minimum size of the
!           arrays, xa, ya, yppa and the arrays x, y and kxa (if present).
!
! !REFERENCES:
!     Press, W. H., S. A. Teukolsky, W. T. Vetterling and B. P. Flannery,
!          1992: Numerical Recipes.  Cambridge University Press, 
!          New York, NY, 963 pp.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     09Oct2001  C. Redder   Original code.  Adapted from the routine,
!                            splint, in Press et al (1992) with
!                            enhancements.
!     18Apr2002  C. Redder   Changed method of determining the numbers n
!                            and na from the size of the arrays.
!
! EOP
!-------------------------------------------------------------------------

      logical :: kxa_not_needed
      real    :: a, b, h, hmin, xamax, xamin
      integer :: na, n, i, k1, k2

      n  = min ( size ( x  ), size ( y  ))
      na = min ( size ( xa ), size ( ya ), size ( yppa ))
      if ( present ( kxa )) na = min ( na, size ( kxa ))
      if ( na .le. 0 ) return
      kxa_not_needed = present ( kxa )

!     Set minimum interval size allowed
!     ---------------------------------
      xamax = max ( xa (1), xa (na))
      xamin = min ( xa (1), xa (na))
      hmin  = PrTol * ( max ( abs ( xamax - xamin ),
     .                        tiny ( 1.0 ) / PrTol ))

      do i = 1, n

!        Interpolate only if value remains in the domain of tabulated values
!        -------------------------------------------------------------------
         if ( x (i) .gt. xamin - hmin .and.
     .        x (i) .lt. xamax + hmin ) then

!           Identify appropriate interval for tabulated values
!           --------------------------------------------------
            if ( kxa_not_needed ) then
               k1 = max ( 1, min ( na - 1, kxa(i) ))
            else
               k1 = max ( 1, min ( na - 1, FBiSect ( xa, x(i) )))
            end if
            k2 = min ( k1 + 1, na )

!           Interpolate
!           -----------
            h = xa (k2) - xa (k1)
            if ( abs (h) .gt. hmin ) then
               a = ( xa (k2) - x  (i)  ) / h
               b = ( x  (i)  - xa (k1) ) / h
               y (i) = a * ya (k1) + b * ya (k2)
     .               + (( a**3 - a ) * yppa (k1)
     .               +  ( b**3 - b ) * yppa (k2) ) * h**2 / 6.0
            else
               y (i) = 0.5 * ( ya (k2) + ya (k1) )  ! If interval size is 0
            end if
         end if
      end do

      return
      end subroutine CSplY_main

!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CSplY_kkxa () --- Calculate values by cubic splines with no search
!
! !DESCRIPTION:
!     Same as the routine, CSplY_main, except for the extra required
!     argument kkxa(i) where xa(kkxa(i)) <= x(i) <= xa(kkxa(i)+1) and the
!     remaining variables are defined in the routine CSplY_main.  This is,
!     this routine performs no bisection search for the correct entry in
!     the table xa.
!
! !INTERFACE:
      pure subroutine CSplY_kkxa ( xa, ya, yppa, kkxa, x, y )
!
! !INPUT PARAMETERS:
      real,        intent (in) ::
     .   xa   (:), ! The tabulated points (must be increasing or decreasing)
     .   ya   (:), ! ... and their function values
     .   yppa (:), ! ... and their second derivatives
     .   x    (:)  ! The given values at which interpolation is performed
      integer,     intent (in) ::
     .   kkxa (:)  ! The interval number for given values, x (i) where
                   !   xa(kx(i)) <= x(i) <= xa(kx(i)+1).
!
! !INPUT/OUTPUT PARAMETERS:
      real,        intent (inout) ::
     .   y    (:)  ! The cubic spline interpolated values.  
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     22Oct2001  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      call CSplY_main ( xa, ya, yppa, x, y, kxa = kkxa )

      return
      end subroutine CSplY_kkxa

!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: FCSplY () --- Calculate the function value by cubic splines
!
! !DESCRIPTION:
!     Given the arrays of tabulated points, xa(:na), their function values,
!     ya(:na) and their second derivatives, yppa(:na), (from the routine
!     CSpline), this routine returns the cubic spline a interpolated value,
!     at a given points, x, where na is the number of tabulated points.  If
!     x is outside the domain of xa, then the returned value is set to zero
!     unless the default value is defined by calling the routine.
!
! !INTERFACE:
      pure function FCSplY ( xa, ya, yppa, x, dfault )
!
! !INPUT PARAMETERS:
      real,        intent (in) ::
     .   xa   (:), ! The tabulated points.
     .   ya   (:), ! ... and their function values
     .   yppa (:), ! ... and their second derivatives
     .   x         ! The given value at which interpolation is performed
      real,        intent (in), optional ::
     .   dfault    ! The default value
!
! !OUTPUT PARAMETERS:
      real ::
     .   FCSplY    ! The interpolated value
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     15Oct2001  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real :: y (1)

      y = 0.0
      if ( present ( dfault ) ) y = dfault
      call CSplY_main ( xa, ya, yppa, (/x/), y ) 
      FCSplY = y(1)

      return
      end function FCSplY

!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CSplYP_main () --- Calculate first derivatives by cubic splines
!
! !DESCRIPTION:
!     Given the arrays of tabulated points, xa(:na), their function values,
!     ya(:na) and their second derivatives, yppa(:na), (from the routine
!     CSpline), this routine returns the cubic spline interpolated first
!     derivatives, yp(:n), at a given set of points, x(:n), where na and n
!     are the number of tabulated and interpolated points.  If x(i) for any
!     point i is outside the domain of xa or if abs(x(i+1)-x(i)) = 0, then
!     the yp(i) is not defined by this subroutine.
!
! !INTERFACE:
      pure subroutine CSplYP_main ( xa, ya, yppa, x, yp, kxa )
!
! !INPUT PARAMETERS:
      real,        intent (in) ::
     .   xa   (:), ! The tabulated points.
     .   ya   (:), ! ... and their function values
     .   yppa (:), ! ... and their second derivatives
     .   x    (:)  ! The given values at which interpolation is performed
      integer,     intent (in), optional ::
     .   kxa  (:)  ! The interval number for given values, x (i) where
                   !   xa(kx(i)) <= x(i) <= xa(kx(i)+1).
!
! !INPUT/OUTPUT PARAMETERS:
      real,        intent (inout) ::
     .   yp   (:)  ! The cubic spline interpolated first derivatives.  
!
!     Note: The number of tabulated points, na, and the number of points
!           to calculate, n, are determined by the minimum size of the
!           arrays, xa, ya, yppa and the arrays x, yp and kxa (if present).
!
! !REFERENCES:
!     Press, W. H., S. A. Teukolsky, W. T. Vetterling and B. P. Flannery,
!          1992: Numerical Recipes.  Cambridge University Press, 
!          New York, NY, 963 pp.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     15Oct2001  C. Redder   Original code.  Adapted from the routine,
!                            splint, in Press et al (1992) with enhancements.
!     18Apr2002  C. Redder   Changed method of determining the numbers n
!                            and na from the size of the input arrays.
! EOP
!-------------------------------------------------------------------------

      logical :: kxa_not_needed
      real    :: a, b, da, db, h, hmin, xamax, xamin
      integer :: na, n, i, k1, k2

      n  = min ( size ( x  ), size ( yp ))
      na = min ( size ( xa ), size ( ya ), size ( yppa ))
      if ( present ( kxa )) na = min ( na, size ( kxa ))
      if ( na .le. 0 ) return
      kxa_not_needed = present ( kxa )

!     Set minimum interval size allowed
!     ---------------------------------
      xamax = max ( xa (1), xa (na))
      xamin = min ( xa (1), xa (na))
      hmin  = PrTol * ( max ( abs ( xamax - xamin ),
     .                        tiny ( 1.0 ) / PrTol ))

      do i = 1, n

!        Interpolate only if value remains in the domain of tabulated values
!        -------------------------------------------------------------------
         if ( x (i) .gt. xamin - hmin .and.
     .        x (i) .lt. xamax + hmin ) then

!           Identify appropriate interval for tabulated values
!           --------------------------------------------------
            if ( kxa_not_needed ) then
               k1 = max ( 1, min ( na - 1, kxa(i) ))
            else
               k1 = max ( 1, min ( na - 1, FBiSect ( xa, x(i) )))
            end if
            k2 = min ( k1 + 1, na )

!           Interpolate
!           -----------
            h = xa (k2) - xa (k1)
            if ( abs (h) .gt. hmin ) then
               a  = ( xa (k2) - x  (i)  ) / h
               b  = ( x  (i)  - xa (k1) ) / h
               da = - 1.0 / h
               db = - da
               yp (i) =   da * ya (k1) + db * ya (k2)
     .                + ( da * ( 3.0 * a**2 - 1.0 ) * yppa (k1)
     .                +   db * ( 3.0 * b**2 - 1.0 ) * yppa (k2) )
     .                       * h**2 / 6.0
            end if
         end if
      end do

      return
      end subroutine CSplYP_main

!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CSplYP_kkxa () --- Calculate first derivatives by cubic splines with no search
!
! !DESCRIPTION:
!     Same as the routine, CSplYP_main, except for the extra required
!     argument kkxa(i) where xa(kkxa(i)) <= x(i) <= xa(kkxa(i)+1) and the
!     remaining variables are defined in the routine CSplYP_main.  This is,
!     this routine performs no bisection search for the correct entry in
!     the table xa.
!
! !INTERFACE:
      pure subroutine CSplYP_kkxa ( xa, ya, yppa, kkxa, x, yp )
!
! !INPUT PARAMETERS:
      real,        intent (in) ::
     .   xa   (:), ! The tabulated points (must be increasing or decreasing)
     .   ya   (:), ! ... and their function values
     .   yppa (:), ! ... and their second derivatives
     .   x    (:)  ! The given values at which interpolation is performed
      integer,     intent (in) ::
     .   kkxa (:)  ! The interval number for given values, x (i) where
                   !   xa(kx(i)) <= x(i) <= xa(kx(i)+1).
!
! !INPUT/OUTPUT PARAMETERS:
      real,        intent (inout) ::
     .   yp   (:)  ! The cubic spline interpolated first derivatives.  
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     22Oct2001  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      call CSplYP_main ( xa, ya, yppa, x, yp, kxa = kkxa )

      return
      end subroutine CSplYP_kkxa

!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: FCSplYP () --- Calculate the first derivative by cubic splines
!
! !DESCRIPTION:
!     Given the arrays of tabulated points, xa(:na), their function values,
!     ya(:na) and their second derivatives, yppa(:na), (from the routine
!     CSpline), this routine returns the cubic spline a interpolated first,
!     derivative at a given points, x, where na is the number of tabulated
!     points.  If x(i) for any point i is outside the domain of xa or if
!     abs(x(i+1)-x(i)) = 0, then the yp(i) is not defined by this
!     subroutine unless the default value is defined by the calling
!     routine.
!
! !INTERFACE:
      pure function FCSplYP ( xa, ya, yppa, x, dfault )
!
! !INPUT PARAMETERS:
      real,        intent (in) ::
     .   xa   (:), ! The tabulated points.
     .   ya   (:), ! ... and their function values
     .   yppa (:), ! ... and their second derivatives
     .   x         ! The given value at which interpolation is performed
      real,        intent (in), optional ::
     .   dfault    ! The default value
!
! !OUTPUT PARAMETERS:
      real ::
     .   FCSplYP   ! The interpolated first derivative
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     15Oct2001  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real :: yp (1)

      yp = 0.0
      if ( present ( dfault ) ) yp = dfault
      call CSplYP ( xa, ya, yppa, (/x/), yp ) 
      FCSplYP = yp(1)

      return
      end function FCSplYP

!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CSplYPP_main () --- Calculate second derivatives by cubic splines
!
! !DESCRIPTION:
!     Given the arrays of tabulated points, xa(:na) and their second
!     derivatives, yppa(:na), (from the routine CSpline), this routine
!     returns the cubic spline interpolated second derivatives, yp(:n),
!     at a given set of points, x(:n), where na and n are the number of
!     tabulated and interpolated points.  If x(i) for any point i is
!     outside the domain of xa, then the y(i) is not defined by this
!     subroutine.
!
! !INTERFACE:
      pure subroutine CSplYPP_main ( xa, yppa, x, ypp, kxa )
!
! !INPUT PARAMETERS:
      real,        intent (in) ::
     .   xa   (:), ! The tabulated points.
     .   yppa (:), ! ... and their second derivatives
     .   x    (:)  ! The given values at which interpolation is performed
      integer,     intent (in), optional ::
     .   kxa  (:)  ! The interval number for given values, x (i) where
                   !   xa(kx(i)) <= x(i) <= xa(kx(i)+1).
!
! !INPUT/OUTPUT PARAMETERS:
      real,        intent (inout) ::
     .   ypp   (:) ! The cubic spline interpolated second derivatives.  
!
!     Note: The number of tabulated points, na, and the number of points
!           to calculate, n, are determined by the minimum size of the
!           arrays, xa, yppa and the arrays x, ypp and kxa (if present).
!
! !REFERENCES:
!     Press, W. H., S. A. Teukolsky, W. T. Vetterling and B. P. Flannery,
!          1992: Numerical Recipes.  Cambridge University Press, 
!          New York, NY, 963 pp.
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     15Oct2001  C. Redder   Original code.  Adapted from the routine,
!                            splint, in Press et al (1992) with enhancements.
!     18Apr2002  C. Redder   Changed method of determining the numbers n
!                            and na from the size of the input arrays.
!
! EOP
!-------------------------------------------------------------------------

      logical :: kxa_not_needed
      real    :: a, b, h, hmin, xamax, xamin
      integer :: na, n, i, k1, k2

      n  = min ( size ( x  ), size ( ypp  ))
      na = min ( size ( xa ), size ( yppa ))
      if ( present ( kxa )) na = min ( na, size ( kxa ))
      if ( na .le. 0 ) return
      kxa_not_needed = present ( kxa )

!     Set minimum interval size allowed
!     ---------------------------------
      xamax = max ( xa (1), xa (na))
      xamin = min ( xa (1), xa (na))
      hmin  = PrTol * ( max ( abs ( xamax - xamin ),
     .                        tiny ( 1.0 ) / PrTol ))

      do i = 1, n

!        Interpolate only if value remains in the domain of tabulated values
!        -------------------------------------------------------------------
         if ( x (i) .gt. xamin - hmin .and.
     .        x (i) .lt. xamax + hmin ) then

!           Identify appropriate interval for tabulated values
!           --------------------------------------------------
            if ( kxa_not_needed ) then
               k1 = max ( 1, min ( na - 1, kxa(i) ))
            else
               k1 = max ( 1, min ( na - 1, FBiSect ( xa, x(i) )))
            end if
            k2 = min ( k1 + 1, na )

!           Interpolate
!           -----------
            h = xa (k2) - xa (k1)
            if ( abs (h) .gt. hmin ) then
               a  = ( xa (k2) - x  (i)  ) / h
               b  = ( x  (i)  - xa (k1) ) / h
               ypp (i) = a * yppa (k1) + b * yppa (k2)
            else
               ypp (i) =  0.5 * ( yppa (k1) + yppa (k2) )
            end if
         end if
      end do

      return
      end subroutine CSplYPP_main

!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: CSplYPP_kkxa () --- Calculate second derivatives by cubic splines with no search
!
! !DESCRIPTION:
!     Same as the routine, CSplYP_main, except for the extra required
!     argument kkxa(i) where xa(kkxa(i)) <= x(i) <= xa(kkxa(i)+1) and the
!     remaining variables are defined in the routine CSplYP_main.  This is,
!     this routine performs no bisection search for the correct entry in
!     the table xa.
!
! !INTERFACE:
      pure subroutine CSplYPP_kkxa ( xa, yppa, kkxa, x, ypp )
!
! !INPUT PARAMETERS:
      real,        intent (in) ::
     .   xa   (:), ! The tabulated points (must be increasing or decreasing)
     .   yppa (:), ! ... and their second derivatives
     .   x    (:)  ! The given values at which interpolation is performed
      integer,     intent (in) ::
     .   kkxa (:)  ! The interval number for given values, x (i) where
                   !   xa(kx(i)) <= x(i) <= xa(kx(i)+1).
!
! !INPUT/OUTPUT PARAMETERS:
      real,        intent (inout) ::
     .   ypp  (:)  ! The cubic spline interpolated second derivatives.  
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     22Oct2001  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      call CSplYPP_main ( xa, yppa, x, ypp, kxa = kkxa )

      return
      end subroutine CSplYPP_kkxa

!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: FCSplYPP () --- Calculate second derivative by cubic splines
!
! !DESCRIPTION:
!     Given the arrays of tabulated points, xa(:na), their function values,
!     ya(:na) and their second derivatives, yppa(:na), (from the routine
!     CSpline), this routine returns the cubic spline a interpolated second
!     derivative at a given points, x, where na is the number of tabulated
!     points.  If x(i) for any point i is outside the domain of xa, then
!     the yp(i) is not defined by this subroutine unless the default value
!     is defined by the calling routine.
!
! !INTERFACE:
      pure function FCSplYPP ( xa, yppa, x, dfault )
!
! !INPUT PARAMETERS:
      real,        intent (in) ::
     .   xa   (:), ! The tabulated points.
     .   yppa (:), ! ... and their second derivatives
     .   x         ! The given value at which interpolation is performed
      real,        intent (in), optional ::
     .   dfault    ! The default value
!
! !OUTPUT PARAMETERS:
      real ::
     .   FCSplYPP  ! The interpolated second derivative
!
!     Note: The number of tabulated points, na, is determined by the size
!           of the arrays, xa.
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     15Oct2001  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      real :: ypp (1)

      ypp = 0.0
      if ( present ( dfault ) ) ypp = dfault
      call CSplYPP ( xa, yppa, (/x/), ypp ) 
      FCSplYPP = ypp(1)

      return
      end function FCSplYPP

!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: TriDiag () --- Solves tridiagonal matrix equation
!
! !DESCRIPTION:
!     This routine solves for the vector x in the tridiagonal matrix
!     equation, Ax = b, where A is the tridiagonal matrix and b is a
!     vector.  This routines performs no pivoting.
!
! !INTERFACE:
      pure subroutine TriDiag ( A, b, x, iret )
!
! !INPUT PARAMETERS:
      real,         intent (in)  ::
     .   A   (:,:), ! Tridiagonal matrix where A (1,:) is the lower diagonal
     .              !   and A (3,:) is the upper diagonal so that 
     .              !   A(1,i)*x(i-1) + A(2,i)*x(i) + A(3,i)*x(i+1) = b(i)
     .              !   and A(1,0) = A(3,n) = 0.0
     .   b     (:)  ! Input vector
! !OUTPUT PARAMETERS:
      real,         intent (out) ::
     .   x     (:)  ! Output vector
      integer,      intent (out) ::
     .   iret       ! return status code
                    !   =  0 if all ok
                    !   >  0 to denote ith equation where zero pivot occurs
                    !   < -1 if size of dim 1 of A is less than 3
!
!     Note: Size of A and length of x is determine by the minimum size of 
!           A (2nd dimension), b and x 
!
! !REFERENCES:
!     Press, W. H., S. A. Teukolsky, W. T. Vetterling and B. P. Flannery,
!          1992: Numerical Recipes.  Cambridge University Press, 
!          New York, NY, 963 pp.
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     09Oct2001  C. Redder   Original code.  Adapted from the routine,
!                            tridag, in Press et al (1992).
!     18Apr2002  C. Redder   Changed method of determining the sizes
!                            of the input arrays.
!     02May2007  C. Redder   Change intent from out to in for the input
!                            arguments, A and b
! EOP
!-------------------------------------------------------------------------

      integer :: j, n
      real    :: bet, gam ( size ( B ) )

!     Check size of first dimension in A
!     ----------------------------------
      if ( size ( A, dim = 1 ) .lt. 3 ) then
         iret = -1
         return
      endif

!     Get and check matrix and vector sizes
!     -------------------------------------
      n       = min ( size ( A, dim = 2), size ( b ), size ( x ))
      if ( n .le. 0 ) return

!     Decomposition and forward substitution
!     --------------------------------------
      bet     = a ( 2, 1 )
      if ( bet .eq. 0.0 ) then
         iret = 1
         return
      end if
      x ( 1 ) = b ( 1 ) / bet
      do j = 2, n
         gam ( j ) = A ( 3, j - 1 ) / bet
         bet       = A ( 2, j ) - a ( 1, j ) * gam ( j )
         if ( bet .eq. 0.0 ) then
            iret = j
            return
         end if 
         x   ( j ) =  ( b ( j ) - a ( 1, j ) * x ( j - 1 ) ) / bet

      end do

!     Back substitution
!     -----------------
      do j = n - 1, 1, -1
         x   ( j ) = x ( j ) - gam ( j + 1 ) * x ( j + 1 )
      end do

      return
      end subroutine TriDiag

!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: BiSect () ---  Perform bisection search 
!
! !DESCRIPTION:
!     This routine performs bisection searches for the appropriate indices,
!     ka, such that xa(ka) <= x < x(ka+1) where xa(:na) is an array of
!     tabulated values sorted in increasing order, x(:n) is the set of
!     values to be located, and ka is within the range [0,na].  If xa is
!     sorted in decreasing order, then the routine returns the values of
!     ka such that xa(ka) > x >= x(ka+1).  If ka is 0 or na, then x is out
!     of range.
!
! !INTERFACE:
      pure subroutine BiSect ( xa, x, ka )
!
! !INPUT PARAMETERS:
      real,        intent (in ) ::
     .   xa   (:), ! The tabulated values.
     .   x    (:)  ! The values to be located
!
! !OUTPUT PARAMETERS:
      integer,     intent (out) ::
     .   ka   (:)  ! The location of the values of x
!
!     Note: The number of tabulated points, na, and the number values to
!           locate, n, are determined by the size of the array x and the
!           minimum size of the arrays, x and ka.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     09Oct2001  C. Redder   Original code.  Adapted from the routine,
!                            GetIRange_IntArr() in the module m_Range
!     18Apr2002  C. Redder   Changed method of determining the numbers n
!                            and na from the size of the input arrays.
! EOP
!-------------------------------------------------------------------------

      integer :: n, na, i, ia, ibeg, iend, imid
      real    :: xx
      logical :: decreasing

      n  = min ( size (x),  size (ka)) ! Number of searches 
      na =       size (xa)             ! Number of tabulated values

!     Decreasing?
!     -----------
      if ( na .gt. 0 ) decreasing = xa (na) .lt. xa (1)

!     For each search ...
!     -------------------
      do i = 1, n
         xx = x (i)

         iBeg = 0                      ! Initialize lower
         iEnd = na + 1                 ! ... and upper bounds
         LocateLoop : do ia = 1, na    ! Until the search is completed
            if ( iEnd - iBeg .le. 1 ) exit LocateLoop
            iMid = ( iEnd + iBeg ) / 2 ! ... compute the midpoint
            if ( decreasing .neqv. xx .lt. xa (iMid) ) then
               iEnd = iMid             ! ... and replace the upper or
            else
               iBeg = iMid             ! ... lower limit as appropriate
            end if
         end do LocateLoop
         ka (i) = iBeg                 ! At end of search, save and go to
                                       !   the next point.
      end do

      return
      end subroutine BiSect
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: FBiSect () ---  Perform bisection search 
!
! !DESCRIPTION:
!     This function performs a bisection searches for the appropriate index,
!     ka, such that xa(ka) <= x < x(ka+1) where xa(:na) is an array of
!     tabulated values sorted in increasing order, x is the value to be
!     located, and ka is within the range [0,na].  If xa is sorted in
!     decreasing order, then the function returns the value of ka such
!     xa(ka) > x >= x(ka+1).  If ka is 0 or na, then x is out of range.
!
! !INTERFACE:
      pure function FBiSect ( xa, x )
!
! !INPUT PARAMETERS:
      real,        intent (in) ::
     .   xa   (:), ! The tabulated values.
     .   x         ! The value to be located
!
! !OUTPUT PARAMETERS:
      integer ::
     .   FBiSect   ! The location of the value
!
!     Note: The number of tabulated points, na, is determined by the size
!           of the array, xa.
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     15Oct2001  C. Redder   Original code.
!
! EOP
!-------------------------------------------------------------------------

      integer :: ka (1)
      call BiSect ( xa, (/x/), ka )
      FBiSect = ka (1)

      return
      end function FBiSect

!.................................................................
      end module m_CSpline
!====================================================================

