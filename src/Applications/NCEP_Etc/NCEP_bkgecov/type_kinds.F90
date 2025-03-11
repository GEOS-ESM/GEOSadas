!------------------------------------------------------------------------------
!M+
! NAME:
!       type_kinds
!
! PURPOSE:
!       Module to hold specification kinds for variable declaration.
!
! CATEGORY:
!       General
!
! CALLING SEQUENCE:
!       USE type_kinds
!
! OUTPUTS:
!       Byte     Specification kind for byte (1-byte) integer variable
!       Short    Specification kind for short (2-byte) integer variable
!       Long     Specification kind for long (4-byte) integer variable
!       LLong    Specification kind for double long (8-byte) integer variable
!       Single   Specification kind for single precision (4-byte) real variable
!       Double   Specification kind for double precision (8-byte) real variable
!       Quad     Specification kind for quad precision (16-byte) real variable
!
!       ip_kind: Generic specification kind for default integer
!       fp_kind: Generic specification kind for default floating point
!
! MODULES:
!       None
!
! CONTAINS:
!       type_size:  PUBLIC function to return the number of bytes used to
!                   represent the data type.
!
! SIDE EFFECTS:
!       If the LLong or Quad type kinds are not available they default to the
!       Long and Double kind specifications.
!
! RESTRICTIONS:
!       None
!
! EXAMPLE:
!       USE type_kinds
!       INTEGER( Long ) :: i, j
!       REAL( Single )  :: x, y
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS@NOAA/NCEP 12-Jun-2000
!                       pvandelst@ncep.noaa.gov
!
!
!  Copyright (C) 2000 Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!------------------------------------------------------------------------------

MODULE type_kinds


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibility
  ! ------------------

  PRIVATE
  PUBLIC :: type_size


  ! ----------
  ! Intrinsics
  ! ----------

  INTRINSIC ABS,                &
            KIND,               &
            SELECTED_INT_KIND,  &
            SELECTED_REAL_KIND, &
            SIZE,               &
            TRANSFER,           &
            TRIM


  ! -------------------
  ! Integer definitions
  ! -------------------

  ! -- Integer types
  INTEGER, PARAMETER, PUBLIC  :: Byte    = SELECTED_INT_KIND(1)   ! Byte  integer
  INTEGER, PARAMETER, PUBLIC  :: Short   = SELECTED_INT_KIND(4)   ! Short integer
  INTEGER, PARAMETER, PUBLIC  :: Long    = SELECTED_INT_KIND(8)   ! Long  integer
  INTEGER, PARAMETER, PRIVATE :: LLong_t = SELECTED_INT_KIND(16)  ! LLong integer
  INTEGER, PARAMETER, PUBLIC  :: LLong   = MAX( LLong_t, Long )
!  INTEGER, PARAMETER, PUBLIC  :: LLong   = ( ( ( 1 + SIGN( 1, LLong_t ) ) / 2 ) * LLong_t ) + &
!                                           ( ( ( 1 - SIGN( 1, LLong_t ) ) / 2 ) * Long    )

  ! -- Expected 8-bit byte sizes of the integer kinds
  INTEGER, PARAMETER, PUBLIC :: n_bytes_for_Byte_kind  = 1
  INTEGER, PARAMETER, PUBLIC :: n_bytes_for_Short_kind = 2
  INTEGER, PARAMETER, PUBLIC :: n_bytes_for_Long_kind  = 4
  INTEGER, PARAMETER, PUBLIC :: n_bytes_for_LLong_kind = 8

  ! -- Define arrays for default definition
  INTEGER, PARAMETER, PRIVATE :: N_IP_KINDS = 4
  INTEGER, PARAMETER, DIMENSION( N_IP_KINDS ), PRIVATE :: IP_KIND_TYPES = (/ Byte,  &
                                                                             Short, &
                                                                             Long,  &
                                                                             LLong  /) 
  INTEGER, PARAMETER, DIMENSION( N_IP_KINDS ), PRIVATE :: IP_BYTE_SIZES = (/ n_bytes_for_Byte_kind,  &
                                                                             n_bytes_for_Short_kind, &
                                                                             n_bytes_for_Long_kind,  &
                                                                             n_bytes_for_LLong_kind  /)

  ! -- Default values

  ! **** CHANGE THE FOLLOWING TO CHANGE THE DEFAULT INTEGER TYPE KIND ***
  INTEGER, PARAMETER, PRIVATE :: IIP = 3  ! 1=Byte, 2=Short, 3=Long, 4=LLong

  INTEGER, PARAMETER, PUBLIC  :: ip_kind             = IP_KIND_TYPES( IIP )
  INTEGER, PARAMETER, PUBLIC  :: n_bytes_for_ip_kind = IP_BYTE_SIZES( IIP )


  ! --------------------------
  ! Floating point definitions
  ! --------------------------

  ! -- Floating point types
  INTEGER, PARAMETER, PUBLIC  :: Single = SELECTED_REAL_KIND(6)  ! Single precision
  INTEGER, PARAMETER, PUBLIC  :: Double = SELECTED_REAL_KIND(15) ! Double precision
  INTEGER, PARAMETER, PRIVATE :: Quad_t = SELECTED_REAL_KIND(20) ! Quad precision
  INTEGER, PARAMETER, PUBLIC  :: Quad   = MAX( Quad_t, Double )
!  INTEGER, PARAMETER, PUBLIC  :: Quad   = ( ( ( 1 + SIGN( 1, Quad_t ) ) / 2 ) * Quad_t ) + &
!                                          ( ( ( 1 - SIGN( 1, Quad_t ) ) / 2 ) * Double )

  ! -- Expected 8-bit byte sizes of the floating point kinds
  INTEGER, PARAMETER, PUBLIC :: n_bytes_for_Single_kind = 4
  INTEGER, PARAMETER, PUBLIC :: n_bytes_for_Double_kind = 8
  INTEGER, PARAMETER, PUBLIC :: n_bytes_for_Quad_kind   = 16

  ! -- Define arrays for default definition
  INTEGER, PARAMETER, PRIVATE :: N_FP_KINDS = 3
  INTEGER, PARAMETER, DIMENSION( N_FP_KINDS ), PRIVATE :: FP_KIND_TYPES = (/ Single, &
                                                                             Double, &
                                                                             Quad    /) 
  INTEGER, PARAMETER, DIMENSION( N_FP_KINDS ), PRIVATE :: FP_BYTE_SIZES = (/ n_bytes_for_Single_kind, &
                                                                             n_bytes_for_Double_kind, &
                                                                             n_bytes_for_Quad_kind    /)

  ! -- Default values

  ! **** CHANGE THE FOLLOWING TO CHANGE THE DEFAULT FLOATING POINT KIND ***

#ifdef comp_dbl
  INTEGER, PARAMETER, PRIVATE :: IFP = 2  ! 1=Single, 2=Double, 3=Quad
#else
  INTEGER, PARAMETER, PRIVATE :: IFP = 1  ! 1=Single, 2=Double, 3=Quad
#endif

  INTEGER, PARAMETER, PUBLIC  :: fp_kind             = FP_KIND_TYPES( IFP )
  INTEGER, PARAMETER, PUBLIC  :: n_bytes_for_fp_kind = FP_BYTE_SIZES( IFP )


CONTAINS


!--------------------------------------------------------------------------------
!S+
! NAME:
!       type_size
!
! PURPOSE:
!       PUBLIC function to determine the size (in bytes) of a particular data type.
!
! CATEGORY:
!       General
!
! CALLING SEQUENCE:
!       result = type_size( type_kind, &
!                           expected_type_size = expected_type_size )
!
! INPUT ARGUMENTS:
!       type_kind:   String describing the definition of the data type. Valid
!                    values are (CASE SENSITIVE):
!                      "Byte"
!                      "Short"
!                      "Long"
!                      "LLong"
!                      "Single"
!                      "Double"
!                      "Quad"
!                      "ip_kind"
!                      "fp_kind"
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       expected_type_size:  Integer argument containing the *expected* size
!                            of the kind type in bytes. Useful if you will be
!                            using 8-byte integer (LLong) or 16-byte floating
!                            point (Quad) types and want to check if what they
!                            *should* be is what they actually *are*. Not all
!                            compilers support the LLong or Quad types.
!
!                            If included in argument list and an invalid or
!                            unrecognised type kind is specified, the returned
!                            value is 0.
!
! FUNCTION RESULT:
!       The returned value is the number of bytes used to represent the data
!       type on the platform the source code was compiled.
!
!       If an invalid or unrecognised type_kind is specified, the returned
!       result is -1.
!
! CALLS:
!       None.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       Assumes that a single character == 1 byte (8 bits).
!S-
!--------------------------------------------------------------------------------

  FUNCTION type_size( type_kind,         &
                      expected_type_size )

    ! -- Arguments
    CHARACTER( * ), INTENT( IN  )           :: type_kind
    INTEGER,        INTENT( OUT ), OPTIONAL :: expected_type_size

    ! -- Function
    INTEGER                            :: type_size

    ! -- Local variables
    INTEGER :: esize
    CHARACTER( 1 ), DIMENSION( 1 ) :: x

    SELECT CASE ( TRIM( type_kind ) )

      CASE ( 'Byte' )
        type_size = SIZE( TRANSFER( 0_Byte, x ) )
        esize     = n_bytes_for_Byte_kind

      CASE ( 'Short' )
        type_size = SIZE( TRANSFER( 0_Short, x ) )
        esize     = n_bytes_for_Short_kind

      CASE ( 'Long' )
        type_size = SIZE( TRANSFER( 0_Long, x ) )
        esize     = n_bytes_for_Long_kind

      CASE ( 'LLong' )
        type_size = SIZE( TRANSFER( 0_LLong, x ) )
        esize     = n_bytes_for_LLong_kind

      CASE ( 'ip_kind' )
        type_size = SIZE( TRANSFER( 0_ip_kind, x ) )
        esize     = n_bytes_for_ip_kind

      CASE ( 'Single' )
        type_size = SIZE( TRANSFER( 0.0_Single, x ) )
        esize     = n_bytes_for_Single_kind

      CASE ( 'Double' )
        type_size = SIZE( TRANSFER( 0.0_Double, x ) )
        esize     = n_bytes_for_Double_kind

      CASE ( 'Quad' )
        type_size = SIZE( TRANSFER( 0.0_Quad, x ) )
        esize     = n_bytes_for_Quad_kind

      CASE ( 'fp_kind' )
        type_size = SIZE( TRANSFER( 0.0_fp_kind, x ) )
        esize     = n_bytes_for_fp_kind

      CASE DEFAULT
        WRITE( *, '( /5x, "Invalid type kind: ", a )' ) TRIM( type_kind )
        type_size = -1
        esize     = 0

    END SELECT

    IF ( PRESENT( expected_type_size ) ) expected_type_size = esize

  END FUNCTION type_size

END MODULE type_kinds

!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: type_kinds.F90,v 1.4 2010/04/12 19:53:21 wgu Exp $
!
! $Date: 2010/04/12 19:53:21 $
!
! $Revision: 1.4 $
!
! $Name: GEOSadas-5_27_0 $
!
! $State: Exp $
!
! $Log: type_kinds.F90,v $
! Revision 1.4  2010/04/12 19:53:21  wgu
! add oz
!
! Revision 1.3  2007-11-21 00:33:32  wgu
! Merged at wgu-fvgsi-b2p4-1 to gAdas-1_5beta10p20 with changes for
! linear balancing.
!
! Revision 1.2  2005/05/03 17:18:33  wgu
! Halem related bug fixes
!
! Revision 1.1  2005/04/21 20:55:10  todling
! apr 05 update
!
! Revision 1.1  2003/11/24 20:25:44  todling
! placing code for gsi bkg err generation in the repository
!
! Revision 2.8  2001/10/24 17:33:27  paulv
! - Added "Name" to RCS keyword list.
!
! Revision 2.7  2001/09/17 19:55:57  paulv
! - Module now resides in the GENERAL module directory.
!
! Revision 2.6  2001/08/31 20:47:00  paulv
! - Updated definitions such that when the default type definition is changed
!   so is the assumed byte size of the result.
! - Commented out correct definition of type for LLong and Quad. PGI compiler
!   has a bug in it that does not allow elemental intrinsic functions to be
!   used in parameter initialisation expressions.
! - Function TYPE_SIZE altered to optional return the "expected" byte size
!   of specified type. This allows the user to check if the requested kind
!   type is supported by the compiler, i.e. if the actual and expected sizes
!   do not agree, then the kind type is unsupported.
!
! Revision 2.5  2001/07/12 16:43:16  paulv
! - Replaced possible LLong (8-byte integer) kind definition from
!     LLong   = ( ( ABS( LLong_t ) + LLong_t ) * LLong_t + &
!                 ( ABS( LLong_t ) - LLong_t ) * Long ) / &
!               ( 2 * ABS( LLong_t ) )
!   to
!     LLong   = MAX( LLong_t, Long )
! - Replaced possible Quad (16-byte floating point) kind definition from
!     Quad   = ( ( ABS( Quad_t ) + Quad_t ) * Quad_t + &
!                ( ABS( Quad_t ) - Quad_t ) * Double ) / &
!              ( 2 * ABS( Quad_t ) )
!   to
!     Quad   = MAX( Quad_t, Double )
! - Added commented definition for Quad precision kind.
! - Added comment in TYPE_SIZE function header explaining reliance on
!   1 character = 8 bits for function to return storage.
! - Removed LEN intrinsic from character definitions.
!
! Revision 2.4  2001/03/26 22:35:18  paulv
! - Renamed ip_precision and fp_precision parameters with ip_kind and
!   fp_kind.
! - Initialisation of ip_kind and fp_kind are now defined from the definitions
!   of the "fundamental" types. Currently ip_kind = Long and fp_kind = Double.
!   This was changed from the default kind typing using KIND( 0 ) and KIND( 0.0 )
!   respectively to allow the user to easily and explicitly redefine some sort
!   of default integer and floating point kind.
!
! Revision 2.3  2000/08/31 19:36:34  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 2.2  2000/08/31 15:55:34  paulv
! - Added documentation delimiters.
! - Added documentation for type_size function.
! - Changed default module visibility from PUBLIC to PRIVATE.
! - Added true default integer and floating point types.
!
! Revision 2.1  2000/08/08 17:08:55  paulv
! - Added definitions of 64-bit integers and reals. Definitions default to
!   the largest integer and real available on a system if not available.
! - Added type_size function to return the number of bytes used by a defined
!   data type - both integer and real.
!
! Revision 1.1  2000/07/12 16:08:11  paulv
! Initial checked in version
!
!
!
