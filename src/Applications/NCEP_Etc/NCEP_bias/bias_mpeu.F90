module bias_mpeu
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module bias_mpeu
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-04-22
!
! abstract: an interface to environment utilities
!
! program history log:
!   2010-04-22  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:
!#define NCEP_ENV

#ifdef NCEP_ENV
	! USE GSI_GridComp/
  use mpeu_mpif, only: MPI_comm_world
  use mpeu_util, only: die, perr
  use mpeu_util, only: stderr, stdout, stdin
  use mpeu_util, only: luavail
  use mpeu_util, only: indexSet
  use mpeu_util, only: indexSort
#else
	! USE GMAO_mpeu/
  use m_mpif90 , only: mpi_comm_world => mp_comm_world
  use m_die    , only: die, perr
  use m_stdio  , only: stderr, stdout, stdin
  use m_ioutil , only: luavail
  use m_SortingTools, only: indexSet
  use m_SortingTools, only: indexSort
#endif
  implicit none
  private	! except
  public :: MPI_comm_world
  public :: die, perr
  public :: stderr, stdout, stdin
  public :: luavail
  public :: indexSet
  public :: indexSort
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='bias_mpeu'
end module bias_mpeu
