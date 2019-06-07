!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: trace.H - macros for code tracing.
!
! !DESCRIPTION:
!
!   This header file contains following macros:
!
!	_ENTRY_		: used at the entry point of a routine
!	_EXIT_		: used at the exit of a routine
!	_TRACE_(s)	: used with a string message s=""
!	_SHOW_("v") v	: write out variable v.
!
! !INTERFACE:
!
!   The interface of this header file is better represented in the
! following example:
!
!   #include "mytrace.H"
!   implicit none
!	! myname_ is required for all macros.
!   character(len=*),parameter :: myname_='m_this::that'
!				! if turned on (with -DDEBUG_TRACE),
!   _ENTRY_			! produces "m_this::that: entered";
!   _TRACE("checkpoint")	! produces "m_this::that: checkpoint";
!   _EXIT_			! produces "m_this::that: exiting".
!
! !REVISION HISTORY:
! 	16Feb05	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

	! These macros are always redefined, such that one may toggle
	! between turn them on or off.

#ifdef _ENTRY_
#undef _ENTRY_
#endif
#ifdef _EXIT_
#undef _EXIT_
#endif
#ifdef _TRACE_
#undef _TRACE_
#endif
#ifdef _SHOW_
#undef _SHOW_
#endif

	! NOw define all of them, if trace is turned on by -DDEBUG_TRACE
#ifdef DEBUG_TRACE

#define _ENTRY_ write(*,'(1x,2a)') myname_,'(): entered'
#define _EXIT_  write(*,'(1x,2a)') myname_,'(): exiting'
#define _TRACE_(EX) write(*,'(1x,3a)') myname_,'(): ',EX
#define _SHOW_(EX) write(*,*) myname_,'(): ',EX,' =',
#define _SHOWX_(E,X) write(*,*) myname_,'(): ',E,' =',X

	! Or define them to null, if DEBUG_TRACE is not defined.
#else
#define _ENTRY_
#define _EXIT_
#define _TRACE_(EX)
#define _SHOW_(EX) !!write(*,*) myname_,'(): ',EX,' =',
#define _SHOWX_(E,X)

#endif

	! The following macro is always defined.
#ifndef _ALWAYS_TRACE_
#define _ALWAYS_TRACE_(EX) write(*,'(1x,3a)') myname_,'(): ',EX
#endif
#ifndef _ALWAYS_SHOWX_
#define _ALWAYS_SHOWX_(E,X) write(*,*) myname_,'(): ',E,' =',X
#endif

