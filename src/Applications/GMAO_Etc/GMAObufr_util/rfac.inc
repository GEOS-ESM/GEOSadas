! ... Include file to set record length factor for the RECL keyword.
! ... This version is for compilers where RECL is in *** BYTES ***.

! ... Systems with compilers where this is the default include:
! ... HP-UX
! ... Sun Solaris
! ... IBM AIX
! ... SGI MIPS (with "-bytereclen" compiler switch)
! ... INTEL UNIX (with "-assume byterec" ifort compiler switch)

! ... The parameter RFAC is used as a divisor for the record length
! ... variable in all open statements which use the RECL keyword, thus
! ... all record lengths in these modules are defined in BYTES.

      integer, parameter ::  rfac = 1
