module m_sisind
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_sisind
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2012-07-10
!
! abstract: convert old NCEP nusat code to SIS (satellite/instrument/
!           sensor indicator).
!
! program history log:
!   2012-07-10  Jing Guo <jguo@nasa.gov>
!               . fixed incorrect name conversion to n05 (NOAA-5) back
!                 to tirosn (TIROS-N).
!		- added this document block
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

  implicit none
  private
  public :: sisind_get	! convert a nusat number to SIS

  interface sisind_get; module procedure sat2sis_; end interface

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='m_sisind'

contains
subroutine sat2sis_(nusat,sisname,stat)
!-- map old numerical nusat values to new sis values.
  implicit none
  integer,intent(in) :: nusat
  character(len=*),intent(out) :: sisname
  integer,optional,intent(out) :: stat

  integer :: isat

if(present(stat)) stat=0
select case(nusat)
case(5)
  isat=nusat
  sisname='hirs2_tirosn'
case(6:12,14)
  isat=nusat
  write(sisname,'(a,i2.2)') 'hirs2_n',isat
case(15:17)
  isat=nusat
  write(sisname,'(a,i2.2)') 'hirs3_n',isat
case(18)
  isat=nusat
  write(sisname,'(a,i2.2)') 'hirs4_n',isat
case(25)
  write(sisname,'(a)') 'hirs4_metop-a'
case(26)
  write(sisname,'(a)') 'hirs4_metop-b'
case(27)
  write(sisname,'(a)') 'hirs4_metop-c'
case(49)
  isat=nusat
  sisname='airs281SUBSET_aqua'
case(58,60,62)
  isat=nusat-50
  write(sisname,'(a,i2.2)') 'sndr_g',isat
case(205)
  isat=nusat-200
  sisname='msu_tirosn'
case(206:212,214)
  isat=nusat-200
  write(sisname,'(a,i2.2)') 'msu_n',isat
case(258,260,262)
  isat=nusat-250
  write(sisname,'(a,i2.2)') 'imgr_g',isat
case(305)
  isat=nusat-300
  sisname='ssu_tirosn'
case(306:312,314)
  isat=nusat-300
  write(sisname,'(a,i2.2)') 'ssu_n',isat
case(315:317)
  isat=nusat-300
  write(sisname,'(a,i2.2)') 'amsua_n',isat
case(318)
  isat=nusat-300
  write(sisname,'(a,i2.2)') 'amsua_n',isat
case(325)
  write(sisname,'(a)') 'amsua_metop-a'
case(326)
  write(sisname,'(a)') 'amsua_metop-b'
case(327)
  write(sisname,'(a)') 'amsua_metop-c'
case(349)
  isat=nusat-300
  sisname='amsua_aqua'
case(415:417)
  isat=nusat-400
  write(sisname,'(a,i2.2)') 'amsub_n',isat
case(418)
  isat=nusat-400
  write(sisname,'(a,i2.2)') 'mhs_n',isat
case(449)
  isat=nusat-400
  sisname='hsb_aqua'
case(516)
  isat=nusat-500
  write(sisname,'(a,i2.2)') 'ssmis_f16'
case(549)
  isat=nusat-500
  sisname='amsre_aqua'
case(616:618)
  isat=nusat-600
  write(sisname,'(a,i2.2)') 'avhrr3_n',isat
case(708,710,711,713:715)
  isat=nusat-700
  write(sisname,'(a,i2.2)') 'ssmi_f',isat
case(825)
  write(sisname,'(a)') 'mhs_metop-a'
case(826)
  write(sisname,'(a)') 'mhs_metop-b'
case(827)
  write(sisname,'(a)') 'mhs_metop-c'
case(875)
  write(sisname,'(a)') 'iasi616_metop-a'
case(876)
  write(sisname,'(a)') 'iasi616_metop-b'
case(877)
  write(sisname,'(a)') 'iasi616_metop-c'

case default
  isat=nusat
  write(sisname,'(a,i3.3)') 'unknown_',isat
  if(present(stat)) stat=-1
end select
end subroutine sat2sis_
end module m_sisind
