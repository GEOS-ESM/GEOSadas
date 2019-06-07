module m_sisind
!
!	SIS: satellite/instrument/sensor indicator
!
  implicit none
  private
  public :: sisind_get	! convert a nusat number to SIS

  interface sisind_get; module procedure sat2sis_; end interface

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
case(5:12,14)
  isat=nusat
  write(sisname,'(a,i2.2)') 'hirs2_n',isat
case(15:17)
  isat=nusat
  write(sisname,'(a,i2.2)') 'hirs3_n',isat
case(18)
  isat=nusat
  write(sisname,'(a,i2.2)') 'hirs4_n',isat
case(49)
  isat=nusat
  sisname='airs281SUBSET_aqua'
case(58,60,62)
  isat=nusat-50
  write(sisname,'(a,i2.2)') 'sndr_g',isat
case(205:212,214)
  isat=nusat-200
  write(sisname,'(a,i2.2)') 'msu_n',isat
case(258,260,262)
  isat=nusat-250
  write(sisname,'(a,i2.2)') 'imgr_g',isat
case(305:312,314)
  isat=nusat-300
  write(sisname,'(a,i2.2)') 'ssu_n',isat
case(315:317)
  isat=nusat-300
  write(sisname,'(a,i2.2)') 'amsua_n',isat
case(318)
  isat=nusat-300
  write(sisname,'(a,i2.2)') 'amsua_n',isat
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

case default
  isat=nusat
  write(sisname,'(a,i3.3)') 'unknown_',isat
  if(present(stat)) stat=-1
end select
end subroutine sat2sis_
end module m_sisind
