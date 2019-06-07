program table_and_timestamp

!  Add a BUFR table and a time stamp to OMPS TC ozone data
!
!
  implicit none

  character(len=200) :: filnam
  character(len=200) :: tblnam
  character(len=10) ::  datestr
  character(len=8) :: subset

  integer :: lutbl = 14
  integer :: luout = 16
  integer :: iymdh

  integer nargs, iargc


  nargs = iargc()
  if (nargs < 4) call usage()

  call getarg(1,tblnam)
  call getarg(2,subset)
  call getarg(3,datestr)
  call getarg(4,filnam)

  open(lutbl,file=tblnam,form='formatted')


  open(luout,file=filnam,form='unformatted')

  call datelen(10)

  call openbf(luout,'OUT',lutbl)

  read(datestr,'(i10)') iymdh
  call openmg(luout,subset,iymdh)
  call closbf(luout)
  close(lutbl)

  stop
  end program table_and_timestamp
  subroutine usage
    print *,'Usage: table_and_timestamp.x tablename subset-name yyyymmddhh ', &
         'output-file'
    stop
  end subroutine usage
