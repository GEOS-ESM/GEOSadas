program testtbl
  use m_sischnTable, only: sischnTable
  use m_sischnTable, only: sischnTable_build
  use m_sischnTable, only: sischnTable_locate
  use m_sischnTable, only: sischnTable_clean

  implicit none

  character(len=20) sis(100), sval
  integer           ichn(100), ival
  integer ich, istat, ii, i, nin, ier

  character(len=200) infile
  type(sischnTable):: tabl
  real :: tlap, c_ang

  call getarg(1,infile)

  open(10,file=infile,form='formatted')

  nin=0
  do i = 1,100

     read(10,'(I5,1x,A20,1x,I5,e15.6/9(4x,10f7.3/))',iostat=ier) ich, &
          sis(i), ichn(i), tlap, (c_ang,ii=1,90)

     if(ier/=0) exit
     nin=i

  end do



  call sischnTable_build(tabl,sis(1:nin),ichn(1:nin))

  istat = 0

  do while ( istat == 0 )

     print *,'Enter a sis/ich pair'
     read(*,*,iostat=istat) sval,ival
     if (istat /= 0) exit

     ii = sischnTable_locate(tabl,sval,ival)


     if (ii < 1 .or. ii > nin) then
        print *,'did not find ',sval,ival
     else if ( sis(ii) == sval .and. ichn(ii) == ival) then
        print *,'found ',sval,ival,'at index ',ii
     else
        print *,'returned wrong index ',ii, sis(ii), ichn(ii)
     end if

  end do

  call sischnTable_clean(tabl)

  stop
  end program testtbl

