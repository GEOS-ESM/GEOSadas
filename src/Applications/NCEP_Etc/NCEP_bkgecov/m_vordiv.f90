subroutine m_vordiv(u,v)

  use type_kinds, only: r_kind=>fp_kind, i_kind=>ip_kind
  use variables, only: nGlat, nGlon, nsig 
  use compact_diffs, only: iglobal,itotsub,ltosi,ltosj
  use compact_diffs, only: ltosi_s,ltosj_s
  use compact_diffs, only: uv2vordiv

  implicit none

! Declare passed variables
  real(r_kind),dimension(nGlat,nGlon,nsig),intent(inout):: u,v

! Declare local variables
  integer(i_kind) i,j,k,isize,i1,j1

  real(r_kind),dimension(itotsub):: work3,work4

! Initialize variables
  isize=max(iglobal,itotsub)

  if(isize.ne. nGlat*nGlon)then
    print*,"STOP: isize.ne.nGlat*nGlon"
    stop
  endif

  do k=1,nsig

    do j=1,isize
      i1=ltosi(j)
      j1=ltosj(j)
      work3(j)=u(i1,j1,k)
      work4(j)=v(i1,j1,k)
    end do

!   call u,v --> vor,div routine 
    call uv2vordiv(work3(1),work4(1))

    do j=1,isize
      i1=ltosi_s(j)
      j1=ltosj_s(j)
      u(i1,j1,k)=work3(j)  ! vorticity
      v(i1,j1,k)=work4(j)  ! divergency
    end do

  end do
    
 return
end subroutine m_vordiv
