subroutine m_rdgrids(nn,npes,mype,mycases)

  use type_kinds, only: fp_kind

  use variables, only: nlat,nlon,nsig
  use variables, only: bbt,bbs,bbv,bbp
  use variables, only: filunit
  use variables, only: grdsf,grdvp,gridt,grdc,gridq,grdrh,gridp,grdoz
  use variables, only: gridqi,gridql,gridqr,gridqs
  use variables, only: biasrm 
  use variables, only: hydromet 

  integer,intent(in) :: nn,npes,mype,mycases

  integer ierror

  if(nn>mycases)then
    write(6,*)' case distributions are not correct'
    stop
  endif

  filunit = 1000*(mype+1)+nn

  open(filunit,form='unformatted')
  rewind(filunit)
  if (hydromet) then
     read(filunit)grdsf,grdvp,gridt,gridp,gridq,gridqi,gridql,gridqr,gridqs,grdrh,grdoz,grdc
  else
     read(filunit)grdsf,grdvp,gridt,gridp,gridq,grdrh,grdoz,grdc
  endif
  close(filunit)

  if (.not. biasrm) then ! remove the mean only
    gridp = gridp - bbp
    gridt = gridt - bbt 
    grdvp = grdvp - bbv
    grdsf = grdsf - bbs 
    grdq  = grdq  - bbq 
    if (hydromet) then
       grdqi = grdqi - bbqi
       grdql = grdql - bbql
       grdqr = grdqr - bbqr
       grdqs = grdqs - bbqs
    endif
    grdoz = grdoz - bboz 
  endif

  return
end subroutine m_rdgrids
