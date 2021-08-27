program ut_sst_berror

use sstmod, only: create_sstvars
use sstmod, only: destroy_sstvars
use sstmod, only: sst_stats
use variables, only: create_grids
use variables, only: nlat,nlon

implicit none
integer, parameter :: im=1152
integer, parameter :: jm=721
integer, parameter :: jcap=254

nlat=jm;nlon=im ! quick initialization of variables in common block

call create_grids (nlat,nlon,jcap,.false.)
call create_sstvars(nlat,nlon)
call sst_stats
call destroy_sstvars

end program ut_sst_berror
