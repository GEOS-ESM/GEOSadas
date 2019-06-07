module m_hurrmess
  implicit none
  private
        ! Messages from a tcvital file
  public:: NST
  public:: KSTM
  public:: ST_NAME
  public:: TCVT
  public:: clat_tcvin, clon_tcvin, stat_tcvin
  public:: hurrmess_summary     ! list

        ! values of stat_tcvin
  public:: STRM_Normal
  public:: STRM_Bogus
  public:: STRM_NoTrack
  public:: STRM_Excluded
  public:: STRM_TooClose
  public:: STRM_TooWide
  public:: STRM_ZGT500m

        ! Latitude limit of relocation action, in storm-center locations
  public:: southEx_lat, &
           northEx_lat  ! The south and north latitude limit of relocation

        ! Grid information of HDAT(:,:,:,k_Storm)
  public:: CLON_N, CLAT_N       ! time-adjusted location in degree lat-lon
  public::   IC_N,   JC_N       ! its location in grid of HDATA (background)

  public:: SLON_N, SLAT_N       ! Low-left grid corner
  public:: DLON_N, DLAT_N       ! grid intervals


!!  include "hurrmess.h"

        ! Note: The leading digit, if equals to 1, indicating an unprocessible report.
        !       The last digit, is its index to message array, stat_names(0:).

        ! Hard data flags, good for further processing or not.
      integer,parameter :: STRM_Normal   = 0    ! default status as "relocated"
      integer,parameter :: STRM_NoTrack  =11    ! no track data from the tracker
      integer,parameter :: STRM_Excluded =12    ! excluded by latitude constraint

        ! Soft data flags, data are processed, with or without relocation
      integer,parameter :: STRM_Bogus    = 3    ! flagged by the tracker, as no center reported
      integer,parameter :: STRM_TooClose = 4    ! too closed to relocate, dist<=0.2
      integer,parameter :: STRM_TooWide  = 5    ! too wide to relocate, (amdx,amdy)=0 is forced
      integer,parameter :: STRM_ZGT500m  = 6    ! Topographic > 500m, wind relocation only.

      integer,parameter:: len_message = len('Excluded')
      character(len=len_message),dimension(0:6),parameter :: stat_names = &
        (/ "Normal", "NoTrack", "Excluded", "Bogus", "TooClose", "TooWide", "ZGT500m" /)

      integer,parameter :: NST=10

      integer :: KSTM
      COMMON /NHC/ KSTM

      integer :: IC_N,JC_N
      real    :: SLON_N,SLAT_N,CLON_N,CLAT_N
      COMMON /NHC/ IC_N(NST),JC_N(NST)
      COMMON /NHC1/SLON_N(NST),SLAT_N(NST),CLON_N(NST),CLAT_N(NST)
      real,parameter:: DLON_N=1.
      real,parameter:: DLAT_N=1.

      real :: clon_tcvin,clat_tcvin
      integer:: stat_tcvin
      COMMON /NHC1/clon_tcvin(NST),clat_tcvin(NST)
      COMMON /NHC1/stat_tcvin(NST)

      CHARACTER(len=03) :: ST_NAME
      CHARACTER(len=95) :: TCVT
      COMMON /STNAME/ST_NAME(NST)
      COMMON /TCVIT/TCVT(NST)

      character(len=*),parameter:: myname="m_hurrmess"

!      integer,parameter :: NST=10
!
!      integer :: KSTM,IC_N,JC_N
!      COMMON /NHC/ KSTM,IC_N(NST),JC_N(NST)
!
!      real :: SLON_N,SLAT_N,CLON_N,CLAT_N,clon_tcvin,clat_tcvin
!      COMMON /NHC1/SLON_N(NST),SLAT_N(NST),CLON_N(NST),CLAT_N(NST),clon_tcvin(NST),clat_tcvin(NST)
!
!      CHARACTER(len=03) :: ST_NAME
!      CHARACTER(len=95) :: TCVT
!      COMMON /STNAME/ST_NAME(NST)
!      COMMON /TCVIT/TCVT(NST)

        real,save:: southEx_lat = -65.
        real,save:: northEx_lat = +65.

contains
subroutine hurrmess_summary(lu)
  implicit none
  integer,optional,intent(in):: lu
  integer:: lu_,i,j,k,l,m
  character(len=*),parameter:: myname_=myname//"::STRM_summary"
  lu_=6
  if(present(lu)) lu_=lu

  write(lu_,'(2a,i2,a)') myname_,': Number_of_vortexes_=',KSTM,' _____tcvital_message____code______________tcvin________relocation__'
!!write(lu_,'(2a,i2,a)') myname_,': Number_of_vortexes_='' _____tcvital_message____code______________tcvin________relocated___'
!!write(lu_,'(2a,i2,a)') myname_,': "MFTC 25W MEARI     20040927 0000 261N 1249E" -TooClose-     26.10  124.90   26.10  124.90'
  m=size(stat_tcvin)
  do k=1,kstm
    i=mod(stat_tcvin(k),m)
    l=len_trim(stat_names(i))
    j=max(0,len_message+2-l)
    write(lu_,'(5a,4f8.2)') myname_,': "',TCVT(k)(1:43),'" ',   &
        '-'//stat_names(i)(1:l)//'-'//repeat(' ',j),            &
        clat_tcvin(k),clon_tcvin(k), clat_n(k),clon_n(k)
  enddo
end subroutine hurrmess_summary

end module m_hurrmess
