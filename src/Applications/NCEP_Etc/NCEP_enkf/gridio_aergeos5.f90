 module gridio
!$$$  module documentation block
!
! module: gridio_aer                 subroutines for reading and writing
!                                    ensemble members files for aerosols using
!                                    EnKF internal format. 
!
! prgmmr: buchard      date: 2015
!
! abstract: I/O for ensemble member files.
!
! Public Functions:
!  readgriddata, writegriddata
!
! this version reads and writes abkg files and aod files --> AER_NV and EXT_NC
!
! Public Variables: None
!
! program history log:
!   Oct 2015 Buchard Initial version

! attributes:
!   language: f95
!
!$$$
 use constants, only: zero,one,cp,rd,grav,zero
 use params, only: nlons,nlats,reducedgrid,nvars,nlevs,pseudo_rh, &
                   cliptracers,nlons,nlats,datestring,datapath,nsfcvars,&
                   nhr_anal, expid, nbackgrounds, do_avk
 use kinds, only: i_kind,r_double,r_kind,r_single
 use gridinfo, only: npts
 use gridinfo, only: nvarhumid,nvarozone
 use gridinfo, only: logaodeps
 use mpisetup

 use MAPL_IOMod
 use PFIO
 use gftl_StringVector

 use m_tick, only: tick
 use m_die, only: die

 implicit NONE
 private
 public :: readgriddata, writegriddata
 public :: get_weight, destroy_weight, divide_weight
 integer(i_kind) :: freq
 integer(i_kind) :: nstep
 integer(i_kind), parameter :: prec=0 ! 0=32 bit file; 1=64 bit file

! Aerosol species are wired-in for now
! ------------------------------------
 integer, parameter :: nqAERO = 18
 character(len=8), dimension(nqAERO), parameter :: aer_fields = &
              (/'DU001   ', 'DU002   ', 'DU003   ', 'DU004   ', 'DU005   ', &
                'SS001   ', 'SS002   ', 'SS003   ', 'SS004   ', 'SS005   ', &
                'BCPHOBIC', 'BCPHILIC', 'OCPHOBIC', 'OCPHILIC', 'SO4     ', &
                'NO3AN1  ', 'NO3AN2  ', 'NO3AN3  '/)
 integer, parameter :: nqAOD = 1
 character(len=9), dimension(nqAOD), parameter :: aod_fields = &
              (/'AOD'/)
!             (/'TOTEXTTAU'/)

 contains
    subroutine hflip_ ( q,im,jm,km )
       implicit none
       integer  im,jm,km,i,j,k
       real, intent(inout) :: q(im,jm,km)
       real, allocatable   :: dum(:)
       allocate ( dum(im) )
       do k=1,km
       do j=1,jm
       do i=1,im/2
         dum(i) = q(i+im/2,j,k)
         dum(i+im/2) = q(i,j,k)
       enddo
         q(:,j,k) = dum(:)
       enddo
       enddo
       deallocate ( dum )
    end subroutine hflip_
    subroutine vflip_(q,im,jm,km)
       implicit none
       integer,intent(in) :: im,jm,km
       real,intent(inout) :: q(im,jm,km)
       real, allocatable  :: dum(:)
       integer i,j
       return
       allocate(dum(km))
       do j=1,jm
          do i=1,im
           dum      = q(i,j,:)
           q(i,j,:) = dum(km:1:-1)
          end do
       end do
       deallocate(dum)
    end subroutine vflip_
!----------------------------
    subroutine inq_dims_(fname, im, jm, km)
       character(len=120), intent(in) :: fname
       integer, intent(out) :: im
       integer, intent(out) :: jm
       integer, optional, intent(out) :: km

       type(NetCDF4_FileFormatter) :: formatter
       type(FileMetadata) :: metadata 
       integer :: rc
  
       call formatter%open(trim(fname),PFIO_READ,rc=rc)
       if ( rc == 0 ) then
          print *,'[r] open file:'//trim(fname)
       else
          call die('inq_dims_: ', '[x] error opening AOD background file in PFIO' //trim(fname)//', rc= ', rc)
       endif 

       metadata = formatter%read()
       im = metadata%get_dimension('lon')
       jm = metadata%get_dimension('lat')
       if(present(km)) then
          km = metadata%get_dimension('lev')
       endif

       call formatter%close()
    end subroutine inq_dims_
!-------------------------------------------------
    subroutine read_aod_(fname, aod, im, jm)
       character(len=120), intent(in) :: fname
       real(r_single),  intent(inout) :: aod(:,:,:)
       integer, intent(out) :: im
       integer, intent(out) :: jm

       type(Netcdf4_FileFormatter) :: formatter
       type(FileMetadata) :: metadata 
       integer :: rc
  
       call formatter%open(trim(fname),PFIO_READ,rc=rc)
       if ( rc == 0 ) then
          print *, '[r] open file:'//trim(fname)
       else
          call die('read_aod_: ', '[x] error opening AOD background file in PFIO' //trim(fname)//', rc= ', rc)
       endif
       metadata = formatter%read() 

       im = metadata%get_dimension('lon')
       jm = metadata%get_dimension('lat')
     
       call MAPL_VarRead(formatter,trim(aod_fields(1)),aod(:,:,1),lev=1,rc=rc)
       if ( rc == 0 ) then
           print *, '[r] reading bkg AOD from file:'//trim(fname)
       else
           call die('read_aod_: ', '[x] error reading AOD background file in PFIO' //trim(fname)//', rc= ', rc)
       endif 
 
       call hflip_(aod, im, jm, 1)
   
       call formatter%close()
    end subroutine read_aod_
!-------------------------------------------------

    subroutine read_aero_MR_(fname, x, im, jm, km, ps)
       character(len=120), intent(in) :: fname

       real(r_single), intent(inout) :: x(:,:,:,:)
       integer, intent(out) :: im
       integer, intent(out) :: jm
       integer, intent(out) :: km
       real(r_single), intent(inout) :: ps(:,:,:)
 
       type(Netcdf4_FileFormatter) :: formatter
       type(FileMetadata) :: metadata 
       integer :: rc, nq, k, nVars_


      !  Read dimensions and aerosol mixing ratio from abkg files
      !  --------------------------
    
       call formatter%open(trim(fname),PFIO_READ,rc=rc)
       if ( rc == 0 ) then
          print *, '[r] open file:'//trim(fname)
       else
          call die('read_aero_MR_: ', '[x] error opening MR background file in PFIO' //trim(fname)//', rc= ', rc)
       endif 

       metadata = formatter%read()
 
       im = metadata%get_dimension('lon')
       jm = metadata%get_dimension('lat')

     ! Read MR for each aer_fields specified in rc file 
     ! ----------------------------
       do nq = 1, nqAERO
          do k = 1, km
             call MAPL_VarRead(formatter, trim(aer_fields(nq)), x(:,:,k,nq), lev=k ,rc=rc)
          enddo      
       enddo   
       if ( rc == 0 ) then
          print *, '[r] reading aer MR from file:'//trim(fname)
       else
          call die('read_aero_MR_: ', '[x] error reading MR in background file in PFIO' //trim(fname)//', rc= ', rc)
       endif 

     ! Read PS to put at the bottom of the state vector
     ! ----------------------------
       call MAPL_VarRead(formatter, "PS", ps(:,:,1), lev=1, rc=rc)
       if ( rc == 0 ) then
            print *, '[r] reading PS from file:'//trim(fname)
       else
            call die('read_aero_MR_: ', '[x] error reading PS in background file in PFIO' //trim(fname)//', rc= ', rc)
       endif 
 
     ! Convert GMAO units to NCEP units, flip horizontally and vertically
     ! ---------------------------
       do nq = 1, nqAERO
          call hflip_(x(:,:,:,nq), im, jm, km)
          call vflip_(x(:,:,:,nq), im, jm, km)
       end do

       call hflip_(ps, im, jm, 1)

     ! Clean up
     ! ------
       call formatter%close(rc=rc)
       if ( rc /= 0 ) then
          call die('read_aero_MR_: ', '[x] error closing file in PFIO' //trim(fname)//', rc= ', rc)
       endif   
    end subroutine read_aero_MR_ 
! ---------------------------------------------------------

    subroutine write_aod_(fnamei,fnameo,aod,im,jm,km)
        character(len=120), intent(in) :: fnamei
        character(len=120), intent(in) :: fnameo
        integer, intent(in) :: im
        integer, intent(in) :: jm
        integer, intent(in) :: km
        real(r_single), intent(in) :: aod(im,jm,km) 
        type(Netcdf4_FileFormatter) :: lui,luo
        type(FileMetadata) :: lui_meta, luo_meta

        integer rc

        call lui%open(trim(fnamei),PFIO_READ)
        if ( rc == 0 ) then
           print *, '[r] open file:'//trim(fnamei)
        else
          call die('write_aod_: ', '[x] error opening AOD background file in PFIO' //trim(fnamei)//', rc= ', rc)
        endif
        lui_meta=lui%read()
        call MAPL_IOChangeRes(lui_meta,luo_meta,['lon','lat','lev'],[im,jm,km] ) 

        call luo%create(trim(fnameo))
        call luo%write(luo_meta)
!    call hflip_(aod, im, jm, 1)
        call MAPL_VarWrite(luo,trim(aod_fields(1)),aod(:,:,1),lev=1)

        call lui%close(rc=rc)
        if ( rc /= 0 ) then
           call die('write_aod_: ', '[x] error closing AOD background file in PFIO' //trim(fnamei)//', rc= ', rc)
        endif 
        call luo%close(rc=rc)
        if ( rc /= 0 ) then
           call die('write_aod_: ', '[x] error closing AOD update file in PFIO' //trim(fnameo)//', rc= ', rc)
        endif 
    end subroutine write_aod_
!-----------------------------------------------

    subroutine write_aero_MR_(fnamei,fnameo, im,jm,km, x,ps)
       character(len=120), intent(in) :: fnamei
       character(len=120), intent(in) :: fnameo
       integer, intent(in) :: im 
       integer, intent(in) :: jm
       integer, intent(in) :: km
       real(r_single), intent(in) :: x(:,:,:,:) 
       real(r_single), intent(in) ::ps(im,jm,1)

       type(NetCDF4_FileFormatter) :: aer_bkg, aer_bkg_upd
       type(FileMetadata) :: aer_bkg_meta, aer_bkg_upd_meta
       character(len=20), allocatable :: name_(:)
       real(r_single), allocatable :: fields(:,:,:,:)
       logical, allocatable :: mask(:)
       integer, allocatable :: nrank(:)
       integer :: rc, nq, k, nVars_, n, nrk
       integer :: dimsize(4)
       type(StringVector), pointer :: vars
       type(StringVariableMap), pointer :: variables
       type(Variable), pointer :: myVar
       type(StringVectorIterator) :: siter
       type(StringVector), pointer :: var_dims 
       character(len=:), pointer :: var_name

  
       call aer_bkg%open(trim(fnamei),PFIO_READ,rc=rc)
       if ( rc == 0 ) then
          print *, '[r] open file for writing:'//trim(fnamei)
       else
          call die('write_aero_MR_: ','[x] error opening MR background file in PFIO for writing' //trim(fnamei)//', rc= ', rc)
       endif 
       aer_bkg_meta = aer_bkg%read()
       vars = MAPL_IOGetNonDimVars(aer_bkg_meta)
       nVars= vars%size()
       allocate(name_(nVars_))
 
       allocate(fields(im,jm,km,nVars_))
       allocate(nrank(nVars_))
       siter = vars%begin()
       variables => aer_bkg_meta%get_variables()
       do while (siter /= vars%end())
          var_name => siter%get()
          myVar => variables%at(var_name)
          var_dims => myVar%get_dimensions()
          nrk = var_dims%size()

          nrank(n)=nrk
          if (nrk == 3) then
             call MAPL_VarRead(aer_bkg, name_(n), fields(:,:,1,n), lev =1, rc=rc )
          else if (nrk == 4) then
             do k = 1, km 
                call MAPL_VarRead(aer_bkg, name_(n), fields(:,:,k,n), lev =k, rc=rc )             
             enddo
          endif     
       enddo  

       call MAPL_IOChangeRes(aer_bkg_meta, aer_bkg_upd_meta,['lon','lat','lev'],[im,jm,km])
       call aer_bkg_upd%create(trim(fnameo))
       call aer_bkg_upd%write(aer_bkg_upd_meta)

       allocate(mask(nVars_))
       mask = .True.   ! mask for all fields not in aer_fields (to cp them in file)
       do n =1, nVars_
          mask(n) = .NOT.(any(name_(n)==aer_fields(:)))
       enddo
       print*, 'mask', mask

    ! Write all aer MR updated after analysis in file
    ! ------------------------
       do nq = 1, nqAERO
          do k = 1, km
             call MAPL_VarWrite(aer_bkg_upd,trim(aer_fields(nq)),x(:,:,k,nq),lev=k,rc=rc)
          enddo
       enddo

       if ( rc == 0 ) then
          print *, '[w] write MR updated in file '//trim(fnameo)
       else
          call die('write_aero_MR_: ','[x] error writing aerosol MR analysis file ' //trim(fnameo)//', rc= ', rc)
       endif 
  
     ! Write all other fields in file not updated by the analysis
     ! -----------------------------
       do n = 1, nVars_  
          if (mask(n) == .True.) then
             if (nrank(n) == 3) then
                 if(name_(n) == 'PS') then
                   call MAPL_VarWrite(aer_bkg_upd,'PS',ps(:,:,1),lev=1,rc=rc)
                 else
                   call MAPL_VarWrite(aer_bkg_upd,name_(n),fields(:,:,1,n),lev=1,rc=rc)
                 endif   
             else if (nrank(n) == 4) then
                  do k = 1, km
                     call MAPL_VarWrite(aer_bkg_upd,name_(n),fields(:,:,k,n),lev=k,rc=rc)
                  enddo
             endif
         endif
       enddo

       if ( rc == 0 ) then
          print *, '[w] write supplement fields in file (not updated by analysis) '//trim(fnameo)
       else
          call die('write_aero_MR_: ','[x] error writing aer supplement fields ' //trim(fnameo)//', rc= ', rc)
       endif 
 
     ! clean up
     ! ------
       call aer_bkg%close(rc=rc)
       if ( rc /= 0 ) then
          call die('write_aero_MR_: ','[x] error closing aer_bkg file, rc= ', rc)
       endif   

       call aer_bkg_upd%close(rc=rc)
       if ( rc /= 0 ) then
          call die('write_aero_MR_: ','[x] error closing aer_ana file, rc= ', rc)
       endif   

       deallocate(name_)
       deallocate(fields)
       deallocate(mask)
       deallocate(nrank)
    end subroutine write_aero_MR_
!--------------------------------------------------------------
!-----------------------------READGRIDDATA---------------------
!--------------------------------------------------------------
    subroutine readgriddata(nanal,ft,mode,grdin,qsat,infilename)
       implicit none

       integer, intent(in) :: nanal                                         !  ensemble member counter
       integer, intent(in) :: ft
       integer, intent(in) :: mode
       real(r_single), dimension(npts,nvars*nlevs+nsfcvars,nbackgrounds), intent(out) :: grdin  !  all fields of interest lined up
                                                                       !    ps must be last
       real(r_double), dimension(npts,nlevs,nbackgrounds), intent(out), optional :: qsat           !  saturation q
       character(len=*), intent(in), optional :: infilename
!
       character(len=*), parameter:: myname_='readgriddata'
       character(len=120) :: abkgfn
       character(len=120) :: aodfn 
       character(len=3) charnanal

       real(r_single),allocatable :: aod(:,:,:)
       real(r_single),allocatable :: x_f(:,:,:,:)
       real(r_single),allocatable :: ps(:,:,:)

       logical :: do_AOD, do_MR 

       character(len=12) geosdate
       character(len=14) geosdate2
 
       integer nymd, nhms
       integer nymd0,nhms0
       integer nsfc, nq, rc
       integer i,j,k,im,jm,km,nb

       nvarhumid=0
       nvarozone=0

       write(charnanal,'(i3.3)') nanal ! ensemble member count

       nsfc=nsfcvars
 
       if (nsfc>=1  .and. nvars == 0) then
           do_AOD = .true.
           do_MR = .false.
       else
           do_MR = .true.
           do_AOD = .false.
       endif
 
       ! Read analysis date and tick clock to previous analysis time
       ! -----------------------------------------------------------
       read(datestring,'(i8,i2)') nymd0, nhms0
       nhms0 = nhms0 * 10000
       call tick (nymd0,nhms0,-6*3600)

       nb=1
       do while (nhr_anal(nb) > 0)
          nymd=nymd0;nhms=nhms0
          call tick (nymd,nhms,nhr_anal(nb)*3600)
          write(geosdate ,'(i8.8,a,i2.2,a)') nymd, '_', nhms/10000, 'z'
          write(geosdate2,'(i8.8,a,i4.4,a)') nymd, '_', nhms/100, 'z'

          ! AOD ANALYSIS
          ! ------------
          if (do_AOD) then
       
             if(trim(aod_fields(1))=='AOD') then
                aodfn = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".aod_f.sfc."//geosdate2//".nc4"
             else
                aodfn = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".gaas_bkg.sfc."//geosdate//".nc4"
             endif
            ! inquire dimensions
             call inq_dims_(aodfn, im, jm)

            ! read in AOD
             allocate(aod(im,jm,1))
             call read_aod_(aodfn, aod, im, jm)
         
            ! Consistency check
            ! ----------------
             if (npts /= im*jm) then
                print *,'[x] error, inconsistent input file - im*jm != ', im*jm
                call stop2(99)
             end if
      
            ! Fill in state vector grdin
            ! ------------------
             if (logaodeps>0.0_r_single) then
                grdin(:,1,nb) = log(reshape(aod(:,:,1),(/npts/))+logaodeps)
             else
                grdin(:,1,nb) = reshape(aod(:,:,1),(/npts/))
             endif
             deallocate(aod)

          endif

          ! MR ANALYSIS
          ! -----------
          if ( do_MR ) then

             abkgfn = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".abkg.eta."//geosdate//".nc4"

             ! inquire dimensions
             ! ------------------
             call inq_dims_(abkgfn, im, jm, km=km)

             ! read in data
             ! ------------
             allocate(x_f(im,jm,km,nqAERO))
             allocate(ps(im,jm,1))
             call read_aero_MR_(abkgfn, x_f, im, jm, km, ps)  ! return im, jm, km and x_f aer MR and ps
    
            ! consistency check
            ! -----------------
             if (nlevs /= km) then
                 print *,'[x] error, inconsistent input file in readgridddata - nlevs != ', km
                 call stop2(99)
             end if
             if (npts /= im*jm) then ! original code more general than this
                 print *,'[x] error, inconsistent input file in readgriddata- im*jm != ', im*jm
                 call stop2(99)
             end if
             if (nvars /= nqAERO) then ! original code more general than this
                 print *,'[x] error, inconsistent input file in readgriddata - nvars != ', nqAERO
                 call stop2(99)
             end if
       
            ! Fill in state vector (grdin,nlevs,nvars,npts), ps in hPa at the bottom
            ! ----------------------------------------------------------------------
             do nq = 1, nqAERO
                do k = 1, nlevs
                   grdin(:, (nq-1)*nlevs +k,nb) = max(0.0,reshape(x_f(:,:,k,nq),(/npts/)))
                enddo
             enddo
             if (nsfc == 1)  grdin(:,nvars*nlevs+nsfc,nb) = reshape(ps(:,:,1),(/npts/))*0.01 ! in hPa

             deallocate(x_f)
             deallocate(ps)
   
          endif
   
          if (present(qsat)) then
             qsat(:,:,nb) = 1._r_double ! is an output argument so set to 1 (used if  nvarhumid > 1)
          endif

          nb = nb + 1 ! next background index
       enddo
     
    end subroutine readgriddata
!------------------------------------------------------
!------------------------------------------------------
!-----------------------_WRITEGRIDDATA-----------------
!------------------------------------------------------
    subroutine writegriddata(nanal,grdin,no_inflate_flag)
       use m_set_eta, only: set_eta
       implicit none

       integer, intent(in) :: nanal
       real(r_single), dimension(npts,nvars*nlevs+nsfcvars,nbackgrounds), intent(inout) :: grdin
       logical, intent(in) :: no_inflate_flag

       character(len=*), parameter:: myname_='writegriddata'

       real(r_single), allocatable :: x_f(:,:,:,:), x_a(:,:,:,:)
       real(r_single), allocatable :: ps(:,:,:)
       real(r_single), allocatable :: ps_(:,:,:)

       character(len=3) charnanal
       character(len=120):: aodfn, aodfnw
       character(len=120):: abkgfn, abkgfnw
       character(len=12) geosdate
       character(len=14) geosdate2

       integer nymd, nhms
       integer nymd0,nhms0
       integer im,jm,km, i,j,k, nq, nsfc, nb
       real(r_single),allocatable :: bkg_aod(:,:,:), ana_aod(:,:,:)

       logical,parameter:: upddelp=.false. ! to avoid conflict w/ meteorology upd; do not update delp
       logical,parameter:: wrtinc=.false.
       logical :: do_AOD, do_MR

       write(charnanal,'(i3.3)') nanal

       nsfc=nsfcvars
 
       if (nsfc>=1  .and. nvars == 0) then
            do_AOD = .true.
            do_MR = .false. 
       else
            do_MR = .true.
            do_AOD = .false.
       endif
 
       ! Read analysis date and tick clock to previous analysis time
       ! -----------------------------------------------------------
       read(datestring,'(i8,i2)') nymd0, nhms0
       nhms0 = nhms0 * 10000
       call tick (nymd0,nhms0,-6*3600)

       nb=1
       do while (nhr_anal(nb) > 0)
          nymd=nymd0;nhms=nhms0
          call tick (nymd,nhms,nhr_anal(nb)*3600)
          write(geosdate ,'(i8.8,a,i2.2,a)') nymd, '_', nhms/10000, 'z'
          write(geosdate2,'(i8.8,a,i4.4,a)') nymd, '_', nhms/100, 'z'

          ! AOD ANALYSIS
          ! ------------
          if (do_AOD) then
        
             if(trim(aod_fields(1))=='AOD') then
                aodfn = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".aod_f.sfc."//geosdate2//".nc4"
             else
                aodfn = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".gaas_bkg.sfc."//geosdate//".nc4"
             endif
            ! inquire dimensions
             call inq_dims_(aodfn, im, jm)

             allocate(bkg_aod(im,jm,1))
             allocate(ana_aod(im,jm,1))
             call read_aod_(aodfn, bkg_aod, im, jm)
    
!            As in the PSAS-based analysis, write out both increment and analysis
!            --------------------------------------------------------------------

!            Increment first ...
!            --------------------
             ana_aod(:,:,1) = reshape(grdin(:,1,nb),(/im,jm/))
             call hflip_(ana_aod, im, jm, 1)
             if(trim(aod_fields(1))=='AOD') then
                if (do_avk) then
                   aodfnw = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".aod_k.sfc."//geosdate2//".mem"//charnanal//".nc4"
                else
                   aodfnw = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".aod_d.sfc."//geosdate2//".mem"//charnanal//".nc4"
                endif
             else
                aodfnw = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".gaas_inc.sfc."//geosdate//".mem"//charnanal//".nc4"
             endif
             call write_aod_(aodfn, aodfnw, ana_aod, im, jm, 1)

!            then analysis ...
!            -----------------
             if (.not.do_avk ) then

                if (logaodeps>0.0_r_single) then
                   ana_aod(:,:,1) = log(bkg_aod(:,:,1)+logaodeps) + reshape(grdin(:,1,nb),(/im,jm/))
                   ana_aod(:,:,1) = max(0.0,exp(ana_aod(:,:,1))-logaodeps)
                else
                   ana_aod(:,:,1) = bkg_aod(:,:,1) + max(reshape(grdin(:,1,nb),(/im,jm/)),-bkg_aod(:,:,1))
                endif
                call hflip_(ana_aod, im, jm, 1)

                if(trim(aod_fields(1))=='AOD') then
                   aodfnw = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".aod_a.sfc."//geosdate2//".mem"//charnanal//".nc4"
                else
                   aodfnw = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".gaas_ana.sfc."//geosdate//".mem"//charnanal//".nc4"
                endif
                call write_aod_(aodfn, aodfnw, ana_aod, im, jm, 1)

             endif

             deallocate(ana_aod)
             deallocate(bkg_aod)
          endif

          ! MR ANALYSIS
          !---------------
          if (do_MR) then
      
             abkgfn = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".abkg.eta."//geosdate//".nc4"
             ! inquire dimensions
             call inq_dims_(abkgfn, im, jm, km=km)

             ! read in variables
             allocate(x_f(im,jm,km,nqAERO))
             allocate(ps(im,jm,1))
             call read_aero_MR_(abkgfn, x_f, im, jm, km, ps)
   
             allocate(x_a(im,jm,km,nqAERO))

             if ( wrtinc .or. do_avk ) then
                do nq = 1, nqAERO
                   do k=1,nlevs
                      x_a(:,:,k,nq) = reshape(grdin(:, (nq-1)*nlevs + k,nb),(/im,jm/))
                   enddo   
                enddo
             else
                do nq = 1, nqAERO
                   do k=1,nlevs
                      x_a(:,:,k,nq) = x_f(:,:,k,nq) + max(reshape(grdin(:, (nq-1)*nlevs + k,nb),(/im,jm/)),-x_f(:,:,k,nq))
                   enddo   
                enddo
             endif

            do nq = 1, nqAERO
               call hflip_(x_a(:,:,:,nq), im, jm, km)
               call vflip_(x_a(:,:,:,nq), im, jm, km)
            end do

            allocate(ps_(im,jm,1))
            if (nsfc==1) then
               ps_(:,:,1) = ps(:,:,1)+100.0*reshape(grdin(:,nvars*nlevs+nsfc,nb),(/im,jm/))
            else
               ps_(:,:,1) = ps(:,:,1)
            endif
            call hflip_(ps_,im,jm,1)

            if ( do_avk ) then
                abkgfnw = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".aker.eta."//geosdate//".mem"//charnanal//".nc4"
            else
                abkgfnw = trim(adjustl(datapath))//"mem"//charnanal//"/"//trim(expid)//".aana.eta."//geosdate//".mem"//charnanal//".nc4"
            endif
            print *, 'writegridata output =', abkgfn
            call write_aero_MR_(abkgfn, abkgfnw, im, jm, km, x_a, ps_)
 
            deallocate( x_a )
            deallocate( x_f )
            deallocate( ps )

          endif  

          nb = nb + 1 ! next background index
       enddo 
    
    end subroutine writegriddata

    subroutine get_weight
    end subroutine get_weight

    subroutine destroy_weight
    end subroutine destroy_weight

    subroutine divide_weight(grdin)
    use params, only: ndim
    use loadbal, only: npts_max
    implicit none
    real(r_single), dimension(npts_max,ndim), intent(inout) :: grdin
    end subroutine divide_weight

 end module gridio
