      program getsfcensmean
! create ensemble mean NCEP GFS surface file.
      USE SFCIO_MODULE
      implicit none
      TYPE(SFCIO_HEAD) :: SFCHEADI,SFCHEADO
      TYPE(SFCIO_DATA) :: SFCDATAI,SFCDATAO
      character*120 filenamesfcin,filenamesfcout,datapath,fileprefix
      character*3 charnanal
      integer nsfci,nsfco,iret,nanals,nanal
      NSFCI=21
      NSFCO=61
      call getarg(1,datapath)
      call getarg(2,filenamesfcout)
      call getarg(3,fileprefix)
      call getarg(4,charnanal)
      read(charnanal,'(i2)') nanals
      filenamesfcout = trim(adjustl(datapath))//filenamesfcout
      print *,filenamesfcout
      do nanal=1,nanals
         write(charnanal,'(i3.3)') nanal
         filenamesfcin = trim(adjustl(datapath))// &
        trim(adjustl(fileprefix))//'_mem'//charnanal
         !print *,filenamesfcin
         call sfcio_srohdc(nsfci,filenamesfcin,sfcheadi,sfcdatai,iret)
         !print *,trim(filenamesfcin),iret
         if (nanal .eq. 1) then
!   sfcio_data        Surface file data fields
!     tsea              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       surface temperature in K
!     smc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       soil volumetric water content in fraction
!     sheleg            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       snow depth in m
!     stc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       soil temperature in K
!     tg3               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       deep soil temperature in K
!     zorl              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       roughness in cm
!     cv                Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud cover in fraction
!     cvb               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud bottom in kpa
!     cvt               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud top in kpa
!     alvsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for visible scattered in fraction
!     alvwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for visible beam in fraction
!     alnsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for near-IR scattered in fraction
!     alnwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for near-IR beam in fraction
!     slmsk             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       sea-land-ice mask (0-sea, 1-land, 2-ice)
!     vfrac             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       vegetation fraction in fraction
!     canopy            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       canopy water in m
!     f10m              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       10-meter wind speed over lowest model wind speed
!     t2m               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       2-meter temperature in K
!     q2m               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       2-meter specific humidity in kg/kg
!     vtype             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       vegetation type in integer 1-13
!     stype             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       soil type in integer 1-9
!     facsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in fraction
!     facwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in fraction
!     uustar            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     ffmm              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     ffhh              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     hice              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     fice              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     tisfc             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     tprcp             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     srflag            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     snwdph            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     slc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       xxx in xxx
!     shdmin            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     shdmax            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     slope             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       slope type
!     snoalb            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     orog              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       orography in m
            call sfcio_aldata(sfcheadi,sfcdatao,iret)
            !print *,'aldata',iret
            sfcheado = sfcheadi
	    sfcdatao%tsea	=sfcdatai%tsea	/float(nanals)
	    sfcdatao%smc	=sfcdatai%smc	/float(nanals)
	    sfcdatao%sheleg	=sfcdatai%sheleg/float(nanals)
	    sfcdatao%stc	=sfcdatai%stc	/float(nanals)
	    sfcdatao%tg3 	=sfcdatai%tg3	/float(nanals)
	    sfcdatao%zorl	=sfcdatai%zorl  /float(nanals)
	    sfcdatao%cv		=sfcdatai%cv	/float(nanals)
	    sfcdatao%cvb	=sfcdatai%cvb	/float(nanals)
	    sfcdatao%cvt	=sfcdatai%cvt	/float(nanals)
	    sfcdatao%alvsf	=sfcdatai%alvsf /float(nanals)
	    sfcdatao%alvwf	=sfcdatai%alvwf /float(nanals)
	    sfcdatao%alnsf	=sfcdatai%alnsf /float(nanals)
	    sfcdatao%alnwf	=sfcdatai%alnwf /float(nanals)
	    sfcdatao%slmsk	=sfcdatai%slmsk
	    sfcdatao%vfrac	=sfcdatai%vfrac /float(nanals)
	    sfcdatao%canopy	=sfcdatai%canopy/float(nanals)
	    sfcdatao%f10m	=sfcdatai%f10m  /float(nanals)
	    sfcdatao%t2m	=sfcdatai%t2m  /float(nanals)
	    sfcdatao%q2m	=sfcdatai%q2m  /float(nanals)
	    sfcdatao%vtype	=sfcdatai%vtype 
	    sfcdatao%stype	=sfcdatai%stype 
	    sfcdatao%facsf	=sfcdatai%facsf /float(nanals)
	    sfcdatao%facwf	=sfcdatai%facwf /float(nanals)
	    sfcdatao%uustar	=sfcdatai%uustar/float(nanals)
	    sfcdatao%ffmm	=sfcdatai%ffmm  /float(nanals)
	    sfcdatao%ffhh	=sfcdatai%ffhh  /float(nanals)
	    sfcdatao%hice	=sfcdatai%hice  /float(nanals)
	    sfcdatao%fice	=sfcdatai%fice  /float(nanals)
	    sfcdatao%tisfc	=sfcdatai%tisfc  /float(nanals)
	    sfcdatao%tprcp      =sfcdatai%tprcp  /float(nanals)
	    sfcdatao%srflag	=sfcdatai%srflag  /float(nanals)
	    sfcdatao%snwdph	=sfcdatai%snwdph  /float(nanals)
	    sfcdatao%slc	=sfcdatai%slc  /float(nanals)
	    sfcdatao%shdmin	=sfcdatai%shdmin  /float(nanals)
	    sfcdatao%shdmax	=sfcdatai%shdmax  /float(nanals)
	    sfcdatao%slope	=sfcdatai%slope  
	    sfcdatao%snoalb	=sfcdatai%snoalb  /float(nanals)
	    sfcdatao%orog	=sfcdatai%orog  /float(nanals)
         else
	    sfcdatao%tsea	=sfcdatao%tsea  +sfcdatai%tsea /float(nanals)
	    sfcdatao%smc	=sfcdatao%smc	+sfcdatai%smc  /float(nanals)
	    sfcdatao%sheleg	=sfcdatao%sheleg+sfcdatai%sheleg/float(nanals)
	    sfcdatao%stc	=sfcdatao%stc	+sfcdatai%stc  /float(nanals)
	    sfcdatao%tg3 	=sfcdatao%tg3	+sfcdatai%tg3  /float(nanals)
	    sfcdatao%zorl	=sfcdatao%zorl  +sfcdatai%zorl  /float(nanals)
	    sfcdatao%cv		=sfcdatao%cv	+sfcdatai%cv   /float(nanals)
	    sfcdatao%cvb	=sfcdatao%cvb	+sfcdatai%cvb  /float(nanals)
	    sfcdatao%cvt	=sfcdatao%cvt	+sfcdatai%cvt  /float(nanals)
	    sfcdatao%alvsf	=sfcdatao%alvsf +sfcdatai%alvsf /float(nanals)
	    sfcdatao%alvwf	=sfcdatao%alvwf +sfcdatai%alvwf /float(nanals)
	    sfcdatao%alnsf	=sfcdatao%alnsf +sfcdatai%alnsf /float(nanals)
	    sfcdatao%alnwf	=sfcdatao%alnwf +sfcdatai%alnwf /float(nanals)
	    sfcdatao%vfrac	=sfcdatao%vfrac +sfcdatai%vfrac /float(nanals)
	    sfcdatao%canopy	=sfcdatao%canopy+sfcdatai%canopy/float(nanals)
	    sfcdatao%f10m	=sfcdatao%f10m  +sfcdatai%f10m  /float(nanals)
	    sfcdatao%t2m	=sfcdatao%t2m  +sfcdatai%t2m  /float(nanals)
	    sfcdatao%q2m	=sfcdatao%q2m  +sfcdatai%q2m  /float(nanals)
	    sfcdatao%facsf	=sfcdatao%facsf +sfcdatai%facsf /float(nanals)
	    sfcdatao%facwf	=sfcdatao%facwf +sfcdatai%facwf /float(nanals)
	    sfcdatao%uustar	=sfcdatao%uustar+sfcdatai%uustar/float(nanals)
	    sfcdatao%ffmm	=sfcdatao%ffmm  +sfcdatai%ffmm  /float(nanals)
	    sfcdatao%ffhh	=sfcdatao%ffhh  +sfcdatai%ffhh  /float(nanals)
	    sfcdatao%hice	=sfcdatao%hice + sfcdatai%hice  /float(nanals)
	    sfcdatao%fice	=sfcdatao%fice + sfcdatai%fice  /float(nanals)
	    sfcdatao%tisfc	=sfcdatao%tisfc + sfcdatai%tisfc  /float(nanals)
	    sfcdatao%tprcp      =sfcdatao%tprcp + sfcdatai%tprcp  /float(nanals)
	    sfcdatao%srflag	=sfcdatao%srflag + sfcdatai%srflag  /float(nanals)
	    sfcdatao%snwdph	=sfcdatao%snwdph + sfcdatai%snwdph  /float(nanals)
	    sfcdatao%slc	=sfcdatao%slc + sfcdatai%slc  /float(nanals)
	    sfcdatao%shdmin	=sfcdatao%shdmin + sfcdatai%shdmin  /float(nanals)
	    sfcdatao%shdmax	=sfcdatao%shdmax + sfcdatai%shdmax  /float(nanals)
	    sfcdatao%snoalb	=sfcdatao%snoalb + sfcdatai%snoalb  /float(nanals)
	    sfcdatao%orog	=sfcdatao%orog + sfcdatai%orog  /float(nanals)
         end if
         call sfcio_axdata(sfcdatai,iret)
         !print *,'axdata',iret
      enddo
      call sfcio_swohdc(nsfco,filenamesfcout,sfcheado,sfcdatao,iret)
      !print *,trim(filenamesfcout),iret
      STOP
      END
