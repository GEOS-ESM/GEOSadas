	module innovstats
	!$$$  module documentation block
	!
	! module: innovstats                   print ensemble innovation statistics.
	!
	! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
	!
	! abstract:
	!
	! Public Subroutines:
	!  print_innovstats: given obfit_prior and obsprd_prior (observation - 
	!   ensemble mean observation variable, ensemble standard deviation of
	!   observation variable), print some statistics useful for monitoring.
	!
	! Public Variables: None
	!
	! program history log:
	!   2009-02-23  Initial version.
	!   2012-05-    Todling : added the Jo fits to the statistics table
	!   2012-05-14  El Akkraoui : use the oberr_orig instead of oberrvar in the
	!                             statistics table
	!   2016-03-21  Todling - add stats for AOD
	!   2017-05-20  Todling - allow calc of stats for assimilated obs only 
	!
	! attributes:
	!   language: f95
	!
	!$$$

	use enkf_obsmod, only:  oberrvar,ob,ensmean_ob,obtype,nobs_conv,nobs_oz,&
			   nobs_sat,nobstot,obloclat,ensmean_obnobc,obpress,stattype,&
			   oberrvar_orig,indxsat,iused
	use params, only : latbound,stats_usedob_only
	use kinds, only: i_kind, r_kind,r_single
	use radinfo, only: jpch_rad,nusis,nuchan
	use constants, only: one,zero

	implicit none

	private

	public :: print_innovstats

	contains

	subroutine print_innovstats(obfit,obsprd)
	real(r_single), intent(in) :: obfit(nobstot), obsprd(nobstot)
	integer(i_kind) nobst_nh,nobst_sh,nobst_tr,&
	 nobspw_nh,nobspw_sh,nobspw_tr,&
	 nobsspd_nh,nobsspd_sh,nobsspd_tr,&
	 nobsgps_nh,nobsgps_sh,nobsgps_tr,&
	 nobsaod_nh,nobsaod_sh,nobsaod_tr,&
	 nobsq_nh,nobsq_sh,nobsq_tr,nobswnd_nh,nobswnd_sh,nobswnd_tr,&
	 nobsoz_nh,nobsoz_sh,nobsoz_tr,nobsps_sh,nobsps_nh,nobsps_tr,nob, &
	 nobsps_tot,nobst_tot,nobswnd_tot,nobsq_tot,nobsoz_tot,nobspw_tot,&
	 nobsspd_tot,nobsgps_tot,nobsaod_tot
	integer(i_kind) nobssat_tot,ntotassim
	real(r_single) sumps_nh,biasps_nh,sumps_sh,biasps_sh,&
	 sumps_tr,biasps_tr,&
	 sumps_spread_nh,sumps_spread_sh,sumps_spread_tr,sumps_oberr_nh,&
	 sumps_oberr_sh,sumps_oberr_tr,&
	 sumt_nh,biast_nh,sumt_spread_nh,sumt_oberr_nh,&
	 sumt_sh,biast_sh,sumt_spread_sh,sumt_oberr_sh,&
	 sumt_tr,biast_tr,sumt_spread_tr,sumt_oberr_tr,&
	 sumq_nh,biasq_nh,sumq_spread_nh,sumq_oberr_nh,&
	 sumq_sh,biasq_sh,sumq_spread_sh,sumq_oberr_sh,&
	 sumq_tr,biasq_tr,sumq_spread_tr,sumq_oberr_tr,&
	 sumspd_nh,biasspd_nh,sumspd_spread_nh,sumspd_oberr_nh,&
	 sumspd_sh,biasspd_sh,sumspd_spread_sh,sumspd_oberr_sh,&
	 sumspd_tr,biasspd_tr,sumspd_spread_tr,sumspd_oberr_tr,&
	 sumgps_nh,biasgps_nh,sumgps_spread_nh,sumgps_oberr_nh,&
	 sumgps_sh,biasgps_sh,sumgps_spread_sh,sumgps_oberr_sh,&
	 sumgps_tr,biasgps_tr,sumgps_spread_tr,sumgps_oberr_tr,&
	 sumaod_nh,biasaod_nh,sumaod_spread_nh,sumaod_oberr_nh,&
	 sumaod_sh,biasaod_sh,sumaod_spread_sh,sumaod_oberr_sh,&
	 sumaod_tr,biasaod_tr,sumaod_spread_tr,sumaod_oberr_tr,&
	 sumpw_nh,biaspw_nh,sumpw_spread_nh,sumpw_oberr_nh,&
	 sumpw_sh,biaspw_sh,sumpw_spread_sh,sumpw_oberr_sh,&
	 sumpw_tr,biaspw_tr,sumpw_spread_tr,sumpw_oberr_tr,&
	 sumoz_nh,biasoz_nh,sumoz_spread_nh,sumoz_oberr_nh,&
	 sumoz_sh,biasoz_sh,sumoz_spread_sh,sumoz_oberr_sh,&
	 sumoz_tr,biasoz_tr,sumoz_spread_tr,sumoz_oberr_tr,&
	 sumwnd_nh,biaswnd_nh,sumwnd_spread_nh,sumwnd_oberr_nh,&
	 sumwnd_sh,biaswnd_sh,sumwnd_spread_sh,sumwnd_oberr_sh,&
	 sumwnd_tr,biaswnd_tr,sumwnd_spread_tr,sumwnd_oberr_tr
	real(r_single) sumpsjo_nh,sumpsjo_sh,sumpsjo_tr,sumpsjo
	real(r_single) sumtjo_nh,sumtjo_sh,sumtjo_tr,sumtjo
	real(r_single) sumwndjo_nh,sumwndjo_sh,sumwndjo_tr,sumwndjo
	real(r_single) sumqjo_nh,sumqjo_sh,sumqjo_tr,sumqjo
	real(r_single) sumspdjo_nh,sumspdjo_sh,sumspdjo_tr,sumspdjo
	real(r_single) sumgpsjo_nh,sumgpsjo_sh,sumgpsjo_tr,sumgpsjo
	real(r_single) sumaodjo_nh,sumaodjo_sh,sumaodjo_tr,sumaodjo
	real(r_single) sumpwjo_nh,sumpwjo_sh,sumpwjo_tr,sumpwjo
	real(r_single) sumozjo_nh,sumozjo_sh,sumozjo_tr,sumozjo
	real(r_single) sumjo_tot
	! stuff for computing sat data innovation stats.
	real(r_single) sumsprd_sat(jpch_rad),sumerr_sat(jpch_rad), &
	     sumfit_sat(jpch_rad),sumfitsq_sat(jpch_rad),sumjo_sat(jpch_rad),&
	     predicted_innov,innov
	integer(i_kind) nob_sat(jpch_rad),nchan,nn
	real(r_single) :: denom
	!==> stats for conventional + ozone obs.
	  !==> pre-process obs, obs metadata.
	  nobsps_nh = 0
	  nobsps_sh = 0
	  nobsps_tr = 0
	  nobsps_tot= 0
	  nobst_nh = 0
	  nobst_sh = 0
	  nobst_tr = 0
	  nobst_tot=0
	  nobsq_nh = 0
	  nobsq_sh = 0
	  nobsq_tr = 0
	  nobsq_tot=0
	  nobsoz_nh = 0
	  nobsoz_sh = 0
	  nobsoz_tr = 0
	  nobsoz_tot=0
	  nobswnd_nh = 0
	  nobswnd_sh = 0
	  nobswnd_tr = 0
	  nobswnd_tot=0
	  nobspw_nh = 0
	  nobspw_sh = 0
	  nobspw_tr = 0
	  nobspw_tot=0
	  nobsgps_nh = 0
	  nobsgps_sh = 0
	  nobsgps_tr = 0
	  nobsgps_tot=0
	  nobsaod_nh = 0
	  nobsaod_sh = 0
	  nobsaod_tr = 0
	  nobsaod_tot=0
	  nobsspd_nh = 0
	  nobsspd_sh = 0
	  nobsspd_tr = 0
	  nobsspd_tot=0
	  nobssat_tot=0
	  ntotassim=0
	  sumpsjo =0.0
	  sumtjo  =0.0
	  sumwndjo=0.0
	  sumqjo  =0.0
	  sumspdjo=0.0
	  sumpwjo =0.0
	  sumgpsjo=0.0
	  sumaodjo=0.0
	  sumozjo =0.0
	  sumjo_tot=0.0
	  sumps_nh =0.0;sumps_tr =0.0;sumps_sh =0.0
	  sumq_nh  =0.0;sumq_tr  =0.0;sumq_sh  =0.0
	  sumt_nh  =0.0;sumt_tr  =0.0;sumt_sh  =0.0
	  sumpw_nh =0.0;sumpw_tr =0.0;sumpw_sh =0.0
	  sumaod_nh=0.0;sumaod_tr=0.0;sumaod_sh=0.0
	  sumgps_nh=0.0;sumgps_tr=0.0;sumgps_sh=0.0
	  sumspd_nh=0.0;sumspd_tr=0.0;sumspd_sh=0.0
	  sumwnd_nh=0.0;sumwnd_tr=0.0;sumwnd_sh=0.0
	  sumoz_nh =0.0;sumoz_tr =0.0;sumoz_sh =0.0
	if (nobs_conv+nobs_oz > 0) then
	  do nob=1,nobs_conv+nobs_oz
	     if(stats_usedob_only) then
	       if(iused(nob)==0) cycle ! do not include not assim obs in statistics
	     endif 
	     if(oberrvar(nob) < 1.e10_r_single)then
		 if (obtype(nob)(1:3) == ' ps') then
		     call obstats(obfit(nob),oberrvar(nob),oberrvar_orig(nob),&
			 obsprd(nob),obloclat(nob),&
			 sumps_nh,biasps_nh,sumps_spread_nh,sumps_oberr_nh,nobsps_nh,sumpsjo_nh,&
			 sumps_sh,biasps_sh,sumps_spread_sh,sumps_oberr_sh,nobsps_sh,sumpsjo_sh,&
			 sumps_tr,biasps_tr,sumps_spread_tr,sumps_oberr_tr,nobsps_tr,sumpsjo_tr)
		    nobsps_tot = nobsps_nh + nobsps_sh + nobsps_tr
		    sumpsjo = sumpsjo_nh + sumpsjo_sh + sumpsjo_tr 
		    ntotassim=ntotassim+1
		 else if (obtype(nob)(1:3) == '  t' .and. stattype(nob) /= 121) then
		     call obstats(obfit(nob),oberrvar(nob),oberrvar_orig(nob),&
			 obsprd(nob),obloclat(nob),&
			 sumt_nh,biast_nh,sumt_spread_nh,sumt_oberr_nh,nobst_nh,sumtjo_nh,&
			 sumt_sh,biast_sh,sumt_spread_sh,sumt_oberr_sh,nobst_sh,sumtjo_sh,&
			 sumt_tr,biast_tr,sumt_spread_tr,sumt_oberr_tr,nobst_tr,sumtjo_tr)
		    nobst_tot = nobst_nh + nobst_sh + nobst_tr
		    sumtjo = sumtjo_nh + sumtjo_sh + sumtjo_tr
		    ntotassim=ntotassim+1
		! all winds
		 else if (obtype(nob)(1:3) == '  u' .or. obtype(nob)(1:3) == '  v') then
		! only in-situ winds (no sat winds)
		!else if (obtype(nob)(1:3) == '  u' .or. obtype(nob)(1:3) == '  v' .and. &
		!        ((stattype(nob) >= 280 .and. stattype(nob) <= 282) .or. &
		!         (stattype(nob) >= 220 .and. stattype(nob) <= 221) .or. &
		!         (stattype(nob) >= 230 .and. stattype(nob) <= 235) ) then
		     call obstats(obfit(nob),oberrvar(nob),oberrvar_orig(nob),&
			 obsprd(nob),obloclat(nob),&
			 sumwnd_nh,biaswnd_nh,sumwnd_spread_nh,sumwnd_oberr_nh,nobswnd_nh,sumwndjo_nh,&
			 sumwnd_sh,biaswnd_sh,sumwnd_spread_sh,sumwnd_oberr_sh,nobswnd_sh,sumwndjo_sh,&
			 sumwnd_tr,biaswnd_tr,sumwnd_spread_tr,sumwnd_oberr_tr,nobswnd_tr,sumwndjo_tr)
		    nobswnd_tot = nobswnd_nh + nobswnd_sh + nobswnd_tr
		    sumwndjo = sumwndjo_nh + sumwndjo_sh + sumwndjo_tr
		    ntotassim=ntotassim+1
		 else if (obtype(nob)(1:3) == '  q') then
		     call obstats(obfit(nob),oberrvar(nob),oberrvar_orig(nob),&
			 obsprd(nob),obloclat(nob),&
			 sumq_nh,biasq_nh,sumq_spread_nh,sumq_oberr_nh,nobsq_nh,sumqjo_nh,&
			 sumq_sh,biasq_sh,sumq_spread_sh,sumq_oberr_sh,nobsq_sh,sumqjo_sh,&
			 sumq_tr,biasq_tr,sumq_spread_tr,sumq_oberr_tr,nobsq_tr,sumqjo_tr)
		    nobsq_tot = nobsq_nh + nobsq_sh + nobsq_tr
		    sumqjo = sumqjo_nh + sumqjo_sh + sumqjo_tr
		    ntotassim=ntotassim+1
		 else if (obtype(nob)(1:3) == 'spd') then
		     call obstats(obfit(nob),oberrvar(nob),oberrvar_orig(nob),&
			 obsprd(nob),obloclat(nob),&
			 sumspd_nh,biasspd_nh,sumspd_spread_nh,sumspd_oberr_nh,nobsspd_nh,sumspdjo_nh,&
			 sumspd_sh,biasspd_sh,sumspd_spread_sh,sumspd_oberr_sh,nobsspd_sh,sumspdjo_sh,&
			 sumspd_tr,biasspd_tr,sumspd_spread_tr,sumspd_oberr_tr,nobsspd_tr,sumspdjo_tr)
		    nobsspd_tot = nobsspd_nh + nobsspd_sh + nobsspd_tr
		    sumspdjo = sumspdjo_nh + sumspdjo_sh + sumspdjo_tr
		    ntotassim=ntotassim+1
		 else if (obtype(nob)(1:3) == 'gps') then
		     call obstats(obfit(nob),oberrvar(nob),oberrvar_orig(nob),&
			 obsprd(nob),obloclat(nob),&
			 sumgps_nh,biasgps_nh,sumgps_spread_nh,sumgps_oberr_nh,nobsgps_nh,sumgpsjo_nh,&
			 sumgps_sh,biasgps_sh,sumgps_spread_sh,sumgps_oberr_sh,nobsgps_sh,sumgpsjo_sh,&
			 sumgps_tr,biasgps_tr,sumgps_spread_tr,sumgps_oberr_tr,nobsgps_tr,sumgpsjo_tr)
		    nobsgps_tot = nobsgps_nh + nobsgps_sh + nobsgps_tr
		    sumgpsjo = sumgpsjo_nh + sumgpsjo_sh + sumgpsjo_tr
		    ntotassim=ntotassim+1
		 else if (obtype(nob)(1:3) == ' pw') then
		     call obstats(obfit(nob),oberrvar(nob),oberrvar_orig(nob),&
			 obsprd(nob),obloclat(nob),&
			 sumpw_nh,biaspw_nh,sumpw_spread_nh,sumpw_oberr_nh,nobspw_nh,sumpwjo_nh,&
			 sumpw_sh,biaspw_sh,sumpw_spread_sh,sumpw_oberr_sh,nobspw_sh,sumpwjo_sh,&
			 sumpw_tr,biaspw_tr,sumpw_spread_tr,sumpw_oberr_tr,nobspw_tr,sumpwjo_tr)
		    nobspw_tot = nobspw_nh + nobspw_sh + nobspw_tr
		    sumpwjo = sumpwjo_nh + sumpwjo_sh + sumpwjo_tr
		    ntotassim=ntotassim+1
		 else if (nob > nobs_conv .and. nob < nobs_conv+nobs_oz) then
		     call obstats(obfit(nob),oberrvar(nob),oberrvar_orig(nob),&
			 obsprd(nob),obloclat(nob),&
			 sumoz_nh,biasoz_nh,sumoz_spread_nh,sumoz_oberr_nh,nobsoz_nh,sumozjo_nh,&
			 sumoz_sh,biasoz_sh,sumoz_spread_sh,sumoz_oberr_sh,nobsoz_sh,sumozjo_sh,&
			 sumoz_tr,biasoz_tr,sumoz_spread_tr,sumoz_oberr_tr,nobsoz_tr,sumozjo_tr)
		    nobsoz_tot = nobsoz_nh + nobsoz_sh + nobsoz_tr
		    sumozjo= sumozjo_nh + sumozjo_sh + sumozjo_tr 
		    ntotassim=ntotassim+1
		 end if
	      end if
	  end do ! loop over non-radiance obs
	  do nob=nobs_conv+nobs_oz+nobs_sat+1,size(obtype)
	     if(stats_usedob_only) then
	       if(iused(nob)==0) cycle ! do not include not assim obs in statistics
	     endif 
	     if(oberrvar(nob) < 1.e10_r_kind)then
		 if (obtype(nob)(1:3) == 'aod') then
		     call obstats(obfit(nob),oberrvar(nob),oberrvar_orig(nob),&
			 obsprd(nob),obloclat(nob),&
			 sumaod_nh,biasaod_nh,sumaod_spread_nh,sumaod_oberr_nh,nobsaod_nh,sumaodjo_nh,&
			 sumaod_sh,biasaod_sh,sumaod_spread_sh,sumaod_oberr_sh,nobsaod_sh,sumaodjo_sh,&
			 sumaod_tr,biasaod_tr,sumaod_spread_tr,sumaod_oberr_tr,nobsaod_tr,sumaodjo_tr)
		    nobsaod_tot = nobsaod_nh + nobsaod_sh + nobsaod_tr
		    sumaodjo = sumaodjo_nh + sumaodjo_sh + sumaodjo_tr
		    ntotassim=ntotassim+1
		 endif
	     endif
	  end do ! loop over aod observations
	!--> print innovation statistics for subset of conventional data.
	   print *,'conventional obs'
	   print *,'region, obtype, nobs, bias, innov stdev, sqrt(S+R), sqrt(S), sqrt(R), Jo:'
	   call printstats('   all ps',sumps_nh,biasps_nh,sumps_spread_nh,sumps_oberr_nh,sumpsjo_nh,nobsps_nh,&
		sumps_sh,biasps_sh,sumps_spread_sh,sumps_oberr_sh,sumpsjo_sh,nobsps_sh,&
		sumps_tr,biasps_tr,sumps_spread_tr,sumps_oberr_tr,sumpsjo_tr,nobsps_tr)
	   call printstats('    all t',sumt_nh,biast_nh,sumt_spread_nh,sumt_oberr_nh,sumtjo_nh,nobst_nh,&
		sumt_sh,biast_sh,sumt_spread_sh,sumt_oberr_sh,sumtjo_sh,nobst_sh,&
		sumt_tr,biast_tr,sumt_spread_tr,sumt_oberr_tr,sumtjo_tr,nobst_tr)
	   call printstats('   all uv',sumwnd_nh,biaswnd_nh,sumwnd_spread_nh,sumwnd_oberr_nh,sumwndjo_nh,nobswnd_nh,&
		sumwnd_sh,biaswnd_sh,sumwnd_spread_sh,sumwnd_oberr_sh,sumwndjo_sh,nobswnd_sh,&
		sumwnd_tr,biaswnd_tr,sumwnd_spread_tr,sumwnd_oberr_tr,sumwndjo_tr,nobswnd_tr)
	   call printstats('    all q',sumq_nh,biasq_nh,sumq_spread_nh,sumq_oberr_nh,sumqjo_nh,nobsq_nh,&
		sumq_sh,biasq_sh,sumq_spread_sh,sumq_oberr_sh,sumqjo_sh,nobsq_sh,&
		sumq_tr,biasq_tr,sumq_spread_tr,sumq_oberr_tr,sumqjo_tr,nobsq_tr)
	   call printstats('  all spd',sumspd_nh,biasspd_nh,sumspd_spread_nh,sumspd_oberr_nh,sumspdjo_nh,nobsspd_nh,&
		sumspd_sh,biasspd_sh,sumspd_spread_sh,sumspd_oberr_sh,sumspdjo_sh,nobsspd_sh,&
		sumspd_tr,biasspd_tr,sumspd_spread_tr,sumspd_oberr_tr,sumspdjo_tr,nobsspd_tr)
	   call printstats('   all pw',sumpw_nh,biasq_nh,sumpw_spread_nh,sumpw_oberr_nh,sumpwjo_nh,nobspw_nh,&
		sumpw_sh,biaspw_sh,sumpw_spread_sh,sumpw_oberr_sh,sumpwjo_sh,nobspw_sh,&
		sumpw_tr,biaspw_tr,sumpw_spread_tr,sumpw_oberr_tr,sumpwjo_tr,nobspw_tr)
	   call printstats('  all gps',sumgps_nh,biasq_nh,sumgps_spread_nh,sumgps_oberr_nh,sumgpsjo_nh,nobsgps_nh,&
		sumgps_sh,biasgps_sh,sumgps_spread_sh,sumgps_oberr_sh,sumgpsjo_sh,nobsgps_sh,&
		sumgps_tr,biasgps_tr,sumgps_spread_tr,sumgps_oberr_tr,sumgpsjo_tr,nobsgps_tr)
	   call printstats('  all aod',sumaod_nh,biasq_nh,sumaod_spread_nh,sumaod_oberr_nh,sumaodjo_nh,nobsaod_nh,&
		sumaod_sh,biasaod_sh,sumaod_spread_sh,sumaod_oberr_sh,sumaodjo_sh,nobsaod_sh,&
		sumaod_tr,biasaod_tr,sumaod_spread_tr,sumaod_oberr_tr,sumaodjo_tr,nobsaod_tr)
           if(nobs_oz>0) then
	     call printstats(' sbuv2 oz',sumoz_nh,biasoz_nh,sumoz_spread_nh,sumoz_oberr_nh,sumozjo_nh,nobsoz_nh,&
		  sumoz_sh,biasoz_sh,sumoz_spread_sh,sumoz_oberr_sh,sumozjo_sh,nobsoz_sh,&
		  sumoz_tr,biasoz_tr,sumoz_spread_tr,sumoz_oberr_tr,sumozjo_tr,nobsoz_tr)
           endif
	end if ! nobs_conv+nobs_oz > 0

	if (size(obtype)-(nobs_conv+nobs_oz+nobs_sat+1) > 0) then
	  do nob=nobs_conv+nobs_oz+nobs_sat+1,size(obtype)
	     if(stats_usedob_only) then
	       if(iused(nob)==0) cycle ! do not include not assim obs in statistics
	     endif 
	     if(oberrvar(nob) < 1.e10_r_kind)then
		 if (obtype(nob)(1:3) == 'aod') then
		     call obstats(obfit(nob),oberrvar(nob),oberrvar_orig(nob),&
			 obsprd(nob),obloclat(nob),&
			 sumaod_nh,biasaod_nh,sumaod_spread_nh,sumaod_oberr_nh,nobsaod_nh,sumaodjo_nh,&
			 sumaod_sh,biasaod_sh,sumaod_spread_sh,sumaod_oberr_sh,nobsaod_sh,sumaodjo_sh,&
			 sumaod_tr,biasaod_tr,sumaod_spread_tr,sumaod_oberr_tr,nobsaod_tr,sumaodjo_tr)
		    nobsaod_tot = nobsaod_nh + nobsaod_sh + nobsaod_tr
		    sumaodjo = sumaodjo_nh + sumaodjo_sh + sumaodjo_tr
		    ntotassim=ntotassim+1
		 endif
	     endif
	  end do ! loop over aod observations
	   call printstats('  all aod',sumaod_nh,biasq_nh,sumaod_spread_nh,sumaod_oberr_nh,sumaodjo_nh,nobsaod_nh,&
		sumaod_sh,biasaod_sh,sumaod_spread_sh,sumaod_oberr_sh,sumaodjo_sh,nobsaod_sh,&
		sumaod_tr,biasaod_tr,sumaod_spread_tr,sumaod_oberr_tr,sumaodjo_tr,nobsaod_tr)
	endif

	sumjo_tot = sumpsjo+sumtjo+sumwndjo+sumqjo+sumspdjo+sumpwjo+sumgpsjo+sumaodjo+sumozjo
	!==> stats for satellite brightness temp obs (amsua only).
	  sumsprd_sat = zero
	  sumfit_sat = zero
	  sumerr_sat = zero
	  sumfitsq_sat = zero
	  sumjo_sat = zero
	  nob_sat = 0
	if (nobs_sat > 0) then
	  nn = 0
	  do nob=nobs_conv+nobs_oz+1,nobs_conv+nobs_oz+nobs_sat
	     nn = nn + 1
	     if(stats_usedob_only) then
	       if(iused(nob)==0) cycle ! do not include not assim obs in statistics
	     endif 
	     nchan = indxsat(nn)
	     if (oberrvar(nob) < 1.e10_r_single .and. nchan > 0) then
	       sumsprd_sat(nchan)=sumsprd_sat(nchan)+obsprd(nob)
	       sumerr_sat(nchan)=sumerr_sat(nchan)+oberrvar_orig(nob)
	       sumfitsq_sat(nchan)=sumfitsq_sat(nchan)+obfit(nob)**2
	       sumfit_sat(nchan)=sumfit_sat(nchan)+obfit(nob)
	       sumjo_sat(nchan)=sumjo_sat(nchan)+obfit(nob)**2/oberrvar(nob)
	       nob_sat(nchan)=nob_sat(nchan) + 1
	       nobssat_tot=nobssat_tot+1
	       ntotassim=ntotassim+1
	     end if
	  end do ! loop over obs

	  sumjo_tot = sumjo_tot + sum(sumjo_sat)

	!--> print innovation statistics for amsu-a sat data..
	  print *,'satellite brightness temp'
	  print *,'instrument, channel #, nobs, bias, innov stdev, sqrt(S+R), sqrt(S), sqrt(R), Jo:'
	  do nchan=1,jpch_rad
	     if (nob_sat(nchan) > 0) then
	       denom=one/real(nob_sat(nchan),r_single)
	       sumfit_sat(nchan) = sumfit_sat(nchan)*denom
	       sumfitsq_sat(nchan) = sumfitsq_sat(nchan)*denom
	       sumerr_sat(nchan) = sumerr_sat(nchan)*denom
	       sumsprd_sat(nchan) = sumsprd_sat(nchan)*denom
	       predicted_innov = sqrt(sumsprd_sat(nchan)+sumerr_sat(nchan))
	       !innov = sqrt(sumfitsq_sat(nchan)-sumfit_sat(nchan)**2)
	       innov = sqrt(sumfitsq_sat(nchan))
	       write(6,9805) trim(adjustl(nusis(nchan))),nuchan(nchan),nob_sat(nchan),sumfit_sat(nchan),innov,&
			  predicted_innov,sqrt(sumsprd_sat(nchan)),&
			  sqrt(sumerr_sat(nchan)),sumjo_sat(nchan)
	     end if
	  end do
	  write(6,9805) trim(adjustl('GL all sat')), 999, 999,sum(sumfit_sat),0.,&
		    0.,0.,0.,sum(sumjo_sat)
	9805 format(a20,i4,1x,i5,1p,6(1x,e10.3))
	end if !nobs_sat>0

	write(6,9806)'Observation Type',' ','Nobs','Jo','Jo/n' 
	if(nobsps_tot >0) write(6,9807) "surface pressure",nobsps_tot,real(sumpsjo,r_kind),real(sumpsjo/nobsps_tot,r_kind)
	if(nobst_tot  >0) write(6,9807) "temperature",nobst_tot,real(sumtjo,r_kind),real(sumtjo/nobst_tot,r_kind) 
	if(nobswnd_tot>0) write(6,9807) "wind",nobswnd_tot,real(sumwndjo,r_kind),real(sumwndjo/nobswnd_tot,r_kind)
	if(nobsq_tot  >0) write(6,9807) "moisture",nobsq_tot,real(sumqjo,r_kind),real(sumqjo/nobsq_tot,r_kind)
	if(nobspw_tot >0) write(6,9807) "precipitation",nobspw_tot,real(sumpwjo,r_kind),real(sumpwjo/nobspw_tot,r_kind)
	if(nobsoz_tot >0) write(6,9807) "ozone",nobsoz_tot,real(sumozjo,r_kind),real(sumozjo/nobsoz_tot,r_kind)
	if(nobsgps_tot>0) write(6,9807) "gps",nobsgps_tot,real(sumgpsjo,r_kind),real(sumgpsjo/nobsgps_tot,r_kind)
	if(nobsaod_tot>0) write(6,9807) "aod",nobsaod_tot,real(sumaodjo,r_kind),real(sumaodjo/nobsaod_tot,r_kind)
	if(nobssat_tot>0) write(6,9807) "radiance",nobssat_tot,real(sum(sumjo_sat),r_kind),real(sum(sumjo_sat)/nobssat_tot,r_kind)
	if(ntotassim > 0) write(6,9807) "Jo Global",ntotassim,real(sumjo_tot,r_kind),&
			    real(sumjo_tot/(ntotassim),r_kind)
	9806 format(a20,2x,a3,2x,a8,2x,a24,4x,a8)
	9807 format(a20,2x,3x,2x,i8,2x,es24.16,2x,f10.3)

	end subroutine print_innovstats

	subroutine obstats(obfit,oberrjo,oberrvar,obsprd,obloclat,&
			   sumfit_nh,sumbias_nh,sumspread_nh,sumoberr_nh,nobs_nh,sumjo_nh,&
			   sumfit_sh,sumbias_sh,sumspread_sh,sumoberr_sh,nobs_sh,sumjo_sh,&
			   sumfit_tr,sumbias_tr,sumspread_tr,sumoberr_tr,nobs_tr,sumjo_tr)

	  implicit none
	  real(r_single), intent(in out) ::  sumfit_nh, sumbias_nh, sumspread_nh, sumoberr_nh,&
	       sumfit_tr, sumbias_tr, sumspread_tr, sumoberr_tr,&
	       sumfit_sh, sumbias_sh, sumspread_sh, sumoberr_sh,&
	       sumjo_nh, sumjo_sh, sumjo_tr
	  real(r_single), intent(in) :: obfit,oberrjo, oberrvar, obsprd, obloclat
	  integer(i_kind), intent(in out) :: nobs_nh, nobs_sh, nobs_tr

	! compute innovation statistics in nh,sh,tropics.

	  if (obloclat > latbound) then
	     if (nobs_nh == 0) then
	       sumfit_nh = obfit**2
	       sumbias_nh = obfit
	       sumspread_nh = obsprd
	       sumoberr_nh = oberrvar
	       sumjo_nh = obfit**2/oberrjo
	     else
	       sumfit_nh = sumfit_nh + obfit**2
	       sumbias_nh = sumbias_nh + obfit
	       sumspread_nh = sumspread_nh + obsprd
	       sumoberr_nh = sumoberr_nh + oberrvar
	       sumjo_nh = sumjo_nh + obfit**2/oberrjo
	     end if
	     nobs_nh = nobs_nh + 1
	  else if (obloclat < -latbound) then
	     if (nobs_sh == 0) then
	       sumfit_sh = obfit**2
	       sumbias_sh = obfit
	       sumspread_sh = obsprd
	       sumoberr_sh = oberrvar
	       sumjo_sh = obfit**2/oberrjo
	     else
	       sumfit_sh = sumfit_sh + obfit**2
	       sumbias_sh = sumbias_sh + obfit
	       sumspread_sh = sumspread_sh + obsprd
	       sumoberr_sh = sumoberr_sh + oberrvar
	       sumjo_sh = sumjo_sh + obfit**2/oberrjo
	     end if
	     nobs_sh = nobs_sh + 1
	  else
	     if (nobs_tr == 0) then
	       sumfit_tr = obfit**2
	       sumbias_tr = obfit
	       sumspread_tr = obsprd
	       sumoberr_tr = oberrvar
	       sumjo_tr = obfit**2/oberrjo
	     else
	       sumfit_tr = sumfit_tr + obfit**2
	       sumbias_tr = sumbias_tr + obfit
	       sumspread_tr = sumspread_tr + obsprd
	       sumoberr_tr = sumoberr_tr + oberrvar
	       sumjo_tr = sumjo_tr + obfit**2/oberrjo
	     end if
	     nobs_tr = nobs_tr + 1
	  end if

	end subroutine obstats

	subroutine printstats(obtype,sum_nh,bias_nh,sum_spread_nh,sum_oberr_nh,sum_jo_nh,nobs_nh,&
		      sum_sh,bias_sh,sum_spread_sh,sum_oberr_sh,sum_jo_sh,nobs_sh,&
		      sum_tr,bias_tr,sum_spread_tr,sum_oberr_tr,sum_jo_tr,nobs_tr)
	  implicit none
	  real(r_single), intent(in out) ::  bias_nh, sum_spread_nh, sum_oberr_nh,&
	       bias_tr, sum_spread_tr, sum_oberr_tr,&
	       bias_sh, sum_spread_sh, sum_oberr_sh, &
	       sum_nh,sum_sh,sum_tr
	  real(r_single), intent(in out) :: sum_jo_nh,sum_jo_sh,sum_jo_tr
	  integer(i_kind), intent(in) :: nobs_nh, nobs_sh, nobs_tr
	  character(len=9), intent(in) :: obtype
	  real(r_single) :: denom
	  integer(i_kind) :: ntot
	  real(r_single) :: sum_tot,bias_tot,sum_oberr_tot,sum_spread_tot,sum_jo_tot

	!   print *,'obtype,nobs_nh,nobs_sh,nobs_tr ',obtype,nobs_nh,nobs_sh,nobs_tr
	  if (nobs_nh > 0) then
	     denom=one!_RT/real(nobs_nh,r_single)
	     sum_nh = sum_nh*denom
	     bias_nh = bias_nh*denom
	     sum_oberr_nh = sum_oberr_nh*denom
	     sum_spread_nh = sum_spread_nh*denom
	     write(6,9805) &
	     'NH',obtype,nobs_nh,bias_nh,sqrt(sum_nh),sqrt(sum_spread_nh+sum_oberr_nh),sqrt(sum_spread_nh),sqrt(sum_oberr_nh),&
	     sum_jo_nh
	  end if
	  if (nobs_tr > 0) then
	     denom=one!_RT/real(nobs_tr,r_single)
	     sum_tr = sum_tr*denom
	     bias_tr = bias_tr*denom
	     sum_oberr_tr = sum_oberr_tr*denom
	     sum_spread_tr = sum_spread_tr*denom
	     write(6,9805) &
	     'TR',obtype,nobs_tr,bias_tr,sqrt(sum_tr),sqrt(sum_spread_tr+sum_oberr_tr),sqrt(sum_spread_tr),sqrt(sum_oberr_tr),&
	     sum_jo_tr
	  end if
	  if (nobs_sh > 0) then
	     denom=one!_RT/real(nobs_sh,r_single)
	     sum_sh = sum_sh*denom
	     bias_sh = bias_sh*denom
	     sum_oberr_sh = sum_oberr_sh*denom
	     sum_spread_sh = sum_spread_sh*denom
	     write(6,9805) &
	     'SH',obtype,nobs_sh,bias_sh,sqrt(sum_sh),sqrt(sum_spread_sh+sum_oberr_sh),sqrt(sum_spread_sh),sqrt(sum_oberr_sh),&
	     sum_jo_sh
	  end if
	  ntot=nobs_nh+nobs_tr+nobs_sh
	  if ( ntot>0 ) then
	     sum_tot = (sum_nh+sum_tr+sum_sh)/ntot
	     bias_tot = (bias_nh+bias_tr+bias_sh)/ntot
	     sum_oberr_tot = (sum_oberr_nh+sum_oberr_tr+sum_oberr_sh)/ntot
	     sum_spread_tot = (sum_spread_nh+sum_spread_tr+sum_spread_sh)/ntot
	     sum_jo_tot = sum_jo_nh+sum_jo_tr+sum_jo_sh
	     write(6,9805) &
	     'GL',obtype,ntot,bias_tot,sqrt(sum_tot),sqrt(sum_spread_tot+sum_oberr_tot),sqrt(sum_spread_tot),sqrt(sum_oberr_tot),&
     sum_jo_tot
  end if
9805 format(a2,1x,a9,1x,i6,1p,6(1x,e10.3))
end subroutine printstats

end module innovstats
