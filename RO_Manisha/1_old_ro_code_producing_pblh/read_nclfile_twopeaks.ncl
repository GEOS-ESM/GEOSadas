;**********************************************************************************
;This script reads the NCL output file (converted from BUFR using Jianjun's script)
;Differs from read_nclfile_new.ncl because it identifies two peaks - a global 
;minimum and secondary local minimum that exceed the chosen threshold and are
;atleast 500m apart
;**********************************************************************************
;Load the following module prior to running this script
;module load ncl/6.6.2
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;**********************************************************************************

set_default_fillvalue("float",   -9.9e+10)
set_default_fillvalue("integer",   -999)

yyyy="2015"
mm="09"
dd="30"
hh="00"
f = addfile("/discover/nobackup/mganesha/DSI_PBL/bufr2nc/testrun/gnssro_obs_"+yyyy+mm+dd+hh+".nc4", "r")

printVarSummary(f)

varnames = getfilevarnames(f)

print(varnames)

print(dimsizes(varnames))

printFileVarSummary(f,varnames(0))

metadata = getfileatts(f)

print(metadata)

dimnames = getfiledimnames(f)

print(dimnames)

dims = getfiledimsizes(f)

nrecs = dims(0)

print(dims)

print(nrecs)

att1 = getfilevaratts(f,varnames(0))

print(att1)

;time = f@$metadata(0)$  
;print(time)

;All variables have a '@' sign so this makes NCL think that we are referencing an attribute of the variable which is not defined.
;In order to read the variables from this file, we need to use predefined varnames and using '$' sign to read it 
;This prevents NCL from confusing the variable nomenclature with that of attribute 

lat = f->$varnames(0)$

;print(lat)
;print(dimsizes(lat))


;Read in all variables one by one

lon = f->$varnames(1)$

timeoffset = f->$varnames(2)$	;time offset to analysis time in hours

datetime = f->$varnames(3)$	

profile_number = f->$varnames(4)$ 	;GNSS RO profile identifier

gnss_sat_id = f->$varnames(5)$		;GNSS satellite classification, e.g., 401=GPS, 402=GLONASS

ref_sat_id = f->$varnames(6)$		;GNSS satellite transmitter identifier (1-32)

occ_sat_id = f->$varnames(7)$		;Low Earth Orbit satellite identifier, e.g., COSMIC2=750-755

occ_sat_is = f->$varnames(8)$		;satellite instrument

ascend_flag = f->$varnames(9)$          ;the original occultation ascending/descending flag

process_center = f->$varnames(10)$	;originally data processing_center, e.g., 60 for UCAR, 94 for DMI, 78 for GFZ  

ref = f->$varnames(11)$			;Atmospheric refractivity  

ref_err = f->$varnames(12)$                 ;Input error in atmospheric refractivity

alt = f->$varnames(13)$			;Geometric Altitude   

bend_ang = f->$varnames(14)$		;Bending angle in radians   

bend_ang_err = f->$varnames(15)$            ;Input error in Bending angle in radians

impact_parm = f->$varnames(16)$		;distance from centre of curvature in meters  

impact_height = f->$varnames(17)$         ;distance from mean sea level in meters

sensor_azim_ang = f->$varnames(18)$     ;GNSS->LEO line of sight  in degree    

geoid_height =  f->$varnames(19)$       ;Geoid height above WGS-84 ellipsoid

R =  f->$varnames(20)$			;Earth’s local radius of curvature in meters    

;print(ref)

;print(ref_sat_id)

;print(ascend_flag)

;print(alt)

print("This is the fill value for refractivity")
print(ref@_FillValue)

print("This is the fill value for longitude")
print(lon@_FillValue)

print("This is the fill value for altitude")
print(alt@_FillValue)

print("This is thee fill value for sat id")
print(gnss_sat_id@_FillValue)

print("This is the minimum and maximum longitude")
print(min(lon))
print(max(lon))

print("This is the min and max time offset")
print(min(timeoffset))
print(max(timeoffset)) 

utc_hh = stringtointeger(hh)
utc_dd = stringtointeger(dd)
utc_mm = stringtointeger(mm)
utc_yy = stringtointeger(yyyy)
;utc_hour  = utc_hh  + timeoffset

print("This is utc day")
print(utc_dd)

;print("This is the min and max UTC hour")
;print(min(utc_hour))
;print(max(utc_hour))

;***********************************************************************************
;Now separate all records into individual profiles by identifying where the height
;differences exceed 10km or 10000m
;***********************************************************************************

diff_alt = alt(1:nrecs-1)-alt(0:nrecs-2)

print(any(diff_alt.lt.0))
print(num(diff_alt.lt.0))

indend = ind(diff_alt.lt.0)
print(dimsizes(indend))
;print(indend)

indices = new(dimsizes(indend)+1,"integer")
indices(dimsizes(indend)) = nrecs-1
indices(0:dimsizes(indend)-1)=indend

print("This is the total number of profiles")
print(dimsizes(indices))
nprof =  dimsizes(indices)

;Now calculate the maximum vertical levels for any given profile
diff_indices = new(dimsizes(indices),typeof(indices))
diff_indices(0) = indices(0)+1
diff_indices(1:dimsizes(indices)-1) = indices(1:dimsizes(indices)-1) - indices(0:dimsizes(indices)-2)
print("This is  the maximum no. of levels for any given profile")
print(max(diff_indices))
print("This is the minimum no. of levels for any given profile")
print(min(diff_indices))

;print(diff_indices)

klev = max(diff_indices)

refractivity = new((/dimsizes(indices),max(diff_indices)/),typeof(ref))
ref_error = new((/dimsizes(indices),max(diff_indices)/),typeof(ref_err))
altitude = new((/dimsizes(indices),max(diff_indices)/),typeof(alt))
latitude  = new((/dimsizes(indices),max(diff_indices)/),typeof(lat))
longitude = new((/dimsizes(indices),max(diff_indices)/),typeof(lon))
utc_time = new((/dimsizes(indices),dims(1)/),typeof(datetime))
gnss_satellite_id = new(dimsizes(indices),typeof(gnss_sat_id)) 
ref_satellite_id = new(dimsizes(indices),typeof(ref_sat_id))
occ_satellite_id = new(dimsizes(indices),typeof(occ_sat_id))
utc_hour = new(dimsizes(indices),typeof(timeoffset))
lon_start = new(dimsizes(indices),typeof(lon))
lat_start = new(dimsizes(indices),typeof(lat))
timeoff = new((/dimsizes(indices),max(diff_indices)/),typeof(timeoffset))
datetime2 = new((/dimsizes(indices),max(diff_indices),dims(1)/),typeof(datetime))

indstart = 0
do i = 0,dimsizes(indices)-1
 refractivity(i,0:diff_indices(i)-1) = ref(indstart:indstart+diff_indices(i)-1)
 ref_error(i,0:diff_indices(i)-1) = ref_err(indstart:indstart+diff_indices(i)-1)
 altitude(i,0:diff_indices(i)-1) = alt(indstart:indstart+diff_indices(i)-1)
 latitude(i,0:diff_indices(i)-1) = lat(indstart:indstart+diff_indices(i)-1)
 longitude(i,0:diff_indices(i)-1) = lon(indstart:indstart+diff_indices(i)-1)
 timeoff(i,0:diff_indices(i)-1) = timeoffset(indstart:indstart+diff_indices(i)-1)
 datetime2(i,0:diff_indices(i)-1,:) = datetime(indstart:indstart+diff_indices(i)-1,:)
 ;utc_time(i,:) = datetime(indstart,:)
 utc_time(i,:) = (/datetime2(i,0,:)/)
 gnss_satellite_id(i) = gnss_sat_id(indstart)
 ref_satellite_id(i) = ref_sat_id(indstart)
 occ_satellite_id(i) = occ_sat_id(indstart)
 ;utc_hour(i)  = utc_hh  + timeoffset(indstart)
 utc_hour(i)  = utc_hh  + timeoff(i,0)
 ;lon_start(i) = lon(indstart)
 ;lat_start(i) = lat(indstart)
 lon_start(i) = (/longitude(i,0)/)
 lat_start(i) = (/latitude(i,0)/)

 indstart = indices(i)+1
end do

;print("****** SAMPLE time ********")
;print(datetime2(0,:,:))
;utc_hour  = utc_hh  + timeoffset

print("This is the min and max UTC hour")
print(min(utc_hour))
print(max(utc_hour))

;print(refractivity(0,:))
;print(altitude(0,:))
;print(refractivity(dimsizes(indices)-1,:))
;print(altitude(dimsizes(indices)-1,:))

;Now compute the refractivity gradient

ref_gr = new((/dimsizes(indices),max(diff_indices)/),typeof(refractivity),ref@_FillValue)
min_ref_gr = new(dimsizes(indices),typeof(refractivity),ref@_FillValue)
min_ref_gr_alt = new(dimsizes(indices),typeof(altitude),ref@_FillValue)
min_ref_gr2  = new(dimsizes(indices),typeof(refractivity),ref@_FillValue)
min_ref_gr_alt2 = new(dimsizes(indices),typeof(altitude),ref@_FillValue)
utc_day =  new(dimsizes(indices),"integer")
utc_hour_whole = new(dimsizes(indices),"integer")
utc_month = new(dimsizes(indices),"integer")
utc_year = new(dimsizes(indices),"integer")
utc_min = new(dimsizes(indices),"integer")
lat_pblh = new(dimsizes(indices),typeof(lat_start)) 
lon_pblh = new(dimsizes(indices),typeof(lon_start))
utc_time_pblh = new((/dimsizes(indices),dims(1)/),typeof(utc_time)) 
utc_hour_pblh = new(dimsizes(indices),typeof(utc_hour)) 
lat_pblh2 = new(dimsizes(indices),typeof(lat_start))
lon_pblh2 = new(dimsizes(indices),typeof(lon_start))
utc_time_pblh2 = new((/dimsizes(indices),dims(1)/),typeof(utc_time))
utc_hour_pblh2 = new(dimsizes(indices),typeof(utc_hour))

print("*********This is the Missing Value for time variable**********")
print(utc_time_pblh@_FillValue)
ref_gr@units = "N-unit/km"
ref_gr@long_name = "Refractivity Gradient"

do i = 0,dimsizes(indices)-1 
   alt_diff = altitude(i,1:max(diff_indices)-1) - altitude(i,0:max(diff_indices)-2)
   alt_diff = alt_diff*0.001
   ref_gr(i,0:max(diff_indices)-2) = (refractivity(i,1:max(diff_indices)-1) - refractivity(i,0:max(diff_indices)-2))/alt_diff
   delete(alt_diff)
end do

print(getVarFillValue(ref_gr))

lat_pblh = lat_start
lon_pblh = lon_start
utc_time_pblh = utc_time
utc_hour_pblh = utc_hour

lat_pblh2 = lat_start
lon_pblh2 = lon_start
utc_time_pblh2 = utc_time
utc_hour_pblh2 = utc_hour

;CHANGE HERE
;delta = -40

cnt1 = 0
cnt2 = 0
cnt3 = 0
cnt4 = 0
cnt5 = 0
cnt6 = 0
cnt7 = 0
cnt8 = 0
cnt9 = 0
cnt10 = 0
;print(ref_gr(9,:))
;print(refractivity(9,:))
;print(diff_indices(9))
;print(indices(9))
do i = 0,dimsizes(indices)-1
   min_ind = new(1,"integer")
   min_ind2 = new(1,"integer")
   max_ht = new(1,"integer")
   min_ind = minind(ref_gr(i,:))
   if(.not.ismissing(min_ind)) then
     cnt1 = cnt1+1
     min_ref_gr(i) = ref_gr(i,min_ind)
     min_ref_gr_alt(i) = altitude(i,min_ind)
     lat_pblh(i) = (/latitude(i,min_ind)/)
     lon_pblh(i) = (/longitude(i,min_ind)/)
     utc_time_pblh(i,:) = (/datetime2(i,min_ind,:)/)
     utc_hour_pblh(i) = utc_hh  + timeoff(i,min_ind)
     if (min_ref_gr(i).ge.-40) then
	min_ref_gr(i) = ref@_FillValue
        min_ref_gr_alt(i) = altitude@_FillValue
        lat_pblh(i) = lat_pblh@_FillValue
        lon_pblh(i) = lon_pblh@_FillValue
        utc_time_pblh(i,:) = utc_time_pblh@_FillValue
        utc_hour_pblh(i) = utc_hour_pblh@_FillValue
     end if

     min_ref_gr2(i) = min_ref_gr(i)
     min_ref_gr_alt2(i) = min_ref_gr_alt(i)
     lat_pblh2(i) = lat_pblh(i)
     lon_pblh2(i) = lon_pblh(i)
     utc_time_pblh2(i,:) = utc_time_pblh(i,:)
     utc_hour_pblh2(i) = utc_hour_pblh(i)

     max_ht = minind(abs(altitude(i,:)-6000))
     alt_diff = altitude(i,1:diff_indices(i)-1) - altitude(i,0:diff_indices(i)-2)
     alt_diff = alt_diff*0.001
     print(dimsizes(alt_diff))
     print(max_ht)
;     print(min_ref_gr(i))
;     print(min_ref_gr_alt(i))
;     print(alt_diff(0:max_ht))
;     print(ref_gr(i,0:max_ht))
     ref_gr_avg = dim_sum(alt_diff(0:max_ht)*ref_gr(i,0:max_ht))/dim_sum(alt_diff(0:max_ht))  ; avg gradient below 6 km
     delta = 0.25*min_ref_gr(i)+0.75*ref_gr_avg
     ;print(delta)
;     print(altitude(i,max_ht))
;     print(num(ismissing(ref_gr(i,0:max_ht))))
;     print(ind(ismissing(ref_gr(i,0:max_ht))))
     if((max_ht.gt.0).and.(.not.ismissing(delta))) then
      cnt2=cnt2+1
      ref_gr_tmp = linmsg(ref_gr(i,0:max_ht),-1)
      ;print(ref_gr_tmp)
      ;print(num(ismissing(ref_gr_tmp)))
      ;print(ind(ismissing(ref_gr_tmp)))
      ;print(ref_gr(i,0:max_ht))
      qmin = local_min_1d(ref_gr_tmp, False, 0, 0)
      imin_tmp = local_min_1d(ref_gr_tmp, False, 0,1)
      ip = dim_pqsort(qmin,2)
      imin = imin_tmp(ip)
      ;print(ref_gr_tmp)
      ;print(min_ind)
      ;print(imin_tmp(0))
      if (.not.ismissing(imin_tmp(0))) then
       cnt4 = cnt4+1
       if (imin(0).eq.min_ind) then
	cnt6 = cnt6+1
	j=1
        do while (j.le.dimsizes(qmin)-1)
	 if ((abs(altitude(i,imin(j))-min_ref_gr_alt(i)).gt.500).and.qmin(j).lt.delta) then
	  cnt8 = cnt8+1
          min_ref_gr2(i) = qmin(j)
          min_ref_gr_alt2(i) = altitude(i,imin(j))
          min_ind2 = imin(j)
          lat_pblh2(i) = (/latitude(i,min_ind2)/)
          lon_pblh2(i) = (/longitude(i,min_ind2)/)
          utc_time_pblh2(i,:) = (/datetime2(i,min_ind2,:)/)
          utc_hour_pblh2(i) = utc_hh  + timeoff(i,min_ind2)
	  break
         end if
         j=j+1
        end do 
       else
	cnt7 = cnt7+1 
        j=0
        do while (j.le.dimsizes(qmin)-1)
         if ((abs(altitude(i,imin(j))-min_ref_gr_alt(i)).gt.500).and.qmin(j).lt.delta) then
          cnt9 = cnt9+1
          min_ref_gr2(i) = qmin(j)
          min_ref_gr_alt2(i) = altitude(i,imin(j))
          min_ind2 = imin(j)
          lat_pblh2(i) = (/latitude(i,min_ind2)/)
          lon_pblh2(i) = (/longitude(i,min_ind2)/)
          utc_time_pblh2(i,:) = (/datetime2(i,min_ind2,:)/)
          utc_hour_pblh2(i) = utc_hh  + timeoff(i,min_ind2)
          break
         end if
	 j=j+1
        end do
       end if
      else
       cnt5 = cnt5+1
       print("This profile is NOPEAK")
       ;print(ref_gr_tmp)
       qmin = ref_gr(i,min_ind)
       imin_tmp = min_ind
       min_ref_gr2(i) = qmin
       min_ref_gr_alt2(i) = altitude(i,imin_tmp)
       min_ind2 = imin_tmp
       lat_pblh2(i) = (/latitude(i,min_ind2)/)
       lon_pblh2(i) = (/longitude(i,min_ind2)/)
       utc_time_pblh2(i,:) = (/datetime2(i,min_ind2,:)/)
       utc_hour_pblh2(i) = utc_hh  + timeoff(i,min_ind2)
      ; print(min_ref_gr(i))
      ; print(min_ref_gr2(i))
      ; print(min_ref_gr_alt(i))
      ; print(min_ref_gr_alt2(i))
      end if
      delete(qmin)
      delete(imin_tmp)
      delete(imin)  
      delete(ip)
      delete(ref_gr_tmp)
     else					;This is if the min. altitude is above 6km .and. or missing PBLH values that don't exceed threshold -40
      cnt3 = cnt3+1
      min_ref_gr2(i) = min_ref_gr(i)
      min_ref_gr_alt2(i) = min_ref_gr_alt(i)
      lat_pblh2(i) = lat_pblh(i)
      lon_pblh2(i) = lon_pblh(i)
      utc_time_pblh2(i,:) = utc_time_pblh(i,:)
      utc_hour_pblh2(i) = utc_hour_pblh(i)
     end if
     delete(ref_gr_avg)
     delete(delta)
     delete(alt_diff)
   else
     cnt10=cnt10+1
;     min_ref_gr2(i) = min_ref_gr(i)
;     min_ref_gr_alt2(i) = min_ref_gr_alt(i)
;     lat_pblh2(i) = lat_pblh(i)
;     lon_pblh2(i) = lon_pblh(i)
;     utc_time_pblh2(i,:) = utc_time_pblh(i,:)
;     utc_hour_pblh2(i) = utc_hour_pblh(i)
   end if
   delete(min_ind)
   delete(min_ind2)
   delete(max_ht)
end do

print("The total no. of profiles with valid min. ref. gr.")
print(cnt1)
print("The total no. of profiles with valid min. ref. gr. below 6km and satisfying the -40 threshold")
print(cnt2)
print("The total no. of profiles with min. ref. gr. above 6km including thos that are rejected because it is higher than -40 threshold")
print(cnt3)
print("The total no. of profiles with valid local minima peaks")
print(cnt4)
print("The total no. of profiles WITHOUT valid local minima peaks")
print(cnt5)
print("The total no. of profiles where the min. ref. gr. is a peak")
print(cnt6)
print("Ouf of this, total no. of profiles where the 2nd peak satisfies the threshold criteria")
print(cnt8)
print("The total no. of profiles where the min. ref. gr. is NOT a peak")
print(cnt7)
print("Ouf of this, total no. of profiles where the 2nd peak satisfies the threshold criteria")
print(cnt9)
print("This is the total no. of missing min. ref. gr. that are NOT due  to -40 threshold")
print(cnt10)

do i = 0,dimsizes(indices)-1
   if (utc_hour(i).lt.0) then
      utc_hour(i) = 24+utc_hour(i)
      utc_day(i) = utc_dd - 1
      utc_month(i) = utc_mm
      utc_year(i) = utc_yy
      utc_hour_whole(i) = tointeger(floor(utc_hour(i)))
      utc_min(i) = round((utc_hour(i) - utc_hour_whole(i))*60,3)
      if (utc_day(i).eq.0) then
         utc_day(i)  = 31       ;CHANGE HERE depending on month. Since this data period only has one change going from 31st Aug to 1st Sep, the value when day.eq.0 is automatically 31. 
         utc_month(i) = utc_mm-1
      end if
   else
      utc_day(i) = utc_dd
      utc_month(i) = utc_mm
      utc_year(i) = utc_yy
      utc_hour_whole(i) = tointeger(floor(utc_hour(i)))
      utc_min(i) = round((utc_hour(i) - utc_hour_whole(i))*60,3)
   end if
end do


;print(ref_gr(0,:))
;print(ref_gr(dimsizes(indices)-1,:))

;Now find  profiles that reach at least 500m or lower (for PBL studies) however, one would need the surface elevation data for this.
;Instead, just find the minimum refractivity gradient exceeding -50 (or -40) N-unit/km

;print(min_ref_gr)
;print(ref(1988:1989))
;print(alt(1988:1989))

print("This is the number of profiles with missing refractivity values")
print(num(ismissing(min_ref_gr)))
print("This is the number of profiles with missing 2nd peak in refractivity values")
print(num(ismissing(min_ref_gr2)))

print("This is the number of profiles where only 1 ref. gr. minimum peak exists")
print("i.e. the number of cases where min. altitude is above 6km or there are no valid ref. values below 6km, and/or number of cases where no peak exists, and/or no. of cases where peak exists but doesn't satisfy the threshold criteria")
print(num((min_ref_gr_alt.eq.min_ref_gr_alt2).and.(.not.ismissing(min_ref_gr))))
print("The number of cases where the 2nd ref. peak is lower than the min. ref. gr")
print(num((min_ref_gr_alt2.lt.min_ref_gr_alt).and.(.not.ismissing(min_ref_gr))))
print("The number of cases where the 2nd ref. peak is higher than the min. ref. gr")
print(num((min_ref_gr_alt2.gt.min_ref_gr_alt).and.(.not.ismissing(min_ref_gr))))
;********** CHANGE HERE the ref. gr. threshold as needed to -50 or -40 *********************************
print("This is the number of profiles with refractivity gradient exceeding -40 N-unit/km")
print(num(min_ref_gr.lt.-40))
print(num(min_ref_gr2.lt.-40))

;Now make sure that  when the 2nd peak doesn't exceed -40 threshold, it is set to the 1st PBLH
ind_pbl = ind(min_ref_gr2.ge.-40)

print(dimsizes(ind_pbl))

min_ref_gr2(ind_pbl) = min_ref_gr(ind_pbl)
min_ref_gr_alt2(ind_pbl) = min_ref_gr_alt(ind_pbl)
lat_pblh2(ind_pbl) = lat_pblh(ind_pbl)
lon_pblh2(ind_pbl) = lon_pblh(ind_pbl)
utc_time_pblh2(ind_pbl,:) = utc_time_pblh(ind_pbl,:)
utc_hour_pblh2(ind_pbl) = utc_hour_pblh(ind_pbl)

;*******************************************************
;Now re-order the PBLH based on lower and higher values
;********************************************************
;min_ref_gr(ind_pbl) =  min_ref_gr@_FillValue
;min_ref_gr_alt(ind_pbl) = min_ref_gr_alt@_FillValue

;min_ref_gr2(ind_pbl2) =  min_ref_gr2@_FillValue
;min_ref_gr_alt2(ind_pbl2) = min_ref_gr_alt2@_FillValue

pbl_ht = new(dimsizes(min_ref_gr),typeof(min_ref_gr_alt),altitude@_FillValue)
pbl_ht@units = "Meters"
pbl_ht@long_name = "Lower Planetary Boundary Layer height"

pbl_ref = new(dimsizes(min_ref_gr),typeof(min_ref_gr),min_ref_gr@_FillValue)
pbl_ref@units = "N-unit per km"
pbl_ref@long_name = "Lower PBL refractivity gradient"

print("This is the number of profiles with refractivity gradient exceeding -40 N-unit/km after re-ordering")
pbl_ref =  where(min_ref_gr_alt.le.min_ref_gr_alt2,min_ref_gr,min_ref_gr2)
print(num(pbl_ref.lt.-40))

pbl_ht2 = new(dimsizes(min_ref_gr2),typeof(min_ref_gr_alt2),altitude@_FillValue)
pbl_ht2@units = "Meters"
pbl_ht2@long_name = "Higher Planetary Boundary Layer height"

pbl_ref2 = new(dimsizes(min_ref_gr2),typeof(min_ref_gr2),min_ref_gr2@_FillValue)
pbl_ref2@units = "N-unit per km"
pbl_ref2@long_name = "Higher PBL refractivity gradient"

pbl_ref2 =  where(min_ref_gr_alt.ge.min_ref_gr_alt2,min_ref_gr,min_ref_gr2)
print(num(pbl_ref2.lt.-40))

;Once you have pbl_ref and pbl_ref2  defined properly, without introducing missing values  in the min_ref_gr_alt and min_ref_gr_alt2,  then you can 
;proceed to define the rest of the pbl height variables 

pbl_ht = where(min_ref_gr_alt.le.min_ref_gr_alt2,min_ref_gr_alt,min_ref_gr_alt2)
pbl_ht = where(pbl_ref.lt.-40,pbl_ht,pbl_ht@_FillValue)

lon_pblh = where(min_ref_gr_alt.le.min_ref_gr_alt2,lon_pblh,lon_pblh2)
lon_pblh = where(pbl_ref.lt.-40,lon_pblh,lon_pblh@_FillValue)

lat_pblh = where(min_ref_gr_alt.le.min_ref_gr_alt2,lat_pblh,lat_pblh2)
lat_pblh = where(pbl_ref.lt.-40,lat_pblh,lat_pblh@_FillValue)

pbl_ht2 = where(min_ref_gr_alt.ge.min_ref_gr_alt2,min_ref_gr_alt,min_ref_gr_alt2)
pbl_ht2 = where(pbl_ref2.lt.-40,pbl_ht2,pbl_ht2@_FillValue)

lon_pblh2 = where(min_ref_gr_alt.ge.min_ref_gr_alt2,lon_pblh,lon_pblh2)
lon_pblh2 = where(pbl_ref2.lt.-40,lon_pblh2,lon_pblh2@_FillValue)

lat_pblh2 = where(min_ref_gr_alt.ge.min_ref_gr_alt2,lat_pblh,lat_pblh2)
lat_pblh2 = where(pbl_ref2.lt.-40,lat_pblh2,lat_pblh2@_FillValue)

utc_time_pblh_new = new((/dimsizes(indices),dims(1)/),typeof(utc_time))
utc_time_pblh_new2 = new((/dimsizes(indices),dims(1)/),typeof(utc_time))

do i = 0,dimsizes(indices)-1
      if (.not.ismissing(pbl_ht2(i))) then
	if (min_ref_gr_alt(i).gt.min_ref_gr_alt2(i)) then
		utc_time_pblh_new(i,:) = utc_time_pblh2(i,:)
		utc_time_pblh_new2(i,:) = utc_time_pblh(i,:)
        else
                utc_time_pblh_new(i,:) = utc_time_pblh(i,:)
		utc_time_pblh_new2(i,:) = utc_time_pblh2(i,:)
	end if
      else 
	utc_time_pblh_new(i,:) = utc_time_pblh@_FillValue
	utc_time_pblh_new2(i,:) = utc_time_pblh2@_FillValue
      end if
end do


;***************** CHANGES END *************************************************
;print(pbl_ht)

print("This is the maximum first PBL height")
print(max(pbl_ht))
print("This is the minimum first PBL height")
print(min(pbl_ht))

print("This is the maximum second PBL height")
print(max(pbl_ht2))
print(pbl_ht(maxind(pbl_ht2)))
print("This is the minimum second PBL height")
print(min(pbl_ht2))
print(pbl_ht(minind(pbl_ht2)))

;Make sure the PBL ref.gr. corresponding to min. and max. pblh is less than -40.
print("This is the ref.gr. associated with the max. first PBLH")
print(pbl_ref(maxind(pbl_ht)))
print("This is the ref.gr. associated with the min. first PBLH")
print(pbl_ref(minind(pbl_ht)))

print("This is the ref.gr. associated with the max. second PBLH")
print(pbl_ref2(maxind(pbl_ht2)))
print(pbl_ref(maxind(pbl_ht2)))
print("This is the ref.gr. associated with the min. second PBLH")
print(pbl_ref2(minind(pbl_ht2)))
print(pbl_ref(minind(pbl_ht2)))

print("This is the number of profiles where only 1 ref. gr. minimum peak exists")
print("i.e. the number of cases where min. altitude is above 6km or there are no valid ref. values below 6km, and/or number of cases where no peak exists, and/or no. of cases where peak exists but doesn't satisfy the threshold criteria")
print(num((pbl_ht.eq.pbl_ht2).and.(.not.ismissing(pbl_ht))))
print("The number of cases where the 2nd ref. peak is lower than the min. ref. gr")
print(num((pbl_ht2.lt.pbl_ht).and.(.not.ismissing(pbl_ht))))
print("The number of cases where the 2nd ref. peak is higher than the min. ref. gr")
print(num((pbl_ht2.gt.pbl_ht).and.(.not.ismissing(pbl_ht))))
print("The number of cases where the 1st ref. gr. min. is higher than second ref. gr. peak")
print(num((pbl_ht.gt.pbl_ht2).and.(.not.ismissing(pbl_ht))))
print("The number of cases where the 1st ref. gr. min. is lower than second ref. gr. peak")
print(num((pbl_ht.lt.pbl_ht2).and.(.not.ismissing(pbl_ht))))
print("This is the number of non missing first PBLH")
print(num(.not.ismissing(pbl_ht)))
print("This is the number of non missing second PBLH")
print(num(.not.ismissing(pbl_ht2)))


;Define quality flag to be 1 if PBLH are different, 0 if same
qc = new(dimsizes(pbl_ht),"integer")
qc = 0
print(dimsizes(qc))
ind_qc = ind((pbl_ht.eq.pbl_ht2))
qc(ind_qc) = 0
ind_noqc  = ind((pbl_ht.ne.pbl_ht2))
qc(ind_noqc) = 1
print(num(ind_qc))
print(num(ind_noqc))
print(num(qc.eq.0))
print(num(qc.eq.1))

print("CHECK HERE for UTC time ranges")
print("This is the min and max UTC hour")
print(min(utc_hour))
print(max(utc_hour))
print("This is the min and max utc hour")
print(min(utc_hour_whole))
print(max(utc_hour_whole))
print("This is the min and max UTC minute")
print(min(utc_min))
print(max(utc_min))
print("This is the min and max UTC month")
print(min(utc_month))
print(max(utc_month))
print("This is the min and max UTC day")
print(min(utc_day))
print(max(utc_day))
print("This is the min and max UTC year")
print(min(utc_year))
print(max(utc_year))

;local_hours = (lon_pblh/15) + utc_hour_pblh
local_hours = (lon_start/15) + utc_hour
 
;print(local_hours)

local_hour = where(local_hours.ge.24,local_hours-24,local_hours)
local_day = where(local_hours.ge.24,utc_day+1,utc_day)

;print(local_hour)
;print(local_day)
local_hour = where(local_hours.lt.0,local_hours+24,local_hour)
local_day = where(local_hours.lt.0,utc_day-1,local_day)
;print(local_day)

;Uncomment for beginning of month
local_month = where(local_day.eq.0,utc_month-1,utc_month)
local_day = where(local_day.eq.0,31,local_day)	;CHANGE HERE depending on month. Since this data period only has one change going from 31st Aug to 1st Sep, the value when day.eq.0 is automatically 31	

local_month = where(local_day.eq.31,utc_month+1,local_month)      ;Change here : 32 for Aug; 31 for  end of Sep
local_day = where(local_day.eq.31,1,local_day)		;Change here: 32 for Aug, 31 for  end of  Sep

;print(floor(local_hour))
floor_localhr = floor(local_hour)
print(typeof(local_hour))
print(typeof(floor_localhr))

local_minute = new(dimsizes(local_hour),"integer")
local_minute = round((local_hour - floor_localhr)*60,3)

print("CHECK HERE that the min and max is within 0 and 24")
print("This is the min and max local hour")
print(min(local_hour))
print(max(local_hour))

print("CHECK here that the min and max day are consecutive or with a gap of 2 days")
print("This is the min and max local day")
print(min(local_day))
print(max(local_day))

print("This is the min and max local minute")
print(min(local_minute))
print(max(local_minute))

print("This is the min and max local month")
print(min(local_month))
print(max(local_month))

local_hhs = where(local_minute.eq.60,ceil(local_hour),floor(local_hour))
local_mm = where(local_minute.eq.60,0,local_minute)
local_dd = where(local_hhs.ge.24,local_day+1,local_day)
local_hh = where(local_hhs.ge.24,tointeger(local_hhs-24),tointeger(local_hhs))
local_mom = where(local_dd.eq.31,local_month+1,local_month)	;Change here:  32 for Aug; 31 for end of Sep
local_dd = where(local_dd.eq.31,1,local_dd)		;Change here: 32 for Aug; 31 for  end of Sep

print(typeof(local_mm))
print(typeof(local_hh))
print(typeof(local_dd))
print(typeof(local_mom))

;print(local_hour)
;print(local_day)

;print(utc_hour)
;print(utc_day)

;print(lon_start)

print("CHECK HERE that the min and max is within 0 and 24")
print("This is the min and max local hour after rounding up minutes")
print(min(local_hh))
print(max(local_hh))

print("CHECK here that the min and max day are consecutive or with a gap of 2, after rounding up minutes")
print("This is the min and max local day")
print(min(local_dd))
print(max(local_dd))

print("This is the min and max local minute after rounding up minutes")
print(min(local_mm))
print(max(local_mm))

print("This is the min and max local month after rounding up minutes")
print(min(local_mom))
print(max(local_mom))

;print(local_dd)
;print(local_hh)
;print(local_mm)


;Finally define a local time variable
lt = new(dimsizes(indices),"string")

do i = 0,dimsizes(indices)-1

  lt(i) = utc_year(i)+"-"+sprinti("%0.2i",local_mom(i))+"-"+sprinti("%0.2i",local_dd(i))+"_"+sprinti("%0.2i",local_hh(i))+":"+sprinti("%0.2i",local_mm(i))

end do

print(lt(0))
print(lt(5))
print(lt(106))
print(lt(505))

lt@units = "yyyy-mm-dd_hh:mimi"

local_time = tochar(lt)
print(dimsizes(local_time))
dim_new = dimsizes(local_time)

print(local_time(0,:))
print(utc_time(0,:))
print(lon_start(0))

local_time@units = "yyyy-mm-dd_hh:mimi"


;Write variable descriptions:

lat_start@long_name = "Latitude at first level"
lon_start@long_name = "Longitude at first level"

latitude@long_name = "Latitude profile"
longitude@long_name = "Longitude profile"

lat_pblh@long_name = "Latitude at PBL level"
lon_pblh@long_name = "Longitude at PBL level"

utc_time@long_name = "UTC time at first level"
utc_time_pblh@long_name = "UTC time at PBL level"
local_time@long_name = "Local time at first level"
;**********************************************************************************************
; Write output to netcdf file
;**********************************************************************************************

;==============================================================================================================  
; Variables time, local_time, satellite id, and pbl height. Also refractivity profile, ref. gr.
; profile, altitude profile, lat profile and lon profile are written out and have 
; associated meta data: (a) coordinate variables nprof, klev, and ndatetime       
; and (b) attributes
;==============================================================================================================  
ndatetime  = dims(1) 
ndatetime2 = dim_new(1)

diro = "./output/twopeaks/thresh40/firstPBLH/"                     ; Output directory
diro2 = "./output/twopeaks/thresh40/secondPBLH/"                     ; Output directory 2
filo = "gnssro_pbl_obs_"+yyyy+mm+dd+hh+".nc4"             ; Output file
system("/bin/rm -f " + diro + filo)    ; remove if exists
system("/bin/rm -f " + diro2 + filo)   ; remove if exists
fout  = addfile (diro + filo, "c")  ; open output file
fout2  = addfile (diro2 + filo, "c")  ; open output file

;===================================================================
; explicitly declare file definition mode. Improve efficiency.
;===================================================================
setfileoption(fout,"DefineMode",True)
setfileoption(fout2,"DefineMode",True)

;===================================================================
; create global attributes of the file
;===================================================================
fAtt               = True            ; assign file attributes
fAtt@title         = "PBL height derived from GNSS-RO"  
fAtt@source_file   = "gnssro_obs_"+yyyy+mm+dd+hh+".nc4"
fAtt@Conventions   = "None"   
fAtt@creation_date = systemfunc ("date")        
fileattdef( fout, fAtt )            ; copy file attributes    
fileattdef( fout2, fAtt )            ; copy file attributes

;===================================================================
; predefine the coordinate variables and their dimensionality
; Note: to get an UNLIMITED record dimension, we set the dimensionality
; to -1 (or the actual size) and set the dimension name to True.
;===================================================================
dimNames = (/"profiles", "levels","ndate","ndatelocal"/)  
dimSizes = (/nprof, klev, ndatetime, ndatetime2/) 
dimUnlim = (/False, False, False, False/)   
filedimdef(fout,dimNames,dimSizes,dimUnlim)
filedimdef(fout2,dimNames,dimSizes,dimUnlim)

;===================================================================
; predefine the the dimensionality of the variables to be written out
;===================================================================
; Here we are using NCL functions to facilitate defining 
; each variable's dimension name(s) and type. 
; The following could be replaced with explicit, user defined dimension 
; names different from those associated with the variable in memory. 
; Say, PS(time,lat,lon) in the NCL script. They could be redefined for the file via: 
; filevardef(fout, "PS"   ,typeof(PS) ,(/"TIME","latitude","longitude"/)) 
;===================================================================
filevardef(fout, "time_start" ,typeof(utc_time),(/"profiles", "ndate"/)) 
filevardef(fout, "localtime_start"  ,typeof(local_time),(/"profiles","ndatelocal"/))                           
filevardef(fout, "lat"  ,typeof(latitude),(/"profiles", "levels"/))                          
filevardef(fout, "lon"  ,typeof(longitude),(/"profiles", "levels"/))                          
filevardef(fout, "ref"    ,typeof(refractivity),(/"profiles", "levels"/))    
filevardef(fout, "alt"   ,typeof(altitude),(/"profiles", "levels"/))          
filevardef(fout, "ref_gradient",typeof(ref_gr),(/"profiles", "levels"/))  ; variable name on the file                
                                                              ; different from name on script
filevardef(fout, "PBLH"  ,typeof(pbl_ht),"profiles")
filevardef(fout, "gnss_satid"  ,typeof(gnss_satellite_id),"profiles")
filevardef(fout, "ref_satid"  ,typeof(ref_satellite_id),"profiles")
filevardef(fout, "occ_satid"  ,typeof(occ_satellite_id),"profiles")
filevardef(fout, "latitude"  ,typeof(lat_pblh),"profiles")
filevardef(fout, "longitude"  ,typeof(lon_pblh),"profiles")
filevardef(fout, "latstart"  ,typeof(lat_start),"profiles")
filevardef(fout, "lonstart"  ,typeof(lon_start),"profiles")
filevardef(fout, "time_pblh" ,typeof(utc_time_pblh_new),(/"profiles", "ndate"/))
filevardef(fout, "QC", typeof(qc),"profiles")


filevardef(fout2, "time_start" ,typeof(utc_time),(/"profiles", "ndate"/))
filevardef(fout2, "localtime_start"  ,typeof(local_time),(/"profiles","ndatelocal"/))
filevardef(fout2, "lat"  ,typeof(latitude),(/"profiles", "levels"/))
filevardef(fout2, "lon"  ,typeof(longitude),(/"profiles", "levels"/))
filevardef(fout2, "ref"    ,typeof(refractivity),(/"profiles", "levels"/))
filevardef(fout2, "alt"   ,typeof(altitude),(/"profiles", "levels"/))
filevardef(fout2, "ref_gradient",typeof(ref_gr),(/"profiles", "levels"/))  ; variable name on the file
                                                              ; different from name on script
filevardef(fout2, "PBLH"  ,typeof(pbl_ht2),"profiles")
filevardef(fout2, "gnss_satid"  ,typeof(gnss_satellite_id),"profiles")
filevardef(fout2, "ref_satid"  ,typeof(ref_satellite_id),"profiles")
filevardef(fout2, "occ_satid"  ,typeof(occ_satellite_id),"profiles")
filevardef(fout2, "latitude"  ,typeof(lat_pblh2),"profiles")
filevardef(fout2, "longitude"  ,typeof(lon_pblh2),"profiles")
filevardef(fout2, "latstart"  ,typeof(lat_start),"profiles")
filevardef(fout2, "lonstart"  ,typeof(lon_start),"profiles")
filevardef(fout2, "time_pblh" ,typeof(utc_time_pblh_new2),(/"profiles", "ndate"/))
filevardef(fout2, "QC", typeof(qc),"profiles")
;===================================================================
; Copy attributes associated with each variable to the file
; All attributes associated with each variable will be copied.
;====================================================================
filevarattdef(fout,"ref",refractivity)                           ; copy T attributes
filevarattdef(fout,"time_start" ,utc_time)                    ; copy time attributes
filevarattdef(fout,"localtime_start" ,local_time)                    ; copy local time attributes
filevarattdef(fout,"alt"  ,altitude)                     ; copy lev attributes
filevarattdef(fout,"lat"  ,latitude)                     ; copy lat attributes
filevarattdef(fout,"lon"  ,longitude)                     ; copy lon attributes
filevarattdef(fout,"ref_gradient"   ,ref_gr)                      ; copy PS attributes
filevarattdef(fout,"PBLH",pbl_ht)                     ; copy TOPOG attributes
filevarattdef(fout,"gnss_satid",gnss_satellite_id) 
filevarattdef(fout,"ref_satid",ref_satellite_id)
filevarattdef(fout,"occ_satid",occ_satellite_id)
filevarattdef(fout,"latitude",lat_pblh)
filevarattdef(fout,"longitude",lon_pblh)
filevarattdef(fout,"latstart",lat_start)
filevarattdef(fout,"lonstart",lon_start)
filevarattdef(fout,"time_pblh",utc_time_pblh_new)
filevarattdef(fout,"QC",qc)

filevarattdef(fout2,"ref",refractivity)                           ; copy T attributes
filevarattdef(fout2,"time_start" ,utc_time)                    ; copy time attributes
filevarattdef(fout2,"localtime_start" ,local_time)                    ; copy local time attributes
filevarattdef(fout2,"alt"  ,altitude)                     ; copy lev attributes
filevarattdef(fout2,"lat"  ,latitude)                     ; copy lat attributes
filevarattdef(fout2,"lon"  ,longitude)                     ; copy lon attributes
filevarattdef(fout2,"ref_gradient"   ,ref_gr)                      ; copy PS attributes
filevarattdef(fout2,"PBLH",pbl_ht2)                     ; copy TOPOG attributes
filevarattdef(fout2,"gnss_satid",gnss_satellite_id)
filevarattdef(fout2,"ref_satid",ref_satellite_id)
filevarattdef(fout2,"occ_satid",occ_satellite_id)
filevarattdef(fout2,"latitude",lat_pblh2)
filevarattdef(fout2,"longitude",lon_pblh2)
filevarattdef(fout2,"latstart",lat_start)
filevarattdef(fout2,"lonstart",lon_start)
filevarattdef(fout2,"time_pblh",utc_time_pblh_new2)
filevarattdef(fout2,"QC",qc)

;===================================================================
; explicitly exit file definition mode. **NOT REQUIRED**
;===================================================================
setfileoption(fout,"DefineMode",False)
setfileoption(fout2,"DefineMode",False)

;===================================================================
; output only the data values since the dimensionality and such have
; been predefined. The "(/", "/)" syntax tells NCL to only output the
; data values to the predefined locations on the file.
;====================================================================
fout->time_start   = (/utc_time/)     
fout->localtime_start = (/local_time/)
fout->alt    = (/altitude/)
fout->lat    = (/latitude/)
fout->lon    = (/longitude/) 
fout->ref      = (/refractivity/)
fout->PBLH      = (/pbl_ht/)
fout->ref_gradient  = (/ref_gr/)
fout->gnss_satid  = (/gnss_satellite_id/)
fout->ref_satid = (/ref_satellite_id/)
fout->occ_satid = (/occ_satellite_id/)
fout->latitude = (/lat_pblh/)
fout->longitude = (/lon_pblh/)
fout->latstart = (/lat_start/)
fout->lonstart = (/lon_start/)
fout->time_pblh = (/utc_time_pblh_new/)
fout->QC = (/qc/)

fout2->time_start   = (/utc_time/)
fout2->localtime_start = (/local_time/)
fout2->alt    = (/altitude/)
fout2->lat    = (/latitude/)
fout2->lon    = (/longitude/)
fout2->ref      = (/refractivity/)
fout2->PBLH      = (/pbl_ht2/)
fout2->ref_gradient  = (/ref_gr/)
fout2->gnss_satid  = (/gnss_satellite_id/)
fout2->ref_satid = (/ref_satellite_id/)
fout2->occ_satid = (/occ_satellite_id/)
fout2->latitude = (/lat_pblh2/)
fout2->longitude = (/lon_pblh2/)
fout2->latstart = (/lat_start/)
fout2->lonstart = (/lon_start/)
fout2->time_pblh = (/utc_time_pblh_new2/)
fout2->QC = (/qc/)
