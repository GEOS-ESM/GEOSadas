#!/usr/bin/env python
import numpy as np
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import argparse
import math
import tarfile
import tempfile

import xarray as xr
import pandas as pd

def safe_filled(arr, fallback_value):
    if np.issubdtype(arr.dtype, np.floating):
        return np.ma.filled(arr, fill_value=np.nan)
    else:   
        return np.ma.filled(arr, fill_value=fallback_value)

def ioda_from_tarball (tarball,filename):

   if tarball == "none":

     ds = Dataset(filename, 'r')

   else:

     print (f"reading from tarball: {tarball}")

     # Open the tarball (compressed or not)
     with tarfile.open(tarball, "r:*") as tar:
       # Check that the target file exists
       try:
           tarinfo = tar.getmember(filename)
       except KeyError:
           raise FileNotFoundError(f"{filename} not found in tarball.")

       # Extract to temporary file
       with tempfile.NamedTemporaryFile(suffix=".nc4") as tmp_file:
           fileobj = tar.extractfile(tarinfo)
           tmp_file.write(fileobj.read())
           tmp_file.flush()

           # Open with netCDF4.Dataset
           ds = Dataset(tmp_file.name, "r")
        
           # Example: list variables
           #print(f"Variables in {filename}:")
           #print(ds.variables.keys())
        
           # Close when done
           #ds.close()

   return ds

def ods_pressure_binned(filename,varname,levlim,nbins,scaleby,satid,kt,usrqc):

# Load NetCDF data
   with Dataset(filename, 'r') as nc:
      # Read variables and offsets
      nc.set_auto_mask(False)
      ktp = nc.variables['kt'][:].astype(np.int16)# + 1
#     kx_raw = nc.variables['kx'][:].astype(np.int32)
#     kx_offset = nc.variables['kx'].getncattr('add_offset')  # usually 32768
#     sid = kx_raw + kx_offset
      sid = nc.variables['kx'][:].astype(np.int32)
      qc  = nc.variables['qcexcl'][:]
      pressure = nc.variables['lev'][:]
      ombg = nc.variables['omf'][:]
      oman = nc.variables['oma'][:]
      obs  = nc.variables['obs'][:]
      sigo = nc.variables['xvec'][:]
  
      # Flatten everything for simplicity
      pressure = pressure.flatten()
      ktp = ktp.flatten()
      sid = sid.flatten()
      qc  = qc.flatten()
      ombg = ombg.flatten()
      oman = oman.flatten()
      scale = obs.flatten()
      sigo = sigo.flatten()
      amb  = ombg - oman

      if scaleby == "hofx0" or scaleby == "bkg":
         scale = scale - ombg

# Mask valid data
   missing_val = 1.e+15
   valid_mask = (
       (qc == usrqc) &
       (sid == satid) &
       (ktp == kt) &
       (pressure < missing_val) &
       (ombg < missing_val) &
       (oman < missing_val) &
       (amb  < missing_val)
   )

   nobs_valid =   qc[valid_mask]
   ombg_valid = ombg[valid_mask]
   oman_valid = oman[valid_mask]
   amb_valid  =  amb[valid_mask]
   sigo_valid = sigo[valid_mask]

   if scaleby != 'null':
      scale_valid = scale[valid_mask]
   pressure_valid = pressure[valid_mask]# / 100.0  # Convert to hPa

# Bin pressure
   bins = np.logspace(np.log10(levlim[1]), np.log10(levlim[0]), num=nbins)
   bin_indices = np.digitize(pressure_valid, bins)
   bin_centers = (bins[:-1] + bins[1:]) / 2
   bin_heights = np.diff(bins)

# Initialize results
   sum_nobs = []
   mean_ombg = []
   rms_ombg = []
   mean_oman = []
   rms_oman = []
   mean_job = []
   mean_joa = []
   mean_sigo = []
   mean_esigo = []
   mean_esigb = []
   if scaleby != 'null':
      mean_scale = []

   for i in range(1, len(bins)):
       bin_mask = (bin_indices == i)
       if np.any(bin_mask):
          nobs_bin = nobs_valid[bin_mask]
          ombg_bin = ombg_valid[bin_mask]
          oman_bin = oman_valid[bin_mask]
          amb_bin  =  amb_valid[bin_mask]
          sigo_bin = sigo_valid[bin_mask]
  
          this=0
          for j in range(len(nobs_bin)):
              this += 1
          sum_nobs.append(this)

          mean_ombg.append(np.mean(ombg_bin))
          rms_ombg.append(np.sqrt(np.mean(ombg_bin**2)))
          mean_oman.append(np.mean(oman_bin))
          rms_oman.append(np.sqrt(np.mean(oman_bin**2)))

          mean_job.append(np.sum((ombg_bin/sigo_bin)**2))
          mean_joa.append(np.sum((oman_bin/sigo_bin)**2))

          mean_sigo.append(np.mean(sigo_bin))
          mean_esigo.append(np.sqrt(np.abs(np.mean(ombg_bin*oman_bin))))
          mean_esigb.append(np.sqrt(np.abs(np.mean(ombg_bin*amb_bin))))

          if scaleby != 'null':
             scale_bin = scale_valid[bin_mask]
             mean_scale.append(np.mean(scale_bin))

       else:
          sum_nobs.append(np.nan)
          mean_ombg.append(np.nan)
          rms_ombg.append(np.nan)
          mean_oman.append(np.nan)
          rms_oman.append(np.nan)
          mean_job.append(np.nan)
          mean_joa.append(np.nan)
          mean_sigo.append(np.nan)
          mean_esigo.append(np.nan)
          mean_esigb.append(np.nan)
          if scaleby != 'null':
             mean_scale.append(np.nan)

# Convert to arrays
   sum_nobs  = np.array(sum_nobs)
   mean_ombg = np.array(mean_ombg)
   rms_ombg  = np.array(rms_ombg)
   mean_oman = np.array(mean_oman)
   rms_oman  = np.array(rms_oman)
   mean_job  = np.array(mean_job)
   mean_joa  = np.array(mean_joa)
   mean_sigo = np.array(mean_sigo)
   mean_esigo = np.array(mean_esigo)
   mean_esigb = np.array(mean_esigb)
   if scaleby != 'null':
      mean_scale = np.array(mean_scale)
      mean_ombg = mean_ombg / mean_scale
      rms_ombg  =  rms_ombg / mean_scale
      mean_oman = mean_oman / mean_scale
      rms_oman  =  rms_oman / mean_scale
      mean_sigo = mean_sigo / mean_scale
      mean_esigo = mean_esigo / mean_scale
      mean_esigb = mean_esigb / mean_scale

   mean_job = mean_job / sum_nobs
   mean_joa = mean_joa / sum_nobs

   plt.figure(figsize=(10, 7))
   plt.subplot(2, 2, 1)
   show_nobs_panel(varname,sum_nobs, sum_nobs, bin_centers, bin_heights, usrqc,False,False)
   plt.subplot(2, 2, 2)
   labels = [ 'Mean o-b','Mean o-a', 'RMS o-b', 'RMS o-a'] 
   show_resstats_panel(varname,mean_ombg,mean_oman,rms_ombg,rms_oman,bin_centers,bin_heights,labels,False)
   plt.subplot(2, 2, 3)
   labels = ['Jo(b)/p','Jo(a)/p']
   show_jo_panel(varname,mean_job,mean_joa,bin_centers,bin_heights,labels,False)
   plt.subplot(2, 2, 4)
   labels = ['sigO', 'esigO', 'esigB']
   show_sigo_panel(varname,mean_sigo, mean_esigo, mean_esigb, bin_centers, bin_heights, labels, False)

def ods_channel(filename,varname,levlim,nbins,satid,kt,usrqc):

   ds = xr.open_dataset(filename)

   # Extract variables
   lev  = ds['lev']
   omf  = ds['omf']
   oma  = ds['oma']
   sigo = ds['xvec']
   qcexcl = ds['qcexcl']

   # Flatten arrays
   lev_flat  = lev.values.flatten()
   omf_flat  = omf.values.flatten()
   oma_flat  = oma.values.flatten()
   sigo_flat = sigo.values.flatten()
   qcexcl_flat = qcexcl.values.flatten()

   # Derived quantities
   amb_flat   = omf_flat - oma_flat
   esigo_flat = omf_flat * oma_flat
   esigb_flat = omf_flat * amb_flat
   job = omf_flat * omf_flat / (sigo_flat*sigo_flat)
   joa = oma_flat * oma_flat / (sigo_flat*sigo_flat)

   # Build full DataFrame
   df_all = pd.DataFrame({
       'lev': lev_flat,
       'omf': omf_flat,
       'oma': oma_flat,
       'sigo': sigo_flat,
       'esigo': esigo_flat,
       'esigb': esigb_flat,
       'job':   job,
       'joa':   joa,
       'qcexcl': qcexcl_flat
   }).dropna(subset=['lev', 'qcexcl'])  # Keep rows with valid lev and qcexcl

   # Get all unique levels
   all_levs = np.unique(df_all['lev'])

   # --- Compute Mean and RMS for qcexcl == 0 ---
   df_valid = df_all[df_all['qcexcl'] == 0].dropna(subset=['omf', 'oma'])
   grouped = df_valid.groupby('lev').agg({
       'omf': ['mean', lambda x: np.sqrt(np.mean(x**2))],
       'oma': ['mean', lambda x: np.sqrt(np.mean(x**2))],
       'sigo': ['mean', lambda x: np.sqrt(np.mean(x**2))],
       'esigo':[lambda x:np.sqrt(np.abs(np.mean(x)))],
       'esigb':[lambda x:np.sqrt(np.abs(np.mean(x)))],
       'job':  ['sum'],
       'joa':  ['sum']
   })
   grouped.columns = ['omf_mean', 'omf_rms', 'oma_mean', 'oma_rms', 'sigo_mean', 'sigo_rms',
                      'esigo_mean', 'esigb_mean', 'sum_job', 'sum_joa']
   grouped = grouped.reset_index()

   # Merge with all levels to ensure NaN padding
   full_result = pd.DataFrame({'lev': all_levs})
   result = pd.merge(full_result, grouped, on='lev', how='left').sort_values('lev')

   # --- Count qcexcl == 0 and != 0 per lev ---
   df_all['qcexcl_valid'] = df_all['qcexcl'] == 0
   count_data = df_all.groupby(['lev', 'qcexcl_valid']).size().unstack(fill_value=0)
   count_data = count_data.rename(columns={True: 'valid_count', False: 'excluded_count'}).reset_index()

   # Merge counts with all levels to pad missing with 0
   count_full = pd.merge(full_result, count_data, on='lev', how='left').fillna(0)
   count_full = count_full.sort_values('lev')

   # Bin channels
   channel_numbers = result['lev']
   bins = np.arange(len(channel_numbers)+1)
   bin_centers = channel_numbers
   bin_heights = 0.95*np.diff(bins)
   # replace with channel numbers
   bin_centers = np.arange(1,len(channel_numbers)+1)
   bin_heights = 0.95*np.ones(len(bin_centers), dtype=int)#np.diff(bin_centers)
   channel_numbers = bin_centers

   # Convert to arrays
   sum_nobs = count_full['valid_count']
   sum_job = result['sum_job'] / sum_nobs
   sum_joa = result['sum_joa'] / sum_nobs

# Now plot
   plt.figure(figsize=(10, 7))
   plt.subplot(2, 2, 1)
   show_nobs_panel(varname,sum_nobs, count_full['excluded_count'], bin_centers, bin_heights, usrqc,False,True)
   plt.subplot(2, 2, 2)
   labels = [ 'Mean o-b','Mean o-a', 'RMS o-b', 'RMS o-a']
   show_resstats_panel(varname,result['omf_mean'],result['oma_mean'],result['omf_rms'],result['oma_rms'],bin_centers,bin_heights,labels,True)
   plt.subplot(2, 2, 3)
   labels = ['Jo(b)/p','Jo(a)/p']
   show_jo_panel(varname,sum_job,sum_joa,bin_centers,bin_heights,labels,True)
   plt.subplot(2, 2, 4)
   labels = ['sigO', 'esigO', 'esigB']
   show_sigo_panel(varname,result['sigo_mean'], result['esigo_mean'], result['esigb_mean'], bin_centers, bin_heights, labels,  True)

def ioda_pressure_binned(nc,varname,levlim,nbins,scaleby,satid,usrqc):

# Load NetCDF data
#  with Dataset(filename, 'r') as nc:
   # Read data
   ombg = nc.groups['ombg'].variables[varname][:]
   oman = nc.groups['oman'].variables[varname][:]
   sigo = nc.groups['EffectiveError0'].variables[varname][:]
   if scaleby != 'null':
      scale = nc.groups[scaleby].variables[varname][:]
   qc  = nc.groups['EffectiveQC0'].variables[varname][:]
   pressure = nc.groups['MetaData'].variables['pressure'][:]
   if satid > 0:
      sid = nc.groups['MetaData'].variables['satelliteIdentifier'][:]
   amb = ombg-oman
  
   # Read fill values
   ombg_fill = nc.groups['ombg'].variables[varname]._FillValue
   oman_fill = nc.groups['oman'].variables[varname]._FillValue
   sigo_fill = nc.groups['EffectiveError0'].variables[varname]._FillValue
   if scaleby != 'null':
      scale_fill = nc.groups[scaleby].variables[varname]._FillValue
   pressure_fill = nc.groups['MetaData'].variables['pressure']._FillValue
   qc_fill = nc.groups['EffectiveQC0'].variables[varname]._FillValue

# Apply valid mask
   if satid > 0:
      valid_mask = (
         (sid == satid) &
         (qc == usrqc) &
         (ombg != ombg_fill) &
         (oman != oman_fill) &
         (sigo != sigo_fill) &
         (pressure != pressure_fill) &
         (qc != qc_fill)
      )
   else:
      valid_mask = (
         (qc == usrqc) &
         (ombg != ombg_fill) &
         (oman != oman_fill) &
         (sigo != sigo_fill) &
         (pressure != pressure_fill) &
         (qc != qc_fill)
      )

   nobs_valid =   qc[valid_mask]
   ombg_valid = ombg[valid_mask]
   oman_valid = oman[valid_mask]
   sigo_valid = sigo[valid_mask]
   amb_valid  =  amb[valid_mask]
   if scaleby != 'null':
      scale_valid = scale[valid_mask]

# Bin pressure
   pressure_valid = pressure[valid_mask] / 100.0  # Convert to hPa
   bins = np.logspace(np.log10(levlim[1]), np.log10(levlim[0]), num=nbins)
   bin_indices = np.digitize(pressure_valid, bins)
   bin_centers = (bins[:-1] + bins[1:]) / 2
   bin_heights = np.diff(bins)

# Initialize results
   sum_nobs = []
   mean_ombg = []
   rms_ombg = []
   mean_oman = []
   rms_oman = []
   mean_job = []
   mean_joa = []
   mean_sigo = []
   mean_esigo = []
   mean_esigb = []
   if scaleby != 'null':
      mean_scale = []

   for i in range(1, len(bins)):
       bin_mask = (bin_indices == i)
       if np.any(bin_mask):
          nobs_bin = nobs_valid[bin_mask]
          ombg_bin = ombg_valid[bin_mask]
          oman_bin = oman_valid[bin_mask]
          amb_bin  =  amb_valid[bin_mask]
          sigo_bin = sigo_valid[bin_mask]
  
          this=0
          for j in range(len(nobs_bin)):
              this += 1
          sum_nobs.append(this)

          mean_ombg.append(np.mean(ombg_bin))
          rms_ombg.append(np.sqrt(np.mean(ombg_bin**2)))
          mean_oman.append(np.mean(oman_bin))
          rms_oman.append(np.sqrt(np.mean(oman_bin**2)))

          mean_job.append(np.sum((ombg_bin/sigo_bin)**2))
          mean_joa.append(np.sum((oman_bin/sigo_bin)**2))

          mean_sigo.append(np.mean(sigo_bin))
          mean_esigo.append(np.sqrt(np.abs(np.mean(ombg_bin*oman_bin))))
          mean_esigb.append(np.sqrt(np.abs(np.mean(ombg_bin*amb_bin))))

          if scaleby != 'null':
             scale_bin = scale_valid[bin_mask]
             mean_scale.append(np.mean(scale_bin))

       else:
          sum_nobs.append(np.nan)
          mean_ombg.append(np.nan)
          rms_ombg.append(np.nan)
          mean_oman.append(np.nan)
          rms_oman.append(np.nan)
          mean_job.append(np.nan)
          mean_joa.append(np.nan)
          mean_sigo.append(np.nan)
          mean_esigo.append(np.nan)
          mean_esigb.append(np.nan)
          if scaleby != 'null':
             mean_scale.append(np.nan)

# Convert to arrays
   sum_nobs  = np.array(sum_nobs)
   mean_ombg = np.array(mean_ombg)
   rms_ombg  = np.array(rms_ombg)
   mean_oman = np.array(mean_oman)
   rms_oman  = np.array(rms_oman)
   mean_job  = np.array(mean_job)
   mean_joa  = np.array(mean_joa)
   mean_sigo = np.array(mean_sigo)
   mean_esigo = np.array(mean_esigo)
   mean_esigb = np.array(mean_esigb)
   if scaleby != 'null':
      mean_scale = np.array(mean_scale)
      mean_ombg = mean_ombg / mean_scale
      rms_ombg  =  rms_ombg / mean_scale
      mean_oman = mean_oman / mean_scale
      rms_oman  =  rms_oman / mean_scale
      mean_sigo = mean_sigo / mean_scale
      mean_esigo = mean_esigo / mean_scale
      mean_esigb = mean_esigb / mean_scale

   mean_job = mean_job/sum_nobs
   mean_joa = mean_joa/sum_nobs

   plt.figure(figsize=(10, 7))
   plt.subplot(2, 2, 1)
   show_nobs_panel(varname,sum_nobs, sum_nobs, bin_centers, bin_heights, usrqc,False,False)
   plt.subplot(2, 2, 2)
   labels = [ 'Mean o-b','Mean o-a', 'RMS o-b', 'RMS o-a']
   show_resstats_panel(varname,mean_ombg,mean_oman,rms_ombg,rms_oman,bin_centers,bin_heights,labels,False)
   plt.subplot(2, 2, 3)
   labels = ['Jo(b)/p','Jo(a)/p']
   show_jo_panel(varname,mean_job,mean_joa,bin_centers,bin_heights,labels,False)
   plt.subplot(2, 2, 4)
   labels = ['sigO', 'esigO', 'esigB']
   show_sigo_panel(varname,mean_sigo, mean_esigo, mean_esigb, bin_centers, bin_heights,labels, False)

def ioda_channel(nc,varname,levlim,nbins,scaleby,satid,usrqc):

   varBC = False
# Open the file once
#  with Dataset(filename, 'r') as nc:
   # Read ombg brightnessTemperature (float)
   if varBC:
      obs = nc.groups['ObsValue'].variables[varname][:]
      obs_fill = nc.groups['ObsValue'].variables[varname]._FillValue
      ombg = nc.groups['hofx0'].variables[varname][:]
      ombg_fill = nc.groups['hofx0'].variables[varname]._FillValue
      bias = nc.groups['ObsBias0'].variables[varname][:]
      bias_fill = nc.groups['ObsBias0'].variables[varname]._FillValue
      ombg = obs - ombg + bias 
      omb_fill = obs_fill - ombg_fill + bias_fill 
   else:
      ombg = nc.groups['ombg'].variables[varname][:]
      ombg_fill = nc.groups['ombg'].variables[varname]._FillValue
   oman = nc.groups['oman'].variables[varname][:]
   oman_fill = nc.groups['oman'].variables[varname]._FillValue
   sigo = nc.groups['EffectiveError0'].variables[varname][:]
   sigo_fill = nc.groups['EffectiveError0'].variables[varname]._FillValue
   
   amb = ombg - oman
   amb_fill = ombg_fill - oman_fill

   # Read EffectiveQC0 brightnessTemperature (int)
   qc = nc.groups['EffectiveQC0'].variables[varname][:]
   qc_fill = nc.groups['EffectiveQC0'].variables[varname]._FillValue

   # Read sensorChannelNumber (for x-axis labels)
   if varname == "aerosolOpticalDepth":
      channel_numbers = nc.groups['MetaData'].variables['obs_wavelength'][:]
   else:
#     channel_numbers = nc.groups['MetaData'].variables['sensorChannelNumber'][:]
      channel_numbers = nc.variables["Channel"][:]

   # Mask both ombg and QC arrays
   ombg = np.where(ombg == ombg_fill, np.nan, ombg)
   oman = np.where(oman == oman_fill, np.nan, oman)
   sigo = np.where(sigo == sigo_fill, np.nan, sigo)
   amb  = np.where( amb ==  amb_fill, np.nan,  amb)
   qc   = np.where(  qc ==   qc_fill, np.nan,   qc)

   # Mask out ombg where QC is not 0
   ombg_masked = np.where(qc == 0, ombg, np.nan)
   oman_masked = np.where(qc == 0, oman, np.nan)
   amb_masked  = np.where(qc == 0,  amb, np.nan)
   sigo_masked = np.where(qc == 0, sigo, np.nan)

   # Compute mean across Location dimension (axis 0)
   sum_nobs  = np.sum(qc== 0, axis=0)
   mean_ombg = np.nanmean(ombg_masked, axis=0)
   mean_oman = np.nanmean(oman_masked, axis=0)
   mean_sigo = np.nanmean(sigo_masked, axis=0)
   rms_ombg  = np.sqrt(np.nanmean(ombg_masked**2, axis=0))
   rms_oman  = np.sqrt(np.nanmean(oman_masked**2, axis=0))
   rms_sigo  = np.sqrt(np.nanmean(sigo_masked**2, axis=0))

   mean_esigo  = np.sqrt(np.abs(np.nanmean(ombg_masked*oman_masked,axis=0)))
   mean_esigb  = np.sqrt(np.abs(np.nanmean(ombg_masked* amb_masked,axis=0)))

   mean_job = np.nansum((ombg_masked/sigo_masked)**2,axis=0)
   mean_joa = np.nansum((oman_masked/sigo_masked)**2,axis=0)
   mean_job = mean_job/sum_nobs
   mean_joa = mean_joa/sum_nobs

   bins = np.linspace(channel_numbers[0], channel_numbers[-1], num=len(channel_numbers)+1)
   bin_centers = (channel_numbers[:-1] + channel_numbers[1:]) / 2
   bin_heights = np.diff(bins)

   # replace with channel numbers
   bin_centers = np.arange(1,len(channel_numbers)+1)
   bin_heights = 0.95*np.ones(len(bin_centers), dtype=int)#np.diff(bin_centers)
   channel_numbers = bin_centers

   # Plotting
   plt.figure(figsize=(10, 7))
   plt.subplot(2, 2, 1)
   show_nobs_panel(varname,sum_nobs, sum_nobs, channel_numbers, bin_heights, usrqc,False,True)
   plt.subplot(2, 2, 2)
   labels = [ 'Mean o-b','Mean o-a', 'RMS o-b', 'RMS o-a']
   show_resstats_panel(varname,mean_ombg,mean_oman,rms_ombg,rms_oman,channel_numbers,bin_heights,labels,True)
   plt.subplot(2, 2, 3)
   labels = ['Jo(b)/p','Jo(a)/p']
   show_jo_panel(varname,mean_job,mean_joa,bin_centers,bin_heights,labels,True)
   plt.subplot(2, 2, 4)
   labels = ['sigO', 'esigO', 'esigB']
   show_sigo_panel(varname,mean_sigo, mean_esigo, mean_esigb, channel_numbers, bin_heights, labels, True)

def jediXgsi_channel(nc,varname,levlim,nbins,scaleby,satid,usrqc,comp_bias,common):

   calculate_omb = False
# Open the file once
#  with Dataset(filename, 'r') as nc:
   # Read ombg brightnessTemperature (float)
   obs = nc.groups['ObsValue'].variables[varname][:]
   obs_fill = nc.groups['ObsValue'].variables[varname]._FillValue

   # from JEDI:
   ombg = nc.groups['ombg'].variables[varname][:]
   ombg_fill = nc.groups['ombg'].variables[varname]._FillValue
   jhox = nc.groups['hofx0'].variables[varname][:]
   jhox_fill = nc.groups['hofx0'].variables[varname]._FillValue
   jbias = nc.groups['ObsBias0'].variables[varname][:]
   jbias_fill = nc.groups['ObsBias0'].variables[varname]._FillValue

   jsigo = nc.groups['EffectiveError0'].variables[varname][:]
   jsigo_fill = nc.groups['EffectiveError0'].variables[varname]._FillValue

   # from GSI:
   gomb = nc.groups['GsiHofX'].variables[varname][:]
   gomb_fill = nc.groups['GsiHofX'].variables[varname]._FillValue
   gbias = nc.groups['GsiBc'].variables[varname][:]
   gbias_fill = nc.groups['GsiBc'].variables[varname]._FillValue

   gsigo = nc.groups['GsiFinalObsError'].variables[varname][:]
   gsigo_fill = nc.groups['GsiFinalObsError'].variables[varname]._FillValue
   
   # Read GSI EffectiveQC brightnessTemperature (int)
   gqc = nc.groups['GsiEffectiveQC'].variables[varname][:]
   gqc_fill = nc.groups['GsiEffectiveQC'].variables[varname]._FillValue

   # Read JEDI EffectiveQC0 brightnessTemperature (int)
   jqc = nc.groups['EffectiveQC0'].variables[varname][:]
   jqc_fill = nc.groups['EffectiveQC0'].variables[varname]._FillValue

   # Read sensorChannelNumber (for x-axis labels)
   channel_numbers = nc.groups['MetaData'].variables['sensorChannelNumber'][:]

   # Mask out relavant attributes and QC arrays removing fill
   obs   = np.where( obs ==  obs_fill, np.nan,  obs)
   jsigo = np.where(jsigo==jsigo_fill, np.nan,jsigo)
   gsigo = np.where(gsigo==gsigo_fill, np.nan,gsigo)
   jhox  = np.where(jhox == jhox_fill, np.nan, jhox)
   gomb  = np.where(gomb == gomb_fill, np.nan, gomb)
   jbias = np.where(jbias==jbias_fill, np.nan,jbias)
   gbias = np.where(gbias==gbias_fill, np.nan,gbias)
   jqc   = np.where( jqc ==  jqc_fill, np.nan,  jqc)
   gqc   = np.where( gqc ==  gqc_fill, np.nan,  gqc)

   # Apply mask by setting unwanted values to np.nan (or another fill value)
   if common:
      mask = np.logical_and(gqc == 0, jqc == 0)
      gqc_filtered = np.where(mask, gqc, np.nan)
      gqc = gqc_filtered

   # Mask out relavant attributes where QC is not 0
   jobs_masked  = np.where(jqc == 0,   obs, np.nan)
   gobs_masked  = np.where(gqc == 0,   obs, np.nan)
   ombg_masked  = np.where(jqc == 0,  ombg, np.nan)
   jhox_masked  = np.where(jqc == 0,  jhox, np.nan)
   gomb_masked  = np.where(gqc == 0,  gomb, np.nan)
   jbias_masked = np.where(jqc == 0, jbias, np.nan)
   gbias_masked = np.where(gqc == 0, gbias, np.nan)
   jsigo_masked = np.where(jqc == 0, jsigo, np.nan)
   gsigo_masked = np.where(gqc == 0, gsigo, np.nan)

   gomb_masked = gobs_masked - gomb_masked - gbias_masked
   if calculate_omb:
      jomb_masked = jobs_masked - jhox_masked - jbias_masked # it looks like jedi wants a plus in front of bias 
#     jomb_masked = jobs_masked - jhox_masked + jbias_masked # it looks like jedi wants a plus in front of bias 
   else:
      jomb_masked = ombg_masked
      
   # Compute mean across Location dimension (axis 0)
   gsum_nobs = np.sum(gqc== 0, axis=0)
   jsum_nobs = np.sum(jqc== 0, axis=0)
   mean_jomb = np.nanmean(jomb_masked, axis=0)
   mean_gomb = np.nanmean(gomb_masked, axis=0)
   mean_jsigo = np.nanmean(jsigo_masked, axis=0)
   mean_gsigo = np.nanmean(gsigo_masked, axis=0)
   rms_jomb  = np.sqrt(np.nanmean(jomb_masked**2, axis=0))
   rms_gomb  = np.sqrt(np.nanmean(gomb_masked**2, axis=0))
   rms_jsigo = np.sqrt(np.nanmean(jsigo_masked**2, axis=0))
   rms_gsigo = np.sqrt(np.nanmean(gsigo_masked**2, axis=0))

   mean_jbias = np.nanmean(jbias_masked, axis=0)
   mean_gbias = np.nanmean(gbias_masked, axis=0)
   rms_jbias = np.sqrt(np.nanmean(jbias_masked**2, axis=0))
   rms_gbias = np.sqrt(np.nanmean(gbias_masked**2, axis=0))

#  mean_esigo  = np.sqrt(np.abs(np.nanmean(ombg_masked*oman_masked,axis=0)))
#  mean_esigb  = np.sqrt(np.abs(np.nanmean(ombg_masked* amb_masked,axis=0)))

   mean_jjob = np.nansum((jomb_masked/jsigo_masked)**2,axis=0)
   mean_gjob = np.nansum((gomb_masked/gsigo_masked)**2,axis=0)
   mean_jjob = mean_jjob/jsum_nobs
   mean_gjob = mean_gjob/gsum_nobs

   bins = np.linspace(channel_numbers[0], channel_numbers[-1], num=len(channel_numbers)+1)
   bin_centers = (channel_numbers[:-1] + channel_numbers[1:]) / 2
   bin_heights = 0.5*np.diff(bins)
   # replace with channel numbers
   bin_centers = np.arange(1,len(channel_numbers)+1)
   bin_heights = 0.95*np.ones(len(bin_centers), dtype=int)#np.diff(bin_centers)
   channel_numbers = bin_centers

   # Plotting
   plt.figure(figsize=(10, 7))
   plt.subplot(2, 2, 1)
   comp_nobs_panel(varname,jsum_nobs, gsum_nobs, channel_numbers, bin_heights, usrqc, True)
   plt.subplot(2, 2, 2)
   labels = [ 'JEDI mean','GSI mean', 'JEDI RMS', 'GSI RMS'] 
   show_resstats_panel(varname,mean_jomb,mean_gomb,rms_jomb,rms_gomb,channel_numbers,bin_heights,labels,True)
   plt.subplot(2, 2, 3)
   labels = ['JEDI Jo(b)/p','GSI Jo(b)/p']
   show_jo_panel(varname,mean_jjob,mean_gjob,channel_numbers,bin_heights,labels,True)
   plt.subplot(2, 2, 4)
   if comp_bias:
      labels = [ 'JEDI mean BC','GSI mean BC', 'JEDI RMS BC', 'GSI RMS BC'] 
      show_resstats_panel(varname,mean_jbias,mean_gbias,rms_jbias,rms_gbias,channel_numbers,bin_heights,labels,True)
   else:
      labels = [ 'JEDI sigO','GSI sigO']
      show_sigo_panel(varname,mean_jsigo, mean_gsigo, mean_gsigo, channel_numbers, bin_heights, labels, True)

def get_pressure_mask (usrqc, sid, pressure,      qc,
                            satid, pressure_fill, qc_fill ):

# Apply valid mask
     if satid > 0:
        valid_mask = (
           (sid == satid) &
           (qc == usrqc) &
           (pressure != pressure_fill) &
           (qc != qc_fill)
        )
     else:
        valid_mask = (
           (qc == usrqc) &
           (pressure != pressure_fill) &
           (qc != qc_fill)
        )

     return valid_mask

def accum_pressure_mask (bins,bin_indices,scaleby,usrqc,
                         sid, ombg,      oman,      sigo,      pressure,      qc,
                       satid, ombg_fill, oman_fill, sigo_fill, pressure_fill, qc_fill ):

# Apply valid mask
   if satid > 0:
      valid_mask = (
         (sid == satid) &
         (qc == usrqc) &
         (ombg != ombg_fill) &
         (oman != oman_fill) &
         (sigo != sigo_fill) &
         (pressure != pressure_fill) &
         (qc != qc_fill)
      )
   else:
      valid_mask = (
         (qc == usrqc) &
         (ombg != ombg_fill) &
         (oman != oman_fill) &
         (sigo != sigo_fill) &
         (pressure != pressure_fill) &
         (qc != qc_fill)
      )

   nobs_valid =   qc[valid_mask]
   ombg_valid = ombg[valid_mask]
   oman_valid = oman[valid_mask]
   sigo_valid = sigo[valid_mask]
#  amb_valid  =  amb[valid_mask]
   if scaleby != 'null':
      scale_valid = scale[valid_mask]
# Initialize results
   sum_nobs = []
   mean_ombg = []
   rms_ombg = []
   mean_oman = []
   rms_oman = []
   mean_job = []
   mean_joa = []
   mean_sigo = []
#  mean_esigo = []
#  mean_esigb = []
   mean_scale = []

   for i in range(1, len(bins)):
       bin_mask = (bin_indices == i)
       if np.any(bin_mask):
          nobs_bin = nobs_valid[bin_mask]
          ombg_bin = ombg_valid[bin_mask]
          oman_bin = oman_valid[bin_mask]
#         amb_bin  =  amb_valid[bin_mask]
          sigo_bin = sigo_valid[bin_mask]
  
          this=0
          for j in range(len(nobs_bin)):
              this += 1
          sum_nobs.append(this)

          mean_ombg.append(np.mean(ombg_bin))
          rms_ombg.append(np.sqrt(np.mean(ombg_bin**2)))
          mean_oman.append(np.mean(oman_bin))
          rms_oman.append(np.sqrt(np.mean(oman_bin**2)))

          mean_job.append(np.sum((ombg_bin/sigo_bin)**2))
          mean_joa.append(np.sum((oman_bin/sigo_bin)**2))

          mean_sigo.append(np.mean(sigo_bin))
#         mean_esigo.append(np.sqrt(np.abs(np.mean(ombg_bin*oman_bin))))
#         mean_esigb.append(np.sqrt(np.abs(np.mean(ombg_bin*amb_bin))))

          if scaleby != 'null':
             scale_bin = scale_valid[bin_mask]
             mean_scale.append(np.mean(scale_bin))

       else:
          sum_nobs.append(np.nan)
          mean_ombg.append(np.nan)
          rms_ombg.append(np.nan)
          mean_oman.append(np.nan)
          rms_oman.append(np.nan)
          mean_job.append(np.nan)
          mean_joa.append(np.nan)
          mean_sigo.append(np.nan)
#         mean_esigo.append(np.nan)
#         mean_esigb.append(np.nan)
          if scaleby != 'null':
             mean_scale.append(np.nan)

# Convert to arrays
   sum_nobs  = np.array(sum_nobs)
   mean_ombg = np.array(mean_ombg)
   rms_ombg  = np.array(rms_ombg)
   mean_oman = np.array(mean_oman)
   rms_oman  = np.array(rms_oman)
   mean_job  = np.array(mean_job)
   mean_joa  = np.array(mean_joa)
   mean_sigo = np.array(mean_sigo)
#  mean_esigo = np.array(mean_esigo)
#  mean_esigb = np.array(mean_esigb)
   if scaleby != 'null':
      mean_scale = np.array(mean_scale)
      mean_ombg = mean_ombg / mean_scale
      rms_ombg  =  rms_ombg / mean_scale
      mean_oman = mean_oman / mean_scale
      rms_oman  =  rms_oman / mean_scale
      mean_sigo = mean_sigo / mean_scale
#     mean_esigo = mean_esigo / mean_scale
#     mean_esigb = mean_esigb / mean_scale

   mean_job = mean_job/sum_nobs
   mean_joa = mean_joa/sum_nobs

   return sum_nobs, mean_ombg, rms_ombg, mean_oman, rms_oman, mean_job, mean_joa, mean_sigo, mean_scale

def jediXgsi_pressure_binned(nc,varname,levlim,nbins,scaleby,satid,usrqc):

   calculate_omb = False
# Open the file once
#  with Dataset(filename, 'r') as nc:
   # Read ombg brightnessTemperature (float)
   obs = nc.groups['ObsValue'].variables[varname][:]
   obs_fill = nc.groups['ObsValue'].variables[varname]._FillValue

   # from JEDI:
   ombg = nc.groups['ombg'].variables[varname][:]
   ombg_fill = nc.groups['ombg'].variables[varname]._FillValue
   jhox = nc.groups['hofx0'].variables[varname][:]
   jhox_fill = nc.groups['hofx0'].variables[varname]._FillValue
#  jbias = nc.groups['ObsBias0'].variables[varname][:]
#  jbias_fill = nc.groups['ObsBias0'].variables[varname]._FillValue

   jsigo = nc.groups['EffectiveError0'].variables[varname][:]
   jsigo_fill = nc.groups['EffectiveError0'].variables[varname]._FillValue

   # from GSI:
   ghox = nc.groups['GsiHofXBc'].variables[varname][:]
   ghox_fill = nc.groups['GsiHofXBc'].variables[varname]._FillValue
#  gbias = nc.groups['GsiBc'].variables[varname][:]
#  gbias_fill = nc.groups['GsiBc'].variables[varname]._FillValue

   gsigo = nc.groups['GsiFinalObsError'].variables[varname][:]
   gsigo_fill = nc.groups['GsiFinalObsError'].variables[varname]._FillValue
       
   # Read GSI EffectiveQC brightnessTemperature (int)
   gqc = nc.groups['GsiEffectiveQC'].variables[varname][:]
   gqc_fill = nc.groups['GsiEffectiveQC'].variables[varname]._FillValue

   # Read JEDI EffectiveQC0 brightnessTemperature (int)
   jqc = nc.groups['EffectiveQC0'].variables[varname][:]
   jqc_fill = nc.groups['EffectiveQC0'].variables[varname]._FillValue

   # Read pressure levels
   pressure = nc.groups['MetaData'].variables['pressure'][:]
   pressure_fill = nc.groups['MetaData'].variables['pressure']._FillValue
 
   if satid > 0:
      sid = nc.groups['MetaData'].variables['satelliteIdentifier'][:]
   else:
      sid = 0

   valid_mask =  get_pressure_mask (usrqc, sid, pressure,      jqc,
                                         satid, pressure_fill, jqc_fill )

#  Bin pressures
   pressure_valid = pressure[valid_mask] / 100.0  # Convert to hPa
   bins = np.logspace(np.log10(levlim[1]), np.log10(levlim[0]), num=nbins)
   bin_indices = np.digitize(pressure_valid, bins)
   bin_centers = (bins[:-1] + bins[1:]) / 2
   bin_heights = np.diff(bins)

#  Accum and bin JEDI resutls
   [jsum_nobs, mean_jomb, rms_jomb, dum1, dum1, mean_jjob, dum3,
   mean_jsigo, mean_scale] = accum_pressure_mask (bins,bin_indices,scaleby,usrqc,
                         sid, ombg,      ombg,      jsigo,      pressure,      jqc,
                       satid, ombg_fill, ombg_fill, jsigo_fill, pressure_fill, jqc_fill )

#  Accum and bin GSI resutls
   gomb      = obs      - ghox     # - gbias
   gomb_fill = obs_fill - ghox_fill# - gbias_fill

   [gsum_nobs, mean_gomb, rms_gomb, dum1, dum2, mean_gjob, dum3,
   mean_gsigo, mean_scale] = accum_pressure_mask (bins,bin_indices,scaleby,usrqc,
                         sid, gomb,      ghox,      gsigo,      pressure,      gqc,
                       satid, gomb_fill, ghox_fill, gsigo_fill, pressure_fill, gqc_fill )

   plt.figure(figsize=(10, 7))
   plt.subplot(2, 2, 1)
   comp_nobs_panel(varname,jsum_nobs, gsum_nobs, bin_centers, bin_heights, usrqc, False)
   plt.subplot(2, 2, 2)
   labels = [ 'JEDI mean','GSI mean', 'JEDI RMS', 'GSI RMS'] 
   show_resstats_panel(varname,mean_jomb,mean_gomb,rms_jomb,rms_gomb,bin_centers,bin_heights,labels,False)
   plt.subplot(2, 2, 3)
   labels = ['JEDI Jo(b)/p','GSI Jo(b)/p']
   show_jo_panel(varname,mean_jjob,mean_gjob,bin_centers,bin_heights,labels,False)
   plt.subplot(2, 2, 4)
   labels = [ 'JEDI sigO','GSI sigO']
   show_sigo_panel(varname,mean_jsigo, mean_gsigo, mean_gsigo, bin_centers, bin_heights, labels, False)

#-----------------------------------------------------------------------------------------------
def show_resstats_panel(varname,mean_ombg,mean_oman,rms_ombg,rms_oman,bin_centers,
                        bin_heights,labels,radiance):

# Bar width and offset setup
   bar_width = bin_heights * 0.4  # 40% of height
   offsets = [-1.5, -0.5, 0.5, 1.5]  # offset positions for the 4 bars

# Flags to only add each label once
   labeled = {
       "mean_ombg": False,
       "mean_oman": False,
       "rms_ombg": False,
       "rms_oman": False
   }
   for i in range(len(bin_centers)):
       y = bin_centers[i]
#      if np.isnan(mean_ombg[i]):
#         continue

       # Calculate statistics (scaled or standard)
#      if scaleby != 'null':
#         this_mean_ombg = mean_ombg[i]/mean_scale[i]
#         this_rms_ombg = rms_ombg[i]/mean_scale[i]
#         this_mean_oman = mean_oman[i]/mean_scale[i]
#         this_rms_oman = rms_oman[i]/mean_scale[i]
#      else:
       this_mean_ombg = mean_ombg[i]
       this_rms_ombg = rms_ombg[i]
       this_mean_oman = mean_oman[i]
       this_rms_oman = rms_oman[i]

       # Draw bars: mean_ombg, rms_ombg, mean_oman, rms_oman
       plt.barh(
           y + offsets[0]*bar_width[i], this_mean_ombg, height=bar_width[i],
           color='cyan', label=labels[0] if not labeled["mean_ombg"] else ""
       )
       labeled["mean_ombg"] = True

       plt.barh(
           y + offsets[2]*bar_width[i], this_mean_oman, height=bar_width[i],
           color='orange', label=labels[1] if not labeled["mean_oman"] else ""
       )
       labeled["mean_oman"] = True

       plt.barh(
           y + offsets[1]*bar_width[i], this_rms_ombg, height=bar_width[i],
           color='blue', label=labels[2] if not labeled["rms_ombg"] else ""
       )
       labeled["rms_ombg"] = True

       plt.barh(
           y + offsets[3]*bar_width[i], this_rms_oman, height=bar_width[i],
           color='red', label=labels[3] if not labeled["rms_oman"] else ""
       )
       labeled["rms_oman"] = True

# Plot formatting
   if not radiance:
      plt.yscale('log')
      plt.gca().invert_yaxis()
   if varname == 'ozoneProfile':
      plt.xlabel('Ozone Value (mol mol$^{-1}$)')
   if varname == 'virtualTemperature':
      plt.xlabel('Virtual Temperature (K)')
   if varname == 'bendingAngle':
      plt.xlabel('Bending Angle')
   if radiance:
      plt.ylabel('Channel')
      plt.title('Mean & RMS of Obs Residuals vs Channel')
   else:
      plt.ylabel('Pressure (hPa)')
      plt.title('Mean & RMS of Obs Residuals vs Pressure')
   plt.grid(True, which='both', linestyle='--', alpha=0.5)
   plt.tight_layout()
   plt.legend()
#  plt.legend(loc='upper center', bbox_to_anchor=(0.5, 1.05), framealpha=0.8,
#             ncol=3, fancybox=True, shadow=True,fontsize='large')
#  plt.show()

#------------------------------------------------------------------------------------
def show_jo_panel(varname,mean_job, mean_joa, bin_centers, bin_heights, labels, radiance):

# Bar width and offset setup
   bar_width = bin_heights * 0.4  # 40% of height
   offsets = [-1.5, -0.5, 0.5, 1.5]  # offset positions for the 4 bars
#  offsets = [-1.0,  1.0, 0.5, 1.5]  # offset positions for the 4 bars

# Flags to only add each label once
   labeled = {
       "mean_job": False,
       "mean_joa": False,
   }
   for i in range(len(bin_centers)):
       y = bin_centers[i]
#      if not np.isnan(mean_job[i]):
#         continue

       # Calculate statistics (scaled or standard)
#      this_mean_ombg = mean_job[i]
#      this_mean_oman = mean_joa[i]
       # Draw bars: mean_ombg, rms_ombg, mean_oman, rms_oman
#      if not np.isnan(mean_job[i]):
       plt.barh(
           y + offsets[0]*bar_width[i], mean_job[i], height=bar_width[i],
           color='blue', label=labels[0] if not labeled["mean_job"] else ""
       )
       labeled["mean_job"] = True
   
#      if not np.isnan(mean_joa[i]):
       plt.barh(
           y + offsets[1]*bar_width[i], mean_joa[i], height=bar_width[i],
           color='red', label=labels[1] if not labeled["mean_joa"] else ""
       )
       labeled["mean_joa"] = True

# Plot formatting
   if not radiance:
      plt.yscale('log')
      plt.gca().invert_yaxis()
   if varname == 'ozoneProfile':
      plt.xlabel('Ozone Value (mol mol$^{-1}$)')
   if varname == 'virtualTemperature':
      plt.xlabel('Virtual Temperature (K)')
   if varname == 'bendingAngle':
      plt.xlabel('Bending Angle')
   if radiance:
      plt.ylabel('Channel')
      plt.title('Jo/p vs Channel')
   else:
      plt.ylabel('Pressure (hPa)')
      plt.title('Jo/p vs Pressure')
   plt.grid(True, which='both', linestyle='--', alpha=0.5)
   plt.tight_layout()
   plt.legend()
#  plt.legend(loc='upper center', bbox_to_anchor=(0.5, 1.05), framealpha=0.8,
#             ncol=3, fancybox=True, shadow=True,fontsize='large')

#------------------------------------------------------------------------------------
def show_sigo_panel(varname,mean_sigo, mean_esigo, mean_esigb, bin_centers, bin_heights, labels, radiance):

# Bar width and offset setup
   bar_width = bin_heights * 0.4  # 40% of height
   offsets = [-1.5, -0.5, 0.5, 1.5]  # offset positions for the 4 bars

# Flags to only add each label once
   labeled = {
       "mean_sigo": False,
       "mean_esigo": False,
       "mean_esigb": False,
   }
#  print (100*bin_centers[::-1])
#  print (mean_esigo[::-1])
   for i in range(len(bin_centers)):
       y = bin_centers[i]
#      if np.isnan(mean_sigo[i]):
#         continue

       # Draw bars: mean_ombg, rms_ombg, mean_oman, rms_oman
       plt.barh(
           y + offsets[0]*bar_width[i], mean_sigo[i], height=bar_width[i],
           color='cyan', label=labels[0] if not labeled["mean_sigo"] else ""
       )
       labeled["mean_sigo"] = True
   
       plt.barh(
           y + offsets[1]*bar_width[i], mean_esigo[i], height=bar_width[i],
           color='orange', label=labels[1] if not labeled["mean_esigo"] else ""
       )
       labeled["mean_esigo"] = True

       if len(labels)>2:
          plt.barh(
              y + offsets[2]*bar_width[i], mean_esigb[i], height=bar_width[i],
              color='black', label=labels[2] if not labeled["mean_esigb"] else ""
          )
          labeled["mean_esigb"] = True

# Plot formatting
   if not radiance:
      plt.yscale('log')
      plt.gca().invert_yaxis()
   if varname == 'ozoneProfile':
      plt.xlabel('Ozone Value (mol mol$^{-1}$)')
   if varname == 'virtualTemperature':
      plt.xlabel('Virtual Temperature (K)')
   if varname == 'bendingAngle':
      plt.xlabel('Bending Angle')
   if radiance: 
      plt.ylabel('Channel')
      plt.title('Prescribed & Estimated Errors vs Channel')
   else:
      plt.ylabel('Pressure (hPa)')
      plt.title('Prescribed & Estimated Errors vs Pressure')
   plt.grid(True, which='both', linestyle='--', alpha=0.5)
   plt.tight_layout()
   plt.ticklabel_format(axis='x', style='sci', scilimits=(0,10))
   plt.legend()
#  plt.legend(loc='upper center', bbox_to_anchor=(0.5, 1.05), framealpha=0.8,
#             ncol=3, fancybox=True, shadow=True,fontsize='large')

#------------------------------------------------------------------------------------
def show_nobs_panel(varname,sum_nobs, sum_nonobs, bin_centers, bin_heights, usrqc, show2, radiance):

# Bar width and offset setup
   bar_width = bin_heights * 0.4  # 40% of height
   offsets = [-0.5, -0.25, 0.5, 1.5]  # offset positions for the 4 bars

   if usrqc == 0:
      mylabel = 'used'
      mycolor = 'not used'
   else:
      mylabel = 'qc='+str(usrqc)
      mycolor = 'red'
      
# Flags to only add each label once
   labeled = {
       "sum_nobs": False,
       "sum_nonobs": False,
   }
   for i in range(len(bin_centers)):
       y = bin_centers[i]
#      if np.isnan(sum_nobs[i]):
#         continue

       # Draw bars: mean_ombg, rms_ombg, mean_oman, rms_oman
       plt.barh(
           y + offsets[0]*bar_width[i], sum_nobs[i], height=bar_width[i],
           color='green', label='used' if not labeled["sum_nobs"] else ""
       )
       labeled["sum_nobs"] = True
   
       if show2:
          plt.barh(
              y + offsets[2]*bar_width[i], sum_nonobs[i], height=bar_width[i],
              color='red', label='not used' if not labeled["sum_nonobs"] else ""
          )
          labeled["sum_nonobs"] = True

# Plot formatting
   if radiance:
      plt.ylabel('Channel')
      plt.title('Observation Count vs Channel')
   else:
      plt.yscale('log')
      plt.gca().invert_yaxis()
      plt.ylabel('Pressure (hPa)')
      plt.title('Observation Count vs Pressure')

   if varname == 'ozoneProfile':
      plt.xlabel('Ozone Value (mol mol$^{-1}$)')
   if varname == 'virtualTemperature':
      plt.xlabel('Virtual Temperature (K)')
   if varname == 'bendingAngle':
      plt.xlabel('Bending Angle')

   plt.grid(True, which='both', linestyle='--', alpha=0.5)
   plt.tight_layout()
   plt.legend()
#  plt.legend(loc='upper center', bbox_to_anchor=(0.5, 1.05), framealpha=0.8,
#             ncol=3, fancybox=True, shadow=True,fontsize='large')

#------------------------------------------------------------------------------------
def comp_nobs_panel(varname,jsum_nobs, gsum_nobs, bin_centers, bin_heights, usrqc, radiance):

# Bar width and offset setup
   bar_width = bin_heights * 0.4  # 40% of height
   offsets = [-1.5, -0.5, 0.5, 1.5]  # offset positions for the 4 bars

# Flags to only add each label once
   labeled = {
       "JEDI nobs": False,
       "GSI nobs": False,
   }
   for i in range(len(bin_centers)):
       y = bin_centers[i]
#      if np.isnan(jsum_nobs[i]):
#         continue
#      if np.isnan(gsum_nobs[i]):
#         continue

       # Draw bars: 
       plt.barh(
           y + offsets[0]*bar_width[i], jsum_nobs[i], height=bar_width[i],
           color='green', label='JEDI nobs' if not labeled["JEDI nobs"] else ""
       )
       labeled["JEDI nobs"] = True
   
       plt.barh(
           y + offsets[1]*bar_width[i], gsum_nobs[i], height=bar_width[i],
           color='red', label='GSI nobs' if not labeled["GSI nobs"] else ""
       )
       labeled["GSI nobs"] = True

# Plot formatting
   if not radiance:
      plt.yscale('log')
      plt.gca().invert_yaxis()
   if varname == 'ozoneProfile':
      plt.xlabel('Ozone Value (mol mol$^{-1}$)')
   if varname == 'virtualTemperature':
      plt.xlabel('Virtual Temperature (K)')
   if varname == 'bendingAngle':
      plt.xlabel('Bending Angle')
   if radiance:
      plt.ylabel('Channel')
      plt.title('Observation Count vs Channel')
   else:
      plt.ylabel('Pressure (hPa)')
      plt.title('Observation Count vs Pressure')
   plt.grid(True, which='both', linestyle='--', alpha=0.5)
   plt.tight_layout()
   plt.legend()
#  plt.legend(loc='upper center', bbox_to_anchor=(0.5, 1.05), framealpha=0.8,
#             ncol=3, fancybox=True, shadow=True,fontsize='large')

def main(): 

  # Set up command-line argument parsing
  parser = argparse.ArgumentParser(description='Pressure-binned statistics in file.')
  parser.add_argument('filename', help='Path to the log file')

  parser.add_argument('--scale', type=str, default = 'null',
                      help='Scale by obs/hofx0/etc (default: False)')
  parser.add_argument('--obtype', type=str, default='mls55_aura',
                        help='Observation type (default: mls55_aura)')
  parser.add_argument('--var', type=str, default='auto',
                        help='Variable name (default: auto)')
  parser.add_argument('--tarname', type=str, default='none',
                        help='Tar file name (default: none)')
  parser.add_argument('--satid', type=int, default='-999',
                        help='Observation type (default: -999 (all))')
  parser.add_argument('--qc', type=int, default='0',
                        help='Quality mark (default: 0 (used obs))')
  parser.add_argument('--xGSI', action='store_true',
                        help='Comp with GSI(default: True)')
  parser.add_argument('--common', action='store_true',
                        help='When xGSI, use only common used in comp: True)')
  parser.add_argument('--bias', action='store_true',
                        help='When xGSI, comp bias as opposed to sigo: True)')
  parser.add_argument('--fig', type=str, default='none',
                        help='give a filename to save plot (default: none)')

  args = parser.parse_args()

  kt = 9999
  levlim = [ 1000., 0.1 ] # [ bottom, top ] hPa
  nbins = 40
  radiance = False
  if args.var == 'auto':
     if args.obtype == "mls55_aura":
        varname = 'ozoneProfile'
     if args.obtype == "gps":
        varname = 'bendingAngle'
     if args.obtype == "aircraft_tsen" or args.obtype == "temperature":
        varname = 'airTemperature'
     if args.obtype == "sondes_tv":
        varname = 'virtualTemperature'
     if args.obtype == "sondes_tsen":
        varname = 'airTemperature'
     if args.obtype == "sondes_q":
        varname = 'specificHumidity'
     if args.obtype == "sondes_u":
        varname = 'windEastward'
     if args.obtype == "sondes_v":
        varname = 'windNorthward'
     if args.obtype == "saberT":
        varname = 'airTemperature'
#       levlim = [100., 0.001]
     if args.obtype == "radiance":
        varname = 'brightnessTemperature'
        radiance = True
        levlim = [1, 616] 
     if args.obtype == "aero":
        varname = 'aerosolOpticalDepth'
  else:
#    if args.obtype == "sondes":
#       nbins = 20
     varname = args.var

  if args.scale == 'obs':
     scaleby = 'ObsValue'
  elif args.scale == 'hofx0': 
     scaleby = 'hofx0'
  else:
     scaleby = 'null'

  if varname == 'bendingAngle':
     kt = 89
  elif varname == 'windEastward':
     kt = 4
  elif varname == 'windNorthward':
     kt = 5
  elif varname == 'specificHumidity':
     kt = 11
  elif varname == 'virtualTemperature' or varname == 'airTemperature':
     kt = 44
  elif varname == 'ozoneProfile':
     kt = 87
  elif varname == 'brightnessTemperature':
     kt = 40

# echo options
  print (f'obtype: {args.obtype}')
  print (f'variable: {varname}')
  print (f'kt: {kt}')
  print (f'bins: {nbins}')
  print (f'scaled by: {scaleby}')

  ext = args.filename.rsplit('.', 1)[-1]
  if ext == 'ods':
     if args.obtype == "radiance":
        ods_channel(args.filename,varname,levlim,nbins,args.satid,kt,args.qc)
     else:
        ods_pressure_binned(args.filename,varname,levlim,nbins,scaleby,args.satid,kt,args.qc)
  else:
     if args.xGSI:
        nc = ioda_from_tarball(args.tarname,args.filename)
        if args.obtype == "radiance":
           jediXgsi_channel(nc,varname,levlim,nbins,scaleby,args.satid,args.qc,args.bias,args.common)
        else:
           jediXgsi_pressure_binned(nc,varname,levlim,nbins,scaleby,args.satid,args.qc)
     else:
        nc = ioda_from_tarball(args.tarname,args.filename)
        if args.obtype == "radiance" or args.obtype == "aero":
           ioda_channel(nc,varname,levlim,nbins,scaleby,args.satid,args.qc)
        else:
           ioda_pressure_binned(nc,varname,levlim,nbins,scaleby,args.satid,args.qc)
  if args.fig == "none" :
     plt.show()
  else:
    plt.savefig(args.fig,dpi=300,orientation='landscape',format='png')

if __name__ == "__main__":
    main() 
