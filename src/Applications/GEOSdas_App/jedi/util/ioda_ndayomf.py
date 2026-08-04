#!/usr/bin/env python
import xarray as xr
import numpy as np
import matplotlib.pyplot as plt
import tarfile
import tempfile

def ioda_from_tarball(tarball, filename, group=None):
    if tarball == "none":
        return xr.open_dataset(filename, group=group)

    with tarfile.open(tarball, "r:*") as tar:
        tarinfo = tar.getmember(filename)

        with tempfile.NamedTemporaryFile(suffix=".nc4") as tmp_file:
            tmp_file.write(tar.extractfile(tarinfo).read())
            tmp_file.flush()

            return xr.open_dataset(tmp_file.name, group=group).load()

#......................................................................
def plot_one_by_one(groups,varname,satid,rms):
   plt.figure(figsize=(7,5))
   plt.bar(groups, rms, color="steelblue")
   plt.xlabel("Forecast Length")
   plt.ylabel("RMS of OMF")
   if satid == 0:
      plt.title(varname + " RMS (EffectiveQC0 = 0)")
   else:
      plt.title(varname + " RMS (EffectiveQC0 = 0, Id="+str(satid)+")")
   plt.grid(axis="y", alpha=0.3)
   plt.tight_layout()
   return
#......................................................................
def heatmap_omfx(fnames,varnames,groups,matrix,what):

    fig, ax = plt.subplots(figsize=(14, 7))

    if what == 1:
       this_title = "RMS"
       im = ax.imshow(matrix,
                      aspect='auto',
                      origin='upper',
                      cmap='viridis')

    else:
       this_title = "RMS("+expid+") - RMS("+ctlid+")"
       im = ax.imshow(
           matrix,
           cmap="RdBu_r",          # blue=improvement, red=degradation
           vmin=-0.1, vmax=0.1,  # choose limits appropriate for your data
           aspect="auto"
       )

    # Tick labels
    ax.set_xticks(np.arange(len(varnames)))
    ax.set_xticklabels(varnames, rotation=45, ha='right')

    ax.set_yticks(np.arange(len(groups)))
    ax.set_yticklabels(groups)

    ax.set_xlabel("Variable")
    ax.set_ylabel("Forecast Group")
    ax.set_title(this_title+" by Variable and Forecast Group")

    # Color bar
    cbar = plt.colorbar(im, ax=ax)
    cbar.set_label(this_title)

    # Bottom x-axis: variable names
    ax.set_xticks(np.arange(len(varnames)))
    ax.set_xticklabels(varnames, rotation=45, ha="right")

    # Top x-axis: file/observation type names
    ax_top = ax.secondary_xaxis('top')
    ax_top.set_xticks(np.arange(len(fnames)))
    ax_top.set_xticklabels(fnames,rotation=45, ha="right")
    ax_top.set_xlabel("Observation Type")

    plt.tight_layout()
    plt.show()

    return

#......................................................................
def heatmap_omfx_percent(fnames,varnames,groups,pct_matrix):

    fig, ax = plt.subplots(figsize=(14,7))

    im = ax.imshow(
        pct_matrix,
        cmap="RdBu_r",      # blue=improvement, red=degradation
        vmin=-80, vmax=80,  # choose limits appropriate for your data
        aspect="auto"
    )

    ax.set_xticks(np.arange(len(varnames)))
    ax.set_xticklabels(varnames, rotation=45, ha="right")

    ax.set_yticks(np.arange(len(groups)))
    ax.set_yticklabels(groups)

    cbar = plt.colorbar(im, ax=ax)
    cbar.set_label("% RMS change from ombg")

    # annotate
    for i in range(len(groups)):
        for j in range(len(varnames)):
            if np.isfinite(pct_matrix[i, j]):
#               ax.text(j, i,
#                       f"{pct_matrix[i,j]:+.1f}",
#                       ha="center", va="center",
#                       fontsize=9)
                ax.text(j, i,
                        f"{pct_matrix[i,j]:+.1f}",
                        ha="center",
                        va="center",
                        color="lime",     # bright green
                        fontsize=9,
                        fontweight="bold")


    # Bottom x-axis: variable names
    ax.set_xticks(np.arange(len(varnames)))
    ax.set_xticklabels(varnames, rotation=45, ha="right")

    # Top x-axis: file/observation type names
    ax_top = ax.secondary_xaxis('top')
    ax_top.set_xticks(np.arange(len(fnames)))
    ax_top.set_xticklabels(fnames, rotation=45, ha="right")
    ax_top.set_xlabel("Observation Type")

    plt.tight_layout()
    plt.show()
    return

#omieff_aura

#......................................................................
def fcst_ver_obs(tarname,fname,groups,varnames,satid):

 # rows = groups, columns = variables
 rms_matrix   = np.full((len(groups), len(varnames)), np.nan)

 for ii,these in enumerate(fnames):

   qc_ds = ioda_from_tarball(tarname, fnames[ii]+ncsfx, group="EffectiveQC0")
   qc = qc_ds[varnames[ii]].values
        
   if satids[ii] != 0:
       meta_ds = ioda_from_tarball(tarname, fnames[ii]+ncsfx, group="MetaData")
       sid = meta_ds["satelliteIdentifier"].values

   if print_summary:
      print((qc == 0).sum())
      if satids[ii] != 0:
         print(np.unique(sid))
         print((sid == satids[ii]).sum())
         print(((qc == 0) & (sid == satids[ii])).sum())

# Combined selection
   if satids[ii] == 0:
      mask = (qc == 0)
   else:
      mask = (qc == 0) & (sid == satids[ii])


   rms = []

   for jj,grp in enumerate(groups):
       g = ioda_from_tarball(tarname, fnames[ii]+ncsfx, group=grp)

       x = g[varnames[ii]].values

       ob = ioda_from_tarball(tarname, fnames[ii]+ncsfx, group="ObsError")
       o  = ob[varnames[ii]].values

       # Keep only data within mask
       x = x[mask]
       x = x[np.isfinite(x)]
       x = x[np.abs(x) < 1e30]

       o = o[mask]
       o = o[np.isfinite(o)]
       o = o[np.abs(o) < 1e30]

       if x.size == 0:
           value = np.nan
           if print_summary:
              print(f"{grp}: no valid observations")
       else:
           value = np.sqrt(np.mean(x**2))
           if print_summary:
              print(f"{grp}: N={x.size}, RMS={value:.3f}")

       if o.size == 0:
           ovalue = np.nan
           if print_summary:
              print(f"{grp}: no valid observations")
       else:
           ovalue = np.sqrt(np.mean(o**2))
           if print_summary:
              print(f"{grp}: N={o.size}, MEAN={value:.3f}")

       rms.append(value/ovalue)
       rms_matrix[jj, ii] = value / ovalue

       # RMS for ombg (first row)
       rms0 = rms_matrix[0, :]          # shape = (nvar,)

       # Percent change relative to ombg
       pct_matrix = 100.0 * (rms_matrix - rms0) / rms0

 return rms_matrix,pct_matrix

print_summary = False
ncsfx = ".20251229T150000Z.nc4"
trsfx = ".20251229_15z.tar"
expid = "j54rp1"
ctlid = "j54rp2"
tarname = "/discover/nobackup/projects/gmao/dadev/rtodling/archive/JEDI/544/"+expid+"/jedi//obs/Y2025/M12/"+expid+".jedi_hofx"+trsfx
tarctl = "null"
tarctl = "/discover/nobackup/projects/gmao/dadev/rtodling/archive/JEDI/544/"+ctlid+"/jedi/obs/Y2025/M12/"+ctlid+".jedi_hofx"+trsfx

# Open file
fnames = ["sfc","sfcship","sondes",
          "sondes","sondes",
          "sondes","satwind","scatwind",
          "sondes",
          "mls55_aura","ompslpnc_n21","ompslpnc_npp",
          "gps",]
varnames = ["stationPressure","stationPressure","stationPressure",
            "airTemperature","virtualTemperature",
            "windEastward","windEastward","windEastward",
            "specificHumidity",
            "ozoneProfile","ozoneProfile","ozoneProfile","bendingAngle"]
satids   = [ 0, 180, 120, 
             120, 120, 
             220, 247, 0, 
             120,
             0, 0, 0,
             269 ]


# Forecast groups
groups = ["ombg", "oman", "omf24", "omf48", "omf72", "omf96", "omf120"]
groups = ["ombg", "oman"]

[rms_matrix,    pct_matrix]     = fcst_ver_obs(tarname,fnames,groups,varnames,satids)
[rms_matrix_ctl,pct_matrix_ctl] = fcst_ver_obs(tarctl,fnames,groups,varnames,satids)
rms_matrix = (rms_matrix - rms_matrix_ctl)

# Plot
if tarctl == "null":
  #plot_one_by_one{groups,varnames[ii],satids[ii],rms);
   heatmap_omfx(fnames,varnames,groups,rms_matrix,1)
  #heatmap_omfx_percent(fnames,varnames,groups,pct_matrix)
else:
   heatmap_omfx(fnames,varnames,groups,rms_matrix,2)
