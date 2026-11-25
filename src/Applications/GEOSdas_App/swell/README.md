# GEOSdas_App/swell — run JEDI from ADAS through swell

ADAS owns time, data placement and model coupling; swell owns everything about
JEDI (YAML, obs operators, B, executables). ADAS ships no JEDI YAML: each role
has a small override template holding only the values ADAS knows.

| File | Purpose |
|---|---|
| `etc/swell_config.csh` | the one place that pins the swell venv/source/commit and the JEDI build, plus role settings |
| `swell_setup.pl` | called by `fvsetup`: writes `$FVHOME/run/swell/swell_config.csh`, `mkiau.rc.tenv`, the forecast IAU acquire |
| `swell_env.csh` | sourced: read the pin file, activate the venv, check the commit (`SWELL_ENV_OK`) |
| `swell_render.csh` | find a template, fill `$VAR`s from the environment, reject leftovers |
| `swell_run.csh` | generic: render templates → `swell create` → `swell launch -b` → check → clean |
| `swell_driver.csh` | central analysis of one cycle, called by `GEOSdas.csm`; replaces `jedi_driver.csh` |
| `swell_gsi2ioda.csh` | role: GSI ncdiag + GSI bias → IODA (suite `convert_ncdiags`) |
| `swell_ana_central.csh` | role: central 3D-FGAT analysis (suite `3dfgat_atmos`), replaces `jedi_set.csh` + `jedi_run.csh` |
| `swell_mkiau.csh` | analysis and IAU forcing from the JEDI increment (`dyn_jediupd.x`, `mkiau.x`), replaces `jedi_mkiau.csh` |
| `swell_upd4geos.csh` | GEOS uses the JEDI IAU forcing, replaces `jedi_upd4geos.csh` |
| `swell_ana_atmens.csh` | role: ensemble analysis (EDA / GETKF), called by `atm_ens.j`; replaces `jedi_driver_atmens.csh` |
| `etc/convert_ncdiags.yaml.tmpl` | override for `swell_gsi2ioda.csh` |
| `etc/central_3dfgat.yaml.tmpl`, `etc/central_slurm.yaml.tmpl` | override and slurm settings for `swell_ana_central.csh` |
| `etc/atmens_{eda,getkf,getkf2}.yaml.tmpl`, `etc/atmens_slurm.yaml.tmpl` | overrides and slurm settings for `swell_ana_atmens.csh` |
| `etc/mkiau.rc.tenv` | `mkiau.x` resource template (AGCM grid filled in by `swell_setup.pl`) |

## The GEOSJEDI switch

One value selects the JEDI analysis of an experiment. `fvsetup` takes it from
the environment and writes it into `$FVHOME/run/FVDAS_Run_Config`; at run time
`GEOSdas.csm` and `atm_ens.j` read only that value. The two JEDI designs share
no code: `GEOSJEDI=1` runs only `GEOSdas_App/jedi`, `GEOSJEDI=2` only `GEOSdas_App/swell`.

| `GEOSJEDI` | `fvsetup` | central analysis (`GEOSdas.csm`) | ensemble analysis (`atm_ens.j`) |
|---|---|---|---|
| 0 or unset | GSI only | none | GSI EnKF |
| 1 | `setup_aanajedi.pl` → `run/jedi` | `jedi_driver.csh` | `jedi_driver_atmens.csh` (its own gate) |
| 2 | `swell_setup.pl` → `run/swell` | `swell_driver.csh` (3D-FGAT) | `swell_ana_atmens.csh` when `SWELL_ENSEMBLE` is not `none` |

With `GEOSJEDI=2`:

- the central analysis is 3D-FGAT with climatological B, also when the ensemble
  is on (`hyb_ens >= 3`; hybrid through swell is still to come);
- `fvsetup -jedistatic DIR` becomes `SWELL_STATIC_FILES`; the answers
  `jedi_ens_scheme` 1 (EDA) / 2 (GETKF) become `SWELL_ENSEMBLE` `eda` / `getkf2`;
  the cubed resolutions give `SWELL_CENTRAL_CRES` / `SWELL_ENS_CRES`; `SKIPGSI`
  needs `OFFLINE_IODA_DIR` (→ `SWELL_OFFLINE_IODA_DIR`);
- `$FVHOME/run/swell/swell_config.csh` is the installed default with the
  experiment settings in a marked block at the top; edit it to change them;
- `fcst/rst4fcst.acq` acquires the JEDI IAU (`jedi_agcmrst`) in place of the
  GSI IAU (`agcm_import_rst`); the restart line stays;
- `atm_ens.j` skips the GSI observer when the swell ensemble runs, as it does
  for the `GEOSJEDI=1` ensemble.

New experiments always get `setenv GEOSJEDI 0|1|2` in `FVDAS_Run_Config`, so a
`GEOSJEDI` exported in the shell that submits the job does not matter. Experiments
set up before the switch existed have no such line and take `GEOSJEDI` from the
job's environment (none: GSI only); add `setenv GEOSJEDI 1` (or `0`) to their
`FVDAS_Run_Config`.

Templates and an optional `slurm.yaml` placed in `$FVHOME/run/swell` take
precedence over the installed ones in `$FVROOT/etc/swell`.

## Central analysis (swell_driver.csh)

Each step is restartable through its `.DONE` file in `$FVWORK`.

| step | |
|---|---|
| 1. observations | `swell_gsi2ioda.csh` → `$FVWORK/ioda.<ana>`; or links to `$SWELL_OFFLINE_IODA_DIR/<ana>/geos_atmosphere` |
| 2. analysis | `swell_ana_central.csh` (below) |
| 3. IAU (`SWELL_MKIAU=1`) | `swell_mkiau.csh`: `dyn_jediupd.x`(`$FVHOME/recycle` bkg.eta, increment) → `mkiau.x` |
| 4. use the IAU (`SWELL_IAU_OVERWRITE=1`, forced by `SKIPGSI`) | `swell_upd4geos.csh`: `$FVWORK/agcm_import_rst` → the JEDI IAU |

Logs `$FVWORK/$EXPID.jedi_{drv,g2i,run,var,cfg,miau,upd}.log.<B>z.txt` and the
tars below keep the names of the jedi layer, so the archive rules are unchanged.

| swell_ana_central.csh | |
|---|---|
| observations | `$FVWORK/ioda.<ana>` → swell `ioda_locations_not_in_r2d2` |
| backgrounds | `$SWELL_BKG_ARCHIVE/$EXPID/rs/Y/M/$EXPID.bkgcrst.<ana-9h>.tar` (swell `GetBackgroundGeosExperiment`) |
| bias correction | previous cycle's JEDI VarBC in `$SWELL_VARBC_DIR` when `SWELL_FEEDBACK_VARBC=1`, else GSI biases |
| B | GSIBEC climatological, `gsibec/$SWELL_GSIBEC_VERSION` in `$SWELL_STATIC_FILES` |
| → `$JEDIWORK/inc` | `$EXPID.jedi_inc1.eta.<date>.nc4` (lat-lon increment, read by `swell_mkiau.csh`) |
| → `$JEDIWORK/ana` | `$EXPID.jedi_ana.ceta.<date>.nc4` (cubed analysis at the analysis time) |
| → `$FVWORK` | `$EXPID.jedi_{hofx,vbc}.<B>.tar`, `$EXPID.jedi_{var,cfg}.log.<B>.txt` |
| → `$SWELL_VARBC_DIR` | VarBC output at the window begin, input of the next cycle |
| → `$SWELL_OBS_SHARE/<ana>` | copy of the observations and biases used, read by the ensemble |

| swell_mkiau.csh | |
|---|---|
| → `$JEDIWORK/ana`, `$FVWORK/$EXPID.jedi_ana.<B>.tar` | `$EXPID.jedi_ana.eta.<ana>.nc4` (lat-lon analysis) |
| → `$JEDIWORK/inc`, `$FVWORK/$EXPID.jedi_inc.<B>.tar` | increments (`jedi_inc1` from JEDI, `jedi_inc` on the background grid) |
| → `$JEDIWORK/iau`, `$FVWORK/$EXPID.jedi_agcmrst.<B>.tar` | `$EXPID.agcm_import_rst.<B>.nc4` (tagged with the IAU start) |

Processor layout and GSIBEC grid follow from `SWELL_CENTRAL_CRES` (table in
`swell_ana_central.csh`; any `SWELL_CENTRAL_*` already set wins).

## Ensemble analysis data flow (swell_ana_atmens.csh)

| | |
|---|---|
| method | `SWELL_ENSEMBLE`: `eda` (eda_atmos), `getkf` (localensembleda), `getkf2` (localensembleda_2steps) |
| observations | `$SWELL_OBS_SHARE/<ana>`: the same observations and biases as the central analysis of the cycle |
| backgrounds | `$ATMENSLOC/ensbkgx/memNNN/*.bkg_clcv.*` read in place (swell links an unpacked `atmens_ebkgx` layout) |
| B (EDA) | GSIBEC climatological, `gsibec/$SWELL_GSIBEC_VERSION` |
| → member increments | lat-lon, from the JEDI solver (EDA `inc_iter1`, GETKF posterior ensemble increments) |
| → `$FVWORK/updated_ens/memNNN` | `$EXPID.ana.eta.<date>.nc4` = `dyn_jediupd.x`(`$ATMENSLOC/memNNN/$EXPID.bkg.eta`, increment) |
| → `$FVWORK/updated_ens` | `ensmean/`, `ensrms/` (filled by `post_eana.csh`), `$EXPID.atmens_olog.<ana>.tar.gz` |

The ensemble needs the central analysis of the same cycle to have run through
swell first (it reads `$SWELL_OBS_SHARE/<ana>`); it aborts otherwise. Layout and
GSIBEC grid follow from `SWELL_ENS_CRES` (13, 25, 91, 181).

## swell_run.csh

    swell_run.csh suite template exproot expid nymd nhms

- `expid` must be unique per role and cycle (e.g. `${EXPID}-ana-2026082612`).
- Templates are rendered with `vED -env`; any unresolved `$VAR` aborts.
  `SWELL_SLURM_TEMPLATE` is rendered the same way and passed with `-s`.
- Success requires `swell launch -b` to return 0 and swell's `cycle_done_*`
  marker in the cycle directory (`SWELL_DONE_MARKER=none` to skip).
- Each run leaves `<exproot>/<expid>/swell_run.info` (swell commit, JEDI build,
  override) and `cylc_log.tar.gz`. The `~/cylc-run` entry is removed on success
  (`SWELL_KEEP_CYLC_RUN=1` to keep) and kept on failure.

## Debugging interactively

    setenv FVHOME /path/to/exp
    source $FVROOT/bin/swell_env.csh        # SWELL_ENV_OK should be 1
    swell task RunJediVariationalExecutable <expdir>/<expid>-suite/experiment.yaml -d 2026-08-26T12:00:00Z -m geos_atmosphere
