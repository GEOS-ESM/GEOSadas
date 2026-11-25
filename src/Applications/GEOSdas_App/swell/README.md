# GEOSdas_App/swell — run JEDI from ADAS through swell

ADAS owns time, data placement and model coupling; swell owns everything about
JEDI (YAML, obs operators, B, executables). ADAS ships no JEDI YAML: each role
has a small override template holding only the values ADAS knows.

| File | Purpose |
|---|---|
| `etc/swell_config.csh` | the one place that pins the swell venv/source/commit and the JEDI build, plus role settings |
| `swell_env.csh` | sourced: read the pin file, activate the venv, check the commit (`SWELL_ENV_OK`) |
| `swell_render.csh` | find a template, fill `$VAR`s from the environment, reject leftovers |
| `swell_run.csh` | generic: render templates → `swell create` → `swell launch -b` → check → clean |
| `swell_gsi2ioda.csh` | role: GSI ncdiag + GSI bias → IODA (suite `convert_ncdiags`) |
| `swell_ana_central.csh` | role: central 3D-FGAT analysis (suite `3dfgat_atmos`), replaces `jedi_set.csh` + `jedi_run.csh` |
| `etc/convert_ncdiags.yaml.tmpl` | override for `swell_gsi2ioda.csh` |
| `etc/central_3dfgat.yaml.tmpl`, `etc/central_slurm.yaml.tmpl` | override and slurm settings for `swell_ana_central.csh` |

## Enabling in an experiment

    mkdir -p $FVHOME/run/swell
    cp $FVROOT/etc/swell/swell_config.csh $FVHOME/run/swell/
    # edit: SWELL_CENTRAL_CRES (npx of the JEDI backgrounds, e.g. 13 for C12, 91 for C90)
    #       and, if needed, the swell/JEDI pins and SWELL_STATIC_FILES

With `$FVHOME/run/swell/swell_config.csh` present (opt-in):

- `jedi_driver.csh` / `jedi_driver_atmens.csh` convert GSI output with
  `swell_gsi2ioda.csh` instead of `jedi_gsi2ioda.csh` (when `JEDI_OBS_OPT=3`).
- `jedi_driver.csh` runs the central analysis with `swell_ana_central.csh`
  instead of `jedi_set.csh` + `jedi_run.csh` when `SWELL_CENTRAL=1`,
  `JEDI_HYBRID=0` and `JEDI_OBS_OPT` is 2 or 3; otherwise the old path is used.
  `jedi_mkiau.csh` and `jedi_upd4geos.csh` follow unchanged.

Templates and an optional `slurm.yaml` placed in `$FVHOME/run/swell` take
precedence over the installed ones in `$FVROOT/etc/swell`.

## Central analysis data flow (swell_ana_central.csh)

| | |
|---|---|
| observations | `$FVWORK/ioda.<ana>` (from `swell_gsi2ioda.csh`) → swell `ioda_locations_not_in_r2d2` |
| backgrounds | `$SWELL_BKG_ARCHIVE/$EXPID/rs/Y/M/$EXPID.bkgcrst.<ana-9h>.tar` (swell `GetBackgroundGeosExperiment`) |
| bias correction | previous cycle's JEDI VarBC in `$SWELL_VARBC_DIR` when `JEDI_FEEDBACK_VARBC=1`, else GSI biases |
| B | GSIBEC climatological, `gsibec/$SWELL_GSIBEC_VERSION` in `$SWELL_STATIC_FILES` |
| → `$JEDIWORK/inc` | `$EXPID.jedi_inc1.eta.<date>.nc4` (lat-lon increment, read by `jedi_mkiau.csh`) |
| → `$JEDIWORK/ana` | `$EXPID.jedi_ana.ceta.<date>.nc4` (cubed analysis at the analysis time) |
| → `$FVWORK` | `$EXPID.jedi_{hofx,vbc}.<B>.tar`, `$EXPID.jedi_var.log.<B>.txt` |
| → `$SWELL_VARBC_DIR` | VarBC output at the window begin, input of the next cycle |

Processor layout and GSIBEC grid follow from `SWELL_CENTRAL_CRES` (table in
`swell_ana_central.csh`; any `SWELL_CENTRAL_*` already set wins).

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
