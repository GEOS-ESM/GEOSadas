# GEOSdas_App/swell — run JEDI from ADAS through swell

ADAS owns time, data placement and model coupling; swell owns everything about
JEDI (YAML, obs operators, B, executables). ADAS ships no JEDI YAML: each role
has a small override template holding only the values ADAS knows.

| File | Purpose |
|---|---|
| `etc/swell_config.csh` | the one place that pins the swell venv/source/commit and the JEDI build |
| `swell_env.csh` | sourced: read the pin file, activate the venv, check the commit (`SWELL_ENV_OK`) |
| `swell_run.csh` | generic: render template → `swell create` → `swell launch -b` → check → clean |
| `swell_gsi2ioda.csh` | role: GSI ncdiag + GSI bias → IODA (suite `convert_ncdiags`) |
| `etc/convert_ncdiags.yaml.tmpl` | override template for `swell_gsi2ioda.csh` |

## Enabling in an experiment

    mkdir -p $FVHOME/run/swell
    cp $FVROOT/etc/swell/swell_config.csh $FVHOME/run/swell/    # edit pins if needed

With `$FVHOME/run/swell/swell_config.csh` present, `jedi_driver.csh` and
`jedi_driver_atmens.csh` use `swell_gsi2ioda.csh` instead of `jedi_gsi2ioda.csh`
(when `JEDI_OBS_OPT=3`). Templates and an optional `slurm.yaml` placed in
`$FVHOME/run/swell` take precedence over the installed ones in `$FVROOT/etc/swell`.

## swell_run.csh

    swell_run.csh suite template exproot expid nymd nhms

- `expid` must be unique per role and cycle (e.g. `${EXPID}-g2i-2026082612`).
- The template is rendered with `vED -env`; any unresolved `$VAR` aborts.
- Success requires `swell launch -b` to return 0 and swell's `cycle_done_*`
  marker in the cycle directory (`SWELL_DONE_MARKER=none` to skip).
- Each run leaves `<exproot>/<expid>/swell_run.info` (swell commit, JEDI build,
  override) and `cylc_log.tar.gz`. The `~/cylc-run` entry is removed on success
  (`SWELL_KEEP_CYLC_RUN=1` to keep) and kept on failure.

## Debugging interactively

    setenv FVHOME /path/to/exp
    source $FVROOT/bin/swell_env.csh        # SWELL_ENV_OK should be 1
    swell task GsiNcdiagToIoda <expdir>/<expid>-suite/experiment.yaml -d 2026-08-26T12:00:00Z -m geos_atmosphere
