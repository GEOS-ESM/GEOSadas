#!/bin/csh
#
# swell_env.csh - set up the environment for the swell command line (swell, cylc)
#
# usage (must be sourced):  source swell_env.csh
#
# On return SWELL_ENV_OK is 1 on success and 0 on failure (the reason is printed).
# The calling script must check it; this file never exits, so it is also safe
# to source from an interactive shell for debugging.
#
# Notes:
#  - PYTHONPATH/PYTHONHOME are cleared: the ADAS (GEOSpyD) python settings break
#    the swell venv. Call this from a script (own process), not from the ADAS job
#    shell itself, so the ADAS environment is left untouched.
#  - HOSTNAME is exported if missing; swell uses it to detect the NCCS platform.
#  - cylc tasks do not inherit this environment: each task sources the
#    'modules' file that swell writes into the suite directory.
# ---------------------------------------------------------------------------

setenv SWELL_ENV_OK 0

# locate and read the pin file
if ( ! $?SWELL_CONFIG ) then
   if ( $?FVHOME ) then
      if ( -e $FVHOME/run/swell/swell_config.csh ) setenv SWELL_CONFIG $FVHOME/run/swell/swell_config.csh
   endif
endif
if ( ! $?SWELL_CONFIG ) then
   if ( $?FVROOT ) then
      if ( -e $FVROOT/etc/swell/swell_config.csh ) setenv SWELL_CONFIG $FVROOT/etc/swell/swell_config.csh
   endif
endif

if ( ! $?SWELL_CONFIG ) then
   if ( $?SWELL_ETC_FALLBACK ) then
      if ( -e $SWELL_ETC_FALLBACK/swell_config.csh ) setenv SWELL_CONFIG $SWELL_ETC_FALLBACK/swell_config.csh
   endif
endif

if ( ! $?SWELL_CONFIG ) then
   echo "swell_env.csh: cannot find swell_config.csh (set SWELL_CONFIG, or provide FVHOME/run/swell or FVROOT/etc/swell)"
else if ( ! -e $SWELL_CONFIG ) then
   echo "swell_env.csh: SWELL_CONFIG=$SWELL_CONFIG does not exist"
else
   source $SWELL_CONFIG

   if ( $?PYTHONPATH ) unsetenv PYTHONPATH
   if ( $?PYTHONHOME ) unsetenv PYTHONHOME
   if ( ! $?HOSTNAME ) setenv HOSTNAME `hostname`

   if ( ! -e $SWELL_VENV/bin/activate.csh ) then
      echo "swell_env.csh: no venv at SWELL_VENV=$SWELL_VENV"
   else
      source $SWELL_VENV/bin/activate.csh
      rehash
      set swell_exe = `which swell`
      if ( $status ) then
         echo "swell_env.csh: swell not found in $SWELL_VENV/bin"
      else
         setenv SWELL_ENV_OK 1

         # provenance and commit pin
         setenv SWELL_HEAD unknown
         if ( -d $SWELL_SRC/.git ) setenv SWELL_HEAD `git -C $SWELL_SRC rev-parse HEAD`
         set swell_dirty = ""
         if ( -d $SWELL_SRC/.git ) then
            set nmod = `git -C $SWELL_SRC status --porcelain --untracked-files=no | wc -l`
            if ( $nmod > 0 ) set swell_dirty = " (with $nmod uncommitted change(s))"
         endif
         echo "swell_env.csh: swell  = $swell_exe"
         echo "swell_env.csh: commit = $SWELL_HEAD$swell_dirty  [$SWELL_SRC]"
         echo "swell_env.csh: JEDI   = $SWELL_JEDI_BUILD"

         if ( "$SWELL_COMMIT" != "" ) then
            echo $SWELL_HEAD | grep -q "^$SWELL_COMMIT"
            if ( $status ) then
               echo "swell_env.csh: swell commit $SWELL_HEAD does not match pinned SWELL_COMMIT=$SWELL_COMMIT"
               if ( $SWELL_COMMIT_STRICT ) setenv SWELL_ENV_OK 0
            endif
         endif
         unset nmod
      endif
      unset swell_exe swell_dirty
   endif
endif
