#!/bin/csh -f
#
# swell_render.csh - locate a template, fill in $VARs from the environment, reject leftovers
#
# usage: swell_render.csh template output
#
#   template  path, or bare name searched for in $FVHOME/run/swell, $FVROOT/etc/swell,
#             $SWELL_ETC_FALLBACK (in this order)
#   output    rendered file
#
# Rendering is done with vED -env; any $VAR or ${VAR} left over (outside comment
# lines) is an error. The template path actually used is printed on success.
# Exit status: 0 on success, 1 on failure.
# ---------------------------------------------------------------------------

set myname = swell_render.csh

if ( $#argv < 2 ) then
   echo "usage: $myname template output"
   exit 1
endif
set template = $1
set output   = $2

which vED >& /dev/null
if ( $status ) then
   echo "${myname}: vED not found in PATH, aborting"
   exit 1
endif

set tmpl = ""
if ( -e $template ) then
   set tmpl = $template
else
   set search = ()
   if ( $?FVHOME ) then
      set search = ( $search $FVHOME/run/swell )
   endif
   if ( $?FVROOT ) then
      set search = ( $search $FVROOT/etc/swell )
   endif
   if ( $?SWELL_ETC_FALLBACK ) then
      set search = ( $search $SWELL_ETC_FALLBACK )
   endif
   foreach d ( $search )
      if ( "$tmpl" == "" && -e $d/$template ) set tmpl = $d/$template
   end
endif
if ( "$tmpl" == "" ) then
   echo "${myname}: cannot find template $template, aborting"
   exit 1
endif

vED -env $tmpl -o $output
if ( $status || ! -e $output ) then
   echo "${myname}: failed to render $tmpl, aborting"
   exit 1
endif

# (\x24 is '$'; a literal dollar cannot be used inside csh backquotes)
set nunres = `grep -v '^[[:space:]]*#' $output | grep -cP '\x24\{?[A-Za-z_]'`
if ( $nunres > 0 ) then
   echo "${myname}: unresolved variables in $output (from $tmpl):"
   grep -v '^[[:space:]]*#' $output | grep -nP '\x24\{?[A-Za-z_]'
   exit 1
endif

echo $tmpl
exit 0
