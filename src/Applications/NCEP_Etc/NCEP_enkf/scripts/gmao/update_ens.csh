#!/bin/csh
# update_ens.csh - simple script to reposition new ensemble
#                  for ease of moving it around
#
# !REVISION HISTORY:
#
#  04Nov2011  Todling   Initial script
#  28Jan2013  Todling   Add prog.eta handle
#  13Jun2014  Todling   Add abkg,cbkg and diag handle
#  28Jun2016  Todling   Handle for gaas_bkg.sfc
#  07Mar2017  Todling   Handle for AOD-related files
#  20Mar2017  Todling   Handle for GAAS-related files
#  26Mar2017  Todling   Add aconc to handle aerosol concentrations
#  04Apr2017  Todling   Add non-inflated analysis (niana) for offline FSO
#  19Nov2019  Todling   Add high-res (lat-lon) Bkg.eta file
#  20Jun2020  Todling   A little more flexible member index
#-------------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME update_ens.csh

if ( $#argv < 6 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - saves updated ensemble member as it completes running" 
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid member type workdir rcfile ncsuffix"
   echo " "
   echo " where"
   echo "   expid    -  usual experiment name, e.g., u000\_c72"
   echo "   member   -  current ensemble member name"
   echo "   type     -  file type, e.g. ana or bkg"
   echo "   workdir  -  work directory where run is taking place"
   echo "   rcfile   -  typically HISTORY file, otherwise NULL"
   echo "   ncsuffix -  filename suffix (nc4 or hdf)"
   echo " "
   echo " DESCRIPTION "
   echo " "
   echo "   This procedure is responsible for moving the analysis and background "
   echo "  to their proper location within ENSWORK, after they have been generated" 
   echo "  by the underlying ensemble analysis system and the multiple calls the "
   echo "  ensemble of GCMs."
   echo " "
   echo "  These files will end-up under a directory named updated_ens under ENSWORK."
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME u000_c72 4 bkg /discover/nobackup/$user/enswork.1234 nc4"
   echo " "
   echo " REMARKS"
   echo " "
   echo "  1. Changing the location within ENSWORK where updated members end up is"
   echo "     not an option the user can control. This is part of the internal "
   echo "     mechanisms of the ensemble scripts"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 22Jun2020      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

    set expid    = $1
    set member   = $2
    set type     = $3
    set workdir  = $4
    set rcfile   = $5
    set ncsuffix = $6

    cd $workdir

    if ( "$type" == "bkg" ) then
        mkdir -p ../updated_ens/$member
        touch    ../updated_ens/.no_archiving
        foreach fntype ( Bkg.eta bkg.eta bkg.sfc abkg.eta cbkg.eta gaas_bkg.sfc prog.eta asm.eta )
           foreach fn ( `/bin/ls $expid.${fntype}.*.${member}.$ncsuffix` )
              set newname = `echo $fn | cut -d. -f1-4 `
              /bin/mv $fn ../updated_ens/${member}/$newname.$ncsuffix
           end # fn
        end # fntype
    endif
    if ( "$type" == "niana" ) then
        mkdir -p ../updated_ens/${member}
        touch    ../updated_ens/.no_archiving
        foreach fn ( `/bin/ls $expid.niana.eta.*.${member}.$ncsuffix ` )
           set newname = `echo $fn | cut -d. -f1-4 `
           /bin/mv $fn ../updated_ens/${member}/$newname.$ncsuffix
#          ln -sf ../updated_ens/${member}/$newname.$ncsuffix $fn
        end
    endif
    if ( "$type" == "ana" ) then
        mkdir -p ../updated_ens/${member}
        touch    ../updated_ens/.no_archiving
        foreach fn ( `/bin/ls $expid.ana.eta.*.${member}.$ncsuffix $expid.inc.eta.*.${member}.$ncsuffix $expid.xinc.eta.*.${member}.$ncsuffix` )
           set newname = `echo $fn | cut -d. -f1-4 `
           /bin/mv $fn ../updated_ens/${member}/$newname.$ncsuffix
           ln -sf ../updated_ens/${member}/$newname.$ncsuffix $fn
        end
    endif
    if ( "$type" == "aconc" ) then
        mkdir -p ../updated_ens/${member}
        touch    ../updated_ens/.no_archiving
        foreach fn ( `/bin/ls $expid.aana.eta.*.${member}.$ncsuffix $expid.ainc.eta.*.${member}.$ncsuffix $expid.aker.eta.*.${member}.$ncsuffix` )
           set newname = `echo $fn | cut -d. -f1-4 `
           /bin/mv $fn ../updated_ens/${member}/$newname.$ncsuffix
           ln -sf ../updated_ens/${member}/$newname.$ncsuffix $fn
        end
    endif
    if ( "$type" == "aaero" ) then
        mkdir -p ../updated_ens/${member}
        touch    ../updated_ens/.no_archiving
        foreach fn ( `/bin/ls $expid.aod_[a,d,k].sfc.*.${member}.$ncsuffix $expid.gaas_ana.eta.*.${member}.$ncsuffix $expid.gaas_inc.eta.*.${member}.$ncsuffix` )
           set newname = `echo $fn | cut -d. -f1-4 `
           /bin/mv $fn ../updated_ens/${member}/$newname.$ncsuffix
           ln -sf ../updated_ens/${member}/$newname.$ncsuffix $fn
        end
    endif
    if ( "$type" == "baero" ) then
        mkdir -p ../updated_ens/${member}
        touch    ../updated_ens/.no_archiving
        foreach fn ( `/bin/ls $expid.aod_f.sfc.*.${member}.$ncsuffix $expid.gaas_bkg.eta.*.${member}.$ncsuffix` )
           set newname = `echo $fn | cut -d. -f1-4 `
           /bin/mv $fn ../updated_ens/${member}/$newname.$ncsuffix
           ln -sf ../updated_ens/${member}/$newname.$ncsuffix $fn
        end
    endif
    if ( "$type" == "fsens" ) then
        mkdir -p ../updated_ens/${member}
        touch    ../updated_ens/.no_archiving
        foreach fn ( `/bin/ls $expid.Jnorm.*.txt $expid.Jgradf*.eta.*.$ncsuffix $expid.fsens*.eta.*.$ncsuffix` )
           /bin/mv $fn ../updated_ens/${member}/
           ln -sf ../updated_ens/${member}/$fn .
        end
    endif
    # The following handles HISTORY diagnostic; typically files with unconventional name template
    if ( "$type" == "diag" ) then
        mkdir -p ../updated_ens/ensdiag/${member}
        touch    ../updated_ens/ensdiag/.no_archiving
        set fntype_all = (`edhist.pl -q 3 -list inc -X Bkg.eta,bkg.eta,bkg.sfc,abkg.eta,cbkg.eta,gaas_bkg.sfc -i $rcfile`)
        foreach fntype ( $fntype_all )
           if ( $fntype == "bkg_clcv" ) then
              if ( ! -d   ../updated_ens/ensbkgx/${member} ) then
                 mkdir -p ../updated_ens/ensbkgx/${member}
                 touch    ../updated_ens/ensbkgx/.no_archiving
                 foreach fn ( `/bin/ls $expid.${fntype}.*.${member}.$ncsuffix` )
                    set newname = `echo $fn | cut -d. -f1-3 `
                    /bin/mv $fn ../updated_ens/ensbkgx/${member}/$newname.$ncsuffix
                 end # fn
              endif
           else 
              foreach fn ( `/bin/ls $expid.${fntype}.*.${member}.$ncsuffix` )
                 set newname = `echo $fn | cut -d. -f1-3 `
                 /bin/mv $fn ../updated_ens/ensdiag/${member}/$newname.$ncsuffix
              end # fn
           endif
        end # fntype
    endif
