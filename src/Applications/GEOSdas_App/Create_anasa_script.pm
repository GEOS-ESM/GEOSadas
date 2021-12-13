package Create_anasa_script;
#########################################################################
#
# name - create_anasa_script.pm
#
# purpose - To create job script for stand-alone analysis
#
# !REVISION HISTORY:
#
# 06Nov2009  Todling   Initial code
# 02Jun2014  Todling   ObsClass now comes from Config file
#
#########################################################################
use strict;
use warnings;

use English;
use File::Basename;
use FindBin qw( $Bin );
use lib "$Bin";
use GMAO_utils ("get_siteID");

require Exporter;
our @ISA = "Exporter";
our @EXPORT_OK = qw( anasa_script );

#=======================================================================
sub anasa_script {

    # input parameters
    #-----------------
    my %inputparams = @_;

    my $fvhome          = $inputparams{"fvhome"};
    my $fvroot          = $inputparams{"fvroot"};
    my $fvbcs           = $inputparams{"fvbcs"};
    my $mywork          = $inputparams{"mywork"};
    my $jobsa           = $inputparams{"jobsa"};
    my $group_list      = $inputparams{"group_list"};
    my $export_none     = $inputparams{"export_none"};
    my $hyb_ens         = $inputparams{"hyb_ens"};
    my $jobqueue1       = $inputparams{"jobqueue1"};
    my $mem             = $inputparams{"mem"};
    my $ncpus_gsi       = $inputparams{"ncpus_gsi"};
    my $nodeflg         = $inputparams{"nodeflg"};
    my $fcswallclk      = $inputparams{"fcswallclk"};
    my $nametag         = $inputparams{"nametag"};
    my $gid             = $inputparams{"gid"};
    my $expid           = $inputparams{"expid"};
    my $g5gcm           = $inputparams{"g5gcm"};
    my $prepqc          = $inputparams{"prepqc"};
    my $oiqc            = $inputparams{"oiqc"};
    my $varwindow       = $inputparams{"varwindow"};
    my $acftbias        = $inputparams{"acftbias"};
    my $asynbkg_min     = $inputparams{"asynbkg_min"};
    my $varoffset       = $inputparams{"varoffset"};
    my $do4dvar         = $inputparams{"do4dvar"};
    my $do4diau         = $inputparams{"do4diau"};
    my $wcnstrt         = $inputparams{"wcnstrt"};
    my $nvarouter       = $inputparams{"nvarouter"};
    my $convsfc         = $inputparams{"convsfc"};
    my $convupa         = $inputparams{"convupa"};
    my $ncsuffix        = $inputparams{"ncsuffix"};
    my $berror_env      = $inputparams{"berror_env"};
    my $specres         = $inputparams{"specres"};
    my $osfmodule       = $inputparams{"osfmodule"};
    my $obClass         = $inputparams{"obClass"};
    my $newradbc        = $inputparams{"newradbc"};
    my $doRcorr         = $inputparams{"doRcorr"};
    my $qsub            = $inputparams{"qsub"};
  
 # local variables
 my( $os, $siteID, $nodeflg );

 $siteID = get_siteID();

 open(SCRIPT,">$fvhome/anasa/$jobsa.j") or
 die ">>> ERROR <<< cannot write $fvhome/anasa/$jobsa.j";

 print  SCRIPT <<"EOF";
#!/bin/csh -fx
#$group_list
#$jobqueue1
##SBATCH --export=NONE
#SBATCH --job-name=anasa
#SBATCH --output=anasa.log.o%j
#SBATCH --ntasks=$ncpus_gsi
#SBATCH --ntasks-per-node=24
#SBATCH --constraint=$nodeflg
#SBATCH --time=1:30:00
#PBS -N anasa
#PBS -o anasa.log.o%j
#PBS -l ncpus=$ncpus_gsi
#PBS -S /bin/csh
#PBS -j eo
# ------------------------------
#
# AnaStandAlone driver script.
#
# This file has been automatically generated by fvsetup.
#
# $nametag
#--------------------------------------------------------------------
 set myname = `basename \$0`
 cd # start at home for SLURM to resolve paths correctly

 setenv FVHOME  $fvhome  # experiment home directory
 if (-e \$FVHOME/run/FVDAS_Run_Config) source \$FVHOME/run/FVDAS_Run_Config


#
#                 ----------------------------------
#                  PART I - Prepare the Environment
#                 ----------------------------------

# Experiment environment
# ----------------------
  setenv GID $gid
  setenv group_list \"$group_list\"
  setenv ARCH `uname -s`
  setenv HOST `uname -n`
  setenv NCPUS   $ncpus_gsi   # number of CPUS
  setenv N_CPU   \$NCPUS
  setenv EXPID   $expid   # experiment ID
  setenv CASE    \$EXPID  # experiment ID (for LSM's sake)
  setenv FVROOT  `cat \$FVHOME/.FVROOT`
  setenv ANASA   \$FVHOME/anasa
  if(`uname -s` == "OSF1" ) then
    cd \$TMPDIR_UBC
    setenv FVWORK  `pwd`           # temporary fix while TMPDIR not avail on Halem
    setenv FVWORK  \$FVWORK/tmp.\$\$  # temporary fix while TMPDIR not avail on Halem
    /bin/mkdir -p \$FVWORK
  endif
  if(`uname -s` == "IRIX64" ) then
    setenv FVWORK  \$SCRATCH1 # working directory
  endif
  if( (`uname -s` == "Linux") && ((`uname -m` == "ia64") || (`uname -m` == "x86_64") )) then
      setenv FORT90L -Wl,-T
      setenv FVWORK $fvhome/../tmp.\$\$
      if(   -d \$FVWORK ) /bin/rm -r  \$FVWORK
      /bin/mkdir -p \$FVWORK
  endif

# Load BASEDIR and modules
# ------------------------
  unsetenv LD_LIBRARY_PATH
  source \$FVROOT/bin/g5_modules
  setenv LD_LIBRARY_PATH \${BASEDIR}/\${ARCH}/lib:\${FVROOT}/lib:\${LD_LIBRARY_PATH}

  setenv BATCH_SUBCMD $qsub

# Internal parameters controling system behavior
# ----------------------------------------------
  setenv ACFTBIAS $acftbias # knob for aircraft bias correction (1=on; 0=off)  
  setenv G5GCM   $g5gcm   # 1 = enables GEOS-5 GCM, 0 = disables GEOS-5 GCM (back to fvGCM)
  setenv PREPQC  $prepqc  # 1 = enables NCEP-QC, 0 = disables QC
  setenv OIQC    $oiqc    # 1 = enables OIQC, 0 = disables OIQC
  setenv NEWRADBC $newradbc
  setenv ANGLEBC  $newradbc
  setenv NOSFCANA 1       # for now, shut down sfc-analysis
  setenv TIMEINC $varwindow
  setenv ASYNBKG $asynbkg_min   # 1 = enables ASYNOPTIC BCKG mode, 0 = disables it
  setenv VAROFFSET $varoffset   # abs value of time off from 1st synoptic hour of var window
  setenv DO4DVAR  $do4dvar    # 1=enables 4dvar
  setenv DO4DIAU $do4diau
  setenv WCONSTRAINT $wcnstrt # set/unset weak constraint option
  setenv NVAROUTER $nvarouter # number of iteration for var loop
  setenv DIAG2ODS 1        # 1 = enables conversion of gsi-diag files to ODS, 0 = disables it
  setenv CONVSFC  $convsfc # 1 = enables call to lcv2prs for conversion of surface output fields
  setenv CONVUPA  $convupa # 1 = enables call to lcv2prs for conversion of upper-air output fields
  setenv NCSUFFIX $ncsuffix
  setenv GSI_NETCDF_DIAG 1
  setenv LOCFERR  /dev/null
  setenv DORCORR $doRcorr
EOF

  if ( $hyb_ens < 3 ) {
    print  SCRIPT <<"EOF";
# setenv HYBRIDGSI  \$FVHOME/atmens
# setenv STAGE4HYBGSI  \$HYBRIDGSI/central
EOF
  } else {
    print  SCRIPT <<"EOF";
  setenv HYBRIDGSI  \$FVHOME/atmens
  setenv STAGE4HYBGSI  \$HYBRIDGSI/central
EOF
  }

  print  SCRIPT <<"EOF";

# SSI/GSI specifics
# -----------------
  setenv NCEPINPUT  $fvbcs
  $berror_env

  setenv skipTRANSF                 # no transform needed for ana-sensitivity executable
  setenv skipSOLVER                 # need to run the analysis sensitivity solver
  setenv skipSATBIAS "-skipSATBIAS" # no need to worry about running satellite bias correction

  setenv SPECRES    $specres
  setenv SPECVTX    \$SPECRES
  setenv LEVSVTX     64        # this is the wired-in number of levels when tracking reloc used

# Needed for TLM/ADM
# ------------------
  setenv N_SMP                        \$NCPUS    # number of CPUS
  setenv N_MPI                        \$NCPUS    # number of MPI processes
  setenv NUMBER_MLP_PROCESSES         \${N_MPI} # still used by GCM
  setenv NUMBER_CPUS_PER_MLP_PROCESS  \${N_SMP} # still used by GCM


# Make sure files are accessible
# ------------------------------
  umask 022

# Add FVROOT/bin to front of path so fvDAS binaries are found first
# -----------------------------------------------------------------
  if ( `uname -s` == "Linux" ) then
    set path = ( . \$FVHOME/anasa \$FVHOME/run \$FVROOT/bin \$FVROOT/lib/grads \$BASEDIR/\$ARCH/bin \$path )
  else
    set path = ( . \$FVHOME/anasa \$FVHOME/run \$FVROOT/bin \$FVROOT/lib/grads \$path )
  endif

  echo \$FVWORK > \$FVHOME/.ANASA   # record working directory

  setenv EXTDATA \$FVWORK/ExtData  # External data directory
  /bin/mkdir -p \$EXTDATA
  /bin/touch \$EXTDATA/.no_archiving
  /bin/rm -f \$EXTDATA/g5chem
  /bin/ln -s \$NCEPINPUT/g5chem \$EXTDATA/


EOF

# System dependent configuration ..........................................
# ------------------------------
  $os = uc $OSNAME;

  if ( $os =~ /IRIX/ ) {

  print  SCRIPT <<"EOF";

  limit stacksize    4000000kb
  limit coredumpsize 0
  setenv MP_SLAVE_STACKSIZE 100000000       # bytes
  set stacksize = `limit stacksize | awk '{print \$2}' -`
  if (\${stacksize} == "unlimited" || \${stacksize} > 4000000) then
   setenv MP_STACK_OVERFLOW OFF
  endif
  setenv MPC_GANG off


EOF
   }

  elsif ( $os =~ /AIX/ ) {

  print  SCRIPT <<"EOF";

# IBM specific configuration
# --------------------------
  limit stacksize   4000000kb   # 4 Gbytes
  limit coredumpsize 0
  setenv MP_SLAVE_STACKSIZE 100000000       # bytes
  setenv MP_PROCS \${N_MPI}
  setenv MP_EUIDEVICE css0
  setenv MP_EUILIB us
  setenv MP_HOSTILE host.list
  setenv MP_INFOLEVEL 2
  setenv XLSMPOPTS "parthds=\${N_SMP}:schedule=static"
  setenv MP_CSS_INTERRUPT yes

EOF
}

  elsif ( $os =~ /LINUX/ ) {
    if(`uname -n` =~ /thunder/) {
       print  SCRIPT <<"EOF";


# Linux specific configuration
# ---------------------------------
  limit stacksize unlimited
  limit coredumpsize 0
  setenv KMP_STACKSIZE    450m
  unsetenv F_UFMTENDIAN
  setenv KMP_LIBRARY turnaround
  setenv MPI_DSM_DISTRIBUTE
  setenv MPI_TYPE_MAX 6553600
  setenv MPI_UNBUFFERED_STDIO
  setenv DSM_DISTRIBUTE
  setenv MPI_USE_XPMEM
  setenv MPI_BUFFER_MAX 2000
  setenv MPI_MSGS_MAX   10485760
EOF


    }elsif ($siteID eq "nas") {

  print  SCRIPT <<"EOF";

# Altix specific configuration
# ----------------------------
  limit stacksize unlimited
  limit coredumpsize 0
  setenv KMP_STACKSIZE    450m
  unsetenv F_UFMTENDIAN
  setenv KMP_LIBRARY turnaround
  setenv MPI_DSM_DISTRIBUTE  # pin MPI proceeses to assigned CPU
  setenv MPI_REQUEST_MAX 32768
  setenv MPI_TYPE_MAX 6553600
  setenv MPI_UNBUFFERED_STDIO
  setenv DSM_DISTRIBUTE
  setenv MPI_USE_XPMEM
  setenv MPI_BUFFER_MAX 2000
  setenv MPI_MSGS_MAX   10485760
  setenv MPI_GROUP_MAX  128

  setenv GADDIR   /u/ltakacs/grads
EOF


    }elsif ($siteID eq "nccs" ) {

  print  SCRIPT <<"EOF";

# Linux specific configuration
# ----------------------------
  limit stacksize unlimited
  limit coredumpsize 0

  source \$SHARE/dao_ops/opengrads/setup.csh 1.9-rc1-gmao

  if (\$?I_MPI_ROOT) then
#    setenv I_MPI_USE_DYNAMIC_CONNECTIONS 0
     setenv I_MPI_FABRICS shm:ofa
  endif
  setenv MPI_BUFS_PER_PROC 1024
EOF

    }else{ # end of Altix block

  print  SCRIPT <<"EOF";

# Linux specific configuration
# ----------------------------
  setenv FORT90L -Wl,-T
  setenv F_UFMTENDIAN big
  setenv KMP_STACKSIZE    450m
  setenv KMP_LIBRARY turnaround
  limit stacksize   unlimited
  limit coredumpsize 0
  setenv MPSTKZ     32M
EOF
    } # end of Linux block

} elsif ( $os =~ /OSF/ ) {

 $osfmodule = "fortran/551J";
 print  SCRIPT <<"EOF";


# OSF specific configuration
# --------------------------
 setenv KMP_STACKSIZE 64000000             # bytes
 setenv MP_STACK_SIZE 64000000             # bytes
 setenv PARALLEL            \${N_SMP}

EOF
}
# End of system dependent configuration ...................................


# Continue with rest of script
# ----------------------------
  print  SCRIPT <<"EOF";

# Define gsi command line execution
# ---------------------------------
  set ANAX = `which GSIsa.x`
  set SACX = `which sac.x`
  setenv MPIRUN_ANA    "esma_mpirun -perhost 8 -np \$NCPUS \$ANAX"
# setenv MPIRUN_SAC    "esma_mpirun -np \$NCPUS \$SACX"
  setenv MPIRUN_SAC    "esma_mpirun -np  1      \$SACX"

# Execute driving script for stand-alone analysis
# -----------------------------------------------

#
#                 -------------------------------------------------------
#                    PART II - Stage and Run 1 Stand-alone Analysis Exp
#                 ------------------------------------------------------

                             cd \$FVHOME/anasa
  
    if( -e atmens_replay.acq ) /bin/cp atmens_replay.acq \$FVWORK/

    if (\$?this_nymdhh) then
       set lstcases = `/bin/ls -1 standalone.\${this_nymdhh}z`
    else
       set lstcases = `/bin/ls -1 standalone.*`
    endif
    if ( \$status ) then
      echo \$myname": standalone cases listed"
      exit 1
    endif
    echo \$lstcases[1] | grep +
    if ( \$status ) then
 
       set fcst_end = `echo \$lstcases[1] | cut -d. -f2`
       set nymde = `echo \$fcst_end | cut -c1-8`
       set hhe   = `echo \$fcst_end | cut -c10-11`
       set nhmse = \${hhe}0000
       set fcst_beg = \$fcst_end
 
       set nymd = \$nymde
       set nhms = \$nhmse
 
       if ( \$LOCFERR != "/dev/null" ) then
            set ferrfile = `ls -1 \$LOCFERR/\$EXPID.ferr.eta.????????_??z+\${nymde}_\${hhe}z.\$NCSUFFIX`
            /bin/cp \$ferrfile[1] \$FVWORK/ferr.eta.\$NCSUFFIX
            set startag = `echo \$ferrfile[1] | cut -d. -f4 | cut -d+ -f1`
       endif
 
    else  # calculating n-day omfs
 
       set fcst_beg = `echo \$lstcases[1] | cut -d. -f2 | cut -d+ -f1`
       set fcst_end = `echo \$lstcases[1] | cut -d. -f2 | cut -d+ -f2`
 
       set nymdb = `echo \$fcst_beg | cut -c1-8`
       set yyb   = `echo \$fcst_beg | cut -c1-4`
       set mmb   = `echo \$fcst_beg | cut -c5-6`
       set ddb   = `echo \$fcst_beg | cut -c7-8`
       set nhmsb = `echo \$fcst_beg | cut -c1-8`
       set hhb   = `echo \$fcst_beg | cut -c10-11`
 
       set nymde = `echo \$fcst_end | cut -c1-8`
       set hhe   = `echo \$fcst_end | cut -c10-11`
       set nhmse = \${hhe}0000

       set fendm = `tick \$nymde \$nhmse -10800`
       set hhem  = `echo \$fendm[2] | cut -c1-2`
       set fcst_endm = \${fendm[1]}_\${hhem}00z

       set fendp = `tick \$nymde \$nhmse  10800`
       set hhep  = `echo \$fendp[2] | cut -c1-2`

       if ( \$LOCFERR != "/dev/null" ) then
            set fcst_endp = \${fendp[1]}_\${hhep}z
            set ferrfile = `ls -1 \$LOCFERR/\$EXPID.ferr.eta.\${fcst_beg}\+\${fcst_end}.\$NCSUFFIX`
            if( -e \$ferrfile ) then
              /bin/cp \$ferrfile \$FVWORK/ferr.eta.\$NCSUFFIX
              set startag = `echo \$ferrfile[1] | cut -d. -f4 | cut -d+ -f1`
            else
              echo " ferr not found "
              exit 1
            endif 
       endif
       set fcst_endp = \${fendp[1]}_\${hhep}00z

#      Get prognostic files
#      --------------------
       setenv LOCPROG /archive/u/\$USER/\$EXPID/prog/Y\${yyb}/M\${mmb}/D\${ddb}/H\${hhb}
       set NOMF = \$FVHOME/Nbkg
       mkdir \$NOMF
       touch \$NOMF/.no_archiving
       set fname = "acquire.pbs"
       /bin/rm \$fname
       alias echo1 "echo \\!* >! \$fname"
       alias echo2 "echo \\!* >> \$fname"
       if (`uname -n` =~ borg*) then
          set data_queue=datamove
       else if (`uname -n` =~ r*i[0-3]n*) then
          set data_queue=normal
       else
          set data_queue=""
       endif
       echo1 "#\\!/bin/csh -xvf"
       echo2 "#$group_list"
       echo2 "#$export_none"
       echo2 "#SBATCH --account=$gid"
       echo2 "#SBATCH --time=2:00:00"
       echo2 "#SBATCH --job-name=acquire"
       echo2 "#SBATCH --output=acquire.log.o%j"
       echo2 "#SBATCH --ntasks=1"
       echo2 "#SBATCH --partition=\$data_queue"
       echo2 "#PBS -N acquire"
       echo2 "#PBS -o acquire.log.o%j"
       echo2 "#PBS -l nodes=1:ppn=1"
       echo2 "#PBS -l walltime=1:00:00"
       echo2 "#PBS -q \$data_queue"
       echo2 "#PBS -S /bin/csh"
       echo2 "#PBS -V"
       echo2 "#PBS -j eo"
       echo2 "/bin/cp \$LOCPROG/\$EXPID.prog.eta.\${fcst_beg}+\${fcst_endm}.\$NCSUFFIX \$NOMF/\$EXPID.bkg.eta.\${fcst_endm}.\$NCSUFFIX"
       echo2 "/bin/cp \$LOCPROG/\$EXPID.prog.eta.\${fcst_beg}+\${fcst_end}.\$NCSUFFIX  \$NOMF/\$EXPID.bkg.eta.\${fcst_end}.\$NCSUFFIX"
       echo2 "/bin/cp \$LOCPROG/\$EXPID.prog.eta.\${fcst_beg}+\${fcst_endp}.\$NCSUFFIX \$NOMF/\$EXPID.bkg.eta.\${fcst_endp}.\$NCSUFFIX"
       echo2 "/bin/cp \$LOCPROG/\$EXPID.prog.sfc.\${fcst_beg}+\${fcst_endm}.\$NCSUFFIX \$NOMF/\$EXPID.bkg.sfc.\${fcst_endm}.\$NCSUFFIX"
       echo2 "/bin/cp \$LOCPROG/\$EXPID.prog.sfc.\${fcst_beg}+\${fcst_end}.\$NCSUFFIX  \$NOMF/\$EXPID.bkg.sfc.\${fcst_end}.\$NCSUFFIX"
       echo2 "/bin/cp \$LOCPROG/\$EXPID.prog.sfc.\${fcst_beg}+\${fcst_endp}.\$NCSUFFIX \$NOMF/\$EXPID.bkg.sfc.\${fcst_endp}.\$NCSUFFIX"
       echo2 "exit"

       if ( \$BATCH_SUBCMD == "sbatch" ) then
          \$BATCH_SUBCMD -W \$fname
       else
          \$BATCH_SUBCMD -W block=true \$fname
       endif

       set nymd = \$nymde
       set nhms = \$nhmse

    endif
    
    fvssi -obsclass \$OBSCLASS \$skipTRANSF \$skipSOLVER \$skipSATBIAS \$FVHOME \$FVWORK \$FVWORK \$nymd \$nhms \$EXPID

    set rc = \$status
    if ( \$rc > 0 && \$rc < 100 ) then
         echo \$myname": abnormal error condition from fvssi ..."
         exit 1
    endif

    cd \$FVWORK
    ls -lrt
    foreach fn ( `ls \$EXPID.diag_*bin` )
      set prefix = `echo \$fn | cut -d. -f1-2`
      set suffix = `echo \$fn | cut -d. -f3-`
      /bin/mv \$fn \$FVWORK/\$prefix.\${fcst_beg}+\$suffix
    end
    if ( \$fcst_beg != \$fcst_end ) then
      foreach fn ( `ls \$EXPID.diagsa_*ods` )
        set prefix = `echo \$fn | cut -d. -f1`
        set postpx = `echo \$fn | cut -d. -f2`
        set suffix = `echo \$fn | cut -d. -f3-`
        /bin/mv \$fn \$FVWORK/\$prefix.\$postpx.\${fcst_beg}+\${suffix}
      end
      foreach fn ( `ls \$EXPID.*.log*txt` )
        set prefix = `echo \$fn | cut -d. -f1-3`
        /bin/mv \$fn \$FVWORK/\$prefix.\${fcst_beg}+\${fcst_end}.txt
      end
      if ( -e \$EXPID.anasa.eta.\${fcst_beg}.\$NCSUFFIX ) then # do not allow saving anasa.eta when
                                                               # running observer on prognostic files
         /bin/mv \$EXPID.anasa.eta.\${fcst_beg}.\$NCSUFFIX _\$EXPID.anasa.eta.\${fcst_beg}.\$NCSUFFIX
      endif
    endif

    if ( \$LOCFERR != "/dev/null" ) then
         set ferrfile = `ls -1 \$FVWORK/\$EXPID.ferr_rm1.eta.\${nymde}_\${hhe}z.\$NCSUFFIX`
         if ( -e \$ferrfile ) then
           set prefix = `echo \$ferrfile[1] | cut -d. -f1-4`
           set suffix = `echo \$ferrfile[1] | cut -d. -f5-`
           /bin/mv \$ferrfile[1] \${prefix}.\${startag}+\${suffix}
         endif
    endif

#
#
#                 ----------------------------------------
#                      PART III - Data Archival
#                 ---------------------------------------

                           cd \$FVHOME/anasa


# Move files to FVHOME for archiving and/or post-processing
# ---------------------------------------------------------
  ls -altuF \$FVWORK
  setenv PESTOROOT `dirname \$FVHOME`
  pesto -v -d \$FVWORK -arc anasastorage.arc -cp /bin/mv


# Mass storage archival as an 1 CPU batch job
# -------------------------------------------
  if ( !(\$?this_nymdhh) ) then   # only archive when submitting one analysis at a time
     if ( \$BATCH_SUBCMD == "sbatch" ) then
        sbatch -J anasa.archive -o anasa.archive.log.o%j --export=arch_type=ANASA \$FVHOME/run/fvarchive.j
     else
        qsub   -N anasa.archive -o anasa.archive.log.o%j -v arch_type=ANASA \$FVHOME/run/fvarchive.j
     endif
  endif

# --------------------------
# No post-processing for now
# --------------------------


#
#
#                  ------------------------------
#                    PART IV - Next Job Segment
#                  ------------------------------

                          cd \$FVHOME/anasa

# If forecast sucessfully completed, remove rs and resubmit
# ---------------------------------------------------------
 if ( \$rc == 0 || \$rc == 101 ) then


  /bin/rm \$lstcases[1]

  if (\$?this_nymdhh) then
     if( (`uname -s` == "Linux") && ((`uname -m` == "ia64")||(`uname -m` == "x86_64")) ) then
        cd
        /bin/rm -r \$FVWORK
     endif
  else
     set lstcases = `/bin/ls -1 standalone.*`
     if ( \$status ) then
          echo \$myname": no more restart files, forecast job completed"
          exit 0
     endif
     if ( \$#lstcases > 0 ) then
        set fcst_nxt = `echo \$lstcases[1] | cut -d. -f2 | cut -d+ -f1`
        set jname = a\${fcst_nxt}
        set lname = \$jname.log.o%j
        if ( \$BATCH_SUBCMD == "sbatch" ) then
           sbatch -d afterany:\${PBS_JOBID} -J \$jname -o \$lname $jobsa.j
        else
           qsub -W depend=afterany:\${PBS_JOBID} -N \$jname -o \$lname $jobsa.j
        endif
     endif
  endif

# Because on Columbia this is not a legitimate TMPDIR, remove dir to avoid pile up
# --------------------------------------------------------------------------------
  if( (`uname -s` == "Linux") && ((`uname -m` == "ia64")||(`uname -m` == "x86_64")) ) then
     cd
     /bin/rm -r \$FVWORK
  endif

# or else, something went wrong
# -----------------------------
  else

   echo \$myname": unexpected return code from fvpsas, rc = \$rc"
   exit 1

  endif

# All done
# --------
  exit 0

EOF

 close(SCRIPT);

}

1;
