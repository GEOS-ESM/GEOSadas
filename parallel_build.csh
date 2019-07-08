#!/bin/tcsh -f
#------------------------------------------------------------------------
# name: parallel_build.csh
# purpose: This subroutine builds using install
#          for compilation parallelization.
#
# !REVISION HISTORY
# 15Mar2007  Stassi  Modified version of g5das_parallel_build.csh
# 22Mar2007  Kokron  Add support for discover
# 28Mar2007  Stassi  Attempt to find default group for job submittal
# 13Apr2007  Stassi  Change order in which LOG/info names get iterated
# 04May2007  Stassi  Hard-code NCPUS=4 for discover
# 10Aug2007  TO/JS   Added distclean option; reordered script
# 15Apr2008  Stassi  Added code to cd to src directory, if not already
#                    there. Rename ESMASRC variable to ESMADIR
# 16Apr2008  Stassi  Use only half of CPUs on palm; mpi needs more memory
# 30Mar2008  Stassi  Add runtime options: -h, -hdf, -q queue
# 13Apr2009  Stassi  Updated for pleiades
# 05Aug2009  Stassi  Added -debug and -tmpdir flags
# 19Aug2010  Stassi  Added -walltime flag
# 01Apr2011  Stassi  Added clean and realclean options
# 04Nov2014  MAT     Moved to sbatch on discover, added Haswell 
#                    as an option, removed some older batch systems
# 07Jul2016  MAT     Added Broadwell at NAS. Removed Westmere. Made
#                    Broadwell default at NAS.
# 10Oct2017  MAT     Added Skylake at NAS. Added option to pass in 
#                    account
# 08Jul2019  MAT     Changes for git-based GEOSadas
#------------------------------------------------------------------------
set name = $0
set scriptname = $name
set BUILD_LOG_DIR = BUILD_LOG_DIR

setenv ARCH `uname -s`
#--set time = (1000 "%Uu %Ss %E  %X+%Dk %Mk %I+%Oio %Fpf+%Ww")
set time = ( 1000 "%Uu %Ss %E" )
set NCPUs_min = 6

#=====================
# determine the site 
#=====================
setenv SITE undetermined

set node = `uname -n`
if ( ($node == dirac)   \
  || ($node =~ borg*)   \
  || ($node =~ warp*)   \
  || ($node =~ discover*)) then
   setenv SITE nccs

else if (($node =~ pfe*)      \
      || ($node =~ bridge*)   \
      || ($node =~ r[0-9]*i[0-9]*n[0-9]*)) then
   setenv SITE nas
endif

# if batch, then skip over job submission
#----------------------------------------
if ($?Parallel_build_bypass_flag) then
   goto build
endif

# change to src directory, if not already there
#----------------------------------------------
if ($name != $name:t) then
   set scriptname = $name:t
   cd $name:h
endif
set srcdir = `pwd`
setenv ESMADIR $srcdir

# set defaults
#-------------
setenv ddb         0
setenv debug       ""
setenv verbose     ""
setenv interactive 0
setenv proc        ""
setenv prompt      1
setenv queue       ""
setenv account     ""
setenv tmpdir      ""
setenv walltime    ""

# check for runtime parameters
#-----------------------------
@ n = 0
while ($#argv)

   # usage information
   #------------------
   if ("$1" == "-help") goto usage
   if ("$1" == "-h"   ) goto usage

   # developer's debug
   #------------------
   if ("$1" == "-ddb") set ddb = 1

   # compile with debug
   #-------------------
   if (("$1" == "-debug") || ("$1" == "-db")) then
      setenv debug "-DCMAKE_BUILD_TYPE=Debug"
   endif

   # compile with GPU (if using pgfortran compiler)
   #-----------------------------------------------
   if ("$1" == "-gpu") then
      set GPU = 1
   endif

   # specify node type
   #------------------
   if ("$1" == "-sky")  set nodeTYPE = "Skylake"
   if ("$1" == "-bro")  set nodeTYPE = "Broadwell"
   if ("$1" == "-has")  set nodeTYPE = "Haswell"
   if ("$1" == "-ivy")  set nodeTYPE = "IvyBridge"
   if ("$1" == "-sand") set nodeTYPE = "SandyBridge"

   # reset Fortran TMPDIR
   #---------------------
   if ("$1" == "-tmpdir") then
      shift; if (! $#argv) goto usage
      setenv tmpdir $1
   endif

   # specify Fortran compiler
   #-------------------------
   if ("$1" =~ *=*) then
      set var = `echo $1 | cut -d= -f1`
      set val = `echo $1 | cut -d= -f2`
      if ($var == "ESMA_FC") setenv ESMA_FC $val
   endif

   # run job interactively
   #----------------------
   if ("$1" == "-i") set interactive = 1

   # run job interactively
   #----------------------
   if ("$1" == "-verbose") set verbose = "VERBOSE=1"
   if ("$1" == "-v") set verbose = "VERBOSE=1"

   # submit batch job to alternative queue/qos
   #------------------------------------------
   if ("$1" == "-q") then
      shift; if (! $#argv) goto usage
      if ($SITE == nccs) then
         setenv queue "--qos=$1"
      else
         setenv queue "-q $1"
      endif
   endif

   # submit batch job to specified account
   #--------------------------------------
   if ("$1" == "-account") then
      shift; if (! $#argv) goto usage
      setenv account "$1"
   endif

   # set batch walltime
   #-------------------
   if ("$1" == "-walltime") then
      shift; if (! $#argv) goto usage
      setenv walltime $1

      set hr = `echo $walltime | cut -d: -f1`
      set ss = `echo $walltime | cut -d: -f3`
      if (("$hr" == "$walltime") || ("$ss" == "")) then
         echo ""
         echo walltime must be in hr:mm:ss format
         goto usage
      endif
   endif

   # set noprompt option
   #--------------------
   if (("$1" == "-np") || ("$1" == "-noprompt")) then
      setenv prompt 0
   endif
   shift
end

# set BOPT=GPU if appropriate
#----------------------------
if ($?GPU && $?ESMA_FC) then
   if ("$ESMA_FC" == "pgfortran") setenv debug "BOPT=GPU"
endif

# default nodeTYPE
#-----------------
if (! $?nodeTYPE) then
   if ($SITE == nccs) set nodeTYPE = "Haswell"
   if ($SITE == nas)  set nodeTYPE = "Broadwell"
endif

# at NCCS
#--------
if ($SITE == nccs) then
   set nT = `echo $nodeTYPE| tr "[A-Z]" "[a-z]" | cut -c1-4 `
   if (($nT != hasw)) then
      echo "ERROR. Unknown node type at NCCS: $nodeTYPE"
      exit
   endif
   if ($nT == hasw) @ NCPUS_DFLT = 28

   if ($nT == hasw) set proc = 'hasw'
endif

# at NAS
#-------
if (($node =~ pfe*) || ($node =~ bridge*) ) then
   set nT = `echo $nodeTYPE | cut -c1-3 | tr "[A-Z]" "[a-z]"`
   if (($nT != san) && ($nT != ivy) && ($nT != has) && ($nT != bro) && ($nT != sky)) then
      echo "ERROR. Unknown node type at NAS: $nodeTYPE"
      exit
   endif
   if ($nT == sky) set nT = 'sky_ele'
   set proc = ":model=$nT"
   if ($nT == san)     @ NCPUS_DFLT = 16
   if ($nT == ivy)     @ NCPUS_DFLT = 20
   if ($nT == has)     @ NCPUS_DFLT = 24
   if ($nT == bro)     @ NCPUS_DFLT = 28
   if ($nT == sky_ele) @ NCPUS_DFLT = 40

   # TMPDIR needs to be reset
   #-------------------------
   if (! "$tmpdir") then
      set tmpdirDFLT = "/nobackup/$USER/scratch/"
      if ($prompt) then
         echo ""
         echo -n "Define TMPDIR location "
         echo    "where scratch files can be written during build."
         echo -n "TMPDIR [$tmpdirDFLT] "
         setenv tmpdir $<
         if ("$tmpdir" == "") setenv tmpdir $tmpdirDFLT
      endif
   endif
   echo "TMPDIR: $tmpdir"

endif

# developer's debug
#------------------
if ($ddb) then
   echo "ESMADIR = $ESMADIR"
   echo "debug = $debug"
   echo "verbose = $verbose"
   if ($?nodeTYPE) then
      echo "nodeTYPE = $nodeTYPE"
   endif
   echo "tmpdir = $tmpdir"
   if ($?ESMA_FC) then
      echo "ESMA_FC = $ESMA_FC"
   endif
   echo "proc = $proc"
   echo "interactive = $interactive"
   echo "queue = $queue"
   echo "account = $account"
   echo "walltime = $walltime"
   echo "prompt = $prompt"
   echo "NCPUS_DFLT = $NCPUS_DFLT"
   exit
endif

# if user defines TMPDIR ...
#---------------------------
if ("$tmpdir" != "") then

   # ... mkdir if it does not exist
   #-------------------------------
   if (! -d $tmpdir) then
      echo "Making TMPDIR directory: $tmpdir"
      mkdir -p $tmpdir
      if ($status) then
         echo ">> Error << mkdir $tmpdir "
         exit
      endif
   endif

   # ... check that it is writeable
   #-------------------------------
   if (! -w $tmpdir) then
      echo ">> Error << TMPIR is not writeable: $tmpdir"  
      exit
   endif
   echo ""

endif

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                            JOB SUBMISSION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (! -d $ESMADIR/@cmake) then
   if ($?PBS_JOBID || $?SLURM_JOBID) then
      echo " checkout_externals must be run!"
      echo " This requires internet access but you are on a compute node"
      echo " Please run from a head node"
      exit 1
   endif
endif

#========
# intro
#========
echo ""
echo "   ================"
echo "    PARALLEL BUILD "
echo "   ================"
echo ""

# ----------------------
# Run checkout_externals
# ----------------------
if (! -d $ESMADIR/@cmake) then
   echo " Running checkout_externals"
   checkout_externals
endif

# set environment variables
#--------------------------
source $ESMADIR/@env/g5_modules
setenv Pbuild_source_directory $ESMADIR/BUILD
setenv Parallel_build_bypass_flag
set jobname = "parallel_build"

# Make the BUILD directory
# ------------------------
if (! -d $Pbuild_source_directory) then
   echo "Making BUILD directory: $Pbuild_source_directory"
   mkdir -p $Pbuild_source_directory
   if ($status) then
      echo ">> Error << mkdir $Pbuild_source_directory "
      exit
   endif
endif

#===========================
# query for number of CPUs
#===========================
if ( ($node =~ discover*) || ($node =~ dali*) \
  || ($node =~ pfe*)      || ($node =~ bridge*) ) then

   # use set number of CPUs on discover/dali and pleiades
   #-----------------------------------------------------
   @ ncpus_val  = $NCPUS_DFLT
   @ numjobs_val  = $ncpus_val - 2  # save some CPUs for memory
   if ($numjobs_val > 14) @ numjobs_val = 14
   echo ""
   echo "The build will proceed with $numjobs_val parallel processes on $ncpus_val CPUs."

else

   # how many?
   #----------
   echo ""
   echo -n "Parallel build using how many CPUs ($NCPUs_min minimum)? "
   echo -n "[$NCPUS_DFLT] "
   set ncpus_val = $<
   if ("$ncpus_val" == "") set ncpus_val = $NCPUS_DFLT
   echo ""

   # check for minimum number of CPUs
   #---------------------------------
   if ($ncpus_val < $NCPUs_min) then
      @ ncpus_val = $NCPUs_min
      echo "Defaulting to minimum $ncpus_val CPUs."
   endif

   # use only even number of CPUs
   #-----------------------------
   @ check = ( $ncpus_val / 2 ) * 2
   if ($check != $ncpus_val) then
      @ ncpus_val ++
      echo "Rounding up to an even $ncpus_val CPUs."
   endif

   # save some CPUs for memory
   #--------------------------
   @ numjobs_val = $ncpus_val / 2
   echo -n "The build will proceed with $numjobs_val parallel processes"
   echo    " on $ncpus_val CPUs."

endif

setenv ncpus   $ncpus_val
setenv numjobs $numjobs_val

#=================================================
# check for LOG and info files from previous build
#=================================================
setenv bldlogdir $Pbuild_source_directory/$BUILD_LOG_DIR
setenv cmakelog  $bldlogdir/CLOG
setenv buildlog  $bldlogdir/LOG
setenv buildinfo $bldlogdir/info
setenv cleanFLAG ""

ls $cmakelog $buildlog $buildinfo >& /dev/null
if ($status == 0) then
   if ($prompt) then
      echo ''
      echo 'Previous build detected - Do you want to clean?'
      echo '(c)     clean: runs "make clean"'
      echo '(r) realclean: removes BUILD directory and re-runs CMake'
      echo '(n)  no clean'
      echo ''
      echo -n 'Select (c,r,n) <<c>> '

      set do_clean = $<
      if ("$do_clean" != "r" && "$do_clean" != "n") then
         set do_clean = "c"
      endif
   else
      set do_clean = "c"
   endif

   if ("$do_clean" == "c") then
      setenv cleanFLAG clean
      echo  "make clean before rebuild"
   else if ("$do_clean" == "r") then
      setenv cleanFLAG realclean
      echo  "remove BUILD directory and re-run CMake before rebuild"
   else
      echo "No clean before rebuild"
   endif 
endif

#==============
# info for user
#==============
echo ""
echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
echo "LOG and info will be written to the BUILD/$BUILD_LOG_DIR directory."
echo "Do the following to check the status/success of the build:"
echo ""
echo "  cd BUILD/$BUILD_LOG_DIR"
echo ""
echo "Note: Check the latest version of LOG for the most recent build info."
echo ""

# build if interactive
#---------------------
if ($?PBS_JOBID || $?SLURM_JOBID) then
   goto build
endif

#=============
# submit job
#=============
set groupflag = ""
if ("$account" != "") then
   if ($SITE == nas) then
      set groupflag = "-W group_list=$account"
   else if ($SITE == nccs) then
      set groupflag = "--account=$account"
   endif
else if (-e `which getsponsor` && (! $interactive)) then
   set group = ""
   set sponsorinfo = `getsponsor`

   while ("$group" == "")
      @ n = 1
      while ($n < $#sponsorinfo)
         if ($sponsorinfo[$n] =~ g[0..9]*) then
            set group = $sponsorinfo[$n]
            #--break  # uncomment this line to make 1st entry the default
         endif
         @ n++
      end

      getsponsor
      if ("$group" != "") then
         echo -n "select group: [$group] "
      else
         echo -n "select group:  "
      endif
      if ($prompt) then
         set reponse = $<
         if ("$reponse" != "") set group = $reponse
      endif
   end

   set groupflag = "--account=$group"
endif

if ($interactive) then
   @ numjobs = $numjobs - 2
   goto build
else if ( $SITE == nas ) then
   if ("$walltime" == "") setenv walltime "1:00:00"
   set echo
   qsub  $groupflag $queue     \
        -N $jobname            \
        -l select=1:ncpus=${ncpus}:mpiprocs=${numjobs}$proc \
        -l walltime=$walltime  \
        -S /bin/csh            \
        -V -j oe               \
        $0
   unset echo
   sleep 1
   qstat -a | grep $USER
else if ( $SITE == nccs ) then
   if ("$walltime" == "") setenv walltime "1:00:00"
   set echo
   sbatch $groupflag $queue    \
        --constraint=$proc     \
        --job-name=$jobname    \
        --output=$jobname.o%j  \
        --nodes=1              \
        --ntasks=${numjobs}    \
        --time=$walltime       \
        --partition=compute --qos=debug    \
        $0
   unset echo
   sleep 1
   # Add a longer format for the job name for scripting purposes
   squeue -a -o "%.10i %.12P %.10q %.30j %.8u %.8T %.10M %.9l %.6D %.6C %R" -u $USER
else
   echo $scriptname": batch procedures are not yet defined for node=$node at site=$SITE"
endif
exit



build:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                             BUILD SYSTEM
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set BUILDDIR = $Pbuild_source_directory
chdir $BUILDDIR

setenv ARCH `uname -s`

if ( $cleanFLAG == "realclean" ) then
   rm -rf $Pbuild_source_directory
   mkdir -p $Pbuild_source_directory
endif

#=================================================
# create $BUILD_LOG_DIR, plus LOG and info files
#=================================================

# create build log directory
#---------------------------
if (! -d $bldlogdir) mkdir -p $bldlogdir

# increment LOG and info files if previous versions exist
#--------------------------------------------------------
ls $cmakelog $buildlog $buildinfo >& /dev/null
if ($status == 0) then
   @ next = 0
   foreach file ( $cmakelog $buildlog $buildinfo )
      @ num = 1
      while (-e ${file}.$num)
         @ num ++
      end
      if ($num > $next) @ next = $num
   end
   set buildinfo = $bldlogdir/info.$next
   set buildlog  = $bldlogdir/LOG.$next
   set cmakelog  = $bldlogdir/CLOG.$next
endif

# alias definitions
#------------------
alias echo2 "echo \!* |& tee    $buildinfo $buildlog"
alias echo1 "echo \!* |& tee -a $buildinfo"
alias date1 "date     |& tee -a $buildinfo"

# initialize files
#-----------------
echo "Writing LOG and info files to directory: $bldlogdir:t"
echo2 ""

#================
# set environment  
#================
source $ESMADIR/@env/g5_modules

# write environment info to build log
#------------------------------------
if ("$tmpdir" != "") setenv TMPDIR $tmpdir
echo1 "======================================"
echo1 `uname -a`
echo1 "ESMADIR: $ESMADIR"
echo1 "BASEDIR: $BASEDIR"
echo1 "SITE: $SITE"
echo1 "TMPDIR = $TMPDIR"
if ($?ESMA_FC) then
   echo1 "ESMA_FC: $ESMA_FC"
endif
if ("$debug" != "") then
   echo1 "debug: $debug"
endif
if ("$verbose" != "") then
   echo1 "verbose: $verbose"
endif
if ("$queue" != "") then
   echo1 "queue: $queue"
endif
if ("$account" != "") then
   echo1 "account: $account"
endif

echo1 "======================================"
module list >>& $buildinfo
echo1 "======================================"

#===============
# build system
#===============
if ( $cleanFLAG == "clean" ) then
    echo1 ""
    echo1 "--------------------------------------"
    date1
    echo1  "make $cleanFLAG"
    make $cleanFLAG 
    set buildstatus = $status
    echo1 "clean complete; status = $buildstatus"
    date1
    echo1 "--------------------------------------"
    time >> $buildinfo
endif

set cmd1 = "cmake $ESMADIR -DBASEDIR=${BASEDIR}/${ARCH} $debug"
set cmd2 = "make --jobs=$numjobs install $verbose"
echo1 "" 
echo1 ""
echo1 "--------------------------------------"
echo1 $cmd1
$cmd1 |& tee -a $cmakelog
echo1 ""
echo1 "--------------------------------------"
echo1 $cmd2
date1
echo1  "Parallel build: $numjobs JOBS on $ncpus CPUs ... "
set echo
$cmd2 |& tee -a $buildlog
set buildstatus = $status

echo1 "build complete; status = $buildstatus"
date1
echo1 "Your build is located in $Pbuild_source_directory/install"
echo1 ""
echo1 "You can find setup scripts in $Pbuild_source_directory/install/bin"
echo1 "--------------------------------------"
time >> $buildinfo
echo1 ""

# check build results
#--------------------
chdir $bldlogdir
ls $buildlog >& /dev/null
set logstatus = $status

exit $buildstatus

usage:
cat <<EOF

usage: $0:t [ESMA_FC=fc] [flagged options]
where
    fc                 user-specified Fortran compiler

flagged options
   -debug (or -db)     compile with debug flags (BOPT=g)
   -gpu                compile with BOPT=GPU (only valid with ESMA_FC=pgfortran)
   -help (or -h)       echo usage information
   -i                  run interactively rather than queuing job
   -opt n              set optimization flag to n, where n={1,2,3,4,5}
   -q qos/queue        send batch job to qos/queue
   -account account    send batch job to account
   -np                 do not prompt for responses; use defaults
   -tmpdir dir         alternate Fortran TMPDIR location
   -walltime hh:mm:ss  time to use as batch walltime at job submittal

   -sky                compile on Skylake nodes (only at NAS)
   -bro                compile on Broadwell nodes (only at NAS, default at NAS)
   -has                compile on Haswell nodes (default at NCCS)
   -ivy                compile on Ivy Bridge nodes 
   -sand               compile on SandyBridge nodes
EOF
