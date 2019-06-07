#/bin/sh

#@ job_name=gsistats
#@ error=stats.e$(jobid)
#@ job_type=parallel
#@ network.MPI=csss,shared,us
#@ total_tasks=10
#@ blocking=unlimited
#@ class=dev
#@ group=devonprod
#@ environment = CHECKONE=one; CHECKTWO=two
#@ wall_clock_limit=00:20:00
#@ notification=error
#@ queue

exp=SIG62

set -x
rm -rf /ptmp/wx20kd/stats/$exp
mkdir -p /ptmp/wx20kd/stats/$exp
cd /ptmp/wx20kd/stats/$exp
rcp /nfsuser/g01/wx20kd/stats_gsi/sorc.1a/calcstats.exe ./stats.x
rcp /nfsuser/g01/wx20kd/global_gsi/fix/sst2dvar_stat0.5 ./berror_sst

cat << EOF > stats.parm
 &NAMSTAT
   jcap=62,nsig=64,nlat=96,nlon=192,maxcases=100,hybrid=.false.,smoothdeg=0.3,
 /
EOF

set +x

ls /ptmp/wx20kd/fcs/sig62/sigf24.fnl.200306* >> infiles
ls /ptmp/wx20kd/fcs/sig62/sigf48.fnl.200306* >> infiles

ls /ptmp/wx20kd/fcs/sig62/sigf24.fnl.200307* >> infiles
ls /ptmp/wx20kd/fcs/sig62/sigf48.fnl.200307* >> infiles

ls /ptmp/wx20kd/fcs/sig62/sigf24.fnl.200308* >> infiles
ls /ptmp/wx20kd/fcs/sig62/sigf48.fnl.200308* >> infiles

ls /ptmp/wx20kd/fcs/sig62/sigf24.fnl.200309* >> infiles
ls /ptmp/wx20kd/fcs/sig62/sigf48.fnl.200309* >> infiles

ls /ptmp/wx20kd/fcs/sig62/sigf24.fnl.200312* >> infiles
ls /ptmp/wx20kd/fcs/sig62/sigf48.fnl.200312* >> infiles

ls /ptmp/wx20kd/fcs/sig62/sigf24.fnl.200401* >> infiles
ls /ptmp/wx20kd/fcs/sig62/sigf48.fnl.200401* >> infiles

ls /ptmp/wx20kd/fcs/sig62/sigf24.fnl.200402* >> infiles
ls /ptmp/wx20kd/fcs/sig62/sigf48.fnl.200402* >> infiles

ls /ptmp/wx20kd/fcs/sig62/sigf24.fnl.200403* >> infiles
ls /ptmp/wx20kd/fcs/sig62/sigf48.fnl.200403* >> infiles


set -x
ln -s -f infiles fort.10

poe hpmcount ./stats.x < stats.parm  > gsistats.out
rc=$?

rcp /nfsuser/g01/wx20kd/stats_gsi/ctlfiles/stats62L64.hyb.ctl /ptmp/wx20kd/stats/$exp/stats.ctl
rcp /nfsuser/g01/wx20kd/stats_gsi/ctlfiles/balvar.ctl /ptmp/wx20kd/stats/$exp/