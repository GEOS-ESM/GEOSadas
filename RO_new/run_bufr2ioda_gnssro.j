#!/bin/bash

# Slurm
# -----
#SBATCH --account=g0613
#SBATCH --job-name=jedi_ufo_tests
#SBATCH --output=jedi_ufo_tests.o%j
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --constraint=mil
#SBATCH --qos=advda
#SBATCH --time=00:30:00


# Path to JEDI build
# ------------------
export jedibuild=/discover/nobackup/wgu/jedi_build/fv3-bundle/build-intel-release/
#export jedibuild=/discover/nobackup/projects/gmao/advda/swell/JediBundles/fv3_soca_SLES15_07042025/build-intel-release/
export bufrbuild=/discover/nobackup/wgu/bufr-query/

# Load modules
# ------------
source $MODULESHOME/init/bash
module purge
#source $jedibuild/modules.csh
source $jedibuild/modules

# List modules
# ------------
module list

cd /discover/nobackup/mganesha/DSI_PBL/fromWei

#mpirun -np 1 $jedibuild/bin/bufr2ioda.x iasi_bufr2ioda.yaml
#srun -n 1 $bufrbuild/build/bin/bufr2netcdf.x  ./input/gdas1.240715.t18z.gpsro.tm00.bufr_d gnssro.yaml ./output/gnssro_obs_2024071518_{splits/satId}.nc4
#srun -n 1 $bufrbuild/build/bin/bufr2netcdf.x  ./input/gdas1.240830.t18z.gpsro.tm00.bufr_d gnssro.yaml ./output/gnssro_obs_2024083018.nc4
srun -n 1 $bufrbuild/build/bin/bufr2netcdf.x  ./input/gdas1.240903.t00z.gpsro.tm00.bufr_d gnssro_metadata_bending_angle_nosplits.yaml ./output/gnssro_metadata_bending_angle_2024090300.nc4

