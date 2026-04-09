#!/bin/bash -l

# usage:
# sh bt_gcm.j 0   ::  clone only
# sh bt_gcm.j 1   ::  continue to build and install

np=8
repo=GEOSadas
tag=v5.42.12
bran=feature/ygyu/v5.42.12/t1

name=$tag
d_src=$name
d_build=build_$tag
d_install=install_$tag


if [[ ! -d $name ]] ; then
  rm -rf ./$d_build
  rm -rf ./$d_install
  cp -rp  adas_fix $name
  # git clone --filter=blob:none -b $bran https://github.com/GEOS-ESM/${repo}.git $name
  # cd $name
  # mepo clone --partial=blobless
  # cd -
fi


if [[ $# -le 0 ]] || [[ $1 -eq 0 ]]; then
    echo "\$# -le 0  OR  \$1 == 0 ;  stop after git clone"
    return 0 2>/dev/null || exit 0
else
    echo "proceed to cmake + make install "
fi

rm -rf ./$d_build
rm -rf ./$d_install


# source /home/yonggang/hq/igloo/env_setup/geos_modules/baselibs_gcc
source $d_src/@env/g5_modules.sh
mkdir -p $d_build $d_install
cd $d_build


if [ -n "$LD_LIBRARY_PATH" ]; then
    # libxml2 xz libiconv glibc numactl zlib-ng pmix texinfo
    export LDFLAGS="-L/home/yonggang/tools/ss_all/spack-stack_1.9.3/envs/env_openmp/install/gcc/13.3.0/libxml2-2.11.9-2du33yb/lib -L/home/yonggang/tools/ss_all/spack-stack_1.9.3/envs/env_openmp/install/gcc/13.3.0/xz-5.4.6-bhqm72g/lib -L/home/yonggang/tools/ss_all/spack-stack_1.9.3/envs/env_openmp/install/gcc/13.3.0/libiconv-1.17-6xm2r2b/lib -L/home/yonggang/tools/ss_all/spack-stack_1.9.3/envs/env_openmp/install/gcc/13.3.0/openmpi-5.0.5-74tszzc/lib"
    echo "    → $LDFLAGS"
fi

cmake_flags=(
    "../$d_src"
    "-DBASEDIR=$BASEDIR"
    "-DCMAKE_Fortran_COMPILER=mpifort"      # ← changed
    "-DCMAKE_C_COMPILER=mpicc"              # ← add
    "-DCMAKE_CXX_COMPILER=mpicxx"           # ← add
    "-DCMAKE_INSTALL_PREFIX=../$d_install"
    "-DCMAKE_BUILD_TYPE=Release"            # Debug
    "-DEXTENDED_SOURCE=" 
    "-DBIG_ENDIAN=-fconvert=big-endian"
    "-DUSE_F2PY=OFF"
    "-DBLAS_LIBRARIES=$BASEDIR/lib/libgslcblas.a"
    "-DLAPACK_LIBRARIES=$BASEDIR/lib/libgslcblas.a"
#    "-DCMAKE_Fortran_FLAGS_INIT=-fallow-argument-mismatch -fdec -ffixed-line-length-none -std=legacy"
    "-DCMAKE_Fortran_FLAGS_INIT=-fallow-argument-mismatch -fdec -std=legacy"
)

printf "Executing: cmake %s &> z.cmake\n" "${cmake_flags[*]}"

cmake "${cmake_flags[@]}"  2>&1 | tee z.cmake
make VERBOSE=1 -j $np  install  2>&1 | tee z.make.install

return 0 2>/dev/null || exit 0
echo "EXIT" 


# note
#  -DUSE_F2PY=OFF \                  # safety (skips the f2py part of GMAO_ods)
#  -DEXTENDED_SOURCE="" \            # explicitly empty → if() skips
#  -DBIG_ENDIAN="-fconvert=big-endian" \   # correct gfortran flag for ODS files

##if [ -n "$LD_LIBRARY_PATH" ]; then
##    export LDFLAGS=$(printf ' -L%s' $(echo "$LD_LIBRARY_PATH" | tr ':' ' '))
##    echo "✓ LDFLAGS automatically built from LD_LIBRARY_PATH"
##    libpath="
##    /home/yonggang/tools/ss_all/spack-stack_1.9.3/envs/env_openmp/install/gcc/13.3.0/xz-5.4.6-bhqm72g
##    /home/yonggang/tools/ss_all/spack-stack_1.9.3/envs/env_openmp/install/gcc/13.3.0/libiconv-1.17-6xm2r2b"
##    for x in $libpath; do
##      LDFLAGS="$LDFLAGS -L${x}/lib"
##    done
##    echo "    → $LDFLAGS"
##fi

##    "-DCMAKE_Fortran_COMPILER=gfortran"
##export FCFLAGS="-fallow-argument-mismatch"
##    "-DCMAKE_Fortran_FLAGS=-fallow-argument-mismatch -fdec -ffixed-line-length-none"
