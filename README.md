# GEOS ADAS Fixture

## How to build GEOS ADAS

### Preliminary Steps

#### Load Build Modules

In your `.bashrc` or `.tcshrc` or other rc file add a line:

##### NCCS (SLES11)

```
module use -a /discover/swdev/gmao_SIteam/modulefiles-SLES11
```

##### NCCS (SLES12)

```
module use -a /discover/swdev/gmao_SIteam/modulefiles-SLES12
```
###### Auto detection of OS
To better automate this, you can have for bash:
```
if [[ -e /etc/os-release ]]
then
   module use -a /discover/swdev/gmao_SIteam/modulefiles-SLES12
else
   module use -a /discover/swdev/gmao_SIteam/modulefiles-SLES11
fi
```
or for tcsh:
```
if (-e /etc/os-release) then
   module use -a /discover/swdev/gmao_SIteam/modulefiles-SLES12
else
   module use -a /discover/swdev/gmao_SIteam/modulefiles-SLES11
endif
```

##### NAS
```
module use -a /nobackup/gmao_SIteam/modulefiles
```

##### GMAO Desktops
On the GMAO desktops, the SI Team modulefiles should automatically be
part of running `module avail` but if not, they are in:

```
module use -a /ford1/share/gmao_SIteam/modulefiles
```

Also do this in any interactive window you have. This allows you to get module files needed to correctly checkout and build the model.

Now load the `GEOSenv` module:
```
module load GEOSenv
```
which obtains the latest `git`, `CMake`, and `manage_externals` modules.

#### Obtain the Model

```
git clone git@github.com:GEOS-ESM/GEOSadas.git
```

---

### Single Step Building of the Model

If all you wish is to build the model, you can run `parallel_build.csh` from a head node. Doing so will checkout all the external repositories of the model and build it. When done, the resulting model build will be found in `build/` and the installation will be found in `install/` with setup scripts like `gcm_setup` and `fvsetup` in `install/bin`.

#### Debug Version of GEOS ADAS

To obtain a debug version, you can run `parallel_build.csh -debug` which will build with debugging flags. This will build in `build-Debug/` and install into `install-Debug/`.

#### Mepo Version of GEOS ADAS

GEOS ADAS will soon be transitioning from using `checkout_externals` to
using [`mepo`](https://github.com/GEOS-ESM/mepo), a GMAO-developed
multi-repository management tool. If you wish to use it via
`parallel_build.csh` you can run:
```
parallel_build.csh -mepo
```
along with any other flags you usually use.

---

### Multiple Steps for Building the Model

The steps detailed below are essentially those that `parallel_build.csh` performs for you. Either method should yield identical builds.

#### Checkout externals

Using the `checkout_externals` command to compose the model is done by:

```
cd GEOSadas
checkout_externals
```

#### Mepo

To checkout the full model with the
[`mepo`](https://github.com/GEOS-ESM/mepo) tool, you run:

```
mepo init
mepo clone
```

The first command initializes the multi-repository and the second one
clones and assembles all the sub-repositories according to
`components.yaml`

#### Build the Model

##### Load Compiler, MPI Stack, and Baselibs
On tcsh:
```
source @env/g5_modules
```
or on bash:
```
source @env/g5_modules.sh
```

##### Create Build Directory
We currently do not allow in-source builds of GEOSadas. So we must make a directory:
```
mkdir build
```
The advantages of this is that you can build both a Debug and Release version with the same clone if desired.

##### Run CMake
CMake generates the Makefiles needed to build the model.
```
cd build
cmake .. -DBASEDIR=$BASEDIR/Linux -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_INSTALL_PREFIX=../install
```
This will install to a directory parallel to your `build` directory. If you prefer to install elsewhere change the path in:
```
-DCMAKE_INSTALL_PREFIX=<path>
```
and CMake will install there.

##### Build and Install with Make
```
make -jN install
```
where `N` is the number of parallel processes. On discover head nodes, this should only be as high as 2 due to limits on the head nodes. On a compute node, you can set `N` has high as you like, though 8-12 is about the limit of parallelism in our model's make system.

### Run the AGCM

Once the model has built successfully, you will have an `install/` directory in your checkout. To run `gcm_setup` go to the `install/bin/` directory and run it there:
```
cd install/bin
./gcm_setup
```

### Run the ADAS

Like the AGCM step above, the ADAS setup scripts are also `install/bin/`
