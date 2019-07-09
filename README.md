# GEOS ADAS Fixture

## How to build GEOS ADAS on Discover

### Preliminary Steps

#### Load Build Modules

In your `.bashrc` or `.tcshrc` or other rc file add a line:
```
module use -a /discover/nobackup/projects/gmao/sit/modulefiles-SLES11
```
Also do this in any interactive window you have. This allows you to get module files needed to correctly checkout and build the model.

Now load the `GEOSenv` module:
```
module load GEOSenv
```
which obtains the latest `git`, `CMake`, and `manage_externals` modules.

### Single Step Building of the Model

If all you wish is to build the model, you can run `parallel_build.csh` from a head node. Doing so will checkout all the external repositories of the model and build it. When done, the resulting model will be found in `BUILD/install` with setup scripts like `gcm_setup` and `fvsetup` in `BUILD/install/bin`.

#### Debug Version of GEOS

To obtain a debug version, you can run `parallel_build.csh -debug` which will build with debugging flags.

### Multiple Steps for Building the Model

The steps detailed below are essentially those that `parallel_build.csh` performs for you. Either method should yield identical builds.

#### Obtain the Model

##### Cloning

```
git clone git@github.com:GEOS-ESM/GEOSadas.git
```

##### Checkout externals
```
cd GEOSadas
checkout_externals
```

#### Build the Model

##### Load Compiler, MPI Stack, and Baselibs
```
source @env/g5_modules
```

##### Create Build Directory
We currently do not allow in-source builds of GEOSadas. So we must make a directory:
```
mkdir BUILD
```
The advantages of this is that you can build both a Debug and Release version with the same clone if desired.

##### Run CMake
CMake generates the Makefiles needed to build the model.
```
cd BUILD
cmake .. -DBASEDIR=$BASEDIR/Linux
```
This (when installed) will install to a directory inside your `BUILD` directory. If you prefer to install elsewhere add:
```
-DCMAKE_INSTALL_PREFIX=<path>
```
and CMake will install there.

##### Build and Install with Make
```
make -j6 install
```

### Run the AGCM

Once the model has built successfully, you will have an `install/` directory in your `BUILD` directory. To run `gcm_setup` go to the `bin/` directory in the install directory and run it there:
```
cd install/bin
./gcm_setup
```
