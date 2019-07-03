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
which obtains the latest `git`, `CMake`, and `manage_externals` modules. (*NOTE*: This also loads the `other/python/GEOSpyD/Ana2019.03_py2.7` module as the stock Python on SLES11 discover it too old.)

### Obtain the Model

#### Cloning

```
git clone git@developer.nasa.gov:GEOSproto/GEOSadas.git
```

#### Checkout externals
```
cd GEOSadas
checkout_externals
```

### Build the Model

#### Load Compiler, MPI Stack, and Baselibs
```
module load ./@modules/modules.NCCS
```

#### Create Build Directory
We currently do not allow in-source builds of GEOSadas. So we must make a directory:
```
mkdir BUILD
```
The advantages of this is that you can build both a Debug and Release version with the same clone if desired.

#### Run CMake
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

#### Build and Install with Make
```
make -j6 install
```

### Run the AGCM

Once the model has built successfully, you will have an `install/` directory in your `BUILD` directory. To run `gcm_setup` go to the `bin/` directory in the install directory and run it there:
```
cd install/bin
./gcm_setup
```
