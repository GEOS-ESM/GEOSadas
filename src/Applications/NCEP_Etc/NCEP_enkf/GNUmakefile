#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 20oct2004  da Silva  First ESMA version.
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../..
endif

THIS := $(shell basename `pwd`)

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk


#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

esma_help help:
	@echo "Standard ESMA targets:"
	@echo "% make esma_install    (builds and install under ESMADIR)"
	@echo "% make esma_clean      (removes deliverables: *.[aox], etc)"
	@echo "% make esma_distclean  (leaves in the same state as cvs co)"
	@echo "% make esma_doc        (generates PDF, installs under ESMADIR)"
	@echo "% make esma_help       (this message)"
	@echo "Environment:"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE) "
	@echo "         THIS = $(THIS) "


#                  --------------------------------
#                   Recurse Make in Sub-directories
#                  --------------------------------

SSUBDIRS = geos aero
XSUBDIRS = scripts

TARGETS = esma_install esma_clean esma_doc \
          install clean doc

# GPU With CUDA Fortran, one of the compiler options interferes
# with f2py. This turns off the GPU options if on.
GPU_TARGET = 

$(TARGETS):
	@ t=$@; argv="$(SSUBDIRS)" ;\
	  for d in $$argv; do			     \
	       ( $(MKDIR) $$d            		;\
	         echo "$(MAKE) -C $$d -f ../GNUmakefile_in -e $$t NTHIS=$(THIS) CASE=$$d"; \
	               $(MAKE) -C $$d -f ../GNUmakefile_in -e $$t NTHIS=$(THIS) CASE=$$d ) \
	  done
	$(MAKE) local_$@
	@ t=$@; argv="$(XSUBDIRS)" ;\
	  for d in $$argv; do                    \
	    ( cd $$d                            ;\
	      echo ""; echo Making $$t in `pwd`          ;\
	      $(MAKE) -e $$t ) \
	  done
	$(MAKE) local_$@

local_esma_install local_install: $(LIB)
	@echo No local install in here

local_esma_clean local_clean:
	-$(RM) *~ *.[aox] *.mod *.pyf *.so

local_esma_doc local_doc:
	@echo No doc to build here

esma_distclean distclean:
	-$(RM) -r *~ *.[aoxd] *.mod *.pyf *.so $(SSUBDIRS)

#                  --------------------
#                  User Defined Targets
#                  --------------------

geos_install: 
	$(MKDIR) geos
	echo Making install for geos in $(PWD)
	$(MAKE) -C geos -f ../GNUmakefile_in -e install NTHIS=$(THIS) CASE=geos

aero_install:
	$(MKDIR) aero
	echo Making install for aero in $(PWD)
	$(MAKE) -C aero -f ../GNUmakefile_in -e install NTHIS=$(THIS) CASE=aero

scripts_install:
	echo Making install for scripts in $(PWD)
	cd scripts
	$(MAKE) -C scripts -f GNUmakefile -e install
	cd -

dist:
	$(MAKE) distclean
	( cd ..  ;\
          $(TAR)  cvf evac-`date '+%d%b%Y'`.tar src ;\
          $(GZIP) evac-`date '+%d%b%Y'`.tar )

test:
	$(MAKE) -C geos -f ../GNUmakefile_in -e test NTHIS=$(THIS) CASE=geos
	$(MAKE) -C aero -f ../GNUmakefile_in -e test NTHIS=$(THIS) CASE=aero

#...............................................................

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
