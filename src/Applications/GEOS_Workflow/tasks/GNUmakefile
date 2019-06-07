!SHELL=/bin/sh

#.......................................................................
#
# Makefile for ESMA components
#
# REVISION HISTORY:
#
#  14Sep2018  jstassi  Created with gmm script.
#
#.......................................................................

#---------------------------------------------------
# Get GNUmakefile name
# note: MAKEFILE_LIST defined for gmake versions >= 3.80
#---------------------------------------------------
ifdef MAKEFILE_LIST
   GNUMAKEFILE := $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
else
   GNUMAKEFILE := GNUmakefile
endif

#------------------------------
# Make sure ESMADIR is defined
# -----------------------------
ifndef ESMADIR
   ESMADIR := $(PWD)/../../../..
endif

#-------------------------------
# Compilation rules, flags, etc
#-------------------------------
include $(ESMADIR)/Config/ESMA_base.mk   # Generic stuff
include $(ESMADIR)/Config/ESMA_arch.mk   # System dependencies
include $(ESMADIR)/Config/GMAO_base.mk   # System dependencies

########################################################################

THIS = $(shell basename `pwd`)
FILES = $(wildcard *.sh) \
	$(wildcard atmens/*.sh) \
	$(wildcard utils/datetime/*.sh) \
	$(wildcard utils/env/*.py) $(wildcard utils/env/*.sh)

########################################################################

#-----------------------
# Standard ESMA Targets
#-----------------------
esma_install install:
	@$(MKDIR) $(ESMAWFL)/$(THIS)/atmens
	@$(MKDIR) $(ESMAWFL)/$(THIS)/utils/datetime
	@$(MKDIR) $(ESMAWFL)/$(THIS)/utils/env
	@for file in $(FILES); do \
	   ( echo "copying to \$$(ESMAWFL)/$(THIS)/$$file"; \
	     $(CP) $$file $(ESMAWFL)/$(THIS)/$$file ) \
	done
	@echo ""
	@echo "\$$(ESMAWFL) = $(ESMAWFL)"
	@echo ""

esma_clean esma_distclean esma_doc clean distclean doc:
	@echo "nothing to do here"

esma_help help:
	@echo "Standard ESMA targets:"
	@echo "% gmake install       (builds and install under ESMADIR)"
	@echo "% gmake help          (this message)"
	@echo
	@echo "Environment:"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE)"

########################################################################

#----------------------------------------------------
# Hack to prevent remaking dep files during cleaning
#----------------------------------------------------
ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
ifneq ($(findstring clean,$(SUBTARGET)),clean)
   -include $(ALLDEPS)
endif
endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
