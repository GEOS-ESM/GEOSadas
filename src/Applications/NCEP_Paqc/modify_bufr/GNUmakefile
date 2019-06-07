SHELL=/bin/sh

#.......................................................................
#
# Makefile for ESMA components
#
# REVISION HISTORY:
#
#  20Mar2013  jstassi  Created with gmm script.
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

INC_LIST = $(INC_BUFR)
LIB_LIST = $(LIB_BUFR)
THIS_BUFR = NCEP_bufr_r4i4

########################################################################

BINS = fix_ascat.x change_said.x explode.x

########################################################################

#-----------------------
# Standard ESMA Targets
#-----------------------
TARGETS = esma_install esma_clean esma_distclean esma_doc \
          install clean distclean doc         

$(TARGETS):
	@echo
	@if [ "$(SUBDIRS)" != "" ]; then ( \
	   SUBTARGET=$@; export SUBTARGET; $(MAKE) -f $(GNUMAKEFILE) subdirs \
	) fi
	$(MAKE) -f $(GNUMAKEFILE) local_$@

local_esma_install local_install:
	$(MKDIR) $(ESMABIN)
	$(MAKE) -f $(GNUMAKEFILE) bins

local_esma_clean local_clean:
	$(RM) *~ $(BINS) *.[aox] *.[Mm][Oo][Dd]

local_esma_distclean local_distclean:
	$(RM) *~ $(BINS) *.[aoxd] *.[Mm][Oo][Dd]

esma_help help:
	@echo "Standard ESMA targets:"
	@echo "% gmake install       (builds and install under ESMADIR)"
	@echo "% gmake clean         (removes deliverables: *.[aox], etc)"
	@echo "% gmake distclean     (leaves in the same state as cvs co)"
	@echo "% gmake esma_doc      (generates PDF, installs under ESMADIR)"
	@echo "% gmake help          (this message)"
	@echo
	@echo "Subtargets:"
	@echo "% gmake bins          (compiles and installs binary files)"
	@echo
	@echo "Environment:"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE)"

pzs:
	@echo $(LIB_LIST)

########################################################################

bins: $(BINS)
	@if [ "$(BINS)" != "" ]; then ( \
	   echo; \
	   echo "================================="; \
	   for file in $(BINS); do \
	      ( echo "copy to \$$(ESMABIN): $$file"; \
	        $(CP) $$file $(ESMABIN) ) \
	   done; \
	   echo "\$$(ESMABIN) = $(ESMABIN)"; \
	   echo "================================="; \
	   echo; \
	) fi


########################################################################

#----------------------
# User Defined Targets
#----------------------

vpath % $(INC_LIST) /usr/include

USER_FMODS  = $(foreach dir,$(INC_LIST),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(INC_LIST),$(I)$(dir)) 

FREAL = $(FREAL4)

%.x : %.o
	$(LD) $(LDFLAGS) -o $@ $*.o $(LIB_LIST) $(LIB_SYS)


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
