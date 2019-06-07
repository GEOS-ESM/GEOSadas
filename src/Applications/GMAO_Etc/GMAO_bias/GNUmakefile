SHELL=/bin/sh

#.......................................................................
#
# Makefile for ESMA components
#
# REVISION HISTORY:
#
#  22May2013  jstassi  Created with gmm script.
#  19Jun2013  msienkie  added copy of 'parm' file to Linux/etc
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

INC_LIST = $(INC_MPEU)
LIB_LIST = $(LIB_EU)

########################################################################

BINS = cft_update.x convert_cft.x
DEPS := $(addsuffix .d, $(basename $(BINS)))

RCFILES = gmao_acft_bias.parm

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
	$(MKDIR) $(ESMABIN) $(ESMAETC) $(ESMALIB)
	$(MAKE) -f $(GNUMAKEFILE) bins
	$(MAKE) -f $(GNUMAKEFILE) rc

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
	@echo "% gmake rc            (installs rc files)"
	@echo
	@echo "Environment:"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE)"

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

rc:
	@if [ "$(RCFILES)" != "" ]; then ( \
	   echo "................................."; \
	   for file in $(RCFILES); do \
	      ( echo "copy to \$$(ESMAETC): $$file"; \
	        $(CP) $$file $(ESMAETC) ) \
	   done; \
	   echo "\$$(ESMAETC) = $(ESMAETC)"; \
	   echo "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"; \
	) fi


########################################################################

#----------------------
# User Defined Targets
#----------------------

vpath % $(INC_LIST) /usr/include

USER_FFLAGS = $(BIG_ENDIAN) $(DGEOS5)
USER_FMODS  = $(foreach dir,$(INC_LIST),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(INC_LIST),$(I)$(dir)) 
FREAL = $(FREAL4)

$(LIB) lib : $(DEPS) $(OBJS)
	@if [ "$(OBJS)" != "" ]; then ( \
	   $(RM) $(LIB); \
	   $(AR) $(AR_FLAGS) $(LIB) $(OBJS) \
	) fi

%.x : $(LIB) %.o #--$(LIB_LIST)
	$(LD) $(LDFLAGS) -o $@ $*.o $(LIB) $(LIB_LIST) $(LIB_SYS)


#----------------------------------------------------
# Hack to prevent remaking dep files during cleaning
#----------------------------------------------------
ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
ifneq ($(findstring clean,$(SUBTARGET)),clean)
   -include $(DEPS)
endif
endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
