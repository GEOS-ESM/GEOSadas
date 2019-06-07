#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 3mar2004  Zaslavsky  Initial imlementation.
# 20Oct2004  da Silva  Standardization
# 17Apr2006  Todling   Added to PaQC
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../../../..
endif

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk  # System dependencies


#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------


THIS = $(shell basename `pwd`)
BINS = gmao_prevents.x

esma_install install: $(BINS)
	$(MKDIR) $(ESMAETC) $(ESMABIN)
	$(CP) -p $(BINS)    $(ESMABIN)
	$(CP) -p *.parm     $(ESMAETC)
	$(CP) -p prepobs_errtable.global $(ESMAETC)

esma_clean clean:
	$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd] $(BINS)

esma_distclean distclean:
	$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd] $(BINS)

esma_doc doc:
	@echo "Target $@ not implemented yet in `pwd`"


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
	@echo "         SITE = $(SITE)"
	@echo "        FREAL = $(FREAL)"

#                  --------------------
#                  User Defined Targets
#                  --------------------

SUBDIRS = 

SRCS := gblevents_gmao.f prevents.f

OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS)))

#FOPT = 
#FREAL =

THIS_W3    = NCEP_w3_r8i4
THIS_BUFR  = NCEP_bufr_r8i4

MOD_DIRS = . $(INC_HERMES)

USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir)) 

vpath % $(MOD_DIRS)


LIBS = $(LIB_W3) $(LIB_BUFR)  $(LIB_HERMES) $(LIB_GFIO)
#                     Specific targets
#                     ----------------

#prevents.o : prevents.f
#	$(FC) -c $<

#gblevents_gmao.o : gblevents_gmao.f
#	$(FC) -c $<

gmao_prevents.x : prevents.o  gblevents_gmao.o $(LIBS)
	$(LD) -o $@ $(LDFLAGS) prevents.o  gblevents_gmao.o \
	       $(LIBS) $(LIB_SDF) $(LIB_SYS)

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros
#.
