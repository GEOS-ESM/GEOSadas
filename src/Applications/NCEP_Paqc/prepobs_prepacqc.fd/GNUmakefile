#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 03mar2004  Zaslavsky Initial imlementation.
# 20Oct2004  da Silva  Standardization
# 17Apr2006  Todling   Added to PaQC
# 29Mar2007  Stassi    Changed COPT to FOPT2 for $(FC) compilation
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := ../../..
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
BINS = prepacqc.x

esma_install install: $(BINS)
	$(MKDIR) $(ESMAETC) $(ESMABIN)
	$(CP) -p $(BINS)    $(ESMABIN)
	$(CP) -p *.parm     $(ESMAETC)
	$(CP) -p prepobs_landc prepobs_waypoints $(ESMAETC)

esma_clean clean:
	$(RM) *~ *.[aox] *.[Mm][Oo][Dd] $(BINS)

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

SRCS := prepacqc.f

OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS)))

FOPT = 
FREAL = $(FREAL4)

THIS_BUFR = NCEP_bufr_r4i4
THIS_W3   = NCEP_w3_r4i4
THIS_SP   = NCEP_sp_r4i4

INC_DIRS += . $(SUBDIRS) $(INC_MPI)
MOD_DIRS += . $(INC_MPEU)

USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir)) 
USER_CDEFS  = $(D)NAGf90Fortran

vpath % $(INC_DIRS) $(MOD_DIRS) /usr/include

#           Special case compilations
#           -------------------------

prepacqc.o: prepacqc.f
	$(FC) -c $(FOPT2) $<


#                     Specific targets
#                     ----------------

prepacqc.x :	prepacqc.o $(LIB_BUFR) $(LIB_W3)  
	$(LD) $(LDFLAGS) -o prepacqc.x prepacqc.o $(LIB_BUFR) $(LIB_W3)  


# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
