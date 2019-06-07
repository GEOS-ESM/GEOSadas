#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 3mar2004  Zaslavsky  Initial imlementation.
# 20Oct2004  da Silva  Standardization
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../../..
endif

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk  # System dependencies

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------


THIS := $(shell basename `pwd`)
LIB   = 
BINS  = mkiau.x mp_stats.x pod.x rstupd.x updAincWpert.x mp_dyn_stats.x stochphysics.x

esma_install install: $(DEPS) $(BINS)
	$(MKDIR) $(ESMALIB) $(ESMAETC) $(ESMAINC)/$(THIS) $(ESMABIN)
	$(CP) -p *.x         $(ESMABIN)
	$(CP) -p *.rc.tmpl   $(ESMAETC)

esma_clean clean:
	$(RM) *~ *.[aox] *.[Mm][Oo][Dd]

esma_distclean distclean:
	$(RM) $(ACGS) *~ *.[aoxd] *.[Mm][Oo][Dd] *.x

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

SRCS  = iau_stubs.F90
LIB_STUBS = iau_stubs.o

OBJS := $(addsuffix .o, $(basename $(SRCS)))
STUB := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS))) \
        $(addsuffix .d, $(basename $(BINS))) 
ACGS := 

ACG_FLAGS += -F

FOPT = $(FOPT3)
FPE  =

MOD_DIRS = . $(INC_GEOS_FV3) $(INC_GEOS_MKIAU) $(INC_GEOS_MOIST) $(INC_CHEM_PCHEM) $(INC_GEOS_SHARED) $(INC_HERMES) $(INC_MPEU) $(INC_MAPL_BASE) $(INC_SDF) $(INC_ESMF) $(INC_CFIO) $(INC_GMAO_STOCH) $(INC_GMAO_TRANS)
INC_DIRS = . $(MOD_DIRS)
LIB_FV3 = $(ESMALIB)/libFVdycoreCubed_GridComp.a $(ESMALIB)/libfvdycore.a
LIB_FV  = $(LIB_FV3) \
          $(ESMALIB)/libGFDL_fms.a                        

INC_DIRS = $(MOD_DIRS)

THIS_SP   = NCEP_sp_r4i4
THIS_W3   = NCEP_w3_r4i4
THIS_GFIO = GMAO_gfio_r8
THIS_CFIO = MAPL_cfio_r4

USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(MOD_DIRS),$(I)$(dir)) 

FREAL = $(FREAL4) -traceback

vpath % $(MOD_DIRS)

$(LIB) lib : $(ACGS) $(DEPS) $(OBJS)
	$(RM) $(LIB)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)

#$(ACGS) : $(NAME)_Registry.rc $(ACG)
#	@$(ACG) $(ACG_FLAGS) $(NAME)_Registry.rc

%.x : $(STUB) $(LIB) $(LIB_FV) %.o
	$(LD) $(LDFLAGS) -o $@ $*.o $(LIB) \
	      $(ESMALIB)/libGEOSmkiau_GridComp.a \
	      $(ESMALIB)/libGEOSmoist_GridComp.a \
	      $(ESMALIB)/libGEOSpchem_GridComp.a \
              $(ESMALIB)/libChem_Base.a \
              $(ESMALIB)/libGMAO_stoch.a \
              $(LIB_MAPL_BASE) \
              $(LIB_FV) \
              $(LIB_GEOS_SHARED) \
              $(LIB_HERMES) \
	      $(LIB_TRANSF) \
              $(LIB_MAPL_BASE) \
              $(LIB_GMAO_pFIO) \
              $(LIB_CFIO) $(LIB_GFIO) $(LIB_MPEU) $(LIB_W3) $(LIB_SP) \
              $(LIB_ESMF) $(LIB_SDF) \
              $(LIB_SCI) $(LIB_SYS) $(LIB_MPI) $(LIB_CHEM_SHARED)

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
