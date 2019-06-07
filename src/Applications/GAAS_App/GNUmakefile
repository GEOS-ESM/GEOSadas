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
LIB   = lib$(THIS).a

BINS   = mpana_aod.x ana_aod.x 
CSSCRP = get_aero_obs.csh run_gaas_ana.csh
PYSCRP = modis_l2a.py mxd04_l2a.py avhrr_l2a.py patmosx_l2a.py aod_data.py
PYMODS = mxd04_nnr.py avhrr_nnr.py

esma_install install: $(DEPS) $(LIB) $(BINS) $(PYMODS)
	$(MKDIR) $(ESMALIB) $(ESMAETC) $(ESMAINC)/$(THIS) $(ESMABIN)
	$(CP) -p *.a               $(ESMALIB)
	$(CP) -p $(PYMODS)         $(ESMALIB)/Python
	$(CP) -p *.rc *.pcf *.tmpl $(ESMAETC)
	$(CP) -p *.mod             $(ESMAINC)/$(THIS)
	$(CP) -p $(BINS) $(PYSCRP) $(ESMABIN)
	$(CP) -p $(CSSCRP)         $(ESMABIN)

esma_clean clean:
	$(RM) *~ *.[aox] *.[Mm][Oo][Dd] *.x

esma_distclean distclean:
	$(RM)  *~ *.[aoxd] *.[Mm][Oo][Dd] *.x

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
	@echo "         THIS = $(THIS)"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE)"
	@echo "        FREAL = $(FREAL)"

#                  --------------------
#                  User Defined Targets
#                  --------------------

SRCS := aod_util.F90  m_ana.F90  m_duplicate.F  m_obs.F90  m_simul.F90  m_sqc.F90
OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS))) \
        $(addsuffix .d, $(basename $(BINS))) 

FOPT = $(FOPT3)
FPE =

INC_PSAS = $(ESMAINC)/GMAO_psas
LIB_PSAS = $(ESMALIB)/libGMAO_psas.a

MOD_DIRS = . $(INC_MPEU) $(INC_ODS) $(INC_PSAS) $(INC_HERMES) \
             $(INC_CHEM_BASE) $(INC_MAPL_BASE) $(INC_CFIO) $(INC_SDF) $(INC_ESMF)

INC_DIRS = $(MOD_DIRS)

THIS_GFIO := GMAO_gfio_r4
THIS_CFIO := MAPL_cfio_r4

USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(MOD_DIRS),$(I)$(dir)) 

FREAL = $(FREAL4)

vpath % $(MOD_DIRS)

$(LIB) lib :  $(DEPS) $(OBJS)
	$(RM) $(LIB)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)

ana_aod.x : $(LIB) ana_aod.o
	$(FC) $(LDFLAGS) -o $@ ana_aod.o $(LIB) \
              $(LIB_CHEM_BASE) $(LIB_ODS) $(LIB_PSAS) $(LIB_HERMES) \
              $(LIB_GFIO) $(LIB_MAPL_BASE) $(LIB_GMAO_pFIO) $(LIB_MAPL_BASE_STUBS) $(LIB_MAPL_BASE_STUBS2) \
              $(LIB_EU) $(LIB_ESMF) \
              $(LIB_SDF) $(LIB_SCI) $(LIB_SYS) $(LIB_MPI)

mpana_aod.x : $(LIB) mpana_aod.o
	$(FC) $(LDFLAGS) -o $@ mpana_aod.o $(LIB) \
              $(LIB_CHEM_BASE) $(LIB_ODS) $(LIB_PSAS) $(LIB_HERMES) \
              $(LIB_GFIO) $(LIB_MAPL_BASE) $(LIB_GMAO_pFIO) $(LIB_MAPL_BASE_STUBS) $(LIB_MAPL_BASE_STUBS2) \
              $(LIB_CFIO) $(LIB_MPEU) $(LIB_ESMF) \
              $(LIB_SDF) $(LIB_SCI) $(LIB_SYS) $(LIB_MPI)

odsqc.x : $(LIB) odsqc.o
	$(FC) $(LDFLAGS) -o $@ odsqc.o $(LIB) \
              $(LIB_CHEM_BASE) $(LIB_ODS) $(LIB_PSAS) $(LIB_HERMES) \
              $(LIB_GFIO) $(LIB_MAPL_BASE) $(LIB_MAPL_BASE_STUBS) $(LIB_MAPL_BASE_STUBS2) \
              $(LIB_EU) $(LIB_ESMF) \
              $(LIB_SDF) $(LIB_SCI) $(LIB_SYS) $(LIB_MPI)

%.x : $(LIB) %.o
	$(FC) $(LDFLAGS) -o $@ $*.o $(LIB) \
              $(LIB_CHEM_BASE) $(LIB_HERMES) \
              $(LIB_GFIO) $(LIB_MAPL_BASE) $(LIB_MAPL_BASE_STUBS) $(LIB_MAPL_BASE_STUBS2) \
              $(LIB_CFIO) $(LIB_MPEU) \
              $(LIB_ESMF) $(LIB_SDF) $(LIB_SCI) $(LIB_SYS) $(LIB_MPI)

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
