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


THIS := $(shell basename `pwd`)
LIB  = lib$(THIS).a
BIN  = relocate_mv_nvortex.x

.PHONY: all

esma_install install: all
	$(MKDIR)             $(ESMABIN)
	$(CP) -p $(BIN)      $(ESMABIN)
	$(SED) -e "s^@DASPERL^$(PERL)^" < vtxreloc > $(ESMABIN)/vtxreloc
	chmod 755 $(ESMABIN)/vtxreloc

all: $(LIB) $(BIN)


esma_clean clean:
	$(RM) *~ *.[aox] *.[Mm][Oo][Dd]

esma_distclean distclean:
	$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd]

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

SRCS := modules.f BOUND.f fft99.f sig_p_convt1.f SEPAR.f WNLIT.f \
	FDUMP.f H12.f I1MACH.f J4SAVE.f XGETUA.f WNLSM.f WNNLS.f \
	XERABT.f XERCTL.f XERPRT.f XERROR.f XERRWV.f XERSAV.f \
	srotm.f srotmg.f amatrix.f rodist.f m_cutdm.F90 \
	m_stmGrid.F90 m_show.F90 \
	m_specGridIO.F		\
	m_geosGridIO.F90	\
	m_hurrmess.F90		\
	m_hurr.F90		\
	m_Hdata.F90		\
	mv_nvortex.f		\
	HURR_MESS.F		\
	HURR_REL.F		\
	m_vtxgrid.F90

OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename main.F90 $(SRCS)))

FREAL = $(FREAL8)
#------------------------------------------------------------------------------
# NOTE: 19Jul2009 JCS (after consulting with M.Sienkiewicz)
# Remove -fpe0 flag to allow non-critical underflow in vortex relocation program
#------------------------------------------------------------------------------
ifneq ($(BOPT),g)
FOPT = $(FOPT2)
FPE := $(subst -fpe0,,$(FPE))
endif
#------------------------------------------------------------------------------


THIS_W3    = NCEP_w3_r8i4
THIS_SP    = NCEP_sp_r8i4

INC_DIRS = . $(INC_TRANSF) $(INC_HERMES) $(INC_MPEU)
MOD_DIRS = . $(INC_DIRS)

USER_FDEFS = $(D)_LAPACK_ $(D)gmao_intf -DDEBUG_TRACE
USER_FDEFS = $(D)_LAPACK_ $(D)gmao_intf
USER_FFLAGS = $(BIG_ENDIAN) -check bounds
USER_FFLAGS = $(BIG_ENDIAN)
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir)) 

vpath % $(MOD_DIRS)

$(LIB) lib : $(DEPS) $(OBJS)
	$(RM) $(LIB)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)

relocate_mv_nvortex.x:  $(LIB) main.o
	$(LD) $(LDFLAGS) -o $@ main.o $(LIB) \
	      $(LIB_TRANSF) $(LIB_HERMES) $(LIB_GFIO) \
	      $(LIB_EU) $(LIB_W3) $(LIB_SP) $(LIB_SDF) $(LIB_SYS) $(LIB_SCI)

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
