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

BIN_SRCS = statsmain.F90 write_berror_global.f90
BIN      = calcstats.x write_berror_global.x

esma_install install: $(LIB) $(BIN) 
	$(MKDIR) $(ESMALIB) $(ESMAETC) $(ESMAINC)/$(THIS) $(ESMABIN)
	$(CP) stats.parm.sample   $(ESMAETC)/stats.parm
	$(CP) -p $(BIN)      $(ESMABIN)
	$(CP) -p *.tmpl      $(ESMAETC)
#	$(CP) -p *.a         $(ESMALIB)
#	$(CP) -p *.mod       $(ESMAINC)/$(THIS)

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

SRCS := calcbal.F90   m_rdgrids.f90  genqsat.f90 \
        getcases.f90 m_getcases.f90 m_vordiv.f90	\
        m_fvHeader.f90 m_GsiGrided.f90 horlength.F90 isrchfge.f90 mat.f90 \
        postmod.f90  m_readpairs.F90 smoothz.f90 specgrid.f90 \
        m_stvp.f90 sstmod.f90 m_speclap.f90 m_calap.f90 compact_diffs.f90 \
        type_kinds.F90 variables.f90 variances.F90 \
        vertlength.F90 addoz.f90
OBJS := $(addsuffix .o, $(basename $(SRCS)))

ALL_SRCS = $(SRCS) $(BIN_SRCS)
DEPS := $(addsuffix .d, $(basename $(ALL_SRCS)))

FOPT = $(FOPT3) -traceback
FREAL = $(FREAL4)

THIS_BACIO = NCEP_bacio_r4i4
THIS_W3    = NCEP_w3_r4i4
THIS_SP    = NCEP_sp_r4i4
THIS_GFIO  = GMAO_gfio_r8

MOD_DIRS = . $(INC_MPEU) $(INC_HERMES) $(INC_TRANSF)
USER_FDEFS = $(D)_LAPACK_ $(D)gmao_intf
USER_FFLAGS = $(BIG_ENDIAN) $(BYTERECLEN)
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINC   = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 

vpath % $(MOD_DIRS)

$(LIB) lib : $(DEPS) $(OBJS)
	$(RM) $(LIB)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)

calcstats.x:  $(LIB) statsmain.o
	$(LD) $(LDFLAGS) -o $@ statsmain.o $(LIB) \
              $(LIB_TRANSF) $(LIB_HERMES) $(LIB_GFIO) $(LIB_MPEU) \
              $(LIB_SP) $(LIB_BACIO) $(LIB_W3) $(LIB_SDF) \
              $(LIB_MPI) $(LIB_SYS) $(LIB_SCI)

write_berror_global.x:  write_berror_global.o
	$(LD) $(LDFLAGS) -o $@ write_berror_global.o \
	      $(LIB_HERMES) $(LIB_GFIO) $(LIB_MPEU) $(LIB_SP) $(LIB_BACIO) $(LIB_W3)


# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
