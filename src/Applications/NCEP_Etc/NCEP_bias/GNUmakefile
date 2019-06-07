#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 20Oct2004  da Silva  Standardization
# 11Nov2011  Todling   link with GSI (for kinds and others)
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
BIN  = init_coeffs.x \
       sac.x sac_convert.x sac_convert \
	     sbc_convert.x sbc_convert \
	     sif_convert.x sif_convert \
	     write_biascr_option.x \
             sac.pl

esma_install install: $(LIB) $(BIN)
	chmod +x sac_convert
	chmod +x sbc_convert
	chmod +x sif_convert
	chmod +x sac.pl
	$(MKDIR) $(ESMABIN) 
	$(CP) $(BIN) $(ESMABIN)
	$(CP) sac.nl.tmpl $(ESMAETC)

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

SRCS := determine_time_levels.f90 m_sisind.f90 m_sischnTable.F90 bias_mpeu.F90
OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(OBJS) main.o \
	sischnTable_utest.o init_coeffs.o \
	sac_convert.o sbc_convert.o sif_convert.o ))

FOPT = $(FOPT3)
FREAL = 
FPE   =

THIS_BUFR = NCEP_bufr_r4i4
THIS_SP   = NCEP_sp_r4i4
THIS_W3   = NCEP_w3_r4i4

MOD_DIRS = . $(INC_GSI) $(INC_MPEU) $(INC_HERMES) $(INC_TRANSF)
USER_FFLAGS = $(BIG_ENDIAN)
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINC   = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 

LIBS_ALL = $(LIB) $(LIB_GSI_UTIL) $(LIB_GSI_UTIL) $(LIB_W3) $(LIB_NCDIAG_READ)
# if you may need MPI
_Lmp	 = $(shell for l in $(LIBS_ALL) $(LIB_MPEU); do echo -L$$(dirname $$l) -l$$(basename $$l .a|sed -e's/^lib//'); done) 

# if you don't need or don't have a MPI implementation
_Leu	 = $(shell for l in $(LIBS_ALL) $(LIB_EU); do echo -L$$(dirname $$l) -l$$(basename $$l .a|sed -e's/^lib//'); done) 

test:
	@ echo "_Lmp = $(_Lmp)"
	@ echo "_Leu = $(_Leu)"

vpath % $(MOD_DIRS)

$(LIB) lib : $(DEPS) $(OBJS)
	$(RM) $(LIB)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)

all: $(BIN)

init_coeffs.x:  $(LIB) init_coeffs.o
	$(LD) $(LDFLAGS) -o $@ init_coeffs.o $(_Lmp) $(LIB_SDF)

sac.x:  $(LIB) main.o
	$(LD) $(LDFLAGS) -o $@ main.o $(_Lmp) $(LIB_SDF)

sac_convert.x: $(LIB) sac_convert.o
	$(LD) $(LDFLAGS) -o $@ sac_convert.o $(_Leu)

sbc_convert.x: $(LIB) sbc_convert.o
	$(LD) $(LDFLAGS) -o $@ sbc_convert.o $(_Leu)

sif_convert.x: $(LIB) sif_convert.o
	$(LD) $(LDFLAGS) -o $@ sif_convert.o $(_Leu)

write_biascr_option.x: $(LIB) write_biascr_option.o
	$(LD) $(LDFLAGS) -o $@ write_biascr_option.o $(_Leu)

sac_convert sbc_convert sif_convert:
	@ ls $@

utests: sischnTable.utest # sac.utest

sischnTable.utest: sischnTable_utest.sh sischnTable_utest.x
	bash $< &>$@ || (mv $@ $@,error; false)

sischnTable_utest.x: sischnTable_utest.o $(LIB)
	$(LD) $(LDFLAGS) -o $@ $< $(_Leu)

testtbl.utest: testtbl.x satbang_in.sample.txt
	./$< satbang_in.sample.txt

testtbl.x: testtbl.o $(LIB)
	$(LD) $(LDFLAGS) -o $@ $< $(_Leu)

sac.utest: sac_utest.sh sac.x
	bash $< >$@

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
