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
BIN  = gettrk.x

esma_install install: $(LIB) $(BIN)
	$(MKDIR)     $(ESMABIN) 
	$(CP) $(BIN) $(ESMABIN)
	$(CP) vtrack_training    $(ESMABIN)
	$(CP) vtx.ctl.tmpl       $(ESMAETC)
	$(CP) vtrack.ctl.tmpl    $(ESMAETC)
	$(CP) *.ctl              $(ESMAETC)
	$(CP) *.rc               $(ESMAETC)
	$(CP) *.table            $(ESMAETC)
	$(SED) -e "s^@DASPERL^$(PERL)^" < vtrack > $(ESMABIN)/vtrack
	chmod 755 $(ESMABIN)/vtrack_training
	chmod 755 $(ESMABIN)/vtrack

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

SRCS := gettrk_modules.f gettrk_main.f
OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS)))

FOPT = 
FREAL = $(FREAL4)

THIS_BACIO = NCEP_bacio_r4i4
THIS_W3    = NCEP_w3_r4i4
THIS_SP    = NCEP_sp_r4i4
THIS_GFIO  = GMAO_gfio_r4

MOD_DIRS = . $(INC_MPEU) $(INC_HERMES) $(INC_TRANSF)
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINC   = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 

vpath % $(MOD_DIRS)

$(LIB) lib : $(DEPS) $(OBJS)
	$(RM) $(LIB)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)

gettrk.x:  $(LIB) gettrk_main.o
	$(LD) $(LDFLAGS) -o $@ gettrk_main.o $(LIB) \
	      $(LIB_BACIO) $(LIB_W3) $(LIB_SYS)

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
