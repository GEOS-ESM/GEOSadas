#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 09Jun2003  da Silva  First crack.
# 09Jan2017  Todling    Allow hook up to WRF components; just setenv WRF_PATH, e.g.,
#                          setenv WRF_PATH /discover/swdev/mathomp4/WRF/3.7/WRFV3
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

THIS = GSI_App 

# Check presence of ESMF to build executables
# -------------------------------------------
libpresence := $(shell ls $(LIB_MAPL_BASE))
ifeq ( $(wildcard $(libpresence)),$(null) )
   INC_ESMF =
   INC_GEOS =
   BIN      = gsi.x
   SRCS     =
else
   INC_GEOS = $(INC_MAPL_BASE) $(INC_GEOS_SHARED) $(INC_NCDIAG_WRITE) $(INC_NCDIAG_READ)
   BIN      = GSIsa.x gsidiag_bin2txt.x #GSIsa_Gaussian.x
   SRCS    := gsi_stubs.F90 GSIsa.F90 #GSIsa_Gaussian.F90
endif

# Define libs related to GOES-5 Cubed-Sphere Perturbation Model
# -------------------------------------------------------------
LIB_GEOSCHEM_PERT = $(wildcard $(ESMALIB)/libGEOSchemPert_GridComp.a \
                          $(ESMALIB)/libGEOSpchemPert_GridComp.a )

LIB_PHYSICS_PERT  = $(wildcard $(ESMALIB)/libGEOSphysicsPert_GridComp.a\
                         $(ESMALIB)/libGEOSradiationPert_GridComp.a \
                         $(ESMALIB)/libGEOSsolarPert_GridComp.a \
                         $(ESMALIB)/libGEOSirradPert_GridComp.a \
                         $(ESMALIB)/libGEOSgwdPert_GridComp.a \
                         $(ESMALIB)/libGEOSmoistPert_GridComp.a \
                         $(ESMALIB)/libGEOSturbulencePert_GridComp.a \
                         $(ESMALIB)/libGEOSsurfacePert_GridComp.a \
                         $(LIB_GEOSCHEM_PERT))

LIB_DYN_PERT    = $(wildcard $(ESMALIB)/libGEOSdynamicsPert_GridComp.a )
LIB_AGCM_PERT   = $(wildcard $(ESMALIB)/libGEOSagcmPert_GridComp.a \
                             $(ESMALIB)/libGEOS_PertShared.a )

LIB_FV_PERT = $(wildcard $(ESMALIB)/libfvdycorepert.a \
                         $(ESMALIB)/libGFDL_fms.a \
                         $(ESMALIB)/libTapenade.a )

LIB_COMP_PERT = $(LIB_AGCM_PERT) $(LIB_PHYSICS_PERT) $(LIB_FV_PERT) $(LIB_DYN_PERT)

ifeq ( $(wildcard $(WRF_PATH)),$(null) )
   LIB_WRF = 
else
   LIB_WRF = $(WRF_PATH)/external/io_int/libwrfio_int.a $(WRF_PATH)/external/io_netcdf/libwrfio_nf.a
endif

# Check presence of coupler GEOS-GSI library to decide which GSI lib to use
# -------------------------------------------------------------------------
libpresence := $(shell ls $(LIB_GEOSGSI_COUPLER))
ifeq ( $(wildcard $(libpresence)),$(null) )
   LIB_NCEPGSI_COUPLER = $(ESMALIB)/libNCEPgsi_Coupler.a
   LIB_GSI_UTIL = $(ESMALIB)/libNCEPgsi_Util.a
   LIB_MYGSI = $(LIB_GSI_SOLVER) $(LIB_GSI_OTHER) $(LIB_GSI_OBSERVER) $(LIB_GSI_OTHER) $(LIB_NCEPGSI_COUPLER) $(LIB_GSI_UTIL) $(LIB_WRF)
   LIB_FVGCM =
   LIB_GMAO  =
   LIB_STUBS =
else
   LIB_MYGSI = $(LIB_GSI) $(LIB_GSI_OTHER) $(LIB_GSI_SOLVER) $(LIB_GSI_OBSERVER) $(LIB_GSI_OTHER) $(LIB_GEOSGSI_COUPLER)
   LIB_GMAO  = $(LIB_TRANSF) $(LIB_HERMES) $(LIB_GFIO) $(LIB_COMP_PERT) $(LIB_CHEM_BASE)

   libpresence := $(shell ls $(ESMALIB)/libGEOSdynamicsPert_GridComp.a)
   ifeq ( $(wildcard $(libpresence)),$(null) )
      LB_STUBS = $(LIB_MAPL_BASE_STUBS)
   else
      LIB_STUBS = gsi_stubs.o
   endif
endif

esma_install install: $(BIN)
	$(MKDIR) $(ESMABIN) $(ESMAETC) $(ESMABIN)
	$(CP) -p GSI_GridComp.rc.tmpl  $(ESMAETC)
	$(CP) -p GSI_GridComp.rc.tmpl  $(ESMAETC)/OBS_GridComp.rc.tmpl
	$(CP) -p GSI_GridComp_1.rc.tmpl  $(ESMAETC)
	$(CP) -p GSI_GridComp_2.rc.tmpl  $(ESMAETC)
	$(CP) fvssi         $(ESMABIN)
	$(CP) kxinfo.txt kxlist.txt $(ESMAETC)
	$(CP) prepobs_prep.bufrtable $(ESMAETC)
	$(SED) -e "s^@DASPERL^$(PERL)^" < fvssisetup > $(ESMABIN)/fvssisetup
	chmod 755 $(ESMABIN)/fvssisetup
	$(CP) -p *.x        $(ESMABIN)

#	$(SED) -e "s^@DASPERL^$(PERL)^" < gsisasetup > $(ESMABIN)/gsisasetup
#	chmod 755 $(ESMABIN)/gsisasetup

esma_clean clean:
	-$(RM) *~ *.[aox] *.mod

esma_distclean distclean:
	-$(RM) *~ *.[aoxd] *.mod

DOC_SRCS = ../../GEOSaana_GridComp/GEOSaana_GridCompMod.F90 \
	   ../../GEOSaana_GridComp/GSI_GridComp/GSI_GridCompMod.F90
esma_doc doc:
	$(PROTEX) -g AnIntro.tex $(DOC_SRCS) > $(ESMADOC)/GSIsa.tex

#                  --------------------
#                  User Defined Targets
#                  --------------------

OBJS := $(addsuffix .o, $(basename $(SRCS))) 
DEPS := $(addsuffix .d, $(basename $(SRCS))) 

FOPT  = $(FOPT3)
FREAL = $(FREAL4) # for now, require 32 bit reals (R4)

THIS_SP    = NCEP_sp_r8i4
THIS_W3    = NCEP_w3_r8i4
THIS_BACIO = NCEP_bacio_r4i4
THIS_NEMSIO = NCEP_nemsio
THIS_BUFR  = NCEP_bufr_r8i4
THIS_CFIO  = MAPL_cfio_r4

INC_MAPL_CFIO = $(ESMAINC)/MAPL_cfio_r4
LIB_MAPL_CFIO = $(ESMALIB)/libMAPL_cfio_r4.a
LIB_GFSIO = $(ESMALIB)/libNCEP_gfsio.a

MOD_DIRS = . $(INC_SDF) $(INC_ESMF) $(INC_MAPL_BASE) $(INC_GEOS) $(INC_MAPL_CFIO) \
	     $(ESMAINC)/GEOSaana_GridComp $(ESMAINC)/GSI_GridComp $(INC_MPEU) $(INC_HERMES) 
USER_FFLAGS = $(BIG_ENDIAN) $(BYTERECLEN)
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_LDFLAGS = $(OMPFLAG)
FPE = -fp-model strict

ifdef I_MPI_ROOT
   LIB_MPI := $(LIB_MPI_OMP)
endif


vpath % $(MOD_DIRS)

gsi.x:  $(LIB_MYGSI) gsimain.o
	$(FC) $(LDFLAGS) -o gsi.x gsimain.o $(LIB_MYGSI) $(LIB_CRTM)  \
	     $(LIB_SFCIO)  $(LIB_BUFR) $(LIB_NEMSIO) $(LIB_BACIO) $(LIB_GFSIO) $(LIB_SIGIO) \
	     $(LIB_SP) $(LIB_W3) $(LIB_NCDIAG_READ) $(LIB_NCDIAG_WRITE) \
	     $(LIB_SDF) $(LIB_MPI) $(LIB_SCI) $(LIB_SYS)

GSIsa.x :  $(OBJS) $(LIB_GMAO) $(ESMALIB)/libGEOSaana_GridComp.a $(LIB_MYGSI) GSIsa.o
	$(FC) $(LDFLAGS) -o GSIsa.x GSIsa.o \
	$(ESMALIB)/libGEOSaana_GridComp.a \
	$(LIB_MYGSI) $(LIB_SIGIO) $(LIB_FVGCM) \
	$(LIB_CRTM) \
   $(LIB_GMAO) \
	$(LIB_MAPL_BASE) $(LIB_GMAO_pFIO) $(LIB_STUBS) $(LIB_GFSIO) \
	$(LIB_NEMSIO) $(LIB_BACIO) $(LIB_BUFR) $(LIB_W3) \
	$(LIB_SP) $(LIB_SFCIO) \
	$(LIB_MAPL_CFIO) \
	$(LIB_ESMF) $(LIB_MFHDF3) $(LIB_MPEU) $(LIB_FV_PERT) $(LIB_NCDIAG_READ) $(LIB_NCDIAG_WRITE) \
	$(LIB_SDF) $(LIB_MPI) $(LIB_SCI) $(LIB_SYS)

GSIsa_Gaussian.x :  $(OBJS) $(LIB_MYGSI) GSIsa_Gaussian.o gauss_lat_nmc.o
	$(FC) $(LDFLAGS) -o GSIsa_Gaussian.x GSIsa_Gaussian.o gauss_lat_nmc.o \
	$(ESMALIB)/libGEOSaana_GridComp.a \
	$(LIB_MYGSI) $(LIB_SIGIO) $(LIB_FVGCM) \
	$(LIB_CRTM) \
	$(LIB_MAPL_BASE) $(LIB_GMAO_pFIO) $(LIB_STUBS) \
	$(LIB_W3) $(LIB_NEMSIO) $(LIB_BACIO)  $(LIB_BUFR) \
	$(LIB_SP) $(LIB_GMAO) $(LIB_SFCIO)\
	$(LIB_MAPL_CFIO) \
	$(LIB_ESMF) $(LIB_MFHDF3) $(LIB_MPEU) $(LIB_NCDIAG_READ) $(LIB_NCDIAG_WRITE) \
	$(LIB_SDF) $(LIB_MPI) $(LIB_SCI) $(LIB_SYS)

gsidiag_bin2txt.x: $(OBJS) $(LIB_GMAO) $(LIB_MYGSI) gsidiag_bin2txt.o
	$(FC) $(LDFLAGS) -o gsidiag_bin2txt.x gsidiag_bin2txt.o \
	$(LIB_MYGSI) \
	$(LIB_NCDIAG_READ) $(LIB_NCDIAG_WRITE) \
	$(LIB_SDF) $(LIB_MPI) $(LIB_SCI) $(LIB_SYS)

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros
#.
