#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 09Jun2003  da Silva  First crack.
# 21Feb2005  da Silva  Implemented SDF macfros of NetCDF/HDF interoperability.
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
  include $(ESMADIR)/Config/GMAO_base.mk  # GMAO Generic stuff

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

BINS = GEOSgcmPert.x SingularVectors.x

#GCMDOC := GCM_doc
#TEXFILES := $(wildcard $(ESMADOC)/*GridComp.tex)

esma_install install: $(BINS)
	$(MKDIR) $(ESMABIN) $(ESMAETC) 
	$(CP) -p *.x    $(ESMABIN)
	$(CP) -p *.rc   $(ESMAETC)
	$(CP) -p *.tmpl $(ESMAETC)

esma_clean clean:
	-$(RM) *~ *.[aox] *.mod *.x #$(GCMDOC).*


esma_distclean distclean:
	-$(RM) *~ *.[aoxd] *.mod *.x #$(GCMDOC).*

#esma_doc doc:
#	@echo TEXFILES: $(TEXFILES)
#	$(CAT) $(ESMADOC)/GCM_Preamble.tex > $(ESMADOC)/$(GCMDOC).tex
#	$(foreach texfile,$(TEXFILES),$(CAT) $(texfile) >> $(ESMADOC)/$(GCMDOC).tex;)
#	@echo "\\end{document}" >> $(ESMADOC)/$(GCMDOC).tex
#	$(PDFLATEX) $(ESMADOC)/$(GCMDOC).tex
#	$(PDFLATEX) $(ESMADOC)/$(GCMDOC).tex
#	$(CP) $(GCMDOC).pdf $(ESMADOC)
#	$(RM) $(TEXFILES)
#	$(RM) $(ESMADOC)/GCM_Preamble.tex $(ESMADOC)/$(GCMDOC).tex
#	$(RM) $(ESMADOC)/*.tex



#                  ---------------------------
#                  Availability Driven Options
#                  ---------------------------

LIB_GEOSCHEM_PERT = $(wildcard $(ESMALIB)/libGEOSchemPert_GridComp.a \
                          $(ESMALIB)/libGEOSpchemPert_GridComp.a )

#INC_GEOSCHEM = $(wildcard $(ESMAINC)/Chem_Base \
#                          $(ESMAINC)/Chem_Shared \
#                          $(ESMAINC)/GEOSchem_GridComp \
#                          $(ESMAINC)/GMIchem_GridComp \
#                          $(ESMAINC)/GEOSpchem_GridComp )
#
#LIB_OCEAN = $(wildcard $(ESMALIB)/libGEOSdatasea_GridComp.a \
#                       $(ESMALIB)/libPoseidon_GridComp.a \
#                       $(ESMALIB)/libGuestOcean_GridComp.a \
#                       $(ESMALIB)/libMOM_GEOS5PlugMod.a \
#                       $(ESMALIB)/libmom.a \
#                       $(ESMALIB)/libmit.a \
#                       $(ESMALIB)/libGEOSoceanbiogeochem_GridComp.a \
#                       $(ESMALIB)/libGEOSoceanbiosimple_GridComp.a \
#                       $(ESMALIB)/libGEOSdataseaice_GridComp.a \
#                       $(ESMALIB)/libGEOSCICEDyna_GridComp.a \
#                       $(ESMALIB)/libGEOSorad_GridComp.a \
#                       $(ESMALIB)/libGEOSoradbio_GridComp.a )

#THIS_SP     = NCEP_sp_r4i4
#THIS_W3     = NCEP_w3_r4i4
#THIS_BACIO  = NCEP_bacio_r4i4
#THIS_BUFR   = NCEP_bufr_r8i4
#THIS_NEMSIO = NCEP_nemsio
#
#LIB_GFSIO   = $(ESMALIB)/libNCEP_gfsio.a
#ifeq ( $(wildcard $(LIB_CRTM)), $(null) )
#   LIB_ANASUPP =
#else
#   LIB_ANASUPP = $(LIB_GFSIO) $(LIB_SIGIO) $(LIB_CRTM) $(LIB_IRSSE) \
#                 $(LIB_NEMSIO) $(LIB_BACIO) $(LIB_BUFR) $(LIB_W3) \
#                 $(LIB_SP) $(LIB_SFCIO) $(LIB_TRANSF)  
#   USER_LDFLAGS = $(OMPFLAG)
#   ifdef I_MPI_ROOT
#      LIB_MPI := $(LIB_MPI_OMP)
#   endif
#endif

LIB_SHARED = $(wildcard \
             $(ESMALIB)/libChem_Shared.a \
             $(ESMALIB)/libChem_Base.a \
             $(ESMALIB)/libGEOS_Shared.a \
             $(ESMALIB)/libGEOS_PertShared.a \
             $(ESMALIB)/libGEOS_PertSvecs.a \
             $(ESMALIB)/libMAPL_Base.a \
             $(ESMALIB)/libGMAO_pFIO.a \
             $(ESMALIB)/libGEOSdynamicsPert_GridComp.a \
             stubs.o \
             $(ESMALIB)/libGMAO_hermes.a  \
             $(ESMALIB)/libMAPL_cfio_r4.a \
             $(ESMALIB)/libGMAO_gfio_r4.a \
             $(ESMALIB)/libGMAO_gems.a  \
             $(ESMALIB)/libGMAO_mpeu.a \
             $(ESMALIB)/libTapenade.a )

#LIB_AANA = $(wildcard $(ESMALIB)/libGEOSaana_GridComp.a \
#                      $(ESMALIB)/libGEOSgsi_Coupler.a \
#                      $(ESMALIB)/libGEOSgsi_Util.a \
#                      $(ESMALIB)/libGSI_GridComp.a \
#                      $(ESMALIB)/libGSI_Other.a \
#                      $(ESMALIB)/libGSI_Solver.a \
#                      $(ESMALIB)/libGSI_Observer.a \
#                      $(ESMALIB)/libGSI_Other.a \
#                      $(ESMALIB)/libGSI_Util.a )
#
#LIB_ANA = $(wildcard $(ESMALIB)/libGEOSana_GridComp.a \
#                     $(ESMALIB)/libGMAOodas_GridComp.a \
#                     $(LIB_AANA) )

LIB_DYN_PERT    = $(wildcard $(ESMALIB)/libGEOSdynamicsPert_GridComp.a \
                     $(ESMALIB)/libGEOS_PertShared.a \
                     $(ESMALIB)/libGFDL_fms.a  \
                     $(ESMALIB)/libfvdycorepert.a )







LIB_PHYSICS_PERT = $(wildcard $(ESMALIB)/libGEOSphysicsPert_GridComp.a\
                         $(ESMALIB)/libGEOSradiationPert_GridComp.a \
                         $(ESMALIB)/libGEOSsolarPert_GridComp.a \
                         $(ESMALIB)/libGEOSirradPert_GridComp.a \
                         $(ESMALIB)/libGEOSgwdPert_GridComp.a \
                         $(ESMALIB)/libGEOSmoistPert_GridComp.a \
                         $(ESMALIB)/libGEOSturbulencePert_GridComp.a \
                         $(ESMALIB)/libGEOSsurfacePert_GridComp.a \
                         $(LIB_GEOSCHEM_PERT))

LIB_LIN_ALG = $(wildcard \
             $(ESMALIB)/libparpack.a \
             $(ESMALIB)/libplanso.a \
             $(ESMALIB)/libarpack.a \
             $(ESMALIB)/libutil.a  \
             $(ESMALIB)/libputil.a)

LIB_SVECS = $(wildcard $(ESMALIB)/libGEOS_PertSvecs.a )







#LIB_LANL_CICE = $(wildcard $(ESMALIB)/libLANL_cice.a)

LIB_AGCM_PERT    = $(wildcard $(ESMALIB)/libGEOSagcmPert_GridComp.a )





#                  --------------------
#                  User Defined Targets
#                  --------------------

FREAL = $(FREAL4)
SRCS = GEOSgcmPert.F90 SingularVectors.F90 stubs.F90
OBJS = $(SRCS:.F90=.o) 

DIR_GC = $(wildcard $(ESMAINC)/GEOS[a-z]*Pert_GridComp $(INC_GEOSCHEM))  
INC_GC = $(foreach FF,$(DIR_GC),$(FF))

INC_DIRS = . $(INC_GC) $(INC_GMAO_SHARED) $(INC_ESMF) $(INC_MPI) $(ESMAINC)/fvdycorepert \
             $(ESMAINC)/GEOS_PertShared $(ESMAINC)/Tapenade $(ESMAINC)/GEOS_PertSvecs
MOD_DIRS = . $(INC_DIRS)

USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir))
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir))
USER_FFLAGS = $(USER_DEFS) $(BIG_ENDIAN)
LDFLAGS += -Xlinker -zmuldefs

vpath % $(MOD_DIRS)

#LIB_GCM = $(ESMALIB)/libGEOSgcm_GridComp.a $(LIB_ATM) \
#          $(ESMALIB)/libGEOSogcm_GridComp.a $(LIB_OCEAN) $(LIB_LANL_CICE)

#LIB_GCS = $(wildcard $(ESMALIB)/libGEOSgcs_GridComp.a $(LIB_GCM) $(LIB_ANA))
#

LIB_COMP = $(LIB_AGCM_PERT) $(LIB_PHYSICS_PERT) $(LIB_DYN_PERT)

ifneq ( $(wildcard $(ESMALIB)/libGEOSgcs_GridComp.a), $(null) ) 
   USER_FDEFS += -DUSE_GCS
endif


ifeq ($(LIB_SDF),$(LIB_HDF))
   LIB_SDF :=  $(ESMALIB)/libGMAO_mfhdf3.a $(LIB_SDF)
endif



%.x : %.o stubs.o $(LIB_COMP)
	$(LD) -o $@  $(LDFLAGS) $+ \
                                $(LIB_MPI) $(LIB_SCI) \
                                $(LIB_SYS) $(LIB_SHARED) $(LIB_DYN_PERT) \
                                $(LIB_ESMF) $(LIB_SDF) $(LIB_LIN_ALG) $(LIB_SVECS)


#                  --------------------
#                      Dependencies
#                  --------------------

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
