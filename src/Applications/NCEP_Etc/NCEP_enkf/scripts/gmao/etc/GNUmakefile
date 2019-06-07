#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 03Oct2011  Todling    Adapted from GMAO_etc (Stassi's style)
# 01Mar2014  Todling    Update to EnADAS-5_13_2
#

# Get GNUmakefile name (requires gmake versions >= 3.80)
#-------------------------------------------------------
ifdef MAKEFILE_LIST
   GNUMAKEFILE := $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
else
   GNUMAKEFILE := GNUmakefile
endif

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../../../../../../..
endif

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk  # System dependencies

########################################################################

ALLPERLSCRIPT = 

PERLSCRIPT = $(wildcard $(ALLPERLSCRIPT))

PERLPKG = $(wildcard *.pm)

#-------------------------------------------------------
# ALLSRC   = source files for binaries with no extension
# ALLSRCX  = source files for binaries with .x extension
# OTHERBIN = other files for ESMABIN directory
# ALLETC   = files for ESMAETC directory
#-------------------------------------------------------
ALLSRC = 

ALLSRCX = 

OTHERBIN = 

ALLETC =  AGCM.rc.tmpl \
          AtmEnsConfig.csh \
          aens_stoch.rc \
          aero_enkf.nml.tmpl \
          ana_aodmean.rc \
          ana_aodmember.rc \
          atmens_incenergy.rc.tmpl \
          atmens_replay.acq \
          atmens_rst_regrid.rc \
          atmens_storage.arc \
          atmos_enkf.nml.tmpl \
          atmos_enkf_sens.nml.tmpl \
          CAP.rc.tmpl \
          dyn_recenter_l72.rc \
          dyn_recenter_l132.rc \
          dyn_recenter_l72_sppt.rc \
          dyn_recenter_l132_sppt.rc \
          GAAS_GridComp.rc \
          GEOS_ChemGridComp.rc \
          GSI_GridComp.rc.tmpl \
          GSI_GridComp_ensfinal.rc.tmpl \
          HISTAENS.rc.tmpl \
          listagcmrc.rc \
          mkiau.rc.tmpl \
          mp_stats_perts.rc \
          mp_stats.rc \
          nmcperts.rc \
          post_egcm.rc \
          odsstats_ktonly.rc \
          obs_aodmean.rc \
          obs_aodmember.rc \
          obs1gsi_mean.rc \
          obs1gsi_member.rc

########################################################################

bin        = $(BINS)
etc        = $(ETCFILES)
perlpkg    = $(PERLPKG)
perlscript = $(PERLSCRIPT)
PERLFILES  = $(PERLSCRIPT) $(PERLPKG)

SEDCMD = s^\@DASPERL^\$(PERL)^;s^\@DASSED^\$(SED)^;s^\@DASHELL^\$(SHELL)^

########################################################################

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

THIS := $(shell basename $(PWD))

SRC = # $(wildcard $(ALLSRC))
BIN = # $(basename $(SRC))

SRCX = # $(wildcard $(ALLSRCX))
BINX = # $(addsuffix .x, $(basename $(SRCX)))

BINO = $(wildcard $(OTHERBIN))

# all files for bin, and etc
#---------------------------
BINS      = $(BIN) $(BINX) $(BINO)
ETCFILES  = $(wildcard $(ALLETC))


esma_install install: $(BINS)
	$(MKDIR) $(ESMAETC)/atmens $(ESMABIN) 

	@if [ "$(BINS)" != "  " ]; then (\
	   gmake -f \$(GNUMAKEFILE) bin \
	) fi

	@if [ "$(PERLFILES)" != " " ]; then (\
	   gmake -f \$(GNUMAKEFILE) perl \
	) fi

	@if [ "$(ETCFILES)" != "" ]; then (\
	   gmake -f \$(GNUMAKEFILE) etc \
	) fi


esma_clean clean:
	$(RM) -r $(BIN) *.[axol] *.[Mm][Oo][Dd] *~ Makefile*~ rii_files/ \
	         obs.ods *.hdf *.nc4 *.aux *.dvi *.ps  *.toc *.log
esma_distclean distclean:
	$(RM) -r $(BIN) *.[axodl] *.[Mm][Oo][Dd] *~ Makefile*~ rii_files/ \
	         obs.ods *.hdf *.nc4 *.aux *.dvi *.ps  *.toc *.log
esma_doc doc:
	@echo "Target $@ not implemented yet in `pwd`"

esma_help:
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

help:
	@ echo "Available targets:"
	@ echo "  make lib             (to create libods.a)"
	@ echo "  make lib MODOBJS=    (to omit f90 modules)"
	@ echo "  make test            (to test ODS library)"
	@ echo "  make doc             (to create documentation)"
	@ echo "  make all"  
	@ echo "  make export"  
	@ echo "  make clean"  
	@ echo "  make distclean"
	@ echo "  make depend"  

########################################################################

bin:
	@echo ""
	@for file in $($@); do  \
	   ( echo "copy to \$$(ESMABIN): $$file"; \
	     $(CP) $$file $(ESMABIN))  \
	done
	@echo ""
	@echo "\$$(ESMABIN) = $(ESMABIN)"
	@echo ""

etc:
	@echo ""
	@for file in $($@); do  \
	   ( echo "copy to \$$(ESMAETC)/atmens: $$file"; \
	     $(CP) $$file $(ESMAETC)/atmens)  \
	done
	@echo ""
	@echo "\$$(ESMAETC) = $(ESMAETC)/atmens"
	@echo ""

perlpkg:
	@echo ""
	@for pkg in $($@); do  \
	   ( echo "perl package to \$$(ESMABIN): $$pkg"; \
	     $(CP) $$pkg $(ESMABIN)/$$pkg )  \
	done
	@echo ""
	@echo "\$$(ESMABIN) = $(ESMABIN)"
	@echo ""

perlscript:
	@echo ""
	@for scr in $($@); do  \
	   ( $(SED) -e "$(SEDCMD)"< $$scr > $(ESMABIN)/$$scr; \
	     echo "perl script to \$$(ESMABIN): $$scr";  \
	     chmod 755 $(ESMABIN)/$$scr ) \
	done
	@echo ""
	@echo "\$$(ESMABIN) = $(ESMABIN)"
	@echo ""

perl:
	@if [ "$(PERLSCRIPT)" != "" ]; then (\
	   gmake -f \$(GNUMAKEFILE) perlscript \
	) fi

	@if [ "$(PERLPKG)" != "" ]; then (\
	   gmake -f \$(GNUMAKEFILE) perlpkg \
	) fi

#                  --------------------
#                  User Defined Targets
#                  --------------------

FOPT = $(FOPT3)
USER_FFLAGS = $(BIG_ENDIAN) $(M). $(M)$(INC_MPEU) $(I)$(INC_SDF)


  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
