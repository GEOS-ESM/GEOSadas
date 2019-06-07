SHELL=/bin/sh

#.......................................................................
#
# Makefile for ESMA components
#
# REVISION HISTORY:
#
#  04Jan2012  jstassi  Created with gmm script.
#
#.......................................................................

#---------------------------------------------------
# Get GNUmakefile name
# note: MAKEFILE_LIST defined for gmake versions >= 3.80
#---------------------------------------------------
ifdef MAKEFILE_LIST
   GNUMAKEFILE := $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
else
   GNUMAKEFILE := GNUmakefile
endif

#------------------------------
# Make sure ESMADIR is defined
# -----------------------------
ifndef ESMADIR
   ESMADIR := $(PWD)/../../..
endif

#-------------------------------
# Compilation rules, flags, etc
#-------------------------------
include $(ESMADIR)/Config/ESMA_base.mk   # Generic stuff
include $(ESMADIR)/Config/ESMA_arch.mk   # System dependencies
include $(ESMADIR)/Config/GMAO_base.mk   # System dependencies

########################################################################

BINS = 

UT = 

PKGS = 

MOD_DIRS = 

PERLSCRIPTS = $(wildcard *.pl) 

PERLPKGS = $(wildcard *.pm)

SHELLSCRIPTS = $(wildcard *.csh) 

JOBSCRIPTS = atm_ens.j

RCFILES = 

NMLFILES = 

SUBDIRS = etc

########################################################################

SCRIPTS  = $(JOBSCRIPTS) $(PERLSCRIPTS) $(PERLPKGS) $(SHELLSCRIPTS)
ETCFILES = $(RCFILES) $(NMLFILES)

scripts  = $(SCRIPTS)
perl     = $(PERLSCRIPTS)
csh      = $(SHELLSCRIPTS)
etc      = $(ETCFILES)
rc       = $(RCFILES)
nml      = $(NMLFILES)

SEDCMD = s^\@DASPERL^\$(PERL)^;s^\@DASSED^\$(SED)^;s^\@DASHELL^\$(SHELL)^

########################################################################

THIS = $(shell basename `pwd`)
LIB  = 

SRCS = 

########################################################################

#-----------------------
# Standard ESMA Targets
#-----------------------
esma_install install:
	gmake -f \$(GNUMAKEFILE) local

	@if [ "$(SUBDIRS)" != "" ]; then (\
	   SUBTARGET=$@;export SUBTARGET;gmake -f \$(GNUMAKEFILE) subdirs \
	) fi

local_install local: $(LIB) $(BINS)
	$(MKDIR) $(ESMALIB) $(ESMAETC) $(ESMAINC)/$(THIS) $(ESMABIN)

	@if [ "$(wildcard *.a)" != "" ]; then (\
	   $(CP) -p *.a $(ESMALIB) \
	) fi

	@if [ "$(wildcard *.x)" != "" ]; then (\
	   $(CP) -p *.x $(ESMABIN) \
	) fi

	@if [ "$(wildcard *.[Mm][Oo][Dd])" != "" ]; then (\
	   $(CP) -p *.[Mm][Oo][Dd] $(ESMAINC)/$(THIS) \
	) fi

	@if [ "$(wildcard *.[Hh])" != "" ]; then (\
	   $(CP) -p *.[Hh] $(ESMAINC)/$(THIS) \
	) fi

	@if [ "$(SCRIPTS)" != "  " ]; then (\
	   gmake -f \$(GNUMAKEFILE) scripts \
	) fi

	@if [ "$(ETCFILES)" != " " ]; then (\
	   gmake -f \$(GNUMAKEFILE) etc \
	) fi

esma_clean clean:
	$(RM) *~ *.[aox] *.[Mm][Oo][Dd]
	$(RM) *.aux *.log *.toc *.tex usage.txt

	@if [ "$(SUBDIRS)" != "" ]; then (\
	   SUBTARGET=$@;export SUBTARGET;gmake -f \$(GNUMAKEFILE) subdirs \
	) fi

esma_distclean distclean:
	$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd]

	@if [ "$(SUBDIRS)" != "" ]; then (\
	   SUBTARGET=$@;export SUBTARGET;gmake -f \$(GNUMAKEFILE) subdirs \
	) fi

esma_help help:
	@echo "Standard ESMA targets:"
	@echo "% gmake esma_install    (builds and install under ESMADIR)"
	@echo "% gmake esma_clean      (removes deliverables: *.[aox], etc)"
	@echo "% gmake esma_distclean  (leaves in the same state as cvs co)"
	@echo "% gmake esma_doc        (generates PDF, installs under ESMADIR)"
	@echo "% gmake esma_help       (this message)"
	@echo "% gmake scripts         (installs scripts only)"
	@echo "% gmake perl            (installs perl scripts only)"
	@echo "% gmake csh             (installs shell scripts only)"
	@echo "% gmake etc             (installs rc and nml files only)"
	@echo "% gmake rc              (installs rc files only)"
	@echo "% gmake nml             (installs nml files only)"
	@echo "% gmake ut              (compiles utility programs)"
	@echo "Environment:"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE)"

########################################################################

scripts perl csh:
	@echo ""
	@for scr in $($@); do  \
	   ( $(SED) -e "$(SEDCMD)"< $$scr > $(ESMABIN)/$$scr; \
	     echo "script to \$$(ESMABIN): $$scr";  \
	     chmod 755 $(ESMABIN)/$$scr ) \
	done
	@echo ""
	@echo "\$$(ESMABIN) = $(ESMABIN)"
	@echo ""

etc rc nml:
	@echo ""
	@for file in $($@); do  \
	   ( echo "copy to \$$(ESMAETC): $$file"; \
	     $(CP) $$file $(ESMAETC))  \
	done
	@echo ""
	@echo "\$$(ESMAETC) = $(ESMAETC)"
	@echo ""

subdirs:
	@ SUBTARGET=
	@ for subdir in $(SUBDIRS); do  \
	   ( cd $$subdir;  \
	     echo "$(MAKE) -e $(SUBTARGET) $$subdir";  \
	     $(MAKE) -e $(SUBTARGET) ) \
	done

ut:
	@echo ""
	@for prog in $(UT); do  \
	   ( echo ""; echo "making $$prog in local directory"; \
	     gmake -f $(GNUMAKEFILE) $$prog ) \
	done
	@echo ""

########################################################################

#----------------------
# User Defined Targets
#----------------------

vpath % $(MOD_DIRS) /usr/include

OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS))) \
        $(addsuffix .d, $(basename $(BINS)))

USER_FFLAGS = $(BIG_ENDIAN) $(DGEOS5)
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(MOD_DIRS),$(I)$(dir)) 
LATEX = pdflatex
PDFLATEX = pdflatex

$(LIB) lib : $(DEPS) $(OBJS)
	$(RM) $(LIB)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)


%.x : $(LIB) %.o #--$(PKGS)
	$(LD) $(LDFLAGS) -o $@ $*.o $(LIB) $(PKGS) $(LIB_SYS)

#----------------------------------------------------
#                 Documentation
#----------------------------------------------------
PDF_DOCS = AtmosEnsADAS.pdf

esma_doc doc: AtmosEnsDAS.tex $(PDF_DOCS)

scriptsdoc:
	rm -rf usage.txt
	@echo ""
	@for file in $(SHELLSCRIPTS); do  \
	   ( $$file >> usage.txt ) \
	done
	@echo ""

AtmosEnsDAS.tex: scriptsdoc AnIntro
	rm -rf AtmosEnsADAS.tex
	$(PROTEX) -g AnIntro  > AtmosEnsADAS.tex

#----------------------------------------------------
# Prevent remaking dep files during cleaning
#----------------------------------------------------
ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
   -include $(DEPS)
endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
