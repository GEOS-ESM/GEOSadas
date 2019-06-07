#.......................................................................
#
# Makefile for ESMA components
#
# REVISION HISTORY:
#
#  24Oct2007  stassi   Created with gmm script.
#  15Sep2008  Todling  Add g54var script
#  20Mar2009  Todling  Remove fvana
#  11Sep2008  Todling  Add fvoseledec
#
#.......................................................................

########################################################################

SHELL=/bin/sh

VERSION = "5_25_0"

#--PORTAL = $(wildcard tc4_wx.pl g5_ddf.pl)

PERLSCRIPTS = fcst_convert fvarchive fvsavecf fvsetup fvconvert \
              $(wildcard *.pl) monthly_means.pl.tmpl

PySCRIPTS = $(wildcard *.py)

PERLPKGS = $(wildcard *.pm)

SHELLSCRIPTS = $(wildcard            \
                   FixUnblocked.csh  \
                   fix_gocart_rc.csh \
                   GEOSdas.csm       \
                   Reblock.csh       \
                   fvoseledec        \
                   fvpert            \
                   fvpsas            \
                   fvsens            \
                   fvsvec            \
                   g54dvar           \
		   fp_seamless       \
                   read_HIST.csh )

RCFILES = fvpsas.rc plot.rc das_plot.tmpl

MISC_ETC = $(wildcard \
              monthly.yyyymm.pl.tmpl      \
              monthly_tarandclean.j.tmpl  \
              monthly_prefetch.j.tmpl     \
              monthly_means.j.tmpl        \
              monthly_plots.j.tmpl        \
              ncep_ana.tbl                \
              ncep_diag.tbl               \
              ncep_prog.tbl               \
              noreplay.acq                \
	      plots_transfer.csh.tmpl )

BINS = 
UT = 
PKGS = 
MOD_DIRS = 
NMLFILES = 

########################################################################

SCRIPTS  = $(wildcard $(PERLSCRIPTS) $(SHELLSCRIPTS) $(PySCRIPTS))
PACKAGES = $(wildcard $(PERLPKGS))
ETCFILES = $(wildcard $(RCFILES) $(NMLFILES) $(MISC_ETC))

scripts  = $(SCRIPTS)
perl     = $(PERLSCRIPTS)
python   = $(PySCRIPTS)
packages = $(PACKAGES)
csh      = $(SHELLSCRIPTS)
etc      = $(ETCFILES)
rc       = $(RCFILES)
nml      = $(NMLFILES)

########################################################################

THIS = $(shell basename `pwd`)
SRCS = 

########################################################################

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

#--------------
# ESMA Targets
#--------------
ALLDIRS = testsuites
SUBDIRS = $(wildcard $(ALLDIRS))
TARGETS = esma_install esma_clean esma_distclean \
               install      clean      distclean

$(TARGETS):
	@ t=$@; argv="$(SUBDIRS)";  \
	  for d in $$argv; do  \
	    ( cd $$d;  \
	      echo ""; echo Making $$t in `pwd`;  \
	      $(MAKE) -e $$t )  \
	  done
	$(MAKE) local_$@

local_esma_install local_install: $(BINS)
	$(MKDIR) $(ESMABIN) $(ESMALIB) $(ESMAETC) $(ESMAETC)/images
	echo $(VERSION) > $(ESMAETC)/VERSION

	@if [ "$(SCRIPTS)" != " " ]; then (\
	   gmake -f \$(GNUMAKEFILE) scripts \
	) fi

	@if [ "$(PACKAGES)" != "" ]; then (\
	   gmake -f \$(GNUMAKEFILE) packages \
	) fi

	@if [ "$(ETCFILES)" != "  " ]; then (\
	   gmake -f \$(GNUMAKEFILE) etc \
	) fi

local_esma_clean local_clean:
	$(RM) *~ *.[aox] *.[Mm][Oo][Dd]

local_esma_distclean local_distclean:
	$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd]

esma_help help:
	@echo "Standard ESMA targets:"
	@echo "% gmake esma_install    (builds and install under ESMADIR)"
	@echo "% gmake esma_clean      (removes deliverables: *.[aox], etc)"
	@echo "% gmake esma_distclean  (leaves in the same state as cvs co)"
	@echo "% gmake esma_doc        (generates PDF, installs under ESMADIR)"
	@echo "% gmake esma_help       (this message)"
	@echo "% gmake scripts         (installs scripts only)"
	@echo "% gmake perl            (installs perl scripts only)"
	@echo "% gmake python          (installs python scripts only)"
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
	@echo " SHELLSCRIPTS = $(SHELLSCRIPTS)"

########################################################################

SEDCMD = s^\@DASPERL^\$(PERL)^;s^\@DASSED^\$(SED)^;s^\@DASHELL^\$(SHELL)^

scripts perl csh python:
	@echo ""
	@for scr in $($@); do  \
	   ( $(SED) -e "$(SEDCMD)"< $$scr > $(ESMABIN)/$$scr; \
	     echo "script to \$$(ESMABIN): $$scr";  \
	     chmod 755 $(ESMABIN)/$$scr ) \
	done
	@echo ""
	@echo "\$$(ESMABIN) = $(ESMABIN)"
	@echo ""

packages:
	@echo ""
	@for scr in $($@); do  \
	   ( $(SED) -e "$(SEDCMD)"< $$scr > $(ESMABIN)/$$scr; \
	     echo "package to \$$(ESMABIN): $$scr") \
	done
	@echo ""
	@echo "\$$(ESMABIN) = $(ESMABIN)"
	@echo ""

etc rc nml:
	@echo ""
	@for file in $($@); do  \
	   ( $(CP) $$file $(ESMAETC); \
	     echo "copy to \$$(ESMAETC): $$file")  \
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
DEPS := $(addsuffix .d, $(basename $(SRCS))) 
        $(addsuffix .d, $(basename $(BINS)))

USER_FFLAGS = $(BIG_ENDIAN) $(DGEOS5)
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(MOD_DIRS),$(I)$(dir)) 

$(LIB) lib : $(DEPS) $(OBJS)
	$(RM) $(LIB)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)


%.x : $(LIB) %.o #--$(PKGS)
	$(LD) $(LDFLAGS) -o $@ $*.o $(LIB) $(PKGS) $(LIB_SYS)


#----------------------------------------------------
# Hack to prevent remaking dep files during cleaning
#----------------------------------------------------
ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
   -include $(DEPS)
endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
