SHELL=/bin/sh

#.......................................................................
#
# Makefile for ESMA components
#
# REVISION HISTORY:
#
#  18Jun2007  stassi  Created with gmm script.
#
#.......................................................................

########################################################################

BIN = raobcore.x hradcor.x
UT = read_prepbufr.x
PKGS = 
MOD_DIRS = 
PERLSCRIPTS = 
PERLPKGS = 
SHELLSCRIPTS = 
RCFILES = 
NMLFILES = 
SUBDIRS = 

########################################################################

SCRIPTS  = $(PERLSCRIPTS) $(PERLPKGS) $(SHELLSCRIPTS)
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
LIB  = lib$(THIS).a

SRCS = errexit.f	\
	m_AdvError.f 	\
	m_ArrList.f 	\
	m_AtoX.f 	\
	m_CARDS.f 	\
	m_clouds.f 	\
	m_convert.f 	\
	m_CSpline.f 	\
	m_ellipsoid.f 	\
	m_humidity.f 	\
	m_IGRA.f 	\
	m_PText.f 	\
	m_RadBufr.f 	\
	m_RadData.f 	\
	m_RadDataF.f 	\
	m_RadList.f 	\
	m_RadLists.f 	\
	m_RadNCEP.f 	\
	m_RadSort.f 	\
	m_RadTplate.f 	\
	m_Range.f 	\
	m_RaobCore.f 	\
	m_soundings.f 	\
	m_stdatm.f 	\
	m_SunAlt.f 	\
	m_SysIO.f 	\
	m_SysUtil.f 	\
	m_temperature.f 	\
	m_TextUtil.f 	\
	m_VaiReg.f 	\
	m_VaiUtil.f 	\
	m_VIZReg.f

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


#-----------------------
# Standard ESMA Targets
#-----------------------
esma_install install: $(LIB) $(BIN)
	$(MKDIR) $(ESMALIB) $(ESMAETC) $(ESMAINC)/$(THIS) $(ESMABIN)
	-$(CP) -p *.a $(ESMALIB)
	-$(CP) -p *.x $(ESMABIN)
	-$(CP) -p *.[Mm][Oo][Dd] $(ESMAINC)/$(THIS)

	@if [ "$(SCRIPTS)" != "  " ]; then (\
	   gmake -f \$(GNUMAKEFILE) scripts \
	) fi

	@if [ "$(ETCFILES)" != " " ]; then (\
	   gmake -f \$(GNUMAKEFILE) etc \
	) fi

	@if [ "$(SUBDIRS)" != "" ]; then (\
	   SUBTARGET=$@;export SUBTARGET;gmake -f \$(GNUMAKEFILE) subdirs\
	) fi

esma_clean clean:
	$(RM) *~ *.[aox] *.[Mm][Oo][Dd]

	@if [ "$(SUBDIRS)" != "" ]; then (\
	   SUBTARGET=$@;export SUBTARGET;gmake -f \$(GNUMAKEFILE) subdirs\
	) fi


esma_distclean distclean:
	$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd]

	@if [ "$(SUBDIRS)" != "" ]; then (\
	   SUBTARGET=$@;export SUBTARGET;gmake -f \$(GNUMAKEFILE) subdirs\
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
DEPS := $(addsuffix .d, $(basename $(SRCS))) \
        $(addsuffix .d, $(basename $(BIN)))


USER_FFLAGS = $(DGEOS5)
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(MOD_DIRS),$(I)$(dir)) 

THIS_BUFR = NCEP_bufr_r8i8
FOPT = $(FOPT2)
FINT = $(FINT8)

$(LIB) lib : $(DEPS) $(OBJS)
	$(RM) $(LIB)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)


%.x : $(LIB) %.o $(LIB_BUFR) #--$(PKGS)
	$(LD) $(LDFLAGS) -o $@ $*.o $(LIB) $(LIB_BUFR) $(PKGS) $(LIB_SYS)

read_prepbufr.x: $(LIB) read_prepbufr.o
	$(LD) $(LDFLAGS) -o $@ read_prepbufr.o $(LIB)

#----------------------------------------------------
# Hack to prevent remaking dep files during cleaning
#----------------------------------------------------
ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
   -include $(DEPS)
endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
