#.......................................................................
#
# Makefile for ESMA components
#
# REVISION HISTORY:
#
#  09Jun2010  Stassi   Modified from GNUmakefile in GEOSdas_App directory
#
#.......................................................................

########################################################################

SHELL=/bin/sh

scripts = runjob.pl checkinput.pl
readme = runjob.README 
files  = $(wildcard *.input)

########################################################################
allscripts = $(wildcard $(scripts))
allreadme = $(wildcard $(readme))

progs = $(allscripts:.pl=)

#------------------------------
# Make sure ESMADIR is defined
# -----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../../../..
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
esma_install install: $(progs)
	@for file in $(allreadme); do  \
	   ( $(CP) $$file $(ESMABIN); \
	     echo "file to \$$(ESMABIN): $$file" )  \
	done
	@echo "\$$(ESMABIN) = $(ESMABIN)"

	@$(MKDIR) $(ESMATST)
	@for file in $(files); do  \
	   ( $(CP) $$file $(ESMATST); \
	     echo "file to \$$(ESMATST): $$file" )  \
	done
	@echo "\$$(ESMATST) = $(ESMATST)"

esma_clean clean esma_distclean distclean:
	@echo "Nothing to clean."

########################################################################

CVSTAG = $(shell cat $(ESMAETC)/CVSTAG)
SEDCMD = -e s^__ESMABIN__^\$(ESMABIN)^ -e s^__CVSTAG__^\$(CVSTAG)^ \
         -e s^__ESMATST__^\$(ESMATST)^

%: %.pl
	@$(MKDIR) $(ESMABIN)
	@$(SED) $(SEDCMD) < $< > $(ESMABIN)/$@
	@chmod 755 $(ESMABIN)/$@
	@echo "script to \$$(ESMABIN): $@"

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
