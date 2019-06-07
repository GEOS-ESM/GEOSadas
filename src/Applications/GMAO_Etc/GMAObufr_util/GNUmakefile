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

BINS = tranavhrr.x table_and_timestamp.x

esma_install install: $(BINS)
	$(MKDIR) $(ESMABIN) $(ESMAETC) 
	$(CP) -p *.x    $(ESMABIN)

esma_clean clean:
	-$(RM) *~ *.[aox] *.lst *.mod *.x 

esma_distclean distclean:
	-$(RM) *~ *.[aoxd] *.lst *.mod *.x

FC = ifort

OBJS_bufr =  avhrr.o func.o w3trnarg.o tranavhrr.o
OBJS_table=  table_and_timestamp.o

LIBS = $(ESMALIB)/libNCEP_bufr_r4i4.a $(ESMALIB)/libNCEP_w3_r4i4.a

FFLAGS = -O2 -convert big_endian -list -fp-model strict \
	 -assume noold_ldout_format -assume byterecl $(DEBUG)

tranavhrr.x: $(OBJS_bufr)
	$(LD) $(LDFLAGS) -o $(@) $(OBJS_bufr) $(LIBS)

table_and_timestamp.x: $(OBJS_table)
	$(LD) $(LDFLAGS) -o $(@) $(OBJS_table) $(LIBS)

.SUFFIXES: .o .f90 .f

.f90.o:
	$(FC) $(FFLAGS) -c $<

.f.o:
	$(FC) $(FFLAGS) -c $<

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
