SHELL=  /bin/sh
ISIZE = 4
RSIZE = 8
COMP=   xlf
LIBS=   /nwprod/w3lib90/w3lib_d /nwprod/w3lib90/bacio_4
LDFLAGS= -bloadmap:loadmap -bnoquiet
FFLAGS= -O -qarch=pwr3 -qflttrap=ov:und:zero:inv:enable -qintsize=$(ISIZE) -qrealsize=$(RSIZE)

#FFLAGS= -O -qarch=pwr3 -qintsize=$(ISIZE) -qrealsize=$(RSIZE)
#FFLAGS= -O -qcheck -qextchk -qwarn64 -qarch=pwr3 -qflttrap=ov:und:zero:inv:enable -qintsize=$(ISIZE) -qrealsize=$(RSIZE)
#FFLAGS= -g -qcheck -qextchk -qwarn64 -qarch=pwr3 -qflttrap=ov:und:zero:inv:enable -qintsize=$(ISIZE) -qrealsize=$(RSIZE)

gettrk:      gettrk_main.f gettrk_modules.o
	@echo " "
	@echo "  Compiling the tracking program....."
	$(COMP) $(FFLAGS) $(LDFLAGS) gettrk_modules.o gettrk_main.f $(LIBS) -o gettrk
	@echo " "

gettrk_modules.o:    gettrk_modules.f
	@echo " "
	@echo "  Compiling the modules....."
	$(COMP) $(FFLAGS) -c gettrk_modules.f -o gettrk_modules.o
	@echo " "
