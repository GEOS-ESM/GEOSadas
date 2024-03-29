#
# Archiving rules for fvDAS output.
#
# This is a PESTO (Put Experiment in Mass Storage) resource file.
#
# The environment variable PESTOROOT refers to the destination archive
# location, e.g.,
#
# a) for moving files to SILO:
#    setenv PESTOROOT '/scratch/johndoe'
#
# b) for moving files to MASS STORAGE:
#    setenv PESTOROOT 'johndoe@machine.nas.nasa.gov:'
#
# This file has been automatically generated by fvsetup.


#...........................................................................
#
#                 ---------------------------
#                    Atmospheric EFSO   
#                 ---------------------------
#
${PESTOROOT}%s/atmens/efso/Y%y4/M%m2/%s.eimp%c_%c%c%c.%y4%m2%d2_%h2z+%y4%m2%d2_%h2z-%y4%m2%d2_%h2z.ods
${PESTOROOT}%s/atmens/efso/Y%y4/M%m2/%s.atm_enkf_osens.log.%y4%m2%d2_%c%c%c.txt
${PESTOROOT}%s/atmens/efso/Y%y4/M%m2/%s.atmens_eoimp.%y4%m2%d2_%c%c%c.tar
