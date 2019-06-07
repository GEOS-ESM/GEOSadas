#!/usr/bin/python
#---------------------------------------------------------------------------------------------------------------------------
'''
After first four python functions, main routine is followed.
code and data paths, satellite id, and processing time period
are set in config_avhrr.py:
nobackup_path='/discover/nobackup/jgkim'
build_path   =nobackup_path+'/GEOSadas-5_17/GEOSadas'
work_path    =nobackup_path+'/avhrr'
data_path    =nobackup_path+'/avhrr/NOAA_CLASS/avhrr_n15_orbit/2005'
bufr_path    =nobackup_path+'/avhrr/NOAA_CLASS/avhrr_n15_bufr'
which_satellite='n15'
start_date   ='2005,09,02 00:00'
end_date     ='2005,09,02 00:00'

    03Nov2017  Jong Kim   Initial code
'''
#---------------------------------------------------------------------------------------------------------------------------

import os, fnmatch
import sys
import shutil
from datetime import  datetime, timedelta
from config_avhrr import *

start_date = datetime.strptime(start_date, '%Y,%m,%d %H:%M')
end_date   = datetime.strptime(end_date, '%Y,%m,%d %H:%M')
process_folder  = work_path + '/process_folder'

def set_6hr_time_bound(open_date, close_date):
    open_year  = open_date.strftime('%Y')
    open_month = open_date.strftime('%m')
    open_days  = str((datetime.date(open_date)-\
                    datetime.date(datetime(int(open_year),1,1,00,00,00))).days+1)
    open_hour  = open_date.strftime('%H')

    close_year = close_date.strftime('%Y')
    close_month= close_date.strftime('%m')
    close_days = str((datetime.date(close_date)-\
                    datetime.date(datetime(int(close_year),1,1,00,00,00))).days+1)
    close_hour = close_date.strftime('%H')

    open_bound = int(open_year[2:])*10000000+int(open_days)*10000+int(open_hour)*100
    close_bound= int(close_year[2:])*10000000+int(close_days)*10000+int(close_hour)*100

    return open_bound, close_bound;

def get_orbit_file_time_mark(file_string):
    y_mark = int(file_string[13:15])
    d_mark = int(file_string[15:18])
    s_mark = int(file_string[20:24])
    e_mark = int(file_string[26:30])

    s_time = y_mark*10000000+d_mark*10000+s_mark
    e_time = y_mark*10000000+d_mark*10000+e_mark
    if e_mark < s_mark:
        e_time=   d_mark*10000+10000+e_mark

    return s_time, e_time;

def set_input_files(open_date, proc_date, close_date):
    date_minus_3hour  = str(open_date.strftime('%Y')+' '+\
                        open_date.strftime('%m')+' '+\
                        open_date.strftime('%d')+' '+open_date.strftime('%H'))
    date_plus_3hour   = str(close_date.strftime('%Y')+' '+\
                        close_date.strftime('%m')+' '+\
                        close_date.strftime('%d')+' '+close_date.strftime('%H'))
    date_current      = str(proc_date.strftime('%Y')+proc_date.strftime('%m')+\
                        proc_date.strftime('%d')+proc_date.strftime('%H'))
    bufr_util = build_path+'/src/Applications/GMAO_Etc/GMAObufr_util'

    if os.path.isfile(process_folder+"/infile"):
        os.system("rm "+process_folder+"/infile")
    if not os.path.isfile(bufr_util+"/lnd_sea_mask_dat"):
        print "land ocean mask file doesn't exist"
    if not os.path.isfile(bufr_util+"/bufrtab.021"):
        print "bufr table doesn't exist"
    if not os.path.isfile(bufr_util+"/table_and_timestamp.x"):
        print "table_and_timestamp.x doesn't exist"

    if which_satellite =='n16' or which_satellite =='n18' or which_satellite =='n19' :
        mnemonic ='NC021053 '
        infile_xz='b021 xx053 xxzz '
        infile_xa='b021 xx054 xxaa '
    else:
        mnemonic ='NC021051 '
        infile_xz='b021 xx051 xxzz '
        infile_xa='b021 xx052 xxaa '

    f = open("infile", 'w')
    f.write(infile_xz + date_current + '\n')
    f.write("NOTIMLIM" + '\n')
    f.write(infile_xa + date_current + '\n')
    f.write("NOTIMLIM" + '\n')
    f.write(date_minus_3hour + ' ' + date_plus_3hour + '\n')
    f.close()

    src_file = bufr_util+"/lnd_sea_mask_dat"
    shutil.copy(src_file, process_folder)
    src_file = bufr_util+"/bufrtab.021"
    shutil.copy(src_file, process_folder)
    src_file = bufr_util+"/table_and_timestamp.x"
    shutil.copy(src_file, process_folder)

    command_bufr_head ='table_and_timestamp.x bufrtab.021 '+mnemonic+ date_current+ ' HEAD'
    os.system(command_bufr_head)

def create_bufr_files(bufr_file_name, bufr_file_path, orbit_files):
    i = 0
    while( i <= len(orbit_files)-1):
        file_name = orbit_files[i]
        print 'processing ', file_name
        land_output_file  = file_name+".Land.Bufr"
        ocean_output_file = file_name+".Ocean.Bufr"
        ocean_output_file_txt = file_name+".Ocean.Bufr.txt"

        if os.path.isfile("input.bufr"):
            os.remove('input.bufr')
        if os.path.isfile("fort.20"):
            os.remove('fort.20')

        clean_headinfo="dd bs=512 skip=1 if="+file_name+" of=input.bufr"
        os.system(clean_headinfo)
        os.symlink('bufrtab.021','fort.20')
        os.system("tranavhrr.x < infile")
        if os.path.isfile("fort.51"):
            os.rename('fort.51',ocean_output_file)
        if os.path.isfile("fort.52"):
            os.rename('fort.52',land_output_file)
        i=i+1

    cat_bufr = 'cat HEAD *.Ocean.Bufr > '+bufr_file_name
    os.system(cat_bufr)
    shutil.copy(bufr_file_name, bufr_file_path)

if __name__ == "__main__":

    check_satid = 0
    if which_satellite == 'n15':
        matches ='NSS.GHRR.NK.*D*.S*.E*'
        am_or_pm='AVCSAM'
        check_satid = 1
    if which_satellite == 'n16':
        matches ='NSS.GHRR.NL.*D*.S*.E*'
        am_or_pm='AVCSPM'
        check_satid = 1
    if which_satellite == 'n17':
        matches ='NSS.GHRR.NM.*D*.S*.E*'
        am_or_pm='AVCSAM'
        check_satid = 1
    if which_satellite == 'n18':
        matches ='NSS.GHRR.NN.*D*.S*.E*'
        am_or_pm='AVCSPM'
        check_satid = 1
    if which_satellite == 'n19':
        matches ='NSS.GHRR.NP.*D*.S*.E*'
        am_or_pm='AVCSPM'
        check_satid = 1        
    if check_satid == 0:
        print 'Check satellite ID: N15, N16, N17, N18, and N19 !'
        sys.exit()

    files = fnmatch.filter(os.listdir(data_path), matches)

    if len(files) == 0:
        print 'there are no data files to process in data_path'
        sys.exit()
    if len(files) > 0:
        print 'total number of data files in data_path=',len(files)

# iterate over time intervals with 6 hourly time period -------------------------------
    proc_date = start_date
    while( proc_date <= end_date):
        print 'creating bufr file for ',proc_date
        open_date  = proc_date-timedelta(hours=3)
        close_date = proc_date+timedelta(hours=3)
        [open_bound, close_bound] = set_6hr_time_bound(open_date, close_date)

# step 1: make a temporary processing directory to process files ------------------------------------------------------------------------
        if os.path.isdir(process_folder):
            shutil.rmtree(process_folder)
            os.makedirs(process_folder) 
        else:
            os.makedirs(process_folder) 
        os.chdir(process_folder)

# step 2: move involved sat orbit files to the processing directory ----------------------------------------------------------------------
        i = 0
        while( i <= len(files)-1):
            file_string = files[i]
            [s_time, e_time] = get_orbit_file_time_mark(file_string)

            if s_time >= open_bound and s_time <= close_bound or \
                e_time >= open_bound and e_time <= close_bound : 
                src_file= data_path+'/'+files[i]
                shutil.copy(src_file, process_folder)
            i=i+1

# step 3: move buffer table, land ocean mask, and input parameter files to the processing directory ------------------------
        set_input_files(open_date, proc_date, close_date)

# step 4: set an appropriate bufr file path and name ---------------------------------------------------------------------------------------
        bufr_file_path = bufr_path+'/'+am_or_pm+'/Y' + str(proc_date.strftime('%Y'))+\
                        '/M'+str(proc_date.strftime('%m'))
        date_current   = str(proc_date.strftime('%Y')+proc_date.strftime('%m')+\
                        proc_date.strftime('%d')+proc_date.strftime('%H'))
        bufr_file_name = 'gmao.'+str(date_current[2:8])+'.t'+str(date_current[8:10])+\
                        'z.'+am_or_pm.lower()+'.tm00.'+which_satellite+'.bufr_d'
        if not os.path.exists(bufr_file_path):
            os.makedirs(bufr_file_path)
        if os.path.isfile(bufr_file_path+'/'+bufr_file_name):
            command_del_bufr ='rm '+bufr_file_path+'/'+bufr_file_name
            os.system(command_del_bufr)

# step 5: run tranavhrr.x in processing directory and create bufr file-----------------------------------------------------------------
        shutil.copy(build_path+'/Linux/bin/tranavhrr.x', process_folder)
        matches = 'NSS.GHRR*'
        orbit_files = fnmatch.filter(os.listdir(process_folder), matches)

        if len(orbit_files) == 0:
            print 'there are no data files to process for this time period',proc_date
            sys.exit()
        if len(orbit_files) > 0:
            create_bufr_files(bufr_file_name, bufr_file_path, orbit_files)

        os.chdir(work_path)
        proc_date = proc_date + timedelta(hours=6)

    if os.path.isdir(process_folder):
        shutil.rmtree(process_folder)
