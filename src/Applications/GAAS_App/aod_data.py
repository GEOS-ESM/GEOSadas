#!/usr/bin/env python
import os, sys

def aod_type(dataID, stdoutFLG=True):
    """
    purpose
    Identify AOD data type

    input parameters
     => dataID: label used to identify data set in the obsys_rc file
     => stdoutFLG: if True, then print result to STDOUT before returning it

    return value
     => aod_type_val: AOD data type
    """

    # modis terra
    #------------
    if dataID[0:3] == "mod":
        if dataID.find("land") > 1:
            aod_type_val = "terra_land"
        elif dataID.find("ocean") > 1:
            aod_type_val = "terra_ocean"
        else:
            aod_type_val = "terra_L2"

    # modis aqua
    #-----------
    elif dataID[0:3] == "myd":
        if dataID.find("land") > 1:
            aod_type_val = "aqua_land"
        elif dataID.find("ocean") > 1:
            aod_type_val = "aqua_ocean"
        else:
            aod_type_val = "aqua_L2"

    # others
    #-------
    else:
        aod_type_val = dataID

    # return value
    #-------------
    if stdoutFLG:
        print(aod_type_val)

    return aod_type_val

#.......................................................................
def aod_filter(classlist_string, stdoutFLG=True):
    """
    purpose: remove excess AOD data sets from obsclass list.

    input parameters
     => classlist_string: AOD obsclass list (string with labels separated by
                          commas, no spaces)
     => stdoutFLG: if True, then print result to STDOUT before returning it

    return value
     => newlist_string: filtered obsclass list string
    """
    if classlist_string[0] in ("'", '"'):
        if classlist_string[0] == classlist_string[-1]:
            qm = classlist[0]
            classlist = classlist[1:-1]
    else:
        qm = ""

    type_name_list = []
    classlist = classlist_string.split(",")

    for name in classlist:
        type = aod_type(name, stdoutFLG=False)

        if type == "":
            continue

        if type in dict(type_name_list):
            continue

        if type in ["aqua_land", "aqua_ocean"]:
            if "aqua_L2" in dict(type_name_list):
                continue
        if type == "aqua_L2":
            if "aqua_land" in dict(type_name_list):
                continue
            if "aqua_ocean" in dict(type_name_list):
                continue

        if type in ["terra_land", "terra_ocean"]:
            if "terra_L2" in dict(type_name_list):
                continue
        if type == "terra_L2":
            if "terra_land" in dict(type_name_list):
                continue
            if "terra_ocean" in dict(type_name_list):
                continue

        type_name_list.append((type, name))

    newlist = [name for (type, name) in type_name_list]
    newlist_string = ",".join(newlist)

    # return value
    #-------------
    if stdoutFLG:
        print(newlist_string)

    return newlist_string

#.......................................................................
if __name__ == "__main__":
    routineList = { "aod_type":aod_type, "aod_filter": aod_filter }

    if len(sys.argv) < 2:
        scriptname = os.path.basename(sys.argv[0])
        print("\n    usage: {0} routineName routineParams".format(scriptname))
        print( "     routineName options = {0}\n".format(routineList.keys()))
        exit()

    routineName = sys.argv[1]
    if routineName not in routineList.keys():
        raise Exception("Error. Unknown routine name: {0}".format(routineName))

    routineList[routineName](*sys.argv[2:])
