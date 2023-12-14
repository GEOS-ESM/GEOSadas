#!/usr/bin/env python3
import os, sys

def aod_type(dataID, stdoutFLG=True):
    """
    input_parameters
     => dataID: label used to identify data set in the obsys_rc file
     => stdoutFLG: set to False, if calling directly from Python code

    purpose
    Identify AOD data type

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
    input_parameters
     => classlist_string: AOD obsclass list (separated by commas, no spaces)
     => stdoutFLG: set to False, if calling directly from Python code

    purpose: remove duplicate AOD data sets from obsclass list

    NOTE: THIS ROUTINE DOES NOT CHECK DATA AVAILABILITY

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
    scriptname = os.path.basename(sys.argv[0])

    if len(sys.argv) < 2:
        print(("\nusage: {0} routineName input_parameters".format(scriptname)))
        print(( " routineName options = {0}\n\n".format(list(routineList.keys()))))
        exit()

    routineName = sys.argv[1]
    if routineName not in list(routineList.keys()):
        raise Exception("Error. Unknown routine name: {0}".format(routineName))
    routine = routineList[routineName]

    if len(sys.argv) < 3:
        scriptname = os.path.basename(sys.argv[0])
        print(("\nusage: {0} {1} input_parameters".format(scriptname, routineName)))
        print((routineList[routineName].__doc__))
        exit()

    routineList[routineName](*sys.argv[2:])
