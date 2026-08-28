#!/usr/local/other/python/GEOSpyD/4.10.3_py3.9/2022-01-14/bin/python3
import argparse
import os
import re
import sys

from argparse import ArgumentDefaultsHelpFormatter, ArgumentParser
from history_rc import HistoryRc

#.......................................................................
def main(args):

    addflag  = args.addflag
    arcflag  = args.arcflag
    infile   = args.infile
    listflag = args.listflag
    mnthlyRC = args.mnthlyRC
    plotHIST = args.plotHIST
    quiet    = args.quiet
    sumflag  = args.sumflag

    strSubs = string_substitution_requests(args)
    hist_rc = HistoryRc(infile, strSubs, quiet)
    apply_runtime_choices(hist_rc, args)

    if plotHIST:
        plot_edit(hist_rc, args)

    if listflag:
        print_list_to_stdout(hist_rc, args)
        exit()

    if sumflag:
        sum_slices(hist_rc, args)
        exit()

    if addflag:
        add_silo_mstorage_traits(hist_rc, args)

    if arcflag:
        write_silo_mstorage_arc(hist_rc, args)
        exit()

    write_HISTfile(hist_rc, args)

#.......................................................................
def add_silo_mstorage_traits(hist_rc, args):
    """
    Add the silo and mstorage trits to each collection, if they are not
    already present.
    """
    fcstflag = args.fcstflag

    tmpl_info = [ { "id": "prog",
                    "labels": ["prog", "traj", "ptrj"] },

                  { "id": "ana",
                    "labels": ["vtx", ".eta", ".sfc", ".prs"] },

                  { "id": "chem",
                    "labels": ["_adg_", "_aer_", "_ctm_", "_chm_",
                               "_gas_", "_nav_", "_tag_",
                               "adg_", "aer_", "ctm_", "chm_",
                               "gas_", "nav_", "tag_"] },

                  { "id": "diag",
                    "labels": ["_asm_", "_cld_", "_csp_", "_dyn_",
                               "_ext_", "_flx_", "_glc_", "_hwl_",
                               "_int_", "_lfo_", "_lnd_", "_lsf_",
                               "_met_", "_mst_", "_ocn_", "_odt_",
                               "_qdt_", "_rad_", "_slv_", "_tdt_",
                               "_tmp_", "_trb_", "_udt_", "_wnd_",
                               "asm_", "cld_", "csp_", "dyn_",
                               "ext_", "flx_", "glc_", "hwl_",
                               "int_", "lfo_", "lnd_", "lsf_",
                               "met_", "mst_", "ocn_", "odt_",
                               "qdt_", "rad_", "slv_", "tdt_",
                               "tmp_", "trb_", "udt_", "wnd_"] } ]

    # add silo trait
    #---------------
    trait = "silo"
    for cname in hist_rc.collection_names():

        # skip restart collections
        #-------------------------
        if cname.endswith("_rst") and cname not in ["traj_lcv_rst", "bkg_clcv_rst"]:
            continue

        # get tmpl id
        #------------
        for tmpl in tmpl_info:
            for label in tmpl["labels"]:
                if label in cname:
                    id = tmpl["id"]
                    break
            else:
                continue
            break
        else:
            id = "other"

        # set value for silo trait
        #-------------------------
        if fcstflag and id not in ["prog", "other"]:
            val = "'prog/Y%y4/M%m2/D%d2/H%h2'"
        else:
            val = f"'{id}/Y%y4/M%m2'"

        # add silo trait after template trait
        #------------------------------------
        index1 = hist_rc.trait_index(cname, "template") + 1
        hist_rc.add_trait(cname, trait, val, index=index1, overwrite=False)

    # add mstorage trait
    #-------------------
    trait = "mstorage"
    val = "'yes'"
    for cname in hist_rc.collection_names():

        # skip restart collections
        #-------------------------
        if cname.endswith("_rst") and cname != "traj_lcv_rst":
            continue

        # add mstorage trait after silo trait
        #------------------------------------
        index1 = hist_rc.trait_index(cname, "silo") + 1
        hist_rc.add_trait(cname, trait, val, index=index1, overwrite=False)

#.......................................................................
def apply_runtime_choices(hist_rc, args):
    """
    Turn collections on and off by commenting or uncommenting them.
    """
    excDEP  = args.excDEP
    excPM   = args.excPM
    exclude = args.exclude
    incPM   = args.incPM
    include = args.include

    excDASH    = args.excDASH
    excPLUS    = args.excPLUS
    excludeAll = args.excludeAll
    incDASH    = args.incDASH
    incPLUS    = args.incPLUS
    includeAll = args.includeAll
    rmDASH     = args.rmDASH
    rmPLUS     = args.rmPLUS

    includePM = []
    excludePM = []

    dashlist = []
    pluslist = []

    if rmDASH and rmPLUS:
        raise Exception(f"Cannot choose both -rD and -rP")

    if rmDASH: excPLUS = True
    if rmPLUS: excDASH = True

    # pattern match inclusions and exclusions
    #----------------------------------------
    for cname in hist_rc.collection_names():
        for pm in incPM:
            if cname.find(pm) >= 0:
                includePM.append(cname)
        for pm in excPM:
            if cname.find(pm) >= 0:
                excludePM.append(cname)

        if cname.find("+-") >= 0:
            pluslist.append(cname)
        elif cname.find("-") >= 0:
            dashlist.append(cname)

    # check for contradicting input flags
    #------------------------------------
    intersectX = set.intersection(set(include), set(exclude))
    if intersectX:
        raise Exception(f"-I and -X conflict: {intersectX}")

    intersectX = set.intersection(set(incPM), set(excPM))
    if intersectX:
        raise Exception(f"-Ipm and -Xpm conflict: {intersectX}")

    # global exclusion/inclusion
    #---------------------------
    if excludeAll:
        for cname in hist_rc.collection_names():
            hist_rc.comment_collection(cname)

    if includeAll:
        for cname in hist_rc.collection_names():
            hist_rc.comment_collection(cname, flag=False)

    for cname in hist_rc.collection_names():

        # pattern match exclusion/inclusion
        #----------------------------------
        if cname in excludePM:
            hist_rc.comment_collection(cname)

        if cname in includePM:
                hist_rc.comment_collection(cname, flag=False)

        # ".-"/".+-" exclusion/inclusion
        #-------------------------------
        if excDASH:
            if cname in dashlist:
                hist_rc.comment_collection(cname)

        if incDASH:
            if cname in dashlist:
                hist_rc.comment_collection(cname, flag=False)

        if excPLUS:
            if cname in pluslist:
                hist_rc.comment_collection(cname)

        if incPLUS:
            if cname in pluslist:
                hist_rc.comment_collection(cname, flag=False)

        # file-specified exclusion/inclusion
        #-----------------------------------
        if cname in exclude:
            hist_rc.comment_collection(cname)

        if cname in include:
                hist_rc.comment_collection(cname, flag=False)

    # exclude collections with specified dependencies
    #------------------------------------------------
    if excDEP:
        for dep in excDEP:
            hist_rc.comment_collection(dep=dep)
            
    # remove "-"/"+-" labels
    #-----------------------
    if rmDASH:
        for cname in dashlist:
            hist_rc.strip_dashplus(cname)

    if rmPLUS:
        for cname in pluslist:
            hist_rc.strip_dashplus(cname)        

#.......................................................................
def plot_edit(hist_rc, args):
    """
    Edit the HISTORY information for outputting the HISTORY.rc_tmpl for
    monthly_plots.
    """
    mnthlyRC    = args.mnthlyRC
    plotHISTdir = args.plotHISTdir

    includeList = []
    for cname in hist_rc.collection_names():
        clean = cname.replace("_NCKS", "")
        skip = True
        if clean[-2:] in ["Cp", "Np", "Nx"]: skip = False
        if clean[-3:] in ["slv", "p42"]: skip = False
        if clean[-3:] in ["v72", "v73"]: skip = False
        if skip: continue
        includeList.append(cname)

    # get names from monthly rcfile
    #------------------------------
    plotList = []
    with open(mnthlyRC, mode="r") as mrcfile:
        for line in mrcfile:
            cleanline = line.strip()
            if cleanline == "": continue

            # remove following line to include commented entries in output
            if cleanline.startswith("#"): continue

            vals = cleanline.split()
            if len(vals) > 3: continue

            while len(vals) < 3:
                vals.append("")
            (template, processflags, htype) = vals

            if "P" not in processflags: continue
            cname = template.split(".")[1]
            if cname not in includeList: continue

            # comment in collections list if commented in mnthlyRC
            #-----------------------------------------------------
            plotList.append(cname)
            if cleanline.startswith("#"):
                hist_rc.comment_collection(cname)

            # write datarc template file
            #---------------------------
            if plotHISTdir:
                write_datarc(template, cname, plotHISTdir)

    # only include collections which are to be plotted
    #-------------------------------------------------
    hist_rc.trim_collections(plotList)

    # adjust traits as necessary
    #---------------------------
    for cname in includeList:
        hist_rc.delete_trait(cname, "end_date")
        hist_rc.delete_trait(cname, "end_time")

        trait = "grads_ddf"
        val = f"'>>>PLOTSDIR<<</{cname}.tabl'"
        hist_rc.add_trait(cname, trait, val, index=2)

#.......................................................................
def print_list_to_stdout(hist_rc, args):
    """
    print list of collections to standard output.
    """
    listflag = args.listflag

    printlist = []
    for cname in hist_rc.collection_names():
        if listflag.find("all") == 0:
            printlist.append(cname)

        elif listflag.find("inc") == 0:
            if not hist_rc.commented(cname):
                printlist.append(cname)

        elif listflag.find("exc") == 0:
            if hist_rc.commented(cname):
                printlist.append(cname)

        else:
            raise Exception(f"ERROR. Unrecognizable listflag = {listflag}")

    if listflag[-1] in [":", ","]:
        sep = listflag[-1]
    else:
        sep = "\n"

    printlist_string = sep.join(printlist)
    print(printlist_string)

#.......................................................................
def rm_dash_plus(cname, args):
    """
    Remove the "-" or "+-" from a collection name.
    """
    rmDASH = args.rmDASH
    rmPLUS = args.rmPLUS

    if cname.find("+-") >= 0:
        if rmPLUS: cname = cname.replace("+-", "")

    elif cname.find("-") >= 0:
        if rmDASH: cname = cname.replace("-", "")

    return cname

#.......................................................................
def split(list):
    """
    Split entries in a list on commas and colons.
    """
    newlist = []
    for str in list:
        newlist.extend(str.split(","))

    list = []
    for str in newlist:
        list.extend(str.split(":"))

    return list

#.......................................................................
def string_substitution_requests(args):
    """
    Gather the string substitution requests for the HISTORY.rc.tmpl file
    """
    str1str2list = args.str1str2list
    str1list     = args.str1list
    str2list     = args.str2list

    strSubs = {}
    for s1s2 in str1str2list:
        s12 = s1s2.split("=")
        if len(s12) == 2:
            (str1, str2) = s12
            strSubs[str1] = str2
        else:
            raise Exception(f"ERROR. ill-formatted string substitution: {s1s2}")

    if str1list and len(str1list) == len(str2list):
        strSubs = strSubs | dict(zip(str1list, str2list))

    return strSubs

#.......................................................................
def sum_slices(hist_rc, args):
    """
    """
    sumflag = args.sumflag
    print("The -sum option has not yet been implemented.")

#.......................................................................
def write_HISTfile(hist_rc, args):
    """
    Write the output HISTORY template file.
    """
    orderflag = args.orderflag
    outfile   = args.outfile

    if os.path.isfile(outfile):
        outtilde = outfile + "~"
        if not os.path.exists(outtilde):
            os.rename(outfile, outtilde)

    hist_rc.hwrite(outfile, orderflag)

#.......................................................................
def write_datarc(template, cname, plotHISTdir):
    """
    Write data rc template file.
    """

    # write rc file for const data
    #-----------------------------
    if cname.startswith("const"):
        dirlocSTR = r"#*(\w+)/"
        dirlocPTR = re.compile(dirlocSTR)

        if not dirlocPTR.match(template):
            raise Exception(f"ERROR. Unable to write datarc template for {cname}")

        dirloc = "/"+dirlocPTR.match(template).group(1)
        datarc = f"{plotHISTdir}/{cname}.tmpl"
        with open(datarc, mode="w") as drc:
            name_tmpl = f"{cname}.nc4"
            drc.write(f"DSET >>>FVHOME<<<{dirloc}>>>EXPID<<<.{name_tmpl}\n")
            drc.write(f"TITLE >>>EXPID<<<\n")
            drc.write(f"OPTIONS template\n")
            drc.write(f"TDEF time 1 LINEAR 00:00Z01>>>TDEF<<< 1mo\n")
        return

    # write rc file for all other data
    #---------------------------------
    dirlocSTR = r"#*(.*M%m2/)"
    dirlocPTR = re.compile(dirlocSTR)
    
    if not dirlocPTR.match(template):
        raise Exception(f"ERROR. Unable to write datarc template for {cname}")

    dirloc = "/"+dirlocPTR.match(template).group(1)
    datarc = f"{plotHISTdir}/{cname}.tmpl"
    with open(datarc, mode="w") as drc:
        name_tmpl = f"{cname}.monthly.%y4%m2.nc4"
        drc.write(f"DSET >>>FVHOME<<<{dirloc}>>>EXPID<<<.{name_tmpl}\n")
        drc.write(f"TITLE >>>EXPID<<<\n")
        drc.write(f"OPTIONS template\n")
        drc.write(f"TDEF time >>>NFILES<<< LINEAR 00:00Z01>>>TDEF<<< 1mo\n")

#.......................................................................
def write_silo_mstorage_arc(hist_rc, args):
    """
    Write arc files, silo_arc and mstorage_arc, based on info in HISTORY file
    """
    appendflag   = args.appendflag
    exclude      = args.exclude
    fcstflag     = args.fcstflag
    infile       = args.infile
    mstorage_arc = args.mstorage_arc
    silo_arc     = args.silo_arc

    if appendflag: modeflag = "a"
    else:          modeflag = "w"

    for arcfile in (silo_arc, mstorage_arc):
        with open(arcfile, mode=modeflag) as arcf:
            arcf.write("#\n" +
                       "#   -------------------\n" +
                       "#   HISTORY COLLECTIONS\n" +
                       "#   -------------------\n" +
                       f"#   {infile}\n" +
                       "#\n")

            for cname in hist_rc.collection_names():

                # write the collection comments
                #------------------------------
                cmtLIST = hist_rc.collections_comments(cname)
                if cmtLIST:
                    arcf.write("#\n")
                    for comment in cmtLIST:
                        arcf.write(comment+"\n")
                    arcf.write("#\n")

                # write template
                #---------------
                silo = hist_rc.trait_value(cname, "silo")
                msflag = hist_rc.trait_value(cname, "mstorage")
                template = hist_rc.trait_value(cname, "template")

                (dt, punkt, ext) = template.partition(".")
                if fcstflag and ext:
                    if "h2%n2z" in dt:
                        dt1 = dt.replace("%n2", "")
                    else:
                        dt1 = dt
                    template = f"{dt1}+{dt}.{ext}"
                name1 = rm_dash_plus(cname, args)

                pestoroot = "${PESTOROOT}"
                silo_path = f"{pestoroot}%s/{silo}/%s.{name1}.{template}\n"
                silo_path = silo_path.replace("'", "")
                silo_path = silo_path.replace(">>>NCSUFFIX<<<", "nc4")
                if cname in exclude:
                    silo_path = "#" + silo_path

                if arcf == mstorage_arc:
                    if msflag.lower() != "'yes'":
                        silo_path = "#--" + silo_path
                arcf.write(silo_path)

#.......................................................................
if __name__ == "__main__":
    """
    Edit the HISTORY.rc.tmpl file.
    """
    script = os.path.basename(__file__)

    help_I = "names to include in list of collections"
    help_ID = 'include collections which end with "-"'
    help_IP = 'include collections which end with "+-"'
    help_Iall = "include all collections except those excluded by other flags"
    help_Ipm = ("match patterns used to determine which names\n"+
               "to include in list of collections")
    help_X = "names to exclude from list of collections"
    help_XD = 'exclude collections which end with "-"'
    help_XP = 'exclude collections which end wit "+-"'
    help_Xall = "exclude all collections except those included by other flags"
    help_Xdep = "exclude collections which include data from source, e.g. GOCART"
    help_Xpm = ("match patterns usedto determine which names\n"+
               "to exclude from list of collections")
    help_db = "print extra info for debugging purposes"
    help_arc = ("write arc files: silo.hist.arc and mstorage.hist.arc"+
               "(see -silo and -mstorage flags)")
    help_add = ("add silo and mstorage traits to collections that do not have\n"+
               "them; if the -arc flag is not called, then the HISTORY.rc file\n"+
               "will be written with silo and mstorage traits for all collections")
    help_append = ("with -arc flag; append to arc files if they already exist,"+
                  "rather then overwriting")
    help_fcst = "with -arc flag; write arc entries for forecast output"
    help_silo= "with -arc flag; use alternate name for silohist.arc output"
    help_mstorage = "with -arc flag; use alternate name for mstorage.hist.arc output"
    help_i = 'edit input file "in place"; i.e. overwrite input with output'
    help_infile = "input HISTORY file (default: HISTORY.rc.tmpl)"
    help_list = ("all     list all collections\n"+
                "inc     list only the included collections\n"+
                "exc     list only the excluded collections\n\n"+
                "Add ':' or ',' to end of list option to get output as\n"+
                "single string with names separated by colons or commas;\n\n"+
                f"Ex: {script} -list inc  (print included collections, "+
                "one per line)\n"+
                f"    {script} -list inc: (print collections "+
                "in string, separated by colons)\n"+
                f"    {script} -list inc, (print collections "+
                "in string, separated by commas)\n\n")
    help_o = ("name of output HISTORY template file; defaults to STDOUT;\n"+
             "if outfile is directory name, then the output will be written\n"+
             "in the directory with the same name as the infile")
    help_order = "print definitions in same order as listed in top COLLECTIONS"
    help_plot = ("name or directory location of monthly.rc file to\n"+
                "use for writin the monthly_plots HISTORY file;\n")
    help_q = ("quiet mode; turns off some of the WARNINGs\n"+
             "   n==1 turn off INFO messages\n"+
             "   n==2 turn off WARNING messages\n"+
             "   n==3 turn off INFO and WARNING messages\n"+
             "if flag is given without value, then defaults to 3\n\n")
    help_rD = 'remove "-" from collection name endings'
    help_rP = 'remove "+-" from collection name endings'
    help_s = "substitute str1 with str2"
    help_s1 = "substitute str1 with str2"
    help_s2 = "substitute str1 with str2"
    help_sum = ("sum the number of output slices per 24-hour period\n"+
               "i.e. sum of variables times levels times frequency\n"+
               "     for each output collection\n\n"+
               "if n=0, then print only the final value\n"+
               "if n=1, then print details grouped by number of atmoshere levels\n"+
               "if n=2, then print details grouped by data frequency\n"+
               "* defaults to n=1 if 'n' is excluded.\n\n"+
               "You may want to use the -q flag with -sum to turn off WARNINGs")

    class CustomFormatter(argparse.RawTextHelpFormatter,
                          argparse.RawDescriptionHelpFormatter):
        pass

    parser = argparse.ArgumentParser(description='test\ntest\ntest.',
                                     epilog='test\ntest\ntest.',
                                     formatter_class=CustomFormatter)

    parser = ArgumentParser(description=__doc__, formatter_class=CustomFormatter)

    group1 = parser.add_mutually_exclusive_group()
    group2 = parser.add_mutually_exclusive_group()
    group3 = parser.add_mutually_exclusive_group()
    group4 = parser.add_mutually_exclusive_group()

    parser.add_argument("infile",
                        default="./HISTORY.rc.tmpl",
                        nargs="?",
                        help=help_infile)

    group1.add_argument("-o", dest="outfile",
                        help=help_o)

    group1.add_argument("-i", dest="inplace",
                        action="store_true",
                        help=help_i)

    group2.add_argument("-Iall", dest="includeAll",
                        action="store_true",
                        help=help_Iall)

    group2.add_argument("-Xall", dest="excludeAll",
                        action="store_true",
                        help=help_Xall)

    parser.add_argument("-I", dest="include",
                        action="extend",
                        default=[], 
                        metavar="names",
                        nargs="+",
                        help=help_I)

    parser.add_argument("-X", dest="exclude",
                        action="extend",
                        default=[],
                        metavar="names",
                        nargs="+",
                        help=help_X)

    parser.add_argument("-Ipm", dest="incPM",
                        action="extend",
                        default=[],
                        metavar="patterns",
                        nargs="+",
                        help=help_Ipm)

    parser.add_argument("-Xpm", dest="excPM",
                        action="extend",
                        default=[],
                        metavar="patterns",
                        nargs="+",
                        help=help_Xpm)

    parser.add_argument("-Xdep", dest="excDEP",
                        action="extend",
                        default=[],
                        metavar="source",
                        nargs="+",
                        help=help_Xdep)

    group3.add_argument("-ID", dest="incDASH",
                        action="store_true",
                        help=help_ID)

    group3.add_argument("-XD", dest="excDASH",
                        action="store_true",
                        help=help_XD)

    group4.add_argument("-IP", dest="incPLUS",
                        action="store_true",
                        help=help_IP)

    group4.add_argument("-XP", dest="excPLUS",
                        action="store_true",
                        help=help_XP)

    parser.add_argument("-rD", dest="rmDASH",
                        action="store_true",
                        help=help_rD)

    parser.add_argument("-rP", dest="rmPLUS",
                        action="store_true",
                        help=help_rP)

    parser.add_argument("-s", dest="str1str2list",
                        action="extend",
                        default=[],
                        metavar="str1=str2",
                        nargs='+',
                        help=help_s)

    parser.add_argument("-str1", "-s1", dest="str1list",
                        action="extend",
                        default=[],
                        metavar="str1",
                        nargs="+",
                        help=help_s1)

    parser.add_argument("-str2", "-s2", dest="str2list",
                        action="extend",
                        default=[],
                        metavar="str2",
                        nargs="+",
                        help=help_s2)

    parser.add_argument("-q", dest="quiet",
                        choices=[1,2,3],
                        default=0,
                        nargs="?",
                        type=int,
                        help=help_q)

    parser.add_argument("-order", dest="orderflag",
                        action="store_true",
                        help=help_order)

    parser.add_argument("-db", "-debug",
                        action="store_true",
                        help=help_db)

    parser.add_argument("-plot", dest="mnthlyRC",
                        default=False,
                        nargs="?",
                        help=help_plot)

    parser.add_argument("-list", dest="listflag",
                        help=help_list)

    parser.add_argument("-sum", dest="sumflag",
                        default=False,
                        nargs="?",
                        help=help_sum)

    # arc file options
    #-----------------
    parser.add_argument("-arc", dest="arcflag",
                        action="store_true",
                        help=help_arc)

    parser.add_argument("-add", dest="addflag",
                        action="store_true",
                        help=help_add)

    parser.add_argument("-append", dest="appendflag",
                        action="store_true",
                        help=help_append)

    parser.add_argument("-fcst", dest="fcstflag",
                        action="store_true",
                        help=help_fcst)

    parser.add_argument("-silo", dest="silo_arc",
                        metavar="name",
                        help=help_silo)

    parser.add_argument("-mstorage", dest="mstorage_arc",
                        metavar="name",
                        help=help_mstorage)

    args = parser.parse_args()

    # flag defaults and dependencies
    #-------------------------------
    if args.sumflag is None:
        args.sumflag = True

    if args.appendflag:
        args.arcflag = True

    if args.arcflag:
        args.mnthlyRC is None
        args.addflag = True

        if not args.silo_arc:
            args.silo_arc = "silo.hist.arc"
        if not args.mstorage_arc:
            args.mstorage_arc = "mstorage.hist.arc"

    args.plotHIST = False
    if args.mnthlyRC is not False:
        if args.mnthlyRC is None:
            args.mnthlyRC = "monthly.rc"

        if os.path.isdir(args.mnthlyRC):
            args.mnthlyRC += "/monthly.rc"

        if not os.path.isfile(args.mnthlyRC):
            raise Exception(f"ERROR. Cannot find {mnthlyRC}")

        args.plotHIST = True
        args.orderflag = True

    if args.quiet is None:
        args.quiet = 3

    if not args.outfile:
        if args.inplace:
            args.outfile = args.infile
        elif args.plotHIST:
            args.outfile = "HISTORY.rc_tmpl"

    if args.outfile:
        if os.path.isdir(args.outfile):
            if args.plotHIST:
                args.outfile += "/HISTORY.rc_tmpl"
            else:
                args.outfile += f"/{args.infile}"

        if args.plotHIST:
            args.plotHISTdir = os.path.dirname(args.outfile)

    args.excDEP  = split(args.excDEP)
    args.excPM   = split(args.excPM)
    args.exclude = split(args.exclude)
    args.incPM   = split(args.incPM)
    args.include = split(args.include)

    args.str1str2list = split(args.str1str2list)
    args.str1list     = split(args.str1list)
    args.str2list     = split(args.str2list)

    len1 = len(args.str1list)
    len2 = len(args.str2list)
    if len1 != len2:
        msg = f"str1list/str2list count mismatch ({len1} != {len2})"
        raise Exception(f"ERROR. {msg}")

    main(args)
