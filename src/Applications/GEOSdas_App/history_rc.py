import os
import re

class HistoryRc(object):

    #.......................................................................
    def __init__(self, filename="HISTORY.rc.tmpl", strSubs={}, quietflag=0):
        """
        Initialize HISTORY.rc.tmpl instance.

        Note:
        self._info = { "prolog"       : prolog,
                       "header"       : header,
                       "gridlabels"   : gridlabels,
                       "gridLIST"     : gridLIST,
                       "collections"  : collections,
                       "collectLIST"  : collectLIST }
        where

          prolog: comments at the top of the file
          header: variable definitions following the prolog
          gridlabels: list of grid labels
          gridLIST: list of grid definitions
          collections: list of collection names
          collectLIST: list of collection definitions
          
          prolog = [ cmtLIST ]

          header = [ ("VERSION", VERSION),
                     ("EXPID", EXPID),
                     ("EXPDSC", EXPDSC) ]
                          
          gridlabels = [ gridlabels ]

          gridLIST = [ (gridlabel, gridDEF), ... ]
          gridDEF = [ ("GRID_TYPE", grid_type),
                      ("IM_WORLD", im_world),
                      ("JM_WORLD", jm_world),
                      ("POLE", pole),
                      ("DATELINE", dateline),
                      ("LM", lm) ]
                              
          collections = [ (cname, cmt3) }

          collectLIST = [ (cname, (cmtLIST, collectDEF)), ... ]

          collectDEF = [ ("format",     (format,     cmtflag)),
                         ("descr",      (descr,      cmtflag)),
                         ("nbits",      (nbits,      cmtflag)),
                         ("template",   (template,   cmtflag)),
                         ("mode",       (mode,       cmtflag)),
                         ("grid_label", (gridlabel,  cmtflag)),
                         ("frequency",  (frequency,  cmtflag)),
                         ("duration",   (duration,   cmtflag)),
                         ("end_date",   (end_date,   cmtflag)),
                         ("end_time",   (end_time,   cmtflag)),
                         ("vscale",     (vscale,     cmtflag)),
                         ("vunit",      (vunit,      cmtflag)),
                         ("vvars",      (vvars,      cmtflag)),
                         ("levels",     (levelLIST,  cmtflag)),
                         ("fields",     (fieldLIST,  cmtflag)) ]
          fieldLIST = [ (field, cmt4), ... ]
          field = [ fname, source, nname, max/min ]

          cmt3 = { "list": cmtLIST,        # comment lines prior to item
                   "flag": cmtflag,        # True, if item is commented
                   "at_label": at_label }  # inline @ label

          cmt4 = { "list": cmtLIST,        # comment lines prior to item
                   "flag": cmtflag,        # True, if item is commented
                   "inline": inline,       # inline comment
                   "at_label": at_label }  # inline @ label
                   
        """
        if not os.path.isfile(filename):
            raise ValueError(f"File not found: {filename}")
        self.filename = filename
        self.quietflag = quietflag
        self._info = {}
        self.__read(strSubs)

    #.......................................................................
    def __read(self, strSubs):
        """
        Read information from HISTORY.rc.tmpl file.
        """
        with open(self.filename, mode='r') as history_rc:

            # create RE pattern objects
            #--------------------------
            headerSTR = r"\s*(\S+)\s*:\s*(\S+)"
            headerPTR = re.compile(headerSTR)

            gdefSTR = r"\s*(\S+)\.(\S+)\s*:\s*(\S+)"
            gdefPTR = re.compile(gdefSTR)

            stringSTR = r"\s*(\S+)"
            stringPTR = re.compile(stringSTR)

            collectionsSTR = r"\s*COLLECTIONS:"
            collectionsPTR = re.compile(collectionsSTR)

            cnameSTR = r"\s*(['\"])(\S+)\1"
            cnamePTR = re.compile(cnameSTR)

            atSTR = r"(@\w+)\b"
            atPTR = re.compile(atSTR)

            cdefSTR = r"\s*(\S+)\.(\S+)\s*:(.+)$"
            cdefPTR = re.compile(cdefSTR)

            fieldSTR = r"\s*(['\"])(\S+)\1"
            fieldPTR = re.compile(fieldSTR)

            blankSTR = r"\s*$"
            blankPTR = re.compile(blankSTR)

            commentSTR = r"\s*#"
            commentPTR = re.compile(commentSTR)

            incommentSTR = r"\s*#([\w\s]*)(['\"])\S+\2"
            incommentPTR = re.compile(incommentSTR)

            endSTR = r"::"
            endPTR = re.compile(endSTR)

            fieldendSTR = r"^::|\W::"
            fieldendPTR = re.compile(fieldendSTR)

            look4prolog = True
            look4header = False
            look4glabels = False
            look4gdefs = False
            look4cname = False
            look4clist = False
            look4fields = False

            prolog = []
            header = []
            gridlabels = []
            dummy = []
            gridLIST = []
            gridDEF = []
            gridlabel_curr = ""
            collections = []
            collectLIST = []
            collectDEF = []
            fieldLIST = []
            cmtLIST = []

            startcdef = True

            duplicate = []
            conflict = []

            # loop through lines of file
            #---------------------------
            for line in history_rc:
                line = line.rstrip()
                if blankPTR.match(line): continue

                # string substitutions
                #---------------------
                for str1 in strSubs:
                    str2 = strSubs[str1]
                    line = line.replace(str1, str2)

                # read prolog
                #------------
                if look4prolog:

                    if headerPTR.match(line):
                        look4prolog = False
                        look4header = True
                        
                    else:
                        prolog.append(line)
                        continue

                # extract header lines
                #---------------------
                if look4header:

                    if headerPTR.match(line):
                        (var, val) = headerPTR.match(line).groups()

                        if var == "GRID_LABELS":
                            look4header = False
                            look4glabels = True

                        else:
                            header.append((var, val))
                            continue

                # extract grid labels
                #--------------------
                # NOTE: the gridlabels info is read into a dummy variable now.
                #       the real info will be extracted later from gridLIST.
                #-----------------------------------------------------------
                if look4glabels:

                    if headerPTR.match(line):
                        (var, val) = headerPTR.match(line).groups()
                        if var != "GRID_LABELS":
                            errmsg = "New variable found in GRID_LABELS list: {}"
                            raise Exception(errmsg.format(var))
                        else:
                            dummy.append(val.strip())

                    elif stringPTR.search(line):
                        val = stringPTR.search(line).group(0)
                        if val != "::": 
                            dummy.append(val.strip())

                    if endPTR.search(line):
                        look4glabels = False
                        look4gdefs = True

                    continue

                # extract grid definitions
                #-------------------------
                if look4gdefs:

                    if gdefPTR.match(line):
                        (gridlabel, var, val) = gdefPTR.match(line).groups()
                        
                        if gridlabel_curr == "":
                            gridlabel_curr = gridlabel

                        if gridlabel in dict(gridLIST):
                            gridDEF = dict(gridLIST)[gridlabel]

                            if var in dict(gridDEF):
                                val0 = dict(gridDEF)[var]
                                if val == val0:
                                    duplicate.append(line)
                                else:
                                    line = f"{gridlabel}.{var}: {val0} != {val}"
                                    conflict.append(line)
                            else:
                                index = gridLIST.index((gridlabel, gridDEF))
                                gridLIST.pop(index)
                                gridDEF.append((var, val))
                                gridLIST.insert(index, (gridlabel, gridDEF))

                        else:
                            gridDEF= []
                            gridDEF.append((var, val))
                            gridLIST.append((gridlabel, gridDEF))

                    elif collectionsPTR.match(line):
                        if gridlabel_curr not in dict(gridLIST):
                            gridLIST.append((gridlabel_curr, gridDEF))

                        look4gdefs = False
                        look4cname = True
                        cmtLIST = []

                        if duplicate:
                            for ll in duplicate:
                                msg = f"WARNING. Duplicate grid variable: {ll}"
                                self.print(msg)

                        if conflict:
                            for ll in conflict:
                                msg = f"WARNING. grid variable conflict: {ll}"
                                self.print(msg)

                # extract collections list
                #-------------------------
                if look4cname:
                    
                    if cnamePTR.search(line):
                        cname = cnamePTR.search(line).group(2)
                        if commentPTR.match(line):
                            cmtflag = True
                        else:
                            cmtflag = False
                        if atPTR.search(line):
                            at_label = atPTR.search(line).group(1)
                        else:
                            at_label = ""

                        cmt3 = { "list": cmtLIST,
                                 "flag": cmtflag,
                                 "at_label": at_label }

                        self.__update_collections(collections, cname, cmt3)
                        cmtLIST = []

                    elif commentPTR.match(line):
                        cmtLIST.append(line)

                    if endPTR.search(line):
                        look4cname = False
                        look4clist = True

                    continue

                # extract collection definitions
                #-------------------------------
                if look4clist:

                    if startcdef:
                        if commentPTR.match(line):
                            cmtLIST.append(line)

                    if cdefPTR.search(line):
                        (cname, var, val) = cdefPTR.search(line).groups()
                        val = val.strip("\t ,")

                        if commentPTR.match(line):
                            cmtflag = True
                        else:
                            cmtflag = False

                        if startcdef:
                            cname_curr = cname
                            startcdef = False
                            cmtLIST_ = cmtLIST
                            cmtLIST = []
                        else:
                            if cname != cname_curr:
                                warn = "WARNING. Name inconsistency in {} def"
                                msg = "WARNING. Changing {0}.{1} to {2}.{1}"
                                self.print(warn.format(cname_curr))
                                self.print(msg.format(cname, var, cname_curr))
                                cname = cname_curr

                        if var == "fields":
                            look4clist = False
                            look4fields = True
                            firstfield = True

                        else:
                            collectDEF.append((var, (val, cmtflag)))

                # extract collection fields
                #--------------------------
                if look4fields:

                    # if multiple "fields", then store and start over
                    #------------------------------------------------
                    if cdefPTR.search(line):
                        if fieldLIST:
                            collectDEF.append(("fields", (fieldLIST, cmtflag)))
                            fieldLIST = []

                            (cname, var, val) = cdefPTR.search(line).groups()
                            if var != "fields":
                                raise Exception(f"ERROR; var = {var}")
                            val = val.strip("\t ,")

                    if fieldPTR.search(line):
                        field = [fld for indx, fld in fieldPTR.findall(line)]

                        while len(field) < 4:
                            field.append("")

                        cmtflag = False
                        inline = ""
                        at_label = ""
                        if commentPTR.match(line):
                            cmtflag = True
                            if incommentPTR.match(line):
                                inline = incommentPTR.match(line).group(1).strip()
                        if atPTR.search(line):
                            at_label = atPTR.search(line).group(1)

                        cmt4 = { "list": cmtLIST,
                                 "flag": cmtflag,
                                 "inline": inline,
                                 "at_label": at_label }
                                 
                        fieldLIST.append((field, cmt4))
                        cmtLIST = []

                    else:
                        if commentPTR.match(line):
                            cmtLIST.append(line)

                    if fieldendPTR.search(line):
                        collectDEF.append(("fields", (fieldLIST, cmtflag)))
                        collectLIST.append((cname, (cmtLIST_, collectDEF)))
                        collectDEF = []
                        fieldLIST = []
                        cmtLIST = []
                        look4fields = False
                        look4clist = True
                        startcdef = True
                    continue

            # disregard previous gridlabels list;
            # redefine based on grid defs
            #------------------------------------
            gridlabels = [ glabel for glabel, gdef in gridLIST ]

            # store extracted values
            #-----------------------
            self.__check_grids(gridlabels, gridLIST)
            self.__check_collections(collections, collectLIST, gridlabels)

            self._info["prolog"] = prolog
            self._info["header"] = header
            self._info["gridlabels"] = gridlabels
            self._info["gridLIST"] = gridLIST
            self._info["collections"] = collections
            self._info["collectLIST"] = collectLIST        

    #.......................................................................
    def __update_collections(self, collections, cname, cmt3):
        """
        Add a new collection to the list; if a previous record exists with
        the same collection name, then replace the previous record if the
        previous record is commented and the new one isn't. Otherwise, discard
        the new record (i.e. first record read takes precedence).
        """
        appendflag = True

        # check for duplicate entry
        #--------------------------
        if cname in dict(collections):
            warn = "WARNING. Duplicate name in COLLECTIONS: {}"

            cmt3_ = dict(collections)[cname]
            cmtflag_ = cmt3_["flag"]
            cmtflag = cmt3["flag"]

            # replace if previous entry commented and new one isn't
            #------------------------------------------------------
            if cmtflag_ and not cmtflag:
                index = collections.index((cname, cmt3_))
                collections.pop(index)
                warn += "; replaced"
            else:
                warn += "; discarded"
                appendflag = False

            self.print(warn.format(cname))

        # append record
        #--------------
        if appendflag:
            collections.append((cname, cmt3))

    #.......................................................................
    def __check_grids(self, gridlabels, gridLIST):
        """
        Check for errors in the gridLIST.
        """
        gvars = ["GRID_TYPE", "IM_WORLD", "JM_WORLD", "POLE", "DATELINE", "LM"]

        errcnt = 0
        for glabel, gdef in gridLIST:
            for var, val in gdef:
                if var not in gvars:
                    self.print(f"WARNING. Unexpected grid variable: {glabel}.{var}")
                    errcnt += 1

            for var in gvars:
                if var not in dict(gdef):
                    warn = "WARNING. Expected grid variable not found: {}.{}"
                    msg = "WARNING. Adding grid variable, {}.{}"
                    self.print(warn.format(glabel, var))
                    self.print(msg.format(glabel, var))
                    errcnt += 1

                    # set "LM" value equal to 72, if missing
                    #---------------------------------------
                    if var == "LM":
                        gridDEF = dict(gridLIST)[glabel]
                        index = gridLIST.index((glabel, gridDEF))
                        gridLIST.pop(index)

                        gridDEF.append(("LM", 72))
                        gridLIST.insert(index, (glabel, gridDEF))
                        
    #.......................................................................
    def __check_collections(self, collections, collectLIST, gridlabels):
        """
        Check for errors in collections and collectLIST.
        """
        script = os.path.basename(__file__)

        # check that each collection definition is listed in COLLECTIONS list
        # if not, then add it to the list and comment it
        #-----------------------------------------------
        notfound = []
        for cname, (cmtLIST, cdef) in collectLIST:
            if cname not in dict(collections):
                notfound.append(cname)

        if notfound:
            first = True

            for cname in notfound:
                self.print(f"WARNING. Added to COLLECTIONS: {cname}")

                if first:
                    msg1 = f"# Added by {script} script"
                    len1 = len(msg1)-2
                    msg2 = f"# {'!'*len1}"

                    cmtLIST = [ msg2, msg1, msg2 ]
                    first = False
                else:
                    cmtLIST = []
                at_label = ""

                cmt3 = { "list": cmtLIST, "flag": True, "at_label": at_label }
                collections.append((cname, cmt3))

        # check that each name in COLLECTIONS list has a definition
        # if not, then move name to end of the list and comment it
        #---------------------------------------------------------
        notfound = []
        for cname, cmt3 in collections:
            if cname not in dict(collectLIST):
                index = collections.index((cname, cmt3))
                notfound.append((index, cname))

        if notfound:
            first = True
            for index, cname in notfound:
                self.print(f"WARNING. No definition: {cname}")
                if first:
                    msg0 = "#"
                    msg1 = "# Collection name(s) without definition"
                    msg2 = "# -------------------------------------"
                    cmtLIST = [ msg0, msg1, msg2 ]
                    first = False
                else:
                    cmtLIST = []
                at_label = ""

                cmt3 = { "list": cmtLIST, "flag": True, "at_label": at_label }

                collections.pop(index)
                collections.append((cname, cmt3))
                
        # check grid_label in each collectDEF is also in gridlabels
        #----------------------------------------------------------
        errcnt = 0
        for cname, (cmtLIST, cdef) in collectLIST:
            var = "grid_label"
            if var in dict(cdef):
                glabel = dict(cdef)[var][0]
                if glabel not in gridlabels:
                    errmsg = "ERROR. grid_label not defined: {}.{}: {}"
                    print(errmsg.format(cname, var, glabel))
                    errcnt += 1

    #.......................................................................
    def add_trait(self, cname, trait, val, cmtflag=False, index=None,
                  overwrite=False):
        """
        Add a trait for collection name to collectLIST.
        input parameters:
        => cname: collection into which to insert trait
        => trait: trait name
        => val: value of trait
        => cmtflag: if True, then trait is to be commented, default = False
        => index: index indicating where to insert trait into cdef array;
                  default = append to end
        => overwrite: if True, then overwrite trait value if one exists
        """

        # get collectLIST information
        #----------------------------
        collectLIST = self._info["collectLIST"]
        if cname not in dict(collectLIST):
            self.print(f"WARNING. collection {cname} not found for add_grads_ddf()")
            return

        (cmtLIST, cdef) = dict(collectLIST)[cname]
        indexCL = collectLIST.index((cname, (cmtLIST, cdef)))

        # is trait already in collection?
        #--------------------------------
        if trait in dict(cdef):
            info = f"{trait} trait already in {cname} collection"

            # do not overwrite
            #-----------------
            if not overwrite:
                self.print(f"INFO. {info}.")
                return

            # collection already has specified trait and value
            #-------------------------------------------------
            (val_, cmtflag_) = dict(cdef)[trait]
            if (val, cmtflag) == (val_, cmtflag_):
                info += f", with (val, cmtflag) = ({val}, {cmtflag})\n"
                self.print(f"INFO. {info}")
                return

            # remove conflicting trait and value
            #-----------------------------------
            info += (". Replacing (val, cmtflag): " +
                    f"({val_}, {cmtflag_}) => ({val}, {cmtflag})\n")
            index_ = cdef.index((trait, (val_, cmtflag_)))
            cdef.pop(index_)
            cdef.insert(index_, (trait, (val, cmtflag)))

        # add new trait and value
        #------------------------
        elif index:
            cdef.insert(index, (trait, (val, cmtflag)))
            info = f"{trait} trait inserted into {cname} collection"
        else:
            cdef.append((trait, (val, cmtflag)))
            info = f"{trait} trait appended to {cname} collection"
        self.print(f"INFO. {info}")

        # replace old cname record with modified record
        #----------------------------------------------
        collectLIST.pop(indexCL)
        collectLIST.insert(indexCL, (cname, (cmtLIST, cdef)))
        self._info["collectLIST"] = collectLIST

    #.......................................................................
    def collectLIST_comments(self, cname):
        """
        Return the commentLIST comments for a specified collection.
        """
        collectLIST = self._info["collectLIST"]
        (cmtLIST, cdef) = dict(collectLIST)[cname]
        return cmtLIST

    #.......................................................................
    def collection_names(self, flag=1):
        """
        Generator function returns collection names one at a time.
        if flag == 1, then return cnames in the order from "collections"
        if flag != 1, then return cnames in the order from "collectLIST"
        """
        if flag == 1:
            for cname in dict(self._info["collections"]): yield cname
        else:
            for cname in dict(self._info["collectLIST"]): yield cname

    #.......................................................................
    def collections_comments(self, cname):
        """
        Return the collections comments for a specified collection.
        """
        collections = self._info["collections"]
        cmt3 = dict(collections)[cname]
        return cmt3["list"]

    #.......................................................................
    def comment_collection(self, cname=None, dep=None, flag=True):
        """
        if cname is given:
           comment cname collection, if flag is True
           uncomment cname collection, if flag is False.

        if dep is given:
           comment all collections which have dep as a field source
        """
        if cname:
            collections = self._info["collections"]
            cmt3 = dict(collections)[cname]

            flag_ = cmt3["flag"]
            if flag != flag_:
                cmt3["flag"] = flag
                index = collections.index((cname, cmt3))
                self._info["collections"].pop(index)
                self._info["collections"].insert(index, (cname, cmt3))

        if dep:
            collections = self._info["collections"]
            collectLIST = self._info["collectLIST"]

            #---------------------------------------
            # cmtflag1 = collection name cmtflag
            # cmtflag2 = collection variable cmtflag
            # cmtflag3 = collection field cmtflag
            #---------------------------------------
            for cname, (cmtLIST, collectDEF) in collectLIST:
                cmt3 = dict(collections)[cname]
                cmtflag1 = cmt3["flag"]

                if not cmtflag1:
                    (fieldLIST, cmtflag2) = dict(collectDEF)["fields"]
                    if not cmtflag2:
                        for (field, cmt4) in fieldLIST:
                            cmtflag3 = cmt4["flag"]
                            if not cmtflag3:
                                [fname, source, nname, maxmin] = field
                                if source.upper() == dep.upper():
                                    self.comment_collection(cname)

    #.......................................................................
    def commented(self, cname):
        """
        Return True if cname is commented in COLLECTIONS list
        """
        collections = self._info["collections"]
        cmt3 = dict(collections)[cname]
        return cmt3["flag"]

    #.......................................................................
    def delete_trait(self, cname, trait):
        """
        Delete trait from specified collection (cname)
        """
        collectLIST = self._info["collectLIST"]

        # return if cname not found
        #--------------------------
        if cname not in dict(collectLIST):
            self.print(f"WARNING. collection {cname} not found for delete {trait}")
            return

        # return if cname record does not contain trait
        #----------------------------------------------
        (cmtLIST, cdef) = dict(collectLIST)[cname]
        if trait not in dict(cdef):
            self.print(f"WARNING. trait {trait} not found in collection {cname}")
            return

        # find index of cname record
        #---------------------------
        index = collectLIST.index((cname, (cmtLIST, cdef)))

        # remove trait from cname record
        #-------------------------------
        (val, cmtflag) = dict(cdef)[trait]
        cdef.remove((trait, (val, cmtflag)))

        # replace old cname record and insert modified record
        #----------------------------------------------------
        collectLIST.pop(index)
        collectLIST.insert(index, (cname, (cmtLIST, cdef)))

        self._info["collectLIST"] = collectLIST

    #.......................................................................
    def hwrite(self, newname="default", orderflag=False):
        """
        Write data from self._info to output file.

        input parameters:
        => newname: name of output file
        """
        if newname == "default":
            newname = self.filename + ".new"

        if os.path.isfile(newname):
            os.remove(newname)
        with open(newname, mode='w') as new:

            # write prolog
            #-------------
            prolog = self._info["prolog"]
            for line in prolog:
                new.write(line+"\n")
            new.write("\n")

            # write header
            #-------------
            header = self._info["header"]
            for var, val in header:
                line = f"{var}: {val}\n"
                new.write(line)
            new.write("\n")

            # write gridlabels
            #-----------------
            gridlabels = self._info["gridlabels"]
            first = True
            for glabel in gridlabels:
                cmtflag = False
                if glabel[0:1] == "#":
                    glabel = glabel[1:]
                    cmtflag = True
                if first:
                    if cmtflag:
                        new.write("GRID_LABELS:\n")
                        line = f"             {glabel}"
                    else:
                        line = f"GRID_LABELS: {glabel}"
                    first = False
                else:
                    line = f"             {glabel}"
                    if cmtflag:
                        line = "#" + line[1:]
                new.write(line+"\n")
            new.write(" ::\n\n")
                     
            # write gridLIST
            #---------------
            gridLIST = self._info["gridLIST"]
            for glab, gdef in gridLIST:
                for var, val in gdef:
                    new.write(f"{glab}.{var}: {val}\n")
                new.write("\n")

            # write collections
            #------------------
            collections = self._info["collections"]
            first = True
            for cname, cmt3 in collections:
                for line in cmt3["list"]:
                    new.write(line+"\n")

                if first:
                    if cmt3["flag"]:
                        new.write("COLLECTIONS:\n")
                        line = f"             '{cname}'\n"
                    else:
                        line = f"COLLECTIONS: '{cname}'\n"
                    first = False
                else:
                    line = f"             '{cname}'\n"

                front = cmt3["at_label"]
                if cmt3["flag"]:
                    front = "#" + front
                line = front + line[len(front):]
                new.write(line)
            new.write("             ::\n\n")
                 
            # write collection definitions
            #-----------------------------
            collectLIST = self._info["collectLIST"]

            if orderflag:
                cnameLIST = [cname for (cname, cmt3) in collections]
            else:
                cnameLIST = [cname for (cname, (cmtLIST, cdef)) in collectLIST]
            
            for cname in cnameLIST:
                (cmtLIST, cdef) = dict(collectLIST)[cname]
                if cmtLIST:
                    for line in cmtLIST:
                        new.write(line+"\n")
                    new.write("\n")

                maxlen = max([len(f"{cname}.{vvc[0]}:") for vvc in cdef])
                for var, (val, cmtflag) in cdef:
                    cnamevar = "{}.{}:".format(cname, var)
                    front  = f"  {cnamevar:{maxlen}}"
                    front_ = f"  {'':{maxlen}}"

                    if var == "fields":
                        fldLIST = val
                        first = True

                        maxLEN = []
                        for ii in range(4):
                            maxii = max([len(flds[ii]) for flds, cmt4 in fldLIST])
                            maxLEN.append(maxii+3)

                        for flds, cmt4 in fldLIST:
                            cmtLIST = cmt4["list"]
                            cmtflag = cmt4["flag"]
                            inline = cmt4["inline"]
                            at_label = cmt4["at_label"]

                            if cmtLIST:
                                for line in cmtLIST:
                                    new.write(line+"\n")

                            line = ""
                            if cmtflag:
                                line = "#"
                            if inline:
                                line = line + " " + inline + " "
                            if at_label:
                                line = line + at_label + " "

                            if first:
                                start = min([1, len(line)])
                                line += front[start:]
                                first = False
                            else:
                                start = len(line)
                                line += front_[start:]

                            for ii in range(4):
                                if flds[ii] == "": continue
                                str = f" '{flds[ii]}'"
                                line = line + f" {str:{maxLEN[ii]}} ,"

                            line += "\n"
                            new.write(line)

                    else:
                        if cmtflag:
                            front = "#" + front[1:]
                        new.write(f"{front}  {val} ,\n")

                new.write(f"{'':{maxlen+4}}::\n\n")

    #.......................................................................
    def print(self, msg):
        """
        Print a message to standard output, turning off some messages if the
        self.quietflag is non-zero.
        if self.quietflag = 1, turn off INFO messages
        if self.quietflag = 2, turn off WARNING messages
        if self.quietflag = 3, turn off INFO messages
        """
        if self.quietflag == 1:
            if msg.startswith("INFO"):
                return

        elif self.quietflag == 2:
            if msg.startswith("WARNING"):
                return

        elif self.quietflag == 3:
            if msg.startswith("INFO"):
                return
            if msg.startswith("WARNING"):
                return

        print(msg)

    #.......................................................................
    def strip_dashplus(self, cname):
        """
        Strip "-" or "+-" from collection name in collections and collectLIST.
        """
        collections = self._info["collections"]
        collectLIST = self._info["collectLIST"]

        # remove "+-" or "-" from end of cname
        #-------------------------------------
        if cname.find("+-") >= 0:
            cname_ = cname.replace("+-", "")

        elif cname.find("-") >= 0:
            cname_ = cname.replace("-", "")

        # check for cname conflict
        #-------------------------
        if cname_ in dict(collections):
            self.print(f"WARNING. Name conflict; cannot rename {cname} to {cname_}.")
            return

        # replace cname in "collections"
        #-------------------------------
        cmt3 = dict(collections)[cname]
        index = collections.index((cname, cmt3))
        self._info["collections"].pop(index)
        self._info["collections"].insert(index, (cname_, cmt3))

        # replace cname in "collectLIST"
        #-------------------------------
        (cmtLIST, cdef) = dict(collectLIST)[cname]
        index = collectLIST.index((cname, (cmtLIST, cdef)))
        self._info["collectLIST"].pop(index)
        self._info["collectLIST"].insert(index, (cname_, (cmtLIST, cdef)))

    #.......................................................................
    def trait_index(self, cname, trait):
        """
        Return the index for trait in the cname collectDEF list.
        """
        collectLIST = self._info["collectLIST"]
        if cname not in dict(collectLIST):
            raise Exception(f"Error. collection {cname} not found.")

        (cmtLIST, collectDEF) = dict(collectLIST)[cname]
        if trait in dict(collectDEF):
            (val, cmtflag) = dict(collectDEF)[trait]
            index = collectDEF.index((trait, (val, cmtflag)))
        else:
            index = -1

        return index

    #.......................................................................
    def trait_value(self, cname, trait):
        """
        Return the value of a collection trait.
        """
        collectLIST = self._info["collectLIST"]
        if cname not in dict(collectLIST):
            raise Exception(f"Error. collection {cname} not found.")

        (cmtLIST, collectDEF) = dict(collectLIST)[cname]
        if trait in dict(collectDEF):
            (val, cmtflag) = dict(collectDEF)[trait]
        else:
            val = None

        return val

    #.......................................................................
    def trim_collections(self, cnameLIST, incflag=True):
        """
        Remove collections from the data:

        input parameters:
        => cnameLIST: list of collection names
        => incflag:
              if true, then remove all collections except those in cnameLIST
              if false, then remove cnameLIST collections
        """
        collections = self._info["collections"]
        collectLIST = self._info["collectLIST"]

        for cname in dict(collections):
            if cname in cnameLIST:
                if incflag:
                    continue
            else:
                if not incflag:
                    continue

            cmt3 = dict(collections)[cname]
            collections.remove((cname, cmt3))

        for cname in dict(collectLIST):
            if cname in cnameLIST:
                if incflag:
                    continue
            else:
                if not incflag:
                    continue

            (cmtLIST, cdef) = dict(collectLIST)[cname]
            collectLIST.remove((cname, (cmtLIST, cdef)))

        self._info["collections"] = collections
        self._info["collectLIST"] = collectLIST
