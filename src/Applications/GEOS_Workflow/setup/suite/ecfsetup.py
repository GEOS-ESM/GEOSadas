"""Collection of routine to build/write an ecFlow suite"""

import os
import datetime
from collections import OrderedDict
import ecflow
from pylib import utils  # path to pylib needs to be added to path


def read_config_file(suite_config_file):
    """
    Read input file suite_config_file (yaml) and return an OrderedDict
    """
    suite_config_dict = utils.yaml2dict(suite_config_file)
    assert len(suite_config_dict) == 1
    utils.unicode2ascii(suite_config_dict)  # convert in-place
    return suite_config_dict


def _add_suite_variables(ecfste, rootdir, time0, stename):
    """
    Add these hard coded variables to the suite
    """
    ste_vars = OrderedDict([
        ("ECF_TRIES", 1),
        ("ECF_HOME", rootdir),
        ("ECF_INCLUDE", os.path.join(rootdir, "include")),
        ("STE_NAME", stename),
        ("TIME0", time0.strftime("%Y%m%d_%Hz")),
        ("BATCHOUT_DIR", os.path.join(rootdir, "batchout")),
        ("BATCH_SUBMIT", os.path.join(rootdir, "tools", "slurm", "submit.sh")),
        ("BATCH_KILL", os.path.join(rootdir, "tools", "slurm", "kill.sh")),
    ])
    map(lambda (k, v): ecfste.add_variable(k, v), ste_vars.iteritems())


def _add_variables_and_triggers(obj, defn):
    """
    obj: ecflow family/task to which variables and triggers are added
    defn: dict containing ecflow object definition
    """
    if "Variable" in defn:
        map(lambda (k, v): obj.add_variable(k, v), defn["Variable"].items())
    if "Trigger" in defn:
        # {'T1': 'complete'} -> ['T1==complete']
        eqlist = ["==".join(item) for item in defn["Trigger"].iteritems()]
        # ['T1==complete', 'T2==complete'] -> 'T1==complete and T2==complete'
        obj.add_trigger(reduce(lambda a, b: a+" and "+b, eqlist))


def _build_family(defn, obj):
    """
    defn: dict containing the family definition
    obj: an ecflow family object (updated)
    Recurse over ecflow families in "defn" and build ecflow family obj
    """
    for k in defn.keys():
        if k in ["Type", "Trigger", "Variable"]: continue
        if defn[k]["Type"] == "Family":
            fam = obj.add_family(k)
            _add_variables_and_triggers(fam, defn[k])
            _build_family(defn[k], fam)  # recurse
        elif defn[k]["Type"] == "Task":
            tsk = obj.add_task(k)  # create task
            _add_variables_and_triggers(tsk, defn[k])
        else:
            raise ValueError("Invalid type [%s]" % defn[k]["Type"])


def _add_repeat_to_timeloop(ecfste, time0, n_sixhr_segments):
    """
    Manually add repeat to the family TimeLoop
    """
    timeloop = ecfste.find_family("TimeLoop")
    if timeloop:
        six_hrs = datetime.timedelta(hours=6)  # each segment is 6-hr long
        dt_list = [(time0+i*six_hrs).strftime("%Y%m%d_%Hz")
                   for i in range(n_sixhr_segments)]
        timeloop.add_repeat(ecflow.RepeatString("SEGMENT", dt_list))


def build_suite(suite_config_dict, rootdir, time0, n_sixhr_segments):
    """
    Using suite_config_dict, create ecflow suite object and call the
    recursive method, _buildFamily, to build the full suite, then
    write the suite definition to file
    """
    stename = suite_config_dict.keys()[0]  # root node
    stedef = ecflow.Defs()
    with stedef.add_suite(stename) as ste:
        _add_suite_variables(ste, rootdir, time0, stename)
        _build_family(suite_config_dict[stename], ste)
        _add_repeat_to_timeloop(ste, time0, n_sixhr_segments)
    return (stename, stedef)


def write_suite(stedef, stefile):
    """
    Write suite definition to file
    """
    stedef.check_job_creation()  # .ecf -> job0
    stedef.check()  # check trigger expressions
    stedef.save_as_defs(stefile)


def copy_ecf_scripts(suite_config_dict, srcdir, dstdir):
    """
    Recurse through suite_config_dict, and
    - generate dest paths for ecf tasks that mirror the dict layout
    - copy the ecf task to its generated destination
    """
    for key, val in suite_config_dict.iteritems():
        if key in ["Type", "Trigger", "Variable"]: continue
        objtype = val["Type"]
        if objtype in ["Suite", "Family"]:
            copy_ecf_scripts(val, srcdir, os.path.join(dstdir, key))  # recurse
        elif objtype == "Task":
            utils.cpfile("%s.ecf" % key, srcdir, dstdir)  # copy
        else:
            raise ValueError("Invalid type: [%s]" % objtype)
