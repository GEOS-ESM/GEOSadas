#!/usr/bin/env python
"""program for simple visualization of family/task hierarchy in a suite"""

import argparse
from collections import OrderedDict
from pylib import utils


def main():
    """main program"""
    argv = parse_args()
    suite_config = utils.yaml2dict(argv.suite_config_file)
    simpleviz(suite_config, verbose=argv.verbose, indent=argv.indent, level=1)


def simpleviz(mydict, verbose=False, indent=4, level=1):
    """recursive visualization function"""
    for key, val in mydict.iteritems():
        if key in ["Type", "Trigger", "Variable"] and not verbose:
            continue
        print "%s%s" % (("|"+" "*(indent-1))*(level-1), key)
        if isinstance(val, OrderedDict):
            simpleviz(val, verbose, indent, level+1)  # recurse


def parse_args():
    """parse command line arguments"""
    prsr = argparse.ArgumentParser(description="Simple visualization of task hierarchy")
    prsr.add_argument("suite_config_file", help="yaml file")
    prsr.add_argument("--indent", help="default=4", default=4, type=int)
    prsr.add_argument("--verbose", help="toggle verbosity", action="store_true")
    return prsr.parse_args()


if __name__ == "__main__":
    main()
