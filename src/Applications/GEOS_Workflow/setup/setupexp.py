#!/usr/bin/env python
"""
Script to setup ecflow workflow for an ADAS experiment
"""

import os
import argparse
import shutil
from datetime import datetime
from suite.setup import setup_suite
from env.setup import setup_env
from other.setup import setup_other

def main():
    """main program"""

    argv = parse_args()
    setup_suite(argv.suite_config_file, argv.time0, argv.n_sixhr_segs, argv.run_dir)
    setup_env(argv.env_config_file, argv.run_dir)
    setup_other([argv.suite_config_file, argv.env_config_file], argv.run_dir)


def parse_args():
    """parse command line arguments"""
    prsr = argparse.ArgumentParser(
        description="Setup an ecFlow workflow of tasks based on config " + \
        "files describing (1) ecFlow suite defintion and (2) environment")
    prsr.add_argument("suite_config_file", help="suite configuration file (yaml)")
    prsr.add_argument("env_config_file", help="environment config file (yaml)")
    prsr.add_argument(
        "time0",
        help="date/time (YYYYMMDD_HHz) of initial condition ",
        type=lambda d: datetime.strptime(d, "%Y%m%d_%Hz"))
    prsr.add_argument("n_sixhr_segs", help="number of 6-hr segments", type=int)
    prsr.add_argument("run_dir", help="root location of workflow area")
    return prsr.parse_args()


if __name__ == "__main__":
    main()
