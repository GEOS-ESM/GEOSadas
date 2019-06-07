#!/usr/bin/env python
"""
Script to setup environment
"""

import os
import argparse
import shutil
from datetime import datetime
from pylib import utils


class EnvSetup(object):
    """Setup env files <env-name>.sh"""

    __slots__ = ["config_dict", "env_dir"]

    def __init__(self, config_file, run_dir):
        self.config_dict = utils.yaml2dict(config_file)
        self.env_dir = os.path.join(run_dir, "env")
        os.makedirs(self.env_dir)
        
    @staticmethod
    def _write_commands(cmd_list, out_file_handle):
        if not isinstance(cmd_list, list): # cmd_list should be a list
            raise ValueError("cmd_list [%s] is NOT a list" % cmd_list)
        for cmd in cmd_list:
            out_file_handle.write("%s\n" % cmd)

    def _write_env_file(self, env_name, env_file):
        with open(env_file, "w") as fout:
            for key, val in self.config_dict[env_name].items():
                if key == "COMMAND": # val: list of commands
                    cpfout = fout
                    EnvSetup._write_commands(val, cpfout)
                else: # environment variable
                    fout.write("export %s=%s\n" % (key, val))

    def create(self):
        """
        for each key in config_dict, write the file key.sh
        in self.env_dir that can be sourced at run time
        """
        for env_name in self.config_dict:
            env_file = os.path.join(self.env_dir, "%s.sh" % env_name)
            self._write_env_file(env_name, env_file)


def setup_env(config_file, run_dir):
    """main program"""

    sanity_check(config_file, run_dir)
    two_levels_up = os.path.join(
        os.path.dirname(os.path.realpath(__file__)),
        "../..")
    
    print "Setting up environment in %s..." % run_dir,
    env = EnvSetup(config_file, run_dir)
    env.create()
    print "done."
    
    # TODO: This is a kludgy solution - it would be better to copy
    # ../pylib to scripts/pylib and set PYTHONPATH to scripts/pylib
    print "KLUDGE: Copy [pylib] to scripts/tasks/utils/env...",
    utils_dir = os.path.join(run_dir, "pylib")
    shutil.copytree(os.path.join(two_levels_up, "pylib"), utils_dir)
    print "done."


def sanity_check(config_file, run_dir):
    """check input arguments"""
    assert os.path.isfile(config_file)
    assert os.path.isdir(run_dir)  # run_dir should exist


def parse_args():
    """parse command line arguments"""
    prsr = argparse.ArgumentParser(
        description=
        "Setup environement for a workflow based on config an env config file ")
    prsr.add_argument("env_config_file", help="environment config file (yaml)")
    prsr.add_argument("run_dir", help="root location of workflow area")
    return prsr.parse_args()


if __name__ == "__main__":
    argv = parse_args()
    setup_env(argv.env_config_file, argv.run_dir)
