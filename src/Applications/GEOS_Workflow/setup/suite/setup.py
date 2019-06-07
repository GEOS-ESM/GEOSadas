#!/usr/bin/env python
"""
Script to setup a workflow suite
"""

import os
import argparse
import shutil
from datetime import datetime
import ecfsetup


def setup_suite(config_file, time0, n_sixhr_segs, run_dir):
    """main program"""

    sanity_check(config_file, time0, n_sixhr_segs, run_dir)
    
    config = ecfsetup.read_config_file(config_file)
    two_levels_up = os.path.join(
        os.path.dirname(os.path.realpath(__file__)),
        '..',
        '..')

    print 'Copying [*.ecf] scripts to %s...' % run_dir,
    ecfdir = os.path.join(two_levels_up, 'ecf')
    ecfsetup.copy_ecf_scripts(config, ecfdir, run_dir)
    print 'done.'

    tasksdir = os.path.join(run_dir, 'tasks')
    print 'Copying [tasks/*.sh] scripts to %s...' % tasksdir,
    srcdir = os.path.join(two_levels_up, 'tasks')
    shutil.copytree(srcdir, tasksdir)
    print 'done.'

    incdir = os.path.join(run_dir, 'include')
    print 'Copying [include] files to %s...' % incdir,
    srcdir = os.path.join(two_levels_up, 'ecf', 'include')
    shutil.copytree(srcdir, incdir)
    print 'done.'

    print 'Building ecflow definition file...',
    stename, stedef = ecfsetup.build_suite(config, run_dir, time0, n_sixhr_segs)
    stefile = os.path.join(run_dir, '%s.def'%stename)
    ecfsetup.write_suite(stedef, stefile)
    print 'done.'


def sanity_check(config_file, time0, n_sixhr_segs, run_dir):
    """check input arguments"""
    assert os.path.isfile(config_file)
    assert time0.minute == 0
    assert time0.second == 0
    assert n_sixhr_segs > 0
    os.makedirs(run_dir)


def parse_args():
    """parse command line arguments"""
    prsr = argparse.ArgumentParser(
        description=
        'Setup a workflow suite based on a config file describing the suite')
    prsr.add_argument('suite_config_file', help='suite configuration file (yaml)')
    prsr.add_argument(
        'time0',
        help='date/time (YYYYMMDD_HHz) of initial condition',
        type=lambda d: datetime.strptime(d, '%Y%m%d_%Hz'))
    prsr.add_argument('n_sixhr_segs', help='number of 6-hr segments', type=int)
    prsr.add_argument('run_dir', help='root location of workflow area')
    return prsr.parse_args()


if __name__ == '__main__':
    argv = parse_args()
    setup_suite(
        argv.suite_config_file,
        argv.time0,
        argv.n_sixhr_segs,
        argv.run_dir)
