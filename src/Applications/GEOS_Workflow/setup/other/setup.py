#!/usr/bin/env python
"""
Script to setup a workflow suite
"""

import os
import shutil


def setup_other(config_file_list, run_dir):
    """main program"""

    sanity_check(config_file_list, run_dir)

    two_levels_up = os.path.join(
        os.path.dirname(os.path.realpath(__file__)),
        "../..")

    print 'Creating dir for slurm output files...',
    os.makedirs(os.path.join(run_dir, 'batchout'))
    print 'done.'

    print 'Copying tools directory (runex.py, slurm/submit.sh etc.) to %s...' % run_dir,
    shutil.copytree(os.path.join(two_levels_up, 'tools'), os.path.join(run_dir, 'tools'))
    print 'done.'

    config_dir = os.path.join(run_dir, 'config')
    print 'Copy config files to %s' % config_dir,
    os.makedirs(config_dir)
    for config_file in config_file_list:
        shutil.copy(config_file, config_dir)
    print 'done.'

def sanity_check(config_file_list, run_dir):

    assert os.path.isdir(run_dir)
    for config_file in config_file_list:
        assert os.path.isfile(config_file)
