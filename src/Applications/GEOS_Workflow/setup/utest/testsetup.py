#!/usr/bin/env python
"""Unit tests for ecfseteup.py"""

import os
import unittest
import shutil
import filecmp
import glob
from datetime import datetime
import subprocess as sp
import sys
sys.path.append(os.path.join(os.path.dirname(os.path.realpath(__file__)), ".."))
from suite import ecfsetup

class TestEcfSetup(unittest.TestCase):
    """
    TODO: add docstring
    """

    def setUp(self):
        self.utest_dir = os.path.dirname(os.path.realpath(__file__))
        self.workflow_test_dir = "./tmp-test-workflow-setup"
        if os.path.isdir(self.workflow_test_dir):
            shutil.rmtree(self.workflow_test_dir)
        os.makedirs(self.workflow_test_dir)

    def test_build_ecflow_suite(self):
        """
        Test building suite definition
        """
        config_file = os.path.join(self.utest_dir, "input/ex_ste.yaml")
        config = ecfsetup.read_config_file(config_file)
        time0 = datetime(year=2016, month=9, day=18, hour=21)
        stename, stedef = ecfsetup.build_suite(config, self.workflow_test_dir, time0, 2)
        stefile = os.path.join(self.workflow_test_dir, "%s.def" % stename)
        ecfsetup.write_suite(stedef, stefile)

        # Compare newly built definition with the saved one
        new_def = os.path.join(self.workflow_test_dir, "Central.def")
        saved_def = os.path.join(self.utest_dir, "output", "Central.def")
        failmsg = "%s != %s"%(new_def, saved_def)
        self.assertTrue(filecmp.cmp(new_def, saved_def, False), msg=failmsg)

    def tearDown(self):
        shutil.rmtree(self.workflow_test_dir)


class TestEnvSetup(unittest.TestCase):
    """
    TODO: add docstring
    """

    def setUp(self):
        self.utest_dir = os.path.dirname(os.path.realpath(__file__))
        self.env_dir = "./tmp-test-env-setup"
        if os.path.isdir(self.env_dir):
            shutil.rmtree(self.env_dir)
        os.makedirs(self.env_dir)

    def test_env_setup(self):
        """
        Test env setup script, envsetup.py
        """
        config_file = os.path.join(self.utest_dir, "input/ex_env.yaml")
        cmd = "%s/../env/setup.py %s %s" % (self.utest_dir, config_file, self.env_dir)
        sp.check_call(cmd.split())

        # Compare with saved env files
        saved_dir = os.path.join(self.utest_dir, "output", "env")
        for envfile in glob.glob(os.path.join(self.env_dir, "*_env.sh")):
            # print envfile
            file2 = os.path.join(saved_dir, os.path.basename(envfile))
            failmsg = "%s != %s"%(envfile, file2)
            self.assertTrue(filecmp.cmp(envfile, file2, False), msg=failmsg)

    def tearDown(self):
        shutil.rmtree(self.env_dir)


if __name__ == "__main__":
    unittest.main()
