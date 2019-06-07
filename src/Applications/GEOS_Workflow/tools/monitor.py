#!/usr/bin/env python
"""Script to monitor runs managed by ecflow"""

import argparse
import ecflow


def _get_colored_text(txt, color):
    beg = "\x1b[%dm" % (90+color)
    end = "\x1b[0m"
    return beg+txt+end # len(beg+end) = 9


def _get_status_in_color(status):
    black, red, green, yellow, blue, magenta, cyan = range(7)
    status_color_map = {
        "aborted": red,
        "active": green,
        "complete": black,
        "queued": blue,
        "submitted": cyan,
        "suspended": magenta,
        "unknown": yellow
    }
    return _get_colored_text(status, status_color_map[status])


class Monitor(object):
    """Encapsulating routines to monitor ecflow jobs"""

    __slots__ = ["_ci", "_defs", "_suites2monitor"]

    def __init__(self, host, port, specified_suites=None):
        self._ci = Monitor._get_ecflow_client(host, port)
        self._ci.sync_local()
        self._defs = self._get_definitions()
        self._suites2monitor = self._get_suites_to_monitor(specified_suites)

    @staticmethod
    def _get_ecflow_client(host, port):
        try:
            client = ecflow.Client(host, port)
            client.ping()
        except RuntimeError as err:
            print str(err)
            raise
        return client

    def _get_definitions(self):
        defs = self._ci.get_defs()
        if defs is None:
            raise RuntimeError("No ecflow definitions on server")
        return defs

    def _get_suites_to_monitor(self, specified_suites):
        all_suites = self._ci.suites()
        if specified_suites is None:
            return all_suites
        if not isinstance(specified_suites, list):
            raise ValueError("specified_suites is not a list")
        for ste in specified_suites:
            if ste not in all_suites:
                raise ValueError("Unknown suite: %s" % ste)
        return specified_suites

    @staticmethod
    def _get_node_repeat(node):
        repeat = None
        rpt = node.get_repeat()
        rptstr = "%s" % rpt
        rptndx = rpt.value()
        if not rpt.empty():
            repeat = "[REPEAT] %s: %s" % (rpt.name(), rptstr.split()[3+rptndx])
        return repeat

    @staticmethod
    def _get_node_details(node):
        split_path = node.get_abs_node_path().split("/")
        depth = len(split_path) - 2
        name = split_path[-1]
        status = _get_status_in_color(node.get_state().name)
        repeat = Monitor._get_node_repeat(node)
        if not repeat:
            repeat = ""
        return depth, name, status, repeat

    def _group_nodes_by_suites(self):
        """
        Return a dict, dct, such that
          dct[suite_name] = suite_nodes
        """
        dct = dict()
        all_nodes = self._defs.get_all_nodes()
        nodectr = 0
        while nodectr < len(all_nodes):
            if isinstance(all_nodes[nodectr], ecflow.ecflow.Suite):
                suite_node = all_nodes[nodectr]
                dct[suite_node.name()] = suite_node.get_all_nodes()
                num_suite_nodes = len(suite_node.get_all_nodes())
                nodectr += num_suite_nodes
        return dct

    def monitor(self):
        """Public method for monitoring"""
        nodes_by_suite = self._group_nodes_by_suites()
        for suite in nodes_by_suite:
            if suite not in self._suites2monitor: continue
            for my_node in nodes_by_suite[suite]:
                depth, name, status, repeat = Monitor._get_node_details(my_node)
                fmtdepth = "|    "*depth
                # extra 9 characters in status due to coloring
                print " [%-18s]    %s%s %s" % (status.center(18), fmtdepth, name, repeat)
            print


def parse_args():
    """parse command line arguments"""
    prsr = argparse.ArgumentParser(description="Utility to monitor ecflow suites")
    prsr.add_argument("--port", default="3141", help="default: 3141")
    prsr.add_argument("--host", default="localhost", help="default: localhost")
    prsr.add_argument("--suites", nargs="+", default=None, help="suites to monitor")
    return prsr.parse_args()


def main():
    """main program"""
    argv = parse_args()
    monitor = Monitor(argv.host, argv.port, argv.suites)
    monitor.monitor()


if __name__ == "__main__":
    main()
