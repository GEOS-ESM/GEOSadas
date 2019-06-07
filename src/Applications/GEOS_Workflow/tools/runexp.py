#!/usr/bin/env python2.7
"""Run an ecflow workflow"""

import argparse
import ecflow


def main():
    """Main program"""

    args = parse_args()
    myste = args.suite
    host_port = '%s:%s' % (args.host, args.port)

    try:
        client = ecflow.Client(host_port)
        client.ping()
        if myste in client.suites():
            client.delete(myste)         # clear older run
        client.load("%s.def" % myste)    # load definition into server
        client.begin_suite(myste)        # start the suite
    except RuntimeError as err:
        print "Failed:", err


def parse_args():
    """Parse command line arguments"""
    parser = argparse.ArgumentParser(
        description="Run suite specified on command line"
        )
    parser.add_argument("suite", help="suite to run")
    parser.add_argument("host", help="name of host (e.g. discoverXX)")
    parser.add_argument("port", help="port number")
    return parser.parse_args()


if __name__ == "__main__":
    main()
