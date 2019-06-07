#!/usr/bin/env python

import os
import sys
import glob
import shutil
import argparse


def main(opddir, verbose):

    # opendap dir location
    assert os.path.isdir(opddir)
    opddir.rstrip('/')

    fcast_dir = opddir + '/fcast'
    assim_dir = opddir + '/assim'
    assert os.path.isdir(fcast_dir)
    assert os.path.isdir(assim_dir)

    # clean up seamless dir
    smls_dir = opddir + '/seamless'
    if os.path.isdir(smls_dir):
        olddirlist = os.listdir(smls_dir)
        for olddir in olddirlist:
            olddirpath = smls_dir+os.sep+olddir
            if os.path.isdir(olddirpath):
                shutil.rmtree(olddirpath)

    # get a list of collections from 'fcast/'
    latestfiles = glob.glob(fcast_dir + '/*.latest')
    collections = [x.split('/')[-1].replace('.latest','') for x in latestfiles]
    assert len(collections)>0
    if verbose:
        print collections

    # if a corresponding ctl file exists in the 'assim/'
    # dir, we need a seamless ctl file for this collection
    for clctnid in collections:

        # special treatment of inst1_2d_hwl_Nx
        # for now, we do not create seamless ctls for this clctn
        if clctnid=='inst1_2d_hwl_Nx':
            continue

        ctl_assim = assim_dir + '/' + clctnid

        # we are interested in those collections that
        # also appear in the assim directory
        if not os.path.isfile(ctl_assim):
            continue

        if verbose: 
            print '\nSeamless collection:', ctl_assim.split('/')[-1]

        # get assim dset and tdef
        tdef_assim = None
        ain = open(ctl_assim, 'r')
        for line in ain:
            if line.startswith('tdef'):
                tdef_assim = line.strip()
            if line.startswith('dset'):
                dset_assim = line.strip()
        ain.close()
        assert tdef_assim # shouldn't be None
        assert dset_assim
        nsteps_assim = int(tdef_assim.split()[1])
        if verbose:
            print tdef_assim

        # we want seamless ctls for all fcast ctls including latest
        ctl_fcast_all = glob.glob(fcast_dir + '/' + clctnid + '/*')
        ctl_fcast_all += [fcast_dir + '/' + clctnid + '.latest']

        for ctl_fcast in ctl_fcast_all:
            # store fcast ctl
            fin = open(ctl_fcast, 'r')
            ctl_fcast_lines = fin.readlines()
            fin.close()

            # get fcast dset and tdef
            tdef_fcast = None
            for line in ctl_fcast_lines:
                if line.startswith('tdef'):
                    tdef_fcast = line.strip()
                if line.startswith('dset'):
                    dset_fcast = line.strip()
            assert tdef_fcast
            assert dset_fcast
            if verbose:
                print tdef_fcast
            nsteps_fcast = int(tdef_fcast.split()[1])

            # for both assim and fcast tdefs, we expect
            # (a) delta t to be the same
            # (b) both are linear
            tdef_assim_splt = tdef_assim.split()
            tdef_fcast_splt = tdef_fcast.split()
            assert tdef_assim_splt[-1]==tdef_fcast_splt[-1]
            assert tdef_assim_splt[2]=='linear'
            assert tdef_fcast_splt[2]=='linear'

            # tdef for seamless ctl
            nsteps_total = nsteps_assim + nsteps_fcast
            tdef_seamless_splt = tdef_assim_splt
            tdef_seamless_splt[1] = str(nsteps_total)
            tdef_seamless = ' '.join(tdef_seamless_splt) + '\n'
            if verbose:
                print tdef_seamless.strip()

            # dset for seamless ctl
            dset_base = dset_fcast.split('/forecast/')[0] + '/%ch\n'
            ch_assim = 'das/' + dset_assim.split('/das/')[1]
            ch_fcast = 'forecast/' + dset_fcast.split('/forecast/')[1]
            chsub1 = 'CHSUB 1 %d %s\n' % (nsteps_assim, ch_assim)
            chsub2 = 'CHSUB %d %d %s\n' % (nsteps_assim+1, nsteps_total, ch_fcast)
            dset_seamless = dset_base + chsub1 + chsub2
            if verbose:
                print dset_seamless

            # write seamless ctl with necessary changes
            # to dset and tdef
            ctl_seamless = ctl_fcast.replace('fcast', 'seamless').rstrip('/')
            
            # create intermediate dir
            seamless_dir = os.path.dirname(ctl_seamless)
            assert seamless_dir # cannot be None
            if not os.path.isdir(seamless_dir):
                os.makedirs(seamless_dir)

            # then write seamless ctl
            fout = open(ctl_seamless, 'w')
            for line in ctl_fcast_lines:
                if line.startswith('dset'):
                    line = dset_seamless
                elif line.startswith('tdef'):
                    line = tdef_seamless
                else:
                    pass
                fout.write(line)
            fout.close()



def parse_args():
    p = argparse.ArgumentParser(
        description='Create a *seamless* stream where the ' \
            'assimilation fields are continued in the future using the ' \
            'forecast data for that extension'
        )

    p.add_argument('-v','--verbose', action="store_true", help='verbose mode')

    # location of opendap directory
    p.add_argument(
        'opddir', 
        help="location of 'opendap/' containing assim/, fcast/"
        )

    a = p.parse_args()

    return a


if __name__=='__main__':
    a = parse_args()
    main(a.opddir, a.verbose)
