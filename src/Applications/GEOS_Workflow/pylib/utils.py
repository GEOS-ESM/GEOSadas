"""Utilities for setting up ecflow workflows"""

import os
import shutil
from collections import OrderedDict
import yaml

def cpfile(filename, srcdir, dstdir):
    """
    Copy file filename from srcdir to dstdir
    Create dstdir if needed
    """

    if not os.path.exists(dstdir):
        os.makedirs(dstdir)
    shutil.copy(os.path.join(srcdir, filename), os.path.join(dstdir, filename))

def unicode2ascii(dct):
    """Convert, in line, unicode dict/OrderedDict dct to ascii"""

    assert isinstance(dct, (dict, OrderedDict))
    for unicode_key, val in dct.items():
        ascii_key = unicode_key.encode("ascii")
        dct[ascii_key] = dct.pop(unicode_key)
        if isinstance(val, (dict, OrderedDict)):
            unicode2ascii(val) # recurse
        else:
            if isinstance(val, unicode):
                dct[ascii_key] = val.encode("ascii")
            else:
                dct[ascii_key] = val

def ordered_load(stream, Loader=yaml.Loader, object_pairs_hook=OrderedDict):
    """
    Load yaml mappings as OrderedDict
    Taken from: https://stackoverflow.com/a/21912744
    """
    class OrderedLoader(Loader):
        pass
    def construct_mapping(loader, node):
        loader.flatten_mapping(node)
        return object_pairs_hook(loader.construct_pairs(node))
    OrderedLoader.add_constructor(
        yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG,
        construct_mapping)
    return yaml.load(stream, OrderedLoader)

def yaml2dict(yaml_file):
    """
    Read a yaml file into a dict
    """
    with open(yaml_file, "r") as fin:
        config_dict = ordered_load(fin)
    return config_dict
