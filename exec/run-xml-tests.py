#! /usr/bin/env python3

"""
Compare the GCAM xml files in olddir with their counterparts in newdir.  

Usage:  run-xml-tests.py [-v] olddir newdir

Discrepancies will be reported to stdout; all other messages (progress indicators,
etc.) will be written to stderr
"""

import sys
import os
import glob
import argparse
import textwrap

import xml_verify

def parse_args():
    """
    Parse command line arguments

    :return: parsed arguments object
    """

    parser = argparse.ArgumentParser(
        description = "Compare the GCAM xml files in olddir with their counterparts in newdir.",
        epilog = textwrap.dedent( 
        """
        The olddir will be searched recursively for xml files.  Currently newdir is
        *not* searched recursively (gcamdata currently generates all of its xml files in
        the top-level directory).  This might change in the future.

        Discrepancies will be reported to stdout; all other messages (progress indicators,
        etc.) will be written to stderr.
        """),
        formatter_class = argparse.RawDescriptionHelpFormatter
    )

    parser.add_argument('olddir', help='Directory containing the old GCAM xml files')
    parser.add_argument('newdir', help='Directory containing the new GCAM xml files')
    parser.add_argument('-v', action='store_true',
                        help='Verbose mode: add extra informational messages')

    return parser.parse_args()


if __name__ == "__main__":

    args = parse_args()

    oldfiles = glob.glob( os.path.join( os.path.expanduser(args.olddir), '**', '*.xml'), recursive=True )
    newdir = os.path.expanduser(args.newdir)

    misscount = 0
    statcount = 0

    for oldfile in oldfiles:
        newfile = os.path.join( newdir, os.path.basename(oldfile) )
        if not os.path.exists(newfile):
            sys.stdout.write('ERROR : File does not exist: {}\n'.format(newfile))
            misscount += 1
        else:
            stat = xml_verify.compare_files(oldfile, newfile, args.v) 
            if stat != 0:
                sys.stdout.write('ERROR: Discrepancy between files: {} and {}\n'.format(oldfile, newfile))
                statcount += 1

    if misscount == 1:
        fs = 'file'
    else:
        fs = 'files'
    if statcount == 1:
        ds = 'discrepancy'
    else:
        ds = 'discrepancies'

    nold = len(oldfiles)
    if nold == 1:
        fno = 'file'
    else:
        fno = 'files'

    sys.stdout.write('{} {} tested.\n'.format(nold, fno))
    sys.stdout.write('{} missing output {}.\n'.format(misscount, fs))
    sys.stdout.write('{} file {}.\n'.format(statcount, ds))

            
    
    
    
