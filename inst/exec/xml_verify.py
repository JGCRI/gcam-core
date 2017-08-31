#! /usr/bin/env python3

"""Compare two GCAM xml files for consistency

The files are parsed into xml trees.  At each level the child nodes
are sorted into a canonical ordering before comparing, so changes in
the order of elements will not be flagged as differences.

"""

import xml.etree.ElementTree as ET
from sys import stdout

## We will compare GCAM numeric values to 5 significant digits.  This
## should be sufficient accuracy for just about any quantities we use
## in GCAM.  Notably, it ensures that years (4 significant figures)
## will not get rounded.
comparison_digits = 3

def signif(x, digits=6):
    """Round a numeric to the specified number of significant digits.

    Examples:

    signif(12345000, 3) == 12300000
    signif(1.2345, 3) == 1.23
    signif(0.0012345, 3) == 0.00123

    """

    from math import log10, floor

    ## Using the conventions of the round function, the first
    ## significant figure is at round(x, -exponent).  Each additional
    ## significant figure adds one to the rounding position.

    if digits <= 0:
        raise ValueError('signif: digits argument must be positive')

    try:
        exponent = int(floor(log10(abs(x))))
    except ValueError:
        ## If this happens, it means the input value was zero
        return 0.0
    
    return round(x, -exponent + (digits-1))


def strnormalize(s):
    """Convert input string into canonical form.

    For nonnumeric strings this means stripping leading and trailing
    whitespace.  For numeric strings, convert to a number and round to
    a specified number of significant digits (except integers, which
    are reported as-is), then convert back to string.

    """ 

    from math import floor
    
    try:
        x = float(s)            # strips whitespace if it succeeds
        if x == floor(x):
            ## integer: return as-is
            return str(int(x))
        else:
            ## floating point:  round to appropriate significant figures.
            return str(signif(x,comparison_digits))
    except ValueError:
        ## The string isn't a number, so strip leading and trailing
        ## whitespace and return the rest as-is
        return s.strip()
    

def eltsortkey(elt):

    """Construct a sort key for an XML Etree element.

    The sort criteria are:
      * tag
      * name attribute 
      * list of all attributes in lexical order
      * list of attribute values ordered by attribute name
      * the element's text, stripped of leading and trailing whitespace
        ** If an element has no text, record an empty string for this criterion
      * the element's tail, stripped of leading and trailing whitespace
        ** If an element has no tail, record an empty string for this criterion

    Elements that match on all of these criteria are considered equal.
    In particular, number, name, and content of child nodes are not
    considered in the sort order.
    """

    sortkey = []

    sortkey.append(elt.tag)

    sortkey.append(elt.get('name', default=''))

    attribs = sorted(elt.keys())
    sortkey.append(attribs)

    vals = [elt.get(attrib) for attrib in attribs]
    sortkey.append(vals)

    text = elt.text
    if text is None:
        sortkey.append('')
    else:
        sortkey.append(strnormalize(text))

    tail = elt.tail
    if tail is None:
        sortkey.append('')
    else:
        sortkey.append(strnormalize(tail))

    return sortkey

def eltdiff(left, right, path=None, outstream=stdout):
    """Diff two XML Etree elements recursively.
       
    Two elements differ if:
      * the elements have different sort keys
      * the elements have a different number of children
      * any of the children differ, after being ordered canonically

    The elements 'left' and 'right' are compared for differences.  The
    argument 'path' is used in reporting differences, to indicate
    where in the tree the difference was found.  If we find a
    difference, we stop reporting on this branch because comparing the
    children of two nodes that are different is probably not
    meaningful.

    The function will return 0 (no differences found in this branch),
    or 1 (differences found in this node or children)

    This comparison is a little naive.  Ideally we should be looking
    for additions and deletions, but that's a bit more work, and it
    isn't really necessary for what we are trying to accomplish here,
    which is to validate old and new files as equivalent and to give
    some help in diagnosing failed validations.

    """

    if path is None:
        ## Start an empty path.
        path = []

        
    lkey = eltsortkey(left)
    rkey = eltsortkey(right)

    if lkey != rkey:
        report_difference(left, right, path, outstream)
        return 1

    if len(left) != len(right):
        ## len(element) returns the number of children
        report_child_mismatch(left, right, path, outstream)
        return 1

    lchildren = sorted(list(left), key=eltsortkey)
    rchildren = sorted(list(right), key=eltsortkey)

    stat = 0
    path.append( (left.tag, left.get('name', default='')) )
    for lchild,rchild in zip(lchildren, rchildren):
        cstat = eltdiff(lchild, rchild, path, outstream)
        if cstat == 1:
            stat = 1

    del path[-1]          # remove this node from the end of the path.
    return stat


def report_difference(left, right, path, outstream):
    """Report two nodes that differ to the supplied output stream."""

    outstream.write('At: {}\n'.format(str(path)))
    outstream.write('\tLeft:\n')
    for row in eltsortkey(left):
        outstream.write('\t\t{}\n'.format(str(row)))
    outstream.write('\tRight:\n')
    for row in eltsortkey(right):
        outstream.write('\t\t{}\n'.format(str(row)))
    outstream.write('\n')

def report_child_mismatch(left, right, path, outstream):
    """Report two nodes that have an unequal number of children to the output stream."""

    outstream.write('At: {}\n'.format(str(path)))
    outstream.write('\tnode: {}  name= {}\n'.format(left.tag, left.get('name', default='')))
    outstream.write('\tLeft:  {} child nodes\n'.format(len(left)))
    outstream.write('\tRight: {} child nodes\n'.format(len(right)))
    outstream.write('\n')

        
if __name__ == "__main__":
    from sys import argv, stderr, exit

    if len(argv) != 3:
        stderr.write('Usage: {} file1 file2'.format(argv[0]))
        exit(2)

    ## Parse and compare the files.
    left = ET.parse(argv[1]).getroot()
    right = ET.parse(argv[2]).getroot()

    stat = eltdiff(left, right)

    if stat==0:
        exit(0)
    else:
        ## Return code will be 1 if it isn't 0, but python exits with
        ## code 1 when it terminates on an exception, so pick a
        ## different exit code so we can distinguish between finding
        ## differences and failing with an exception.
        exit(3)
