#ifndef BITVECTOR_HPP_
#define BITVECTOR_HPP_

/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/

#include <iostream>
#include <iomanip>
#include <string.h>

// table of pop counts for values 0-255.  This is a hacky way of
// doing it, but it avoids the problem of arranging to get the table
// defined before it is used.
// Ref: http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetTable
static const unsigned char PopCountTbl[256] = {
#define B2(n) n,        n+1,    n+1,      n+2
#define B4(n) B2(n), B2(n+1), B2(n+1), B2(n+2)
#define B6(n) B4(n), B4(n+1), B4(n+1), B4(n+2)
  B6(0), B6(1), B6(1), B6(2)
};
#undef B2
#undef B4
#undef B6

class bitvector;

//! struct for iterating over the nonzero (set) elements of a bit vector 
//! \details This structure is intended to be mostly opaque to the
//! user.  There are accessors for the elements that are not strictly
//! reserved for internal use.  The interface is somewhat simplified
//! relative to that of stl iterators.  Note that the iterator is
//! unidirectional
class bitvector_iterator {
  friend class bitvector; 
private:
  unsigned _dindex;
  unsigned _mask;
  unsigned _bindex;
  bool _good;
  const bitvector *_bv;

public:
  bitvector_iterator(const bitvector *bv) : _dindex(0), _mask((unsigned)-1), _bindex(0), _good(true), _bv(bv) {}
  void reset(void) {_dindex=0; _mask = unsigned(-1); _bindex=0; _good=true;}
  unsigned bindex(void) const {return _bindex;}
  bool end(void) const {return !_good;}
  bool next(void);
};

class bitvector {
  unsigned *data;
  unsigned dsize;                    // maximum index of data
  unsigned bsize;                    // size of the bit vector
  unsigned last_word_mask;           // used to zero out excess bits
                                     // in the last word (needed only
                                     // when you do an operation that
                                     // might mangle the excess bits,
                                     // like setdifference.)

  void setup(unsigned bs) {
    const int wdsize = 8*sizeof(unsigned);
    if(bs > 0) {
    bsize = bs;
    dsize = bsize / wdsize + 1;
    data = new unsigned[dsize];
    }
    else {
      data = 0;
      dsize = bsize = 0;
    }
    memset(data,0, dsize*sizeof(unsigned));

    int excess = bsize - wdsize*(dsize-1);
    last_word_mask = (unsigned) -1;
    last_word_mask >>= excess;
    last_word_mask <<= excess;
    last_word_mask = ~last_word_mask;
  }

  static void find(unsigned i, unsigned &idx, unsigned &mask) {
    unsigned bidx;
    idx = i / (8*sizeof(unsigned));
    bidx = i % (8*sizeof(unsigned));
    mask = (1 << bidx);
  }

  static unsigned popcount(unsigned x) {
    unsigned c=0;
    c = PopCountTbl[x & 0xff] + PopCountTbl[(x>>8) & 0xff]
      + PopCountTbl[(x>>16) & 0xff] + PopCountTbl[x>>24];
    return c;
  } 
  
public:
  bitvector() : data(0), dsize(0), bsize(0), last_word_mask(0) {} 
    
  bitvector(unsigned bs) {
    setup(bs);
  }

  bitvector(const bitvector &bv) {
    setup(bv.bsize);
    for(unsigned i=0; i<dsize; ++i)
      data[i] = bv.data[i];
  }
  ~bitvector() {delete [] data;}
  const bitvector &operator=(const bitvector &bv) {
    if(&bv == this)
      return *this;
    else {
      if(bv.bsize != bsize) {   // optimization for case where vectors are the same size
        delete [] data;
        setup(bv.bsize);
      }
      memcpy(data, bv.data, dsize*sizeof(unsigned));
    } 
    return *this;
  }

  void copyin(const bitvector &bv) {
    // like operator=, but the user warrants that the vector being
    // copied is the same size as the destination vector and is not
    // aliased to it.  This allows us to bypass some safety checks
    // from operator=.  DO NOT USE THIS FUNCTION UNLESS THE TWO
    // CONDITIONS ARE PROVABLY MET BY THE CALLING CODE.
    memcpy(data,bv.data, dsize*sizeof(unsigned));
  }

  //! get the size of the vector
  unsigned length(void) const {return bsize;}
  //! get the popcount of the vector
  unsigned count(void) const {
    unsigned pcount = 0;
    for(unsigned i=0; i<dsize; ++i)
      pcount += popcount(data[i]);
    return pcount;
  }
  //! empty flag
  bool empty(void) const {
    for(unsigned i=0; i<dsize; ++i)
      if(data[i])
        return false;
    return true;
  }
  //! predicate: is there > 1 set bit in the vector 
  //! \remark F.count()>1 crops up in the graph parser, and it turns
  //! out to be a significant contributor to run time.  This test
  //! avoids doing a popcount on every word in the vector
  bool gt1set(void) const {
    int nwordset = 0;
    int setword=0;
    for(unsigned i=0; i<dsize; ++i)
      if(data[i]) {
        ++nwordset;
        setword = i;
      }
    if(nwordset>1)
      return true;
    else if(nwordset == 0)
      return false;
    else if(popcount(data[setword]) > 1)
      return true;
    else
      return false;             // exactly one set.
  } 
  //! set a bit in the vector
  void set(unsigned i) {
    unsigned idx,mask;
    find(i,idx, mask);
    data[idx] |= mask;
  }
  //! clear a bit in the vector
  void clear(unsigned i) {
    unsigned idx,mask;
    find(i, idx, mask);
    data[idx] &= ~mask;
  }
  //! clear all bits
  void clearall(void) {
    memset(data,0, dsize*sizeof(unsigned));
  }
  //! set all bits
  void setall(void) {
    for(unsigned i=0; i<dsize; ++i)
      data[i] = (unsigned) -1;
    data[dsize-1] &= last_word_mask;
  }
  //! get a bit in the vector 
  //! \details returns zero if the bit is cleared, nonzero if it is
  //! set.  Which nonzero value you get depends on the position of the
  //! bit in its word.  Usually it's irrelevant
  unsigned get(unsigned i) const {
    unsigned idx, mask;
    find(i,idx,mask);
    return data[idx] & mask;
  }

  // The following operators implement set operations using the
  // bitvectors.  I have decided not to use operator overloads, since
  // they would slightly abuse the definitions of '+', '*', and
  // especially '-'.

  //! Set union, in place 
  //! \warning We do not check for compatible sizes between the two
  //! vectors.  That's the caller's responsibility.
  const bitvector &setunion(const bitvector &bv) {
    for(unsigned i=0; i<dsize; ++i)
      data[i] |= bv.data[i];
    return *this;
  }

  //! Set intersection, in place
  //! \warning We do not check for compatible sizes between the two
  //! vectors.  That's the caller's responsibility.
  const bitvector &setintersection(const bitvector &bv) {
    for(unsigned i=0; i<dsize; ++i)
      data[i] &= bv.data[i];
    return *this;
  }

  //! Set difference, in place 
  //! \details The set difference B-A is the set of elements in B, but
  //! not A (B intersect A-complement).
  //! \warning We do not check for compatible sizes between the two
  //! vectors.  That's the caller's responsibility.
  const bitvector &setdifference(const bitvector &bv) {
    for(unsigned i=0; i<dsize; ++i)
      data[i] &= ~bv.data[i];
    data[dsize-1] &= last_word_mask;
    return *this;
  }

  //! Test whether this set is a subset of another set 
  //! \details You could do this with a set difference.  A is a subset
  //!          of B if A - B is empty.  However, this function does it
  //!          without modifying this set or making a copy.
  //! \warning As always, we don't check for length compatibility
  bool subset(const bitvector &bv) const {
    for(unsigned i=0; i<dsize-1; ++i)
      if (data[i] & ~bv.data[i])
        // something left over when all members of B removed => not a subset of B
        return false;
    
    // do the last element specially, since it needs the last word mask
    if (data[dsize-1] & ~bv.data[dsize-1] & last_word_mask)
      return false;
    else
      return true;
  }

  //! Equality comparison
  bool operator==(const bitvector &bv) const {
    for(unsigned i=0; i<dsize; ++i)
      if(data[i] != bv.data[i])
        return false;
    return true;
  }
  //! Inequality
  bool operator!=(const bitvector &bv) const {return !operator==(bv);}
  //! Less than comparison (suitable for sorting)
  bool operator<(const bitvector &bv) const {
    for(unsigned i=0; i<dsize; ++i)
      if(data[i] < bv.data[i])
        return true;
      else if(data[i] > bv.data[i])
        return false;
    // if we made it this far, they're equal
    return false;
  }
  //! iterate over a bit vector, picking up only the bits that are set
  void iterate(bitvector_iterator *bvit) const;
  //! print
  void prn(std::ostream &o) const {
    o << "[" << std::hex << std::setfill('0');
    for(unsigned i=dsize-1;i>0; --i)
      o << std::setw(8) << data[i] << " ";
    o << std::setw(8) << data[0] << std::dec << "](" << bsize << "," << count() << ")";
  }
};

//! Set intersection, with temporary
//! \warning We do not check for compatible sizes between the two
//! vectors.  That's the caller's responsibility.
inline bitvector setintersection (const bitvector &av, const bitvector &bv) {
  bitvector temp(av);
  return temp.setintersection(bv);
}

//! Set union, with a temporary
//! \warning We do not check for compatible sizes between the two
//! vectors.  That's the caller's responsibility.
inline bitvector setunion(const bitvector & av, const bitvector &bv) {
  bitvector temp(av);
  return temp.setunion(bv);
}

//! Set difference, with temporary
//! \warning We do not check for compatible sizes between the two
//! vectors.  That's the caller's responsibility.
inline bitvector setdifference(const bitvector &av, const bitvector &bv) {
  bitvector temp(av);
  return temp.setdifference(bv);
}

inline std::ostream &operator<<(std::ostream &o, const bitvector &bv)
{
  bv.prn(o);
  return o;
}

inline void bitvector::iterate(bitvector_iterator *bvit) const
{
  /* This algorithm is based on the one found at:
     http://graphics.stanford.edu/~seander/bithacks.html#ZerosOnRightParallel
  */
  // manually unroll the first iteration of the loop over d-index,
  // since it is a special case
  unsigned idx = bvit->_dindex;
  unsigned d = data[idx] & bvit->_mask;
  if(d) {
    unsigned c=31;
    d &= -(signed) d;
    if(d & 0x0000ffff) c -= 16;
    if(d & 0x00ff00ff) c -= 8;
    if(d & 0x0f0f0f0f) c -= 4;
    if(d & 0x33333333) c -= 2;
    if(d & 0x55555555) c -= 1;

    unsigned mask = ((unsigned)-1) << c;
    bvit->_mask = mask<<1;
    bvit->_bindex = idx*8*sizeof(unsigned) + c;
    return;
  }

  // the previous word was tapped out, so iterate over the remaining ones
  for(++idx; idx<dsize; ++idx) {
    d = data[idx];
    if(d) {
      unsigned c=31;
      d &= -(signed) d;
      if(d & 0x0000ffff) c -= 16;
      if(d & 0x00ff00ff) c -= 8;
      if(d & 0x0f0f0f0f) c -= 4;
      if(d & 0x33333333) c -= 2;
      if(d & 0x55555555) c -= 1;

      bvit->_dindex = idx;
      unsigned mask = ((unsigned)-1) << c;
      bvit->_mask = mask<<1;
      bvit->_bindex = idx*8*sizeof(unsigned) + c;
      return;
    }
  }

  // if we get to here, then there are no more set bits.
  bvit->_good = false;
}

inline bool bitvector_iterator::next(void) {
  _bv->iterate(this);
  return _good;
}


#endif
