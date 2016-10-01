#ifndef CLANID_HPP_
#define CLANID_HPP_

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

#include <set>
#include "parallel/include/digraph.hpp"
#include "parallel/include/bitvector.hpp"
#include "parallel/include/util.hpp"
#include <assert.h>

enum clan_type {unknown, linear, independent, primitive, pseudoindependent, serialized};

//! Node id type for a clan tree parsed out of a DAG. 
//! \tparam nodeid_t The type of the identifier for graph nodes (i.e.,
//! the nodes in the graph that we are parsing into clans). 
//! \details Technically, a tree is just a type of DAG, so we should
//! be able to represent the clan tree using the digraph class.
//! However, the each node in the clan tree needs to be able to keep
//! track of the order of its children, so we need some chicanery to
//! ensure that the children of each node are kept in topological
//! order.  We arrange this by creating a nodeid type for clans that
//! includes not just the name (nodeset) of the clan, but also a link
//! to the original graph.  We override operator< to ensure
//! topological ordering amongst the children ofe each clan.

template <class nodeid_t>
class clanid {
  const digraph<nodeid_t> *G;   //!< The graph from which the nodes were taken
  bitvector nodeset;            //!< nodes in this clan
  int top_index;                //!< Minimum topological index over the clan's nodes

public:
  mutable enum clan_type type;  //!< The classification of the clan
  // Constructors
  clanid() : G(0) {}
  clanid(const bitvector &n, const digraph<nodeid_t> *gin,
         enum clan_type tp = unknown) : G(gin), nodeset(n), type(tp) {
    if(!G->topology_valid())
      G->topological_sort();
    top_index = G->nodelist().size()+1; // larger than the largest valid index
    bitvector_iterator nodeit(&nodeset);
    while(nodeit.next()) {
      // the index of nodes in the bitset is the topological index,
      // which is exactly what we're after here.
      int i = nodeit.bindex();
      if(i<top_index)
        top_index = i;
    }
  }
  clanid(const nodeid_t &n, const digraph<nodeid_t> *gin,
         enum clan_type tp = unknown) : G(gin), nodeset(gin->nodelist().size()),
                                        type(tp) {
    if(!G->topology_valid())
      G->topological_sort();
    int indx = G->topological_index(n);
    nodeset.set(indx);
    top_index = indx;
  }

  // Accessors.
  const digraph<nodeid_t> *graph(void) const {return G;}
  const bitvector &nodes(void) const {return nodeset;}
  // Find sources and sinks for the clan subgraph
  bitvector clan_sources(void) const {bitvector sources(nodeset.length()); G->find_all_sources(nodeset, sources); return sources;}
  bitvector clan_sinks(void) const {bitvector sinks(nodeset.length()); G->find_all_sinks(nodeset, sinks); return sinks;}
  // return member nodes as an STL vector or set
  const std::vector<nodeid_t> &get_member_nodes(std::vector<nodeid_t> &members) const;
  //void get_member_nodes(std::set<nodeid_t> &members) const;
  int topological_index(void) const {return top_index;}

  //! Order operator for a clanid.
  //! \details Clan A < B if any node in A is an ancestor in G of a
  //! node in B.  Under the definition of a clan, if a node outside
  //! the clan is an ancestor of any node in the clan, then it is an
  //! ancestor of all nodes in the clan, so it should be sufficient to
  //! test one node.  However, because we don't actually complete the
  //! subgraph in a primitive reduction, we are only guaranteed that
  //! such a node is an ancestor of one of the source nodes.  We break
  //! ties (sets where neither set is an ancestor of the other) using
  //! the nodeset's order relation.  \remark By the time we get to
  //! building the clan tree, there should be no partially overlapping
  //! clans.  That is, for any two clans, either A subset B, B subset
  //! A, or A and B are disjoint.
  bool operator<(const clanid &B) const;

  //! Equality operator.  A == B if they have the same nodes in the same graph.
  //! \remark Note that under this definition, you can have !(A<B) && !(B<A) && !(A==B).
  bool operator==(const clanid &B) const {return G==B.G && nodeset==B.nodeset;}
  //! Inequality operator, in case we need it
  bool operator!=(const clanid &B) const {return !operator==(B);}
};


//! Get a vector of member nodes
//! \remark Ideally we would want to templatize the container so that
//!         we could get whatever kind of container we want.  That
//!         turns out to be harder than it should be because some of
//!         the STL containers have two template parameters, and some
//!         have three.  For now, we'll just stick with the vector
//!         type, which is cheap to create and cheap to iterate over.
//!         We don't use this very often anyhow.
template<class nodeid_t>
const std::vector<nodeid_t> &clanid<nodeid_t>::get_member_nodes(std::vector<nodeid_t> &members) const
{
  int nmember = nodeset.count();
  members.resize(nmember);
  bitvector_iterator memberit(&nodeset);
  for(int i=0;i<nmember;++i) {
    memberit.next();            // iterator starts not pointing to any element.
    int indx = memberit.bindex();
    members[i] = G->topological_lookup(indx);
  }
  return members;
}


template <class nodeid_t>
bool clanid<nodeid_t>::operator<(const clanid &B) const
{
  
  // If one set is a subset of the other, place the larger set first
  if(B.nodeset.count() < nodeset.count() && B.nodeset.subset(nodeset))
    return true;
  else if(nodeset.subset(B.nodeset))
    return false;

  // If neither node is a subset of the other, then you want to look
  // at the topological ordering of the clans.

  // Any two clans that we are comparing with this test should be
  // disjoint (as a result of the way the parsing algorithm unfolds),
  // unless one was a subset (in which case we didn't get to this
  // point).  Test that here.
  assert(setintersection(nodeset,B.nodeset).empty());
  // Also, G and B.G had better be the same
  assert(G == B.G);

  // Whichever clan comes first in the topological ordering is "less
  // than" the other.
  return top_index < B.top_index;
}

//! Adjust the types of child clans in a clan tree 
//! \details When we add a primitive clan to a tree it will sometimes
//! be relabeled as a linear clan.  This happens *after* the tree has
//! been built, with the result that the type field in the clanids in
//! the successors member of the parent clans will not match the
//! revised type of the clan as reflected in the allnodes member of
//! the graph.  This function updates all clanid type fields in the
//! successors and backlinks fields. 
//! \remark We can make the argument of this function const because
//! the only fields we will be updating are mutable fields.
template <class nodeid_t>
void canonicalize(const digraph<clanid<nodeid_t> > &clantree)
{
  for(typename digraph<clanid<nodeid_t> >::nodelist_c_iter_t clanit =
        clantree.nodelist().begin();
      clanit != clantree.nodelist().end(); ++clanit) {
    // for each clan in the tree:
    for(typename std::set<clanid<nodeid_t> >::const_iterator child_clan_it =
          clanit->second.successors.begin();
        child_clan_it != clanit->second.successors.end(); ++child_clan_it) {
      // for each child clan of the first clan:
      // find the corresponding node in the clan tree
      const typename digraph<clanid<nodeid_t> >::nodelist_c_iter_t childnode =
        clantree.nodelist().find(*child_clan_it);
      assert(childnode != clantree.nodelist().end());
      // adjust the type recorded in the child's entry to match its
      // type in the tree's master record.
      child_clan_it->type = childnode->first.type;
    }
  }
}

/* string representations of clan types */
const char *ctypestr[6] = {"unknown", "LI", "IN", "PR", "PI", "SE"}; 


#endif
