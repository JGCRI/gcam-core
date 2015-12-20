#ifndef GRAIN_COLLECT_HPP_
#define GRAIN_COLLECT_HPP_

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

#include "parallel/include/digraph.hpp"
#include "parallel/include/clanid.hpp"
#include <sstream>

template<class T> T* unique_nodetitle(T* bestnode, size_t setsize)
{
  /* The intended use of this specialization is for a pointer to a
     type (which will presumably be used to do something in the flow
     graph).  Because we know that valid pointers will have to be
     aligned on some reasonable boundary, we can generate
     "interstitial" pointers that will not point to valid nodes.
     Since we presumably won't be following the pointers that point to
     rolled-up nodes, it should be safe to use these as names for
     roll-up nodes.

     WARNING: What we are doing here is wrong, wrong, wrong.  The name
     we generate isn't actually guaranteed to be unique (though in
     practical terms it probably will be), and you'll be in a world of
     hurt if you try to dereference this fake pointer.  Use this at
     your own risk, if you are too lazy to define a type to wrap
     around your pointer so you can use the basic template.
  */
  unsigned long fakep = (unsigned long) bestnode;
  return (T*) (fakep+1);
}

std::string unique_nodetitle(const std::string &bestnode, size_t setsize)
{
  /*
     Form a name of the following form:
     <first node>_<size>
  */
  std::ostringstream title;

  title << bestnode << "_" << setsize;
  return title.str();
} 

template<class nodeid_t>
nodeid_t unique_nodetitle(const nodeid_t &bestnode, size_t setsize)
{
  return bestnode.gen_unique(setsize);
}


template<class nodeid_t>
nodeid_t grain_title(const std::set<nodeid_t> &nodeset, const digraph<nodeid_t> &topology)
{
  /* Find the topologically senior node in the nodeset.  Form a unique
     name based on that node's name
  */
  nodeid_t bestnode = *nodeset.begin();
  int bestidx = topology.topological_index(bestnode);

  for(typename std::set<nodeid_t>::const_iterator nodeit=nodeset.begin();
      nodeit != nodeset.end(); ++nodeit) {
    nodeid_t nextnode = *nodeit;
    int nextidx = topology.topological_index(nextnode);
    if(nextidx < bestidx) {
      bestnode = nextnode;
      bestidx = nextidx;
    }
  }

  return unique_nodetitle(bestnode, nodeset.size());
}


template<class nodeid_t>
void grain_collect(const digraph<clanid<nodeid_t> > &ClanTree,
                   const typename digraph<clanid<nodeid_t> >::nodelist_c_iter_t &claniterator,
                   digraph <nodeid_t> &GrainGraph,
                   unsigned grain_min)
{
  // define the clanid type
  typedef clanid<nodeid_t> Clanid;
  // get the topology from the clan struct
  const digraph<nodeid_t> &topology = *claniterator->first.graph();
  // name string for newly formed grains.  The name we initialize it
  // to is not the real name; that will be set later, but it's handy
  // when debugging to know what clan we are dealing with.
  nodeid_t grain_name = grain_title(claniterator->first.nodes(), topology);

  
  std::set<nodeid_t> node_group; // list of nodes to roll up
  // Threshold for splitting the "leftover" nodes of an independent
  // clan.  We fudge a little bit on the minimum size here to get some
  // extra parallelism.  The minimum was probably just a guess anyhow.
  unsigned int ind_split_min = 3*grain_min/2;

  switch(claniterator->first.type) {
    // our procedure here depends on whether the clan is independent or linear
  case independent:
  case pseudoindependent:

    // Large subclans get subdivided recursively.  Small ones get
    // aggregated into a grain of their own.  We don't have to worry
    // about ordering, so long as the nodes within each grain are
    // executed in topological order because each subclan is
    // independent of the others (and we always keep subclans
    // together).

    {
    for(typename std::set<Clanid>::const_iterator subclan = claniterator->second.successors.begin();
        subclan != claniterator->second.successors.end(); ++subclan) {
      unsigned nsub = subclan->nodes().size();
      // search large subclans for grains
      if(nsub >= grain_min)
        grain_collect(ClanTree, ClanTree.nodelist().find(*subclan), GrainGraph, grain_min);
      else
        node_group.insert(subclan->nodes().begin(), subclan->nodes().end());
    }

    // We've got this pool of small independent clans left.  If there
    // are enough of them it might be worth making another pass
    // through the list to try to break it up.  This is hard to do
    // exactly, since we don't know the distribution of the sizes of
    // the leftover clans.  We'll guess that they're pretty uniform
    // and build heuristics around that.
    int nbreakup = node_group.size() / grain_min;
    if(nbreakup < 2 && node_group.size() >= ind_split_min )
      // fudge the minimum grain size a little for extra parallelism.
      // It was probably just a guess anyhow.
      nbreakup = 2;

    if(nbreakup > 1) {
      // this will be the approximate size of the new grains we will make.
      unsigned grain_size_thresh = node_group.size() / nbreakup;
      node_group.clear();
      for(typename std::set<Clanid>::const_iterator subclan = claniterator->second.successors.begin();
          subclan != claniterator->second.successors.end(); ++subclan)
        if(subclan->nodes().size() < grain_min) { // skip the ones that were already processed above
          node_group.insert(subclan->nodes().begin(), subclan->nodes().end());
          if(node_group.size() >= grain_size_thresh) {
            // have enough for a grain
            grain_name = grain_title(node_group, topology);
            GrainGraph.collapse_subgraph(node_group, grain_name);
            node_group.clear();   // start the next grain
          }
        }
    }
    
    if(!node_group.empty()) {
      // leftover nodes from the iteration above form the last grain
      grain_name = grain_title(node_group, topology);
      GrainGraph.collapse_subgraph(node_group, grain_name);
    }
    // all of the nodes in this clan have been assigned to grains, so we're done.
    break;
    } // end of independent case

  case primitive:
    // This one is easy.  There is nothing to break down, so we just
    // make a grain out of the whole clan.  Note that we can only get
    // here if we set the threshold for decomposing primitive clans
    // larger than the minimum clan size.  That shouldn't really
    // happen.
    GrainGraph.collapse_subgraph(claniterator->first.nodes(), grain_name);
    break;

  case linear:
    // In a linear clan, it only makes sense to split the clan up at
    // all if there is a possibility that one of the independent
    // subclans might be splittable.  If we do recurse on one of the
    // subclans, we need to make sure that all of the nodes before the
    // subclan go in a different grain from the ones after (otherwise
    // we might get loops in the reduced graph).
    {
    for(typename std::set<Clanid>::const_iterator subclan = claniterator->second.successors.begin();
        subclan != claniterator->second.successors.end(); ++subclan) {
      if( (subclan->type == independent || subclan->type == pseudoindependent) &&
          subclan->nodes().size() >= ind_split_min ) {
        // only recurse on independent clans that are guaranteed to
        // split (an independent could split with as few as
        // grain_min+1 clans, but it's not guaranteed and rarely
        // useful).

        // first make a grain out of the nodes we've already collected
        if(!node_group.empty()) {
          GrainGraph.collapse_subgraph(node_group, grain_title(node_group, topology));
          node_group.clear();   // start the next grain
        }
        // then recurse on the subclan
        grain_collect(ClanTree, ClanTree.nodelist().find(*subclan), GrainGraph, grain_min);
      }
      else {
        // add this clan's nodes to the node group
        node_group.insert(subclan->nodes().begin(), subclan->nodes().end());
      }
    }

    // make a final grain out of any remaining nodes
    if(!node_group.empty())
      GrainGraph.collapse_subgraph(node_group, grain_title(node_group, topology));

    } // end of linear case 
    break;

  default:
    // shouldn't be able to get here.
    std::cerr << "Invalid clan type: " << ctypestr[claniterator->first.type] << "\n"; 
    abort();
  } 
}

#endif

