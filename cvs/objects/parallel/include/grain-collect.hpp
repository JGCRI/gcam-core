#include "digraph.hpp"
#include "clanid.hpp"
#include "bitvector.hpp"
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
     <first node>_<size of set>
  */
  std::ostringstream title;

  title << bestnode << "_" << setsize;
  return title.str();
} 

/* Generate a unique node id for a set of nodes that is being rolled up
 *
 * In this generic case, the nodeid_t is expected to provide a method
 * to generate a unique nodeid.  The method is called on the nodeid
 * object for the topologically senior node of the set, and it gets
 * the size of the set as an argument.  From this you could reproduce
 * something like the string specialization.  You could also use class
 * facilities for generating GUIDs for nodes, or the like.
 */
template<class nodeid_t>
nodeid_t unique_nodetitle(const nodeid_t &bestnode, size_t setsize)
{
  return bestnode.gen_unique(setsize);
}


template<class nodeid_t>
nodeid_t grain_title(const bitvector &nodeset, const digraph<nodeid_t> &topology)
{
  /* Find the topologically senior node in the nodeset.  Form a unique
     name based on that node's name
  */

  /* Since nodes are indexed in bitvectors by their topological index,
     the first node in the set is automatically the senior.  Thus, there
     is no need to check any others.  
  */
  bitvector_iterator best(&nodeset);
  // The bitvector_iterator starts out pointing nowhere, so we have to
  // advance it to the first member of the set.
  best.next();
  nodeid_t bestnode = topology.topological_lookup(best.bindex()); 

  return unique_nodetitle(bestnode, nodeset.count());
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

  
  bitvector node_group(topology.nodelist().size());
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
      unsigned nsub = subclan->nodes().count();
      // search large subclans for grains
      if(nsub >= grain_min)
        grain_collect(ClanTree, ClanTree.nodelist().find(*subclan), GrainGraph, grain_min);
      else
        node_group.setunion(subclan->nodes());
    }

    // We've got this pool of small independent clans left.  If there
    // are enough of them it might be worth making another pass
    // through the list to try to break it up.  This is hard to do
    // exactly, since we don't know the distribution of the sizes of
    // the leftover clans.  We'll guess that they're pretty uniform
    // and build heuristics around that.
    int nnode = node_group.count(); // cache the number of nodes in the group.  Be careful to update whenever we change the group membership!
    int nbreakup = nnode / grain_min;
    if(nbreakup < 2 && nnode >= ind_split_min )
      // fudge the minimum grain size a little for extra parallelism.
      // It was probably just a guess anyhow.
      nbreakup = 2;

    if(nbreakup > 1) {
      // this will be the approximate size of the new grains we will make.
      unsigned grain_size_thresh = nnode / nbreakup;
      node_group.clearall();       // nnode no lonber valid!
      for(typename std::set<Clanid>::const_iterator subclan = claniterator->second.successors.begin();
          subclan != claniterator->second.successors.end(); ++subclan)
        if(subclan->nodes().count() < grain_min) { // skip the ones that were already processed above
          node_group.setunion(subclan->nodes());
          if(node_group.count() >= grain_size_thresh) {
            // have enough for a grain
            grain_name = grain_title(node_group, topology);
            GrainGraph.collapse_subgraph(topology.convert_to_set(node_group), grain_name);
            node_group.clearall();   // start the next grain
          }
        }
    }
    
    if(!node_group.empty()) {
      // leftover nodes from the iteration above form the last grain
      grain_name = grain_title(node_group, topology);
      GrainGraph.collapse_subgraph(topology.convert_to_set(node_group), grain_name);
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
    GrainGraph.collapse_subgraph(topology.convert_to_set(claniterator->first.nodes()), grain_name);
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
          subclan->nodes().count() >= ind_split_min ) {
        // only recurse on independent clans that are guaranteed to
        // split (an independent could split with as few as
        // grain_min+1 clans, but it's not guaranteed and rarely
        // useful).

        // first make a grain out of the nodes we've already collected
        if(!node_group.empty()) {
          GrainGraph.collapse_subgraph(topology.convert_to_set(node_group),
                                       grain_title(node_group, topology));
          node_group.clearall();   // start the next grain
        }
        // then recurse on the subclan
        grain_collect(ClanTree, ClanTree.nodelist().find(*subclan), GrainGraph, grain_min);
      }
      else {
        // add this clan's nodes to the node group
        node_group.setunion(subclan->nodes());
      }
    }

    // make a final grain out of any remaining nodes
    if(!node_group.empty())
      GrainGraph.collapse_subgraph(topology.convert_to_set(node_group),
                                   grain_title(node_group, topology));

    } // end of linear case 
    break;

  default:
    // shouldn't be able to get here.
    std::cerr << "Invalid clan type: " << ctypestr[claniterator->first.type] << "\n"; 
    abort();
  } 
}
