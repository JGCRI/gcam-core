#ifndef DIGRAPH_HPP_
#define DIGRAPH_HPP_

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

#include <vector>
#include <map>
#include <set>
#include <list>
#include <algorithm>
#include <string>
#include <assert.h>
#include "parallel/include/util.hpp"
#include "parallel/include/bmatrix.hpp"
#include "parallel/include/bitvector.hpp"

#include <iostream>

//! Store and operate on a directed graph 
//! \tparam nodeid_t The type of the identifiers for graph nodes. THIS TYPE MUST BE
//! COPYABLE! It must also have an order relation.
template <class nodeid_t> 
class digraph {
public: 
/***
 *** Graph node structure
 ***/ 
  //! Structure representing a graph node 
  //! \details The node_t wraps node object, assigns it a name, and
  //! keeps a list of successors
  struct node_t {
    nodeid_t id;               //!< The wrapped object
    //! Tag for algorithms that need one.  You can run a marking
    //! algorithm even on a const graph.
    mutable int mark;                 
    //! rank in the topological sort ordering
    mutable int topological_rank;
    //! Object's successors, stored by index into the graph's master node list.
    std::set<nodeid_t> successors;
    //! Backlinks to immediate ancestors
    std::set<nodeid_t> backlinks;
    //! Subgraph encapsulated by this node, if any.
    digraph *subgraph; 
    
    // constructors and destructors
    node_t(void) : topological_rank(-1),subgraph(0) {}
    explicit node_t(const nodeid_t &o) : id(o), topological_rank(-1),subgraph(0) {}
    node_t(const nodeid_t &o, const digraph &g) : id(o),topological_rank(-1) {
      subgraph = new digraph(g);
      subgraph->subp = true;
    }
    node_t(const node_t &n) {
      id = n.id;
      topological_rank = n.topological_rank;
      successors = n.successors;
      backlinks = n.backlinks;
      if(n.subgraph)
        subgraph = new digraph(*n.subgraph);
      else
        subgraph = 0;
    }
    node_t &operator=(const node_t &n) {
      if(this != &n) {
        id = n.id;
        topological_rank = n.topological_rank;
        successors = n.successors;
        backlinks = n.backlinks;
        if(n.subgraph)
          subgraph = new digraph(*n.subgraph);
        else
          subgraph = 0;
      }
      return *this;
    }
    ~node_t() {delete subgraph;}

    //! operator< passes through to the encapsulated object
    bool operator<(const nodeid_t id2) const {return id < id2;}
    bool operator==(const node_t B) const {
      return id == B.id && successors == B.successors && backlinks==B.backlinks
        && subgraph == B.subgraph;
    }
  };

/***
 *** Typedefs
 ***/
  typedef std::map<nodeid_t, node_t>                           nodelist_t;
  typedef typename std::map<nodeid_t, node_t>::iterator        nodelist_iter_t;
  typedef typename std::map<nodeid_t, node_t>::const_iterator  nodelist_c_iter_t;
  typedef typename std::map<nodeid_t, node_t>::value_type      nodelist_value_t; 
  
  //! Predicate class for filtering node lists
  class NodelistMemberTest {
    const nodelist_t &nodes;
    bool sense;
  public:
    NodelistMemberTest(const nodelist_t &thenodes, bool thesense) : nodes(thenodes), sense(thesense) {}
    bool operator()(const nodeid_t &tst) const {
      bool ismember = nodes.find(tst) != nodes.end();
      return sense ? ismember : !ismember;
    }
  };

protected:
/***
 *** Member Variables
 ***/ 
  //! Master list of nodes in this graph
  nodelist_t allnodes;
  std::string gtitle;
  bool subp;                    //!< Flag indicating whether this is a subgraph
  //! Lookup table for topological sort ordering
  mutable std::vector<nodeid_t> topsrtlookup;
  //! Flag indicating whether the topology is valid
  mutable bool topvalid;

public:
  // Constructors
  //! Default constructor makes empty graph
  digraph(std::string t="G", bool sub=false) : gtitle(t),subp(sub), topvalid(false) {}
  //! Constructor for a vector of objects. Nodes created from all objects, no edges added.
  //! \todo Make this accept first and last InputIterators,
  //! irrespective of the input container type.
  digraph(std::vector<nodeid_t> vo, std::string t="G", bool sub=false) : gtitle(t),subp(sub), topvalid(false) {
    for(int i=0;i<vo.size(); ++i)
      allnodes[vo[i]] = node_t(vo[i]); 
  }

  //! Constructor for a nodelist. Edges incident on other nodes in the input set are added.
  //! \remark Note the difference between this and the object version.
  //! This version keeps the link structure of the input nodes;
  //! whereas, the object version assumes there is none.  Thus, this
  //! version can be used to create a graph that is a subgraph of an
  //! existing graph.
  digraph(const std::map<nodeid_t, node_t> &input_nodes, const std::string &t="G");
  //! Constructor for an adjacency matrix and a list of names
  digraph(const bmatrix &adj, const std::vector<nodeid_t> &ids, const std::string &t="G");

  //! examine the node list
  const nodelist_t &nodelist() const {return allnodes;}
  //! get a handle to a node by nodeid
  const node_t &getnode(const nodeid_t &node) const;
  //! get a handle to a node by topological index
  const node_t &getnode(int index) const;
  //! is a subgraph?
  bool issub(void) const {return subp;} 
  //! get title
  const std::string &title(void) const {return gtitle;}
  //! get or set the title on a mutable graph
  std::string &title(void) {return gtitle;}
  //! get the topological sort order for a node
  int topological_index(const nodeid_t &n) const;
  //! topological index, given a nodelist iterator
  int topological_index(const nodelist_c_iter_t & niter) const
  {return niter->second.topological_rank;}
  //! get the node id for a given topological index
  nodeid_t topological_lookup(unsigned i) const;
  //! query whether the topological sort data is valid
  bool topology_valid(void) const {return topvalid;}
  //! convert a set of nodes (e.g. successors of a node) to a bitvector
  bitvector convert_to_bv(const std::set<nodeid_t> &nodes) const;
  //! convert a bitvector to a set of nodes
  std::set<nodeid_t> convert_to_set(const bitvector &nodes) const;
  
  //! Create a node with a given id
  void addnode(const nodeid_t &id) {
    if(allnodes.find(id) == allnodes.end())
      allnodes[id] = node_t(id);
    topvalid = false;
  }
  //! Create a subgraph node with a given id
  void addsubgraph(const nodeid_t &id, const digraph<nodeid_t> &g) {
    // this will be a no-op if there is already a node named id
    node_t node(id,g);
    allnodes[id] = node;
    topvalid = false;
  } 
  
  //! Add an edge to the graph
  //! \param o1 The origin of the edge
  //! \param o2 The destination of the edge 
  //! \details Create an edge from o1 to o2.  The nodes will be
  //! created and added if they don't already exist in the graph.
  void addedge(const nodeid_t &o1, const nodeid_t &o2,
               bool change_topology = true) {
    if(o1 == o2) {
      // Ignore self-edges
      std::cerr << "Warning: graph specification contains self-edge.  Ignoring.\n";
    }
    else {
      nodelist_value_t v1 = nodelist_value_t(o1, node_t(o1));
      nodelist_value_t v2 = nodelist_value_t(o2, node_t(o2));
      
      // These inserts will add the node if it doesn't exist, but will
      // not overwrite it if it does.
      nodelist_iter_t pn1 = allnodes.insert(v1).first;
      nodelist_iter_t pn2 = allnodes.insert(v2).first;
      pn1->second.successors.insert(o2);
      pn2->second.backlinks.insert(o1);
      if(change_topology)
        // When completing the linkage between the sources of a
        // primitive graph and the second layer of nodes, it is
        // convenient to avoid recomputing the topology until we're
        // finished with the graph parsing.
        topvalid = false;
    }
  }

  //! Remove all nodes and edges
  void clear(void) {allnodes.clear(); topvalid = false;}
  
  //! Remove an edge, incident nodes given as iterators in nodelist 
  //! \remark We don't invalidate the topology on removing an edge
  //! because the previous ordering will still be a valid ordering.
  void deledge(nodelist_iter_t pn1, nodelist_iter_t pn2) {
    pn1->second.successors.erase(pn2->first);
    pn2->second.backlinks.erase(pn1->first);
  }
  //! Remove an edge, incident nodes specified by name
  void deledge(const nodeid_t &o1, const nodeid_t &o2) {
    nodelist_iter_t pn1 = allnodes.find(o1);
    nodelist_iter_t pn2 = allnodes.find(o2);

    // Make sure the nodes exist before proceeding
    if(pn1 != allnodes.end() && pn2 != allnodes.end()) 
      deledge(pn1, pn2);
  }

  //! Remove a node, iterator version.
  //! \details If this node is a subgraph, all of the nodes in the subgraph will also be destroyed. 
  //! \param pn Nodelist iterator pointing at the node to delete 
  //! \param preserve_connect If true, connect the parents of the
  //!        removed node to its children.  If false, leave the graph
  //!        disconnected where the deleted node was.  (default =
  //!        false) 
  //! \remark As with removing an edge, we don't invalidate the
  //! topology.  The bogus node will still be in the topological
  //! index, but it doesn't cause any harm by being there.  All we
  //! really care about is the ordering amongst the valid nodes.
  void delnode(nodelist_iter_t &pn, bool preserve_connect = false)
  {
    const nodeid_t & nodeid = pn->first;
    std::set<nodeid_t> &successors = pn->second.successors;
    std::set<nodeid_t> &backlinks = pn->second.backlinks;
    typename std::set<nodeid_t>::iterator iter;
    
    if(preserve_connect) {
      // We need to connect all of this node's parent nodes to its child nodes
      for(typename std::set<nodeid_t>::iterator piter = backlinks.begin();
          piter != backlinks.end(); ++piter)
        for(typename std::set<nodeid_t>::iterator citer = successors.begin();
            citer != successors.end(); ++citer)
          addedge(*piter, *citer);
    }

    // need to delete all the edges connecting to this node
    // erase backlink to this node from successor node
    for(iter = successors.begin(); iter != successors.end(); ++iter)
      allnodes[*iter].backlinks.erase(nodeid);
    
    // erase forward link to this node from ancestor node
    for(iter = backlinks.begin(); iter != backlinks.end(); ++iter)
      allnodes[*iter].successors.erase(nodeid); 
    
    // erase the node itself
    allnodes.erase(pn);
  }
  //! Remove a node, node version
  void delnode(const nodeid_t &n, bool preserve_edges = false) {
    nodelist_iter_t pn = allnodes.find(n);
    if (pn!=allnodes.end())
      delnode(pn, preserve_edges);
  } 

  //! Collapse a subgraph into a single node
  void collapse_subgraph(const std::set<nodeid_t> &node_names, const nodeid_t &name);

  // *** Some functions for debugging graphs
  //! Query for the existence of a node
  bool node_exists(const nodeid_t &n) const {
    return allnodes.find(n) != allnodes.end();
  } 

  //! Query whether an edge exists.  Note that this function is direction-sensitive
  bool edge_exists(const nodeid_t &n1, const nodeid_t &n2) const {
    nodelist_c_iter_t pn1 = allnodes.find(n1);
    if(pn1 != allnodes.end() &&
       pn1->second.successors.find(n2) != pn1->second.successors.end())
      return true;
    else
      return false;
  }
  
  //! Check that the link structure of the graph is intact (all
  //! edges have a corresponding back-link and vice versa
  bool integrity_check(void) const {
    nodelist_c_iter_t it;
    for(it = allnodes.begin(); it != allnodes.end(); ++it) {
      // for each node...
      const std::set<nodeid_t> &successors = it->second.successors;
      typename std::set<nodeid_t>::const_iterator ssit;
      for(ssit = successors.begin(); ssit != successors.end(); ++ssit) {
        // for each successor of that node...
        if(!node_exists(*ssit)) {
          // check that the successor exists
          assert(node_exists(*ssit)); // barf if NDEBUG not defined
          return false;
        }
        const std::set<nodeid_t> &backlinks = allnodes.find(*ssit)->second.backlinks;
        if(backlinks.find(it->first) == backlinks.end()) {
          // throw an error if the successor does not have a backlink to this node.
          assert(backlinks.find(it->first) != backlinks.end());
          return false; 
        }
      }

      const std::set<nodeid_t> &backlinks = it->second.backlinks;
      // check that all backlinks correspond to valid edges
      for(ssit = backlinks.begin(); ssit != backlinks.end(); ++ssit) {
        if(!node_exists(*ssit)) {
          assert(node_exists(*ssit)); // barf if NDEBUG not defined
          return false;
        } 
        const std::set<nodeid_t> &successors = allnodes.find(*ssit)->second.successors;
        if(successors.find(it->first) == successors.end()) {
          // putative ancestor does not have an edge to this node 
          assert(successors.find(it->first) != successors.end());
          return false;
        }
      }
    }
    // passed all tests.
    return true;
  } 

/***
 *** Marking and testing marks
 ***/
  //! Clear marks on all nodes
  void clear_all_marks(void) const;
  //! Test whether node is marked
  bool node_is_marked(const nodeid_t &node) const;
  //! Set mark
  void set_mark(const nodeid_t &node, int markval=1) const;
  // These next two are specifically for testing flow graphs
  bool all_parents_marked(const nodeid_t &node) const;
  bool any_child_marked(const nodeid_t &node) const;
  

  
/***
 *** Graph algorithms
 ***/
  //! Find a node with no ancestors. If there is more than one, which
  //! one you get is arbitrary.
  nodeid_t find_source_node(void) const;
  //! Find a node in the specified subset with no ancestors in the subset
  //! (i.e., find a source node in a subgraph)
  nodeid_t find_source_node(const std::set<nodeid_t> &subg) const;
  //! Find a source node in a subgraph specified by a bitvector set 
  //! \remark It's not clear whether this version is useful or not,
  //! since it still winds up doing some searches on STL structures.
  //! I am beginning to think that what is really needed is a version
  //! of the digraph that uses bitsets from start to finish.
  nodeid_t find_source_node(const bitvector &subg) const; 
  //! Find all source nodes.
  void find_all_sources(std::set<nodeid_t> &rslt) const;
  //! Find all source nodes in a subgraph
  void find_all_sources(const std::set<nodeid_t> &subg, std::set<nodeid_t> &rslt) const;
  //! Find all source nodes in a subgraph specified by a bitvector set
  void find_all_sources(const bitvector &subg, bitvector &rslt) const;
  
  //! Find a node with no successors.  If there is more than one,
  //! which one you get is arbitrary.
  nodeid_t find_sink_node(void) const;
  //! Find a sink node in a subgraph
  nodeid_t find_sink_node(const std::set<nodeid_t> &subg) const;
  //! Find a sink node in a subgraph specified by a bitvector set
  nodeid_t find_sink_node(const bitvector &subg) const; 
  //! Find all sink nodes
  void find_all_sinks(std::set<nodeid_t> &rslt) const;
  //! Find all sink nodes in a subgraph
  void find_all_sinks(const std::set<nodeid_t> &subg, std::set<nodeid_t> &rslt) const;
  //! Find all sink nodes in a subgraph specified by a bitvector set
  void find_all_sinks(const bitvector &subg, bitvector &rslt) const;

  //! Build an adjacency matrix.  This also returns a vector of
  //! identifiers for the nodes corresponding to the rows and columns. 
  //! \remark Adjacency matrices currently ignore subgraphs, meaning
  //! that if you do some operations on one and then make a new graph
  //! out of the resulting matrix, you will lose all your subgraphs.
  //! Right now I'm planning to do all my graph operations before I
  //! make any subgraphs anyhow, so I'm not too worried about it, but
  //! maybe we should fix it some day.
  void build_adj_matrix(bmatrix &B, std::vector<nodeid_t> &ids) const;

  //! Depth-first search on the graph
  //! \param start The node at which to start the search 
  //! \param seen The set of nodes that have already been seen.
  //! Should be empty for the initial call. 
  //! \param reverse Flag indicating we should search the backlinks
  //! rather than the forward links (e.g. for finding ancestors). 
  //! \param targ Pointer to the id of the search target. Can be nil,
  //! in which case the search will traverse all successors and return
  //! them as a set in 'seen'. 
  //! \param partial_BFS Flag indicating that we can search the
  //! immediate successors of the current node for the target before
  //! continuing the depth-first search.  Whether this speeds or slows
  //! the search depends on the structure of the graph.  For trees
  //! with a lot of fan-out this can prevent some fruitless descent
  //! into subtrees at fairly minimal (i.e., log(fan-out)) cost. 
  //! \param self_include Flag indicating we should include the start
  //! node in the search results.  Generally this will be false for
  //! the initial call.  It will always be true for recursive calls. 
  //! \remark We could further optimize this function by using the
  //! nodes' marks instead of a list of already-seen nodes.  Note,
  //! however, that this would come at the cost of making concurrent
  //! or overlapping DFS calls on a single graph unsafe.
  bool DFS(const nodeid_t &start, std::set<nodeid_t> &seen, bool reverse=false,
           const nodeid_t *targ=0, bool partial_BFS=false,
           bool self_include=false) const;
  
  //! Depth-first search on a subset of a graph 
  //! \details This version of the depth-first search accepts a
  //! node-set representing the subset of the graph to operate on.
  //! Forward or backward links extending outside of the subset are
  //! treated as though they don't exist.  Other params are as for the
  //! regular DFS.
  //! \sa DFS(const nodeid_t, std::set<nodeid_t>, bool, const nodeid_t*, bool, bool) 
  //! \param subset Set of nodes to limit the search to. 
  //! \remark This is intended as an alternative to creating subgraphs
  //! in recursive graph calculations
  bool DFS(const nodeid_t &start, std::set<nodeid_t> &seen,
           const std::set<nodeid_t> &subset, bool reverse=false,
           const nodeid_t *targ=0, bool partial_BFS=false,
           bool self_include=false) const;
  
  
  //! Depth-first Search using a bitvector to store the seen nodes.
  //! This version should be used when performance is critical.
  //! \sa DFS(const nodeid_t, std::set<nodeid_t>, bool, const nodeid_t*, bool, bool)
  bool DFS(const nodeid_t &start, bitvector &seen, bool reverse=false,
           const nodeid_t *targ=0, bool partial_BFS=false,
           bool self_include=false) const;

  //! Subset depth-first search using bitvectors
  //! \sa DFS(const nodeid_t, std::set<nodeid_t>, const std::set<nodeid_t>, bool, const nodeid_t*, bool, bool) 
  bool DFS(const nodeid_t &start, bitvector &seen, const bitvector &subset,
           bool reverse=false, const nodeid_t *targ=0, bool partial_BFS=false,
           bool self_include=false) const;
  
  //! get descendants in a subgraph.
  //! \details It's up to the caller to ensure that the start node is part of the subgraph.
  //!          If subset is null, search the whole graph
  void find_descendants(const nodeid_t &start, std::set<nodeid_t> &rslt, const std::set<nodeid_t> *subset=0) const {
    rslt.clear();
    if(subset)
      DFS(start, rslt, subset);
    else
      DFS(start, rslt);
  }
  
  
  //! get descendants in a subgraph, using a bitvector set
  //! It's up to the caller to ensure that start is part of the subgraph
  //! If subset is null, search the whole graph.
  void find_descendants(const nodeid_t &start, bitvector &rslt, const bitvector *subset=0) const {
    rslt.clearall();
    if(subset)
      DFS(start,rslt,*subset);
    else
      DFS(start, rslt);
  }
  
  //! get ancestors in a subgraph.  If subset is null, search the whole graph
  void find_ancestors(const nodeid_t &start, std::set<nodeid_t> &rslt, const std::set<nodeid_t> *subset=0) const {
    rslt.clear();
    if(subset)
      DFS(start, rslt, *subset, true);
    else
      DFS(start, rslt, true);
  }

  //! get ancestors in a subgraph using a bitvector.  If subgraph is null, search the whole graph
  void find_ancestors(const nodeid_t &start, bitvector &rslt, const bitvector *subset=0) const {
    rslt.clearall();
    if(subset)
      DFS(start, rslt, *subset, true);
    else
      DFS(start, rslt, true);
  }
  
  // In order to find a path we need to include an order-preserving
  // structure like a list in DFS.  I'm going to be lazy and add this
  // only if we turn out to need it.  Odds are we'll want to do it as
  // a shortest path, which requires a BFS. 
  // //! Find a path from node a to node d

  //! check whether node d is a descendant of node a 
  //! \remark If I had it to do over again, I'd reverse the order of a
  //! and d in this function and is_ancestor, so that it read like an
  //! infix operator d (is_descendant) a.  As it stands, the argument
  //! order reflects the argument order in DFS (where the start node
  //! for the search is the first argument) 
  bool is_descendant(const nodeid_t &a, const nodeid_t &d, const std::set<nodeid_t> *subset=0) const {
    std::set<nodeid_t> seen;
    if(subset)
      return DFS(a, seen, *subset, false, &d);
    else
      return DFS(a, seen, false, &d);
  }
  //! test for a descendant using a bit set 
  //! \remark The subset parameter is not optional (if you omit it,
  //!         you get the STL version instead).  This version is
  //!         potentially a lot faster; you can force it to be used
  //!         over the STL version by passing a null bitvector pointer
  //!         as the third argument. 
  //! \warning You must have performed a topological sort on the graph
  //!          in order to use this version.
  bool is_descendant(const nodeid_t &a, const nodeid_t &d, const bitvector *subset) const {
    assert(topology_valid());
    bitvector seen(allnodes.size());
    if(subset)
      return DFS(a, seen, *subset, false, &d);
    else
      return DFS(a, seen, false, &d);
  }
  
  //! check whether node a is an ancestor of node d. 
  //! \remark Given the existence of is_descendant, this function is
  //! not strictly necessary; however, there is one slight difference.
  //! The start node in DFS must exist; the target node need not.
  //! Therefore, is_ancestor and is_descendant let you choose which of
  //! the two nodes, a or d, you guarantee to exist in the graph.
  //! (Whether you really want to substitute an exhaustive depth-first
  //! search for a log-N find is debatable, but far be it from me to
  //! judge you.)
  bool is_ancestor(const nodeid_t &d, const nodeid_t &a, const std::set<nodeid_t> *subset=0) const {
    std::set<nodeid_t> seen;
    if(subset)
      return DFS(d, seen, *subset, true, &a);
    else
      return DFS(d, seen, true, &a);
  }
  //! test for ancestor using a bitset
  bool is_ancestor(const nodeid_t &d, const nodeid_t &a, const bitvector *subset) const {
    assert(topology_valid());
    bitvector seen(allnodes.size());
    if(subset)
      return DFS(d, seen, *subset, true, &a);
    else
      return DFS(d, seen, true, &a);
  }
  
  
  //! Find the connected component containing the input node. 
  //! \remark Note that the search is necessarily bidirectional.  A
  //! single-directional search would yield a set of ancestors or
  //! descendants.
  void connected_component(const nodeid_t &start, std::set<nodeid_t> &comp, const std::set<nodeid_t> *subset=0) const;
  //! Find connected component using bitvector sets
  void connected_component(const nodeid_t &start, bitvector &comp,
                           const bitvector *subset=0) const;

  //! compute the adjacency matrix for the transitive completion of the graph
  void tcomplete(bmatrix &A, std::vector<nodeid_t> &nodes) const;
  //! transitive reduction, given the matrix from the transitive completion
  digraph<nodeid_t> treduce(const bmatrix &GT, const std::vector<nodeid_t> &nodes) const;
  //! compute the transitive reduction of the graph.
  //! \details This version uses a different algorithm than the matrix one.
  digraph<nodeid_t> treduce(void) const;

  //! perform a topological sort and store the results in the node objects
  const std::vector<nodeid_t> &topological_sort(void) const;

  protected:
/***
 *** Utility functions
 ***/
  static std::vector<nodeid_t> find_outside_edges(const nodelist_t &nodelist, const std::set<nodeid_t> node_t::* edgelist);
  static bool no_ancestors(const nodelist_value_t &n) {return n.second.backlinks.empty();}
  static bool has_ancestors(const nodelist_value_t &n) {return !n.second.backlinks.empty();}
  static bool no_descendants(const nodelist_value_t &n) {return n.second.successors.empty();}
  static bool has_descendants(const nodelist_value_t &n) {return !n.second.successors.empty();}
  void treduce_internal(const nodeid_t &nodename, const nodeid_t &last);
  nodeid_t find_srcsink_internal(const std::set<nodeid_t> &subg, bool reverse) const;
  nodeid_t find_srcsink_internal(const bitvector &subg, bool reverse) const;
  void find_sources_or_sinks_internal(const std::set<nodeid_t> &subg, std::set<nodeid_t> &rslt,
                                      bool reverse) const;
  void find_sources_or_sinks_internal(const bitvector &subg, bitvector &rslt, bool reverse) const;

};


template <class nodeid_t>
digraph<nodeid_t>::digraph(const nodelist_t &input_nodes, const std::string &t) : gtitle(t), subp(false), topvalid(false)
{
  // copy the input nodes.
  allnodes = input_nodes;
  
  // Remove all edges incident on nodes that are not among the input nodes
  NodelistMemberTest notin(allnodes, false);
  nodelist_iter_t it; 
  for(it=allnodes.begin(); it != allnodes.end(); ++it) {
    set_filter(it->second.successors, notin);
    set_filter(it->second.backlinks, notin);
  }
}

template <class nodeid_t>
digraph<nodeid_t>::digraph(const bmatrix &adj, const std::vector<nodeid_t> &ids, const std::string &t) :
  gtitle(t), subp(false), topvalid(false)
{
  for(int i=0; i<adj.nrow(); ++i) {
    addnode(ids[i]); // have to do this to make sure nodes with no edges get added -- addedge will create others as needed
    for(int j=0; j<adj.ncol(); ++j)
      if(adj[i][j])
        addedge(ids[i],ids[j]);
  }
}

template <class nodeid_t>
const typename digraph<nodeid_t>::node_t &digraph<nodeid_t>::getnode(const nodeid_t &node) const
{
  return allnodes.find(node)->second;
}

template <class nodeid_t>
const typename digraph<nodeid_t>::node_t &digraph<nodeid_t>::getnode(int index) const
{
  return getnode(topological_lookup(index));
}

template <class nodeid_t>
void digraph<nodeid_t>::collapse_subgraph(const std::set<nodeid_t> &node_names, const nodeid_t &name)
{
  assert(allnodes.find(name) == allnodes.end());

  nodelist_t subg_nodes;
  typename std::set<nodeid_t>::iterator nit;
  for(nit = node_names.begin(); nit != node_names.end(); ++nit)
    subg_nodes[*nit] = allnodes[*nit];
  
  std::vector<nodeid_t> inedges(find_outside_edges(subg_nodes, &node_t::backlinks));
  std::vector<nodeid_t> outedges(find_outside_edges(subg_nodes, &node_t::successors));

  
  typename std::vector<nodeid_t>::iterator it;

  // make sure that the node gets added, even if there are no in-edges
  // or out-edges.
  addnode(name);
  // add edges from the container node's ancestors to the container node
  for(it=inedges.begin(); it != inedges.end(); ++it) {
    addedge(*it, name);
    // delete all edges to the nodes within the subgraph
    nodelist_iter_t nodeit = subg_nodes.begin();
    for( ; nodeit != subg_nodes.end(); ++nodeit)
      deledge(*it, nodeit->first);
  }

  // add edges from container node to its ancestors
  for(it=outedges.begin(); it != outedges.end(); ++it) {
    addedge(name, *it);
    // delete all edges from the nodes within the subgraph
    nodelist_iter_t nodeit = subg_nodes.begin();
    for( ; nodeit != subg_nodes.end(); ++nodeit)
      deledge(nodeit->first, *it);
  }

  // delete subgraph nodes from the original graph
  nodelist_iter_t nodeit = subg_nodes.begin();
  for( ; nodeit != subg_nodes.end(); ++nodeit)
    delnode(nodeit->first);
  
  // make a subgraph out of the subg nodes and store with the container node
  allnodes[name].subgraph = new digraph(subg_nodes);
  allnodes[name].subgraph->subp = true;

  topvalid = false;             // we've added a new node
}


template <class nodeid_t>
std::vector<nodeid_t> digraph<nodeid_t>::find_outside_edges(const nodelist_t &nodelist,
                                                            const std::set<nodeid_t> node_t::* edgelist)
{
  // make one pass over the nodelist to count the total number of
  // edges.  We know the outside edges can be at most this many.
  int ntot = 0;
  typename nodelist_t::const_iterator it;
  for(it=nodelist.begin(); it != nodelist.end(); ++it)
    ntot += (it->second.*edgelist).size();

  std::vector<nodeid_t> rslt;
  rslt.resize(ntot);
  typename std::vector<nodeid_t>::iterator nextr = rslt.begin();

  NodelistMemberTest inlist(nodelist,true);
  
  for(it=nodelist.begin(); it != nodelist.end(); ++it) {
    const std::set<nodeid_t> & edges = it->second.*edgelist;
    nextr = std::remove_copy_if(edges.begin(), edges.end(), nextr, inlist);
  }

  // sort and remove duplicates 
  std::sort(rslt.begin(), nextr);
  nextr = std::unique(rslt.begin(),nextr);

  // trim the length of the result to the number of entries remaining
  rslt.resize(nextr-rslt.begin());

  return rslt;
}

template <class nodeid_t>
nodeid_t digraph<nodeid_t>::find_srcsink_internal(const std::set<nodeid_t> &subg, bool reverse) const
{
  for(typename std::set<nodeid_t>::const_iterator nodeit = subg.begin();
      nodeit != subg.end(); ++nodeit) {
    nodelist_c_iter_t nodepr = allnodes.find(*nodeit);
    bool edge = false;
    const std::set<nodeid_t> &next = reverse ? nodepr->second.backlinks : nodepr->second.successors;
    for(typename std::set<nodeid_t>::const_iterator nextit = next.begin();
        nextit != next.end(); ++nextit)
      if(subg.find(*nextit) != subg.end()) {
        edge = true;
        break;
      }
    if(!edge)
      return *nodeit;
  }
  assert(false);                // shouldn't be able to get here
  return nodeid_t();
}

template <class nodeid_t>
nodeid_t digraph<nodeid_t>::find_srcsink_internal(const bitvector &subg, bool reverse) const
{
  // This is a bit ugly, since we have to get the nodeid to look up its successors and backlinks.  
  bitvector_iterator nodeit(&subg);
  while(nodeit.next()) {
    nodeid_t node = topological_lookup(nodeit.bindex());
    nodelist_c_iter_t nodepr = allnodes.find(node);
    bool edge = false;
    const std::set<nodeid_t> &next = reverse ? nodepr->second.backlinks : nodepr->second.successors;
    for(typename std::set<nodeid_t>::const_iterator nextit = next.begin();
        nextit != next.end(); ++nextit)
      if(subg.get(topological_index(*nextit))) {
        edge = true;
        break;
      }
    if(!edge)
      return node;
  }
  assert(false);
  return nodeid_t();            // should we return a bit index here instead?
}

template <class nodeid_t>
void digraph<nodeid_t>::find_sources_or_sinks_internal(const std::set<nodeid_t> &subg, std::set<nodeid_t> &rslt,
                                                       bool reverse) const
{
  for(typename std::set<nodeid_t>::const_iterator nodeit = subg.begin();
      nodeit != subg.end(); ++nodeit) {
    nodelist_c_iter_t nodepr = allnodes.find(*nodeit);
    bool edge = false;
    const std::set<nodeid_t> &next = reverse ? nodepr->second.backlinks : nodepr->second.successors;
    for(typename std::set<nodeid_t>::const_iterator nextit = next.begin();
        nextit != next.end(); ++nextit)
      if(subg.find(*nextit) != subg.end()) {
        edge = true;
        break;
      }
    if(!edge)
      rslt.insert(*nodeit);
  }
  assert(!rslt.empty());
}

template <class nodeid_t>
void digraph<nodeid_t>::find_sources_or_sinks_internal(const bitvector &subg, bitvector &rslt, bool reverse) const
{
  // as before, we have to convert into the STL representation in
  // order to look up the graph edges, which makes this version a
  // little less useful than it might be.
  bitvector_iterator nodeit(&subg);
  while(nodeit.next()) {
    nodeid_t node = topological_lookup(nodeit.bindex());
    nodelist_c_iter_t nodepr = allnodes.find(node);
    bool edge = false;
    const std::set<nodeid_t> &next = reverse ? nodepr->second.backlinks : nodepr->second.successors;
    for(typename std::set<nodeid_t>::const_iterator nextit = next.begin();
        nextit != next.end(); ++nextit)
      if(subg.get(topological_index(*nextit))) {
        edge = true;
        break;
      }
    if(!edge)
      rslt.set(topological_index(node));
  }
  assert(!rslt.empty());
}

template <class nodeid_t>
nodeid_t digraph<nodeid_t>::find_source_node(void) const
{
  nodelist_c_iter_t rslt = std::find_if(allnodes.begin(), allnodes.end(), no_ancestors);
  if(rslt != allnodes.end())
    return rslt->first;
  else {                          // no sources - shouldn't happen in a DAG
    assert(false);
    return nodeid_t();
  }
} 

template <class nodeid_t>
nodeid_t digraph<nodeid_t>::find_source_node(const std::set<nodeid_t> &subg) const
{
  return find_srcsink_internal(subg, true);
}

template <class nodeid_t>
nodeid_t digraph<nodeid_t>::find_source_node(const bitvector &subg) const
{
  return find_srcsink_internal(subg, true);
}


template <class nodeid_t>
void digraph<nodeid_t>::find_all_sources(std::set<nodeid_t> &rslt) const
{
  nodelist_t srcnodes;
  std::remove_copy_if(allnodes.begin(), allnodes.end(),
                      std::inserter(srcnodes,srcnodes.end()), has_ancestors);
  getkeys(srcnodes,rslt);
}

template <class nodeid_t>
void digraph<nodeid_t>::find_all_sources(const std::set<nodeid_t> &subg, std::set<nodeid_t> &rslt) const
{
  find_sources_or_sinks_internal(subg, rslt, true);
}

template <class nodeid_t>
void digraph<nodeid_t>::find_all_sources(const bitvector &subg, bitvector &rslt) const
{
  find_sources_or_sinks_internal(subg, rslt, true);
}

template <class nodeid_t>
nodeid_t digraph<nodeid_t>::find_sink_node(void) const
{
  nodelist_c_iter_t rslt = std::find_if(allnodes.begin(), allnodes.end(), no_descendants);
  if(rslt == allnodes.end()) {
    assert(false);
    return nodeid_t();
  }
  else
    return rslt->first;
}

template <class nodeid_t>
nodeid_t digraph<nodeid_t>::find_sink_node(const std::set<nodeid_t> &subg) const
{
  return find_srcsink_internal(subg, false);
}

template <class nodeid_t>
nodeid_t digraph<nodeid_t>::find_sink_node(const bitvector &subg) const
{
  return find_srcsink_internal(subg, false);
}


template <class nodeid_t>
void digraph<nodeid_t>::find_all_sinks(std::set<nodeid_t> &rslt) const
{
  nodelist_t sinknodes;
  std::remove_copy_if(allnodes.begin(), allnodes.end(),
                      std::inserter(sinknodes, sinknodes.end()), has_descendants);
  getkeys(sinknodes,rslt);
}

template <class nodeid_t>
void digraph<nodeid_t>::find_all_sinks(const std::set<nodeid_t> &subg, std::set<nodeid_t> &rslt) const
{
  find_sources_or_sinks_internal(subg, rslt, false);
}

template <class nodeid_t>
void digraph<nodeid_t>::find_all_sinks(const bitvector &subg, bitvector &rslt) const
{
  find_sources_or_sinks_internal(subg, rslt, false);
}


template <class nodeid_t>
void digraph<nodeid_t>::build_adj_matrix(bmatrix &B, std::vector<nodeid_t> &ids) const
{
  getkeys(allnodes, ids);
  int m = ids.size();
  B.resize(m,m);

  for(int i=0; i<m; ++i) {
    const node_t &nodei = allnodes.find(ids[i])->second;
    unsigned *bi = B[i];
    for(int j=0; j<m; ++j)
      bi[j] = (nodei.successors.find(ids[j]) != nodei.successors.end());
  }
}

template <class nodeid_t>
bool digraph<nodeid_t>::DFS(const nodeid_t &start, std::set<nodeid_t> &seen, bool reverse,
                            const nodeid_t *targ, bool partial_BFS,
                            bool self_include) const
{
  if(self_include) {
    seen.insert(start);
    if(targ && start == *targ)
      return true;
  }
  
  // assume start is a valid node id.  If you call this function
  // with an invalid id, expect pain.
  const node_t &snode = allnodes.find(start)->second;
  assert(allnodes.find(start) != allnodes.end());
  const std::set<nodeid_t> &next(reverse ? snode.backlinks : snode.successors);

  // if permitted, check to see if targ is among this nodes immediate
  // successors.  If so, return immediately
  if(targ && partial_BFS) {
    if(next.find(*targ) != next.end()) {
      seen.insert(*targ);
      return true;
    }
  }

  // search recursively
  bool rv = false;
  typename std::set<nodeid_t>::const_iterator nit(next.begin());
  while(nit != next.end()) {
    // continue the search pass through all args except the starting
    // node and self_include.  Always self-include recursively
    // searched nodes.
    if(seen.find(*nit) == seen.end()) { // skip already visited nodes.
      rv = DFS(*nit, seen, reverse, targ, partial_BFS, true);
      if(rv)
        break;                  // stop search if we've found the target
    }
    ++nit;
  }

  return rv;
}


template <class nodeid_t>
bool digraph<nodeid_t>::DFS(const nodeid_t &start, std::set<nodeid_t> &seen,
                            const std::set<nodeid_t> &subset, bool reverse,
                            const nodeid_t *targ, bool partial_BFS,
                            bool self_include) const
{
  /* This is largely copied from the non-subset variant, with minor modifications
     to support the subsetting.  Maybe we should refactor at some point. */
  
  //! \pre start is a member of subset.  If not, it will be treated as if it is
  
  if(self_include) {
    seen.insert(start);
    if(targ && start == *targ)
      return true;
  }
  
  // assume start is a valid node id.  If you call this function
  // with an invalid id, expect pain.
  const node_t &snode = allnodes.find(start)->second;
  assert(allnodes.find(start) != allnodes.end());
  const std::set<nodeid_t> &next(reverse ? snode.backlinks : snode.successors);

  // if permitted, check to see if targ is among this nodes immediate
  // successors.  If so, return immediately
  if(targ && partial_BFS) {
    if(next.find(*targ) != next.end()) {
      if(subset.find(*targ) != subset.end()) {
        seen.insert(*targ);
        return true;
      }
      else                      // exclude if targ is not part of the subgraph
        return false;           // we test this rather late to avoid redundant testing at every recursion
    }
  }

  // search recursively
  bool rv = false;
  typename std::set<nodeid_t>::const_iterator nit(next.begin());
  while(nit != next.end()) {
    // continue the search pass through all args except the starting
    // node and self_include.  Always self-include recursively
    // searched nodes.
    if(seen.find(*nit) == seen.end() && subset.find(*nit) != subset.end()) { // skip already visited nodes and excluded nodes
      rv = DFS(*nit, seen, subset, reverse, targ, partial_BFS, true);
      if(rv)
        break;                  // stop search if we've found the target
    }
    ++nit;
  }

  return rv;
}



template <class nodeid_t>
bool digraph<nodeid_t>::DFS(const nodeid_t &start, bitvector &seen, bool reverse,
                            const nodeid_t *targ, bool partial_BFS,
                            bool self_include) const
{
  if(self_include) {
    seen.set(topological_index(start));
    if(targ && start == *targ)
      return true;
  }

  // caveats apply as in the other version of DFS
  const node_t &snode = allnodes.find(start)->second;
  assert(allnodes.find(start) != allnodes.end());
  const std::set<nodeid_t> &next(reverse ? snode.backlinks : snode.successors);

    // if permitted, check to see if targ is among this nodes immediate
  // successors.  If so, return immediately
  if(targ && partial_BFS) {
    if(next.find(*targ) != next.end()) {
      seen.set(topological_index(*targ));
      return true;
    }
  }

  // search recursively
  bool rv = false;
  typename std::set<nodeid_t>::const_iterator nit(next.begin());
  while(nit != next.end()) {
    // continue the search pass through all args except the starting
    // node and self_include.  Always self-include recursively
    // searched nodes.
    if(seen.get(topological_index(*nit)) == 0) {
      // skip already visited nodes
      // TODO: avoid a map lookup by using the mark field of the node?
      rv = DFS(*nit, seen, reverse, targ, partial_BFS, true);
      if(rv)
        break;                  // stop search if we've found the target
    }
    ++nit;
  }
  return rv;
}


template <class nodeid_t>
bool digraph<nodeid_t>::DFS(const nodeid_t &start, bitvector &seen,
                            const bitvector &subset, bool reverse,
                            const nodeid_t *targ, bool partial_BFS,
                            bool self_include) const
{
  /* As with the STL set version, this version of DFS is largely copied from its non-
     subgraph counterpart. */

  //! \pre start is a member of subset.  If not, then it will be treated as if it is

  if(self_include) {
    seen.set(topological_index(start));
    if(targ && start == *targ)
      return true;
  }

  // caveats apply as in the other version of DFS
  const node_t &snode = allnodes.find(start)->second;
  assert(allnodes.find(start) != allnodes.end());
  const std::set<nodeid_t> &next(reverse ? snode.backlinks : snode.successors);

  // if permitted, check to see if targ is among this nodes immediate
  // successors.  If so, return immediately
  if(targ && partial_BFS) {
    if(next.find(*targ) != next.end()) {
      if(subset.get(topological_index(*targ)) != 0) {
        seen.set(topological_index(*targ));
        return true;
      }
      else                      // exclude if target is not in the subgraph
        return false;           // we test this rather late to avoid redundant testing on every recursion
    }
  }

  // search recursively
  bool rv = false;
  typename std::set<nodeid_t>::const_iterator nit(next.begin());
  while(nit != next.end()) {
    // continue the search pass through all args except the starting
    // node and self_include.  Always self-include recursively
    // searched nodes.
    int nidx = topological_index(*nit);
    if(seen.get(nidx) == 0 && subset.get(nidx) != 0) {
      // skip already visited nodes and excluded nodes
      // TODO: avoid a map lookup by using the mark field of the node?
      rv = DFS(*nit, seen, subset, reverse, targ, partial_BFS, true);
      if(rv)
        break;                  // stop search if we've found the target
    }
    ++nit;
  }
  return rv;
}

template <class nodeid_t>
void digraph<nodeid_t>::tcomplete(bmatrix &A, std::vector<nodeid_t> &nodeids) const
{
  getkeys(allnodes, nodeids);
  int n = nodeids.size();
  A.resize(n,n);

  for(int i=0; i<n; ++i)
    for(int j=0; j<n; ++j)
      if(is_descendant(nodeids[i],nodeids[j]))
        A[i][j] = 1;
      else
        A[i][j] = 0;
}

template <class nodeid_t>
digraph<nodeid_t> digraph<nodeid_t>::treduce(void) const
{
  digraph<nodeid_t> Greduce(*this);
  Greduce.gtitle += "_transitive_reduction";
  // unmark all nodes
  for(nodelist_iter_t nodeit=Greduce.allnodes.begin();
      nodeit != Greduce.allnodes.end(); ++nodeit)
    nodeit->second.mark = 0;
  
  std::set<nodeid_t> srcnodes;
  Greduce.find_all_sources(srcnodes);
  if(srcnodes.empty()) {
    std::cerr << "No source nodes in this graph => cyclic => transitive reduction may not be unique.\n";
    // we'll start with an arbitrary first node in the graph and see what happens
    srcnodes.insert(allnodes.begin()->first);
  }
  for(typename std::set<nodeid_t>::iterator snodeit = srcnodes.begin();
      snodeit != srcnodes.end(); ++snodeit) {
    Greduce.treduce_internal(*snodeit,*snodeit);
  }
  return Greduce;
}

template <class nodeid_t>
void digraph<nodeid_t>::treduce_internal(const nodeid_t &nodename, const nodeid_t &last)
{
  node_t &node = allnodes.find(nodename)->second;
  if(node.mark) {
    std::cerr << "Cycle detected in graph.  Transitive reduction may not be unique.\n";
    // We don't try to print out the identity of the cycle node because we have no
    // guarantee that the nodeid type is printable.
    return;
  }
  // mark this node
  node.mark = 1;

  // examine all backlinks.  If any of them other than 'last' are
  // marked, then they are "shortcut" edges.  Delete any such edges we
  // find. (Note that the way we've set this up, this will delete self
  // edges too)
  typename std::set<nodeid_t>::iterator previt = node.backlinks.begin();
  while(previt != node.backlinks.end()) {
    nodeid_t prev = *previt++;
    if(prev != last) {
      node_t &pnode = allnodes.find(prev)->second;
      if(pnode.mark)
        deledge(prev,nodename); // removes prev from the backlinks
    }
  }
  // recurse on each outgoing edge in turn.
  typename std::set<nodeid_t>::iterator nextit = node.successors.begin();
  while(nextit != node.successors.end()) {
    nodeid_t next = *nextit;
    treduce_internal(next, nodename);
    nextit++;
  }

  // unmark this node as we leave
  node.mark = 0;
}

    

template <class nodeid_t>
digraph<nodeid_t> digraph<nodeid_t>::treduce(const bmatrix &GT,
                                             const std::vector<nodeid_t> &nodes) const
{
  bmatrix G;
  build_adj_matrix(G,nodes);

  bmatrix GTR(G - G*GT);

  return digraph<nodeid_t>(GTR,nodes,gtitle+"_transitive_reduction");
}


template <class nodeid_t>
void digraph<nodeid_t>::connected_component(const nodeid_t &start, std::set<nodeid_t> &comp, const std::set<nodeid_t> *subset) const
{
  nodelist_c_iter_t nit(allnodes.find(start));

  if(nit != allnodes.end() && (!subset || subset->find(start) != subset->end())) {
    comp.insert(start);
    const node_t &node(nit->second);
    typename std::set<nodeid_t>::const_iterator sit;
    for(sit = node.successors.begin(); sit != node.successors.end(); ++sit)
      if(comp.find(*sit) == comp.end() &&
         (!subset || subset->find(*sit) != subset->end())) // if node hasn't already been visited and is part of the subgraph
        connected_component(*sit,comp, subset); // visit it
    for(sit = node.backlinks.begin(); sit != node.backlinks.end(); ++sit) // now follow back-links
      if(comp.find(*sit) == comp.end() &&
         (!subset || subset->find(*sit) != subset->end()))
        connected_component(*sit,comp, subset);
  }
}

template <class nodeid_t>
void digraph<nodeid_t>::connected_component(const nodeid_t &start, bitvector &comp,
                                            const bitvector *subset) const
{
  nodelist_c_iter_t nit(allnodes.find(start));

  if(nit != allnodes.end()) {
    int stindx = topological_index(start);
    if(!subset || subset->get(stindx) != 0) {
      comp.set(stindx);
      const node_t &node(nit->second);
      typename std::set<nodeid_t>::const_iterator sit;
      for(sit = node.successors.begin(); sit != node.successors.end(); ++sit) {
        int nextidx = topological_index(*sit);
        if(comp.get(topological_index(*sit)) == 0
           && (!subset || subset->get(nextidx) != 0)) // if node hasn't already been visited and is part of the subgraph
          connected_component(*sit,comp,subset); // visit it
      }
      for(sit = node.backlinks.begin(); sit != node.backlinks.end(); ++sit) { // now follow back-links
        int nextidx = topological_index(*sit);
        if(comp.get(topological_index(*sit)) == 0 &&
           (!subset || subset->get(nextidx) != 0))
          connected_component(*sit,comp,subset);
      }
    }
  }
}


template <class nodeid_t>
int digraph<nodeid_t>::topological_index(const nodeid_t &n) const
{
  nodelist_c_iter_t nit = allnodes.find(n);
  if(nit == allnodes.end())
    return -1;                  // consider throwing an exception here

  return nit->second.topological_rank;
}

template<class nodeid_t>
nodeid_t digraph<nodeid_t>::topological_lookup(unsigned i) const
{
  if(topology_valid() && i<topsrtlookup.size())
    return topsrtlookup[i];
  else                          // again, consider throwing an exception
    return nodeid_t();
}

template <class nodeid_t>
const std::vector<nodeid_t> & digraph<nodeid_t>::topological_sort(void) const
{
  std::list<nodeid_t> ready;    // nodes that whose predecessors have already been sorted
  topsrtlookup.clear();
  
  // use the mutable mark field in each node to record the number of
  // unprocessed incoming links for each node.
  for(nodelist_c_iter_t nit=allnodes.begin();
      nit != allnodes.end(); ++nit) {
    int nedgein = nit->second.backlinks.size();
    nit->second.mark = nedgein;
    if(nedgein == 0)
      ready.push_back(nit->first);
  }

  // add each ready node to the sorted list; decrement the edge count
  // for each of its children.
  while(!ready.empty()) {
    nodeid_t n = *ready.begin();
    ready.pop_front();

    topsrtlookup.push_back(n);
    nodelist_c_iter_t nn = allnodes.find(n);
    assert(nn->second.mark == 0);
    const std::set<nodeid_t> &children = nn->second.successors;
    for(typename std::set<nodeid_t>::const_iterator child = children.begin();
        child != children.end(); ++child) {
      if(--allnodes.find(*child)->second.mark == 0)
        ready.push_back(*child);
    }
  }

  assert(topsrtlookup.size() == allnodes.size());

  // record the sort order for future use
  for(unsigned i=0; i<topsrtlookup.size(); ++i) {
    nodelist_c_iter_t nit = allnodes.find(topsrtlookup[i]);
    assert(nit != allnodes.end());
    nit->second.topological_rank = i;
  }

  topvalid = true;

  return topsrtlookup;
}

template <class nodeid_t>
bitvector digraph<nodeid_t>::convert_to_bv(const std::set<nodeid_t> &nodes) const
{
  bitvector bvnodes(allnodes.size());
  for(typename std::set<nodeid_t>::const_iterator nodeit = nodes.begin();
      nodeit != nodes.end(); ++nodeit) {
    bvnodes.set(topological_index(*nodeit));
  }
  return bvnodes;
}

template <class nodeid_t>
std::set<nodeid_t> digraph<nodeid_t>::convert_to_set(const bitvector &nodes) const
{
  std::set<nodeid_t> snodes;
  bitvector_iterator nodeit(&nodes);
  while(nodeit.next())
    snodes.insert(topological_lookup(nodeit.bindex()));
  return snodes;
}

template <class nodeid_t>
void digraph<nodeid_t>::clear_all_marks(void) const
{
  for(nodelist_c_iter_t nodeit = allnodes.begin();
      nodeit != allnodes.end(); ++nodeit)
    nodeit->second.mark = 0;
}

template <class nodeid_t>
bool digraph<nodeid_t>::node_is_marked(const nodeid_t &node) const
{
  nodelist_c_iter_t nodeit = allnodes.find(node);
  if(nodeit == allnodes.end())
    // no such node.  Treat it as not marked, I guess?
    return false;
  else
    return nodeit->second.mark;
}

/*! \brief Set mark
 *! \details Optionally, you can provide a specific mark value.  Note that
 *!          a mark value of 0 effectively clears the mark
 */
template <class nodeid_t>
void digraph<nodeid_t>::set_mark(const nodeid_t &node, int markval) const
{
  nodelist_c_iter_t nodeit = allnodes.find(node);
  if(nodeit != allnodes.end())
    // trying to set a mark on a nonexistent node is a no-op
    nodeit->second.mark = markval;
}

/*! \brief Determine whether a node has all parents marked
 *! \details Implemented as not-"any_parent_unmarked", which is true if and only
 *!          if there exists a parent node that is unmarked
 */
template <class nodeid_t>
bool digraph<nodeid_t>::all_parents_marked(const nodeid_t & node) const
{
  nodelist_c_iter_t nodeit = allnodes.find(node);
  if(nodeit == allnodes.end())
    // no such node; therefore, no unmarked parent; therefore, true
    return true;

  const std::set<nodeid_t> & parents = nodeit->second.backlinks;
  for(typename std::set<nodeid_t>::const_iterator parent = parents.begin();
      parent != parents.end(); ++parent) {
    if(allnodes.find(*parent)->second.mark == 0)
      return false;             // found an unmarked parent
  }

  // if we made it this far, all parents are marked
  return true;
}

/*! \brief Test whether any of a node's children are marked
 *! \details A non-existent node or one with no children does
 *!          not have any marked children
 */
template <class nodeid_t>
bool digraph<nodeid_t>::any_child_marked(const nodeid_t &node) const
{
  nodelist_c_iter_t nodeit = allnodes.find(node);
  if(nodeit == allnodes.end())
    return false;

  const std::set<nodeid_t> & children = nodeit->second.successors;
  for(typename std::set<nodeid_t>::const_iterator child = children.begin();
      child != children.end(); ++child) {
    if(allnodes.find(*child)->second.mark)
      return true;
  }

  // if we made it this far, then there were no marked children
  return false;
}

/*! \brief Output a brief representation of a graph
 *! \details Output will be in the form:
 *!           <digraph: graphtitle, N nodes>
 */
template <class nodeid_t>
std::ostream &operator<<(std::ostream &o, const digraph<nodeid_t> &G)
{
  o << "<digraph: " << G.title() << ", " << G.nodelist().size() << " nodes>";
  return o;
}


#endif

