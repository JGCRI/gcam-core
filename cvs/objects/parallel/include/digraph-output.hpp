#ifndef DIGRAPH_OUTPUT_HH_
#define DIGRAPH_OUTPUT_HH_


#include <iostream>
#include <fstream>
#include <string> 
#include "digraph.hpp"

// Template function for detailed digraph output.  This requires the
// nodeid type to have an operator<< defined, so don't include this
// file unless that is the case.

/*! \brief Write a detailed representation of a graph to a stream
 *! \details Output will be in graphvis dot format
 */
template <class nodeid_t>
void write_as_dot(std::ostream &o, const digraph<nodeid_t> &G)
{
  typedef digraph<nodeid_t> graph;

  // Write the dot header
  o << "digraph " << G.title() << " {\n";

  // iterate over nodes
  for(typename graph::nodelist_c_iter_t nodeit = G.nodelist().begin();
      nodeit != G.nodelist().end(); ++nodeit) {
    nodeid_t nodeid = nodeit->first;
    const typename graph::node_t & node = nodeit->second;

    // If the node is disconnected, output it
    if(node.successors.empty() && node.backlinks.empty()) {
      o << "\t" << nodeid << ";\n";
    }
    else {
      // iterate over children
      for(typename std::set<nodeid_t>::const_iterator childit = node.successors.begin();
          childit != node.successors.end(); ++childit) {
        nodeid_t childid = *childit;
        
        // output a line defining the edge between node and child
        o << "\t" << nodeid << " -> " << childid << ";\n";
      }
    }
  }

  // output the dot footer
  o << "}\n";
}

/*! \brief Write detailed representation of graph to a file
 *! \details This is just a wrapper around write_as_dot()
 *! \sa write_as_dot
 */
template <class nodeid_t>
void write_dot_to_file(const std::string &filename, const digraph<nodeid_t> &G)
{
  std::ofstream ofile(filename);

  if(ofile.fail()) {
    std::cerr << "Failed to open outfile " << filename << "\n";
    return;
  }

  write_as_dot(ofile, G);

  if(ofile.fail()) {
    std::cerr << "Error writing " << G.title() << " to file " << filename << "\n";
  }
}


#endif
