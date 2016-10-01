#ifndef DIGRAPH_OUTPUT_HPP_
#define DIGRAPH_OUTPUT_HPP_

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
#include <fstream>
#include <string> 
#include "parallel/include/digraph.hpp"

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
