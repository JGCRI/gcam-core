#ifndef GCAM_FLOW_GRAPH_HPP_
#define GCAM_FLOW_GRAPH_HPP_

#include "util/base/include/definitions.h"

#if GCAM_PARALLEL_ENABLED

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
#include <set>

/* TBB headers */
#include <tbb/flow_graph.h>
#include <tbb/global_control.h>

// Forward declare when possible
class IActivity;
class MarketDependencyFinder;

/*!
 * \brief Class to package all of the information we need to carry around to use the flow graph
 */
class GcamFlowGraph {
    // Only select classes will be allowed to create or call flow graphs.
    friend class GcamParallel;
    friend class World;
    friend class MarketDependencyFinder;
private:
    //! Private constructor to only allow select classes to create flow graphs.
    GcamFlowGraph();
    
    //! The TBB calculation flow graph.
    tbb::flow::graph mTBBFlowGraph;
    
    //! The broadcast node which can be used to kick off calculations.
    tbb::flow::broadcast_node<tbb::flow::continue_msg> mHead;
    
    static tbb::global_control* mParallelismConfig;

public:
    
    ~GcamFlowGraph();
    //! The model period which will be calculated when called.
    static int mPeriod;
};

class GcamParallel {
public:
    static void makeTBBFlowGraph( const MarketDependencyFinder& aDependencyFinder,
                                  GcamFlowGraph& aTBBGraph );
    
    static void makeTBBFlowGraph( const MarketDependencyFinder& aDependencyFinder,
                                  GcamFlowGraph& aTBBGraph,
                                  const std::vector<IActivity*>& aPartialCalcList );
};

  
#endif // GCAM_PARALLEL_ENABLED


#endif // GCAM_FLOW_GRAPH_HPP_
