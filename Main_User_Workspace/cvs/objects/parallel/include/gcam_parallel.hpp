#ifndef GCAM_FLOW_GRAPH_HPP_
#define GCAM_FLOW_GRAPH_HPP_

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

/* standard headers */
#include <list>
#include <set>

/* graph analysis headers */
#include "parallel/include/digraph.hpp"

/* TBB headers */
#include <tbb/flow_graph.h>

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
    GcamFlowGraph() : mTBBFlowGraph(), mHead( mTBBFlowGraph ), mPeriod( 0 ), mCalcList( 0 ) {}
    
    //! The TBB calculation flow graph.
    tbb::flow::graph mTBBFlowGraph;
    
    //! The broadcast node which can be used to kick off calculations.
    tbb::flow::broadcast_node<tbb::flow::continue_msg> mHead;

    //! The model period which will be calculated when called.
    int mPeriod;
    
    //! A list of items which actually need to be calculated which may be a subset
    //! of the total activities.  This can be used when individual flow graphs have
    //! not be calculated for sub-graphs.  Note when null it implies all activities
    //! will be calculated.
    const std::vector<IActivity*>* mCalcList;
};

/*!
 * \brief A class which converts activities and dependecies tracked by the MarketDependencyFinder
 *        and turn them into a TBB flow graph which can be calculated in parallel.
 * \details The process to convert to the flow graph is several steps which includes aggregating
 *          activities into larger grains to mitigate context switching overhead when the
 *          work to be done by an individual activity is small.
 */
class GcamParallel {
public:
    /* Types */

    //! Typedef for the type of a vertex in the flow graph
    typedef IActivity* FlowGraphNodeType;

    //! Flow graph of calc vertex dependencies for parallel analysis
    typedef digraph<FlowGraphNodeType> FlowGraph;
    
    GcamParallel();
    
    /* Graph analysis and parsing methods */
    void makeGCAMFlowGraph( const MarketDependencyFinder& aDependencyFinder, FlowGraph& aGCAMFlowGraph );
    
    void graphParseGrainCollect( const FlowGraph& aGCAMFlowGraph, FlowGraph& aGrainGraph );
    
    void graphParseGrainCollect( const FlowGraph& aGCAMFlowGraph, FlowGraph& aGrainGraph,
                                 const std::vector<FlowGraphNodeType>& aCalcItems );
    
    void makeTBBFlowGraph( const FlowGraph& aGrainGraph, const FlowGraph& aTopology,
                           GcamFlowGraph& aTBBGraph );
  
protected:
    //! Helper class for sorting lists in topological order
    struct TopologicalComparator {
        TopologicalComparator(const FlowGraph& aGraph ) : mTopology( aGraph ) {}
        
        bool operator()( const FlowGraphNodeType& aLHS, const FlowGraphNodeType& aRHS ) {
            return mTopology.topological_index( aLHS ) < mTopology.topological_index( aRHS );
        }
        
        //! A flow graph which can be used to check topological indices.
        const FlowGraph& mTopology;
    };
    
    /*!
     * \brief Body structure for tbb::flow_graph
     *
     * \details This structure defines a grain of work for TBB.  We will
     * place a bunch of these into a tbb::flow::graph structure, and TBB
     * will take care of the dispatch.  What this structure has to do is
     * to provide a way to execute the calculation vertices in the
     * topologically correct order.  We do that by taking in the set of
     * vertices and the graph structure that defines the topology.  From
     * that we can create a list that is sorted in topological order.
     * After that, the body struct no longer needs the graph.
     */
    struct TBBFlowGraphBody {
        TBBFlowGraphBody( const std::set<FlowGraphNodeType>& aNodes, const FlowGraph& aTopology,
                          const GcamFlowGraph& aGraph );
        
        void operator()( tbb::flow::continue_msg aMessage );

        //! The list of activities which will be calculated when TBB calls this class
        //! to execute.
        std::list<FlowGraphNodeType> mNodes;
        
        //! A reference to the TBB flow graph to which this node belongs.
        const GcamFlowGraph& mGraph;
    };
    
    /* data members */
    
    /*!
     * \brief Target grain size for parallel decomposition
     * \details Target grain size in the graph decomposition heuristics.
     *          Grains won't turn out to be exactly this size; however,
     *          larger gsize will generally result in larger grains, less
     *          parallelism, and less overhead.  Smaller gsize will result
     *          in the opposite.  The default is 30.
     */
    int mGrainSizeTarget;
    
    //! Default grain size
    static const int DEFAULT_GRAIN_SIZE;
    
    // right now, grain size is the only parameter in the heuristics.
    // We may add more later.
};

  
#endif // GCAM_PARALLEL_ENABLED


#endif // GCAM_FLOW_GRAPH_HPP_
