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

#include "util/base/include/definitions.h"

#if GCAM_PARALLEL_ENABLED
#include <cassert>
#include <vector>
#include <list>
#include <Eigen/SparseCore>
/* gcam headers */
#include "parallel/include/gcam_parallel.hpp"
#include "util/base/include/configuration.h"
#include "containers/include/world.h"
#include "containers/include/iactivity.h"
#include "containers/include/market_dependency_finder.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/timer.h"
#include "util/base/include/auto_file.h"
/* more graph analysis headers */
#include "parallel/include/clanid.hpp"
#include "parallel/include/graph-parse.hpp"
#include "parallel/include/grain-collect.hpp"
#include "parallel/include/digraph-output.hpp"

using namespace std;

int GcamFlowGraph::mPeriod = 0;
tbb::global_control* GcamFlowGraph::mParallelismConfig = 0;

/*
 * \brief Default constructor.
 * \details We lookup the "max-parallelism", aka number of cores to use, from the
 *          configuration so we can initialize TBB with it.
 */
GcamFlowGraph::GcamFlowGraph() : mTBBFlowGraph(), mHead( mTBBFlowGraph )
{
    const int maxParallelism = Configuration::getInstance()->getInt( "max-parallelism", -1 );
    if( maxParallelism > 0 && !mParallelismConfig ) {
        mParallelismConfig = new tbb::global_control( tbb::global_control::max_allowed_parallelism, maxParallelism );
    }
}

//! Destrcutor
GcamFlowGraph::~GcamFlowGraph() {
    delete mParallelismConfig;
    mParallelismConfig = 0;
    for(auto vert : mTBBVertices) {
        delete vert;
    }
}

/*!
 * \brief Build the TBB flow graph from the given dependency finder which contains
 *        the activities.
 * \details This function builds a TBB flow graph from the input dependency finder
 *          and.  The resulting structure can be used to run the GCAM model
 *          (in essence, it replaces the loop over the vector of IActivity* in
 *          world->calc()).  When it is run, the TBB runtime will automatically
 *          determine how many threads to use, and it will queue and dispatch
 *          the computational grains automatically, in keeping with the
 *          relationships embodied in the dependency finder.
 * \param[in] aDependencyFinder Defines the activities and their relationships.
 * \param[inout] aTBBGraph: The class that will hold the flow graph nodes as well
 *             as any other required items to run the flow graph including the
 *             broadcast node which serves as the trigger that causes
 *             the model to run, using this incantation:
 *               head.try_put(tbb::flow::continue_msg());
 *               tbbfg.wait_for_all();
 *             On input it should be default-constructed.  It needs to remain
 *             alive for as long as we're going to be running, so it should
 *             probably live in World, Scenario, or some similarly long-lived
 *             object.
 */
void GcamParallel::makeTBBFlowGraph( const MarketDependencyFinder& aDependencyFinder,
                                     GcamFlowGraph& aTBBGraph )
{
    using tbb::flow::continue_node;
    using tbb::flow::continue_msg;
    
    tbb::flow::graph& tbbFlowGraph = aTBBGraph.mTBBFlowGraph;
    tbb::flow::broadcast_node<tbb::flow::continue_msg>& head = aTBBGraph.mHead;
    
    ILogger& pgLog = ILogger::getLogger( "parallel-grain-log" );
    pgLog.setLevel( ILogger::NOTICE );
    
    // first read the information out of the dependency finder into an
    // adjacency matrix for easy as we are going to have to take multiple
    // passes to create the TBB structures required
    vector<IActivity*> globalOrdering = aDependencyFinder.getOrdering();
    vector<MarketDependencyFinder::CalcVertex*> calcVertexList( globalOrdering.size(), 0 );
    vector<bool> isSourceNode( globalOrdering.size(), true );
    using TripletType = Eigen::Triplet<bool>;
    list<TripletType> adjTriplets;
    
    for( MarketDependencyFinder::DependencyItem* item : aDependencyFinder.getDependencyItems() ) {
        for( MarketDependencyFinder::CalcVertex* vertex : item->mPriceVertices ) {
            calcVertexList[ vertex->mUID ] = vertex;
            for( MarketDependencyFinder::CalcVertex* outEdge : vertex->mOutEdges ) {
                isSourceNode[ outEdge->mUID ] = false;
                adjTriplets.push_back(TripletType(vertex->mUID, outEdge->mUID, true));
            }
        }
        for( MarketDependencyFinder::CalcVertex* vertex : item->mDemandVertices ) {
            calcVertexList[ vertex->mUID ] = vertex;
            for( MarketDependencyFinder::CalcVertex* outEdge : vertex->mOutEdges ) {
                isSourceNode[ outEdge->mUID ] = false;
                adjTriplets.push_back(TripletType(vertex->mUID, outEdge->mUID, true));
            }
        }
    }
    
    // TODO: in principle doing a transitive reduction should give us a speed up
    // running the model, however it seems to be rather computationaly expensive
    // see if we can come up with a faster algorithm that scales well with a large
    // number of vertices/edges.
    Eigen::SparseMatrix<bool> adjMatrix( globalOrdering.size(), globalOrdering.size());
    adjMatrix.setFromTriplets( adjTriplets.begin(), adjTriplets.end() );
    
    /*Eigen::SparseMatrix<bool> adjMatrixTransClosure = adjMatrix;
    for (int k=0; k<adjMatrix.outerSize(); ++k) {
        adjMatrixTransClosure = adjMatrix * adjMatrix;
    }
    
    // transative reduction
    adjMatrix = (adjMatrix - adjMatrixTransClosure).pruned();*/
    
    // we have to take two passes, first to create each of the verticies which
    // apparently can not be copied so we hang on to them with a pointer
    vector<continue_node<continue_msg>*>& tbbVert = aTBBGraph.mTBBVertices;
    tbbVert.reserve( calcVertexList.size() );
    for( MarketDependencyFinder::CalcVertex* vert : calcVertexList ) {
        IActivity* activity = vert->mCalcItem;
        tbbVert.push_back(new continue_node<continue_msg>(tbbFlowGraph, [activity](continue_msg) {
            activity->calc(GcamFlowGraph::mPeriod);
        }));
    }
    // now create the edges
    for (int k=0; k<adjMatrix.outerSize(); ++k) {
        for (Eigen::SparseMatrix<bool>::InnerIterator it(adjMatrix,k); it; ++it) {
            // regular dependency between activities
            pgLog << calcVertexList[it.row()]->mCalcItem->getDescription() << " -> " << calcVertexList[it.col()]->mCalcItem->getDescription() << endl;
            make_edge(*tbbVert[it.row()], *tbbVert[it.col()]);
        }
        // also include the "edge" from the head node to all activities that
        // have no incoming dependencies
        if(isSourceNode[k]) {
            pgLog << " head -> " << calcVertexList[k]->mCalcItem->getDescription() << endl;
            make_edge(head, *tbbVert[k]);
        }
    }
}


/*!
* \brief Build the TBB a partial flow graph from the given dependency finder which
*        contains the activities and given the list of the activities which are to be included
 *  in the partial graph.
* \details This method is basically does the same thing as for the global graph but is a bit
 *     more complicated due to not being able to use UIDs so we have to resort to set
 *     looks which are slower. So we seperate it into it's own method.
* \param[in] aDependencyFinder Defines the activities and their relationships.
 * \param[in] aPartialCalcList A list representing the subset of vertices to include
 *     in this subgraph.
* \param[inout] aTBBGraph: The class that will hold the flow graph nodes as well
*             as any other required items to run the flow graph including the
*             broadcast node which serves as the trigger that causes
*             the model to run, using this incantation:
*               head.try_put(tbb::flow::continue_msg());
*               tbbfg.wait_for_all();
*             On input it should be default-constructed.  It needs to remain
*             alive for as long as we're going to be running, so it should
*             probably live in World, Scenario, or some similarly long-lived
*             object.
*/
void GcamParallel::makeTBBFlowGraph( const MarketDependencyFinder& aDependencyFinder,
                                     GcamFlowGraph& aTBBGraph,
                                     const std::vector<IActivity*>& aPartialCalcList )
{
    // TODO:implement this.  We don't use it at the moment and to make sure
    // it works it would be handy to actually test it with something.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::ERROR );
    mainLog << "TBB flow graphs for partial subgraphs are not yet implemented." << endl;
    abort();
}

#endif // GCAM_PARALLEL_ENABLED
