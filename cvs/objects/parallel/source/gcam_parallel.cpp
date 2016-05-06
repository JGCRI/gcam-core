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

#if GCAM_PARALLEL_ENABLED
#include <map>
/* gcam headers */
#include "parallel/include/gcam_parallel.hpp"
#include "util/base/include/configuration.h"
#include "containers/include/world.h"
#include "containers/include/iactivity.h"
#include "containers/include/market_dependency_finder.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/timer.h"
/* more graph analysis headers */
#include "parallel/include/clanid.hpp"
#include "parallel/include/graph-parse.hpp"
#include "parallel/include/grain-collect.hpp"
#include "parallel/include/digraph-output.hpp"

using namespace std;

const int GcamParallel::DEFAULT_GRAIN_SIZE = 30;

/*!
 * \brief Default constructor
 *
 * \details Checks configuration for parallel-grain-size tag.  If so,
 *            uses it to set mGrain_size_tgt; if not, uses the default
 *            value. 
 * \remark Grain size is currently the only configurable parameter,
 *         but others may be added in the future.  Consider
 *         configuration parameter names starting with "parallel-" to
 *         be reserved for this purpose.
 */
GcamParallel::GcamParallel()
{
    mGrainSizeTarget = Configuration::getInstance()->getInt( "parallel-grain-size", DEFAULT_GRAIN_SIZE );
}
  


/*!
 * \brief Build the GCAM flow graph from the information in the MarketDependencyFinder 
 * \details The information we need is stored in the DependencyItem objects,
 *          which are stored in the MarketDependencyFinder.  This function uses
 *          that information to create an explicit flow graph for the parallel
 *          analysis functions.  This function should be run *after*
 *          MarketDependencyFinder::createOrdering(). 
 * \param[in] aDependencyFinder: The MarketDependencyFinder object. 
 * \param[out] aGCAMFlowGraph: The flow graph created by the routine.  On input it
 *                             should be a newly-created (i.e., empty) FGraph
 *                             object. 
 * \pre aDependencyFinder.createOrdering() has been run
 */
void GcamParallel::makeGCAMFlowGraph( const MarketDependencyFinder& aDependencyFinder, FlowGraph& aGCAMFlowGraph )
{
    // Basic procedure: 
    // *  For each dependency item, d:
    // **  For each calc vertex, v, of d:
    // ***  For each child, c, of v:
    // ****  Add the edge v -> c
    
    // Note that we don't have to recurse into the children because each
    // child belongs to some other dependency item, d'.  Thus, we pick
    // up all the child's dependencies when we process d'.
    FlowGraph fgTemp;
    
    const MarketDependencyFinder::DependencyItemSet& dependencyItems = aDependencyFinder.getDependencyItems();
    MarketDependencyFinder::CItemIterator diIter = dependencyItems.begin();
    for( ; diIter != dependencyItems.end(); ++diIter ) {
        // the vertex list actually comes in two parts: price vertices and
        // demand vertices.  Handle each in turn.
        for( int priceOrDemand = 0; priceOrDemand <= 1; ++priceOrDemand ) {
            const MarketDependencyFinder::VertexList &cvertices = priceOrDemand ?
                (*diIter)->mPriceVertices : (*diIter)->mDemandVertices;
            MarketDependencyFinder::CVertexIterator vIter = cvertices.begin();
            for( ; vIter != cvertices.end(); ++vIter ) {
                // *vIter is a CalcVertex*, it contains a node identifier used in
                // our flow graph.  It also has a vector of identifiers for its
                // children
                if( (*vIter)->mOutEdges.empty() ) {
                    // vertices that have neither parents nor children have to
                    // be created specially.  We can't easily tell whether a
                    // node has parents, so we add any vertices that have no
                    // children.  Both addnode and addedge guard against
                    // duplicate creation, so there's no problem with that.
                    fgTemp.addnode( (*vIter)->mCalcItem );
                }
                else {
                    MarketDependencyFinder::CVertexIterator cIter = (*vIter)->mOutEdges.begin();
                    for( ; cIter != (*vIter)->mOutEdges.end(); ++cIter ) {
                        // addedge will create the relevant nodes if they don't
                        // already exist; otherwise it will find them and connect
                        // them.
                        fgTemp.addedge( (*vIter)->mCalcItem, (*cIter)->mCalcItem );
                    }
                }
            }
        }
    }
    write_dot_to_file("gcam4.dot", fgTemp);
    // copy the transitive reduction of the flow graph we just made into
    // the output argument
    Timer &graphtimer = TimerRegistry::getInstance().getTimer("graph-timer");
    graphtimer.start();
    aGCAMFlowGraph = fgTemp.treduce();
    // perform a topological sort and record the results.
    aGCAMFlowGraph.topological_sort();
    graphtimer.stop();
    ILogger &mainlog = ILogger::getLogger("main_log");
    mainlog.setLevel(ILogger::DEBUG);
    graphtimer.print(mainlog, "Graph analysis in makeGCAMFlowGraph:  ");
}


/*!
 * \brief Parse the GCAM flow graph and collect IActivies into computational grains 
 * \details This function collects the individual computational tasks (price and
 *          demand vertices) into computational "grains," that is, collections
 *          that will be dispatched as a unit and computed serially (different
 *          grains can and will still be computed in parallel to one another).
 *          These grains still have a graph structure that determines the order
 *          in which they may be executed and which ones may be executed in
 *          parallel, so the output of this function is a graph of grains (as
 *          contrasted with the input graph of calc vertices).  We do this all
 *          as a single step (vs. the alternative of parsing and collecting as
 *          separate steps) to avoid exposing structures like "Clan Trees" to
 *          the outside world. 
 * \param[in] aGCAMFlowGraph: The gcam flow graph generated by makeGCAMFlowGraph 
 * \param[out] aGrainGraph: The graph of computational grains.  On input it
 *                          should be empty. 
 */
void GcamParallel::graphParseGrainCollect( const FlowGraph& aGCAMFlowGraph, FlowGraph& aGrainGraph )
{
    // some intermediate types involving "clans".  These will hold the
    // intermediate results of the parsing.
    typedef clanid<FlowGraphNodeType> ClanidType;
    typedef digraph<ClanidType> ClanTree;

    write_dot_to_file("gcam4a.dot", aGCAMFlowGraph);
    
    Timer &parsetimer = TimerRegistry::getInstance().getTimer("parse-timer");
    Timer &graintimer = TimerRegistry::getInstance().getTimer("grain-timer");
    ILogger &mainlog = ILogger::getLogger("main_log");
    mainlog.setLevel(ILogger::DEBUG);

    parsetimer.start();
    FlowGraph gcamFGReduce = aGCAMFlowGraph.treduce(); // find transitive reduction of gcamfg
    gcamFGReduce.topological_sort();
    
    ClanTree parseTree; 
    graph_parse( gcamFGReduce, 0, parseTree, mGrainSizeTarget );
    parsetimer.stop();
    
    // Use the parse tree to roll up the node graph into a grain graph.  Start
    // with a copy of the node graph.
    graintimer.start();
    FlowGraph grainGraphTemp = gcamFGReduce;
    grain_collect( parseTree, parseTree.nodelist().begin(), grainGraphTemp, mGrainSizeTarget );
    
    // set the output graph to the transitive reduction of what came out of the
    // grain collection algorithm.
    aGrainGraph = grainGraphTemp.treduce();
    graintimer.stop();

    parsetimer.print(mainlog, "Graph parse in graphParseGrainCollect:  ");
    graintimer.print(mainlog, "Grain collect in graphParseGrainCollect:  ");
}

/*!
 * \brief Parse the GCAM flow graph and collect IActivies into computational grains 
 *        for only a subset of the full graph.
 * \details This method determines the nodes which should remain in the graph according
 *          to aCalcItems then calls graphParseGrainCollect with that resulting graph.
 * \param[in] aGCAMFlowGraph: The gcam flow graph generated by makeGCAMFlowGraph 
 * \param[out] aGrainGraph: The graph of computational grains.  On input it
 *                          should be empty. 
 * \param[in] aCalcItems: The list of items to use to subset aGCAMFlowGraph.
 */
void GcamParallel::graphParseGrainCollect( const FlowGraph& aGCAMFlowGraph, FlowGraph& aGrainGraph,
                                           const vector<FlowGraphNodeType>& aCalcItems )
{
    FlowGraph::nodelist_t fullGraph = aGCAMFlowGraph.nodelist();
    FlowGraph::nodelist_t subGraph;
    for( vector<FlowGraphNodeType>::const_iterator it = aCalcItems.begin(); it != aCalcItems.end(); ++it ) {
        subGraph[ *it ] = fullGraph[ *it ];
    }
    FlowGraph subFlowGraph( subGraph, aGCAMFlowGraph.title() );
    graphParseGrainCollect( subFlowGraph, aGrainGraph );
}

/*!
 * \brief Build the TBB flow graph for an input grain structure and topology 
 * \details This function builds a TBB flow graph for the input grain graph and
 *          topology.  The resulting structure can be used to run the GCAM model
 *          (in essence, it replaces the loop over the vector of IActivity* in
 *          world->calc()).  When it is run, the TBB runtime will automatically
 *          determine how many threads to use, and it will queue and dispatch
 *          the computational grains automatically, in keeping with the
 *          relationships embodied in the grain graph. 
 * \remark The reason you need the topology is because although the grain graph
 *         captures all of the relationships between the grains, the ordering of
 *         the nodes within a grain is not available in any readily accessible
 *         form.  (Technically, it is there, so with some effort we should be
 *         able to remove this requirement . . . later) 
 * \param[in] aGrainGraph: graph of the computational grains
 *            (produced by graph_parse_grain_collect()) 
 * \param[in] aTopology: original gcam flow graph (see remark) 
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
void GcamParallel::makeTBBFlowGraph( const FlowGraph& aGrainGraph, const FlowGraph& aTopology,
                                     GcamFlowGraph& aTBBGraph )
{
    using tbb::flow::continue_node;
    using tbb::flow::continue_msg;
    
    tbb::flow::graph& tbbFlowGraph = aTBBGraph.mTBBFlowGraph;
    tbb::flow::broadcast_node<tbb::flow::continue_msg>& head = aTBBGraph.mHead;
    
    ILogger& pgLog = ILogger::getLogger( "parallel-grain-log" );
    pgLog.setLevel( ILogger::NOTICE );
    
    // We need a place to stash all of the TBB flow graph nodes, and we need to be
    // able to find them from the node identifiers.
    map<FlowGraphNodeType, continue_node<continue_msg>* > nodeTable;
    map<FlowGraphNodeType, int> nodeSizeTable;
    
    // The TBB flow graph structures don't automatically create nodes, so we'll do
    // two passes, creating nodes on the first and connecting them on the second.
    for( FlowGraph::nodelist_c_iter_t gnodeIt = aGrainGraph.nodelist().begin();
         gnodeIt != aGrainGraph.nodelist().end(); ++gnodeIt )
    {
        // Find the nodes from the original graph that are in this node.  We have to
        // extract them from the subgraph contained in the node, which is a little
        // ugly.  That subgraph has the information we need to order the nodes
        // without carrying an otherwise superfluous topology graph down into this
        // function.  Thus, this should be a prime candidate for refactoring.
        set<FlowGraphNodeType> subGraphNodes;
        getkeys( gnodeIt->second.subgraph->nodelist(), subGraphNodes );
        
        // create the node for this grain.  Note that the tbbfg_body constructor
        // uses the topology to order the elements of the grain, but does not store
        // a reference.
        size_t nodeSize = subGraphNodes.size();
        nodeTable[ gnodeIt->first ] = new continue_node<continue_msg>( tbbFlowGraph,
            TBBFlowGraphBody( subGraphNodes, aTopology, aTBBGraph ) );
        nodeSizeTable[ gnodeIt->first ] = nodeSize;
        pgLog << "\tContinue node: " << nodeTable[ gnodeIt->first ] << endl;
    }
    
    // In the second pass, connect edges in the nodes we just created.
    // This will make the TBB flow graph isomorphic to the grain graph.
    for( FlowGraph::nodelist_c_iter_t gnodeIt= aGrainGraph.nodelist().begin();
         gnodeIt != aGrainGraph.nodelist().end(); ++gnodeIt )
    {
        // get the children of each node in the flow graph
        set<FlowGraphNodeType> children = gnodeIt->second.successors;
        for( set<FlowGraphNodeType>::const_iterator cnodeIt = children.begin();
             cnodeIt != children.end(); ++cnodeIt )
        {
            // find the TBB flow graph nodes for the grain graph node and
            // the child node.  Connect them in the TBB flow graph.
            tbb::flow::make_edge( *nodeTable[ gnodeIt->first ], *nodeTable[ *cnodeIt ] );
            pgLog << nodeTable[ gnodeIt->first ] << "_" << nodeSizeTable[ gnodeIt->first ]
                << " -> " << nodeTable[ *cnodeIt ] << "_" << nodeSizeTable[ *cnodeIt ] << endl;
        }
    }
    
    // Find the source nodes and connect the TBB broadcast node to all of them.
    set<FlowGraphNodeType> sources;
    aGrainGraph.find_all_sources( sources );
    for( set<FlowGraphNodeType>::const_iterator srcIt = sources.begin();
         srcIt != sources.end(); ++srcIt )
    {
        tbb::flow::make_edge( head, *nodeTable[ *srcIt ] );
        pgLog << "start node found:  " << nodeTable[ *srcIt ] << "_" << nodeSizeTable[ *srcIt ] << endl;
    }
    // TBB flow graph is ready to go.
}

void GcamParallel::TBBFlowGraphBody::operator()( tbb::flow::continue_msg aMessage )
{
    for( list<FlowGraphNodeType>::const_iterator nodeIt = mNodes.begin();
         nodeIt != mNodes.end(); ++nodeIt )
    {
        if( !mGraph.mCalcList ||
            find( mGraph.mCalcList->begin(), mGraph.mCalcList->end(), *nodeIt ) != mGraph.mCalcList->end() )
        {
            (*nodeIt)->calc( mGraph.mPeriod );
        }
    }
}

GcamParallel::TBBFlowGraphBody::TBBFlowGraphBody( const std::set<FlowGraphNodeType>& aNodes,
                                                  const FlowGraph& aTopology,
                                                  const GcamFlowGraph& aGraph )
:mGraph( aGraph )
{
    ILogger& pgLog = ILogger::getLogger( "parallel-grain-log" );
    pgLog.setLevel( ILogger::NOTICE );
    
    if( !aTopology.topology_valid() ) {
        pgLog.setLevel( ILogger::SEVERE );
        pgLog << "Creating tbbfg_body with invalid topology." << endl;
        abort();
    }
    
    mNodes.insert( mNodes.end(), aNodes.begin(), aNodes.end() );
    mNodes.sort( TopologicalComparator( aTopology ) );
    
    // log some output to allow us to analyze the parallel grain
    // structure (this allows us to see what is in the grains, but not
    // the relationships between grains)
    pgLog << "\nGrain id: " << this << "   size:  " << mNodes.size() << "  Contents:";
    int i = 0;
    for( list<FlowGraphNodeType>::const_iterator it = mNodes.begin(); it != mNodes.end(); ++it ) {
        if( i % 3 == 0 ) {
            pgLog << "\n\t";
        }
        pgLog << (*it)->getDescription() << ", ";
    }
    pgLog << endl;
}

#endif // GCAM_PARALLEL_ENABLED
