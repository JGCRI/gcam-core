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


/*! 
 * \file market_dependency_finder.cpp
 * \ingroup Objects
 * \brief The MarketDependencyFinder class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include <boost/algorithm/string/predicate.hpp>
#include "containers/include/market_dependency_finder.h"
#include "util/logger/include/ilogger.h"
#include "marketplace/include/marketplace.h"
#include "marketplace/include/market_container.h"
#include "marketplace/include/market_locator.h"
#include "marketplace/include/market.h"
#include "marketplace/include/linked_market.h"
#include "containers/include/iactivity.h"

#if GCAM_PARALLEL_ENABLED
#include "parallel/include/gcam_parallel.hpp"
#endif

using namespace std;

/*!
 * \brief Constructor.
 * \param aMarketplace The marketplace object in which this object is contained.
 */
MarketDependencyFinder::MarketDependencyFinder( Marketplace* aMarketplace ):
mMarketplace( aMarketplace ), mCalcVertexUIDCount( 0 )
#if GCAM_PARALLEL_ENABLED
,mTBBGraphGlobal( 0 )
#endif
{
}

/*!
 * \brief Destructor.
 * \details Note that this class assumes ownership of all member variables
 *          including the IActivity objects passed in.
 */
MarketDependencyFinder::~MarketDependencyFinder() {
    for( ItemIterator it = mDependencyItems.begin(); it != mDependencyItems.end(); ++it ) {
        delete *it;
    }
#if GCAM_PARALLEL_ENABLED
    delete mTBBGraphGlobal;
    for( CMarketToDepIterator it = mMarketsToDep.begin(); it != mMarketsToDep.end(); ++it ) {
        delete (*it)->mFlowGraph;
        delete *it;
    }
#endif
}

//! DependencyItem destructor
MarketDependencyFinder::DependencyItem::~DependencyItem() {
    for( CVertexIterator priceIter = mPriceVertices.begin(); priceIter != mPriceVertices.end(); ++priceIter ) {
        delete *priceIter;
    }
    for( CVertexIterator demandIter = mDemandVertices.begin(); demandIter != mDemandVertices.end(); ++demandIter ) {
        delete *demandIter;
    }
}

//! CalcVertex destructor
MarketDependencyFinder::CalcVertex::~CalcVertex() {
    delete mCalcItem;
}

/*!
 * \brief Add a dependency relationship to keep track of.
 * \details Dependency relationships are given in terms of price dependence.  So
 *          that if aDependencyName changes it's price then aDependentName must
 *          be recalculated.  Note that region names must be specified as well to
 *          allow direct trading between regions.
 * \param aDependentName The name of the item which must be calculated after
 *                       aDependencyName.
 * \param aDependentRegion The region name in which aDependentName is contained.
 * \param aDependencyName The name of the item which must be calculated before
 *                       aDependentName.
 * \param aDependencyRegion The region name in which aDependencyName is contained.
 * \param aCanBeBroken True if when trial markets are created for aDependencyName
 *                     this dependency can be removed, false otherwise.  Note true
 *                     is the default value since it is almost always the case.
 * \return Wether a new entry was created to by adding this dependency, in other
 *         words it wasn't duplicative.
 */
bool MarketDependencyFinder::addDependency( const string& aDependentName,
                                            const string& aDependentRegion,
                                            const string& aDependencyName,
                                            const string& aDependencyRegion,
                                            const bool aCanBeBroken )
{
    // Find/create a DependencyItem entry for the dependent item
    auto_ptr<DependencyItem> item( new DependencyItem( aDependentName, aDependentRegion ) );
    ItemIterator dependentIter = mDependencyItems.find( item.get() );
    if( dependentIter == mDependencyItems.end() ) {
        dependentIter = mDependencyItems.insert( item.release() ).first;
    }

    // Check for self dependence
    if( aDependentName == aDependencyName && aDependentRegion == aDependencyRegion ) {
        (*dependentIter)->mHasSelfDependence = true;
        return false;
    }
    
    // Find/create a DependencyItem entry for the dependency 
    item.reset( new DependencyItem( aDependencyName, aDependencyRegion ) );
    ItemIterator dependencyIter = mDependencyItems.find( item.get() );
    if( dependencyIter == mDependencyItems.end() ) {
        dependencyIter = mDependencyItems.insert( item.release() ).first;
    }
    (*dependencyIter)->mCanBreakCycle &= aCanBeBroken;
    
    // These are kept track of by adding to the list of dependents for the
    // dependency item.
    return (*dependencyIter)->insertDependent( *dependentIter );
}

/*!
 * \brief Bind a dependency name to the actual activity which can be used to calculate
 *        it.
 * \details Note that it is possible for the same name to refer to multiple activities
 *          For instance with an end-use sector and the end-use final demand.
 *          In this case we assume a linear dependency for all items of the same name.
 *          The items which were dependencies of that name can just be dependent on the
 *          first activity item.
 * \param aRegionName The name of the region in which activity exists.
 * \param aActivityName The name of the activity.
 * \param aDemandActivity The item to bind to a dependency name.  This class will
 *                        assume ownership of this memory.
 * \param aPriceActivity The optional price item to bind to a dependency name.
 *                       This class will assume ownership of this memory.
 */
void MarketDependencyFinder::resolveActivityToDependency( const string& aRegionName,
                                                          const string& aActivityName,
                                                          IActivity* aDemandActivity,
                                                          IActivity* aPriceActivity )
{
    auto_ptr<DependencyItem> item( new DependencyItem( aActivityName, aRegionName ) );
    ItemIterator itemIter = mDependencyItems.find( item.get() );
    if( itemIter == mDependencyItems.end() ){
        // No dependencies have been added for this activity yet.  They may get added
        // later in a different region so we can just add the dependency item for now
        // with not dependency links.  There will be more error checking later to ensure
        // this wasn't an actual mistake.
        itemIter = mDependencyItems.insert( item.release() ).first;
    }
    
    (*itemIter)->mDemandVertices.push_back( new CalcVertex( aDemandActivity, *itemIter, mCalcVertexUIDCount++ ) );
    // The price activity may not be necessary.
    if( aPriceActivity ) {
        (*itemIter)->mPriceVertices.push_back( new CalcVertex( aPriceActivity, *itemIter, mCalcVertexUIDCount++ ) );
    }
}

/*!
 * \brief Get an in-order list of activities to calculate.
 * \details If called with a market number of -1 (the default value) then the complete
 *          ordering which will calculate all objects in the model is returned.
 *          When a valid market number is given the in-order list of activities which
 *          would be affected by that market changing it's price would be generated
 *          and returned.
 * \param aMarketNumber The market number to get an ordered list of items which
 *                      are required to be calculated if that market changes prices,
 *                      or if -1 the full global list.
 * \return The appropriate list of activities to calculate for the given market.
 *         Note the caller is not responsible for the returned memory.
 */
const vector<IActivity*> MarketDependencyFinder::getOrdering( const int aMarketNumber ) const {
    if( aMarketNumber == -1 ) {
        // Just return the global ordering which has already been generated.
        return mGlobalOrdering;
    }
    else {
        // Find the entry points into the graph for the given market.
        auto_ptr<MarketToDependencyItem> marketToDep( new MarketToDependencyItem( aMarketNumber ) );
        CMarketToDepIterator mrktIter = mMarketsToDep.find( marketToDep.get() );
        if( mrktIter == mMarketsToDep.end() ) {
            // Somehow this market was not linked to any entry points into the graph.
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Could not find market: " << mMarketplace->mMarkets[ aMarketNumber ]->getName()
                    << " to get an ordering for." << endl;
            exit( 1 );
        }

        // First check the MarketToDependencyItem and see if we have this cached.
        if( !(*mrktIter)->mCalcList.empty() ) {
            return (*mrktIter)->mCalcList;
        }

        // We must generate the full list of nodes to calculate.  The createOrdering
        // method has created a link from markets to "entry points" into the graph.
        // We must then do a search on the graph from those entry points to come up
        // with a full set of items which must be recalculated.
        // To ensure the set of items are in order we can utilize the already created
        // global ordering.
        
        
        // Do a search from all of the entry points to get a set of unique items
        // to calculate.
        set<IActivity*> dependenentCalcs;
        typedef set<CalcVertex*>::const_iterator CVertexIterator;
        for( CVertexIterator it = (*mrktIter)->mImpliedVertices.begin(); it != (*mrktIter)->mImpliedVertices.end(); ++it ) {
            findVerticesToCalculate( *it, dependenentCalcs );
        }
        
        // Loop over the global ordering and move an item from the dependenentCalcs
        // into the ordered list to return should it exist in dependenentCalcs.
        vector<IActivity*> orderedListForMarket;
        for( vector<IActivity*>::const_iterator it = mGlobalOrdering.begin(); it != mGlobalOrdering.end(); ++it ) {
            set<IActivity*>::iterator dependIter = dependenentCalcs.find( *it );
            if( dependIter != dependenentCalcs.end() ) {
                orderedListForMarket.push_back( *dependIter );
                dependenentCalcs.erase( dependIter );
            }
        }
        // Go ahead and cache this list so we do not need to calculate it again.
        (*mrktIter)->mCalcList = orderedListForMarket;
        return orderedListForMarket;
    }
}

#if GCAM_PARALLEL_ENABLED
/*!
 * \brief Get flow graph which can be used to calculate the model in parallel.
 * \details If called with a market number of -1 (the default value) then the global
 *          graph which will calculate all objects in the model is returned.
 *          When a valid market number is given the flow graph of activities which
 *          would be affected by that market changing it's price would be generated
 *          and returned.
 * \param aMarketNumber The market number to get a flow graph of items which
 *                      are required to be calculated if that market changes prices,
 *                      or if -1 the full global list.
 * \return The appropriate list of activities to calculate for the given market.
 *         Note the caller is not responsible for the returned memory.
 */
GcamFlowGraph* MarketDependencyFinder::getFlowGraph( const int aMarketNumber ) {
    if( aMarketNumber == -1 ) {
        if( !mTBBGraphGlobal ) {
            // convert dependency table to flow graph
            mTBBGraphGlobal = new GcamFlowGraph();
            GcamParallel::makeTBBFlowGraph( *this, *mTBBGraphGlobal );
        }
        return mTBBGraphGlobal;
    }
    else {
        // Find the entry points into the graph for the given market.
        auto_ptr<MarketToDependencyItem> marketToDep( new MarketToDependencyItem( aMarketNumber ) );
        CMarketToDepIterator mrktIter = mMarketsToDep.find( marketToDep.get() );
        if( mrktIter == mMarketsToDep.end() ) {
            // Somehow this market was not linked to any entry points into the graph.
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Could not find market: " << mMarketplace->mMarkets[ aMarketNumber ]->getName()
                    << " to get an ordering for." << endl;
            abort();
        }

        // First check the MarketToDependencyItem and see if we have this cached.
        if( (*mrktIter)->mFlowGraph ) {
            return (*mrktIter)->mFlowGraph;
        }

        // build the tbb graph structure for the subgraph that is affected by this market
        (*mrktIter)->mFlowGraph = new GcamFlowGraph();
        GcamParallel::makeTBBFlowGraph( *this, *(*mrktIter)->mFlowGraph, getOrdering( aMarketNumber ) );
        return (*mrktIter)->mFlowGraph;
    }
}
#endif

/*!
 * \brief A depth first search collecting a unique set of the vertices visited.
 * \details Recursively search for vertices.  The end points for recursion are if
 *          at vertices which have already been found or that have no out edges.
 * \param aVertex The current vertex being visited.
 * \param aVisited The set of vertices which have already been visited.
 */
void MarketDependencyFinder::findVerticesToCalculate( CalcVertex* aVertex, std::set<IActivity*>& aVisited ) const {
    // Attempt to add the current vertex to the list of visited vertices.
    if( !aVisited.insert( aVertex->mCalcItem ).second ) {
        // Already processed this subgraph so no need to continue search from
        // here.
        return;
    }
    
    // Visit all vertices along the out edges from aVertex.
    typedef vector<CalcVertex*>::const_iterator CVertexIterator;
    for( CVertexIterator it = aVertex->mOutEdges.begin(); it != aVertex->mOutEdges.end(); ++it ) {
        findVerticesToCalculate( *it, aVisited );
    }

    // Add any implied in edges that should be calculated (special case for the
    // land-allocator).
    typedef set<CalcVertex*>::const_iterator CImpVertexIterator;
    for( CImpVertexIterator it = aVertex->mImpliedInEdges.begin(); it != aVertex->mImpliedInEdges.end(); ++it ) {
        findVerticesToCalculate( *it, aVisited );
    }
}

/*!
 * \brief A comparison functor to distinguish between DependencyItems.  Provides
 *        a way to determine if one DependencyItem is less than another.
 * \param aLHS The DependencyItem on the left hand side of the comparison.
 * \param aRHS The DependencyItem on the right hand side of the comparison.
 * \return True if aLHS < aRHS.
 */
bool MarketDependencyFinder::DependencyItemComp::operator()( const DependencyItem* aLHS,
                                                             const DependencyItem* aRHS ) const
{
    return aLHS->mName != aRHS->mName ? aLHS->mName < aRHS->mName :
        aLHS->mLocatedInRegion < aRHS->mLocatedInRegion;
}

/*!
 * \brief Connect the dependency graph then do a topological sort to come up with
 *        a serial ordering.  Note that cycles will be automatically broken by
 *        creating trial price and demand markets.
 * \details Note that this method will also need to bind markets with their entry
 *          points into the graph.  This can be used later to create market specific
 *          ordering.
 */
void MarketDependencyFinder::createOrdering() {
    // We have collected all dependency information and now all markets will have
    // been created so now we may create an ordering.
    
    // First associate markets to their corresponding dependency items
    map<int, DependencyItemSet> marketDepGrouping;
    for( CItemIterator it = mDependencyItems.begin(); it != mDependencyItems.end(); ++it ) {
        // locate the market by name
        int marketNumber = mMarketplace->mMarketLocator->getMarketNumber( (*it)->mLocatedInRegion, (*it)->mName );
        if( marketNumber != MarketLocator::MARKET_NOT_FOUND ) {
            // Find/create an entry for the market to dependency struct
            auto_ptr<MarketToDependencyItem> marketToDep( new MarketToDependencyItem( marketNumber ) );
            MarketToDepIterator mrktIter = mMarketsToDep.find( marketToDep.get() );
            if( mrktIter == mMarketsToDep.end() ) {
                mrktIter = mMarketsToDep.insert( marketToDep.release() ).first;
            }
            
            // Stash the market number and whether it is a solved market in any
            // model period.
            // Implied entry points will be added below.
            (*it)->mLinkedMarket = marketNumber;
            bool isSolved = false;
            for( int period = 1; period < mMarketplace->mMarkets[ marketNumber ]->size() && !isSolved; ++period ) {
                isSolved = mMarketplace->mMarkets[ marketNumber ]->getMarket( period )->isSolvable();
            }
            (*it)->mIsSolved = isSolved;
            // we don't need to wory about grouping solved markets since they will just
            // get disconnected anyways
            if( !isSolved || !(*it)->mCanBreakCycle ) {
                marketDepGrouping[ marketNumber ].insert( *it );
            }
        }
    }
    
    // Ajust dependencies for multi-region dependencies (such as global markets ) since a dependency
    // on an activity in just one region should actually apply to all regions in the market.  Note
    // we have excluded solved markets from this list for simplicitly since they will just drop the
    // dependencies anyways.
    for( auto depGrouping : marketDepGrouping ) {
        // skip groupings that are not multi-region
        if( depGrouping.second.size() > 1 ) {
            // add dependencies from any one of the regions to all of the others
            for( auto currDep : depGrouping.second ) {
                for( auto otherDep : depGrouping.second ) {
                    // note mDependentList is a set so we don't need to worry about
                    // duplicates which will happen a bunch but oh well..
                    for( auto depToAdd : otherDep->mDependentList ) {
                        currDep->insertDependent( depToAdd );
                    }
                }
            }
        }
    }
    
    // Initialize vertices in the graph.
    CalcVertexCountMap numDependencies;
    for( CItemIterator it = mDependencyItems.begin(); it != mDependencyItems.end(); ++it ) {
        // Initialize dependency counts which will be used to do the topological sort.
        for( CVertexIterator vertexIter = (*it)->mPriceVertices.begin(); vertexIter != (*it)->mPriceVertices.end(); ++vertexIter ) {
            numDependencies[ (*vertexIter) ] = 0;
        }
        for( CVertexIterator vertexIter = (*it)->mDemandVertices.begin(); vertexIter != (*it)->mDemandVertices.end(); ++vertexIter ) {
            numDependencies[ (*vertexIter) ] = 0;
        }
        
        // Should this dependency item have multiple activities then we will just
        // connect the price/demand vertices to each other and in that way from
        // now on we only need to worry about connecting other activities to just 
        // the first price/demand vertex.
        if( (*it)->mPriceVertices.size() > 1 ) {
            for( vector<CalcVertex*>::iterator vertexIter = (*it)->mPriceVertices.begin() + 1; vertexIter != (*it)->mPriceVertices.end(); ++vertexIter ) {
                (*(vertexIter - 1))->mOutEdges.push_back( *vertexIter );
                ++numDependencies[ *vertexIter ];
            }
        }
        if( (*it)->mDemandVertices.size() > 1 ) {
            for( vector<CalcVertex*>::reverse_iterator vertexIter = (*it)->mDemandVertices.rbegin() + 1; vertexIter != (*it)->mDemandVertices.rend(); ++vertexIter ) {
                (*(vertexIter - 1))->mOutEdges.push_back( *vertexIter );
                ++numDependencies[ *vertexIter ];
            }
        }
    }
    
    // Connect the graph by setting the out edges.
    for( CItemIterator it = mDependencyItems.begin(); it != mDependencyItems.end(); ++it ) {
        if( (*it)->mIsSolved ) {
            auto_ptr<MarketToDependencyItem> marketToDep( new MarketToDependencyItem( (*it)->mLinkedMarket ) );
            MarketToDepIterator mrktIter = mMarketsToDep.find( marketToDep.get() );
            assert( mrktIter != mMarketsToDep.end() );
            if( !(*it)->mPriceVertices.empty() ) {
                (*mrktIter)->mImpliedVertices.insert( (*it)->getFirstPriceVertex() );
            }
            if( !(*it)->mDemandVertices.empty() ) {
                (*mrktIter)->mImpliedVertices.insert( (*it)->getFirstDemandVertex() );
            }
            
            // If the market is solved then there really is no dependency on this
            // vertex; the dependent items do not need to wait for it to calculate
            // a price rather they get the solver price instead.  We do however
            // still need to make sure that they are recalculated when the solver
            // changes the price for this market.  We do that by adding them as
            // "implied" vertices to calculate.
            for( CItemIterator dependIt = (*it)->mDependentList.begin(); dependIt != (*it)->mDependentList.end(); ++dependIt ) {
                if( (*dependIt)->mName == "land-allocator" ) {
                    // The land allocator is a special case since it must work in conjunction
                    // with the ag supply sectors to set supplies into the marketplace.
                    // So we imply that any time the land allocator is recalculated then
                    // all of the items which directly depend on it must also be recalculated.
                    if( !(*it)->mPriceVertices.empty() ) {
                        (*it)->getLastPriceVertex()->mOutEdges.push_back( (*dependIt)->getFirstDemandVertex() );
                        ++numDependencies[ (*dependIt)->getFirstDemandVertex() ];
                        (*dependIt)->getLastDemandVertex()->mOutEdges.push_back( (*it)->getFirstDemandVertex() );
                        ++numDependencies[ (*it)->getFirstDemandVertex() ];
                    }
                    else {
                        // These implied in edges will be added to the list of verticies to calculate
                        // any time the land-allocator needs to be recalculated for any reason.
                        (*mrktIter)->mImpliedVertices.insert( (*dependIt)->getFirstDemandVertex() );
                    }
                }
                else {
                    if( !(*dependIt)->mPriceVertices.empty() ) {
                        (*mrktIter)->mImpliedVertices.insert( (*dependIt)->getFirstPriceVertex() );
                    }
                    else if( !(*dependIt)->mDemandVertices.empty() ) {
                        // Could get here for instance if a resource has dependencies
                        (*mrktIter)->mImpliedVertices.insert( (*dependIt)->getFirstDemandVertex() );
                    }
                }
            }
            // if this depenency can not have it's dependency broken then we can't skip adding
            // it's dependents even if it is solved
            if( (*it)->mCanBreakCycle ) {
                continue;
            }
        }
        
        if( (*it)->mDemandVertices.empty() ) {
            // Relies on an unsolved tax market so there is nothing to calculate.
            continue;
        }
        
        if( (*it)->mDependentList.empty() && !(*it)->mPriceVertices.empty() ) {
            // This is the fold back point, or final demand, so loop back on self
            // by linking the final price calculation to it's demand calculation.
            (*it)->getLastPriceVertex()->mOutEdges.push_back( (*it)->getFirstDemandVertex() );
            ++numDependencies[ (*it)->getFirstDemandVertex() ];
        }
        else {
            for( CItemIterator dependIt = (*it)->mDependentList.begin(); dependIt != (*it)->mDependentList.end(); ++dependIt ) {
                // Typical dependency case where we add an edge from the last price vertex
                // of this item to the first price vertex of it's dependent's item.
                // For the demand that is reversed so the last demand vertex of the
                // dependent's item links to the first demand vertex of this item.
                if( !(*it)->mPriceVertices.empty() ) {
                    if( (*dependIt)->mPriceVertices.empty() ) {
                        if( !(*dependIt)->mDemandVertices.empty() ) {
                        // Could get here for instance if a resource has dependencies
                        (*it)->getLastPriceVertex()->mOutEdges.push_back( (*dependIt)->getFirstDemandVertex() );
                            ++numDependencies[ (*dependIt)->getFirstDemandVertex() ];
                        }
                        // else would get here if we had dependencies between two items which do
                        // not have anything to calculate yet are unsolved such as linked markets.
                    }
                    else {
                        (*it)->getLastPriceVertex()->mOutEdges.push_back( (*dependIt)->getFirstPriceVertex() );
                        ++numDependencies[ (*dependIt)->getFirstPriceVertex() ];
                    }
                }
                if( !(*dependIt)->mDemandVertices.empty()) {
                    (*dependIt)->getLastDemandVertex()->mOutEdges.push_back( (*it)->getFirstDemandVertex() );
                    ++numDependencies[ (*it)->getFirstDemandVertex() ];
                }
            }
        }
    }
    
    // Do some error checking for activities that are not related to any other
    // activities in the model as it may be an indication of misconfiguration.
    ILogger& depLog = ILogger::getLogger( "dependency_finder_log" );
    for( CItemIterator it = mDependencyItems.begin(); it != mDependencyItems.end(); ++it ) {
        if( !(*it)->mHasIncomingDependency && (*it)->mDependentList.empty() ) {
            depLog.setLevel( ILogger::SEVERE );
            depLog << (*it)->mName << " in " << (*it)->mLocatedInRegion << " is not related to any other activities." << endl;
        }
    }

    // Before we can create an ordering we must take care of any item which have a
    // self dependence by converting them to solved via trial markets
    for( CItemIterator it = mDependencyItems.begin(); it != mDependencyItems.end(); ++it ) {
        if( (*it)->mHasSelfDependence ) {
            depLog.setLevel( ILogger::WARNING );
            depLog << "Creating trial markets for " << (*it)->mName << " in " << (*it)->mLocatedInRegion
                   << " due to self dependency." << endl;
            createTrialsForItem( it, numDependencies );
        }
    }
    
    // A map which will be populated with vertices in cycles the first time one is
    // found and can be used there after to break cycles as needed while
    // performing the topological sort.
    CalcVertexCountMap totalVisits;

    // Create a global ordering by performing a topological sort on the graph.
    // Cycles will be broken when they are no longer possible to avoid.
    while( !numDependencies.empty() ) {
        // We can clear a vertex and add it to the ordering list if the number of
        // dependencies on it is equal to zero.
        vector<CalcVertex*> justRemoved;
        typedef CalcVertexCountMap::iterator NumDepIterator;
        for( NumDepIterator it = numDependencies.begin(); it != numDependencies.end(); ) {
            if( (*it).second == 0 ) {
                justRemoved.push_back( (*it).first );
                numDependencies.erase( it++ );
            }
            else {
                ++it;
            }
        }
        for( VertexIterator removedIter = justRemoved.begin(); removedIter != justRemoved.end(); ++removedIter ) {
            // When a vertex is cleared we can reduce the number of remaining
            // dependencies from it's direct dependents.
            for( VertexIterator depIter = (*removedIter)->mOutEdges.begin(); depIter != (*removedIter)->mOutEdges.end(); ++ depIter ) {
                NumDepIterator dependCountIter = numDependencies.find( *depIter );
                if( dependCountIter != numDependencies.end() ) {
                    --(*dependCountIter).second;
                }
            }
            mGlobalOrdering.push_back( (*removedIter)->mCalcItem );
        }
        
        // We are no longer able to find any vertices without any dependencies and
        // all vertices have not yet been cleared.  This means we are now forced
        // to break a cycle.
        if( justRemoved.empty() && !numDependencies.empty() ) {
            depLog.setLevel( ILogger::WARNING );
            depLog << "Cycle detected attempting to break it." << endl;

            list<CalcVertex*> hasVisited;
            if( totalVisits.empty() ) {
                // We will need to create the list of possible vertices to use to break the
                // cycle.  We will do this by finding the strongly coupled components of the
                // graph that we have left, or put another way the vertices that are part of
                // a cycle.  Once we have these we can quickly find a suitable vertex to break
                // the cycle without having to search through a potentially large number of
                // extraneous vertices.
                int index = 0;
                for( NumDepIterator it = numDependencies.begin(); it != numDependencies.end(); ++it ) {
                    if( (*it).first->mIndex == -1 ) {
                        findStronglyConnected( (*it).first, index, hasVisited, totalVisits );
                    }
                }
            }
            else {
                // Since we have already determined which vertices are in a cycle we 
                // can just use them again.  We just need to update the list to remove
                // vertices which have been cleared since the last time it was used.
                for( NumDepIterator it = totalVisits.begin(); it != totalVisits.end(); ) {
                    if( numDependencies.find( (*it).first ) == numDependencies.end() ) {
                        totalVisits.erase( it++ );
                    }
                    else {
                        (*it).second = 0;
                        ++it;
                    }
                }
            }

            // The vertex chosen to break the cycle will be the one most visited
            // when searching the graph from all of the vertices which are part of
            // a strongly connected component.
            for( NumDepIterator it = totalVisits.begin(); it != totalVisits.end(); ++it ) {
                hasVisited.clear();
                markCycles( (*it).first, hasVisited, totalVisits );
            }

            // We will choose the vertex with the most visits to break a cycle unless
            // it has a dependency which can not be broken when creating trial markets.
            int max = 0;
            CalcVertex* maxVertex = 0;
            for( NumDepIterator it = totalVisits.begin(); it != totalVisits.end(); ++it ) {            
                if( (*it).first->mDepItem->mCanBreakCycle && (*it).second > max ) {
                    maxVertex = (*it).first;
                    max = (*it).second;
                }
            }
            if( !maxVertex ) {
                depLog.setLevel( ILogger::SEVERE );
                depLog << "Could not find an unbreakable item to break the cycle." << endl;
                exit( 1 );
            }
            
            depLog << "The following activity has been chosen to break the cycle: "
                   << maxVertex->mCalcItem->getDescription() << endl;
            
            // Now that we identified the best vertex to remove we must find the
            // corresponding dependency item since the current strategy for breaking
            // cycles requires that we solve the price and demand together.
            CItemIterator maxItem = mDependencyItems.end();
            for( CItemIterator it = mDependencyItems.begin(); it != mDependencyItems.end() && maxItem == mDependencyItems.end(); ++it ) {
                // The max vertex could be either a price or demand vertex so we
                // must check both vectors.
                for( VertexIterator vertexIter = (*it)->mPriceVertices.begin(); vertexIter != (*it)->mPriceVertices.end() && maxItem == mDependencyItems.end(); ++vertexIter ) {
                    if( *vertexIter == maxVertex ) {
                        maxItem = it;
                    }
                }
                for( VertexIterator vertexIter = (*it)->mDemandVertices.begin(); vertexIter != (*it)->mDemandVertices.end() && maxItem == mDependencyItems.end(); ++vertexIter ) {
                    if( *vertexIter == maxVertex ) {
                        maxItem = it;
                    }
                }
            }
            
            // Reset this item to be solved via trials and adjust the depenencies accordingly.
            createTrialsForItem( maxItem, numDependencies );

            // Remove both the price and demand vertex from totalVisits since they can not be
            // used again to try to break a dependency.
            NumDepIterator delIt = totalVisits.find( (*maxItem)->getFirstDemandVertex() );
            if( delIt != totalVisits.end() ) {
                totalVisits.erase( delIt );
            }
            delIt = totalVisits.find( (*maxItem)->getFirstPriceVertex() );
            if( delIt != totalVisits.end() ) {
                totalVisits.erase( delIt );
            }
        }
    }
    
    // All vertices are now cleared and we have a global ordering.
    depLog.setLevel( ILogger::DEBUG );
    depLog << "Global Ordering:" << endl;
    for( vector<IActivity*>::iterator it = mGlobalOrdering.begin(); it != mGlobalOrdering.end(); ++it ) {
        depLog << "- " << (*it)->getDescription() << endl;
    }
}

/*!
 * \brief An implementation of Tarjan's strongly connected components algorithm which
 *        is used to identify vertices that are part of a cycle.
 * \details This is an efficient algorithm to quickly identify activities that are
 *          part of a cycle and can give us a small subset of vertices to run
 *          markCycles on to figure out which is the best to use to break the cycles.
 * \param aCurrVertex The current vertex being visited.
 * \param aMaxIndex The current max index which can be used to give an index to an 
 *                  unprocessed vertex.
 * \param aHasVisited The list of vertices which make up the current search path.
 * \param aTotalVisits The map of vertices to the total number of times that node
 *                      has been visited to be used in markCycles but will be
 *                      initialized here.
 */
void MarketDependencyFinder::findStronglyConnected( CalcVertex* aCurrVertex, int& aMaxIndex,
                                                    list<CalcVertex*>& aHasVisited,
                                                    CalcVertexCountMap& aTotalVisits ) const
{
    // Initialize the newly found vertex with an index, increase the max count,
    // and add it to the current search patch.
    aCurrVertex->mIndex = aCurrVertex->mLowLink = aMaxIndex++;
    aHasVisited.push_back( aCurrVertex );

    for( VertexIterator it = aCurrVertex->mOutEdges.begin(); it != aCurrVertex->mOutEdges.end(); ++it ) {
        if( (*it)->mIndex == -1 ) {
            // This successor has not been processed recurse on it.
            findStronglyConnected( *it, aMaxIndex, aHasVisited, aTotalVisits );
            aCurrVertex->mLowLink = min( aCurrVertex->mLowLink, (*it)->mLowLink );
        }
        else if( find( aHasVisited.begin(), aHasVisited.end(), *it ) != aHasVisited.end() ) {
            // This successor is in the path thus we have found a cycle.
            aCurrVertex->mLowLink = min( aCurrVertex->mLowLink, (*it)->mIndex );
        }
    }

    // If the current vertex was part of a cycle then initialize aTotalVisits
    // with the members of the strongly connected component.
    /*
     * \note We are not currently keeping track of each set of strongly connected
     *       components and instead are just interested in any vertex that is part
     *       of a set of strongly connected components.  This is because we use
     *       markCycles to perform searches on this set to understand how they relate
     *       however if we want replace markCycles with a method that does not require
     *       searching this information may be valuable.
     */
    if( aCurrVertex->mIndex == aCurrVertex->mLowLink ) {
        if( aHasVisited.back() != aCurrVertex ) {
            aTotalVisits[ aCurrVertex ] = 0;
        }
        while( aHasVisited.back() != aCurrVertex ) {
            CalcVertex* w = aHasVisited.back();
            aTotalVisits[ w ] = 0;
            aHasVisited.pop_back();
        }
        aHasVisited.pop_back();
    }
}

/*!
 * \brief A helper method to perform a depth first search and count the total
 *        number of times each vertex has been visited in a cycle.
 * \details Since this graph can have a cycle the stop point for searching is
 *          when the current search path has returned to a vertex that is already
 *          in the search path.  Note an arbitrary threshold is placed on the number
 *          of times a vertex can be found to be in a cycle to avoid excessive searching
 *          when a good vertex to break a cycle has already been found.
 * \param aCurrVertex The current vertex being visited.
 * \param aHasVisited The list of vertices which make up the current search path.
 * \param aTotalVisits The map of vertices to the total number of times that node
 *                      has been visited.
 * \return The maximum number of cycle-visits so far.
 */
int MarketDependencyFinder::markCycles( CalcVertex* aCurrVertex, list<CalcVertex*>& aHasVisited,
                                        CalcVertexCountMap& aTotalVisits ) const
{
    if( aTotalVisits.find( aCurrVertex ) == aTotalVisits.end() ) {
        return 0;
    }
    typedef CalcVertexCountMap::iterator NumDepIterator;
    if( find( aHasVisited.begin(), aHasVisited.end(), aCurrVertex ) != aHasVisited.end() ) {
        // This search path has just formed a cycle, increase the visit count and
        // indicate that this path leads to a cycle.
        NumDepIterator it = aTotalVisits.find( aCurrVertex );
        if( it == aTotalVisits.end() ) {
            aTotalVisits[ aCurrVertex ] = 1;
        }
        else {
            ++aTotalVisits[ aCurrVertex ];
        }
        return aTotalVisits[ aCurrVertex ];
    }
    else {
        // Have not yet created a cycle so add this vertex to the search path and
        // keep searching.
        aHasVisited.push_back( aCurrVertex );
        const int MAX_CYCLE_VISITS = 1000;
        int cycleVisits = 0;
        for( VertexIterator it = aCurrVertex->mOutEdges.begin(); it != aCurrVertex->mOutEdges.end() && cycleVisits < MAX_CYCLE_VISITS; ++it ) {
            int currCycleVisits = markCycles( *it, aHasVisited, aTotalVisits );
            cycleVisits = max( cycleVisits, currCycleVisits );
        }
        aHasVisited.pop_back();

        // If any searches from this vertex eventually leads to a cycle then we
        // must increase the visit count for this vertex.
        if( cycleVisits ) {
            NumDepIterator it = aTotalVisits.find( aCurrVertex );
            if( it == aTotalVisits.end() ) {
                aTotalVisits[ aCurrVertex ] = 1;
            }
            else {
                ++aTotalVisits[ aCurrVertex ];
            }
        }
        return cycleVisits;
    }
}

/*!
 * \brief Reset a market identified by it's iterator into the dependency items to a solved
 *        market by using trial price/demand markets.
 * \details We instruct the marketplace to the the heavy lifting to restructure the markets.
 *          However the dependencies still need to be adjusted now that this market is solved.
 *          Namely:
 *            - All dependencies out of the price vertex are removed (only recalculated when
 *               the solver changes the price).
 *            - All dependencies into the demand vertex are removed.
 *            - A direct dependency between the price vertex and the demand must be added back.
 * \param aItemToReset An iterator into the dependency items that identifies which market to reset.
 * \param aNumDependencies The current count of dependencies on each activity which is used in
 *                         createOrdering.  This will need to be updated to reflect the changed
 *                         dependencies since the market is now solved.
 */
void MarketDependencyFinder::createTrialsForItem( CItemIterator aItemToReset, CalcVertexCountMap& aNumDependencies ) {
    // Instruct the marketplace to go ahead and create solved trial price and demand
    // markets for this good.
    const int demandMrkt = mMarketplace->resetToPriceMarket( (*aItemToReset)->mLinkedMarket );
    if( demandMrkt < 0 ) {
        ILogger& depLog = ILogger::getLogger( "dependency_finder_log" );
        depLog.setLevel( ILogger::SEVERE );
        depLog << "Unable to break the cycle." << endl;
        abort();
    }
    (*aItemToReset)->mIsSolved = true;

    // Remove dependencies on the demand vertex now that it is solved.
    // Dependencies on the price vertex must remain since it is responsible
    // for setting it's actual price into the marketplace.
    vector<CalcVertex*> fixedOutputVertices;
    for( CItemIterator it = mDependencyItems.begin(); it != mDependencyItems.end(); ++it ) {
        for( VertexIterator vertexIter = (*it)->mDemandVertices.begin(); vertexIter != (*it)->mDemandVertices.end(); ++vertexIter ) {
            VertexIterator dependIter = find( (*vertexIter)->mOutEdges.begin(), (*vertexIter)->mOutEdges.end(), (*aItemToReset)->getFirstDemandVertex() );
            if( dependIter != (*vertexIter)->mOutEdges.end() ) {
                if( boost::algorithm::ends_with( (*vertexIter)->mCalcItem->getDescription(), "-fixed-output" ) ) {
                    fixedOutputVertices.push_back( *vertexIter );
                }
                else {
                    (*vertexIter)->mOutEdges.erase( dependIter );
                }
            }
        }
    }
    aNumDependencies[ (*aItemToReset)->getFirstDemandVertex() ] = fixedOutputVertices.size();

    // Lookup/create the associated market linkages to the price and demand
    // vertices.
    auto_ptr<MarketToDependencyItem> marketToDep( new MarketToDependencyItem( (*aItemToReset)->mLinkedMarket ) );
    MarketToDepIterator priceMrktIter = mMarketsToDep.find( marketToDep.get() );
    assert( priceMrktIter != mMarketsToDep.end() );
    MarketToDepIterator demandMrktIter = mMarketsToDep.insert( new MarketToDependencyItem( demandMrkt ) ).first;

    // The price/demand vertices are obviously implied when the it's corresponding
    // price/demand trial price changes.
    (*priceMrktIter)->mImpliedVertices.insert( (*aItemToReset)->getFirstPriceVertex() );
    (*demandMrktIter)->mImpliedVertices.insert( (*aItemToReset)->getFirstDemandVertex() );
    
    for( VertexIterator fixedVertexIt = fixedOutputVertices.begin(); fixedVertexIt != fixedOutputVertices.end(); ++fixedVertexIt ) {
        (*demandMrktIter)->mImpliedVertices.insert( *fixedVertexIt );
    }

    // Make dependencies from these price vertices implied only.
    for( VertexIterator dependIter = (*aItemToReset)->getLastPriceVertex()->mOutEdges.begin(); dependIter != (*aItemToReset)->getLastPriceVertex()->mOutEdges.end(); ) {
        CalcVertexCountMap::iterator dependCountIter = aNumDependencies.find( *dependIter );
        if( dependCountIter != aNumDependencies.end() ) {
            --(*dependCountIter).second;
        }
        (*priceMrktIter)->mImpliedVertices.insert( *dependIter );
        dependIter = (*aItemToReset)->getLastPriceVertex()->mOutEdges.erase( dependIter );
    }

    // The price vertex still must be calculated before the demand vertex
    // so add that dependency back in.
    (*aItemToReset)->getLastPriceVertex()->mOutEdges.push_back( (*aItemToReset)->getFirstDemandVertex() );
    if( aNumDependencies.find( (*aItemToReset)->getFirstPriceVertex() ) != aNumDependencies.end() ) {
        ++aNumDependencies[ (*aItemToReset)->getFirstDemandVertex() ];
    }
}

