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
#include "containers/include/market_dependency_finder.h"
#include "util/logger/include/ilogger.h"
#include "marketplace/include/marketplace.h"
#include "marketplace/include/market_locator.h"
#include "marketplace/include/market.h"
#include "containers/include/iactivity.h"

using namespace std;

/*!
 * \brief Constructor.
 * \param aMarketplace The marketplace object in which this object is contained.
 */
MarketDependencyFinder::MarketDependencyFinder( Marketplace* aMarketplace ):
mMarketplace( aMarketplace )
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
 * \return Wether a new entry was created to by adding this dependency, in other
 *         words it wasn't duplicative.
 */
bool MarketDependencyFinder::addDependency( const string& aDependentName,
                                            const string& aDependentRegion,
                                            const string& aDependencyName,
                                            const string& aDependencyRegion )
{
    // Find/create a DependencyItem entry for the dependent item
    auto_ptr<DependencyItem> item( new DependencyItem( aDependentName, aDependentRegion ) );
    ItemIterator dependentIter = mDependencyItems.find( item.get() );
    if( dependentIter == mDependencyItems.end() ) {
        dependentIter = mDependencyItems.insert( item.release() ).first;
    }
    
    // Find/create a DependencyItem entry for the dependency 
    item.reset( new DependencyItem( aDependencyName, aDependencyRegion ) );
    ItemIterator dependencyIter = mDependencyItems.find( item.get() );
    if( dependencyIter == mDependencyItems.end() ) {
        dependencyIter = mDependencyItems.insert( item.release() ).first;
    }
    
    // These are kept track of by adding to the list of dependents for the
    // dependency item.
    return (*dependencyIter)->mDependentList.insert( *dependentIter ).second;
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
        // Could not match up the activity to a dependency name.
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Could not order activity: " << aActivityName <<  " in " << aRegionName << endl;
        delete aDemandActivity;
        delete aPriceActivity;
        return;
    }
    
    (*itemIter)->mDemandVertices.push_back( new CalcVertex( aDemandActivity ) );
    // The price activity may not be necessary.
    if( aPriceActivity ) {
        (*itemIter)->mPriceVertices.push_back( new CalcVertex( aPriceActivity ) );
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
        // We must generate the full list of nodes to calculate.  The createOrdering
        // method has created a link from markets to "entry points" into the graph.
        // We must then do a search on the graph from those entry points to come up
        // with a full set of items which must be recalculated.
        // To ensure the set of items are in order we can utilize the already created
        // global ordering.
        
        // Find the entry points into the graph for the given market.
        auto_ptr<MarketToDependencyItem> marketToDep( new MarketToDependencyItem( aMarketNumber ) );
        CMarketToDepIterator mrktIter = mMarketsToDep.find( marketToDep.get() );
        if( mrktIter == mMarketsToDep.end() ) {
            // Somehow this market was not linked to any entry points into the graph.
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Could not find market: " << mMarketplace->markets[ aMarketNumber ][ 0 ]->getName()
                    << " to get an ordering for." << endl;
            exit( 1 );
        }
        
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
        return orderedListForMarket;
    }
}

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
    return ( aLHS->mName + aLHS->mLocatedInRegion )
        < ( aRHS->mName + aRHS->mLocatedInRegion );
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
    
    // The land allocator is a special case in that it must work together with it's
    // associated ag supply sectors to set prices from and add supplies to the marketplace
    // even though those sectors may be solved.
    map<DependencyItem*, vector<DependencyItem*> > landAllocatorMap;
    for( CItemIterator it = mDependencyItems.begin(); it != mDependencyItems.end(); ++it ) {
        for( CItemIterator dependIt = (*it)->mDependentList.begin(); dependIt != (*it)->mDependentList.end(); ++dependIt ) {
            if( (*dependIt)->mName == "land-allocator" ) {
                landAllocatorMap[ *dependIt ].push_back( *it );
            }
        }
    }
    
    // First associate markets to their corresponding dependency items
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
            for( int period = 1; period < mMarketplace->markets[ marketNumber ].size() && !isSolved; ++period ) {
                isSolved = mMarketplace->markets[ marketNumber ][ period ]->isSolvable();
            }
            (*it)->mIsSolved = isSolved;
        }
    }
    
    // Initialize vertices in the graph.
    map<CalcVertex*, int> numDependencies;
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
                    (*it)->getLastPriceVertex()->mOutEdges.push_back( (*dependIt)->getFirstDemandVertex() );
                    ++numDependencies[ (*dependIt)->getFirstDemandVertex() ];
                    (*dependIt)->getLastDemandVertex()->mOutEdges.push_back( (*it)->getFirstDemandVertex() );
                    ++numDependencies[ (*it)->getFirstDemandVertex() ];
                    map<DependencyItem*, vector<DependencyItem*> >::iterator laExtrasIter = landAllocatorMap.find( *dependIt );
                    assert( laExtrasIter != landAllocatorMap.end() );
                    for( vector<DependencyItem*>::iterator laDepIter = (*laExtrasIter).second.begin(); laDepIter != (*laExtrasIter).second.end(); ++laDepIter ) {
                        if( !(*laDepIter)->mPriceVertices.empty() ) {
                            (*mrktIter)->mImpliedVertices.insert( (*laDepIter)->getFirstPriceVertex() );
                        }
                    }
                    
                }
                else {
                    if( !(*dependIt)->mPriceVertices.empty() ) {
                        (*mrktIter)->mImpliedVertices.insert( (*dependIt)->getFirstPriceVertex() );
                    }
                }
            }
            continue;
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
                        // Could get here for instance if a resource has dependencies
                        (*it)->getLastPriceVertex()->mOutEdges.push_back( (*dependIt)->getFirstDemandVertex() );
                        ++numDependencies[ (*dependIt)->getFirstDemandVertex() ];
                    }
                    else {
                        (*it)->getLastPriceVertex()->mOutEdges.push_back( (*dependIt)->getFirstPriceVertex() );
                        ++numDependencies[ (*dependIt)->getFirstPriceVertex() ];
                    }
                }
                (*dependIt)->getLastDemandVertex()->mOutEdges.push_back( (*it)->getFirstDemandVertex() );
                ++numDependencies[ (*it)->getFirstDemandVertex() ];
            }
        }
    }
    
    // Create a global ordering by performing a topological sort on the graph.
    // Cycles will be broken when they are no longer possible to avoid.
    ILogger& depLog = ILogger::getLogger( "dependency_finder_log" );
    while( !numDependencies.empty() ) {
        // We can clear a vertex and add it to the ordering list if the number of
        // dependencies on it is equal to zero.
        vector<CalcVertex*> justRemoved;
        typedef map<CalcVertex*, int>::iterator NumDepIterator;
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
                --numDependencies[ *depIter ];
            }
            mGlobalOrdering.push_back( (*removedIter)->mCalcItem );
        }
        
        // We are no longer able to find any vertices without any dependencies and
        // all vertices have not yet been cleared.  This means we are now forced
        // to break a cycle.
        if( justRemoved.empty() && !numDependencies.empty() ) {
            depLog.setLevel( ILogger::WARNING );
            depLog << "Cycle detected attempting to break it." << endl;
            // The vertex chosen to break the cycle will be the one most visited
            // when searching the graph from all of the vertices which have not
            // yet been cleared.
            map<CalcVertex*, int> totalVisits;
            int numSearches = 0;
            /*!
             * \warning Searching from every vertex to every other vertex to find a
             *          cycle is overkill.  Instead we are limiting the number of
             *          searches to an arbitrary threshold however this provides
             *          no guarantees that it will find the cycle.  In practice it
             *          tends to be more than enough and significantly reduces
             *          runtime.
             */
            const double MAX_SEARCHES = 50;
            for( NumDepIterator it = numDependencies.begin(); it != numDependencies.end() && numSearches < MAX_SEARCHES; ++it ) {
                list<CalcVertex*> hasVisited;
                markCycles( (*it).first, hasVisited, totalVisits );
                ++numSearches;
            }
            int max = 0;
            CalcVertex* maxVertex = 0;
            for( NumDepIterator it = totalVisits.begin(); it != totalVisits.end(); ++it ) {            
                if( (*it).second > max ) {
                    maxVertex = (*it).first;
                    max = (*it).second;
                }
            }
            if( !maxVertex ) {
                depLog.setLevel( ILogger::SEVERE );
                depLog << "Unable to find a vertex in a cycle with maximum searches of "
                       << MAX_SEARCHES << "." << endl;
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
            
            // Instruct the marketplace to go ahead and create solved trial price and demand
            // markets for this good.
            const int demandMrkt = mMarketplace->resetToPriceMarket( (*maxItem)->mLinkedMarket );
            if( demandMrkt < 0 ) {
                depLog.setLevel( ILogger::SEVERE );
                depLog << "Unable to break the cycle." << endl;
                exit( 1 );
            }
            (*maxItem)->mIsSolved = true;
            
            // Remove dependencies on the demand vertex now that it is solved.
            // Dependencies on the price vertex must remain since it is responsible
            // for setting it's actual price into the marketplace.
            for( CItemIterator it = mDependencyItems.begin(); it != mDependencyItems.end(); ++it ) {
                for( VertexIterator vertexIter = (*it)->mDemandVertices.begin(); vertexIter != (*it)->mDemandVertices.end(); ++vertexIter ) {
                    VertexIterator dependIter = find( (*vertexIter)->mOutEdges.begin(), (*vertexIter)->mOutEdges.end(), (*maxItem)->getFirstDemandVertex() );
                    if( dependIter != (*vertexIter)->mOutEdges.end() ) {
                        (*vertexIter)->mOutEdges.erase( dependIter );
                    }
                }
            }
            numDependencies[ (*maxItem)->getFirstDemandVertex() ] = 0;
            
            // Lookup/create the associated market linkages to the price and demand
            // vertices.
            auto_ptr<MarketToDependencyItem> marketToDep( new MarketToDependencyItem( (*maxItem)->mLinkedMarket ) );
            MarketToDepIterator priceMrktIter = mMarketsToDep.find( marketToDep.get() );
            assert( priceMrktIter != mMarketsToDep.end() );
            MarketToDepIterator demandMrktIter = mMarketsToDep.insert( new MarketToDependencyItem( demandMrkt ) ).first;
            
            // The price/demand vertices are obviously implied when the it's corresponding
            // price/demand trial price changes.
            (*priceMrktIter)->mImpliedVertices.insert( (*maxItem)->getFirstPriceVertex() );
            (*demandMrktIter)->mImpliedVertices.insert( (*maxItem)->getFirstDemandVertex() );
            
            // Make dependencies from these price vertices implied only.
            for( VertexIterator dependIter = (*maxItem)->getLastPriceVertex()->mOutEdges.begin(); dependIter != (*maxItem)->getLastPriceVertex()->mOutEdges.end(); ) {
                --numDependencies[ *dependIter ];
                (*priceMrktIter)->mImpliedVertices.insert( *dependIter );
                dependIter = (*maxItem)->getLastPriceVertex()->mOutEdges.erase( dependIter );
            }
            
            // The price vertex still must be calculated before the demand vertex
            // so add that dependency back in.
            (*maxItem)->getLastPriceVertex()->mOutEdges.push_back( (*maxItem)->getFirstDemandVertex() );
            ++numDependencies[ (*maxItem)->getFirstDemandVertex() ];
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
 * \brief A helper method to perform a depth first search and count the total
 *        number of times each vertex has been visited in a cycle.
 * \details Since this graph can have a cycle so the stop point for searching is 
 *          when the current search path has returned to a vertex that is already
 *          in the search path.
 * \param aCurrVertex The current vertex being visited.
 * \param aHasVisited The list of vertices which make up the current search path.
 * \param aTotalVisists The map of vertices to the total number of this that node
 *                      has been visited.
 * \return Whether the search eventually lead to a cycle.
 */
bool MarketDependencyFinder::markCycles( CalcVertex* aCurrVertex, list<CalcVertex*>& aHasVisited,
                                         map<CalcVertex*, int>& aTotalVisists ) const
{
    typedef map<CalcVertex*, int>::iterator NumDepIterator;
    if( find( aHasVisited.begin(), aHasVisited.end(), aCurrVertex ) != aHasVisited.end() ) {
        // This search path has just formed a cycle, increase the visit count and
        // indicate that this path leads to a cycle.
        NumDepIterator it = aTotalVisists.find( aCurrVertex );
        if( it == aTotalVisists.end() ) {
            aTotalVisists[ aCurrVertex ] = 1;
        }
        else {
            ++aTotalVisists[ aCurrVertex ];
        }
        return true;
    }
    else {
        // Have not yet created a cycle so add this vertex to the search path and
        // keep searching.
        aHasVisited.push_back( aCurrVertex );
        bool didFindCycle = false;
        for( VertexIterator it = aCurrVertex->mOutEdges.begin(); it != aCurrVertex->mOutEdges.end(); ++it ) {
            didFindCycle |= markCycles( *it, aHasVisited, aTotalVisists );
        }
        aHasVisited.pop_back();
        
        // If any searches from this vertex eventually leads to a cycle then we
        // must increase the visit count for this vertex.
        if( didFindCycle ) {
            NumDepIterator it = aTotalVisists.find( aCurrVertex );
            if( it == aTotalVisists.end() ) {
                aTotalVisists[ aCurrVertex ] = 1;
            }
            else {
                ++aTotalVisists[ aCurrVertex ];
            }
        }
        return didFindCycle;
    }
}
