/*! 
* \file dependency_finder.cpp
* \ingroup Objects
* \brief The DependencyFinder class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include "containers/include/dependency_finder.h"
#include "util/logger/include/ilogger.h"
#include "marketplace/include/marketplace.h"

#include <stack>
#include <list>
#include <algorithm>

using namespace std;

/*! Constructor
* \param aMarketplace Pointer to the marketplace for the region.
* \param aRegionName Name of the region containing the dependency finder.
*/
DependencyFinder::DependencyFinder( Marketplace* aMarketplace, const string& aRegionName ):
mMarketplace( aMarketplace ),
mRegionName( aRegionName )
{
}

/*! \brief Add a dependency for a sector.
* \details In MiniCAM, a sector has a dependency on another sector when it uses
*          that sector as an input to its production. A single sector may have
*          dependencies on many goods, as each technology within each subsector
*          may consume a different good. This function is used to mark a single
*          dependency, from aSector to aDependency.
* \param aSector Name of the sector which has a new dependency.
* \param aDependency name of the item aSector is dependent on.
* \return Whether the dependency was added to the matrix.
*/
bool DependencyFinder::addDependency( const string& aSector, const string& aDependency ){
    // Check if the sector is already in the mapping of sector name to matrix index.
    SectorIndexMap::iterator sectorLocation = mSectorIndices.find( aSector );
    if( sectorLocation == mSectorIndices.end() ){
        // Update the sector location iterator after the item is added.
        sectorLocation = addTrackedItem( aSector );
    }

    // Check if the dependency is already in the mapping.
    SectorIndexMap::iterator dependencyLocation = mSectorIndices.find( aDependency );
    if( dependencyLocation == mSectorIndices.end() ){
        // Update the dependency location iterator after the item is added.
        dependencyLocation = addTrackedItem( aDependency );
    }

    // The matrix is now setup correctly, add the dependency.
    assert( mDependencyMatrix.size() > sectorLocation->second );
    assert( mDependencyMatrix[ sectorLocation->second ].size() > dependencyLocation->second );

    // Check if the dependency already exists.
    if( mDependencyMatrix[ sectorLocation->second ][ dependencyLocation->second ] ){
        return false;
    }
    // Add the dependency and return that it was a new dependency.
    mDependencyMatrix[ sectorLocation->second ][ dependencyLocation->second ] = true;
    return true;
}

/*! \brief Find an ordering of the sectors in the dependency finder which orders
*          each sector before each sector that depends on it.
* \details This is referred to as a topological sort. The algorithm is as
*          follows: Search the adjacency matrix for a vertice, in this
*          implementation a column in the matrix, with no dependencies. If there
*          is none, the graph has a cycle and cannot currently be ordered. To
*          correct this, call findAndBreakCycle to remove a single cycle from
*          the matrix so the algorithm can continue. Next, add the vertice found
*          in the previous step to the ordering, and remove it and all
*          dependencies on it from the adjacency matrix. Start over at the first
*          step. Repeat this process until there are no nodes left in the graph.
*          Once this function has been called, the caller may then call
*          getOrdering to return the ordered vector.
* \note This function removes nodes by setting the removed flag to true instead
*       of actually removing the vertice.
* \todo The control flow of this function could be less confusing.
* \sa getOrdering
*/
void DependencyFinder::createOrdering() {
    // If there is an existing stored ordering, clear it.
    mOrdering.clear();

    // Create a vector which marks which vertices are removed.
    vector<bool> removed( mDependencyMatrix.size() );

    // Search until the ordering contains all vertices in the matrix.
    while( mOrdering.size() < mDependencyMatrix.size() ){
        unsigned int verticeToRemove = INT_MAX;

        // Search for a vertice with no dependencies.
        for( unsigned int i = 0; i < mDependencyMatrix.size(); ++i ){
            // Only search the vertice if it has not been removed.
            if( removed[ i ] ){
                continue;
            }
            // Check for any dependencies.
            bool depFound = false;
            for( unsigned int j = 0; j < mDependencyMatrix.size(); ++j ){
                // Check if there is a dependency at this location.
                if( !removed[ j ] && mDependencyMatrix[ i ][ j ] ){
					// Found a dependency so break out of the loop to stop
                    // searching this column and move onto the next.
                    depFound = true;
                    break;
                }
            }
            // If we did not find a dependency, set the index to remove and
            // break the loop so the sector can be removed. Otherwise continue
            // searching.
            if( !depFound ){
                verticeToRemove = i;
                break;
            }
        }

        // Check if we found a vertice to remove.
        if( verticeToRemove == INT_MAX ){
            // Since there was no vertice with zero dependencies, this graph has
            // a cycle.
            ILogger& depFinderLog = ILogger::getLogger( "dep_finder_log" );
            depFinderLog.setLevel( ILogger::DEBUG );
            depFinderLog << "Graph has at least one cycle, attempting to remove." << endl;
            // should the removed vector be passed to find and break cycle?
            if( !findAndBreakCycle() ){
                // There was a cycle but it could not be broken.
				ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::ERROR );
				depFinderLog.setLevel( ILogger::ERROR );
                mainLog << "Failed to remove cycle from dependency matrix. Sector ordering failed." << endl;
                depFinderLog << "Failed to remove cycle from dependency matrix. Sector ordering failed." << endl;
                mOrdering.clear();
                return;
            }
        }
        else {
            // Add the vertice found to the ordering and remove it from the
            // matrix.
            mOrdering.push_back( getNameFromIndex( verticeToRemove ) );
            removed[ verticeToRemove ] = true;
        }
    }
    // Sorting finished, the internal ordering can now be fetched by
    // getOrdering.
}

/*! \brief Get the sector ordering for a region.
* \pre createOrdering has been called.
* \return The correct sector ordering for a region.
* \note Because all dependencies added may not be actual supply sectors, for
*       example resources, all items within the ordering may not be actual
*       sectors. These items can be safely ignored.
* \sa createOrdering
*/
const vector<string>& DependencyFinder::getOrdering() const {
    // Check if the ordering has been initialized. Print a warning if it has
    // not.
    if( mOrdering.empty() ){
		ILogger& depFinderLog = ILogger::getLogger( "dep_finder_log" );
        depFinderLog.setLevel( ILogger::ERROR );
        depFinderLog << "Returning an empty sector ordering for region " << mRegionName << "." << endl;
    }
    return mOrdering;
}

/*! \brief Remove a dependency from the matrix.
* \param aSector Sector index for which to remove the dependency.
* \param aDependency Dependency index to remove.
*/
void DependencyFinder::removeDependency( const size_t aSector, const size_t aDependency ){
    // Remove the dependency, or edge.
    assert( mDependencyMatrix.size() > aSector );
    assert( mDependencyMatrix[ aSector ].size() > aDependency );
    mDependencyMatrix[ aSector ][ aDependency ] = false;
}

/*! \brief Add an item which should have its dependencies tracked.
* \pre The item is not already being tracked.
* \param aItem Name of the item to track.
* \return An iterator to the location of the new item.
*/
DependencyFinder::SectorIndexMap::iterator DependencyFinder::addTrackedItem( const string& aItem ){
    // Add the item to the mapping of item name to index within the matrix.
    const size_t newLocation = mDependencyMatrix.size();

	// Make pair creates a name value pair to insert into the matrix.
    pair<SectorIndexMap::iterator, bool> newPositionPair = mSectorIndices.insert( make_pair( aItem, newLocation ) );
    
    // Check the precondition that the item does not already exist.
    assert( newPositionPair.second );
    
    // Now add the item to the dependency matrix. Loop through the matrix and
    // add a new position on the end of each row.
    for( unsigned int row = 0; row < mDependencyMatrix.size(); ++row ){
        mDependencyMatrix[ row ].push_back( false );
    }

    // Add a new row to the matrix for the item. Default to not having any
    // dependencies.
    mDependencyMatrix.push_back( vector<bool>( newLocation + 1, false ) );

    // Return an iterator to the position within the index map.
    return newPositionPair.first;
}

/*! \brief Break a single cycle between two sectors by adding two markets for
*          it, and removes the dependencies or edges from the matrix.
* \param aFirstSector First node in the cycle.
* \param aSecondSector Second node in the cycle.
* \note size_t is a unsigned int used by the standard library to represent
*       positions with a container.
*/
void DependencyFinder::breakCycle( const size_t aFirstSector, const size_t aSecondSector ){
    // Notify that we are removing a cycle.
    ILogger& depFinderLog = ILogger::getLogger( "dep_finder_log" );
    depFinderLog.setLevel( ILogger::DEBUG );
    depFinderLog << "Breaking cycle between " << getNameFromIndex( aFirstSector ) << " and "
                 << getNameFromIndex( aSecondSector ) << "." << endl;

    // Add simul markets to remove the dependency. Note that one of these
    // sectors may already have a simul market setup for it, the marketplace
    // will ignore the request to convert the market in that case.
    mMarketplace->resetToPriceMarket( getNameFromIndex( aFirstSector ), mRegionName );
    mMarketplace->resetToPriceMarket( getNameFromIndex( aSecondSector ), mRegionName );
    
    // Remove the cycle from the graph by removing both edges.
    removeDependency( aFirstSector, aSecondSector );
    removeDependency( aSecondSector, aFirstSector );
}

/*! \brief Function which locates and breaks a single cycle in the dependency
*          matrix.
* \details This function performs a depth first search through the dependency
*          graph, starting at each node so that all independent sub-graphs are
*          searched, and stops when it finds an edge which leads back to a node
*          in the current search path. Once this cycle is found, it calls
*          breakCycle to remove the edges between two nodes in the cycle.
*          Control is then returned to the ordering algorithm, which may call
*          this function again if multiple cycles exist.
* \todo This search is not completely optimal either in performance or in
*       minimizing the number of cycles broken.
* \return Whether a cycle was found and broken.
*/
bool DependencyFinder::findAndBreakCycle() {

    // Perform a depth first search using each node as a starting point to
    // detect all possible cycles.
    for( unsigned int startingNode = 0; startingNode < mDependencyMatrix.size(); ++startingNode ){
        // Keep track of the current path we are on to determine the path of the
        // cycle.
        list<size_t> path;

        // Create a list of vertices which we have already visited.
        list<size_t> visitedVertices;

        // Create a stack object to control the search. Each item in the stack
        // is the index of the node in the matrix, and a flag representing
        // whether the node is the first child of its parent. This flag is
        // needed to keep the search path correct.
        stack<pair<size_t, bool> > searchStack;
        
        // Push the starting node on to the stack.
        searchStack.push( make_pair( startingNode, false ) );

        // Continue the search until we run out of nodes to search.
        while( !searchStack.empty() ){
            // Store the index and first-child flag.
            size_t currentNode = searchStack.top().first;
            bool popPath = searchStack.top().second;
            
            // Pop the top node off the stack, its the next node to search.
            searchStack.pop();
            
            // Skip the node if the search has already visited it.
            if( find( visitedVertices.begin(), visitedVertices.end(), currentNode ) != visitedVertices.end() ){
                continue;
            }
            // Add the current node to the list of searched nodes.
            visitedVertices.push_back( currentNode );
            
            // Add the node to the current path.
            path.push_back( currentNode );

            // Search the row of the matrix for dependencies.
            bool firstChild = true;
            for( unsigned int i = 0; i < mDependencyMatrix[ currentNode ].size(); ++i ){
                // Check if there is a dependency at this vertex.
                if( mDependencyMatrix[ currentNode ][ i ] ){
                    // Check if the back edge points back to a node in the
                    // current path. This is a cycle.
                    if( find( path.begin(), path.end(), i ) != path.end() ){
                        // Print the path for debugging.
                        printPath( path );
                        // Break the cycle between the current node and the
                        // current child.
                        breakCycle( i, currentNode );
                        
                        // A cycle was found and broken successfully.
                        return true;
                    }
                    // Otherwise add the child to the list of vertices to
                    // search. Checking for duplicates is performed before the
                    // vertex is searched from, which avoids having to search
                    // the stack and the list of visited vertices here. Set a
                    // flag on the stack if this is the first child of a node so
                    // that we know when to remove an element from the path.
                    searchStack.push( make_pair( i, firstChild ) );
                    firstChild = false;
                }
            }
            // If we just searched a childless node, or the last child of a
            // node, pop the node off the path.
            if( firstChild || popPath ){
                path.pop_back();
            }
        }
    }

    // The loop exited without finding a cycle.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    ILogger& depFinderLog = ILogger::getLogger( "dep_finder_log" );
    mainLog.setLevel( ILogger::ERROR );
	depFinderLog.setLevel( ILogger::ERROR );
    mainLog << "Could not find a cycle in the dependency matrix." << endl;
    depFinderLog << "Could not find a cycle in the dependency matrix." << endl;
    return false;
}

/*! \brief Print a list of sectors which represents an ordered path.
* \details Prints an ordering of sectors to the dependency finder log at DEBUG level. This is
*          useful for determining where cycles exist.
* \param aPath Path of indexes to print.
*/
void DependencyFinder::printPath( const list<size_t>& aPath ) const {
    // Get the main log.
    ILogger& depFinderLog = ILogger::getLogger( "dep_finder_log" );
    depFinderLog.setLevel( ILogger::DEBUG );

    // Make sure we didn't get an empty list.
    if( aPath.empty() ){
        depFinderLog << "Cannot print an empty path." << endl;
        return;
    }

    // Print the path.
    depFinderLog << "Path: ";
    // Loop through the vertices and print the path.
    for( list<size_t>::const_iterator iter = aPath.begin(); iter != aPath.end(); ++iter ){
        depFinderLog << getNameFromIndex( *iter ) << "->";
    }
    
    // Add the first item to clearly show the cycle.
    depFinderLog << getNameFromIndex( *aPath.begin() ) << endl;
}

/*! \brief Get the name of a sector from the matrix index.
* \param A matrix index to fetch the name for.
* \note This function is slow as it performs a reverse map lookup and should not
*       be used in sections of code which are called frequently.
* \return The name of the item associated with the index, NO_NAME if it is not
*         found.
*/
const string& DependencyFinder::getNameFromIndex( const size_t aIndex ) const {
    // Search the map linearly as this is the reverse lookup.
    for( SectorIndexMap::const_iterator item = mSectorIndices.begin(); item != mSectorIndices.end(); ++item ){
        // Check if we found the index we are searching for.
        if( item->second == aIndex ){
            return item->first;
        }
    }
    // The index does not exist.
    const static string& NO_NAME = "NO_NAME";
    return NO_NAME;
}
