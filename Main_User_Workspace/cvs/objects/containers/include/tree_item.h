#ifndef _TREE_ITEM_H_
#define _TREE_ITEM_H_
#if defined(_MSC_VER)
#pragma once
#endif

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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */


/*! 
* \file tree_item.h
* \ingroup Objects
* \brief Header file for the TreeItem template class.
* \author Josh Lurz
*/

#include <string>
#include <deque>

/*!
 * \brief An enum containing the types of searches (BFS/DFS)
 */
enum SearchType {
    /*!
     * \brief Depth-first search.
     */
    eDFS,
    
    /*!
     * \brief Breadth-first search.
     */
    eBFS
};

/*! 
* \ingroup Objects
* \brief This class defines an interface to any leaf or node in a leaf node
*        structure.
* \author Josh Lurz
*/
template<class T> 
class TreeItem {
public:

    typedef typename std::unary_function<const T*, bool> MatchesFunction;

    /*! \brief Get the number of children which the tree item has.
    * \details Returns the number of children which the tree item has. In the
    *          case of a leaf in the tree this will be zero. These children
    *          should be accessible by getChildAt().
    * \return The number of children the tree item has.
    */
    virtual size_t getNumChildren() const = 0;

    /*! \brief Return the child at the index.
    * \details Queries the tree item for the child at a specific index. If this
    *          index is less than zero or greater than or equal to the size,
    *          this will throw an exception.
    * \return The child at the index.
    */
    virtual const T* getChildAt( const size_t aIndex ) const = 0;

    /*! \brief Return the child at the index.
    * \details Queries the tree item for the child at a specific index. If this
    *          index is less than zero or greater than or equal to the size,
    *          this will throw an exception.
    * \return The child at the index.
    */
    virtual T* getChildAt( const size_t aIndex ) = 0;
};

/*!
 * \brief Perform a search of the tree below this item for a tree item with
 *          with the specified Predicate.
 * \details Performs a search of the tree for the item that satisfies the
 *          predicate. This will return null if the tree item is not found.
 * \param aSearchType An enum representing the type of search to be used.
 * \param aNode The node to start search.
 * \param aIsGoal The predicate.
 * \return The first item found below this item that satisfies the predicate.
 */
template<class T, class Predicate>
const T* findItem( SearchType aSearchType, const T* aNode, Predicate aIsGoal ){
    // Create a deque for search.
    std::deque<const T*> searchStack;

    // Add the current item.
    searchStack.push_front( aNode );

    // Search the deque.
    while( !searchStack.empty() ){
        const T* curr = searchStack.front();
        searchStack.pop_front();

        // Check if this is our goal.
        if( aIsGoal( curr ) ){
            return curr;
        }

        // Add the children onto the deque.
        for( unsigned int child = 0; child < curr->getNumChildren(); ++child ){
            switch( aSearchType ){
                case eDFS:
                    searchStack.push_front( curr->getChildAt( child ) );
                    break;
                case eBFS:
                    searchStack.push_back( curr->getChildAt( child ) );
                    break;
            }
        }
    }
    // We searched the entire tree and failed.
    return 0;
}

/*!
 * \brief Perform a search of the tree below this item for a tree item with
 *          with the specified Predicate.
 * \details Performs a search of the tree for the item that satisfies the
 *          predicate. This will return null if the tree item is not found.
 * \param aSearchType An enum representing the type of search to be used.
 * \param aNode The node to start search.
 * \param aIsGoal The predicate.
 * \return The first item found below this item that satisfies the predicate.
 */
template<class T, class Predicate>
T* findItem( SearchType aSearchType, T* aNode, Predicate aIsGoal ){
    // Create a deque for search.
    std::deque<T*> searchStack;

    // Add the current item.
    searchStack.push_front( aNode );

    // Search the deque.
    while( !searchStack.empty() ){
        T* curr = searchStack.front();
        searchStack.pop_front();

        // Check if this is our goal.
        if( aIsGoal( curr ) ){
            return curr;
        }

        // Add the children onto the deque.
        for( unsigned int child = 0; child < curr->getNumChildren(); ++child ){
            switch( aSearchType ){
                case eDFS:
                    searchStack.push_front( curr->getChildAt( child ) );
                    break;
                case eBFS:
                    searchStack.push_back( curr->getChildAt( child ) );
                    break;
            }
        }
    }
    // We searched the entire tree and failed.
    return 0;
}

#endif // _TREE_ITEM_H_
