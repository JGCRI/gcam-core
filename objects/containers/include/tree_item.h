#ifndef _TREE_ITEM_H_
#define _TREE_ITEM_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file tree_item.h
* \ingroup Objects
* \brief Header file for the TreeItem template class.
* \author Josh Lurz
*/

#include <string>
#include <stack>

/*!
* \brief An enum containing the possible types for items in a tree.
* \note This enum is defined outside the TreeItem because TreeItem is a
*       templated class, so the enum would be a dependent type.
*/
enum TreeItemType {
    /*!
    * \brief Node type.
    */
    eNode,

    /*!
    * \brief Leaf type.
    */
    eLeaf,

    /*!
    * \brief Any type, either node or leaf.
    */
    eAny
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
    /*!
     * \brief Return whether the item matches the specified parameters.
     * \details Returns whether the item matches according to both the given
     *          item name and type.
     * \return Whether the item matches.
     */
    virtual bool matches( const std::string& aName, 
                          const TreeItemType aType ) const = 0;

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

protected:
    /*!
     * \brief Perform a search of the tree below this item for a tree item with
     *          the specified name and type
     * \details Performs a depth first search of the tree for the item with the
     *          specified name and type This will return null if the tree item is not
     *          found. This will return the first item found with the specified
     *          name during the left-to-right, depth-first search. 
     * \return The first item found below this item with the specified name and type.
     */
    T* findItem( const std::string& aName,
                 const TreeItemType aType );

    /*!
     * \brief Perform a search of the tree below this item for a tree item with
     *          the specified name and type
     * \details Performs a depth first search of the tree for the item with the
     *          specified name and type This will return null if the tree item is not
     *          found. This will return the first item found with the specified
     *          name during the left-to-right, depth-first search. 
     * \return The first item found below this item with the specified name and type.
     */
    const T* findItem( const std::string& aName,
                       const TreeItemType aType ) const;
};

template<class T>
T* TreeItem<T>::findItem( const std::string& aName,
                          const TreeItemType aType )
{
    // Create a stack for the DFS.
    std::stack<T*> searchStack;
    
    // Add the current item to the stack.
    searchStack.push( static_cast<T*>( this ) );

    // Search the stack.
    while( !searchStack.empty() ){
        T* curr = searchStack.top();
        searchStack.pop();

        // Check if this is our goal.
        if( curr->matches( aName, aType ) ){
            return curr;
        }

        // Add the children onto the stack.
        for( unsigned int child = 0; child < curr->getNumChildren(); ++child ){
            searchStack.push( curr->getChildAt( child ) );
        }
    }
    // We searched the entire tree and failed.
    return 0;
}

template<class T>
const T* TreeItem<T>::findItem( const std::string& aName,
                                const TreeItemType aType ) const
{
    // Create a stack for the DFS.
    std::stack<const T*> searchStack;
    
    // Add the current item to the stack.
    searchStack.push( static_cast<const T*>( this ) );

    // Search the stack.
    while( !searchStack.empty() ){
        const T* curr = searchStack.top();
        searchStack.pop();

        // Check if this is our goal.
        if( curr->matches( aName, aType ) ){
            return curr;
        }

        // Add the children onto the stack.
        for( unsigned int child = 0; child < curr->getNumChildren(); ++child ){
            searchStack.push( curr->getChildAt( child ) );
        }
    }
    // We searched the entire tree and failed.
    return 0;
}

#endif // _TREE_ITEM_H_
