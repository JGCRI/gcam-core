#ifndef _DEPENDENCY_FINDER_H_
#define _DEPENDENCY_FINDER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file dependency_finder.h
* \ingroup Objects
* \brief The DependencyFinder class header file.
* \author Josh Lurz
*/

#include <vector>
#include <map>
#include <list>
#include <string>

class Marketplace;
/*! 
* \ingroup Objects
* \brief This class calculates an ordering of sectors through a scheduling
*        algorithm based on supplied dependencies.
* \details To use this class to create a correct sector ordering, the user
*          should first use the addDependency function for each dependency of a
*          sector. This is currently performed at the technology level, where
*          each technology adds its fuel. Once all dependencies have been added,
*          calling createOrdering constructs an internal ordering. This may
*          require creating extra markets to resolve dependency cycles. When
*          these markets are created, the corresponding dependencies will be
*          removed from the dependency matrix. This function should not fail,
*          but will report errors if it does. Additional dependencies can be
*          added once the ordering is created, but createOrdering must be called
*          again. Once createOrdering has completed, the user can then call
*          getOrdering to return an ordered list of sector names, starting with
*          the sector which should be called first by the model. The two key
*          data structures are mDependencyMatrix and mSectorIndices.
*          mDependencyMatrix is a two-dimensional matrix where each cell
*          represents whether there is a dependency from the column sector to
*          the row sector. mSectorIndices conains a mapping of sector name to
*          the index in mDependencyMatrix. Both of these data structures are
*          updated by the addDependency function.
* \author Josh Lurz
*/

class DependencyFinder
{
public:
    DependencyFinder( Marketplace* aMarketplace, const std::string& aRegionName );
    bool addDependency( const std::string& aSector, const std::string& aDependency );
    void createOrdering();
    const std::vector<std::string>& getOrdering() const;
private:
    bool findAndBreakCycle();
    void breakCycle( const size_t aFirstSector, const size_t aSecondSector );
    void removeDependency( const size_t aSector, const size_t aDependency );
    void printPath( const std::list<size_t>& aPath ) const;
    const std::string& getNameFromIndex( const size_t aIndex ) const;
    
    // Need the typedef here as addTrackedItem returns a SectorIndexMap
    // iterator.
    typedef std::map<const std::string, size_t> SectorIndexMap;   
    SectorIndexMap::iterator addTrackedItem( const std::string& aItem ); 
    
    //! Mapping of sector name to matrix index to avoid storing all names as
    //! strings within the matrix.
    SectorIndexMap mSectorIndices;

    //! A square matrix where each cell contains whether the column sector
    //! depends on the row sector.
    std::vector<std::vector<bool> > mDependencyMatrix;
    
    //! The correctly ordered list of sectors.
    std::vector<std::string> mOrdering;

    //! Name of the region containing the dependency finder.
    const std::string mRegionName;

    //! The marketplace for this region.
    Marketplace* mMarketplace;
};

#endif // _DEPENDENCY_FINDER_H_
