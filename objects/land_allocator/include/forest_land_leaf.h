#ifndef _FOREST_LAND_LEAF_H_
#define _FOREST_LAND_LEAF_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file forest_land_leaf.h
 * \ingroup Objects
 * \brief The ForestLandLeaf class header file.
 * \author James Blackwood
 */

#include "land_allocator/include/land_leaf.h"

/*!
 * \brief A type of leaf which contains land used for managed forestry.
 * \details This is a special type of production leaf which produces the managed
 *          forestry crop. It is unique it because it contains multiple vintages
 *          of a crop.
 *
 *          <b>XML specification for ForestLandLeaf</b>
 *          - XML name: \c Not parsed
 *          - Contained by: LandNode
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements: None
 */
class ForestLandLeaf : public LandLeaf {
public:
    ForestLandLeaf();
    virtual ~ForestLandLeaf();

    virtual void completeInit( const std::string& aRegionName, 
                               const IInfo* aRegionInfo );

    virtual void calcLandAllocation( const std::string& aRegionName,
                                     const double aLandAllocationAbove,
                                     const int aPeriod );

    virtual double getLandAllocation ( const std::string& aProductName,
                                       const int aPeriod ) const;

    virtual double getTotalLandAllocation( const std::string& aProductName,
                                           const int aPeriod ) const;

    virtual void setCalLandAllocation( const std::string& aLandType,
                                       const std::string& aProductName,
                                       const double aCalLandUsed,
                                       const int aHarvestPeriod, 
                                       const int aCurrentPeriod );

    virtual void setCalObservedYield( const std::string& aLandType,
                                      const std::string& aProductName,
                                      const double aCalObservedYield,
                                      const int aPeriod );
protected:
    // TODO: Create a Value class for ints and use it here.
    int mSteps; //!< number of model time steps for a rotation period

    //! Land allocation set aside for future production.
    std::vector<double> mLandToBeHarvested;  


    virtual void toDebugXMLDerived( const int aPeriod,
                                    std::ostream& aOut,
                                    Tabs* aTabs ) const;

    virtual const std::string& getXMLName() const;
};

#endif // _FOREST_LAND_LEAF_H_

