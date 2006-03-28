#ifndef _FOREST_LAND_LEAF_H_
#define _FOREST_LAND_LEAF_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file forest_land_leaf.h
* \ingroup CIAM
* \brief The ForestLandLeaf class header file.
* \author James Blackwood
*/

#include "land_allocator/include/land_leaf.h"

/*! \brief A type of leaf which contains land used for managed forestry.
* \details This is a special type of production leaf which produces the managed
*          forestry crop. It is unique it because it contains multiple vintages
*          of a crop.
*/
class ForestLandLeaf : public LandLeaf {
public:
    ForestLandLeaf();
    virtual ~ForestLandLeaf();

    static const std::string& getXMLNameStatic();

    virtual void completeInit( const std::string& aRegionName, 
                               const IInfo* aRegionInfo );

    virtual void calcLandAllocation( double landAllocationAbove, int period );

    virtual double getLandAllocation ( const std::string& aProductName,
                                       const int aPeriod ) const;

    virtual double getTotalLandAllocation( const std::string& productName, int period );

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
    int rotationPeriod; //!< rotation period for forests
    int steps; //!< number of model time steps for a rotation period
    std::vector<double> landToBeHarvested;  //!< Land allocation set aside for future production.
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual const std::string& getXMLName() const;
};

#endif // _FOREST_LAND_LEAF_H_

