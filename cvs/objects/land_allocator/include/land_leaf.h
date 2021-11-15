#ifndef _LAND_LEAF_H_
#define _LAND_LEAF_H_
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
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/



/*!
 * \file land_leaf.h
 * \ingroup Objects
 * \brief The LandLeaf class header file.
 * \author James Blackwood
 */

#include "land_allocator/include/aland_allocator_item.h"
#include "util/base/include/ivisitable.h"

class Tabs;
class ICarbonCalc;

/*!
 * \brief A LandLeaf is the leaf of a land allocation tree.
 * \details A leaf in the land allocator which represents the land used to
 *          produce a single crop. Land leaves can be separated into two
 *          categories, managed land leaves which are used by farming
 *          technologies, and unmanaged land leaves.
 *
 *          <b>XML specification for LandLeaf</b>
 *          - XML name: Not parsed
 *          - Contained by: LandNode
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements: None
 */
class LandLeaf : public ALandAllocatorItem {
    friend class XMLDBOutputter;
public:
    LandLeaf( const ALandAllocatorItem* aParent,
              const std::string& aName );
    
    LandLeaf();

    virtual ~LandLeaf();

    static const std::string& getXMLNameStatic();
    
    virtual const std::string& getXMLName() const;

    // Tree Item methods.
    virtual size_t getNumChildren() const;

    virtual const ALandAllocatorItem* getChildAt( const size_t aIndex ) const;

    virtual ALandAllocatorItem* getChildAt( const size_t aIndex );

    virtual void completeInit( const std::string& aRegionName, 
                               const IInfo* aRegionInfo );
        
    virtual void initCalc( const std::string& aRegionName,
                           const int aPeriod );

    virtual void setInitShares( const std::string& aRegionName,
                                const double aLandAllocationAbove,
                                const int aPeriod );

    virtual void calculateNodeProfitRates( const std::string& aRegionName,
                                           const int aPeriod );

	virtual void setProfitRate( const std::string& aRegionName,
                                   const std::string& aProductName,
                                   const double aProfitRate,
                                   const int aPeriod );

    virtual void setCarbonPriceIncreaseRate( const double aCarbonPriceIncreaseRate, 
                                      const int aPeriod );

    virtual void setSoilTimeScale( const int aTimeScale );

    virtual double calcLandShares( const std::string& aRegionName,
                                   IDiscreteChoice* aChoiceFnAbove,
                                   const int aPeriod );

    virtual void calcLandAllocation( const std::string& aRegionName,
                                     const double aLandAllocationAbove,
                                     const int aPeriod );

    virtual void calcLUCEmissions( const std::string& aRegionName,
                                   const int aPeriod, const int aEndYear,
                                   const bool aStoreFullEmiss );

    virtual double getLandAllocation( const std::string& aProductName,
                                      const int aPeriod ) const;

    virtual double getCalLandAllocation( const LandAllocationType aType,
                                         const int aPeriod ) const;
    
    virtual void setUnmanagedLandProfitRate( const std::string& aRegionName, 
                                             double aAverageProfitRate,
                                             const int aPeriod );
    
    virtual void resetCalLandAllocation( const std::string& aRegionName,
                                            double aNewLandAllocation,
                                            const int aPeriod );
    
    virtual void getObservedAverageProfitRate( double& aProfitRate, double& aShare, const int aPeriod ) const;
    
    virtual const ALandAllocatorItem* getChildWithHighestShare( const bool aIncludeAllChildren, const int aPeriod ) const;

    virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const;

    virtual void acceptDerived( IVisitor* aVisitor,
                         const int aPeriod ) const;

    virtual ICarbonCalc* getCarbonContentCalc() const;
        
	virtual bool isUnmanagedLandLeaf( )  const;
    
    bool hasLandAllocationCalculated( const int aPeriod ) const;

protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        ALandAllocatorItem,
                            
        //! Land allocated typically in thous km2.
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "land-allocation", mLandAllocation, objects::PeriodVector<Value> ),

        //! Carbon content and emissions calculator for the leaf.
        DEFINE_VARIABLE( CONTAINER, "carbon-calc", mCarbonContentCalc, ICarbonCalc* ),

        //! Social discount rate stored from the region info.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "social-discount-rate", mSocialDiscountRate, Value ),

        //! Minimum above ground carbon density (used for carbon subsidy and not emissions calculations)
        DEFINE_VARIABLE( SIMPLE, "minAboveGroundCDensity", mMinAboveGroundCDensity, Value ),

        //! Minimum below ground carbon density (used for carbon subsidy and not emissions calculations)
        DEFINE_VARIABLE( SIMPLE, "minBelowGroundCDensity", mMinBelowGroundCDensity, Value ),

        //! Expected rate of increase of the carbon price from the region info.
        DEFINE_VARIABLE( ARRAY, "carbon-price-increase-rate", mCarbonPriceIncreaseRate, objects::PeriodVector<Value> ),

        //! Container of historical land use.
        DEFINE_VARIABLE( CONTAINER, "land-use-history", mLandUseHistory, LandUseHistory* ),
        
        DEFINE_VARIABLE( ARRAY, "landAllocation", mReadinLandAllocation, objects::PeriodVector<Value> ),
                            
        //! Name of land constraint policy
        DEFINE_VARIABLE( SIMPLE, "land-constraint-policy", mLandConstraintPolicy, std::string ),
                            
        //! State value necessary to use Marketplace::addToDemand for CO2 emissions
        DEFINE_VARIABLE( SIMPLE | STATE | NOT_PARSABLE, "luc-state", mLastCalcCO2Value, Value ),

        //! The name of a negative emissions policy which may scale back
        //! carbon subsidies if there isn't a budget to support it
        DEFINE_VARIABLE( SIMPLE, "negative-emiss-market", mNegEmissMarketName, std::string )
    )

    double getCarbonSubsidy( const std::string& aRegionName,
                           const int aPeriod ) const;
    
    double getLandConstraintCost( const std::string& aRegionName,
                            const int aPeriod ) const;

    virtual void toDebugXMLDerived( const int aPeriod,
                                    std::ostream& aOut,
                                    Tabs* aTabs ) const;

    virtual void initLandUseHistory( const std::string& aRegionName );
};

#endif // _LAND_LEAF_H_
