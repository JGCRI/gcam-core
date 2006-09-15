#ifndef _LAND_LEAF_H_
#define _LAND_LEAF_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file land_leaf.h
 * \ingroup Objects
 * \brief The LandLeaf class header file.
 * \author James Blackwood
 */

#include <xercesc/dom/DOMNode.hpp>
#include "land_allocator/include/aland_allocator_item.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/value.h"

class Tabs;
class ICarbonCalc;
class LandNode;

/*!
 * \brief A LandLeaf is the leaf of a land allocation tree.
 * \details A leaf in the land allocator which represents the land used to
 *          produce a single crop. Land leaves can be seperated into two
 *          categories, managed land leaves which are created by farming
 *          technologies, and unmanaged land leaves which are created through
 *          input to contain unmanaged arrable land.
 *
 *          <b>XML specification for LandLeaf</b>
 *          - XML name: Not parsed
 *          - Contained by: LandNode
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements: None
 */
class LandLeaf : public ALandAllocatorItem {
public:
    LandLeaf( const ALandAllocatorItem* aParent,
              const std::string& aName );

    virtual ~LandLeaf();

    // Tree Item methods.
    virtual size_t getNumChildren() const;

    virtual const ALandAllocatorItem* getChildAt( const size_t aIndex ) const;

    virtual ALandAllocatorItem* getChildAt( const size_t aIndex );

    virtual void completeInit( const std::string& aRegionName, 
                               const IInfo* aRegionInfo );
    
    virtual void addLandUsage( const std::string& aLandType,
                               const std::string& aProductName,
                               const ILandAllocator::LandUsageType aLandUsageType,
                               const int aPeriod );

    virtual void setInitShares( const std::string& aRegionName,
                                const double aSigmaAbove,
                                const double aLandAllocationAbove,
                                const double aParentHistoryShare,
                                const LandUseHistory* aParentHistory,
                                const int aPeriod );

    virtual void setIntrinsicYieldMode( const double aIntrinsicYieldAbove,
                                        const double aSigmaAbove,
                                        const int aPeriod );
    
    virtual void setIntrinsicRate( const std::string& aRegionName,
                                   const std::string& aLandType,
                                   const std::string& aProductName,
                                   const double aIntrinsicRate,
                                   const int aPeriod );
    
    virtual void setCalLandAllocation( const std::string& aLandType,
                                       const std::string& aProductName,
                                       const double aCalLandUsed,
                                       const int aHarvestPeriod, 
                                       const int aCurrentPeriod );

    virtual void setCalObservedYield( const std::string& aLandType,
                                      const std::string& aProductName,
                                      const double aCalObservedYield, 
                                      const int aPeriod );

    virtual void applyAgProdChange( const std::string& aLandType,
                                    const std::string& aProductName,
                                    const double aAgProdChange,
                                    const int aPeriod );

    virtual double calcLandShares( const std::string& aRegionName,
                                   const double aSigmaAbove,
                                   const double aTotalLandAllocated,
                                   const int aPeriod );

    virtual void calcLandAllocation( const std::string& aRegionName,
                                     const double aLandAllocationAbove,
                                     const int aPeriod );
    
    virtual void calcYieldInternal( const std::string& aLandType,
                                    const std::string& aProductName,
                                    const std::string& aRegionName,
                                    const double aProfitRate,
                                    const double aAvgIntrinsicRate,
                                    const int aHarvestPeriod,
                                    const int aCurrentPeriod );
    
    virtual double getYield( const std::string& aLandType,
                             const std::string& aProductName,
                             const int aPeriod ) const;

    virtual double getLandAllocation( const std::string& aLandType,
                                      const std::string& aProductName,
                                      const int aPeriod ) const;

    virtual double getTotalLandAllocation( const LandAllocationType aType,
                                           const int aPeriod ) const;
    
    virtual double getBaseLandAllocation( const int aPeriod ) const;

    virtual void setUnmanagedLandAllocation( const std::string& aRegionName,
                                             const double aNewUnmanaged,
                                             const int aPeriod );

    virtual void csvOutput( const std::string& aRegionName ) const;

    virtual void dbOutput( const std::string& aRegionName ) const;

    virtual bool isUnmanagedNest() const;

    virtual bool isConceptualRoot() const;

    virtual double getSigma() const;

    virtual void setUnmanagedLandValues( const std::string& aRegionName,
                                         const int aPeriod );
 
    virtual void setCarbonContent( const std::string& aLandType,
                                   const std::string& aProductName,
                                   const double aAboveGroundCarbon,
                                   const double aBelowGroundCarbon,
                                   const int aPeriod );

    virtual bool XMLParse( const xercesc::DOMNode* aNode );

    virtual void toInputXML( std::ostream& aOut,
                             Tabs* aTabs ) const;

	virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const;

protected:
    //! The intrinsic yield mode of the leaf.
    objects::PeriodVector<Value> mIntrinsicYieldMode;

    // TODO: Convert this to a PeriodVector. Currently can't because the forest
    // leaf is adding extra periods.
    //! Actual yield.
    std::vector<Value> mYield;
    
    //! The calibrated observed yield.
    objects::PeriodVector<Value> mCalObservedYield;

    //! Calculated cumulative technical change.
    objects::PeriodVector<Value> mAgProdChange;

    //! Land allocated in 1000's of hectars
    objects::PeriodVector<Value> mLandAllocation;

    //! Carbon content and emissions calculator for the leaf.
	std::auto_ptr<ICarbonCalc> mCarbonContentCalc;

    //! Interest rate stored from the region info.
    Value mInterestRate;

    double getCarbonValue( const std::string& aRegionName,
                           const int aPeriod ) const;

    virtual void initCarbonCycle();

    virtual bool XMLDerivedClassParse( const std::string& aNodeName,
                                       const xercesc::DOMNode* aCurr );

    virtual void toDebugXMLDerived( const int aPeriod,
                                    std::ostream& aOut,
                                    Tabs* aTabs ) const;

    virtual const std::string& getXMLName() const;

    virtual void addChild( ALandAllocatorItem* aChild );

    virtual void checkCalObservedYield( const int aPeriod ) const;

    virtual void initLandUseHistory( const double aParentHistoryShare,
                                     const LandUseHistory* aParentHistory,
                                     const int aFirstCalibratedPeriod );
};

#endif // _LAND_LEAF_H_
