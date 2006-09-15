#ifndef _UNMANAGED_LAND_LEAF_H_
#define _UNMANAGED_LAND_LEAF_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file unmanaged_land_leaf.h
 * \ingroup Objects
 * \brief The LandAllocatorLeaf class header file.
 * \author James Blackwood
 */
#include <memory>
#include "land_allocator/include/land_leaf.h"
class LandUseHistory;

/*!
 * \brief A type of leaf which contains unmanaged land.
 * \details Unmanaged land leaves represent land that is not currently used for
 *          crops or grazing, such as unmanaged forests. Unmanaged land may be
 *          converted to other uses given high enough prices.
 *
 *          <b>XML specification for UnmanagedLandLeaf</b>
 *          - XML name: \c UnmanagedLandLeaf
 *          - Contained by: LandNode
 *          - Parsing inherited from class: None
 *          - Attributes:
 *              - \c name ALandAllocatorItem::mName
 *          - Elements:
 *              - \c intrinsicRate UnmanagedLandLeaf::mBaseIntrinsicRate
 *              - \c unmanaged-carbon-calc LandLeaf::mCarbonContentCalc
 *              - \c land-use-history UnmanagedLandLeaf::mLandUseHistory
 */
class UnmanagedLandLeaf : public LandLeaf {
public:
    explicit UnmanagedLandLeaf( const ALandAllocatorItem* aParent );
    virtual ~UnmanagedLandLeaf();
    static const std::string& getXMLNameStatic();

    virtual void setUnmanagedLandAllocation( const std::string& aRegionName,
                                             const double aNewUnmanaged,
                                             const int aPeriod );
    
    virtual void setUnmanagedLandValues( const std::string& aRegionName,
                                         const int aPeriod );

    virtual double calcLandShares( const std::string& aRegionName,
                                   const double aSigmaAbove,
                                   const double aTotalLandAllocated,
                                   const int aPeriod );
    
    virtual double getTotalLandAllocation( const LandAllocationType aType,
                                           const int aPeriod ) const;

    virtual bool isUnmanagedNest() const;

    virtual void toInputXML( std::ostream& aOut,
                             Tabs* aTabs ) const;
protected:
    //! Unadjusted intrinsic rate.
    objects::PeriodVector<Value> mBaseIntrinsicRate;
    
    //! Unadjusted land value
    objects::PeriodVector<Value> mBaseLandAllocation;

    //! Container of historical land use.
    std::auto_ptr<LandUseHistory> mLandUseHistory;

    virtual void initCarbonCycle();
    virtual const std::string& getXMLName() const;

    virtual void toDebugXMLDerived( const int aPeriod, std::ostream& out, Tabs* tabs ) const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual double getBaseLandAllocation( const int aPeriod ) const;
    virtual void checkCalObservedYield( const int aPeriod ) const;

    virtual void initLandUseHistory( const double aParentHistoryShare,
                                     const LandUseHistory* aParentHistory,
                                     const int aFirstCalibratedPeriod );
};

#endif // _UNMANAGED_LAND_LEAF_H_
