#ifndef _UNMANAGED_LAND_LEAF_H_
#define _UNMANAGED_LAND_LEAF_H_
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

    virtual void completeInit( const std::string& aRegionName, 
                               const IInfo* aRegionInfo );
    
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

    virtual void calcLandAllocation( const std::string& aRegionName,
                                     const double aLandAllocationAbove,
                                     const int aPeriod );

    virtual bool isUnmanagedNest() const;

    virtual void resetToCalLandAllocation( const int aPeriod );

    virtual void toInputXML( std::ostream& aOut,
                             Tabs* aTabs ) const;
protected:
    //! Unadjusted intrinsic rate.
    objects::PeriodVector<Value> mBaseIntrinsicRate;
    
    //! Unadjusted land value
    objects::PeriodVector<Value> mBaseLandAllocation;

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
