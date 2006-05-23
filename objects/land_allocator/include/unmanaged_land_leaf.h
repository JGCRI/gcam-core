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

#include "land_allocator/include/land_leaf.h"

/*!
 * \brief A type of leaf which contains unmanaged land.
 * \details Unmanaged land leaves represent land that is not currently used for
 *          crops or grazing, such as unmanaged forests. Unmanaged land may be
 *          converted to other uses given high enough prices.
 *
 *          <b>XML specification for UnmanagedLandLeaf</b>
 *          - XML name: \c UnmanagedLandLeaf
 *          - Contained by: LandNode
 *          - Parsing inherited from class: ALandAllocatorItem
 *          - Attributes: Derived only.
 *          - Elements:
 *              - \c GHG_INPUT LandLeaf::mGHGs
 *              - \c historyYear UnmanagedLandLeaf::historyYear
 *              - \c intrinsicRate UnmanagedLandLeaf::mBaseIntrinsicRate
 *              - \c unmanaged-carbon-calc LandLeaf::mCarbonContentCalc
 */
class UnmanagedLandLeaf : public LandLeaf {
public:
    UnmanagedLandLeaf();
    virtual ~UnmanagedLandLeaf();
    static const std::string& getXMLNameStatic();

    virtual void setUnmanagedLandAllocation( const std::string& aRegionName,
                                             const double aLandAllocation,
                                             const int aPeriod );
    
    virtual void setUnmanagedLandValues( const std::string& aRegionName,
                                         const int aPeriod );

    virtual void calcLandShares( const std::string& aRegionName,
                                 const double aSigmaAbove,
                                 const double aTotalLandAllocated,
                                 const int aPeriod );

    virtual void calcEmission( const std::string& aRegionName,
                               const GDP* aGDP,
                               const int aPeriod );

    virtual void toInputXML( std::ostream& out,
                             Tabs* tabs ) const;

    virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const;

    virtual void updateSummary( Summary& aSummary,
                                const int aPeriod );

    virtual void csvOutput( const std::string& aRegionName ) const;

    virtual void dbOutput( const std::string& aRegionName ) const;
protected:
    //! Unadjusted intrinsic rate.
    objects::PeriodVector<double> mBaseIntrinsicRate;
    
    //! Unadjusted land value
    objects::PeriodVector<double> mBaseLandAllocation;
    
    // TODO: GHGs in the land allocator are difficult to deal with because the interface
    // is designed for Technologies. The cost is not currently included in profit rates.
    
    //! Vector of suites of greenhouse gases.
    std::vector<Ghg*> mGHGs;
  
    //! Year before which land allocations are historical values.
    unsigned int mHistoryYear;

    virtual void initCarbonCycle();
    virtual bool isProductionLeaf() const;
    virtual const std::string& getXMLName() const;

    virtual void toDebugXMLDerived( const int aPeriod, std::ostream& out, Tabs* tabs ) const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual double getBaseLandAllocation( const int aPeriod ) const;
    virtual void checkCalObservedYield( const int aPeriod ) const;
    
    static unsigned int defaultHistoryYear();

};

#endif // _UNMANAGED_LAND_LEAF_H_