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
* \date $Date$
* \version $Revision$
*/

#include "land_allocator/include/land_leaf.h"

/*! \brief A type of leaf which contains unmanaged land.
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
protected:
    std::vector<double> baseIntrinsicRate; //!< Unadjusted intrinsic rate
    std::vector<double> baseLandAllocation; //!< Unadjusted land value

    virtual bool isProductionLeaf() const;
    virtual const std::string& getXMLName() const;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int aPeriod, std::ostream& out, Tabs* tabs ) const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual double getBaseLandAllocation ( int period );
    virtual void checkCalObservedYield( const int aPeriod ) const;
    
    static int defaultHistoryYear();

    int historyYear;  //!< Year before which land allocations are historical values
};

#endif // _UNMANAGED_LAND_LEAF_H_
