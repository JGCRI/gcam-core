#ifndef _ALAND_ALLOCATOR_ITEM_H_
#define _ALAND_ALLOCATOR_ITEM_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file aland_allocator_item.h
 * \ingroup Objects
 * \brief The ALandAllocatorItem class header file.
 * \author James Blackwood
 */

#include <vector>
#include <map>
#include <xercesc/dom/DOMNode.hpp>

#include "containers/include/tree_item.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/iparsable.h"
#include "util/base/include/iround_trippable.h"
#include "util/base/include/time_vector.h"

// For LandUsageType enum.
#include "land_allocator/include/iland_allocator.h"

// Forward declarations
class AGHG;
class Summary;
class IInfo;
class Tabs;
class GDP;
class LandUseHistory;

/*!
 * \brief A single item in the land allocator tree.
 * \details This is the abstract base class of all nodes and leaves in the land
 *          allocation tree, including the root. It inherits from the TreeItem
 *          class so that it can make use of the tree library functions.
 *
 *          <b>XML specification for ALandAllocatorItem</b>
 *          - XML name: Not parsed
 *          - Contained by: TreeLandAllocator
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements: None
 */
class ALandAllocatorItem : public TreeItem<ALandAllocatorItem>,
                           public IVisitable,
                           public IParsable,
                           public IRoundTrippable
{
public:
    ALandAllocatorItem();
   
    virtual ~ALandAllocatorItem();

    // Tree Item methods.
    virtual bool matches( const std::string& aName,
                          const TreeItemType aType ) const = 0;

    virtual size_t getNumChildren() const = 0;

    virtual const ALandAllocatorItem* getChildAt( const size_t aIndex ) const = 0;
    
    virtual ALandAllocatorItem* getChildAt( const size_t aIndex ) = 0;
    
    // IParsable
    virtual bool XMLParse( const xercesc::DOMNode* aNode ) = 0;
    
    // IRoundTrippable
    virtual void toInputXML( std::ostream& aOut,
                             Tabs* aTabs ) const = 0;
    
    // IVisitable
    virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const = 0;

    void toDebugXML( const int aPeriod,
                     std::ostream& aOut,
                     Tabs* aTabs ) const;

    const std::string& getName() const;

    virtual void completeInit( const std::string& aRegionName, 
                               const IInfo* aRegionInfo ) = 0;

    virtual void addLandUsage( const std::string& aLandType,
                               const std::string& aProductName,
                               const ILandAllocator::LandUsageType aLandUsageType ) = 0;

    virtual double getLandAllocation( const std::string& aProductName,
                                      const int aPeriod ) const = 0;

    /*!
     * \brief An enumeration of possible land allocation types.
     */
    enum LandAllocationType {
        // Managed land allocation.
        eManaged,

        //! Unmanaged land allocation.
        eUnmanaged,

        //! Any land allocation.
        eAnyLand
    };

    virtual double getTotalLandAllocation( const LandAllocationType aType,
                                           const int aPeriod ) const = 0;

    virtual double getBaseLandAllocation( const int aPeriod ) const = 0;

    void normalizeLandAllocation( const double aSum,
                                  const int aPeriod );

    virtual void setInitShares( const double aLandAllocationAbove,
                                const LandUseHistory* aLandUseHistory,
                                const int aPeriod ) = 0;

    virtual void setIntrinsicYieldMode( const double aIntrinsicRateAbove,
                                        const double aSigmaAbove,
                                        const int aPeriod ) = 0;

    virtual void setIntrinsicRate( const std::string& aRegionName,
                                   const std::string& aLandType,
                                   const std::string& aProductName,
                                   const double aIntrinsicRate,
                                   const int aPeriod ) = 0;
    
   /*!
    * \brief Returns the calibrated average observed intrinsic rate for unmanaged land.
    * \details The calibrated average observed Intrinsic rate for the unmanaged
    *          land nest is equal to the intrinsic rate for the unmanaged land node
    *          successively divided by its share to the power of the sigma
    *          parameter for each level.
    * \warning This function will not behave properly if there are unmanaged land
    *          nodes in more than 1 place.
    * \note UnmanagedLand leaves return 1 in this function, all other leaves
    *       return 0.
    * \author James Blackwood
    * \author Steve Smith
    * \author Jim Naslund
    * \param aPeriod model period.
    * \param aSigmaAbove the sigma value from the node above this level.
    */
    virtual double getUnmanagedCalAveObservedRateInternal( const int aPeriod,
                                                           const double aSigma ) const = 0;

    virtual void setCalLandAllocation( const std::string& aLandType,
                                       const std::string& aProductName,
                                       const double aCalLandUsed,
                                       const int aHarvestPeriod, 
                                       const int aCurrentPeriod ) = 0;

    virtual void setCalObservedYield( const std::string& aLandType,
                                      const std::string& aProductName,
                                      const double aCalObservedYield, 
                                      const int aPeriod ) = 0;

    virtual void applyAgProdChange( const std::string& aLandType,
                                    const std::string& aProductName,
                                    const double aAgProdChange,
                                    const int aPeriod ) = 0;

    virtual void addChild( ALandAllocatorItem* aChild ) = 0;
    
    virtual double calcLandShares( const std::string& aRegionName,
                                   const double aSigmaAbove,
                                   const double aTotalLandAllocated,
                                   const int aPeriod ) = 0;

    virtual void calcLandAllocation( const std::string& aRegionName,
                                     const double aLandAllocationAbove,
                                     const int aPeriod ) = 0;
    
    virtual void calcYieldInternal( const std::string& aLandType,
                                    const std::string& aProductName,
                                    const std::string& aRegionName,
                                    const double aProfitRate,
                                    const double aAvgIntrinsicRate,
                                    const int aHarvestPeriod,
                                    const int aCurrentPeriod ) = 0;

    virtual double getYield( const std::string& aLandType,
                             const std::string& aProductName,
                             const int aPeriod ) const = 0;

    virtual void csvOutput( const std::string& aRegionName ) const = 0;

    virtual void dbOutput( const std::string& aRegionName ) const = 0;
    
    virtual void setUnmanagedLandAllocation( const std::string& aRegionName,
                                             const double aLandAllocation,
                                             const int aPeriod ) = 0;

    virtual void setUnmanagedLandValues( const std::string& aRegionName,
                                         const int aPeriod ) = 0;
    
    virtual void calcEmission( const std::string& aRegionName,
                               const GDP* aGDP,
                               const int aPeriod ) = 0;

    virtual void setCarbonContent( const std::string& aLandType,
                                   const std::string& aProductName,
                                   const double aAboveGroundCarbon,
                                   const double aBelowGroundCarbon,
                                   const int aPeriod ) = 0;

    virtual void updateSummary ( Summary& aSummary,
                                 const int aPeriod ) = 0;

protected:
    //! Percent of land
    objects::PeriodVector<double> mShare;
    
    //! Rate in dollars to rent the land
    objects::PeriodVector<double> mIntrinsicRate;
    
    //! Name of the land allocator item. This is the name of the product for
    //! leafs and name of the type of land for nodes.
    std::string mName;

    virtual bool XMLDerivedClassParse( const std::string& aNodeName,
                                       const xercesc::DOMNode* aCurr ) = 0;

    virtual void toDebugXMLDerived( const int aPeriod,
                                    std::ostream& aOut,
                                    Tabs* aTabs ) const = 0;

    virtual const std::string& getXMLName() const = 0;
};

#endif // _ALAND_ALLOCATOR_ITEM_H_

