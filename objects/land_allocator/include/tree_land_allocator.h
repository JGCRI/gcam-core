#ifndef _TREE_LAND_ALLOCATOR_H_
#define _TREE_LAND_ALLOCATOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file tree_land_allocator.h
 * \ingroup Objects
 * \brief The TreeLandAllocator class header file.
 * \author James Blackwood, Josh Lurz
 */
#include "land_allocator/include/iland_allocator.h"
#include "land_allocator/include/land_node.h"
#include "util/base/include/ivisitable.h"

class IInfo;
class GDP;

/*! 
 * \brief Root of a single land allocation tree.
 * \details The land allocator root contains the root of the land allocation
 *          tree and controls all access from the model into the land allocation
 *          system. This is accomplished by implementing the ILandAllocator
 *          interface, which is the only interface to which Regions have access.
 *          Many methods on this interface are implemented by directly calling
 *          the LandAllocatorNode functions.
 *
 *          <b>XML specification for TreeLandAllocator</b>
 *          - XML name: -c LandAllocatorRoot
 *          - Contained by: Region
 *          - Parsing inherited from class: None
 *          - Attributes:
 *              - \c name ALandAllocatorItem::mName
 *          - Elements:
 *              - \c landAllocation ALandAllocatorItem::mLandAllocation
 */
class TreeLandAllocator : public ILandAllocator,
                          public LandNode {
public:
    TreeLandAllocator();
    virtual ~TreeLandAllocator();
    static const std::string& getXMLNameStatic();

    // IParsable
    virtual bool XMLParse( const xercesc::DOMNode* aNode );
    
    // ILandAllocator methods.
    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;
    
    virtual void toInputXML( std::ostream& aOut,
                             Tabs* aTabs ) const;
    
    virtual void addLandUsage( const std::string& aLandType,
                               const std::string& aProductName,
                               const LandUsageType aLandUsageType );

    virtual double getUnmanagedCalAveObservedRate( const int aPeriod ) const;
    
    virtual double getLandAllocation( const std::string& aProductName,
                                      const int aPeriod ) const;

    virtual void applyAgProdChange( const std::string& aLandType,
                                    const std::string& aProductName,
                                    const double aAgProdChange,
                                    const int aPeriod );

    virtual void calcYield( const std::string& aLandType,
                            const std::string& aProductName,
                            const std::string& aRegionName,
                            const double aProfitRate,
                            const int aHarvestPeriod, 
                            const int aCurrentPeriod );
    
    virtual double getYield( const std::string& aLandType,
                             const std::string& aProductName,
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

    virtual void setIntrinsicRate( const std::string& aRegionName,
                                   const std::string& aLandType,
                                   const std::string& aProductName,
                                   const double aIntrinsicRate,
                                   const int aPeriod );
   
    virtual void calcFinalLandAllocation( const std::string& aRegionName, 
                                          const int aPeriod );

    virtual void completeInit( const std::string& aRegionName, 
                               const IInfo* aRegionInfo );
    
    virtual void setCarbonContent( const std::string& aLandType,
                                   const std::string& aProductName,
                                   const double aAboveGroundCarbon,
                                   const double aBelowGroundCarbon,
                                   const int aPeriod );

    virtual void csvOutput( const std::string& aRegionName ) const; 
    
    virtual void dbOutput( const std::string& aRegionName ) const;
    
    virtual void calcEmission( const std::string& aRegionName,
                               const GDP* aGDP,
                               const int aPeriod );
    
    virtual void updateSummary( Summary& aSummary,
                                const int aPeriod );

	virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const;

    // Land allocator node methods.
    double getAvgIntrinsicRate( const int aPeriod ) const;

    virtual void setInitShares( const double aLandAllocationAbove,
                                const LandUseHistory* aLandUseHistory,
                                const int aPeriod );

    virtual double calcLandShares( const std::string& aRegionName,
                                   const double aSigmaAbove,
                                   const double aTotalLandAllocated,
                                   const int aPeriod );

     virtual void calcLandAllocation( const std::string& aRegionName,
                                     const double aLandAllocationAbove,
                                     const int aPeriod );
protected:
    virtual const std::string& getXMLName() const;

    virtual bool XMLDerivedClassParse( const std::string& aNodeName,
                                       const xercesc::DOMNode* aCurr );

    virtual void toInputXMLDerived( std::ostream& aOutput,
                                    Tabs* aTabs ) const;
private:
    //! Land allocated in 1000's of hectars
    objects::PeriodVector<double> mLandAllocation;

    void checkRotationPeriod( const IInfo* aRegionInfo ) const;

    void adjustTotalLand( const int aPeriod );
};

#endif // _LAND_ALLOCATOR_ROOT_H_
