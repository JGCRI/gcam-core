#ifndef _LAND_NODE_H_
#define _LAND_NODE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file land_node.h
* \ingroup Objects
* \brief The LandAllocatorNode class header file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "land_allocator/include/aland_allocator_item.h"

// Forward declarations
class GDP;
class Ghg;

/*! \brief A node in the land allocation tree.
* \details A land allocator node represents a type of land available for
*          producing crops. It does not itself produce anything, but may have
*          leaves below it which produce products. The land node may also
*          contain other land nodes, or land types. Land nodes are always
*          read-in and are never created dynamically.
*/
class LandNode : public ALandAllocatorItem {
public:
    LandNode();
    virtual ~LandNode();

    // Tree Item methods.
    virtual size_t getNumChildren() const;
    virtual const ALandAllocatorItem* getChildAt( const size_t aIndex ) const;
    virtual ALandAllocatorItem* getChildAt( const size_t aIndex );

    static const std::string& getXMLNameStatic();
    
    virtual void completeInit( const std::string& aRegionName, 
                               const IInfo* aRegionInfo );

    virtual void addLandUsage( const std::string& aLandType,
                               const std::string& aProductName,
                               const ILandAllocator::LandUsageType aLandUsageType );

    virtual void setInitShares( double landAllocationAbove, int period );
    virtual void setIntrinsicYieldMode( double intrinsicRateAbove, double sigmaAbove, int period );
    
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

    virtual double getCalAveObservedRateInternal( const std::string& aLandType,
                                                  const int aPeriod,
                                                  const double aSigma ) const;

    virtual void applyAgProdChange( const std::string& aLandType,
                                    const std::string& aProductName,
                                    const double aAgProdChange,
                                    const int aPeriod );

    virtual void calcLandShares( const std::string& aRegionName,
                                 const double aSigmaAbove,
                                 const double aTotalLandAllocated,
                                 const int aPeriod );

    virtual void calcLandAllocation ( double landAllocationAbove, int period );
    
    virtual void calcYieldInternal( const std::string& aLandType,
                                    const std::string& aProductName,
                                    const double aProfitRate,
                                    const double aAvgIntrinsicRate,
                                    const int aPeriod );

    virtual double getYield( const std::string& aLandType,
                             const std::string& aProductName,
                             const int aPeriod ) const;
    
    virtual double getLandAllocation( const std::string& aProductName,
                                      const int aPeriod ) const;

    virtual double getTotalLandAllocation ( const std::string& productName, int period );
    virtual double getBaseLandAllocation ( int period );
    
    virtual void setUnmanagedLandAllocation( const std::string& aRegionName,
                                             const double aLandAllocation,
                                             const int aPeriod );

    virtual void csvOutput( const std::string& aRegionName ) const;
    virtual void dbOutput( const std::string& aRegionName ) const;
    virtual bool isProductionLeaf() const;

    virtual void setUnmanagedLandValues( const std::string& aRegionName,
                                         const int aPeriod );

    virtual void setCarbonContent( const std::string& aLandType,
                                   const std::string& aProductName,
                                   const double aAboveGroundCarbon,
                                   const double aBelowGroundCarbon,
                                   const int aPeriod );

    virtual void calcEmission( const std::string& aRegionName,
                               const GDP* aGDP,
                               const int aPeriod );
    virtual void updateSummary ( Summary& aSummary, const int aPeriod );

	virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;

protected:
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual const std::string& getXMLName() const;
    virtual void addChild( ALandAllocatorItem* child );

    //! Exponential constant used to distribute land shares.
    double sigma;

    //! List of the children of this land node located below it in the land
    //! allocation tree.
    std::vector<ALandAllocatorItem*> children;
};

#endif // _LAND_NODE_H_
