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

class Tabs;
class ICarbonCalc;

/*! \brief A LandLeaf is the leaf of a land allocation tree.
* \details A leaf in the land allocator which represents the land used to
*          produce a single crop. Land leaves can be seperated into two
*          categories, managed land leaves which are created by farming
*          technologies, and unmanaged land leaves which are created through
*          input to contain unmanaged arrable land.
*/
class LandLeaf : public ALandAllocatorItem {
public:
    LandLeaf();
    virtual ~LandLeaf();

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
   
    virtual double getCalAveObservedRateInternal( const std::string& aLandType,
                                                  const int aPeriod,
                                                  const double aSigma ) const;

    virtual void setCalObservedYield( const std::string& aLandType,
                                      const std::string& aProductName,
                                      const double aCalObservedYield, 
                                      const int aPeriod );

    virtual void applyAgProdChange( const std::string& aLandType,
                                    const std::string& aProductName,
                                    const double aAgProdChange,
                                    const int aPeriod );

    virtual void calcLandShares( const std::string& aRegionName,
                                 const double aSigmaAbove,
                                 const double aTotalLandAllocated,
                                 const int aPeriod );

    virtual void calcLandAllocation( double landAllocationAbove, int period );
    
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

    virtual void updateSummary( Summary& aSummary, const int aPeriod );

	virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    std::vector<double> intrinsicYieldMode;
    std::vector<double> calObservedYield;
    std::vector<double> yield;

	std::auto_ptr<ICarbonCalc> mCarbonContentCalc;

    //! Interest rate stored from the region info.
    double mInterestRate;

    double getCarbonValue( const std::string& aRegionName, const int aPeriod ) const;
    virtual void initCarbonCycle();

    std::map<std::string,double> emissmap; //! < Emissions mapping of ghg names to values
    std::vector<double> agProdChange;  //!< The technological change factor.
    std::vector<double> production;  //!< The production output of this leaf.
    std::vector <Ghg*> mGHGs; //!< vector of suites of greenhouse gases
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;

    virtual const std::string& getXMLName() const;
    virtual void addChild( ALandAllocatorItem* child );
    virtual void checkCalObservedYield( const int aPeriod ) const;
};

#endif // _LAND_LEAF_H_

