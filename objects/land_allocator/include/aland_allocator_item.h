#ifndef _ALAND_ALLOCATOR_ITEM_H_
#define _ALAND_ALLOCATOR_ITEM_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file aland_allocator_item.h
* \ingroup CIAM
* \brief The ALandAllocatorItem class header file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <map>
#include <xercesc/dom/DOMNode.hpp>

#include "containers/include/tree_item.h"

// Forward declarations
class Ghg;
class Summary;
class IInfo;
class Tabs;
class GDP;

/*! \brief A single item in the land allocator tree.
* \details This is the abstract base class of all nodes and leaves in the land
*          allocation tree, including the root. It inherits from the TreeItem
*          class so that it can make use of the tree library functions.
*/
class ALandAllocatorItem : public TreeItem<ALandAllocatorItem>
{
public:
    ALandAllocatorItem();
    virtual ~ALandAllocatorItem();

    // Tree Item methods.
    virtual size_t getNumChildren() const = 0;
    virtual const ALandAllocatorItem* getChildAt( const size_t aIndex ) const = 0;
    virtual ALandAllocatorItem* getChildAt( const size_t aIndex ) = 0;
    
    void XMLParse( const xercesc::DOMNode* node );
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    
    const std::string& getName() const;
    void setName( const std::string& nameIn );
    double getShare( int period );

    virtual void completeInit( const std::string& aRegionName, 
                               const IInfo* aRegionInfo ) = 0;

    virtual void addLandUsage( const std::string& aLandType,
                               const std::string& aProductName ) = 0;

    virtual double getLandAllocation( const std::string& aProductName,
                                      const int aPeriod ) const = 0;

    virtual double getTotalLandAllocation ( const std::string& productName, int period ) = 0;
    virtual double getBaseLandAllocation ( int period ) = 0;

    void normalizeLandAllocation( double sum, int period );

    virtual void setInitShares( double landAllocationAbove, int period ) = 0;
    virtual void setIntrinsicYieldMode( double intrinsicRateAbove, double sigmaAbove, int period ) = 0;

    virtual void setIntrinsicRate( const std::string& aRegionName,
                                   const std::string& aLandType,
                                   const std::string& aProductName,
                                   const double aIntrinsicRate, 
                                   const int aPeriod ) = 0;
    
    virtual double getCalAveObservedRateInternal( const std::string& aLandType,
                                                  const int aPeriod,
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

    virtual void addChild( ALandAllocatorItem* child ) = 0;
    
    virtual void calcLandShares( const std::string& aRegionName,
                                 const double aSigmaAbove,
                                 const double aTotalLandAllocated,
                                 const int aPeriod ) = 0;

    virtual void calcLandAllocation( double sigmaAbove, int period ) = 0;
    
    virtual void calcYieldInternal( const std::string& aLandType,
                                    const std::string& aProductName,
                                    const double aProfitRate,
                                    const double aAvgIntrinsicRate,
                                    const int aPeriod ) = 0;

    virtual double getYield( const std::string& aLandType,
                             const std::string& aProductName,
                             const int aPeriod ) const = 0;

    virtual void csvOutput( const std::string& aRegionName ) const = 0; 
    virtual void dbOutput( const std::string& aRegionName ) const = 0;
    virtual bool isProductionLeaf() const = 0;
    
    virtual void setUnmanagedLandAllocation( const std::string& aRegionName,
                                             const double aLandAllocation,
                                             const int aPeriod ) = 0;

    virtual void setUnmanagedLandValues( const std::string& aRegionName,
                                         const int aPeriod ) = 0;
    
    virtual void calcEmission( const std::string& aRegionName,
                               const GDP* aGDP,
                               const int aPeriod ) = 0;

    virtual void updateSummary ( Summary& aSummary, const int aPeriod ) = 0;
protected:
    std::vector<double> share; //!< percent of land
    std::vector<double> intrinsicRate; //!< rate in dollars to rent the land
    //! land allocated in 1000's of hectars
    std::vector<double> landAllocation;

    std::vector< std::vector <Ghg*> > mGHGs; //!< vector of suites of greenhouse gases
    std::vector< std::map<std::string,int> > ghgNameMap; //!< vector of Maps of ghg name to integer position in vector. 
    std::vector<Summary> summary; //!< summary for reporting
    std::string name; //!< name of the product for leafs. name of the type of land for nodes.

    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ) = 0;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const = 0;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
    virtual const std::string& getXMLName() const = 0;
};

#endif // _ALAND_ALLOCATOR_ITEM_H_

