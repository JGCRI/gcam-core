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
* \date $Date$
* \version $Revision$
*/
#include "land_allocator/include/iland_allocator.h"
#include "land_allocator/include/land_node.h"

class IInfo;
class GDP;

/*! \brief Root of a single land allocation tree.
* \details The land allocator root contains the root of the land allocation tree
*          and controls all access from the model into the land allocation
*          system. This is accomplished by implementing the ILandAllocator
*          interface, which is the only interface to which Regions have access.
*          Many methods on this interface are implemented by directly calling
*          the LandAllocatorNode functions.
*/
class TreeLandAllocator : public ILandAllocator,
                          public LandNode {
public:
    TreeLandAllocator();
    virtual ~TreeLandAllocator();
    static const std::string& getXMLNameStatic();

    // ILandAllocator methods.
    virtual void XMLParse( const xercesc::DOMNode* aNode );
    
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;
    
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;
    
    virtual void addLandUsage( const std::string& aLandType,
                               const std::string& aProductName );

    virtual double getCalAveObservedRate( const std::string& LandType,
                                          const int aPeriod ) const;
    
    virtual double getLandAllocation( const std::string& aProductName,
                                      const int aPeriod ) const;

    virtual void applyAgProdChange( const std::string& aLandType,
                                    const std::string& aProductName,
                                    const double aAgProdChange,
                                    const int aPeriod );

    virtual void calcYield( const std::string& aLandType,
                            const std::string& aProductName,
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
    
    virtual void csvOutput( const std::string& aRegionName ) const; 
    
    virtual void dbOutput( const std::string& aRegionName ) const;
    
    virtual void calcEmission( const std::string& aRegionName,
                               const GDP* aGDP,
                               const int aPeriod );
    
    virtual void updateSummary( Summary& aSummary, const int aPeriod );

    // Land allocator node methods.
    double getAvgIntrinsicRate( int year );
    virtual void setInitShares( double landAllocationAbove, int period );

    virtual void calcLandShares( const std::string& aRegionName,
                                 const double aSigmaAbove,
                                 const double aTotalLandAllocated,
                                 const int aPeriod );

    virtual void calcLandAllocation( double landAllocationAbove, int period );
protected:
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
};

#endif // _LAND_ALLOCATOR_ROOT_H_
