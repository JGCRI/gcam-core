#ifndef _TREE_LAND_ALLOCATOR_H_
#define _TREE_LAND_ALLOCATOR_H_
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
 * \file tree_land_allocator.h
 * \ingroup Objects
 * \brief The TreeLandAllocator class header file.
 * \author James Blackwood, Josh Lurz
 */
#include "land_allocator/include/iland_allocator.h"
#include "land_allocator/include/land_node.h"
#include "util/base/include/ivisitable.h"

class IInfo;

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
 *          - XML name: \c LandAllocatorRoot
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
                               const LandUsageType aLandUsageType,
                               const int aPeriod );

    virtual double getAverageLandProfitRate( const int aPeriod,
                                                   const std::string& aLandType ) const;
    
    virtual double getLandAllocation( const std::string& aLandType,
                                      const std::string& aProductName,
                                      const int aPeriod ) const;

    virtual void applyAgProdChange( const std::string& aLandType,
                                    const std::string& aProductName,
                                    const double aAgProdChange,
                                    const int aHarvestPeriod, 
                                    const int aCurrentPeriod );

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

    virtual void setMaxYield( const std::string& aLandType,
                                      const std::string& aProductName,
                                      const double aMaxYield, 
                                      const int aPeriod );

    virtual void setCarbonPriceIncreaseRate( const double aCarbonPriceIncreaseRate,
                                        const int aPeriod );

    virtual void setSoilTimeScale( const int aTimeScale );

    virtual void setIntrinsicRate( const std::string& aRegionName,
                                   const std::string& aLandType,
                                   const std::string& aProductName,
                                   const double aIntrinsicRate,
                                   const int aPeriod );
   
    virtual void calcFinalLandAllocation( const std::string& aRegionName, 
                                          const int aPeriod );
    void calcFinalLandAllocationHelper( const std::string& aRegionName,
                                        const int aYear );

    virtual void completeInit( const std::string& aRegionName, 
                               const IInfo* aRegionInfo );
    
    virtual void initCalc( const std::string& aRegionName,
                           const int aPeriod );

    virtual void setCarbonContent( const std::string& aLandType,
                                   const std::string& aProductName,
                                   const double aAboveGroundCarbon,
                                   const double aBelowGroundCarbon,
                                   const int aMatureAge,    
                                   const int aPeriod );

    virtual void csvOutput( const std::string& aRegionName ) const; 
    
    virtual void dbOutput( const std::string& aRegionName ) const;

    virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const;

    // Land allocator node methods.
    virtual void setInitShares( const std::string& aRegionName,
                                const double aSigmaAbove,
                                const double aLandAllocationAbove,
                                const double aParentHistoryShare,
                                const LandUseHistory* aParentHistory,
                                const int aPeriod );

    virtual double calcLandShares( const std::string& aRegionName,
                                   const double aSigmaAbove,
                                   const double aTotalLandAllocated,
                                   const int aPeriod );

     virtual void calcLandAllocation( const std::string& aRegionName,
                                     const double aLandAllocationAbove,
                                     const int aYear );
     virtual void calcLUCCarbonFlowsOut( const std::string& aRegionName,
                                             const int aYear );

    virtual void calcLUCCarbonFlowsIn( const std::string& aRegionName,
                                              const int aYear );

    virtual void calcCarbonBoxModel( const std::string& aRegionName,
                                             const int aYear );
protected:
    virtual const std::string& getXMLName() const;

    virtual bool XMLDerivedClassParse( const std::string& aNodeName,
                                       const xercesc::DOMNode* aCurr );

    virtual void toInputXMLDerived( std::ostream& aOutput,
                                    Tabs* aTabs ) const;
private:
    //! Land allocated in 1000's of hectares
    objects::PeriodVector<Value> mLandAllocation;

    //! Rate at which carbon price is expected to increase
    objects::PeriodVector<Value> mCarbonPriceIncreaseRate;

    //! Boolean storing whether a year has been calculated or not.
    objects::YearVector<bool> mCalculated;

    //! Flag to indicate that calibration data is present
    objects::PeriodVector<bool> mCalDataExists;

    //! Double storing the average price of land in a region
    double mAvgProfitRate;

    //! Integer storing the soil time scale for a region
    int mSoilTimeScale;

    const ALandAllocatorItem* findParentOfType( const std::string& aType ) const;

    void checkRotationPeriod( const IInfo* aRegionInfo ) const;

    void adjustTotalLand( const int aPeriod );

    void resetToCalibrationData( const std::string& aRegionName, const int aPeriod );
};

#endif // _LAND_ALLOCATOR_ROOT_H_
