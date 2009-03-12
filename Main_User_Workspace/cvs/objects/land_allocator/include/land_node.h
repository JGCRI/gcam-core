#ifndef _LAND_NODE_H_
#define _LAND_NODE_H_
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
 * \file land_node.h
 * \ingroup Objects
 * \brief The LandNode class header file.
 * \author James Blackwood
 */

#include <vector>
#include <memory>
#include <xercesc/dom/DOMNode.hpp>
#include "land_allocator/include/aland_allocator_item.h"
#include "util/base/include/value.h"

// Forward declarations
class LandUseHistory;
class ICarbonCalc;
/*!
 * \brief A node in the land allocation tree.
 * \details A land allocator node represents a type of land available for
 *          producing crops. It does not itself produce anything, but may have
 *          leaves below it which produce products. The land node may also
 *          contain other land nodes, or land types. Land nodes are always
 *          read-in and are never created dynamically.
 *
 *          <b>XML specification for LandNode</b>
 *          - XML name: -c LandAllocatorNode
 *          - Contained by: LandAllocatorRoot
 *          - Parsing inherited from class: None
 *          - Attributes:
 *              - \c name ALandAllocatorItem::mName
 *          - Elements:
 *              - \c LandNode LandNode::children
 *              - \c UnmanagedLandLeaf LandNode::children
 *              - \c sigma LandNode::mSigma
 *              - \c land-use-history LandNode::mLandUseHistory
 */
class LandNode : public ALandAllocatorItem {
public:
    explicit LandNode( const ALandAllocatorItem* aParent );

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
                               const ILandAllocator::LandUsageType aLandUsageType,
                               const int aPeriod );

    virtual void setInitShares( const std::string& aRegionName,
                                const double aSigmaAbove,
                                const double aLandAllocationAbove,
                                const double aParentHistoryShare,
                                const LandUseHistory* aParentHistory,
                                const int aPeriod );

    virtual void resetToCalLandAllocation( const int aPeriod );

    virtual void setIntrinsicYieldMode( const double aIntrinsicYieldAbove,
                                        const double aSigmaAbove,
                                        const int aPeriod );

    virtual void setActualCarbonMult( const double aCarbonMultAbove,
                                        const double aSigmaAbove,
                                        const int aPeriod );
    
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

    virtual void setMaxYield( const std::string& aLandType,
                                      const std::string& aProductName,
                                      const double aMaxYield, 
                                      const int aPeriod );

    virtual void setCarbonPriceIncreaseRate( const double aCarbonPriceIncreaseRate, 
                                      const int aPeriod );

    /*!
     * \brief Set the number of years needed to for soil carbons emissions/uptake
     * \details This method sets the soil time scale into the carbon calculator
     *          for each land leaf.
     * \param aTimeScale soil time scale (in years)
     * \author Kate Calvin
     */
    virtual void setSoilTimeScale( const int aTimeScale );


    virtual void applyAgProdChange( const std::string& aLandType,
                                    const std::string& aProductName,
                                    const double aAgProdChange,
                                    const int aHarvestPeriod, 
                                    const int aCurrentPeriod );

    virtual double calcLandShares( const std::string& aRegionName,
                                   const double aSigmaAbove,
                                   const double aTotalLandAllocated,
                                   const int aPeriod );

    virtual void calcLandAllocation( const std::string& aRegionName,
                                     const double aLandAllocationAbove,
                                     const int aPeriod );
    
    virtual void calcLUCCarbonFlowsOut( const std::string& aRegionName,
                                        const int aYear );

    virtual void calcLUCCarbonFlowsIn( const std::string& aRegionName,
                                       const int aYear );

    virtual void calcCarbonBoxModel( const std::string& aRegionName,
                                     const int aYear );
    
    virtual void calcYieldInternal( const std::string& aLandType,
                                    const std::string& aProductName,
                                    const std::string& aRegionName,
                                    const double aProfitRate,
                                    const double aAvgIntrinsicRate,
                                    const int aHarvestPeriod,
                                    const int aCurrentPeriod );

    virtual double getYield( const std::string& aLandType,
                             const std::string& aProductName,
                             const int aPeriod ) const;
    
    virtual double getLandAllocation( const std::string& aLandType,
                                      const std::string& aProductName,
                                      const int aPeriod ) const;

    virtual double getTotalLandAllocation( const LandAllocationType aType,
                                           const int aPeriod ) const;

    virtual LandUseHistory* getLandUseHistory();

    virtual double getBaseLandAllocation( const int aPeriod ) const;
    
    virtual void setUnmanagedLandAllocation( const std::string& aRegionName,
                                             const double aNewUnmanaged,
                                             const int aPeriod );

    virtual void csvOutput( const std::string& aRegionName ) const;
    virtual void dbOutput( const std::string& aRegionName ) const;
    
    virtual bool isUnmanagedNest() const;
    
    virtual bool isConceptualRoot() const;
    
    virtual double getSigma() const;
    
    virtual void setUnmanagedLandValues( const std::string& aRegionName,
                                         const int aPeriod );

    virtual void setCarbonContent( const std::string& aLandType,
                                   const std::string& aProductName,
                                   const double aAboveGroundCarbon,
                                   const double aBelowGroundCarbon,
                                   const int aMatureAge,    
                                   const int aPeriod );

    virtual void accept( IVisitor* aVisitor, 
                         const int aPeriod ) const;

    virtual void toInputXML( std::ostream& out, 
                             Tabs* tabs ) const;

    virtual bool XMLParse( const xercesc::DOMNode* aNode );

protected:
    virtual bool XMLDerivedClassParse( const std::string& nodeName, 
                                       const xercesc::DOMNode* curr );

    virtual void toDebugXMLDerived( const int period, 
                                    std::ostream& out, 
                                    Tabs* tabs ) const;

    virtual void toInputXMLDerived( std::ostream& aOutput, 
                                    Tabs* aTabs ) const;

    virtual const std::string& getXMLName() const;

    virtual void addChild( ALandAllocatorItem* child );
    
    ALandAllocatorItem* findChild( const std::string& aName,
                                   const TreeItemType aType );
    
    const ALandAllocatorItem* findChild( const std::string& aName,
                                         const TreeItemType aType ) const;

    //! Land allocated -- used for conceptual roots
    objects::PeriodVector<Value> mLandAllocation;

    //! List of the children of this land node located below it in the land
    //! allocation tree.
    std::vector<ALandAllocatorItem*> mChildren;

    //! Exponential constant used to distribute land shares.
    Value mSigma;

    //! Container of historical land use.
    std::auto_ptr<LandUseHistory> mLandUseHistory;

    std::auto_ptr<ICarbonCalc> mCarbonBoxModelTemplate;

};

#endif // _LAND_NODE_H_
