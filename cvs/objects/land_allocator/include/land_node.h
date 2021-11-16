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
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/



/*! 
 * \file land_node.h
 * \ingroup Objects
 * \brief The LandNode class header file.
 * \author James Blackwood
 */

#include <vector>
#include <memory>
#include "land_allocator/include/aland_allocator_item.h"

// Forward declarations
class LandUseHistory;
class NodeCarbonCalc;
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
 *              - \c land-use-history LandNode::mLandUseHistory
 *              - \c node-carbon-calc LandNode::mCarbonCalc
 */
class LandNode : public ALandAllocatorItem {
public:
    explicit LandNode( const ALandAllocatorItem* aParent );
    
    explicit LandNode();

    virtual ~LandNode();

    // Tree Item methods.
    virtual size_t getNumChildren() const;

    virtual const ALandAllocatorItem* getChildAt( const size_t aIndex ) const;

    virtual ALandAllocatorItem* getChildAt( const size_t aIndex );

    static const std::string& getXMLNameStatic();
    
    virtual const std::string& getXMLName() const;
    
    virtual void completeInit( const std::string& aRegionName, 
                               const IInfo* aRegionInfo );
    
    virtual void initCalc( const std::string& aRegionName,
                           const int aPeriod );

    virtual void setInitShares( const std::string& aRegionName,
                                const double aLandAllocationAbove,
                                const int aPeriod );

    virtual void calculateNodeProfitRates( const std::string& aRegionName,
                                           const int aPeriod );

    virtual void calculateShareWeights( const std::string& aRegionName, 
                                        IDiscreteChoice* aChoiceFnAbove,
                                        const int aPeriod,
                                        const bool aCalcFutureSW );

    virtual void setProfitRate( const std::string& aRegionName,
                                   const std::string& aProductName,
                                   const double aProfitRate,
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

    virtual double calcLandShares( const std::string& aRegionName,
                                   IDiscreteChoice* aChoiceFnAbove,
                                   const int aPeriod );

    virtual void calcLandAllocation( const std::string& aRegionName,
                                     const double aLandAllocationAbove,
                                     const int aPeriod );
    
    virtual void calcLUCEmissions( const std::string& aRegionName,
                                   const int aYear, const int aEndYear,
                                   const bool aStoreFullEmiss );
    
    virtual double getLandAllocation( const std::string& aProductName,
                                      const int aPeriod ) const;

    virtual double getCalLandAllocation( const LandAllocationType aType,
                                         const int aPeriod ) const;

    virtual LandUseHistory* getLandUseHistory();
        
    virtual void setUnmanagedLandProfitRate( const std::string& aRegionName, 
                                             double aAverageProfitRate,
                                             const int aPeriod );
    
    virtual void getObservedAverageProfitRate( double& aProfitRate, double& aShare, const int aPeriod ) const;
    
    virtual const ALandAllocatorItem* getChildWithHighestShare( const bool aIncludeAllChildren,
                                                                const int aPeriod ) const;
    
	virtual bool isUnmanagedLandLeaf( )  const;

    virtual void accept( IVisitor* aVisitor, 
                         const int aPeriod ) const;

protected:
    virtual void setParent( const ALandAllocatorItem* aParent );
    
    virtual void toDebugXMLDerived( const int period, 
                                    std::ostream& out, 
                                    Tabs* tabs ) const;
    
    ALandAllocatorItem* findChild( const std::string& aName,
                                   const LandAllocatorItemType aType );
    
    const ALandAllocatorItem* findChild( const std::string& aName,
                                         const LandAllocatorItemType aType ) const;
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        ALandAllocatorItem,

        //! Logit exponent -- should be positive since we are sharing on profit
        DEFINE_VARIABLE( CONTAINER, "discrete-choice-function", mChoiceFn, IDiscreteChoice* ),

        //! Double storing the average price of land in a region or subregion
        DEFINE_VARIABLE( SIMPLE | STATE, "unManagedLandValue", mUnManagedLandValue, Value ),

        //! List of the children of this land node located below it in the land
        //! allocation tree.
        DEFINE_VARIABLE( CONTAINER, "child-nodes", mChildren, std::vector<ALandAllocatorItem*> ),

        //! Container of historical land use.
        DEFINE_VARIABLE( CONTAINER, "land-use-history", mLandUseHistory, LandUseHistory* ),

        //! (optional) A carbon calculation which can used when children maybe similar
        //! in terms of switching between them does not mean carbon is emitted per se.
        DEFINE_VARIABLE( CONTAINER, "node-carbon-calc", mCarbonCalc, NodeCarbonCalc* )
    )
};

#endif // _LAND_NODE_H_
