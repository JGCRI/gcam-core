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
 * \file land_node.cpp
 * \ingroup Objects
 * \brief LandNode class source file.
 * \author James Blackwood
 */

#include "util/base/include/definitions.h"
#include "util/base/include/xml_helper.h"
#include <xercesc/dom/DOMNodeList.hpp>
#include "land_allocator/include/land_node.h"
#include "land_allocator/include/unmanaged_land_leaf.h"
#include "land_allocator/include/carbon_land_leaf.h"
#include "land_allocator/include/land_use_history.h"
#include "ccarbon_model/include/node_carbon_calc.h"
#include "containers/include/iinfo.h"
#include "containers/include/scenario.h"
#include "util/base/include/ivisitor.h"
#include "functions/include/idiscrete_choice.hpp"
#include "functions/include/discrete_choice_factory.hpp"
#include "sectors/include/sector_utils.h"
#include <numeric>
#include <utility>

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
typedef std::map<unsigned int, double> LandMapType;
/*!
 * \brief Constructor.
 * \param aParent Pointer to this leafs's parent.
 * \author James Blackwood
 */
LandNode::LandNode( const ALandAllocatorItem* aParent )
: ALandAllocatorItem( aParent, eNode ),
mChoiceFn( 0 ),
mUnManagedLandValue( 0.0 ),
mLandUseHistory( 0 ),
mCarbonCalc( 0 )
 {
    mType = eNode;
 }

//! Destructor
LandNode::~LandNode() {
    for( unsigned int i = 0; i < mChildren.size(); i++ ) {
        delete mChildren[ i ];
    }
}

size_t LandNode::getNumChildren() const {
    return mChildren.size();
}

const ALandAllocatorItem* LandNode::getChildAt( const size_t aIndex ) const {
    // aIndex must be less than the size of the child vector.
    assert( aIndex < mChildren.size() );
    return mChildren[ aIndex ];
}

ALandAllocatorItem* LandNode::getChildAt( const size_t aIndex ) {
    // aIndex must be less than the size of the child vector.
    assert( aIndex < mChildren.size() );
    return mChildren[ aIndex ];
}

bool LandNode::XMLParse( const xercesc::DOMNode* aNode ){

    // assume we are passed a valid node.
    assert( aNode );
    
    // Set the node name.
    mName = XMLHelper<string>::getAttr( aNode, "name" );

    // get all the children.
    DOMNodeList* nodeList = aNode->getChildNodes();
    
    for( unsigned int i = 0; i < nodeList->getLength(); ++i ){
        const DOMNode* curr = nodeList->item( i );
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if ( nodeName == LandNode::getXMLNameStatic() ) {
            parseContainerNode( curr, mChildren, new LandNode( this ) );
        }
        else if ( nodeName == UnmanagedLandLeaf::getXMLNameStatic() ) {
            parseContainerNode( curr, mChildren, new UnmanagedLandLeaf( this ) );
        }
        else if ( nodeName == CarbonLandLeaf::getXMLNameStatic() ) {
            parseContainerNode( curr, mChildren, new CarbonLandLeaf( this ) );
        }
        else if ( nodeName == LandLeaf::getXMLNameStatic() ) {
            parseContainerNode( curr, mChildren, new LandLeaf( this, "" ) );
        }
        else if( DiscreteChoiceFactory::isOfType( nodeName ) ) {
            parseSingleNode( curr, mChoiceFn, DiscreteChoiceFactory::create( nodeName ).release() );
        }
        else if( nodeName == LandUseHistory::getXMLNameStatic() ){
            parseSingleNode( curr, mLandUseHistory, new LandUseHistory );
        }
        else if( nodeName == NodeCarbonCalc::getXMLNameStatic() ){
            parseSingleNode( curr, mCarbonCalc, new NodeCarbonCalc );
        }
        else if( nodeName == "ghost-unnormalized-share" ){
            XMLHelper<Value>::insertValueIntoVector( curr, mGhostUnormalizedShare, scenario->getModeltime() );
        }
        else if( nodeName == "unManagedLandValue" ){
            mUnManagedLandValue = XMLHelper<double>::getValue( curr );
        }
        else if ( !XMLDerivedClassParse( nodeName, curr ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing "
                    << getXMLName() << "." << endl;
        }
    }

    // TODO: Improve error checking
    return true;
}

bool LandNode::XMLDerivedClassParse( const std::string& aNodeName,
                                     const xercesc::DOMNode* aCurr )
{
    return false;
}

void LandNode::toInputXML( ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag ( getXMLName(), out, tabs, mName );

    const Modeltime* modeltime = scenario->getModeltime();
    for( int period = 0; period < modeltime->getmaxper(); ++period ) {
        if( mGhostUnormalizedShare[ period ].isInited() ) {
            const int year = modeltime->getper_to_yr( period );
            XMLWriteElement( mGhostUnormalizedShare[ period ], "ghost-unnormalized-share", out, tabs, year );
        }
    }
    XMLWriteElement( mUnManagedLandValue, "unManagedLandValue", out, tabs );  

    if( mLandUseHistory.get() ){
        mLandUseHistory->toInputXML( out, tabs );
    }
    
    if( mCarbonCalc.get() ) {
        mCarbonCalc->toInputXML( out, tabs );
    }

    if( mChoiceFn.get() ) {
        mChoiceFn->toInputXML( out, tabs );
    }
    
    // write out for mChildren
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[i]->toInputXML( out, tabs );
    }

    toInputXMLDerived( out, tabs );
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

void LandNode::toInputXMLDerived( ostream& aOut, Tabs* aTabs ) const {
    // Allow derived classes to override.
}

void LandNode::toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {
    XMLWriteElement( mUnManagedLandValue, "unManagedLandValue", out, tabs );

    if( mLandUseHistory.get() ){
        mLandUseHistory->toDebugXML( period, out, tabs );
    }
    if( mCarbonCalc.get() ) {
        mCarbonCalc->toDebugXML( period, out, tabs );
    }
    if( mChoiceFn.get() ) {
        mChoiceFn->toDebugXML( period, out, tabs );
    }
    // write out for mChildren
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[i]->toDebugXML( period, out, tabs );
    }
}

const string& LandNode::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author James Blackwood
* \return The XML name of the object.
*/
const string& LandNode::getXMLNameStatic() {
    const static string XML_NAME = "LandNode";    // original XML text
    return XML_NAME;
}

void LandNode::completeInit( const string& aRegionName,
                             const IInfo* aRegionInfo )
{
    if( !mChoiceFn.get() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "No Discrete Choice function set in " << getXMLName() << " for "
                << aRegionName << ", " << mName << endl;
        abort();
    }
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->completeInit( aRegionName, aRegionInfo );
    }

    const double privateDiscountRateLand = aRegionInfo->getDouble( "private-discount-rate-land", true );
    if( mCarbonCalc.get() ) {
        accept( mCarbonCalc.get(), -1 );
        mCarbonCalc->completeInit( privateDiscountRateLand );
    }
}

void LandNode::initCalc( const string& aRegionName, const int aPeriod )
{
    // TODO: all kinds of things including error checking
    if ( aPeriod > 1 ) {
        // Copy share weights forward if new ones haven't been read in or computed
        if ( !mShareWeight[ aPeriod ].isInited() ) {
            mShareWeight[ aPeriod ] = mShareWeight[ aPeriod - 1 ];
        }
    }

    // Call initCalc on any children
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->initCalc( aRegionName, aPeriod );
    }
}

/*!
* \brief Initializes the share of land of a node
* \details Calculates the share of land allocated to a node and calls
*          a similar method for the node's children. This method is
*          called during the calibration process so the shares 
*          set are prior to any calculations of share weights.
* \param aRegionName Region.
* \param aLandAllocationAbove Land allocation of the parent node
* \param aPeriod Model period
*/
void LandNode::setInitShares( const string& aRegionName,
                                const double aLandAllocationAbove,
                                const int aPeriod )
{
    // Calculate the total land within this node.
    double nodeLandAllocation = getCalLandAllocation( eAnyLand, aPeriod );
    
    // If there is no land allocation for the parent land type, set the share to
    // a small number.
    if( aLandAllocationAbove <= 0.0 ){
        mShare[ aPeriod ] = 0.0;
    }
    // Otherwise, set the share of this node
    else {
        mShare[ aPeriod ] = nodeLandAllocation / aLandAllocationAbove;
   }

    // Call setInitShares on all children
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {        
        mChildren[ i ]->setInitShares( aRegionName,
                                       nodeLandAllocation,
                                       aPeriod );
    }
}

/*!
* \brief Sets land leaf's profit rate
* \details Locates a land leaf and calls the setProfitRate
*          method for that leaf. The profit rate provided is
*          passed in from the ag production technology.
* \param aRegionName Region
* \param aProductName Name of the product grown on the land
* \param aProfitRate Profit rate passed in from ag food production technology
* \param aPeriod Period.
*/
void LandNode::setProfitRate( const string& aRegionName,
                                 const string& aProductName,
                                 const double aProfitRate,
                                 const int aPeriod )
{
    ALandAllocatorItem* curr = findChild( aProductName, eLeaf );
    // Set the rental rate.
    if( curr ){
        curr->setProfitRate( aRegionName, aProductName,
                                aProfitRate, aPeriod );
    } 
}

void LandNode::setCarbonPriceIncreaseRate( const double aCarbonPriceIncreaseRate,
                                    const int aPeriod )
{
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->setCarbonPriceIncreaseRate( aCarbonPriceIncreaseRate, aPeriod );
    }
}

/*!
* \brief Set the number of years needed to for soil carbons emissions/uptake
* \details This method sets the soil time scale into the carbon calculator
*          for each land leaf.
* \param aTimeScale soil time scale (in years)
* \author Kate Calvin
*/
void LandNode::setSoilTimeScale( const int aTimeScale ) {

    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->setSoilTimeScale( aTimeScale );
    }

}

/*!
* \brief Calculates the share of land allocated to the node and its children
* \details Uses the logit formulation to calculate the share
*          of land allocated to a particular land type. A node's share
*          is based on its profit rate and distribution parameter.
*          A node's profit rate is NOT the weighted average of its
*          childrens' profit rates but is based on the J. Clarke and Edmonds
*          Logit paper and uses the scaled profits of the child nodes and leafs.
* \param aRegionName Region
* \param aChoiceFnAbove The discrete choice function from the level above.
* \param aPeriod Period.
*/
double LandNode::calcLandShares( const string& aRegionName,
                                 IDiscreteChoice* aChoiceFnAbove,
                                 const int aPeriod )
{

    vector<double> unnormalizedShares( mChildren.size() );

    // Step 1.  Calculate the unnormalized shares.
    // These calls need to be made to initiate recursion into lower nests even
    // if the current node will have fixed shares.
    // Note these are the log( unnormalized shares )
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        unnormalizedShares[ i ] = mChildren[ i ]->calcLandShares( aRegionName,
                                                                  mChoiceFn.get(),
                                                                  aPeriod );
    }

    // Step 2 Normalize and set the share of each child
    // The log( unnormalized ) shares will be normalizd after this call and it will
    // do it making an attempt to avoid numerical instabilities given the profit rates
    // may be large values.  The value returned is a pair<unnormalizedSum, log(scale factor)>
    // again in order to try to make calculations in a numerically stable way.
    pair<double, double> unnormalizedSum = SectorUtils::normalizeLogShares( unnormalizedShares );
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->setShare( unnormalizedShares[ i ], aPeriod );
    }

    // Step 3 Option (a) . compute node profit based on share denominator
    mProfitRate[ aPeriod ] = mChoiceFn->calcAverageCost( unnormalizedSum.first, unnormalizedSum.second, aPeriod );

    // Step 4. Calculate the unnormalized share for this node, but here using the discrete choice of the 
    // containing or parant node.  This will be used to determine this nodes share within its 
    // parent node.
    double unnormalizedShareAbove = aChoiceFnAbove->calcUnnormalizedShare( mShareWeight[ aPeriod ], mProfitRate[ aPeriod ], aPeriod );
    
    return unnormalizedShareAbove; // the unnormalized share of this node.
}

void LandNode::calculateShareWeights( const string& aRegionName, 
                                      IDiscreteChoice* aChoiceFnAbove,
                                      const int aPeriod )
{

    // we can use the base class implementation to calculate the share weight at this node.
    ALandAllocatorItem::calculateShareWeights( aRegionName, aChoiceFnAbove, aPeriod );
    
    // Call share weight calculation for each child
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->calculateShareWeights( aRegionName, mChoiceFn.get(), aPeriod );
    }

}

/*!
 * \brief Sets the base profit rate for all unmanaged land leafs
 * \details Unmanaged land leafs have a base profit rate that
 *          is equal to the average profit rate of that region
 *          or subregion. 
 * \param aRegionName Region name.
 * \param aAverageProfitRate Average profit rate of region or subregion.
 * \param aPeriod model period.
 */
void LandNode::setUnmanagedLandProfitRate( const string& aRegionName,
                                           double aAverageProfitRate,
                                           const int aPeriod )
{
    double avgProfitRate = aAverageProfitRate;
    // If node is the root of a fixed land area nest ( typically a subregion )
    // or the root of the entire land allocatory, then set the average profit
    // rate to the previously calculated value. 
    if ( mUnManagedLandValue > 0.0 ) {
        avgProfitRate = mUnManagedLandValue;
    }
    else {
        mUnManagedLandValue = avgProfitRate;
    }
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        // assign the unmanaged land value to this node and to children. 
        mChildren[ i ]->setUnmanagedLandProfitRate( aRegionName, avgProfitRate, aPeriod );
    }
}


void LandNode::calculateNodeProfitRates( const string& aRegionName,
                                         double aAverageProfitRateAbove,
                                         IDiscreteChoice* aChoiceFnAbove,
                                         const int aPeriod )
{
    const Modeltime* modeltime = scenario->getModeltime();
    // store this value in this node 
    double avgProfitRate = -util::getSmallNumber();
    // If we have a valid profit rate above then we can calculate the implied profit rate this node
    // would have to recieve the share it did.  If not (such as at the root) we just use the
    // unmanaged land value.
    if( aAverageProfitRateAbove > 0.0 ) {
        if( mShare[ aPeriod ] > 0.0 ) {
            avgProfitRate = aChoiceFnAbove->calcImpliedCost( mShare[ aPeriod ], aAverageProfitRateAbove, aPeriod );
        }
        else if( aPeriod == modeltime->getFinalCalibrationPeriod() ) {
            // It may be the case that this node contains only "future" crop/technologies.  In this case
            // we use the ghost share in it's first available year to calculate the implied profit rate.
            for( int futurePer = aPeriod + 1; futurePer < modeltime->getmaxper() && avgProfitRate < 0.0; ++futurePer ) {
                if( mGhostUnormalizedShare[ futurePer ].isInited() ) {
                    avgProfitRate = aChoiceFnAbove->calcImpliedCost( mGhostUnormalizedShare[ futurePer ],
                                                                     aAverageProfitRateAbove,
                                                                     aPeriod );
                }
            }
        }
    }
    else {
        avgProfitRate = mUnManagedLandValue;
    }

    // Store the profit rate which will be used during calibration when calculating share-weights, etc.
    mProfitRate[ aPeriod ] = avgProfitRate;
    mChoiceFn->setOutputCost( avgProfitRate );

    // pass the node profit rate down to children and trigger their calculation
    // and pass down the logit exponent of this node
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->calculateNodeProfitRates( aRegionName, avgProfitRate, 
                                                  mChoiceFn.get(), aPeriod );
    }

    // Calculate a reasonable "base" profit rate to use set the scale for when
    // changes in absolute profit rates would be made relative.  We do this by
    // taking the higest profit rate from any of the direct child items.
    double maxChildProfitRate = -std::numeric_limits<double>::infinity();
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        maxChildProfitRate = std::max( maxChildProfitRate,
                mChildren[i]->getProfitRate( aPeriod ) );
    }
    mChoiceFn->setBaseCost( maxChildProfitRate, mName );
}

/*!
 * \brief Calculates land allocation
 * \details Uses the land share and the allocation of land to 
 *          the parent node to calculate the allocation of this
 *          node. CalculateLandShares must be called first.
 * \param aRegionName Region name.
 * \param aLandAllocationAbove Land allocation of parent.
 * \param aPeriod model period.
 */
void LandNode::calcLandAllocation( const string& aRegionName,
                                   const double aLandAllocationAbove,
                                   const int aPeriod )
{
    assert( mShare[ aPeriod ] >= 0.0 && mShare[ aPeriod ] <= 1.0 );

    // Calculate node land allocation
    double nodeLandAllocation = 0.0;
    if ( aLandAllocationAbove > 0.0 && mShare[ aPeriod ] > 0.0 ) {
        nodeLandAllocation = aLandAllocationAbove * mShare[ aPeriod ];
    }
    
    // Call calcLandAllocation for each child
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->calcLandAllocation( aRegionName, nodeLandAllocation, aPeriod );
    }
}

/*!
 * \brief Calculates LUC emissions for the model period
 * \param aRegionName Region name.
 * \param aPeriod The current model period.
 * \param aEndYear The year to calculate LUC emissions to.
 */
void LandNode::calcLUCEmissions( const string& aRegionName,
                                 const int aPeriod, const int aEndYear )
{
    if( mCarbonCalc.get() ) {
        mCarbonCalc->calc( aPeriod, aEndYear );
    }
    
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->calcLUCEmissions( aRegionName, aPeriod, aEndYear );
    }
}

/*!
 * \brief Finds a child of this node that has the desired name and type.
 * \param aName The desired name.
 * \param aType The desired type.
 * \return ALandAllocatorItem pointer to the child.
 */
ALandAllocatorItem* LandNode::findChild( const string& aName,
                                         const LandAllocatorItemType aType ) {
    return findItem<ALandAllocatorItem>( eDFS, this, MatchesTypeAndName( aName, aType ) );
}

/*!
 * \brief Finds a child of this node that has the desired name and type.
 * \param aName The desired name.
 * \param aType The desired type.
 * \return ALandAllocatorItem pointer to the child.
 */
const ALandAllocatorItem* LandNode::findChild( const string& aName,
                                               const LandAllocatorItemType aType ) const {
    return findItem<ALandAllocatorItem>( eDFS, this, MatchesTypeAndName( aName, aType ) );
}

/*!
 * \brief Gets land allocation for a particular product.
 * \param aProductName The product name type.
 * \param aPeriod Model period.
 * \return Total land allocation of a given product.
 */
double LandNode::getLandAllocation( const string& aProductName,
                                    const int aPeriod ) const 
{
    // Allows land for entire node to be returned
    if ( aProductName == mName ) {
        double sum = 0;
        for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
            sum += mChildren[ i ]->getLandAllocation( mChildren[ i ]->getName(), aPeriod );
        }
        return sum;
    }
    else {
        const ALandAllocatorItem* curr = findChild( aProductName, eLeaf );
        if( curr ){
            return curr->getLandAllocation( aProductName, aPeriod );
        }
    }
    return 0.0;
}

/*!
 * \brief Calculates and returns total land allocation of a given type.
 * \param aType The desired type.
 * \param aPeriod Model period.
 * \return Total land allocation of a given type.
 */
double LandNode::getCalLandAllocation( const LandAllocationType aType,
                                       const int aPeriod ) const
{
    double sum = 0;
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        sum += mChildren[ i ]->getCalLandAllocation( aType, aPeriod );
    }
    return sum;
}

void LandNode::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitLandNode( this, aPeriod );
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[i]->accept( aVisitor, aPeriod );
    }
    aVisitor->endVisitLandNode( this, aPeriod );
}

LandUseHistory* LandNode::getLandUseHistory(){
    return( this->mLandUseHistory.get() );
}

bool LandNode::isUnmanagedLandLeaf( )  const 
{
    return false;
}

