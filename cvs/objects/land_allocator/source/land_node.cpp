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
#include <numeric>
#include <typeinfo>

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
mLogitExponent( 1.0 ),
mUnManagedLandValue( 0.0 ),
mGhostShareNumeratorForNode( 0.25 ),
mAdjustScalersForNewTech( false )
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
        else if( nodeName == "logit-exponent" ){
            XMLHelper<double>::insertValueIntoVector( curr, mLogitExponent,
                                                 scenario->getModeltime() );
        }
        else if( nodeName == LandUseHistory::getXMLNameStatic() ){
            parseSingleNode( curr, mLandUseHistory, new LandUseHistory );
        }
        else if( nodeName == NodeCarbonCalc::getXMLNameStatic() ){
            parseSingleNode( curr, mCarbonCalc, new NodeCarbonCalc );
        }
        else if( nodeName == "ghost-share-node" ){
			mGhostShareNumeratorForNode = XMLHelper<double>::getValue( curr );
        }
		else if( nodeName == "isNewTechnology" ){
			mIsNewTech = XMLHelper<bool>::getValue( curr );
        } 
		else if( nodeName == "default-share" ){
            // KVC_IRR: Not sure what to do here.
        }
        else if( nodeName == "unManagedLandValue" ){
            mUnManagedLandValue = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "adjustForNewTech" ){
            mAdjustScalersForNewTech = XMLHelper<bool>::getValue( curr );
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
    XMLWriteVector( mLogitExponent, "logit-exponent", out, tabs, modeltime );
	XMLWriteElement( mGhostShareNumeratorForNode, "ghost-share-node", out, tabs );  
    XMLWriteElement( mUnManagedLandValue, "unManagedLandValue", out, tabs );  

    if( mLandUseHistory.get() ){
        mLandUseHistory->toInputXML( out, tabs );
    }
    
    if( mCarbonCalc.get() ) {
        mCarbonCalc->toInputXML( out, tabs );
    }
    
    if( mAdjustScalersForNewTech ) {
        XMLWriteElement( mAdjustScalersForNewTech, "adjustForNewTech", out, tabs );
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
    const Modeltime* modeltime = scenario->getModeltime();
    XMLWriteVector( mLogitExponent, "logit-exponent", out, tabs, modeltime );
    XMLWriteElement( mUnManagedLandValue, "unManagedLandValue", out, tabs );
    XMLWriteElement( getLandAllocation( mName, period ), "landAllocation", out, tabs );

    if( mLandUseHistory.get() ){
        mLandUseHistory->toDebugXML( period, out, tabs );
    }
    if( mCarbonCalc.get() ) {
        mCarbonCalc->toDebugXML( period, out, tabs );
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
    if ( aPeriod > 1 ) {
        // Copy share weights forward if new ones haven't been read in or computed
		// This works as calibration is called before this initCalc
        if ( mProfitScaler[ aPeriod ] == -1 ) {
            mProfitScaler[ aPeriod ] = mProfitScaler[ aPeriod - 1 ];
        }
    }
    else if ( mProfitScaler[ aPeriod ] == -1 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Negative profit scaler in period " << aPeriod
                << " for region " << aRegionName << " in land node "
                << mName << endl;
        exit( -1 );
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
* \param aLogitExpAbove Distribution parameter of the parent node
* \param aPeriod Period.
*/
double LandNode::calcLandShares( const string& aRegionName,
                                 const double aLogitExpAbove,
                                 const int aPeriod )
{

    double unnormalizedSum;
    vector<double> unnormalizedShares( mChildren.size() );
    double unnormalizedShare = 0.0;

    // Step 1.  Calculate the unnormalized shares.
    // These calls need to be made to initiate recursion into lower nests even
    // if the current node will have fixed shares.
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        unnormalizedShares[ i ] = mChildren[ i ]->calcLandShares( aRegionName,
                                                                  mLogitExponent[ aPeriod ],
                                                                  aPeriod );          
    }
    unnormalizedSum = accumulate( unnormalizedShares.begin(),
                                   unnormalizedShares.end(),
                                    0.0 );

    // Step 2 Option (a). If this is a zero-logit exponent, fixed share node
    // then set shares equal to initshares if set by calibration in a 
    // calibration period, or set to the previous period's value if not set.
    // Test to check if shares have changed from initialized value of -1 
    // set in the constructor of a_land_allocator class.
    if ( mLogitExponent[ aPeriod ] == 0 && aPeriod > 0) {
        for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
            // copy forward previous period share if this is not a calibration
            // period in which case it will be set to something
            if ( mChildren[ i ]->getShare( aPeriod ) == -1) {
                 mChildren[ i ]->setShare( mChildren[ i ]->getShare( aPeriod - 1 ),
                                      aPeriod );
            }
            else { //do nothing for now.  Assume it has been set by calibration
            }
        }
    }
    // Step 2 Option (b). Otherwise (as in most cases), node is not a fixed share node
    // and shares are computed based on relative profits as usual  
    else {
        // But first check to make sure at least one child has a non-zero share
        if ( unnormalizedSum == 0.0 ){ // which means all children have zero share
           mProfitRate[ aPeriod ] = 0;
           unnormalizedShare = 0.0;
        }
        else {
           // Calculate and set the share of each child
           for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
               mChildren[ i ]->setShare( unnormalizedShares[ i ] / unnormalizedSum,
                                      aPeriod );
           }
        }
     }
      

   // Step 3 Option (a) . compute node profit based on share denominator
   // but only if logit exponent >0. If logit exponent is zero, math will crash.
    if ( mLogitExponent[ aPeriod ] > 0 && unnormalizedSum > 0 ) {
        mProfitRate[ aPeriod ] = pow( unnormalizedSum, 1.0 / mLogitExponent[ aPeriod ] );
    }
    else if ( mLogitExponent[ aPeriod ] == 0 ) {
        // Step 3 Option (b). Profit for nodes with zero logit exponent will not change, 
        // so we set the profit rate equal to the read in value of unmanaged land.
        // Note: this profit rate shouldn't matter
        // Note: if profit rates of children change over time this wouldn't capture that
        mProfitRate[ aPeriod ] = mUnManagedLandValue;
    } 
    else if ( unnormalizedSum == 0 ) {
        // Step 3 Option (c). If unnormalizedSum == 0 then all children must have had
        // zero profit rates.  So, set the profit rate of the node to zero.
        mProfitRate[ aPeriod ] = 0.0;
    }

    // Step 4. Calculate the unnormalized share for this node, but here using the logit exponent of the 
    // containing or parant node.  This will be used to determine this nodes share within its 
    // parent node.
    if( aLogitExpAbove > 0 ){ // unnormalized share will be ignored if logitexpo = 0
        unnormalizedShare = pow( mProfitScaler[ aPeriod ] * mProfitRate[ aPeriod ] * mAdjustForNewTech[ aPeriod ],
                                 aLogitExpAbove );
    }
    else {
        // Can't calculate share, so return zero.
        unnormalizedShare = 0.0;
    }
    
    return unnormalizedShare; // the unnormalized share of this node.
}

/*!
 * \brief Calculates share profit scalers
 * \param aRegionName Region name.
 * \param aPeriod model period.
 */
void LandNode::calculateProfitScalers( const string& aRegionName, 
                                          const int aPeriod ) {

    // profit scaler of a node is calibrated as equal to 1 (the leaf
    // calibration guarantees the node profits will equal what the 
    // calibration shares would imply)   
    mProfitScaler[ aPeriod ] = 1.0;
	
	calcCalibrationProfitForNewTech( aPeriod );
    
    // Call share profit scalers calculation for each child
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->calculateProfitScalers( aRegionName, aPeriod );
    }

}

/*!
 * \brief Adjusts share profit scalers. This method is only called during non-calibration periods
 * \param aRegionName Region name.
 * \param aPeriod model period.
 */
// KVC_AGLU: probably need code for when logit exponent is zero
void LandNode::adjustProfitScalers( const string& aRegionName, 
                                          const int aPeriod ) 
{
    // We want to adjust scalers when a new technology is added, but only
    // if it is producing an existing crop. If we don't do this, then adding
    // more technologies increases a crop's share (even if the technologies are
    // identical, simply because there are more chances.  This code should not
    // apply to new crops like biomass.  
    // Note: currently, the boolean that indicates whether this adjustment occurs
    //       is read in and stored at the node level.  However, we may want it 
    //       read in at the LandLeaf level.

    if ( mAdjustScalersForNewTech ) {
        // Call method in children first
        for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
            mChildren[ i ]->adjustProfitScalers( aRegionName, aPeriod );
        }

        // MINSHARE is the share of the node a technology must have in order
        // to be considered "existing"
        const double MINSHARE = 0.0; 
        int lastCalibPeriod = scenario->getModeltime()->getFinalCalibrationPeriod();
        int numNewTech = 0;
        int numExistingTech = 0;
        for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
            if ( mChildren[ i ]->getShare( lastCalibPeriod ) > MINSHARE ) {
                numExistingTech += 1;
            }
            else if ( mChildren[ i ]->isNewTech( aPeriod ) ) {  
                numNewTech += 1;
            }
        }

        // First, check if there are new technologies.  If not, nothing needs to happen 
        // and we can exit this method.
        if ( numNewTech == 0 ) {
            return;
        }

        // Next, check if there are existing technologies in this node
        if ( numExistingTech == 0 ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Can not calibrate new tech for land leaf " << mName
                    << " for region " << aRegionName
                    << " since there are no existing techs." << endl;
            exit( 1 );
        }
        else {
            // Calculate adjustment factor
            double ratio = (double) ( numExistingTech + numNewTech ) / (double) numExistingTech;
            double sigma = 1.0 / mLogitExponent[ aPeriod ];
            double adjustment = 1.0 / pow( ratio, sigma );

            // Set adjustment factor
            for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
                mChildren[ i ]->setNewTechAdjustment( adjustment, aPeriod );
            }
        }
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
    if ( ( getParent() ? getParent()->getLogitExponent( 1 ) == 0 : true )
                    && mUnManagedLandValue > 0.0 ) {
        avgProfitRate = mUnManagedLandValue;
    }

    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        // assign the unmanaged land value to this node and to children. 
        mUnManagedLandValue = avgProfitRate;
        mChildren[ i ]->setUnmanagedLandProfitRate( aRegionName, avgProfitRate, aPeriod );
    }
}


/*!
 * \brief Compute the calibration profit rate for all land nodes and land leafs below
 * \details If it is a root (meaning no sharing above), then its profit
 *          is equal to the average profit rate of that region
 *          or subregion. Otherwise, calcuate it based on calibration share.
 * \param aRegionName Region name.
 * \param aAverageProfitRate Average calibration profit rate of node above.
 * \param aLogitExponentAbove - logit exponent of the containing node
 * \author Marshall Wise
 */
void LandNode::calculateCalibrationProfitRate( const string& aRegionName,
                                           double aAverageProfitRateAbove,
                                           double aLogitExponentAbove,
                                           const int aPeriod )
{
    double avgProfitRate = aAverageProfitRateAbove;
    // If node is the root of a fixed land area nest ( typically a subregion )
    // or the root of the entire land allocatory, then use the previously
    // calculated value as the average profit rate.  Otherwise, profit is 
    // calculate from the share and the logit exponent
    if ( ( getParent() ? getParent()->getLogitExponent( 1 ) == 0 : true )
                                            && mUnManagedLandValue > 0.0 ) {
        avgProfitRate = mUnManagedLandValue;
    }
    else if( aLogitExponentAbove < util::getTinyNumber() ) {
        // avoid floating point exception
        // 0 < share < 1 => pow(share, infinity) = 0
        avgProfitRate = 0;
    }
	else if ( mIsNewTech ) { 
		avgProfitRate *= pow( mGhostShareNumeratorForNode,  1.0 / aLogitExponentAbove ); 
    }
    else { 
        avgProfitRate *= pow( mShare[ aPeriod ],  1.0 / aLogitExponentAbove ); 
    } 

    // store this value in this node 
    mCalibrationProfitRate[ aPeriod ] = avgProfitRate;

    // pass the node profit rate down to children and trigger their calculation
    // and pass down the logit exponent of this node
    double aLogitExponent = mLogitExponent[ aPeriod ];
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->calculateCalibrationProfitRate( aRegionName, avgProfitRate, 
                                                        aLogitExponent, aPeriod );
    }
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

double LandNode::getLogitExponent( const int aPeriod ) const {
    return mLogitExponent[ aPeriod ];
}

// To be called by contained leaf for new tech
double LandNode::getCalibrationProfitForNewTech( const int aPeriod ) const {
	return mCalibrationProfitForNewTech[ aPeriod ];
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

void LandNode::calcCalibrationProfitForNewTech( const int aPeriod ) {
	if ( aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod() ) {
		if ( mIsNewTech ) {
			mCalibrationProfitForNewTech[ aPeriod ] = getParent()->getCalibrationProfitForNewTech( aPeriod );
		}
		else {
			mCalibrationProfitForNewTech[ aPeriod ] = getProfitForChildWithHighestShare( aPeriod );
		}
	}
	else {
		mCalibrationProfitForNewTech[ aPeriod ] = mCalibrationProfitForNewTech[ aPeriod - 1 ];
	}

}
 
double LandNode::getProfitForChildWithHighestShare( const int aPeriod ) const {
	
	double maxShare = 0.0;
	double profitAtMaxShare = 0.0;
	int indexOfMaxShare = 0;
	
	// First, cycle through all the nodes in this node
	// and calculate the profit of the one with the largest share.
	for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        if( !mChildren[ i ]->isUnmanagedLandLeaf() ) {
            if( mChildren[ i ]->getShare( aPeriod ) > maxShare ) {
                maxShare = mChildren[ i ]->getShare( aPeriod );
                indexOfMaxShare = i;
            }
        }
	}
	
	// Then, call this method on the child with highest share.
	profitAtMaxShare = mChildren[ indexOfMaxShare ]->getProfitForChildWithHighestShare( aPeriod );
	
	if ( profitAtMaxShare == 0 ) {
		profitAtMaxShare = mUnManagedLandValue;
	}
	
	return profitAtMaxShare;
}
