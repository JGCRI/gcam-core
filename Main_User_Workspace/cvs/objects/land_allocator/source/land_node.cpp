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
#include "land_allocator/include/forest_land_leaf.h"
#include "land_allocator/include/land_use_history.h"
#include "containers/include/scenario.h"
#include "util/base/include/ivisitor.h"
#include <numeric>
#include <typeinfo>
#include "ccarbon_model/include/carbon_box_model.h"

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
: ALandAllocatorItem( aParent, eNode )
 {
    mType = eNode;
 }

//! Destructor
LandNode::~LandNode() {
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
        else if ( nodeName == CarbonBoxModel::getXMLNameStatic() ){
            parseSingleNode( curr, mCarbonBoxModelTemplate, new CarbonBoxModel );
        }
        else if( nodeName == "sigma" ) {
            mSigma = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == LandUseHistory::getXMLNameStatic() ){
            parseSingleNode( curr, mLandUseHistory, new LandUseHistory );
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

    // finished writing xml for the class members.

    XMLWriteElement( mSigma, "sigma", out, tabs );

    if( mLandUseHistory.get() ){
        mLandUseHistory->toInputXML( out, tabs );
    }
    
    // write out the Carbon Model template to input.xml, if and only if, one exists
    if ( this->mCarbonBoxModelTemplate.get() ) {
        this->mCarbonBoxModelTemplate->toInputXML( out, tabs );
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
    XMLWriteElement( mSigma, "sigma", out, tabs );

    if( mLandUseHistory.get() ){
        mLandUseHistory->toDebugXML( period, out, tabs );
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
    const static string XML_NAME = "LandAllocatorNode";    // origianl XML text
    return XML_NAME;
}

void LandNode::completeInit( const string& aRegionName,
                             const IInfo* aRegionInfo )
{
    // Verify that sigma is initialized and valid.
    if( !mSigma.isInited() || mSigma < 0 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Sigma for land node " << mName
                << " was not initialized or initialized to a negative value. Resetting to 1." << endl;
        mSigma = 1;
    }

    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->completeInit( aRegionName, aRegionInfo );
    }
}

/*!
* \brief Adds land leafs to the land allocator
* \details Food production technologies dynamically create land
*          leafs via this method. The method determines which
*          type of leaf to create and then inserts it into the
*          tree
* \param aLandType Type of land.
* \param aProductName Product grown on this parcel of land
* \param aLandUsageType Type of land leaf to create (e.g., forest )
* \param aPeriod Model period
*/
void LandNode::addLandUsage( const string& aLandType,
                             const string& aProductName,
                             ILandAllocator::LandUsageType aLandUsageType,
                             const int aPeriod )
{
    assert( aLandType == mName );

    // Only add land usage in the base period. If this is not the base period check to make
    // sure the leaf was already created.
    if( aPeriod > 0 ){
        ALandAllocatorItem* existingItem = findChild( aProductName, eLeaf );
        if( !existingItem ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "A land leaf is required but cannot be created after the base period." << endl;
        }
    }
    // Add a new leaf for the land usage.
    else {
        LandLeaf* newLeaf = 0;
        switch( aLandUsageType ){
        case ILandAllocator::eCrop:
            newLeaf = new LandLeaf( this, aProductName );
            if ( mCarbonBoxModelTemplate.get() ) {
                newLeaf->copyCarbonBoxModel( this->mCarbonBoxModelTemplate.get() );
            }
            break;
        case ILandAllocator::eForest:
            newLeaf = new ForestLandLeaf( this, aProductName );
            if ( mCarbonBoxModelTemplate.get() ) {
                newLeaf->copyCarbonBoxModel( this->mCarbonBoxModelTemplate.get() );
            }
            break;
            // No default here so that the compiler will detect 
            // a missing case.
        }
        addChild( newLeaf );
    }
}

/*!
* \brief Adjusts the amount of land allocated to unmanaged land leafs
* \details Scales all unmanaged land leafs to ensure that the total 
*          amount of land in a region is equal to the read in size of 
*          a region. Model calculates the amount of land needed for the
*          managed land leafs prior to calling this method. It then
*          adjusts each of the unmanaged land leafs by the same percentage
*          to ensure total land is consistent.
* \param aRegionName Region.
* \param aNewUnmanaged Amount of land needed in unmanaged leafs
* \param aPeriod Model period
*/
void LandNode::setUnmanagedLandAllocation( const string& aRegionName,
                                           const double aNewUnmanaged,
                                           const int aPeriod )
{
    // Determine the current amount of unmanaged land below this node.
    double unmanagedBelow = getTotalLandAllocation( eUnmanaged, aPeriod );

    double landAllocationScaleFactor = unmanagedBelow > util::getSmallNumber() ? 
                                       aNewUnmanaged / unmanagedBelow : 0;

    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        // Scale the child land allocation so the sum of this node's land
        // allocation is equal to the intended allocation.
        double newAllocation = mChildren[ i ]->getTotalLandAllocation( eUnmanaged, aPeriod )
            * landAllocationScaleFactor;

        mChildren[ i ]->setUnmanagedLandAllocation( aRegionName, newAllocation, aPeriod );
        // Check that scaling of the child worked correctly.
        assert( util::isEqual( newAllocation,
                mChildren[ i ]->getTotalLandAllocation( eUnmanaged, aPeriod ),
                util::getSmallNumber() ) ); 
    }
}

/*!
* \brief Initializes the share of land of a node
* \details Calculates the share of land allocated to a node and calls
*          a similar method for the node's children. This method is
*          called during CompleteInit and InitCalc so the shares 
*          set are prior to any calculations of yield, intrinsic rate, etc.
* \param aRegionName Region.
* \param aSigmaAbove Distribution parameter of the parent node
* \param aLandAllocationAbove Land allocation of the parent node
* \param aParentHistoryShare 
* \param aParentHistory
* \param aPeriod Model period
*/
void LandNode::setInitShares( const string& aRegionName,
                              const double aSigmaAbove,
                              const double aLandAllocationAbove,
                              const double aParentHistoryShare,
                              const LandUseHistory* aParentHistory,
                              const int aPeriod )
{
    // Calculate the total land within this node.
    double nodeLandAllocation = getTotalLandAllocation( eAnyLand, aPeriod );
    
    // If there is no land allocation for the parent land type, set the share to
    // a small number.
    if( aLandAllocationAbove < util::getSmallNumber() ){
        mShare[ aPeriod ] = util::getSmallNumber();
    }
    else {
        mShare[ aPeriod ] = nodeLandAllocation / aLandAllocationAbove;
        
        // If this is conceptual root then set total land for all periods
        // Only do this once using 1990 values.
        if ( isConceptualRoot() && aPeriod <= 1 ) {
            const Modeltime* modeltime = scenario->getModeltime();
            for( int period = aPeriod; period < modeltime->getmaxper(); period++ ) {
               mLandAllocation[ period ] = nodeLandAllocation;
            }
        }
   }

    //sjsTEMP
    // Need two work out a more robust method of determining land-use history
    // shares when historical years overlap. For now, hard code 1990 for all history shares.
    int landShareBasePeriod = 1;
    
    // If the current node does not have a land use history object,
    // use the parent's object and land use history share.
    const LandUseHistory* nodeLandUseHistory = mLandUseHistory.get();
    double landUseShare = 1;
    if( !nodeLandUseHistory ){
        nodeLandUseHistory = aParentHistory;
        landUseShare = aParentHistoryShare * mShare[ landShareBasePeriod ];
    }

    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {        
        mChildren[ i ]->setInitShares( aRegionName,
                                       mSigma,
                                       nodeLandAllocation,
                                       landUseShare,
                                       nodeLandUseHistory,
                                       aPeriod );
    }

    // Calculate the intrinsic rate only in unmanaged land. This is to calculate
    // the unmanaged land intrinsic rates, which is needed for calibrated. Other
    // land types cannot calculate their intrinsic rates at this point.
    if( isUnmanagedNest() ){
        double totalBaseLandAllocation = getBaseLandAllocation( aPeriod );
        calcLandShares( aRegionName, 0, totalBaseLandAllocation, aPeriod );
    }
}

void LandNode::resetToCalLandAllocation( const int aPeriod )
{
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->resetToCalLandAllocation( aPeriod );
    }
}

/*!
* \brief Helps calculate the intrinsic yield of the land leafs
* \details Calculates the intrinsic rate of the node and passes it to
*          the children so that land leaf can calculate its intrinsic
*          yield. Nothing is stored for land node in this method. 
*          Intrinsic rate in a node/leaf is equal to the intrinsic yield
*          of the node above multiplied by the node/leaf share raised 
*          to the sigma above
* \param aIntrinsicYieldAbove Yield of parent node.
* \param aSigmaAbove Distribution parameter of the parent node
* \param aPeriod Model period
*/
void LandNode::setIntrinsicYieldMode( const double aIntrinsicYieldAbove,
                                      const double aSigmaAbove,
                                      const int aPeriod )
{
    assert( mShare[ aPeriod ].isInited() );

    // Intrisic yields for a land type cannot be zero.
    if ( aIntrinsicYieldAbove > util::getSmallNumber() ) {

       double share = mShare[ aPeriod ] > util::getSmallNumber() ? mShare[ aPeriod ].get() : getDefaultShare();
       double nodeIntrinsicRate = aIntrinsicYieldAbove * pow( share, aSigmaAbove );
       
       // If this is a child of the root with a sigma of zero the power of the
       // share to the sigma will be one, and since the root has an intrinsic rate
       // of one this will equal 1.
       assert( aSigmaAbove > util::getSmallNumber() || util::isEqual( nodeIntrinsicRate, 1.0 ) );

       for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
           mChildren[ i ]->setIntrinsicYieldMode( nodeIntrinsicRate, mSigma, aPeriod );
       }
    }
}

/*!
* \brief Calculates a multiplier to convert potential ( read in ) carbon
*        to actual carbon which varies with the amount of land
* \details The multiplier for each node/leaf is equal to the multiplier from
*        the node above times the node/leaf's share raised to the sigma above
*        This multiplier adjusts carbon contents for a type of land based 
*        on the amount of land that type is allocated. The assumption is that
*        as land expands it moves into more marginal land which has a lower carbon
*        content. Thus, carbon intensities decrease as land expands and increase
*        when land contracts. The carbon intensity read in is the average intensity
*        for the current parcel of land of that type.
* \param aCarbonMultAbove Multiplier of the parent.
* \param aSigmaAbove Sigma parameter governing the distribution this item
*        is within.
* \param aPeriod Period.
* \author Kate Calvin
*/
void LandNode::setActualCarbonMult( const double aCarbonMultAbove,
                                      const double aSigmaAbove,
                                      const int aPeriod )
{
    assert( mShare[ aPeriod ].isInited() );

    // Intrisic yields for a land type cannot be zero.
    if ( aCarbonMultAbove > util::getSmallNumber() ) {

       double share = mShare[ aPeriod ] > util::getSmallNumber() ? mShare[ aPeriod ].get() : getDefaultShare();
       double nodeIntrinsicRate = aCarbonMultAbove * pow( share, aSigmaAbove );
       
       // If this is a child of the root with a sigma of zero the power of the
       // share to the sigma will be one, and since the root has an intrinsic rate
       // of one this will equal 1.
       assert( aSigmaAbove > util::getSmallNumber() || util::isEqual( nodeIntrinsicRate, 1.0 ) );

       for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
           mChildren[ i ]->setActualCarbonMult( nodeIntrinsicRate, mSigma, aPeriod );
       }
    }
}

/*!
* \brief Sets land leaf's intrinsic rate
* \details Locates a land leaf and calls the setIntrinsicRate
*          method for that leaf. The intrinsic rate provided is
*          passed in from the food production technology.
* \param aRegionName Region
* \param aLandType Type of land
* \param aProductName Name of the product grown on the land
* \param aIntrinsicRate Profit rate passed in from the food production technology
* \param aPeriod Period.
*/
void LandNode::setIntrinsicRate( const string& aRegionName,
                                 const string& aLandType,
                                 const string& aProductName,
                                 const double aIntrinsicRate,
                                 const int aPeriod )
{
    assert( aLandType == mName );

    ALandAllocatorItem* curr = findChild( aProductName, eLeaf );
    // Set the rental rate.
    if( curr ){
        curr->setIntrinsicRate( aRegionName, aLandType, aProductName,
                                aIntrinsicRate, aPeriod );
    }
}

void LandNode::setCalLandAllocation( const string& aLandType,
                                     const string& aProductName,
                                     const double aCalLandUsed,
                                     const int aHarvestPeriod, 
                                     const int aCurrentPeriod )
{
    assert( aLandType == mName );
    ALandAllocatorItem* curr = findChild( aProductName, eLeaf );

    // Set the land allocation.
    if( curr ){
        curr->setCalLandAllocation( aLandType, aProductName, aCalLandUsed,
                                    aHarvestPeriod, aCurrentPeriod );
    }
}

void LandNode::setCalObservedYield( const string& aLandType,
                                    const string& aProductName,
                                    const double aCalObservedYield,
                                    const int aPeriod )
{
    assert( aLandType == mName );
    ALandAllocatorItem* curr = findChild( aProductName, eLeaf );
    
    if( curr ){
        curr->setCalObservedYield( aLandType, aProductName, aCalObservedYield, aPeriod );
    }
}

void LandNode::applyAgProdChange( const string& aLandType,
                                  const string& aProductName,
                                  const double aAgProdChange,
                                  const int aHarvestPeriod, 
                                  const int aCurrentPeriod )
{
    assert( aLandType == mName );
    ALandAllocatorItem* curr = findChild( aProductName, eLeaf );
    
    if( curr ){
        curr->applyAgProdChange( aLandType, aProductName, aAgProdChange, aHarvestPeriod, aCurrentPeriod );
    }
}

/*!
* \brief Calculates the share of land allocated to the node and its children
* \details Uses the pure logit formulation to calculate the share
*          of land allocated to a particular land type. A node's share
*          is based on its intrinsic rate and distribution parameter.
*          A node's intrinsic rate is based on the intrinsic rates of 
*          its children
* \param aRegionName Region
* \param aSigmaAbove Distribution parameter of the parent node
* \param aTotalBaseLand Amount of land allocated to the parent node
* \param aPeriod Period.
*/
double LandNode::calcLandShares( const string& aRegionName,
                                 const double aSigmaAbove,
                                 const double aTotalBaseLand,
                                 const int aPeriod )
{
    // Calculate the temporary unnormalized shares.
    double totalBaseLandAllocation = getBaseLandAllocation( aPeriod );
    vector<double> unnormalizedShares( mChildren.size() );
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        // If this is unmanaged land then use initial land allocation to weight
        // land use. Returns the temporary unnormalized share.
        unnormalizedShares[ i ] = mChildren[ i ]->calcLandShares( aRegionName,
                                                                  mSigma,
                                                                  totalBaseLandAllocation,
                                                                  aPeriod );          
    }

    double unnormalizedSum = accumulate( unnormalizedShares.begin(),
                                         unnormalizedShares.end(),
                                         0.0 );

    double unnormalizedShare;
    if ( unnormalizedSum < util::getSmallNumber() ){ 
        mIntrinsicRate[ aPeriod ] = 0;
        unnormalizedShare = 0;
    }
    else {
        // Set the normalized share for each child.
        for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
            mChildren[ i ]->setShare( unnormalizedShares[ i ] / unnormalizedSum,
                                      aPeriod );
        }

        // The unnormalizedSum is actually the 1/sigma weighted intrinsic rates
        // of the mChildren Therefore, the equation below gives the intrinsic
        // rate of this node
        mIntrinsicRate[ aPeriod ] = pow( unnormalizedSum, mSigma.get() );

        // This is a temporary unnormalized share. If this is an unmanaged land node
        // within a nest that includes managed land nodes, the sigma above will be
        // set to zero during the initial setting of shares.
        if( aSigmaAbove > util::getSmallNumber() ){
            unnormalizedShare = pow( mIntrinsicRate[ aPeriod ].get(), 1 / aSigmaAbove );
            assert( util::isValidNumber( unnormalizedShare ) );
        }
        else {
            // Can't calculate share, so return zero.
            unnormalizedShare = 0;
        }
    }
         
    return unnormalizedShare;
}

void LandNode::setUnmanagedLandValues( const string& aRegionName,
                                       const int aPeriod )
{
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->setUnmanagedLandValues( aRegionName, aPeriod );
    }
}

void LandNode::calcLandAllocation( const string& aRegionName,
                                   const double aLandAllocationAbove,
                                   const int aPeriod )
{
    assert( mShare[ aPeriod ].isInited() );

    // Default value
    double nodeLandAllocation = aLandAllocationAbove * mShare[ aPeriod ];

    // If this is a conceptual root then use fixed land allocation
    if ( isConceptualRoot() ) {
        nodeLandAllocation = mLandAllocation[ aPeriod ];
    }
    
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->calcLandAllocation( aRegionName, nodeLandAllocation, aPeriod );
    }
}

void LandNode::calcLUCCarbonFlowsOut( const string& aRegionName,
                                          const int aYear )
{
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->calcLUCCarbonFlowsOut( aRegionName, aYear );
    }
}

void LandNode::calcLUCCarbonFlowsIn( const string& aRegionName,
                                            const int aYear )
{
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->calcLUCCarbonFlowsIn( aRegionName, aYear );
    }
}

void LandNode::calcCarbonBoxModel( const string& aRegionName,
                                           const int aYear )
{
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->calcCarbonBoxModel( aRegionName, aYear );
    }
}

void LandNode::setCarbonContent( const string& aLandType,
                                 const string& aProductName,
                                 const double aAboveGroundCarbon,
                                 const double aBelowGroundCarbon,
                                 const int aMatureAge,    
                                 const int aPeriod )
{
    assert( aLandType == mName );
    ALandAllocatorItem* curr = findChild( aProductName, eLeaf );
    
    if( curr ){
        curr->setCarbonContent( aLandType, aProductName, aAboveGroundCarbon,
                                aBelowGroundCarbon, aMatureAge, aPeriod );
    }
}

/*!
* \brief Calculates the yield of a managed land leaf
* \details Locates a land leaf and calls the method to
*          calculate its yield
* \param aLandType Type of land.
* \param aProductName Product produced on this parcel of land.
* \param aRegionName Region.
* \param aProfitRate Profit rate of the food production technology ($/GCal)
* \param aAvgIntrinsicRate Profit rate of all land ($/kHa)
* \param aHarvestPeriod Harvest period
* \param aCurrentPeriod Current period
*/
void LandNode::calcYieldInternal( const string& aLandType,
                                  const string& aProductName,
                                  const string& aRegionName,
                                  const double aProfitRate,
                                  const double aAvgIntrinsicRate,
                                  const int aHarvestPeriod,
                                  const int aCurrentPeriod )
{
    assert( aLandType == mName );
    
    // There should always be a positive average intrinsic rate.
    assert( aAvgIntrinsicRate > util::getSmallNumber() );

    ALandAllocatorItem* curr = findChild( aProductName, eLeaf );
    
    if( curr ){
        curr->calcYieldInternal( aLandType, aProductName, aRegionName,
                                 aProfitRate, aAvgIntrinsicRate, aHarvestPeriod,
                                 aCurrentPeriod );
    }
}

double LandNode::getYield( const string& aLandType,
                           const string& aProductName,
                           const int aPeriod ) const
{
    assert( aLandType == mName );
    const ALandAllocatorItem* curr = findChild( aProductName, eLeaf );

    if( curr ){
        return curr->getYield( aLandType, aProductName, aPeriod );
    }
    return 0;
}

void LandNode::addChild( ALandAllocatorItem* child ) {
    assert( child );

    // Check if the child already exists.
    ALandAllocatorItem* existingItem = findChild( child->getName(), eLeaf );
    if( existingItem ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Land type " << mName << " already has a child named " << child->getName() << "." << endl;
        delete child;
        return;
    }
    mChildren.push_back( child );
}

/*!
 * \brief Finds a child of this node that has the desired name and type.
 * \param aName The desired name.
 * \param aType The desired type.
 * \return ALandAllocatorItem pointer to the child.
 */
ALandAllocatorItem* LandNode::findChild( const string& aName,
                                         const TreeItemType aType ) {
    return findItem<ALandAllocatorItem>( eDFS, this, MatchesTypeAndName( aName, aType ) );
}

/*!
 * \brief Finds a child of this node that has the desired name and type.
 * \param aName The desired name.
 * \param aType The desired type.
 * \return ALandAllocatorItem pointer to the child.
 */
const ALandAllocatorItem* LandNode::findChild( const string& aName,
                                               const TreeItemType aType ) const {
    return findItem<ALandAllocatorItem>( eDFS, this, MatchesTypeAndName( aName, aType ) );
}

double LandNode::getLandAllocation( const string& aLandType,
                                    const string& aProductName,
                                    const int aPeriod ) const 
{
    assert( aLandType == mName );
    
    // Allows land for entire node to be returned
    if ( aProductName == mName ) {
        return getTotalLandAllocation( eAnyLand, aPeriod );
    }
    else {
        const ALandAllocatorItem* curr = findChild( aProductName, eLeaf );
        if( curr ){
            return curr->getLandAllocation( aLandType, aProductName, aPeriod );
        }
    }
    return 0;
}

double LandNode::getTotalLandAllocation( const LandAllocationType aType,
                                         const int aPeriod ) const
{
    double sum = 0;
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        sum += mChildren[ i ]->getTotalLandAllocation( aType, aPeriod );
    }
    return sum;
}

double LandNode::getBaseLandAllocation( const int aPeriod ) const {
    double sum = 0;
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        sum += mChildren[ i ]->getBaseLandAllocation( aPeriod );
    }
    return sum;
}

/*!
 * \brief Returns true if all children of a node are unmanaged leafs
 */
bool LandNode::isUnmanagedNest() const {
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        if ( !mChildren[ i ]->isUnmanagedNest() ) {
            return false;
        }
    }
    return true;
}

/*!
 * \brief Returns true if node is either has a sigma above of zero
 *        or no parent
 */
bool LandNode::isConceptualRoot() const {
    return getParent() ? getParent()->getSigma() < util::getSmallNumber() : true;
}

double LandNode::getSigma() const {
    return mSigma;
}

void LandNode::csvOutput( const string& aRegionName ) const {
    ALandAllocatorItem::csvOutput( aRegionName );
    //write output for mChildren
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->csvOutput( aRegionName );
    }
}

void LandNode::dbOutput( const string& aRegionName ) const {
    ALandAllocatorItem::dbOutput( aRegionName );
    //write output for mChildren
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->dbOutput( aRegionName );
    }
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
