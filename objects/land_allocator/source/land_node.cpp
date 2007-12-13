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

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

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

bool LandNode::XMLParse( const DOMNode* aNode ){

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

bool LandNode::XMLDerivedClassParse( const string& aNodeName,
                                     const DOMNode* aCurr )
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
    const static string XML_NAME = "LandAllocatorNode";
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
            break;
        case ILandAllocator::eForest:
            newLeaf = new ForestLandLeaf( this, aProductName );
            break;
            // No default here so that the compiler will detect 
            // a missing case.
        }
        addChild( newLeaf );
    }
}

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
    }

    // If the current node does not have a land use history object,
    // use the parent's object and land use history share.
    const LandUseHistory* nodeLandUseHistory = mLandUseHistory.get();
    double landUseShare = 1;
    if( !nodeLandUseHistory ){
        nodeLandUseHistory = aParentHistory;
        landUseShare = aParentHistoryShare * mShare[ aPeriod ];
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

void LandNode::setIntrinsicYieldMode( const double aIntrinsicYieldAbove,
                                      const double aSigmaAbove,
                                      const int aPeriod )
{
    assert( mShare[ aPeriod ].isInited() );

    // Intrisic yields for a land type cannot be zero.
    assert( aIntrinsicYieldAbove > util::getSmallNumber() );

    double nodeIntrinsicRate = aIntrinsicYieldAbove * pow( mShare[ aPeriod ].get(), aSigmaAbove );
    
    // If this is a child of the root with a sigma of zero the power of the
    // share to the sigma will be one, and since the root has an intrinsic rate
    // of one this will equal 1.
    assert( aSigmaAbove > util::getSmallNumber() || util::isEqual( nodeIntrinsicRate, 1.0 ) );

    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->setIntrinsicYieldMode( nodeIntrinsicRate, mSigma, aPeriod );
    }
}

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

            // If this is unmanaged land, adjust the share for the proportion of
            // base land in this node and base land in the parent. This would
            // only be required if there were multiple unmanaged land nodes
            // below a single conceptual root.
            if ( totalBaseLandAllocation > util::getSmallNumber() &&
                aTotalBaseLand > util::getSmallNumber() )
            {
                unnormalizedShare *= totalBaseLandAllocation / aTotalBaseLand;
            }
            assert( util::isValidNumber( unnormalizedShare ) );
        }
        else {
            // The unnormalized share cannot be calculated. If all children of a
            // node return 0, as in the case where the sigma of the node is
            // zero, the node will not adjust the initial shares.
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

    double nodeLandAllocation = aLandAllocationAbove * mShare[ aPeriod ];
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->calcLandAllocation( aRegionName, nodeLandAllocation, aPeriod );
    }
}

void LandNode::setCarbonContent( const string& aLandType,
                                 const string& aProductName,
                                 const double aAboveGroundCarbon,
                                 const double aBelowGroundCarbon,
                                 const int aPeriod )
{
    assert( aLandType == mName );
    ALandAllocatorItem* curr = findChild( aProductName, eLeaf );
    
    if( curr ){
        curr->setCarbonContent( aLandType, aProductName, aAboveGroundCarbon,
                                aBelowGroundCarbon, aPeriod );
    }
}

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

    const ALandAllocatorItem* curr = findChild( aProductName, eLeaf );

    if( curr ){
        return curr->getLandAllocation( aLandType, aProductName, aPeriod );
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

bool LandNode::isUnmanagedNest() const {
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        if ( !mChildren[ i ]->isUnmanagedNest() ) {
            return false;
        }
    }
    return true;
}

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
