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

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Constructor.
* \author James Blackwood
*/
LandNode::LandNode()
{
}

//! Destructor
LandNode::~LandNode() {
}

bool LandNode::matches( const string& aName,
                        const TreeItemType aType ) const
{
    // Checks the type first to avoid the expensive string comparsion when
    // possible.
    return ( ( ( aType == eNode ) || ( aType == eAny ) ) && ( aName == mName ) );
}

size_t LandNode::getNumChildren() const {
    return mChildren.size();
}

const ALandAllocatorItem* LandNode::getChildAt( const size_t aIndex ) const {
    /*! \pre aIndex is less than the size of the child vector. */
    assert( aIndex < mChildren.size() );
    return mChildren[ aIndex ];
}

ALandAllocatorItem* LandNode::getChildAt( const size_t aIndex ) {
    /*! \pre aIndex is less than the size of the child vector. */
    assert( aIndex < mChildren.size() );
    return mChildren[ aIndex ];
}

/*! \brief Set data members from XML input
*
* \author James Blackwood
* \param node pointer to the current node in the XML input tree
*/
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
            parseContainerNode( curr, mChildren, new LandNode );
        }
        else if ( nodeName == UnmanagedLandLeaf::getXMLNameStatic() ) {
            parseContainerNode( curr, mChildren, new UnmanagedLandLeaf );
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

/*! \brief Parses any attributes specific to derived classes
* \author James Blackwood
* \param nodeName The name of the curr node. 
* \param curr pointer to the current node in the XML input tree
*/
bool LandNode::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ){
    return false;
}

/*! \brief Write XML values specific to derived objects
*
* \author Steve Smith
*/
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

/*! \brief Write XML values to debug stream for this object.
*
* \author Steve Smith
*/
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

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author James Blackwood
* \return The constant XML_NAME.
*/
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
* \return The constant XML_NAME as a static.
*/
const string& LandNode::getXMLNameStatic() {
    const static string XML_NAME = "LandAllocatorNode";
    return XML_NAME;
}

/*! \brief Complete the Initialization in the LandAllocator.
* \author James Blackwood
*/
void LandNode::completeInit( const string& aRegionName,
                             const IInfo* aRegionInfo )
{
    // Verify that sigma is initialized and valid.
    if( !mSigma.isInited() || mSigma < util::getSmallNumber() ){
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
                             ILandAllocator::LandUsageType aLandUsageType )
{
    // Find the parent land item which should have a leaf added.
    ALandAllocatorItem* parent = findItem( aLandType, eNode );

    // Check that the parent exists.
    if( !parent ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Cannot add a land usage for " << aProductName << " as the land type "
                << aLandType << " does not exist." << endl;
    }
    else {
        // Add a new leaf for the land usage.
        LandLeaf* newLeaf = 0;
        if( aLandUsageType == ILandAllocator::eCrop ){
            newLeaf = new LandLeaf( aProductName );
        }
        else if( aLandUsageType == ILandAllocator::eForest ){
            newLeaf = new ForestLandLeaf( aProductName );
        }
        // Unknown type. This can only occur if a new type is added to the enum
        // and not here.
        else {
            assert( false );
        }

        parent->addChild( newLeaf );
    }
}

/*! \brief Sets land allocation of unmanaged land nodes and leafs.
* \details Production nodes have their land allocation set by the supply sectors
*          that create them The land allocation in unmanaged land nodes need to
*          be set as the difference between the land allocated above and the
*          land used in the rest of the mChildren at this level. Unmanaged land
*          leafs have an allocation read in, which acts as a relative share of
*          their land -- this needs to be adjusted to be consistant with the
*          land specified to be used in the production sectors root node is
*          represented by having a landAllocationAbove equal to zero.
* \warning this routine assumes that all leafs under an unmanaged land node are
*          not managed land.
* \todo Generalize this method so that unmanaged land leafs could be at any
*       level
* \param landAllocationIn Total land allocated to be allocated to this node
* \param period Period index
* \author Steve Smith
*/
void LandNode::setUnmanagedLandAllocation( const string& aRegionName,
                                           const double aLandAllocation,
                                           const int aPeriod )
{
    // If this node is not full of production leafs, then this is unmanaged land
    if ( !isProductionLeaf() && aLandAllocation > 0 ) {
        mLandAllocation[ aPeriod ] = aLandAllocation;

        double totalLandAllocated = getLandAllocationInternal( aPeriod );
        if ( totalLandAllocated > 0 ) {
            double landAllocationScaleFactor = mLandAllocation[ aPeriod ] / totalLandAllocated;
            for ( unsigned int i = 0; i < mChildren.size() ; i++ ) {
                double childLandAllocation = mChildren[ i ]->getLandAllocationInternal( aPeriod );
                double newAllocation = childLandAllocation * landAllocationScaleFactor;
                mChildren[ i ]->setUnmanagedLandAllocation( aRegionName, newAllocation, aPeriod );
            }
        }
        calcLandShares( aRegionName, 0, 0, aPeriod );
    }
}

/*! \brief Sets the initial shares and land allocation.
* \warning Unmanged land allocations are not set properly. 
* \todo figure out a way to set the unmanaged land allocation leaves. (Unmanaged Class)
* \author James Blackwood
*/
void LandNode::setInitShares( const double aLandAllocationAbove,
                              const LandUseHistory* aLandUseHistory,
                              const int aPeriod )
{
    // Summing the LandAllocations of the mChildren
    mLandAllocation[ aPeriod ] = getTotalLandAllocation( false, aPeriod ); 

    //Calculating the shares
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->setInitShares( mLandAllocation[ aPeriod ],
                                       mLandUseHistory.get(), aPeriod );
    }

    // If there is no land allocation for the parent land type, set the share to
    // a small number.
    if( aLandAllocationAbove < util::getSmallNumber() ){
        mShare[ aPeriod ] = util::getSmallNumber();
    }
    else {
        mShare[ aPeriod ] = mLandAllocation[ aPeriod ] / aLandAllocationAbove;
        assert( util::isValidNumber( mShare[ aPeriod ] ) );
    }
}

/*! \brief Sets the intrinsic yield mode for the node and its' children.
* \author James Blackwood
*/
void LandNode::setIntrinsicYieldMode( const double aIntrinsicRateAbove,
                                      const double aSigmaAbove,
                                      const int aPeriod )
{
    double intrinsicRateToBePassed = aIntrinsicRateAbove * pow( mShare[ aPeriod], aSigmaAbove );
    if( intrinsicRateToBePassed > 0 ){
        for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
            mChildren[ i ]->setIntrinsicYieldMode( intrinsicRateToBePassed, mSigma, aPeriod );
        }
    }
}

/*! \brief Finds the location to set the intrinsicRate and sets it using the leaf's version of this method.
* If the location is not found it creates it as a leaf from the landType which should be a node.
* \author Josh Lurz, James Blackwood
*/
void LandNode::setIntrinsicRate( const string& aRegionName,
                                 const string& aLandType,
                                 const string& aProductName,
                                 const double aIntrinsicRate,
                                 const int aPeriod )
{
    ALandAllocatorItem* curr = findItem( aProductName, eLeaf );
    // Set the rental rate.
    if( curr ){
        curr->setIntrinsicRate( aRegionName, aLandType, aProductName,
                                aIntrinsicRate, aPeriod );
    }
}

/*! \brief Finds the location to set the calibrated land allocation values and
*          sets them using the leaf's version of this method. If the location is
*          not found it creates it as a leaf from the landType which should be a
*          node.
* \author James Blackwood
*/
void LandNode::setCalLandAllocation( const string& aLandType,
                                     const string& aProductName,
                                     const double aCalLandUsed,
                                     const int aHarvestPeriod, 
                                     const int aCurrentPeriod )
{
    ALandAllocatorItem* curr = findItem( aProductName, eLeaf );

    // Set the land allocation.
    if( curr ){
        curr->setCalLandAllocation( aLandType, aProductName, aCalLandUsed,
                                    aHarvestPeriod, aCurrentPeriod );
    }
}

/*! \brief Finds the location to set the calibrated land allocation and observed
*          yield values and sets them using the leaf's version of this method.
*          If the location is not found it creates it as a leaf from the
*          landType which should be a node.
* \author James Blackwood
*/
void LandNode::setCalObservedYield( const string& aLandType,
                                    const string& aProductName,
                                    const double aCalObservedYield,
                                    const int aPeriod )
{
    ALandAllocatorItem* curr = findItem( aProductName, eLeaf );
    
    if( curr ){
        curr->setCalObservedYield( aLandType, aProductName, aCalObservedYield, aPeriod );
    }
}

double LandNode::getUnmanagedCalAveObservedRateInternal( const int aPeriod,
                                                         const double aSigmaAbove ) const
{
    double rateToReturn = 0;

    // Note that a standard TreeItem search does not work since we need the share and  
    // sigma at everynode that leads to the unmanaged land node.
    // Instead, we will traverse the tree to find the top of the unmanaged land
    // nest.  UnmanagedLandLeaf::getUnmanagedCalAveObservedRateInternal returns
    // 1; all other leaves return 0.  Therefore if all of a node's children
    // return a non-zero value, the node is an unmanaged land nest and will
    // return its instrinsic rate.  The highest unmanaged land node nest's
    // instrinsic rate will be successively divided by its share to the power of
    // the sigma parameter for each level and return by the root.
    
    // It can't be the unmanaged nest if it doesn't have any children.
    bool isUnmanagedNest = mChildren.size() > 0 ? true : false;

    for ( unsigned int i = 0; i < mChildren.size(); i++ ){
        double rateTemp = mChildren[ i ]->getUnmanagedCalAveObservedRateInternal( aPeriod, mSigma );
        if( rateTemp < util::getTinyNumber() ){
            isUnmanagedNest = false;
        }
        else {
            rateToReturn = rateTemp;
        }
    }

    // If all the children of this node returned a non-zero
    // getUnmanagedCalAveObservedRateInternal then we want this node's
    // mInstrinsicRate
    if( isUnmanagedNest ){
        rateToReturn = mIntrinsicRate[ aPeriod ];
    } 

    // Since it may be possible for a node to have no share
    if ( mShare[ aPeriod ] > util::getTinyNumber() ){
        rateToReturn /= pow( mShare[ aPeriod ], aSigmaAbove );
    }
    return rateToReturn;
}

/*! \brief Finds the location to apply the agruculture production change
* and sets them using the leaf's version of this method. If the location is not
* found it creates it as a leaf from the landType which should be a node.
* \author James Blackwood
*/
void LandNode::applyAgProdChange( const string& aLandType,
                                  const string& aProductName,
                                  const double aAgProdChange,
                                  const int aPeriod )
{
    ALandAllocatorItem* curr = findItem( aProductName, eLeaf );
    
    if( curr ){
        curr->applyAgProdChange( aLandType, aProductName, aAgProdChange, aPeriod );
    }
}

/*! \brief This method will calculate the share value for each leaf and node,
*          and then normalize it.
* \details This function will be called from the supply side of the sectors and
*          will be passed a default dummy sigmaAbove. The first loop cycles
*          through all the mChildren. If a child is a leaf then it will call the
*          calcLandShares method in LandAllocatorLeaf, where share is
*          calculated. If a child is a node there will be a recursive call to
*          this method. The second loop uses the sum of all the shares of the
*          mChildren vector and normalizes and overwrites share. Finally, share
*          is calculated for this node using the calculated intrinsicRate and
*          the sigma from one level up.
* \param aSigmaAbove the sigma value from the node above this level.
* \author James Blackwood
* \todo need a better way to check if "UnmanagedLand" to not overwrite
*       intrinsicRate that was read in through input
* \todo May need to add a method to deal with case if total allocation is
*       greater than initial allocation 
* \todo this will not work if unmanaged land nodes are nested
*/
void LandNode::calcLandShares( const string& aRegionName,
                               const double aSigmaAbove,
                               const double aTotalLandAllocated,
                               const int aPeriod )
{
    // First adjust value of unmanaged land nodes
    setUnmanagedLandValues( aRegionName, aPeriod );

    // Calculate the temporary unnormalized shares and sum them
    double unnormalizedSum = 0;
    // double excessShares = 0; (ignore this for now)
    double totalBaseLandAllocation = getBaseLandAllocation( aPeriod );
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        // If this is managed land, or if unmanaged land and no base land
        // allocation is specified then use standard method.
        if ( isProductionLeaf() || util::isEqual( totalBaseLandAllocation, 0.0 ) ) {
            mChildren[ i ]->calcLandShares( aRegionName, mSigma, 0, aPeriod );
        }
        else {
            // If this is unmanaged land then use initial land allocation to
            // weight land use.
            mChildren[ i ]->calcLandShares( aRegionName, mSigma, totalBaseLandAllocation, aPeriod );
        }
        // Get the temporary unnormalized share.
        unnormalizedSum += mChildren[ i ]->getShare( aPeriod );                   
    }

    if ( unnormalizedSum < util::getSmallNumber() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "The children of land type " << mName << " in region " << aRegionName << " have invalid total un-normalized shares of " << unnormalizedSum << "." << endl;
    }

    // Normalizing the temporary unnormalized shares
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->normalizeLandAllocation( unnormalizedSum, aPeriod );
    }

    // The unnormalizedSum is actually the 1/sigma weighted intrinsic rates of
    // the mChildren Therefore, the equation below gives the intrinsic rate of
    // this node
    mIntrinsicRate[ aPeriod ] = pow( unnormalizedSum, mSigma.get() );

    // This is a temporary unnormalized share. Sigma above may be zero when
    // setting the unmanaged land allocation.
    // TODO: Can this be simplified?
    if( aSigmaAbove > util::getSmallNumber() ){
        mShare[ aPeriod ] = pow( mIntrinsicRate[ aPeriod ], 1 / aSigmaAbove );
        assert( util::isValidNumber( mShare[ aPeriod ] ) );
    }
    else {
        mShare[ aPeriod ] = 1;
    }
}

/*! \brief Adjust land values for unmanaged land nodes as necessary
* \param aRegionName Region name.
* \param aPeriod Model period
* \author Steve Smith
*/
void LandNode::setUnmanagedLandValues( const string& aRegionName, const int aPeriod ) {
    for ( unsigned int i = 0; i < mChildren.size() ; i++ ) {
        mChildren[ i ]->setUnmanagedLandValues( aRegionName, aPeriod );
    }
}

/*! \brief Recursively calculates the landAllocation at each leaf and node using
*          the shares. landAllocationAbove is passed the value of 0 at the root
*          when this method is called, so the value in landAllocation at the
*          root will not be changed and be passed down recursively.
* \author Steve Smith, James Blackwood
*/
void LandNode::calcLandAllocation( const string& aRegionName,
                                   const double aLandAllocationAbove,
                                   const int aPeriod )
{
    mLandAllocation[ aPeriod ] = aLandAllocationAbove * mShare[ aPeriod ];
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->calcLandAllocation( aRegionName, mLandAllocation[ aPeriod ], aPeriod );
    }
}

void LandNode::setCarbonContent( const string& aLandType,
                                 const string& aProductName,
                                 const double aAboveGroundCarbon,
                                 const double aBelowGroundCarbon,
                                 const int aPeriod )
{
    ALandAllocatorItem* curr = findItem( aProductName, eLeaf );
    
    if( curr ){
        curr->setCarbonContent( aLandType, aProductName, aAboveGroundCarbon,
                                aAboveGroundCarbon, aPeriod );
    }
}

/*! \brief Finds the location to calculate the yield using the leaf's version of this method.
* \author James Blackwood
*/
void LandNode::calcYieldInternal( const string& aLandType,
                                  const string& aProductName,
                                  const string& aRegionName,
                                  const double aProfitRate,
                                  const double aAvgIntrinsicRate,
                                  const int aHarvestPeriod,
                                  const int aCurrentPeriod )
{
    ALandAllocatorItem* curr = findItem( aProductName, eLeaf );
    
    if( curr ){
        curr->calcYieldInternal( aLandType, aProductName, aRegionName,
                                 aProfitRate, aAvgIntrinsicRate, aHarvestPeriod,
                                 aCurrentPeriod );
    }
}

/*! \brief Finds the location to get the yield using the leaf's version of this method.
* \author James Blackwood
*/
double LandNode::getYield( const string& landType, const string& productName, const int period ) const {
    const ALandAllocatorItem* curr = findItem( productName, eLeaf );

    if( curr ){
        return curr->getYield( landType, productName, period );
    }
    return 0;
}

/*! \brief Adds a child to the mChildren vector by pushing it on the back.
* The child can be a leaf or a node.
* \author Josh Lurz, James Blackwood
*/
void LandNode::addChild( ALandAllocatorItem* child ) {
    /*! \pre The child exists. */
    assert( child );

    // Check if the child already exists.
    ALandAllocatorItem* existingItem = findItem( child->getName(), eLeaf );
    if( existingItem ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Land type " << mName << " already has a child named " << child->getName() << "." << endl;
        delete child;
        return;
    }
    mChildren.push_back( child );
}

/*! \brief Recursively sums the landAllocation of all the nodes and leafs below
*          this landType.
* \author James Blackwood
* \return the landAllocation of this landType
* \todo Does this continue searching after finding the land type?
*/
double LandNode::getLandAllocation( const string& aProductName,
                                    const int aPeriod ) const 
{
    const ALandAllocatorItem* curr = findItem( aProductName, eLeaf );

    if( curr ){
        return curr->getLandAllocation( aProductName, aPeriod );
    }
    return 0;
}

double LandNode::getLandAllocationInternal( const int aPeriod ) const {
    double sum = 0;
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        sum += mChildren[ i ]->getLandAllocationInternal( aPeriod );
    }
    return sum;
}

/*!
 * \brief Returns all land allocated for this land type.
 * \param aProductionOnly Whether to only get land allocation for production
 *        leaves.
 * \param aPeriod Model period.
 * \note This function is needed so that separate function can be called for
 *       land classes with vintaging when total land allocated is necessary.
 * \author Steve Smith
 * \return The total land allocated below the node.
 */
double LandNode::getTotalLandAllocation( const bool aProductionOnly, 
                                         const int aPeriod ) const
{
    double sum = 0;
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        sum += mChildren[ i ]->getTotalLandAllocation( aProductionOnly, aPeriod );
    }
    return sum;
}

/*! \brief Recursively sums the baseLandAllocation of all the nodes and leafs
*          below this landType.
*
* Only sums land in unmanaged land nodes, since these are the only ones that
* have a base land allocation
*
* \author Steve Smith
* \return the baseLandAllocation of this landType
*/
double LandNode::getBaseLandAllocation( const int aPeriod ) const {
    double sum = 0;
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        if ( !mChildren[i]->isProductionLeaf() ) {
            sum += mChildren[ i ]->getBaseLandAllocation( aPeriod );
        }
    }
    return sum;
}

/*! \brief Returns Whether all leaves under this node are production leaves.
* \details Checks all nodes and leaf below this node, if any are not a
*          production leaf this returns false.
* \return Whether all mChildren leaves are production leaves.
* \author Steve Smith
*/
bool LandNode::isProductionLeaf() const {
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        if ( !mChildren[ i ]->isProductionLeaf() ) {
            return false;
        }
    }
    return true;
}


/*! \brief Write output to csv output file. 
*
*
* \author Steve Smith
*/
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

void LandNode::calcEmission( const string& aRegionName,
                             const GDP* aGDP, 
                             const int aPeriod )
{
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->calcEmission( aRegionName, aGDP, aPeriod );
    }
}

void LandNode::updateSummary( Summary& aSummary, const int aPeriod ) {
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) { 
        mChildren[ i ]->updateSummary( aSummary, aPeriod );
    }
}

void LandNode::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitLandNode( this, aPeriod );
	for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
		mChildren[i]->accept( aVisitor, aPeriod );
    }
    aVisitor->endVisitLandNode( this, aPeriod );
}

