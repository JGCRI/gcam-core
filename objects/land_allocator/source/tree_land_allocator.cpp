/*!
 * \file tree_land_allocator.cpp
 * \ingroup Objects
 * \brief TreeLandAllocator class source file.
 * \author James Blackwood
 */

#include "util/base/include/definitions.h"
#include "util/base/include/xml_helper.h"

#include "land_allocator/include/tree_land_allocator.h"
#include "containers/include/scenario.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Default constructor.
* \author James Blackwood
*/
TreeLandAllocator::TreeLandAllocator(){
}

//! Default destructor
TreeLandAllocator::~TreeLandAllocator() {
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author James Blackwood
* \return The constant XML_NAME.
*/
const string& TreeLandAllocator::getXMLName() const {
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
const string& TreeLandAllocator::getXMLNameStatic() {
    const static string XML_NAME = "LandAllocatorRoot";
    return XML_NAME;
}

bool TreeLandAllocator::XMLParse( const DOMNode* aNode ){
    // Call the XML parse.
    return LandNode::XMLParse( aNode );
}

bool TreeLandAllocator::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aCurr ){
    if( aNodeName == "landAllocation" ){
        XMLHelper<double>::insertValueIntoVector( aCurr, mLandAllocation, scenario->getModeltime() );
    }
    else {
        return false;
    }
    return true;
}

void TreeLandAllocator::toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const {
    // Call the node toDebugXML
    ALandAllocatorItem::toDebugXML( aPeriod, aOut, aTabs );
}

void TreeLandAllocator::toInputXML( std::ostream& aOut, Tabs* aTabs ) const {
    // Call the node toInputXML
    LandNode::toInputXML( aOut, aTabs );
}

void TreeLandAllocator::toInputXMLDerived( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteVector( mLandAllocation, "landAllocation", aOut, aTabs, scenario->getModeltime() );
}

/*! \brief Complete the Initialization in the LandAllocator.
* \author James Blackwood
*/
void TreeLandAllocator::completeInit( const string& aRegionName, 
                                      const IInfo* aRegionInfo )
{
    checkRotationPeriod( aRegionInfo );

    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->completeInit( mName, aRegionInfo );
    }

    const Modeltime* modeltime = scenario->getModeltime();
    for( int period = 0; period < modeltime->getmaxper(); period++ ) {
        adjustTotalLand( period );

        // Now re-allocate unmanaged land. Read-in land allocations are used as
        // weights with total unmanaged land set to be equal to total land minus
        // land allocation.
        const double unmanagedLand = mLandAllocation[ period ] - getTotalLandAllocation( true, period );
        for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
            // This will not work for more than one unmanaged land node under the root
            mChildren[i]->setUnmanagedLandAllocation( aRegionName, unmanagedLand, period );
        }

        setInitShares( 0, // No land allocation above this node.
                       mLandUseHistory.get(),
                       period );

        setIntrinsicYieldMode( 1, // Intrinsic rate is one for the root.
                               mSigma,
                               period );
    }
}

/*!
 * \brief Check whether the rotation period is valid and the timesteps are
 *        equal.
 * \details Checks whether the model periods are all equal and evenly divide
 *          into the number of rotation years. Prints a warning if either
 *          condition is not met.
 * \param aRegionInfo Region info.
 */
void TreeLandAllocator::checkRotationPeriod( const IInfo* aRegionInfo ) const {
    // Check that all model periods are equal, which is required for this land
    // allocator.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int period = 1; period < modeltime->getmaxper(); ++period ) {
        if ( modeltime->gettimestep( period ) != modeltime->gettimestep( period - 1 ) ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "All time steps are not constant." << endl;
        }
    }

    // A check to verify that the rotation period is a multiple of the model's
    // timestep.
    const int rotationPeriod = aRegionInfo->getInteger( "rotationPeriod", true );
    for( int period = 0; period < modeltime->getmaxper(); ++period ) {
        if( rotationPeriod % modeltime->gettimestep( period ) != 0 ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::DEBUG );
            mainLog << "Rotation period is not evenly divisible by timestep in period " << period << "." << endl;
        }
    }
}

/*!
 * \brief Adjust the amount of total land if the calibrated production land
 *        exceeds the total.
 * \details Determines the total land allocated to production leaves and
 *          increases the total land by 20% if the total calibrated land exceeds
 *          the total.
 * \param aPeriod Model period.
 * \todo Is it worth adjusting the total land instead of just warning the user?
 */
void TreeLandAllocator::adjustTotalLand( const int aPeriod ){

    const double totalManagedLand = getTotalLandAllocation( true, aPeriod );

    // Check that the total calLandUsed is not greater than the total available.
    if ( totalManagedLand - mLandAllocation[ aPeriod ] > util::getSmallNumber() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::DEBUG );
        mainLog << "The total managed land allocated is greater than the total land in " << mName
            << " in " << scenario->getModeltime()->getper_to_yr( aPeriod ) << " by "
                << totalManagedLand - mLandAllocation[ aPeriod ] 
                << "(" << 100 * ( totalManagedLand - mLandAllocation[ aPeriod ] ) / mLandAllocation[ aPeriod ]
                << "%)" << endl;


        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Total land value set to total managed land plus 20% " << endl;

        mLandAllocation[ aPeriod ] = totalManagedLand * 1.2;
    }
}

/*!
* \brief Returns the intrinsicRate.
* \author James Blackwood
* \param aPeriod Period
* \return the intrinsicRate of this LandAllocator which should only be called by
*         the root node.
*/
double TreeLandAllocator::getAvgIntrinsicRate( const int aPeriod ) const {
    return mIntrinsicRate[ aPeriod ];
}

void TreeLandAllocator::addLandUsage( const string& aLandType,
                                      const string& aProductName,
                                      const LandUsageType aLandUsageType )
{
    LandNode::addLandUsage( aLandType, aProductName, aLandUsageType );
}

double TreeLandAllocator::getCalAveObservedRate( const string& aLandType, int aPeriod ) const {
    return getCalAveObservedRateInternal( aLandType, aPeriod, mSigma );
}

double TreeLandAllocator::getLandAllocation( const string& aLandType,
                                             const int aPeriod ) const
{
    return LandNode::getLandAllocation( aLandType, aPeriod );
}

void TreeLandAllocator::applyAgProdChange( const string& aLandType,
                                           const string& aProductName,
                                           const double aAgProdChange,
                                           const int aPeriod )
{
    LandNode::applyAgProdChange( aLandType, aProductName, aAgProdChange, aPeriod );
}

void TreeLandAllocator::calcYield( const string& aLandType,
                                   const string& aProductName,
                                   const string& aRegionName,
                                   const double aProfitRate,
                                   const int aHarvestPeriod,
                                   const int aCurrentPeriod )
{
    LandNode::calcYieldInternal( aLandType, aProductName, aRegionName, aProfitRate,
                                 mIntrinsicRate[ aCurrentPeriod ], aHarvestPeriod, aCurrentPeriod );
}

double TreeLandAllocator::getYield( const string& aLandType,
                                    const string& aProductName,
                                    const int aPeriod ) const
{
    return LandNode::getYield( aLandType, aProductName, aPeriod );
}

void TreeLandAllocator::setCalLandAllocation( const string& aLandType,
                                              const string& aProductName,
                                              const double aCalLandUsed,
                                              const int aHarvestPeriod, 
                                              const int aCurrentPeriod )
{
    LandNode::setCalLandAllocation( aLandType, aProductName, aCalLandUsed,
                                             aHarvestPeriod, aCurrentPeriod );
}

void TreeLandAllocator::setCalObservedYield( const string& aLandType,
                                             const string& aProductName,
                                             const double aCalObservedYield,
                                             const int aPeriod )
{
    LandNode::setCalObservedYield( aLandType, aProductName, aCalObservedYield, aPeriod );
}

void TreeLandAllocator::setIntrinsicRate( const string& aRegionName,
                                          const string& aLandType,
                                          const string& aProductName,
                                          const double aIntrinsicRate,
                                          const int aPeriod )
{
    LandNode::setIntrinsicRate( aRegionName, aLandType, aProductName,
                                aIntrinsicRate, aPeriod );
}

void TreeLandAllocator::setInitShares( const double aLandAllocationAbove,
                                       const LandUseHistory* aLandUseHistory,
                                       const int aPeriod )
{
    // Calculating the shares
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->setInitShares( mLandAllocation[ aPeriod ],
                                       mLandUseHistory.get(), aPeriod );
    }

    // This is the root node so its share is 100%.
    mShare[ aPeriod ] = 1;
}

void TreeLandAllocator::calcLandShares( const string& aRegionName,
                                        const double aSigmaAbove,
                                        const double aTotalLandAllocated,
                                        const int aPeriod )
{
    LandNode::calcLandShares( aRegionName, aSigmaAbove, aTotalLandAllocated, aPeriod );
 
    // This is the root node so its share is 100%.
    mShare[ aPeriod ] = 1;                                        
}

/*! \brief Recursively calculates the landAllocation at each leaf and node using the shares.
* landAllocationAbove is passed the value of 0 at the root when this method is called,
*  so the value in landAllocation at the root will not be changed and be passed down recursively.
* \author Steve Smith, James Blackwood
*/
void TreeLandAllocator::calcLandAllocation( const string& aRegionName,
                                            const double aLandAllocationAbove,
                                            const int aPeriod )
{
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->calcLandAllocation( aRegionName, mLandAllocation[ aPeriod ], aPeriod );
    }
}

/*! \brief Calculate the land allocation of the entire nest.
* \param aRegionName Region name.
* \param aPeriod Model period.
*/
void TreeLandAllocator::calcFinalLandAllocation( const string& aRegionName,
                                                 const int aPeriod )
{
    calcLandShares( aRegionName,
                    0, // No sigma above the root.
                    0, // No land allocation above the root.
                    aPeriod );

    calcLandAllocation( aRegionName,
                        0, // No land allocation above the root.
                        aPeriod );
}

void TreeLandAllocator::setCarbonContent( const string& aLandType,
                                          const string& aProductName,
                                          const double aAboveGroundCarbon,
                                          const double aBelowGroundCarbon,
                                          const int aPeriod )
{
    LandNode::setCarbonContent( aLandType, aProductName, aAboveGroundCarbon,
                                aBelowGroundCarbon, aPeriod );
}

void TreeLandAllocator::csvOutput( const string& aRegionName ) const {
    LandNode::csvOutput( aRegionName );
}


void TreeLandAllocator::dbOutput( const string& aRegionName ) const {
    LandNode::dbOutput( aRegionName );
}
    
void TreeLandAllocator::calcEmission( const string& aRegionName,
                                      const GDP* aGDP, 
                                      const int aPeriod )
{
    LandNode::calcEmission( aRegionName, aGDP, aPeriod );
}

void TreeLandAllocator::updateSummary( Summary& aSummary, const int aPeriod ){
    LandNode::updateSummary( aSummary, aPeriod );
}

void TreeLandAllocator::accept( IVisitor* aVisitor, const int aPeriod ) const {
    LandNode::accept( aVisitor, aPeriod );
}
