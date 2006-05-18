/*!
 * \file land_allocator_leaf.cpp
 * \ingroup Objects
 * \brief LandLeaf class source file.
 * \author James Blackwood
 */

#include "util/base/include/definitions.h"
#include <string>
#include <vector>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>

#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/scenario.h"
#include "util/base/include/configuration.h"
#include "land_allocator/include/land_leaf.h"
#include "util/base/include/ivisitor.h"
#include "emissions/include/production_carbon_calc.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Constructor.
* \author James Blackwood
*/
LandLeaf::LandLeaf():
mIntrinsicYieldMode( 0 ),
mYield( scenario->getModeltime()->getmaxper(), -1 ),
mCalObservedYield( 0 ),
mAgProdChange( 1 )
{}

//! Default destructor
LandLeaf::~LandLeaf() {
}

bool LandLeaf::matches( const string& aName,
                        const TreeItemType aType ) const
{
    // Checks the type first to avoid the expensive string comparsion when
    // possible.
    return ( ( ( aType == eLeaf ) || ( aType == eAny ) ) && ( aName == mName ) );
}

size_t LandLeaf::getNumChildren() const {
    return 0;
}

const ALandAllocatorItem* LandLeaf::getChildAt( const size_t aIndex ) const {
    /*! \invariant Leaves have no children so this should never be called. */
    assert( false );
    return 0;
}

ALandAllocatorItem* LandLeaf::getChildAt( const size_t aIndex ) {
    /*! \invariant Leaves have no children so this should never be called. */
    assert( false );
    return 0;
}

/*! \brief Parses any attributes specific to derived classes
*
* Method parses any input data attributes (not child nodes, see XMLDerivedClassParse) that are specific to any classes derived from this class.
*
* \author James Blackwood
* \param nodeName The name of the curr node. 
* \param curr pointer to the current node in the XML input tree
*/
bool LandLeaf::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aCurr ){
    return false;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& LandLeaf::getXMLName() const {
    const static string XML_NAME = "LandAllocatorLeaf";
    return XML_NAME;
}

/*! \brief Complete the Initialization in the LandAllocator.
* \author James Blackwood
*/
void LandLeaf::completeInit( const string& aRegionName, const IInfo* aRegionInfo ) {
    // Store the interest rate from the region.
    mInterestRate = aRegionInfo->getDouble( "interest-rate", true );

    // Set the carbon cycle object if it has not already been initialized. Use a
    // virtual function so that derived leaves may use a different default type.
    initCarbonCycle();

    // Ensure that a carbon cycle object has been setup.
    assert( mCarbonContentCalc.get() );
}

/*! \brief Initialize the carbon cycle object.
* \details Instantiate a carbon cycle for a production leaf.
*/
void LandLeaf::initCarbonCycle(){
    if( !mCarbonContentCalc.get() ){
        mCarbonContentCalc.reset( new ProductionCarbonCalc );
    }
}

void LandLeaf::addLandUsage( const string& aLandType,
                             const string& aProductName,
                             const ILandAllocator::LandUsageType aType )
{
    // The leaf is the land usage so this should never be called.
    assert( false );
}

/*! \brief Returns whether this is a production leaf.
* \details Returning true means that this leaf was set-up by a supply sector,
*          returning falses means this is an unmanaged land leaf that was
*          read-in from the land allocator.
* \return Whether this is a production leaf.
* \author Steve Smith
*/
bool LandLeaf::isProductionLeaf() const {
    return true;
}

/*! \brief Set the initial shares for this land.
*
* Sets the share for this land in the period passed in, from the ratio of
* the the land allocated for this leaf over the land allocated for the type above.
*
* \author James Blackwood
*/
void LandLeaf::setInitShares( const double aLandAllocationAbove, const int aPeriod ) {
    if ( aLandAllocationAbove > util::getSmallNumber() ) {
        mShare[ aPeriod ] = mLandAllocation[ aPeriod ] / aLandAllocationAbove;
        assert( util::isValidNumber( mShare[ aPeriod ] ) );
    }
}

/*! \brief Calculate the Intrinsic Yield Mode
*
* Note: intrinsicYieldMode has the observedYield stored in it
* \todo If the above is true, then see if that can be made explicit
* \todo find better way of specifying share for the intrinsic yield calc for good with no initial share
* \author James Blackwood, Steve Smith
*/
void LandLeaf::setIntrinsicYieldMode( const double aIntrinsicRateAbove,
                                      const double aSigmaAbove,
                                      const int aPeriod )
{
    // If share is zero and have read in a calibrated yield (e.g. biomass or
    // other new crops) then use an arbitrary 0.25 share need to figure out what
    // to read in for this, or how to specify (specify comparable?)
    double share = mShare[ aPeriod ] > util::getSmallNumber() ? mShare[ aPeriod ] : 0.25;
    double intrinsicRate = aIntrinsicRateAbove * pow( share, aSigmaAbove );
    fill( mIntrinsicYieldMode.begin() + aPeriod,
          mIntrinsicYieldMode.end(),
          intrinsicRate * mCalObservedYield[ aPeriod ] );

    checkCalObservedYield( aPeriod );
}

/*! \brief Check whether the calibrated observed yield is valid for this leaf
*          type.
* \param aPeriod Model period.
*/
void LandLeaf::checkCalObservedYield( const int aPeriod ) const {
    if ( util::isEqual( mCalObservedYield[ aPeriod ], 0.0 ) ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "calObservedYield is zero in " << mName << " in " << " period " << aPeriod << endl; 
    }
}

void LandLeaf::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    // Do nothing because land leaves are dynamically created.
}

void LandLeaf::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteElement( isProductionLeaf(), "isProductionLeaf", out, tabs );
    XMLWriteElement( mIntrinsicYieldMode[ period ], "intrinsicYieldMode", out, tabs );
    XMLWriteElement( mCalObservedYield[ period ], "calObservedYield", out, tabs );
    XMLWriteElement( mYield[ period ], "yield", out, tabs );
    XMLWriteElement( mAgProdChange[ period ], "agProdChange", out, tabs );
    XMLWriteElement( mInterestRate, "interest-rate", out, tabs );
    mCarbonContentCalc->toDebugXML( period, out, tabs );
}

/*! \brief Overwrites the intrinsicRate for this leaf node.
* \author James Blackwood, Steve Smith
*/
void LandLeaf::setIntrinsicRate( const string& aRegionName,
                                 const string& aLandType,
                                 const string& aProductName,
                                 const double aIntrinsicRate,
                                 const int aPeriod )
{
    assert( aProductName == mName );

    // aIntrinsicRate [$/GCal] * intrinsicYieldMode [GCal/kHa] = [$/kHa]
    // The intrinsicRate that is passed in is $/Gcal. 
    // It is multiplied by intrinsicYieldMode (GCal/kHa) to convert it to $/kHa.   
    // For biomass [$/GJ] * [GJ/kHa] = [$/kHa]
    // Add carbon value to intrinsic rate.
    mIntrinsicRate[ aPeriod ] = max( aIntrinsicRate * mIntrinsicYieldMode[ aPeriod ] 
                               + getCarbonValue( aRegionName, aPeriod ), 0.0 );
}

/*! \brief Calculates the carbon value per hectare for this land type.
* \author James Blackwood, Josh Lurz
*/
double LandLeaf::getCarbonValue( const string& aRegionName, const int aPeriod ) const {
    // Check that the interest rate was initialized.
    assert( mInterestRate.isInited() );

    // Check if a carbon market exists and has a non-zero price.
    const Marketplace* marketplace = scenario->getMarketplace();
    double carbonPrice = marketplace->getPrice( "CO2", aRegionName, aPeriod, false );
    if( carbonPrice != Marketplace::NO_MARKET_PRICE && carbonPrice > util::getSmallNumber() ){
        // With carbon content in Mg C/Ha == TC/Ha, to TC/Ha * $/TC = $/Ha.
        // TODO: Check these units.
        const double HA_PER_KHA = 1000;
        const int year = scenario->getModeltime()->getper_to_yr( aPeriod );

        // Calculate the carbon value as the total carbon content of the land
        // multiplied by the carbon price and the interest rate.
        double carbonSubsidy = ( mCarbonContentCalc->getPotentialAboveGroundCarbon( year )
                                 + mCarbonContentCalc->getPotentialBelowGroundCarbon( year ) )
                               * carbonPrice * mInterestRate * HA_PER_KHA;

        assert( carbonSubsidy >= 0 );

        return carbonSubsidy;
    }
    return 0;
}

void LandLeaf::setCarbonContent( const string& aLandType,
                                 const string& aProductName,
                                 const double aAboveGroundCarbon,
                                 const double aBelowGroundCarbon,
                                 const int aPeriod )
{
    assert( aProductName == mName );
    assert( mCarbonContentCalc.get() );

    mCarbonContentCalc->setUnitAboveGroundCarbon( aAboveGroundCarbon, aPeriod );
    mCarbonContentCalc->setUnitBelowGroundCarbon( aBelowGroundCarbon, aPeriod );
}

/*!
 * \brief Set amount of land allocated for this leaf.
 * \details Sets the amount of land allocated for this leaf in through a passed
 *          in value.
 * \author James Blackwood
 */
void LandLeaf::setCalLandAllocation( const string& aLandType,
                                     const string& aProductName,
                                     const double aCalLandUsed,
                                     const int aHarvestPeriod, 
                                     const int aCurrentPeriod )
{
    assert( aProductName == mName );
    mLandAllocation[ aHarvestPeriod ] = aCalLandUsed;
}

/*! \brief Sets land allocation of unmanged land leaf
* \details Does nothing for normal production leafs
* \param aRegionName Region name.
* \param aLandAllocation Total land to be allocated to this unmanaged land leaf
* \param aPeriod Model period.
* \author Steve Smith
*/
void LandLeaf::setUnmanagedLandAllocation( const string& aRegionName,
                                           const double aLandAllocation,
                                           const int aPeriod )
{
}

/*! \brief Adjust land values for unmanaged land nodes as necessary
*
* Does nothing for normal production leafs
* \param aRegionName Region name.
* \param aPeriod Model period.
* \author Steve Smith
*/
void LandLeaf::setUnmanagedLandValues( const string& aRegionName, const int aPeriod ) {
}

/*! \brief Set calibrated observed yield for this land.
*
* Sets the calibrated observed yield for this land in through a passed in value.
*
* \author James Blackwood
*/
void LandLeaf::setCalObservedYield( const string& aLandType,
                                    const string& aProductName,
                                    const double aCalObservedYield,
                                    const int aPeriod )
{
    assert( aProductName == mName );
    mCalObservedYield[ aPeriod ] = aCalObservedYield;
}

double LandLeaf::getCalAveObservedRateInternal( const string& aLandType,
                                                const int aPeriod,
                                                const double aSigma ) const
{
    // This should never be called for a land leaf.
    assert( false );
    return 0;
}

/*! \brief Applies a technology change for this land.
*
* Changes the agProdChange variable which will later be factored into the
* intrinsicYieldMode.
*
* \author James Blackwood
*/
void LandLeaf::applyAgProdChange( const string& aLandType,
                                  const string& aProductName,
                                  const double aAgProdChange,
                                  const int aPeriod )
{
    assert( aProductName == mName );
    double previousAgProdChange = 1;
    if ( aPeriod > 0 ) {
        previousAgProdChange = mAgProdChange[ aPeriod - 1 ];
    }

    const Modeltime* modeltime = scenario->getModeltime();
    int timestep = modeltime->gettimestep( aPeriod );
    mAgProdChange[ aPeriod ] = previousAgProdChange * pow( 1 + aAgProdChange, timestep );
    mIntrinsicYieldMode[ aPeriod ] *= mAgProdChange[ aPeriod ];
}

/*! \brief This calculates a temporary share, later a normalized share is
*          assigned in the node version of this method. Unmanaged land is shared
*          out proportionately with the land allocated to that level originally.
* \param sigmaAbove the sigma value from the node above this level.
* \param totalBaseLandAllocation total base unmanaged land allocation for this
*        level.
* \author James Blackwood, Steve Smith
*/
void LandLeaf::calcLandShares( const string& aRegionName,
                               const double aSigmaAbove,
                               const double aTotalLandAllocated,
                               const int aPeriod )
{
    // Land node should have validated all sigmas.
    assert( aSigmaAbove > util::getSmallNumber() );

    if( mIntrinsicRate[ aPeriod ] < util::getSmallNumber() ){
        // Intrinsic rates may not be negative.
        assert( mIntrinsicRate[ aPeriod ] >= 0 );
        mShare[ aPeriod ] = util::getSmallNumber();
    }
    else {
        mShare[ aPeriod ] = pow( mIntrinsicRate[ aPeriod ], 1 / aSigmaAbove );
        assert( util::isValidNumber( mShare[ aPeriod ] ) );
    }
}

/*! \brief Calculates the land allocated for this land.
*
* Uses the land allocated in the node above and the share from this land to
* calculate the landAllocation.
*
* \author James Blackwood
*/
void LandLeaf::calcLandAllocation( const string& aRegionName,
                                   const double aLandAllocationAbove,
                                   const int aPeriod )
{
    mLandAllocation[ aPeriod ] = aLandAllocationAbove * mShare[ aPeriod ];

    // Set the amount of land use change into the carbon content calculator.
    mCarbonContentCalc->setTotalLandUse( mLandAllocation[ aPeriod ], aPeriod );

    // Calculate carbon emissions.
    // TODO: Better location.
    mCarbonContentCalc->calc( aPeriod );

    // Add emissions to the carbon market.
    // TODO: Determine how to handle this correctly.
    // Marketplace* marketplace = scenario->getMarketplace();
    // const int year = scenario->getModeltime()->getper_to_yr( aPeriod );
    // double emissions = mCarbonContentCalc->getNetLandUseChangeEmission( year );
    // 
    // marketplace->addToDemand( "CO2", aRegionName, emissions, aPeriod, false );
}

void LandLeaf::calcYieldInternal( const string& aLandType,
                                  const string& aProductName,
                                  const string& aRegionName,
                                  const double aProfitRate,
                                  const double aAvgIntrinsicRate,
                                  const int aHarvestPeriod,
                                  const int aCurrentPeriod )
{
    assert( aHarvestPeriod >= aCurrentPeriod );

    // Compute the full profit rate including payments for carbon storage.
    // ProfitRate[$/GCal] = aProfitRate[$/GCal] + carbonValue[$/kHa] / intrinsicYieldMode[GCal/kHa]
    double totalProfitRate = aProfitRate + getCarbonValue( aRegionName, aCurrentPeriod )
                                           / mIntrinsicYieldMode[ aCurrentPeriod ];

    if ( totalProfitRate >= util::getSmallNumber() ) {
        // AveRate $/kHa / ($/GCal) = GCal/kHa)
        // For biomass: AveRate $/kHa / $/GJ = GJ/kHa.
        mYield[ aHarvestPeriod ] = aAvgIntrinsicRate / totalProfitRate;
    }
    else {
        // Make yield zero so that there is no production at zero or negative
        // profit rates.
        mYield[ aHarvestPeriod ] = 0;
    }
}

double LandLeaf::getYield ( const string& aLandType,
                            const string& aProductName,
                            const int aPeriod ) const
{
    assert( aProductName == mName );
    return mYield[ aPeriod ];
}

/*! \brief Should never be called, because only a child can be added to a node, not a leaf.
*
* \author James Blackwood
*/
void LandLeaf::addChild( ALandAllocatorItem* aChild ) {
    assert( false );
}

/*! \brief The leaf version of getLandAllocation.
*
* \author James Blackwood
* \return the LandAllocation at this node, if the productName matches the name of this landType.
*/
double LandLeaf::getLandAllocation( const string& aProductName,
                                    const int aPeriod ) const
{
    if ( aProductName == mName ) {
        return mLandAllocation[ aPeriod ];
    }
    return 0;
}

/*! \brief Get total land allocation.
*
* For leaves without land vintaging this is identical to getLandAllocation
*
* \author James Blackwood
* \return the LandAllocation at this node, if the productName matches the name of this landType.
*/
double LandLeaf::getTotalLandAllocation( const string& aProductName,
                                         const int aPeriod ) const
{
    return getLandAllocation( aProductName, aPeriod );
}

/*! \brief Returns the baseLandAllocation of this leaf.
* \author Steve Smith
* \return the baseLandAllocation of this landType
*/
double LandLeaf::getBaseLandAllocation( const int aPeriod ) const {
    return 0;
}

void LandLeaf::calcEmission( const string& aRegionName,
                             const GDP* aGDP, 
                             const int aPeriod ){
}

void LandLeaf::updateSummary( Summary& aSummary, const int period ) {
}

/*! \brief Write output to csv output file. 
*
*
* \author Steve Smith
*/
void LandLeaf::csvOutput( const string& aRegionName ) const {
     ALandAllocatorItem::csvOutput( aRegionName );
    
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);

    // function protocol
    void fileoutput3(string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    // write land allocations for region
    fileoutput3(aRegionName, mName," "," ","Intr Rate","$/kHa", mIntrinsicRate.convertToVector() );
    fileoutput3(aRegionName, mName," "," ","Intr Yield Mode","GCal/kHa", mIntrinsicYieldMode.convertToVector() );
    fileoutput3(aRegionName, mName," "," ","calObsYield","GCal/kHa", mCalObservedYield.convertToVector() );
    
    for( int i = 0; i < maxper; ++i ){
        temp[ i ] = getCarbonValue( aRegionName, i );
    }

    fileoutput3(aRegionName, mName," "," ","carbonValue","000Ha", temp);
    fileoutput3(aRegionName, mName," "," ","Ag Productivity Change","none", mAgProdChange.convertToVector() );
}

void LandLeaf::dbOutput( const string& aRegionName ) const {
    ALandAllocatorItem::dbOutput( aRegionName );

    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);

    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    // write land allocations for region
    dboutput4(aRegionName, "Land Allocation", mName,"Intr Rate","$/kHa", mIntrinsicRate.convertToVector() );
    dboutput4(aRegionName, "Land Allocation", mName,"Intr Yield Mode","GCal/kHa", mIntrinsicYieldMode.convertToVector() );
    dboutput4(aRegionName, "Land Allocation", mName,"calObsYield","GCal/kHa", mCalObservedYield.convertToVector() );

    for( int i = 0; i < maxper; ++i ){
        temp[ i ] = getCarbonValue( aRegionName, i );
    }
    dboutput4(aRegionName, "Land Allocation", mName,"carbonValue","000Ha", temp );

    for( int i = 0; i < maxper; ++i ){
        temp[ i ] = mCarbonContentCalc->getNetLandUseChangeEmission( modeltime->getper_to_yr( i ) );
    }
    dboutput4(aRegionName, "Land Allocation", mName,"land-use-change-emission","000Ha", temp );

    dboutput4(aRegionName, "Land Allocation", mName,"Ag Productivity Change","none", mAgProdChange.convertToVector() );
}

/*! \brief Update a visitor for a LandLeaf.
* \param aVisitor Visitor to update.
* \param aPeriod Period to update.
*/
void LandLeaf::accept( IVisitor* aVisitor, const int aPeriod ) const {
	aVisitor->startVisitLandLeaf( this, aPeriod );

    // All land leaves have carbon content calculators, but it may not have been
    // instantiated yet.
    if( mCarbonContentCalc.get() ){
        mCarbonContentCalc->accept( aVisitor, aPeriod );
    }
	aVisitor->endVisitLandLeaf( this, aPeriod );
}
