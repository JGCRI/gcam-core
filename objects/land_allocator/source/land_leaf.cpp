/*!
 * \file land_leaf.cpp
 * \ingroup Objects
 * \brief LandLeaf class source file.
 * \author James Blackwood
 */

#include "util/base/include/definitions.h"
#include <string>
#include <vector>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <deque>

#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/scenario.h"
#include "land_allocator/include/land_leaf.h"
#include "util/base/include/ivisitor.h"
#include "emissions/include/production_carbon_calc.h"
#include "containers/include/iinfo.h"
#include "ccarbon_model/include/acarbon_flow.h"
#include "land_allocator/include/land_use_history.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include <typeinfo>

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*!
 * \brief Constructor.
* \author James Blackwood
 * \param aParent Pointer to this leafs's parent.
 * \param aName Product name.
*/
LandLeaf::LandLeaf( const ALandAllocatorItem* aParent,
                    const string& aName ):
ALandAllocatorItem( aParent, eLeaf ),
mIntrinsicYieldMode( 0 ),
mYield( scenario->getModeltime()->getmaxper(), -1 ),
mCalObservedYield( 0 ),
mLandUseHistory( 0 ),
mCalDataExists( false ),
mIntrinsicYieldModeAgProdMultiplier ( 1 ),
// Give enough room and re-size later
mAgProdChange( scenario->getModeltime()->getmaxper() + 15 )
{
    // Can't use initializer because mName is a member of ALandAllocatorItem,
    // not LandLeaf.
    this->mName = aName;
}

//! Destructor
LandLeaf::~LandLeaf() {
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

bool LandLeaf::XMLParse( const xercesc::DOMNode* aNode ){

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
        if ( !XMLDerivedClassParse( nodeName, curr ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing "
                    << getXMLName() << "." << endl;
        }
    }

    return true;
}

bool LandLeaf::XMLDerivedClassParse( const std::string& aNodeName, const xercesc::DOMNode* aCurr ){
    // Allow derived classes to override.
    return false;
}

/*! \brief Get the XML node name for output to XML.
 *
 * This public function accesses the private constant string, XML_NAME.
 * This way the tag is always consistent for both read-in and output and can be easily changed.
 *  This function may be virtual to be overriden by derived class pointers.
 * \author Josh Lurz, James Blackwood
 * \return The constant XML_NAME.
 */
const string& LandLeaf::getXMLName() const {
    const static string XML_NAME = "LandAllocatorLeaf";
    return XML_NAME;
}

void LandLeaf::completeInit( const string& aRegionName,
                             const IInfo* aRegionInfo )
{
    // Store the interest rate from the region.
    mInterestRate = aRegionInfo->getDouble( "interest-rate", true );

    // Set the carbon cycle object if it has not already been initialized. Use a
    // virtual function so that derived leaves may use a different default type.
    initCarbonCycle();

    // Initialize the carbon-cycle object, passing in the conceptual root key
    mCarbonContentCalc->completeInit( CarbonModelUtils::getConceptualRootKey( this ) );

    // Ensure that a carbon cycle object has been setup.
    assert( mCarbonContentCalc.get() );

    const Modeltime* modeltime = scenario->getModeltime();
    int maxper = modeltime->getmaxper();
    mAgProdChange.resize( maxper );
}

/*!
 * \brief Initialize the carbon cycle object.
 * \details Instantiate a carbon cycle for a production leaf.
 */
void LandLeaf::initCarbonCycle(){
    if( !mCarbonContentCalc.get() ){
        mCarbonContentCalc.reset( new ProductionCarbonCalc );
    }

    // Initialize all historical land allocations.
}

void LandLeaf::addLandUsage( const string& aLandType,
                             const string& aProductName,
                             const ILandAllocator::LandUsageType aType,
                             const int aPeriod )
{
    // The leaf is the land usage so this should never be called.
    assert( false );
}

bool LandLeaf::isUnmanagedNest() const {
    return false;
}

void LandLeaf::setInitShares( const string& aRegionName,
                              const double aSigmaAbove,
                              const double aLandAllocationAbove,
                              const double aParentHistoryShare,
                              const LandUseHistory* aParentHistory,
                              const int aPeriod )
{
    if ( aLandAllocationAbove > util::getSmallNumber() ) {
        mShare[ aPeriod ] = mLandAllocation[ aPeriod ] / aLandAllocationAbove;
    }
    else {
        mShare[ aPeriod ] = 0;
    }

    // Now that the leaf share is known set the land use history.
    initLandUseHistory( aParentHistoryShare, aParentHistory, aPeriod );
}

void LandLeaf::resetToCalLandAllocation( const int aPeriod ) {
   mLandAllocation[ aPeriod ] = mCalLandAllocation[ aPeriod ];
}

/*!
 * \brief Initialize the land use history for the leaf.
 * \param aParentHistoryShare The parent of this leaf's share of the land use
 *        history.
 * \param aParentHistory Land use history container.
 * \param aFirstCalibratedPeriod Model period to assume is the first calibrated period.
 */
void LandLeaf::initLandUseHistory( const double aParentHistoryShare,
                                   const LandUseHistory* aParentHistory,
                                   const int aFirstCalibratedPeriod )
{
    // Kluge until we figure out what do do here. How do we figure out what the first calibrated period
    // is for AgLU? Presumably it will be 1990, but that could change at some point. - sjs
    // Set first calibrated period to 1990
    int localFirstCalibratedPeriod = min( aFirstCalibratedPeriod, scenario->getModeltime()->getyr_to_per( 1990 ) );
    
    // Check that the share has been normalized.
    assert( mShare[ localFirstCalibratedPeriod ].isInited() &&
            mShare[ localFirstCalibratedPeriod ] >= 0 &&
            mShare[ localFirstCalibratedPeriod ] <= 1 );

    mCarbonContentCalc->initLandUseHistory( aParentHistory,
                                            aParentHistoryShare * mShare[ localFirstCalibratedPeriod ] );
}

void LandLeaf::setIntrinsicYieldMode( const double aIntrinsicYieldAbove,
                                      const double aSigmaAbove,
                                      const int aPeriod )
{
    assert( mShare[ aPeriod ].isInited() &&
            mShare[ aPeriod ] >= 0 &&
            mShare[ aPeriod ] <= 1 );

    assert( mCalObservedYield[ aPeriod ].isInited() );

    // Only set the intrinsic yield mode if the calibrated observed yield is known.
    if( mCalObservedYield[ aPeriod ] > util::getSmallNumber() ){
        // TODO: Find better way of specifying share for the intrinsic yield calc
        // for good with no initial share
        // If share is zero and have read in a calibrated yield (e.g. biomass or
        // other new crops) then use an arbitrary 0.25 share need to figure out what
        // to read in for this, or how to specify (specify comparable?)
        double share = mShare[ aPeriod ] > util::getSmallNumber() ? mShare[ aPeriod ].get() : getDefaultShare();
        double intrinsicRate = aIntrinsicYieldAbove * pow( share, aSigmaAbove );
        double intrinsicYield = intrinsicRate * mCalObservedYield[ aPeriod ];
        if( intrinsicYield < util::getSmallNumber() ) {
            // Don't set 
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Skipping -- intrinsic yield mode would have been zero in period " << aPeriod
                    << " for land leaf " << mName << "." << endl;
            return;
        }

        fill( mIntrinsicYieldMode.begin() + aPeriod,
              mIntrinsicYieldMode.end(),
              intrinsicYield );
        checkCalObservedYield( aPeriod );
    }
}

/*! \brief Check whether the calibrated observed yield is valid for this leaf
*          type.
* \param aPeriod Model period.
*/
void LandLeaf::checkCalObservedYield( const int aPeriod ) const {
    if ( util::isEqual( mCalObservedYield[ aPeriod ], Value( 0.0 ) ) ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "calObservedYield is zero in " << mName << " in " << " period " << aPeriod << endl; 
    }
}

void LandLeaf::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    // Do nothing because land leaves are dynamically created.
}

void LandLeaf::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteElement( mLandAllocation[ period ], "landAllocation", out, tabs );
    XMLWriteElement( mIntrinsicYieldMode[ period ], "intrinsicYieldMode", out, tabs );
    XMLWriteElement( mCalObservedYield[ period ], "calObservedYield", out, tabs );
    XMLWriteElement( mYield[ period ], "yield", out, tabs );
    XMLWriteElement( mAgProdChange[ period ], "agProdChange", out, tabs );
    XMLWriteElement( mIntrinsicYieldModeAgProdMultiplier[ period ], "agProdMultiplier", out, tabs );
    XMLWriteElement( mInterestRate, "interest-rate", out, tabs );
    mCarbonContentCalc->toDebugXML( period, out, tabs );
}

void LandLeaf::setIntrinsicRate( const string& aRegionName,
                                 const string& aLandType,
                                 const string& aProductName,
                                 const double aIntrinsicRate,
                                 const int aPeriod )
{
    assert( aProductName == mName );
    assert( mIntrinsicYieldMode[ aPeriod ].isInited() &&
            mIntrinsicYieldMode[ aPeriod ] >= 0 );

    // aIntrinsicRate [$/GCal] * intrinsicYieldMode [GCal/kHa] = [$/kHa]
    // The intrinsicRate that is passed in is $/Gcal. 
    // It is multiplied by intrinsicYieldMode (GCal/kHa) to convert it to $/kHa.   
    // For biomass [$/GJ] * [GJ/kHa] = [$/kHa]
    // Add carbon value to intrinsic rate.
    // TODO -- check if this is correct. Carbonvalue is applied to profit rate AND to mIntrinsicRate
    mIntrinsicRate[ aPeriod ] = max( aIntrinsicRate * mIntrinsicYieldMode[ aPeriod ] * mIntrinsicYieldModeAgProdMultiplier[ aPeriod ] 
                               + getCarbonValue( aRegionName, aPeriod ), 0.0 );
}

/*! \brief Calculates the carbon value per hectare for this land type.
* \author James Blackwood, Josh Lurz
* \param aRegionName Region name.
* \param aPeriod Model period.
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

void LandLeaf::setCalLandAllocation( const string& aLandType,
                                     const string& aProductName,
                                     const double aCalLandUsed,
                                     const int aHarvestPeriod, 
                                     const int aCurrentPeriod )
{
    assert( aProductName == mName );
    
    mCalDataExists[ aCurrentPeriod ] = true;

    mLandAllocation[ aCurrentPeriod ] = aCalLandUsed;
    mCalLandAllocation[ aCurrentPeriod ] = aCalLandUsed;
}

void LandLeaf::setUnmanagedLandAllocation( const string& aRegionName,
                                           const double aNewUnmanaged,
                                           const int aPeriod )
{
    // Does nothing for normal production leafs.
}

void LandLeaf::setUnmanagedLandValues( const string& aRegionName, const int aPeriod ) {
    // Does nothing for production leaves.
}

void LandLeaf::setCalObservedYield( const string& aLandType,
                                    const string& aProductName,
                                    const double aCalObservedYield,
                                    const int aPeriod )
{
    assert( aProductName == mName );
    mCalObservedYield[ aPeriod ] = aCalObservedYield;
}

void LandLeaf::applyAgProdChange( const string& aLandType,
                                  const string& aProductName,
                                  const double aAgProdChange,
                                  const int aHarvestPeriod, 
                                  const int aCurrentPeriod )
{
    assert( aProductName == mName );
    assert( mIntrinsicYieldMode[ aCurrentPeriod ].isInited() );

    double previousAgProdChange = 1;
    if ( aCurrentPeriod >= 1 ) {
        previousAgProdChange = mAgProdChange[ aCurrentPeriod - 1 ];
    }

    const Modeltime* modeltime = scenario->getModeltime();
    int timestep = modeltime->gettimestep( aCurrentPeriod );

    // Calculate Cumulative prod change from current to harvest period.
    // This means that calculated yields for current period are assumed to be applied
    // to the harvest period
    // aAgProdChange must be constant if aHarvestPeriod != aCurrentPeriod
    for ( int aPeriod = aCurrentPeriod; aPeriod <= aHarvestPeriod; ++aPeriod ) {
        // Don't apply prod change in a calibration period
        if ( !( mCalDataExists[ aCurrentPeriod ] && aPeriod == aCurrentPeriod ) ) {
            mAgProdChange[ aPeriod ] = previousAgProdChange * pow( 1 + aAgProdChange, timestep );
        }
        else {
            mAgProdChange[ aPeriod ]  = 1.0;
        }
        previousAgProdChange = mAgProdChange[ aPeriod ];
    }

    // Save amount the intrinsic yield mode is to be adjusted by cumulative prod change
    mIntrinsicYieldModeAgProdMultiplier[ aCurrentPeriod ] = mAgProdChange[ aHarvestPeriod ];
}

double LandLeaf::calcLandShares( const string& aRegionName,
                                 const double aSigmaAbove,
                                 const double aTotalBaseLand,
                                 const int aPeriod )
{
    // Land node should have validated all sigmas.
    assert( aSigmaAbove > util::getSmallNumber() );

    // Intrinsic rates may not be negative.
    assert( mIntrinsicRate[ aPeriod ].isInited() && mIntrinsicRate[ aPeriod ] >= 0 );

    // Production land leaves ignore aTotalLandAllocated.
    double unnormalizedShare;
    if( mIntrinsicRate[ aPeriod ] < util::getSmallNumber() ){
        unnormalizedShare = util::getSmallNumber();
    }
    else {
        unnormalizedShare = pow( mIntrinsicRate[ aPeriod ].get(), 1 / aSigmaAbove );
    }

    // result should be > 0.
    assert( unnormalizedShare >= 0 );

    return unnormalizedShare;
}

void LandLeaf::calcLandAllocation( const string& aRegionName,
                                   const double aLandAllocationAbove,
                                   const int aPeriod )
{
    assert( mShare[ aPeriod ].isInited() &&
            mShare[ aPeriod ] >= 0 &&
            mShare[ aPeriod ] <= 1 );

    mLandAllocation[ aPeriod ] = aLandAllocationAbove * mShare[ aPeriod ];

    // Set the land use in the carbon content calculator.
    mCarbonContentCalc->setTotalLandUse( mLandAllocation[ aPeriod ], aPeriod );

    // Add emissions to the carbon market.
    // Land-use emissions should not be in the market. 
    // This can never be stable with a 15-year solution period as this is an implied c-cycle inversion.
}


void LandLeaf::calcLUCCarbonFlowsOut( const string& aRegionName,
                                          const int aYear ){
    mCarbonContentCalc->setLandUseValue( aYear );
    mCarbonContentCalc->calcLandUseChange( aYear, eLUCFlowOut );
}


void LandLeaf::calcLUCCarbonFlowsIn( const string& aRegionName,
                                            const int aYear ){
    mCarbonContentCalc->calcLandUseChange( aYear, eLUCFlowIn );
}

void LandLeaf::calcCarbonBoxModel( const string& aRegionName,
                                           const int aYear ){
    mCarbonContentCalc->calc( aYear );
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
    assert( mIntrinsicYieldMode[ aCurrentPeriod ].isInited() );

    // Add carbon value
    // ProfitRate[$/GCal] = aProfitRate[$/GCal] + carbonValue[$/kHa] / intrinsicYieldMode[GCal/kHa]
    // Note that the mIntrinsicYieldMode is used, not the actual yield (which is not known until this
    // is finished). This may bias results.
    // TODO -- check if this is correct. Carbonvalue is applied to profit rate AND to mIntrinsicRate
    double carbonValuePerOutput = 0;
    if( mIntrinsicYieldMode[ aCurrentPeriod ] > util::getSmallNumber() ) {
        carbonValuePerOutput = getCarbonValue( aRegionName, aCurrentPeriod ) 
                               / ( mIntrinsicYieldMode[ aCurrentPeriod ] * mIntrinsicYieldModeAgProdMultiplier[ aCurrentPeriod ] );
    }

    // Compute the full profit rate including payments for carbon storage.
    double totalProfitRate = aProfitRate + carbonValuePerOutput;

    if ( totalProfitRate > util::getSmallNumber() ) {
        // AveRate $/kHa / ($/GCal) = GCal/kHa)
        // For biomass: AveRate $/kHa / $/GJ = GJ/kHa.
        mYield[ aHarvestPeriod ] = aAvgIntrinsicRate / totalProfitRate;
    }
    else {
        // Don't allow production at zero or negative profit rates.
        // TODO: Isn't this a discontinuity?
        mYield[ aHarvestPeriod ] = 0;
    }
}

double LandLeaf::getYield( const string& aLandType,
                           const string& aProductName,
                           const int aPeriod ) const
{
    assert( aProductName == mName );
    assert( mYield[ aPeriod ].isInited() );
    return mYield[ aPeriod ];
}

void LandLeaf::addChild( ALandAllocatorItem* aChild ) {
    assert( false );
}

double LandLeaf::getLandAllocation( const string& aLandType,
                                    const string& aProductName,
                                    const int aPeriod ) const
{
    assert( aProductName == mName );
    assert( mLandAllocation[ aPeriod ].isInited() );

    return mLandAllocation[ aPeriod ];
}

double LandLeaf::getTotalLandAllocation( const LandAllocationType aType,
                                         const int aPeriod ) const
{
    // Land allocation may not be set yet if this is an uncalibrated leaf.
    // In this case returning zero is correct.

    // Unless a land leaf is overridden it is a production leaf.
    if( aType == eAnyLand || aType == eManaged ){
        return mLandAllocation[ aPeriod ];
}
    return 0;
}

double LandLeaf::getBaseLandAllocation( const int aPeriod ) const {
    return 0;
}

bool LandLeaf::isConceptualRoot() const {
    return false;
}

double LandLeaf::getSigma() const {
    return 0;
}

void LandLeaf::csvOutput( const string& aRegionName ) const {
     ALandAllocatorItem::csvOutput( aRegionName );
    
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);

    // function protocol
    void fileoutput3(string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    // write land allocations for region
    fileoutput3(aRegionName, mName," "," ","Intr Rate","$/kHa", convertToVector( mIntrinsicRate ) );
    fileoutput3(aRegionName, mName," "," ","Intr Yield Mode","GCal/kHa", convertToVector( mIntrinsicYieldMode ) );
    fileoutput3(aRegionName, mName," "," ","calObsYield","GCal/kHa", convertToVector( mCalObservedYield ) );
    
    for( int i = 0; i < maxper; ++i ){
        temp[ i ] = getCarbonValue( aRegionName, i );
    }

    fileoutput3(aRegionName, mName," "," ","carbonValue","000Ha", temp);
    fileoutput3(aRegionName, mName," "," ","Ag Productivity Change","none",  mAgProdChange );
}

void LandLeaf::dbOutput( const string& aRegionName ) const {
    // TODO: Won't this be wrong any time two leaves have the same name?
    ALandAllocatorItem::dbOutput( aRegionName );

    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);

    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    // write land allocations for region
    dboutput4(aRegionName, "Land Allocation", mName,"Intr Rate","$/kHa", convertToVector( mIntrinsicRate ) );
    dboutput4(aRegionName, "Land Allocation", mName,"Intr Yield Mode","GCal/kHa", convertToVector( mIntrinsicYieldMode ) );
    dboutput4(aRegionName, "Land Allocation", mName,"calObsYield","GCal/kHa", convertToVector( mCalObservedYield ) );

    for( int i = 0; i < maxper; ++i ){
        temp[ i ] = getCarbonValue( aRegionName, i );
    }
    dboutput4(aRegionName, "Land Allocation", mName,"carbonValue","000Ha", temp );

    for( int i = 0; i < maxper; ++i ){
        temp[ i ] = mCarbonContentCalc->getNetLandUseChangeEmission( modeltime->getper_to_yr( i ) );
    }
    dboutput4(aRegionName, "Land Allocation", mName,"land-use-change-emission","000Ha", temp );
    
    if( mAgProdChange.size() <= maxper ) {
        dboutput4(aRegionName, "Land Allocation", mName,"Ag Productivity Change","none", mAgProdChange );
    }
    else {
        // the database can only write out maxper periods of data so any data after
        // that will have to be ignored
        for( int i = 0; i < maxper; ++i ){
            temp[ i ] = mAgProdChange[ i ];
        }
        dboutput4(aRegionName, "Land Allocation", mName,"Ag Productivity Change","none", temp );
    }
}

void LandLeaf::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitLandLeaf( this, aPeriod );

    // All land leaves have carbon content calculators, but it may not have been
    // instantiated yet.
    if( mCarbonContentCalc.get() ){
        mCarbonContentCalc->accept( aVisitor, aPeriod );
    }
    aVisitor->endVisitLandLeaf( this, aPeriod );
}

void LandLeaf::copyCarbonBoxModel( const ICarbonCalc *aCarbonCalc ) {
    if ( aCarbonCalc != NULL ){
        this->mCarbonContentCalc.reset( aCarbonCalc->clone() );
    }    
}

ICarbonCalc* LandLeaf::getCarbonContentCalc() const{
    return ( mCarbonContentCalc.get() );
}
