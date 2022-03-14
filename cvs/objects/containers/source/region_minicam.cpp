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
* \file region_minicam.cpp
* \ingroup Objects
* \brief The RegionMiniCAM class source file.
* \author Sonny Kim
*/

//TODO: Clean up #includes

#include "util/base/include/definitions.h"
#include <fstream>
#include <string>
#include <vector>
#include <cassert>
#include <algorithm>
#include <memory>
#include <stack>

#include "containers/include/region_minicam.h"
#include "containers/include/gdp.h"
#include "containers/include/scenario.h"
#include "containers/include/info_factory.h"
#include "containers/include/iinfo.h"
#include "containers/include/market_dependency_finder.h"

#include "sectors/include/sector.h"
#include "sectors/include/afinal_demand.h"

#include "consumers/include/gcam_consumer.h"
#include "containers/include/national_account.h"

#include "resources/include/resource.h"

#include "demographics/include/demographic.h"

#include "marketplace/include/marketplace.h"

#include "land_allocator/include/land_allocator.h"
#include "emissions/include/emissions_summer.h"
#include "emissions/include/luc_emissions_summer.h"
#include "policy/include/policy_ghg.h"

#include "util/base/include/ivisitor.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_parse_helper.h"
#include "util/base/include/model_time.h"
#include "util/base/include/configuration.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"

#include "containers/include/resource_activity.h"
#include "containers/include/sector_activity.h"
#include "containers/include/final_demand_activity.h"
#include "containers/include/land_allocator_activity.h"
#include "containers/include/consumer_activity.h"

using namespace std;

typedef std::vector<AFinalDemand*>::iterator FinalDemandIterator;
typedef std::vector<AFinalDemand*>::const_iterator CFinalDemandIterator;
typedef std::vector<AResource*>::iterator ResourceIterator;
typedef std::vector<GHGPolicy*>::iterator GHGPolicyIterator;
typedef std::vector<GHGPolicy*>::const_iterator CGHGPolicyIterator;
typedef std::vector<Sector*>::iterator SectorIterator;
typedef std::vector<Sector*>::reverse_iterator SectorReverseIterator;
typedef std::vector<Sector*>::const_iterator CSectorIterator;
typedef std::vector<Consumer*>::iterator ConsumerIterator;
typedef std::vector<Consumer*>::const_iterator CConsumerIterator;

const double DEFAULT_SOCIAL_DISCOUNT_RATE = 0.02;
const double DEFAULT_PRIVATE_DISCOUNT_RATE = 0.1;


extern Scenario* scenario;

//! Default constructor
RegionMiniCAM::RegionMiniCAM() {
    mGDP = 0;
    mLandAllocator = 0;

    mInterestRate = 0;
    mSocialDiscountRate = DEFAULT_SOCIAL_DISCOUNT_RATE;
    mPrivateDiscountRateLand = DEFAULT_PRIVATE_DISCOUNT_RATE;
}

//! Default destructor destroys sector, demsector, Resource, and
//! population objects.
RegionMiniCAM::~RegionMiniCAM() {
    clear();
}

//! Clear member variables and initialize elemental members.
void RegionMiniCAM::clear(){

    for ( FinalDemandIterator demIter = mFinalDemands.begin(); demIter != mFinalDemands.end(); ++demIter ) {
        delete *demIter;
    }

    for( CConsumerIterator consumerIter = mConsumers.begin(); consumerIter != mConsumers.end(); ++consumerIter ) {
        delete *consumerIter;
    }
    
    delete mGDP;
    delete mLandAllocator;
}

bool RegionMiniCAM::XMLParse(rapidxml::xml_node<char>* & aNode) {
    string nodeName = XMLParseHelper::getNodeName(aNode);
    if( nodeName == "PrimaryFuelCO2Coef" ) {
        map<string, string> attrs = XMLParseHelper::getAllAttrs(aNode);
        mPrimaryFuelCO2Coef[ attrs["name"] ] = XMLParseHelper::getValue<double>( aNode );
        return true;
    }
    else {
        return false;
    }
}


/*! Complete the initialization. Get the size of vectors, initialize AGLU,
*   create all markets, call complete initialization
*  functions for nested objects, update the fuel map, and find simultaneities.
* \todo I think since there is one indirect ghg object for each sector, it might
*       be better in sector. This may require deriving supply sector.
*/
void RegionMiniCAM::completeInit() {
    Region::completeInit();

    // Region info has no parent Info.
    mRegionInfo = InfoFactory::constructInfo( 0, mName );

    // Add the interest rate to the region info.
    // TODO: mInterestRate is currently not used in GCAM and could be removed
    mRegionInfo->setDouble( "interest-rate", mInterestRate );
    
    // Add the social discount rate to the region info.
    mRegionInfo->setDouble( "social-discount-rate", mSocialDiscountRate );
    
    // Add the land private discount rate to the region info.
    mRegionInfo->setDouble( "private-discount-rate-land", mPrivateDiscountRateLand );

    // initialize demographic
    if( mDemographic ){
        mDemographic->completeInit();
    }

    // Initialize the GDP
    if( mGDP ){
        mGDP->initData( mDemographic );
    }

    for( SectorIterator sectorIter = mSupplySector.begin(); sectorIter != mSupplySector.end(); ++sectorIter ) {
        ( *sectorIter )->setNames( mName );
        ( *sectorIter )->completeInit( mRegionInfo, mLandAllocator );
    }

    if ( mLandAllocator ) {
        mLandAllocator->completeInit( mName, mRegionInfo );
    }

    for( FinalDemandIterator demandSectorIter = mFinalDemands.begin();
        demandSectorIter != mFinalDemands.end(); ++demandSectorIter )
    {
        ( *demandSectorIter )->completeInit( mName, mRegionInfo );
    }

    for( ConsumerIterator consumerIter = mConsumers.begin(); consumerIter != mConsumers.end();
         ++consumerIter )
    {
        ( *consumerIter )->completeInit( mName, "", "" );
    }
    
    // Set the CO2 coefficients into the Marketplace before the Technologies and
    // GHGs are initialized so they can be accessed.  Note that these are set during
    // completeInit so that we can be sure that they will always be available
    // during initCalc when they will be retrieved.
    for( int period = 0; period < scenario->getModeltime()->getmaxper(); ++period ) {
        setCO2CoefsIntoMarketplace( period );
    }
    
    // Wrap objects which are used to calculate the region so that they can
    // be sorted into a global ordering and called directly from the world.
    // Note that the region continues to own and manage these object.  The memory
    // for the wrappers is managed by the market dependency finder.
    MarketDependencyFinder* markDepFinder = scenario->getMarketplace()->getDependencyFinder();
    for( int i = 0; i < mResources.size(); ++i ) {
        markDepFinder->resolveActivityToDependency( mName, mResources[ i ]->getName(),
            new ResourceActivity( mResources[ i ], mGDP, mName ) );
    }
    for( int i = 0; i < mSupplySector.size(); ++i ) {
        SectorActivity* tempSectorActivity(
            new SectorActivity( mSupplySector[ i ], mGDP, mName ) );
        markDepFinder->resolveActivityToDependency( mName, mSupplySector[ i ]->getName(),
            tempSectorActivity->getSectorDemandActivity(),
            tempSectorActivity->getSectorPriceActivity() );
    }
    for( int i = 0; i < mFinalDemands.size(); ++i ) {
        markDepFinder->resolveActivityToDependency( mName, mFinalDemands[ i ]->getName(),
            new FinalDemandActivity( mFinalDemands[ i ], mGDP, mDemographic, mName ) );
    }
    if( mLandAllocator ) {
        markDepFinder->resolveActivityToDependency( mName, "land-allocator",
            new LandAllocatorActivity( mLandAllocator, mName ) );
    }
    for( int i = 0; i < mConsumers.size(); ++i ) {
        markDepFinder->resolveActivityToDependency( mName, mConsumers[ i ]->getName(),
            new ConsumerActivity( mConsumers[ i ], mDemographic, mName ) );
    }
}

void RegionMiniCAM::toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {
    // write out basic datamembers
    XMLWriteElement( mInterestRate, "interest-rate", out, tabs );
    XMLWriteElement( mSocialDiscountRate, "social-discount-rate", out, tabs );
    XMLWriteElement( mPrivateDiscountRateLand, "private-discount-rate-land", out, tabs );

    XMLWriteElement( mCalibrationGDPs[ period ], "calibrationGDPs", out, tabs );
    XMLWriteElement( getEndUseServicePrice( period ), "priceSer", out, tabs );
    
    // Write out the Co2 Coefficients.
    for( map<string,double>::const_iterator coefAllIter = mPrimaryFuelCO2Coef.begin(); coefAllIter != mPrimaryFuelCO2Coef.end(); coefAllIter++ ) {
        XMLWriteElement( coefAllIter->second, "PrimaryFuelCO2Coef", out, tabs, 0, coefAllIter->first );
    }

    // write the xml for the class members.
    // write out the single population object.
    if( mGDP ){
        mGDP->toDebugXML( period, out, tabs );
    }

    // Write out the land allocator.
    if ( mLandAllocator ) {
        mLandAllocator->toDebugXML( period, out, tabs );
    }

    // write out demand sector objects.
    for( CFinalDemandIterator currSector = mFinalDemands.begin(); currSector != mFinalDemands.end(); ++currSector ){
        (*currSector)->toDebugXML( period, out, tabs );
    }

    // write out consumer objects.
    for( CConsumerIterator consumerIter = mConsumers.begin(); consumerIter != mConsumers.end(); consumerIter++ ){
        ( *consumerIter )->toDebugXML( period, out, tabs );
    }
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. This function may be virtual to be overridden by derived class
* pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& RegionMiniCAM::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& RegionMiniCAM::getXMLNameStatic() {
    // TODO: Change to region-minicam, this will require changing input files
    static const string XML_NAME = "region";
    return XML_NAME;
}

/*! Calculate initial gdp value (without feedbacks)
*
* \param period Model time period
*/
void RegionMiniCAM::calcGDP( const int period ){
    if( !ensureGDP() || !ensureDemographics() ){
        return;
    }
    mGDP->initialGDPcalc( period, mDemographic->getTotal( period ) );
}

/*! \brief Calculate forward-looking gdp (without feedbacks) for AgLU use.
* \details It is necessary to have a gdp without feedbacks so that all values
*          are known and AgLU can calibrate. This routine runs through each
*          period and calculates a series of gdp values without use of the
*          energy price feedback.
* \author Steve Smith, Josh Lurz
* \warning This will interfere with the normal gdp calculation if this is used
*          after model calc starts.
* \todo check to see if this works with AgLU. Not sure about conversions.
*/
const vector<double> RegionMiniCAM::calcFutureGDP() const {
    if( !ensureGDP() || !ensureDemographics() ){
        return vector<double>( 0 );
    }

    const Modeltime* modeltime = scenario->getModeltime();
    vector<double> gdps( modeltime->getmaxper() );

    for ( int period = 0; period < modeltime->getmaxper(); period++ ) {
        mGDP->initialGDPcalc( period, mDemographic->getTotal( period ) );
        gdps[ period ] = mGDP->getApproxScaledGDPperCap( period );
    }
    return gdps;
}

double RegionMiniCAM::getEndUseServicePrice( const int period ) const {
    double servicePrice = 0;
    for ( CFinalDemandIterator currDemSector = mFinalDemands.begin(); currDemSector != mFinalDemands.end(); ++currDemSector ) {
        servicePrice += (*currDemSector)->getWeightedEnergyPrice( mName, period );
    }

    return servicePrice;
}

/*! Adjust regional gdp for energy.
*
* \param period Model time period
* \todo Move this calculation down to GDP
*/
void RegionMiniCAM::adjustGDP( const int period ){
    if( !ensureGDP() ){
        return;
    }

    const Modeltime* modeltime = scenario->getModeltime();

    double tempratio = 1;
    if ( period > modeltime->getFinalCalibrationPeriod() ){
        tempratio = getEndUseServicePrice( period ) / getEndUseServicePrice( period - 1 );
    }
    mGDP->adjustGDP( period, tempratio );
}

/*! \brief Test to see if calibration worked for all sectors in this region
*
* Compares the sum of calibrated + fixed values to output of each sector.
*
* If calibrations are not on then will only printout out diagnostics (if
*
* \author Steve Smith
* \param period Model period
* \param calAccuracy value to which calibrations must match in order to pass
*        calibration test.
* \param printWarnings flag to turn on logging of warnings if calibrations are
*        not accurate.
* \return Boolean true if calibration is ok.
*/
bool RegionMiniCAM::isAllCalibrated( const int period, double calAccuracy, const bool printWarnings ) const {
    const static bool calOn = Configuration::getInstance()->getBool( "CalibrationActive" );
    
    // Don't check calibration in the base period or if calibration is off.
    if( !calOn || period == 0 ){
        return true;
    }

    bool returnVal = true;
    for ( unsigned int i = 0; i < mSupplySector.size(); i++ ) {
        if ( !mSupplySector[ i ]->isAllCalibrated( period, calAccuracy, printWarnings ) ) {
            returnVal = false;
        }
    }

    // always return true if calibrations are not on.
    return returnVal;
}

/*! \brief Call any initializations that are only done once per period.
* \author Steve Smith, Josh Lurz, Sonny Kim
* \param period Model period
* \todo Once postCalc is present, add a check to verify that aggregate emissions worked properly
*/
void RegionMiniCAM::initCalc( const int period )
{
    Region::initCalc( period );
    for( SectorIterator currSector = mSupplySector.begin(); currSector != mSupplySector.end(); ++currSector ){
        (*currSector)->initCalc( 0, mDemographic, period );
    }
    for ( FinalDemandIterator currSector = mFinalDemands.begin(); currSector != mFinalDemands.end(); ++currSector ) {
        (*currSector)->initCalc( mName, mGDP, mDemographic, period  );
    }
    for( ResourceIterator currResource = mResources.begin(); currResource != mResources.end(); ++currResource ){
        (*currResource)->initCalc( mName, period );
    }

    calcGDP( period );
    mGDP->adjustGDP( period, 1.0 );
    
    for( ConsumerIterator currConsumer = mConsumers.begin(); currConsumer != mConsumers.end(); ++currConsumer ) {
        NationalAccount nationalAccount;
        // Note that we are using the unadjusted gdp for these equations and so
        // gdp price feedbacks will be ignored.
        (*currConsumer)->initCalc( mName, "", nationalAccount, mDemographic, mGDP, 0, period );
    }

    // Call initCalc for land allocator last. It needs profit from the ag sectors
    // before it can calculate share weights
    if ( mLandAllocator ) {
        mLandAllocator->initCalc( mName, period );
    }
}

/*
* \brief Initialize the CO2 coefficients read in by the Region into the
*        marketplace.
* \details In each period the Region must set the CO2 coefficients for all goods
*          into the Marketplace, so that Technologies and GHGs can access them.
* \param aPeriod Period.
*/
void RegionMiniCAM::setCO2CoefsIntoMarketplace( const int aPeriod ){
    const static string CO2COEF = "CO2coefficient";
    Marketplace* marketplace = scenario->getMarketplace();
    for( map<string, double>::const_iterator coef = mPrimaryFuelCO2Coef.begin();
        coef != mPrimaryFuelCO2Coef.end(); ++coef )
    {
        // Markets may not exist for incorrect fuel names.
        IInfo* fuelInfo = marketplace->getMarketInfo( coef->first, mName, aPeriod, false );
        if( fuelInfo ){
            fuelInfo->setDouble( CO2COEF, coef->second );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::DEBUG );
            mainLog << "Cannot set emissions factor of zero for fuel " << coef->first
                    << " for region " << mName
                    << " because the name does not match the market name or market does not exist." << endl;
        }
    }
}


/*!
 * \brief Call any calculations that are only done once per period after
 *        solution is found.
 * \author Sonny Kim
 * \param aPeriod Model period
 */
void RegionMiniCAM::postCalc( const int aPeriod ) {
    Region::postCalc( aPeriod );

    for( CConsumerIterator consumerIter = mConsumers.begin(); consumerIter != mConsumers.end(); ++consumerIter ) {
        (*consumerIter)->postCalc( mName, "", aPeriod );
    }
    
    if( mLandAllocator ) {
        mLandAllocator->postCalc( mName, aPeriod );
    }
}

/*! \brief Check whether the GDP object exists and report a warning if it does not.
* \return Whether the GDP object exists.
* \author Josh Lurz
*/
bool RegionMiniCAM::ensureGDP() const {
    if( !mGDP ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "GDP object has not been created and is required. "
            << "Check for a region name mismatch." << endl;
        return false;
    }
    return true;
}

/*! \brief Check whether the Demographics object exists and report a warning if it does not.
* \return Whether the Demographics object exists.
* \author Josh Lurz
*/
bool RegionMiniCAM::ensureDemographics() const {
    if( !mDemographic ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Population object has not been created and is required. "
            << "Check for a region name mismatch." << endl;
        return false;
    }
    return true;
}

/*! \brief Update a visitor for a Region.
* \param aVisitor Visitor to update.
* \param aPeriod Period to update.
*/
void RegionMiniCAM::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitRegionMiniCAM( this, aPeriod );
    Region::accept( aVisitor, aPeriod );

    // Visit GDP object
    if ( mGDP ){
        mGDP->accept( aVisitor, aPeriod );
    }

    // Visit LandAllocator object
    if ( mLandAllocator ){
        mLandAllocator->accept( aVisitor, aPeriod );
    }

    // loop for final demand sectors.
    for( CFinalDemandIterator currDem = mFinalDemands.begin(); currDem != mFinalDemands.end(); ++currDem ){
        (*currDem)->accept( aVisitor, aPeriod );
    }

    // Visit Consumers
    for( CConsumerIterator consumerIter = mConsumers.begin(); consumerIter != mConsumers.end(); ++consumerIter ) {
        (*consumerIter)->accept( aVisitor, aPeriod );
    }
    
    aVisitor->endVisitRegionMiniCAM( this, aPeriod );
}
