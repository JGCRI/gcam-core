
/*!
* \file govt_consumer.cpp
* \ingroup Objects
* \brief The GovtConsumer class source file.
*
* \author Sonny Kim
* \author Katherine Chung
*/
#include "util/base/include/definitions.h"
#include <cmath>
#include <xercesc/dom/DOMNode.hpp>

#include "consumers/include/govt_consumer.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/national_account.h"
#include "technologies/include/expenditure.h"
#include "functions/include/input.h"
#include "functions/include/ifunction.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "functions/include/function_manager.h"
#include "demographics/include/demographic.h"
#include "util/base/include/ivisitor.h"
#include "functions/include/function_utils.h"
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//!< Default Constructor
GovtConsumer::GovtConsumer() {
}

GovtConsumer* GovtConsumer::clone() const {
    return new GovtConsumer( *this );
}

/*! \brief Used to merge an existing consumer with the coefficients from the previous periods
 *
 * \author Sonny Kim
 * \warning Does not copy everything, only non calculated values to pass on to the next period
 */
void GovtConsumer::copyParam( const BaseTechnology* baseTech,
                              const int aPeriod ) {
    BaseTechnology::copyParam( baseTech, aPeriod );
    baseTech->copyParamsInto( *this );
}


/*! \brief Merges consumers, this is a trick to get c++ to let us use the BaseTech pointer as a consumer without casting
 *
 * \author Sonny Kim
 */
void GovtConsumer::copyParamsInto( GovtConsumer& aGovtConsumer ) const {
    aGovtConsumer.mBaseTransferPopCoef.init( mBaseTransferPopCoef );
    aGovtConsumer.mBaseDeficit.init( mBaseDeficit );
    aGovtConsumer.mBaseTransfer.init( mBaseTransfer );
    aGovtConsumer.mTaxProportional.init( mTaxProportional );
    aGovtConsumer.mTaxAdditive.init( mTaxAdditive );
    aGovtConsumer.mRho.init( mRho );
}

//! Parse xml file for data
bool GovtConsumer::XMLDerivedClassParse( const string &nodeName, const DOMNode* curr ) {
    if ( nodeName == "deficit" ) {
        mBaseDeficit.init( XMLHelper<double>::getValue( curr ) );
    }
    else if ( nodeName == "rho" ){
        mRho.init( XMLHelper<double>::getValue( curr ) );
    }
    // base year transfer to household
    else if ( nodeName == "baseTransfer" ){
        mBaseTransfer.init( XMLHelper<double>::getValue( curr ) );
    }
    else {
        return false;
    }
    return true;
}

//! For derived classes to output XML data
void GovtConsumer::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    XMLWriteElement( mBaseDeficit, "deficit", out, tabs );
}

//! Output debug info for derived class
void GovtConsumer::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteElement( mBaseDeficit, "deficit", out, tabs );
    XMLWriteElement( mTaxProportional, "taxProportional", out, tabs );
    XMLWriteElement( mTaxAdditive, "taxAdditive", out, tabs );
    XMLWriteElement( mBaseTransferPopCoef, "baseTransferPopCoef", out, tabs );
}

//! Complete the initializations.
void GovtConsumer::completeInit( const string& aRegionName ) {
    prodDmdFnType = "GovtDemandFn";
    BaseTechnology::completeInit( aRegionName );
    assert( mRho != 1 );
    mSigma.init( 1 / ( 1 - mRho ) );

    const Modeltime* modeltime = scenario->getModeltime();

    // Only the base year government consumer should setup the markets.
    if( modeltime->getyr_to_per( year ) == modeltime->getBasePeriod() ){

        // Setup a trial value market for total household taxes so that the
        // ordering of the government and household consumer does not matter.
        // This will always be a regional market.
        Marketplace* marketplace = scenario->getMarketplace();
        const static string HOUSEHOLD_TAX_MARKET_NAME = "household-taxes";
        if( !marketplace->createMarket( aRegionName, aRegionName, HOUSEHOLD_TAX_MARKET_NAME,
                                        IMarketType::TRIAL_VALUE ) )
        {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Household taxes trial value market already existed." << endl;
        }

        // Set the market to solve. It may have already been set to solve,
        // but that will not cause an error. Note that they are being solved in
        // the base period.
        for( int period = 0; period < scenario->getModeltime()->getmaxper(); ++period ){
            marketplace->setMarketToSolve( HOUSEHOLD_TAX_MARKET_NAME, aRegionName, period );
        }
    }
}

//! initialize anything that won't change during the calcuation
void GovtConsumer::initCalc( const MoreSectorInfo* aMoreSectorInfo, const string& aRegionName,
                             const string& aSectorName, NationalAccount& nationalAccount,
                             const Demographic* aDemographics, const double aCapitalStock, const int aPeriod )
{
    Consumer::initCalc( aMoreSectorInfo, aRegionName, aSectorName,
                        nationalAccount, aDemographics, aCapitalStock,
                        aPeriod );

    if ( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
        // calculate Price Paid
        BaseTechnology::calcPricePaid(aMoreSectorInfo, aRegionName, aSectorName, aPeriod);
        if( aPeriod == 0 ){
            calcBaseCoef( nationalAccount, aDemographics );
            // call income calculation once in the base year to calculate consumption
            calcIncome( nationalAccount, aDemographics, aRegionName, aPeriod );
            prodDmdFn->calcCoefficient(input, expenditures[ aPeriod ].getValue( Expenditure::CONSUMPTION ), aRegionName, aSectorName, aPeriod, mSigma );
        }
        calcTransfer( nationalAccount, aDemographics, aRegionName, aPeriod );
        // Apply technical change to input coefficients.
        prodDmdFn->applyTechnicalChange( input, TechChange(), aRegionName, aSectorName, aPeriod, 0, 0 );
    }
}

//! calculate subsidy
void GovtConsumer::calcSubsidy( NationalAccount& nationalAccount, const string& regionName, int period ) {
    double subsidy = nationalAccount.getAccountValue( NationalAccount::SUBSIDY );
    expenditures[ period ].setType( Expenditure::SUBSIDY, subsidy );
}

//! calculate deficit
void GovtConsumer::calcDeficit( const string& aRegionName, int aPeriod ) {
    // Note: This is incorrect, the numeraire should be able to be any good. Multiplying by
    // the numeraire price at all here may not be correct however.
    const Input* numInput = FunctionUtils::getNumeraireInput( input );
    assert( numInput );
    assert( numInput->getPricePaid( aPeriod ) > 0 );
    double deficit = mBaseDeficit * numInput->getPricePaid( aPeriod );
    expenditures[ aPeriod ].setType( Expenditure::SAVINGS, (-1) * deficit ); // savings is negative of deficit
    assert( util::isValidNumber( deficit ) );
    // add government deficit (savings) to the demand for capital
    Input* capInput = FunctionUtils::getCapitalInput( input );
    assert( capInput );
    // set demand and add to marketplace demand for capital
    capInput->setDemandCurrency( deficit, aRegionName, name, aPeriod  );
}

/*! \brief Calculate total taxes.
* \param aNationalAccount The container of national accouting values.
* \param aRegionName The name of the region.
* \param aPeriod The period in which to calculate taxes.
*/
void GovtConsumer::calcTotalTax( NationalAccount& aNationalAccount, const string& aRegionName,
                                 const int aPeriod )
{
    // Use a trial value for the personal income tax and social security tax as
    // the household consumer may not have calculated them yet. This will be
    // done in the base period as well.
    const Marketplace* marketplace = scenario->getMarketplace();
    double trialHouseholdTaxes = marketplace->getPrice( "household-taxes", aRegionName, aPeriod, true );

    // Now calcualte the governments total taxes, or income.
    mTaxCorporate.set( aNationalAccount.getAccountValue( NationalAccount::CORPORATE_INCOME_TAXES ) );
    mTaxIBT.set( aNationalAccount.getAccountValue( NationalAccount::INDIRECT_BUSINESS_TAX ) );
    double totalTax = aNationalAccount.getAccountValue( NationalAccount::CORPORATE_INCOME_TAXES )
        + trialHouseholdTaxes
        + aNationalAccount.getAccountValue( NationalAccount::INDIRECT_BUSINESS_TAX );
        // carbon tax, txpro, txadd ...

    expenditures[ aPeriod ].setType( Expenditure::INCOME, totalTax );
}

//! calculate transfer
void GovtConsumer::calcTransfer( NationalAccount& nationalAccount, const Demographic* aDemographics,
                                const string& regionName, int period )
{   // Multiplying by the numeraire price at all here may not be correct
    // however.
    const Input* numInput = FunctionUtils::getNumeraireInput( input );
    assert( numInput );
    double transfer = mBaseTransferPopCoef * aDemographics->getTotal( period )
                      * numInput->getPrice( regionName, period );
    expenditures[ period ].setType( Expenditure::TRANSFERS, transfer );
    nationalAccount.setAccount(NationalAccount::TRANSFERS, transfer );
}

//! Calculate government income.
void GovtConsumer::calcIncome( NationalAccount& nationalAccount, const Demographic* aDemographics,
                              const string& regionName, int period )
{
    calcTotalTax( nationalAccount, regionName, period );
    calcTransfer( nationalAccount, aDemographics, regionName, period );
    calcDeficit( regionName, period );
    calcSubsidy( nationalAccount, regionName, period );

    double consumption = expenditures[ period ].getValue( Expenditure::INCOME )
                         - expenditures[ period ].getValue( Expenditure::SUBSIDY )
                         - expenditures[ period ].getValue( Expenditure::TRANSFERS )
                         - expenditures[ period ].getValue( Expenditure::SAVINGS );

    expenditures[ period ].setType( Expenditure::CONSUMPTION, consumption );
    // set National Accounts Consumption for GNP calculation
    nationalAccount.addToAccount( NationalAccount::GNP, consumption );
    nationalAccount.addToAccount( NationalAccount::GOVERNMENT, consumption );
}

//! calculate demand
void GovtConsumer::operate( NationalAccount& nationalAccount, const Demographic* aDemographics,
                           const MoreSectorInfo* aMoreSectorInfo, const string& aRegionName,
                           const string& aSectorName, const bool aIsNewVintageMode, int aPeriod )
{
    if( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ){
        expenditures[ aPeriod ].reset();
        // calculate prices paid for consumer inputs
        BaseTechnology::calcPricePaid( aMoreSectorInfo, aRegionName, aSectorName, aPeriod );
        // calcIncome is already run in the base period by initCalc
        // use setType in expenditure to override and ensure that marketplace supplies
        // and demands are nulled
        calcIncome(nationalAccount, aDemographics, aRegionName, aPeriod );
        // calculate consumption demands for each final good or service
        // Government consumers don't shutdown.
        const double SHUTDOWN_COEF = 1;
        assert( prodDmdFn );
        mOutputs[ aPeriod ] = prodDmdFn->calcDemand( input,
                                                     expenditures[ aPeriod ].getValue( Expenditure::CONSUMPTION ),
                                                     aRegionName, aSectorName, SHUTDOWN_COEF,
                                                     aPeriod, 0, 0, mSigma, 0 );
        calcGovtTaxOrSubsidy( aRegionName, aPeriod );
        calcEmissions( aSectorName, aRegionName, aPeriod );
    }
}

//! calculate base coefficient
void GovtConsumer::calcBaseCoef( NationalAccount& nationalAccount, const Demographic* aDemographics ){
    mBaseTransferPopCoef.set( mBaseTransfer / aDemographics->getTotal( 0 ) );
}

//! calculate government capital demand
void GovtConsumer::calcGovtCapitalDemand( const std::string& regionName, int period ){
    const Input* capInput = FunctionUtils::getCapitalInput( input );
    assert( capInput );
    double tempCapital = capInput->getDemandCurrency( period );
    assert( tempCapital >= 0 );
    assert( util::isValidNumber( tempCapital ) );
    Marketplace* marketplace = scenario->getMarketplace();
    // Should this use the demand currency?
    marketplace->addToDemand( "Capital", regionName, tempCapital, period );
    // add capital to ETE, not done
}

//! calculate government tax or subsidy
/*! \todo This isn't called and doesn't work. */
void GovtConsumer::calcGovtTaxOrSubsidy( const string& regionName, int period ){
    double taxProAdd = 0;
    // need to read in transportationCost in the future!
    double transportationCost = 0;
    double eximport = 1;

    double taxGov = 0;
    double subsidyGov = 0;

    Marketplace* marketplace = scenario->getMarketplace();

    for(unsigned int i=0; i<input.size(); i++){

        // P - marketplace price
        double temp1 = ( marketplace->getPrice( input[i]->getName(), regionName, period )
            + transportationCost * eximport )
            * ( marketplace->getDemand( input[i]->getName(), regionName, period )
                - marketplace->getSupply( input[i]->getName(), regionName, period ) )
            * ( mTaxProportional - 1 );

        // This does not seem completely right -JPL
        if( mTaxProportional > 1 ){
            taxGov += temp1;
            //add to nationalaccounts
        }
        else if( mTaxProportional  < 1 ){
            subsidyGov -= temp1;
            // add to nationalaccounts
        }

        double temp2 = ( marketplace->getDemand( input[i]->getName(), regionName, period )
            - marketplace->getSupply( input[i]->getName(), regionName, period ) ) * mTaxAdditive;

        if( mTaxAdditive >= 0 ){
            taxGov += temp2;
        }
        else{
            subsidyGov -= temp2;
        }

    }

    // do something with taxGov and subsidyGov ...
}


//! calculate budget
void GovtConsumer::calcBudget() {
    // TODO: Figure out what is really supposed to happen here.
    double budget = 0; // ??????????????????????????????
    const Modeltime* modeltime = scenario->getModeltime();
    expenditures[ modeltime->getyr_to_per( year ) ].setType( Expenditure::BUDGET, budget );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& GovtConsumer::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& GovtConsumer::getXMLNameStatic() {
    const static string XML_NAME = "govtConsumer";
    return XML_NAME;
}

//! SGM version of outputing data to a csv file
void GovtConsumer::csvSGMOutputFile( ostream& aFile, const int period ) const {
    if ( year == scenario->getModeltime()->getper_to_yr( period ) ) {
        aFile << "***** Government Sector Results *****" << endl << endl;
        aFile << "Tax Accounts" << endl;
        aFile << "Proportional Tax" << ',' << mTaxProportional << endl;
        aFile << "Additive Tax" << ',' << mTaxAdditive << endl;
        aFile << "Corporate Income Tax" << ',' << mTaxCorporate << endl;
        aFile << "Indirect Business Tax" << ',' << mTaxIBT << endl;
        expenditures[ period ].csvSGMOutputFile( aFile, period );
        aFile << endl;
        BaseTechnology::csvSGMOutputFile( aFile, period );
    }
}

void GovtConsumer::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitGovtConsumer( this, aPeriod );
    Consumer::accept( aVisitor, aPeriod );
    aVisitor->endVisitGovtConsumer( this, aPeriod );
}
