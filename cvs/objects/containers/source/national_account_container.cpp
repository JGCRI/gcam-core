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
* \file national_account_container.cpp
* \ingroup Objects
* \brief The National Account Container class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <cassert>

#include "containers/include/national_account_container.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"
#include "functions/include/nested_ces_production_function_macro.h"
#include "demographics/include/demographic.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "containers/include/national_account.h"
#include "util/base/include/model_time.h"
#include "util/base/include/configuration.h"
#include "containers/include/market_dependency_finder.h"
#include "sectors/include/sector_utils.h"
#include "functions/include/function_utils.h"

using namespace std;

extern Scenario* scenario;

//! Default Constructor
NationalAccountContainer::NationalAccountContainer():
mGDPMrkName("GDP_Trial"),
mGDPActName("GDP_Act"),
mEnergyNetExportMrkName("energy net export"),
mEnergyServiceMrkName("energy service")
{
    // TODO: create production function factory
    // use CES for now
    mGdpMacroFunction = 0;
}

NationalAccountContainer::~NationalAccountContainer() {
    delete mGdpMacroFunction;
    for(auto nationalAccount : mNationalAccounts) {
        delete nationalAccount;
    }
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
const std::string& NationalAccountContainer::getXMLNameStatic() {
    const static string XML_NAME = "nationalAccountContainer";
    return XML_NAME;
}

const string& NationalAccountContainer::getGDPMarketName() const {
    return mGDPMrkName;
}

const string& NationalAccountContainer::getGDPActivityName() const {
    return mGDPActName;
}

//! Output debug info to XML
void NationalAccountContainer::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    mNationalAccounts[period]->toDebugXML(period, out, tabs);
    
}

/*!
 * \brief Complete initializations for the national account
 */
void NationalAccountContainer::completeInit( const string& aRegionName, const Demographic* aDemographics ) {
    
    const Modeltime* modeltime = scenario->getModeltime();
    mRegionName = aRegionName;

    // Set population, labor force and gdp per capita into national accounts from the demographics object
    // This is done here in nationalAccountsContainer rather than in each period national accounts to
    // consolidate population related procedure. This could be done in complete init of national account, however.
    for( int period = 0; period < modeltime->getmaxper(); ++period ) {
        mNationalAccounts[period]->completeInit();
        double popTot = aDemographics->getTotal(period);
        mNationalAccounts[period]->setAccount(NationalAccount::POPULATION, popTot );
        // labor force share must have been read-in
        double laborForce =  popTot * mNationalAccounts[period]->getAccountValue(NationalAccount::LABOR_FORCE_SHARE);
        mNationalAccounts[period]->setAccount(NationalAccount::LABOR_FORCE, laborForce );
        // GDP must have been read-in by national accounts
        // This is inital GDP per Capita. If GDP is solved for, GDP per capita will update accordingly.
        double GDPperCapita =  mNationalAccounts[period]->getAccountValue(NationalAccount::GDP) / popTot ;
        mNationalAccounts[period]->setAccount(NationalAccount::GDP_PER_CAPITA, GDPperCapita );
    }
    
    // complete initializations
    if( !Configuration::getInstance()->getBool( "FixedGDP-Path" ) && !mGdpMacroFunction ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "No GDP macro function parameters set and not running Fixed GDP." << endl;
        abort();
    }
    else if( mGdpMacroFunction ) {
        for( int period = 0; period <= modeltime->getFinalCalibrationPeriod(); ++period ) {
            mNationalAccounts[period]->completeInitHist();
        
            // complete initializations for the gdp macro function
            if( period == modeltime->getFinalCalibrationPeriod() ){
                mGdpMacroFunction->completeInit( mRegionName, mGDPActName );
            }
        }
    }
    
    // Set up GDP operation modes: normal opertion or fixed-path.
    setGDPTrialMarket();
}

//! Create GDP trial market for endogenous GDP response
void NationalAccountContainer::setGDPTrialMarket( ) {
    
    const Modeltime* modeltime = scenario->getModeltime();
    Marketplace* marketplace = scenario->getMarketplace();
    
    const bool isFixedGDP = Configuration::getInstance()->getBool("FixedGDP-Path");
    if ( marketplace->createMarket( mRegionName, mRegionName, mGDPMrkName, isFixedGDP ? IMarketType::CALIBRATION : IMarketType::TRIAL_VALUE ) ) {
        // Set price and output units for period 0 market info
        IInfo* marketInfo = marketplace->getMarketInfo( mGDPMrkName, mRegionName, 0, true );
        marketInfo->setString( "price-unit", "million90$" );
        marketInfo->setString( "output-unit", "million90$" );
        
        objects::PeriodVector<Value> trialGDPs;
        for( int per = 0; per < modeltime->getmaxper(); ++per ){
            trialGDPs[per] = mNationalAccounts[per]->getAccountValue(NationalAccount::GDP);
        }
        marketplace->setPriceVector( mGDPMrkName, mRegionName, trialGDPs );
    }
    
    // Solve GDP market for future periods only
    for( int per = 0; per < modeltime->getmaxper(); ++per ){
        bool isUnsolved = isFixedGDP || per <= modeltime->getFinalCalibrationPeriod();
        if (isUnsolved) {
            // turn off market to solve for historical periods
            marketplace->unsetMarketToSolve( mGDPMrkName, mRegionName, per );
        }
        else {
            // turn on market to solve for future periods
            marketplace->setMarketToSolve( mGDPMrkName, mRegionName, per );
        }
    }
    
    // set market dependencies
    MarketDependencyFinder* depFinder = marketplace->getDependencyFinder();
    depFinder->addDependency( mGDPActName, mRegionName, mGDPMrkName, mRegionName );
    if( mGdpMacroFunction ) {
        depFinder->addDependency( mGDPActName, mRegionName, mEnergyNetExportMrkName, mRegionName );
    }
    // note: given we are just taking capital investments out of savings during initCalc
    // we do really need to log a dependency here, however to avoid warnings we do
    depFinder->addDependency( mGDPActName, mRegionName, "capital", mRegionName);
    depFinder->addDependency( mGDPActName, mRegionName, "consumer durable", mRegionName);
}

/*!
 * \brief Complete initializations for the national account
 */
void NationalAccountContainer::initCalc( const Demographic* aDemographics, const int aPeriod ) {
    if( mGdpMacroFunction ) {
        if(aPeriod > 0){
            // national accounts for base (historical) period should be read in
            mNationalAccounts[aPeriod]->initCalc( mNationalAccounts[aPeriod-1], aPeriod );
        }
        else if( aPeriod == 0 ){
            // passing in same current period national account since previous period not available
            mNationalAccounts[aPeriod]->initCalc( mNationalAccounts[aPeriod], aPeriod );
        }
        
        mGdpMacroFunction->initCalc( mRegionName, mNationalAccounts[aPeriod], aPeriod );
        const bool isFixedGDP = Configuration::getInstance()->getBool("FixedGDP-Path");
        const Modeltime* modeltime = scenario->getModeltime();
        double currTFP = isFixedGDP || aPeriod <= modeltime->getFinalCalibrationPeriod() ? 1.0 : mNationalAccounts[aPeriod]->getAccountValue(NationalAccount::TOTAL_FACTOR_PRODUCTIVITY);
        mGdpMacroFunction->setTotalFactorProductivity(currTFP);
    }
    
    // Set trial GDP bounds which gives a hint to the solver as to valid range of
    // values, although not explicilty enforced
    // We do not expect the GDP to change by more than +/- 50%
    const double GDP_BOUNDS = 0.5;
    double currGDPEstimate = mNationalAccounts[aPeriod]->getAccountValue(NationalAccount::GDP);
    SectorUtils::setSupplyBehaviorBounds(mGDPMrkName, mRegionName, currGDPEstimate * GDP_BOUNDS,
                                         currGDPEstimate*(1.0+GDP_BOUNDS), aPeriod);
}

/*!
 * \brief Calculate gross output
 */
void NationalAccountContainer::calcGDP( const int aPeriod ) {
    const static bool isFixedGDP = Configuration::getInstance()->getBool("FixedGDP-Path");
    Marketplace* marketplace = scenario->getMarketplace();
    const int basePeriod = scenario->getModeltime()->getFinalCalibrationPeriod();
    if(mGdpMacroFunction && aPeriod > basePeriod) {
        double currGrossOutput = mGdpMacroFunction->calcGrossOutput( mRegionName, mNationalAccounts[aPeriod], aPeriod, false );
        
        //Calculate net energy export rents to add to GDP. Convert million 1990 dollars.
        double currEnergyExportValue = -scenario->getMarketplace()->getPrice( mEnergyNetExportMrkName, mRegionName, aPeriod ) *
            1000 * FunctionUtils::DEFLATOR_1990_PER_DEFLATOR_1975();

        // Total GDP includes sources of income from outside of region. Given the structure of our
        // social accounting matrix, the easiest way to adjust gross output accordingly is to
        // add on energy net exports
        double currGDP_total = currGrossOutput + currEnergyExportValue;

        // add the calculated GDP to the trial market (if not running in fixed mode)
        // note, even in fixed mode we want to keep track of this value because we can
        // use it to calibrate a total factor productivity
        mCurrGDP = currGDP_total;
        if(!isFixedGDP) {
            marketplace->addToDemand( mGDPMrkName, mRegionName, mCurrGDP, aPeriod, true );
        }
    }
    else if(!isFixedGDP) {
        mCurrGDP = mNationalAccounts[aPeriod]->getAccountValue(NationalAccount::GDP);
        marketplace->addToDemand( mGDPMrkName, mRegionName, mCurrGDP, aPeriod, true );
    }
    // given the GDP may be dynamic and the negative emissions budget is calculated as a
    // percent of total GDP we need to calculate what that dollar value amounts to and set
    // the value as the supply
    if(!mNegEmissBudgetName.empty()) {
        Marketplace* marketplace = scenario->getMarketplace();
        double currGDP = getMarketGDP(aPeriod);
        mNegEmissBudgetSupply = mNegEmissBudgetFraction * currGDP;
        marketplace->addToSupply( mNegEmissBudgetName, mRegionName, mNegEmissBudgetSupply, aPeriod );
    }
}

/*!
 * \brief Return current Market GDP.
 */
double NationalAccountContainer::getMarketGDP( const int aPeriod ) const{
    // Given the units of our GDP a value of 1000 is still way smaller than anything
    // would would reasonably expect.  To avoid bad numerics we will cap the market GDPs
    // at this floor.
    const double MIN_GDP = 1000;
    double marketGDP = std::max(scenario->getMarketplace()->getPrice( mGDPMrkName, mRegionName, aPeriod, true ),
                                MIN_GDP);
    
    // Note: the solver will never directly generate an invalid (i.e. NaN) price
    
    return marketGDP;
}

/*!
 * \brief Return current market GDP/capita.
 */
double NationalAccountContainer::getMarketGDPperCapita( const int aPeriod ) const{
    
    double marketGDP = getMarketGDP(aPeriod);
    
    assert(mNationalAccounts[aPeriod]->getAccountValue(NationalAccount::POPULATION) > 0);
    
    double GDPperCapita = marketGDP / mNationalAccounts[aPeriod]->getAccountValue(NationalAccount::POPULATION);
    
    return GDPperCapita;
}

/*!
 * \brief Return current market GDP/capita normalized to based year value.
 */
double NationalAccountContainer::getMarketGDPperCapitaNorm( const int aPeriod ) const{
    
    const Modeltime* modeltime = scenario->getModeltime();
    int basePer = modeltime->getBasePeriod();
    
    double GDPperCapita = getMarketGDPperCapita(aPeriod);
    
    double GDPperCapitaBase = mNationalAccounts[basePer]->getAccountValue(NationalAccount::GDP)
                            / mNationalAccounts[basePer]->getAccountValue(NationalAccount::POPULATION);
    
    double GDPperCapitaNorm = GDPperCapita/GDPperCapitaBase;

    return GDPperCapitaNorm;
}

double NationalAccountContainer::getGDPPPP( const int aPeriod ) const {
    return getMarketGDP( aPeriod ) * mPPPConversion;
}

/*!
 * \brief Return total regional population
 */
double NationalAccountContainer::getPop( const int aPeriod ) const{
    
    return mNationalAccounts[aPeriod]->getAccountValue(NationalAccount::POPULATION);
}

/*!
 * \brief Final update of solved national accounts information
 */
void NationalAccountContainer::postCalc( const int aPeriod ) {
    if ( mGdpMacroFunction ) {
        double netExportValue = -scenario->getMarketplace()->getPrice(mEnergyNetExportMrkName, mRegionName, aPeriod);
        double energyService = scenario->getMarketplace()->getPrice(mEnergyServiceMrkName, mRegionName, aPeriod);
        // The energy investment pulled up from GCAM will actually be the total investment over the entire
        // timestep.  We need to annualize it here for which we assume even investment over the timestep
        const double annualizationFactor = scenario->getModeltime()->gettimestep(aPeriod);
        double energyInvestment = scenario->getMarketplace()->getPrice("capital", mRegionName, aPeriod) * 1000 * FunctionUtils::DEFLATOR_1990_PER_DEFLATOR_1975() / annualizationFactor;
        double consumerDurableInv = scenario->getMarketplace()->getPrice("consumer durable", mRegionName, aPeriod) * 1000 * FunctionUtils::DEFLATOR_1990_PER_DEFLATOR_1975() / annualizationFactor;
        double gdpPerCapPPP = getMarketGDPperCapita(aPeriod) * mPPPConversion;
        mNationalAccounts[aPeriod]->setAccount(NationalAccount::GDP_PER_CAPITA_PPP, gdpPerCapPPP);
        
        netExportValue *= 1000 * FunctionUtils::DEFLATOR_1990_PER_DEFLATOR_1975();
        energyService *= 1000 * FunctionUtils::DEFLATOR_1990_PER_DEFLATOR_1975();
        double materialsNetExp = -mNationalAccounts[aPeriod]->getAccountValue(NationalAccount::CAPITAL_NET_EXPORT) - netExportValue;
        mNationalAccounts[aPeriod]->setAccount(NationalAccount::MATERIALS_NET_EXPORT, materialsNetExp);
        mNationalAccounts[aPeriod]->setAccount(NationalAccount::ENERGY_NET_EXPORT, netExportValue);
        mNationalAccounts[aPeriod]->setAccount(NationalAccount::ENERGY_SERVICE, energyService);
        mNationalAccounts[aPeriod]->setAccount(NationalAccount::CONSUMER_DURABLE_INV, consumerDurableInv);
        
        if( aPeriod > scenario->getModeltime()->getFinalCalibrationPeriod() ) {
            // save energy investment value from market into the national accounts
            // avoid setting in the historical years as that is from data
            mNationalAccounts[aPeriod]->setAccount(NationalAccount::CAPITAL_ENERGY_INV, energyInvestment);

            // use regression coefficients to calculate a savings rate for the next model period
            double prevSR = mNationalAccounts[aPeriod-1]->getAccountValue(NationalAccount::SAVINGS_RATE);
            double gdppcGR2 = getMarketGDPperCapita(aPeriod) / getMarketGDPperCapita(aPeriod-1) - 1.0;
            double predictSR = mSavingsRParamBase + mSavingsRParamGR * gdppcGR2 + mSavingsRParamSR * prevSR;
            // note: next period will use this savings rate for its savings / investment calculation
            mNationalAccounts[aPeriod]->setAccount(NationalAccount::SAVINGS_RATE, predictSR);
        }
        
        // Additional calculations for history.
        if( aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod() ) {
            // Note
            mNationalAccounts[aPeriod]->postCalcHist();
        }
        else {
            const bool isFixedGDP = Configuration::getInstance()->getBool("FixedGDP-Path");
            if(isFixedGDP) {
                // if running in fixed mode we can easily back calculate a total factor productivity
                // that would allow us to match the calculated GDP to the exogenously provided one
                double calGrossOutput = mNationalAccounts[aPeriod]->getAccountValue(NationalAccount::GDP) - netExportValue;
                double actualGrossOutput = mCurrGDP - netExportValue;
                double totalFactorProd = calGrossOutput / actualGrossOutput;
                // set this TFP so it can be used in the final call to the materials function
                mGdpMacroFunction->setTotalFactorProductivity(totalFactorProd);
                // save the TFP in the national accounts so it can be reported and potentially
                // read back in to use in open GDP mode
                mNationalAccounts[aPeriod]->setAccount(NationalAccount::TOTAL_FACTOR_PRODUCTIVITY, totalFactorProd);
            }
            else {
                mNationalAccounts[aPeriod]->setAccount(NationalAccount::GDP, mCurrGDP);
            }
        }
        
        // Do a final call to calculate the materials function
        // NOTE:
        // 1. During historical periods this is when the materials function's coefficients
        //    will be calibrated.
        // 2. The calibrated TFP will now be reflected (when running in fixed GDP mode).
        // 3. We set the flag to save detailed results to the national accounts to true so
        //    that the values will be available for reporting.
        mGdpMacroFunction->calcGrossOutput( mRegionName, mNationalAccounts[aPeriod], aPeriod, true );
        
        mGdpMacroFunction->postCalc( mRegionName, mNationalAccounts[aPeriod], aPeriod );
        mNationalAccounts[aPeriod]->postCalc();
    }
}


// for reporting National Account information
void NationalAccountContainer::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitNationalAccountContainer( this, aPeriod );
    
    for( unsigned int i=0; i < mNationalAccounts.size(); ++i ){
        mNationalAccounts[i]->accept( aVisitor, aPeriod );
    }
    // currently not utilized, but may be needed for reporting macro function parameters
    //mGdpMacroFunction->accept(aVisitor, aPeriod);
    
    aVisitor->endVisitNationalAccountContainer( this, aPeriod );

  }
