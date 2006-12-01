/*
    This software, which is provided in confidence, was prepared by employees
    of Pacific Northwest National Laboratory operated by Battelle Memorial
    Institute. Battelle has certain unperfected rights in the software
    which should not be copied or otherwise disseminated outside your
    organization without the express written authorization from Battelle. All rights to
    the software are reserved by Battelle.  Battelle makes no warranty,
    express or implied, and assumes no liability or responsibility for the
    use of this software.
*/

/*!
* \file household_consumer.cpp
* \ingroup Objects
* \brief The HouseholdConsumer class source file.
* \author Pralit Patel
* \author Sonny Kim
*/
#include "util/base/include/definitions.h"
#include <iostream>
#include <cmath>
#include <xercesc/dom/DOMNode.hpp>

#include "consumers/include/household_consumer.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/national_account.h"
#include "technologies/include/expenditure.h"
#include "functions/include/input.h"
#include "functions/include/ifunction.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "demographics/include/demographic.h"
#include "util/base/include/model_time.h"
#include "util/base/include/ivisitor.h"
#include "functions/include/function_utils.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//!< Default Constructor
HouseholdConsumer::HouseholdConsumer() {
    // set all member variables to zero
    baseScalerLand = 0;
    baseScalerLaborMale = 0;
    baseScalerLaborFemale = 0;
    baseScalerSavings = 0;

    maxLandSupplyFrac = 0;
    maxLaborSupplyFracMale = 0;
    maxLaborSupplyFracFemale = 0;
    fixedLaborSupplyFrac = 0;
    maxSavingsSupplyFrac = 0;

    baseLandDemandPerHH = 0;
    baseLaborDemandPerHH = 0;
    landDemand = 0;
    laborDemand = 0;
    householdLandDemand = 0;
    householdLaborDemand = 0;

    socialSecurityTaxRate = 0;
    incomeTaxRate = 0;
    personsPerHousehold = 0;
    numberOfHouseholds = 0;
    totalLandArea = 0;

    baseTransfer = 0;
    transfer = 0;

    baseLandSupply = 0;
    baseLaborSupply = 0;

    landSupply = 0;
    laborSupplyMale = 0;
    laborSupplyFemale = 0;

    workingAgePopMale = -1;
    workingAgePopFemale = -1;

    mInitialSavings = 0;
}

/*! \brief Used to merge an existing householdConsumer with the coefficients from the previous periods
 *
 * \author Pralit Patel
 * \warning Does not copy everything, only non calculated values to pass on to the next period
 */
void HouseholdConsumer::copyParam( const BaseTechnology* baseTech,
                                   const int aPeriod ) {
    BaseTechnology::copyParam( baseTech, aPeriod );
    baseTech->copyParamsInto( *this );
}

/*! \brief Merges consumers, this is a trick to get c++ to let us use the BaseTech pointer as a householdConsumer without casting
 *
 * \author Pralit Patel
 * \warning Does not copy everything, only non calculated values to pass on to the next period
 */
void HouseholdConsumer::copyParamsInto( HouseholdConsumer& householdConsumerIn ) const {
    householdConsumerIn.baseScalerLand = baseScalerLand;
    householdConsumerIn.baseScalerLaborMale = baseScalerLaborMale;
    householdConsumerIn.baseScalerLaborFemale = baseScalerLaborFemale;
    householdConsumerIn.baseScalerSavings = baseScalerSavings;
    householdConsumerIn.maxSavingsSupplyFrac = maxSavingsSupplyFrac;
    householdConsumerIn.maxLaborSupplyFracMale = maxLaborSupplyFracMale;
    householdConsumerIn.maxLaborSupplyFracFemale = maxLaborSupplyFracFemale;
    // only override if fixed labor frac has not been read in
    if( householdConsumerIn.fixedLaborSupplyFrac == 0 ){
        householdConsumerIn.fixedLaborSupplyFrac = fixedLaborSupplyFrac;
    }

    householdConsumerIn.maxLandSupplyFrac = maxLandSupplyFrac;
    householdConsumerIn.socialSecurityTaxRate = socialSecurityTaxRate;
    householdConsumerIn.incomeTaxRate = incomeTaxRate;
    householdConsumerIn.personsPerHousehold = personsPerHousehold;
    householdConsumerIn.totalLandArea = totalLandArea;
    householdConsumerIn.mInitialSavings = mInitialSavings;
}

/*! \brief Creates a clone of this class
 *
 * \author Pralit Patel
 * \warning Does not copy everything, only non calculated values to pass on to the next period
 * \return Pointer to the new class created
 */
HouseholdConsumer* HouseholdConsumer::clone() const {
    return new HouseholdConsumer( *this );
}

//! Parse xml file for data
bool HouseholdConsumer::XMLDerivedClassParse( const string &nodeName, const DOMNode* curr ) {
    if ( nodeName == "baseLandDemandPerHH" ) {
        baseLandDemandPerHH = XMLHelper<double>::getValue( curr );
    }
    else if (nodeName == "baseLaborDemandPerHH" ) {
        baseLaborDemandPerHH = XMLHelper<double>::getValue( curr );
    }
    else if (nodeName == "socialSecurityTaxRate" ) {
        socialSecurityTaxRate = XMLHelper<double>::getValue( curr );
    }
    else if (nodeName == "personalIncomeTaxRate" ) {
        incomeTaxRate = XMLHelper<double>::getValue( curr );
    }
    else if (nodeName == "personsPerHousehold" ) {
        personsPerHousehold = XMLHelper<double>::getValue( curr );
    }
    else if (nodeName == "savings" ) {
        mInitialSavings = XMLHelper<double>::getValue( curr );
    }
    else if ( nodeName == "baseTransfer" ) {
        baseTransfer = XMLHelper<double>::getValue( curr );
    }
    else if (nodeName == "maxSavingsSupplyFrac") {
        maxSavingsSupplyFrac = XMLHelper<double>::getValue( curr );
    }
    else if ( nodeName == "maxLaborSupplyFrac" ) {
        if (XMLHelper<int>::getAttr( curr, "gender" ) == 1) {
            maxLaborSupplyFracMale = XMLHelper<double>::getValue( curr );
        } else {
            maxLaborSupplyFracFemale = XMLHelper<double>::getValue( curr );
        }
        //maxLaborSupply = XMLHelper<double>::getValue( curr );
    }
    else if ( nodeName == "maxLandSupplyFrac" ) {
        maxLandSupplyFrac = XMLHelper<double>::getValue( curr );
    }
    else if ( nodeName == "numberOfHouseholds" ) {
        numberOfHouseholds = XMLHelper<double>::getValue( curr );
    }
    else if ( nodeName == "totalLandArea" ) {
        totalLandArea = XMLHelper<double>::getValue( curr );
    }
    else if ( nodeName == "baseLandSupply" ) {
        baseLandSupply = XMLHelper<double>::getValue( curr );
    }
    else if ( nodeName == "baseLaborSupply" ) {
        baseLaborSupply = XMLHelper<double>::getValue( curr );
    }
    else if ( nodeName == "fixedLaborSupplyFrac" ) {
        fixedLaborSupplyFrac = XMLHelper<double>::getValue( curr );
    }
    else {
        return false;
    }
    return true;
}

//! For derived classes to output XML data
void HouseholdConsumer::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    XMLWriteElement(baseLandDemandPerHH, "baseLandDemandPerHH", out, tabs );
    XMLWriteElement(baseLaborDemandPerHH, "baseLaborDemandPerHH", out, tabs );
    XMLWriteElement( mInitialSavings, "savings", out, tabs );
    XMLWriteElement(maxSavingsSupplyFrac, "maxSavingsSupplyFrac", out, tabs );
    XMLWriteElement(maxLandSupplyFrac, "maxLandSupplyFrac", out, tabs );
    //XMLWriteElement(maxLaborSupplyFracMale, "maxLaborSupplyFrac", out, tabs ); //gender attribute??
    //XMLWriteElement(maxLaborSupplyFracFemale, "maxLaborSupplyFrac", out, tabs ); //gender attribute??
    XMLWriteElement(socialSecurityTaxRate, "socialSecurityTaxRate", out, tabs );
    XMLWriteElement(incomeTaxRate, "incomeTaxRate", out, tabs );
    XMLWriteElement(numberOfHouseholds, "numberOfHouseholds", out, tabs );
    XMLWriteElement(personsPerHousehold, "personsPerHousehold", out, tabs );
    XMLWriteElement(totalLandArea, "totalLandArea", out, tabs );
    XMLWriteElement(baseLandSupply, "baseLandSupply", out, tabs );
    XMLWriteElement(baseLaborSupply, "baseLaborSupply", out, tabs );
    XMLWriteElement(workingAgePopMale, "workingAgePopMale", out, tabs );
    XMLWriteElement(workingAgePopFemale, "workingAgePopFemale", out, tabs );
}

//! Output debug info for derived class
void HouseholdConsumer::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteElement(baseLandDemandPerHH, "baseLandDemandPerHH", out, tabs );
    XMLWriteElement(baseLaborDemandPerHH, "baseLaborDemandPerHH", out, tabs );
    //XMLWriteElement(expenditures[ aPeriod ].getValue( Expenditure::SAVINGS ), "savings", out, tabs );
    XMLWriteElement(maxSavingsSupplyFrac, "maxSavingsSupplyFrac", out, tabs );
    XMLWriteElement(maxLandSupplyFrac, "maxLandSupplyFrac", out, tabs );
    //XMLWriteElement(maxLaborSupplyFracMale, "maxLaborSupplyFrac", out, tabs ); //gender attribute??
    //XMLWriteElement(maxLaborSupplyFracFemale, "maxLaborSupplyFrac", out, tabs ); //gender attribute??
    XMLWriteElement(socialSecurityTaxRate, "socialSecurityTaxRate", out, tabs );
    XMLWriteElement(incomeTaxRate, "incomeTaxRate", out, tabs );
    XMLWriteElement(numberOfHouseholds, "numberOfHouseholds", out, tabs );
    XMLWriteElement(personsPerHousehold, "personsPerHousehold", out, tabs );
    XMLWriteElement(totalLandArea, "totalLandArea", out, tabs );
    XMLWriteElement(landDemand, "landDemand", out, tabs );
    XMLWriteElement(laborDemand, "laborDemand", out, tabs );
    XMLWriteElement(householdLandDemand, "householdLandDemand", out, tabs );
    XMLWriteElement(householdLaborDemand, "householdLaborDemand", out, tabs );
    XMLWriteElement(baseScalerSavings, "baseScalerSavings", out, tabs );
    XMLWriteElement(baseScalerLand, "baseScalerLand", out, tabs );
    XMLWriteElement(baseScalerLaborMale, "baseScalerLaborMale", out, tabs );
    XMLWriteElement(baseScalerLaborFemale, "baseScalerLaborFemale", out, tabs );
    XMLWriteElement(transfer, "transfer", out, tabs );
    XMLWriteElement(baseLandSupply, "baseLandSupply", out, tabs );
    XMLWriteElement(baseLaborSupply, "baseLaborSupply", out, tabs );
    XMLWriteElement(landSupply, "landSupply", out, tabs );
    XMLWriteElement(laborSupplyMale, "laborSupplyMale", out, tabs );
    XMLWriteElement(laborSupplyFemale, "laborSupplyFemale", out, tabs );
    XMLWriteElement(workingAgePopMale, "workingAgePopMale", out, tabs );
    XMLWriteElement(workingAgePopFemale, "workingAgePopFemale", out, tabs );
    //expenditures[ aPeriod ].toDebugXML( period, out, tabs );
}

// complete init only sets up the production demand function, can't do much because
// the markets are not set up yet
void HouseholdConsumer::completeInit( const string& aRegionName ) {
    prodDmdFnType = "HouseholdDemandFn";
    BaseTechnology::completeInit( aRegionName );

    // Only the base year consumer should setup the market, future consumers
    // will use it.
    const Modeltime* modeltime = scenario->getModeltime();
    if( modeltime->getyr_to_per( year ) == modeltime->getBasePeriod() ){
        // Setup a trial value market for consumer's budget. This will always be
        // a regional market, and made unique per consumer type.
        Marketplace* marketplace = scenario->getMarketplace();
        if( !marketplace->createMarket( aRegionName, aRegionName, getBudgetMarketName(),
            IMarketType::NORMAL ) )
        {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Budget trial value market already existed." << endl;
        }

        // Set the market to solve. Note that it is also solving the base
        // period. It might help to set a reasonable initial price in the base
        // period. Future periods will use the last period's price as a starting
        // point, which should be reasonable.
        for( int period = 0; period < scenario->getModeltime()->getmaxper(); ++period ){
            marketplace->setMarketToSolve( getBudgetMarketName(), aRegionName, period );
        }
    }
}

// Init calc only runs in the base period, used to calculate all of the base year coefficients
void HouseholdConsumer::initCalc( const MoreSectorInfo* aMoreSectorInfo, const string& aRegionName,
                                  const string& aSectorName, NationalAccount& nationalAccount,
                                  const Demographic* aDemographics, const double aCapitalStock,
                                  const int aPeriod )
{
    Consumer::initCalc( aMoreSectorInfo, aRegionName, aSectorName,
                        nationalAccount, aDemographics, aCapitalStock,
                        aPeriod );

    if ( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
        Marketplace* marketplace = scenario->getMarketplace();

        // calculate Price Paid
        BaseTechnology::calcPricePaid(aMoreSectorInfo, aRegionName, aSectorName, aPeriod);

        if( aPeriod == 0 ) {
            assert( baseLandSupply > 0 );
            double baseLandPrice = marketplace->getDemand("Land", aRegionName, 0) / baseLandSupply;
            if( baseLandPrice == 0 ){
                baseLandPrice = 1;
            }

            assert( baseLaborSupply > 0 );
            double baseLaborPrice = marketplace->getDemand("Labor", aRegionName, 0) / baseLaborSupply;
            if( baseLaborPrice == 0 ) {
                baseLaborPrice = 1;
            }

            // Set the working age populations from the demographics.
            assert( aDemographics );
            workingAgePopMale = aDemographics->getWorkingAgePopulationMales( aPeriod );
            workingAgePopFemale = aDemographics->getWorkingAgePopulationFemales( aPeriod );

            // call income calculation once in the base year to calculate consumption
            calcIncome( nationalAccount, aRegionName, aPeriod );
            // does not have the right consumption income because dividends are not calculated from production sectors
            prodDmdFn->calcCoefficient(input, expenditures[ aPeriod ].getValue( Expenditure::CONSUMPTION ), aRegionName, aSectorName, aPeriod );
        }
        // Apply technical change to input coefficients.
        prodDmdFn->applyTechnicalChange( input, TechChange(), aRegionName, aSectorName, aPeriod, 0, 0 );
    }
}

//! calculate factor supply
void HouseholdConsumer::calcFactorSupply( const string& regionName, int period ) {
    calcSavings( expenditures[ period ].getValue( Expenditure::DISPOSABLE_INCOME ), regionName, period );
    calcLandSupply( regionName, period );
    calcLaborSupply( regionName, period );
}

//! calculate factor demand
void HouseholdConsumer::calcFactorDemand( const string& regionName, int period ) {

    // *******  Land  *******
    // number of households needed for land demand
    // baseLandDemandPerHH is R(2) calculated from base IO data

    landDemand = numberOfHouseholds * baseLandDemandPerHH;

    // if( landDemand == 0 ){
    //     cout << "Warning: Land demand for householdConsumer is zero." << endl;
    // }
    assert( util::isValidNumber( landDemand ) );
    assert( landDemand >= 0 );

    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->addToDemand( "Land", regionName, landDemand, period );
    householdLandDemand = landDemand * FunctionUtils::getPricePaid( regionName, "Land", period );

    // *******  Labor  *******
    laborDemand = ( laborSupplyMale + laborSupplyFemale ) * baseLaborDemandPerHH;
    assert( laborDemand >= 0 );
    assert( util::isValidNumber( laborDemand ) );
    marketplace->addToDemand( "Labor", regionName, laborDemand, period );
    householdLaborDemand = laborDemand * FunctionUtils::getPricePaid( regionName, "Labor", period );
    assert( util::isValidNumber( householdLaborDemand ) );
    assert( householdLaborDemand >= 0 );
}

//! calculate social security tax
double HouseholdConsumer::calcSocialSecurityTax( NationalAccount& nationalAccount, const string& regionName, int period ) {
    Marketplace* marketplace = scenario->getMarketplace();
    double socialSecurityTax = socialSecurityTaxRate * marketplace->getPrice( "Labor", regionName, period )
                               * ( laborSupplyMale + laborSupplyFemale );

    expenditures[ period ].setType( Expenditure::SOCIAL_SECURITY_TAX, socialSecurityTax );
    return socialSecurityTax;
}

//! calculate savings
void HouseholdConsumer::calcSavings( double disposableIncome, const string& regionName, int period ) {
    Marketplace* marketplace = scenario->getMarketplace();
    //maxSavingsRate = So, S1 = 1, S2 = savingsBaseCoef
    const int S1 = 1;
    double savings = maxSavingsSupplyFrac * disposableIncome * ( 1 - S1*exp(
        baseScalerSavings * marketplace->getPrice( "Capital", regionName, period ) ) ); // looks to be the correct price
    expenditures[ period ].setType( Expenditure::SAVINGS, savings );
    assert( savings > 0 );
    marketplace->addToSupply("Capital", regionName, savings, period );
}

// calculate land supply
void HouseholdConsumer::calcLandSupply( const string& regionName, int period ) {
    Marketplace* marketplace = scenario->getMarketplace();
    //maxSavingsRate = So, S1 = 1, S2 = savingsBaseCoef
    const int R1 = 1;
    landSupply = maxLandSupplyFrac * totalLandArea * ( 1 - R1*exp(
        baseScalerLand * marketplace->getPrice( "Land", regionName, period ) ) ); // not sure i think i found this, and this is the correct price
    marketplace->addToSupply("Land", regionName, landSupply, period );
}

// calculate labor supply
void HouseholdConsumer::calcLaborSupply( const string& regionName, int period ) {
    Marketplace* marketplace = scenario->getMarketplace();
    // alternative labor supply calculation based on fixed fraction.
    /*! \pre Working age population variables have been set from the
    *        demographics object.
    */
    assert( workingAgePopMale != -1 );
    assert( workingAgePopFemale != -1 );
    if(fixedLaborSupplyFrac != 0) {
        laborSupplyMale = workingAgePopMale * fixedLaborSupplyFrac;
        laborSupplyFemale = workingAgePopFemale * fixedLaborSupplyFrac;
    }
    else {
        const int L1 = 1;
        laborSupplyMale = maxLaborSupplyFracMale * workingAgePopMale
                          * ( 1 - L1 * exp( baseScalerLaborMale * marketplace->getPrice( "Labor", regionName, period ) ) );
        laborSupplyFemale = maxLaborSupplyFracFemale * workingAgePopFemale * ( 1 - L1*exp(
            baseScalerLaborFemale * marketplace->getPrice( "Labor", regionName, period ) ) );
    }

    double totalLabor = laborSupplyMale + laborSupplyFemale;
    assert( totalLabor > 0 );
    marketplace->addToSupply("Labor", regionName, totalLabor, period );
}

// is this guaranteed to be called after calculation
double HouseholdConsumer::getLaborSupply() const {
    return laborSupplyMale + laborSupplyFemale;
}
//! calculate income
void HouseholdConsumer::calcIncome( NationalAccount& nationalAccount, const string& regionName, int period ) {
    // national accounts give national totals, how to divide these totals to each comsumer
    // type has not been developed
    // social security tax calculation requires total supply of labor wages
    const Modeltime* modelTime = scenario->getModeltime();
    int basePeriod = modelTime->getBasePeriod();

    // only calculate scalers in the base period
    if ( period == basePeriod ) {
        calcBaseScalerLand( regionName, period );
        calcBaseScalerLaborMale( regionName, period );
        calcBaseScalerLaborFemale( regionName, period );

    }
    calcLaborSupply( regionName, period );
    calcLandSupply( regionName, period );

    if( period == basePeriod ){
        calcBaseLaborDemandPerHH( regionName, period );
        calcBaseLandDemandPerHH( regionName, period );
    }
    double socialSecurityTax = calcSocialSecurityTax( nationalAccount, regionName, period );

    Marketplace* marketplace = scenario->getMarketplace();
    double laborWages = marketplace->getPrice( "Labor", regionName, period )
                        * ( laborSupplyMale + laborSupplyFemale );

    double taxableIncome = nationalAccount.getAccountValue( NationalAccount::DIVIDENDS ) +
        laborWages - socialSecurityTax +
        nationalAccount.getAccountValue( NationalAccount::LAND_RENTS );

    // There are no demands for land and household should not get any land rents, but
    // the old SGM adds land rents to household income, add here for consistency with old
    // code although wrong.
    taxableIncome += landSupply * marketplace->getPrice( "Land", regionName, period );
    double directTaxes = taxableIncome * incomeTaxRate;

    transfer = nationalAccount.getAccountValue( NationalAccount::TRANSFERS );
    if( period == basePeriod ) {
        transfer = baseTransfer;
    }
    double disposableIncome = taxableIncome - directTaxes + transfer;
    expenditures[ period ].setType( Expenditure::DISPOSABLE_INCOME, disposableIncome );
    // now that we have a disposable income we can compute the baseScalerSavings in the base year
    if( period == basePeriod ) {
        calcBaseScalerSavings( regionName, period );
    }
    calcSavings(disposableIncome, regionName, period );
    double consumption = disposableIncome - expenditures[ period ].getValue( Expenditure::SAVINGS );
    assert( consumption >= 0 );
    // total gross income for households
    double income = disposableIncome + directTaxes + socialSecurityTax;
    // add expenditures[ aPeriod ]s to the expenditures[ aPeriod ] map for storage
    expenditures[ period ].setType( Expenditure::WAGES, laborWages );
    expenditures[ period ].setType( Expenditure::TAXABLE_INCOME, taxableIncome );
    expenditures[ period ].setType( Expenditure::DIRECT_TAXES, directTaxes );
    expenditures[ period ].setType( Expenditure::TRANSFERS, transfer );
    expenditures[ period ].setType( Expenditure::DIVIDENDS,
                        nationalAccount.getAccountValue( NationalAccount::DIVIDENDS ) );
    expenditures[ period ].setType( Expenditure::CONSUMPTION, consumption );
    expenditures[ period ].setType( Expenditure::INCOME, income );

    // savings and social securtity tax are added to expenditures[ aPeriod ] as they are
    // calculated Set the trial values for the total household taxes into the
    // marketplace. This is to resolve ordering problems between the household
    // consumer and government consumer. The government consumer will have setup
    // this markets.
    marketplace->addToDemand( "household-taxes", regionName, directTaxes + socialSecurityTax, period );

    // add to National Accounts
    nationalAccount.addToAccount( NationalAccount::SOCIAL_SECURITY_TAX, socialSecurityTax );

    nationalAccount.addToAccount( NationalAccount::PERSONAL_INCOME_TAXES, directTaxes );
    nationalAccount.addToAccount( NationalAccount::GNP, consumption );
    nationalAccount.addToAccount( NationalAccount::CONSUMPTION, consumption );
}

//! calculate demand
void HouseholdConsumer::operate( NationalAccount& aNationalAccount, const Demographic* aDemographics,
                                 const MoreSectorInfo* aMoreSectorInfo, const string& aRegionName,
                                 const string& aSectorName, const bool aIsNewVintageMode, int aPeriod )
{
    // since subsector needs to run operate for previous years also for the
    // production side we need to make sure that this householdConsumer is
    // really supposed to operate in this period
    if ( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
        expenditures[ aPeriod ].reset();

        // calcIncome is already run in the base period by initCalc
        // use setType in expenditures[ aPeriod ] to override and ensure that marketplace supplies
        // and demands are nulled
        workingAgePopMale = aDemographics->getWorkingAgePopulationMales( aPeriod );
        workingAgePopFemale = aDemographics->getWorkingAgePopulationFemales( aPeriod );

        // calculate prices paid for householdConsumer inputs
        BaseTechnology::calcPricePaid( aMoreSectorInfo, aRegionName, aSectorName, aPeriod );

        calcIncome( aNationalAccount, aRegionName, aPeriod );
        // calculate consumption demands for each final good or service. Use the
        // trial value for the budget since the household land and labor demands
        // are not yet known. This is explicitally solving the Consumption =
        // Budget equation. The reason this cannot be directly solved for is
        // that households may demand land and labor, which would increase their
        // income, and increase their budget.
        Marketplace* marketplace = scenario->getMarketplace();
        double trialBudget = marketplace->getPrice( getBudgetMarketName(),
                                                    aRegionName, aPeriod );
        calcInputDemand( trialBudget, aRegionName, aSectorName, aPeriod );

        // Add output, or demand as the supply of the budget equation. This is
        // because as personal income increases the demand will increase, which
        // is the assumed movement of the supply side of the market.
        marketplace->addToSupply( getBudgetMarketName(), aRegionName, mOutputs[ aPeriod ], aPeriod );

        calcNoHouseholds( aDemographics, aPeriod );
        // household's factor demands are calculated after total supplies are
        // known number of household must be calculated before factor demand can
        // be calculated
        calcFactorDemand( aRegionName, aPeriod );

        // calcBudget adjusts consumption for land and labor demands.
        calcBudget( aRegionName, aPeriod );

        calcEmissions( aSectorName, aRegionName, aPeriod );
    }
}


//! calculate number of households
void HouseholdConsumer::calcNoHouseholds( const Demographic* aDemographics, int aPeriod ) {
    assert( aDemographics );
    if( aPeriod == scenario->getModeltime()->getBasePeriod() ) {
        assert( numberOfHouseholds > 0 );
        personsPerHousehold = aDemographics->getTotal( aPeriod ) / numberOfHouseholds;
    }
    assert( personsPerHousehold > 0 );
    numberOfHouseholds = aDemographics->getTotal( aPeriod ) / personsPerHousehold;
}

//! calculate budget
void HouseholdConsumer::calcBudget( const string& aRegionName, const int aPeriod ) {
    // subtract household's land and labor expenditures[ aPeriod ] from cosumption to get budget
    // available for consumption.  However, the origianl consumption amount is used in
    // the demand equation.
    double budget = expenditures[ aPeriod ].getValue( Expenditure::CONSUMPTION ) - householdLandDemand - householdLaborDemand;
    expenditures[ aPeriod ].setType( Expenditure::BUDGET, budget );
    // Set the budget as the demand in the budget constraint equation.
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->addToDemand( getBudgetMarketName(), aRegionName, budget, aPeriod );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& HouseholdConsumer::getXMLName() const {
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
const string& HouseholdConsumer::getXMLNameStatic() {
    const static string XML_NAME = "consumer";
    return XML_NAME;
}

//! Calculate base scaler land
void HouseholdConsumer::calcBaseScalerLand( const string& regionName, const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();
    // Assert on divide by zeros.
    assert( maxLandSupplyFrac * totalLandArea > 0 );
    assert( marketplace->getPrice( "Land", regionName, period ) > 0 );

    baseScalerLand = log( 1 - baseLandSupply / ( maxLandSupplyFrac * totalLandArea ) ) /
        marketplace->getPrice( "Land", regionName, period );
}

//! Calculate base scaler labor male
void HouseholdConsumer::calcBaseScalerLaborMale( const string& regionName, const int& period ) {

    /*! \pre Working age population variables have been set from the
    *        demographics object.
    */
    assert( workingAgePopMale != -1 );
    assert( workingAgePopFemale != -1 );

    // Assert on divide by zeros.
    assert( ( maxLaborSupplyFracMale * ( workingAgePopMale + workingAgePopFemale ) ) > 0 );

    Marketplace* marketplace = scenario->getMarketplace();
    assert( marketplace->getPrice( "Labor", regionName, period ) > 0 );
    baseScalerLaborMale = log( 1 - baseLaborSupply / ( maxLaborSupplyFracMale * ( workingAgePopMale + workingAgePopFemale ) ) ) /
        marketplace->getPrice( "Labor", regionName, period );
}

//! Calculate base scaler labor female
void HouseholdConsumer::calcBaseScalerLaborFemale( const string& regionName, const int period ) {
    /*! \pre Working age population variables have been set from the
    *        demographics object.
    */
    assert( workingAgePopMale != -1 );
    assert( workingAgePopFemale != -1 );

    assert( maxLaborSupplyFracFemale * ( workingAgePopMale + workingAgePopFemale ) > 0 );
    Marketplace* marketplace = scenario->getMarketplace();
    assert( marketplace->getPrice( "Labor", regionName, period ) > 0 );
    baseScalerLaborFemale = log( 1 - baseLaborSupply / ( maxLaborSupplyFracFemale * ( workingAgePopMale + workingAgePopFemale ) ) ) /
        marketplace->getPrice( "Labor", regionName, period );
}

//! Calculate base scaler savings
// Add more asserts.
void HouseholdConsumer::calcBaseScalerSavings( const string& regionName, const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();
    assert( mInitialSavings > 0 );
    assert( expenditures[ aPeriod ].getValue( Expenditure::DISPOSABLE_INCOME ) > 0 );

    baseScalerSavings = log( 1 - mInitialSavings
                        / ( maxSavingsSupplyFrac * expenditures[ period ].getValue( Expenditure::DISPOSABLE_INCOME ) ) ) /
                        marketplace->getPrice( "Capital", regionName, period );
}

//! Calculate base labor demand per household
void HouseholdConsumer::calcBaseLaborDemandPerHH( const string& regionName, const int period ) {
    const Input* laborInput = FunctionUtils::getInput( input, "Labor" );
    // If there is no labor demand set the base labor demand per household to
    // zero.
    if( !laborInput ){
        baseLaborDemandPerHH = 0;
    }
    else {
        double laborPrice = laborInput->getPrice( regionName, period );
        if( laborPrice > 0 && laborSupplyMale + laborSupplyFemale > 0 ){
            // if use get demand currency do we need to get price...
            baseLaborDemandPerHH = laborInput->getDemandCurrency( period )
                                   / ( laborSupplyMale + laborSupplyFemale ) * laborPrice;
        }
        else {
            baseLaborDemandPerHH = 0;
        }
    }
}

//! Calculate base land demand per household
void HouseholdConsumer::calcBaseLandDemandPerHH( const string& aRegionName, const int aPeriod ) {
    const Input* landInput = FunctionUtils::getInput( input, "Land" );
    // If the Land input does not exist set the base land demand per household
    // to zero.
    if( !landInput ){
        baseLandDemandPerHH = 0;
    }
    else {
        double landPrice = landInput->getPrice( aRegionName, aPeriod );

        // Check that there is a positive land supply and land price.
        if( landSupply > 0 && landPrice > 0 ){
            // not sure if this is the correct price
            baseLandDemandPerHH = landInput->getDemandCurrency( aPeriod )
                                  / ( landSupply * landPrice );
        }
        else {
            baseLandDemandPerHH = 0;
        }
    }
}

/*! \brief Return the name of the market used to solve the budget equation for this consumer.
* \return The name of the budget market.
* \author Josh Lurz
*/
const string HouseholdConsumer::getBudgetMarketName() const {
    return getName() + "-budget";
}

/*! \brief For outputing SGM data to a flat csv File
 *
 * \author Pralit Patel
 * \param period The period which we are outputing for
 */
void HouseholdConsumer::csvSGMOutputFile( ostream& aFile, const int period ) const {
    if ( year == scenario->getModeltime()->getper_to_yr( period ) ) {
        aFile << "***** Household Sector Results *****" << endl << endl;

        aFile << "Land Demand" << ',' << landDemand << endl;
        aFile << "Labor Demand" << ',' << laborDemand << endl;
        aFile << "Household Land Demand" << ',' << householdLandDemand << endl;
        aFile << "Household Labor Demand" << ',' << householdLaborDemand << endl;

        aFile << "Persons Per Household" << ',' << personsPerHousehold << endl;
        aFile << "Number Of Households" << ',' << numberOfHouseholds << endl;
        aFile << "Total Land Area" << ',' << totalLandArea << endl;

        aFile << "Land Supply" << ',' << landSupply << endl;
        aFile << "Labor Supply: Male" << ',' << laborSupplyMale << endl;
        aFile << "LaborSupply: Female" << ',' << laborSupplyFemale << endl;
        aFile << "Labor Supply: Total" << ',' << laborSupplyMale + laborSupplyFemale << endl;

        aFile << "Working Age Pop: Male" << ',' << workingAgePopMale << endl;
        aFile << "Working Age Pop: Female" << ',' << workingAgePopFemale << endl;
        aFile << "Working Age Pop: Total" << ',' << workingAgePopMale + workingAgePopFemale << endl;
        expenditures[ period ].csvSGMOutputFile( aFile, period );

        aFile << endl;

        aFile << "HouseholdConsumer Expenditure" << endl << endl;
        BaseTechnology::csvSGMOutputFile( aFile, period );
    }
}

void HouseholdConsumer::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitHouseholdConsumer( this, aPeriod );
    Consumer::accept( aVisitor, aPeriod );
    aVisitor->endVisitHouseholdConsumer( this, aPeriod );
}
