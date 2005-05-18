/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Labratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file householdConsumer.cpp
* \ingroup Objects
* \brief The HouseholdConsumer class source file.
*
*  Detailed Description.
*
* \author Pralit Patel
* \author Sonny Kim
* \date $Date$
* \version $Revision$
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
#include "reporting/include/output_container.h"
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

	workingAgePopMale = 0;
	workingAgePopFemale = 0;

    mInitialSavings = 0;
}

/*! \brief Used to merge an existing householdConsumer with the coefficients from the previous periods
 *
 * \author Pralit Patel
 * \warning Does not copy everything, only non calculated values to pass on to the next period
 */
void HouseholdConsumer::copyParam( const BaseTechnology* baseTech ) {
    BaseTechnology::copyParam( baseTech );
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
	/*
	else if ( nodeName == "baseScalerLand" ) {
		baseScalerLand = XMLHelper<double>::getValue( curr );
	}*/
	else if (nodeName == "baseLaborDemandPerHH" ) {
		baseLaborDemandPerHH = XMLHelper<double>::getValue( curr );
	}/*
	else if ( nodeName == "baseScalerLabor" ) {
		baseScalerLabor = XMLHelper<double>::getValue( curr );
	}*/
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
	else if ( nodeName == "workingAgePopMale" ) {
		workingAgePopMale = XMLHelper<double>::getValue( curr );
	}
	else if ( nodeName == "workingAgePopFemale" ) {
		workingAgePopFemale = XMLHelper<double>::getValue( curr );
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
	//XMLWriteElement(expenditure.getValue( Expenditure::SAVINGS ), "savings", out, tabs );
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
	//expenditure.toDebugXML( period, out, tabs );
}

// complete init only sets up the production demand function, can't do much because
// the markets are not set up yet
void HouseholdConsumer::completeInit( const string& regionName ) {
	prodDmdFnType = "HouseholdDemandFn";
	BaseTechnology::completeInit( regionName );
}

// Init calc only runs in the base period, used to calculate all of the base year coefficients
void HouseholdConsumer::initCalc( const MoreSectorInfo* aMoreSectorInfo, const string& aRegionName, const string& aSectorName, NationalAccount& nationalAccount , Demographic* aDemographics, const double aCapitalStock, const int aPeriod ) {
    if ( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
        Marketplace* marketplace = scenario->getMarketplace();

        // calculate Price Paid
        BaseTechnology::calcPricePaid(aMoreSectorInfo, aRegionName, aSectorName, aPeriod);

        if( aPeriod == 0 ) {
            double baseLandPrice = marketplace->getDemand("Land", aRegionName, 0) / baseLandSupply;
            double baseLaborPrice = marketplace->getDemand("Labor", aRegionName, 0) / baseLaborSupply;
            if (baseLandPrice == 0) {
                baseLandPrice = 1;
            }
            if(baseLaborPrice == 0) {
                baseLaborPrice = 1;
            }
            // call income calculation once in the base year to calculate consumption
            calcIncome( nationalAccount, aRegionName, aPeriod );
            // does not have the right consumption income because dividends are not calculated from production sectors
            prodDmdFn->calcCoefficient(input, expenditure.getValue( Expenditure::CONSUMPTION ), aRegionName, aSectorName, aPeriod );
        }
        // Apply technical change to input coefficients.
        prodDmdFn->applyTechnicalChange( input, TechChange(), aRegionName, aSectorName, aPeriod, 0, 0 );
    }
}

//! constrain demand
void HouseholdConsumer::constrainDemand( double budgetScale, const string& aRegionName, const int aPeriod ) {
    FunctionUtils::scaleDemandInputs( input, budgetScale, aRegionName, aPeriod );
    
    // readjust consumption total with budgetScale
    // Todo: Replace this with a solved equation.
    mOutputs[ aPeriod ] *= budgetScale;
}

//! calculate factor supply
void HouseholdConsumer::calcFactorSupply( const string& regionName, int period ) {
	calcSavings( expenditure.getValue( Expenditure::DISPOSABLE_INCOME ), regionName, period );
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
	householdLandDemand = landDemand * marketplace->getMarketInfo( "Land", regionName, period, "pricePaid" ); // this was price paid in the UML
	//householdLandDemand = landDemand * getPricePaid( "Land" );
	// *******  Labor  *******
	laborDemand = ( laborSupplyMale + laborSupplyFemale )/*marketplace->getSupply( "Labor", regionName, period )*/ * baseLaborDemandPerHH;
    assert( laborDemand >= 0 );
    assert( util::isValidNumber( laborDemand ) );
	marketplace->addToDemand( "Labor", regionName, laborDemand, period );
	householdLaborDemand = laborDemand * marketplace->getMarketInfo( "Labor", regionName, period, "pricePaid" ); // this was price paid in the UML
    assert( util::isValidNumber( householdLaborDemand ) );
    assert( householdLaborDemand >= 0 );

	//householdLaborDemand = laborDemand * getPricePaid( "Labor" ); 
}

//! calculate social security tax
double HouseholdConsumer::calcSocialSecurityTax( NationalAccount& nationalAccount, const string& regionName, int period ) {
	Marketplace* marketplace = scenario->getMarketplace();
	double socialSecurityTax = socialSecurityTaxRate * marketplace->getPrice( "Labor", regionName, period ) 
		                       * ( laborSupplyMale + laborSupplyFemale );
	//double socialSecurityTax = socialSecurityTaxRate * nationalAccount.getAccountValue( NationalAccount::LABOR_WAGES );
    expenditure.setType( Expenditure::SOCIAL_SECURITY_TAX, socialSecurityTax );
	return socialSecurityTax;
}

//! calculate savings
void HouseholdConsumer::calcSavings( double disposableIncome, const string& regionName, int period ) {
	Marketplace* marketplace = scenario->getMarketplace();
	//maxSavingsRate = So, S1 = 1, S2 = savingsBaseCoef
	const int S1 = 1;
	double savings = maxSavingsSupplyFrac * disposableIncome * ( 1 - S1*exp( 
		baseScalerSavings * marketplace->getPrice( "Capital", regionName, period ) ) ); // looks to be the correct price
    expenditure.setType( Expenditure::SAVINGS, savings );
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
	// alternative labor supply calculation based on fixed fraction
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
double HouseholdConsumer::getLaborSupply() const{
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
	// for debugging
	double tempDividends = nationalAccount.getAccountValue( NationalAccount::DIVIDENDS );
	Marketplace* marketplace = scenario->getMarketplace();
	double tempLaborWages = marketplace->getPrice( "Labor", regionName, period ) 
		* ( laborSupplyMale + laborSupplyFemale );
	//double tempLaborWages = nationalAccount.getAccountValue( NationalAccount::LABOR_WAGES );
	//double tempLaborWagesLessSocialSecurityTax = nationalAccount.getAccountValue( NationalAccount::LABOR_WAGES ) - socialSecurityTax;
	double tempLaborWagesLessSocialSecurityTax = tempLaborWages - socialSecurityTax;
	double tempLandRents = nationalAccount.getAccountValue( NationalAccount::LAND_RENTS );
	// end debugging
	double taxableIncome = nationalAccount.getAccountValue( NationalAccount::DIVIDENDS ) +
		tempLaborWages - socialSecurityTax + 
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
	expenditure.setType( Expenditure::DISPOSABLE_INCOME, disposableIncome );
	// now that we have a disposable income we can compute the baseScalerSavings in the base year
	if( period == basePeriod ) {
		calcBaseScalerSavings( regionName, period );
	}
	calcSavings(disposableIncome, regionName, period );
	// for debugging
	double tempSavings = expenditure.getValue( Expenditure::SAVINGS );
	double consumption = disposableIncome - expenditure.getValue( Expenditure::SAVINGS );
	assert( consumption >= 0 );
	// total gross income for households
	double income = disposableIncome + directTaxes + socialSecurityTax;
	// add expenditures to the expenditure map for storage
	expenditure.setType( Expenditure::WAGES, tempLaborWages );
    expenditure.setType( Expenditure::TAXABLE_INCOME, taxableIncome );
	expenditure.setType( Expenditure::DIRECT_TAXES, directTaxes );
	expenditure.setType( Expenditure::TRANSFERS, transfer );
	expenditure.setType( Expenditure::DIVIDENDS, 
						nationalAccount.getAccountValue( NationalAccount::DIVIDENDS ) );
    expenditure.setType( Expenditure::CONSUMPTION, consumption );
    expenditure.setType( Expenditure::INCOME, income );
  //  expenditure.setType( Expenditure::WAGES, nationalAccount.getAccountValue( NationalAccount::LABOR_WAGES )
  //      - socialSecurityTax );
    
    // savings and social securtity tax are added expenditure as they are calculated

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
	// since subsector needs to run operate for previous years also for the production side
	// we need to make sure that this householdConsumer is really supposed to operate in this period
	if ( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
        expenditure.reset();
		// calcIncome is already run in the base period by initCalc
		// use setType in expenditure to override and ensure that marketplace supplies
		// and demands are nulled
		workingAgePopMale = aDemographics->getWorkingAgePopulationMales( aPeriod );
		workingAgePopFemale = aDemographics->getWorkingAgePopulationFemales( aPeriod );

		// calculate prices paid for householdConsumer inputs
		BaseTechnology::calcPricePaid( aMoreSectorInfo, aRegionName, aSectorName, aPeriod );

		calcIncome( aNationalAccount, aRegionName, aPeriod );
		// calculate consumption demands for each final good or service
		calcInputDemand( expenditure.getValue( Expenditure::CONSUMPTION ), aRegionName, aSectorName, 
            aPeriod );

		calcNoHouseholds( *aDemographics, aPeriod );
		// household's factor demands are calculated after total supplies are known
		// number of household must be calculated before factor demand can be calculated
		calcFactorDemand( aRegionName, aPeriod );
		//calcBudget takes our household land and labor demand
		// this budget is used to scale down all household demands
		calcBudget();
		double tempScale = expenditure.getValue( Expenditure::BUDGET ) / mOutputs[ aPeriod ];
		constrainDemand( tempScale, aRegionName, aPeriod );
        calcEmissions( aSectorName, aRegionName, aPeriod );
	}
}


//! calculate number of households
void HouseholdConsumer::calcNoHouseholds( const Demographic& demographics, int period ) {
	if( period == scenario->getModeltime()->getBasePeriod() ) {
		personsPerHousehold = demographics.getTotal( period ) / numberOfHouseholds;
	}
	numberOfHouseholds = demographics.getTotal( period ) / personsPerHousehold;
}

//! calculate budget
void HouseholdConsumer::calcBudget() {
	// subtract household's land and labor expenditure from cosumption to get budget
	// available for consumption.  However, the origianl consumption amount is used in
	// the demand equation.
    double budget = expenditure.getValue( Expenditure::CONSUMPTION ) - householdLandDemand - householdLaborDemand;
    expenditure.setType( Expenditure::BUDGET, budget );
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
	Marketplace* marketplace = scenario->getMarketplace();
    // Assert on divide by zeros.
    assert( ( maxLaborSupplyFracMale * ( workingAgePopMale + workingAgePopFemale ) ) > 0 );
    assert( marketplace->getPrice( "Labor", regionName, period ) > 0 );
	baseScalerLaborMale = log( 1 - baseLaborSupply / ( maxLaborSupplyFracMale * ( workingAgePopMale + workingAgePopFemale ) ) ) /
		marketplace->getPrice( "Labor", regionName, period );
}

//! Calculate base scaler labor female
void HouseholdConsumer::calcBaseScalerLaborFemale( const string& regionName, const int period ) {
	Marketplace* marketplace = scenario->getMarketplace();
	assert( maxLaborSupplyFracFemale * ( workingAgePopMale + workingAgePopFemale ) > 0 );
    assert( marketplace->getPrice( "Labor", regionName, period ) > 0 );
    baseScalerLaborFemale = log( 1 - baseLaborSupply / ( maxLaborSupplyFracFemale * ( workingAgePopMale + workingAgePopFemale ) ) ) /
		marketplace->getPrice( "Labor", regionName, period );
}

//! Calculate base scaler savings
// Add more asserts.
void HouseholdConsumer::calcBaseScalerSavings( const string& regionName, const int period ) {
	Marketplace* marketplace = scenario->getMarketplace();
    assert( mInitialSavings > 0 );
    assert( expenditure.getValue( Expenditure::DISPOSABLE_INCOME ) > 0 );

	baseScalerSavings = log( 1 - mInitialSavings
                        / ( maxSavingsSupplyFrac * expenditure.getValue( Expenditure::DISPOSABLE_INCOME ) ) ) /
		                marketplace->getPrice( "Capital", regionName, period );
}

//! Calculate base labor demand per household
void HouseholdConsumer::calcBaseLaborDemandPerHH( const string& regionName, const int period ) {
    // If the Labor input does not exist.
    if( !util::hasValue( inputNameToNo, string( "Labor" ) ) ){
        return;
    }

    unsigned int laborIndex = util::searchForValue( inputNameToNo, string( "Labor" ) );
    Marketplace* marketplace = scenario->getMarketplace();
     // if use get demand currency do we need to get price...
    assert( ( laborSupplyMale + laborSupplyFemale ) * marketplace->getPrice( "Labor", regionName, period ) > 0 );
    baseLaborDemandPerHH = input[ laborIndex ]->getDemandCurrency() / ( ( laborSupplyMale + laborSupplyFemale ) * marketplace->getPrice( "Labor", regionName, period ) );
}

//! Calculate base land demand per household
void HouseholdConsumer::calcBaseLandDemandPerHH( const string& regionName, const int period ) {
    // If the Land input does not exist.
    if( !util::hasValue( inputNameToNo, string( "Land" ) ) ){
        baseLandDemandPerHH = 0;
        return;
    }

    Marketplace* marketplace = scenario->getMarketplace();
    double landPrice = marketplace->getPrice( "Land", regionName, period );

    if( landSupply > 0 && landPrice > 0 ){
        // not sure if this is the correct price
        unsigned int landIndex = util::searchForValue( inputNameToNo, string( "Land" ) );
        baseLandDemandPerHH = input[ landIndex ]->getDemandCurrency() / 
            ( landSupply * landPrice );
    }
    else {
        baseLandDemandPerHH = 0;
    }
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
		expenditure.csvSGMOutputFile( aFile, period );

		aFile << endl;

		aFile << "HouseholdConsumer Expenditure" << endl << endl;
		BaseTechnology::csvSGMOutputFile( aFile, period );
	}
}

void HouseholdConsumer::updateOutputContainer( OutputContainer* outputContainer,  const string& aRegionName, const string& aSectorName, const int aPeriod ) const {
    // I don't like this here but its difficult to do in the output container because input does not
    // have a year.
    if( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ){
        Consumer::updateOutputContainer( outputContainer, aRegionName, aSectorName, aPeriod );
	    outputContainer->updateHouseholdConsumer( this, aPeriod );
    }
}
