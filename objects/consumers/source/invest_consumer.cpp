/*! 
* \file invest_consumer.cpp
* \ingroup Objects
* \brief The Investment Consumer class source file.
*
* \author Sonny Kim
* \author Pralit Patel
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNode.hpp>

#include "consumers/include/invest_consumer.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/national_account.h"
#include "functions/include/input.h"
#include "functions/include/ifunction.h"
#include "containers/include/scenario.h"
#include "util/base/include/ivisitor.h"
#include "marketplace/include/marketplace.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

InvestConsumer::InvestConsumer(){
}

void InvestConsumer::copyParam( const BaseTechnology* aInvestConsumer ) {
	BaseTechnology::copyParam( aInvestConsumer );
    aInvestConsumer->copyParamsInto( *this );
}

void InvestConsumer::copyParamsInto( InvestConsumer& aInvestConsumer ) const {
}

InvestConsumer* InvestConsumer::clone() const {
	return new InvestConsumer( *this );
}

//! parses rest of InvestConsumer xml object
bool InvestConsumer::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aCurr ) {
	return false;
}

//! Write out additional datamembers to XML output stream.
void InvestConsumer::toInputXMLDerived( ostream& aOut, Tabs* aTabs ) const {
}

//! Output debug info for derived class
void InvestConsumer::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
}

//! Complete the initialization.
void InvestConsumer::completeInit( const string& aRegionName ) {
	prodDmdFnType = "InvestDemandFn";
	BaseTechnology::completeInit( aRegionName );
}

//! initialize anything that won't change during the calcuation
void InvestConsumer::initCalc( const MoreSectorInfo* aMoreSectorInfo, const string& aRegionName, 
                               const string& aSectorName, NationalAccount& aNationalAccount, 
                               const Demographic* aDemographics, const double aCapitalStock, const int aPeriod ) 
{
    if ( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
        BaseTechnology::calcPricePaid( aMoreSectorInfo, aRegionName, aSectorName, aPeriod );
    }
}

//! calculate income
void InvestConsumer::calcIncome( NationalAccount& aNationalAccount, const Demographic* aDemographics, 
                                 const string& aRegionName, int aPeriod ) 
{
    // Whats up with these?-JPL
	//allocateTransportationDemand( nationalAccount, regionName, period );
	//allocateDistributionCost( nationalAccount, regionName, period );
	expenditure.setType( Expenditure::INVESTMENT, mOutputs[ aPeriod ] );
    scenario->getMarketplace()->addToDemand( "Capital", aRegionName, mOutputs[ aPeriod ], aPeriod );
	// set National Accounts Consumption for GNP calculation
	aNationalAccount.addToAccount( NationalAccount::GNP, 
		   aNationalAccount.getAccountValue( NationalAccount::ANNUAL_INVESTMENT ) );
	//aNationalAccount.addToAccount( NationalAccount::INVESTMENT, output[ aPeriod ] );
	aNationalAccount.addToAccount( NationalAccount::INVESTMENT, 
		   aNationalAccount.getAccountValue( NationalAccount::ANNUAL_INVESTMENT ) );
}

//! calculate demand
void InvestConsumer::operate( NationalAccount& aNationalAccount, const Demographic* aDemographics, 
                             const MoreSectorInfo* aMoreSectorInfo, const string& aRegionName, 
                             const string& aSectorName, const bool aIsNewVintageMode, int aPeriod ) 
{
	if( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
        expenditure.reset();
		// calculate prices paid for consumer inputs
		BaseTechnology::calcPricePaid( aMoreSectorInfo, aRegionName, aSectorName, aPeriod );
		// calculate consumption demands for each final good or service
		calcInputDemand( aNationalAccount.getAccountValue( NationalAccount::ANNUAL_INVESTMENT ), 
                         aRegionName, aSectorName, aPeriod );
		calcIncome( aNationalAccount, aDemographics, aRegionName, aPeriod );
        calcEmissions( aSectorName, aRegionName, aPeriod );
	}
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& InvestConsumer::getXMLName() const {
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
const string& InvestConsumer::getXMLNameStatic() {
    const static string XML_NAME = "investConsumer";
	return XML_NAME;
}

//! SGM version of outputing data members to a csv file
void InvestConsumer::csvSGMOutputFile( ostream& aFile, const int aPeriod ) const {
	if ( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
		aFile << "***** Investment Sector Results *****" << endl << endl;
		expenditure.csvSGMOutputFile( aFile, aPeriod );
		aFile << endl;

		aFile << "Investment Consumer Expenditure" << endl << endl;
		BaseTechnology::csvSGMOutputFile( aFile, aPeriod );
	}
}

void InvestConsumer::accept( IVisitor* aVisitor, const int aPeriod ) const {
    if( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ){
        Consumer::accept( aVisitor, aPeriod );
	    aVisitor->updateInvestConsumer( this, aPeriod );
    }
}
