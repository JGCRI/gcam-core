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
* \file trade_consumer.cpp
* \ingroup Objects
* \brief The Trade Consumer class source file.
*
* \author Sonny Kim
* \author Pralit Patel
* \date $Date$
* \version $Revision$
*/
#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNode.hpp>

#include "consumers/include/trade_consumer.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/national_account.h"
#include "functions/include/ifunction.h"
#include "containers/include/scenario.h"
#include "util/base/include/ivisitor.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default constructor
TradeConsumer::TradeConsumer(){
}

void TradeConsumer::copyParam( const BaseTechnology* aTradeConsumer ) {
    BaseTechnology::copyParam( aTradeConsumer );
    aTradeConsumer->copyParamsInto( *this );
}

void TradeConsumer::copyParamsInto( TradeConsumer& aTradeConsumer ) const {
}

TradeConsumer* TradeConsumer::clone() const {
	return new TradeConsumer( *this );
}

//! Parse xml file for data
bool TradeConsumer::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aCurr ) {
	return false;
}

//! For derived classes to output XML data
void TradeConsumer::toInputXMLDerived( ostream& aOut, Tabs* aTabs ) const {
}

//! Output debug info for derived class
void TradeConsumer::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
}

//! Complete the initialization
void TradeConsumer::completeInit( const string& aRegionName ) {
	prodDmdFnType = "TradeDemandFn";
	BaseTechnology::completeInit( aRegionName );
}

//! initialize anything that won't change during the calcuation
void TradeConsumer::initCalc( const MoreSectorInfo* aMoreSectorInfo, const string& aRegionName, 
                              const string& aSectorName, NationalAccount& aNationalAccount, 
                              const Demographic* aDemographics, const double aCapitalStock, 
                              const int aPeriod ) 
{
    if( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
        // calculate Price Paid
        BaseTechnology::calcPricePaid( aMoreSectorInfo, aRegionName, aSectorName, aPeriod );
    }
}

//! calculate income
void TradeConsumer::calcIncome( NationalAccount& aNationalAccount, const Demographic* aDemographics, 
                                const string& aRegionName, const string& aSectorName, int aPeriod ) 
{
    expenditure.reset();
	double netExport = mOutputs[ aPeriod ];
	expenditure.setType( Expenditure::TOTAL_IMPORTS, netExport );
	// set National Accounts Consumption for GNP calculation
	aNationalAccount.addToAccount( NationalAccount::GNP, netExport );
	aNationalAccount.addToAccount( NationalAccount::NET_EXPORT, netExport );
}

//! calculate demand
void TradeConsumer::operate( NationalAccount& aNationalAccount, const Demographic* aDemographics, 
                             const MoreSectorInfo* aMoreSectorInfo, const string& aRegionName, 
                             const string& aSectorName, const bool aIsNewVintageMode, int aPeriod ) 
{
	if( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
		// calculate prices paid for consumer inputs
		BaseTechnology::calcPricePaid( aMoreSectorInfo, aRegionName, aSectorName, aPeriod );
		calcInputDemand( expenditure.getValue( Expenditure::CONSUMPTION ), aRegionName, aSectorName, aPeriod );
		calcIncome( aNationalAccount, aDemographics, aRegionName, aSectorName, aPeriod );
        // Trade consumer does not have emissions so it does not calculate them here.
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
const string& TradeConsumer::getXMLName() const {
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
const string& TradeConsumer::getXMLNameStatic() {
    const static string XML_NAME = "tradeConsumer";
	return XML_NAME;
}

//! SGM version of outputing data to a csv file
void TradeConsumer::csvSGMOutputFile( ostream& aFile, const int aPeriod ) const {
	if ( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
		aFile << "***** Trade Sector Results *****" << endl << endl;
		expenditure.csvSGMOutputFile( aFile, aPeriod );
		aFile << endl;

		aFile << "Trade Consumer Expenditure" << endl << endl;
		BaseTechnology::csvSGMOutputFile( aFile, aPeriod );
	}
}

void TradeConsumer::accept( IVisitor* aVisitor, const int aPeriod ) const {
    if( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ){
        Consumer::accept( aVisitor, aPeriod );
        aVisitor->updateTradeConsumer( this, aPeriod );
    }
}
