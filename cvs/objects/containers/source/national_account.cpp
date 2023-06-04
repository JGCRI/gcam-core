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
* \file national_account.cpp
* \ingroup Objects
* \brief The National ccount class source file.
* \author Pralit Patel
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <cassert>

#include "containers/include/national_account.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_parse_helper.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"
#include "demographics/include/demographic.h"
#include "containers/include/iinfo.h"

using namespace std;

extern Scenario* scenario;

//! Default Constructor
NationalAccount::NationalAccount():
// Size the accounts to one after the last valid value in the vector,
// represented by END.
mAccounts( END, 0 )
{
}

NationalAccount* NationalAccount::cloneAndInterpolate( const int aNewYear ) const {
    
    NationalAccount* newNationalAccount = new NationalAccount();
    newNationalAccount->mYear = aNewYear;
    newNationalAccount->mAccounts = this->mAccounts;
    return newNationalAccount;
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
const std::string& NationalAccount::getXMLNameStatic() {
    const static string XML_NAME = "nationalAccount";
    return XML_NAME;
}

//! Return year of national account
int NationalAccount::getYear() const {
    return mYear;
}

//! Returns name (year as a string)
const std::string NationalAccount::getName() const {
    return util::toString( mYear );
}


bool NationalAccount::XMLParse( rapidxml::xml_node<char>* & aNode ) {
    /*! \pre make sure we were passed a valid node. */
    assert( node );
    
    string nodeName = XMLParseHelper::getNodeName(aNode);

    if  ( nodeName == enumToXMLName( SAVINGS_RATE ) ) {
        setAccount( SAVINGS_RATE, XMLParseHelper::getValue<double>( aNode ) );
    }
    else if  ( nodeName == enumToXMLName( CAPITAL_STOCK ) ) {
        setAccount( CAPITAL_STOCK, XMLParseHelper::getValue<double>( aNode ) );
    }
    else if  ( nodeName == enumToXMLName( DEPRECIATION_RATE ) ) {
        setAccount( DEPRECIATION_RATE, XMLParseHelper::getValue<double>( aNode ) );
    }
    else if  ( nodeName == enumToXMLName( CAPITAL_ENERGY_INV) ) {
        setAccount( CAPITAL_ENERGY_INV, XMLParseHelper::getValue<double>( aNode ) );
    }
    else if  ( nodeName == enumToXMLName( GDP ) ) {
        setAccount( GDP, XMLParseHelper::getValue<double>( aNode ) );
    }
    else if  ( nodeName == enumToXMLName( LABOR_WAGES ) ) {
        setAccount( LABOR_WAGES, XMLParseHelper::getValue<double>( aNode ) );
    }
    else if  ( nodeName == enumToXMLName( LABOR_FORCE_SHARE ) ) {
        setAccount( LABOR_FORCE_SHARE, XMLParseHelper::getValue<double>( aNode ) );
    }
    else if  ( nodeName == enumToXMLName( CAPITAL_NET_EXPORT ) ) {
        setAccount( CAPITAL_NET_EXPORT, XMLParseHelper::getValue<double>( aNode ) );
    }
    else if  ( nodeName == enumToXMLName( TOTAL_FACTOR_PRODUCTIVITY ) ) {
        setAccount( TOTAL_FACTOR_PRODUCTIVITY, XMLParseHelper::getValue<double>( aNode ) );
    }
    else {
        return false;
    }
    return true;
}

//! Output debug info to XML
void NationalAccount::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), out, tabs );

        for( int i = 0; i < NationalAccount::END; ++i ) {
            XMLWriteElement( getAccountValue( static_cast< NationalAccount::AccountType >( i ) ),
            enumToXMLName( static_cast< NationalAccount::AccountType >( i ) ),
            out, tabs );
    }
    
    XMLWriteClosingTag( getXMLNameStatic(), out, tabs );
}

void NationalAccount::completeInit() {
}

/*!
 * \brief Complete initializations for the national account for base year
 */
void NationalAccount::completeInitHist( ) {
}


/*!
 * \brief Complete initializations for the national account
 */
void NationalAccount::initCalc( const NationalAccount* aNationalAccountPrevious, const int aPeriod ) {
    
    const Modeltime* modeltime = scenario->getModeltime();

    // note these are last period rates
    // TODO: may want to use current period savings and depreciation rates instead
    double savings_rate = aNationalAccountPrevious->getAccountValue(SAVINGS_RATE);
    double depreciation_rate = aNationalAccountPrevious->getAccountValue(DEPRECIATION_RATE);
    
    double prevGDP = aNationalAccountPrevious->getAccountValue(GDP);
    double capital = aNationalAccountPrevious->getAccountValue(CAPITAL_STOCK);
    double prevEneInv = aNationalAccountPrevious->getAccountValue(CAPITAL_ENERGY_INV);
    double prevConsDurableInv = aNationalAccountPrevious->getAccountValue(CONSUMER_DURABLE_INV);
    
    // "move" consumer durable investment value from consumption to investment by inflating the
    // savings rate to accommodate so that it can be included in the investment constraint
    int prevPeriod = aPeriod - 1;
    if(prevPeriod <= modeltime->getFinalCalibrationPeriod()) {
        mConsumerDurableSRAdj = prevConsDurableInv / prevGDP;
    }
    else {
        mConsumerDurableSRAdj = aNationalAccountPrevious->mConsumerDurableSRAdj;
    }

    mAccounts[SAVINGS] = prevGDP * (savings_rate + mConsumerDurableSRAdj); // annual savings based on lagged gdp
    // CAPITAL_NET_EXPORT could be significantly negative (from historical data) so we
    // should take care to ensure it doesn't take the "investment" negative.
    mAccounts[INVESTMENT] = std::max(mAccounts[SAVINGS] + mAccounts[CAPITAL_NET_EXPORT], 0.0);
    mAccounts[DEPRECIATION] = capital * depreciation_rate; // annual depreciation based on lagged capital
    
    // Calculate for future periods only
    if( aPeriod > modeltime->getFinalCalibrationPeriod() ){
        // Depreciate capital and add new investments.
        // Calculate annual changes for cumulative capital stock.
        // Assumes constant GDP within period.
        // Given capital stock is cumulative we need to apply the annual changes for
        // each year in the time step.
        double savings = mAccounts[INVESTMENT] - prevEneInv - prevConsDurableInv;
        for(unsigned int i=0; i < scenario->getModeltime()->gettimestep(aPeriod); ++i){
            capital = capital * (1 - depreciation_rate) + savings;
        }
        
        // protect against negative capital stock, note: in such a case the social
        // account matrix will not balance thus signaling an issue
        const double MIN_CAPITAL_STOCK = 100.0;
        mAccounts[CAPITAL_STOCK] = std::max(capital, MIN_CAPITAL_STOCK);
    }
}

/*!
 * \brief Post calculations for the national account
 */
void NationalAccount::postCalc( ) {
     mAccounts[ GDP_PER_CAPITA ] = mAccounts[GDP] / mAccounts[POPULATION];
}

/*!
 * \brief Post calculations for the national account for historical periods
 */
void NationalAccount::postCalcHist( ) {
    
    // Historical GDP is read-in, remove energy net exports to determine
    // domestic GDP.
    // Add energy expenditure to domestic GDP for gross output.
     
    mAccounts[ GROSS_OUTPUT ] = mAccounts[GDP] - mAccounts[ENERGY_NET_EXPORT] ;
}

/*!
 * \brief Add to the value for the national account specified by the account type key.
 * \param aType The account which will be added to.
 * \param aValue The value which will be added to account aType.
 */
void NationalAccount::addToAccount( const AccountType aType, const double aValue ){
    mAccounts[ aType ] += aValue;
}

/*!
 * \brief Set the value for the national account specified by the account type key.
 * \param aType The account for which value will be set.
 * \param aValue The value which will be set to account aType.
 */
void NationalAccount::setAccount( const AccountType aType, const double aValue ){
    mAccounts[ aType ] = aValue;
}

/*!
 * \brief Get the value for the national account specified by the account type key.
 * \param aType The account who's value will be returned.
 * \return The current value of the account aType.
 */
double NationalAccount::getAccountValue( const AccountType aType ) const {
    assert( aType < END );
    return mAccounts[ aType ];
}

/*!
 * \brief Reset the national account values
 * \note The accounts TRANSFERS and INVESTMENT_TAX_CREDIT do not get reset.
 */
void NationalAccount::reset() {
    // Clear the values.
    mAccounts.clear();
    mAccounts.resize( END );
}

/*!
 * \brief Convert between the NationalAccount enum type to the XML String representation
 * \param aType The enum NationalAccount type
 * \return The XML string representation of the type
 * \author Pralit Patel
 */
const string& NationalAccount::enumToXMLName( const AccountType aType ) const {
    /*! \pre aType is a valid account type. */
    assert( aType < END );
    // Create a static array of values. This will only on the first entrance to
    // the function since this is a const static.

    const static string names[] = {
            "savings-rate",
            "depreciation-rate",
            "savings",
            "investment",
            "depreciation",
            "capital-stock",
            "energy-investment",
            "consumer-durable",
            "GDP",
            "value-added",
            "gross-output",
            "labor-wages",
            "labor-force",
            "labor-force-share",
            "total-factor-productivity",
            "fac-share-labor",
            "fac-share-capital",
            "fac-share-energy",
            "population",
            "gdp-per-capita",
            "gdp-per-capita-ppp",
            "energy-service",
            "energy-service-value",
            "energy-net-export",
            "materials-net-export",
            "capital-net-export"
    };
    // Return the string in the static array at the index.
    return names[ aType ];
}

// for reporting National Account information
void NationalAccount::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitNationalAccount( this, aPeriod );
    aVisitor->endVisitNationalAccount( this, aPeriod );
}
