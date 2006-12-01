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
* \file national_account.cpp
* \ingroup Objects
* \brief The National ccount class source file.
* \author Pralit Patel
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "containers/include/national_account.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace xercesc;

//! Default Constructor
NationalAccount::NationalAccount() {
    // Size the accounts to one after the last valid value in the vector,
    // represented by END.
    mAccounts.resize( END );
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

//! parse SOME xml data
void NationalAccount::XMLParse( const DOMNode* node ) {
    /*! \pre make sure we were passed a valid node. */
    assert( node );

    // get all child nodes.
    DOMNodeList* nodeList = node->getChildNodes();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if  ( nodeName == enumToXMLName( RETAINED_EARNINGS ) ) {
            addToAccount( RETAINED_EARNINGS, XMLHelper<double>::getValue( curr ) );
        }
        else if  ( nodeName == enumToXMLName( DIVIDENDS ) ) {
            addToAccount( DIVIDENDS, XMLHelper<double>::getValue( curr ) );
        }
        else if  ( nodeName == enumToXMLName( TRANSFERS ) ) {
            addToAccount( TRANSFERS, XMLHelper<double>::getValue( curr ) );
        }
        else if  ( nodeName == enumToXMLName( CORPORATE_INCOME_TAX_RATE ) ) {
            setAccount( CORPORATE_INCOME_TAX_RATE, XMLHelper<double>::getValue( curr ) );
        }
        else if  ( nodeName == enumToXMLName( EXCHANGE_RATE ) ) {
            addToAccount( EXCHANGE_RATE, XMLHelper<double>::getValue( curr ) );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing " << getXMLNameStatic() << "." << endl;
        }
    }
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

//! Add to the value for the national account specified by the account type key. 
void NationalAccount::addToAccount( const AccountType aType, const double aValue ){
    mAccounts[ aType ]+= aValue;
}

//! Set the value for the national account specified by the account type key. 
void NationalAccount::setAccount( const AccountType aType, const double aValue ){
    mAccounts[ aType ] = aValue;
}

//! Get the value for the national account specified by the account type key. 
double NationalAccount::getAccountValue( const AccountType aType ) const {
    assert( aType < END );
    return mAccounts[ aType ];
}

//! Reset the national account values
void NationalAccount::reset() {
    // save tranfer amount calculated from government's initCalc
    // this is a giant hack. store the values that should not be reset.
    double tempTransfers = mAccounts[ TRANSFERS ];
    assert( tempTransfers > 0 );
    double tempInvTaxCreditRate = mAccounts[ INVESTMENT_TAX_CREDIT ];
    
    // Clear the values.
    mAccounts.clear();
    mAccounts.resize( END );

    // Restore the two values that were saved.
    mAccounts[ TRANSFERS ] = tempTransfers;
    mAccounts[ INVESTMENT_TAX_CREDIT ] = tempInvTaxCreditRate;
}

/*! \brief For outputing SGM data to a flat csv File
 * 
 * \author Pralit Patel
 * \param period The period which we are outputing for
 */
void NationalAccount::csvSGMOutputFile( ostream& aFile, const int period ) const {
    // write headers
    aFile << "GNP Accounts" << endl;
    aFile << enumToName( GNP ) << ',';
    aFile << enumToName( CONSUMPTION ) << ',';
    aFile << enumToName( GOVERNMENT ) << ',';
    aFile << enumToName( INVESTMENT ) << ',';
    aFile << enumToName( NET_EXPORT ) << ',';
    aFile << endl;
    // write data
    aFile << getAccountValue( GNP ) << ',';
    aFile << getAccountValue( CONSUMPTION ) << ',';
    aFile << getAccountValue( GOVERNMENT ) << ',';
    aFile << getAccountValue( INVESTMENT ) << ',';
    aFile << getAccountValue( NET_EXPORT ) << ',';
    aFile << endl << endl;
    // write tax accounts
    aFile << "National Tax Accounts" << endl;
    // write headers
    aFile << enumToName( CORPORATE_INCOME_TAXES ) << ',';
    aFile << enumToName( INDIRECT_BUSINESS_TAX ) << ',';
    aFile << enumToName( SOCIAL_SECURITY_TAX ) << ',';
    aFile << enumToName( PERSONAL_INCOME_TAXES ) << ',';
    aFile << enumToName( INVESTMENT_TAX_CREDIT ) << ',';
    aFile << "Total Taxes";
    aFile << endl;
    // write data
    aFile << getAccountValue( CORPORATE_INCOME_TAXES ) << ',';
    aFile << getAccountValue( INDIRECT_BUSINESS_TAX ) << ',';
    aFile << getAccountValue( SOCIAL_SECURITY_TAX ) << ',';
    aFile << getAccountValue( PERSONAL_INCOME_TAXES ) << ',';
    aFile << getAccountValue( INVESTMENT_TAX_CREDIT ) << ',';
    aFile << getAccountValue( CORPORATE_INCOME_TAXES )
            + getAccountValue( INDIRECT_BUSINESS_TAX )
            + getAccountValue( SOCIAL_SECURITY_TAX )
            + getAccountValue( PERSONAL_INCOME_TAXES )
            + getAccountValue( INVESTMENT_TAX_CREDIT );
    aFile << endl << endl;

}

/*! \brief Convert between the NationalAccount enum type to the String representation
 *
 * \author Pralit Patel
 * \param aType The enum NationalAccount type
 * \return The string representation of the type
 */
const string& NationalAccount::enumToName( const AccountType aType ) const {
    /*! \pre aType is a valid account type. */
    assert( aType < END );
    // Create a static array of values. This will only on the first entrance to
    // the function since this is a const static.
    const static string names[] = {
            "GDP",
            "Retained Earnings",
            "Subsidy",
            "Corporate Profits",
            "Corporate Retained Earnings",
            "Corporate Income Taxes",
            "Corporate Income Tax Rate",
            "Personal Income Taxes",
            "Investment Tax Credit",
            "Dividends",
            "Labor Wages",
            "Land Rents",
            "Transfers",
            "Social Security Tax",
            "Indirect Buisness Tax",
            "GNP",
            "GNP VA",
            "Consumption",
            "Government",
            "Investment",
            "Net Export",
            "Exchange Rate",
            "Annual Investment" 
    };
    // Return the string in the static array at the index.
    return names[ aType ];
}

/*! \brief Convert between the NationalAccount enum type to the XML String representation
 *
 * \author Pralit Patel
 * \param aType The enum NationalAccount type
 * \return The XML string representation of the type
 */
const string& NationalAccount::enumToXMLName( const AccountType aType ) const {
    /*! \pre aType is a valid account type. */
    assert( aType < END );
    // Create a static array of values. This will only on the first entrance to
    // the function since this is a const static.

    const static string names[] = {
            "GDP",
            "retainedEarning",
            "subsidy",
            "corpProfits",
            "corpRetainedEarnings",
            "corporateIncomeTax",
            "corporateIncomeTaxRate",
            "personalIncomeTax",
            "investmentTaxCredit",
            "dividends",
            "laborWages",
            "landRents",
            "transfers",
            "socialSecurityTax",
            "indirectBusinessTax",
            "GNP",
            "GNPVA",
            "consumption",
            "government",
            "investment",
            "netExport",
            "exchangeRate",
            "annualInvestment"
    };
    // Return the string in the static array at the index.
    return names[ aType ];
}

// for reporting National Account information
void NationalAccount::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitNationalAccount( this, aPeriod );
    aVisitor->endVisitNationalAccount( this, aPeriod );
}
