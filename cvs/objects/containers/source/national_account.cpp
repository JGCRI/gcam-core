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
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"

using namespace std;

//! Default Constructor
NationalAccount::NationalAccount():
// Size the accounts to one after the last valid value in the vector,
// represented by END.
mAccounts( END )
{
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

/*!
 * \brief Add to the value for the national account specified by the account type key.
 * \param aType The account which will be added to.
 * \param aValue The value which will be added to account aType.
 */
void NationalAccount::addToAccount( const AccountType aType, const double aValue ){
    mAccounts[ aType ]+= aValue;
}

/*!
 * \brief Set the value for the national account specified by the account type key.
 * \param aType The account for which value will be set.
 * \param aValue The value which wil be set to account aType.
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
    // save transfer amount calculated from government's initCalc
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

/*!
 * \brief Convert between the NationalAccount enum type to the String representation
 * \param aType The enum NationalAccount type
 * \return The string representation of the type
 * \author Pralit Patel
 */
const string& NationalAccount::enumToName( const AccountType aType ) const {
    /*! \pre aType is a valid account type. */
    assert( aType < END );
    // Create a static array of values. This will only on the first entrance to
    // the function since this is a const static.
    const static string names[] = {
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
            "GNP Nominal",
            "GNP VA",
            "GNP Real",
            "Consumption Nominal",
            "Government Nominal",
            "Investment Nominal",
            "Net Export Nominal",
            "Consumption Real",
            "Government Real",
            "Investment Real",
            "Net Export Real",
            "Exchange Rate",
            "Annual Investment",
            "Carbon Tax",
            "Export Nominal",
            "Export Real",
            "Import Nominal",
            "Import Real"
    };
    // Return the string in the static array at the index.
    return names[ aType ];
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
            "GNP-nominal",
            "GNPVA",
            "GNP-real",
            "consumption-nominal",
            "government-nominal",
            "investment-nominal",
            "netExport-nominal",
            "consumption-real",
            "government-real",
            "investment-real",
            "netExport-real",
            "exchangeRate",
            "annualInvestment",
            "carbon-tax",
            "export-nominal",
            "export-real",
            "import-nominal",
            "import-real"
    };
    // Return the string in the static array at the index.
    return names[ aType ];
}

// for reporting National Account information
void NationalAccount::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitNationalAccount( this, aPeriod );
    aVisitor->endVisitNationalAccount( this, aPeriod );
}
