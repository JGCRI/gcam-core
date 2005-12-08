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
* \file expenditure.cpp
* \ingroup Objects
* \brief The Expenditure class source file.
*
*  Detailed Description.
*
* \author Pralit Patel
* \author Sonny Kim
* \date $Date$
* \version $Revision$
* \todo This class design could still use some work. Expenditure and National accounts
* should inherit from a base class. A string based enum type could make toDebugXML not have
* to explicitally write everything out, although it would inflate the static class size.-JPL
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <fstream>
#include <string>
#include <map>

#include "technologies/include/expenditure.h"
#include "util/base/include/xml_helper.h"

using namespace std;


//! Default Constructor
Expenditure::Expenditure(){
    mExpenditures.resize( END );
}

//! Add to the value for the national account specified by the account type key. 
void Expenditure::addToType( const ExpenditureType aType, const double aValue ){
    mExpenditures[ aType ]+= aValue;
}

//! Get the value for the national account specified by the account type key. 
double Expenditure::getValue( const ExpenditureType aType ) const {
    return mExpenditures[ aType ];
}

//! Reset all expenditures. This still exposes the underlying map too much.
void Expenditure::reset() {
	mExpenditures.clear();
    mExpenditures.resize( END );
}

//! Output debug info to XML
void Expenditure::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
	// write the beginning tag.
    XMLWriteOpeningTag( "Expenditure", out, tabs );
    XMLWriteElement( getValue( SOCIAL_SECURITY_TAX ), "socialSecurityTax", out, tabs );
    XMLWriteElement( getValue( SAVINGS ), "savings", out, tabs );
    XMLWriteElement( getValue( TAXABLE_INCOME ), "taxableIncome", out, tabs );
    XMLWriteElement( getValue( DIRECT_TAXES ), "directTaxes", out, tabs );
    XMLWriteElement( getValue( TRANSFERS ), "transfers", out, tabs );
    XMLWriteElement( getValue( DISPOSABLE_INCOME ), "disposableIncome", out, tabs );
    XMLWriteElement( getValue( CONSUMPTION ), "consumption", out, tabs );
    XMLWriteElement( getValue( INCOME ), "income", out, tabs );
    XMLWriteElement( getValue( BUDGET ), "budget", out, tabs );
	XMLWriteElement( getValue( SUBSIDY ), "subsidy", out, tabs );
	XMLWriteElement( getValue( INVESTMENT ), "investment", out, tabs );
	XMLWriteElement( getValue( TOTAL_IMPORTS ), "totalImports", out, tabs );
	// for production sectors
	XMLWriteElement( getValue( DIVIDENDS ), "dividends", out, tabs );
	XMLWriteElement( getValue( RETAINED_EARNINGS ), "retainedEarnings", out, tabs );
	XMLWriteElement( getValue( INDIRECT_TAXES ), "indirectTaxes", out, tabs );
	XMLWriteElement( getValue( INTERMEDIATE_INPUTS ), "intermediateInput", out, tabs );
	XMLWriteElement( getValue( WAGES ), "wages", out, tabs );
	XMLWriteElement( getValue( LAND_RENTS ), "landRents", out, tabs );
	XMLWriteElement( getValue( RENTALS ), "rentals", out, tabs );
	XMLWriteElement( getValue( TARIFFS ), "tariffs", out, tabs );
	XMLWriteElement( getValue( IMPORTS ), "imports", out, tabs );
	XMLWriteElement( getValue( SALES ), "sales", out, tabs );
	XMLWriteElement( getValue( COSTS ), "costs", out, tabs );
    XMLWriteClosingTag( "Expenditure", out, tabs );
}

/*! \brief Set the value of the expenditure for the specified type
 *
 * \author Pralit Patel
 * \param aType The type of expenditure to set
 * \param aValue The value the to set in the expenditure
 */
void Expenditure::setType( const ExpenditureType aType, const double aValue ) {
	mExpenditures[ aType ] = aValue;
}

/*! \brief For outputing SGM data to a flat csv File
 * 
 * \author Pralit Patel
 * \param period The period which we are outputing for
 */
void Expenditure::csvSGMOutputFile( ostream& aFile, const int period ) const {
    for( int i = 0; i < END; ++i ){
        // Need to statically cast the index into an expenditure type. Since its
        // starting at zero and stopping below end, this won't fail.
		aFile << enumToName( static_cast<ExpenditureType>( i ) ) << ",";
		// reset format to default
		aFile.setf( ios_base::fixed, ios_base::floatfield );
		aFile << mExpenditures[ i ] << endl;
	}
}

/*! \brief Convert between the Expenditure enum type to the String representation
 *
 * \author Pralit Patel
 * \param aType The enum Expenditure type
 * \return The string representation of the type
 */
const string& Expenditure::enumToName( const ExpenditureType aType ) const {
    assert( aType < END );
    
    // Create a static array of type names. Since this is a const static it will
    // only occur once.
    const static string names[] = {
        "Social Security Tax",
            "Savings",
            "Taxable Income",
            "Direct Taxes",
            "Transfers",
            "Disposable Income",
            "Consumption",
            "Income",
            "Budget",
            "Subsidy",
			"Investment",
            "Total Imports",
            "Dividends",
            "Retained Earnings",
            "Indirect Taxes",
            "Intermediate Inputs",
            "Wages",
            "Land Rents",
            "Rentals",
            "Tariffs",
            "Imports",
            "Sales",
            "Costs" 
    };
    // Index into the array to find the right name.
    return names[ aType ];
}
