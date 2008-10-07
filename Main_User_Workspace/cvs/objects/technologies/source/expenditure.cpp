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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
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
#include "util/base/include/ivisitor.h"

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
    XMLWriteOpeningTag( "expenditure", out, tabs );

    for( int i = 0; i < Expenditure::END; ++i ) {
        XMLWriteElement( getValue( static_cast< Expenditure::ExpenditureType >( i ) ),
            enumToXMLName( static_cast< Expenditure::ExpenditureType >( i ) ),
            out, tabs );
    }

    XMLWriteClosingTag( "expenditure", out, tabs );
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

/*! \brief For outputting SGM data to a flat csv File
* 
* \author Pralit Patel
* \param period The period which we are outputting for
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

/*! \brief Convert between the Expenditure enum type to the XML String representation
 *
 * \author Pralit Patel
 * \param aType The enum Expenditure type
 * \return The XML string representation of the type
 */
const string& Expenditure::enumToXMLName( const ExpenditureType aType ) const {
    assert( aType < END );
    
    // Create a static array of type names. Since this is a const static it will
    // only occur once.

    const static string names[] = {
        "social-security-tax",
            "savings",
            "taxable-income",
            "direct-taxes",
            "transfers",
            "disposable-income",
            "consumption",
            "income",
            "budget",
            "subsidy",
            "investment",
            "total-imports",
            "dividends",
            "retained-earnings",
            "indirect-taxes",
            "intermediate-input",
            "wages",
            "land-rents",
            "rentals",
            "tariffs",
            "imports",
            "sales",
            "costs"
    };
    // Index into the array to find the right name.
    return names[ aType ];
}

void Expenditure::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitExpenditure( this, aPeriod );
    aVisitor->endVisitExpenditure( this, aPeriod );
}
