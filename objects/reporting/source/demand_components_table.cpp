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
* \file demand_components_table.cpp
* \ingroup Objects
* \brief The DemandComponentsTable source file.
*
*  Detailed description.
*
* \author Pralit Patel
* \author Sonny Kim
* \author Josh Lurz
*/
#include "util/base/include/definitions.h"
#include "reporting/include/demand_components_table.h"
#include "containers/include/region_cge.h"
#include "sectors/include/factor_supply.h"
#include "consumers/include/household_consumer.h"
#include "consumers/include/govt_consumer.h"
#include "consumers/include/trade_consumer.h"
#include "consumers/include/invest_consumer.h"
#include "technologies/include/production_technology.h"
#include "functions/include/input.h"
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"
#include "reporting/include/storage_table.h"
#include "sectors/include/sector.h"

using namespace std;

extern Scenario* scenario;

//! Default Constructor
DemandComponentsTable::DemandComponentsTable( ostream& aFile ):
mFile( aFile ),
mTable( new StorageTable ){
}

/*! \brief For outputing SGM data to a flat csv File
*/
void DemandComponentsTable::finish() const {

    mFile << "Demand Components Table" << endl << "Industry" << ',';
    // Now output the table column labels.
    const vector<string> cols = mTable->getColLabels();
    for( vector<string>::const_iterator col = cols.begin(); col != cols.end(); ++col ){
        mFile << *col <<','; // output the label.
    }
    mFile << endl;

    // Note: This is structurly different from SAM. This goes through the rows and prints
    // out each of the category values.
    mFile.precision(0);

    // Get the row labels.
    const vector<string> rows = mTable->getRowLabels();

    // Loop through each row and print all the columns.
    for( vector<string>::const_iterator row = rows.begin(); row != rows.end(); ++row ){
        mFile << *row; // output the row label.
        for( vector<string>::const_iterator col = cols.begin(); col != cols.end(); ++col ){
            mFile << ',' << mTable->getValue( *row, *col ); // output the value.
        }
        mFile << endl;
    }
    mFile << endl;
    // reset format to default
    mFile.precision(3);
}

void DemandComponentsTable::startVisitRegionCGE( const RegionCGE* regionCGE, const int aPeriod ) {
    // Add columns to the table.
    for( int i = TOTAL; i <= TRADE; ++i ){
        mTable->addColumn( getLabel( CategoryType( i ) ) );
    }
    // Add the rows in the right order.
    // This has to be done explicitally because the rows are added later by input, so the
    // ordering would be wrong.
    for( unsigned int i = 0; i < regionCGE->supplySector.size(); ++i ){
        mTable->addToType( regionCGE->supplySector[ i ]->getName(), "Trade", 0 ); // Column doesn't matter.
    }
    // Add the factor supplies.
    for( unsigned int i = 0; i < regionCGE->factorSupply.size(); ++i ){
        mTable->addToType( regionCGE->factorSupply[ i ]->getName(), "Trade", 0 );
    }
}

void DemandComponentsTable::startVisitHouseholdConsumer( const HouseholdConsumer* householdConsumer,
                                                        const int aPeriod )
{
    // add only current year consumer
    if( householdConsumer->year == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
        for( unsigned int i = 0; i < householdConsumer->input.size(); ++i ){
            mTable->addToType( householdConsumer->input[ i ]->getName(), getLabel( CONSUMPTION ),
                householdConsumer->input[ i ]->getDemandCurrency( aPeriod ) );
        }
    }
}

void DemandComponentsTable::startVisitGovtConsumer( const GovtConsumer* govtConsumer,
                                                   const int aPeriod )
{
    // add only current year consumer
    if( govtConsumer->year == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
        for( unsigned int i = 0; i < govtConsumer->input.size(); ++i ){
            mTable->addToType( govtConsumer->input[ i ]->getName(), getLabel( GOVERNMENT ),
                govtConsumer->input[ i ]->getDemandCurrency( aPeriod ) );
        }
    }
}

void DemandComponentsTable::startVisitTradeConsumer( const TradeConsumer* tradeConsumer, const int aPeriod )
{
    // add only current year consumer
    if( tradeConsumer->year == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
        for( unsigned int i = 0; i< tradeConsumer->input.size(); i++ ){
            mTable->addToType( tradeConsumer->input[ i ]->getName(), getLabel( TRADE ),
                tradeConsumer->input[ i ]->getDemandCurrency( aPeriod ) );
        }
    }
}

void DemandComponentsTable::startVisitInvestConsumer( const InvestConsumer* investConsumer,
                                                     const int aPeriod )
{
    // add only current year consumer
    if( investConsumer->year == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
        for( unsigned int i = 0; i < investConsumer->input.size(); ++i ){
            mTable->addToType( investConsumer->input[ i ]->getName(), getLabel( INVESTMENT ),
                investConsumer->input[ i ]->getDemandCurrency( aPeriod ) );
        }
    }
}

void DemandComponentsTable::startVisitProductionTechnology( const ProductionTechnology* prodTech,
                                                           const int aPeriod ) 
{
    if( aPeriod == -1 || ( prodTech->isAvailable( aPeriod ) && !prodTech->isRetired( aPeriod ) ) ) {
        for( unsigned int i=0; i<prodTech->input.size(); i++ ){
            // if capital, add only current vintage demand
            if( prodTech->input[i]->getName() == "Capital" ) {
                if( prodTech->getYear() == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
                    mTable->addToType( prodTech->input[ i ]->getName(), getLabel( INTERMED ),
                        prodTech->input[ i ]->getDemandCurrency( aPeriod ) );
                }
            }
            // for all other inputs, add all vintages
            else {
                mTable->addToType( prodTech->input[ i ]->getName(), getLabel( INTERMED ),
                    prodTech->input[ i ]->getDemandCurrency( aPeriod ) );
            }
        }
    }
}

/*! \brief Determine the category label based on the enum type value.
* \param aType The type of category.
* \return The label for the category. 
*/
const string& DemandComponentsTable::getLabel( const CategoryType aType ) const {
    // Setup a static array with the labels in the correct positions.
    // This will only be done on the first call to the function.
    const static string labels[] = 
    { "Total", "Intermediate Production", "Consumption", "Investment", "Government", "Trade" };

    // Return the correct label.
    return labels[ aType ];
}
