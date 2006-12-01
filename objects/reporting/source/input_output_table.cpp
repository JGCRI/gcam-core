/*! 
* \file input_output_table.cpp
* \ingroup Objects
* \brief The InputOutputTable class source file.
*
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"

#include <string>
#include <vector>

#include "reporting/include/input_output_table.h"
#include "functions/include/input.h"
#include "functions/include/demand_input.h"
#include "functions/include/production_input.h"
#include "technologies/include/production_technology.h"
#include "reporting/include/storage_table.h"
#include "sectors/include/sector.h"
#include "sectors/include/factor_supply.h"
#include "containers/include/region_cge.h"
#include "containers/include/scenario.h" // only for modeltime
#include "util/base/include/model_time.h"
#include "consumers/include/consumer.h"

extern Scenario* scenario;

using namespace std;

//! Default Constructor
InputOutputTable::InputOutputTable( const string& aRegionName, ostream& aFile ):
mFile( aFile ),
mRegionName( aRegionName ),
mInternalTable( new StorageTable ),
mParsingConsumer( false ),
mUseInput( false ){
}

/*! \brief Output the IOTable to a CSV
 * 
 * \author Josh Lurz
 */
void InputOutputTable::finish() const {
    mFile << "-----------------------------" << endl;
    mFile << "Regional Input Output Table" << endl;
    mFile << "-----------------------------" << endl << endl;

    // Get the column labels which are sector names.
    const vector<string> colNames = mInternalTable->getColLabels();
    for( vector<string>::const_iterator col = colNames.begin(); col != colNames.end(); ++col ){
        mFile << ',' << *col;
    }   
    mFile << endl;

    // Write out each row of the IOTable.
    const vector<string> rowNames = mInternalTable->getRowLabels();
    for ( vector<string>::const_iterator row = rowNames.begin(); row != rowNames.end(); ++row ){
        mFile << *row;
        // Iterate through the columns.
        for( vector<string>::const_iterator col = colNames.begin(); col != colNames.end(); ++col ) {
            mFile << ',' << mInternalTable->getValue( *row, *col );
        }
        mFile << endl;
    }
    mFile << endl;
}

//! Update the region. 
void InputOutputTable::startVisitRegionCGE( const RegionCGE* aRegion, const int aPeriod ){
    // Loop through the sectors and add a blank item for each just to set the ordering.
    for( unsigned int i = 0; i < aRegion->supplySector.size(); ++i ){
        mInternalTable->setType( aRegion->supplySector[ i ]->getName(), aRegion->supplySector[ i ]->getName(), 0 );
    }
    // Add factor supplies at the end. right?
    for( unsigned int i = 0; i < aRegion->factorSupply.size(); ++i ){
        mInternalTable->setType( aRegion->factorSupply[ i ]->getName(), aRegion->factorSupply[ i ]->getName(), 0 );
    }
}

void InputOutputTable::startVisitSector( const Sector* sector, const int aPeriod ) {
    // Add a column for ourselves.
    mInternalTable->addColumn( sector->getName() );

    // Store the sector name as we'll need it at the technology level.
    mCurrSectorName = sector->getName();
}

void InputOutputTable::startVisitProductionTechnology( const ProductionTechnology* prodTechnology,
                                                   const int aPeriod )
{
    if( aPeriod == -1 || ( prodTechnology->isAvailable( aPeriod ) && 
        !prodTechnology->isRetired( aPeriod ) ) ) {
            mUseInput = true;
            // Add the technologies output as a negative demand on the diagonal.
            mInternalTable->addToType( mCurrSectorName, mCurrSectorName, -1 * prodTechnology->getOutput( aPeriod ) );
            mInternalTable->addToType( "Capital", mCurrSectorName, prodTechnology->getAnnualInvestment( aPeriod ) );

            // Everything else will be updated at the input level.
            mParsingConsumer = false; // set that we aren't currently parsing a consumer.
        }
    else {
        mUseInput = false;
    }
}

void InputOutputTable::startVisitFactorSupply( const FactorSupply* factorSupply, const int aPeriod ){
    // Add factor supplies to the household column.
    mInternalTable->addToType( factorSupply->getName(), "Household", 
        -1 * factorSupply->getSupply( mRegionName, aPeriod ) );
}

//! Update the inputs contribution to the IOTable.
void InputOutputTable::startVisitProductionInput( const ProductionInput* aProdInput, 
                                                 const int aPeriod )
{
    if( mUseInput ) {
        // Add the currency demand.
        // The capital row is not truely capital but other value added.
        // The row is only the OVA row in ProductionSectors, it behaves as capital in Consumers.
        if( aProdInput->isCapital() && !mParsingConsumer ){
            mInternalTable->addToType( "OVA", mCurrSectorName, 
                aProdInput->getDemandCurrency( aPeriod ) );
        }
        else {
            mInternalTable->addToType( aProdInput->getName(), mCurrSectorName, 
                aProdInput->getDemandCurrency( aPeriod ) );
        }
    }
}

//! Update the inputs contribution to the IOTable.
void InputOutputTable::startVisitDemandInput( const DemandInput* aDemandInput, const int aPeriod ){
    if( mUseInput ) {
        mInternalTable->addToType( aDemandInput->getName(), mCurrSectorName, 
            aDemandInput->getDemandCurrency( aPeriod ) );
    }
}

//! Update the consumer. Set that the current state is parsing a consumer, not a production tech.
void InputOutputTable::startVisitConsumer( const Consumer* aConsumer, const int aPeriod ){
    if( aConsumer->getYear() == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
        mParsingConsumer = true;
        mUseInput = true;
    }
    else {
        mUseInput = false;
    }
}
