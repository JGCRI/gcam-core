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
* \file sector_report.cpp
* \ingroup Objects
* \brief The SectorReport class source file.
*
* \author Praelit Patel
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <string>

#include "reporting/include/sector_report.h"
#include "reporting/include/storage_table.h"

#include "sectors/include/sector.h"
#include "technologies/include/production_technology.h"
#include "functions/include/input.h"
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h" // for modeltime
#include "util/base/include/util.h"

extern Scenario* scenario; // for modeltime.

using namespace std;

//! Default Constructor
SectorReport::SectorReport( ostream& aFile ):
mFile( aFile ),
mTable( new StorageTable )
{
}

/*! \brief For outputing SGM data to a flat csv File
*
* \author Sonny Kim
*/
void SectorReport::finish() const {
    if ( !mTable->isEmpty() ){ // only print out for Production Sectors
        mFile << "-----------------------------" << endl;
        mFile << "Sector Report for Production Sector:   " << mSectorName << endl;
        mFile << "-----------------------------" << endl;
        mFile << "Vintages" ;
        mFile << " ";
        // write out column headings (years)
        // Get the list of row names.
        const vector<string> rowNames = mTable->getRowLabels();

        // Get the column labels
        const vector<string> colNames = mTable->getColLabels();
        for( vector<string>::const_iterator col = colNames.begin(); col != colNames.end(); ++col ){
            mFile << ',' << *col;
        }
        mFile << endl;

        // Note: This is structuarlly different from SAM. This goes through the rows and prints
        // out each of the category values.
        for ( vector<string>::const_iterator row = rowNames.begin(); row != rowNames.end(); ++row ){
            mFile << *row;
            // Iterate through the columns.
            for( vector<string>::const_iterator col = colNames.begin(); col != colNames.end(); ++col ) {
                mFile << ',' << mTable->getValue( *row, *col );
            }
            mFile << endl;
        }
        mFile << endl;
    }
}

void SectorReport::startVisitSector( const Sector* sector, const int aPeriod ) {
    mSectorName = sector->getName();
}

//! Update the SectorReport with information from the the ProductionTechnology.
// This function is currently hacked to get ordering right.
void SectorReport::startVisitProductionTechnology( const ProductionTechnology* prodTechnology,
                                                  const int aPeriod )
{
    if( aPeriod == -1 || ( prodTechnology->isAvailable( aPeriod ) &&
        !prodTechnology->isRetired( aPeriod ) ) ) {
            // Create a unique name for the vintage based on the name and year.
            const string currName = util::toString( prodTechnology->getYear() ) + prodTechnology->getName();

            // Add a column for the current tech.
            mTable->addColumn( currName );

            // Add the values for the technology.
            if( prodTechnology->isNewInvestment( aPeriod ) ){
                mTable->addToType( "Annual Investment", currName, prodTechnology->mAnnualInvestment );
                mTable->addToType( "Capital Stock", currName, prodTechnology->capital );
            }
            mTable->addToType( "Output", currName, prodTechnology->mOutputs[ aPeriod ] );
            mTable->addToType( "Profits", currName, prodTechnology->mProfits[ aPeriod ] );
            mTable->addToType( "Costs", currName, prodTechnology->mCostsReporting[ aPeriod ] );
            // This isn't right, retained earnings has values by period.
            mTable->addToType( "Retained Earnings", currName, prodTechnology->expenditures[ aPeriod ].getValue(Expenditure::RETAINED_EARNINGS) );
            if( prodTechnology->isNewInvestment( aPeriod ) ){
                mTable->setType( "Expected Profit Rate", currName, prodTechnology->mExpectedProfitRateReporting );
                // mTable->setType( "Expected Price Received", currName, prodTechnology->mExpectedPriceReceivedReporting );
            }
            const Modeltime* modeltime = scenario->getModeltime();
            // Its questionable if this should be here or in updateInput.
            for( unsigned int i = 0; i < prodTechnology->input.size(); ++i ){
                // report price paid only for new vintage.
                // Since the new vintage is the last vintage, this also allows price paid to be the last column.
                if( prodTechnology->isNewInvestment( aPeriod ) ||
                    // If we are in the base year the base year technology is the newest technology.
                    // There is no new investment.
                    ( aPeriod == modeltime->getBasePeriod() &&
                    modeltime->getper_to_yr( modeltime->getBasePeriod() ) == prodTechnology->year ) )
                {
                    // Add a column to the table.
                    mTable->addColumn( "Price Paid" );
                    mTable->setType( prodTechnology->input[ i ]->getName(), "Price Paid",
                        prodTechnology->input[ i ]->getPricePaid( aPeriod ) );
                }
                mTable->addToType( prodTechnology->input[ i ]->getName(), currName,
                    prodTechnology->input[ i ]->getDemandCurrency( aPeriod ) );
            }
        }
}

