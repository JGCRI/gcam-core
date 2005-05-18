/*! 
* \file govt_results.cpp
* \ingroup Objects
* \brief The GovtResults class source file.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"

#include <string>
#include <vector>
#include "reporting/include/govt_results.h"
#include "technologies/include/production_technology.h"
#include "reporting/include/storage_table.h"
#include "sectors/include/production_sector.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "consumers/include/govt_consumer.h"
#include "consumers/include/household_consumer.h"

using namespace std;

extern Scenario* scenario; // for marketplace. Remove if unneeded and includes.

//! Default Constructor
GovtResults::GovtResults( const string& aRegionName ):
mRegionName( aRegionName ),
mTaxReceipts( new StorageTable ),
mSubsidies( new StorageTable ),
mGovtExpenditures( new StorageTable ),
mGovtTransfers( 0 ),
mParsingGovt( false ){
}

/*! \brief Output the Government Sector Results table to a CSV
 * 
 * \author Josh Lurz
 * \param aPeriod The aPeriod which we are outputing for
 */
void GovtResults::output( ostream& aFile, const int aPeriod ) const {
    aFile << "-----------------------------" << endl;
    aFile << "Government Sector Results Table" << endl;
    aFile << "-----------------------------" << endl << endl;
    
    // Block out the code which writes out tax receipts.
    {
        // Write out the tax receipts table.
        aFile << "Tax receipts by Sector" << endl;

        // Get the column labels for the taxes table which are types of taxes.
        const vector<string> colNames = mTaxReceipts->getColLabels();
        for( vector<string>::const_iterator col = colNames.begin(); col != colNames.end(); ++col ){
            aFile << ',' << *col;
        }	
        aFile << endl;

        // Write out each row of the table, representing one sector
        const vector<string> rowNames = mTaxReceipts->getRowLabels();
        for ( vector<string>::const_iterator row = rowNames.begin(); row != rowNames.end(); ++row ){
            aFile << *row;
            // Iterate through the columns.
            for( vector<string>::const_iterator col = colNames.begin(); col != colNames.end(); ++col ) {
                aFile << ',' << mTaxReceipts->getValue( *row, *col );
            }
            aFile << endl;
        }
        aFile << endl;
    }
    // Block out the code which writes the subsidies table.
    {
        // Write out the subsidies table.
        aFile << "Subsidies by Sector" << endl;
        
        aFile << "Sector";
        // Get the column labels for the subsidies table.
        const vector<string> colNames = mSubsidies->getColLabels();
        for( vector<string>::const_iterator col = colNames.begin(); col != colNames.end(); ++col ){
            aFile << ',' << *col;
        }	
        aFile << endl;

        // Write out each row of the table, representing one sector
        const vector<string> rowNames = mSubsidies->getRowLabels();
        for ( vector<string>::const_iterator row = rowNames.begin(); row != rowNames.end(); ++row ){
            aFile << *row;
            // Iterate through the columns.
            for( vector<string>::const_iterator col = colNames.begin(); col != colNames.end(); ++col ) {
                aFile << ',' << mSubsidies->getValue( *row, *col );
            }
            aFile << endl;
        }
        aFile << endl;
    }
    
    // Write out total transfers.
    aFile << "Government Transfers, " << mGovtTransfers << endl << endl;

    // Write out expenditures.
}

//! Update the CGE Region
void GovtResults::updateRegionCGE( const RegionCGE* aRegionCGE ){
    // Add the column labels to all the tables.
    mTaxReceipts->addColumn( "Proportional Tax" ); // what is the AND in Legacy? Additive taxes?
    mTaxReceipts->addColumn( "Social Security Tax" );
    mTaxReceipts->addColumn( "Corporate Tax" ); // whats the "And" in Legacy?
    mTaxReceipts->addColumn( "IBT" );
    mTaxReceipts->addColumn( "Carbon" );
    mTaxReceipts->addColumn( "ITC" );

    mSubsidies->addColumn( "Amount" );

    mGovtExpenditures->addColumn( "Price" );
    mGovtExpenditures->addColumn( "Physical Quantity" );
    mGovtExpenditures->addColumn( "Amount" );
}

void GovtResults::updateSector( const Sector* aSector ){    
    // Store the sector name as we'll need it at the technology level.
    // This has to be done here instead of updateProductionSector because that is called
    // after all the technologies are updated.
    mCurrSectorName = aSector->getName();
}

void GovtResults::updateProductionTechnology( const ProductionTechnology* aProdTechnology, 
		const string& aRegionName, const string& aSectorName, const int aPeriod )
{
    mParsingGovt = false;
    // Fill up the tax table.
    // This isn't complete yet, its zero anyway right now.
    mTaxReceipts->addToType( mCurrSectorName, "Proportional Tax", 0 );
    
    mTaxReceipts->addToType( mCurrSectorName, "Social Security Tax",
        aProdTechnology->expenditure.getValue( Expenditure::SOCIAL_SECURITY_TAX ) );
    
    // this would be wrong if more was added to DIRECT_TAXES. 
    mTaxReceipts->addToType( mCurrSectorName, "Corporate Tax",
        aProdTechnology->expenditure.getValue( Expenditure::DIRECT_TAXES ) );
    
    // this would be wrong if more was added to indirect taxes.
    mTaxReceipts->addToType( mCurrSectorName, "IBT",
        aProdTechnology->expenditure.getValue( Expenditure::INDIRECT_TAXES ) );

    // these two aren't finished yet.
    mTaxReceipts->addToType( mCurrSectorName, "Carbon", 0 );
    mTaxReceipts->addToType( mCurrSectorName, "ITC", 0 );

    // Fill up the subsidy table. This isn't done per sector in the model yet.
    mSubsidies->addToType( mCurrSectorName, "Amount", 0 );
}    

void GovtResults::updateGovtConsumer( const GovtConsumer* aGovtConsumer, const int aPeriod ){
    // Set the total transfers.
    mGovtTransfers = aGovtConsumer->expenditure.getValue( Expenditure::TRANSFERS );

    // Need to setup the expenditure table.
    mParsingGovt = true;
}

void GovtResults::updateHouseholdConsumer( const HouseholdConsumer* aHouseholdConsumer, const int aPeriod ){
    // Need to get social security tax added.
    mTaxReceipts->addToType( mCurrSectorName, "Social Security Tax",
        aHouseholdConsumer->expenditure.getValue( Expenditure::SOCIAL_SECURITY_TAX ) );

    // Also corporate income tax?
    mTaxReceipts->addToType( mCurrSectorName, "Corporate Tax",
        aHouseholdConsumer->expenditure.getValue( Expenditure::DIRECT_TAXES ) ); // this might be wrong.
}
