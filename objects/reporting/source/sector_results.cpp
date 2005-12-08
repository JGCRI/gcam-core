/*! 
* \file sector_results.cpp
* \ingroup Objects
* \brief The SectorResults class source file.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"

#include <string>
#include <vector>
#include "reporting/include/sector_results.h"
#include "technologies/include/production_technology.h"
#include "reporting/include/storage_table.h"
#include "sectors/include/production_sector.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "functions/include/function_utils.h"

using namespace std;

extern Scenario* scenario; // for marketplace

//! Default Constructor
SectorResults::SectorResults( const string& aRegionName, ostream& aFile ):
mCurrentRegionName( aRegionName ),
mFile( aFile ),
mInternalTable( new StorageTable ){
}

/*! \brief Output the Sector Results table to a CSV
 * 
 * \author Josh Lurz
 */
void SectorResults::finish() const {
    mFile << "-----------------------------" << endl;
    mFile << "Regional Sector Results Table" << endl;
    mFile << "-----------------------------" << endl << endl;
    
    // The table is stored inverted so that totals are easily calculated.
    // Use row labels to print the columns.
    // Get the column labels which are the types of outputs.
    mFile << "Sector";
    const vector<string> colNames = mInternalTable->getRowLabels();
    for( vector<string>::const_iterator col = colNames.begin(); col != colNames.end(); ++col ){
        mFile << ',' << *col;
    }	
    mFile << endl;

	//aFile.precision(0);
    // Write out each row of the table, representing one sector
    const vector<string> rowNames = mInternalTable->getColLabels();
    for ( vector<string>::const_iterator row = rowNames.begin(); row != rowNames.end(); ++row ){
        mFile << *row;
        // Iterate through the columns.
        for( vector<string>::const_iterator col = colNames.begin(); col != colNames.end(); ++col ) {
            mFile << ',' << mInternalTable->getValue( *col, *row );
        }
        mFile << endl;
    }
    mFile << endl;
}
void SectorResults::startVisitSector( const Sector* aSector, const int aPeriod ){    
    // Store the sector name as we'll need it at the technology level.
    // This has to be done here instead of updateProductionSector because that is called
    // after all the technologies are updated.
    mCurrentSectorName = aSector->getName();
}

void SectorResults::updateProductionSector( const ProductionSector* aProdSector, const int aPeriod ) {
    // Add a column for ourselves.
    mInternalTable->addColumn( aProdSector->getName() );
}

void SectorResults::updateProductionTechnology( const ProductionTechnology* aProdTechnology,
											    const int aPeriod ) 
{
      // Add to the physical output column.
    const Marketplace* marketplace = scenario->getMarketplace();
    double convFactor = Input::getMarketConversionFactor( mCurrentSectorName, mCurrentRegionName );
    mInternalTable->addToType( "Phys. Out.", mCurrentSectorName, aProdTechnology->mOutputs[ aPeriod ] * convFactor );

    // Add to the sales column
    mInternalTable->addToType( "Sales", mCurrentSectorName, aProdTechnology->expenditure.getValue( Expenditure::SALES ) );

	double priceReceived = FunctionUtils::getPriceReceived( mCurrentRegionName, mCurrentSectorName, aPeriod ); 
    // Add to the revenue column.
    mInternalTable->addToType( "Revenue", mCurrentSectorName,
                                aProdTechnology->expenditure.getValue( Expenditure::SALES ) * priceReceived );

    // Add to the profits column.
    mInternalTable->addToType( "Profits", mCurrentSectorName, aProdTechnology->mProfits[ aPeriod ] );

    // Add to the retained earnings column.
    mInternalTable->addToType( "Retained Earnings", mCurrentSectorName,
                               aProdTechnology->expenditure.getValue( Expenditure::RETAINED_EARNINGS ) );

    // Add to the costs column.
    mInternalTable->addToType( "Costs", mCurrentSectorName,
                               aProdTechnology->mCostsReporting[ aPeriod ] );
}
