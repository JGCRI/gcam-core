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
SectorResults::SectorResults( const string& aRegionName ):
mRegionName( aRegionName ),
mInternalTable( new StorageTable ){
}

/*! \brief Output the Sector Results table to a CSV
 * 
 * \author Josh Lurz
 * \param aPeriod The aPeriod which we are outputing for
 */
void SectorResults::output( ostream& aFile, const int aPeriod ) const {
    aFile << "-----------------------------" << endl;
    aFile << "Regional Sector Results Table" << endl;
    aFile << "-----------------------------" << endl << endl;
    
    // The table is stored inverted so that totals are easily calculated.
    // Use row labels to print the columns.
    // Get the column labels which are the types of outputs.
    aFile << "Sector";
    const vector<string> colNames = mInternalTable->getRowLabels();
    for( vector<string>::const_iterator col = colNames.begin(); col != colNames.end(); ++col ){
        aFile << ',' << *col;
    }	
    aFile << endl;

	//aFile.precision(0);
    // Write out each row of the table, representing one sector
    const vector<string> rowNames = mInternalTable->getColLabels();
    for ( vector<string>::const_iterator row = rowNames.begin(); row != rowNames.end(); ++row ){
        aFile << *row;
        // Iterate through the columns.
        for( vector<string>::const_iterator col = colNames.begin(); col != colNames.end(); ++col ) {
            aFile << ',' << mInternalTable->getValue( *col, *row );
        }
        aFile << endl;
    }
    aFile << endl;
}
void SectorResults::updateSector( const Sector* aSector ){    
    // Store the sector name as we'll need it at the technology level.
    // This has to be done here instead of updateProductionSector because that is called
    // after all the technologies are updated.
    mCurrSectorName = aSector->getName();
}

void SectorResults::updateProductionSector( const ProductionSector* aProdSector, const int aPeriod ) {
    // Add a column for ourselves.
    mInternalTable->addColumn( aProdSector->getName() );
}

void SectorResults::updateProductionTechnology( const ProductionTechnology* aProdTechnology, 
		const string& aRegionName, const string& aSectorName, const int aPeriod ) 
{
      // Add to the physical output column.
    const Marketplace* marketplace = scenario->getMarketplace();
    double convFactor = Input::getMarketConversionFactor( aSectorName, aRegionName );
    mInternalTable->addToType( "Phys. Out.", mCurrSectorName, aProdTechnology->mOutputs[ aPeriod ] * convFactor );

    // Add to the sales column
    mInternalTable->addToType( "Sales", mCurrSectorName, aProdTechnology->expenditure.getValue( Expenditure::SALES ) );

	double priceReceived = FunctionUtils::getPriceReceived( mRegionName, mCurrSectorName, aPeriod ); 
    // Add to the revenue column.
    mInternalTable->addToType( "Revenue", mCurrSectorName,
                                aProdTechnology->expenditure.getValue( Expenditure::SALES ) * priceReceived );

    // Add to the profits column.
    mInternalTable->addToType( "Profits", mCurrSectorName, aProdTechnology->mProfits[ aPeriod ] );

    // Add to the retained earnings column.
    mInternalTable->addToType( "Retained Earnings", mCurrSectorName,
                               aProdTechnology->expenditure.getValue( Expenditure::RETAINED_EARNINGS ) );

    // Add to the costs column.
    mInternalTable->addToType( "Costs", mCurrSectorName, aProdTechnology->mCostsReporting[ aPeriod ] );
}
