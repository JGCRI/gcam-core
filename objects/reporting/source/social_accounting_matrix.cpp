/*
This software, which is provided in confidence, was prepared by employees
of Pacific Northwest National Labratory operated by Battelle Memorial
Institute. Battelle has certain unperfected rights in the software
which should not be copied or otherwise disseminated outside your
organization without the express written authorization from Battelle. All rights to
the software are reserved by Battelle.  Battelle makes no warranty,
express or implied, and assumes no liability or responsibility for the 
use of this software.
*/

/*! 
* \file social_accounting_matrix.cpp
* \ingroup Objects
* \brief The SocialAccountingMatrix class source file.
*
* \author Praelit Patel
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <string>

#include "reporting/include/social_accounting_matrix.h"

#include "containers/include/region.h"
#include "sectors/include/sector.h"
#include "sectors/include/subsector.h"
#include "technologies/include/base_technology.h"
#include "consumers/include/consumer.h"
#include "consumers/include/household_consumer.h"
#include "consumers/include/govt_consumer.h"
#include "consumers/include/trade_consumer.h"
#include "consumers/include/invest_consumer.h"
#include "technologies/include/production_technology.h"
#include "functions/include/input.h"
#include "sectors/include/factor_supply.h"
#include "reporting/include/storage_table.h"

using namespace std;

//! Default Constructor
SocialAccountingMatrix::SocialAccountingMatrix( const string& aRegionName ):
mRegionName( aRegionName ), mTable( new StorageTable ){
}

/*! \brief For outputing SGM data to a flat csv File
 * 
 * \author Pralit Patel
 * \param period The period which we are outputing for
 */
void SocialAccountingMatrix::output( ostream& aFile, const int period ) const {
	aFile << "Social Accounting Matrix" << endl
	      << "Receipts" << ',' << "Activites" << ',' << "Commoditites" << ',' << "Land" << ',' 
          << "Labor" << ',' << "Capital" << ',' << "Households" << ',' << "Enterprises" << ',' 
          << "Government" << ',' << "Capital Account" << ',' << "Rest Of World" << ',' 
          << "Totals" << endl;

	for ( int categoryIndex = ACTIVITIES; categoryIndex <= TOTALS; categoryIndex++ ) {
        mTable->addColumn( getString( CategoryType( categoryIndex ) ) );
		aFile << getString( CategoryType( categoryIndex ) );
        for( int sectorIndex = ACTIVITIES; sectorIndex <= TOTALS; sectorIndex++ ) {
			if( !( sectorIndex == TOTALS && categoryIndex == TOTALS ) ) {
				aFile << ',' << mTable->getValue( getString( CategoryType ( sectorIndex ) ),
                    getString( CategoryType( categoryIndex ) ) );
			}
		}
		aFile << endl;
	}
	aFile << endl;
}

void SocialAccountingMatrix::updateHouseholdConsumer( const HouseholdConsumer* householdConsumer,
                                                      const int aPeriod ) 
{
	// for household column of SAM
	addToType( HOUSEHOLDS, COMMODITIES,
        householdConsumer->expenditure.getValue( Expenditure::CONSUMPTION ) );
	addToType( HOUSEHOLDS, HOUSEHOLDS,
        householdConsumer->expenditure.getValue( Expenditure::TRANSFERS ) );
	addToType( HOUSEHOLDS, GOVERNMENT,
        householdConsumer->expenditure.getValue( Expenditure::DIRECT_TAXES ) );
	addToType( HOUSEHOLDS, CAPITAL_ACCOUNT,
        householdConsumer->expenditure.getValue( Expenditure::SAVINGS ) );
}

void SocialAccountingMatrix::updateGovtConsumer( const GovtConsumer* govtConsumer,
                                                 const int aPeriod )
{
	// for government column of SAM
	addToType( GOVERNMENT, ACTIVITIES,
        govtConsumer->expenditure.getValue( Expenditure::SUBSIDY ) );
	addToType( GOVERNMENT, COMMODITIES,
        govtConsumer->expenditure.getValue( Expenditure::CONSUMPTION ) );
	addToType( GOVERNMENT, HOUSEHOLDS,
        govtConsumer->expenditure.getValue( Expenditure::TRANSFERS ) );
	addToType( GOVERNMENT, CAPITAL_ACCOUNT,
        govtConsumer->expenditure.getValue( Expenditure::SAVINGS ) );	
}

void SocialAccountingMatrix::updateTradeConsumer( const TradeConsumer* tradeConsumer,
                                                  const string& aRegionName, const int aPeriod )
{
	// for net export or rest of world column of SAM
	addToType( REST_OF_WORLD, ACTIVITIES,
        tradeConsumer->expenditure.getValue( Expenditure::TOTAL_IMPORTS ) );
}

void SocialAccountingMatrix::updateInvestConsumer( const InvestConsumer* investConsumer, 
                                                   const int aPeriod )
{
	// for capital account column of SAM
	addToType( CAPITAL_ACCOUNT, COMMODITIES,
        investConsumer->expenditure.getValue( Expenditure::INVESTMENT ) );
}

void SocialAccountingMatrix::updateProductionTechnology( const ProductionTechnology* prodTech, 
		const string& aRegionName, const string& aSectorName, const int aPeriod ) 
{
	// for activities column of SAM
	addToType( ACTIVITIES, COMMODITIES,
        prodTech->expenditure.getValue( Expenditure::INTERMEDIATE_INPUTS ) );
	addToType( ACTIVITIES, FACTORS_LABOR, prodTech->expenditure.getValue( Expenditure::WAGES ) );
	addToType( ACTIVITIES, FACTORS_LAND, prodTech->expenditure.getValue( Expenditure::LAND_RENTS ) );
	addToType( ACTIVITIES, FACTORS_CAPITAL, prodTech->expenditure.getValue( Expenditure::RENTALS ) );
	addToType( FACTORS_CAPITAL, ENTERPRISES, prodTech->expenditure.getValue( Expenditure::RENTALS ) );
	addToType( ACTIVITIES, GOVERNMENT, prodTech->expenditure.getValue( Expenditure::INDIRECT_TAXES ) );
	
    // for commodities column of SAM
	addToType( COMMODITIES, ACTIVITIES, prodTech->expenditure.getValue( Expenditure::SALES ) );
	addToType( COMMODITIES, GOVERNMENT, prodTech->expenditure.getValue( Expenditure::TARIFFS ) );
	addToType( COMMODITIES, REST_OF_WORLD, prodTech->expenditure.getValue( Expenditure::IMPORTS ) );
	
    // for enterprise column of SAM
	addToType( ENTERPRISES, HOUSEHOLDS, prodTech->expenditure.getValue( Expenditure::DIVIDENDS ) );
	addToType( ENTERPRISES, GOVERNMENT, prodTech->expenditure.getValue( Expenditure::DIRECT_TAXES ) );
	addToType( ENTERPRISES, CAPITAL_ACCOUNT,
        prodTech->expenditure.getValue( Expenditure::RETAINED_EARNINGS ) );
}

void SocialAccountingMatrix::updateFactorSupply( const FactorSupply* factorSupply,
                                                 const int period )
{	
	double tempSupply = factorSupply->getSupply( mRegionName, period );

	if( factorSupply->getName() == "Land" ) {
		// for land column of SAM
		addToType( FACTORS_LAND, HOUSEHOLDS, tempSupply );
	}
	else if( factorSupply->getName() == "Labor" ) {
		// for labor column of SAM
		addToType( FACTORS_LABOR, HOUSEHOLDS, tempSupply );
	}
	// for factors_capital column see updateProductionTechnology
    // Is this right?
}

//! Helper function which converts enums to strings and writes to the internal table.
void SocialAccountingMatrix::addToType( CategoryType aRowCat, CategoryType aColCat, double aValue ){
    assert( mTable.get() );
    mTable->addToType( getString( aRowCat ), getString( aColCat ), aValue );
}

const string& SocialAccountingMatrix::getString( const CategoryType aType ) {
    // Create a static array of labels in order of their enum.
    const static string labels[] = { "Activities", "Commodities", "Factors: Land", "Factors: Labor",
                                     "Factors: Capital", "Households", "Enterprises", "Government",
                                     "Capital Account", "Rest of world", "Total" };
    // Return the correct label based on the enum value.
    return labels[ aType ];
}
