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
* \file social_accounting_matrix.cpp
* \ingroup Objects
* \brief The SocialAccountingMatrix class source file.
*
* \author Praelit Patel
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <string>

#include "reporting/include/social_accounting_matrix.h"

#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
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
extern Scenario* scenario;

//! Default Constructor
SocialAccountingMatrix::SocialAccountingMatrix( const string& aRegionName, ostream& aFile ):
mFile( aFile ), mRegionName( aRegionName ), mTable( new StorageTable ){
}

/*! \brief For outputting SGM data to a flat csv File
 * 
 * \author Pralit Patel
 */
void SocialAccountingMatrix::finish() const {
    mFile << "Social Accounting Matrix" << endl
          << "Receipts" << ',' << "Activites" << ',' << "Commoditites" << ',' << "Land" << ',' 
          << "Labor" << ',' << "Capital" << ',' << "Households" << ',' << "Enterprises" << ',' 
          << "Government" << ',' << "Capital Account" << ',' << "Rest Of World" << ',' 
          << "Totals" << endl;

    for ( int categoryIndex = ACTIVITIES; categoryIndex <= TOTALS; categoryIndex++ ) {
        mTable->addColumn( getString( CategoryType( categoryIndex ) ) );
        mFile << getString( CategoryType( categoryIndex ) );
        for( int sectorIndex = ACTIVITIES; sectorIndex <= TOTALS; sectorIndex++ ) {
            if( !( sectorIndex == TOTALS && categoryIndex == TOTALS ) ) {
                mFile << ',' << mTable->getValue( getString( CategoryType ( sectorIndex ) ),
                    getString( CategoryType( categoryIndex ) ) );
            }
        }
        mFile << endl;
    }
    mFile << endl;
}

void SocialAccountingMatrix::startVisitHouseholdConsumer( const HouseholdConsumer* householdConsumer,
                                                      const int aPeriod ) 
{
    if( householdConsumer->year == scenario->getModeltime()->getper_to_yr( aPeriod ) ){
        // for household column of SAM
        addToType( HOUSEHOLDS, COMMODITIES,
            householdConsumer->expenditures[ aPeriod ].getValue( Expenditure::CONSUMPTION ) );
        addToType( HOUSEHOLDS, HOUSEHOLDS,
            householdConsumer->expenditures[ aPeriod ].getValue( Expenditure::TRANSFERS ) );
        addToType( HOUSEHOLDS, GOVERNMENT,
            householdConsumer->expenditures[ aPeriod ].getValue( Expenditure::DIRECT_TAXES ) );
        addToType( HOUSEHOLDS, CAPITAL_ACCOUNT,
            householdConsumer->expenditures[ aPeriod ].getValue( Expenditure::SAVINGS ) );
    }
}

void SocialAccountingMatrix::startVisitGovtConsumer( const GovtConsumer* govtConsumer,
                                                 const int aPeriod )
{
    if( govtConsumer->year == scenario->getModeltime()->getper_to_yr( aPeriod ) ){
        // for government column of SAM
        addToType( GOVERNMENT, ACTIVITIES,
            govtConsumer->expenditures[ aPeriod ].getValue( Expenditure::SUBSIDY ) );
        addToType( GOVERNMENT, COMMODITIES,
            govtConsumer->expenditures[ aPeriod ].getValue( Expenditure::CONSUMPTION ) );
        addToType( GOVERNMENT, HOUSEHOLDS,
            govtConsumer->expenditures[ aPeriod ].getValue( Expenditure::TRANSFERS ) );
        addToType( GOVERNMENT, CAPITAL_ACCOUNT,
            govtConsumer->expenditures[ aPeriod ].getValue( Expenditure::SAVINGS ) );   
    }
}

void SocialAccountingMatrix::startVisitTradeConsumer( const TradeConsumer* tradeConsumer, const int aPeriod )
{
    if ( tradeConsumer->year == scenario->getModeltime()->getper_to_yr( aPeriod ) ) {
        // for net export or rest of world column of SAM
        addToType( REST_OF_WORLD, ACTIVITIES,
            tradeConsumer->expenditures[ aPeriod ].getValue( Expenditure::TOTAL_IMPORTS ) );
    }
}

void SocialAccountingMatrix::startVisitInvestConsumer( const InvestConsumer* investConsumer, 
                                                   const int aPeriod )
{
    if( investConsumer->year == scenario->getModeltime()->getper_to_yr( aPeriod ) ){
        // for capital account column of SAM
        addToType( CAPITAL_ACCOUNT, COMMODITIES,
            investConsumer->expenditures[ aPeriod ].getValue( Expenditure::INVESTMENT ) );
    }
}

void SocialAccountingMatrix::startVisitProductionTechnology( const ProductionTechnology* prodTech,
                                                         const int aPeriod ) 
{
    if( aPeriod == -1 || ( prodTech->isAvailable( aPeriod ) && !prodTech->isRetired( aPeriod ) ) ) {
        // for activities column of SAM
        addToType( ACTIVITIES, COMMODITIES,
            prodTech->expenditures[ aPeriod ].getValue( Expenditure::INTERMEDIATE_INPUTS ) );
        addToType( ACTIVITIES, FACTORS_LABOR, prodTech->expenditures[ aPeriod ].getValue( Expenditure::WAGES ) );
        addToType( ACTIVITIES, FACTORS_LAND, prodTech->expenditures[ aPeriod ].getValue( Expenditure::LAND_RENTS ) );
        addToType( ACTIVITIES, FACTORS_CAPITAL, prodTech->expenditures[ aPeriod ].getValue( Expenditure::RENTALS ) );
        addToType( FACTORS_CAPITAL, ENTERPRISES, prodTech->expenditures[ aPeriod ].getValue( Expenditure::RENTALS ) );
        addToType( ACTIVITIES, GOVERNMENT, prodTech->expenditures[ aPeriod ].getValue( Expenditure::INDIRECT_TAXES ) );

        // for commodities column of SAM
        addToType( COMMODITIES, ACTIVITIES, prodTech->expenditures[ aPeriod ].getValue( Expenditure::SALES ) );
        addToType( COMMODITIES, GOVERNMENT, prodTech->expenditures[ aPeriod ].getValue( Expenditure::TARIFFS ) );
        addToType( COMMODITIES, REST_OF_WORLD, prodTech->expenditures[ aPeriod ].getValue( Expenditure::IMPORTS ) );

        // for enterprise column of SAM
        addToType( ENTERPRISES, HOUSEHOLDS, prodTech->expenditures[ aPeriod ].getValue( Expenditure::DIVIDENDS ) );
        addToType( ENTERPRISES, GOVERNMENT, prodTech->expenditures[ aPeriod ].getValue( Expenditure::DIRECT_TAXES ) );
        addToType( ENTERPRISES, CAPITAL_ACCOUNT,
            prodTech->expenditures[ aPeriod ].getValue( Expenditure::RETAINED_EARNINGS ) );
    }
}

void SocialAccountingMatrix::startVisitFactorSupply( const FactorSupply* factorSupply,
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
    // for factors_capital column see startVisitProductionTechnology
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
