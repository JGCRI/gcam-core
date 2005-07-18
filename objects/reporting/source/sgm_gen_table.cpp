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
* \file sgm_gen_table.cpp
* \ingroup Objects
* \brief The SGMGenTable class source file.
*
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <string>
#include <map>

#include "reporting/include/sgm_gen_table.h"

#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/region.h"
#include "containers/include/region_cge.h"
#include "demographics/include/demographic.h"
#include "sectors/include/sector.h"
#include "sectors/include/production_sector.h"
#include "sectors/include/subsector.h"
#include "emissions/include/ghg.h"
#include "containers/include/national_account.h"
#include "technologies/include/expenditure.h"
#include "technologies/include/base_technology.h"
#include "consumers/include/consumer.h"
#include "consumers/include/household_consumer.h"
#include "consumers/include/govt_consumer.h"
#include "consumers/include/trade_consumer.h"
#include "consumers/include/invest_consumer.h"
#include "technologies/include/production_technology.h"
#include "functions/include/input.h"
#include "sectors/include/factor_supply.h"
#include "util/base/include/model_time.h"

using namespace std;
extern Scenario* scenario;

//! Default Constructor
SGMGenTable::SGMGenTable( const string& aName, const string& aHeader, const Modeltime* aModeltime ): 
OutputContainer(), mName( aName ), mHeader( aHeader ), mModeltime( aModeltime ) {
}

//! Add to the value for the DCT specified by the account type key. 
void SGMGenTable::addToType( const int aTypeRow, const string aTypeCol, const double value ){
	// add to column and row totals here
	// Do not add to totals anywhere else
    mTable[ aTypeRow ][ aTypeCol ] += value;
	// add to total for this type of table only
	mTable[ aTypeRow ][ "zTotal" ] += value;
}

//! set the value for the DCT specified by the account type key. 
void SGMGenTable::setType( const int aTypeRow, const string aTypeCol, const double value ){
    mTable[ aTypeRow ][ aTypeCol ] = value;
}

//! Get the value for the DCT specified by the account type key. 
double SGMGenTable::getValue( const int aTypeRow, const string aTypeCol ) const{
	return util::searchForValue( util::searchForValue( mTable, aTypeRow ), aTypeCol );
}

/*! \brief For outputing SGM data to a flat csv File
 *
 */
void SGMGenTable::output( ostream& aSGMGenFile, int aPeriod ) const {
	if ( !mTable.empty() ){ // don't print if empty

		aSGMGenFile << mHeader << endl;
		// Note: This is structuarlly different from SAM. This goes through the rows and prints
		// out each of the category values.
		// write column labels
		aSGMGenFile << "Year" << ',';
		for( map< string, double>::const_iterator strIter = (*mTable.begin()).second.begin(); strIter != (*mTable.begin()).second.end(); ++strIter ) {
			aSGMGenFile << (*strIter).first << ','; 
		}
		aSGMGenFile << endl;

		for ( std::map<int, map<string, double> >::const_iterator mapIter = mTable.begin(); mapIter != mTable.end(); ++mapIter ) {
			aSGMGenFile << (*mapIter).first;
			for( map< string, double >::const_iterator strIter = ((*mapIter).second).begin(); strIter != ((*mapIter).second).end(); strIter++ ) {
				aSGMGenFile << ',' << (*strIter).second; 
			}
			aSGMGenFile << endl;
		}
		aSGMGenFile << endl;
	}
}

void SGMGenTable::updateRegionCGE( const RegionCGE* regionCGE ) {
	// if table output is by sector, then add sector names to map
	if( mName == "PECbySector" ) {
		for ( std::map<string,int>::const_iterator secNameIter = regionCGE->supplySectorNameMap.begin(); secNameIter != regionCGE->supplySectorNameMap.end(); secNameIter++ ) {
			for ( int per = 0; per < mModeltime->getmaxper(); per++ ) {
				int year = mModeltime->getper_to_yr( per );
				addToType( year, (*secNameIter).first, 0 );
			}
		}
	}
}

// Write sector market prices to the SGM gen file
void SGMGenTable::updateProductionSector( const ProductionSector* aProductionSector, const int aPeriod ) {
	if( mName == "PRICE" ) {
		addToType( mModeltime->getper_to_yr( aPeriod ), aProductionSector->getName(),
			scenario->getMarketplace()->getPrice( aProductionSector->getName(), aProductionSector->regionName, aPeriod ) );
	}
}

// Write factor supply market prices to the SGM gen file
void SGMGenTable::updateFactorSupply( const FactorSupply* aFactorSupply, const int aPeriod ) {
	if( mName == "PRICE" ) {
		addToType( mModeltime->getper_to_yr( aPeriod ), aFactorSupply->getName(),
            scenario->getMarketplace()->getPrice( aFactorSupply->getName(), aFactorSupply->marketName, aPeriod ) );
	}
}

// Write National Account information to the SGM gen file
void SGMGenTable::updateNationalAccount( const NationalAccount* aNationalAccount, const int aPeriod ) {
	if( mName == "GNPREAL" ) {
		addToType( mModeltime->getper_to_yr( aPeriod ), "Consumption", 
			aNationalAccount->getAccountValue( NationalAccount::CONSUMPTION ) );
		addToType( mModeltime->getper_to_yr( aPeriod ), "Investment", 
			aNationalAccount->getAccountValue( NationalAccount::INVESTMENT ) );
		addToType( mModeltime->getper_to_yr( aPeriod ), "Government", 
			aNationalAccount->getAccountValue( NationalAccount::GOVERNMENT ) );
		addToType( mModeltime->getper_to_yr( aPeriod ), "Trade Balance", 
			aNationalAccount->getAccountValue( NationalAccount::NET_EXPORT ) );
		addToType( mModeltime->getper_to_yr( aPeriod ), "GNP", 
			aNationalAccount->getAccountValue( NationalAccount::GNP ) );
	}
	else if( mName == "GNPNOM" ) {
		addToType( mModeltime->getper_to_yr( aPeriod ), "Consumption", 
			aNationalAccount->getAccountValue( NationalAccount::CONSUMPTION ) );
		addToType( mModeltime->getper_to_yr( aPeriod ), "Investment", 
			aNationalAccount->getAccountValue( NationalAccount::INVESTMENT ) );
		addToType( mModeltime->getper_to_yr( aPeriod ), "Government", 
			aNationalAccount->getAccountValue( NationalAccount::GOVERNMENT ) );
		addToType( mModeltime->getper_to_yr( aPeriod ), "Trade Balance", 
			aNationalAccount->getAccountValue( NationalAccount::NET_EXPORT ) );
		addToType( mModeltime->getper_to_yr( aPeriod ), "GNP", 
			aNationalAccount->getAccountValue( NationalAccount::GNP ) );
	}
}

// Write demographics results to the SGM gen file
void SGMGenTable::updateDemographic( const Demographic* aDemographic, const int aPeriod ) {
	if( mName == "DEM" ) {
		addToType( mModeltime->getper_to_yr( aPeriod ), "Tot Pop", aDemographic->getTotal( aPeriod ) );
		addToType( mModeltime->getper_to_yr( aPeriod ), "Working Age Male",
                   aDemographic->getWorkingAgePopulationMales( aPeriod ) );
		addToType( mModeltime->getper_to_yr( aPeriod ), "Working Age Female",
                   aDemographic->getWorkingAgePopulationFemales( aPeriod ) );
		addToType( mModeltime->getper_to_yr( aPeriod ), "Working Age Tot",
                   aDemographic->getWorkingAgePopulation( aPeriod ) );
	}
}

// Write to SGM general table.
// This routine assumes that only and all operating technology vintages are passed in as 
// an arguement.
void SGMGenTable::updateConsumer( const Consumer* aConsumer, const int aPeriod ) 
{
    // Only update the current consumer.
    if( aConsumer->getYear() != mModeltime->getper_to_yr( aPeriod ) ){
        return;
    }

	// add output of each technology for each period
	if( ( mName == "CO2" ) || ( mName == "CO2bySec" ) || ( mName == "CO2byTech" ) ){
		unsigned int CO2index = util::searchForValue( aConsumer->mGhgNameMap, string( "CO2" ) );
		double CO2Emiss = aConsumer->mGhgs[ CO2index ]->getEmission( aPeriod );

		if( mName == "CO2" ) {
			addToType( mModeltime->getper_to_yr( aPeriod ), "CO2",  CO2Emiss );
		}
		else if( mName == "CO2bySec" ) {
            addToType( mModeltime->getper_to_yr( aPeriod ), aConsumer->getXMLName(), CO2Emiss );
		}
		else if( mName == "CO2byTech" ) {
			addToType( mModeltime->getper_to_yr( aPeriod ), aConsumer->getName(), CO2Emiss );
		}
	}
}

void SGMGenTable::updateHouseholdConsumer( const HouseholdConsumer* householdConsumer, const int aPeriod ) {
	if( mName == "CAP" ) {
		// add only current year consumer
		if( householdConsumer->year == mModeltime->getper_to_yr( aPeriod ) ) {
			addToType( mModeltime->getper_to_yr( aPeriod ), "Savings", 
				householdConsumer->expenditure.getValue( Expenditure::SAVINGS ) );
		}
	}
	else if( mName == "DEM" ) {
		if( householdConsumer->year == mModeltime->getper_to_yr( aPeriod ) ) {
			addToType( mModeltime->getper_to_yr( aPeriod ), "Labor Supply", 
				householdConsumer->getLaborSupply() );
		}
	}
}

void SGMGenTable::updateGovtConsumer( const GovtConsumer* govtConsumer, const int aPeriod ) {
	if( mName == "CAP" ) {
		// add only current year consumer
		if( govtConsumer->year == mModeltime->getper_to_yr( aPeriod ) ) {
			// note the negative sign to get
			addToType( mModeltime->getper_to_yr( aPeriod ), "Govt Deficit", 
				- govtConsumer->expenditure.getValue( Expenditure::SAVINGS ) );
		}
	}
}

void SGMGenTable::updateTradeConsumer( const TradeConsumer* tradeConsumer, const string& aRegionName,
                                       const int aPeriod ) {
	// net energy trade
	if(	mName == "ETRADE" )	{
		// add only	current	year consumer
		if(	tradeConsumer->getYear() == mModeltime->getper_to_yr( aPeriod ) ) {
			// get energy inputs only
			for( unsigned int i=0; i<tradeConsumer->input.size(); i++ ){
				if(	scenario->getMarketplace()->getMarketInfo( tradeConsumer->input[ i	]->getName(), aRegionName, 0, "IsEnergyGood", false ) ){
					addToType( mModeltime->getper_to_yr( aPeriod ),	tradeConsumer->input[ i	]->getName(),  
						tradeConsumer->input[ i	]->getDemandCurrency()
                        * tradeConsumer->input[ i ]->getConversionFactor( aRegionName ) );
				}
			}
		}
	}
    else if( mName == "EmissBySource" ){
        // add or remove emissions only for the current consumer.
        if( tradeConsumer->getYear() != mModeltime->getper_to_yr( aPeriod ) ){
            return;
        }

        // Loop through the inputs and find primary goods.
        Marketplace* marketplace = scenario->getMarketplace();
        for( unsigned int i = 0; i < tradeConsumer->input.size(); ++i ){
            // Skip non-primary inputs
            if( !marketplace->getMarketInfo( tradeConsumer->input[ i ]->getName(), aRegionName, 0,
                                             "IsPrimaryEnergyGood", false ) )
            {
                continue;
            }

            // Calculate the amount of emissions that are being traded.
            // Determine the output coefficient.
            const static string COEF_STRING = "coefficient";
            const double outputCoef = marketplace->getMarketInfo( tradeConsumer->input[ i ]->getName(),
                aRegionName, 0, "CO2" + COEF_STRING, false ); // not sure how to handle multiple gases.
            const double tradedEmissions = tradeConsumer->input[ i ]->getDemandPhysical( aRegionName ) 
                * outputCoef;
            // Add or remove the emissions to the column for the sector and year. Check that the sign is right.
            addToType( mModeltime->getper_to_yr( aPeriod ), tradeConsumer->input[ i ]->getName(), -1 * tradedEmissions );
        }
    }
}

void SGMGenTable::updateInvestConsumer( const InvestConsumer* investConsumer, const int aPeriod ) {
	// add only current year consumer
	if( investConsumer->year == mModeltime->getper_to_yr( aPeriod ) ) {
	}
}

// Write to SGM general table.
// This routine assumes that only and all operating technology vintages are passed in as 
// an arguement.
void SGMGenTable::updateProductionTechnology( const ProductionTechnology* prodTech, 
											  const string& aRegionName, const string& aSectorName,
                                              const int aPeriod ) 
{
	// add output of each technology for each period
	if( ( mName == "CO2" ) || ( mName == "CO2bySec" ) || ( mName == "CO2byTech" ) ){
		unsigned int CO2index = util::searchForValue( prodTech->mGhgNameMap, string( "CO2" ) );
		double CO2Emiss = prodTech->mGhgs[CO2index]->getEmission( aPeriod );
		if( mName == "CO2" ) {
			addToType( mModeltime->getper_to_yr( aPeriod ), "CO2", CO2Emiss );
		}
		else if( mName == "CO2bySec" ) {
			addToType( mModeltime->getper_to_yr( aPeriod ), aSectorName, CO2Emiss );
		}
		else if( mName == "CO2byTech" ) {
			addToType( mModeltime->getper_to_yr( aPeriod ), prodTech->getName(), CO2Emiss );
		}
    }
    else if( mName == "EmissBySource" ){
        unsigned int CO2index = util::searchForValue( prodTech->mGhgNameMap, string( "CO2" ) );
        const double emissFuel = prodTech->mGhgs[ CO2index ]->getEmissFuel( aPeriod );
        // Only primary energy sectors will have getEmissFuel > 0.
        if( emissFuel > 0 ){
            addToType( mModeltime->getper_to_yr( aPeriod ), aSectorName, emissFuel );
        }
    }
	else if( mName == "PEC" ) {
		for( unsigned int i=0; i<prodTech->input.size(); i++ ){
			// get primary energy input only
			string inputName = prodTech->input[ i ]->getName();
			if( scenario->getMarketplace()->getMarketInfo( inputName, aRegionName, 0, "IsPrimaryEnergyGood", false ) ){
				addToType( mModeltime->getper_to_yr( aPeriod ), prodTech->input[ i ]->getName(),  
					prodTech->input[ i ]->getDemandCurrency() * prodTech->input[ i ]->getConversionFactor( aRegionName ) );
			}
		}
		// special code to add renewable, nuclear and hydro electricity to primary energy consumption
		if( aSectorName == "ElectricityGeneration" ) {
			// TODO: use average fossil efficiency instead of hardcoded 0.3
			double fossilEfficiency = 0.3;
			if( prodTech->categoryName == "Renewable"){
				addToType( mModeltime->getper_to_yr( aPeriod ), prodTech->name,  
				prodTech->getOutput( aPeriod ) * 
				scenario->getMarketplace()->getMarketInfo( aSectorName, aRegionName, 0, "ConversionFactor", false  ) / fossilEfficiency );
			}
		}
	}
	// primary energy production
	else if( mName == "PEP" ) {
		// get primary energy input only
		if( scenario->getMarketplace()->getMarketInfo( aSectorName, aRegionName, 0, "IsPrimaryEnergyGood", false ) ){
			addToType( mModeltime->getper_to_yr( aPeriod ), aSectorName,  
			prodTech->getOutput( aPeriod ) * 
			scenario->getMarketplace()->getMarketInfo( aSectorName, aRegionName, 0, "ConversionFactor", false ) );
		}
		// special code to add renewable, nuclear and hydro electricity to primary energy production
		if( aSectorName == "ElectricityGeneration" ) {
			// TODO: use average fossil efficiency instead of hardcoded 0.3
			double fossilEfficiency = 0.3;
			if( prodTech->categoryName == "Renewable"){
				addToType( mModeltime->getper_to_yr( aPeriod ), prodTech->name,  
				prodTech->getOutput( aPeriod ) * 
				scenario->getMarketplace()->getMarketInfo( aSectorName, aRegionName, 0, "ConversionFactor", false  ) / fossilEfficiency );
			}
		}
	}
	// secondary energy production
	else if( mName == "SEP" ) {
		// get secondary energy goods only
		if( scenario->getMarketplace()->getMarketInfo( aSectorName, aRegionName, 0, "IsSecondaryEnergyGood", false ) ){
			addToType( mModeltime->getper_to_yr( aPeriod ), aSectorName,  
			prodTech->getOutput( aPeriod ) * 
			scenario->getMarketplace()->getMarketInfo( aSectorName, aRegionName, 0, "ConversionFactor", false ) );
		}
	}
	// non-energy sector output
	else if( mName == "NEP" ) {
		// get non-energy goods only
		if( !scenario->getMarketplace()->getMarketInfo( aSectorName, aRegionName, 0, "IsEnergyGood", false ) ){
			addToType( mModeltime->getper_to_yr( aPeriod ), aSectorName, prodTech->getOutput( aPeriod ) );
		}
	}
	// electricity generation by technology
	else if( mName == "ELEC" ) {
		if( aSectorName == "ElectricityGeneration" ) {
			addToType( mModeltime->getper_to_yr( aPeriod ), prodTech->getName(),
			prodTech->getOutput(aPeriod) *
			scenario->getMarketplace()->getMarketInfo( aSectorName, aRegionName, 0, "ConversionFactor", false  ) );
		}
	}
	// fuel consumption for electricity generation
	else if( mName == "ElecFuel" ) {
		if( aSectorName == "ElectricityGeneration" ) {
			for( unsigned int i=0; i<prodTech->input.size(); i++ ){
				// get energy input only
				string inputName = prodTech->input[ i ]->getName();
				if( scenario->getMarketplace()->getMarketInfo( inputName, aRegionName, 0, "IsEnergyGood", false ) ){
					addToType( mModeltime->getper_to_yr( aPeriod ), prodTech->input[ i ]->getName(),  
						prodTech->input[ i ]->getDemandCurrency() * prodTech->input[ i ]->getConversionFactor( aRegionName ) );
				}
			}
		}
	}
	// capital stock and other related output
	else if( mName == "CAP" ) {
		addToType( mModeltime->getper_to_yr( aPeriod ), "Capital", prodTech->getCapital() );
		addToType( mModeltime->getper_to_yr( aPeriod ), "Cap/1000 Worker", prodTech->getCapital() /
			scenario->getMarketplace()->getSupply( "Labor", aRegionName, aPeriod ) );
		addToType( mModeltime->getper_to_yr( aPeriod ), "Profits", prodTech->mProfits[ aPeriod ] );
		addToType( mModeltime->getper_to_yr( aPeriod ), "Retained Earnings", 
			prodTech->expenditure.getValue( Expenditure::RETAINED_EARNINGS ) );
	}
	// energy investments annual
	else if( mName == "EINV" ) {
		// get energy technologies only
		if( scenario->getMarketplace()->getMarketInfo( aSectorName, aRegionName, 0, "IsEnergyGood", false ) ){
			addToType( mModeltime->getper_to_yr( aPeriod ), aSectorName,
				prodTech->getAnnualInvestment( aPeriod ) );
		}
	}
	// non-energy investments annual
	else if( mName == "NEINV" ) {
		// get non-energy technologies only
		if( !scenario->getMarketplace()->getMarketInfo( aSectorName, aRegionName, 0, "IsEnergyGood", false ) ){
			addToType( mModeltime->getper_to_yr( aPeriod ), aSectorName,  
				prodTech->getAnnualInvestment( aPeriod ) );
		}
	}
	// write out all passenger transportation sector results
	else if( (mName == "PASSTRAN") || (mName == "PASSTRANFC") || (mName == "PASSTRANFCM") || 
		(mName == "PASSTRANFCT")  || (mName == "PASSTRANMPG") || (mName == "PASSTRANCOST") ) {
		// get passenger transport technologies only that have non zero production
		if( (prodTech->categoryName == "PassTransport") && (prodTech->mOutputs[ aPeriod ] != 0) ){
			double conversionFactor = scenario->getMarketplace()->getMarketInfo( aSectorName, aRegionName, 0, "ConversionFactor", false );
			if( mName == "PASSTRAN" ) {
				addToType( mModeltime->getper_to_yr( aPeriod ), aSectorName, prodTech->mOutputs[ aPeriod ] * conversionFactor );
			}
			else if ( mName == "PASSTRANCOST" ) {
				addToType( mModeltime->getper_to_yr( aPeriod ), aSectorName, scenario->getMarketplace()->getPrice(aSectorName, aRegionName, aPeriod) * conversionFactor );
			}
			// for all other tables that require inputs
			else {
				for( unsigned int i=0; i<prodTech->input.size(); i++ ){
					// get secondary energy input only
					string inputName = prodTech->input[ i ]->getName();
					// *** skip if not refined oil input ***
					// this is problematic for other vehicles that do not use refined oil
					if( inputName != "RefinedOil" ) {
						continue;
					}
					double fuelConsumption = prodTech->input[ i ]->getDemandCurrency() * prodTech->input[ i ]->getConversionFactor( aRegionName );
					// passenger transportation technology fuel consumption by fuel
					if( mName == "PASSTRANFC" ) {
						addToType( mModeltime->getper_to_yr( aPeriod ), inputName, fuelConsumption ); 
					}
					// passenger transportation technology fuel consumption by mode
					else if( mName == "PASSTRANFCM" ) {
						addToType( mModeltime->getper_to_yr( aPeriod ), aSectorName, fuelConsumption ); 
					}
					// passenger transportation technology fuel consumption by technology
					else if( mName == "PASSTRANFCT" ) {
						addToType( mModeltime->getper_to_yr( aPeriod ), prodTech->name, fuelConsumption ); 
					}
					// passenger transportation technology fuel economy
					else if( mName == "PASSTRANMPG" ) {
						double mpg = prodTech->mOutputs[ aPeriod ] * conversionFactor / fuelConsumption;
						addToType( mModeltime->getper_to_yr( aPeriod ), prodTech->name, mpg );
					}
				}
			}
		}
	}
}
