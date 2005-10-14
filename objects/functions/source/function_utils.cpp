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
* \file function_utils.cpp
* \ingroup Objects
* \brief The FunctionUtils class source file.
*
* \author Sonny Kim
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <vector>
#include <cmath>
#include <cassert>

#include "functions/include/function_utils.h"
#include "functions/include/input.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/util.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "functions/include/ifunction.h" // for TechChange.
#include "containers/include/iinfo.h"

using namespace std;

extern Scenario* scenario; // for marketplace.

//! Scale Input Coefficients
void FunctionUtils::scaleCoefficientInputs( vector<Input*>& input, double scaler ) {
	for ( unsigned int i = 0; i < input.size(); ++i ) {
		input[i]->scaleCoefficient( scaler ); // scale total demand
	}
}

//! Function to return sum of all demand inputs
double FunctionUtils::getDemandSum( const std::vector<Input*>& aInputs ) {
    double sum = 0;
    for ( unsigned int i = 0; i < aInputs.size(); ++i ) {
        sum += aInputs[i]->getDemandCurrency(); // sum each demand
    }
    return sum;
}

//! Return sum of Inputs
double FunctionUtils::getCoefSum( const vector<Input*>& input ) {
	double sum = 0;
	for ( unsigned int i = 0; i < input.size(); ++i ) {
		sum += input[i]->getCoefficient(); // sum each coefficient
	}
	return sum;
}

/*! \brief Return a specific input given its name.
* \param aInputs The vector of inputs to search.
* \param aInputName The name of the input to find.
* \return A constant pointer to the input, null if it does not exist.
*/
Input* FunctionUtils::getInput( const vector<Input*>& aInputs,
                                  const string& aInputName )
{
    for( vector<Input*>::const_iterator input = aInputs.begin(); input != aInputs.end(); ++input ) {
		if( (*input)->getName() == aInputName ) {
			return *input;
		}
	}
	return 0;
}

//! Helper function to find the index of capital.
Input* FunctionUtils::getCapitalInput( const vector<Input*>& aInputs ) {
    for( unsigned int i = 0; i < aInputs.size(); ++i ){
        if( aInputs[ i ]->isCapital() ){
            return aInputs[ i ];
        }
    }
    return 0;
}

/*! \brief Helper function to find the index of the numeraire input.
* \param aInputs The vector of inputs to the production function.
* \return A pointer to the numeraire input, null if there is not one(an error).
* \author Josh Lurz
*/
Input* FunctionUtils::getNumeraireInput( const vector<Input*>& aInputs ) {
    for( unsigned int i = 0; i < aInputs.size(); ++i ){
        if( aInputs[ i ]->isNumeraire() ){
            return aInputs[ i ];
        }
    }
    return 0;
}

//! Calculate Rho from Sigma and return
double FunctionUtils::getRho( const double aSigma ) {
    assert( aSigma > 0 );
	return ( aSigma - 1 ) / aSigma;
}

/*! \brief Calculate the net present value multiplier using the current price of
*          the capital input.
* \details 
* \param aInputs The vector of inputs to the production function.
* \param aLifetimeYears The nameplate lifetime of the vintage.
* \return The net present value multiplier.
* \author Josh Lurz
*/
double FunctionUtils::getNetPresentValueMult( const vector<Input*>& aInputs,
                                              const double aLifetimeYears )
{
    // Find the capital input.
    const Input* capInput = getCapitalInput( aInputs );
    assert( capInput );
	double discountRate = capInput->getPricePaid(); // already includes adjustments
	
    // calculate net present value multiplier
	double netPresentValueMult = FunctionUtils::calcNetPresentValueMult( discountRate, aLifetimeYears );
	assert( util::isValidNumber( netPresentValueMult ) );
    assert( netPresentValueMult >= 0 );
    return netPresentValueMult;
}

/*! \brief Function that calculates the net present value based on discount rate
*          and lifetime.
* \details This function separates the net present value calculation from the
*          model as a utility function that can be called and used from
*          anywhere. More specifically this function is used to calculate the
*          expected future price received and price paid. The
*          netPresentValueMult is multiplied by the price paid and received.
* \param aDiscountRate The discount rate
* \param aLifetime The lifetime over which to calculate the multiplier.
* \return The net present value multiplier.
* \author Sonny Kim
*/
double FunctionUtils::calcNetPresentValueMult( const double aDiscountRate, const double aLifetime ) {
    double netPresentValueMult = pow( ( 1 / ( 1 + aDiscountRate ) ), 0.5 ) 
                                 * ( 1 + ( 1 / aDiscountRate ) ) 
                                 * ( 1 - pow( (1 / ( 1 + aDiscountRate ) ), aLifetime ) );
    return netPresentValueMult;
}

/*! \brief Set the price paid for a good into the marketplace.
* \details Helper function which gets the information object from the market and
*          sets the price paid.
* \param aRegionName Name of the region for which to set the price paid.
* \param aGoodName Name of the good for which to set the price paid.
* \param aPeriod Model period in which to set the price paid.
* \param aPricePaid The price paid.
*/
void FunctionUtils::setPricePaid( const string& aRegionName,
								  const string& aGoodName,
							      const int aPeriod,
								  const double aPricePaid )
{
	assert( aGoodName != "USA" );
	IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( aGoodName, aRegionName,
		                                                           aPeriod, true );

	/*! \invariant The market and market info must exist. */
	assert( marketInfo );
	marketInfo->setDouble( "pricePaid", aPricePaid );
}

/*! \brief Gets the price paid for the good by querying the marketplace.
* \details Helper function which gets the information object from the market,
*          and queries it for the price paid.
* \param aRegionName Name of the region for which to get the price paid.
* \param aGoodName Name of the good for which to get the price paid.
* \param aPeriod Model period in which to get the price paid.
* \return The price paid, 0 if there is not one set.
*/
double FunctionUtils::getPricePaid( const string& aRegionName,
								    const string& aGoodName,
									const int aPeriod )
{
	assert( aGoodName != "USA" );
    const Marketplace* marketplace = scenario->getMarketplace();
	const IInfo* marketInfo = marketplace->getMarketInfo( aGoodName, aRegionName,
                                                          aPeriod, true );
	
    /*! \invariant The market and market info must exist. */
	assert( marketInfo );
	return marketInfo->getDouble( "pricePaid", true );
}

/*! \brief Set the price received for a good into the marketplace.
* \details Helper function which gets the information object from the market and
*          sets the price received.
* \param aRegionName Name of the region for which to set the price received.
* \param aGoodName Name of the good for which to set the price received.
* \param aPeriod Model period in which to set the price received.
* \param aPriceReceived Price received to set.
*/
void FunctionUtils::setPriceReceived( const string& aRegionName,
								      const string& aGoodName,
									  const int aPeriod,
								      const double aPriceReceived )
{
	assert( aGoodName != "USA" );
    Marketplace* marketplace = scenario->getMarketplace();
	IInfo* marketInfo = marketplace->getMarketInfo( aGoodName, aRegionName,
		                                            aPeriod, true );

	/*! \invariant The market and market info must exist. */
	assert( marketInfo );
	marketInfo->setDouble( "priceReceived", aPriceReceived );
}

/*! \brief Gets the price received for the good by querying the marketplace.
* \details Helper function which gets the information object from the market,
*          and queries it for the price received.
* \param aRegionName Name of the region for which to get the price received.
* \param aGoodName Name of the good for which to get the price received.
* \param aPeriod Model period in which to get the price received.
* \return The price received, 0 if there is not one set.
*/
double FunctionUtils::getPriceReceived( const string& aRegionName,
									    const string& aGoodName,
										const int aPeriod )
{
	assert( aGoodName != "USA" );
    const Marketplace* marketplace = scenario->getMarketplace();
	const IInfo* marketInfo = marketplace->getMarketInfo( aGoodName, aRegionName,
                                                          aPeriod, true );
	/*! \invariant The market and market info must exist. */
	assert( marketInfo );
	return marketInfo->getDouble( "priceReceived", true );
}

/*! \brief Calculate the expected price received for the good produced by the
*          production function.
* \details Calculate the expected price received for a good over the lifetime.
*          This is done by performing a net present value calculation using the
*          interest rate as determined from the capital input. 
* \param aInputs The vector of inputs to the production function.
* \param aRegionName The name of the region containing the production function.
* \param aGoodName The name of the sector containing the production function.
* \param aLifetimeYears The nameplate lifetime of vintage.
* \param aPeriod Period
* \return The expected price received for the good.
* \author Josh Lurz
*/
double FunctionUtils::getExpectedPriceReceived( const vector<Input*>& aInputs,
                                                const string& aRegionName,
                                                const string& aGoodName,
                                                const double aLifetimeYears,
                                                const int aPeriod )
{
    // calculate expected price received for the produced good
	return getPriceReceived( aRegionName, aGoodName, aPeriod )
           * getNetPresentValueMult( aInputs, aLifetimeYears );
}

/*! \brief Apply technical change to a vector of inputs.
* \details 
* \param input Vector of inputs for the demand function.
* \param aTechChange A structure containing the various possible types of
*        technical change.
* \param regionName Name of the region containing the function.
* \param sectorName Name of the sector containing the function.
* \param alphaZero The up-front scaler.
* \param sigma Sigma coefficient.
* \return The new alpha zero.
*/
double FunctionUtils::applyTechnicalChangeInternal( vector<Input*>& input,
                                                    const TechChange& aTechChange,
                                                    const string& regionName,
                                                    const string& sectorName,
                                                    const int aPeriod,
                                                    double alphaZero,
                                                    double sigma ) 
{
    for( unsigned int i = 0; i < input.size(); ++i ){
        double techChange = input[ i ]->getTechChange( 0, 0, regionName );
        if( techChange > 0 ){
            double scaleFactor = pow( 1 + techChange, scenario->getModeltime()->gettimestep( aPeriod ) );
            double newCoef = input[i]->getCoefficient() / scaleFactor;
            input[i]->setCoefficient( newCoef );
        }
    }
    // Apply hicks tech change.
    if( aTechChange.mHicksTechChange > 0 ){
        double scaleFactor = pow( 1 + aTechChange.mHicksTechChange,
                                  scenario->getModeltime()->gettimestep( aPeriod ) );
        alphaZero *= scaleFactor;
    }

    return alphaZero;
}
