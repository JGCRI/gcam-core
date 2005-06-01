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
* \file production_demand_function.cpp
* \ingroup Objects
* \brief The Production Demand Function class source file.
*
*  Detailed description.
*
* \author Pralit Patel
* \author Sonny Kim
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <string>
#include <vector>
#include <cmath>
#include <cassert>

#include "functions/include/production_demand_function.h"
#include "functions/include/input.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/util.h"
#include "util/base/include/model_time.h"
#include "functions/include/function_utils.h"

using namespace std;

extern Scenario* scenario;

/*! \brief Apply technical change to demand functions.
* \details 
* \note This function currently makes a call to
*       FunctionUtils::applyTechChangeInternal so that it can share code with
*       AProductionFunction::applyTechnicalChange. In the future these
*       implementations may vary so that function is not called directly.
* \param input Vector of inputs for the demand function.
* \param aTechChange A structure containing the various possible types of
*        technical change.
* \param regionName Name of the region containing the function.
* \param sectorName Name of the sector containing the function.
* \param alphaZero The up-front scaler.
* \param sigma Sigma coefficient.
* \return The new alpha zero.
*/
double ADemandFunction::applyTechnicalChange( vector<Input*>& input, const TechChange& aTechChange,
                                              const string& regionName, const string& sectorName,
                                              const int aPeriod, double alphaZero, double sigma ) const 
{
    return FunctionUtils::applyTechnicalChangeInternal( input, aTechChange, regionName, sectorName,
                                                        aPeriod, alphaZero, sigma );
}

//! Calculate Demand
double HouseholdDemandFn::calcDemand( vector<Input*>& input, double personalIncome, 
                                      const string& regionName, const string& sectorName,
                                      const double aShutdownCoef,
                                      int period,  double capitalStock, double alphaZero, 
                                      double sigma, double IBT ) const 
{
    // Find the numeraire to determine the price paid.
    const Input* numInput = FunctionUtils::getNumeraireInput( input );
    assert( numInput );
    const double pricePaidNumeraire = numInput->getPricePaid();
    assert( pricePaidNumeraire > 0 );

    double totalDemand = 0; // total demand used for scaling
	for ( unsigned int i = 0; i < input.size(); ++i ) {
        if( !input[ i ]->isFactorSupply() ){
            assert( input[i]->getPricePaid() > 0 );
            assert( input[i]->getCoefficient() > 0 );
            assert( input[i]->getIncomeElasticity() > 0 );

            double demand = input[i]->getCoefficient() * 
                pow( personalIncome / pricePaidNumeraire, input[i]->getIncomeElasticity() ) *
				pow( input[i]->getPricePaid() / pricePaidNumeraire, input[i]->getPriceElasticity() );

            assert( util::isValidNumber( demand ) );
			input[i]->setDemandCurrency( demand, regionName, sectorName, period );
			totalDemand += demand * input[i]->getPricePaid();
		}
	}
	return totalDemand;
}

double HouseholdDemandFn::calcCoefficient( vector<Input*>& input, double consumption,
                                          const string& regionName, const string& sectorName,
                                          int period, double sigma, double IBT,
                                          double capitalStock ) const
{
	Marketplace* marketplace = scenario->getMarketplace();
	for ( unsigned int i = 0; i < input.size(); ++i ) {
		if( !input[ i ]->isFactorSupply() ){
             // if we use price paid for calcDemand probably need to use it here also
			double tempCoefficient = input[i]->getDemandCurrency() /
                                     pow( consumption, input[i]->getIncomeElasticity() ) *
                                     pow( input[i]->getPrice( regionName, period ),
                                          input[i]->getPriceElasticity() );
			input[i]->setCoefficient( tempCoefficient );
		}
	}
	return 0; // return null for CES AlphaZero only
}

double GovtDemandFn::calcDemand( vector<Input*>& input, double consumption,
                                 const string& regionName, 
                                 const string& sectorName,
                                 const double aShutdownCoef,
                                 int period, double capitalStock, 
                                 double alphaZero, double sigma, double IBT ) const 
{
    double totalUnscaledDemand = 0; // total demand used for scaling
    for( unsigned int i = 0; i < input.size(); ++i ){
        if ( !input[ i ]->isCapital() ) {
            totalUnscaledDemand += input[ i ]->getCoefficient() * input[ i ]->getPricePaid();
        }
        // input is capital
        else {
            // use numeraire price paid
            const Input* numInput = FunctionUtils::getNumeraireInput( input );
            assert( numInput );
            const double pricePaidNumeraire = numInput->getPricePaid(); 
            assert( pricePaidNumeraire > 0 );
            totalUnscaledDemand += input[ i ]->getCoefficient() * pricePaidNumeraire;
        }
    }
    double rho = FunctionUtils::getRho( sigma );
	double govtPrefScaler = consumption * pow( totalUnscaledDemand, sigma*(rho-1) );

	double totalDemand = 0; // total final demand
	for( unsigned int i = 0; i < input.size(); ++i ){
		double demand = 0;
		if ( !input[ i ]->isCapital() ) {
			demand = input[ i ]->getCoefficient() * govtPrefScaler;
			totalDemand += demand;
			input[ i ]->setDemandCurrency( demand, regionName, sectorName, period );
		}
	}
	return totalDemand;
}

double GovtDemandFn::calcCoefficient( vector<Input*>& input, double consumption, 
                                      const string& regionName, const string& sectorName, 
                                      int period, double sigma, double IBT, 
                                      double capitalStock ) const 
{
	Marketplace* marketplace = scenario->getMarketplace();
	// get government's demand for capital
	double capitalDemand = 0;
	for( unsigned int i = 0; i<input.size(); ++i ){
		if ( input[ i ]->isCapital() ) {
			capitalDemand = input[ i ]->getDemandCurrency();
            break;
		}
	}
	// total used for government demand does not include capital
	double tempTotal = FunctionUtils::getDemandSum( input ) - capitalDemand;
	// government capital demand is calculated separatly
	// this needs to be corrected, legacy SGM probably could not handle
	for( unsigned int i = 0; i<input.size(); ++i ){
		if ( !input[ i ]->isCapital() ) {
            // not sure which price
			double tempCoefficient = input[ i ]->getDemandCurrency() 
			                         / tempTotal 
                                     / input[ i ]->getPrice( regionName, period );
			input[ i ]->setCoefficient( tempCoefficient );
		}
	}
    // Is this complete or not? -JPL
	//  to be continued ..

	return 0; // fake, for now
}

double TradeDemandFn::calcDemand( vector<Input*>& input, double consumption, const string& regionName,
                                  const string& sectorName, const double aShutdownCoef, int period,
                                  double capitalStock, 
                                  double alphaZero, double sigma, double IBT ) const 
{
	double totalNetExport = 0; // total demand used for scaling
	Marketplace* marketplace = scenario->getMarketplace();
	for( unsigned int i = 0; i<input.size(); ++i ){
        // Trade exists in all comodities except land and labor.
        if( !input[ i ]->isFactorSupply() ){
			// Open Trade
            double netExport;
            if( marketplace->getMarketInfo( input[ i ]->getName(), regionName, period, "IsFixedPrice", false ) ){
				netExport = marketplace->getSupply( input[ i ]->getName(), regionName, period ) -
					marketplace->getDemand( input[ i ]->getName(), regionName, period );
			}
			// Fixed Trade
			else {
				netExport = input[ i ]->getDemandCurrency();
			}
			// set here adds to marketplace demand as well as setting net export in input
			input[ i ]->setDemandCurrency( netExport, regionName, sectorName, period );
            totalNetExport += netExport * input[ i ]->getPrice( regionName, period );
		}
		// for capital, add to market demand but not to total net export
        else if( input[ i ]->isCapital() ){
			double netExport = input[ i ]->getDemandCurrency();
			// set here adds to marketplace demand
			input[ i ]->setDemandCurrency( netExport, regionName, sectorName, period );
			// not added to total net export?????
		}
	}
    assert( util::isValidNumber( totalNetExport ) );
	return totalNetExport;
}

double TradeDemandFn::calcCoefficient( vector<Input*>& input, double consumption, const string& regionName,
                                       const string& sectorName, int period, double sigma, double IBT,
                                       double capitalStock ) const
{
	return 0; // function not needed for trade consumer class
}

double InvestDemandFn::calcDemand( vector<Input*>& input, double capitalTotal, const string& regionName,
                                   const string& sectorName, const double aShutdownCoef,
                                   int period, double capitalStock, double alphaZero,
                                   double sigma, double IBT ) const
{
	double capitalSum = 0;
    for( unsigned int i = 0; i < input.size(); ++i ){
		if( !input[ i ]->isFactorSupply() ) {
			double demand = capitalTotal * input[ i ]->getCoefficient();
            if( demand < 0 ){
                cout << "Trying to add negative demand currency for " << input[ i ]->getName() << " in " << sectorName << endl;
            }
			input[ i ]->setDemandCurrency( demand, regionName, sectorName, period );
             // looking at the fortran seems as though this should be price paid
            // Yes it should be.
            capitalSum += demand * input[i]->getPrice( regionName, period );
        }
	}
	return capitalSum;
}

// Calculate coefficients for Investment's demand function
double InvestDemandFn::calcCoefficient( vector<Input*>& input, double consumption,
                                        const string& regionName, const string& sectorName,
                                        int period, double sigma, double IBT,
                                        double capitalStock ) const
{
    return 0; // Investment coefficients are read in, not calculated
}

/*! \brief Calculate Costs
* \pre Demand currencies for all inputs must be known.
* \return The total cost of production.
*/
double AProductionFunction::calcCosts( vector<Input*>& input, const string& regionName , int period ) const {
	double totalCost = 0;
	for( unsigned int i = 0; i < input.size(); ++i ) {
		// capital input name should be changed to OtherValueAdded
		if( !input[i]->isCapital() ) {
			totalCost += input[i]->getDemandCurrency() * input[i]->getPricePaid();
		}
	}
	return totalCost;
}

//! Calculate the profits using the production scale factor.
double AProductionFunction::calcProfits( vector<Input*>& input, const string& regionName, 
                                         const string& sectorName, const double aShutdownCoef,
                                         int period, 
                                         double capitalStock, double alphaZero, double sigma ) const
{

    double profits = calcUnscaledProfits( input, regionName, sectorName, period,
                                          capitalStock, alphaZero, sigma );
    // Scale the profits by the scaler which determines how much of the vintage
    // was shutdown.
    return aShutdownCoef * profits;
}

// Calculate CES coefficients
double CESProductionFn::calcCoefficient( vector<Input*>& input, double consumption, 
                                         const string& regionName, const string& sectorName, 
                                         int period, double sigma, double indBusTax, 
                                         double capitalStock ) const 
{
    /*! \pre sigma is greater than 0.05, otherwise we should be using a Leontief
    *        production function. 
    */
    assert( sigma >= 0.05 );
	assert( capitalStock > 0 );

	if( input.empty() ) {
		cout << "Error no inputs while trying to calculate CES Coefficient" << endl;
	}
	// first part of calculating coefficient, divide IO value by price
	for ( unsigned int i = 0; i < input.size(); ++i ) {
		if( !input[i]->isCapital() ) {
            double tempCoefficient = input[i]->getDemandCurrency() 
                                     / input[ i ]->getPrice( regionName, period );
			input[i]->setCoefficient( tempCoefficient );
		}
	}
	// get price of the numeraireIndex.
    const Input* numeraireInput = FunctionUtils::getNumeraireInput( input );
    assert( numeraireInput );
	const double priceNumeraire = numeraireInput->getPrice( regionName, period );
    const double coefNumeraire = numeraireInput->getCoefficient();

    double total = 0;
	for ( unsigned int i = 0; i < input.size(); ++i ) {
		if( !input[i]->isCapital() ) {
            total += input[i]->getCoefficient() / coefNumeraire * input[ i ]->getPrice( regionName, period );
		}
	}

	// calculate alpha 0
	double priceOutput = FunctionUtils::getDemandSum(input)/( FunctionUtils::getDemandSum(input) + indBusTax);
	double rho = FunctionUtils::getRho( sigma );
	double mu = rho/(1-rho);
	double alphaZero = pow( priceOutput/priceNumeraire,  -(1/rho) )
		* pow( numeraireInput->getCoefficient()/( FunctionUtils::getDemandSum(input)+indBusTax), (1/mu) );

	double Z = 1 - pow( (alphaZero * priceOutput), mu ) * total;

	// second part of calculation coefficient, normalize to numeraireIndex and apply elasticities
	for ( unsigned int i = 0; i < input.size(); ++i ) {
		if( !input[i]->isCapital() ) {
            double tempCoefficient = pow( (input[i]->getCoefficient() / coefNumeraire ),
                ( 1 / sigma ) ) * input[ i ]->getPrice( regionName, period ) / priceNumeraire;
			input[i]->setCoefficient( tempCoefficient );
		}
	}

	// calculate alpha for capital
	double capCoef = pow( (FunctionUtils::getDemandSum(input)+indBusTax)/alphaZero/capitalStock, rho) * Z;
    Input* capInput = FunctionUtils::getCapitalInput( input );
    assert( capInput );
	capInput->setCoefficient( capCoef );

	// third part of calculation coefficient, normalize to total and then to 100
	// normalize alphaZero
	alphaZero *= pow( ( FunctionUtils::getCoefSum(input)/100), (1/rho) );
	// normalize and set new coefficients
	FunctionUtils::scaleCoefficientInputs(input, 100/ FunctionUtils::getCoefSum(input));
	// normalize alphaZero to 1
	alphaZero = normalizeAlphaZero( input, alphaZero, sigma );
	return alphaZero;
}

/*! \brief Normalize alpha zero scaler to 1 and readjust inputs
* \param 
* \author Sonny Kim
* \return alpha zero.
*/
double CESProductionFn::normalizeAlphaZero( vector<Input*>& input, double aAlphaZero, double sigma ) const {
    /*! \pre sigma is greater than 0.05, otherwise we should be using a Leontief
    *        production function. 
    */
    assert( sigma >= 0.05 );

	const double rho = FunctionUtils::getRho( sigma );
	for ( unsigned int i = 0; i < input.size(); ++i ) {
		double newCoef = input[i]->getCoefficient() * pow( aAlphaZero, rho );
		input[i]->setCoefficient( newCoef );
	}
	return 1;
}

/*! \brief Transform production coefficients to utilize elasticity of substitution directly
* \param 
* \author Sonny Kim
* \return alpha-zero.
*/
double CESProductionFn::transformCoefficients( vector<Input*>& input, double priceReceived, 
                                               double alphaZero, double sigma ) const 
{
    /*! \pre sigma is greater than 0.05, otherwise we should be using a Leontief
    *        production function. 
    */
    assert( sigma >= 0.05 );

    const double rho = FunctionUtils::getRho( sigma );
    for	( unsigned int i = 0; i	< input.size();	++i	) {
        if(	!input[i]->isCapital() ) {
            double newCoef = pow( input[i]->getCoefficient(), sigma );
            input[i]->setCoefficient( newCoef );
        }
    }
    return pow( alphaZero, rho * sigma );
}

/*! \brief Transform production coefficients based on long-term elasticity to
*          short-term elasticity.
* \param 
* \author Sonny Kim
* \return alpha zero.
*/
double CESProductionFn::changeElasticity( vector<Input*>& input, double priceReceived, 
                                         double aProfits, double capitalStock, double alphaZero, 
                                         double sigmaNew, double sigmaOld ) const 
{
    // Note: This could actually happen and we should handle it by shutting down
    // the technology. I think we could just set alphaZero to zero.
    assert( aProfits > 0 );
    // Calculate parameters.
    const double rhoNew = FunctionUtils::getRho( sigmaNew );
    const double rhoOld = FunctionUtils::getRho( sigmaOld );
	const double sigmaRatio1 = sigmaNew / sigmaOld;
	const double sigmaRatio2 = ( rhoNew - rhoOld ) * sigmaNew;

    for	( unsigned int i = 0; i	< input.size();	++i	) {
		// all inputs other than capital
        double newCoef = 0;
        if(	!input[i]->isCapital() ) {
			double priceRatio = priceReceived / input[i]->getPricePaid();
            newCoef = pow( input[i]->getCoefficient(), sigmaRatio1 ) * pow( priceRatio, sigmaRatio2 );
        }
		// for capital or OVA input
		else {
		    double priceRatio = priceReceived / (aProfits/capitalStock);
            newCoef = pow( input[i]->getCoefficient(), sigmaRatio1 ) * pow( priceRatio, sigmaRatio2 );
		}
		// set new transformed coefficient in input object
        input[i]->setCoefficient( newCoef );
    }

	// new alpha zero coefficient
    const double sigmaRatio = rhoNew / rhoOld * sigmaNew / sigmaOld; 
	return pow( alphaZero, sigmaRatio );
}

/*! \brief Calculate capital scaler for production technology, includes capital stock */
double CESProductionFn::calcCapitalScaler( const vector<Input*>& input, double aAlphaZero, double sigma, 
                                           double capitalStock ) const 
{
    /*! \pre sigma is greater than 0.05, otherwise we should be using a Leontief
    *        production function. 
    */
    assert( sigma >= 0.05 );

    const double rho = FunctionUtils::getRho( sigma );

    // find capital coefficient and multiply by stock to the power of rho
    Input* capitalInput = FunctionUtils::getCapitalInput( input );
    assert( capitalInput );
    assert( capitalStock > 0 );

    double temp = capitalInput->getCoefficient() * pow( capitalStock, rho );
    assert( util::isValidNumber( temp ) );
    double tempCapitalScaler = pow( temp, ( 1 / rho ) );

    /*! \post The capital scaler is a valid number. */
    assert( util::isValidNumber( tempCapitalScaler ) );
    return tempCapitalScaler;
}

/*! \brief Calculate capital rate scaler for expected profit rate calculation,
*          does not include actual capital stock
* \return The capital rate scaler.
*/
double CESProductionFn::calcCapitalRateScaler( const vector<Input*>& input, double sigma ) const {
    /*! \pre sigma is greater than 0.05, otherwise we should be using a Leontief production function. */
    assert( sigma >= 0.05 );
	const double rho = FunctionUtils::getRho( sigma );
    const Input* capInput = FunctionUtils::getCapitalInput( input );
    assert( capInput );
    // do only capital to get capital contribution to production level
    assert( capInput->getCoefficient() > 0 );
    return pow( capInput->getCoefficient(), ( 1 / rho ) );
}

/*! \brief Calculate profit scaler for production technology.
*/
double CESProductionFn::calcFinalProfitScaler( const vector<Input*>& input, const string& regionName, 
                                               const string& sectorName, int period, 
                                               double alphaZero, double sigma ) const 
{
    /*! \pre sigma is greater than 0.05, otherwise we should be using a Leontief
    *        production function. 
    */
    assert( sigma >= 0.05 );

	const double rho = FunctionUtils::getRho( sigma );
	// calculate tempZ for all inputs except capital
	double tempZ = 0; // temporary scaler
	for ( unsigned int i = 0; i < input.size(); ++i ) {
		// capital contribution calculated separately, see calcCapitalScaler()
		if( !input[i]->isCapital() ) {
			double tempCoef = pow( input[i]->getCoefficient(), sigma );
			tempZ += tempCoef * pow( input[i]->getPricePaid(), (-rho * sigma) );
		}
	}
	// use price received for the good in the next equation
    const Marketplace* marketplace = scenario->getMarketplace();
	double priceReceived = marketplace->getMarketInfo( sectorName, regionName, period, "priceReceived" );
	double Z = 1 - tempZ * pow( ( priceReceived * alphaZero), (rho * sigma) );
    return ( Z > 0 ) ? pow( Z, -(1/rho) ) : 0;
}

//! Calculate Demand
double CESProductionFn::calcDemand( vector<Input*>& input, double personalIncome, 
                                    const string& regionName, const string& sectorName,
                                    const double aShutdownCoef, int period, double capitalStock,
                                    double alphaZero, 
                                    double sigma, double IBT) const 
{
    /*! \pre sigma is greater than 0.05, otherwise we should be using a Leontief
    *        production function. 
    */
    assert( sigma >= 0.05 );
	Marketplace* marketplace = scenario->getMarketplace();

	const double Z1 = calcCapitalScaler( input, alphaZero, sigma, capitalStock );
	const double Z2 = calcFinalProfitScaler( input, regionName, sectorName, period, alphaZero, sigma );
	const double Z = Z1 * Z2 * 
        pow( ( marketplace->getMarketInfo( sectorName, regionName, period, "priceReceived" ) * alphaZero), sigma );
    
	
    double totalDemand = 0; // total demand used for scaling
	for ( unsigned int i = 0; i < input.size(); ++i ) {
		// capital input name should be changed to OtherValueAdded
		if( !input[i]->isCapital() ) {
			assert( input[i]->getPricePaid() >= 0 );
            double pricePaid = max( input[i]->getPricePaid(), util::getSmallNumber() );
			double demand = pow( ( input[i]->getCoefficient() / pricePaid ), sigma ) * aShutdownCoef * Z;
			if (demand < 0) {
				cout << "Demand < 0 for Region: " << regionName << " Sector: " << sectorName << " Input: " << input[i]->getName() 
					<< " Demand: " << demand << endl;
			}
			input[i]->setDemandCurrency( demand, regionName, sectorName, period );
			totalDemand += demand;
		}
	}
	return totalDemand;
}

/*! \brief Calculate expected profit scaler for investment decision.
*/
double CESProductionFn::calcExpProfitScaler( const vector<Input*>& input, double aLifetimeYears, 
                                             const string& regionName, const string& sectorName, 
                                             int period, double alphaZero, double sigma ) const 
{
    /*! \pre sigma is greater than 0.05, otherwise we should be using a Leontief
    *        production function. 
    */
    assert( sigma >= 0.05 );
	const double rho = FunctionUtils::getRho( sigma );
    
    // Calculate the net present value multiplier to determine expected prices.
    const double netPresentValueMult = FunctionUtils::getNetPresentValueMult( input, aLifetimeYears );

	// calculate tempZ for all inputs except capital
    double tempZ = 0; // temporary scaler
	for ( unsigned int i = 0; i < input.size(); ++i ) {
		// capital contribution calculated separately, see calcCapitalScaler()
		if( !input[i]->isCapital() ) {
			tempZ += pow(input[i]->getCoefficient(), sigma) 
                     * pow( input[i]->getPricePaid() * netPresentValueMult , ( -rho * sigma ) );
		}
	}
    // Calculate the expected price received.
    const double expPriceReceived = FunctionUtils::getExpectedPriceReceived( input, regionName,
                                                                             sectorName,
                                                                             aLifetimeYears, period );

	// use expected price received for the good in the next equation
	double Z = 1 - tempZ * pow( ( expPriceReceived * alphaZero ), ( rho * sigma ) );
	// okay to have negative Z, it means that technology is not profitable set Z
	// = 0 so that power does not blow up
    Z = max( Z, 0.0 );
    assert( util::isValidNumber( Z ) );
	return Z;
}

//! Calculate Expected Profit Rate
double CESProductionFn::calcExpProfitRate( const vector<Input*>& input, const string& regionName, 
                                           const string& sectorName, double aLifetimeYears, int period,
                                           double alphaZero, double sigma ) const 
{
    /*! \pre sigma is greater than 0.05, otherwise we should be using a Leontief
    *        production function. 
    */
    assert( sigma >= 0.05 );

	const double rho = FunctionUtils::getRho( sigma );
	const double Z1 = calcCapitalRateScaler( input, sigma );
	const double Z2 = calcExpProfitScaler( input, aLifetimeYears, regionName, 
                                           sectorName, period, alphaZero, sigma );
    // Note: Dividing by the numeraire price at all here may not be correct.
    const Input* numInput = FunctionUtils::getNumeraireInput( input );
    assert( numInput );
    const double pricePaidNumeraire = numInput->getPricePaid();
    assert( pricePaidNumeraire > 0 );
	// returns a rate, using price ratios
    const double expPriceReceived = FunctionUtils::getExpectedPriceReceived( input, regionName, sectorName,
                                                              aLifetimeYears, period );
	double expectedProfitRate = alphaZero * ( expPriceReceived / pricePaidNumeraire ) * 
        Z1 *  pow( Z2, (1/(-rho*sigma)) );

	assert( expectedProfitRate >= 0 );
    assert( util::isValidNumber( expectedProfitRate ) );
	return expectedProfitRate;
}

/*! \brief Calculate the levelized cost for this technology.
* \details Performs a levelized cost calculation, or cost per unit of output,
*          for the current period. This is performed by summing the coefficient,
*          or quantity used per unit of output, multiplied by the price paid for
*          that good. Capital is treated specially as the price paid is replaced
*          by the discount factor, which is equal to the interest rate
*          multiplied by the input's price adjustment. The rho and exp exponents
*          are used to allow substitution between inputs to a limited degree.
*          This function does not correct for the non-sensical levelized cost of
*          zero, it only avoids calculating invalid exponents. It is assumed the
*          caller will determine how to handle the zero. 
* 
* \author Josh Lurz
* \param aInputs Vector of inputs for the technology.
* \param aRegionName Name of the region containing the production function.
* \param aSectorName Name of the sector containing the production function.
* \param aPeriod Period in which to calculate levelized cost.
* \param aAlphaZero Out front scalar.
* \param aSigma Sigma coefficient.
* \warning This function does not correct for zero levelized cost.
* \return The levelized cost.
*/
double CESProductionFn::calcLevelizedCost( const vector<Input*>& aInputs,
                                           const string& aRegionName,
                                           const string& aSectorName,
                                           int aPeriod,
                                           double aAlphaZero,
                                           double aSigma ) const
{
    // Calculate the exponents.
    const double rho = FunctionUtils::getRho( aSigma );
    const double exp = ( rho - 1 ) / rho;

    // Loop through the inputs and add on their costs.
    double levelizedCost = 0;
    for( unsigned int i = 0; i < aInputs.size(); ++i ){
        // Capital is done specially.
        if( aInputs[ i ]->isCapital() ){
            // Store the capital price because getting it requires a call to the marketplace.
            const double capPrice = aInputs[ i ]->getPrice( aRegionName, aPeriod );
            // Check that the coefficient and adjusted price can both be raised
            // to exponents without overflowing or underflowing. Note: The price
            // adjustment is not included in price paid as the adjustment is not
            // correct. There should be a discount adjustment however.
            if( aInputs[ i ]->getCoefficient() > util::getVerySmallNumber() &&
                ( capPrice > util::getVerySmallNumber() ) )
            {
                levelizedCost += pow( aInputs[ i ]->getCoefficient(), aSigma )
                    * pow( capPrice, exp );
                assert( util::isValidNumber( levelizedCost ) );
            }
        }
        // All other inputs.
        else {
            // Check that the coefficient and adjusted price can both be raised
            // to exponents without overflowing or underflowing.
            if( aInputs[ i ]->getCoefficient() > util::getVerySmallNumber() &&
                ( aInputs[ i ]->getPricePaid() > util::getVerySmallNumber() ) )
            {
                levelizedCost += pow( aInputs[ i ]->getCoefficient(), aSigma )
                    * pow( aInputs[ i ]->getPricePaid(), exp );
                assert( util::isValidNumber( levelizedCost ) );
            }
        }
    }
    /*! \post Levelized cost is greater than zero, as output must not be free. */
    assert( levelizedCost > 0 );
    // Make sure this calculation succeeds even if levelized cost is
    // non-sensical.
    return ( levelizedCost > 0 ) ? pow( levelizedCost, aSigma ) / aAlphaZero: 0;
}

/*! \brief Calculate the variable cost per unit of output for the technology.
* \details This function returns the variable cost per unit of output, or the
*          levelized cost minus the payments to capital. MORE HERE
* \author Josh Lurz
* \param aInputs Vector of inputs for the technology.
* \param aRegionName Name of the region containing the production function.
* \param aSectorName Name of the sector containing the production function.
* \param aPeriod Period in which to calculate variable cost.
* \param aAlphaZero Out front scalar.
* \param aSigma Sigma coefficient.
* \return The variable cost.
*/
double CESProductionFn::calcVariableCost( const vector<Input*>& aInputs,
                                          const string& aRegionName,
                                          const string& aSectorName,
                                          int aPeriod,
                                          double aAlphaZero,
                                          double aSigma ) const 
{
    // Calculate the exponents.
    const double rho = FunctionUtils::getRho( aSigma );
    const double exp = ( rho - 1 ) / rho;

    // Loop through the inputs other than capital and add on their costs.
    double variableCost = 0;
    for( unsigned int i = 0; i < aInputs.size(); ++i ){
        // Variable cost does not include capital.
        if( !aInputs[ i ]->isCapital() ){
            // Check that the coefficient and adjusted price can both be raised
            // to exponents without overflowing or underflowing.
            if( aInputs[ i ]->getCoefficient() > util::getVerySmallNumber() &&
                ( aInputs[ i ]->getPricePaid() > util::getVerySmallNumber() ) )
            {
                variableCost += pow( aInputs[ i ]->getCoefficient(), aSigma )
                    * pow( aInputs[ i ]->getPricePaid(), exp );
                assert( util::isValidNumber( variableCost ) );
            }
        }
    }

    // Make sure this calculation succeeds even if a vintage has no inputs other
    // than capital, so that it has zero variable cost.
    return ( variableCost > 0 ) ? pow( variableCost, aSigma ) / aAlphaZero : 0;
}

//! Calculate profits
double CESProductionFn::calcUnscaledProfits( const vector<Input*>& input, const string& regionName,
                                             const string& sectorName, const int period, 
                                             const double capitalStock, const double alphaZero, 
                                             const double sigma ) const 
{
    /*! \pre sigma is greater than 0.05, otherwise we should be using a Leontief
    *        production function. 
    */
    assert( sigma >= 0.05 );

    double rho = FunctionUtils::getRho( sigma );
	double Z1 = calcCapitalScaler( input, alphaZero, sigma, capitalStock );
	double Z2 = calcFinalProfitScaler( input, regionName, sectorName, period, alphaZero, sigma );
    Marketplace* marketplace = scenario->getMarketplace();
	double priceReceived = marketplace->getMarketInfo( sectorName, regionName, period, "priceReceived" );
	return alphaZero * priceReceived * Z1 * pow( Z2, (1 - rho) );
}

//! Calculate output
double CESProductionFn::calcOutput( vector<Input*>& input, const string& regionName, 
                                    const string& sectorName, const double aShutdownCoef,
                                    int period, double capitalStock, 
                                    double alphaZero, double sigma ) const 
{
    /*! \pre sigma is greater than 0.05, otherwise we should be using a Leontief
    *        production function. 
    */
    assert( sigma >= 0.05 );

    return alphaZero * 
           aShutdownCoef * 
           calcCapitalScaler( input, alphaZero, sigma, capitalStock ) * 
           calcFinalProfitScaler( input, regionName, sectorName, period, alphaZero, sigma );
}

//! Return the amount of output produced by one unit of capital.
double CESProductionFn::getCapitalOutputRatio( const vector<Input*>& aInputs, const string& aRegionName,
                                               const string& aSectorName, double aLifetimeYears, int aPeriod,
                                               double aAlphaZero, double aSigma ) const
{
    // Calculate the expected profit rate.
    const double profitRate = calcExpProfitRate( aInputs, aRegionName, aSectorName, aLifetimeYears, 
                                                 aPeriod, aAlphaZero, aSigma );
    
    // Check for negative profit rates.
    if( profitRate > 0 ){
        const double rho = FunctionUtils::getRho( aSigma );
        const double expA0 = rho / ( 1 - rho ); // var name is bad.
        const double capCoef = FunctionUtils::getCapitalInput( aInputs )->getCoefficient();
        const double expPriceReceived = FunctionUtils::getExpectedPriceReceived( aInputs, aRegionName, aSectorName,
                                                                                 aLifetimeYears, aPeriod );
        assert( capCoef > 0 );
        const double capQ = pow( aAlphaZero, expA0 ) * pow( capCoef, aSigma ) *
            pow( expPriceReceived / profitRate, aSigma );

        return capQ; // this is actually 1 / ratio, might be better to return
                     // the ratio.
    }
    // If there is no profit rate.
    return 0;
}

/*! \brief Apply technical change to production functions.
* \details 
* \note This function currently makes a call to
*       FunctionUtils::applyTechChangeInternal so that it can share code with
*       ADemandFunction::applyTechnicalChange. In the future these
*       implementations may vary so that function is not called directly.
* \param input Vector of inputs for the demand function.
* \param aTechChange A structure containing the various possible types of
*        technical change.
* \param regionName Name of the region containing the function.
* \param sectorName Nmae of the sector containing the function.
* \param alphaZero The up-front scaler.
* \param sigma Sigma coefficient.
* \return The new alpha zero.
*/
double CESProductionFn::applyTechnicalChange( vector<Input*>& input, const TechChange& aTechChange,
                                                  const string& regionName, const string& sectorName,
                                                  const int aPeriod, double alphaZero, double sigma ) const 
{
     /*! \pre sigma is greater than 0.05, otherwise we should be using a Leontief production function. */
    assert( sigma >= 0.05 );
    // Need to apply to a0 for hicks neutral, also material and energy.
    const double rho = FunctionUtils::getRho( sigma );
    for( unsigned int i = 0; i < input.size(); ++i ){
        double techChange = input[i]->getTechChange( aTechChange.mEnergyTechChange, aTechChange.mMaterialTechChange,
                                                     regionName );
		if( techChange > 0 ) {
			double scaleFactor = pow( 1 + techChange, scenario->getModeltime()->gettimestep( aPeriod ) );
            double newCoef = input[i]->getCoefficient() * pow( scaleFactor, rho );
			input[i]->setCoefficient( newCoef );
		}
    }
    // Apply hicks tech change. Check to make sure this works correctly as it is untested.
    if( aTechChange.mHicksTechChange > 0 ){
        double scaleFactor = pow( 1 + aTechChange.mHicksTechChange,
                                  scenario->getModeltime()->gettimestep( aPeriod ) );
        alphaZero *= scaleFactor;
    }
    return alphaZero;
}

// LEONTIEF
//! Calculate the capital scaler.
double LeontiefProductionFn::calcCapitalScaler( const std::vector<Input*>& input, double aAlphaZero, 
                                                double sigma, double capitalStock ) const 
{
    double capitalCoef = FunctionUtils::getCapitalInput( input )->getCoefficient();
    double qCapital = 0;
    if( capitalCoef != 0 && aAlphaZero != 0 ){
        qCapital = 1 / capitalCoef / aAlphaZero;
    }
    assert( util::isValidNumber( qCapital ) );
    return qCapital;
}

/*! \brief Calculate Leontief coefficients
* \return alpha zero.
*/
double LeontiefProductionFn::calcCoefficient( vector<Input*>& input, double consumption, 
                                              const string& regionName, const string& sectorName, 
                                              int period, double sigma, double indBusTax, 
                                              double capitalStock ) const 
{
    assert( sigma < 0.05 ); // Should be a CES.
    // If there is no capital stock, initialize technical coefficients to zero.
    if( capitalStock < 1 ){
        for( unsigned int i = 0; i < input.size(); ++i ){
            input[ i ]->setCoefficient( 0 );
        }
    }
    
    // Calculate output quantity.
    double demandCurrencySum = 0; // find real name.
    for( unsigned int i = 0; i < input.size(); ++i ){
        demandCurrencySum += input[ i ]->getDemandCurrency();
    }
    
    // use price received(psubj) for the good in the next equation
	double priceReceived = scenario->getMarketplace()->getMarketInfo( sectorName, regionName, period, "priceReceived" );
    assert( priceReceived > 0 );
    double qSubJ = demandCurrencySum / priceReceived;
    assert( qSubJ > 0 );

    // Set the coefficients. 
    for( unsigned int i = 0; i < input.size(); ++i ){
        if( input[ i ]->isCapital() ){
            input[ i ]->setCoefficient( capitalStock / qSubJ );
        }
        else {
            if( input[ i ]->getPricePaid() > 0 ){
                double currToPrice = input[ i ]->getDemandCurrency() / input[ i ]->getPricePaid();
                input[ i ]->setCoefficient( currToPrice / qSubJ );
            }
            else {
                input[ i ]->setCoefficient( 0 );
            }
        }
    }
    // Initialize alpha zero to unity.
    return 1;
}

/*! \brief Transform production coefficients to utilize elasticity of
*          substitution directly 
* \author Josh Lurz
* \return alpha-zero
*/
double LeontiefProductionFn::transformCoefficients( vector<Input*>& input, double priceReceived, 
                                                    double alphaZero, double sigma ) const 
{
    // Transform the coefficients. I dont think a special branch is needed here
    // for capital bc price received should equal the price paid for capital.
    for( unsigned int i = 0; i < input.size(); ++i ){
        if( input[ i ]->getPricePaid() > 0 ){
            double newCoef = alphaZero * input[ i ]->getCoefficient();
            input[ i ]->setCoefficient( newCoef );
        }
        else {
            input[ i ]->setCoefficient( 0 );
        }
    }
    return alphaZero;
}

/*! \brief Transform production coefficients based on long-term elasticity to
*          short-term elasticity.
* \author Sonny Kim
*/
double LeontiefProductionFn::changeElasticity( vector<Input*>& input, double priceReceived, 
                                               double aProfits, double capitalStock, double alphaZero,
                                               double sigmaNew, double sigmaOld ) const 
{
    assert( sigmaNew > 0 );
    assert( aProfits > 0 );
    const double rhoNew = FunctionUtils::getRho( sigmaNew );
    const double alphaZeroExp = rhoNew / ( 1 - rhoNew );

    for	( unsigned int i = 0; i	< input.size();	++i	) {
		// all inputs other than capital
        double priceRatio = 0;
        if(	!input[i]->isCapital() ) {
			priceRatio = priceReceived / input[i]->getPricePaid();
        }
		// for capital or OVA input
		else {
            priceRatio = priceReceived / (aProfits/capitalStock);
		}
        double newCoef = pow( alphaZero, alphaZeroExp )
                         * pow( input[i]->getCoefficient(), sigmaNew )
                         * pow( priceRatio, sigmaNew );

		// set new transformed coefficient in input object
        input[i]->setCoefficient( newCoef );
    }
    
    // Set alpha zero to 1.
	return 1;
}

/*! \brief Calculate Demand.
* \return Returns total demand
*/
double LeontiefProductionFn::calcDemand( vector<Input*>& input, double personalIncome, 
                                        const string& regionName, const string& sectorName,
                                        const double aShutdownCoef,
                                        int period, double capitalStock, double alphaZero, 
                                        double sigma, double IBT ) const 
{
    double qCapital = calcCapitalScaler( input, alphaZero, sigma, capitalStock );

    double qTemp = aShutdownCoef * qCapital * capitalStock;
    
    // Calculate direct demands. Not sure why this doesn't use scaleFactor.
    double totalDemand = 0;
    for( unsigned int i = 0; i < input.size(); ++i ){
        if( !input[ i ]->isCapital() ){
            double currDemand = qTemp * input[ i ]->getCoefficient() / alphaZero;
            totalDemand += currDemand;
            if( currDemand < 0 ){
                cout << "Trying to add negative demand currency for " << input[ i ]->getName() << " in " << sectorName << endl;
            }
            input[ i ]->setDemandCurrency( currDemand, regionName, sectorName, period );
        }
    }
    assert( totalDemand >= 0 );
    assert( util::isValidNumber( totalDemand ) );
    return totalDemand;
}

//! Calculate Expected Profit Rate.
double LeontiefProductionFn::calcExpProfitRate( const vector<Input*>& input, const string& regionName, 
                                                const string& sectorName, double aLifetimeYears,
                                                int period, double alphaZero, double sigma ) const 
{
    double qCapital = calcCapitalScaler( input, alphaZero, sigma, 0 );
    double valQ = qCapital * FunctionUtils::getExpectedPriceReceived( input, regionName, sectorName,
                                                                      aLifetimeYears, period );
    assert( valQ >= 0 );
    assert( util::isValidNumber( valQ ) );
    const double netPresentValueMult = FunctionUtils::getNetPresentValueMult( input, aLifetimeYears );
    double sum = 0;
    for( unsigned int i = 0; i < input.size(); ++i ){
        if( !input[ i ]->isCapital() ){
            sum += qCapital / alphaZero * input[ i ]->getCoefficient() * netPresentValueMult;
        }
    }
    assert( sum >= 0 );
    assert( util::isValidNumber( sum ) );
    return max( valQ - sum, 0.0 );
}

/*! \brief Calculate the levelized cost for this technology.
* \details 
* 
* \author Josh Lurz
* \param aInputs Vector of inputs for the technology.
* \param aRegionName Name of the region containing the production function.
* \param aSectorName Name of the sector containing the production function.
* \param aPeriod Period in which to calculate levelized cost.
* \param aAlphaZero Out front scalar.
* \param aSigma Sigma coefficient.
* \return The levelized cost.
*/
double LeontiefProductionFn::calcLevelizedCost( const vector<Input*>& aInputs, const string& aRegionName,
                                                const string& aSectorName, int aPeriod,
                                                double aAlphaZero, double aSigma ) const
{
    // Loop through the inputs and add on their costs.
    double levelizedCost = 0;
    for( unsigned int i = 0; i < aInputs.size(); ++i ){
        // Capital is done specially.
        if( aInputs[ i ]->isCapital() ){
            levelizedCost += aInputs[ i ]->getCoefficient() 
                             * aInputs[ i ]->getPrice( aRegionName, aPeriod )
                             * aInputs[ i ]->getPriceAdjustment();
        }
        else {
            levelizedCost += aInputs[ i ]->getCoefficient() * aInputs[ i ]->getPricePaid();
        }
    }
    return levelizedCost / aAlphaZero;
}

/*! \brief Calculate the variable cost per unit of output for the technology.
* \details This function returns the variable cost per unit of output, or the
* levelized cost minus the payments to capital. MORE HERE
* \author Josh Lurz
* \param aInputs Vector of inputs for the technology.
* \param aRegionName Name of the region containing the production function.
* \param aSectorName Name of the sector containing the production function.
* \param aPeriod Period in which to calculate variable cost.
* \param aAlphaZero Out front scalar.
* \param aSigma Sigma coefficient.
* \return The variable cost.
*/
double LeontiefProductionFn::calcVariableCost( const vector<Input*>& aInputs,
                                               const string& aRegionName,
                                               const string& aSectorName,
                                               int aPeriod,
                                               double aAlphaZero,
                                               double aSigma ) const
{
    // Loop through the inputs other than capital and add on their costs.
    double variableCost = 0;
    for( unsigned int i = 0; i < aInputs.size(); ++i ){
        if( !aInputs[ i ]->isCapital() ){
            variableCost += aInputs[ i ]->getCoefficient() * aInputs[ i ]->getPricePaid();
        }
    }
    
    return variableCost / aAlphaZero;
}

//! Calculate profits.
double LeontiefProductionFn::calcUnscaledProfits( const vector<Input*>& input, const string& regionName,
                                                  const string& sectorName, const int period, 
                                                  const double capitalStock, const double alphaZero, 
                                                  const double sigma ) const 
{
    double qCapital = calcCapitalScaler( input, alphaZero, sigma, capitalStock );

    double valQ = qCapital * 
        scenario->getMarketplace()->getMarketInfo( sectorName, regionName, period, "priceReceived" );
    
    assert( valQ >= 0 );
    assert( util::isValidNumber( valQ ) );

    double sum = 0;
    for( unsigned int i = 0; i < input.size(); ++i ){
        if( !input[ i ]->isCapital() ){
            sum += qCapital * input[ i ]->getCoefficient() / alphaZero * input[ i ]->getPricePaid();
        }
    }
    assert( sum >= 0 );
    assert( util::isValidNumber( sum ) );
    return max( ( valQ - sum ) * capitalStock, 0.0 );
}

//! Calculate output.
double LeontiefProductionFn::calcOutput( vector<Input*>& input, const string& regionName, 
                                         const string& sectorName, const double aShutdownCoef,
                                         int period, double capitalStock, 
                                         double alphaZero, double sigma ) const 
{
    double qCapital = calcCapitalScaler( input, alphaZero, sigma, capitalStock );
    return aShutdownCoef * qCapital * capitalStock;
}

//! Return the amount of output produced by one unit of capital.
double LeontiefProductionFn::getCapitalOutputRatio( const vector<Input*>& aInputs, const string& aRegionName,
                                                    const string& aSectorName, double aLifetimeYears, 
                                                    int aPeriod, double aAlphaZero,
                                                    double aSigma ) const
{
    // this is actually 1 / ratio, might be better to return the ratio.
    return aAlphaZero * FunctionUtils::getCapitalInput( aInputs )->getCoefficient();
}

/*! \brief Apply technical change to production functions.
* \details 
* \note This function currently makes a call to
*       FunctionUtils::applyTechChangeInternal so that it can share code with
*       ADemandFunction::applyTechnicalChange. In the future these
*       implementations may vary so that function is not called directly.
* \param input Vector of inputs for the demand function.
* \param aTechChange A structure containing the various possible types of
*        technical change.
* \param regionName Name of the region containing the function.
* \param sectorName Nmae of the sector containing the function.
* \param alphaZero The up-front scaler.
* \param sigma Sigma coefficient.
* \return The new alpha zero.
*/
double LeontiefProductionFn::applyTechnicalChange( vector<Input*>& input, const TechChange& aTechChange,
                                                  const string& regionName, const string& sectorName,
                                                  const int aPeriod, double alphaZero, double sigma ) const 
{
    return FunctionUtils::applyTechnicalChangeInternal( input, aTechChange, regionName, sectorName,
                                                        aPeriod, alphaZero, sigma );
}
