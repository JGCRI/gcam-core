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
* \file simple_expected_profit_calculator.cpp
* \ingroup Objects
* \brief SimpleExpectedProfitCalculator class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <cmath>
#include "investment/include/simple_expected_profit_calculator.h"
#include "util/base/include/util.h"
#include "investment/include/investment_utils.h"
#include "investment/include/iinvestable.h"
#include "functions/include/ifunction.h"
#include "containers/include/national_account.h"
#include "functions/include/input.h"
#include "functions/include/function_utils.h"
#include "util/base/include/xml_helper.h"

using namespace std;

//! Constructor
SimpleExpectedProfitCalculator::SimpleExpectedProfitCalculator(){
}

/*! \brief Return the XML name for this object as a string.
* \return The XML name for the object.
*/
const string& SimpleExpectedProfitCalculator::getXMLNameStatic(){
    const static string XML_NAME = "simple-expected-profit-calculator";
    return XML_NAME;
}

/*! \brief Write the object to an XML output stream.
* \param aOut The output stream to write to.
* \param aTabs The object which tracks the number of tabs to write.
*/
void SimpleExpectedProfitCalculator::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

/*! \brief Write the object to an XML output stream for debugging.
* \param aPeriod Period to write debugging information for.
* \param aOut The output stream to write to.
* \param aTabs The object which tracks the number of tabs to write.
*/
void SimpleExpectedProfitCalculator::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

/*! \brief Calculate the sector level expected profit rate.
* \details
* \param aInvestables Children to for which to calculate an average expected
*        profit rate.
* \param aNationalAccount National Account container.
* \param aRegionName Name of the region in which investment is occurring.
* \param aGoodName Name of the sector in which investment is occurring.
* \param aInvestmentLogitExp The investment logit exponential.
* \param aIsShareCalc Whether this expected profit rate is being used to
*        calculate shares.
* \param aPeriod The period in which to calculate the expected profit rate.
* \return The sector level expected profit rate.
*/
double SimpleExpectedProfitCalculator::calcSectorExpectedProfitRate( const vector<IInvestable*>& aInvestables,
                                                                     const NationalAccount& aNationalAccount,
                                                                     const string& aRegionName,
                                                                     const string& aGoodName,
                                                                     const double aInvestmentLogitExp,
                                                                     const bool aIsShareCalc,
                                                                     const int aPeriod ) const
{
    // Sum expected profit rates for the subsector with a logit distribution.
    double expProfitRateNum = 0;
    double expProfitRateDenom = 0;
    
    // I didn't code in beta since as far as i could tell it was always 1. 
    for( InvestmentUtils::CInvestableIterator currInv = aInvestables.begin();
        currInv != aInvestables.end(); ++currInv )
    {
        double currExpProfitRate = (*currInv)->getExpectedProfitRate( aNationalAccount,
                                                                      aRegionName,
                                                                      aGoodName,
                                                                      this,
                                                                      aInvestmentLogitExp,
                                                                      aIsShareCalc,
                                                                      aPeriod );
        if( currExpProfitRate > 0 ){
            expProfitRateNum += pow( currExpProfitRate, aInvestmentLogitExp + 1 );
            expProfitRateDenom += pow( currExpProfitRate, aInvestmentLogitExp );
        }
    }
    
    // If this is the share calc return only the sum of the profit rate to the logit.
    if( aIsShareCalc ){
        return expProfitRateDenom;
    }
    // Check for a numerator greater than zero so the profit rate is positive and a denominator
    // greater than zero so the division can occur correctly and the profit rate is positive.
    return ( expProfitRateNum > 0 && expProfitRateDenom > 0 ) ? expProfitRateNum / expProfitRateDenom : 0;
}

/*! \brief Calculate the expected profit of a technology.
* \details
* \param aTechProdFuncInfo A structure containing the neccessary data items to
*        call the production technology's expected profit function.
* \param aRegionName Name of the region in which investment is occurring.
* \param aGoodName Name of the sector in which investment is occurring.
* \param aDelayedInvestmentTime The lag before this technology will come online.
* \param aLifetime Nameplate lifetime of the technology.
* \param aTimestep Length in years of the timestep.
* \param aPeriod Period in which to calculate the expected profit rate.
* \return Final expected profit rate for the technology.
*/
double SimpleExpectedProfitCalculator::calcTechnologyExpectedProfitRate( const ProductionFunctionInfo& aTechProdFuncInfo,
                                                                         const NationalAccount& aNationalAccount,
                                                                         const string& aRegionName,
                                                                         const string& aSectorName,
                                                                         const double aDelayedInvestmentTime,
                                                                         const int aLifetime,
                                                                         const int aTimeStep,
                                                                         const int aPeriod ) const
{
    // Compute the expected profit rate.
    double expectedProfit = aTechProdFuncInfo.mProductionFunction
                                              ->calcExpProfitRate( aTechProdFuncInfo.mInputs,
                                                                   aRegionName, 
                                                                   aSectorName,
                                                                   aLifetime,
                                                                   aPeriod,
                                                                   aTechProdFuncInfo.mAlphaZeroScaler,
                                                                   aTechProdFuncInfo.mSigma );

    // Check that the denominator of the next equation is not zero.
    assert( aNationalAccount.getAccountValue( NationalAccount::INVESTMENT_TAX_CREDIT ) != 1 );
    
    // Increase the raw expected profit by the investment tax credit rate.
    expectedProfit /= ( 1 - aNationalAccount.getAccountValue( NationalAccount::INVESTMENT_TAX_CREDIT ) );

    // Discount the expected profit for when it will come online, at least one period.
    const Input* capInput = FunctionUtils::getInput( aTechProdFuncInfo.mInputs, "Capital" );
    assert( capInput );
    const double pricePaidCapital = capInput->getPricePaid();
    // Calculate the discounted value.
    double discountFactor = 0;
    for( int lagYear = 0; lagYear <= aDelayedInvestmentTime * aTimeStep; lagYear += aTimeStep ){
        discountFactor += 1 / pow( 1 + pricePaidCapital, lagYear );
    }
    discountFactor *= pow( 1 + pricePaidCapital, aDelayedInvestmentTime * aTimeStep )
                      / static_cast<double>( aDelayedInvestmentTime + 1 );
    
    // Check that the discount factor is still a valid number. 
    assert( util::isValidNumber( discountFactor ) );
    
    // Adjust for the discount factor.
    if( discountFactor > 0 ){
        expectedProfit /= discountFactor;
    }
    /*! \post expected profit is greater than or equal to zero.*/
    assert( expectedProfit >= 0 );
    /*! \post expected profit is a valid number. */
    assert( util::isValidNumber( expectedProfit ) );
    return expectedProfit;
}
