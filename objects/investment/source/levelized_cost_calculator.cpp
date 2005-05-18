/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Labratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responisbility for the 
	use of this software.
*/

/*! 
* \file levelized_cost_calculator.cpp
* \ingroup Objects
* \brief LevelizedCostCalculator class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <cmath>
#include "investment/include/levelized_cost_calculator.h"
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
LevelizedCostCalculator::LevelizedCostCalculator(){
}

/*! \brief Return the XML name for this object as a string.
* \return The XML name for the object.
*/
const string& LevelizedCostCalculator::getXMLNameStatic(){
    const static string XML_NAME = "levelized-cost-calculator";
    return XML_NAME;
}

/*! \brief Write the object to an XML output stream.
* \param aOut The output stream to write to.
* \param aTabs The object which tracks the number of tabs to write.
*/
void LevelizedCostCalculator::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

/*! \brief Write the object to an XML output stream for debugging.
* \param aPeriod Period to write debugging information for.
* \param aOut The output stream to write to.
* \param aTabs The object which tracks the number of tabs to write.
*/
void LevelizedCostCalculator::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

/*! \brief Calculate the parent level levelized cost.
* \details
* \param aInvestables Children to for which to calculate an average levelized
*        cost.
* \param aNationalAccount National Account container.
* \param aRegionName Name of the region in which investment is occurring.
* \param aGoodName Name of the sector in which investment is occurring.
* \param aInvestmentLogitExp The investment logit exponential.
* \param aIsShareCalc Whether this expected profit rate is being used to
*        calculate shares.
* \param aPeriod The period in which to calculate the expected profit rate.
* \return The parent level levelized cost.
*/
double LevelizedCostCalculator::calcSectorExpectedProfitRate( const vector<IInvestable*>& aInvestables,
                                                              const NationalAccount& aNationalAccount,
                                                              const string& aRegionName,
                                                              const string& aGoodName,
                                                              const double aInvestmentLogitExp,
                                                              const bool aIsShareCalc,
                                                              const int aPeriod ) const
{
    // Sum expected profit rates for the subsector with a logit distribution.
    double levelizedCostNum = 0;
    double levelizedCostDenom = 0;
    
    // I didn't code in beta since as far as i could tell it was always 1. 
    for( InvestmentUtils::CInvestableIterator currInv = aInvestables.begin();
        currInv != aInvestables.end(); ++currInv )
    {
        double currLevelizedCost = (*currInv)->getExpectedProfitRate( aNationalAccount,
                                                                      aRegionName,
                                                                      aGoodName,
                                                                      this,
                                                                      -1 * aInvestmentLogitExp, // HACK
                                                                      aIsShareCalc,
                                                                      aPeriod );
        if( currLevelizedCost > 0 ){
            levelizedCostNum += pow( currLevelizedCost, aInvestmentLogitExp + 1 );
            levelizedCostDenom += pow( currLevelizedCost, aInvestmentLogitExp );
        }
    }
    
    // If this is the share calc return only the sum of the profit rate to the
    // logit.
    if( aIsShareCalc ){
        return levelizedCostDenom;
    }
    // Check for a numerator greater than zero so the profit rate is positive
    // and a denominator greater than zero so the division can occur correctly
    // and the profit rate is positive.
    return ( levelizedCostNum > 0 && levelizedCostDenom > 0 ) ? levelizedCostNum / levelizedCostDenom : 0;
}

/*! \brief Calculate the levelized cost for a technology.
* \details This function calculates the levelized cost for a production
*          technology. Currently this calls into the passed in
*          ProductionDemandFunction and uses the its levelized cost function to
*          calculate a base levelized cost. This levelized cost is then adjusted
*          for the investment tax credit. Adjustments should also be made for
*          technologies with longer lifetimes or which have delays before they
*          come online, but this has not been implemented.
* \param aTechProdFuncInfo A structure containing the neccessary data items to
*        call the production technology's levelized cost function.
* \param aRegionName Name of the region in which investment is occurring.
* \param aGoodName Name of the sector in which investment is occurring.
* \param aDelayedInvestmentTime The lag before this technology will come online.
* \param aLifetime Nameplate lifetime of the technology.
* \param aTimestep Length in years of the timestep.
* \param aPeriod Period in which to calculate the levelized cost.
* \return Levelized cost per unit of output for the technology.
* \todo Need to handle lifetime and delayed investment time.
*/
double LevelizedCostCalculator::calcTechnologyExpectedProfitRate( const ProductionFunctionInfo& aTechProdFuncInfo,
                                                                  const NationalAccount& aNationalAccount,
                                                                  const string& aRegionName,
                                                                  const string& aSectorName,
                                                                  const double aDelayedInvestmentTime,
                                                                  const int aLifetime,
                                                                  const int aTimeStep,
                                                                  const int aPeriod ) const
{
    // Compute the expected profit rate.
    double levelizedCost = aTechProdFuncInfo.mProductionFunction->calcLevelizedCost(
                                                                   aTechProdFuncInfo.mInputs,
                                                                   aRegionName, 
                                                                   aSectorName,
                                                                   aPeriod,
                                                                   aTechProdFuncInfo.mAlphaZeroScaler,
                                                                   aTechProdFuncInfo.mSigma );
    // Decrease the raw levelized cost by the investment tax credit rate.
    // Check if this is right.
    levelizedCost /= ( 1 + aNationalAccount.getAccountValue( NationalAccount::INVESTMENT_TAX_CREDIT ) );
    
    // Need to handle lifetime and delayed investment time.

    /*! \post expected profit is greater than or equal to zero.*/
    assert( levelizedCost >= 0 );
    /*! \post expected profit is a valid number. */
    assert( util::isValidNumber( levelizedCost ) );
    return levelizedCost;
}
