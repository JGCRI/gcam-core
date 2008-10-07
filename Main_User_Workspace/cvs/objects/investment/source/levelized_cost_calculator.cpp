/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */


/*! 
 * \file levelized_cost_calculator.cpp
 * \ingroup Objects
 * \brief LevelizedCostCalculator class source file.
 * \author Josh Lurz
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
#include "functions/include/iinput.h"
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
* \details TODO
* \param aInvestables Children to for which to calculate a levelized cost.
* \param aNationalAccount National accounts container.
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
*          come on-line, but this has not been implemented.
* \param aTechProdFuncInfo A structure containing the necessary data items to
*        call the production technology's levelized cost function.
* \param aRegionName Name of the region in which investment is occurring.
* \param aGoodName Name of the sector in which investment is occurring.
* \param aDelayedInvestmentTime The lag before this technology will come on-line.
* \param aLifetime Nameplate lifetime of the technology.
* \param aTimestep Length in years of the time step.
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
