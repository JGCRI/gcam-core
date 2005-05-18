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
* \file rate_logit_distributor.cpp
* \ingroup Objects
* \brief RateLogitDistributor class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <cmath>
#include <iostream> // remove when we have logging.
#include "investment/include/rate_logit_distributor.h"
#include "investment/include/iinvestable.h"
#include "investment/include/iexpected_profit_calculator.h"
#include "investment/include/investment_utils.h"
#include "util/base/include/util.h"

using namespace std;

/*! \brief Constructor
* \param aInvestmentLogitExp The investment logit exponential.
*/
RateLogitDistributor::RateLogitDistributor( const double aInvestmentLogitExp ):
mInvestmentLogitExp( aInvestmentLogitExp ) {
}

/*! \brief Distribute investment based on the expected profit rates of the
*          children and a logit.
* \details LOTS HERE.
* \param aRateCalc An expected profit rate calculation object.
* \param aInvestables A vector of children which will receive investment.
* \param aNationalAccount The national account for this region.
* \param aRegionName The name of the containing region.
* \param aSectorName The name of the containing sector.
* \param aAmount The amount of investment to distribute.
* \param aPeriod The period in which to distribute investment.
* \return The total amount of investment actually distributed.
* \author Josh Lurz
*/
double RateLogitDistributor::distribute( const IExpectedProfitRateCalculator* aRateCalc,
                                         vector<IInvestable*>& aInvestables,
                                         NationalAccount& aNationalAccount,
                                         const std::string& aRegionName,
                                         const std::string& aSectorName,
                                         const double aAmount,
                                         const int aPeriod ) const 
{
    // Calculate the total expected profit rate. Rename sector to parent or
    // something, it could be subsector.
    const double expProfitRate = aRateCalc->calcSectorExpectedProfitRate( aInvestables,
                                                                          aNationalAccount,
                                                                          aRegionName,
                                                                          aSectorName,
                                                                          mInvestmentLogitExp,
                                                                          true,
                                                                          aPeriod );
    // Determine the shares used to distribute investment. Create a vector of
    // investment shares, one per child.
    vector<double> shares( aInvestables.size() );

    // If the sector total expected profit rate is zero, no investment is done
    // except fixed investments.
    if( expProfitRate > 0 ){
        // Loop through subsectors first to calculate shares.
        for( unsigned int i = 0; i < aInvestables.size(); ++i ){
            // Determine the expected profit rate. This will change I think.
            double currExpProfitRate = aInvestables[ i ]->getExpectedProfitRate( aNationalAccount,
                                                                                 aRegionName,
                                                                                 aSectorName,
                                                                                 aRateCalc,
                                                                                 mInvestmentLogitExp,
                                                                                 false,
                                                                                 aPeriod );

            // Calculate the share. The share will default to zero if the profit
            // rate is not positive.
            if( currExpProfitRate > 0 ){
				shares[ i ] = pow( currExpProfitRate, mInvestmentLogitExp ) / expProfitRate;
            }
        }
    }
    
    // Calculate the variable amount of investment.
    const double varInvestment = aAmount - InvestmentUtils::calcFixedInvestment( aInvestables, aPeriod );
    
    // Normalize the investment shares.
    double sumShares = InvestmentUtils::normalizeShares( shares );

    // Check if the shares could not be normalized, which means they have
    // expected profit rates of zero, and there is variable investment which
    // should be distributed.
    if( sumShares < 1 && varInvestment > 0 ){
        cout << "Warning: Variable investment will not be distributed as there are no children"
             << " with positive expected profit rates in sector " 
             << aSectorName << " in region " << aRegionName << "." << endl;
    }

    // Distribute new investment.
    double sumDistributed = 0;
    for( unsigned int i = 0; i < aInvestables.size(); ++i ){
        // If the subsector only has fixed investment the share will be zero.
        // Fixed investment will be used at the child level. This will pass zero
        // investment to the child for fixed investment.
        sumDistributed += aInvestables[ i ]->distributeInvestment( this,
                                                                   aNationalAccount,
                                                                   aRateCalc,
                                                                   aRegionName,
                                                                   aSectorName,
                                                                   varInvestment * shares[ i ],
                                                                   aPeriod );
    }
    // Check if the amount distributed is equal to that was passed in.
    if( !util::isEqual( sumDistributed, aAmount ) ){
        cout << "Warning: " << aAmount - sumDistributed << " difference between requested and distributed investment in "
             << aSectorName << " in " << aRegionName << "." << endl;
    }
    return sumDistributed;
}
