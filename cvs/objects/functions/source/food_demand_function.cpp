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
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*! 
* \file food_demand_function.cpp
* \ingroup Objects
* \brief The FoodDemandFunction class source file.
* \author Pralit Patel
* \author Jiyong Eom
*/

#include "util/base/include/definitions.h"
#include <string>
#include <vector>
#include <cmath>
#include <cassert>

#include "functions/include/food_demand_function.h"
#include "functions/include/iinput.h"
#include "functions/include/food_demand_input.h"
//#include "containers/include/scenario.h"
//#include "util/base/include/model_time.h"
//#include "sectors/include/sector_utils.h"

using namespace std;

//extern Scenario* scenario;

double FoodDemandFunction::calcDemand( InputSet& input, double income, const std::string& regionName,
                       const std::string& sectorName, const double aShutdownCoef, int period,
                       double capitalStock, double alphaZero, double sigma, double IBT, const IInput* aParentInput ) const
{
    double priceMaterials = IBT/*aParentInput->getPricePaid( regionName, period )*/;
    //std::cout << "priceMaterials: " << priceMaterials << endl;
    vector<FoodDemandInput*> foodInputs( input.size() );
    vector<double> adjPrices( input.size() );
    for( size_t i = 0; i < input.size(); ++i ) {
        foodInputs[i] = static_cast<FoodDemandInput*>( input[i] );
        adjPrices[i] = foodInputs[i]->getPrice( regionName, period ) / priceMaterials * foodInputs[i]->getPriceScaler();
        //std::cout << "adjPrices[" << i << "]: " << adjPrices[i] << endl;
    }
    double adjIncome = foodInputs[ 0 ]->getSubregionalIncome() / priceMaterials;
    //std::cout << "adjIncome: " << adjIncome << endl;

    double totalDemand = 1.0;
    double alphaTotal = 0.0;
    vector<double> alphaActual( input.size() );
    vector<double> demands( input.size() );
    for( size_t i = 0; i < input.size(); ++i ) {
        double currDemand = foodInputs[i]->getScaleTerm() */* pow( adjIncome,*/ foodInputs[i]->calcIncomeExponent( adjIncome ) /* ) */ *
            pow( adjPrices[i], foodInputs[i]->calcSelfPriceExponent( adjIncome, regionName, period ) ) *
            pow( adjPrices[(i+1) % 2], foodInputs[i]->calcCrossPriceExponent( foodInputs[(i+1) % 2], adjIncome, regionName, period ) );
        //foodInputs[i]->setPhysicalDemand( currDemand, regionName, period );
        //std::cout << "getScaleTerm[" << i << "]: " << foodInputs[i]->getScaleTerm() << endl;
        //std::cout << "calcIncomeExponent[" << i << "]: " << foodInputs[i]->calcIncomeExponent( adjIncome ) << endl;
        //std::cout << "calcSelfPriceExponent[" << i << "]: " << foodInputs[i]->calcSelfPriceExponent( adjIncome, regionName, period ) << endl;
        //std::cout << "calcCrossPriceExponent[" << i << "]: " << foodInputs[i]->calcCrossPriceExponent( foodInputs[(i+1) % 2], adjIncome, regionName, period ) << endl;
        demands[i] = currDemand;
        //std::cout << "demands[" << i << "]: " << demands[i] << endl;
        totalDemand *= currDemand;
        alphaActual[i] = adjPrices[i] * currDemand / adjIncome / foodInputs[i]->getPriceScaler();
        //std::cout << "alphaActual[" << i << "]: " << alphaActual[i] << endl;
        alphaTotal += alphaActual[i];
    }
    
    // Check budget constraint.  If we're spending more than the total income on
    // food, then reduce nonstaples first, followed by staples.  We'll use the
    // actuals for this calculation.
    //double alphat = alphas_actual + alphan_actual;
    double budget = 1.0;        // Fraction of income available for food. We
                                // didn't expermient with changing this in the
                                // original model development.
    
    if(alphaTotal > budget) {
        if(alphaActual[1] < budget) {
            alphaActual[0] = budget - alphaActual[1];
        }
        else {
            alphaActual[0] = 0.0;
            alphaActual[1] = budget;
        }
        for( size_t i = 0; i < input.size(); ++i ) {
            //foodInputs[i]->setPhysicalDemand(alphaActual[i] * adjIncome/adjPrices[i] * foodInputs[i]->getPriceScaler(), regionName, period );
            demands[i] = alphaActual[i] * adjIncome/adjPrices[i] * foodInputs[i]->getPriceScaler();
        }
        // Recalculate quantities based on the new budget fractions
        //Qs = alphas_actual * x/ws * psscl;
        //Qn = alphan_actual * x/wn * pnscl;
    }
    for( size_t i = 0; i < input.size(); ++i ) {
        foodInputs[i]->setPhysicalDemand( demands[i], regionName, period );
        foodInputs[i]->setActualShare( alphaActual[i], regionName, period );
    }

    return totalDemand;
}

