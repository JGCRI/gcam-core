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
* \file building_gompertz_function.cpp
* \ingroup Objects
* \brief The GompertzDemandFunction class source file.
* \author Jon Sampedro
*/

#include "util/base/include/definitions.h"
#include <string>
#include <vector>
#include <cmath>
#include <cassert>

#include "functions/include/building_function.h"
#include "functions/include/iinput.h"
#include "functions/include/building_node_input.h"
#include "functions/include/satiation_demand_function.h"
#include "functions/include/building_gompertz_function.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "sectors/include/sector_utils.h"


using namespace std;

extern Scenario* scenario;


double GompertzDemandFunction::calcDemand(InputSet& input, double income, const std::string& regionName,
    const std::string& sectorName, const double aShutdownCoef, int period,
    double capitalStock, double alphaZero, double sigma, double IBT, const IInput* aParentInput) const {
    assert(input.size() == 1);
    BuildingNodeInput* bldInput = static_cast<BuildingNodeInput*>(input[0]);
    assert(bldInput);
    double unadjustSatiation = bldInput->mUnadjustSatiation;
    double landDensityParam = bldInput->mLandDensityParam;
    double subregionalPopulation = bldInput->mCurrentSubregionalPopulation;
    double habitableLand = bldInput->mHabitableLand;
    double bParam = bldInput->mbParam;
    double incomeParam = bldInput->mIncomeParam;
    double subregionalIncome = bldInput->mCurrentSubregionalIncome;
    double biasAdjustParam = bldInput->mBiasAdjustParam;

    double pcfloorspace = (unadjustSatiation + (-landDensityParam) * log(subregionalPopulation / habitableLand))
        * exp((-bParam)
            * exp((-incomeParam) * log(subregionalIncome)))
            + biasAdjustParam;

    // unit conversions to convert from from billion m^2 to m^2
    const double CONV_M2_BM2 = 1e-9;
    // unit conversions to convert from thous ppl to ppl
    const double CONV_POP_THOUS = 1e3;
    double floorspace = pcfloorspace * CONV_M2_BM2 * subregionalPopulation * CONV_POP_THOUS;

    bldInput->setPhysicalDemand(floorspace, regionName, period);

    return floorspace;
}

