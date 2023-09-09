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
* \file building_function.cpp
* \ingroup Objects
* \brief The BuildingFunction class source file.
* \author Pralit Patel
* \author Jiyong Eom
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
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "sectors/include/sector_utils.h"

using namespace std;

extern Scenario* scenario;

double BuildingFunction::calcCoefficient( InputSet& input, double consumption, const std::string& regionName,
                            const std::string& sectorName, int period, double sigma, double IBT,
                            double capitalStock, const IInput* aParentInput ) const
{
    // TODO: for SGM which should be converted to use the aParentInput approach;
    //       the first input is the parent node input
    InputSet::iterator inputIter = input.begin();
    for( ++inputIter; inputIter != input.end(); ++inputIter ) {
        BuildingNodeInput* buildingNodeInput = static_cast<BuildingNodeInput*>( *inputIter );
        double perCapitaBaseFloorspace = (*inputIter)->getPhysicalDemand( period )
            / buildingNodeInput->getSubregionalPopulation();
        // first calibrate the satiation impedance
        SatiationDemandFunction* demandFunction = buildingNodeInput->getSatiationDemandFunction();
        demandFunction->calibrateSatiationImpedance( perCapitaBaseFloorspace, buildingNodeInput->getSubregionalIncome(regionName, period), period );
    }
    return 1;
}

double BuildingFunction::calcDemand( InputSet& input, double income, const std::string& regionName,
                       const std::string& sectorName, const double aShutdownCoef, int period,
                       double capitalStock, double alphaZero, double sigma, double IBT, const IInput* aParentInput ) const
{
    double totalDemand = 0;
    const Modeltime* modeltime = scenario->getModeltime();
    for( InputSet::iterator inputIter = input.begin(); inputIter != input.end(); ++inputIter ) {
        BuildingNodeInput* buildingNodeInput = static_cast<BuildingNodeInput*>( *inputIter );
        // Compute energy service price change and raise it to the price exponenet.
        double priceRatio = period > modeltime->getFinalCalibrationPeriod() ?
            buildingNodeInput->getPricePaid( regionName, period ) / buildingNodeInput->getPricePaid( regionName, modeltime->getFinalCalibrationPeriod() )
            : 1;
        double cappedPriceRatio = max( priceRatio, SectorUtils::getDemandPriceThreshold() );
        double priceTerm = pow( cappedPriceRatio, buildingNodeInput->getPriceElasticity( period ) );
        // Use the satiation demand function to calculate per capita demand.
        double perCapitaDemand =
            buildingNodeInput->getSatiationDemandFunction()->calcDemand( buildingNodeInput->getSubregionalIncome(regionName, period) * priceTerm );
        // May need to make an adjustment in case of negative prices.
        if( priceRatio < cappedPriceRatio && buildingNodeInput->getPriceElasticity( period ) != 0 ) {
            perCapitaDemand = SectorUtils::adjustDemandForNegativePrice( perCapitaDemand, priceRatio );
        }
        // Multiply by population to get total demand.
        double demand = perCapitaDemand * buildingNodeInput->getSubregionalPopulation();
        (*inputIter)->setPhysicalDemand( demand, regionName, period );
        totalDemand += demand;
    }

    return totalDemand;
}

double BuildingFunction::calcLevelizedCost( const InputSet& aInputs, const std::string& aRegionName,
                         const std::string& aSectorName, int aPeriod, double aAlphaZero, double aSigma,
                         const IInput* aParentInput ) const
{
    double price = 0;
    // For the levelized cost we are doing an unweighted price sum.  This price is not
    // utilized.
    for( InputSet::const_iterator inputIter = aInputs.begin(); inputIter != aInputs.end(); ++inputIter ) {
        price += (*inputIter)->getPricePaid( aRegionName, aPeriod );
    }
    return price;
}
