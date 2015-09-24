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
* \file building_service_function.cpp
* \ingroup Objects
* \brief The BuildingServiceFunction class source file.
* \author Pralit Patel
* \author Jiyong Eom
*/

#include "util/base/include/definitions.h"
#include <string>
#include <vector>
#include <cmath>
#include <cassert>

#include "functions/include/building_service_function.h"
#include "functions/include/iinput.h"
#include "functions/include/building_node_input.h"
#include "functions/include/building_service_input.h"
#include "functions/include/satiation_demand_function.h"
#include "sectors/include/sector_utils.h"

using namespace std;

double BuildingServiceFunction::calcCoefficient( InputSet& input, double consumption, const std::string& regionName,
                            const std::string& sectorName, int period, double sigma, double IBT,
                            double capitalStock, const IInput* aParentInput ) const
{
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    const BuildingNodeInput* buildingParentInput = static_cast<const BuildingNodeInput*>( aParentInput );
    // income is 1990 thousand $ and service price is 1975 $
    double income = buildingParentInput->getSubregionalIncome() * 1000 / CVRT90;
    const double floorSpace = buildingParentInput->getPhysicalDemand( period );
    const double internalGainsPerSqMeter = buildingParentInput->getInternalGains( period )
        / floorSpace;
    for( InputSet::iterator inputIter = input.begin(); inputIter != input.end(); ++inputIter ) {
        double coefficient = 1;
        // Guard against zero floorspace which happens for 1975 since no data was read in for
        // that period.
        assert( floorSpace != 0 || period == 0 );
        if( floorSpace != 0 ) {
            BuildingServiceInput* buildingServiceInput = static_cast<BuildingServiceInput*>( *inputIter );
            double thermalLoad = buildingServiceInput->calcThermalLoad( buildingParentInput, internalGainsPerSqMeter, period );
            double servicePerFloorspace = buildingServiceInput->getPhysicalDemand( period ) / floorSpace;
            double servicePrice = max( buildingServiceInput->getPricePaid( regionName, period ), SectorUtils::getDemandPriceThreshold() );
            buildingServiceInput->getSatiationDemandFunction()->calibrateSatiationImpedance( servicePerFloorspace, income / servicePrice, period );
            double serviceDensity = calcServiceDensity( buildingServiceInput, income, regionName, period );
            coefficient =  servicePerFloorspace / ( serviceDensity * thermalLoad );
        }
        (*inputIter)->setCoefficient( coefficient, period );
    }
    return 1;
}

double BuildingServiceFunction::calcDemand( InputSet& input, double consumption, const std::string& regionName,
                       const std::string& sectorName, const double population, int period,
                       double capitalStock, double alphaZero, double sigma, double IBT, const IInput* aParentInput ) const
{
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    const BuildingNodeInput* buildingParentInput = static_cast<const BuildingNodeInput*>( aParentInput );
    // income is 1990 thousand $ and service price is 1975 $
    double income = buildingParentInput->getSubregionalIncome() * 1000 / CVRT90;
    const double floorSpace = buildingParentInput->getPhysicalDemand( period );
    const double internalGainsPerSqMeter = buildingParentInput->getInternalGains( period )
        / floorSpace;
    double totalDemand = 0;
    for( InputSet::iterator inputIter = input.begin(); inputIter != input.end(); ++inputIter ) {
        double demand = 0;
        // Guard against zero floorspace which happens for 1975 since no data was read in for
        // that period.
        assert( floorSpace != 0 || period == 0 );
        if( floorSpace != 0 ) {
            // calculations for energy service
            BuildingServiceInput* buildingServiceInput = static_cast<BuildingServiceInput*>( *inputIter );
            double thermalLoad = buildingServiceInput->calcThermalLoad( buildingParentInput, internalGainsPerSqMeter, period );
            double serviceDensity = calcServiceDensity( buildingServiceInput, income, regionName, period );
            double adjustedServiceDensity = buildingServiceInput->getCoefficient( period ) * thermalLoad * serviceDensity;
            // Set the thermal load adjusted service density back into the input for reporting.
            buildingServiceInput->setServiceDensity( adjustedServiceDensity, period );
            demand = floorSpace * adjustedServiceDensity;
        }
        totalDemand += demand;
        (*inputIter)->setPhysicalDemand( demand, regionName, period );
    }
    return totalDemand;
}

double BuildingServiceFunction::calcLevelizedCost( const InputSet& aInputs, const std::string& aRegionName,
                         const std::string& aSectorName, int aPeriod, double aAlphaZero, double aSigma,
                         const IInput* aParentInput ) const
{
    double parentPrice = 0;
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    const BuildingNodeInput* buildingParentInput = static_cast<const BuildingNodeInput*>( aParentInput );
    // income is 1990 thousand $ and service price is 1975 $
    double income = buildingParentInput->getSubregionalIncome() * 1000 / CVRT90;
    for( InputSet::const_iterator inputIter = aInputs.begin(); inputIter != aInputs.end(); ++inputIter ) {
        // calculation for energy services
        BuildingServiceInput* buildingServiceInput = static_cast<BuildingServiceInput*>( *inputIter );
        double serviceDensity = calcServiceDensity( buildingServiceInput, income, aRegionName, aPeriod );
        double servicePrice = serviceDensity
            * buildingServiceInput->getPricePaid( aRegionName, aPeriod );

        parentPrice += servicePrice;
    }
    return parentPrice;
}

/*!
 * \brief Calculate the per square meter service density.
 * \param aBuildingServiceInput The service input for which to calculate the service density.
 * \param aIncome The converted 1975$ subregional income.
 * \param aRegionName The GCAM region name.
 * \param aPeriod The model period.
 * \return Service density.
 */
double BuildingServiceFunction::calcServiceDensity( BuildingServiceInput* aBuildingServiceInput,
                                                    const double aIncome,
                                                    const string& aRegionName,
                                                    const int aPeriod ) const
{
    const double servicePrice = aBuildingServiceInput->getPricePaid( aRegionName, aPeriod );
    const double cappedPrice = max( servicePrice, SectorUtils::getDemandPriceThreshold() );

    const double serviceAffordability = aIncome / cappedPrice;
    double serviceDensity = aBuildingServiceInput->getSatiationDemandFunction()->calcDemand( serviceAffordability );
    // May need to make an adjustment in case of negative prices.
    if( servicePrice < cappedPrice ) {
        serviceDensity = SectorUtils::adjustDemandForNegativePrice( serviceDensity, servicePrice );
    }
    return serviceDensity;
}
