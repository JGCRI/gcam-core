#ifndef _BUILDING_GOMPERTZ_FUNCTION_H_
#define _BUILDING_GOMPERTZ_FUNCTION_H_
#if defined(_MSC_VER)
#pragma once
#endif

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
 * \file building_gompertz_function.h
 * \ingroup Objects
 * \brief GompertzDemandFunction class header file.
 * \author    Jon Sampedro
*/

#include <string>
#include <vector>
#include "functions/include/aproduction_function.h"

class IInput;

/*! 
 * \ingroup Objects
 * \brief A function which defines demand behavior for a building size in terms
 *         of floorspace given the income, population density and base floorspace per capita value
 * \details The total floorspace is estimated as follows:
 *          Per capita floorspace=( unadjust_satiation − land_density_param ∗ log⁡( population/habitable_land))
 *
 *                                  ∗ exp(−b_param
 *
 *                                  ∗ exp(−income_param ∗log⁡(per capita GDP) ) ) − bias_adjust_param
 *
 * \author Jon Sampedro
 */
class GompertzDemandFunction : public AProductionFunction {

public:
    double calcDemand(InputSet& input, double consumption, const std::string& regionName,
        const std::string& sectorName, const double aShutdownCoef, int period,
        double capitalStock = 0, double alphaZero = 0, double sigma = 0, double IBT = 0,
        const IInput* aParentInput = 0) const;


    // AProductionFunction methods not implemented by this function
    
    double calcCoefficient(InputSet& input, double consumption, const std::string& regionName,
        const std::string& sectorName, int period, double sigma = 0, double IBT = 0,
        double capitalStock = 0, const IInput* aParentInput = 0) const 
    {
        return 0;
    }    
    
    double calcLevelizedCost(const InputSet& aInputs, const std::string& aRegionName,
        const std::string& aSectorName, int aPeriod, double aAlphaZero = 0,
        double aSigma = 0, const IInput* aParentInput = 0) const
    {
        return 0;
    }

    
    double changeElasticity(InputSet& input, const std::string& aRegionName, double priceReceived,
        double aProfits, double capitalStock, const int aPeriod, double alphaZero,
        double sigmaNew, double sigmaOld) const
    {
        return 1;
    }

    double calcOutput(InputSet& input, const std::string& regionName,
        const std::string& sectorName, const double aShutdownCoef,
        int period, double capitalStock = 0, double alphaZero = 0, double sigma = 0) const
    {
        return 0;
    }

    double calcExpProfitRate(const InputSet& input, const std::string& regionName,
        const std::string& sectorName, double aLifeTimeYears, int period, double alphaZero = 0,
        double sigma = 0) const
    {
        return 0;
    }


    double getCapitalOutputRatio(const InputSet& aInputs, const std::string& aRegionName,
        const std::string& aSectorName, double aLifeTimeYears, int aPeriod,
        double aAlphaZero, double aSigma) const
    {
        return 0;
    }

    double applyTechnicalChange(InputSet& input, const TechChange& aTechChange,
        const std::string& regionName, const std::string& sectorName, const int aPeriod,
        double alphaZero = 0, double sigma = 0) const
    {
        return 0;
    }

    double calcUnscaledProfits(const InputSet& aInputs,
        const std::string& aRegionName,
        const std::string& aSectorName,
        const int aPeriod,
        const double aCapitalStock,
        const double aAlphaZero,
        const double aSigma) const
    {
        return 0;
    }
private:
    virtual double calcCapitalScaler(const InputSet& input, double aAlphaZero, double sigma,
        double capitalStock, const int aPeriod) const
    {
        return 0;
    }

}; 

#endif // BUILDING_GOMPERTZ_FUNCTION_H_
