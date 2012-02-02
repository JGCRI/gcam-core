#ifndef _BUILDING_SERVICE_FUNCTION_H_
#define _BUILDING_SERVICE_FUNCTION_H_
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
* \file building_service_function.h
* \ingroup Objects
* \brief BuildingServiceFunction class header file.
* \author Pralit Patel
* \author Jiyong Eom
*/

#include <string>
#include <vector>
#include "functions/include/aproduction_function.h"

class IInput;
class BuildingServiceInput;

/*!
 * \ingroup Objects
 * \brief Defines a function to drive building services.
 * \details The function also accommodates for floorspace service by passing down
 *          the building demand directly to that input.  The energy services will
 *          utilize a satiated demand function:
 *          demand = coefficient * floorspace * thermal load * service density
 *          thermal load = degree days * shell conductance * floor to surface ratio
 *                        + internal gains scalar * internal gains per square meter
 *          service density  = ( satiation level - satiation adder ) 
 *                 * ( 1 - e^( -log(2) / satiation impedance * ( income / price ) )
 *                 + satiation adder
 *
 * \author Pralit Patel
 * \author Jiyong Eom
 */
class BuildingServiceFunction : public AProductionFunction {
public:
    double calcDemand( InputSet& input, double consumption, const std::string& regionName,
                       const std::string& sectorName, const double aShutdownCoef, int period,
                       double capitalStock = 0, double alphaZero = 0, double sigma = 0, double IBT = 0,
                       const IInput* aParentInput = 0 ) const;
    
    double calcCoefficient( InputSet& input, double consumption, const std::string& regionName,
                            const std::string& sectorName, int period, double sigma = 0, double IBT = 0,
                            double capitalStock = 0, const IInput* aParentInput = 0 ) const;


    double calcLevelizedCost( const InputSet& aInputs, const std::string& aRegionName,
                              const std::string& aSectorName, int aPeriod, double aAlphaZero, double aSigma,
                              const IInput* aParentInput = 0 ) const;

    // AProductionFunction methods not implemented
    double changeElasticity( InputSet& input, const std::string& aRegionName, double priceReceived,
                             double aProfits, double capitalStock, const int aPeriod, double alphaZero = 0,
                             double sigmaNew = 0, double sigmaOld = 0 ) const
    {
        return 0;
    }
    
    double calcOutput( InputSet& input, const std::string& regionName,
                       const std::string& sectorName, const double aShutdownCoef,
                       int period, double capitalStock = 0, double alphaZero = 0, double sigma = 0 ) const
    {
        return 0;
    }
    
    double calcExpProfitRate( const InputSet& input, const std::string& regionName,
                              const std::string& sectorName, double aLifeTimeYears, int period, double alphaZero = 0,
                              double sigma = 0 ) const
    {
        return 0;
    }

    double getCapitalOutputRatio( const InputSet& aInputs, const std::string& aRegionName,
                                  const std::string& aSectorName, double aLifeTimeYears, int aPeriod,
                                  double aAlphaZero, double aSigma ) const
    {
        return 0;
    }
    
    double applyTechnicalChange( InputSet& input, const TechChange& aTechChange,
                                 const std::string& regionName,const std::string& sectorName, const int aPeriod,
                                 double alphaZero = 0, double sigma = 0 ) const
    {
        return 0;
    }
    
    double calcUnscaledProfits( const InputSet& aInputs, 
                                const std::string& aRegionName,
                                const std::string& aSectorName,
                                const int aPeriod,
                                const double aCapitalStock,
                                const double aAlphaZero,
                                const double aSigma ) const
    {
        return 0;
    }
private:
    virtual double calcCapitalScaler( const InputSet& input, double aAlphaZero, double sigma,
        double capitalStock, const int aPeriod ) const
    {
        return 0;
    }

    double calcServiceDensity( BuildingServiceInput* aBuildingServiceInput,
                               const double aIncome,
                               const std::string& aRegionName,
                               const int aPeriod ) const;
};

#endif // _BUILDING_SERVICE_FUNCTION_H_
