#ifndef _PRODUCTION_DEMAND_FUNCTION_H_
#define _PRODUCTION_DEMAND_FUNCTION_H_
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
* \file production_demand_function.h
* \ingroup Objects
* \brief The header file for all IProductionFunction's.
* \author Pralit Patel
* \author Sonny Kim
*/

#include <string>
#include <vector>
#include "functions/include/ifunction.h"
class Input;

/*! 
* \ingroup Objects
* \brief Defines a common base class for all demand functions.
* \details TODO
* \author Pralit Patel, Sonny Kim, Josh Lurz
* \todo Clean up default arguments and make the input vector constant where possible.
*/
class ADemandFunction: public IFunction {
public:
    // Functions derived classes redefine.
	virtual double calcDemand( std::vector<Input*>& input, double consumption,
        const std::string& regionName, const std::string& sectorName, const double aShutdownCoef,
        int period,
        double capitalStock = 0, double alphaZero = 0, double sigma = 0, 
        double IBT = 0 ) const = 0;
    
    virtual double calcCoefficient( std::vector<Input*>& input, double consumption,
        const std::string& regionName, const std::string& sectorName, int period,
        double sigma = 0, double IBT = 0, double capitalStock = 0 ) const = 0;
	
    // Functions defined by DemandFunction.
    double applyTechnicalChange( std::vector<Input*>& input, const TechChange& aTechChange, 
        const std::string& regionName, const std::string& sectorName, 
        const int aPeriod, double alphaZero = 0, double sigma = 0 ) const;
    
    // Functions defined as null in DemandFunction.
    double transformCoefficients( std::vector<Input*>& input,
                                  double priceReceived,
                                  const int aPeriod,
                                  double alphaZero = 0,
                                  double sigma = 0 ) const 
    { 
        return 0; 
    }

    double changeElasticity( std::vector<Input*>& input, double priceReceived, double aProfits, 
        double capitalStock, const int aPeriod, double alphaZero = 0,
        double sigmaNew = 0, 
        double sigmaOld = 0 ) const 
    { 
        return 0; 
    }

	double calcOutput( std::vector<Input*>& input,const std::string& regionName,
        const std::string& sectorName, const double aShutdownCoef,
        int period, double capitalStock = 0, double alphaZero = 0, 
        double sigma = 0 ) const 
    { 
        return 0; 
    }

    double calcProfits( std::vector<Input*>& input,const std::string& regionName, 
        const std::string& sectorName, const double aShutdownCoef, int period,
        double capitalStock = 0, double alphaZero = 0, 
        double sigma = 0 ) const 
    { 
        return 0; 
    }

    double calcCosts( std::vector<Input*>& input, const std::string& regionName, 
        int period ) const 
    { 
        return 0; 
    }

    double calcExpProfitRate( const std::vector<Input*>& input, const std::string& regionName,
        const std::string& sectorName, double aLifeTimeYears, int period, double alphaZero = 0,
        double sigma = 0 ) const
    { 
        return 0; 
    }

    double getCapitalOutputRatio( const std::vector<Input*>& aInputs, const std::string& aRegionName,
        const std::string& aSectorName, double aLifeTimeYears, int aPeriod, double aAlphaZero = 0,
        double aSigma = 0 ) const 
    {
        return 0;
    }

    double calcLevelizedCost( const std::vector<Input*>& aInputs, const std::string& aRegionName,
        const std::string& aSectorName, int aPeriod, double aAlphaZero, double aSigma ) const
    {
        return 0;
    }

    double calcVariableCost( const std::vector<Input*>& aInputs,
        const std::string& aRegionName,
        const std::string& aSectorName,
        int aPeriod,
        double aAlphaZero,
        double aSigma ) const 
    {
        return 0;
    }

    double calcUnscaledProfits( const std::vector<Input*>& aInputs, 
                                const std::string& aRegionName,
                                const std::string& aSectorName,
                                const int aPeriod,
                                const double aCapitalStock,
                                const double aAlphaZero,
                                const double aSigma ) const
        {
            return 0;
        }
};

/*! 
* \ingroup Objects
* \brief Defines the HouseholdConsumer demand function.
* \details TODO
* \author Pralit Patel, Sonny Kim, Josh Lurz
*/
class HouseholdDemandFn : public ADemandFunction {
public:
	double calcDemand( std::vector<Input*>& input, double consumption,const std::string& regionName,
        const std::string& sectorName, const double aShutdownCoef,
        int period, double capitalStock = 0, double alphaZero = 0, double sigma = 0,
        double IBT = 0 ) const;

    double calcCoefficient( std::vector<Input*>& input, double consumption, const std::string& regionName,
        const std::string& sectorName, int period, double sigma = 0, double IBT = 0, double capitalStock = 0 ) const;
};

/*! 
* \ingroup Objects
* \brief Defines the GovernmentConsumer demand function.
* \details TODO
* \author Pralit Patel, Sonny Kim, Josh Lurz
*/
class GovtDemandFn : public ADemandFunction {
public:
	double calcDemand( std::vector<Input*>& input, double consumption, const std::string& regionName,
        const std::string& sectorName, const double aShutdownCoef, 
        int period, double capitalStock = 0, double alphaZero = 0, double sigma = 0,
        double IBT = 0 ) const;

    double calcCoefficient( std::vector<Input*>& input, double consumption, const std::string& regionName,
        const std::string& sectorName, int period, double sigma = 0, double IBT = 0, double capitalStock = 0 ) const;
};

/*! 
* \ingroup Objects
* \brief Defines the TradeConsumer demand function.
* \details TODO
* \author Pralit Patel, Sonny Kim, Josh Lurz
*/
class TradeDemandFn : public ADemandFunction {
public:
	double calcDemand( std::vector<Input*>& input, double consumption, const std::string& regionName,
        const std::string& sectorName, const double aShutdownCoef, int period, double capitalStock = 0, double alphaZero = 0, double sigma = 0,
        double IBT = 0 ) const;

    double calcCoefficient( std::vector<Input*>& input, double consumption, const std::string& regionName,
        const std::string& sectorName, int period, double sigma = 0, double IBT = 0, double capitalStock = 0 ) const;
};

/*! 
* \ingroup Objects
* \brief Defines the InvestmentConsumer demand function.
* \details TODO
* \author Pralit Patel, Sonny Kim, Josh Lurz
*/
class InvestDemandFn : public ADemandFunction {
public:
	double calcDemand( std::vector<Input*>& input, double consumption, const std::string& regionName,
        const std::string& sectorName, const double aShutdownCoef, int period, double capitalStock = 0, double alphaZero = 0, double sigma = 0,
        double IBT = 0  ) const;

    double calcCoefficient( std::vector<Input*>& input, double consumption, const std::string& regionName,
        const std::string& sectorName, int period, double sigma = 0, double IBT = 0, double capitalStock = 0 ) const;
};

/*! 
* \ingroup Objects
* \brief Defines a common base class for all production functions.
* \details TODO
* \author Pralit Patel, Sonny Kim, Josh Lurz
*/
class AProductionFunction: public IFunction {
public:
	virtual double calcDemand( std::vector<Input*>& input, double consumption, const std::string& regionName,
        const std::string& sectorName, const double aShutdownCoef, int period, double capitalStock = 0, double alphaZero = 0, double sigma = 0,
        double IBT = 0 ) const = 0;

	virtual double calcCoefficient( std::vector<Input*>& input, double consumption, const std::string& regionName,
        const std::string& sectorName, int period, double sigma = 0, double IBT = 0, double capitalStock = 0 ) const = 0;
	
    virtual double transformCoefficients( std::vector<Input*>& input,
                                          double priceReceived,
                                          const int aPeriod,
                                          double alphaZero = 0,
        double sigma = 0 ) const = 0;

	virtual double changeElasticity( std::vector<Input*>& input, double priceReceived, double aProfits,
        double capitalStock, const int aPeriod, double alphaZero = 0,
        double sigmaNew = 0, double sigmaOld = 0 ) const = 0;
	
    virtual double calcOutput( std::vector<Input*>& input, const std::string& regionName,
        const std::string& sectorName, const double aShutdownCoef,
        int period, double capitalStock = 0, double alphaZero = 0, double sigma = 0 ) const = 0;
	
    double calcProfits( std::vector<Input*>& input, const std::string& regionName, const std::string& sectorName,
         const double aShutdownCoef, int period, double capitalStock = 0, double alphaZero = 0, double sigma = 0 ) const;
    
    double calcCosts( std::vector<Input*>& input, const std::string& regionName, int period ) const;
	
	virtual double calcExpProfitRate( const std::vector<Input*>& input, const std::string& regionName,
        const std::string& sectorName, double aLifeTimeYears, int period, double alphaZero = 0,
        double sigma = 0 ) const = 0;
    
    virtual double getCapitalOutputRatio( const std::vector<Input*>& aInputs, const std::string& aRegionName,
        const std::string& aSectorName, double aLifeTimeYears, int aPeriod, double aAlphaZero,
        double aSigma ) const = 0;
    
    virtual double calcLevelizedCost( const std::vector<Input*>& aInputs, const std::string& aRegionName,
        const std::string& aSectorName, int aPeriod, double aAlphaZero = 0, double aSigma = 0 ) const = 0;

    virtual double calcVariableCost( const std::vector<Input*>& aInputs,
        const std::string& aRegionName,
        const std::string& aSectorName,
        int aPeriod,
        double aAlphaZero,
        double aSigma ) const = 0;

    virtual double applyTechnicalChange( std::vector<Input*>& input, const TechChange& aTechChange,
        const std::string& regionName,const std::string& sectorName, const int aPeriod, 
        double alphaZero = 0, double sigma = 0 ) const = 0;
    
    virtual double calcUnscaledProfits( const std::vector<Input*>& aInputs, 
                                        const std::string& aRegionName,
                                        const std::string& aSectorName,
                                        const int aPeriod,
                                        const double aCapitalStock,
                                        const double aAlphaZero,
                                        const double aSigma ) const = 0;
protected:
    virtual double calcCapitalScaler( const std::vector<Input*>& input, double aAlphaZero, double sigma,
        double capitalStock ) const = 0;
};

/*! 
* \ingroup Objects
* \brief Defines the CES production function.
* \details TODO
* \author Pralit Patel, Sonny Kim, Josh Lurz
*/
class CESProductionFn : public AProductionFunction {
public:
	double calcDemand( std::vector<Input*>& input, double consumption, const std::string& regionName,
        const std::string& sectorName, const double aShutdownCoef, int period, double capitalStock = 0,
        double alphaZero = 0, double sigma = 0,
        double IBT = 0 ) const;
	
    double calcCoefficient( std::vector<Input*>& input, double consumption, const std::string& regionName,
       const std::string& sectorName, int period, double sigma = 0, double IBT = 0, double capitalStock = 0 ) const;

	double transformCoefficients( std::vector<Input*>& input,
                                  double priceReceived,
                                  const int aPeriod,
                                  double alphaZero = 0,
        double sigma = 0 ) const;
	
    double changeElasticity( std::vector<Input*>& input, double priceReceived, double aProfits,
        double capitalStock, const int aPeriod, double alphaZero = 0,
        double sigmaNew = 0, double sigmaOld = 0 ) const;
	
    double calcOutput( std::vector<Input*>& input, const std::string& regionName,
        const std::string& sectorName, const double aShutdownCoef,
        int period, double capitalStock = 0, double alphaZero = 0, double sigma = 0 ) const;
	
    double calcExpProfitRate( const std::vector<Input*>& input, const std::string& regionName,
        const std::string& sectorName, double aLifeTimeYears, int period, double alphaZero = 0,
        double sigma = 0 ) const;

    double calcLevelizedCost( const std::vector<Input*>& aInputs, const std::string& aRegionName,
        const std::string& aSectorName, int aPeriod, double aAlphaZero, double aSigma ) const;
    
    double calcVariableCost( const std::vector<Input*>& aInputs,
                             const std::string& aRegionName,
                             const std::string& aSectorName,
                             int aPeriod,
                             double aAlphaZero,
                             double aSigma ) const;

    double getCapitalOutputRatio( const std::vector<Input*>& aInputs, const std::string& aRegionName,
        const std::string& aSectorName, double aLifeTimeYears, int aPeriod, double aAlphaZero,
        double aSigma ) const;
    
    double applyTechnicalChange( std::vector<Input*>& input, const TechChange& aTechChange,
        const std::string& regionName,const std::string& sectorName, const int aPeriod, 
        double alphaZero = 0, double sigma = 0 ) const;
    
    double calcUnscaledProfits( const std::vector<Input*>& aInputs, 
                                const std::string& aRegionName,
                                const std::string& aSectorName,
                                const int aPeriod,
                                const double aCapitalStock,
                                const double aAlphaZero,
                                const double aSigma ) const;
private:
	double normalizeAlphaZero( std::vector<Input*>& input, double aAlphaZero, double sigma ) const;
    double calcCapitalScaler( const std::vector<Input*>& input, double aAlphaZero, double sigma, double capitalStock ) const;
    double calcCapitalRateScaler( const std::vector<Input*>& input, double sigma ) const;
	double calcFinalProfitScaler( const std::vector<Input*>& input, const std::string& regionName,
        const std::string& sectorName, int period, double alphaZero, double sigma ) const;
	
    double calcExpProfitScaler( const std::vector<Input*>& input, double aLifetimeYears,
        const std::string& regionName, const std::string& sectorName, int period, double alphaZero,
        double sigma ) const;
};

/*! 
* \ingroup Objects
* \brief Defines the Leontief production function.
* \details TODO
* \todo Evaluate if CES production functions with zero elasticities could replace
*       this class.
* \author Pralit Patel, Sonny Kim, Josh Lurz
*/
class LeontiefProductionFn : public AProductionFunction {
public:
	double calcDemand( std::vector<Input*>& input, double consumption, const std::string& regionName,
        const std::string& sectorName, const double aShutdownCoef, int period, double capitalStock = 0, double alphaZero = 0, double sigma = 0,
        double IBT = 0 ) const;
    
    double calcCoefficient( std::vector<Input*>& input, double consumption, const std::string& regionName,
        const std::string& sectorName, int period, double sigma = 0, double IBT = 0, double capitalStock = 0 ) const;
	
    double transformCoefficients( std::vector<Input*>& input,
                                  double priceReceived,
                                  const int aPeriod,
                                  double alphaZero = 0,
        double sigma = 0 ) const;
	
    double changeElasticity( std::vector<Input*>& input, double priceReceived, double aProfits, double capitalStock,
        const int aPeriod,
        double alphaZero = 0, double sigmaNew = 0, double sigmaOld = 0 ) const;
	
    double calcOutput( std::vector<Input*>& input, const std::string& regionName,
        const std::string& sectorName, const double aShutdownCoef,
        int period, double capitalStock = 0, double alphaZero = 0, double sigma = 0 ) const;
    
    double calcExpProfitRate( const std::vector<Input*>& input, const std::string& regionName, 
        const std::string& sectorName, double aLifeTimeYears, int period, double alphaZero = 0,
        double sigma = 0 ) const;
    
    double calcLevelizedCost( const std::vector<Input*>& aInputs, const std::string& aRegionName,
        const std::string& aSectorName, int aPeriod, double aAlphaZero, double aSigma ) const;

    double calcVariableCost( const std::vector<Input*>& aInputs,
                             const std::string& aRegionName,
                             const std::string& aSectorName,
                             int aPeriod,
                             double aAlphaZero,
                             double aSigma ) const;

    double getCapitalOutputRatio( const std::vector<Input*>& aInputs, const std::string& aRegionName,
        const std::string& aSectorName, double aLifeTimeYears, int aPeriod, double aAlphaZero = 0,
        double aSigma = 0 ) const;

    double applyTechnicalChange( std::vector<Input*>& input, const TechChange& aTechChange,
        const std::string& regionName,const std::string& sectorName, const int aPeriod, 
        double alphaZero = 0, double sigma = 0 ) const;

    double calcUnscaledProfits( const std::vector<Input*>& aInputs, 
                                const std::string& aRegionName,
                                const std::string& aSectorName,
                                const int aPeriod,
                                const double aCapitalStock,
                                const double aAlphaZero,
                                const double aSigma ) const;
protected:
    double calcCapitalScaler( const std::vector<Input*>& input, double aAlphaZero, double sigma, double capitalStock ) const;
};

#endif // _PRODUCTION_DEMAND_FUNCTION_H_


