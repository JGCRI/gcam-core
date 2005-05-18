#ifndef _IFUNCTION_H_
#define _IFUNCTION_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Labratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file ifunction.h
* \ingroup Objects
* \brief Interface to the AProductionDemandFunction
*
*  Detailed description.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <vector>
#include "functions/include/input.h"

struct TechChange {
    TechChange():mMaterialTechChange( 0 ), mEnergyTechChange( 0 ), mHicksTechChange( 0 ){}
    double mMaterialTechChange; //!< Technical change rate for the materials.
    double mEnergyTechChange; //!< Technical change rate for energy usage.
    double mHicksTechChange; //!< Hicks neutral technical change(applies to all inputs together)
};

class IFunction {
public:
	virtual double calcDemand( std::vector<Input*>& aInputs, double aConsumption, 
        const std::string& aRegionName, const std::string& aSectorName, 
        const double aShutdownCoef,
        int aPeriod, 
        double aCapitalStock = 0, double aAlphaZero = 0, double aSigma = 0, double aIBT = 0 ) const = 0;
    
    virtual double calcCoefficient( std::vector<Input*>& aInput, double aConsumption, 
        const std::string& aRegionName, const std::string& aSectorName, int aPeriod, double aSigma = 0, 
        double aIBT = 0, double aCapitalStock = 0 ) const = 0;

	virtual double transformCoefficients( std::vector<Input*>& aInputs, double aPriceReceived, 
        double aAlphaZero = 0, double aSigma = 0 ) const = 0;
	
    virtual double changeElasticity( std::vector<Input*>& aInputs, double aPriceReceived, 
        double aProfits, double aCapitalStock, double aAlphaZero = 0, double aSigmaNew = 0, 
        double aSigmaOld = 0 ) const = 0;
	
    virtual double applyTechnicalChange( std::vector<Input*>& aInputs, const TechChange& aTechChange,
        const std::string& aRegionName, const std::string& aSectorName, 
        const int aPeriod, double aAlphaZero = 0, double aSigma = 0 ) const = 0;

	virtual double calcOutput( std::vector<Input*>& aInputs, const std::string& aRegionName,
        const std::string& aSectorName, const double aShutdownCoef, int aPeriod,
        double aCapitalStock = 0, double aAlphaZero = 0, 
        double aSigma = 0 ) const = 0;

    virtual double calcProfits( std::vector<Input*>& aInputs, const std::string& aRegionName,
        const std::string& aSectorName, const double aShutdownCoef,
        int aPeriod, double aCapitalStock = 0, double aAlphaZero = 0, 
        double aSigma = 0 ) const = 0;

    virtual double calcLevelizedCost( const std::vector<Input*>& aInputs,
        const std::string& aRegionName, const std::string& aSectorName, int aPeriod,
        double aAlphaZero, double aSigma ) const = 0;
    
    virtual double calcVariableCost( const std::vector<Input*>& aInputs,
        const std::string& aRegionName, const std::string& aSectorName, int aPeriod,
        double aAlphaZero, double aSigma ) const = 0;

    virtual double calcCosts( std::vector<Input*>& aInputs,
        const std::string& aRegionName, int aPeriod ) const = 0;

	virtual double calcExpProfitRate( const std::vector<Input*>& aInputs, const std::string& aRegionName,
        const std::string& aSectorName, double aLifeTimeYears, int aPeriod, double aAlphaZero = 0,
        double aSigma = 0 ) const = 0;

    virtual double getCapitalOutputRatio( const std::vector<Input*>& aInputs,
        const std::string& aRegionName, const std::string& aSectorName, double aLifeTimeYears,
        int aPeriod, double aAlphaZero, double aSigma ) const = 0;
    
    virtual double calcUnscaledProfits( const std::vector<Input*>& aInputs, 
                                        const std::string& aRegionName,
                                        const std::string& aSectorName,
                                        const int aPeriod,
                                        const double aCapitalStock,
                                        const double aAlphaZero,
                                        const double aSigma ) const = 0;
};

#endif // _IFUNCTION_H_


