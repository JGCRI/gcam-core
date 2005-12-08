#ifndef _MAGICC_MODEL_H_
#define _MAGICC_MODEL_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file magicc.h
* \ingroup Objects
* \brief The MagiccModel header file.
* \author Josh Lurz
*/

#include <string>
#include <vector>
#include "climate/include/iclimate_model.h"

class Modeltime;
class IVisitor;

/*! 
* \ingroup Objects
* \brief An implementation of the IClimateModel interface using the MAGICC
*        climate module.
* \details The MagiccModel performs climate calculations by passing data to and
*          from the MAGICC fortran module. No climate calculating code is
*          contained in the C++ MagiccModel code. This wrapper is responsible
*          for reading in a set of default gas emissions for each gas by period,
*          overriding those with values from the model where calculated, and
*          interpolating them into a set of inputs for MAGICC. It then writes
*          those values to a file and calls MAGICC to calculate climate
*          parameters. A subset of those output can then be written by this
*          wrapper to the database and a CSV file.
* \author Josh Lurz
*/

class MagiccModel: public IClimateModel {
public:
    MagiccModel( const Modeltime* aModeltime );

    virtual void completeInit( const std::string& aScenarioName );
    
    virtual bool setEmissions( const std::string& aGasName,
                               const int aPeriod,
                               const double aEmission );
    
    virtual bool runModel();

    virtual double getConcentration( const std::string& aGasName,
                                     const int aPeriod ) const;

    virtual double getTemperature( const int aPeriod ) const;
    
    virtual double getForcing( const std::string& aGasName,
                               const int aPeriod ) const;
    
    virtual double getTotalForcing( const int aPeriod ) const;

    virtual void printFileOutput() const;
    virtual void printDBOutput() const;
	void accept( IVisitor* aVisitor, const int aPeriod ) const;
    static const std::string& getXMLNameStatic();
private:
    int getGasIndex( const std::string& aGasName ) const;
	static unsigned int getNumGases();
    void readFile();

	//! A fixed list of the gases Magicc reads in.
	static const std::string sGasNames[];
	
	//! Return value of getGasIndex if it cannot find the gas.
    static const int INVALID_GAS_NAME = -1;
	
	//! Emissions levels by gas and period.
    std::vector<std::vector<double> > mEmissionsByGas;

	//! Name of the scenario.
    std::string mScenarioName;

	//! A reference to the scenario's modeltime object.
    const Modeltime* mModeltime;

    //! Whether the climate model output is updated.
    bool mIsValid;
};

#endif // _MAGICC_MODEL_H_
