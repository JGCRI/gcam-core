#ifndef _MAGICC_MODEL_H_
#define _MAGICC_MODEL_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file magicc_model.h
* \ingroup Objects
* \brief The MagiccModel header file.
* \author Josh Lurz
*/

#include <map>
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
*          from the MAGICC Fortran module. No climate calculating code is
*          contained in the C++ MagiccModel code. This wrapper is responsible
*          for reading in a set of default gas emissions for each gas by period,
*          overriding those with values from the model where calculated, and
*          interpolating them into a set of inputs for MAGICC. It then writes
*          those values to a file and calls MAGICC to calculate climate
*          parameters. A subset of those output can then be written by this
*          wrapper to the database and a CSV file.
* \note It is possible to run MAGICC using the Objects framework without running
*       the economic model. This is done by reading in a scenario container with
*       only a modeltime object and an empty world object. It will run off the
*       values in the input_gases.emk file.
* \author Josh Lurz
*/

class MagiccModel: public IClimateModel {
public:
    MagiccModel( const Modeltime* aModeltime );

    virtual void completeInit( const std::string& aScenarioName );
    
    static const std::string& getXMLNameStatic();
    virtual void XMLParse( const xercesc::DOMNode* node );
    virtual void toInputXML( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    
    virtual bool setEmissions( const std::string& aGasName,
                               const int aPeriod,
                               const double aEmission );
    
    virtual double getEmissions( const std::string& aGasName,
                                 const int aYear ) const;

    virtual bool runModel();

    virtual double getConcentration( const std::string& aGasName,
                                     const int aYear ) const;

    virtual double getTemperature( const int aYear ) const;
    
    virtual double getForcing( const std::string& aGasName,
                               const int aYear ) const;
    
    virtual double getTotalForcing( const int aYear ) const;

    double getNetTerrestrialUptake( const int aYear ) const;

    double getNetOceanUptake( const int aYear ) const;

    double getNetLandUseChangeEmission( const int aYear ) const;

    virtual int getCarbonModelStartYear() const;

    virtual void printFileOutput() const;
    virtual void printDBOutput() const;
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
private:

    bool isValidClimateModelYear( const int aYear ) const;

    int getGasIndex( const std::string& aGasName ) const;
    static unsigned int getNumGases();
    void readFile();
    void overwriteMAGICCParameters( );

    //! A fixed list of the gases Magicc reads in.
    static const std::string sGasNames[];

    //! A map of the gases Magicc can report out.
    std::map<std::string,int> mOutputGasNameMap; 

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

    //! Climate Sensitivity.
    double mClimateSensitivity;

    //! Soil Feedback Factor (MAGICC Parameter btSoil)
    double mSoilTempFeedback;

    //! Humus Feedback Factor (MAGICC Parameter btHumus)
    double mHumusTempFeedback;

    //! GPP Feedback Factor (MAGICC Parameter btGPP)
    double mGPPTempFeedback;

    //! 1980s Ocean Uptake (MAGICC Parameter FUSER)
    double mOceanCarbFlux80s;

    //! 1980s net terrestrial Deforestation (MAGICC Parameter DUSER)
    double mNetDeforestCarbFlux80s;

    //! The year the carbon model should start running.
    int mCarbonModelStartYear;
};

#endif // _MAGICC_MODEL_H_
