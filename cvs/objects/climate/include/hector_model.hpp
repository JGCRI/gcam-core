#ifndef _HECTOR_MODEL_HPP_
#define _HECTOR_MODEL_HPP_
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
 * \file hector_model.hpp
 * \ingroup Objects
 * \brief Header file for HectorModel wrapper class
 * \author Robert Link
 */

#include "util/base/include/definitions.h"

#if USE_HECTOR

#include <map>
#include <string>
#include <vector>

#include "climate/include/iclimate_model.h"
#include "climate/source/hector/inst/include/core.hpp"
#include "climate/source/hector/inst/include/csv_outputstream_visitor.hpp"

class IVisitor;

/*!
 * \ingroup Objects
 * \brief IClimateModel interface wrapper for the Hector climate model
 *
 * \details This wrapper class handles communication with the Hector
 *          climate model.  These communications comprise setting
 *          emissions, retrieving climate outputs (temperature,
 *          forcing, etc), and starting runs.  Hector is set up to run
 *          on a period-by-period basis, so at the end of each GCAM
 *          period emissions can be set, the climate model run up to
 *          the current date, and climate outputs retrieved.  All of
 *          these communications are performed using the Hector core's
 *          messaging capability.
 *
 *          The wrapper keeps track of the last year we ran up to.  If
 *          the input year is less than or equal to the last year we
 *          ran to, then we re-initialize Hector, re-run its spin-up,
 *          and run up to the requested date.  This allows us to use
 *          the Hector module in a batch run (where we will reset at
 *          the beginning of each new scenario) or in a stabilization
 *          run (where we might have to run each stabilization period
 *          many times to find the right GHG tax).
 */
class HectorModel: public IClimateModel {
public:
    
    HectorModel();
    
    // IClimateModel interface
    virtual const std::string& getXMLName() const { return getXMLNameStatic(); }
    virtual void toDebugXML( const int period, std::ostream& out, Tabs *tabs ) const;
    virtual void completeInit( const std::string& aScenarioName );
    virtual bool setEmissions( const std::string& aGasName,
                               const int aPeriod, const double aEmissions );
    virtual bool setLUCEmissions( const std::string &aGasName,
                                  const int aYear, const double aEmissions );
    virtual double getEmissions( const std::string& aGasName, const int aYear ) const;
    virtual runModelStatus runModel();
    virtual runModelStatus runModel( const int aPeriod );
    virtual double getConcentration( const std::string& aGasName, const int aYear ) const;
    virtual double getTemperature( const int aYear ) const;
    virtual double getForcing( const std::string& aGasName, const int aYear ) const;
    virtual double getTotalForcing( const int aYear ) const;
    virtual double getNetTerrestrialUptake( const int aYear ) const;
    virtual double getNetOceanUptake( const int aYear ) const;
    virtual void accept( IVisitor *aVisitor, const int aPeriod ) const;

    // xml name
    static const std::string& getXMLNameStatic();
protected:
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        IClimateModel,

        //! A map of the gases Magicc can report out.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "output-gas-name-map", mOutputGasNameMap, std::map<std::string,int> ),
        
        //! Last year to record year-by-year climate
        DEFINE_VARIABLE( SIMPLE, "hector-end-year", mHectorEndYear, int ),
        
        //! Last year to use historical emissions
        DEFINE_VARIABLE( SIMPLE, "emissions-switch-year", mEmissionsSwitchYear, int ),

        //! Last year the climate model has been run to
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "last-calc-year", mLastYear, int ),

        //! Hector initialization file
        DEFINE_VARIABLE( SIMPLE, "hector-ini-file", mHectorIniFile, std::string )
    )

private:


    /* TODO: The gas lookup tables in this class have really
     * proliferated.  We should change them to vectors and include a
     * single lookup table for looking up the index for a gas.  One
     * obstacle to this is that a couple of the tables include the
     * unimplemented gasses (so that we are ready when they get
     * implemented in Hector). */
    
    //! translation between GCAM names for gasses and Hector names.
    std::map<std::string, std::string> mHectorEmissionsMsg;

    //! Hector unit strings
    std::map<std::string, int> mHectorUnits;

    //! table of emissions passed in from GCAM
    std::map<std::string, std::vector<double> > mEmissionsTable;

    //! table of concentrations retrieved from Hector
    std::map<std::string, std::vector<double> > mConcTable;

    //! Some Hector components store their forcing in a time series.
    //! This is a lookup table for the message strings used to
    //! retrieve them.
    std::map<std::string, std::string> mHectorRFTseriesMsg;

    //! table of forcings retrieved from Hector, by gas (for those
    //! that don't keep their data in a time series)
    std::map<std::string, std::vector<double> > mGasRFTable;

    //! total forcings retrieved from Hector
    std::vector<double> mTotRFTable;

    //! temperatures retrieved from Hector
    std::vector<double> mTemperatureTable;

    //! land fluxes retrieved from Hector
    std::vector<double> mLandFlux;

    //! ocean fluxes retrieved from Hector
    std::vector<double> mOceanFlux;

    //! conversion factors from GCAM's output units to Hector's native
    //! units (multiply GCAM's value by this to get the hector value)
    std::map<std::string, double> mUnitConvFac;

    //! Hector core object
    std::auto_ptr<Hector::Core> mHcore;

    //! file handle for the outputstream visitor
    std::auto_ptr<std::ofstream> mOfile;

    //! output stream visitor
    std::auto_ptr<Hector::CSVOutputStreamVisitor> mHosv;
    
    // private functions
    
    //! reset the Hector GCAM component and the Hector model for a new run
    void reset( const int aPeriod );

    //! worker routine for setting emissions
    bool setEmissionsByYear( const std::string& aGasName, const int aYear, double aEmissions );

    //! subroutines for getting data from Hector and storing it in the tables
    void storeConc( const int aYear, const bool aHadError );
    void storeRF( const int aYear, const bool aHadError );
    void storeGlobals( const int aYear, const bool aHadError );

    //! set up the tables used by the functions in the previous block
    void setupConcTbl();
    void setupRFTbl();

    int yearlyDataIndex( const int aYear ) const;
};

#endif // USE_HECTOR

#endif // _HECTOR_MODEL_HPP_

