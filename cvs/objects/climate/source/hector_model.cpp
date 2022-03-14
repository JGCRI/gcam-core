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
 * \file hector_model.cpp
 * \ingroup Objects
 * \brief Implementation for the HectorModel class.
 * \author Robert Link
 */

#include <memory>
#include <limits>
#include <fstream>

#include "climate/include/hector_model.hpp"

#if USE_HECTOR 

#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"

#include "climate/source/hector/inst/include/component_data.hpp"
#include "climate/source/hector/inst/include/unitval.hpp"
#include "climate/source/hector/inst/include/message_data.hpp"
#include "climate/source/hector/inst/include/core.hpp"
#include "climate/source/hector/inst/include/ini_to_core_reader.hpp"
#include "climate/source/hector/inst/include/h_exception.hpp"
#include "climate/source/hector/inst/include/csv_outputstream_visitor.hpp"

using namespace std;

extern Scenario* scenario;

namespace {
    // These are multiplicative conversion factors.  I.e., if you have
    // the first unit, multiply by the factor to get the second.
    const double TG_TO_PG = 1.0e-3; // 1 Pg = 1000 Tg
    const double TG_TO_GG = 1.0e3;  // Also, 1Tg = 1Mt
    const double GG_TO_TG = 1.0e-3;
    const double S_TO_SO2 = 2.0;       // 2g SO2 has 1g of S

    // default values
    const int def_end_year = 2300;
    const int def_switch_year = 2005;
    const char *def_ini_file = "../input/climate/hector-gcam.ini";

    // don't ask
    bool hector_log_is_init = false;
} 

HectorModel::HectorModel()
{
    // Set default values for config variables.  All of these can be
    // overridden in XML input.
    
    // HectorEndYear allows for running the climate model past the
    // end of the gcam run.
    mHectorEndYear = def_end_year;
    // EmissionsSwitchYear is the last year of historical emissions.
    // Subsequent years will use GCAM emissions
    mEmissionsSwitchYear = def_switch_year;
    // Hector config location.  
    mHectorIniFile = def_ini_file; 
}



const string& HectorModel::getXMLNameStatic() {
    static string XMLNAME( "HectorModel" );
    return XMLNAME;
}

void HectorModel::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag( getXMLName(), out, tabs );
    XMLWriteElement( mHectorEndYear, "hector-end-year", out, tabs );
    XMLWriteElement( mEmissionsSwitchYear, "emissions-switch-year", out, tabs );
    XMLWriteElement( mHectorIniFile, "hector-ini-file", out, tabs );
    XMLWriteClosingTag( getXMLName(), out, tabs );
} 


/*!
 * \brief Do the setup for the HectorModel GCAM component
 *
 * \details This is different from the setup for the Hector model.
 *          Here we are allocating the data structures that the GCAM
 *          component uses do do its work.  This should be done only
 *          once per GCAM run, no matter how many scenarios we
 *          evaluate in that run.  The Hector setup is done in the
 *          reset() function.  We call that as the last thing we do in
 *          this function, as well as any time we are starting a new
 *          scenario or re-doing a period we've already run.
 */
void HectorModel::completeInit( const string& aScenarioName ) {
    ILogger& climatelog = ILogger::getLogger( "climate-log" );
    climatelog.setLevel( ILogger::NOTICE );
    
    mLastYear = 0;

    climatelog << "Climate model is Hector.  Configuration:"
               << endl << "\thector-end-year = " << mHectorEndYear
               << endl << "\temissions-switch-year = " << mEmissionsSwitchYear
               << endl << "\thector-ini-file = " << mHectorIniFile
               << endl;

    try {
        climatelog << "Setting up stub Hector core." << endl;
        if( mHcore.get() ) {
          // delete all hector components
          mHcore->shutDown();
        }
        mHcore.reset( new Hector::Core(Hector::Logger::WARNING, true, true) );
        mHcore->init();
        climatelog << "Parsing ini file= " << mHectorIniFile << endl;
        Hector::INIToCoreReader coreParser( mHcore.get() );
        coreParser.parse( mHectorIniFile ); 
    }
    catch( const h_exception& e ) {
        cerr << "Exception: " << e << endl;
        throw;
    } 

    // Set up the name tables for each of the gases that GCAM and
    // hector both know about.  
    mHectorEmissionsMsg["CO2"]           = D_FFI_EMISSIONS; 
    mHectorEmissionsMsg["CO2NetLandUse"] = D_LUC_EMISSIONS;
    mHectorEmissionsMsg["SO2tot"]        = D_EMISSIONS_SO2;
    mHectorEmissionsMsg["CF4"]           = D_EMISSIONS_CF4;
    mHectorEmissionsMsg["C2F6"]          = D_EMISSIONS_C2F6;
    mHectorEmissionsMsg["HFC23"]        = D_EMISSIONS_HFC23;
    mHectorEmissionsMsg["HFC125"]        = D_EMISSIONS_HFC125;
    mHectorEmissionsMsg["HFC134a"]       = D_EMISSIONS_HFC134a;
    mHectorEmissionsMsg["HFC143a"]       = D_EMISSIONS_HFC143a;
    mHectorEmissionsMsg["HFC227ea"]        = D_EMISSIONS_HFC227ea;
    mHectorEmissionsMsg["HFC245fa"]      = D_EMISSIONS_HFC245fa;
    mHectorEmissionsMsg["SF6"]           = D_EMISSIONS_SF6;
    mHectorEmissionsMsg["BC"]            = D_EMISSIONS_BC;
    mHectorEmissionsMsg["OC"]            = D_EMISSIONS_OC;
    mHectorEmissionsMsg["NOx"]           = D_EMISSIONS_NOX;
    mHectorEmissionsMsg["CO"]            = D_EMISSIONS_CO;
    mHectorEmissionsMsg["NMVOCs"]         = D_EMISSIONS_NMVOC;

    mHectorEmissionsMsg["CH4"] = D_EMISSIONS_CH4;
    mHectorEmissionsMsg["N2O"] = D_EMISSIONS_N2O;

    // Not implemented in hector at all: regional SO2 (total
    // SO2 is in)

    // Implemented in Hector, but not in GCAM:
    // CFC11, CFC12, CFC113, CFC114, CFC115,
    // CCl4, CH3CCl3, HCF22, HCF141b, HCF142b, halon1200, halon1301,
    // halon2402, CH3Cl, CH3Br (default emissions will be used for
    // these.

    // Set up the message tables for components (mostly halocarbons)
    // that store their radiative forcing as a time series.
    mHectorRFTseriesMsg["CF4"]      = D_RF_CF4;
    mHectorRFTseriesMsg["C2F6"]     = D_RF_C2F6;
    mHectorRFTseriesMsg["HFC23"]     = D_RF_HFC23;
    mHectorRFTseriesMsg["HFC125"]   = D_RF_HFC125;
    mHectorRFTseriesMsg["HFC134A"]  = D_RF_HFC134a;
    mHectorRFTseriesMsg["HFC143A"]  = D_RF_HFC143a;
    mHectorRFTseriesMsg["HFC227ea"]  = D_RF_HFC227ea;
    mHectorRFTseriesMsg["HFC245fa"] = D_RF_HFC245fa;
    mHectorRFTseriesMsg["SF6"]      = D_RF_SF6;
    mHectorRFTseriesMsg["Albedo"]   = D_RF_T_ALBEDO;
    mHectorRFTseriesMsg["HFC32"]     = D_RF_HFC32;
    mHectorRFTseriesMsg["HFC4310"]   = D_RF_HFC4310;
    mHectorRFTseriesMsg["CFC11"]     = D_RF_CFC11;
    mHectorRFTseriesMsg["CFC12"]     = D_RF_CFC12;
    mHectorRFTseriesMsg["CFC113"]    = D_RF_CFC113;
    mHectorRFTseriesMsg["CFC114"]    = D_RF_CFC114;
    mHectorRFTseriesMsg["CFC115"]    = D_RF_CFC115;
    mHectorRFTseriesMsg["CCl4"]      = D_RF_CCl4;
    mHectorRFTseriesMsg["CH3CCl3"]   = D_RF_CH3CCl3;
    mHectorRFTseriesMsg["HCF22"]     = D_RF_HCF22;
    mHectorRFTseriesMsg["HCF141b"]   = D_RF_HCF141b;
    mHectorRFTseriesMsg["HCF142b"]   = D_RF_HCF142b;
    mHectorRFTseriesMsg["halon1211"] = D_RF_halon1211;
    mHectorRFTseriesMsg["halon1301"] = D_RF_halon1301;
    mHectorRFTseriesMsg["halon2402"] = D_RF_halon2402;
    mHectorRFTseriesMsg["CH3Cl"]     = D_RF_CH3Cl;
    mHectorRFTseriesMsg["CH3Br"]     = D_RF_CH3Br;
    
    // Set up the storage for GCAM emissions for each of the gasses we
    // know about.  We need this data to report emissions when we are
    // asked for them (since Hector isn't set up to report its
    // inputs).
    int nrslt = yearlyDataIndex( mHectorEndYear ) + 1;
    map<std::string, std::string>::const_iterator it;
    for( it = mHectorEmissionsMsg.begin(); it != mHectorEmissionsMsg.end(); ++it ) {
        mEmissionsTable[ it->first ].resize (scenario->getModeltime()->getmaxper() );
        mUnitConvFac[ it->first ] = 1.0; // default value; will set exceptions below
        mHectorUnits[ it->first ] = Hector::U_GG; // This is the default; exceptions below

        // per-gas results tables
        mConcTable[ it->first ].resize( nrslt );
        mGasRFTable[ it->first ].resize( nrslt ); 
        
        climatelog << "Tracking GCAM gas " << it->first << " as Hector gas "
                   << it->second << endl;
    }
    // Land Use CO2 is special; it can be set each year, rather than each period.
    mEmissionsTable["CO2NetLandUse"].resize( nrslt );

    // tables for temperature and total forcing and land and ocean fluxes
    mTotRFTable.resize( nrslt );
    mTemperatureTable.resize( nrslt );
    mLandFlux.resize( nrslt );
    mOceanFlux.resize( nrslt );
    // set up the other results tables
    setupConcTbl();
    setupRFTbl();
    
    // Set conversion factors for gasses that require them
    mUnitConvFac["SO2tot"] = TG_TO_GG / S_TO_SO2; // GCAM in Tg-SO2; Hector in Tg-S
    mUnitConvFac["BC"]  = GG_TO_TG;            // GCAM produces BC/OC in Tg but converts
    mUnitConvFac["OC"]  = GG_TO_TG;            // to Gg for MAGICC. Hector wants Tg.
    // Already in correct units:
    // CO2 - produced in Mt C, but converted to Gt C before passing in,
    // CH4 - produced in Mt CH4, which is what Hector wants.
    // halocarbons - produced in Gg, which is what Hector wants.
    // CO and NMVOC - produced in Tg of the relevant gas
    // NOx -- GCAM produces this in TgNOx, but it is converted to N by world::setEmissions
    // N2O -- GCAM produces this in TgN2O, but it is converted to N by world::setEmissions

    // set units for gasses that are not in Gg.  These units are
    // defined in the Hector header files.
    mHectorUnits["CO2"] = mHectorUnits["CO2NetLandUse"] = Hector::U_PGC_YR;
    mHectorUnits["BC"]  = mHectorUnits["OC"]            = Hector::U_TG;
    mHectorUnits["NOx"]                                 = Hector::U_TG_N;
    mHectorUnits["N2O"]                                 = Hector::U_TG_N;
    mHectorUnits["CO"]                                  = Hector::U_TG_CO;
    mHectorUnits["NMVOCs"]                              = Hector::U_TG_NMVOC;
    mHectorUnits["CH4"]                                 = Hector::U_TG_CH4;
    mHectorUnits["SO2tot"]                              = Hector::U_GG_S;
    
    // reset up to (but not including) period 1.
    reset( 1 );
}


/*!
 * \brief Reset the hector model
 *
 * \details Reset the hector model back to a previous time period so
 *          that we can run a new scenario or rerun some periods that
 *          we've already done.  Currently this entails shutting down
 *          all of the hector components, freeing them, and
 *          re-initializing.  Hopefully we will at some point fix
 *          hector so that we can just roll it back to a previous
 *          time.
 */
void HectorModel::reset( const int aPeriod ) {
    ILogger& climatelog = ILogger::getLogger( "climate-log" );
    climatelog.setLevel( ILogger::DEBUG );

    climatelog << "Hector reset to period= " << aPeriod << endl;
    
    if (mHcore.get() ) {
        // shutdown all Hector components and delete.
        climatelog << "Shutting down old Hector core." << endl;
        mHcore->shutDown();
        mHcore.release();
    }
    if( !mOfile.get() ) {
        mOfile.reset( new ofstream( "logs/gcam-hector-outputstream.csv" ) );
        mHosv.reset( new Hector::CSVOutputStreamVisitor( *mOfile, true ) );
    }
    else {
        // log the core reset
        (*mOfile) << "\n\n################ Hector Core Reset ################\n\n";
    }

    // set up a new core
    climatelog << "Setting up new Hector core." << endl;
    mHcore.reset( new Hector::Core(Hector::Logger::WARNING, true, true) );
    mHcore->init();
    climatelog << "Parsing ini file= " << mHectorIniFile << endl;
    Hector::INIToCoreReader coreParser( mHcore.get() );
    coreParser.parse( mHectorIniFile );
    mHcore->addVisitor( mHosv.get() ); 
    mHcore->prepareToRun();

    const Modeltime* modeltime = scenario->getModeltime();

    // loop over all gasses
    map<std::string, std::vector<double> >::iterator it;
    for( it = mEmissionsTable.begin(); it != mEmissionsTable.end(); ++it ) {
        const string& gas = it->first;
        vector<double>& emissions = it->second;
        if( gas != "CO2NetLandUse" ) {
            // Replay emissions up to, and including, the aPeriod argument.
            // Note: We also skip period 0, since it's not a "real" period.
            for( int i = 1; i <= aPeriod; ++i ) {
                if( util::isValidNumber( emissions[ i ] ) ) {
                    setEmissions( gas, i, emissions[ i ] );
                }
            }
        }
        else {
            // LUC emissions are stored yearly, not just by period.
            // Otherwise, as above.
            int ymin = modeltime->getper_to_yr( 1 );
            int ymax = modeltime->getper_to_yr( aPeriod );
            for( int yr = ymin; yr <= ymax; ++yr ) {
                int i = yearlyDataIndex( yr );
                if( util::isValidNumber( emissions[ i ] ) ) {
                    setLUCEmissions( gas, yr, emissions[ i ] );
                }
            }
        }
    } 
    // Hector is now ready to run up to the year associated with aPeriod.
    // For now catch us up to the GCAM start year and let runModel catch
    // us up the rest of the way since it will ensure that it gets any
    // updated output we would like to report from hector along the way.
    mLastYear = modeltime->getStartYear();
    mHcore->run( static_cast<double>( mLastYear ) );
}

/*! \brief Set emissions for hector model 
 *  \details Set emissions for the requested gas, unless the year is
 *           before the historical switch-over year, in which case we
 *           ignore the call.  Eventually we will also do a little
 *           sanity checking here. 
 *  \return flag indicating whether the gas was valid, irrespective of
 *          w whether we were able to set the emissions.
 */
bool HectorModel::setEmissionsByYear( const string& aGasName,
                                      const int aYear, double aEmissions )
{
    ILogger& climatelog = ILogger::getLogger( "climate-log" );
    climatelog.setLevel( ILogger::DEBUG );
    
    map<string, string>::const_iterator nameit =
        mHectorEmissionsMsg.find( aGasName );
    if( nameit == mHectorEmissionsMsg.end() ) {
        // This is not an error; MAGICC accepts some gasses that
        // Hector doesn't handle.  Log the event only at debug level.
        climatelog << "Unknown gas:  " << aGasName << "  year= " << aYear
                   << endl;
        return false;
    }
    const Modeltime* modeltime = scenario->getModeltime();
    if( aYear < modeltime->getStartYear() || aYear > modeltime->getEndYear() ) {
        // trying to store these emissions will cause a segfault.
        climatelog.setLevel( ILogger::ERROR );
        climatelog << "HectorModel::setEmissions():  Year out of range.  year= "
                   << aYear << endl;
        return false;
    }
    
    if( aYear <= mEmissionsSwitchYear ) {
        climatelog << "Skipping year= " << aYear << " for gas= " << aGasName
                   << endl;
        return true;            // see note above
    } 

    // XXX TODO: do some error checking here.  Ideally we would like
    // to filter bad values and replace them with some kind of
    // extrapolated estimate.

    // Apply the conversion factor and send the data to the hector
    // core to route to the relevant component.
    double emiss = aEmissions*mUnitConvFac[ aGasName ];
    climatelog << "Setting emissions for gas= " << aGasName
               << "  year= " << aYear
               << "  emissions= " << emiss << " " << mHectorUnits[aGasName] << endl;
    mHcore->sendMessage( M_SETDATA, nameit->second,
                         Hector::message_data( static_cast<double>( aYear ),
                         Hector::unitval( emiss,
                         static_cast<Hector::unit_types>( mHectorUnits[ aGasName ] ) ) ) ); 
    return true;
}


bool HectorModel::setEmissions( const string& aGasName, const int aPeriod,
                                double aEmissions )
{
    int year = scenario->getModeltime()->getper_to_yr( aPeriod ); 
    bool valid = setEmissionsByYear( aGasName, year, aEmissions );
    if( valid ) {
        mEmissionsTable[ aGasName ][ aPeriod ] = aEmissions;
    }
    return valid;
}


/*! \brief Set land use change emissions for hector model 
 *  \details Set emissions for the requested gas, unless the year is
 *           before the historical switch-over year, in which case we
 *           ignore the call.  We also record the emissions in the
 *           relevant array, and we do a little sanity-checking.
 */
bool HectorModel::setLUCEmissions( const string& aGasName,
                                   const int aYear, double aEmissions )
{
    if( aGasName != "CO2NetLandUse" ) {
        ILogger& climatelog = ILogger::getLogger( "climate-log" );
        climatelog.setLevel (ILogger::ERROR );
        climatelog << "Invalid LUC gas:  " << aGasName
                   << " .  Perhaps you meant to call setEmissions()?" << endl;
        return false;
    }
    
    bool valid = setEmissionsByYear( aGasName, aYear, aEmissions );

    if( valid ) {
        mEmissionsTable[ aGasName ] [ yearlyDataIndex( aYear ) ] = aEmissions;
    }
    return valid;
}

/* \brief run the climate model through a specified period
 */
IClimateModel::runModelStatus HectorModel::runModel( const int aYear ) {
    const Modeltime* modeltime = scenario->getModeltime();
    if( aYear <= mLastYear ) {
        int period;
        if( aYear <= modeltime->getper_to_yr( 1 )) {
            // before the first valid period.
            period = 1;
        }
        else if( aYear > modeltime->getEndYear() ) {
            // after the last valid period
            period = modeltime->getmaxper();
        }
        else {
            // in the middle somewhere
            // note that model time will convert years that are
            // inbetween timesteps to the next model period
            // this is exactly the behavior we want here since
            // GCAM emissions need to be reset up to the next
            // time period so hector can have an endpoint to
            // interpolate from
            period = modeltime->getyr_to_per( aYear );
        }

        reset( period );
    }

    // TODO: We have to run in one-year steps so that we can record
    // Hector's current values every year.  If we update hector to
    // store its outputs in time series (as it already does for some
    // outputs), we can bypass this and the local storage for the
    // yearly results.
    bool hadError = false;
    int lastSuccessYear = mLastYear;
    for( int year = mLastYear + 1; year <= aYear; ++year ) {
        if( !hadError ) {
            try {
                mHcore->run( static_cast<double>( year ) );
                lastSuccessYear = year;
            } catch (h_exception& e) {
                ILogger& climatelog = ILogger::getLogger( "climate-log" );
                climatelog.setLevel (ILogger::ERROR );
                climatelog << "Receieve hector exception while running year " << year << ":" << endl;
                climatelog << "* Program exception: " << e << endl;
                hadError = true;
            }
        }
        storeConc( year, hadError );
        storeRF( year, hadError );
        storeGlobals( year, hadError );
    }
    mLastYear = lastSuccessYear;
    return hadError ? EXCEPTION : SUCCESS;
}

/* \brief run the climate model through its configured end date 
 * \details This function is run at the end of a scenario run.  Since
 *          the model should have been run at each period while the
 *          scenario was running, we take this opportunity to extend
 *          the model run beyond the end of the GCAM scenario.  By
 *          default Hector will hold emissions constant (I think) past
 *          the time of the last emissions sent to the model.
 *          Alternatively, we could put in some reasonable
 *          extrapolations.  This capability is a bit of a work in
 *          progress.
 */
IClimateModel::runModelStatus HectorModel::runModel() {
    int year = mHcore->getEndDate();
    IClimateModel::runModelStatus stat = runModel( year );
    ILogger& climatelog = ILogger::getLogger( "climate-log" );
    climatelog.setLevel( ILogger::NOTICE );
    climatelog << "Final climate year: " << year << endl
               << "\tCO2 conc= " << getConcentration( "CO2", year )
               << "\tRFtot= " << getTotalForcing( year )
               << "\tTemperature= " << getTemperature( year )
               << endl;
    return stat;
}

/* \brief return the atmospheric concentration for a gas 
 * \details Note that not all gasses have concentrations available.
 *
 * \warning Currently Hector only outputs results for the current time
 */
double HectorModel::getConcentration( const string& aGasName, const int aYear) const {
    ILogger& climatelog = ILogger::getLogger( "climate-log" );
    climatelog.setLevel( ILogger::DEBUG );
    
    map<string, vector<double> >::const_iterator it =
        mConcTable.find( aGasName );
    if( it != mConcTable.end() ) {
        int idx = yearlyDataIndex( aYear );
        double conc = (it->second)[ idx ];
        climatelog << "\tgetConcentration:  gas= " << aGasName
                   << "\tyear= " << aYear << "  index= " << idx
                   << "\tconc= " << conc << endl;
        return conc;
    }
    else {
        climatelog << "HectorModel::getConcentration():  Unsupported gas:  "
                   << aGasName << endl;
        return 0.0;
    }
}

/* \brief return the global temperature anomaly
 */
double HectorModel::getTemperature( const int aYear ) const {
    ILogger& climatelog = ILogger::getLogger( "climate-log" );
    if( aYear > mHectorEndYear ) {
        climatelog.setLevel( ILogger::WARNING );
        climatelog << "getTemperature():  invalid year: " << aYear << endl;
        return 0.0;
    }

    int idx = yearlyDataIndex( aYear );
    double tempval = mTemperatureTable[ idx ];
    climatelog.setLevel( ILogger::DEBUG );
    climatelog << "\tgetTemperature:  year= " << aYear << "  index= " << idx
               << "\ttemperature= " << tempval << endl;
    return tempval;
}

double HectorModel::getTotalForcing( const int aYear ) const {
    ILogger& climatelog = ILogger::getLogger( "climate-log" );
    
    if( aYear > mHectorEndYear ) {
        climatelog.setLevel( ILogger::WARNING );
        climatelog << "getTotalForcing():  invalid year: " << aYear << endl;
        return 0.0;
    }

    double forcingval = mTotRFTable[ yearlyDataIndex( aYear ) ];
    climatelog.setLevel( ILogger::DEBUG );
    climatelog << "\tgetTotalForcing:  year= " << aYear
               << "\ttotal forcing= " << forcingval << endl;
    return forcingval;
}

double HectorModel::getForcing( const string& aGas, int aYear ) const {
    ILogger& climatelog = ILogger::getLogger( "climate-log" );
    climatelog.setLevel( ILogger::DEBUG );
    
    if( aYear > mHectorEndYear ) {
        climatelog.setLevel( ILogger::WARNING ); 
        climatelog << "getForcing():  invalid year: " << aYear << endl;
        return 0.0;
    }

    // In hector, halocarbons store their data in a time series, so we
    // don't have to make a table for them in the GCAM component.  We
    // can just ask the hector core for them by name and date. 
    // TODO: make all hector components work this way.

    map<std::string, std::string>::const_iterator ittseries =
        mHectorRFTseriesMsg.find( aGas ); 
    // If the gas is a halocarbon, grab the RF request string and send
    // it to the Hector core.
    if( ittseries != mHectorRFTseriesMsg.end() ) {
        double haloForcing = -1.0;
        // We might get an error trying to retrieve halocarbon RF if
        // hector had crashed trying to run up the given year.
        // In that case issue a warning and return an invalid result.
        try {
            haloForcing = mHcore->sendMessage( M_GETDATA, ittseries->second, aYear );
        }
        catch( const h_exception& e ) {
            ILogger& climatelog = ILogger::getLogger( "climate-log" );
            climatelog.setLevel (ILogger::WARNING );
            climatelog << "Exception: " << e << endl;
        }
        return haloForcing;
    }

    // For other RF components, 
    map<std::string, std::vector<double> >::const_iterator it =
        mGasRFTable.find( aGas );

    if( it == mGasRFTable.end() ) {
        climatelog << "getForcing(): invalid gas: " << aGas << endl;
        return 0.0;
    }

    int idx = yearlyDataIndex( aYear );
    double forcing = (it->second)[ idx ];
    climatelog << "\tgetForcing:  gas= " << aGas
               << "\tyear= " << aYear << "  index= " << idx
               << "\tforcing= " << forcing << endl;
    return forcing;
}


int HectorModel::yearlyDataIndex( const int year ) const {
    return year - scenario->getModeltime()->getStartYear();
}

void HectorModel::storeConc( const int aYear, const bool aHadError ) {
    ILogger& climatelog = ILogger::getLogger( "climate-log" );

    // No need to check the index because we checked it in runModel
    int i = yearlyDataIndex( aYear );

    // These are all of the atmospheric concentrations that Hector is
    // set up to provide.
    Hector::message_data date( aYear );
    mConcTable["CH4"][i]   = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_ATMOSPHERIC_CH4,date );
    mConcTable["N2O"][i]   = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_ATMOSPHERIC_N2O,date );
    mConcTable["O3"][i]    = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_ATMOSPHERIC_O3, date );
    mConcTable["CO2"][i]   = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_ATMOSPHERIC_CO2 );

    // Hector doesn't actually compute concentrations for these
    // gasses. (we use their emissions to compute O3 concentration,
    // but don't compute the concentrations of the original gasses.) 
    // mConcTable["CO"][i]    = mHcore->sendMessage(M_GETDATA, D_ATMOSPHERIC_CO);
    // mConcTable["NOX"][i]   = mHcore->sendMessage(M_GETDATA, D_ATMOSPHERIC_NOX);
    // mConcTable["NMVOC"][i] = mHcore->sendMessage(M_GETDATA, D_ATMOSPHERIC_NMVOC);

    // Log the concentrations we are seeing here
    climatelog.setLevel( ILogger::DEBUG );
    climatelog << "\tstoreConc: year= " << aYear << "  index= " << i << endl
               << "\t\tCO2 = " << mConcTable["CO2"][i] << endl
               << "\t\tCH4 = " << mConcTable["CH4"][i] << endl
               << "\t\tN2O = " << mConcTable["N2O"][i] << endl
               << "\t\tO3  = " << mConcTable["O3"][i] << endl;
    
}

void HectorModel::setupConcTbl() {
    int size = yearlyDataIndex( mHectorEndYear ) + 1;

    mConcTable["CH4"].resize( size );
    mConcTable["N2O"].resize( size );
    mConcTable["O3"].resize( size );
    mConcTable["CO2"].resize( size );
}    

// Be sure to keep this in sync with setupRFTbl.  If you add a gas
// here, you need to add it there too!
void HectorModel::storeRF(const int aYear, const bool aHadError ) {
    ILogger& climatelog = ILogger::getLogger( "climate-log" );
    int i = yearlyDataIndex( aYear );
    
    // total
    mTotRFTable[i]             = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_RF_TOTAL );

    // misc gases requested by GCAM
    mGasRFTable["CO2"][i]      = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_RF_CO2 );
    mGasRFTable["CH4"][i]      = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_RF_CH4 );
    mGasRFTable["N2O"][i]      = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_RF_N2O );
    mGasRFTable["BC"][i]       = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_RF_BC );
    mGasRFTable["OC"][i]       = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_RF_OC );
    mGasRFTable["SO2"][i]      = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_RF_SO2 );
    mGasRFTable["StratH2O"][i] = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_RF_H2O );
    mGasRFTable["DirSO2"][i]   = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_RF_SO2d );
    mGasRFTable["TropO3"][i]   = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_RF_O3 );

#if 0
    // Forcings that hector can provide, but which are not currently
    // requested by GCAM.  In the interests of keeping memory usage
    // down, we won't actually store these unless someone wants them.
    // Remember, if you enable them here, then you also have to add
    // them in setupRFTbl below.

    // Not added since can get from SO2 - SO2dir
    mGasRFTable["SO2i"][i]   = mHcore->sendMessage( M_GETDATA, D_RF_SO2i );

    // Volcanoes!  <- ?
    
#endif

    // Log what we saw here in the debugging log
    climatelog << "\tstoreRF:  year= " << aYear << "\tindex= " << i << endl
               << "\t\ttotal RF  = " << mTotRFTable[i] << endl
               << "\t\t     CO2  = " << mGasRFTable["CO2"][i] << endl
               << "\t\t     CH4  = " << mGasRFTable["CH4"][i] << endl
               << "\t\t     N2O  = " << mGasRFTable["N2O"][i] << endl
               << "\t\t      BC  = " << mGasRFTable["BC"][i] << endl
               << "\t\t      OC  = " << mGasRFTable["OC"][i] << endl;
}

void HectorModel::setupRFTbl() {
    int size = yearlyDataIndex( mHectorEndYear ) + 1;

    mGasRFTable["CO2"].resize( size );
    mGasRFTable["CH4"].resize( size );
    mGasRFTable["N2O"].resize( size );
    mGasRFTable["BC"].resize( size );
    mGasRFTable["OC"].resize( size );
    mGasRFTable["SO2"].resize( size );
    mGasRFTable["DirSO2"].resize( size );
    mGasRFTable["StratH2O"].resize( size );
    mGasRFTable["TropO3"].resize( size );
}

//! Store the global quantities retrieved from Hector, except total
//! forcing, which gets stored in storeRF()
void HectorModel::storeGlobals( const int aYear, const bool aHadError ) {
    int idx = yearlyDataIndex( aYear );

    mTemperatureTable[idx] = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_GLOBAL_TEMP );
    mLandFlux[idx]         = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_LAND_CFLUX );
    mOceanFlux[idx]        = aHadError ? numeric_limits<double>::quiet_NaN() : mHcore->sendMessage( M_GETDATA, D_OCEAN_CFLUX );
} 
    

double HectorModel::getNetTerrestrialUptake( const int aYear ) const {
    // Is this the same as land flux?
    return mLandFlux[ yearlyDataIndex( aYear ) ];
}

double HectorModel::getNetOceanUptake(const int aYear ) const {
    // Is this the same thing as ocean flux?
    return mOceanFlux[ yearlyDataIndex( aYear ) ];
}

void HectorModel::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitClimateModel( this, aPeriod );
    aVisitor->endVisitClimateModel( this, aPeriod );
}

double HectorModel::getEmissions( const string& aGasName, const int aYear ) const {
    ILogger& climatelog = ILogger::getLogger( "climate-log" );

    const Modeltime* modeltime = scenario->getModeltime();
    if( aYear <= modeltime->getEndYear() && aYear >= modeltime->getStartYear() ) {
        if( aGasName == "CO2NetLandUse" ) {
            return (mEmissionsTable.find( aGasName )->second)[ yearlyDataIndex( aYear ) ]; 
        }
        else {
            map<std::string, std::vector<double> >::const_iterator it =
                mEmissionsTable.find( aGasName );
            if( it != mEmissionsTable.end() ) {
                return (it->second)[ modeltime->getyr_to_per( aYear ) ];
            }
            else {
                climatelog.setLevel( ILogger::DEBUG );
                climatelog << "getEmissions:  unknown gas: " << aGasName << endl;
                return 0.0;
            }
        }
    }
    else {
        climatelog.setLevel( ILogger::ERROR );
        climatelog << "getEmissions: invalid year: " << aYear << endl;
        return 0.0;
    }
}

#endif // USE_HECTOR

