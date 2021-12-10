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
* \file world.cpp
* \ingroup Objects
* \brief world class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include "util/base/include/timer.h"

#include <string>
#include <cassert>
#include <vector>
#include <map>
#include <algorithm>

#include "util/base/include/xml_helper.h"
#include "containers/include/world.h"
#include "containers/include/region_minicam.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/configuration.h"
#include "util/base/include/util.h"
#include "util/curves/include/curve.h"
#include "util/curves/include/point_set_curve.h"
#include "util/curves/include/point_set.h"
#include "util/curves/include/explicit_point_set.h"
#include "util/curves/include/xy_data_point.h"
#include "solution/util/include/calc_counter.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/ivisitor.h"
#include "climate/include/iclimate_model.h"
// Could hide with a factory method.
#include "climate/include/magicc_model.h"
#include "climate/include/hector_model.hpp"
#include "climate/include/no_climate_model.h"
#include "emissions/include/emissions_summer.h"
#include "emissions/include/luc_emissions_summer.h"
#include "technologies/include/global_technology_database.h"
#include "reporting/include/energy_balance_table.h"
#include "containers/include/market_dependency_finder.h"
#include "technologies/include/global_technology_database.h"
#include "containers/include/iactivity.h"

#if GCAM_PARALLEL_ENABLED
#include "parallel/include/gcam_parallel.hpp"
#endif

// Uncommenting the following two lines will turn on floating-point exceptions within World::calc(),
// which will cause any invalid operation to crash the code and leave a core dump.  It's useful for
// tracking down errant NaN values; however, it only works on Linux, so we include it only when we're
// actively trying to track down a problem.
// #define GNU_SOURCE
// #include <fenv.h>

using namespace std;

extern Scenario* scenario;

//! Default constructor.
World::World()
{
    mClimateModel = 0;
    mCalcCounter = new CalcCounter();
    mGlobalTechDB = new GlobalTechnologyDatabase();
}

//! World destructor. 
World::~World(){
    clear();
}

//! Helper member function for the destructor. Performs memory deallocation. 
void World::clear(){
    for ( RegionIterator regionIter = mRegions.begin(); regionIter != mRegions.end(); regionIter++ ) {
        delete *regionIter;
    }
    delete mClimateModel;
    delete mCalcCounter;
    delete mGlobalTechDB;
}

/*! \brief Complete the initialization
*
* This routine is only called once per model run
*
* \author Josh Lurz
*/
void World::completeInit() {
    //If none has been read in, instantiate the default climate model
    if ( !mClimateModel ) {
        mClimateModel = new MagiccModel();
    }
    
    // Initialize Climate Model
    mClimateModel->completeInit( scenario->getName() );
    
    // Finish initializing all the regions.
    for( RegionIterator regionIter = mRegions.begin(); regionIter != mRegions.end(); regionIter++ ) {
        ( *regionIter )->completeInit();
    }

    // Now that all regions have finished with completeInit we can instruct the
    // market dependency finder to create the global ordering.  We will store that
    // ordering here to avoid re-copying it every time world.calc is called.
    MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();
    depFinder->createOrdering();
    mGlobalOrdering = depFinder->getOrdering();
#if GCAM_PARALLEL_ENABLED
    Timer &totalgraphtimer = TimerRegistry::getInstance().getTimer("total-graph");
    totalgraphtimer.start();
    mTBBGraphGlobal = depFinder->getFlowGraph();
    totalgraphtimer.stop();
    ILogger &mainlog = ILogger::getLogger("main_log");
    totalgraphtimer.print(mainlog, "Total of all graph analysis setup:  ");
#endif
    
    // At this point we can assume all model components have been initialized and will
    // no longer require the global technology database to get parameters from so we are
    // free to delete it
    delete mGlobalTechDB;
    mGlobalTechDB = 0;
}

//! Write out XML for debugging purposes.
/*! \warning This only call Region::toInputXML for the US. */
void World::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag ( getXMLNameStatic(), out, tabs, "", period );

    // write the xml for the class members.

    scenario->getMarketplace()->toDebugXML( period, out, tabs );

    // Only print debug XML information for the specified region to avoid
    // unmanagably large XML files.
    const static string debugRegion = Configuration::getInstance()->getString( "debug-region", "USA" );
    for( CRegionIterator i = mRegions.begin(); i != mRegions.end(); i++ ) {
        if( ( *i )->getName() == debugRegion ){
            ( *i )->toDebugXML( period, out, tabs );
        }
    }

    // Climate model parameters
    if ( !mClimateModel ) {
        mClimateModel->toDebugXML( period, out, tabs );
    }

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLNameStatic(), out, tabs );
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& World::getXMLNameStatic() {
    const static string XML_NAME = "world";
    return XML_NAME;
}

/*! \brief Returns the name of the World.
* \details Although there is only one World in the model so a name is
*          unnecessary, this is needed for compatibility with the IParsable
*          interface. The function returns instead the XML name.
* \note In the future World object may have read-in names.
* \return The name of the world.
*/
const string& World::getName() const {
    return getXMLNameStatic();
}

//! initialize anything that won't change during the calculation
/*! Examples: share weight scaling due to previous calibration, 
* cumulative technology change, etc.
*/
void World::initCalc( const int period ) {

    for( vector<Region*>::iterator i = mRegions.begin(); i != mRegions.end(); i++ ){
        ( *i )->initCalc( period );
    }
    
    Configuration* conf = Configuration::getInstance();
    if( conf->getBool( "CalibrationActive" ) ){
        // print an I/O table for debuging before we do any calibration
        ILogger& calLog = ILogger::getLogger( "calibration_log" );
        calLog.setLevel( ILogger::DEBUG );
        for( CRegionIterator reigonIt = mRegions.begin(); reigonIt != mRegions.end(); ++reigonIt ){
            // for this table we will want a condensed table without non-calibrated values
            // so the user can get an easy to see view of what they put in
            EnergyBalanceTable table( (*reigonIt)->getName(), calLog, true, false );
            (*reigonIt)->accept( &table, period );
            table.finish();
        }
    }
    
    // Reset the calc counter.
    mCalcCounter->startNewPeriod();
#if GCAM_PARALLEL_ENABLED
    // stash the period in the flow graph which just otherwise just set
    // at the start of the model run and doesn't change
    GcamFlowGraph::mPeriod = period;
#endif
}

/*!
 * \brief Calculate supply and demand and emissions for all regions and sectors.
 * \details This will call calc with the global ordering.
 * \param aPeriod The model period to calculate.
 */
void World::calc( const int aPeriod ) {
    calc( aPeriod, mGlobalOrdering );
}

/*! \brief Calculate supply and demand and emissions for the given items.
* \details Loops through the given activities and calls calc on them.  They
*          are assumed to be in proper order and could be from any region.
* \param aPeriod Period to calculate.
* \param aItemsToCalc The items which need to be calculated.
*/
void World::calc( const int aPeriod, const std::vector<IActivity*>& aItemsToCalc ) {   
    /*! \invariant The number of items to calculate must be between 0 and the
     *              total number of items globally inclusive. 
     */
    assert( aItemsToCalc.size() <= mGlobalOrdering.size() );

#ifdef GNU_SOURCE
    int except = feenableexcept(FE_DIVBYZERO | FE_INVALID);
#endif
    
    // Increment the world.calc count based on the number of items to solve. 
    mCalcCounter->incrementCount( static_cast<double>( aItemsToCalc.size() ) / static_cast<double>( mGlobalOrdering.size() ) );
    
    // Perform calculation on each item to calculate. 
    for( vector<IActivity*>::const_iterator it = aItemsToCalc.begin(); it != aItemsToCalc.end(); ++it ) {
        (*it)->calc( aPeriod );
    }
#ifdef GNU_SOURCE
    feenableexcept(except);
#endif
}

#if GCAM_PARALLEL_ENABLED
/*! Calculate supply, demand, and emissions for a single time period
 * \details This version of calc uses the TBB Flow Graph to do the calculation in
 *          parallel.  Correct ordering of the calculations is ensured by the graph.
 * \param aPeriod Time period to calculate
 * \param aWorkGraph Structure containing the TBB flow graph.  It can be the graph
 *          for the whole model, or for a desired subset.  If null is provided the
 *          flow graph for the full model will be used.
 * \param aCalcList This can be used when only a partial model calculation is needed
 *                  and a flow graph has not been created for it.  In that case the
 *                  full model flow graph will be used while skipping calculations
 *                  not contained in aCalcList.
 */
void World::calc( const int aPeriod, GcamFlowGraph *aWorkGraph, const vector<IActivity*>* aCalcList )
{
#ifdef GNU_SOURCE
    int except = feenableexcept(FE_DIVBYZERO | FE_INVALID);
#endif

    // increment the evaulation count by the fraction of the whole model that we're solving
    mCalcCounter->incrementCount( aCalcList ? (double)(aCalcList->size()) / (double) mGlobalOrdering.size() : 1.0 );

    if( !aWorkGraph ) {
        // If a work graph was not provided just use the global flow graph
        aWorkGraph = mTBBGraphGlobal;
    }

    // do the model calculation
    aWorkGraph->mHead.try_put( tbb::flow::continue_msg() );
    aWorkGraph->mTBBFlowGraph.wait_for_all();

#ifdef GNU_SOURCE
    feenableexcept(except);
#endif
}
#endif


/*! Calculates the global emissions.
 */
void World::setEmissions( int period ) {
    // Declare visitors which will aggregate emissions by period.
    EmissionsSummer co2Summer( "CO2" );
    LUCEmissionsSummer co2LandUseSummer( "CO2NetLandUse" );
    EmissionsSummer ch4Summer( "CH4" );
    EmissionsSummer ch4agrSummer( "CH4_AGR" );
    EmissionsSummer ch4awbSummer( "CH4_AWB" );
    EmissionsSummer coSummer( "CO" );
    EmissionsSummer coagrSummer( "CO_AGR" );
    EmissionsSummer coawbSummer( "CO_AWB" );
    EmissionsSummer n2oSummer( "N2O" );
    EmissionsSummer n2oagrSummer( "N2O_AGR" );
    EmissionsSummer n2oawbSummer( "N2O_AWB" );
    EmissionsSummer noxSummer( "NOx" );
    EmissionsSummer noxagrSummer( "NOx_AGR" );
    EmissionsSummer noxawbSummer( "NOx_AWB" );
    EmissionsSummer so21Summer( "SO2_1" );
    EmissionsSummer so22Summer( "SO2_2" );
    EmissionsSummer so23Summer( "SO2_3" );
    EmissionsSummer so24Summer( "SO2_4" );
    EmissionsSummer so21awbSummer( "SO2_1_AWB" );
    EmissionsSummer so22awbSummer( "SO2_2_AWB" );
    EmissionsSummer so23awbSummer( "SO2_3_AWB" );
    EmissionsSummer so24awbSummer( "SO2_4_AWB" );
    EmissionsSummer cf4Summer( "CF4" );
    EmissionsSummer c2f6Summer( "C2F6" );
    EmissionsSummer sf6Summer( "SF6" );
    EmissionsSummer hfc125Summer( "HFC125" );
    EmissionsSummer hfc134aSummer( "HFC134a" );
    EmissionsSummer hfc245faSummer( "HFC245fa" );
    EmissionsSummer hfc23Summer( "HFC23" );
    EmissionsSummer hfc32Summer( "HFC32" );
    EmissionsSummer hfc43Summer( "HFC43" );
    EmissionsSummer hfc143aSummer( "HFC143a" );
    EmissionsSummer hfc152aSummer( "HFC152a" );
    EmissionsSummer hfc227eaSummer( "HFC227ea" );
    EmissionsSummer hfc236faSummer( "HFC236fa" );
    EmissionsSummer hfc365mfcSummer( "HFC365mfc" );
    EmissionsSummer vocSummer( "NMVOC" );
    EmissionsSummer vocagrSummer( "NMVOC_AGR" );
    EmissionsSummer vocawbSummer( "NMVOC_AWB" );
    EmissionsSummer bcSummer( "BC" );
    EmissionsSummer ocSummer( "OC" );
    EmissionsSummer bcawbSummer( "BC_AWB" );
    EmissionsSummer ocawbSummer( "OC_AWB" );
    
    // Group the EmissionsSummer together for improved performance.
    GroupedEmissionsSummer allSummer;
    allSummer.addEmissionsSummer( &co2Summer );
    allSummer.addEmissionsSummer( &ch4Summer );
    allSummer.addEmissionsSummer( &ch4agrSummer );
    allSummer.addEmissionsSummer( &ch4awbSummer );
    allSummer.addEmissionsSummer( &coSummer );
    allSummer.addEmissionsSummer( &coagrSummer );
    allSummer.addEmissionsSummer( &coawbSummer );
    allSummer.addEmissionsSummer( &n2oSummer );
    allSummer.addEmissionsSummer( &n2oagrSummer );
    allSummer.addEmissionsSummer( &n2oawbSummer );
    allSummer.addEmissionsSummer( &noxSummer );
    allSummer.addEmissionsSummer( &noxagrSummer );
    allSummer.addEmissionsSummer( &noxawbSummer );
    allSummer.addEmissionsSummer( &so21Summer );
    allSummer.addEmissionsSummer( &so22Summer );
    allSummer.addEmissionsSummer( &so23Summer );
    allSummer.addEmissionsSummer( &so24Summer );
    allSummer.addEmissionsSummer( &so21awbSummer );
    allSummer.addEmissionsSummer( &so22awbSummer );
    allSummer.addEmissionsSummer( &so23awbSummer );
    allSummer.addEmissionsSummer( &so24awbSummer );
    allSummer.addEmissionsSummer( &cf4Summer );
    allSummer.addEmissionsSummer( &c2f6Summer );
    allSummer.addEmissionsSummer( &sf6Summer );
    allSummer.addEmissionsSummer( &hfc125Summer );
    allSummer.addEmissionsSummer( &hfc134aSummer );
    allSummer.addEmissionsSummer( &hfc245faSummer );
    allSummer.addEmissionsSummer( &hfc23Summer );
    allSummer.addEmissionsSummer( &hfc32Summer );
    allSummer.addEmissionsSummer( &hfc43Summer );
    allSummer.addEmissionsSummer( &hfc143aSummer );
    allSummer.addEmissionsSummer( &hfc152aSummer );
    allSummer.addEmissionsSummer( &hfc227eaSummer );
    allSummer.addEmissionsSummer( &hfc236faSummer );
    allSummer.addEmissionsSummer( &hfc365mfcSummer );
    allSummer.addEmissionsSummer( &vocSummer );
    allSummer.addEmissionsSummer( &vocagrSummer );
    allSummer.addEmissionsSummer( &vocawbSummer );
    allSummer.addEmissionsSummer( &bcSummer );
    allSummer.addEmissionsSummer( &ocSummer );
    allSummer.addEmissionsSummer( &bcawbSummer );
    allSummer.addEmissionsSummer( &ocawbSummer );

   const double TG_TO_PG = 1000;
   const double N_TO_N2O = 1.571132; 
   const double N_TO_NO2 = 3.2857;
   const double S_TO_SO2 = 2.0; 
   const double HFC_CA_TO_FA = ( 950.0 / 640.0 );
    const double HFC23_TO_143 = ( 14800.0 / 4470.0 );
    const double HFC236_TO_143 = ( 9810.0 / 4470.0 );
    const double HFC32_TO_245 = ( 675.0 / 1030.0 );
    const double HFC152_TO_245 = ( 124.0 / 1030.0 );
    const double HFC365_TO_245 = ( 794.0 / 1030.0 );
    const double HFC43_TO_134 = ( 1640.0 / 1430.0 );
    
    // Update all emissions values.
    accept( &allSummer, period );
    accept( &co2LandUseSummer, period );
        
    // Only set emissions if they are valid. If these are not set
    // MAGICC will use the default values.
    if( co2Summer.areEmissionsSet( period ) ){
        mClimateModel->setEmissions( "CO2", period,
                                     co2Summer.getEmissions( period )
                                     / TG_TO_PG );
    }
    
    const int currYear = scenario->getModeltime()->getper_to_yr( period );
    const int startYear = currYear - scenario->getModeltime()->gettimestep( period ) + 1;
    for ( int i = startYear; i <= currYear; i++ ) {
        if( co2LandUseSummer.areEmissionsSet( i ) ){
            mClimateModel->setLUCEmissions( "CO2NetLandUse", i,
                                            co2LandUseSummer.getEmissions( i )
                                            / TG_TO_PG );
        }
    }
    
    if( ch4Summer.areEmissionsSet( period ) ){
        mClimateModel->setEmissions( "CH4", period,
                                     ch4Summer.getEmissions( period ) +
                                     ch4agrSummer.getEmissions( period ) + 
                                     ch4awbSummer.getEmissions( period ));
    }
    
    if( coSummer.areEmissionsSet( period ) ){
        mClimateModel->setEmissions( "CO", period,
                                     coSummer.getEmissions( period ) +
                                     coagrSummer.getEmissions( period ) +
                                     coawbSummer.getEmissions( period ));
    }
    
    // MAGICC wants N2O emissions in Tg N, but miniCAM calculates Tg N2O
    if( n2oSummer.areEmissionsSet( period ) ){
        mClimateModel->setEmissions( "N2O", period,
                                     ( n2oSummer.getEmissions( period ) +
                                       n2oawbSummer.getEmissions( period ) +
                                       n2oagrSummer.getEmissions( period )  )
                                     / N_TO_N2O );
    }
    
    // MAGICC wants NOx emissions in Tg N, but miniCAM calculates Tg NOx
    // FORTRAN code uses the conversion for NO2
    if( noxSummer.areEmissionsSet( period ) ){
        mClimateModel->setEmissions( "NOx", period,
                                     ( noxSummer.getEmissions( period ) +
                                       noxagrSummer.getEmissions( period ) +
                                       noxawbSummer.getEmissions( period ))
                                     / N_TO_NO2 );
    }
    
    double so2total=0.0;
    // MAGICC wants SO2 emissions in Tg S, but miniCAM calculates Tg SO2
    // Region 1 includes SO21 and 60% of SO24 (FSU)
    if( so21Summer.areEmissionsSet( period ) && so24Summer.areEmissionsSet( period )){
        double so21 = so21Summer.getEmissions( period ) +
            so21awbSummer.getEmissions( period )
            + 0.6*so24Summer.getEmissions( period ) 
            + 0.6*so24awbSummer.getEmissions( period ); 
        
        mClimateModel->setEmissions( "SOXreg1", period, so21/S_TO_SO2);
        so2total += so21;
    }
    
    // MAGICC wants SO2 emissions in Tg S, but miniCAM calculates Tg SO2
    // Region 2 includes SO22 and 40% of SO24 (FSU)
    if( so22Summer.areEmissionsSet( period ) && so24Summer.areEmissionsSet( period )){
        double so22 = so22Summer.getEmissions( period ) +
            so22awbSummer.getEmissions( period )
            + 0.4*so24Summer.getEmissions( period ) 
            + 0.4*so24awbSummer.getEmissions( period );
        
        mClimateModel->setEmissions( "SOXreg2", period, so22 / S_TO_SO2);
        so2total += so22;
    }
    
    // MAGICC wants SO2 emissions in Tg S, but miniCAM calculates Tg SO2
    if( so23Summer.areEmissionsSet( period ) ){
        double so23 = so23Summer.getEmissions( period ) +
            so23awbSummer.getEmissions( period );
        
        mClimateModel->setEmissions( "SOXreg3", period, so23 / S_TO_SO2 );
        so2total += so23;
    }
    
    // set total SO2 emissions for those models that want it.
    // Emissions are in Tg SO2; it is up to models that want
    // something different to make their own conversion.
    mClimateModel->setEmissions("SO2tot", period, so2total);
    
    if( cf4Summer.areEmissionsSet( period ) ){
        mClimateModel->setEmissions( "CF4", period,
                                     cf4Summer.getEmissions( period ) );
    }
    
    if( c2f6Summer.areEmissionsSet( period ) ){
        mClimateModel->setEmissions( "C2F6", period,
                                     c2f6Summer.getEmissions( period ) );
    }
    
    if( sf6Summer.areEmissionsSet( period ) ){
        mClimateModel->setEmissions( "SF6", period,
                                     sf6Summer.getEmissions( period ) );
    }
    
    if( hfc125Summer.areEmissionsSet( period ) ){
        mClimateModel->setEmissions( "HFC125", period,
                                     hfc125Summer.getEmissions( period ) );
    } 
    
    if( hfc134aSummer.areEmissionsSet( period ) && hfc43Summer.areEmissionsSet( period )  ){
        mClimateModel->setEmissions( "HFC134a", period,
                                     hfc134aSummer.getEmissions( period ) +
                                     hfc43Summer.getEmissions( period ) * HFC43_TO_134);
    }

    if( hfc245faSummer.areEmissionsSet( period ) && hfc32Summer.areEmissionsSet( period ) && hfc365mfcSummer.areEmissionsSet( period ) && hfc152aSummer.areEmissionsSet( period ) ){
        // MAGICC needs HFC245fa in kton of HFC245ca
        mClimateModel->setEmissions( "HFC245ca", period,
                                     hfc245faSummer.getEmissions( period ) / HFC_CA_TO_FA +
                                     hfc32Summer.getEmissions( period ) * HFC32_TO_245 +
                                     hfc365mfcSummer.getEmissions( period ) * HFC365_TO_245 +
                                     hfc152aSummer.getEmissions( period ) * HFC152_TO_245);
        // For models that need ktonnes of HFC245fa (no single model should implement both of these):
        mClimateModel->setEmissions("HFC245fa", period,
                                    hfc245faSummer.getEmissions(period)+
                                    hfc32Summer.getEmissions( period ) * HFC32_TO_245 +
                                    hfc365mfcSummer.getEmissions( period ) * HFC365_TO_245 +
                                    hfc152aSummer.getEmissions( period ) * HFC152_TO_245);
    }
    
    // MAGICC needs this in tons of VOC. Input is in TgC
    if( vocSummer.areEmissionsSet( period ) ){
        mClimateModel->setEmissions( "NMVOCs", period,
                                     ( vocSummer.getEmissions( period ) +
                                       vocagrSummer.getEmissions( period ) +
                                       vocawbSummer.getEmissions( period ) ));
    }
    
    // MAGICC needs this in GgC. Model output is in TgC
    if( bcSummer.areEmissionsSet( period ) ){
        mClimateModel->setEmissions( "BC", period,
                                     ( bcSummer.getEmissions( period ) +
                                       bcawbSummer.getEmissions( period ) )
                                     * TG_TO_PG );
    }
    
    // MAGICC needs this in GgC. Model output is in TgC
    if( ocSummer.areEmissionsSet( period ) ){
        mClimateModel->setEmissions( "OC", period,
                                     ( ocSummer.getEmissions( period ) +
                                       ocawbSummer.getEmissions( period ) )
                                     * TG_TO_PG );
    }
    
    
    if( hfc227eaSummer.areEmissionsSet( period ) ){
        mClimateModel->setEmissions( "HFC227ea", period,
                                     hfc227eaSummer.getEmissions( period ) );
    }
    
    if( hfc143aSummer.areEmissionsSet( period ) && hfc23Summer.areEmissionsSet( period ) && hfc236faSummer.areEmissionsSet( period ) ){
        mClimateModel->setEmissions( "HFC143a", period,
                                     hfc143aSummer.getEmissions( period ) +
                                     hfc23Summer.getEmissions( period ) * HFC23_TO_143 +
                                     hfc236faSummer.getEmissions( period ) * HFC236_TO_143);
    }
}
    
void World::runClimateModel() {
    // The Climate model reads in data for the base period, so skip passing it in.
    for( int period = 1; period < scenario->getModeltime()->getmaxper(); ++period ) {
        setEmissions( period );
    }
    
    // Run the model.
    mClimateModel->runModel();
}

void World::runClimateModel( int aPeriod ) {
    if( aPeriod > 0 ) {
        setEmissions( aPeriod );
        mClimateModel->runModel( scenario->getModeltime()->getper_to_yr( aPeriod ) );
    }
}

/*! \brief Test to see if calibration worked for all regions
*
* Compares the sum of calibrated + fixed values to output of each sector.
*
* \author Steve Smith
* \param period Model period
* \param calAccuracy Calibration tolerance.
* \param printWarnings flag to turn on logging of warnings if calibrations are
*        not accurate
* \return Boolean true if calibration is ok.
*/
bool World::isAllCalibrated( const int period, double calAccuracy, const bool printWarnings ) const {
    bool isAllCalibrated = true;
    ILogger& calLog = ILogger::getLogger( "calibration_log" );
    calLog.setLevel( ILogger::DEBUG );
    for( CRegionIterator i = mRegions.begin(); i != mRegions.end(); i++ ){
        bool currRegionCalibrated = ( *i )->isAllCalibrated( period, calAccuracy, printWarnings );
        isAllCalibrated &= currRegionCalibrated;
        // if we did not calibrate this region correctly and we are printing warnings then give the
        // user some I/O tables to help them understand what was inconsistent
        if( !currRegionCalibrated && printWarnings ) {
            // we want to give the user two tables to use one with a condensed view
            // with all inputs and outputs so they can see what didn't calibrate
            // and another table fully expanded with just the calibrated values
            calLog << "Energy balance table where inputs and outputs have been replaced by a"
                << " calibrated value if it exists:" << endl;
            EnergyBalanceTable condensedTable( (*i)->getName(), calLog, true, true );
            (*i)->accept( &condensedTable, period );
            condensedTable.finish();
            
            calLog << "Full energy balalce table with just cal values:" << endl;
            EnergyBalanceTable fullTable( (*i)->getName(), calLog, false, false );
            (*i)->accept( &fullTable, period );
            fullTable.finish();
        }
    }
	
    return isAllCalibrated;
}

/*! \brief This function returns a special mapping of strings to ints for use in
*          the outputs. 
* \details This map is created such that global maps to zero, region 0 maps to
*          1, etc. It is similiar to the regionNamesToNumbers map but has the
*          global element and each region number in the regionMap is 1 + the
*          number in the regionNamesToNumbers map.
* \warning This function should only be used by the database output functions. 
* \return The map of region names to numbers.
*/
const map<string,int> World::getOutputRegionMap() const {
    map<string,int> regionMap;

    for ( unsigned int i = 0; i < mRegions.size(); i++ ) {
        regionMap[mRegions[i]->getName()] = i+1; // start index from 1
    }
    // hardcode for now
    regionMap["global"] = 0;
    return regionMap;
}

/*! \brief Set a fixed tax for all regions.
* \param aTax Tax.
*/
void World::setTax( const GHGPolicy* aTax ){
    for( RegionIterator iter = mRegions.begin(); iter != mRegions.end(); ++iter ){
        (*iter)->setTax( aTax );
    }
}

/*! \brief Get the climate model.
* \return The climate model.
*/
const IClimateModel* World::getClimateModel() const {
    return mClimateModel;
}

/*! \brief A function to generate a series of ghg emissions quantity curves based on an already performed model run.
* \details This function used the information stored in it to create a series of curves, one for each region,
* with each datapoint containing a time period and an amount of gas emissions.
* \note The user is responsible for deallocating the memory in the returned Curves.
* \author Josh Lurz
* \param ghgName The name of the ghg to create a set of curves for.
* \return A map with keys as region names and Curves as values representing the quantity of ghg emissions by time period.
*/
map<string,const Curve*> World::getEmissionsQuantityCurves( const string& ghgName ) const {
    /*! \pre The run has been completed. */
    map<string,const Curve*> emissionsQCurves;

    for( CRegionIterator rIter = mRegions.begin(); rIter != mRegions.end(); rIter++ ){
        emissionsQCurves[ (*rIter)->getName() ] = (*rIter)->getEmissionsQuantityCurve( ghgName );
    }

    return emissionsQCurves;
}

/*! \brief A function to generate a series of ghg emissions price curves based on an already performed model run.
* \details This function used the information stored in it to create a series of curves, one for each period,
* with each datapoint containing a time period and the price gas emissions. 
* \note The user is responsible for deallocating the memory in the returned Curves.
* \author Josh Lurz
* \param ghgName The name of the ghg to create a set of Curves for.
* \return A map with keys as region names and Curves as values representing the price of ghg emissions by time period. 
*/
map<string,const Curve*> World::getEmissionsPriceCurves( const string& ghgName ) const {
    /*! \pre The run has been completed. */
    map<string,const Curve*> emissionsPCurves;
    
    for( CRegionIterator rIter = mRegions.begin(); rIter != mRegions.end(); rIter++ ){
        emissionsPCurves[ (*rIter)->getName() ] = (*rIter)->getEmissionsPriceCurve( ghgName );
    }

    return emissionsPCurves;
}

/*!
 * \brief Gets the reference to the calc counter.
 * \details The calc counter would be needed by solvers however since there
 *          could be many solvers the world object will contain it and each
 *          solver will be able to get it from here.
 * \return A reference to the Calc Counter
 */
CalcCounter* World::getCalcCounter() const {
    return mCalcCounter;
}

/*! \brief Call any calculations that are only done once per period after
*          solution is found.
* \details This function is used to calculate and store variables which are only
*          needed after the current period is complete. 
* \param aPeriod The period to finalize.
* \author Sonny Kim, Josh Lurz
*/
void World::postCalc( const int aPeriod ){
    // Finalize sectors.
    for( RegionIterator region = mRegions.begin(); region != mRegions.end(); ++region ){
        (*region)->postCalc( aPeriod );
    }
}

/*!
 * \brief Get the global technology database to look up global techs.
 * \return A reference to the global technologies database.
 */
const GlobalTechnologyDatabase* World::getGlobalTechnologyDatabase() const {
    if( !mGlobalTechDB ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Attempting to retrieve " << mGlobalTechDB->getXMLNameStatic() << " after completeInit()." << endl;
        abort();
    }
    return mGlobalTechDB;
}

/*! \brief Update a visitor for the World.
* \param aVisitor Visitor to update.
* \param aPeriod Period to update.
*/
void World::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitWorld( this, aPeriod );

    // Visit the marketplace
    scenario->getMarketplace()->accept( aVisitor, aPeriod );

    // Visit the climate model.
    mClimateModel->accept( aVisitor, aPeriod );

    // loop for regions
    for( CRegionIterator currRegion = mRegions.begin(); currRegion != mRegions.end(); ++currRegion ){
        (*currRegion)->accept( aVisitor, aPeriod );
    }

    aVisitor->endVisitWorld( this, aPeriod );
}
