/*! 
 * \file GCAM_E3SM_interface.cpp
 * \brief E3SM gcam driver source file.
 * \author Kate Calvin and Alan Di Vittorio
 */


#include "util/base/include/auto_file.h"
#include "../include/GCAM_E3SM_interface.h"
#include "containers/include/world.h"

#include "util/base/include/configuration.h"
#include "containers/include/iscenario_runner.h"
#include "containers/include/scenario_runner_factory.h"
#include "containers/include/scenario.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/timer.h"
#include "util/base/include/version.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_parse_helper.h"

#include "../include/remap_data.h"
#include "../include/get_data_helper.h"
#include "../include/set_data_helper.h"
#include "../include/carbon_scalers.h"
#include "../include/emiss_downscale.h"

#include <boost/iostreams/device/mapped_file.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/classification.hpp>

#include <numeric>

using namespace std;

template<typename ContainerType>
bool parseXMLInternal(const string& aXMLFile, ContainerType* aRootElement) {
    try {
        // open the file as a memory mapped file and make sure it was successful
        boost::iostreams::mapped_file_source xmlFile(aXMLFile.c_str());
        
        // Parse the file, note the memory for the parser will be released when doc
        // goes out of scope.
        rapidxml::xml_document<> doc;
        doc.parse<rapidxml::parse_non_destructive>(const_cast<char*>(xmlFile.data()));
        
        // Kick off the processing of the XML nodes starting with the root element.
        rapidxml::xml_node<char>* child = doc.first_node()->first_node();
        aRootElement->XMLParse(child);
        
        xmlFile.close();
    }
    catch(std::ios_base::failure ioException) {
        cerr << "Could not open: " << aXMLFile << " failed with: "
             << ioException.what() << endl;
        return false;
    }
    catch(rapidxml::parse_error parseException) {
        cerr << "Failed to parse: " << aXMLFile << " with error: "
             << parseException.what() << endl;
        return false;
    }
    
    return true;
}

ofstream outFile;

// Pointer for a scenario
Scenario* scenario; // model scenario info

int restartPeriod; // Restart period is set during run and used in manage state variables

/*! \brief Constructor
 * \details This is the constructor for the E3SM_driver class.
 * allocate and initialize some data members here
 */
GCAM_E3SM_interface::GCAM_E3SM_interface(int *aNumLon, int *aNumLat, int *aNumReg, int *aNumSector) :
					mGcamCO2EmissPreviousGCAMYear((*aNumReg) * (*aNumSector), 0.0),
        				mGcamCO2EmissCurrentGCAMYear((*aNumReg) * (*aNumSector), 0.0),
					mBaseYearEmissions_sfc((*aNumReg), 0.0),
           				mBaseYearEmissions_air((*aNumReg), 0.0),
           				mBaseYearEmissions_ship((*aNumReg), 0.0),
           				mGcamYearEmissions_sfc((*aNumReg), 0.0),
           				mGcamYearEmissions_air((*aNumReg), 0.0),
           				mGcamYearEmissions_ship((*aNumReg), 0.0),
					mBaseYearEmissionsGridID_sfc((*aNumLon) * (*aNumLat) * 12, 0),
					mBaseYearEmissionsGridLon_sfc((*aNumLon) * (*aNumLat) * 12, 0.0),
					mBaseYearEmissionsGridLat_sfc((*aNumLon) * (*aNumLat) * 12, 0.0),
					mBaseYearEmissionsGrid_sfc((*aNumLon) * (*aNumLat) * 12, 0.0),
					mBaseYearEmissionsGridID_ship((*aNumLon) * (*aNumLat) * 12, 0),
                                        mBaseYearEmissionsGridLon_ship((*aNumLon) * (*aNumLat) * 12, 0.0), 
                                        mBaseYearEmissionsGridLat_ship((*aNumLon) * (*aNumLat) * 12, 0.0), 
                                        mBaseYearEmissionsGrid_ship((*aNumLon) * (*aNumLat) * 12, 0.0),
					mBaseYearEmissionsGridID_air((*aNumLon) * (*aNumLat) * 12 * 2, 0),
                                        mBaseYearEmissionsGridLon_air((*aNumLon) * (*aNumLat) * 12 * 2, 0.0), 
                                        mBaseYearEmissionsGridLat_air((*aNumLon) * (*aNumLat) * 12 * 2, 0.0), 
                                        mBaseYearEmissionsGrid_air((*aNumLon) * (*aNumLat) * 12 * 2, 0.0)	
{
}

//! Destructor. 
GCAM_E3SM_interface::~GCAM_E3SM_interface(){
}

/*! \brief Initializer for GCAM.
 * \details
 *  Initialize gcam log files and read in configuration
 *  and base model information.  Create and setup scenario
 */

void GCAM_E3SM_interface::initGCAM(int *yyyymmdd, std::string aCaseName, std::string aGCAMConfig, std::string aGCAM2ELMCO2Map, std::string aGCAM2ELMLUCMap,
                                   std::string aGCAM2ELMWHMap, std::string aGCAM2ELMCDENMap,
                                   std::string aBaseCO2GcamFileName, std::string aBaseCO2SfcFile, std::string aBaseCO2ShipFile, std::string aBaseCO2AirFile, 
                                   double* aELMArea, int *aNumLon, int *aNumLat, int *aNumReg, int *aNumSector, bool aRestartRun)
{
    // get the current e3sm year and declare variables for gcam time info
    int e3smYear = *yyyymmdd/10000;
    int gcamPeriod, gcamYear, gcamYearPrev;

    // identify default file names for control input and logging controls
    string configurationArg = aGCAMConfig;
    string loggerFactoryArg = "log_conf.xml";
    
    // Add OS dependent prefixes to the arguments.
    const string configurationFileName = configurationArg;
    const string loggerFileName = loggerFactoryArg;
   
    // for reading in co2 restart data
    std::string co2_fname;

    // for diagnostic output
    ofstream oFile;

    // Initialize the LoggerFactory
    XMLParseHelper::initParser();
    bool success = XMLParseHelper::parseXML( loggerFileName, &loggerFactoryWrapper );
    
    // Get the main log file.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::WARNING );
    
    // Get the coupler log
    ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
    coupleLog.setLevel( ILogger::NOTICE );    
    coupleLog.precision(20);

    // print disclaimer
    mainLog << "LEGAL NOTICE" << endl;
    mainLog << "This computer software was prepared by Battelle Memorial Institute," << endl;
    mainLog << "hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830" << endl;
    mainLog << "with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE" << endl;
    mainLog << "CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY" << endl;
    mainLog << "LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this" << endl;
    mainLog << "sentence must appear on any copies of this computer software." << endl;
    
    // print export control notice
    mainLog << "EXPORT CONTROL" << endl;
    mainLog << "User agrees that the Software will not be shipped, transferred or" << endl;
    mainLog << "exported into any country or used in any manner prohibited by the" << endl;
    mainLog << "United States Export Administration Act or any other applicable" << endl;
    mainLog << "export laws, restrictions or regulations (collectively the 'Export Laws')." << endl;
    mainLog << "Export of the Software may require some form of license or other" << endl;
    mainLog << "authority from the U.S. Government, and failure to obtain such" << endl;
    mainLog << "export control license may result in criminal liability under" << endl;
    mainLog << "U.S. laws. In addition, if the Software is identified as export controlled" << endl;
    mainLog << "items under the Export Laws, User represents and warrants that User" << endl;
    mainLog << "is not a citizen, or otherwise located within, an embargoed nation" << endl;
    mainLog << "(including without limitation Iran, Syria, Sudan, Cuba, and North Korea)" << endl;
    mainLog << "    and that User is not otherwise prohibited" << endl;
    mainLog << "under the Export Laws from receiving the Software." << endl;
    mainLog << "" << endl;
    mainLog << "Copyright 2011 Battelle Memorial Institute.  All Rights Reserved." << endl;
    mainLog << "Distributed as open-source under the terms of the Educational Community " << endl;
    mainLog << "License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php" << endl;
    mainLog << "" << endl;
    mainLog << "For further details, see: http://www.globalchange.umd.edu/models/gcam/" << endl;
    
    mainLog << "Running GCAM model code base version " << __ObjECTS_VER__ << " revision "
    << __REVISION_NUMBER__ << endl << endl;
    
    // Parse configuration file.
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Parsing input files..." << endl;
    Configuration* conf = Configuration::getInstance();
    success = XMLParseHelper::parseXML( configurationFileName, conf );
    // TODO: Check if parsing succeeded.
    
    // Initialize the timer.  Create an object of the Timer class.
    Timer timer;
    timer.start();
    
    // Create an empty exclusion list so that any type of IScenarioRunner can be
    // created.
    list<string> exclusionList;
    
    //  create the scenario runner
    runner = ScenarioRunnerFactory::createDefault( exclusionList );
        
    // Setup the scenario.
    success = runner->setupScenarios( timer );
    
    // Overwrite the scenario name
    runner->getInternalScenario()->setName( aCaseName );
    
    // Note we will only allocate space to store GCAM results for one model period
    // at a time.
    // For that reason we will remap all model years to just zero
    const Modeltime* modeltime = runner->getInternalScenario()->getModeltime();
    vector<int> years{ 0 };
    std::map<int, int> yearRemap;
    for(int period = 0; period < modeltime->getmaxper(); ++period) {
        yearRemap[modeltime->getper_to_yr(period)] = 0;
    }
    
    // Setup the CO2 mappings
    success = parseXMLInternal(aGCAM2ELMCO2Map, &mCO2EmissData);
    mCO2EmissData.addYearColumn("Year", years, yearRemap);
    mCO2EmissData.finalizeColumns();

    // read in any needed co2 restart data
    if (aRestartRun) {
       // get appropriate gcam year
       gcamPeriod = modeltime->getyr_to_per( e3smYear );
       if ( modeltime->isModelYear( e3smYear ) ) {
                gcamPeriod = gcamPeriod + 1;
       }
       gcamYearPrev = modeltime->getper_to_yr( gcamPeriod-1 );
       gcamYear = modeltime->getper_to_yr( gcamPeriod );
       coupleLog << "initGCAM: Initializing co2 data for restart run; e3sm year is " << e3smYear << ", gcam year is " << gcamYear << endl;
       // if during first e3sm period (2016-2019), get the base year data for previous
       // assume that restart cannot happen for 2015
       if (gcamYear == 2015) {
          coupleLog << "initGCAM: Can't restart in 2015: start an initial hybrid run" << endl;
          exit(EXIT_FAILURE);
       } else if (gcamYear == 2020){
          co2_fname = aBaseCO2GcamFileName;
          readRegionGcamYearEmissionData(co2_fname);
       } else {
          // else get the previous year based on the output restart files
          co2_fname = std::string("gcam_co2_restart_") + std::to_string(gcamYearPrev) + std::string(".csv");
          readRegionGcamYearEmissionData(co2_fname);
       }
       // note the order here:
       //    sector advances faster than region, and order is surface, aircraft, shipping
       for (int i = 0; i < (*aNumReg); ++i) {
          mGcamCO2EmissPreviousGCAMYear[i*(*aNumSector)] = mGcamYearEmissions_sfc[i];
          mGcamCO2EmissPreviousGCAMYear[i*(*aNumSector)+1] = mGcamYearEmissions_air[i];
          mGcamCO2EmissPreviousGCAMYear[i*(*aNumSector)+2] = mGcamYearEmissions_ship[i];
       }
       coupleLog << "initGCAM: Finished reading previous GCAM year co2 data from " << co2_fname << endl;
       // always get the next year based on the output restart files
       // however, this may not exist yet, which is ok if the restart year is a gcam modelyear
       //    in gcam modelyear, gcam will run again, and so in runGCAM the current data will be set
       if ( !modeltime->isModelYear( e3smYear ) ) {
          co2_fname = std::string("gcam_co2_restart_") + std::to_string(gcamYear) + std::string(".csv");
          readRegionGcamYearEmissionData(co2_fname);
          for (int i = 0; i < (*aNumReg); ++i) {
             mGcamCO2EmissCurrentGCAMYear[i*(*aNumSector)] = mGcamYearEmissions_sfc[i];
             mGcamCO2EmissCurrentGCAMYear[i*(*aNumSector)+1] = mGcamYearEmissions_air[i];
             mGcamCO2EmissCurrentGCAMYear[i*(*aNumSector)+2] = mGcamYearEmissions_ship[i];
          }
          coupleLog << "initGCAM: Finished reading GCAM year co2 data from " << co2_fname << endl;
       }
    } // end if a restart run

    // always read in the base year co2 data (MMTC; also TgC)
    coupleLog << "initGCAM: initializing base gcam co2 data" << endl;
    readRegionGcamYearEmissionData(aBaseCO2GcamFileName);
    for (int i = 0; i < (*aNumReg); ++i) {
       mBaseYearEmissions_sfc[i] = mGcamYearEmissions_sfc[i];
       mBaseYearEmissions_air[i] = mGcamYearEmissions_air[i];
       mBaseYearEmissions_ship[i] = mGcamYearEmissions_ship[i];
       
       // if it is not a restart run then the previous data are the base year data
       if(!aRestartRun) {
          mGcamCO2EmissPreviousGCAMYear[i*(*aNumSector)] = mGcamYearEmissions_sfc[i];
          mGcamCO2EmissPreviousGCAMYear[i*(*aNumSector)+1] = mGcamYearEmissions_air[i];
          mGcamCO2EmissPreviousGCAMYear[i*(*aNumSector)+2] = mGcamYearEmissions_ship[i];
       }
    }
    // calculate the global base year values
    coupleLog << "initGCAM: calculating global gcam co2 values by sector" << endl;
    mBaseYearGlobalSfcCO2Emiss = std::accumulate(mBaseYearEmissions_sfc.begin(), mBaseYearEmissions_sfc.end(), 0.0);
    mBaseYearGlobalAirCO2Emiss = std::accumulate(mBaseYearEmissions_air.begin(), mBaseYearEmissions_air.end(), 0.0);
    mBaseYearGlobalShipCO2Emiss = std::accumulate(mBaseYearEmissions_ship.begin(), mBaseYearEmissions_ship.end(), 0.0); 

    // read in the gridded co2 data for downscaling and calculate the global totals in gcam units of TgC (MMT) C
    coupleLog << "initGCAM: reading in gridded baseline co2 data" << endl;
    // surface - no international shipping
    mBaseYearGridGlobalSfcCO2Emiss = readCO2DataGridCSV(aBaseCO2SfcFile, true, true, true, aELMArea, aNumLon, aNumLat, "surface");
    // international shipping
    mBaseYearGridGlobalShipCO2Emiss = readCO2DataGridCSV(aBaseCO2ShipFile, true, true, true, aELMArea, aNumLon, aNumLat, "shipment");
    // aircraft
    mBaseYearGridGlobalAirCO2Emiss = readCO2DataGridCSV(aBaseCO2AirFile, true, true, true, aELMArea, aNumLon, aNumLat, "aircraft");

    // log the global values
    coupleLog << "initGCAM: global surface data in Tg C: GCAM 2015 = " << mBaseYearGlobalSfcCO2Emiss <<  endl;
    coupleLog << "initGCAM: global shipping data in Tg C: GCAM 2015 = " << mBaseYearGlobalShipCO2Emiss << endl;
    coupleLog << "initGCAM: global aircraft data in Tg C: GCAM 2015 = " << mBaseYearGlobalAirCO2Emiss << endl; 

   // check the member variables
   coupleLog << "initGCAM: global surface data in Tg C: grid 2014 = " << mBaseYearGridGlobalSfcCO2Emiss <<  endl;
   coupleLog << "initGCAM: global shipping data in Tg C: grid 2024 = " << mBaseYearGridGlobalShipCO2Emiss << endl;
   coupleLog << "initGCAM: global aircraft data in Tg C: grid 2014 = " << mBaseYearGridGlobalAirCO2Emiss << endl;

    // scale the grids to 2015 gcam data
    //    because the input grids are 2014, and the downscaling values use GCAM ratios to 2015
    // assume that there are non-zero data in the input grids, otherwise nothing is going to work
    double scale = mBaseYearGlobalSfcCO2Emiss / mBaseYearGridGlobalSfcCO2Emiss;
    coupleLog << "initGCAM: co2 surface scalar to 2015 grid = " << scale << endl;
    std::transform(mBaseYearEmissionsGrid_sfc.begin(), mBaseYearEmissionsGrid_sfc.end(), mBaseYearEmissionsGrid_sfc.begin(), [scale](double x){return scale * x;});
    scale = mBaseYearGlobalShipCO2Emiss / mBaseYearGridGlobalShipCO2Emiss;
    coupleLog << "initGCAM: co2 shipment scalar to 2015 grid = " << scale << endl;
    std::transform(mBaseYearEmissionsGrid_ship.begin(), mBaseYearEmissionsGrid_ship.end(), mBaseYearEmissionsGrid_ship.begin(), [scale](double x){return scale * x;});
    scale = mBaseYearGlobalAirCO2Emiss / mBaseYearGridGlobalAirCO2Emiss;
    coupleLog << "initGCAM: co2 aircraft scalar to 2015 grid = " << scale << endl;
    std::transform(mBaseYearEmissionsGrid_air.begin(), mBaseYearEmissionsGrid_air.end(), mBaseYearEmissionsGrid_air.begin(), [scale](double x){return scale * x;});

    // Setup the land use change mappings
    success = parseXMLInternal(aGCAM2ELMLUCMap, &mLUCData);
    mLUCData.addYearColumn("Year", years, yearRemap);
    mLUCData.finalizeColumns();
    
    // Setup the wood harvest mappings
    success = parseXMLInternal(aGCAM2ELMWHMap, &mWoodHarvestData);
    mWoodHarvestData.addYearColumn("Year", years, yearRemap);
    mWoodHarvestData.finalizeColumns();

    // currently, there is no year associated with the carbon density values
    //    but include because helper functions expect it
    // also, get the data from the land-use-history variable here because it should
    //    not be affected by the restarts because it is not the state variable

    // actually, since this is called on init before any run/restart, the land-carbon-densities should be fine

    // Setup the above ground carbon density  mappings
    success = parseXMLInternal(aGCAM2ELMCDENMap, &mAGCDensityData);
    mAGCDensityData.addYearColumn("Year", years, yearRemap);
    mAGCDensityData.finalizeColumns();

    // Setup the below ground carbon density  mappings
    success = parseXMLInternal(aGCAM2ELMCDENMap, &mBGCDensityData);
    mBGCDensityData.addYearColumn("Year", years, yearRemap);
    mBGCDensityData.finalizeColumns();

    // get base above ground carbon density
    coupleLog << "initGCAM: Getting above ground carbon density" << endl;
    // initialize data to zero
    double *agcd = mAGCDensityData.getData();
    fill(agcd, agcd+mAGCDensityData.getArrayLength(), 0.0);
    GetDataHelper getAGCD("world/region[+NamedFilter,MatchesAny]/land-allocator//child-nodes[+NamedFilter,MatchesAny]/carbon-calc/above-ground-carbon-density", mAGCDensityData);
    getAGCD.run(runner->getInternalScenario());
    //coupleLog << mAGCDensityData << endl;
    // write a diagnostic table
    std::string agcd_oname = "carbon_agcd_data_base.csv";
    oFile.open(agcd_oname);
    mAGCDensityData.printAsTable(oFile);
    oFile.close();

    // get base below ground carbon density
    coupleLog << "initGCAM: Getting below ground carbon density" << endl;
    // initialize data to zero
    double *bgcd = mBGCDensityData.getData();
    fill(bgcd, bgcd+mBGCDensityData.getArrayLength(), 0.0);
    GetDataHelper getBGCD("world/region[+NamedFilter,MatchesAny]/land-allocator//child-nodes[+NamedFilter,MatchesAny]/carbon-calc/below-ground-carbon-density", mBGCDensityData);
    getBGCD.run(runner->getInternalScenario());
    //coupleLog << mBGCDensityData << endl;
    // write a diagnostic table
    std::string bgcd_oname = "carbon_bgcd_data_base.csv";
    oFile.open(bgcd_oname);
    mBGCDensityData.printAsTable(oFile);
    oFile.close();

    // Clean up
    XMLParseHelper::cleanupParser();
    
    // Set start and end year
    gcamStartYear = modeltime->getStartYear();
    gcamEndYear = modeltime->getEndYear();
    
    // Stop the timer
    timer.stop();
    
}

/*!
 * \brief Run GCAM as part of E3SM.
 * \author Kate Calvin
 * \param yyyymmdd Current date, in yyyymmdd format
 * \param tod Time of day - not used in GCAM
 * \param gcami array of inputs to GCAM from E3SM
 * \param gcami_fdim1_nflds number of elements in gcami
 * \param gcami_fdim2_datasize size of gcami
 * \param gcamo array of outputs from GCAM for use in E3SM
 * \param gcamo_fdim1_nflds number of elements in gcamo
 * \param gcamo_fdim2_datasize size of gcamo
 * \param gcamoemis array of emissions outputs from GCAM for use in E3SM
 * \param gcamoemis_fdim1_nflds number of elements in gcamoemis
 * \param gcamoemis_fdim2_datasize size of gcamoemis
 * \param yr1 Year index used in GLM?
 * \param yr2 Year index used in GLM?
 * \param sneakermode integer indicating sneakernet mode is on
 * \param write_rest integer indicating restarts should be written
 * \This now controls the setting of scalars, otherwise they get overwritten by restart data
 */
void GCAM_E3SM_interface::runGCAM( int *yyyymmdd, double *gcamoluc, double *gcamoemiss,
                                   std::string aBaseLucGcamFileName, std::string aBaseCO2GcamFileName, bool aSpinup,
                                   double *aELMArea, double *aELMPFTFract, double *aELMNPP, double *aELMHR,
                                   int *aNumLon, int *aNumLat, int *aNumPFT, int *aNumReg, int *aNumCty, int *aNumSector, int *aNumPeriod,
                                   std::string aMappingFile, int *aFirstCoupledYear, bool aReadScalars,
                                   bool aWriteScalars, bool aScaleAgYield, bool aScaleCarbon,
                                   std::string aBaseNPPFileName, std::string aBaseHRFileName, std::string aBasePFTWtFileName, bool aRestartRun )
{
    int z, p, i, num_it, spinup;
    int row, lurow, r, l;
    ofstream oFile;
    Timer timer;
    double *co2 = mCO2EmissData.getData();
    std::string co2_oname;

    // Get year only of the current date
    const Modeltime* modeltime = runner->getInternalScenario()->getModeltime();
    // Note that GCAM runs to get values up to 5 years ahead of E3SM
    int e3smYear = *yyyymmdd/10000;
    // If the e3smYear is not a GCAM model year, then GCAM does not run;
    //    the existing data are interpolated to the E3SM year 
    //    so the gcam year needs to be set here because the loop below does not run
    int gcamPeriod = modeltime->getyr_to_per( e3smYear );
    // get the gcamYear before the current period has been sorted out
    // don't need to worry about testing the year because the period
    //    and year get updated below if gcam runs
    //    and otherwise it is correct year
    int gcamYear = modeltime->getper_to_yr( gcamPeriod );

    ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
    coupleLog.setLevel( ILogger::NOTICE );

    coupleLog << "Before period is advanced, Current E3SM Year is " << e3smYear << ", Current GCAM Year is " << gcamYear << endl;

    // If this is the initial year 2015 then run spinup first
    // and write the base file data (or the base file itself)
    // should get this year and a flag for doing this from namelist

    // if this is a restart run then do not do spinup
    if (aRestartRun) {aSpinup = 0;}

    // run on GCAM interval
    if( modeltime->isModelYear( e3smYear ) ) {

        if ( e3smYear == 2015 && aSpinup) {
            // do a spinup loop first and save the base year data (2015)
            num_it = 2;
            spinup = 1;
            coupleLog << "Performing GCAM spinup" << endl;
        } else {
            num_it = 1;
            spinup = 0;
        }

	for( z = 0; z < num_it; z++) {

            // If the e3smYear is a GCAM model period, then we need to increment GCAM's model period
            // unless this is the spinup loop
            if ( modeltime->isModelYear( e3smYear ) && spinup == 0) {
                gcamPeriod = gcamPeriod + 1;
                coupleLog << "Advancing period for regular GCAM run" << endl;
            }

            gcamYear = modeltime->getper_to_yr( gcamPeriod );
    
            bool success = false;
    
            if ( gcamYear < gcamStartYear ) {
                coupleLog << "returning from runGCAM: E3SM year: " << *yyyymmdd << " (GCAM year: " << gcamYear << ") is before GCAM starting date: " << gcamStartYear << endl;
                return;
            }
            if ( gcamYear > gcamEndYear ) {
                coupleLog << "returning from runGCAM: E3SM year: " << *yyyymmdd << " (GCAM year: " << gcamYear << ") is after GCAM ending date: " << gcamEndYear << endl;
                return;
            }
   
            coupleLog << "Current E3SM Year is " << e3smYear << ", Current GCAM Year is " << gcamYear << endl;
 
            int finalCalibrationYear = modeltime->getper_to_yr( modeltime->getFinalCalibrationPeriod() );
 
            coupleLog << "finalCalibrationYear is " << finalCalibrationYear << endl;
 
            // set restart period
            // run the calibration if this is the first year
            if ( spinup == 1 ) {
                restartPeriod = -1;
            } else {
                // run this period without restart
                // note that the restart files are used up through gcamPeriod-1
                // in a continuous run (after the initial year) the previous state is stored and no restarts are used to run gcamPeriod
                restartPeriod = gcamPeriod;
            }

            // if it is a restart run need to run up to previous period first using restarts
            // note that restart files include yield scalars (and carbon densities)
            // because of the year check above, this will not initiate a spinup
            if (aRestartRun) {
                coupleLog << "Restart run: first running through period " << gcamPeriod-1 << endl;
                timer.start();
                success = runner->runScenarios( gcamPeriod-1, true, timer );
                timer.stop();
            }

            // now set scalars for the current period
            // this function checks the e3smYear for the first coupled year (hardcoded to 2016)
            //    this is just so scalars are not calculated in the start year of 2015
            // try doing this check here instead
            if( e3smYear >=  *aFirstCoupledYear ) {
               setLandProductivityScalingGCAM(yyyymmdd, aELMArea, aELMPFTFract, aELMNPP, aELMHR,
                            aNumLon, aNumLat, aNumPFT, aMappingFile, aFirstCoupledYear, aReadScalars, aWriteScalars,
                            aScaleAgYield, aScaleCarbon, aBaseNPPFileName, aBaseHRFileName, aBasePFTWtFileName);
            }

            // now run the current period
            //Timer timer;
        
            coupleLog << "Running GCAM for year " << gcamYear;
            coupleLog << ", calculating period = " << gcamPeriod << endl;
        
            coupleLog.precision(20);
       
            // this needs to happen on initialization
            // the base year data should be read in and stored in variable at this level
            // but there also needs to be a restart file that contains the appropriate data
            //    both years are needed to be read/written because a restart can happen between GCAM run years
            //    the file probably should be written at the end of this function
            //if (e3smYear == 2015 && spinup == 0) {
            //    EmissDownscale surfaceCO2(*aNumLon, *aNumLat, 12, 1, *aNumReg, *aNumCty, *aNumSector, *aNumPeriod); // Emissions data is monthly now
                
            //    coupleLog << aBaseCO2GcamFileName << endl;
            //    surfaceCO2.readRegionBaseYearEmissionData(aBaseCO2GcamFileName);
            //    coupleLog << "GCAM run: Finish read Base year emission data" << endl;
                

            //    for (int i = 0; i < (*aNumReg); ++i) {
            //            mGcamCO2EmissPreviousGCAMYear[i*(*aNumSector)] = surfaceCO2.mBaseYearEmissions_sfc[i];
            //            mGcamCO2EmissPreviousGCAMYear[i*(*aNumSector)+1] = surfaceCO2.mBaseYearEmissions_air[i];
            //            mGcamCO2EmissPreviousGCAMYear[i*(*aNumSector)+2] = surfaceCO2.mBaseYearEmissions_ship[i];
            //        }
            //}
            coupleLog << "Previous GCAM Year Emission: " << endl;
            coupleLog << mGcamCO2EmissPreviousGCAMYear[0] << endl;
            coupleLog << mGcamCO2EmissPreviousGCAMYear[1] << endl;
            coupleLog << mGcamCO2EmissPreviousGCAMYear[2] << endl;
            //coupleLog << mCO2EmissData << endl;
        
            // Initialize the timer.  Create an object of the Timer class.
            timer.start();
        
            // Run this GCAM period
            success = runner->runScenarios( gcamPeriod, true, timer );
        
            // Stop the timer
            timer.stop();
        
            coupleLog << "Getting CO2 Emissions" << endl;
            // be sure to reset any data set previously
            fill(co2, co2+mCO2EmissData.getArrayLength(), 0.0);
            GetDataHelper getCo2("world/region[+NamedFilter,MatchesAny]/sector[+NamedFilter,MatchesAny]//ghg[NamedFilter,StringEquals,CO2]/emissions[+YearFilter,IntEquals,"+util::toString(gcamYear)+"]", mCO2EmissData);
            getCo2.run(runner->getInternalScenario());
            std::copy(co2, co2+mCO2EmissData.getArrayLength(), mGcamCO2EmissCurrentGCAMYear.begin());
            
            coupleLog << "mCO2EmissData.getArrayLength:" << endl;
            coupleLog << mCO2EmissData.getArrayLength() << endl;
            coupleLog << mGcamCO2EmissCurrentGCAMYear[0] << endl;
            coupleLog << mGcamCO2EmissCurrentGCAMYear[1] << endl;
            coupleLog << mGcamCO2EmissCurrentGCAMYear[2] << endl;
            // write the 2015 CO2 to a table
            // this is the actual co2 base file, but don't use the namelist value
            // write this to hardcoded name within run directory
            // this eliminates overwrite of the standard baseline file
            if (spinup == 1) {
                co2_oname = "gcam_co2_2015_base_file.csv";
                oFile.open(co2_oname);
                mCO2EmissData.printAsTable(oFile);
                oFile.close();
            } else {
               // otherwise write the co2 data for restarts
               // for restarts < 2020 can just use the base file for 2015
               co2_oname = std::string("gcam_co2_restart_") + std::to_string(gcamYear) + std::string(".csv");
               oFile.open(co2_oname);
               mCO2EmissData.printAsTable(oFile);
               oFile.close();
            }
        
            coupleLog << "Getting LUC" << endl;
            double *luc = mLUCData.getData();
            // be sure to reset any data set previously
            fill(luc, luc+mLUCData.getArrayLength(), 0.0);
            GetDataHelper getLUC("world/region[+NamedFilter,MatchesAny]/land-allocator//child-nodes[+NamedFilter,MatchesAny]/land-allocation[+YearFilter,IntEquals,"+util::toString(gcamYear)+"]", mLUCData);
            getLUC.run(runner->getInternalScenario());
            //coupleLog << mLUCData << endl;
       
            // write the 2015 LUC to a table
            // this is diagnostic so hardcode the file name
            if (spinup == 1) {
                std::string luc_oname = "luc_2015_out.csv";
                oFile.open(luc_oname);
                mLUCData.printAsTable(oFile);
                oFile.close();
            } 

            coupleLog << "Getting Wood harvest" << endl;
            double *woodHarvest = mWoodHarvestData.getData();
            // be sure to reset any data set previously
            fill(woodHarvest, woodHarvest+mWoodHarvestData.getArrayLength(), 0.0);
            GetDataHelper getWH("world/region[+NamedFilter,MatchesAny]/sector[NamedFilter,StringEquals,Forest]/subsector/technology[+NamedFilter,MatchesAny]//output[IndexFilter,IntEquals,0]/physical-output[+YearFilter,IntEquals,"+util::toString(gcamYear)+"]", mWoodHarvestData);
            getWH.run(runner->getInternalScenario());
            //coupleLog << mWoodHarvestData << endl;

            // write the 2015 wood harvest to a table for a base file
            // this is diagnostic so hardcode the file name 
            if (spinup == 1) {
                std::string wh_oname = "wh_2015_out.csv";
                oFile.open(wh_oname);
                mWoodHarvestData.printAsTable(oFile);
                oFile.close();
            }       
 
            // Set data in the gcamoluc* arrays
            // also write the baseline csv file if in spinup mode
            // but don't use the namelist value
            // write this to hardcoded name within run directory
            // this eliminates overwrite of the standard baseline file
            //
            // The ReMapData.printAsTable() function rounds and prints to 7 fixed decimal places
            // Do the same with this output file
            // This is because these data are used for initialization and restarts and should be consistent
            // So round the actual data to 7 fixed decimal places as well

            if (spinup == 1) {
                std::string luwh_oname = "gcam_lu_wh_2015_base_file.csv";
                oFile.open(luwh_oname);
                oFile.setf(ios::fixed, ios::floatfield);
                oFile.precision(7);
                oFile << "glu,type,Year,value" << endl;
            }

            const Modeltime* modeltime = runner->getInternalScenario()->getModeltime();
            row = 0;
            lurow = 0;
            // The variables below are read in, but to get them from the namelist would require changes to the runGCAM call
            // For now, we'll derive them from data already accessible
            int numRegions = mWoodHarvestData.getArrayLength();
            int numLUTypes = mLUCData.getArrayLength() / mWoodHarvestData.getArrayLength();
            for( r = 0; r < numRegions; r++) {
                for( l = 0; l < numLUTypes; l++) {
                    gcamoluc[row] = round(luc[lurow] * 1e7) / 1e7;
                    if (spinup == 1) {
                        oFile << r+1 << "," << l+1 << "," << gcamYear << "," << gcamoluc[row] << endl;
                    }
                    lurow++;
                    row++;
                }
                // Convert to tC and set wood harvest data to output vector
                // this is the factor that GCAM uses; it is an intermediate value
                // conversion from m^3 of biomass to MgC (0.250 tonnes (Mg) C per m^3))
                gcamoluc[row] = round(mWoodHarvestData.getData()[r] * 250000000 * 1e7) / 1e7;
                if (spinup == 1) {
                    oFile << r+1 << "," << l+1 << "," << gcamYear << "," << gcamoluc[row] << endl;
                }
                row++;
            }

            if (spinup == 1) {
                oFile.close();
            }
        
            // Print output
            runner->printOutput(timer);

            spinup = 0;

        } // end for z loop for spinup option

    } // end if GCAM run year

    fill(co2, co2+mCO2EmissData.getArrayLength(), 0.0);
    // interpolate the year to current e3sm year
    // the gcam year is correct here because the period is advanced before gcam runs,
    //    or it is correct from the start
    double ratio = (gcamYear - e3smYear)/5.0;
    //coupleLog << "Ratio:" << endl;
    //coupleLog << ratio << endl;
    // round these data to 1e7 precision
    for (i = 0; i < (*aNumReg)*(*aNumSector); ++i){
         co2[i] = ratio * mGcamCO2EmissPreviousGCAMYear[i] +  (1 - ratio) * mGcamCO2EmissCurrentGCAMYear[i];
         co2[i] = round(co2[i] * 1e7) / 1e7;
    }

    std::copy(co2, co2 + mCO2EmissData.getArrayLength(), gcamoemiss);

    // temporarily output current year data to diagnose restarts
    //coupleLog << "Interpolated co2 data for e3sm year " << e3smYear  << endl;
    //for (r = 0; r < mCO2EmissData.getArrayLength(); r++) {
    //   coupleLog << gcamoemiss[r] << endl;
    //}

    // restart file may need to be written here

    if(e3smYear == (gcamYear-1)) {
        // set previous year CO2 emission
        coupleLog << "Set previous year CO2 Emissions" << endl;  
        std::copy(mGcamCO2EmissCurrentGCAMYear.begin(), mGcamCO2EmissCurrentGCAMYear.end(), mGcamCO2EmissPreviousGCAMYear.begin());
    }

        
}

void GCAM_E3SM_interface::setLandProductivityScalingGCAM(int *yyyymmdd, double *aELMArea, double *aELMPFTFract, double *aELMNPP, double *aELMHR,
                                         int *aNumLon, int *aNumLat, int *aNumPFT, std::string aMappingFile, int *aFirstCoupledYear, bool aReadScalars,
                                          bool aWriteScalars, bool aScaleAgYield, bool aScaleCarbon,
                                          std::string aBaseNPPFileName, std::string aBaseHRFileName, std::string aBasePFTWtFileName) {
    // Get year only of the current date
    // Note that GCAM runs one period ahead of E3SM. We make that adjustment here
    const Modeltime* modeltime = runner->getInternalScenario()->getModeltime();
    int e3smYear = *yyyymmdd/10000;
    // If the e3smYear is not a GCAM period end year, then GCAM runs this period
    int gcamPeriod = modeltime->getyr_to_per( e3smYear );
    // If the e3smYear is a GCAM model period end year, then increment the period
    if ( modeltime->isModelYear( e3smYear ) ) {
        gcamPeriod = gcamPeriod + 1;
    }

    std::string fName;
    int numScalars=0;

    int gcamYear = modeltime->getper_to_yr( gcamPeriod );
    const int finalCalibrationPeriod = modeltime->getFinalCalibrationPeriod();
    const int finalCalibrationYear = modeltime->getper_to_yr(finalCalibrationPeriod);
    
    // Create scalar vectors
    // TODO: Find a better way to determine the length
    int max_size = 17722;
    std::vector<int> scalarYears(max_size);
    vector<std::string> scalarRegion(max_size);
    vector<std::string> scalarLandTech(max_size);
    vector<double> aboveScalarData(max_size);
    vector<double> belowScalarData(max_size);
    // need additional records because RFD and IRR have different agcd
    std::vector<int> cdensityYears;
    std::vector<std::string> cdensityRegion;
    std::vector<std::string> cdensityLandTech;
    std::vector<double> AGCD_scaled;
    std::vector<double> BGCD_scaled;
    
    // Open the coupling log
    ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
    coupleLog.setLevel( ILogger::NOTICE );
   
    coupleLog << "In setLandProductivityScalingGCAM, e3smYear is: " << e3smYear << endl;
    coupleLog << "In setLandProductivityScalingGCAM, gcamYear is: " << gcamYear << endl; 
    coupleLog << "In setLandProductivityScalingGCAM, before first year check, aScaleAgYield is: " << aScaleAgYield << endl;
    coupleLog << "In setLandProductivityScalingGCAM, before first year check, aScaleCarbon is: " << aScaleCarbon << endl;

    // Only set carbon densities during GCAM model years after the first coupled year
    // set carbon densities each year to calc and write them and to match running gcam each year
    //if( modeltime->isModelYear( gcamYear ) && e3smYear >=  *aFirstCoupledYear ) {
    if( e3smYear >=  *aFirstCoupledYear ) {
        CarbonScalers e3sm2gcam(*aNumLon, *aNumLat, *aNumPFT);
        
        // Get scaler information
        // The scalar file name and the year inside refer to the GCAM year they are being applied to
        if ( aReadScalars ) {
            coupleLog << "In setLandProductivityScalingGCAM, Reading scalars from file." << endl;
            fName = "./scalars_" + std::to_string(gcamYear) + ".csv";
            numScalars = e3sm2gcam.readScalers(fName, scalarYears, scalarRegion, scalarLandTech, aboveScalarData, belowScalarData);
        } else {
            coupleLog << "In setLandProductivityScalingGCAM, Calculating scalars from data." << endl;
            e3sm2gcam.readRegionalMappingData(aMappingFile);
            e3sm2gcam.calcScalers(gcamYear, aELMArea, aELMPFTFract, aELMNPP, aELMHR,
                                  scalarYears, scalarRegion, scalarLandTech, aboveScalarData, belowScalarData,
                                  aBaseNPPFileName, aBaseHRFileName, aBasePFTWtFileName, numScalars);
        }
        
        // Optional: write scaler information to a file
        // TODO?: make the file name an input instead of hardcoded
        if( aWriteScalars && !aReadScalars) {
            fName = "./scalars_" + std::to_string(gcamYear) + ".csv";
            e3sm2gcam.writeScalers(fName, scalarYears, scalarRegion, scalarLandTech, aboveScalarData, belowScalarData, numScalars);
        }
      
        // Note that the arrays used below are filled only with records that have existing locations/types and values between:
        //    0.125 and 2; values are set to 1.0 if something is invalid but there is a valid location/type 
        // Empty records are not found when passed through SetDataHelper, so nothing in GCAM changes for these
        
        if( aScaleAgYield ) { 

           coupleLog << "In setLandProductivityScalingGCAM, setting ag yield scalars, aScaleAgYield is: " << aScaleAgYield;
           coupleLog << "; numScalars is " << numScalars << endl;

           SetDataHelper setAgYieldScaler(scalarYears, scalarRegion, scalarLandTech, aboveScalarData, "world/region[+name]/sector/subsector/technology[+name]/period[+year]/yield-scaler");
           setAgYieldScaler.run(runner->getInternalScenario());
        }

        if(aScaleCarbon){
           coupleLog << "In setLandProductivityScalingGCAM, setting scaled carbon density, aScaleCarbon is: " << aScaleCarbon;
           coupleLog << "; numScalars is " << numScalars << endl;

           // should write a function to do the first part before setdatahelper

           // scale the base above and below ground carbon densities
           // do this here so it addresses readScalers as well as calcScalers
           // need to  match the region and basin and landtype
           //    scalarRegion is just the region
           //    scalarLandTech is basin_landtype (from the mapping files, so no irriation or fertilzer tags)
           // the density data objects have a 'region' column that is region_basin and 'land-type' column that is land type (no irr and fert tags)
           //    and a 'water' column (none, IRR, RFD) and an 'mgmt' column (none, IRR, RFD)
           // note that above and below scalarData have the same number and order of records, and so will AGCD_ and BGCD_scaled

           // scale the carbon density
           // only need the same valid locations/land types below, as for valid scalars
           double* agcd = mAGCDensityData.getData();
           double* bgcd = mBGCDensityData.getData();
           // mColumns==0 is region_basin (i.e., regID); 32 of these
           // mColumns==1 is land type (sans irr and fert tags); 41 of these
           // need to make sure locations and land types match the input carbon density data,
           //    as the carbon density data likely is not in the order and selection of these scalar data
           //    but store the result in the same order as for the yield scalars to utilize the region/landtech arrays
           // don't need the year data because there is only one year (it is included in the object for diagnostic purposes)

           std::string regID;
           std::string landType;
           std::string water_tag;
           std::string mgmt_tag;
           std::vector<string> strs;
           int cdCount=0;
           for (int i=0; i<numScalars; i++) {
              // need to recombine the region_basin and extract the land type
              // split the land tech into basin and land type
              // currently no "_" in either basein abr or land type name
              boost::split(strs, scalarLandTech[i], boost::is_any_of("_"));
              // set the appropriate strings
              regID = scalarRegion[i] + "_" + strs[1];
              landType = strs[0];

              // need to get the nonzero elements across the water and mgmt options!
              // currently values across water and mgmt options are the same (if they exist)
              //    but non-crops do not have any water/mgmt
              // note that these abort if the value is not found
              // these are the same for agcd and bgcd cuz they are defined by the same mapping file
              size_t regID_ind = mAGCDensityData.getIndexInColumn("region", regID);
              size_t landType_ind = mAGCDensityData.getIndexInColumn("land-type", landType);
              size_t cdenRow;
              size_t numLandType = (*mAGCDensityData.getColumn("land-type")).mInOrderOutputNames.size();
              size_t numWater = (*mAGCDensityData.getColumn("water")).mInOrderOutputNames.size();
              size_t numMgmt = (*mAGCDensityData.getColumn("mgmt")).mInOrderOutputNames.size();

              //coupleLog << "region ind: " << regID_ind << " " << regID << "; landType ind: " << landType_ind << " " << landType << endl;
              //coupleLog << "scalar ind: " << i << "; numLandType=" << numLandType << "; numWater=" << numWater << "; numMgmt=" << numMgmt << endl; 

              // loop over the 9 water/mgmt indices and store non-zero values
              // these can be the same for agcd and bgcd
              for (size_t water_ind=0; water_ind < numWater; water_ind++) {
                  for (size_t mgmt_ind=0; mgmt_ind < numMgmt; mgmt_ind++) {

                      // math ok cuz all positive and size_t is unsigned long
                      cdenRow = regID_ind * numLandType * numWater * numMgmt + landType_ind * numWater * numMgmt + water_ind * numMgmt + mgmt_ind;

                      // keep if either agcd or bgcd are non-zero
                      if(agcd[cdenRow] != 0 || bgcd[cdenRow] !=0) {
                          //coupleLog << "cdenRow=" << cdenRow << "; water_ind=" << water_ind << "; mgmt_ind=" << mgmt_ind << endl;

                          AGCD_scaled.push_back(agcd[cdenRow] * aboveScalarData[i]);
                          BGCD_scaled.push_back(bgcd[cdenRow] * belowScalarData[i]);
                          cdensityYears.push_back(scalarYears[i]);
                          cdensityRegion.push_back(scalarRegion[i]);
                          // do not append 'none' to the non-crop types
                          water_tag = (*mAGCDensityData.getColumn("water")).mInOrderOutputNames[water_ind];
                          mgmt_tag = (*mAGCDensityData.getColumn("mgmt")).mInOrderOutputNames[mgmt_ind];
                          if(water_tag != "none" && mgmt_tag != "none") {
                             cdensityLandTech.push_back( scalarLandTech[i] + "_" + water_tag + "_" + mgmt_tag );
                          } else {
                             cdensityLandTech.push_back( scalarLandTech[i] );
                          }
                          //coupleLog << "cdCount=" << cdCount << "; cdenRow=" << cdenRow  <<  "; cdYears=" << cdensityYears[cdCount] << "; cdRegion=" << cdensityRegion[cdCount];
                          //coupleLog << "; cdLandTech=" << cdensityLandTech[cdCount] << "; AGCD_scaled=" << AGCD_scaled[cdCount] << "; BGCD_scaled=" << BGCD_scaled[cdCount] << endl;

                          cdCount += 1;
                      }                 
                  } // end for mgmt_ind loop
              } // end for water_ind loop

           } // end for i loop over scalar values

           // set the scaled above ground carbon density to the state variable; the year data are not used
           coupleLog << "In setLandProductivityScalingGCAM: Setting scaled vegetation vegetation carbon density" << endl;
           SetDataHelper setAGCD(cdensityYears, cdensityRegion, cdensityLandTech, AGCD_scaled,
              "world/region[+name]/land-allocator//child-nodes[+name]/carbon-calc/above-ground-carbon-density");
           setAGCD.run(runner->getInternalScenario());

           // set the scaled below ground carbon density to the state variable; the year data are not used
           coupleLog << "In setLandProductivityScalingGCAM: Setting scaled soil carbon density" << endl;
           SetDataHelper setBGCD(cdensityYears, cdensityRegion, cdensityLandTech, BGCD_scaled,
              "world/region[+name]/land-allocator//child-nodes[+name]/carbon-calc/below-ground-carbon-density");
           setBGCD.run(runner->getInternalScenario());
        }

    } // end if >= first coupled year
    
}

void GCAM_E3SM_interface::downscaleEmissionsGCAM(double *gcamoemiss,
                                                 double *gcamoco2sfcjan, double *gcamoco2sfcfeb, double *gcamoco2sfcmar, double *gcamoco2sfcapr,
                                                 double *gcamoco2sfcmay, double *gcamoco2sfcjun, double *gcamoco2sfcjul, double *gcamoco2sfcaug,
                                                 double *gcamoco2sfcsep, double *gcamoco2sfcoct, double *gcamoco2sfcnov, double *gcamoco2sfcdec,
                                                 double *gcamoco2airlojan, double *gcamoco2airlofeb, double *gcamoco2airlomar, double *gcamoco2airloapr,
                                                 double *gcamoco2airlomay, double *gcamoco2airlojun, double *gcamoco2airlojul, double *gcamoco2airloaug,
                                                 double *gcamoco2airlosep, double *gcamoco2airlooct, double *gcamoco2airlonov, double *gcamoco2airlodec,
                                                 double *gcamoco2airhijan, double *gcamoco2airhifeb, double *gcamoco2airhimar, double *gcamoco2airhiapr,
                                                 double *gcamoco2airhimay, double *gcamoco2airhijun, double *gcamoco2airhijul, double *gcamoco2airhiaug,
                                                 double *gcamoco2airhisep, double *gcamoco2airhioct, double *gcamoco2airhinov, double *gcamoco2airhidec,
                                                 std::string aRegionMappingFile, std::string aCountryMappingFile, std::string aCountry2RegionMappingFile,
                                                 std::string aPOPIIASAFileName, std::string aGDPIIASAFileName,
                                                 std::string aPOPGCAMFileName, std::string aGDPGCAMFileName, std::string aCO2GCAMFileName,
                                                 int *aNumReg, int *aNumCty, int *aNumSector, int *aNumPeriod, int *aNumLon, int *aNumLat, bool aWriteCO2, int *aCurrYear,
                                                 std::string aCO2DownscalingMethod) {

    int r, s, row;
    //std::vector<double> gcamoSfcEmissVector((*aNumReg),0);
    //std::vector<double> gcamoAirEmissVector((*aNumReg),0);
   
    // Downscale surface CO2 emissions
    ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
    coupleLog.setLevel( ILogger::NOTICE );
    coupleLog << "Downscaling CO2 emissions" << endl;

    double gcamoemiss_sfc[*aNumReg];
    double gcamoemiss_air[*aNumReg];
    double gcamoemiss_ship[*aNumReg];

    // separate gcamoemiss into surface and aircraft and shipping sector vectors
    row = 0;
    for( r=0; r<(*aNumReg); r++ ) {
        for( s=0; s<(*aNumSector); s++) {
            if(s==0){
                gcamoemiss_sfc[r]=gcamoemiss[row];
            } else if(s==1){
                gcamoemiss_air[r]=gcamoemiss[row];
            }
            else if(s==2){
                gcamoemiss_ship[r]=gcamoemiss[row];
            }
            row++;
        }
    }

    //for(int tmp = 1; tmp <= (*aNumReg); tmp++) {
    //    coupleLog << "Diagnostics: regional surface CO2 Emissions in " << *aCurrYear << " = " << gcamoemiss_sfc[tmp - 1] << endl;
    //    coupleLog << "Regional surface scalar = " << gcamoemiss_sfc[tmp - 1] / mBaseYearEmissions_sfc[tmp - 1] << endl;
    //    coupleLog << "Diagnostics: regional aircraft CO2 Emissions in " << *aCurrYear << " = " << gcamoemiss_air[tmp - 1] << endl;
    //    coupleLog << "Diagnostics: regional shipment CO2 Emissions in " << *aCurrYear << " = " << gcamoemiss_ship[tmp - 1] << endl;
    //}

    // emissions data are monthly, surface data excludes international shipping
    EmissDownscale surfaceCO2(*aNumLon, *aNumLat, 12, 1, *aNumReg, *aNumCty, *aNumSector, *aNumPeriod);
    // Read in the GCAM regionXsector base CO2 data
    //surfaceCO2.readBaseYearCO2Data(aBaseCO2GcamFileName);
    // read in the gridded eam baseline co2 data
    //surfaceCO2.readSpatialData(aBaseCO2SfcFile, true, true, false);
    //surfaceCO2.downscaleCO2Emissions("surface", gcamoSfcEmissVector);

    // this read should not have worked because the ASpatialData constructor is not called to allocate the vectors and the access uses [] 
    //surfaceCO2.readSpatialData(aBaseCO2SfcFile, true, true, false);
    //coupleLog << "Finish read spatial data" << endl;
    
    if (aCO2DownscalingMethod == "Convergence")
    {
        coupleLog << "Start Convergence-based downscaling" << endl;
        surfaceCO2.readRegionMappingData(aRegionMappingFile);
        coupleLog << "Finish read regional mapping data" << aRegionMappingFile << endl;
        
        coupleLog << "Start readCountryMappingData:" << aCountryMappingFile << endl;
        surfaceCO2.readCountryMappingData(aCountryMappingFile);
        coupleLog << "Start readCountry2RegionMappingData:" << aCountry2RegionMappingFile << endl;
        surfaceCO2.readCountry2RegionMappingData(aCountry2RegionMappingFile);
        //coupleLog << "Start readRegionBaseYearEmissionData" << endl;
        //surfaceCO2.readRegionBaseYearEmissionData(aBaseCO2GcamFileName);
        coupleLog << "Start calculateCountryBaseYearEmissionData:" << endl;
        surfaceCO2.calculateCountryBaseYearEmissionData(mBaseYearEmissions_sfc, mBaseYearEmissionsGrid_sfc);
       
        
        coupleLog << "Start readPOPGDPCO2Data" << aCO2GCAMFileName << endl;
        surfaceCO2.readPOPGDPCO2Data(aPOPIIASAFileName, aGDPIIASAFileName, aPOPGCAMFileName, aGDPGCAMFileName, aCO2GCAMFileName);
        coupleLog << "Start downscaleSurfaceCO2EmissionsFromRegion2Country" << endl;
        surfaceCO2.downscaleSurfaceCO2EmissionsFromRegion2Country(gcamoemiss_sfc, *aCurrYear/10000);
        coupleLog << "Start downscaleSurfaceCO2EmissionsFromCountry2Grid" << endl;
        surfaceCO2.downscaleSurfaceCO2EmissionsFromCountry2Grid(gcamoemiss_sfc, mBaseYearEmissions_sfc, mBaseYearEmissionsGrid_sfc);
    }
    else
    {
        surfaceCO2.readRegionMappingData(aRegionMappingFile);
        coupleLog << "Finish read regional mapping data and start surface downscaling" << endl;
        
        //surfaceCO2.readRegionBaseYearEmissionData(aBaseCO2GcamFileName);
        //coupleLog << "GCAM run: Finish read Base year emission data" << endl;
        
        surfaceCO2.downscaleSurfaceCO2EmissionsFromRegion2Grid(gcamoemiss_sfc, mBaseYearEmissions_sfc, mBaseYearEmissionsGrid_sfc);
    }

    coupleLog << "after surface downscaling" << endl;
    // These regions are in order of the output regions in co2.xml 
    //for( r=0; r<(*aNumReg); r++ ) {
    //    coupleLog << "Diagnostics: Regional surface CO2 Emissions (TgC) in " << *aCurrYear << endl;
    //    coupleLog << "Region " << r+1  << " = " << gcamoemiss_sfc[r] << "; Regional surface scalar = " << gcamoemiss_sfc[r] / mBaseYearEmissions_sfc[r] << endl;
    //}

    if ( aWriteCO2 ) {
        // TODO: Set name of file based on case name?
        // note that the downscaleEmissions functions set the value data vector
        // need to set the lat, lon, and id vector for writing out co2 to file
        surfaceCO2.setIDVector(mBaseYearEmissionsGridID_sfc);
        surfaceCO2.setLonVector(mBaseYearEmissionsGridLon_sfc);
        surfaceCO2.setLatVector(mBaseYearEmissionsGridLat_sfc);
        string fNameSfc = "./gridded_co2_sfc_" + std::to_string(*aCurrYear) + ".txt";
        surfaceCO2.writeSpatialData(fNameSfc, true);
    }
    
    // Set the gcamoco2 monthly vector data to the output of this
    //surfaceCO2.separateMonthlyEmissions(gcamoco2sfcjan, gcamoco2sfcfeb, gcamoco2sfcmar, gcamoco2sfcapr,
    //                                   gcamoco2sfcmay, gcamoco2sfcjun, gcamoco2sfcjul, gcamoco2sfcaug,
    //                                   gcamoco2sfcsep, gcamoco2sfcoct, gcamoco2sfcnov, gcamoco2sfcdec, *aNumLon, *aNumLat);

    
    // shipment co2 emissions, monthly (international shipping only)
    EmissDownscale shipmentCO2(*aNumLon, *aNumLat, 12, 1, *aNumReg, *aNumCty, *aNumSector, *aNumPeriod);

    // this read should not have worked because the ASpatialData constructor is not called to allocate the vectors and the access uses []
    //shipmentCO2.readSpatialData(aBaseCO2ShipFile, true, true, false);
    //coupleLog << "Finish read spatial data" << endl;
    shipmentCO2.readRegionMappingData(aRegionMappingFile);
    coupleLog << "Finish read regional mapping data" << endl;
    //shipmentCO2.readRegionBaseYearEmissionData(aBaseCO2GcamFileName);
    coupleLog << "start shipment downscaling" << endl;
    shipmentCO2.downscaleInternationalShipmentCO2Emissions(gcamoemiss_ship, mBaseYearGlobalShipCO2Emiss, mBaseYearEmissionsGrid_ship);
    coupleLog << "after shipment downscaling" << endl;
    // These regions are in order of the output regions in co2.xml
    //for( r=0; r<(*aNumReg); r++ ) {
    //    coupleLog << "Diagnostics: Regional shipment CO2 Emissions (TgC) in " << *aCurrYear << endl;
    //    coupleLog << "Region " << r+1  << " = " << gcamoemiss_ship[r] << endl;
    //}

    if ( aWriteCO2 ) {
        // TODO: Set name of file based on case name?
        // note that the downscaleEmissions functions set the value data vector
        // need to set the lat, lon, and id vector for writing out co2 to file
        shipmentCO2.setIDVector(mBaseYearEmissionsGridID_ship);
        shipmentCO2.setLonVector(mBaseYearEmissionsGridLon_ship);
        shipmentCO2.setLatVector(mBaseYearEmissionsGridLat_ship);
        string fNameShip = "./gridded_co2_ship_" + std::to_string(*aCurrYear) + ".txt";
        shipmentCO2.writeSpatialData(fNameShip, true);
    }
    
    // Set the surface (surface + shipment) co2 data to the output of this
    // international shipping is considered as a surface sector, but it also covers the ocean (other surface sectors are over land only)
    separateSurfaceMonthlyEmissions(surfaceCO2, shipmentCO2, gcamoco2sfcjan, gcamoco2sfcfeb, gcamoco2sfcmar, gcamoco2sfcapr,
                                                gcamoco2sfcmay, gcamoco2sfcjun, gcamoco2sfcjul, gcamoco2sfcaug,
                                                gcamoco2sfcsep, gcamoco2sfcoct, gcamoco2sfcnov, gcamoco2sfcdec, *aNumLon, *aNumLat);
    
    // aircraft co2 monthly emissions at two levels
    EmissDownscale aircraftCO2(*aNumLon, *aNumLat, 12, 2, *aNumReg, *aNumCty, *aNumSector, *aNumPeriod);
    // this read should not have worked because the ASpatialData constructor is not called to allocate the vectors and the access uses []
    //aircraftCO2.readSpatialData(aBaseCO2AirFile, true, true, false);
    aircraftCO2.readRegionMappingData(aRegionMappingFile);
    //aircraftCO2.readRegionBaseYearEmissionData(aBaseCO2GcamFileName);
    coupleLog << "start aircraft downscaling" << endl;
    aircraftCO2.downscaleAircraftCO2Emissions(gcamoemiss_air, mBaseYearGlobalAirCO2Emiss, mBaseYearEmissionsGrid_air);
    coupleLog << "after aircraft downscaling" << endl;
    // These regions are in order of the output regions in co2.xml
    //for( r=0; r<(*aNumReg); r++ ) {
    //    coupleLog << "Diagnostics: Regional aircraft CO2 Emissions (TgC) in " << *aCurrYear << endl;
    //    coupleLog << "Region " << r+1  << " = " << gcamoemiss_air[r] << endl;
    //}

    if ( aWriteCO2 ) {
        // TODO: Set name of file based on case name?
        // note that the downscaleEmissions functions set the value data vector
        // need to set the lat, lon, and id vector for writing out co2 to file
        aircraftCO2.setIDVector(mBaseYearEmissionsGridID_air);
        aircraftCO2.setLonVector(mBaseYearEmissionsGridLon_air);
        aircraftCO2.setLatVector(mBaseYearEmissionsGridLat_air);
        string fNameAir = "./gridded_co2_air_" + std::to_string(*aCurrYear) + ".txt";
        aircraftCO2.writeSpatialData(fNameAir, true);
    }
    
    // Set the aircraft data to the output of this
    aircraftCO2.separateMonthlyEmissionsWithVertical( gcamoco2airlojan, gcamoco2airlofeb, gcamoco2airlomar, gcamoco2airloapr,
                                         gcamoco2airlomay, gcamoco2airlojun, gcamoco2airlojul, gcamoco2airloaug,
                                         gcamoco2airlosep, gcamoco2airlooct, gcamoco2airlonov, gcamoco2airlodec,
                                         gcamoco2airhijan, gcamoco2airhifeb, gcamoco2airhimar, gcamoco2airhiapr,
                                         gcamoco2airhimay, gcamoco2airhijun, gcamoco2airhijul, gcamoco2airhiaug,
                                         gcamoco2airhisep, gcamoco2airhioct, gcamoco2airhinov, gcamoco2airhidec,
                                         *aNumLon, *aNumLat);
    
    
    
}

// Separate the emissions vectors into individual months
void GCAM_E3SM_interface::separateSurfaceMonthlyEmissions(EmissDownscale surfaceCO2, EmissDownscale shipmentCO2, double *gcamoco2sfcjan, double *gcamoco2sfcfeb, double *gcamoco2sfcmar,
                                              double *gcamoco2sfcapr, double *gcamoco2sfcmay, double *gcamoco2sfcjun,
                                              double *gcamoco2sfcjul, double *gcamoco2sfcaug, double *gcamoco2sfcsep,
                                              double *gcamoco2sfcoct, double *gcamoco2sfcnov, double *gcamoco2sfcdec,
                                              int aNumLon, int aNumLat)
{
    std::vector<double> mCurrYearEmissVector;
    int gridPerMonth = aNumLat * aNumLon;
    
    // Perform element-wise addition
    for (int i = 0; i < gridPerMonth * 12; i++) {
        mCurrYearEmissVector.push_back(surfaceCO2.mCurrYearEmissVector[i] + shipmentCO2.mCurrYearEmissVector[i]);
    }
    
    std::copy(mCurrYearEmissVector.begin(),
              mCurrYearEmissVector.begin() + gridPerMonth, gcamoco2sfcjan);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth,
              mCurrYearEmissVector.begin() + gridPerMonth * 2, gcamoco2sfcfeb);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 2,
              mCurrYearEmissVector.begin() + gridPerMonth * 3, gcamoco2sfcmar);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 3,
              mCurrYearEmissVector.begin() + gridPerMonth * 4, gcamoco2sfcapr);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 4,
              mCurrYearEmissVector.begin() + gridPerMonth * 5, gcamoco2sfcmay);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 5,
              mCurrYearEmissVector.begin() + gridPerMonth * 6, gcamoco2sfcjun);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 6,
              mCurrYearEmissVector.begin() + gridPerMonth * 7, gcamoco2sfcjul);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 7,
              mCurrYearEmissVector.begin() + gridPerMonth * 8, gcamoco2sfcaug);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 8,
              mCurrYearEmissVector.begin() + gridPerMonth * 9, gcamoco2sfcsep);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 9,
              mCurrYearEmissVector.begin() + gridPerMonth * 10, gcamoco2sfcoct);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 10,
              mCurrYearEmissVector.begin() + gridPerMonth * 11, gcamoco2sfcnov);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 11,
              mCurrYearEmissVector.begin() + gridPerMonth * 12, gcamoco2sfcdec);
}

// read in gcam emissions for a given year (including the base year)
// the year is determined by the filename
// units are MMTC (TgC)
void GCAM_E3SM_interface::readRegionGcamYearEmissionData(std::string aFileName)
{
    // zero out the read-in arrays
    //memset(&mGcamYearEmissions_sfc[0], 0.0, mGcamYearEmissions_sfc.size() * sizeof mGcamYearEmissions_sfc[0]);
    //memset(&mGcamYearEmissions_air[0], 0.0, mGcamYearEmissions_air.size() * sizeof mGcamYearEmissions_air[0]);
    //memset(&mGcamYearEmissions_ship[0], 0.0, mGcamYearEmissions_ship.size() * sizeof mGcamYearEmissions_ship[0]);

    ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
    coupleLog.setLevel( ILogger::NOTICE );

    ifstream data(aFileName);
    if (!data.is_open())
    {
        coupleLog.setLevel( ILogger::ERROR );
        coupleLog << "File not found: " << aFileName << endl;
        exit(EXIT_FAILURE);
    }
    string str;
    getline(data, str); // skip the first line
    while (getline(data, str))
    {
        istringstream iss(str);
        string token;
        double value;
        string regID;
        string sectorID;

        // Parse Region Name
        getline(iss, token, ',');
        regID = token;
        regID.erase(remove(regID.begin(), regID.end(), '\"'), regID.end());

        // Parse Sector
        getline(iss, token, ',');
        sectorID = token;
        sectorID.erase(remove(sectorID.begin(), sectorID.end(), '\"'), sectorID.end());

        // Skip Year
        getline(iss, token, ',');

        // Parse Base-Year Emission
        getline(iss, token, ',');
        value = std::stod(token);

       // Get the index of the region
       // assumes that data are in order of mapping file
       size_t regID_ind = mCO2EmissData.getIndexInColumn("region", regID);
        
       if (sectorID == "surface")
       {
          mGcamYearEmissions_sfc[regID_ind] = value;
          //coupleLog << "readRegionGcamYearEmissionData: region " << regID << " sfc co2 = " << mGcamYearEmissions_sfc[regID_ind] << endl;
       }
       else if (sectorID == "shipment")
       {
          mGcamYearEmissions_ship[regID_ind] = value;
          //coupleLog << "readRegionGcamYearEmissionData: region " << regID << " ship co2 = " << mGcamYearEmissions_ship[regID_ind] << endl;
       }
       else if (sectorID == "aircraft")
       {
          mGcamYearEmissions_air[regID_ind] = value;
          //coupleLog << "readRegionGcamYearEmissionData: region " << regID << " air co2 = " << mGcamYearEmissions_air[regID_ind] << endl;
       }
       else {
            coupleLog.setLevel( ILogger::ERROR );
            coupleLog << "Sector" << sectorID << " in" << aFileName << "not present in current co2.xml output mapping" << endl;
            exit(EXIT_FAILURE);
        }
        
    }

    return;
}


// read in the gridded initialization emissions from a file to designated member variables
// four columns: id,lon,lat,value
// input value units are kg CO2 m-2 s-1
// aELMArea is grid cell area in km^2 and is in same order as input data, but just once
//    lon faster than lat, starting at 0 lon and -90 lat
// this can calculate and return the global total in GCAM units of TgC (MMTC) per year
double GCAM_E3SM_interface::readCO2DataGridCSV(std::string aFileName, bool aHasLatLon, bool aHasID, bool aCalcTotal, double *aELMArea, int *aNumLon, int *aNumLat,
                          std::string aSectorName)
{
    // create a double to store the converted totals
    double total = 0.0;
    double co22c = 12.0/44.0;
    double k2T = 1e-9;
    double km22m2 = 1e6;
    std::vector<int> secinmonth{2678400, 2419200, 2678400, 2592000, 2678400, 2592000, 2678400, 2678400, 2592000, 2678400, 2592000, 2678400};
    int numCells = (*aNumLon) * (*aNumLat);
    int gridIndex, monthIndex;
    int id;
    double lon, lat, value;
    
    ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
    coupleLog.setLevel( ILogger::NOTICE );
    coupleLog.precision(20);

    ifstream data(aFileName);
    if (!data.is_open())
    {
        coupleLog << "readCO2DataGridCSV: File not found: " << aFileName << endl;
        exit(EXIT_FAILURE);
    }
    string str;
    getline(data, str); // skip the first line
    int row = 0;
    while (getline(data, str))
    {
        istringstream iss(str);
        string token;
        //double value;
        
        if ( aHasID ) {
            //int id;

            // Parse ID
            getline(iss, token, ',');
            id = std::stoi(token);
            if (aSectorName == "surface")
            {
               mBaseYearEmissionsGridID_sfc.at(row) = id;
            } else if (aSectorName == "shipment")
            {
               mBaseYearEmissionsGridID_ship.at(row) = id;
            } else if (aSectorName == "aircraft")
            {
               mBaseYearEmissionsGridID_air.at(row) = id;
            } else
            {
               coupleLog << "readCO2DataGridCSV: Sector not found: " << aSectorName << endl;
               exit(EXIT_FAILURE);
            }
        }
        
        if ( aHasLatLon ) {
            //double lon;
            //int lat;

            // Parse longitude
            getline(iss, token, ',');
            lon = std::stod(token);

            // Parse latitude
            getline(iss, token, ',');
            lat = std::stod(token);

            if (aSectorName == "surface")
            {
               mBaseYearEmissionsGridLon_sfc[row] = lon;
               mBaseYearEmissionsGridLat_sfc[row] = lat;
            } else if (aSectorName == "shipment")
            {
               mBaseYearEmissionsGridLon_ship[row] = lon;
               mBaseYearEmissionsGridLat_ship[row] = lat;
            } else if (aSectorName == "aircraft")
            {
               mBaseYearEmissionsGridLon_air[row] = lon;
               mBaseYearEmissionsGridLat_air[row] = lat;
            } else 
            {
               coupleLog << "readCO2DataGridCSV: Sector not found: " << aSectorName << endl;
               exit(EXIT_FAILURE);
            }
        }

        // Parse value
        getline(iss, token, ',');
        value = std::stod(token);

        if (aSectorName == "surface")
        {
           mBaseYearEmissionsGrid_sfc[row] = value;
        } else if (aSectorName == "shipment")
        {
           mBaseYearEmissionsGrid_ship[row] = value;
        } else if (aSectorName == "aircraft")
        {
           mBaseYearEmissionsGrid_air[row] = value;
        } else
        {
           coupleLog << "readCO2DataGridCSV: Sector not found: " << aSectorName << endl;
           exit(EXIT_FAILURE);
        }
        
        // the 1d grid index is simply the remainder of the row (starting with 0) / number of grid cells
        //    because the grid is repeated monthly but in the same 1d order as the area
        // the 1d month index is the integer value of division because each month is in order
        gridIndex = row % numCells;
        monthIndex = row / numCells;
        // the aircraft data have two levels, with second level repeating after the first, so adjust the month index
        if (aSectorName == "aircraft") {
           if (monthIndex > 23) {
              coupleLog << "readCO2DataGridCSV: " << aSectorName << " data exceeds the expected maximum of two levels with monthIndex =  " << monthIndex << endl;
              exit(EXIT_FAILURE);
           } else if (monthIndex > 11 && monthIndex <= 23){
               monthIndex = monthIndex - 12;
           }
        } else {
           if (monthIndex > 11) {
              coupleLog << "readCO2DataGridCSV: " << aSectorName << " data exceeds the expected maximum of one level with monthIndex =  " << monthIndex << endl;
              exit(EXIT_FAILURE);
           }
        }

        // if aCalcTotal == true, then convert to GCAM units and add this to the total
        if(aCalcTotal){
           total += value * secinmonth[monthIndex] * aELMArea[gridIndex];
        }
   
        // check this somehow
        //if (value != 0) {
        //   coupleLog << aSectorName << " summing total kgCO2/s=" << total << "; row=" << row << "; gridIndex=" << gridIndex;
        //   coupleLog << "; aELMArea[" << gridIndex << "]=" << aELMArea[gridIndex] << "; numCells=" << numCells;
        //   coupleLog << "; lon=" << lon << "; lat=" << lat << "; ID=" << id << endl;
        //}

        row++;
    }

    if(aCalcTotal){
       //coupleLog << aSectorName << " before conversion total = " << total << endl;
       total = total * km22m2 * co22c * k2T;
       //coupleLog << aSectorName << " total = " << total << endl;
    }

    return total;
}

void GCAM_E3SM_interface::finalizeGCAM()
{
    Timer timer;
    // Initialize the timer.  Create an object of the Timer class.
    timer.start();
    ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
    coupleLog.setLevel( ILogger::NOTICE );
    coupleLog << "calling finalize" << endl;
    timer.stop();
}
