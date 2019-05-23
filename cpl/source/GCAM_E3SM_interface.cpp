/*! 
 * \file GCAM_E3SM_interface.cpp
 * \brief E3SM gcam driver source file.
 * \author Kate Calvin
 */


#include "util/base/include/auto_file.h"
#include "../include/GCAM_E3SM_interface.h"
#include "containers/include/world.h"

#include "../include/remap_data.h"
#include "../include/get_data_helper.h"

ofstream outFile;

// Pointer for a scenario
Scenario* scenario; // model scenario info

//auto_ptr<IScenarioRunner> runner;

vector<string> GCAM_E3SM_interface::regionName;
vector<string> GCAM_E3SM_interface::landType;
vector<string> GCAM_E3SM_interface::cropName;

/*! \brief Constructor
 * \details This is the constructor for the E3SM_driver class.
 */


GCAM_E3SM_interface::GCAM_E3SM_interface(){
}

//! Destructor. 
GCAM_E3SM_interface::~GCAM_E3SM_interface(){
}

/*! \brief Initializer for GCAM.
 * \details
 *  Initialize gcam log files and read in configuration
 *  and base model information.  Create and setup scenario
 */

void GCAM_E3SM_interface::initGCAM(void)
{
    
    // identify default file names for control input and logging controls
    string configurationArg = "configuration.xml";
    string loggerFactoryArg = "log_conf.xml";
    
    // Add OS dependent prefixes to the arguments.
    const string configurationFileName = configurationArg;
    const string loggerFileName = loggerFactoryArg;
    
    // Initialize the LoggerFactory
    bool success = XMLHelper<void>::parseXML( loggerFileName, &loggerFactoryWrapper );
    
    // Get the main log file.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::WARNING );
    
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
    success = XMLHelper<void>::parseXML( configurationFileName, conf );
    // Check if parsing succeeded. Non-zero return codes from main indicate
    
    
    // Initialize the timer.  Create an object of the Timer class.
    Timer timer;
    timer.start();
    
    // Create an empty exclusion list so that any type of IScenarioRunner can be
    // created.
    list<string> exclusionList;
    
    //  create the scenario runner
    //auto_ptr<IScenarioRunner> runner = ScenarioRunnerFactory::createDefault( exclusionList );
    runner = ScenarioRunnerFactory::createDefault( exclusionList );
    
    // Setup the scenario.
    success = runner->setupScenarios( timer );

    XMLHelper<void>::cleanupParser();
    
    const Modeltime* modeltime = runner->getInternalScenario()->getModeltime();
    
    gcamStartYear = modeltime->getStartYear();
    gcamEndYear = modeltime->getEndYear();
    
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
 */
void GCAM_E3SM_interface::runGCAM( int *yyyymmdd, int *tod, double *gcami, int *gcami_fdim1_nflds, int *gcami_fdim2_datasize, double *gcamo, int *gcamo_fdim1_nflds, int *gcamo_fdim2_datasize, double *gcamoemis, int *gcamoemis_fdim1_nflds, int *gcamoemis_fdim2_datasize, int *yr1, int *yr2, int *sneakermode, int *write_rest )
{
    
    // Get year only of the current date
    int curryear = *yyyymmdd/10000;
    bool success = false;
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    
    if ( curryear < gcamStartYear ) {
        mainLog << "returning from runGCAM: current date: " << *yyyymmdd << " is before GCAM starting date: " << gcamStartYear << endl;
        return;
    }
    if ( curryear > gcamEndYear ) {
        mainLog << "returning from runGCAM: current date: " << *yyyymmdd << " is after GCAM ending date: " << gcamEndYear << endl;
        return;
    }
    
    const Modeltime* modeltime = runner->getInternalScenario()->getModeltime();
    
    int finalCalibrationYear = modeltime->getper_to_yr( modeltime->getFinalCalibrationPeriod() );
    int period = modeltime->getyr_to_per( curryear );
    int modelyear = modeltime->getper_to_yr( period );
    
    mainLog << "curryear is " << curryear << endl;
    mainLog << "period is " << period << endl;
    mainLog << "modelyear is " << modelyear << endl;
    mainLog << "finalCalibrationYear is " << finalCalibrationYear << endl;
    
    
    if( modeltime->isModelYear( curryear )) {
        
        Timer timer;
        
        // TODO: is this necessary, it will be the same as currYear
        mainLog << "Running GCAM for year" << modelyear << endl;
        mainLog << "calculating period = " << period << endl;
        
        mainLog.precision(20);
        
        // Initialize the timer.  Create an object of the Timer class.
        timer.start();
        
        success = runner->runScenarios( period, true, timer );
        
        timer.stop();

        //PLPTEST
        ReMapData co2Data;
        vector<string> regions = { "USA",
"Africa_Eastern",
"Africa_Northern",
"Africa_Southern",
"Africa_Western",
"Australia_NZ",
"Brazil",
"Canada",
"Central America and Caribbean",
"Central Asia",
"China",
"EU-12",
"EU-15",
"Europe_Eastern",
"Europe_Non_EU",
"European Free Trade Association",
"India",
"Indonesia",
"Japan",
"Mexico",
"Middle East",
"Pakistan",
"Russia",
"South Africa",
"South America_Northern",
"South America_Southern",
"South Asia",
"South Korea",
"Southeast Asia",
"Taiwan",
"Argentina",
"Colombia" };
        map<string, string> regionMap;
        for(auto r : regions) {
            regionMap[r] = "Other";
        }
        regionMap["USA"] = "USA";
        vector<string> regionsOut = { "USA", "Other" };
        co2Data.addColumn("region", regionsOut, regionMap);
        vector<string> sectors = { "refining" };
        co2Data.addColumn("sector", sectors, map<string, string>());
        vector<int> years (modeltime->getmaxper());
        for(int per = 0; per < modeltime->getmaxper(); ++per) {
            years[per] = modeltime->getper_to_yr(per);
        }
        co2Data.addYearColumn("Year", years, map<int, int>());
        co2Data.finalizeColumns();
        GetDataHelper getCo2("world/region[+NamedFilter,MatchesAny]/sector[+NamedFilter,StringEquals,refining]/subsector[NamedFilter,MatchesAny]/technology[NamedFilter,MatchesAny]/period[YearFilter,MatchesAny]/ghg[NamedFilter,StringEquals,CO2]/emissions");
        getCo2.run(runner->getInternalScenario(), co2Data);
        double *co2 = co2Data.getData();
        for(size_t i = 0; i < (2 * 1 * years.size()); ++i) {
            cout << co2[i] << endl;
        }
        //PLPTEST
        
        // write restarts if needed.
        /*
        if(write_rest) {
            mainLog << "write_rest: " << *write_rest << endl;
            //runner->writeRestart( period, curryear );
        }
        */
    }
}

void GCAM_E3SM_interface::finalizeGCAM()
{
    Timer timer;
    // Initialize the timer.  Create an object of the Timer class.
    timer.start();
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "calling finalize" << endl;
    runner->printOutput(timer);
    timer.stop();
}
