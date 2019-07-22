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
#include "../include/set_data_helper.h"
#include "util/base/include/xml_helper.h"

ofstream outFile;

// Pointer for a scenario
Scenario* scenario; // model scenario info

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
    // TODO: Check if parsing succeeded. Non-zero return codes from main indicate
    
    
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

    // Setup the mappings
    success = XMLHelper<void>::parseXML("../cpl/mappings/co2.xml", &mCO2EmissData);
    const Modeltime* modeltime = runner->getInternalScenario()->getModeltime();
    vector<int> years (modeltime->getmaxper());
    for(int per = 0; per < modeltime->getmaxper(); ++per) {
        years[per] = modeltime->getper_to_yr(per);
    }
    mCO2EmissData.addYearColumn("Year", years, map<int, int>());
    
    success = XMLHelper<void>::parseXML("../cpl/mappings/luc.xml", &mLUCData);
    mLUCData.addYearColumn("Year", years, map<int, int>());

    // Clean up
    XMLHelper<void>::cleanupParser();
    
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
    
    mainLog << "Current Year is " << curryear << endl;
    
    
    if( modeltime->isModelYear( curryear )) {
        
        Timer timer;
        
        // TODO: is this necessary, it will be the same as currYear
        mainLog << "Running GCAM for year " << modelyear;
        mainLog << ", calculating period = " << period << endl;
        
        mainLog.precision(20);
        
        // Initialize the timer.  Create an object of the Timer class.
        timer.start();
        
        // Run this GCAM period
        success = runner->runScenarios( period, true, timer );
        
        // Stop the timer
        timer.stop();
       
        // Get all data that needs to be passed back to E3SM
        mCO2EmissData.finalizeColumns();
        GetDataHelper getCo2("world/region[+NamedFilter,MatchesAny]/sector[+NamedFilter,MatchesAny]//ghg[NamedFilter,StringEquals,CO2]/emissions");
        getCo2.run(runner->getInternalScenario(), mCO2EmissData);
        double *co2 = mCO2EmissData.getData();
        
        mLUCData.finalizeColumns();
        GetDataHelper getLUC("world/region[+NamedFilter,MatchesAny]/land-allocator//child-nodes[+NamedFilter,MatchesAny]/land-allocation");
        getLUC.run(runner->getInternalScenario(), mLUCData);
        double *luc = mLUCData.getData();
        
        // Set data in the gcamo* arrays
        const Modeltime* modeltime = runner->getInternalScenario()->getModeltime();
        vector<int> years (modeltime->getmaxper());
        for(size_t i = 0; i < mCO2EmissData.getArrayLength(); ++i) {
            gcamoemis[i] = co2[i];
        }
        for(size_t i = 0; i < mLUCData.getArrayLength(); ++i) {
            gcamo[i] = luc[i];
        }
        
    }
}

void GCAM_E3SM_interface::setDensityGCAM(int *yyyymmdd, int *tod, std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs, std::vector<double>& aScalers) {
    // Get year only of the current date
    int curryear = *yyyymmdd/10000;
    const Modeltime* modeltime = runner->getInternalScenario()->getModeltime();
    
    // Get scaler information
    // For now, we are reading this from a file, but we will need to get it from E3SM directly eventually.
    readScalers(yyyymmdd, aYears, aRegions, aLandTechs, aScalers);
    
    // Only set carbon densities during GCAM model years.
    if( modeltime->isModelYear( curryear )) {
        SetDataHelper setScaler(aYears, aRegions, aLandTechs, aScalers, "world/region[+name]/sector/subsector/technology[+name]/period[+year]/yield-scaler");
        setScaler.run(runner->getInternalScenario());
    }
    
}

// Read in scalers from a csv file
// Note: this is used for diagnostics and testing. In fully coupled E3SM-GCAM, these scalers
// are passed in code to the wrapper
void GCAM_E3SM_interface::readScalers(int *yyyymmdd, std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs, std::vector<double>& aScalers) {
    // Get year only of the current date
    int curryear = *yyyymmdd/10000;
    const Modeltime* modeltime = runner->getInternalScenario()->getModeltime();
    
    // Only set carbon densities during GCAM model years.
    if( modeltime->isModelYear( curryear )) {
        // TEMPORARY: Read scaler data from a file
        // NOTE: This will be passed from E3SM eventually
        ifstream data("scaler_data.csv");
        if (!data.is_open())
        {
            exit(EXIT_FAILURE);
        }
        string str;
        getline(data, str); // skip the first line
        int row = 0;
        while (getline(data, str))
        {
            istringstream iss(str);
            string token;
            int year;
            std::string region;
            std::string tech;
            double scaler;
            
            // Parse current year
            getline(iss, token, ',');
            year = std::stoi(token);
            
            // Parse region
            getline(iss, region, ',');
            
            // Parse ag production technology name
            getline(iss, tech, ',');
            
            // Parse scaler
            getline(iss, token, ',');
            scaler = std::stod(token);
            
            aYears.at(row) = year;
            aRegions[row] = region;
            aLandTechs[row] = tech;
            aScalers[row] = scaler;
            
            row++;
        }
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
