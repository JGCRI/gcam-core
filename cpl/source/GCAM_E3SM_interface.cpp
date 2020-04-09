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
#include "../include/carbon_scalers.h"
#include "../include/emiss_downscale.h"
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

void GCAM_E3SM_interface::initGCAM(std::string aCaseName, std::string aGCAMConfig, std::string aGCAM2ELMCO2Map, std::string aGCAM2ELMLUCMap, std::string aGCAM2ELMWHMap)
{
    
    // identify default file names for control input and logging controls
    string configurationArg = aGCAMConfig;
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
    success = XMLHelper<void>::parseXML(aGCAM2ELMCO2Map, &mCO2EmissData);
    mCO2EmissData.addYearColumn("Year", years, yearRemap);
    mCO2EmissData.finalizeColumns();

    // Setup the land use change mappings
    success = XMLHelper<void>::parseXML(aGCAM2ELMLUCMap, &mLUCData);
    mLUCData.addYearColumn("Year", years, yearRemap);
    mLUCData.finalizeColumns();
    
    // Setup the wood harvest mappings
    success = XMLHelper<void>::parseXML(aGCAM2ELMWHMap, &mWoodHarvestData);
    mWoodHarvestData.addYearColumn("Year", years, yearRemap);
    mWoodHarvestData.finalizeColumns();

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
void GCAM_E3SM_interface::runGCAM( int *yyyymmdd, double *gcamoluc, double *gcamoemis, std::string aBaseCO2File, double aBaseCO2EmissSfc,
                                  int aNumLon, int aNumLat, bool aWriteCO2 )
{
    // Get year only of the current date
    int curryear = *yyyymmdd/10000;
    bool success = false;
    ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
    coupleLog.setLevel( ILogger::NOTICE );
    
    if ( curryear < gcamStartYear ) {
        coupleLog << "returning from runGCAM: current date: " << *yyyymmdd << " is before GCAM starting date: " << gcamStartYear << endl;
        return;
    }
    if ( curryear > gcamEndYear ) {
        coupleLog << "returning from runGCAM: current date: " << *yyyymmdd << " is after GCAM ending date: " << gcamEndYear << endl;
        return;
    }
    
    const Modeltime* modeltime = runner->getInternalScenario()->getModeltime();
    
    int finalCalibrationYear = modeltime->getper_to_yr( modeltime->getFinalCalibrationPeriod() );
    int period = modeltime->getyr_to_per( curryear );
    int modelyear = modeltime->getper_to_yr( period );
    
    coupleLog << "Current Year is " << curryear << endl;
    
    
    if( modeltime->isModelYear( curryear )) {
        
        Timer timer;
        
        // TODO: is this necessary, it will be the same as currYear
        coupleLog << "Running GCAM for year " << modelyear;
        coupleLog << ", calculating period = " << period << endl;
        
        coupleLog.precision(20);
        
        // Initialize the timer.  Create an object of the Timer class.
        timer.start();
        
        // Run this GCAM period
        success = runner->runScenarios( period, true, timer );
        
        // Stop the timer
        timer.stop();
        
        double *co2 = mCO2EmissData.getData();
        // be sure to reset any data set previously
        fill(co2, co2+mCO2EmissData.getArrayLength(), 0.0);
        GetDataHelper getCo2("world/region[+NamedFilter,MatchesAny]/sector[+NamedFilter,MatchesAny]//ghg[NamedFilter,StringEquals,CO2]/emissions[+YearFilter,IntEquals,"+util::toString(curryear)+"]", mCO2EmissData);
        getCo2.run(runner->getInternalScenario());
        coupleLog << mCO2EmissData << endl;
        
        coupleLog << "Getting LUC" << endl;
        double *luc = mLUCData.getData();
        // be sure to reset any data set previously
        fill(luc, luc+mLUCData.getArrayLength(), 0.0);
        GetDataHelper getLUC("world/region[+NamedFilter,MatchesAny]/land-allocator//child-nodes[+NamedFilter,MatchesAny]/land-allocation[+YearFilter,IntEquals,"+util::toString(curryear)+"]", mLUCData);
        getLUC.run(runner->getInternalScenario());
        coupleLog << mLUCData << endl;
        
        coupleLog << "Getting Wood harvest" << endl;
        double *woodHarvest = mWoodHarvestData.getData();
        // be sure to reset any data set previously
        fill(woodHarvest, woodHarvest+mWoodHarvestData.getArrayLength(), 0.0);
        GetDataHelper getWH("world/region[+NamedFilter,MatchesAny]/sector[NamedFilter,StringEquals,Forest]/subsector/technology[+NamedFilter,MatchesAny]//output[IndexFilter,IntEquals,0]/physical-output[+YearFilter,IntEquals,"+util::toString(curryear)+"]", mWoodHarvestData);
        getWH.run(runner->getInternalScenario());
        coupleLog << mWoodHarvestData << endl;
        
        // Set data in the gcamoluc* arrays
        const Modeltime* modeltime = runner->getInternalScenario()->getModeltime();
        int row = 0;
        for(size_t i = 0; i < mLUCData.getArrayLength(); ++i) {
            gcamoluc[i] = luc[i];
            row++;
        }
        for(size_t i = 0; i < mWoodHarvestData.getArrayLength(); ++i) {
            gcamoluc[row] = mWoodHarvestData.getData()[i];
            row++;
        }
        
        // Downscale CO2 emissions
        coupleLog << "Downscaling CO2 emissions" << endl;
        EmissDownscale gcam2e3sm(aNumLon * aNumLat * 12); // Emissions data is monthly now
        gcam2e3sm.readSpatialData(aBaseCO2File, true, true, true);
        gcam2e3sm.downscaleCO2Emissions(aBaseCO2EmissSfc, co2[0]);
        if ( aWriteCO2 ) {
            // TODO: Set name of file based on case name?
            gcam2e3sm.writeSpatialData("./gridded_co2.txt", false);
        }
        // Set gcamoemis to the output of this
        gcamoemis = gcam2e3sm.getValueVector().data();
    }
}

void GCAM_E3SM_interface::setDensityGCAM(int *yyyymmdd, double *aELMArea, double *aELMLandFract, double *aELMPFTFract, double *aELMNPP, double *aELMHR,
                                         int aNumLon, int aNumLat, int aNumPFT, std::string aMappingFile, int aFirstCoupledYear, bool aReadScalars, bool aWriteScalars) {
    // Get year only of the current date
    int curryear = *yyyymmdd/10000;
    const Modeltime* modeltime = runner->getInternalScenario()->getModeltime();
    const int finalCalibrationPeriod = modeltime->getFinalCalibrationPeriod();
    const int finalCalibrationYear = modeltime->getper_to_yr(finalCalibrationPeriod);
    
    // Create scalar vectors
    // TODO: Find a better way to determine the length
    std::vector<int> scalarYears(17722);
    vector<std::string> scalarRegion(17722);
    vector<std::string> scalarLandTech(17722);
    vector<double> aboveScalarData(17722);
    vector<double> belowScalarData(17722);
    
    // Open the coupling log
    ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
    coupleLog.setLevel( ILogger::NOTICE );
    
    // Only set carbon densities during GCAM model years after the first coupled year
    if( modeltime->isModelYear( curryear ) && curryear >=  aFirstCoupledYear ) {
        coupleLog << "Setting carbon density in year: " << curryear << endl;
        CarbonScalers e3sm2gcam(aNumLon, aNumLat, aNumPFT);
        
        // Get scaler information
        if ( aReadScalars ) {
            coupleLog << "Reading scalars from file." << endl;
            e3sm2gcam.readScalers(yyyymmdd, scalarYears, scalarRegion, scalarLandTech, aboveScalarData);
        } else {
            coupleLog << "Calculating scalers from data." << endl;
            e3sm2gcam.readRegionalMappingData(aMappingFile);
            e3sm2gcam.calcScalers(yyyymmdd, aELMArea, aELMLandFract, aELMPFTFract, aELMNPP, aELMHR,
                                  scalarYears, scalarRegion, scalarLandTech, aboveScalarData, belowScalarData);
        }
        
        // Optional: write scaler information to a file
        // TODO: make the file name an input instead of hardcoded
        if( aWriteScalars ) {
            e3sm2gcam.writeScalers("./scalers.csv", scalarYears, scalarRegion, scalarLandTech, aboveScalarData, belowScalarData, 17722);
        }
        
        // TODO: What happens if there is no scalarData or if the elements are blank?
        SetDataHelper setScaler(scalarYears, scalarRegion, scalarLandTech, aboveScalarData, "world/region[+name]/sector/subsector/technology[+name]/period[+year]/yield-scaler");
        setScaler.run(runner->getInternalScenario());
    }
    
}

void GCAM_E3SM_interface::finalizeGCAM()
{
    Timer timer;
    // Initialize the timer.  Create an object of the Timer class.
    timer.start();
    ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
    coupleLog.setLevel( ILogger::NOTICE );
    coupleLog << "calling finalize" << endl;
    runner->printOutput(timer);
    timer.stop();
}
