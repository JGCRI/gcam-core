/*! 
 * \file GCAM_E3SM_interface.cpp
 * \brief E3SM gcam driver source file.
 * \author Kate Calvin
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

void GCAM_E3SM_interface::initGCAM(std::string aCaseName, std::string aGCAMConfig, std::string aGCAM2ELMCO2Map, std::string aGCAM2ELMLUCMap,
                                   std::string aGCAM2ELMWHMap, std::string aGCAM2ELMCDENMap)
{
    // identify default file names for control input and logging controls
    string configurationArg = aGCAMConfig;
    string loggerFactoryArg = "log_conf.xml";
    
    // Add OS dependent prefixes to the arguments.
    const string configurationFileName = configurationArg;
    const string loggerFileName = loggerFactoryArg;
   
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
                                   int *aNumLon, int *aNumLat, int *aNumPFT, std::string aMappingFile, int *aFirstCoupledYear, bool aReadScalars,
                                   bool aWriteScalars, bool aScaleAgYield, bool aScaleCarbon,
                                   std::string aBaseNPPFileName, std::string aBaseHRFileName, std::string aBasePFTWtFileName, bool aRestartRun )
{
    int z, p, num_it, spinup;
    int row, lurow, r, l;
    ofstream oFile;
    Timer timer;

    // Get year only of the current date
    const Modeltime* modeltime = runner->getInternalScenario()->getModeltime();
    // Note that GCAM runs one period ahead of E3SM. We make that adjustment here
    int e3smYear = *yyyymmdd/10000;
    // If the e3smYear is not a GCAM model year, then GCAM will automatically run ahead
    int gcamPeriod = modeltime->getyr_to_per( e3smYear );

    ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
    coupleLog.setLevel( ILogger::NOTICE );

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
        } else {
            num_it = 1;
            spinup = 0;
        }

	for( z = 0; z < num_it; z++) {

            // If the e3smYear is a GCAM model period, then we need to increment GCAM's model period
            // unless this is the spinup loop
            if ( modeltime->isModelYear( e3smYear ) && spinup == 0) {
                gcamPeriod = gcamPeriod + 1;
            }

            int gcamYear = modeltime->getper_to_yr( gcamPeriod );
    
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
            if (aRestartRun) {
                coupleLog << "Restart run: first running through period " << gcamPeriod-1 << endl;
                timer.start();
                success = runner->runScenarios( gcamPeriod-1, true, timer );
                timer.stop();
            }

            // now set scalars for the current period
            setLandProductivityScalingGCAM(yyyymmdd, aELMArea, aELMPFTFract, aELMNPP, aELMHR,
                            aNumLon, aNumLat, aNumPFT, aMappingFile, aFirstCoupledYear, aReadScalars, aWriteScalars,
                            aScaleAgYield, aScaleCarbon, aBaseNPPFileName, aBaseHRFileName, aBasePFTWtFileName);

            // now run the current period
            //Timer timer;
        
            coupleLog << "Running GCAM for year " << gcamYear;
            coupleLog << ", calculating period = " << gcamPeriod << endl;
        
            coupleLog.precision(20);
       
            // Initialize the timer.  Create an object of the Timer class.
            timer.start();
        
            // Run this GCAM period
            success = runner->runScenarios( gcamPeriod, true, timer );
        
            // Stop the timer
            timer.stop();
        
            coupleLog << "Getting CO2 Emissions" << endl;
            double *co2 = mCO2EmissData.getData();
            // be sure to reset any data set previously
            fill(co2, co2+mCO2EmissData.getArrayLength(), 0.0);
            GetDataHelper getCo2("world/region[+NamedFilter,MatchesAny]/sector[+NamedFilter,MatchesAny]//ghg[NamedFilter,StringEquals,CO2]/emissions[+YearFilter,IntEquals,"+util::toString(gcamYear)+"]", mCO2EmissData);
            getCo2.run(runner->getInternalScenario());
            std::copy(co2, co2+mCO2EmissData.getArrayLength(), gcamoemiss);
            coupleLog << mCO2EmissData << endl;

            // write the 2015 CO2 to a table
            // this is the actual co2 base file, but don't use the namelist value
            // write this to hardcoded name within run directory
            // this eliminates overwrite of the standard baseline file
            if (spinup == 1) {
                std::string co2_oname = "gcam_co2_2015_base_file.csv";
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

            if (spinup == 1) {
                std::string luwh_oname = "gcam_lu_wh_2015_base_file.csv";
                oFile.open(luwh_oname);
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
                    gcamoluc[row] = luc[lurow];
                    if (spinup == 1) {
                        oFile << r+1 << "," << l+1 << "," << gcamYear << "," << gcamoluc[row] << endl;
                    }
                    lurow++;
                    row++;
                }
                // Convert to tC and set wood harvest data to output vector
                // this is the factor that GCAM uses; it is an intermediate value
                // conversion from m^3 of biomass to MgC (0.250 tonnes (Mg) C per m^3))
                gcamoluc[row] = mWoodHarvestData.getData()[r] * 250000000;
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

    } // end if valid year
        
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
                          cdensityLandTech.push_back(scalarLandTech[i] + "_" +
                              (*mAGCDensityData.getColumn("water")).mInOrderOutputNames[water_ind] + "_" + (*mAGCDensityData.getColumn("mgmt")).mInOrderOutputNames[mgmt_ind] );

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
                                                 std::string aBaseCO2GcamFileName, std::string aBaseCO2SfcFile, std::string aBaseCO2AirFile,
                                                 int *aNumReg, int *aNumSector, int *aNumLon, int *aNumLat, bool aWriteCO2, int *aCurrYear) {

    int r, s, row;
    std::vector<double> gcamoSfcEmissVector((*aNumReg),0);
    std::vector<double> gcamoAirEmissVector((*aNumReg),0);

    // separate gcamoemiss into surface and aircraft sector vectors
    row = 0;
    for( r=0; r<(*aNumReg); r++ ) {
        for( s=0; s<(*aNumSector); s++) {
            if(s==0){
                gcamoSfcEmissVector[r]=gcamoemiss[row];
            } else if(s==1){
                gcamoAirEmissVector[r]=gcamoemiss[row];
                row++;
            }
        }
    }

    // Downscale surface CO2 emissions
    ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
    coupleLog.setLevel( ILogger::NOTICE );
    coupleLog << "Downscaling CO2 emissions" << endl;
    EmissDownscale surfaceCO2((*aNumLon) * (*aNumLat) * 12, (*aNumReg), (*aNumSector)); // Emissions data is monthly now
    // Read in the GCAM regionXsector base CO2 data
    surfaceCO2.readBaseYearCO2Data(aBaseCO2GcamFileName);
    // read in the gridded eam baseline co2 data
    surfaceCO2.readSpatialData(aBaseCO2SfcFile, true, true, false);
    surfaceCO2.downscaleCO2Emissions("surface", gcamoSfcEmissVector);
    // These regions are in order of the output regions in co2.xml 
    for( r=0; r<(*aNumReg); r++ ) {
        coupleLog << "Diagnostics: Regional surface CO2 Emissions (PgC) in " << *aCurrYear << endl;
        coupleLog << "Region " << r+1  << " = " << gcamoSfcEmissVector[r] << endl;
    }

    if ( aWriteCO2 ) {
        // TODO: Set name of file based on case name?
        string fNameSfc = "./gridded_co2_sfc" + std::to_string(*aCurrYear) + ".txt";
        surfaceCO2.writeSpatialData(fNameSfc, false);
    }
    
    // Set the gcamoco2 monthly vector data to the output of this
    surfaceCO2.separateMonthlyEmissions(gcamoco2sfcjan, gcamoco2sfcfeb, gcamoco2sfcmar, gcamoco2sfcapr,
                                       gcamoco2sfcmay, gcamoco2sfcjun, gcamoco2sfcjul, gcamoco2sfcaug,
                                       gcamoco2sfcsep, gcamoco2sfcoct, gcamoco2sfcnov, gcamoco2sfcdec, *aNumLon, *aNumLat);
    
    // Emissions data is monthly now; we're using two different height levels for aircraft 
    EmissDownscale aircraftCO2((*aNumLon) * (*aNumLat) * 12 * 2, (*aNumReg), (*aNumSector));
    // Read in the GCAM regionXsector base CO2 data
    aircraftCO2.readBaseYearCO2Data(aBaseCO2GcamFileName);
    // read in the gridded eam baseline co2 data
    aircraftCO2.readSpatialData(aBaseCO2AirFile, true, true, false);
    aircraftCO2.downscaleCO2Emissions("aircraft", gcamoAirEmissVector);
    // These regions are in order of the output regions in co2.xml
    for( r=0; r<(*aNumReg); r++ ) {
        coupleLog << "Diagnostics: Regional aircraft CO2 Emissions (PgC) in " << *aCurrYear << endl;
        coupleLog << "Region " << r+1  << " = " << gcamoAirEmissVector[r] << endl;
    }

    if ( aWriteCO2 ) {
        // TODO: Set name of file based on case name?
        string fNameAir = "./gridded_co2_air_" + std::to_string(*aCurrYear) + ".txt";
        aircraftCO2.writeSpatialData(fNameAir, false);
    }
    
    // Set the gcamoco2 data to the output of this
    aircraftCO2.separateMonthlyEmissionsWithVertical( gcamoco2airlojan, gcamoco2airlofeb, gcamoco2airlomar, gcamoco2airloapr,
                                         gcamoco2airlomay, gcamoco2airlojun, gcamoco2airlojul, gcamoco2airloaug,
                                         gcamoco2airlosep, gcamoco2airlooct, gcamoco2airlonov, gcamoco2airlodec,
                                         gcamoco2airhijan, gcamoco2airhifeb, gcamoco2airhimar, gcamoco2airhiapr,
                                         gcamoco2airhimay, gcamoco2airhijun, gcamoco2airhijul, gcamoco2airhiaug,
                                         gcamoco2airhisep, gcamoco2airhioct, gcamoco2airhinov, gcamoco2airhidec,
                                         *aNumLon, *aNumLat);
    
    
    
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
