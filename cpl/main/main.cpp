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
* \file main.cpp
* \brief This file runs GCAM as if it were coupled to E3SM. This is used for testing.
*
* \author Kate Calvin
*/

// include standard libraries
#include <iostream>
#include <fstream>
#include <string>
#include <memory>
#include <list>

// Include interface
#include "util/logger/include/ilogger.h"
#include "../include/GCAM_E3SM_interface.h"
#include "../include/aspatial_data.h"

using namespace std;

int main( ) {
    /* STEP 1: DEFINE CONTROL VARIABLES WITH DEFAULTS
               THESE WILL BE OVERWRITTEN BY THE NAMELIST,
               BUT NEED TO DEFINE NAME AND TYPE.
     */
    // Current year
    int year = 1990;
    int* YEAR = &year;
    
    // Define base control variables.
    // In fully coupled mode, these are defined in an E3SM namelist.
    std::string CASE_NAME = "Impacts";
    std::string GCAM_CONFIG = "configuration.xml";
    std::string BASE_CO2_SURFACE_FILE = "../cpl/data/gcam_CO2-em-anthro_0.9x1.25_201401-201412_c20200406.txt";
    std::string BASE_CO2_AIRCRAFT_FILE = "../cpl/data/gcam_CO2-em-AIR-anthro_0.9x1.25_201401-201412_c20200427.txt";
    std::string GCAM2ELM_CO2_MAPPING_FILE = "../cpl/mappings/co2.xml";
    std::string GCAM2ELM_LUC_MAPPING_FILE = "../cpl/mappings/luc.xml";
    std::string GCAM2ELM_WOODHARVEST_MAPPING_FILE = "../cpl/mappings/woodharvest.xml";
    std::string ELM2GCAM_MAPPING_FILE = "../cpl/mappings/elm0.9x1.25togcam_mapping.csv";
    std::string BASE_NPP_FILE = "../cpl/data/base_npp_mean_pft.txt";
    std::string BASE_HR_FILE = "../cpl/data/base_hr_mean_pft.txt";
    std::string BASE_PFT_FILE = "../cpl/data/base_pft_wt.txt";
    int readScalars = 0;
    int* READ_SCALARS = &readScalars; // If 0, scalars are calculated from NPP/HR
    bool READ_ELM_FROM_FILE = true; // If FALSE, ELM data (NPP, HR, Area, PFT weight) are passed from E3SM.
    int writeCO2 = 1;
    int* WRITE_CO2 = &writeCO2; // If 1, gridded CO2 emissions will be written to a file (in addition to passed in code).
    int writeScalars = 1;
    int* WRITE_SCALARS = &writeScalars; // If 1, scalars will be written to a file.
    bool RUN_FULL_SCENARIO = false; // If TRUE, will loop over all periods. This is used for testing/offline scenario runs only.
    
    // Define coupling control variables
    // These booleans define what is passed between GCAM & E3SM.
    bool ELM_IAC_CARBON_SCALING = true; // If TRUE, changes in land productivity from ELM are used in GCAM.
    bool IAC_EAM_CO2_EMISSIONS = true; // If TRUE, energy system CO2 is passed from GCAM to EAM.
    int firstCoupledYear = 2016;
    int* FIRST_COUPLED_YEAR = &firstCoupledYear; // First year to include feedbacks from E3SM in GCAM.
    double baseCo2EmissSurface = 9663.0297;
    double* BASE_CO2EMISS_SURFACE = &baseCo2EmissSurface; // Global surface CO2 emissions in the base year. This should be GCAM's emissions in the year of the BASE_CO2_FILE
    double baseCo2EmissAir = 102.157;
    double* BASE_CO2EMISS_AIRCRAFT = &baseCo2EmissAir; // Global aircraft CO2 emissions in the base year.
    
    // Define size control variables
    // These integers define the length of the various arrays used in the coupling
    int numLat = 192;
    int* NUM_LAT = &numLat; // Number of horizontal grid cells
    int numLon = 288;
    int* NUM_LON = &numLon; // Number of vertical grid cells
    int numPFT = 17;
    int* NUM_PFT = &numPFT; // Number of PFTs in ELM
    int numGEReg = 32;
    int* NUM_GCAM_ENERGY_REGIONS = &numGEReg;
    int numGLReg = 392;
    int* NUM_GCAM_LAND_REGIONS = &numGLReg;
    int numLT = 9;
    int* NUM_IAC2ELM_LANDTYPES = &numLT;
    int numES = 2;
    int* NUM_EMISS_SECTORS = &numES;
    int numER = 1;
    int* NUM_EMISS_REGIONS = &numER;
    int numEG = 1;
    int* NUM_EMISS_GASES = &numEG;
    
    /*
     STEP 2: READ NAMELIST
     */
    ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
    coupleLog.setLevel( ILogger::ERROR );
    ifstream namelist("user_nl_gcam");
    if (!namelist.is_open())
    {
        coupleLog << "Namelist not found: user_nl_gcam" << endl;
        exit(EXIT_FAILURE);
    }
    string str;
    getline(namelist, str); // skip the first line
    while (getline(namelist, str))
    {
        istringstream iss(str);
        string name;
        string value;

        getline(iss, name, ' ');
        getline(iss, value, ' '); // Throw away the equals sign
        getline(iss, value, ' ');
        if ( name == "CASE_NAME" ) {
            CASE_NAME = value;
        } else if ( name == "GCAM_CONFIG" ) {
            GCAM_CONFIG = value;
        } else if ( name == "BASE_CO2_SURFACE_FILE" ) {
            BASE_CO2_SURFACE_FILE = value;
        } else if ( name == "BASE_CO2_AIRCRAFT_FILE" ) {
            BASE_CO2_AIRCRAFT_FILE = value;
        } else if ( name == "GCAM2ELM_CO2_MAPPING_FILE" ) {
            GCAM2ELM_CO2_MAPPING_FILE = value;
        } else if ( name == "GCAM2ELM_LUC_MAPPING_FILE" ) {
            GCAM2ELM_LUC_MAPPING_FILE = value;
        } else if ( name == "GCAM2ELM_WOODHARVEST_MAPPING_FILE" ) {
            GCAM2ELM_WOODHARVEST_MAPPING_FILE = value;
        } else if ( name == "ELM2GCAM_MAPPING_FILE" ) {
            ELM2GCAM_MAPPING_FILE = value;
        } else if ( name == "BASE_NPP_FILE" ) {
            BASE_NPP_FILE = value;
        } else if ( name == "BASE_HR_FILE" ) {
            BASE_HR_FILE = value;
        } else if ( name == "BASE_PFT_FILE" ) {
            BASE_PFT_FILE = value;
        } else if ( name == "READ_SCALARS" ) {
            *READ_SCALARS = std::stoi(value);
        } else if ( name == "READ_ELM_FROM_FILE" ) {
            istringstream(value) >> std::boolalpha >> READ_ELM_FROM_FILE;
        } else if ( name == "WRITE_CO2" ) {
            *WRITE_CO2 = std::stoi(value);
        } else if ( name == "WRITE_SCALARS" ) {
            *WRITE_SCALARS = std::stoi(value);
        } else if ( name == "ELM_IAC_CARBON_SCALING" ) {
            istringstream(value) >> std::boolalpha >> ELM_IAC_CARBON_SCALING;
        } else if ( name == "IAC_EAM_CO2_EMISSIONS" ) {
            istringstream(value) >> std::boolalpha >> IAC_EAM_CO2_EMISSIONS;
        } else if ( name == "FIRST_COUPLED_YEAR" ) {
            *FIRST_COUPLED_YEAR = std::stoi(value);
        } else if ( name == "YEAR" ) {
            *YEAR = std::stoi(value);
        } else if ( name == "NUM_LAT" ) {
            *NUM_LAT = std::stoi(value);
        } else if ( name == "NUM_LON" ) {
            *NUM_LON = std::stoi(value);
        } else if ( name == "NUM_PFT" ) {
            *NUM_PFT = std::stoi(value);
        } else if ( name == "NUM_GCAM_ENERGY_REGIONS" ) {
            *NUM_GCAM_ENERGY_REGIONS = std::stoi(value);
        } else if ( name == "NUM_GCAM_LAND_REGIONS" ) {
            *NUM_GCAM_LAND_REGIONS = std::stoi(value);
        } else if ( name == "NUM_IAC2ELM_LANDTYPES" ) {
            *NUM_IAC2ELM_LANDTYPES = std::stoi(value);
        } else if ( name == "NUM_EMISS_SECTORS" ) {
            *NUM_EMISS_SECTORS = std::stoi(value);
        } else if ( name == "NUM_EMISS_REGIONS" ) {
            *NUM_EMISS_REGIONS = std::stoi(value);
        } else if ( name == "NUM_EMISS_GASES" ) {
            *NUM_EMISS_GASES = std::stoi(value);
        } else if ( name == "BASE_CO2EMISS_SURFACE" ) {
            *BASE_CO2EMISS_SURFACE = std::stod(value);
        } else if ( name == "BASE_CO2EMISS_AIRCRAFT" ) {
            *BASE_CO2EMISS_AIRCRAFT = std::stod(value);
        } else if ( name == "RUN_FULL_SCENARIO" ) {
            istringstream(value) >> std::boolalpha >> RUN_FULL_SCENARIO;
        }else {
            coupleLog.setLevel( ILogger::ERROR );
            coupleLog << "Invalid Namelist Variable" << endl;
        }
    
        // Print to coupler log.
        coupleLog.setLevel( ILogger::NOTICE );
        coupleLog << name << " = " << value << endl;
    }
    
    /*
     STEP 3: INITIALIZE INTERFACE AND INTERFACE VARIABLES
     */
    // Initialize Interface
    GCAM_E3SM_interface *p_obj;
    p_obj = new GCAM_E3SM_interface();
    
    // Initialize GCAM
    p_obj->initGCAM(CASE_NAME, GCAM_CONFIG, GCAM2ELM_CO2_MAPPING_FILE, GCAM2ELM_LUC_MAPPING_FILE, GCAM2ELM_WOODHARVEST_MAPPING_FILE);
    
    // Set up data structures that will be passed to runGCAM
    // In fully coupled mode, these are allocated by E3SM
    double *gcamiarea = new double [(*NUM_LAT) * (*NUM_LON)]();
    double *gcamipftfract = new double [(*NUM_LAT) * (*NUM_LON) * (*NUM_PFT)]();
    double *gcaminpp = new double [(*NUM_LAT) * (*NUM_LON) * (*NUM_PFT)]();
    double *gcamihr = new double [(*NUM_LAT) * (*NUM_LON) * (*NUM_PFT)]();
    double *gcamoluc = new double [(*NUM_GCAM_LAND_REGIONS) * (*NUM_IAC2ELM_LANDTYPES)]();
    double *gcamoemiss = new double [(*NUM_EMISS_SECTORS) * (*NUM_EMISS_REGIONS) * (*NUM_EMISS_GASES)](); // Emissions by sector, gas, and region (not gridded)
    double *gcamoco2sfcjan = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2sfcfeb = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2sfcmar = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2sfcapr = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2sfcmay = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2sfcjun = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2sfcjul = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2sfcaug = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2sfcsep = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2sfcoct = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2sfcnov = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2sfcdec = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airhijan = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airhifeb = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airhimar = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airhiapr = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airhimay = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airhijun = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airhijul = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airhiaug = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airhisep = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airhioct = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airhinov = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airhidec = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airlojan = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airlofeb = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airlomar = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airloapr = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airlomay = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airlojun = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airlojul = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airloaug = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airlosep = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airlooct = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airlonov = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    double *gcamoco2airlodec = new double [(*NUM_LAT) * (*NUM_LON)](); // Emissions data is monthly
    
    /*
     STEP 4: RUN GCAM
     */
    if (RUN_FULL_SCENARIO) {
        for ( int y = 1970; y < 2101; y++ ){
            int ymd = y * 10000;
            int *yyyymmdd = &ymd;
            
            // If coupling is active, then set carbon density
            if ( ELM_IAC_CARBON_SCALING ) {
                coupleLog << "E3SM-GCAM: Carbon scaling on" << endl;
                if ( READ_ELM_FROM_FILE ) {
                    // Read the ELM data from a file and then pass it to setDensityGCAM below
                    // These are currently the new base files, can make others from a different year

                    // Read in average NPP
                    ASpatialData tempPFTData((*NUM_LAT) * (*NUM_LON) * (*NUM_PFT));
                    tempPFTData.readSpatialDataCSV("../cpl/data/base_f09_maxEach_2014-2014_npp.csv", true, true, false, gcaminpp);
                    
                    // Read in average HR
                    tempPFTData.readSpatialDataCSV("../cpl/data/base_f09_maxEach_2014-2014_hr.csv", true, true, false, gcamihr);
                    
                    // Read in PFT weight in grid cell
                    tempPFTData.readSpatialDataCSV("../cpl/data/base_f09_maxEach_2014-2014_pft_wt.csv", true, true, false, gcamipftfract);
                    
                    // Read in area of grid cell
                    ASpatialData tempData((*NUM_LAT) * (*NUM_LON));
                    tempData.readSpatialDataCSV("../cpl/data/base_f09_cell_area.csv", true, false, false, gcamiarea);
                }
                p_obj->setDensityGCAM(yyyymmdd, gcamiarea, gcamipftfract, gcaminpp, gcamihr,
                                      NUM_LON, NUM_LAT, NUM_PFT, ELM2GCAM_MAPPING_FILE, FIRST_COUPLED_YEAR, READ_SCALARS, WRITE_SCALARS, ELM_IAC_CARBON_SCALING, BASE_NPP_FILE, BASE_HR_FILE, BASE_PFT_FILE);
            }
            
            // Run model
            p_obj->runGCAM(yyyymmdd, gcamoluc, gcamoemiss);
            
            // TODO: Will we ever want to downscale emissions in this mode?
        }
    } else {
        
        // Run GCAM
        cout << "Running E3SM Year: " << *YEAR << endl;
        int ymd = *YEAR * 10000;
        int *yyyymmdd = &ymd;
        
        // If coupling is active, then set carbon density
        if ( ELM_IAC_CARBON_SCALING ) {
            coupleLog << "E3SM-GCAM: Carbon scaling on" << endl;
            if ( READ_ELM_FROM_FILE ) {
                // Read the ELM data from a file and then pass it to setDensityGCAM below
                // These are currently the new base files, can make others from a different year
                
                // Read in average NPP
                ASpatialData tempPFTData((*NUM_LAT) * (*NUM_LON) * (*NUM_PFT));
                tempPFTData.readSpatialDataCSV("../cpl/data/base_f09_maxEach_2014-2014_npp.csv", true, true, false, gcaminpp);
                
                // Read in average HR
                tempPFTData.readSpatialDataCSV("../cpl/data/base_f09_maxEach_2014-2014_hr.csv", true, true, false, gcamihr);
                
                // Read in PFT weight in grid cell
                tempPFTData.readSpatialDataCSV("../cpl/data/base_f09_maxEach_2014-2014_pft_wt.csv", true, true, false, gcamipftfract);
                
                // Read in area of grid cell
                ASpatialData tempData((*NUM_LAT) * (*NUM_LON));
                tempData.readSpatialDataCSV("../cpl/data/base_f09_cell_area.csv", true, false, false, gcamiarea);
            }
            p_obj->setDensityGCAM(yyyymmdd, gcamiarea, gcamipftfract, gcaminpp, gcamihr,
                                  NUM_LON, NUM_LAT, NUM_PFT, ELM2GCAM_MAPPING_FILE, FIRST_COUPLED_YEAR, READ_SCALARS, WRITE_SCALARS, ELM_IAC_CARBON_SCALING, BASE_NPP_FILE, BASE_HR_FILE, BASE_PFT_FILE);
        }
        
        // Run model
        p_obj->runGCAM(yyyymmdd, gcamoluc, gcamoemiss);
        
        if( IAC_EAM_CO2_EMISSIONS ) {
            p_obj->downscaleEmissionsGCAM(gcamoemiss,
                                      gcamoco2sfcjan, gcamoco2sfcfeb, gcamoco2sfcmar, gcamoco2sfcapr,
                                      gcamoco2sfcmay, gcamoco2sfcjun, gcamoco2sfcjul, gcamoco2sfcaug,
                                      gcamoco2sfcsep, gcamoco2sfcoct, gcamoco2sfcnov, gcamoco2sfcdec,
                                      gcamoco2airlojan, gcamoco2airlofeb, gcamoco2airlomar, gcamoco2airloapr,
                                      gcamoco2airlomay, gcamoco2airlojun, gcamoco2airlojul, gcamoco2airloaug,
                                      gcamoco2airlosep, gcamoco2airlooct, gcamoco2airlonov, gcamoco2airlodec,
                                      gcamoco2airhijan, gcamoco2airhifeb, gcamoco2airhimar, gcamoco2airhiapr,
                                      gcamoco2airhimay, gcamoco2airhijun, gcamoco2airhijul, gcamoco2airhiaug,
                                      gcamoco2airhisep, gcamoco2airhioct, gcamoco2airhinov, gcamoco2airhidec,
                                      BASE_CO2_SURFACE_FILE, BASE_CO2EMISS_SURFACE, BASE_CO2_AIRCRAFT_FILE, BASE_CO2EMISS_AIRCRAFT,
                                      NUM_LON, NUM_LAT, WRITE_CO2, YEAR);
        }
        
    }
    /*
     STEP 5: FINALIZE AND CLEAN UP ALL VARIABLES
     */n
    // Finalize GCAM
    p_obj->finalizeGCAM();
    
    // Remove all arrays
    delete [] gcamiarea;
    delete [] gcamipftfract;
    delete [] gcaminpp;
    delete [] gcamihr;
    delete [] gcamoluc;
    delete [] gcamoemiss;
    delete [] gcamoco2sfcjan;
    delete [] gcamoco2sfcfeb;
    delete [] gcamoco2sfcmar;
    delete [] gcamoco2sfcapr;
    delete [] gcamoco2sfcmay;
    delete [] gcamoco2sfcjun;
    delete [] gcamoco2sfcjul;
    delete [] gcamoco2sfcaug;
    delete [] gcamoco2sfcsep;
    delete [] gcamoco2sfcoct;
    delete [] gcamoco2sfcnov;
    delete [] gcamoco2sfcdec;
    delete [] gcamoco2airhijan;
    delete [] gcamoco2airhifeb;
    delete [] gcamoco2airhimar;
    delete [] gcamoco2airhiapr;
    delete [] gcamoco2airhimay;
    delete [] gcamoco2airhijun;
    delete [] gcamoco2airhijul;
    delete [] gcamoco2airhiaug;
    delete [] gcamoco2airhisep;
    delete [] gcamoco2airhioct;
    delete [] gcamoco2airhinov;
    delete [] gcamoco2airhidec;
    delete [] gcamoco2airlojan;
    delete [] gcamoco2airlofeb;
    delete [] gcamoco2airlomar;
    delete [] gcamoco2airloapr;
    delete [] gcamoco2airlomay;
    delete [] gcamoco2airlojun;
    delete [] gcamoco2airlojul;
    delete [] gcamoco2airloaug;
    delete [] gcamoco2airlosep;
    delete [] gcamoco2airlooct;
    delete [] gcamoco2airlonov;
    delete [] gcamoco2airlodec;
    
    // Finalize Interface
    delete p_obj;

    // Return exit code based on whether the model succeeded (Non-zero is failure by convention).
    return 0;
}
