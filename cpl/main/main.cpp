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
#include "../include/GCAM_E3SM_interface.h"
#include "../include/aspatial_data.h"

int main( ) {
    // Define base control variables.
    // In fully coupled mode, these are defined in an E3SM namelist.
    std::string CASE_NAME = "Impacts";
    std::string GCAM_CONFIG = "configuration.xml";
    std::string BASE_CO2_FILE = "../cpl/data/gcam_CO2-em-anthro_0.9x1.25_201401-201412_c20200406.txt";
    std::string GCAM2ELM_CO2_MAPPING_FILE = "../cpl/mappings/co2.xml";
    std::string GCAM2ELM_LUC_MAPPING_FILE = "../cpl/mappings/luc.xml";
    std::string GCAM2ELM_WOODHARVEST_MAPPING_FILE = "../cpl/mappings/woodharvest.xml";
    std::string ELM2GCAM_MAPPING_FILE = "../cpl/mappings/elm0.9x1.25togcam_mapping.csv";
    bool READ_SCALARS = false; // If FALSE, scalars are calculated from NPP/HR
    bool READ_ELM_FROM_FILE = true; // If FALSE, ELM data (NPP, HR, Area, PFT weight) are passed from E3SM.
    bool WRITE_CO2 = true; // If TRUE, gridded CO2 emissions will be written to a file (in addition to passed in code).
    bool WRITE_SCALARS = true; // If TRUE, scalars will be written to a file.
    
    // Define coupling control variables
    // These booleans define what is passed between GCAM & E3SM.
    bool ELM_IAC_CARBON_SCALING = true; // If TRUE, changes in land productivity from ELM are used in GCAM.
    bool IAC_ELM_CO2_EMISSIONS = true; // If TRUE, energy system CO2 is passed from GCAM to EAM.
    int FIRST_COUPLED_YEAR = 2016; // First year to include feedbacks from E3SM in GCAM.
    double BASE_CO2EMISS_SURFACE = 9725.7847; // Global surface CO2 emissions in the base year. This should be GCAM's emissions in the year of the BASE_CO2_FILE
    
    // Define size control variables
    // These integers define the length of the various arrays used in the coupling
    int NUM_LAT = 192; // Number of horizontal grid cells
    int NUM_LON = 288; // Number of vertical grid cells
    int NUM_PFT = 16; // Number of PFTs in ELM
    int NUM_GCAM_ENERGY_REGIONS = 32;
    int NUM_GCAM_LAND_REGIONS = 391;
    int NUM_IAC2ELM_LANDTYPES = 9;
    int NUM_EMISS_SECTORS = 1;
    int NUM_EMISS_REGIONS = 1;
    int NUM_EMISS_GASES = 1;
    
    // Initialize Interface
    GCAM_E3SM_interface *p_obj;
    p_obj = new GCAM_E3SM_interface();
    
    // Initialize GCAM
    p_obj->initGCAM(CASE_NAME, GCAM_CONFIG, GCAM2ELM_CO2_MAPPING_FILE, GCAM2ELM_LUC_MAPPING_FILE, GCAM2ELM_WOODHARVEST_MAPPING_FILE);
    
    // Set up data structures that will be passed to runGCAM
    // In fully coupled mode, these are allocated by E3SM
    double *gcamiarea = new double [NUM_LAT * NUM_LON]();
    double *gcamilfract = new double [NUM_LAT * NUM_LON]();
    double *gcamipftfract = new double [NUM_LAT * NUM_LON * NUM_PFT]();
    double *gcaminpp = new double [NUM_LAT * NUM_LON * NUM_PFT]();
    double *gcamihr = new double [NUM_LAT * NUM_LON * NUM_PFT]();
    double *gcamoluc = new double [NUM_GCAM_LAND_REGIONS * NUM_IAC2ELM_LANDTYPES]();
    double *gcamoemiss = new double [NUM_EMISS_SECTORS * NUM_EMISS_REGIONS * NUM_EMISS_GASES]();
    double *gcamoco2sfcjan = new double [NUM_LAT * NUM_LON](); // Emissions data is monthly
    double *gcamoco2sfcfeb = new double [NUM_LAT * NUM_LON](); // Emissions data is monthly
    double *gcamoco2sfcmar = new double [NUM_LAT * NUM_LON](); // Emissions data is monthly
    double *gcamoco2sfcapr = new double [NUM_LAT * NUM_LON](); // Emissions data is monthly
    double *gcamoco2sfcmay = new double [NUM_LAT * NUM_LON](); // Emissions data is monthly
    double *gcamoco2sfcjun = new double [NUM_LAT * NUM_LON](); // Emissions data is monthly
    double *gcamoco2sfcjul = new double [NUM_LAT * NUM_LON](); // Emissions data is monthly
    double *gcamoco2sfcaug = new double [NUM_LAT * NUM_LON](); // Emissions data is monthly
    double *gcamoco2sfcsep = new double [NUM_LAT * NUM_LON](); // Emissions data is monthly
    double *gcamoco2sfcoct = new double [NUM_LAT * NUM_LON](); // Emissions data is monthly
    double *gcamoco2sfcnov = new double [NUM_LAT * NUM_LON](); // Emissions data is monthly
    double *gcamoco2sfcdec = new double [NUM_LAT * NUM_LON](); // Emissions data is monthly
    
    // Run GCAM
    for( int yr = 1975; yr < 2025; yr++ ){
        // Set up years
        int ymd = yr * 10000;
        int *yyyymmdd = &ymd;
        
        // If coupling is active, then set carbon density
        if ( ELM_IAC_CARBON_SCALING ) {
            cout << "E3SM-GCAM: Carbon scaling on" << endl;
            if ( READ_ELM_FROM_FILE ) {
                // Read the ELM data from a file and then pass it to setDensityGCAM below
                // Read in average NPP
                ASpatialData tempPFTData(NUM_LAT * NUM_LON * NUM_PFT);
                tempPFTData.readSpatialData("../cpl/data/npp_mean_pft.txt", true, true, false, gcaminpp);
                
                // Read in average HR
                tempPFTData.readSpatialData("../cpl/data/hr_mean_pft.txt", true, true, false, gcamihr);
                
                // Read in PFT weight in grid cell
                tempPFTData.readSpatialData("../cpl/data/pft_wt.txt", true, true, false, gcamipftfract);
                
                // Read in area of grid cell
                ASpatialData tempData(NUM_LAT * NUM_LON);
                tempData.readSpatialData("../cpl/data/area.txt", true, false, false, gcamiarea);
                
                // Read in area of grid cell
                tempData.readSpatialData("../cpl/data/landfrac.txt", true, false, false, gcamilfract);
            }
            p_obj->setDensityGCAM(yyyymmdd, gcamiarea, gcamilfract, gcamipftfract, gcaminpp, gcamihr,
                                  NUM_LON, NUM_LAT, NUM_PFT, ELM2GCAM_MAPPING_FILE, FIRST_COUPLED_YEAR, READ_SCALARS, WRITE_SCALARS);
        }
        
        // Run model
        p_obj->runGCAM(yyyymmdd, gcamoluc, gcamoemiss, NUM_LON, NUM_LAT);
        
        p_obj->downscaleEmissionsGCAM(gcamoemiss, gcamoco2sfcjan, gcamoco2sfcfeb, gcamoco2sfcmar, gcamoco2sfcapr,
                       gcamoco2sfcmay, gcamoco2sfcjun, gcamoco2sfcjul, gcamoco2sfcaug, gcamoco2sfcsep,
                       gcamoco2sfcoct, gcamoco2sfcnov, gcamoco2sfcdec, BASE_CO2_FILE, BASE_CO2EMISS_SURFACE,
                       NUM_LON, NUM_LAT, WRITE_CO2);

    }
    
    // Finalize GCAM
    p_obj->finalizeGCAM();
    
    // Remove all arrays
    delete [] gcamiarea;
    delete [] gcamilfract;
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
    
    // Finalize Interface
    delete p_obj;

    // Return exit code based on whether the model succeeded (Non-zero is failure by convention).
    return 0;
}

