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

int main( ) {
    // Initialize Interface
    GCAM_E3SM_interface *p_obj;
    p_obj = new GCAM_E3SM_interface();
    
    // Initialize GCAM
    p_obj->initGCAM();
    
    // TEMPORARY - Set up data structures that will be passed to runGCAM
    int numEnergyRegions = 32;
    int numLandRegions = 390;
    int numLandTypes = 8;
    int numModelPeriods = 22;
    double *gcami = new double [384];
    double *gcamo = new double [numLandRegions * numLandTypes * numModelPeriods]();
    double *gcamoemis = new double [numEnergyRegions * numModelPeriods]();
    int *temp = (int *)(0); // KVC - Temporarily using this for all values additional integer pointers
    
    // Run GCAM
    for( int yr = 1975; yr < 2025; yr++ ){
        // Set up years
        int ymd = yr * 10000;
        int *yyyymmdd = &ymd;
        
        // Set carbon density
        p_obj->setDensityGCAM(yyyymmdd, temp, gcami, temp, temp);
        
        // Run model
        p_obj->runGCAM(yyyymmdd, temp, gcami, temp, temp,
                       gcamo, temp, temp,
                       gcamoemis, temp, temp,
                       temp, temp, temp, temp);
        
        // Original call (Note lots of these arguments need to be fixed)
        // runcGCAM(ymd,tod,gcami,size(gcami,dim=1),size(gcami,dim=2),gcamo,size(gcamo,dim=1),size(gcamo,dim=2),gcamoemis,size(gcamoemis,dim=1),size(gcamoemis,dim=2), cdata%i(iac_cdatai_gcam_yr1),cdata%i(iac_cdatai_gcam_yr2),cdata%l(iac_cdatal_sneakermode),cdata%l(iac_cdatal_write_rest))
    }
    
    // Finalize GCAM
    p_obj->finalizeGCAM();
    
    // Finalize Interface
    delete p_obj;

    // Return exit code based on whether the model succeeded(Non-zero is failure by convention).
    return 0;
}

