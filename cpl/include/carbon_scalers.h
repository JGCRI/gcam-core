#ifndef __CARBON_SCALERS__
#define __CARBON_SCALERS__

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

// include standard libraries
#include <iostream>
#include <fstream>
#include <string>
#include <memory>
#include <list>
#include <vector>

#include "../include/aspatial_data.h"

class CarbonScalers : public ASpatialData {
public:
    CarbonScalers(int aSize);
    ~CarbonScalers();
    void calcScalers();
    void readAllSpatialData();
private:
    // TODO: Is there a better way of doing this?
    std::vector<double> mNPPVector;
    std::vector<int> mNPPLatVector;
    std::vector<double> mNPPLonVector;
    std::vector<int> mNPPIDVector;
    std::vector<double> mPFTFractVector;
    std::vector<int> mPFTFractLatVector;
    std::vector<double> mPFTFractLonVector;
    std::vector<int> mPFTFractIDVector;
    std::vector<double> mAreaVector;
    std::vector<int> mAreaLatVector;
    std::vector<double> mAreaLonVector;
    std::vector<double> mLandFractVector;
    std::vector<int> mLandFractLatVector;
    std::vector<double> mLandFractLonVector;
};

#endif // __CARBON_SCALERS__
