#ifndef __EMISS_DOWNSCALE__
#define __EMISS_DOWNSCALE__

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

class EmissDownscale : public ASpatialData {
public:
    EmissDownscale(int aSize);
    ~EmissDownscale();
    // TODO: Eventually these will need to be vectors of regional emissions instead of global totals
    void downscaleCO2Emissions(double aBaseYearEmissions, double aCurrYearEmissions);
    void separateMonthlyEmissions(double *gcamoco2sfcjan, double *gcamoco2sfcfeb, double *gcamoco2sfcmar,
                                  double *gcamoco2sfcapr, double *gcamoco2sfcmay, double *gcamoco2sfcjun,
                                  double *gcamoco2sfcjul, double *gcamoco2sfcaug, double *gcamoco2sfcsep,
                                  double *gcamoco2sfcoct, double *gcamoco2sfcnov, double *gcamoco2sfcdec,
                                  int aNumLon, int aNumLat);
    void separateMonthlyEmissionsWithVertical(double *gcamoco2airlojan, double *gcamoco2airlofeb, double *gcamoco2airlomar,
                                                              double *gcamoco2airloapr, double *gcamoco2airlomay, double *gcamoco2airlojun,
                                                              double *gcamoco2airlojul, double *gcamoco2airloaug, double *gcamoco2airlosep,
                                                              double *gcamoco2airlooct, double *gcamoco2airlonov, double *gcamoco2airlodec,
                                                              double *gcamoco2airhijan, double *gcamoco2airhifeb, double *gcamoco2airhimar,
                                                              double *gcamoco2airhiapr, double *gcamoco2airhimay, double *gcamoco2airhijun,
                                                              double *gcamoco2airhijul, double *gcamoco2airhiaug, double *gcamoco2airhisep,
                                                              double *gcamoco2airhioct, double *gcamoco2airhinov, double *gcamoco2airhidec,
                                                              int aNumLon, int aNumLat);
private:
    std::vector<double> mBaseYearEmissVector;
    std::vector<double> mCurrYearEmissVector;
};

#endif // __EMISS_DOWNSCALE__
