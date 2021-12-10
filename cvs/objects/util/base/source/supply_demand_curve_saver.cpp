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
* \file supply_demand_curve_saver.cpp
* \ingroup Objects
* \brief SupplyDemandCurveSaver class source file.
* \author Rich Plevin
*/

#include <cassert>
#include <vector>
#include <iostream>
#include "containers/include/imodel_feedback_calc.h"
#include "containers/include/scenario.h"
#include "containers/include/world.h"
#include "solution/util/include/solution_info_set.h"
#include "solution/util/include/solution_info.h"
#include "solution/util/include/solvable_solution_info_filter.h"
#include "solution/util/include/solution_info_param_parser.h"
#include "util/base/include/auto_file.h"
#include "util/base/include/definitions.h"
#include "util/base/include/supply_demand_curve.h"
#include "util/base/include/supply_demand_curve_saver.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/market.h"
#include "marketplace/include/marketplace.h"

using namespace std;

SupplyDemandCurveSaver::SupplyDemandCurveSaver() : mIsPricesRelative(true) {
}

// First open uses "out" mode to overwrite; subsequent calls append
ios_base::openmode SupplyDemandCurveSaver::mOpenMode = ios_base::out;

SupplyDemandCurveSaver::~SupplyDemandCurveSaver() {
}

const string& SupplyDemandCurveSaver::getXMLNameStatic() {
  // This is the string you will use to refer to this object
  // in input files.
  const static string XML_NAME = "supply-demand-curve-saver";
  return XML_NAME;
}

const string& SupplyDemandCurveSaver::getName() const {
    return mName;
}

/*! \brief Find and print supply-demand curves for designated market.
*
* This function creates a SupplyDemandCurve for the designated market by calculating
* the supply and demand at a series of prices, and prints the resulting curve.
*
* \author Rich Plevin (based on SolutionInfoSet::findAndPrintSD)
* \param aOut Output stream to print the curves to.
* \param aScenario the scenario to use to find the marketplace
* \param aPeriod Period for which to print supply-demand curves.
* \param aPrintHeader whether to print the CSV header (column names)
*/
void SupplyDemandCurveSaver::printCSV( ostream& aOut, Scenario* aScenario,
                                       const int aPeriod, bool aPrintHeader )
{   
    World* world = aScenario->getWorld();
    Marketplace* marketplace = scenario->getMarketplace();
    SolutionInfoSet solnInfoSet = SolutionInfoSet( marketplace );
    SolutionInfoParamParser solnParams;
    solnInfoSet.init( aPeriod, 0.001, 0.001, &solnParams );
    vector<SolutionInfo> solvable = solnInfoSet.getSolvableSet();

    int market_index = getMarketIndex(mName, solvable);

    if ( market_index < 0 ) {
        aOut << "# Market for " << mName << " was not found." << endl;

    } else {
        SupplyDemandCurve sdCurve( market_index, mName );

        sdCurve.calculatePoints( mPrices, solnInfoSet, world, marketplace, aPeriod, mIsPricesRelative );
        sdCurve.printCSV( aOut, aPeriod, aPrintHeader );
    }
}

/*! \brief Find the given marketName in the solvable markets and return its index, if found, else -1.
 */
int SupplyDemandCurveSaver::getMarketIndex(const string& marketName, vector<SolutionInfo> &aSolvable ) {
    for ( int i = 0; i < aSolvable.size(); ++i ) {
        if ( aSolvable[ i ].getName() == marketName )
            return i;
    }
    return -1;
}

void SupplyDemandCurveSaver::calcFeedbacksBeforePeriod( Scenario* aScenario,
							const IClimateModel* aClimateModel,
							const int aPeriod ) 
{
    // do nothing
}

void SupplyDemandCurveSaver::calcFeedbacksAfterPeriod( Scenario* aScenario,
						       const IClimateModel* aClimateModel,
						       const int aPeriod )
{
    const Configuration* conf = Configuration::getInstance();
    string confVarName = "supplyDemandCurves";

    if ( ! conf->shouldWriteFile( confVarName ) ) {
        return;
    }

    // supply and demand curves are useless for calibration periods, so skip them
    if ( aPeriod <= aScenario->getModeltime()->getFinalCalibrationPeriod() ) {
        return;
    }

    string fileName = conf->getFile( confVarName, "supplyDemandCurves.csv");

    AutoOutputFile outFile(fileName, mOpenMode );

    // First time through (before resetting open mode to append) write header, too.
    bool printHeader = (mOpenMode == ios_base::out);
    printCSV( *outFile, scenario, aPeriod, printHeader);

    mOpenMode = ios_base::app;   // after first call, append
}

