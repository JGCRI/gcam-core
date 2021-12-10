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
* \file total_policy_cost_calculator.cpp
* \ingroup Objects
* \brief TotalPolicyCostCalculator class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <vector>
#include <string>
#include "containers/include/scenario.h"
#include "containers/include/world.h"
#include "util/base/include/util.h"
#include "util/curves/include/curve.h"
#include "util/curves/include/point_set_curve.h"
#include "util/curves/include/point_set.h"
#include "util/curves/include/xy_data_point.h"
#include "util/base/include/configuration.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "util/base/include/xml_helper.h"
#include "util/curves/include/explicit_point_set.h"
#include "util/base/include/auto_file.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/total_policy_cost_calculator.h"
#include "containers/include/single_scenario_runner.h"
#include "policy/include/policy_ghg.h"
#include "reporting/include/xml_db_outputter.h"

#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/classification.hpp>

using namespace std;

/*! \brief Constructor.
* \param aSingleScenario The single scenario runner.
*/
TotalPolicyCostCalculator::TotalPolicyCostCalculator( SingleScenarioRunner* aSingleScenario ){
    assert( aSingleScenario );
    mSingleScenario = aSingleScenario;
    mGlobalCost = 0;
    mGlobalDiscountedCost = 0;
    mRanCosts = false;

    // Get the variables from the configuration.
    const Configuration* conf = Configuration::getInstance();
    mGHGName = conf->getString( "AbatedGasForCostCurves", "CO2" );
    mNumPoints = conf->getInt( "numPointsForCO2CostCurve", 5 );
    
    // A user may have specified more than one gas from which to sum emissions quantities
    // which would be seperated by a ';'.  In such a case the first gas is assumed to be
    // the policy name and the subsequent names are the constituent gasses from which to sum
    if( mGHGName.find( ';' ) != string::npos ) {
        boost::algorithm::split( mGHGQuantityNames, mGHGName, boost::is_any_of( ";" ) );
        mGHGName = mGHGQuantityNames.front();
        // Note, we are not going to remove the policy name from the list even though no emissions
        // of that name will match only to ensure the name of the curve gets set correctly.
    }
    else {
        // Just a single gas, same as the policy
        mGHGQuantityNames.push_back( mGHGName );
    }
}

//! Destructor. Deallocated memory for all the curves created. 
TotalPolicyCostCalculator::~TotalPolicyCostCalculator(){
    // This deletes all the curves.
    for( VectorRegionCurvesIterator outerDel = mEmissionsQCurves.begin(); outerDel != mEmissionsQCurves.end(); ++outerDel ){
        for( RegionCurvesIterator innerDel = outerDel->begin(); innerDel != outerDel->end(); ++innerDel ){
            delete innerDel->second;
        }
    }
    
    for( VectorRegionCurvesIterator outerDel = mEmissionsTCurves.begin(); outerDel != mEmissionsTCurves.end(); ++outerDel ){
        for( RegionCurvesIterator innerDel = outerDel->begin(); innerDel != outerDel->end(); ++innerDel ){
            delete innerDel->second;
        }
    }
    
    for( VectorRegionCurvesIterator outerDel = mPeriodCostCurves.begin(); outerDel != mPeriodCostCurves.end(); ++outerDel ){
        for( RegionCurvesIterator innerDel = outerDel->begin(); innerDel != outerDel->end(); ++innerDel ){
            delete innerDel->second;
        }
    }

    for( RegionCurvesIterator del = mRegionalCostCurves.begin(); del != mRegionalCostCurves.end(); ++del ){
        delete del->second;
    }
}

/*! \brief Function to create a cost curve for the mitigation policy.
* \details This function performs multiple calls to scenario.run() with 
* varied fixed carbon taxes in order to determine an abatement cost curve.
* \return Whether all model runs solved successfully.
* \author Josh Lurz
* \todo Find a better way to check for the existance of a carbon market taking into account 
* different carbon policies in different regions. 
*/
bool TotalPolicyCostCalculator::calculateAbatementCostCurve() {
    // If there is no policy market, the model will not create cost curves and 
    // will leave mRanCosts as false. This will prevent the cost curves from printing.
    if( mSingleScenario->getInternalScenario()->getMarketplace()->getPrice( mGHGName, "USA", 1 ) == Marketplace::NO_MARKET_PRICE ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Skipping cost curve calculations for non-policy model run." << endl;
        return true;
    }

    // Set the size of the emissions curve vectors to the number of trials plus 1 for the base.
    mEmissionsQCurves.resize( mNumPoints + 1 );
    mEmissionsTCurves.resize( mNumPoints + 1 );

    // Get prices and emissions for the primary scenario run.
    mEmissionsQCurves[ mNumPoints ] = getEmissionsQuantityCurve();
    mEmissionsTCurves[ mNumPoints ] = mSingleScenario->getInternalScenario()->getEmissionsPriceCurves( mGHGName );
    
    // Run the trials and store the cost curves.
    bool success = runTrials();
    
    // Create a cost curve for each period and region.
    createCostCurvesByPeriod();

    // Create a cost curve for each region and find regional and global costs.
    createRegionalCostCurves();

    // Return whether all trials completed successfully.
    mRanCosts = true;
    return success;
}

/*! \brief Run a trial for each point and store the abatement curves.
* \details First calculates a fraction of the total carbon tax to use, based 
* on the trial number and the total number of points, so that the data points are equally
* distributed from 0 to the full carbon tax for each period. It then calculates and 
* sets the fixed tax for each year. The scenario is then run, and the emissions and 
* tax curves are stored for each region.
* \return Whether all model runs completed successfully.
* \author Josh Lurz
*/
bool TotalPolicyCostCalculator::runTrials(){
    // Get the number of max periods.
    const Modeltime* modeltime = mSingleScenario->getInternalScenario()->getModeltime();
    const int maxPeriod = modeltime->getmaxper();

    bool success = true;
    const static bool usingRestartPeriod = Configuration::getInstance()->getInt(
        "restart-period", -1 ) != -1;
    // Store original solved market prices before looping.
    if( !usingRestartPeriod ) {
        mSingleScenario->getInternalScenario()->getMarketplace()->store_prices_for_cost_calculation();
    }
    // Loop through for each point.
    for( int currPoint = mNumPoints - 1; currPoint >= 0; currPoint-- ){
        // Determine the fraction of the full tax this tax will be.
        const double fraction = static_cast<double>( currPoint ) / static_cast<double>( mNumPoints );
        // Iterate through the regions to set different taxes for each if necessary.
        // Currently this will set the same for all of them.
        for( CRegionCurvesIterator rIter = mEmissionsTCurves[ mNumPoints ].begin(); rIter != mEmissionsTCurves[ mNumPoints ].end(); ++rIter ){
            // Vector which will contain taxes for this trial.
            vector<double> currTaxes( maxPeriod );

            // Set the tax for each year. 
            for( int per = 0; per < maxPeriod; per++ ){
                const int year = modeltime->getper_to_yr( per );
                double origTax = rIter->second->getY( year );
                currTaxes[ per ] = origTax == Marketplace::NO_MARKET_PRICE ? Marketplace::NO_MARKET_PRICE :
                    origTax * fraction;
            }
            // Set the fixed taxes into the world.
            GHGPolicy tax( mGHGName, rIter->first, currTaxes );
            mSingleScenario->getInternalScenario()->setTax( &tax );
        }

        // Create an ending for the output files using the run number.
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Starting cost curve point run number " << currPoint << "." << endl;

        // Run the scenario with the add-on extension to the output file names
        // as the point number. This allows the output file to be named debug +
        // point number.
        success &= mSingleScenario->getInternalScenario()->run( Scenario::RUN_ALL_PERIODS, true,
                                                                util::toString( currPoint ) );

        // Save information.
        mEmissionsQCurves[ currPoint ] = getEmissionsQuantityCurve();
        mEmissionsTCurves[ currPoint ] = mSingleScenario->getInternalScenario()->getEmissionsPriceCurves( mGHGName );

        // Restore original solved market prices after each cost iteration to ensure same
        // starting prices for each iteration.  This is necessary due to changing initial prices.
        if( !usingRestartPeriod || ( currPoint - 1 ) == 0 ) {
            mSingleScenario->getInternalScenario()->getMarketplace()->restore_prices_for_cost_calculation();
        }
    }
    return success;
}

/*! \brief Create a cost curve for each period and region.
* \details Using the cost curves generated by the trials, generate and stored a set of cost
* curves by period and region.
* \author Josh Lurz
*/
void TotalPolicyCostCalculator::createCostCurvesByPeriod() {
    // Create curves for each period based on all trials.
    const Modeltime* modeltime = mSingleScenario->getInternalScenario()->getModeltime();
    const int maxPeriod = mSingleScenario->getInternalScenario()->getModeltime()->getmaxper();
    mPeriodCostCurves.resize( maxPeriod );
    
    for( int per = 0; per < maxPeriod; per++ ){
        const int year = modeltime->getper_to_yr( per );
        // Iterate over each region.
        for( CRegionCurvesIterator rIter = mEmissionsQCurves[ 0 ].begin(); rIter != mEmissionsQCurves[ 0 ].end(); rIter++ ){
            ExplicitPointSet* currPoints = new ExplicitPointSet();
            const string region = rIter->first;
            // Iterate over each trial.
            for( unsigned int trial = 0; trial < mNumPoints + 1; trial++ ){
                double reduction = rIter->second->getY( year )
                                   - mEmissionsQCurves[ trial ][ region ]->getY( year );
                const double tax = mEmissionsTCurves[ trial ][ region ]->getY( year );
                XYDataPoint* currPoint = new XYDataPoint( reduction, tax );
                currPoints->addPoint( currPoint );
            }
            Curve* perCostCurve = new PointSetCurve( currPoints );
            perCostCurve->setTitle( region + " period cost curve" );
            perCostCurve->setNumericalLabel( per );
            mPeriodCostCurves[ per ][ region ] = perCostCurve;
        }
    }
}

/*! \brief Calculate final regional cost curves and total costs.
* \details Calculate for each region a final cost curve by integrating each period 
* cost curve from 0 to the total reduction in the initial constrain scenario. These are then
* used as datapoints to create a total cost curve for each region by period. These regional
* cost curves are then integrated and discounted based on a read-in discount rate. These values
* are both stored by region. A global sum for discounted and undiscounted values is stored as well.

* \author Josh Lurz
*/
void TotalPolicyCostCalculator::createRegionalCostCurves() {
    // Iterate through the regions again to determine the cost per period.
    const Configuration* conf = Configuration::getInstance();

    const double DEFAULT_DISCOUNT_RATE = 0.05;
    double discountRate = conf->getDouble( "discountRate", DEFAULT_DISCOUNT_RATE );

    const int DEFAULT_START_YEAR = 2005;
    int startYear = conf->getInt( "discount-start-year", DEFAULT_START_YEAR );

    // Perform error checking on the configuration values.
    const Modeltime* modeltime = mSingleScenario->getInternalScenario()->getModeltime();
    if( discountRate < 0 || discountRate > 1 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Invalid discount rate of " << discountRate << ". Resetting to default." << endl;
        discountRate = DEFAULT_DISCOUNT_RATE;
    }

    if( startYear < modeltime->getStartYear() || startYear > modeltime->getEndYear() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Discount start year must be a valid model year. Resetting to default." << endl;
        startYear = DEFAULT_START_YEAR;
    }

    const int maxPeriod = modeltime->getmaxper();
    
    //Reset these to zero so that global costs don't accumulate across batch runs
    mGlobalCost = 0;
    mGlobalDiscountedCost = 0;
    
    
    for( CRegionCurvesIterator rNameIter = mPeriodCostCurves[ 0 ].begin(); rNameIter != mPeriodCostCurves[ 0 ].end(); ++rNameIter ){
        ExplicitPointSet* costPoints = new ExplicitPointSet();
        // Loop through the periods. 
        for( int per = 0; per < maxPeriod; per++ ){
            const int year = modeltime->getper_to_yr( per );
            double periodCost = mPeriodCostCurves[ per ][ rNameIter->first ]->getIntegral( 0, DBL_MAX ); // Integrate from zero to the reduction.
            XYDataPoint* currPoint = new XYDataPoint( year, periodCost );
            costPoints->addPoint( currPoint );
        }
        Curve* regCostCurve = new PointSetCurve( costPoints );
        regCostCurve->setTitle( rNameIter->first );

        const double regionalCost = regCostCurve->getIntegral( startYear, modeltime->getEndYear() );

        // Temporary hardcoding of start year.
        const double discountedRegionalCost = regCostCurve->getDiscountedValue( startYear, modeltime->getEndYear(), discountRate );
        mRegionalCostCurves[ rNameIter->first ] = regCostCurve;
        mRegionalCosts[ rNameIter->first ] = regionalCost;
        mRegionalDiscountedCosts[ rNameIter->first ] = discountedRegionalCost;
        
        mGlobalCost += regionalCost;
        mGlobalDiscountedCost += discountedRegionalCost;
    }
}

/*!
 * \brief Get the emissions quantity curves by region.
 * \details This method will loop over each gas listed in mGHGQuantityNames and
 *          query those emissions from the scenario and sum them weighting by the
 *          "demand-adjust" set in that gas' market info.
 * \return A total emissions quantity curve by region.
 */
TotalPolicyCostCalculator::RegionCurves TotalPolicyCostCalculator::getEmissionsQuantityCurve() const {
    /*! \pre At-least one gas is set to sum */
    assert( !mGHGQuantityNames.empty() );
    
    RegionCurves ret;
    
    const Modeltime* modeltime = mSingleScenario->getInternalScenario()->getModeltime();
    const Marketplace* marketplace = mSingleScenario->getInternalScenario()->getMarketplace();
    const string DEMAND_ADJ_KEY = "demand-adjust";
    
    for( string currGHG : mGHGQuantityNames ) {
        RegionCurves currGHGCurves = mSingleScenario->getInternalScenario()->getEmissionsQuantityCurves( currGHG );
        for( auto currRegionCurve : currGHGCurves ) {
            string currRegion = currRegionCurve.first;
            for( int period = 0; period < modeltime->getmaxper(); ++period ) {
                int year = modeltime->getper_to_yr( period );
                const IInfo* currMarketInfo = marketplace->getMarketInfo( currGHG, currRegion, period, false );
                // if the market info contains the "demand-adjust" we should weight the emissions with it
                if( currMarketInfo && currMarketInfo->hasValue( DEMAND_ADJ_KEY ) ) {
                    double demandAdjust = currMarketInfo->getDouble( DEMAND_ADJ_KEY, true );
                    // adjust curve by demand adjust
                    const_cast<Curve*>( currRegionCurve.second )->setY( year, currRegionCurve.second->getY( year ) * demandAdjust );
                }
            }
            // Update the return regional curves for this gas
            auto retRegionCurve = ret.find( currRegion );
            if( retRegionCurve == ret.end() ) {
                // This region doesn't exist yet in the return data so just copy it over
                ret[ currRegion ] = currRegionCurve.second;
            }
            else {
                // Add in the current gas + region to the total curve to return
                for( int period = 0; period < modeltime->getmaxper(); ++period ) {
                    int year = modeltime->getper_to_yr( period );
                    const_cast<Curve*>( (*retRegionCurve).second )->setY( year, (*retRegionCurve).second->getY( year ) + currRegionCurve.second->getY( year ) );
                }
            }
        }
    }
    
    return ret;
}

/*! \brief Print the output.
* \details Print the output to an XML file, the Access database, and the XML
*          database.
*/
void TotalPolicyCostCalculator::printOutput() const {
    // Don't try to print output if the scenarios weren't run.
    if( !mRanCosts ){
        return;
    }
    
    // Create a string with the XML output.
    const string xmlString = createXMLOutputString();
    
    {
        // Open the XML output file and write to it.
        AutoOutputFile ccOut( "costCurvesOutputFileName",
                              "cost_curves.xml" );
        ccOut << xmlString;
    }
    
    // Location to insert the information into the container.
    const string UPDATE_LOCATION = "/scenario/world/region[last()]";
    
    // Append the data to the XML database.
    if( Configuration::getInstance()->shouldWriteFile( "xmldb-location" ) ) {
        mSingleScenario->getXMLDBOutputter()->appendData( xmlString, UPDATE_LOCATION );
    }
}

/*! Create a string containing the XML output.
* \return A string containing the XML output.
*/
const string TotalPolicyCostCalculator::createXMLOutputString() const {
    // Create a buffer to contain the output.
    stringstream buffer;
    Tabs tabs;

    // Create a root tag.
    XMLWriteOpeningTag( "CostCurvesInfo", buffer, &tabs ); 

    XMLWriteOpeningTag( "PeriodCostCurves", buffer, &tabs );

    const Modeltime* modeltime = mSingleScenario->getInternalScenario()->getModeltime();

    for( int per = 0; per < modeltime->getmaxper(); per++ ){
        const int year = modeltime->getper_to_yr( per );
        XMLWriteOpeningTag( "CostCurves", buffer, &tabs, "", year );
        for( CRegionCurvesIterator rIter = mPeriodCostCurves[ per ].begin(); rIter != mPeriodCostCurves[ per ].end(); rIter++ ){
            rIter->second->outputAsXML( buffer, &tabs );
        }
        XMLWriteClosingTag( "CostCurves", buffer, &tabs );
    }
    XMLWriteClosingTag( "PeriodCostCurves", buffer, &tabs );
    
    XMLWriteOpeningTag( "RegionalCostCurvesByPeriod", buffer, &tabs );
    for( CRegionCurvesIterator rIter = mRegionalCostCurves.begin(); rIter != mRegionalCostCurves.end(); ++rIter ){
        rIter->second->outputAsXML( buffer, &tabs );
    }
    XMLWriteClosingTag( "RegionalCostCurvesByPeriod", buffer, &tabs ); 
    
    XMLWriteOpeningTag( "RegionalUndiscountedCosts", buffer, &tabs );
    // Write out undiscounted costs by region.
    for( CRegionalCostsIterator iter = mRegionalCosts.begin(); iter != mRegionalCosts.end(); iter++ ){
        XMLWriteElement( iter->second, "UndiscountedCost", buffer, &tabs, 0, iter->first );
    }
    XMLWriteClosingTag( "RegionalUndiscountedCosts", buffer, &tabs );
     
    // Write out discounted costs by region.
    XMLWriteOpeningTag( "RegionalDiscountedCosts", buffer, &tabs );
    for( CRegionalCostsIterator iter = mRegionalDiscountedCosts.begin(); iter != mRegionalDiscountedCosts.end(); iter++ ){
        XMLWriteElement( iter->second, "DiscountedCost", buffer, &tabs, 0, iter->first );
    }
    XMLWriteClosingTag( "RegionalDiscountedCosts", buffer, &tabs );

    // Write out the total cost and discounted cost.
    XMLWriteElement( mGlobalCost, "GlobalUndiscountedTotalCost", buffer, &tabs );
    XMLWriteElement( mGlobalDiscountedCost, "GlobalDiscountedCost", buffer, &tabs );

    XMLWriteClosingTag( "CostCurvesInfo", buffer, &tabs );
    return buffer.str();
}
