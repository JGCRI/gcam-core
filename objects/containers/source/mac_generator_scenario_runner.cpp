/*! 
* \file mac_generator_scenario_runner.cpp
* \ingroup CIAM
* \brief MACGeneratorScenarioRunner class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <cassert>
#include <vector>
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include "containers/include/mac_generator_scenario_runner.h"
#include "containers/include/scenario_runner.h"
#include "containers/include/single_scenario_runner.h"
#include "containers/include/scenario.h"
#include "containers/include/world.h"
#include "util/base/include/util.h"
#include "util/curves/include/curve.h"
#include "util/curves/include/point_set_curve.h"
#include "util/curves/include/point_set.h"
#include "util/curves/include/xy_data_point.h"
#include "util/base/include/configuration.h"
#include "util/base/include/timer.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/xml_helper.h"
#include "util/curves/include/explicit_point_set.h"
#include "util/base/include/auto_file.h"
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
extern void closeDB();
extern void createMCvarid();

/*! \brief Constructor.
* \param aGhgName The name of the GHG to calculate abatement costs for.
* \param aNumPoints The number of additional trials to run to calculate the abatement cost. 
*/
MACGeneratorScenarioRunner::MACGeneratorScenarioRunner( const string aGhgName, const unsigned int aNumPoints ):mGhgName( aGhgName ){
    mGlobalCost = 0;
    mGlobalDiscountedCost = 0;
    mNumPoints = aNumPoints;
    mSingleScenario.reset( new SingleScenarioRunner() );

    // Check to make sure calibration is off.
    if( Configuration::getInstance()->getBool( "debugChecking" ) && Configuration::getInstance()->getBool( "CalibrationActive" ) ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Calibration is incompatable with the generation of marginal abatement curves." << endl;
    }
}

//! Destructor. Deallocated memory for all the curves created. 
MACGeneratorScenarioRunner::~MACGeneratorScenarioRunner(){
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

/*! \brief Setup the Scenario to be run.
* \detailed This function sets up the contained SingleScenarioRunner.
* \param aTimer The timer used to print out the amount of time spent performing operations.
* \param aName The name to add on to the name read in in the Configuration file.
* \param aScenComponents A list of additional scenario components to read in.
* \return Whether the setup completed successfully.
*/
bool MACGeneratorScenarioRunner::setupScenario( Timer& aTimer, const string aName, const list<string> aScenComponents ){
    return mSingleScenario->setupScenario( aTimer, aName, aScenComponents );
}

/*! \brief Function which handles running the scenario and optionally
* computing a cost curve.
* \details This function wraps around the scenario so that scenario can be called
* multiple times if neccessary to create an abatement cost curve. This function first calls
* the scenario regularly, outputs all data, and then calls scenario several more times 
* and calculates the abatement cost.
* \param aTimer The timer used to print out the amount of time spent performing operations.
* \return Whether all model runs solved successfully.
* \author Josh Lurz
*/
bool MACGeneratorScenarioRunner::runScenario( Timer& aTimer ) {
    // Run the base scenario.
    bool success = mSingleScenario->runScenario( aTimer );

    // Print the output now before it is overwritten.
    mSingleScenario->printOutput( aTimer, false );

    // Now calculate the abatement curves.
    success &= calculateAbatementCostCurve();

    // Return whether the initial run and all datapoint calculations completed successfully.
    return success;
}

/*! \brief Function to create a cost curve for the mitigation policy.
* \details This function performs multiple calls to scenario.run() with 
* varied fixed carbon taxes in order to determine an abatement cost curve.
* \return Whether all model runs solved successfully.
* \author Josh Lurz
*/
bool MACGeneratorScenarioRunner::calculateAbatementCostCurve() {
    // Set the size of the emissions curve vectors to the number of trials plus 1 for the base.
    mEmissionsQCurves.resize( mNumPoints + 1 );
    mEmissionsTCurves.resize( mNumPoints + 1 );

    // Get prices and emissions for the primary scenario run.
    mEmissionsQCurves[ mNumPoints ] = scenario->getEmissionsQuantityCurves( mGhgName );
    mEmissionsTCurves[ mNumPoints ] = scenario->getEmissionsPriceCurves( mGhgName );
    
    // Run the trials and store the cost curves.
    bool success = runTrials();
    
    // Create a cost curve for each period and region.
    createCostCurvesByPeriod();

    // Create a cost curve for each region and find regional and global costs.
    createRegionalCostCurves();

    // Return whether all trials completed successfully.
    return success;
}

/*! \brief Run a trial for each point and store the abatement curves.
* \detailed First calculates a fraction of the total carbon tax to use, based 
* on the trial number and the total number of points, so that the datapoints are equally
* distributed from 0 to the full carbon tax for each period. It then calculates and 
* sets the fixed tax for each year. The scenario is then run, and the emissions and 
* tax curves are stored for each region.
* \return Whether all model runs completed successfully.
* \author Josh Lurz
*/
bool MACGeneratorScenarioRunner::runTrials(){
    // Get the number of max periods.
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxPeriod = modeltime->getmaxper();
    World* world = scenario->getWorld();
    bool success = true;
    // Loop through for each point.
    for( unsigned int currPoint = 0; currPoint < mNumPoints; currPoint++ ){
        // Determine the fraction of the full tax this tax will be.
        const double fraction = static_cast<double>( currPoint ) / static_cast<double>( mNumPoints );
        // Iterate through the regions to set different taxes for each if neccessary.
        // Currently this will set the same for all of them.
        for( CRegionCurvesIterator rIter = mEmissionsTCurves[ mNumPoints ].begin(); rIter != mEmissionsTCurves[ mNumPoints ].end(); ++rIter ){
            // Vector which will contain taxes for this trial.
            vector<double> currTaxes( maxPeriod );

            // Set the tax for each year. 
            for( int per = 0; per < maxPeriod; per++ ){
                const int year = modeltime->getper_to_yr( per );
                currTaxes[ per ] = rIter->second->getY( year ) * fraction;
            }
            // Set the fixed taxes into the world.
            world->setFixedTaxes( mGhgName, rIter->first, currTaxes );
        }

        // Create an ending for the output files using the run number.
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Starting cost curve point run number " << currPoint << "." << endl;
        success &= scenario->run( util::toString( currPoint ) );

        // Save information.
        mEmissionsQCurves[ currPoint ] = scenario->getEmissionsQuantityCurves( mGhgName );
        mEmissionsTCurves[ currPoint ] = scenario->getEmissionsPriceCurves( mGhgName );
    }
    return success;
}

/*! \brief Create a cost curve for each period and region.
* \detailed Using the cost curves generated by the trials, generate and stored a set of cost
* curves by period and region.
* \author Josh Lurz
*/
void MACGeneratorScenarioRunner::createCostCurvesByPeriod() {
    // Create curves for each period based on all trials.
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxPeriod = modeltime->getmaxper();
    mPeriodCostCurves.resize( maxPeriod );
    
    // Whether to print abatement in absolute terms or percent abatement.
    // const bool usePercentReduction = Configuration::getInstance()->getBool( "usePercentReduction", false );
    const bool usePercentReduction = false; // Hiding this option because it makes total costs non-sensical.
    for( int per = 0; per < maxPeriod; per++ ){
        const int year = modeltime->getper_to_yr( per );
        // Iterate over each region.
        for( CRegionCurvesIterator rIter = mEmissionsQCurves[ 0 ].begin(); rIter != mEmissionsQCurves[ 0 ].end(); rIter++ ){
            ExplicitPointSet* currPoints = new ExplicitPointSet();
            const string region = rIter->first;
            // Iterate over each trial.
            for( unsigned int trial = 0; trial < mNumPoints + 1; trial++ ){
                double reduction;
                
                if( usePercentReduction ){
                    reduction = ( rIter->second->getY( year ) - mEmissionsQCurves[ trial ][ region ]->getY( year ) ) / rIter->second->getY( year );
                }
                else {
                    reduction = rIter->second->getY( year ) - mEmissionsQCurves[ trial ][ region ]->getY( year );
                }
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
* \detailed Calculate for each region a final cost curve by integrating each period 
* cost curve from 0 to the total reduction in the initial constrain scenario. These are then
* used as datapoints to create a total cost curve for each region by period. These regional
* cost curves are then integrated and discounted based on a read-in discount rate. These values
* are both stored by region. A global sum for discounted and undiscounted values is stored as well.
* \author Josh Lurz
*/
void MACGeneratorScenarioRunner::createRegionalCostCurves() {
    // Iterate through the regions again to determine the cost per period.
    const vector<string> regions = scenario->getWorld()->getRegionVector();
    typedef vector<string>::const_iterator RNameIter;
    const Configuration* conf = Configuration::getInstance();
    const double discountRate = conf->getDouble( "discountRate", 0.05 );

    const Modeltime* modeltime = scenario->getModeltime();
    const int maxPeriod = modeltime->getmaxper();

    for( RNameIter rNameIter = regions.begin(); rNameIter != regions.end(); rNameIter++ ){
        ExplicitPointSet* costPoints = new ExplicitPointSet();

        // Loop through the periods. 
        for( int per = 0; per < maxPeriod; per++ ){
            const int year = modeltime->getper_to_yr( per );
            double periodCost = mPeriodCostCurves[ per ][ *rNameIter ]->getIntegral( 0, DBL_MAX ); // Integrate from zero to the reduction.
            XYDataPoint* currPoint = new XYDataPoint( year, periodCost );
            costPoints->addPoint( currPoint );
        }
        Curve* regCostCurve = new PointSetCurve( costPoints );
        regCostCurve->setTitle( *rNameIter );
        const double regionalCost = regCostCurve->getIntegral( modeltime->getper_to_yr( 1 ), modeltime->getendyr() );

        // Temporary hardcoding of start year.
        const double discountedRegionalCost = regCostCurve->getDiscountedValue( modeltime->getper_to_yr( 1 ), modeltime->getendyr(), discountRate );
        mRegionalCostCurves[ *rNameIter ] = regCostCurve;
        mRegionalCosts[ *rNameIter ] = regionalCost;
        mRegionalDiscountedCosts[ *rNameIter ] = discountedRegionalCost;
    
        mGlobalCost += regionalCost;
        mGlobalDiscountedCost += discountedRegionalCost;
    }
}

//! Print the output.
void MACGeneratorScenarioRunner::printOutput( Timer& timer, const bool aCloseDB ) const {
    // Database function definition. 
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    
    // Open the output file.
    AutoOutputFile ccOut( "costCurvesOutputFileName", "cost_curves_" + scenario->getName() + ".xml" );
    // Create a root tag.
    Tabs tabs;
	XMLWriteOpeningTag( "CostCurvesInfo", *ccOut, &tabs ); 

    XMLWriteOpeningTag( "PeriodCostCurves", *ccOut, &tabs );
    const Modeltime* modeltime =  scenario->getModeltime();
    const int maxPeriod = modeltime->getmaxper();
    const double CVRT_75_TO_90 = 2.212; //  convert '75 price to '90 price
    vector<double> tempOutVec( maxPeriod );

    for( int per = 0; per < maxPeriod; per++ ){
        const int year = modeltime->getper_to_yr( per );
        XMLWriteOpeningTag( "CostCurves", *ccOut, &tabs, "", year );
        for( CRegionCurvesIterator rIter = mPeriodCostCurves[ per ].begin(); rIter != mPeriodCostCurves[ per ].end(); rIter++ ){
            rIter->second->toInputXML( *ccOut, &tabs );
        }
        XMLWriteClosingTag( "CostCurves", *ccOut, &tabs );
    }
    XMLWriteClosingTag( "PeriodCostCurves", *ccOut, &tabs );
	
    XMLWriteOpeningTag( "RegionalCostCurvesByPeriod", *ccOut, &tabs );
    for( CRegionCurvesIterator rIter = mRegionalCostCurves.begin(); rIter != mRegionalCostCurves.end(); ++rIter ){
        rIter->second->toInputXML( *ccOut, &tabs );
        // Write out to the database.
        for( int per = 0; per < maxPeriod; ++per ){
            tempOutVec[ per ] = rIter->second->getY( modeltime->getper_to_yr( per ) ) * CVRT_75_TO_90;
        }
		dboutput4(rIter->first,"General","PolicyCostUndisc","Period","(millions)90US$",tempOutVec);
    }
    XMLWriteClosingTag( "RegionalCostCurvesByPeriod", *ccOut, &tabs ); 
	
    XMLWriteOpeningTag( "RegionalUndiscountedCosts", *ccOut, &tabs );
    // Write out undiscounted costs by region.
    for( CRegionalCostsIterator iter = mRegionalCosts.begin(); iter != mRegionalCosts.end(); iter++ ){
        XMLWriteElement( iter->second, "UndiscountedCost", *ccOut, &tabs, 0, iter->first );
	    // regional total cost of policy
		tempOutVec[maxPeriod-1] = iter->second * CVRT_75_TO_90;
		dboutput4(iter->first,"General","PolicyCostTotalUndisc","AllYears","(millions)90US$",tempOutVec);
    }
	XMLWriteClosingTag( "RegionalUndiscountedCosts", *ccOut, &tabs );
    // End of writing undiscounted costs by region.
     
    // Write out discounted costs by region.
	XMLWriteOpeningTag( "RegionalDiscountedCosts", *ccOut, &tabs );
    typedef map<const string,double>::const_iterator constDoubleMapIter;
    for( constDoubleMapIter iter = mRegionalDiscountedCosts.begin(); iter != mRegionalDiscountedCosts.end(); iter++ ){
        XMLWriteElement( iter->second, "DiscountedCost", *ccOut, &tabs, 0, iter->first );
	    // regional total cost of policy
		tempOutVec[maxPeriod-1] = iter->second * CVRT_75_TO_90;
		dboutput4(iter->first,"General","PolicyCostTotalDisc","AllYears","(millions)90US$",tempOutVec);
    }
	XMLWriteClosingTag( "RegionalDiscountedCosts", *ccOut, &tabs );
    // End of writing undiscounted costs by region.

    // Write out the total cost and discounted cost.
    XMLWriteElement( mGlobalCost, "GlobalUndiscountedTotalCost", *ccOut, &tabs );
    XMLWriteElement( mGlobalDiscountedCost, "GlobalDiscountedCost", *ccOut, &tabs );

	XMLWriteClosingTag( "CostCurvesInfo", *ccOut, &tabs );
    
    // Close the database.
    if( aCloseDB ){
        createMCvarid();
        closeDB();
    }
}
