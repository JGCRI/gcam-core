/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy ( DOE ). NEITHER THE GOVERNMENT NOR THE
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
* \file scenario.cpp
* \ingroup Objects
* \brief Scenario class source file.
* \author Sonny Kim
*/              

#include "util/base/include/definitions.h"
#include <string>
#include <fstream>
#include <cassert>
#include <ctime>
#include <iomanip>

#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/world.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_parse_helper.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"
#include "util/curves/include/curve.h"
#include "solution/solvers/include/solver.h"
#include "util/base/include/auto_file.h"
#include "util/base/include/timer.h"
#include "reporting/include/graph_printer.h"
#include "reporting/include/land_allocator_printer.h"
#include "solution/solvers/include/solver_factory.h"
#include "solution/solvers/include/bisection_nr_solver.h"
#include "solution/util/include/solution_info_param_parser.h" 
#include "containers/include/imodel_feedback_calc.h"
#include "util/base/include/manage_state_variables.hpp"
#include "util/base/include/supply_demand_curve_saver.h"

#if GCAM_PARALLEL_ENABLED && PARALLEL_DEBUG
#include <stdlib.h>
#include <tbb/tick_count.h>
#endif

using namespace std;
using namespace boost;

extern ofstream outFile;
time_t gGlobalTime;

//! Default constructor
Scenario::Scenario() {
    // Get time and date before model run.
    time( &gGlobalTime );

    mModeltime = 0;
    mMarketplace = new Marketplace();
    mWorld = 0;
    mSolutionInfoParamParser = 0;
    
    mManageStateVars = 0;
}

//! Destructor
Scenario::~Scenario() {
    delete mMarketplace;
    delete mWorld;
    delete mSolutionInfoParamParser;
    delete mManageStateVars;
    // model time is really a singleton and so don't
    // try to delete it
}

/*! \brief Get the static XML name of the Scenario.
* \return The XML name of the scenario element.
*/
const string& Scenario::getXMLNameStatic(){
    const static string XML_NAME = "scenario";
    return XML_NAME;
}

//! Return a reference to the modeltime->
const Modeltime* Scenario::getModeltime() const {
    return mModeltime;
}

//! Return a constant reference to the goods and services marketplace.
const Marketplace* Scenario::getMarketplace() const {
    return mMarketplace;
}

//! Return a mutable reference to the goods and services marketplace.
Marketplace* Scenario::getMarketplace() {
    return mMarketplace;
}

//! Return a constant reference to the world object.
const World* Scenario::getWorld() const {
    return mWorld;
}

//! Return a mutable reference to the world object.
World* Scenario::getWorld() {
    return mWorld;
}

bool Scenario::XMLParse(rapidxml::xml_node<char>* & aNode) {
    string nodeName = XMLParseHelper::getNodeName(aNode);
    if ( nodeName == Modeltime::getXMLNameStatic() ){
        if( !mModeltime ) {
            mModeltime = Modeltime::getInstance();
            rapidxml::xml_node<char>* firstChild = aNode->first_node();
            const_cast<Modeltime*>( mModeltime )->XMLParse( firstChild );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Modeltime can only be parsed once." << endl;
        }
        return true;
    }
    else if( SolverFactory::hasSolver( nodeName ) ) {
        map<string, string> attrs = XMLParseHelper::getAllAttrs(aNode);
        Solver* currSolver = 0;
        Data<Solver*, CONTAINER> currSolverData(currSolver, "");
        XMLParseHelper::parseData(aNode, currSolverData);
        if(currSolver) {
            /*!
             * \pre Modeltime has already been created.
             */
            assert( modeltime.get() );
            
            // this must be done here rather than relying on XMLHelper since we require a factory
            // to create our object
            const int period = mModeltime->getyr_to_per( XMLParseHelper::getValue<int>(attrs["year"]));
            const bool fillOut = XMLParseHelper::getValue<bool>( attrs["fillout"] );
            // we may need to resize the mSolvers which we could do now that we know we have a
            // modeltime
            if( mSolvers.size() == 0 ) {
                mSolvers.resize( mModeltime->getmaxper() );
            }
            boost::shared_ptr<Solver> retSolver(currSolver);
            mSolvers[ period ] = retSolver;
            
            // TODO: I think just using the same object rather than copying should suffice here
            for( int fillOutPeriod = period + 1; fillOut && fillOutPeriod < mModeltime->getmaxper(); ++fillOutPeriod ) {
                mSolvers[ fillOutPeriod ] = retSolver;
            }
        }
        return true;
    }
    else {
        return false;
    }
}
    

//! Sets the name of the scenario. 
void Scenario::setName( string newName ) {
    // Used to override the read-in scenario name.
    mName = newName;
}

//! Finish all initializations needed before the model can run.
void Scenario::completeInit() {
    // Make sure that some name is set.
    if( mName.empty() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "No scenario name was set, using default." << endl;
        mName = "NoScenarioName";
    }
    
    // if we didn't parse any solution info parameters we should at least
    // create an empty one
    if( !mSolutionInfoParamParser ) {
        mSolutionInfoParamParser = new SolutionInfoParamParser();
    }

    // Complete the init of the world object.
    if( mWorld ){
        mWorld->completeInit();

        // initialize solvers
        initSolvers();
    }
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "No world container was parsed from the input files." << endl;
    }

    // Set the valid period vector to false.
    mIsValidPeriod.clear();
    mIsValidPeriod.resize( mModeltime->getmaxper(), false );
}

//! Return scenario name.
const string& Scenario::getName() const {
    return mName;
}

/*! \brief Run the scenario.
* \param aSinglePeriod Single period to run or RUN_ALL_PERIODS if all periods
*        should be run.
* \param aPrintDebugging Whether to print extra debugging files.
* \param aFilenameEnding The string to add to the end of the debug output file
*        for uniqueness.
* \return Whether all model runs solved successfully.
*/
bool Scenario::run( const int aSinglePeriod,
                    const bool aPrintDebugging,
                    const string& aFilenameEnding )
{
    // If QuitFirstFailure bool is set to 1 and model is not running in target finder mode,
    // model will exit after any failed model period (rather than running to completion).
    const Configuration* conf = Configuration::getInstance();
    bool quitFirstFailure = conf->getBool("QuitFirstFailure", false, false);
    bool runTargetFinder = conf->getBool("find-path", false, false);
    // If applicable, print a statement to the log confirming this behavior.
    if (quitFirstFailure & !runTargetFinder) {
        ILogger& mainLog = ILogger::getLogger("main_log");
        mainLog.setLevel(ILogger::WARNING);
        mainLog << "QuitFirstFailure is set to true.  GCAM will exit after first failed model period." << endl;
    }


    // Avoid accumulating unsolved periods.
    mUnsolvedPeriods.clear();
    
    // Open the debugging files.
    AutoOutputFile XMLDebugFile( "xmlDebugFileName", "debug.xml", aPrintDebugging );
    Tabs tabs;
    if( aPrintDebugging ) {
        // Write opening tags for debug XML
        string dateString = util::XMLCreateDate( gGlobalTime );
        XMLDebugFile << "<" << getXMLNameStatic() << " name=\"" << mName << "\" date=\"" << dateString << "\">" << endl;

        tabs.increaseIndent();
    }

    Timer& fullScenarioTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::FULLSCENARIO );
    fullScenarioTimer.start();
    
    // Log that a run is beginning.
    logRunBeginning();

    bool success = true;

    // If the single period is RUN_ALL_PERIODS that means to calculate all periods. Loop over
    // time steps and operate model.
    if( aSinglePeriod == RUN_ALL_PERIODS ){
        for( int per = 0; per < mModeltime->getmaxper(); per++ ){
            success &= calculatePeriod( per, *XMLDebugFile, &tabs, aPrintDebugging );
            // If QuitFirstFailure bool is set to 1 and model is not running in target finder mode,
            // model will exit after any failed model period (rather than running to completion).
            if (!success & quitFirstFailure & !runTargetFinder) {
                ILogger& mainLog = ILogger::getLogger("main_log");
                mainLog.setLevel(ILogger::ERROR);
                mainLog << "Period " << per << " failed to solve. Skipping all remaining model periods." << endl;
                break;
            }
        }
    }
    // Check if the single period is invalid.
    else if( aSinglePeriod >= mModeltime->getmaxper() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Invalid single period " << aSinglePeriod << " passed to run method." << endl;
        success = false;
    } 
    else {
        // Run all periods up to the single period which are invalid.
        // Set up bool to skip all future periods if QuitFirstFailure bool is set to 1 and 
        // model fails to solve.
        bool runNextPeriod = true;
        for( int per = 0; per < aSinglePeriod; per++ ){
            if( !mIsValidPeriod[ per ] ){
                success &= calculatePeriod( per, *XMLDebugFile, &tabs, aPrintDebugging );
                // If QuitFirstFailure bool is set to 1 and model is not running in target finder mode,
                // model will exit after any failed model period (rather than running to completion).
                if (!success & quitFirstFailure & !runTargetFinder) {
                    ILogger& mainLog = ILogger::getLogger("main_log");
                    mainLog.setLevel(ILogger::ERROR);
                    mainLog << "Period " << per << " failed to solve. Skipping all remaining model periods." << endl;
                    runNextPeriod = false;
                    break;
                }
            }
        }
        
        if (runNextPeriod) {
            // Invalidate the period about to be run and all periods past it.
            for( int per = aSinglePeriod; per < mModeltime->getmaxper(); ++per ){
                mIsValidPeriod[ per ] = false;
            }

            // Now run the requested period. Results past this period will no longer
            // be valid. Do not attempt to use them!
            success &= calculatePeriod( aSinglePeriod, *XMLDebugFile, &tabs, aPrintDebugging );
        }
    }
    
    // Print any unsolved periods.
    // TODO: This should be added to the db.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::WARNING );
    
    // Report if all model periods solved correctly.
    if( mUnsolvedPeriods.empty() ) {
        mainLog << "All model periods solved correctly." << endl;
    }
    else {
        // Otherwise print all model periods which did not solve correctly.
        mainLog << "The following model periods did not solve: ";
        for( vector<int>::const_iterator i = mUnsolvedPeriods.begin(); i != mUnsolvedPeriods.end(); i++ ) {
            mainLog << *i << ", ";
        }
        mainLog << endl;
    }

    mainLog.setLevel( ILogger::DEBUG );
    fullScenarioTimer.stop();
    TimerRegistry::getInstance().printAllTimers( mainLog );

    // Run the climate model.
    mWorld->runClimateModel();

    // Close the debugging files.
    if( aPrintDebugging ){
        XMLWriteClosingTag( getXMLNameStatic(), *XMLDebugFile, &tabs );
    }
    
    // Log that the run has finished.
    logRunEnding();
    
    return success;
}

/*! \brief Calculate a single period.
* \param aPeriod Period to calculate.
* \param aXMLDebugFile XML debugging file.
* \param aTabs Tabs formatting object.
* \param aPrintDebugging Whether to print debugging information.
* \return Whether the period was calculated successfully.
*/
bool Scenario::calculatePeriod( const int aPeriod,
                                ostream& aXMLDebugFile,
                                Tabs* aTabs,
                                bool aPrintDebugging )
{
    logPeriodBeginning( aPeriod );

    // If this is period 0 initialize market price.
    if( aPeriod == 0 ){
        mMarketplace->initPrices(); // initialize prices
    }

    // Run the iteration of the model.
    mMarketplace->nullSuppliesAndDemands( aPeriod ); // initialize market demand to null
    mMarketplace->init_to_last( aPeriod ); // initialize to last period's info
    mWorld->initCalc( aPeriod ); // call to initialize anything that won't change during calc
    mMarketplace->assignMarketSerialNumbers( aPeriod ); // give the markets their serial numbers for this period.
    
    // Call any model feedback objects before we begin solving this period but after
    // we are initialized and ready to go.
    for( auto modelFeedback : mModelFeedbacks ) {
        modelFeedback->calcFeedbacksBeforePeriod( this, mWorld->getClimateModel(), aPeriod );
    }
    
    // Set up the state data for the current period.
    delete mManageStateVars;
    mManageStateVars = new ManageStateVariables( aPeriod );
    
    // Be sure to clear out any supplies and demands in the marketplace before making our
    // initial call to world.calc.  There may already be values in there if for instance
    // they got set from a restart file.
    mMarketplace->nullSuppliesAndDemands( aPeriod );

#if GCAM_PARALLEL_ENABLED && PARALLEL_DEBUG
    mWorld->calc( aPeriod );       // get rid of transient bad data
    mMarketplace->nullSuppliesAndDemands( aPeriod );

    ILogger &mainlog = ILogger::getLogger("main_log"); 
    
    ILogger &pdebug = ILogger::getLogger( "parallel_debug_log" );
    pdebug.setLevel(ILogger::DEBUG);
    pdebug << "**************** Starting serial World::calc (period " << aPeriod << ") ****************\n";

    tbb::tick_count t0 = tbb::tick_count::now();
#endif
    
    mWorld->calc( aPeriod ); // call to calculate initial supply and demand

#if GCAM_PARALLEL_ENABLED && PARALLEL_DEBUG
    tbb::tick_count t1 = tbb::tick_count::now();
    
    pdebug << "****************Ending serial World::calc****************\n";
    std::vector<double> serialrslt = marketplace->fullstate( aPeriod );

    pdebug << "%%%%%%%%%%%%%%%% Starting parallel World::calc (period " << aPeriod << ") %%%%%%%%%%%%%%%%\n";
    tbb::tick_count t2;
    tbb::tick_count t3;
    mMarketplace->nullSuppliesAndDemands( aPeriod );

    t2 = tbb::tick_count::now();
    mWorld->calc(aPeriod, world->getGlobalFlowGraph());
    t3 = tbb::tick_count::now();
      
    pdebug << "%%%%%%%%%%%%%%%% Ending parallel World::calc %%%%%%%%%%%%%%%%\n";

    // check for consistency between serial and parallel versions

    ILogger::WarningLevel old_main_log_level = mainlog.setLevel(ILogger::ERROR);
    if(aPeriod > 0) {
        if( !mMarketplace->checkstate(aPeriod, serialrslt, &mainlog, DBL_CMP_LOOSE) ) {
            std::cerr << "ERROR: parallel calc failed to reproduce serial results in period " << aPeriod
                      << ".\n";
            mainlog << "ERROR: parallel calc failed to reproduce serial results in period " << aPeriod
                    << ".\n"; 
        } 
        else {
            mainlog.setLevel(ILogger::NOTICE);
            std::cerr << "Period " << aPeriod << " parallel calc successful.\n";
        }
    }

    mainlog.setLevel(ILogger::WARNING);
    double sertime = (t1-t0).seconds();
    double partime = (t3-t2).seconds();
    double ratio   = sertime/partime;
    mainlog << "===Period: " << aPeriod << ":\tserial time: " << sertime
            << "\tparallel time:  " << partime << "\tspeedup:  " << ratio << "\n";
    mainlog.setLevel( old_main_log_level );
#endif
    
    
    bool success = solve( aPeriod ); // solution uses Bisect and NR routine to clear markets

    mWorld->postCalc( aPeriod );
        
    // Mark that the period is now valid.
    mIsValidPeriod[ aPeriod ] = true;

    // Run the climate model for this period (only if the solver is successful)
    if( !success ) {
        ILogger& climatelog = ILogger::getLogger( "climate-log" );
        climatelog.setLevel( ILogger::WARNING );
        climatelog << "Solver unsuccessful for period " << aPeriod << "." << endl;
    }
    mWorld->runClimateModel( aPeriod );
    
    // Call any model feedbacks now that we are done solving the current period and
    // the climate model has been run.
    for( auto modelFeedback : mModelFeedbacks ) {
        modelFeedback->calcFeedbacksAfterPeriod( this, mWorld->getClimateModel(), aPeriod );
    }

    logPeriodEnding( aPeriod );
    
    // Write out the results for debugging.
    if( aPrintDebugging ){
        writeDebuggingFiles( aXMLDebugFile, aTabs, aPeriod );
    }

    delete mManageStateVars;
    mManageStateVars = 0;
    
    return success;
}

/*! \brief Perform any logging which should occur when a period begins.
* \param aPeriod Model period.
*/
void Scenario::logPeriodBeginning( const int aPeriod ) const {
    ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
    calibrationLog.setLevel( ILogger::DEBUG );
    calibrationLog << "Period " << aPeriod <<": "<< mModeltime->getper_to_yr( aPeriod ) << endl << endl;

    ILogger& worstMarketLog = ILogger::getLogger( "worst_market_log" );
    worstMarketLog.setLevel( ILogger::DEBUG );
    worstMarketLog << "Period " << aPeriod <<": "<< mModeltime->getper_to_yr( aPeriod ) << endl;

    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Period " << aPeriod <<": "<< mModeltime->getper_to_yr( aPeriod ) << endl;
}

/*! \brief Perform any logging which should occur when a period ends.
* \param aPeriod Model period.
*/
void Scenario::logPeriodEnding( const int aPeriod ) const {
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << endl;
}

/*! \brief Perform any logging that should occur when the scenario run begins. */
void Scenario::logRunBeginning() const {
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Model run beginning." << endl;
}

/*! \brief Perform any logging that should occur when the scenario run ends.*/
void Scenario::logRunEnding() const {
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Model run completed." << endl;
}

/*! \brief Write to the debugging files for a given period.
* \param aXMLDebugFile XML debugging file.
* \param aTabs Tabs formatting object.
* \param aPeriod Model period.
*/
void Scenario::writeDebuggingFiles( ostream& aXMLDebugFile,
                                    Tabs* aTabs,
                                    const int aPeriod ) const
{
    mModeltime->toDebugXML( aPeriod, aXMLDebugFile, aTabs );
    mWorld->toDebugXML( aPeriod, aXMLDebugFile, aTabs );
}

/*! \brief Update a visitor for the Scenario.
* \param aVisitor Visitor to update.
* \param aPeriod Period to update.
*/
void Scenario::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitScenario( this, aPeriod );
    // Update the world.
    if( mWorld ){
        mWorld->accept( aVisitor, aPeriod );
    }
    aVisitor->endVisitScenario( this, aPeriod );
}

/*! \brief A function which print dependency graphs showing fuel usage by
*          sector. 
* \details This function creates a filename and stream for printing the graph
*          data in the dot graphing language. The filename is created from the
*          dependencyGraphName configuration attribute concatenated with the
*          period. The function then calls the World::printDependencyGraphs
*          function to perform the printing. Once the data is printed, dot must
*          be called to create the actual graph as follows: dot -Tpng
*          depGraphs_8.dot -o graphs.png where depGraphs_8.dot is the file
*          created by this function and graphs.png is the file you want to
*          create. The output format can be changed, see the dot documentation
*          for further information.
* \param aPeriod The period to print graphs for.
*/
void Scenario::printGraphs( const int aPeriod ) const {
    // Determine which region to print. Default to the US.
    const string regionToGraph = Configuration::getInstance()->getString( "debug-region", "USA" );
    
    // Open the file. It will automatically close.
    AutoOutputFile graphStream( "dependencyGraphName", "graph" );
    if( !graphStream.shouldWrite() ) {
        return;
    }
    
    // Create a graph printer.
    GraphPrinter graphPrinter( regionToGraph, *graphStream );
    
    // Update the graph printer with information from the model.
    accept( &graphPrinter, aPeriod );
    
    // Print the graph.
    graphPrinter.finish();
}

void Scenario::printLandAllocatorGraph( const int aPeriod, const bool aPrintValues ) const {
    // Determine which region to print.  Default to the US.
    const string regionToGraph = Configuration::getInstance()->getString( "debug-region", "USA" );
    
    // Open the file.  It will automatically close.
    AutoOutputFile landAllocatorStream( "landAllocatorGraphName", "LandAllocatorGraph" );
    if( !landAllocatorStream.shouldWrite() ) {
        return;
    }

    // Create the land allocator printer.
    LandAllocatorPrinter landAllocatorPrinter( regionToGraph, *landAllocatorStream,
                                               aPrintValues, true );

    // Update the land allocator printer with information from the model.
    accept( &landAllocatorPrinter, aPeriod );

    // Print the graph.
    landAllocatorPrinter.finish();
}

/*! \brief Set a tax into all regions.
* \details TODO
* \param aTax Tax to set.
*/
void Scenario::setTax( const GHGPolicy* aTax ){
    mWorld->setTax( aTax );
}

/*! \brief Get the climate model.
* \return The climate model.
*/
const IClimateModel* Scenario::getClimateModel() const {
    return mWorld->getClimateModel();
}

/*! \brief A function to generate a series of ghg emissions quantity curves
*          based on an already performed model run.
* \details This function used the information stored in it to create a series of
*          curves, one for each region, with each datapoint containing a time
*          period and an amount of gas emissions.
* \note The user is responsible for deallocating the memory in the returned
*       Curves.
* \author Josh Lurz
* \param ghgName The name of the ghg to create a set of curves for.
* \return A vector of Curve objects representing ghg emissions quantity by time
*         period by region.
* \todo Use a visitor to remove this method.
*/
map<string, const Curve*> Scenario::getEmissionsQuantityCurves( const string& ghgName ) const {
    /*! \pre The run has been completed. */
    return mWorld->getEmissionsQuantityCurves( ghgName );
}

/*! \brief A function to generate a series of ghg emissions price curves based
*          on an already performed model run.
* \details This function used the information stored in it to create a series of
*          curves, one for each period, with each datapoint containing a time
*          period and the price gas emissions. 
* \note The user is responsible for deallocating the memory in the returned
*       Curves.
* \author Josh Lurz
* \param ghgName The name of the ghg to create a set of curves for.
* \return A vector of Curve objects representing the price of ghg emissions by
*         time period by Region.
* \todo Use a visitor to remove this method.
*/
map<string, const Curve*> Scenario::getEmissionsPriceCurves( const string& ghgName ) const {
    /*! \pre The run has been completed. */
    return mWorld->getEmissionsPriceCurves( ghgName );
}

/*! \brief Solve the marketplace using the Solver for a given period. 
* \details The solve method calls the solve method of the instance of the Solver
*          object that was created in the constructor. This method then checks
*          for any errors that occurred while solving and reports the errors if
*          it is the last period. 
* \return Whether the model period solved successfully.
* \param period Period of the model to solve.
*/

bool Scenario::solve( const int period ){
    /*! \pre The solver must be instantiated. */
    assert( mSolvers[ period ].get() );

    // Solve the marketplace. If the return code is false than the model did not
    // solve for the period. Add the period to the scenario list of unsolved
    // periods. 
    const bool success = mSolvers[ period ]->solve( period, mSolutionInfoParamParser );
    if( !success ) {
        mUnsolvedPeriods.push_back( period );
    }
    
    return success;
}

//! Output Scenario members to a CSV file.
// I don't really like this function being hard-coded to an output file, but its very hard-coded.
void Scenario::writeOutputFiles() const {
    
    // Print out dependency graphs.
    const Configuration* conf = Configuration::getInstance();
    bool printValues = conf->getBool("PrintValuesOnGraphs");
    for( int period = 0; period  < getModeltime()->getmaxper(); ++period  ){
        printGraphs( period );
        // We only need to print a graph for each period if we are printing
        // values on the graphs.  Otherwise they are all the same.
        if( period == 0 || printValues ){
            printLandAllocatorGraph( period, printValues );
        }
    }
}

/*!
 * \brief A convience method to initialize solvers for all periods.
 * \details First look into the configuration file to see if the user
 *          specified a solver-config file.  If so then parse that file
 *          overwritting any previously created solvers.  Then we check
 *          each period to make sure we have a solver for that period, if
 *          not we will set it to the default solver.  The default solver
 *          is currently BisectionNRSolver.  Finally we call init for
 *          each solver.
 */
void Scenario::initSolvers() {
    // check the config file for a solver config file
    const string solverConfigFile = Configuration::getInstance()->getFile( "solver-config", "", false );
    
    // parse the solver config if the user specified one
    if( solverConfigFile != "" ) {
        XMLParseHelper::parseXML( solverConfigFile, this );
    }
    
    // we may need to resize the mSolvers which we could do now that we know we have a
    // modeltime
    if( mSolvers.size() == 0 ) {
        mSolvers.resize( mModeltime->getmaxper() );
    }
    
    // unfortunately we have to access this solver directly
    boost::shared_ptr<Solver> defaultSolver( new BisectionNRSolver( mMarketplace, mWorld ) );
    for(vector<boost::shared_ptr<Solver> >::iterator solverIt = mSolvers.begin(); solverIt != mSolvers.end(); ++solverIt ) {
        if( !(*solverIt).get() ) {
            (*solverIt) = defaultSolver;
        }
        // Complete the init of the solution object.
        (*solverIt)->init();
    }
}

/*!
 * \brief Get the periods that did not solve in the last call to run.
 * \return A vector of model periods that did not solve.
 */
const vector<int>& Scenario::getUnsolvedPeriods() const {
    return mUnsolvedPeriods;
}

/*!
 * \brief Reset the flag which indicates if a model period should be
 *        recalculated to force it to do so the next time run is called.
 * \param aPeriod The model period to invalidate.
 */
void Scenario::invalidatePeriod( const int aPeriod ) {
    mIsValidPeriod[ aPeriod ] = false;
}

/*!
 * \brief Get a reference to the object responsible for managing state.
 * \return The ManageStateVariables object.
 */
ManageStateVariables* Scenario::getManageStateVariables() const {
    return mManageStateVars;
}

