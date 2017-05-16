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
* \file solution_info_set.cpp
* \ingroup Solution
* \brief SolutionInfoSet class source file.
* \author Josh Lurz, Sonny Kim
*/
#include "util/base/include/definitions.h"
#include <cassert>
#include <algorithm>
#include "util/base/include/util.h"
#include "solution/util/include/solution_info_set.h"
#include "solution/util/include/solution_info.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/supply_demand_curve.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"
#include "solution/util/include/isolution_info_filter.h"
#include "marketplace/include/market.h"
#include "solution/util/include/solution_info_param_parser.h"
#include "containers/include/market_dependency_finder.h"

using namespace std;

//! Constructor
SolutionInfoSet::SolutionInfoSet( Marketplace* aMarketplace ):
period( 0 ),
marketplace( aMarketplace )
{
    /*!\pre Marketplace is not null. */
    assert( aMarketplace );
}

//! Constructor for new solution set
SolutionInfoSet::SolutionInfoSet( const vector<SolutionInfo> aSolutionSet ): solvable( aSolutionSet )
{
}

/*!
 * \brief Initialize the SolutionInfoSet and its SolutionInfo's.
 * \details Creates a list of markets to solve and wraps them in a SolutionInfo object
 *          so that we can associate solver specific parameters to them.
 * \param aPeriod The current period we are initializing for.
 * \param aDefaultSolutionTolerance The default solution tolerance to use for a SolutionInfo.
 * \param aDefaultSolutionFloor The default solution floor to use for a solution info.
 * \param aSolutionInfoParamParser An object which contains any SolutionInfo specific solution parameters
 *                                 that we should set within them.
 */
void SolutionInfoSet::init( const unsigned int aPeriod, const double aDefaultSolutionTolerance,
                          const double aDefaultSolutionFloor, const SolutionInfoParamParser* aSolutionInfoParamParser )
{
    assert( aPeriod >= 0 );
    this->period = aPeriod;

    // Print a debugging log message.
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::DEBUG );
    solverLog << "Initializing the solvable set." << endl;

    // Request the markets to solve from the marketplace. 
    vector<Market*> marketsToSolve = marketplace->getMarketsToSolve( period );

    // Create and initialize a SolutionInfo object for each market.
    typedef vector<Market*>::const_iterator ConstMarketIterator;
    MarketDependencyFinder* depFinder = marketplace->getDependencyFinder();
    for( ConstMarketIterator iter = marketsToSolve.begin(); iter != marketsToSolve.end(); ++iter ){
        const bool isSolvable = (*iter)->isSolvable();
        const int marketNumber = iter - marketsToSolve.begin();
        const vector<IActivity*> partialList = isSolvable ? depFinder->getOrdering( marketNumber ) : vector<IActivity*>();
#if GCAM_PARALLEL_ENABLED
        // TODO: As it turns out the extra time generating these graphs does not typically
        // get paid back in terms of time saved while calculating partial derivatives.  At
        // least in a single scenario run.  We need to come up with some methodology to figure
        // out when it is beneficial to do this or not until then we are not generating any.
        SolutionInfo currInfo( *iter, partialList, 
               /*isSolvable ? depFinder->getFlowGraph( marketNumber ) :*/ 0 );
#else
        SolutionInfo currInfo( *iter, partialList );
#endif
        currInfo.init( aDefaultSolutionTolerance, aDefaultSolutionFloor,
                       aSolutionInfoParamParser->getSolutionInfoValuesForMarket( (*iter)->getGoodName(), (*iter)->getRegionName(),
                                                                                 currInfo.getTypeName(), period ) );
        if( currInfo.shouldSolve( false ) ){
            solvable.push_back( currInfo );
        }
        else {
            unsolvable.push_back( currInfo );
        }
    }
}

//! Update which markets are currently being solved.
SolutionInfoSet::UpdateCode SolutionInfoSet::updateSolvable( const ISolutionInfoFilter* aSolutionInfoFilter ) {
    /*! \pre The updateFromMarkets has been called. */
    // Code which indicates whether markets were added, removed, both or neither. 
    UpdateCode code( UNCHANGED );
    // Print a debugging log message.
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::DEBUG );
    solverLog << "Updating the solvable set." << endl;

    // Iterate through the solvable markets and determine if any are now unsolvable.
    for( SetIterator iter = solvable.begin(); iter != solvable.end(); ){
        // If it should not be solved for the current method, move it to the unsolvable vector.
        if( !aSolutionInfoFilter->acceptSolutionInfo( *iter ) ){
            unsolvable.push_back( *iter );

            // Print a debugging log message.
            solverLog << iter->getName() << " was removed from the solvable set." << endl;

            // After the erase, iter will point to the next element in the array
            iter = solvable.erase( iter );

            // Update the return code.
            code = REMOVED;
        }
        else{
            // If we don't erase an element, we need to increment iter
            ++iter;
        }
    }

    // Loop through the unsolvable set to see if they should be added to the solved. 
    // This will double check markets that were just added, slightly inefficient.
    for( SetIterator iter = unsolvable.begin(); iter != unsolvable.end(); ){
        // If it should be solved for the current method, move it to the solvable vector.
        if( aSolutionInfoFilter->acceptSolutionInfo( *iter ) ){
            solvable.push_back( *iter );
            // Print a debugging log message.
            solverLog << iter->getName() << " was added to the solvable set." << endl;

            // After the erase, iter will point to the next element in the array
            iter = unsolvable.erase( iter );

            // Update return code.
            if( code == UNCHANGED || ADDED ){
                code = ADDED;
            }
            else {
                code = ADDED_AND_REMOVED;
            }
        }
        else{
            // If we don't erase an element, we need to increment iter
            ++iter;
        }
    }
    return code;
}

//! Update the elasticities for all the markets.
void SolutionInfoSet::updateElasticities() {
    for( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        iter->calcDemandElas( *this );
        iter->calcSupplyElas( *this );
    }
}

/*! \brief Check if the bracket is empty and reset it if neccessary.
* \return Whether any brackets were reset. 
*/
bool SolutionInfoSet::checkAndResetBrackets(){
    bool didReset = false;
    for( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        didReset |= iter->checkAndResetBrackets();
    }
    return didReset;
}

/*! \brief Reset solution set brackets to current prices and excess demands and 
    set bracketed to false.
*/
void SolutionInfoSet::resetBrackets(){
    for( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        iter->resetBrackets();
    }
}

//! Find the maximum relative excess demand.
double SolutionInfoSet::getMaxRelativeExcessDemand() const {
    double largest = -1;
    for ( ConstSetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ) {
        const double relativeED = iter->getRelativeED();

        if ( relativeED > largest ) {
            largest = relativeED;
        }
    }
    return largest;
}

//! Find the maximum absolute excess demand.
double SolutionInfoSet::getMaxAbsoluteExcessDemand() const {
    double largest = -1;
    for ( ConstSetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ) {
        const double absoluteExcessDemand = fabs( iter->getED() );
        if( absoluteExcessDemand > largest ){
            largest = absoluteExcessDemand;
        }
    }
    return largest;
}

/*! \brief Finds the SolutionInfo with the largest relative excess demand.
* \author Josh Lurz
* \details This function determines the SolutionInfo within the set which has the largest relative excess demand as defined by
* getRelativeED.
* \return The SolutionInfo with the largest relative excess demand. 
*/
SolutionInfo* SolutionInfoSet::getWorstSolutionInfo( bool aIgnoreBisected )  {

    SetIterator worstMarket = solvable.begin();
    double largest = -1;

    for ( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ) {
        if( aIgnoreBisected && iter->hasBisected() ){
            continue;
        }
        double relativeED = iter->getRelativeED();
        
        if ( relativeED > largest ) {
            worstMarket = iter;
            largest = relativeED;
        }
    }
    return &*worstMarket;
}

const SolutionInfo* SolutionInfoSet::getWorstSolutionInfo( bool aIgnoreBisected ) const {
  return const_cast<SolutionInfoSet*>( this )->getWorstSolutionInfo( aIgnoreBisected );
}


/*! \brief Returns the best unsolved solution info. 
* \author Josh Lurz
* \details This function determines the SolutionInfo within the set which has the largest relative excess demand as defined by
* getRelativeED. 
* \return The SolutionInfo with the smallest unsolved relative excess demand. 
*/
SolutionInfo* SolutionInfoSet::getWorstSolutionInfoReverse( const bool aIgnoreBisected ) {
    
    // Find the worst one. 
    SolutionInfo* worstMarket = getWorstSolutionInfo( aIgnoreBisected );
    double smallest = worstMarket->getRelativeED();
    SetIterator bestUnsolved = solvable.begin();

    for ( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ) {
        if( aIgnoreBisected && iter->hasBisected() ){
            continue;
        }
        if( iter->isWithinTolerance() ){ // not right
            continue;
        }
        const double relativeED = iter->getRelativeED();
        
        if ( relativeED <= smallest ) {
            bestUnsolved = iter;
            smallest = relativeED;
        }
    }
    return &*bestUnsolved;
}
/*! \brief Find the policy solution info, or the worst if there is no policy.
* \author Josh Lurz
* \details This function determines the SolutionInfo within the set which has the largest relative excess demand as defined by
* getRelativeED.
* \return The SolutionInfo for the policy, or the worst one if that does not exist.
*/
SolutionInfo* SolutionInfoSet::getPolicyOrWorstSolutionInfo() {
    for( SetIterator iter = solvable.begin(); iter != solvable.end(); ++ iter ){
        // TODO: Find a more generic method. 
        if( iter->getName() == "globalCO2" && !iter->isSolved() ){
            return &*iter;
        }
    }

    double largest = -1;
    SetIterator worstMarket = solvable.begin();
    for ( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ) {
        const double relativeED = iter->getRelativeED();

        if ( relativeED > largest ) {
            worstMarket = iter;
            largest = relativeED;
        }
    }
    return &*worstMarket;
}
/*! \brief Find the policy solution info.
* \author Josh Lurz, Sonny Kim
* \details This function determines the SolutionInfo within the set which has the largest relative excess demand as defined by
* getRelativeED. 
* \return The SolutionInfo for the policy.
*/
SolutionInfo* SolutionInfoSet::getPolicySolutionInfo() {
    for( SetIterator iter = solvable.begin(); iter != solvable.end(); ++ iter ){
        // TODO: Find a more generic method. 
        if( iter->getName() == "globalCO2" && !iter->isSolved() ){
            return &*iter;
        }
    }
    return 0;
}

//! Return whether all currently solvable markets are bracketed.
bool SolutionInfoSet::isAllBracketed() const {
    for( ConstSetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        if( !iter->isBracketed() ){
            return false;
        }
    }
    return true;
}

//! Return the demands of all SolutionInfo's.
const vector<double> SolutionInfoSet::getDemands() const {
    vector<double> demands;
    for( ConstSetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        demands.push_back( iter->getDemand() );
    }
    for( ConstSetIterator iter = unsolvable.begin(); iter != unsolvable.end(); ++iter ){
        demands.push_back( iter->getDemand() );
    }
    return demands;
}

//! Return the supplies of all SolutionInfo's.
const vector<double> SolutionInfoSet::getSupplies() const {
    vector<double> supplies;
    for( ConstSetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        supplies.push_back( iter->getSupply() );
    }
    for( ConstSetIterator iter = unsolvable.begin(); iter != unsolvable.end(); ++iter ){
        supplies.push_back( iter->getSupply() );
    }
    return supplies;
}


//! Return the number of solvable SolutionInfos.
unsigned int SolutionInfoSet::getNumSolvable() const {
    return static_cast<unsigned int>( solvable.size() );
}

//! Return the total number of SolutionInfos.
unsigned int SolutionInfoSet::getNumTotal() const {
    return static_cast<unsigned int>( solvable.size() + unsolvable.size() );
}

//! Const getter which references the solvable vector.
const SolutionInfo& SolutionInfoSet::getSolvable( unsigned int index ) const {
    return solvable.at( index );
}

//! Get the solvable set (may not be solved).
vector<SolutionInfo> SolutionInfoSet::getSolvableSet() const{
    return solvable;
}

//! Get the unsolvable set
vector<SolutionInfo> SolutionInfoSet::getUnsolvableSet() const {
    return unsolvable;
}

//! Non-Const getter which references the solvable vector.
SolutionInfo& SolutionInfoSet::getSolvable( unsigned int index ) {
    return solvable.at( index );
}

//! Get the solved set.
vector<SolutionInfo> SolutionInfoSet::getSolvedSet() const{
    vector<SolutionInfo> solvedSet;
    for( ConstSetIterator currInfo = solvable.begin(); currInfo != solvable.end(); ++currInfo ){
        if( currInfo->shouldSolve( false ) && currInfo->isSolved() ){
            solvedSet.push_back( *currInfo );
        }
    }
    for( ConstSetIterator currInfo = unsolvable.begin(); currInfo != unsolvable.end(); ++currInfo ){
        if( currInfo->shouldSolve( false ) && currInfo->isSolved() ){
            solvedSet.push_back( *currInfo );
        }
    }
    return solvedSet;
}

//! Const getter which references the unsolved vector.
SolutionInfo& SolutionInfoSet::getUnsolved( unsigned int index ) {
    unsolved.clear();
    for( SetIterator curr = solvable.begin(); curr != solvable.end(); ++curr ){
        if( !curr->isSolved() ){
            unsolved.push_back( *curr );
        }
    }
    return unsolved.at( index );
}
 
//! Get the unsolved set.
vector<SolutionInfo> SolutionInfoSet::getUnsolvedSet() const {
    vector<SolutionInfo> unsolvedSet;
    for( ConstSetIterator currInfo = solvable.begin(); currInfo != solvable.end(); ++currInfo ){
        if( currInfo->shouldSolve( false ) && !currInfo->isSolved() ){
            unsolvedSet.push_back( *currInfo );
        }
    }
    for( ConstSetIterator currInfo = unsolvable.begin(); currInfo != unsolvable.end(); ++currInfo ){
        if( currInfo->shouldSolve( false ) && !currInfo->isSolved() ){
            unsolvedSet.push_back( *currInfo );
        }
    }
    return unsolvedSet;
}

//! Const getter which references the solvable and unsolvable vectors.
const SolutionInfo& SolutionInfoSet::getAny( unsigned int index ) const {
    if( index < solvable.size() ){
        return solvable.at( index );
    }
    return unsolvable.at( index - solvable.size() );
}

//! Non-Const getter which references the solvable and unsolvable vectors.
SolutionInfo& SolutionInfoSet::getAny( unsigned int index ) {
    if( index < solvable.size() ){
        return solvable.at( index );
    }
    return unsolvable.at( index - solvable.size() );
}

//! Check if there are any unsolved singular markets.
bool SolutionInfoSet::hasSingularUnsolved(){
    // Check solvable first
    for( SetIterator curr = solvable.begin(); curr != solvable.end(); ++curr ){
        if( curr->isUnsolvedAndSingular() ){
            return true;
        }
    }
    
    // Check unsolvable as well, they should have cleared
    for( SetIterator curr = unsolvable.begin(); curr != unsolvable.end(); ++curr ){
        if( curr->isUnsolvedAndSingular() ){
            return true;
        }
    }
    return false;
}
//! Check if every SolutionInfo is solved. Gets information from the markets.
bool SolutionInfoSet::isAllSolved(){
    // Check solvable first
    for( SetIterator curr = solvable.begin(); curr != solvable.end(); ++curr ){
        if( !curr->isSolved() ){
            return false;
        }
    }
    
    // Check unsolvable as well, they should have cleared
    for( SetIterator curr = unsolvable.begin(); curr != unsolvable.end(); ++curr ){
        if( !curr->isSolved() ){
            return false;
        }
    }
    return true;
}

/*! \brief Print all unsolved markets.
* \details Searches through the lists of solvable and unsolvable markets and
*          determines if any are not solved For all markets that are unsolved
*          a set of information is printed.
* \param aOut Output stream to which to write warnings.
*/
void SolutionInfoSet::printUnsolved( ostream& aOut ) {
    aOut << "Currently Unsolved Markets: " << endl;

    // Unsolved Part 1:
    aOut << "Unsolved Part 1: Solvable Markets" << endl;
    aOut.setf(ios_base::left,ios_base::adjustfield); // left alignment
    aOut.width( 10 ); aOut << "X,"; aOut << " ";
    aOut.width( 10 ); aOut << "XL,"; aOut << " ";
    aOut.width( 10 ); aOut << "XR,"; aOut << " ";
    aOut.width( 10 ); aOut << "ED,"; aOut << " ";
    aOut.width( 10 ); aOut << "EDL,"; aOut << " ";
    aOut.width( 10 ); aOut << "EDR,"; aOut << " ";
    aOut.width( 10 ); aOut << "RED,"; aOut << " ";
    aOut.width( 3 ); aOut << "brk,"; aOut << " ";
    aOut.width( 10 ); aOut << "Supply,"; aOut << " ";
    aOut.width( 10 ); aOut << "Demand,"; aOut << " ";
    aOut.width( 10 ); aOut << "Mrk Type," << " ";
    aOut.width( 36 ); aOut << "Market,"; aOut << endl;
    aOut.setf(ios_base::fmtflags( 0 ),ios_base::floatfield); //reset to default
    // Solvable markets that are not solved.
    for( SetIterator curr = solvable.begin(); curr != solvable.end(); ++curr ){
        if( !curr->isSolved() ){
            aOut << *curr << endl;
        }
    }
    
    // Unsolved Part 2:
    aOut << "Unsolved Part 2: Unsolvable Markets Not Cleared" << endl;
    aOut.setf(ios_base::left,ios_base::adjustfield); // left alignment
    aOut.width( 10 ); aOut << "X,"; aOut << " ";
    aOut.width( 10 ); aOut << "XL,"; aOut << " ";
    aOut.width( 10 ); aOut << "XR,"; aOut << " ";
    aOut.width( 10 ); aOut << "ED,"; aOut << " ";
    aOut.width( 10 ); aOut << "EDL,"; aOut << " ";
    aOut.width( 10 ); aOut << "EDR,"; aOut << " ";
    aOut.width( 10 ); aOut << "RED,"; aOut << " ";
    aOut.width( 3 ); aOut << "brk,"; aOut << " ";
    aOut.width( 10 ); aOut << "Supply,"; aOut << " ";
    aOut.width( 10 ); aOut << "Demand,"; aOut << " ";
    aOut.width( 10 ); aOut << "Mrk Type," << " ";
    aOut.width( 36 ); aOut << "Market,"; aOut << endl;
    aOut.setf(ios_base::fmtflags( 0 ),ios_base::floatfield); //reset to default
    // Unsolvable markets that are not cleared.
    for( SetIterator curr = unsolvable.begin(); curr != unsolvable.end(); ++curr ){
        if( !curr->isSolved() ){
            aOut << *curr << endl;
        }
    }
    aOut << endl; // next line
}

void SolutionInfoSet::unsetBisectedFlag(){
   for( SetIterator curr = solvable.begin(); curr != solvable.end(); ++curr ){
        curr->unsetBisectedFlag();
    }
    
    // Check unsolvable as well, they should have cleared
    for( SetIterator curr = unsolvable.begin(); curr != unsolvable.end(); ++curr ){
        curr->unsetBisectedFlag();
    }
}

//! Print out all the SolutionInfo objects' information.
void SolutionInfoSet::print( ostream& out ) const {
    out << endl << "X, XL, XR, ED, EDL, EDR, RED, bracketed, supply, demand, MRK type, Market" << endl;
    for( ConstSetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        out << *iter << endl;
    }
}

/*! \brief Utility function to print out market information for a specified market.
*
* Function useful for debugging. A series of these printouts for a specified market 
* can be turned on from the configuration file
*
* \author Steve Smith
* \param aLocation String describing from where the market is being printed.
* \param aCalcCount Iteration count.
* \param aOut Output stream.
*/
void SolutionInfoSet::printMarketInfo( const string& aLocation,
                                       const double aCalcCount,
                                       ostream& aOut ) const
{
    // Use statics here to avoid reinitialization.
    const static Configuration* conf = Configuration::getInstance();
    const static string monitorMarketGoodName = conf->getString( "monitorMktGood", "", false );

    if( !monitorMarketGoodName.empty() ){
        const static string monitorMktGood = conf->getString( "monitorMktName" )
                                             + monitorMarketGoodName;
        
        // TODO: Use find_if.
        for( ConstSetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
            if ( iter->getName() == monitorMktGood ) {
                aOut << "Iter: " << aCalcCount << ". " << *iter << " at "
                     << aLocation << endl;
                return;
            }
        } // end for loop

        for( ConstSetIterator iter = unsolvable.begin(); iter != unsolvable.end(); ++iter ){
            if ( iter->getName() == monitorMktGood ) {
                aOut << "Iter: " << aCalcCount << ". " << *iter << " at "
                     << aLocation << endl;
                return;
            }
        } // end for loop
    }
}

/*! \brief Find and print supply-demand curves for unsolved markets.
*
* This function determines the n worst markets, where n is defined by the configuration file, 
* and creates a SupplyDemandCurve for each. It then instructs the SupplyDemandCurve to calculate the 
* supply and demand at a series of prices, and to print the resulting curve.
*
* \author Josh Lurz
* \param aWorld The world to use to calculate new points.
* \param aMarketplace The marketplace to use to calculate new points.
* \param aPeriod Period for which to print supply-demand curves.
* \param aLogger Logger stream to print the curves to.
*/
void SolutionInfoSet::findAndPrintSD( World* aWorld, Marketplace* aMarketplace, const int aPeriod, ILogger& aLogger ) {
    const Configuration* conf = Configuration::getInstance();
    const int numMarketsToFindSD = conf->getInt( "numMarketsToFindSD", 5 );
    const int numPointsForSD = conf->getInt( "numPointsForSD", 5 );
    
    // Sort the vector so the worst markets are first.
    sort( solvable.begin(), solvable.end(), SolutionInfo::GreaterRelativeED() );

    // Now determine supply and demand curves for each.
    for ( int i = 0; i < numMarketsToFindSD; ++i ) {
        // If its solved, skip it.
        if( solvable[ i ].isSolved() ){
            continue;
        }
        SupplyDemandCurve sdCurve = solvable[ i ].createSDCurve();
        sdCurve.calculatePoints( numPointsForSD, aWorld, aMarketplace, aPeriod );
        sdCurve.print( aLogger );
    }
}

//! Print the derivatives.
void SolutionInfoSet::printDerivatives( ostream& aOut ) const {
    aOut << "Market";
    for( ConstSetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        aOut << "," << iter->getName();
    }
    aOut << endl;
    for( ConstSetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        iter->printDerivatives( aOut );
    }
    aOut << endl;
}

/*! \brief Return a vector of market IDs 
 * \details The caller has to supply the vector to store the ID
 *          values.  The vector will be resized if necessary.  The
 *          return value is a const reference to the argument vector.
 *          (This is so you can use a call to this function as an
 *          rvalue.)
 *
 *          If the second argument is 'true', then only the solvable
 *          markets will be provided.  If 'false', then ids for all
 *          markets, solvable and unsolvable will be provided.
 */
const std::vector<int> &SolutionInfoSet::getMarketIDs(std::vector<int> &aMktids, bool aSolvableOnly) const
{
    unsigned expected_size;
    if( aSolvableOnly ) {
        expected_size = solvable.size();
    }
    else {
        expected_size = solvable.size() + unsolvable.size();
    }
    
    if(aMktids.size() != expected_size) {
        aMktids.resize( expected_size );
    }

    for(unsigned i=0; i<solvable.size(); ++i) {
        aMktids[i] = solvable[i].getSerialNumber();
    }

    if(!aSolvableOnly) {
        for(unsigned i=solvable.size(), j=0; j<unsolvable.size(); ++i,++j) {
            aMktids[i] = unsolvable[j].getSerialNumber();
        }
    }

    return aMktids;
}

