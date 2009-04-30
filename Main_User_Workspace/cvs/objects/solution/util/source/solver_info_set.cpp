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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
* \file solver_info_set.cpp
* \ingroup Solution
* \brief SolverInfoSet class source file.
* \author Josh Lurz, Sonny Kim
*/
#include "util/base/include/definitions.h"
#include <cassert>
#include <algorithm>
#include "util/base/include/util.h"
#include "solution/util/include/solver_info_set.h"
#include "solution/util/include/solver_info.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/supply_demand_curve.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"

using namespace std;

//! Constructor
SolverInfoSet::SolverInfoSet( Marketplace* aMarketplace ):
marketplace( aMarketplace ),
period( 0 )
{
    /*!\pre Marketplace is not null. */
    assert( aMarketplace );
}

//! Constructor for new solution set
SolverInfoSet::SolverInfoSet( const vector<SolverInfo> aSolutionSet ): solvable( aSolutionSet )
{
}

//! Initialize the SolverInfoSet and its SolverInfo's.
void SolverInfoSet::init( const unsigned int period ) {
    assert( period >= 0 );
    this->period = period;

    // Print a debugging log message.
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::DEBUG );
    solverLog << "Initializing the solvable set." << endl;

    // Request the markets to solve from the marketplace. 
    vector<Market*> marketsToSolve = marketplace->getMarketsToSolve( period );

    // Create and initialize a SolverInfo object for each market.
    typedef vector<Market*>::const_iterator ConstMarketIterator;
    for( ConstMarketIterator iter = marketsToSolve.begin(); iter != marketsToSolve.end(); ++iter ){
        SolverInfo currInfo( *iter );
        currInfo.init();
        if( currInfo.shouldSolve( false ) ){
            solvable.push_back( currInfo );
        }
        else {
            unsolvable.push_back( currInfo );
        }
    }
}

//! Merge two solution sets
void SolverInfoSet::merge( const vector<SolverInfo> aSolutionSet )
{
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::DEBUG );
    // Iterate through passed-in solution set and merge with matching solvable set.
    for( ConstSetIterator newIter = aSolutionSet.begin(); newIter != aSolutionSet.end(); ++newIter ){
        // Find matching original solution, erase and add new solution.
        for( SetIterator currIter = solvable.begin(); currIter != solvable.end(); ++currIter ){
            if( currIter->getName() == newIter->getName() ){
                solvable.erase( currIter );
                solvable.insert( currIter, *newIter );
                solverLog << newIter->getName() << " was merged to the solvable solution set." << endl;
            }
        }
    }
    // Iterate through passed-in solution set and merge with matching unsolvable set.
    for( ConstSetIterator newIter = aSolutionSet.begin(); newIter != aSolutionSet.end(); ++newIter ){
        // Find matching original solution, erase and add new solution.
        for( SetIterator currIter = unsolvable.begin(); currIter != unsolvable.end(); ++currIter ){
            if( currIter->getName() == newIter->getName() ){
                unsolvable.erase( currIter );
                unsolvable.insert( currIter, *newIter );
                solverLog << newIter->getName() << " was merged to the unsolvable solution set." << endl;
            }
        }
    }
    solverLog << endl;
}

//! Update the prices to the marketplace.
void SolverInfoSet::updateToMarkets() {
    // Send each SolverInfo's price to its linked market for solvable markets. 
    for( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        iter->updateToMarket();
    }
    // Update unsolvable as well.
    for( SetIterator iter = unsolvable.begin(); iter != unsolvable.end(); ++iter ){
        iter->updateToMarket();
    }
}

//! Update information from the marketplace.
void SolverInfoSet::updateFromMarkets(){
    // Retrieve information from the linked market.
    for( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        iter->updateFromMarket();
    }
    // Update unsolvable as well.
    for( SetIterator iter = unsolvable.begin(); iter != unsolvable.end(); ++iter ){
        iter->updateFromMarket();
    }
}

//! Update which markets are currently being solved.
SolverInfoSet::UpdateCode SolverInfoSet::updateSolvable( const bool isNR ) {
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
        if( !iter->shouldSolve( isNR ) ){
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
        if( iter->shouldSolve( isNR ) ){
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
void SolverInfoSet::updateElasticities() {
    for( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        iter->calcDemandElas( *this );
        iter->calcSupplyElas( *this );
    }
}
//! Adjust brackets
void SolverInfoSet::adjustBrackets() {
    for( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        iter->adjustBracket();
    }
}

//! Have all contained SolverInfo's store their current values.
void SolverInfoSet::storeValues(){
    // Store values for solvable.
    for( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        iter->storeValues();
    }
    // Store values for unsolvable.
    for( SetIterator iter = unsolvable.begin(); iter != unsolvable.end(); ++iter ){
        iter->storeValues();
    }
}

//! Have all contained SolverInfo's restore their previous values.
void SolverInfoSet::restoreValues(){
    // Retore values for solvable.
    for( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        iter->restoreValues();
    }
    // Store values for unsolvable. 
    for( SetIterator iter = unsolvable.begin(); iter != unsolvable.end(); ++iter ){
        iter->restoreValues();
    }
}

/*! \brief Check if the bracket is empty and reset it if neccessary.
* \return Whether any brackets were reset. 
*/
bool SolverInfoSet::checkAndResetBrackets(){
    bool didReset = false;
    for( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        didReset |= iter->checkAndResetBrackets();
    }
    return didReset;
}

/*! \brief Reset solution set brackets to current prices and excess demands and 
    set bracketed to false.
*/
void SolverInfoSet::resetBrackets(){
    for( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        iter->resetBrackets();
    }
}

//! Find the maximum relative excess demand.
double SolverInfoSet::getMaxRelativeExcessDemand( const double ED_SOLUTION_FLOOR ) const {
    double largest = -1;
    for ( ConstSetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ) {
        const double relativeED = iter->getRelativeED( ED_SOLUTION_FLOOR );

        if ( relativeED > largest ) {
            largest = relativeED;
        }
    }
    return largest;
}

//! Find the maximum absolute excess demand.
double SolverInfoSet::getMaxAbsoluteExcessDemand() const {
    double largest = -1;
    for ( ConstSetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ) {
        const double absoluteExcessDemand = fabs( iter->getED() );
        if( absoluteExcessDemand > largest ){
            largest = absoluteExcessDemand;
        }
    }
    return largest;
}

/*! \brief Finds the SolverInfo with the largest relative excess demand.
* \author Josh Lurz
* \details This function determines the SolverInfo within the set which has the largest relative excess demand as defined by
* getRelativeED. 
* \param ED_SOLUTION_FLOOR Value of ED below which the market should be considered solved. 
* \return The SolverInfo with the largest relative excess demand. 
*/
SolverInfo* SolverInfoSet::getWorstSolverInfo( const double aEDSolutionFloor, const bool aIgnoreBisected ) {

    SetIterator worstMarket = solvable.begin();
    double largest = -1;

    for ( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ) {
        if( aIgnoreBisected && iter->hasBisected() ){
            continue;
        }
        const double relativeED = iter->getRelativeED( aEDSolutionFloor );
        
        if ( relativeED > largest ) {
            worstMarket = iter;
            largest = relativeED;
        }
    }
    return &*worstMarket;
}

/*! \brief Returns the best unsolved solver info. 
* \author Josh Lurz
* \details This function determines the SolverInfo within the set which has the largest relative excess demand as defined by
* getRelativeED. 
* \param aEDSolutionFloor Value of ED below which the market should be considered solved.
* \param aIgnoreBisected Whether to ignore already bisected markets.
* \return The SolverInfo with the smallest unsolved relative excess demand. 
*/
SolverInfo* SolverInfoSet::getWorstSolverInfoReverse( const double aTolerance, const double aEDSolutionFloor, const bool aIgnoreBisected ) {
    
    // Find the worst one. 
    SolverInfo* worstMarket = getWorstSolverInfo( aEDSolutionFloor, aIgnoreBisected );
    double smallest = worstMarket->getRelativeED( aEDSolutionFloor );
    SetIterator bestUnsolved = solvable.begin();

    for ( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ) {
        if( aIgnoreBisected && iter->hasBisected() ){
            continue;
        }
        if( iter->isWithinTolerance( aTolerance, aEDSolutionFloor ) ){ // not right
            continue;
        }
        const double relativeED = iter->getRelativeED( aEDSolutionFloor );
        
        if ( relativeED <= smallest ) {
            bestUnsolved = iter;
            smallest = relativeED;
        }
    }
    return &*bestUnsolved;
}
/*! \brief Find the policy solver info, or the worst if there is no policy.
* \author Josh Lurz
* \details This function determines the SolverInfo within the set which has the largest relative excess demand as defined by
* getRelativeED. 
* \param aEDSolutionFloor Value of ED below which the market should be considered solved. 
* \return The SolverInfo for the policy, or the worst one if that does not exist.
*/
SolverInfo* SolverInfoSet::getPolicyOrWorstSolverInfo( const double aTolerance, const double aEDSolutionFloor) {
    for( SetIterator iter = solvable.begin(); iter != solvable.end(); ++ iter ){
        // TODO: Find a more generic method. 
        if( iter->getName() == "globalCO2" && !iter->isSolved( aTolerance, aEDSolutionFloor ) ){
            return &*iter;
        }
    }

    double largest = -1;
    SetIterator worstMarket = solvable.begin();
    for ( SetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ) {
        const double relativeED = iter->getRelativeED( aEDSolutionFloor );

        if ( relativeED > largest ) {
            worstMarket = iter;
            largest = relativeED;
        }
    }
    return &*worstMarket;
}
/*! \brief Find the policy solver info.
* \author Josh Lurz, Sonny Kim
* \details This function determines the SolverInfo within the set which has the largest relative excess demand as defined by
* getRelativeED. 
* \param aEDSolutionFloor Value of ED below which the market should be considered solved. 
* \return The SolverInfo for the policy.
*/
SolverInfo* SolverInfoSet::getPolicySolverInfo( const double aTolerance, const double aEDSolutionFloor) {
    for( SetIterator iter = solvable.begin(); iter != solvable.end(); ++ iter ){
        // TODO: Find a more generic method. 
        if( iter->getName() == "globalCO2" && !iter->isSolved( aTolerance, aEDSolutionFloor ) ){
            return &*iter;
        }
    }
    return 0;
}

//! Return whether all currently solvable markets are bracketed.
bool SolverInfoSet::isAllBracketed() const {
    for( ConstSetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        if( !iter->isBracketed() ){
            return false;
        }
    }
    return true;
}

//! Return the demands of all SolverInfo's.
const vector<double> SolverInfoSet::getDemands() const {
    vector<double> demands;
    for( ConstSetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        demands.push_back( iter->getDemand() );
    }
    for( ConstSetIterator iter = unsolvable.begin(); iter != unsolvable.end(); ++iter ){
        demands.push_back( iter->getDemand() );
    }
    return demands;
}

//! Return the supplies of all SolverInfo's.
const vector<double> SolverInfoSet::getSupplies() const {
    vector<double> supplies;
    for( ConstSetIterator iter = solvable.begin(); iter != solvable.end(); ++iter ){
        supplies.push_back( iter->getSupply() );
    }
    for( ConstSetIterator iter = unsolvable.begin(); iter != unsolvable.end(); ++iter ){
        supplies.push_back( iter->getSupply() );
    }
    return supplies;
}


//! Return the number of solvable SolverInfos.
unsigned int SolverInfoSet::getNumSolvable() const {
    return static_cast<unsigned int>( solvable.size() );
}

//! Return the total number of SolverInfos.
unsigned int SolverInfoSet::getNumTotal() const {
    return static_cast<unsigned int>( solvable.size() + unsolvable.size() );
}

//! Const getter which references the solvable vector.
const SolverInfo& SolverInfoSet::getSolvable( unsigned int index ) const {
    return solvable.at( index );
}

//! Get the solvable set (may not be solved).
vector<SolverInfo> SolverInfoSet::getSolvableSet() const{
    return solvable;
}

//! Non-Const getter which references the solvable vector.
SolverInfo& SolverInfoSet::getSolvable( unsigned int index ) {
    return solvable.at( index );
}

//! Get the solved set.
vector<SolverInfo> SolverInfoSet::getSolvedSet( const double aSolutionTolerance,
                                  const double aEDSolutionFloor ) const{
    vector<SolverInfo> solvedSet;
    for( ConstSetIterator currInfo = solvable.begin(); currInfo != solvable.end(); ++currInfo ){
        if( currInfo->shouldSolve( false ) && currInfo->isSolved( aSolutionTolerance, aEDSolutionFloor ) ){
            solvedSet.push_back( *currInfo );
        }
    }
    for( ConstSetIterator currInfo = unsolvable.begin(); currInfo != unsolvable.end(); ++currInfo ){
        if( currInfo->shouldSolve( false ) && currInfo->isSolved( aSolutionTolerance, aEDSolutionFloor ) ){
            solvedSet.push_back( *currInfo );
        }
    }
    return solvedSet;
}

//! Const getter which references the unsolved vector.
SolverInfo& SolverInfoSet::getUnsolved( unsigned int index, const double aSolutionTolerance,
                                              const double aEDSolutionFloor  ) {
    unsolved.clear();
    for( SetIterator curr = solvable.begin(); curr != solvable.end(); ++curr ){
        if( !curr->isSolved( aSolutionTolerance, aEDSolutionFloor ) ){
            unsolved.push_back( *curr );
        }
    }
    return unsolved.at( index );
}
 
//! Get the unsolved set.
vector<SolverInfo> SolverInfoSet::getUnsolvedSet( const double aSolutionTolerance,
                                  const double aEDSolutionFloor ) const{
    vector<SolverInfo> unsolvedSet;
    for( ConstSetIterator currInfo = solvable.begin(); currInfo != solvable.end(); ++currInfo ){
        if( currInfo->shouldSolve( false ) && !currInfo->isSolved( aSolutionTolerance, aEDSolutionFloor ) ){
            unsolvedSet.push_back( *currInfo );
        }
    }
    for( ConstSetIterator currInfo = unsolvable.begin(); currInfo != unsolvable.end(); ++currInfo ){
        if( currInfo->shouldSolve( false ) && !currInfo->isSolved( aSolutionTolerance, aEDSolutionFloor ) ){
            unsolvedSet.push_back( *currInfo );
        }
    }
    return unsolvedSet;
}

//! Const getter which references the solvable and unsolvable vectors.
const SolverInfo& SolverInfoSet::getAny( unsigned int index ) const {
    if( index < solvable.size() ){
        return solvable.at( index );
    }
    return unsolvable.at( index - solvable.size() );
}

//! Non-Const getter which references the solvable and unsolvable vectors.
SolverInfo& SolverInfoSet::getAny( unsigned int index ) {
    if( index < solvable.size() ){
        return solvable.at( index );
    }
    return unsolvable.at( index - solvable.size() );
}

//! Check if there are any unsolved singular markets.
bool SolverInfoSet::hasSingularUnsolved( const double aSolTolerance, const double aEDSolutionFloor ){
    // Check solvable first
    for( SetIterator curr = solvable.begin(); curr != solvable.end(); ++curr ){
        if( curr->isUnsolvedAndSingular( aSolTolerance, aEDSolutionFloor ) ){
            return true;
        }
    }
    
    // Check unsolvable as well, they should have cleared
    for( SetIterator curr = unsolvable.begin(); curr != unsolvable.end(); ++curr ){
        if( curr->isUnsolvedAndSingular( aSolTolerance, aEDSolutionFloor ) ){
            return true;
        }
    }
    return false;
}
//! Check if every SolverInfo is solved. Gets information from the markets.
bool SolverInfoSet::isAllSolved( const double SOLUTION_TOLERANCE, const double ED_SOLUTION_FLOOR ){
    // Check solvable first
    for( SetIterator curr = solvable.begin(); curr != solvable.end(); ++curr ){
        if( !curr->isSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) ){
            return false;
        }
    }
    
    // Check unsolvable as well, they should have cleared
    for( SetIterator curr = unsolvable.begin(); curr != unsolvable.end(); ++curr ){
        if( !curr->isSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) ){
            return false;
        }
    }
    return true;
}

/*! \brief Print all unsolved markets.
* \details Searches through the lists of solvable and unsolvable markets and
*          determines if any are not solved given aSolutionTolerance and
*          aEDSolutionFloor. For all markets that are unsolved a set of
*          information is printed.
* \param aSolutionTolerance Solution tolerance to check against.
* \param aEDSolutionFloor Excess demand solution floor to check against.
* \param aOut Output stream to which to write warnings.
*/
void SolverInfoSet::printUnsolved( const double aSolutionTolerance,
                                   const double aEDSolutionFloor,
                                   ostream& aOut )
{
    aOut << "Currently Unsolved Markets: " << endl;

    // Unsolved Part 1:
    aOut << "Unsolved Part 1: Solvable Markets" << endl;
    aOut.setf(ios_base::left,ios_base::adjustfield); // left alignment
    aOut.width(36); aOut << "Market,"; aOut << " ";
    aOut.width(10); aOut << "X,"; aOut << " ";
    aOut.width(10); aOut << "XL,"; aOut << " ";
    aOut.width(10); aOut << "XR,"; aOut << " ";
    aOut.width(10); aOut << "ED,"; aOut << " ";
    aOut.width(10); aOut << "EDL,"; aOut << " ";
    aOut.width(10); aOut << "EDR,"; aOut << " ";
    aOut.width(10); aOut << "RED,"; aOut << " ";
    aOut.width(3); aOut << "brk,"; aOut << " ";
    aOut.width(10); aOut << "Supply,"; aOut << " ";
    aOut.width(10); aOut << "Demand,"; aOut << " ";
    aOut.width(10); aOut << "Mrk Type," << endl;
    aOut.setf(ios_base::fmtflags(0),ios_base::floatfield); //reset to default
    // Solvable markets that are not solved.
    for( SetIterator curr = solvable.begin(); curr != solvable.end(); ++curr ){
        if( !curr->isSolved( aSolutionTolerance, aEDSolutionFloor ) ){
            aOut << *curr << endl;
        }
    }
    
    // Unsolved Part 2:
    aOut << "Unsolved Part 2: Unsolvable Markets Not Cleared" << endl;
    aOut.setf(ios_base::left,ios_base::adjustfield); // left alignment
    aOut.width(36); aOut << "Market,"; aOut << " ";
    aOut.width(10); aOut << "X,"; aOut << " ";
    aOut.width(10); aOut << "XL,"; aOut << " ";
    aOut.width(10); aOut << "XR,"; aOut << " ";
    aOut.width(10); aOut << "ED,"; aOut << " ";
    aOut.width(10); aOut << "EDL,"; aOut << " ";
    aOut.width(10); aOut << "EDR,"; aOut << " ";
    aOut.width(10); aOut << "RED,"; aOut << " ";
    aOut.width(3); aOut << "brk,"; aOut << " ";
    aOut.width(10); aOut << "Supply,"; aOut << " ";
    aOut.width(10); aOut << "Demand,"; aOut << " ";
    aOut.width(10); aOut << "Mrk Type," << endl;
    aOut.setf(ios_base::fmtflags(0),ios_base::floatfield); //reset to default
    // Unsolvable markets that are not cleared.
    for( SetIterator curr = unsolvable.begin(); curr != unsolvable.end(); ++curr ){
        if( !curr->isSolved( aSolutionTolerance, aEDSolutionFloor ) ){
            aOut << *curr << endl;
        }
    }
    aOut << endl; // next line
}

void SolverInfoSet::unsetBisectedFlag(){
   for( SetIterator curr = solvable.begin(); curr != solvable.end(); ++curr ){
        curr->unsetBisectedFlag();
    }
    
    // Check unsolvable as well, they should have cleared
    for( SetIterator curr = unsolvable.begin(); curr != unsolvable.end(); ++curr ){
        curr->unsetBisectedFlag();
    }
}

//! Print out all the SolutionInfo objects' information.
void SolverInfoSet::print( ostream& out ) const {
    out << endl << "Market, X, XL, XR, ED, EDL, EDR, RED, bracketed, supply, demand, MRK type" << endl;
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
void SolverInfoSet::printMarketInfo( const string& aLocation,
                                     const double aCalcCount,
                                     ostream& aOut ) const
{
    // Use statics here to avoid reinitialization.
    const static Configuration* conf = Configuration::getInstance();
    const static string monitorMarketGoodName = conf->getString( "monitorMktGood" );

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
* \param aEDTolerance The ED solution tolerance. 
* \param aDemandFloor The demand floor of the relative ED function.
* \param aWorld The world to use to calculate new points.
* \param aMarketplace The marketplace to use to calculate new points.
* \param aPeriod Period for which to print supply-demand curves.
* \param aLogger Logger stream to print the curves to.
*/
void SolverInfoSet::findAndPrintSD( const double aEDTolerance, const double aDemandFloor, World* aWorld, Marketplace* aMarketplace, const int aPeriod, ILogger& aLogger ) {
    const Configuration* conf = Configuration::getInstance();
    const int numMarketsToFindSD = conf->getInt( "numMarketsToFindSD", 5 );
    const int numPointsForSD = conf->getInt( "numPointsForSD", 5 );
    
    // Sort the vector so the worst markets are first.
    sort( solvable.begin(), solvable.end(), SolverInfo::GreaterRelativeED( aDemandFloor ) );

    // Now determine supply and demand curves for each.
    for ( int i = 0; i < numMarketsToFindSD; ++i ) {
        // If its solved, skip it.
        if( solvable[ i ].isSolved( aEDTolerance, aDemandFloor ) ){
            continue;
        }
        SupplyDemandCurve sdCurve = solvable[ i ].createSDCurve();
        sdCurve.calculatePoints( numPointsForSD, aWorld, aMarketplace, aPeriod );
        sdCurve.print( aLogger );
    }
}

//! Print the derivatives.
void SolverInfoSet::printDerivatives( ostream& aOut ) const {
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
