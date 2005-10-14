/*! 
* \file solver_info.cpp
* \ingroup Solution
* \brief SolverInfo class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include "util/base/include/definitions.h"
#include "util/base/include/util.h"
#include <string>
#include <iostream>
#include <cassert>
#include "solution/util/include/solver_info.h"
#include "solution/util/include/solver_info_set.h"
#include "solution/util/include/solver_library.h"
#include "marketplace/include/market.h"
#include "util/base/include/supply_demand_curve.h"
#include "util/logger/include/ilogger.h"

using namespace std;

//! Constructor
SolverInfo::SolverInfo( Market* aLinkedMarket ){
    assert( aLinkedMarket );
    // initialize the data members.
    linkedMarket = aLinkedMarket;
    X = 0;
    storedX = 0;
    demand = 0;
    storedDemand = 0;
    supply = 0;
    storedSupply = 0;
    XL = 0;
    XR = 0;
    EDL = 0;
    EDR = 0;
    bracketed = false;
    mBisected = false;
}

//! Equals operator based on the equality of the linked market.
bool SolverInfo::operator==( const SolverInfo& rhs ) const {
    return ( linkedMarket == rhs.linkedMarket );
}

//! Not equals operator based on the equals operator.
bool SolverInfo::operator!=( const SolverInfo& rhs ) const {
    return !( *this == rhs );
}

//! Initialize the SolverInfo.
void SolverInfo::init(){
    // Update price, supply and demand from the market.
    X = linkedMarket->getRawPrice();
    supply = linkedMarket->getRawSupply();
    demand = linkedMarket->getRawDemand();
    resetBrackets();
}

/*! \brief Return whether the SolverInfo has had its bracketed flag set. 
* \note this does not mean it is still within the bracket.
* \return Whether the bracketed flag has been set.
*/
bool SolverInfo::isBracketed() const {
    return bracketed;
}

/*! \brief Set the SolverInfos bracketed flag.
*/
void SolverInfo::setBracketed(){
    bracketed = true;
}

//! Return the size of the bracket.
double SolverInfo::getBracketSize() const {
    return fabs( XR - XL );
}

/*! \brief Get the price */
double SolverInfo::getPrice() const {
    return X;
}

//! Set the price
void SolverInfo::setPrice( const double aPrice ){
   X = aPrice;
}

//! Set the price to the median value between the left and right bracket.
void SolverInfo::setPriceToCenter(){
    X = ( XL + XR ) / 2;
}

/*! \brief Get Demand */
double SolverInfo::getDemand() const {
    return demand;
}

//! Remove a given amount from the raw demand for the linked market.
void SolverInfo::removeFromRawDemand( const double aDemand ){
    linkedMarket->removeFromRawDemand( aDemand );
}


/*! \brief Get the Supply */
double SolverInfo::getSupply() const {
    return supply;
}

//! Remove a given amount from the raw supply for the linked market.
void SolverInfo::removeFromRawSupply( const double aSupply ){
    linkedMarket->removeFromRawSupply( aSupply );
}

//! Get the Excess Demand, calculated dynamically.
double SolverInfo::getED() const {
    return demand - supply;
}

//! Get the ED at the left bracket.
double SolverInfo::getEDLeft() const {
    return EDL;
}

//! Get the ED at the right bracket.
double SolverInfo::getEDRight() const {
    return EDR;
}

//! Store X, demand and supply so that they can later be used to calculate derivatives.
void SolverInfo::storeValues() {
    storedX = X;
    storedDemand = demand;
    storedSupply = supply;
}

//! Restore X, demand and supply.
void SolverInfo::restoreValues() {
    X = storedX;
    demand = storedDemand;
    supply = storedSupply;
    linkedMarket->setRawDemand( demand );
    linkedMarket->setRawSupply( supply );
    linkedMarket->setRawPrice( X );
}

/*! \brief Return the name of the SolutionInfo object.
* \author Josh Lurz
* \return The name of the market the SolutionInfo is connected to.
*/
const string& SolverInfo::getName() const {
    return linkedMarket->getName();
}

/*! \brief Sets the price contained in the Solver into its corresponding market.
* \author Josh Lurz
* \details This function sets the market prices from the prices in the SolverInfo
*/
void SolverInfo::updateToMarket() {
    linkedMarket->setRawPrice( X );
}

/*! \brief Get the demands, supplies, prices and excess demands from the market and set them into their corresponding places in the SolverInfo.
* \author Josh Lurz
* \details This function gets the price, supply and demand out the market and sets those values into the SolverInfo
* object. This function also updates the ED value to one based on the retrieved supply and demand.
*/
void SolverInfo::updateFromMarket() {
    X = linkedMarket->getRawPrice();
    supply = linkedMarket->getRawSupply();
    demand = linkedMarket->getRawDemand();
}

/*! \brief Determines if a price or demand market has been unbracketed and attempts to restore it back to a bracketed state. 
* \details This function checks if the linked market is a PriceMarket or DemandMarket and is unbracketed. It then adjust the brackets
* to attempt to bring the market into a bracketed state.
* \note This was originally a kludge and has never been replaced.
* \author Josh Lurz
*/
void SolverInfo::adjustBracket() {
    // How much to adjust brackets by.
    const static double ADJUSTMENT_FACTOR = 1.5;

    // Adjust the brackets if it is a price or demand market. 
    const string marketType = linkedMarket->getType();
    if( marketType == "PriceMarket" || marketType == "DemandMarket" ){
        double rawDemand = linkedMarket->getRawDemand();

        if( XL < rawDemand ) {
            XL = rawDemand * ADJUSTMENT_FACTOR;
        }
        if( XR > rawDemand ) {
            XR = rawDemand / ADJUSTMENT_FACTOR;
        }
    }
}

// Expand the bracket slightly.
void SolverInfo::expandBracket( const double aAdjFactor ) {
    XL *= aAdjFactor;
    XR /= aAdjFactor;
}

/*! \brief Get the relativeED 
* \param ED_SOLUTION_FLOOR Absolute value of ED below which the market should be considered solved.
* \return The relative excess demand. 
*/
double SolverInfo::getRelativeED( const double ED_SOLUTION_FLOOR ) const {
    return SolverLibrary::getRelativeED( getED(), demand, ED_SOLUTION_FLOOR );
}

/*! \brief Determine whether a market is within the solution tolerance. 
* \author Josh Lurz
* \details This function determines if a market is solved to within the solution tolerance. It does this by checking if the 
* relative excess demand is less than the solution tolerance. 
* \param SOLUTION_TOLERANCE The relative excess demand below which a market is considered solved. 
* \param ED_SOLUTION_FLOOR Absolute value of ED below which the market should be considered solved.
* \return Whether the market is within the solution tolerance. 
*/
bool SolverInfo::isWithinTolerance( const double SOLUTION_TOLERANCE, const double ED_SOLUTION_FLOOR ) const {
   return ( getRelativeED( ED_SOLUTION_FLOOR ) < SOLUTION_TOLERANCE );
}

//! Determine whether a SolverInfo is solvable for the current method.
bool SolverInfo::shouldSolve( const bool isNR ) const {
	return isNR ? linkedMarket->shouldSolveNR() : linkedMarket->shouldSolve();
}

/*! \brief Calculate the log change in raw price.
* \author Josh Lurz
* \details This function determines the change in the log of the price and the stored price.
* It will return a very small number if either price is zero.
* \return The change in the logs of the price and stored price.
*/
double SolverInfo::getLogChangeInRawPrice() const {
   double change = 0;
   // Case 1: price or Previous price is zero.
   if( storedX == 0 || X == 0 ) {
      change = util::getVerySmallNumber();
   }
   // Case 2: price and Previous price are both positive.
   else if( ( X > 0 ) && ( storedX > 0 ) ) {
      change = log( X ) - log( storedX );
   }
   // Case 3: price or Previous price is negative. This should not occur.
   else {
      assert( false );
   }
   // Check for a change of 0. Should this use a very very small number?
   if( change == 0 ){
       change = util::getVerySmallNumber();
   }
   return change;
}

/*! \brief Calculate the log change in raw demand.
* \author Josh Lurz
* \details This function determines the change in the log of the demand and the stored demand.
* It will return a very small number if either demand is zero.
* \return The change in the logs of the demand and stored demand.
*/
double SolverInfo::getLogChangeInRawDemand() const {
   double change = 0;

   // Case 1: Demand or Previous Demand is zero.
   if( storedDemand == 0 || demand == 0 ) {
      change = util::getVerySmallNumber();
   }
   // Case 2: Demand and Previous Demand are both positive.
   else if( ( demand > 0 ) && ( storedDemand > 0 ) ) {
      change = log( demand ) - log( storedDemand );
   }
   // Case 3: Demand or Previous Demand is negative. This should not occur.
   else {
      assert( false );
   }
   return change;
}

/*! \brief Calculate the log change in raw supply.
* \author Josh Lurz
* \details This function determines the change in the log of the supply and the stored supply.
* It will return a very small number if either supply is zero.
* \return The change in the logs of the supply and stored supply.
*/
double SolverInfo::getLogChangeInRawSupply() const {
   double change = 0;

   // Case 1: supply or Previous supply is zero.
   if( storedSupply == 0 || supply == 0 ) {
      change = util::getVerySmallNumber();
   }
   // Case 2: supply and Previous supply are both positive.
   else if( ( supply > 0 ) && ( storedSupply > 0 ) ) {
      change = log( supply ) - log( storedSupply );
   }
   // Case 3: supply or Previous supply is negative. This should not occur.
   else {
      assert( false );
   }
   return change;
}

/*! \brief Calculate demand elasticities relative to the current SolverInfo
* \author Sonny Kim
* \details Calculate the elasticities of demand for all markets relative to the linked market. 
* If the price is 0 the cross-elasticity will be set to a very small number.
* \param solvableMarkets Vector of SolutionInfo objects to use to calculate demand elasticities.
*/
void SolverInfo::calcDemandElas( const SolverInfoSet& solvableMarkets ) {
    // TODO: I think a map would make it easier to only update a single cross-derivative. 
    const double dprice = getLogChangeInRawPrice();
    demandElasticities.resize( solvableMarkets.getNumSolvable() );
    
    for ( unsigned int i = 0; i < solvableMarkets.getNumSolvable(); ++i ) {
        string marketName = solvableMarkets.getSolvable( i ).getName();
        double ddemand = solvableMarkets.getSolvable( i ).getLogChangeInRawDemand();
        demandElasticities[ i ] = ddemand / dprice;
        assert( util::isValidNumber( demandElasticities[ i ] ) );
    }
}

/*! \brief Calculate supply elasticities.
* \author Sonny Kim
* \details Calculate the elasticities of supply for all markets relative to the current SolverInfo. 
* If the price is 0 the elasticity will be set to a very small number.
* \param solvableMarkets Vector of SolutionInfo objects to use to calculate supply elasticities.
*/
void SolverInfo::calcSupplyElas( const SolverInfoSet& solvableMarkets ) {
   const double dprice = getLogChangeInRawPrice();
   supplyElasticities.resize( solvableMarkets.getNumSolvable() );

   for ( unsigned int i = 0; i < solvableMarkets.getNumSolvable(); ++i ) {
      double dsupply = solvableMarkets.getSolvable( i ).getLogChangeInRawSupply();
      supplyElasticities[ i ] = dsupply / dprice;
      assert( util::isValidNumber( supplyElasticities[ i ] ) );
   }
}

/*! \brief This function checks for any SolutionInfo objects that are converging too slowly
* and resets the bracket.
* \return Whether the market was rebracketed 
*/
bool SolverInfo::checkAndResetBrackets(){
    // try rebracketing if the current bracket is empty.
    // if ( getBracketSize() < util::getSmallNumber() ) {
    if( !isWithinTolerance( 0.001 * 10, 0.01 ) ){ // TEMP HARDCODING
        resetBrackets();
        return true;
    }
    return false;
}

//! Increase price by 1+multiplier, or set it to lowerBound if it is below the lower bound.
void SolverInfo::increaseX( const double multiplier, const double lowerBound ){
    if( X >= lowerBound ) {
        X *= ( 1 + multiplier );
    }
    else if ( fabs( X ) < lowerBound ) {
        X = lowerBound;
    }
    else { // X is negative.
        assert( false );
    }
}

//! Decrease price by 1+multiplier, or set it to 0 if it is below the lower bound.
void SolverInfo::decreaseX( const double multiplier, const double lowerBound ){
    if( X >= lowerBound ) {
        X *= ( 1 - multiplier );
    }
    else if ( fabs( X ) < lowerBound ) {
        X = 0;
    }
    else {
        assert( false );
    }
}
//! Set the right bracket to X and right bracket ED to the current value of ED.
void SolverInfo::moveRightBracketToX(){
    XR = X;
    EDR = getED();
}

//! Set the left bracket to X and left bracket ED to the current value of ED.
void SolverInfo::moveLeftBracketToX(){
    XL = X; 
    EDL = getED();
}

//! Reset left and right bracket to X.
void SolverInfo::resetBrackets(){
    bracketed = false;
    moveRightBracketToX();
    moveLeftBracketToX();
}

//! Dynamic check to see if the brackets span X.
bool SolverInfo::isCurrentlyBracketed() const {
    return util::sign( EDR ) != util::sign( EDL );
}

//! Check if the market has correctly solved.
bool SolverInfo::isSolved( const double SOLUTION_TOLERANCE, const double ED_SOLUTION_FLOOR ) const {
    // Check if either the market is within tolerance or for a special market
    // type dependent solution condition.
    return ( isWithinTolerance( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR )
		     || linkedMarket->meetsSpecialSolutionCriteria() );
}

//! Get the demand elasticity of this market with respect to another market.
// This should be replaced with a map and market name.
double SolverInfo::getDemandElasWithRespectTo( const unsigned int aMarketNumber ) const {
    if( aMarketNumber > demandElasticities.size() ){
        ILogger& mainLog = ILogger::getLogger( "solver_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Invalid market number in SolverInfo::getDemandElasWithRespectTo of : " << aMarketNumber << endl;
        return 0;
    }
    return demandElasticities.at( aMarketNumber );
}

//! Return the list of regions contained in the linked market for this SolverInfo.
// Probably too much linkages here, not sure how to fix.
const vector<const objects::Atom*>& SolverInfo::getContainedRegions() const {
    return linkedMarket->getContainedRegions();
}

//! Get the supply elasticity of this market with respect to another market.
// This should be replaced with a map and market name.
double SolverInfo::getSupplyElasWithRespectTo( const unsigned int aMarketNumber ) const {
    if( aMarketNumber > supplyElasticities.size() ){
        ILogger& solverLog = ILogger::getLogger( "solver_log" );
        solverLog.setLevel( ILogger::ERROR );
        solverLog << "Invalid market number in SolverInfo::getSupplyElasWithRespectTo of : " << aMarketNumber << endl;
        return 0;
    }
    return supplyElasticities.at( aMarketNumber );
}

//! Return whether this market is currently unsolved and has a singularity. 
bool SolverInfo::isUnsolvedAndSingular( const double aSolTolerance, const double aSolFloor ){
    // Check if the market is not solved, is solved under bisection and is not solved under NR.
    return( !isSolved( aSolTolerance, aSolFloor ) && shouldSolve( false ) && !shouldSolve( true ) );
}

void SolverInfo::setBisectedFlag(){
    mBisected = true;
}

void SolverInfo::unsetBisectedFlag(){
    mBisected = false;
}

bool SolverInfo::hasBisected() const {
    return mBisected;
}
//! Print out information from the SolverInfo to a file.
void SolverInfo::print( ostream& out ) const {
    out << getName() << " X: " << X << " XL: " << XL << " XR: " << XR << " ED: " << getED() << " EDR: " << EDR << " EDL: " << EDL << " RED: " << getRelativeED( 0 ) << " S: " << supply << " D: " << demand;
}

SupplyDemandCurve SolverInfo::createSDCurve(){
    return SupplyDemandCurve( linkedMarket );
}

void SolverInfo::printDerivatives( ostream& aOut ) const {
    aOut << getName() << " SupplyDer: ";
    for( unsigned int i = 0; i < supplyElasticities.size(); ++i ){
        aOut << "," << supplyElasticities[ i ];
    }
    aOut << endl;
        aOut << getName() << " DemandDer: ";
    for( unsigned int i = 0; i < demandElasticities.size(); ++i ){
        aOut << "," << demandElasticities[ i ];
    }
    aOut << endl;
}
