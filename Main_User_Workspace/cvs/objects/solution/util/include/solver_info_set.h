#ifndef _SOLVER_INFO_SET_H_
#define _SOLVER_INFO_SET_H_
#if defined(_MSC_VER)
#pragma once
#endif

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
* \file solver_info_set.h
* \ingroup Solution
* \brief A class which contains a set of SolutionInfo objects.
* \author Josh Lurz, Sonny Kim
*/
#include <vector>
#include "solution/util/include/solver_info.h" // Maybe use pointer instead.
class Marketplace;
class ILogger;
class World;

class SolverInfoSet {
    friend std::ostream& operator<<( std::ostream& os, const SolverInfoSet& solverInfoSet ){
        solverInfoSet.print( os );
        return os;
    }
public:
    enum UpdateCode {
        UNCHANGED,
        ADDED,
        REMOVED,
        ADDED_AND_REMOVED
    };

    typedef std::vector<SolverInfo>::iterator SetIterator;
    typedef std::vector<SolverInfo>::const_iterator ConstSetIterator;
    SolverInfoSet( Marketplace* marketplace );
    SolverInfoSet( const std::vector<SolverInfo> aSolutionSet );
    void init( const unsigned int period );
    void merge( const std::vector<SolverInfo> aSolutionSet );
    void updateFromMarkets();
    void updateToMarkets();
    UpdateCode updateSolvable( const bool isNR );
    void updateElasticities();
    void adjustBrackets();
    void storeValues();
    void restoreValues();
    void resetBrackets();
    bool checkAndResetBrackets();
    SolverInfo* getWorstSolverInfo( const double aEDSolutionFloor, const bool aIgnoreBisected = false );
    SolverInfo* getWorstSolverInfoReverse( const double aTolerance, const double aEDSolutionFloor, const bool aIgnoreBisected = false );
    SolverInfo* getPolicyOrWorstSolverInfo( const double aTolerance, const double aEDSolutionFloor );
    SolverInfo* getPolicySolverInfo( const double aTolerance, const double aEDSolutionFloor );
    double getMaxRelativeExcessDemand( const double ED_SOLUTION_FLOOR ) const;
    double getMaxAbsoluteExcessDemand() const;
    bool isAllBracketed() const;
    const std::vector<double> getDemands() const; // move derivatives and make me private!
    const std::vector<double> getSupplies() const;
    unsigned int getNumSolvable() const;
    unsigned int getNumTotal() const;
    const SolverInfo& getSolvable( unsigned int index ) const;
    SolverInfo& getUnsolved( unsigned int index, const double aSolutionTolerance, const double aEDSolutionFloor );
    SolverInfo& getSolvable( unsigned int index );
    const SolverInfo& getAny( unsigned int index ) const;
    SolverInfo& getAny( unsigned int index );
    std::vector<SolverInfo> getSolvableSet() const;
    std::vector<SolverInfo> getSolvedSet( const double aSolTolerance, const double aEDSolutionFloor ) const;
    std::vector<SolverInfo> getUnsolvedSet( const double aSolTolerance, const double aEDSolutionFloor ) const;
    bool isAllSolved( const double SOLUTION_TOLERANCE, const double ED_SOLUTION_FLOOR );
    bool hasSingularUnsolved( const double aSolTolerance, const double aEDSolutionFloor );
    void unsetBisectedFlag();
    void printUnsolved( const double SOLUTION_TOLERANCE, const double ED_SOLUTION_FLOOR, std::ostream& out );
    void findAndPrintSD( const double aEDTolerance, const double aDemandFloor, World* aWorld, Marketplace* aMarketplace, const int aPeriod, ILogger& aLogger );
    void printMarketInfo( const std::string& comment, const double worldCalcCount, std::ostream& out ) const;
    void printDerivatives( std::ostream& aOut ) const;
private:
    unsigned int period;
    Marketplace* marketplace;
    std::vector<SolverInfo> solvable;
    std::vector<SolverInfo> unsolved; // solvable markets that are not currently solved
    std::vector<SolverInfo> unsolvable;
    void print( std::ostream& out ) const;
};

#endif // _SOLVER_INFO_H_
