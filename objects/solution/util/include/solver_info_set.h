#ifndef _SOLVER_INFO_SET_H_
#define _SOLVER_INFO_SET_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file solver_info_set.h
* \ingroup Solution
* \brief The header file for the SolverInfoSet class.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include <vector>
#include "solution/util/include/solver_info.h"
class Marketplace;
class Logger;
class World;

/*!
* \ingroup Solution
* \brief A class which contains a set of SolutionInfo objects.
* \author Josh Lurz
*/
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
    void init( const unsigned int period );
    void updateFromMarkets();
    void updateToMarkets();
    const UpdateCode updateSolvable( const bool isNR );
    void updateElasticities();
    void adjustBrackets();
    void storeValues();
    void restoreValues();
    bool checkAndResetBrackets();
    SolverInfo& getWorstSolverInfo( const double ED_SOLUTION_FLOOR );
    double getMaxRelativeExcessDemand( const double ED_SOLUTION_FLOOR ) const;
    double getMaxAbsoluteExcessDemand() const;
    bool isAllBracketed() const;
    const std::vector<double> getDemands() const; // move derivatives and make me private!
    const std::vector<double> getSupplies() const;
    unsigned int getNumSolvable() const;
    unsigned int getNumTotal() const;
    const SolverInfo& getSolvable( unsigned int index ) const;
    SolverInfo& getSolvable( unsigned int index );
    const SolverInfo& getAny( unsigned int index ) const;
    SolverInfo& getAny( unsigned int index );
    bool isAllSolved( const double SOLUTION_TOLERANCE, const double ED_SOLUTION_FLOOR );
    void printUnsolved( const double SOLUTION_TOLERANCE, const double ED_SOLUTION_FLOOR );
    void findAndPrintSD( const double aEDTolerance, const double aDemandFloor, World* aWorld, Marketplace* aMarketplace, const int aPeriod, Logger* aLogger );
    void printMarketInfo( const std::string& comment, const double worldCalcCount ) const;
private:
    unsigned int period;
    Marketplace* marketplace;
    std::vector<SolverInfo> solvable;
    std::vector<SolverInfo> unsolvable;
    void print( std::ostream& out ) const;
};

#endif // _SOLVER_INFO_H_
