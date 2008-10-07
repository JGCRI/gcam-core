#ifndef _SOLVER_INFO_H_
#define _SOLVER_INFO_H_
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
* \file solver_info.h
* \ingroup Solution
* \brief The header file for the SolverInfo class.
* \author Josh Lurz
*/

#include <string>
#include <iosfwd>
#include <functional>

class Market;
class SolverInfoSet;
class SupplyDemandCurve;
namespace objects {
    class Atom;
}

/*!
* \ingroup Solution
* \brief A class which contains the solution information for a single market.
* \author Josh Lurz
*/
class SolverInfo {
     friend std::ostream& operator<<( std::ostream& os, const SolverInfo& solverInfo ){
        solverInfo.print( os );
        return os;
    }
public:
    SolverInfo( Market* linkedMarket );
    bool operator==( const SolverInfo& rhs ) const;
    bool operator!=( const SolverInfo& rhs ) const;
    const std::string& getName() const;
    void init();
    bool isBracketed() const;
    void setBracketed();
    double getBracketSize() const;
    double getPrice() const;
    void setPrice( const double aPrice );
    void setPriceToCenter();
    double getDemand() const;
    void removeFromRawDemand( const double aDemand );
    double getSupply() const;
    void removeFromRawSupply( const double aSupply );
    double getED() const;
    double getEDLeft() const;
    double getEDRight() const;
    void storeValues();
    void restoreValues();
    void updateToMarket();
    void updateFromMarket();
    void adjustBracket();
    void expandBracket( const double aAdjFactor );
    double getRelativeED( const double ED_SOLUTION_FLOOR ) const;
    bool isWithinTolerance( const double SOLUTION_TOLERANCE, const double ED_SOLUTION_FLOOR ) const;
    bool shouldSolve( const bool isNR ) const;
    void calcDemandElas( const SolverInfoSet& solvableMarkets );
    void calcSupplyElas( const SolverInfoSet& solvableMarkets );
    bool checkAndResetBrackets();
    void increaseX( const double multiplier, const double lowerBound );
    void decreaseX( const double multiplier, const double lowerBound );
    void moveRightBracketToX();
    void moveLeftBracketToX();
    void resetBrackets();
    bool isCurrentlyBracketed() const;
    bool isSolved( const double SOLUTION_TOLERANCE, const double ED_SOLUTION_FLOOR ) const;
    double getDemandElasWithRespectTo( const unsigned int aMarketNumber ) const;
    double getSupplyElasWithRespectTo( const unsigned int aMarketNumber ) const;
    bool isUnsolvedAndSingular( const double aSolTolerance, const double aSolFloor );
    void setBisectedFlag();
    void unsetBisectedFlag();
    bool hasBisected() const;
    const std::vector<const objects::Atom*>& getContainedRegions() const;
    SupplyDemandCurve createSDCurve();
    void printDerivatives( std::ostream& aOut ) const;
    /*!
    * \brief Binary function used to order SolverInfo* pointers by decreasing relative excess demand. 
    * \author Josh Lurz
    */   
    struct GreaterRelativeED : public std::binary_function<SolverInfo*, SolverInfo*, bool>
    {
        double mDemandFloor; //!< Store the demand floor to use.
        //! Constructor
        GreaterRelativeED( const double aDemandFloor ):mDemandFloor( aDemandFloor ){}

        //! Operator which performs comparison. 
        bool operator()( const SolverInfo& aLHS, const SolverInfo& aRHS ) const
        {   
            return aLHS.getRelativeED( mDemandFloor ) > aRHS.getRelativeED( mDemandFloor );
        }
    };
private:
    bool bracketed; //!< Bracketed or unbracketed.
    bool mBisected;
    Market* linkedMarket; //!< Linked market. 
    double X;       //!< unknown, price.
    double storedX;      //!< previous unknown.
    double demand;  //!< demand for X.
    double storedDemand; //!< previous demand for X.
    double supply; //!< supply for X
    double storedSupply; //!< previous supply for X.
    double XL;      //!< left bracket
    double XR;      //!< right bracket
    double EDL;     //!< excess demand for left bracket
    double EDR;     //!< excess demand for right bracket
    std::vector<double> demandElasticities; //!< demand elasticities
    std::vector<double> supplyElasticities; //!< supply elasticities
    void print( std::ostream& out ) const;
    double getLogChangeInRawPrice() const;
    double getLogChangeInRawDemand() const;
    double getLogChangeInRawSupply() const;
};

#endif // _SOLVER_INFO_H_
