#ifndef _SOLVER_INFO_H_
#define _SOLVER_INFO_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file solver_info.h
* \ingroup Solution
* \brief The header file for the SolverInfo class.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <iosfwd>
#include <functional>

class Market;
class SolverInfoSet;
class SupplyDemandCurve;

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
    const std::vector<std::string> getContainedRegions() const;
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
    bool bracketed;	//!< Bracketed or unbracketed.
    bool mBisected;
    Market* linkedMarket; //!< Linked market. 
    double X;		//!< unknown, price.
    double storedX;      //!< previous unknown.
    double demand;  //!< demand for X.
    double storedDemand; //!< previous demand for X.
    double supply; //!< supply for X
    double storedSupply; //!< previous supply for X.
    double XL;		//!< left bracket
    double XR;		//!< right bracket
    double EDL;		//!< excess demand for left bracket
    double EDR;		//!< excess demand for right bracket
    std::vector<double> demandElasticities; //!< demand elasticities
    std::vector<double> supplyElasticities; //!< supply elasticities
    void print( std::ostream& out ) const;
    double getLogChangeInRawPrice() const;
    double getLogChangeInRawDemand() const;
    double getLogChangeInRawSupply() const;
};

#endif // _SOLVER_INFO_H_
