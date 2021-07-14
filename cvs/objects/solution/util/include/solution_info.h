#ifndef _SOLUTION_INFO_H_
#define _SOLUTION_INFO_H_
#if defined(_MSC_VER)
#pragma once
#endif

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
* \file solution_info.h
* \ingroup Solution
* \brief The header file for the SolutionInfo class.
* \author Josh Lurz
*/

#include <string>
#include <iosfwd>
#include <functional>
#include "util/base/include/definitions.h"
#include "marketplace/include/imarket_type.h"
// need to include this because you can't forward declare
// internal classes
#include "solution/util/include/solution_info_param_parser.h"

class Market;
class IInfo;
class SolutionInfoSet;
class IActivity;
namespace objects {
    class Atom;
}

#if GCAM_PARALLEL_ENABLED
class GcamFlowGraph;
#endif

/*!
* \ingroup Solution
* \brief A class which contains the solution information for a single market.
* \author Josh Lurz
*/
class SolutionInfo {
     friend std::ostream& operator<<( std::ostream& os, const SolutionInfo& SolutionInfo ){
        SolutionInfo.print( os );
        return os;
    }
public:
#if GCAM_PARALLEL_ENABLED
    SolutionInfo( Market* linkedMarket, const std::vector<IActivity*>& aDependenicies, GcamFlowGraph* aFlowGraph );
#else
    SolutionInfo( Market* linkedMarket, const std::vector<IActivity*>& aDependenicies );
#endif
    bool operator==( const SolutionInfo& rhs ) const;
    bool operator!=( const SolutionInfo& rhs ) const;
    const std::string& getName() const;
    const std::string& getRegionName() const;
    const IMarketType::Type getType() const;
    std::string getTypeName() const;
    void init( const double aDefaultSolutionTolerance, const double aDefaultSolutionFloor,
               const SolutionInfoParamParser::SolutionInfoValues& aSolutionInfoValues );
    bool isBracketed() const;
    void setBracketed();
    double getBracketSize() const;
    double getCurrentBracketInterval() const;
    double getBracketInterval( const double aDefaultBracketInverval ) const;
    double getMaxNRPriceJump( const double aDefaultMaxPriceJump ) const;
    double getDeltaPrice( const double aDefaultDeltaPrice ) const;
    double getPrice() const;
    void setPrice( const double aPrice );
    void setPriceToCenter();
    double getDemand() const;
    double getSupply() const;
    double getED() const;
    double getEDLeft() const;
    double getEDRight() const;
    double getSolutionFloor() const;
    void expandBracket( const double aAdjFactor );
    double getRelativeED() const;
    bool isWithinTolerance() const;
    bool shouldSolve( const bool isNR ) const;
    void calcDemandElas( const SolutionInfoSet& solvableMarkets );
    void calcSupplyElas( const SolutionInfoSet& solvableMarkets );
    bool checkAndResetBrackets();
    void increaseX( const double multiplier, const double lowerBound );
    void decreaseX( const double multiplier, const double lowerBound );
    void moveRightBracketToX();
    void moveLeftBracketToX();
    void resetBrackets();
    bool isCurrentlyBracketed() const;
    bool isSolved() const;
    double getDemandElasWithRespectTo( const unsigned int aMarketNumber ) const;
    double getSupplyElasWithRespectTo( const unsigned int aMarketNumber ) const;
    bool isUnsolvedAndSingular();
    void setBisectedFlag();
    void unsetBisectedFlag();
    bool hasBisected() const;
    const std::vector<const objects::Atom*>& getContainedRegions() const;
    const std::vector<IActivity*>& getDependencies() const;

    double getLowerBoundSupplyPrice() const;
    double getUpperBoundSupplyPrice() const;
    double getForecastPrice() const;
    double getForecastDemand() const;
    void setForecastPrice( const double aPrice );
    void setForecastDemand( const double aDemand );
    double getCorrectionSlope() const;
    void setCorrectionSlope( const double aSlope );

    int getSerialNumber( void ) const;
    
    const IInfo* getMarketInfo() const;
#if GCAM_PARALLEL_ENABLED
    GcamFlowGraph* getFlowGraph() const;
#endif
    void printDerivatives( std::ostream& aOut ) const;
    /*!
    * \brief Binary function used to order SolutionInfo* pointers by decreasing relative excess demand. 
    * \author Josh Lurz
    */   
    struct GreaterRelativeED : public std::binary_function<SolutionInfo*, SolutionInfo*, bool>
    {
        //! Constructor
        GreaterRelativeED(){}

        //! Operator which performs comparison. 
        bool operator()( const SolutionInfo& aLHS, const SolutionInfo& aRHS ) const
        {   
            return aLHS.getRelativeED() > aRHS.getRelativeED();
        }
    };
private:
    bool bracketed; //!< Bracketed or unbracketed.
    bool mBisected;
    Market* linkedMarket; //!< Linked market. 
    double XL;      //!< left bracket
    double XR;      //!< right bracket
    double EDL;     //!< excess demand for left bracket
    double EDR;     //!< excess demand for right bracket
    std::vector<double> demandElasticities; //!< demand elasticities
    std::vector<double> supplyElasticities; //!< supply elasticities
    //! This activities which need to recalculate if this solution info adjust
    //! prices
    std::vector<IActivity*> mDependencies;

#if GCAM_PARALLEL_ENABLED
    //! A pointer weak pointer to a flow graph which can be used recalculate if this
    // solution info adjusts it's price.
    GcamFlowGraph* mFlowGraph;
#endif
    
    //! Market specific solution tolerance
    double mSolutionTolerance;
    
    //! Market specific solution floor
    double mSolutionFloor;
    
    //! Market specific bracket interval
    double mBracketInterval;
    
    //! Market specific max newton raphson price jump
    double mMaxNRPriceJump;
    
    //! Market specific delta price for derivative calcs
    double mDeltaPrice;
    
    //! Market specific lower bound price for expected supply/demand behavior.
    double mLowerBoundSupplyPrice;
    
    //! Market specific upper bound price for expected supply/demand behavior.
    double mUpperBoundSupplyPrice;
    
    void print( std::ostream& out ) const;
    double getLogChangeInRawPrice() const;
    double getLogChangeInRawDemand() const;
    double getLogChangeInRawSupply() const;
    double getLowerBoundSupplyPriceInternal() const;
    double getUpperBoundSupplyPriceInternal() const;
};

#endif // _SOLUTION_INFO_H_
