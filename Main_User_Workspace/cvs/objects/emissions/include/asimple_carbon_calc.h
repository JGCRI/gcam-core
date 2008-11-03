#ifndef _ASIMPLE_CARBON_CALC_H_
#define _ASIMPLE_CARBON_CALC_H_
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
 * \file asimple_carbon_calc.h
 * \ingroup Objects
 * \brief The ASimpleCarbonCalc class header file.
 * \author James Blackwood
 */
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/time_vector.h"
#include "emissions/include/icarbon_calc.h"

class LandUseHistory;
enum FlowType;

/*!
 * \brief The abstract base class of all simple carbon content calculators.
 * \details This class serves to share code between simple carbon content
 *          calculators, and to differentiate them from more computationally
 *          intensive carbon content calculators.
 * \todo Determine if this class is needed.
 */
class ASimpleCarbonCalc : public ICarbonCalc {
public:
    ASimpleCarbonCalc();
    virtual ~ASimpleCarbonCalc();

    virtual bool XMLParse( const xercesc::DOMNode* aNode ) = 0;
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const = 0;
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const = 0;

    virtual void completeInit( int aKey ) = 0;

    virtual void initLandUseHistory( const LandUseHistory* aHistory,
                                     const double aShare );

    virtual void calc( const int aYear );

    virtual void calcLandUseChange( const int aYear, FlowType aFlowType );

    virtual double getNetLandUseChangeEmission( const int aYear ) const;

    virtual double getNetTerrestrial( const int aYear ) const;

    virtual void setTotalLandUse( const double aLandUse,
                                  const int aPeriod );

    virtual double getPotentialAboveGroundCarbon( const int aYear ) const = 0;
    
    virtual void setUnitAboveGroundCarbon( const double aAboveGroundCarbon,
                                           const int aPeriod ) = 0;

    virtual double getPotentialBelowGroundCarbon( const int aYear ) const = 0;

    virtual void setUnitBelowGroundCarbon( const double aBelowGroundCarbon,
                                           const int aPeriod ) = 0;

    virtual double getActualAboveGroundCarbonDensity( const int aYear ) const = 0;
    
    virtual void setActualAboveGroundCarbonDensity( const double aAboveGroundCarbonDensity,
                                           const int aPeriod ) = 0;

    virtual double getActualBelowGroundCarbonDensity( const int aYear ) const = 0;

    virtual void setActualBelowGroundCarbonDensity( const double aBelowGroundCarbonDensity,
                                           const int aPeriod ) = 0;
    
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;

    virtual void acceptDerived( IVisitor* aVisitor, const int aPeriod ) const;

protected:
    //! The present period's change in carbon distributed over the lifetime of
    //! the emission for the current iteration.
    objects::YearVector<double> mCurrentEmissions;

    //! Total land used by period.
    objects::PeriodVector<double> mLandUse;

    //! A vector of booleans per year which represent whether the year has been
    //! calculated.
    objects::YearVector<bool> mCalculated;

    //! Total emissions by year.
    objects::YearVector<double> mTotalEmissions;
    
    //! Age at maturity.  This is used to grow forests slowly.
    int mMatureAge;        
    
    //! Whether this is the first time the period has been calculated.
    objects::PeriodVector<bool> mIsFirstTime;

    /*! 
     * \brief The land use history for the land leaf or it's parent land node.
     * \details Weak pointer to the land use history either for this leaf
     *          or the parent land type. The historical land share will be set to
     *          1 if this is the land use history for the leaf, and the historical share
     *          if it is for the parent land type.
     */
    const LandUseHistory* mLandUseHistory;

    /*! 
     * \brief Constant share of historical land to be assigned to this leaf.
     */
    double mHistoricalShare;

    void calcAboveGroundCarbonEmission( const unsigned int aYear,
                                        const bool aIsCurrentYear );

    void calcBelowGroundCarbonEmission( const unsigned int aYear,
                                        const bool aIsCurrentYear );

private:
    void calcSigmoidCurve( double carbonDifference, 
                                              const unsigned int aYear, const bool aIsCurrentYear );
    
    objects::YearVector<double> precalc_sigmoid;
    objects::YearVector<double> precalc_expdecay;

};

#endif // _ASIMPLE_CARBON_CALC_H_
