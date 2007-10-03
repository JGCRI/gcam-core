#ifndef _ASIMPLE_CARBON_CALC_H_
#define _ASIMPLE_CARBON_CALC_H_
#if defined(_MSC_VER)
#pragma once
#endif

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

    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;

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

};

#endif // _ASIMPLE_CARBON_CALC_H_
