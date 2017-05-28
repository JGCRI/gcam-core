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
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/



/*! 
 * \file asimple_carbon_calc.h
 * \ingroup Objects
 * \brief The ASimpleCarbonCalc class header file.
 * \author James Blackwood
 */
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/time_vector.h"
#include "util/base/include/value.h"
#include "ccarbon_model/include/icarbon_calc.h"

class LandUseHistory;

/*!
 * \brief The abstract base class of all simple carbon content calculators.
 * \details This class serves to share code between simple carbon content
 *          calculators, and to differentiate them from more computationally
 *          intensive carbon content calculators.
 */
class ASimpleCarbonCalc : public ICarbonCalc {
    friend class NodeCarbonCalc;
public:
    ASimpleCarbonCalc();
    virtual ~ASimpleCarbonCalc();

    virtual bool XMLParse( const xercesc::DOMNode* aNode ) = 0;
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const = 0;
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const = 0;

    virtual void completeInit() = 0;
    
    virtual void initCalc( const int aPeriod );

    virtual void setLandUseObjects( const LandUseHistory* aHistory, const LandLeaf* aLandLeaf );

    virtual double calc( const int aPeriod, const int aEndYear, const bool aStoreFullEmiss );

    virtual double getNetLandUseChangeEmission( const int aYear ) const;

    virtual double getActualAboveGroundCarbonDensity( const int aYear ) const = 0;
    
    virtual void setActualAboveGroundCarbonDensity( const double aAboveGroundCarbonDensity,
                                           const int aPeriod ) = 0;

    virtual double getActualBelowGroundCarbonDensity( const int aYear ) const = 0;

    virtual void setActualBelowGroundCarbonDensity( const double aBelowGroundCarbonDensity,
                                           const int aPeriod ) = 0;
	
	virtual int getMatureAge( ) const = 0;
    
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;

    virtual void acceptDerived( IVisitor* aVisitor, const int aPeriod ) const;

    virtual double getAboveGroundCarbonSubsidyDiscountFactor( );

    virtual double getBelowGroundCarbonSubsidyDiscountFactor( );
	
	virtual double getAboveGroundCarbonStock( const int aYear ) const;
	
    virtual double getBelowGroundCarbonStock( const int aYear ) const;

    virtual void setSoilTimeScale( const int aTimeScale );

protected:

    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        ICarbonCalc,

        //! Stored above ground emissions which are necessary to clear the total emissions
        //! when recalculating a period.
        DEFINE_VARIABLE( ARRAY | STATE, "above-emissions-by-period", mStoredEmissionsAbove, objects::PeriodVector<objects::YearVector<Value>*> ),
        
        //! Stored above ground emissions which are necessary to clear the total emissions
        //! when recalculating a period.
        DEFINE_VARIABLE( ARRAY | STATE, "below-emissions-by-period", mStoredEmissionsBelow, objects::PeriodVector<objects::YearVector<Value>*> ),
        
        //! Total emissions by year.
        DEFINE_VARIABLE( ARRAY, "land-use-change-emissions", mTotalEmissions, objects::YearVector<double> ),
        
        //! Above ground total emissions by year
        DEFINE_VARIABLE( ARRAY, "above-ground-land-use-change-emissions", mTotalEmissionsAbove, objects::YearVector<double> ),
        
        //! Below ground total emissions by year
        DEFINE_VARIABLE( ARRAY, "above-ground-land-use-change-emissions", mTotalEmissionsBelow, objects::YearVector<double> ),
        
        //! Above ground carbon stock
        DEFINE_VARIABLE( ARRAY | STATE, "above-ground-carbon-stock", mCarbonStock, objects::YearVector<Value> ),
        
        //! Time scale for soil carbon emissions
        DEFINE_VARIABLE( SIMPLE, "soil-time-scale", mSoilTimeScale, int )
    )

    /*!
     * \brief The land use history for the land leaf or it's parent land node.
     * \details Weak pointer to the land use history either for this leaf
     *          or the parent land type. The historical land share will be set to
     *          1 if this is the land use history for the leaf, and the historical share
     *          if it is for the parent land type.
     */
    const LandUseHistory* mLandUseHistory;
    
    //! The containing LandLeaf object which can be used to retrieve the current
    //! land allocation.
    const LandLeaf* mLandLeaf;
    
    //! The difference in the sigmoid curve by year offset + 1 - year offset.
    //! This value get precomputed during initcalc to avoid doing the computationally
    //! expensive operations during calc.
    std::vector<double> precalc_sigmoid_diff;
    
    //! Flag to ensure historical emissions are only calculated a single time
    //! since they can not be reset.
    bool mHasCalculatedHistoricEmiss;

    template<typename DoubleType>
    void calcAboveGroundCarbonEmission(const double aPrevCarbonStock,
                                       const double aPrevLandArea,
                                       const double aCurrLandArea,
                                       const double aPrevCarbonDensity,
                                       const int aYear,
                                       const int aEndYear,
                                       objects::YearVector<DoubleType>& aEmissVector);

    template<typename DoubleType>
    void calcBelowGroundCarbonEmission( const double aCarbonDiff,
                                        const int aYear,
                                        const int aEndYear,
                                        objects::YearVector<DoubleType>& aEmissVector);
private:
    template<typename DoubleType>
    void calcSigmoidCurve( const double aCarbonDiff,
                           const int aYear,
                           const int aEndYear,
                           objects::YearVector<DoubleType>& aEmissVector);
};

/*!
 * \brief Calculate the emission from above ground carbon for a given year.
 * \details Above ground carbon is emitted as a pulse, and will uptake over mature age.
 * \param aPrevCarbonStock The carbon stock from the previous year.
 * \param aPrevLandArea The land area during the previous year.
 * \param aCurrLandArea The land area which will expand/contract to.
 * \param aPrevCarbonDensity The potential carbon density for the previous year.
 * \param aYear The year to start the calculation.
 * \param aEndYear The last future year to calculate to.
 * \param aEmissVector A vector to accumulate emissions into.
 */
template<typename DoubleType>
void ASimpleCarbonCalc::calcAboveGroundCarbonEmission( const double aPrevCarbonStock,
                                                       const double aPrevLandArea,
                                                       const double aCurrLandArea,
                                                       const double aPrevCarbonDensity,
                                                       const int aYear,
                                                       const int aEndYear,
                                                       objects::YearVector<DoubleType>& aEmissVector)
{
    double carbonDiff = aPrevCarbonDensity * ( aPrevLandArea  - aCurrLandArea );
    // If no emissions or sequestration occurred, then exit.
    if( util::isEqual( carbonDiff, 0.0 ) ) {
        return;
    }
    
    // Finally, calculate net land use change emissions from changes in
    // above ground carbon.
    if ( getMatureAge() > 1 && carbonDiff < 0.0 ) {
        // If carbon content increases, then carbon was sequestered.
        // Carbon sequestration is stretched out in time, based on mMatureAge, because some
        // land cover types (notably forests) don't mature instantly.
        calcSigmoidCurve( carbonDiff, aYear, aEndYear, aEmissVector );
    }
    else if( util::isEqual( aPrevLandArea, 0.0 ) ) {
        // If this land category didn't exist before, and now it does,
        // then the calculation below will generate a NaN.  Avoid that
        // by taking the appropriate limit here.
        aEmissVector[ aYear ] -= aCurrLandArea * aPrevCarbonDensity;
    }
    else {
        // If carbon content decreases, then emissions have occurred.
        // Compute the carbon emission as the carbon stock pro rata to
        // the fraction of land converted.
        
        // If the mature age is just one year then sequestration
        // (negative emission) can just be added here as well (so we
        // don't have a separate branch for it).  (It's not obvious,
        // but you can show that the formula below just reduces to the
        // expression for carbonDiff at the top of the function.)
        aEmissVector[ aYear ] += ( aPrevCarbonStock / aPrevLandArea ) * ( aPrevLandArea - aCurrLandArea );
        if( getMatureAge() > 1 ) {
            // Back out the pending future sequestration for the land
            // that has been converted (i.e., that sequestration will
            // no longer happen).  This calculation is necessarily
            // approximate because we don't know how long the
            // destroyed vegetation has been growing.  We do know that
            // when the vegetation is fully mature,
            // carbonStock/LandArea == carbonDensity, so the
            // difference between those two quantities tells us how
            // much pending sequestration we have.  Distribute the
            // correction as a sigmoid between aYear and aEndYear.
            double carbonFutureAdjust = ( aPrevLandArea - aCurrLandArea ) * ( aPrevCarbonDensity -
                                                                             ( aPrevCarbonStock / aPrevLandArea ) );
            calcSigmoidCurve( carbonFutureAdjust, aYear, aEndYear, aEmissVector );
        }
    }
}

/*!
 * \brief Calculate the emission from below ground carbon for the given year.
 * \details Below ground, or soil carbon, is not emitted as a pulse but at a
 *          exponential rate with a half-life computed from the soil time scale.
 * \param aYear Year.
 * \param aEndYear The last future year to calculate to.
 * \param aEmissVector A vector to accumulate emissions into.
 */
template<typename DoubleType>
void ASimpleCarbonCalc::calcBelowGroundCarbonEmission( const double aCarbonDiff,
                                                       const int aYear,
                                                       const int aEndYear,
                                                       objects::YearVector<DoubleType>& aEmissVector )
{
    // If no emissions or sequestration occurred, then exit.
    if( util::isEqual( aCarbonDiff, 0.0 ) ){
        return;
    }
    
    // Exponential Soil carbon accumulation and decay, with half-life assumed to be
    // the soil time scale divided by ten.  At the half-life, half of the change will
    // have occured, at twice the half-life 75% would have occurred, etc.
    // Note also that the aCarbonDiff is passed here as previous carbon minus current carbon
    // so a positive difference means that emissions will occur and a negative means uptake.
    
    const double halfLife = mSoilTimeScale / 10.0;
    const double log2 = log( 2.0 );
    const double lambda = log2 / halfLife;
    int yearCounter = 0;
    double cumStockDiff_t1, cumStockDiff_t2;
    cumStockDiff_t1 = 0.0;
    for( int currYear = aYear; currYear <= aEndYear; ++currYear ) {
        yearCounter += 1;
        cumStockDiff_t2 = aCarbonDiff * ( 1.0 - exp( -1.0 * lambda * yearCounter ) );
        aEmissVector[ currYear ] += cumStockDiff_t2 - cumStockDiff_t1;
        cumStockDiff_t1 = cumStockDiff_t2;
    }
}

/*!
 * \brief    Calculate the sigmoidal sequestration curve.
 * \details  Called by calcAboveGroundCarbonEmission.
 * \param    carbonDifference Annual change in carbon for aYear
 * \param    aYear Year.
 * \param    aEndYear The last future year to calculate to.
 * \param    aEmissVector A vector to accumulate emissions into.
 */
template<typename DoubleType>
void ASimpleCarbonCalc::calcSigmoidCurve( const double aCarbonDiff,
                                          const int aYear,
                                          const int aEndYear,
                                          objects::YearVector<DoubleType>& aEmissVector )
{
    /*!
     * \pre This calculation will not be correct for a mature age of a single
     *      year.
     */
    assert( getMatureAge() > 1 );
    
    for( int currYear = aYear; currYear <= aEndYear; ++currYear ){
        // To avoid expensive calculations the difference in the sigmoid curve
        // has already been precomputed.
        aEmissVector[ currYear ] += precalc_sigmoid_diff[ currYear - aYear ] * aCarbonDiff;
    }
}


#endif // _ASIMPLE_CARBON_CALC_H_
