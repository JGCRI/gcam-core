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
#include <boost/flyweight.hpp>
#include <boost/flyweight/key_value.hpp>
#include <boost/flyweight/no_tracking.hpp>

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

    virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const = 0;

    virtual void completeInit( const double aPrivateDiscountRateLand  ) = 0;
    
    virtual void initCalc( const int aPeriod );

    virtual void setLandUseObjects( const LandUseHistory* aHistory, const LandLeaf* aLandLeaf );

    virtual double calc( const int aPeriod, const int aEndYear, const CarbonCalcMode aCalcMode );

    virtual double getNetLandUseChangeEmission( const int aYear ) const;
    
    virtual double getNetLandUseChangeEmissionAbove( const int aYear ) const;
    
    virtual double getNetLandUseChangeEmissionBelow( const int aYear ) const;

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
        
        //! Total emissions by year.
        DEFINE_VARIABLE( ARRAY | NOT_PARSABLE, "land-use-change-emissions", mTotalEmissions, objects::YearVector<double> ),
        
        //! Above ground total emissions by year
        DEFINE_VARIABLE( ARRAY | NOT_PARSABLE, "above-ground-land-use-change-emissions", mTotalEmissionsAbove, objects::YearVector<double> ),
        
        //! Below ground total emissions by year
        DEFINE_VARIABLE( ARRAY | NOT_PARSABLE, "below-ground-land-use-change-emissions", mTotalEmissionsBelow, objects::YearVector<double> ),
        
        //! Above ground carbon stock
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "above-ground-carbon-stock", mCarbonStock, objects::YearVector<Value> ),
        
        //! Time scale for soil carbon emissions
        DEFINE_VARIABLE( SIMPLE, "soil-time-scale", mSoilTimeScale, int ),
        
        //! Discount rate for land related decisions
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "private-discount-rate", mPrivateDiscountRate, double )
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
    
    //! A copy of the carbon stock from the final year in the previous time-step.
    //! We have to keep a copy incase we need to run the carbon calc in reverse
    //! and the previous timestep got re-run in forward.
    objects::PeriodVector<double> mSavedCarbonStock;
    
    //! A copy of the previous land allocations
    //! We have to keep a copy incase we need to run the carbon calc in reverse
    //! and the previous timestep got re-run in forward.
    objects::PeriodVector<double> mSavedLandAllocation;
    
    // Some boiler plate to be able to take advantage of boost::flyweight to share
    // the precalc sigmoid curve between instances that have the same mature age
    struct precalc_sigmoid_helper {
        precalc_sigmoid_helper( const int aMatureAge );
        std::vector<double> mData;
        
        const double& operator[]( const size_t aPos ) const {
            return mData[ aPos ];
        }
    };
    using precalc_sigmoid_type = boost::flyweights::flyweight<
        boost::flyweights::key_value<int, precalc_sigmoid_helper>,
        boost::flyweights::no_tracking>;
    
    //! The difference in the sigmoid curve by year offset + 1 - year offset.
    //! This value get precomputed during initcalc to avoid doing the computationally
    //! expensive operations during calc.
    precalc_sigmoid_type precalc_sigmoid_diff;
    
    //! Flag to ensure historical emissions are only calculated a single time
    //! since they can not be reset.
    bool mHasCalculatedHistoricEmiss;

    void calcAboveGroundCarbonEmission(const double aPrevCarbonStock,
                                       const double aPrevLandArea,
                                       const double aCurrLandArea,
                                       const double aPrevCarbonDensity,
                                       const int aYear,
                                       const int aEndYear,
                                       objects::YearVector<double>& aEmissVector);

    void calcBelowGroundCarbonEmission( const double aCarbonDiff,
                                        const int aYear,
                                        const int aEndYear,
                                        objects::YearVector<double>& aEmissVector);
private:
    void calcSigmoidCurve( const double aCarbonDiff,
                           const int aYear,
                           const int aEndYear,
                           objects::YearVector<double>& aEmissVector);
};

#endif // _ASIMPLE_CARBON_CALC_H_
