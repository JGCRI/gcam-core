#ifndef _FOOD_DEMAND_INPUT_H_
#define _FOOD_DEMAND_INPUT_H_
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
* \file food_demand_input.h
* \ingroup Objects
* \brief FoodDemandInput class header file.
* \author Pralit Patel
* \author Robert LInk
*/

#include "util/base/include/definitions.h"

#include "functions/include/inested_input.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

/*!
* \ingroup Objects
* \brief The Edmonds, et al. (2016) food demand system
*
* \details This class implements the demand system for representing
*          staple and nonstaple foods, as published by Edmonds, et
*          al. (2017) in _Climate Change Economics DOI:
*          10.1142/S2010007817500129. This demand system is
*          a two-component model for demand.  The components are
*          "staple foods", which are assumed to behave as inferior
*          goods, and "nonstaple foods", which are assumed to behave
*          as normal goods, but with demand eventually saturating at
*          high per capita income levels.  The model also tracks
*          internally "materials", a stand-in for everything else
*          that consumers spend their budget on.  Materials demand is
*          calculated as the residual of consumers' budget after
*          their food needs are met.  GCAM doesn't use the materials
*          demand, and the price for the materials component is fixed
*          as a parameter of the model (i.e., it cannot be set as an
*          input).  Therefore, we do not expose this third component
*          to the rest of GCAM.
*
*          The model has 10 parameters: the 9 parameters given in
*          Table 3 of the paper (in the same order as given in the
*          table), and a tenth regional bias correction factor, as
*          described in the Regional Bias Correction section of the
*          paper.  (Note that since each region will have its own
*          food demand sector, with its own instance of this class,
*          it is only necessary to include the bias correction factor
*          for a single region.)
*
*          The equations in this demand system make use of the budget
*          fractions, \f$\alpha\f$, for the goods in the demand
*          system.  These budget fractions are themselves computed
*          fromt the demand quantities output by the system.  In
*          stand-alone implementations of the demand system we use a
*          nonlinear equation solver to solve for a self-consistent
*          set of demands, but in GCAM we use the Trial Value
*          mechanism.  During the `completeInit()` method we set up a
*          trial value market for each of the two goods (staple and
*          nonstaple foods) in the system.  The GCAM solver mechanism
*          will use these to solve for self-consistent demand
*          values.
*
*          The \f$\alpha\f$ values can range from 0 to 1; and we ask
*          the solver keep to values within this range.
*
*          Output is calculated and set  in units of thousands of dietary
*          calories, per capita, per day.  However rest of GCAM will be
*          expecting Pcal/year so the units are transformed before
*          being set or added to market.  For the purposes of reporting
*          both will be available.
*
*          Although the demand system described in the paper is
*          readily generalizable to an arbitrary number of
*          components, we have not attempted to make this
*          implementation similarly generalizable.  This is mostly due
*          due to complications in deriving cross-price elasticities
*          when the number of goods is more than two.  However in
*          principal it can be done but we have not expended the effort
*          to do so yet.
*/
class FoodDemandInput : public INestedInput
{
    friend class XMLDBOutputter;
public:
    FoodDemandInput();
    virtual ~FoodDemandInput();

	// FoodDemandInput specific methods
	Value getSubregionalIncome() const;
    
    double getAnnualDemandConversionFactor( const int aPeriod ) const;

    std::string getTrialShareMarketName() const;

    double getTrialShare( const std::string& aRegionName,
                          const int aPeriod ) const;

    void setActualShare( double aShare,
                         const std::string& aRegionName,
                         const int aPeriod );
    
    double getRegionalBias( const int aPeriod ) const;

    double getScaleParam() const;

    virtual double calcPriceExponent( const FoodDemandInput* aOther,
                                      double aAdjIncome,
                                      const std::string& aRegionName,
                                      const int aPeriod) const;
    
    // The following food demand methods will have differing behavior for
    // staples and non-staples.
    virtual double getPriceElasticity( const FoodDemandInput* aOther,
                                       const std::string& aRegionName,
                                       const int aPeriod ) const = 0;

    virtual double getPriceScaler() const = 0;

    virtual double calcIncomeTerm( double aAdjIncome ) const = 0;
    
    virtual double calcIncomeTermDerivative( double aAdjIncome ) const = 0;

    // INestedInput methods
    // define them to do nothing since a FoodDemandInput is a leaf in the nesting structure
    // this should be the end point for recursion
    virtual void removeEmptyInputs() {}

    virtual void initialize() {}

    virtual void calcCoefficient( const std::string& aRegionName, const std::string& aSectorName,
        const int aTechPeriod ) {}

    virtual void changeElasticity( const std::string& aRegionName, const int aPeriod,
        const double aAlphaZero ) {}

    virtual void changeSigma( const std::string& aRegionName, const int aPeriod,
        const double aAlphaZero ) {}

    virtual void calcLevelizedCost( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const double aAlphaZero ) {}

    virtual double calcInputDemand( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const double aPhysicalOutput, const double aUtilityParameterA,
        const double aAlphaZero ) { return 0; }

    virtual double calcCapitalOutputRatio( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const double aAlphaZero ) { return 1.0; }

    virtual void calcVariableLevelizedCost( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const double aAlphaZero ) {}

    virtual const IFunction* getFunction() const { return 0; }
    
    virtual double getLevelizedCost( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod ) const { return 0; }

    virtual void applyTechnicalChange( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const TechChange& aTechChange ) {}

    virtual void resetCalcLevelizedCostFlag() {}

    // IInput methods
    virtual IInput* clone() const = 0;
    
    virtual void copyParam( const IInput* aInput,
                            const int aPeriod );

    virtual bool isSameType( const std::string& aType ) const;
    
    virtual const std::string& getName() const;

    virtual const std::string& getMarketName( const std::string& aRegionName ) const { return aRegionName; }

    virtual const std::string& getXMLName() const = 0;

    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;
    
    virtual bool hasTypeFlag( const int aTypeFlag ) const;

    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const std::string& aSubsectorName,
                               const std::string& aTechName,
                               const IInfo* aTechInfo );

    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const bool aIsNewInvestmentPeriod,
                           const bool aIsTrade,
                           const IInfo* aTechInfo,
                           const int aPeriod );

    virtual double getPhysicalDemand( const int aPeriod ) const;
    
    virtual void setPhysicalDemand( const double aPhysicalDemand,
                                    const std::string& aRegionName, 
                                    const int aPeriod );

    virtual double getPrice( const std::string& aRegionName,
                             const int aPeriod ) const;

    virtual void setPrice( const std::string& aRegionName,
                           const double aPrice,
                           const int aPeriod );

    virtual double getPricePaid( const std::string& aRegionName,
                                 const int aPeriod ) const;

    virtual void setPricePaid( const double aPricePaid,
                               const int aPeriod );

    // Methods that NodeInput will not implement
    virtual double getPriceElasticity( const int aPeriod ) const
    {
        return 0;
    }
    virtual double getCurrencyDemand( const int aPeriod ) const
	{
		return 0;
	}

    virtual void setCurrencyDemand( const double aCurrencyDemand,
                                    const std::string& aRegionName, 
                                    const int aPeriod )
	{
	}

    virtual double getCarbonContent( const int aPeriod ) const
	{
		return 0;
	}

	virtual double getPriceAdjustment() const
	{
		return 0;
	}

    virtual double getConversionFactor( const int aPeriod ) const
	{
		return 0;
	}

    virtual double getCO2EmissionsCoefficient( const std::string& aGHGName,
                                             const int aPeriod ) const
	{
		return 0;
	}

    virtual void tabulateFixedQuantity( const std::string& aRegionName,
                                        const double aFixedOutput,
                                        const bool aIsInvestmentPeriod,
                                        const int aPeriod )
	{
	}

    virtual void scaleCalibrationQuantity( const double aScaleFactor )
	{
	}

    virtual double getCalibrationQuantity( const int aPeriod ) const
	{
		return 0;
	}

	virtual double getTechChange( const int aPeriod ) const
	{
		return 0;
	}
    
    virtual double getIncomeElasticity( const int aPeriod ) const
    {
        return 1;
    }
    
    virtual double getCoefficient( const int aPeriod ) const
    {
        return 0;
    }
    
    virtual void setCoefficient( const double aCoefficient,
                                const int aPeriod )
    {
    }

    virtual void calcPricePaid( const std::string& aRegionName,
                                const std::string& aSectorName,
                                const std::vector<AGHG*>& aGhgs,
                                const ICaptureComponent* aSequestrationDevice,
                                const int aLifetimeYears,
                                const int aPeriod ) {}

    virtual double calcTaxes( const std::string& aRegionName,
                            NationalAccount* aNationalAccount,
                            Expenditure* aExpenditure,
                            const int aPeriod ) const { return 0; }

    virtual void copyParamsInto( EnergyInput& aInput,
        const int aPeriod ) const {}

    virtual void copyParamsInto( NonEnergyInput& aInput,
        const int aPeriod ) const {}

    virtual void copyParamsInto( RenewableInput& aInput,
        const int aPeriod ) const {}

    virtual void copyParamsInto( InputSubsidy& aInput,
        const int aPeriod ) const {}

    virtual void copyParamsInto( InputTax& aInput,
        const int aPeriod ) const {}

	virtual void copyParamsInto( InputOMVar& aInput,
								const int aPeriod ) const {}
	
	virtual void copyParamsInto( InputOMFixed& aInput,
								const int aPeriod ) const {}
	
	virtual void copyParamsInto( InputCapital& aInput,
								const int aPeriod ) const {}

    virtual void copyParamsInto( NodeInput& aInput,
		const int aPeriod ) const {}

	virtual void doInterpolations( const int aYear, const int aPreviousYear,
                                   const int aNextYear, const IInput* aPreviousInput,
								   const IInput* aNextInput ) {}

    // IVisitable interface.
    virtual void accept( IVisitor* aVisitor,
                        const int aPeriod ) const;


protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        INestedInput,

        //! The name of this input
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),

        //! Food demand in Pcal/year
        DEFINE_VARIABLE( ARRAY | STATE, "base-service", mFoodDemandQuantity, objects::PeriodVector<Value> ),

        //! Demand scale paramater (A)
        DEFINE_VARIABLE( SIMPLE, "scale-param", mScaleParam, Value ),

        //! Self price elasticity (g_ii)
        DEFINE_VARIABLE( SIMPLE, "self-price-elasticity", mSelfPriceElasticity, Value ),

        //! The actual share of the total budget (alpha)
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "share", mShare, objects::PeriodVector<Value> ),
                            
        //! Regional bias correction term
        DEFINE_VARIABLE( ARRAY | STATE, "regional-bias", mRegionalBias, objects::PeriodVector<Value> ),
        
        //! The Subregional population.  Note that this is just a
        //! temporary value used during demand calculations
        DEFINE_VARIABLE( ARRAY | NOT_PARSABLE, "subregional-population", mSubregionalPopulation, objects::PeriodVector<Value> ),

        //! Current Subregional income (in PPP).  Note that this is just a
        //! temporary value used during demand calculations
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "subregional-income", mCurrentSubregionalIncome, Value )

    )
                           
    void copy( const FoodDemandInput& aNodeInput );
};

/*!
  * \brief FoodDemand subclass with parameters and equations specific to a staple (inferior) good.
 */
class StaplesFoodDemandInput : public FoodDemandInput {
public:
    StaplesFoodDemandInput();
    virtual ~StaplesFoodDemandInput();
    
    static const std::string& getXMLNameStatic();

    // FoodDemandInput specific methods
    virtual double getPriceElasticity( const FoodDemandInput* aOther,
                                       const std::string& aRegionName,
                                       const int aPeriod ) const;

    virtual double getPriceScaler() const;

    virtual double calcIncomeTerm( double aAdjIncome ) const;
    
    virtual double calcIncomeTermDerivative( double aAdjIncome ) const;
    
    // IInput methods
    virtual IInput* clone() const;
    
    virtual const std::string& getXMLReportingName() const;
    
    virtual const std::string& getXMLName() const;
    
protected:
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        FoodDemandInput,

        //! Cross price elasticity (g_sn)
        DEFINE_VARIABLE( SIMPLE, "cross-price-elasticity", mCrossPriceElasticity, Value ),
                            
        //! Income elasticity (lamda)
        DEFINE_VARIABLE( SIMPLE, "income-elasticity", mIncomeElasticity, Value ),

        //! Income at which the maximum quantity is demanded (kappa)
        DEFINE_VARIABLE( SIMPLE, "income-max-term", mIncomeMaxTerm, Value )

    )
    
    void copy( const StaplesFoodDemandInput& aNodeInput );
};

/*!
 * \brief FoodDemand subclass with parameters and equations specific to a non-staple (normal) good.
*/
class NonStaplesFoodDemandInput : public FoodDemandInput {
public:
    NonStaplesFoodDemandInput();
    virtual ~NonStaplesFoodDemandInput();
    
    static const std::string& getXMLNameStatic();

    // FoodDemandInput specific methods
    virtual double getPriceElasticity( const FoodDemandInput* aOther,
                                       const std::string& aRegionName,
                                       const int aPeriod ) const;

    virtual double getPriceScaler() const;

    virtual double calcIncomeTerm( double aAdjIncome ) const;
    
    virtual double calcIncomeTermDerivative( double aAdjIncome ) const;
    
    // IInput methods
    virtual IInput* clone() const;
    
    virtual const std::string& getXMLReportingName() const;
    
    virtual const std::string& getXMLName() const;
    
protected:
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        FoodDemandInput,

        //! Income elasticity (nu)
        DEFINE_VARIABLE( SIMPLE, "income-elasticity", mIncomeElasticity, Value )

    )
    
    void copy( const NonStaplesFoodDemandInput& aNodeInput );
};

#endif // _FOOD_DEMAND_INPUT_H_
