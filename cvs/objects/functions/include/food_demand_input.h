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
* \file building_node_input.h
* \ingroup Objects
* \brief FoodDemandInput class header file.
* \author Pralit Patel
* \author Jiyong Eom
*/

#include "util/base/include/definitions.h"

#include "functions/include/inested_input.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

class IFunction;
class SatiationDemandFunction;

/*! 
 * \ingroup Objects
 * \brief An input which fits into the nested inputs framework to drive food demands.
 *
 * \details This is an abstract base class which 
 *
 *          <b>XML specification for FoodDemandInput</b>
 *          - XML name: \c FoodDemandInput::getXMLNameStatic()
 *          - Contained by: NodeInput
 *          - Parsing inherited from class: None
 *          - Attributes: \c name FoodDemandInput::mName
 *          - Elements:
 *              - \c BuildingServiceInput::getXMLNameStatic() FoodDemandInput::mNestedInputs
 *                   A building service input which will be added as a child to this node.
 *              - \c ThermalBuildingServiceInput::getXMLNameStatic() FoodDemandInput::mNestedInputs
 *                   A heating or cooling building service input which will be added as a
 *                   child to this node.
 *              - \c prodDmdFnType FoodDemandInput::mFunctionType
 *                   The name of the function that will calculate demand for the child inputs.
 *					 This will most likely be a building-service-function.
 *              - \c base-building-size FoodDemandInput::mBuildingSize
 *                   The base year size in terms of floorspace which can be utilized to back out
 *					 coefficients.
 *              - \c price-exponent FoodDemandInput::mPriceExponent
 *                   Price exponent by period to be used in calculating a demand for this
 *					 building type.
 *              - \c shell-conductance FoodDemandInput::mShellConductance
 *                   The building shell conductance by period which encapsulates technical change
 *					 characterisitics which affect heating and cooling service demand.
 *              - \c floor-to-surface-ratio FoodDemandInput::mFloorToSurfaceRatio
 *                   The building floor to surface area by period which building structual change
 *					 characterisitics which affect heating and cooling service demand.
 *              - \c internal-gains-market-name FoodDemandInput::mInternalGainsMarketname
 *                   A name to use when setting up a trial market for the internal gains of this
 *					 building type.
 *              - \c internal-gains-unit FoodDemandInput::mInternalGainsUnit
 *                   A string which describes the units for internal gains.
 *              - \c internal-gains-trial-supply FoodDemandInput::mInternalGainsTrialSupply
 *                   Initial trial supplies to set into the market for internal gains.  These
 *					 values are not necessary and only used to start the solver with a reasonable
 *					 trial supply.
 *              - \c SatiationDemandFunction::getXMLNameStatic() FoodDemandInput::mSatiationDemandFunction
 *                   The self contained satiation demand function which will parse it's own
 *                   parameters.
 *
 * \author Pralit Patel
 * \author Jiyong Eom
 */
class FoodDemandInput : public INestedInput
{
public:
    FoodDemandInput();
    virtual ~FoodDemandInput();

	// FoodDemandInput specific methods
	Value getSubregionalPopulation() const;

	Value getSubregionalIncome() const;

    std::string getTrialShareMarketName() const;

    double getTrialShare( const std::string& aRegionName,
                          const int aPeriod ) const;

    void setActualShare( double aShare,
                         const std::string& aRegionName,
                         const int aPeriod );

    double getScaleTerm() const;
    
    virtual double getCrossPriceElasticity( const FoodDemandInput* aOther,
                                           const std::string& aRegionName,
                                           const int aPeriod ) const = 0;

    virtual double getPriceScaler() const = 0;

    virtual double calcIncomeExponent( double aAdjIncome ) const = 0;
    
    virtual double calcIncomeExponentDerivative( double aAdjIncome ) const = 0;

    virtual double calcSelfPriceExponent( double aAdjIncome,
                                          const std::string& aRegionName,
                                          const int aPeriod ) const;

    virtual double calcCrossPriceExponent( const FoodDemandInput* aOther,
                                           double aAdjIncome,
                                           const std::string& aRegionName,
                                           const int aPeriod) const;

    //INestedInput methods
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

    virtual const std::string& getXMLReportingName() const = 0;

    virtual void XMLParse( const xercesc::DOMNode* aNode );
    
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
                                const MoreSectorInfo* aMoreSectorInfo,
                                const std::vector<AGHG*>& aGhgs,
                                const ICaptureComponent* aSequestrationDevice,
                                const int aLifetimeYears,
                                const int aPeriod ) {}

    virtual double calcTaxes( const std::string& aRegionName,
                            NationalAccount* aNationalAccount,
                            Expenditure* aExpenditure,
                            const int aPeriod ) const { return 0; }
   
    virtual void copyParamsInto( ProductionInput& aInput,
        const int aPeriod ) const {}

    virtual void copyParamsInto( DemandInput& aInput,
        const int aPeriod ) const {}

    virtual void copyParamsInto( TradeInput& aInput,
        const int aPeriod ) const {}

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
        DEFINE_VARIABLE( ARRAY | STATE, "food-demand", mFoodDemandQuantity, objects::PeriodVector<Value> ),

        //! Demand scale paramater (A)
        DEFINE_VARIABLE( SIMPLE, "scale-param", mScaleParam, Value ),

        //! Self price elasticity (g_ii)
        DEFINE_VARIABLE( SIMPLE, "self-price-elasticity", mSelfPriceElasticity, Value ),

        //! The actual share of the total budget (alpha)
        DEFINE_VARIABLE( ARRAY | STATE, "share", mShare, objects::PeriodVector<Value> ),
                            
        //! Regional bias correction term
        DEFINE_VARIABLE( SIMPLE, "regional-bias", mRegionalBias, Value ),
                            
        //! temporary value used during demand calculations
        DEFINE_VARIABLE( SIMPLE, "subregional-population", mCurrentSubregionalPopulation, Value ),

        //! Current Subregional income.  Note that this is just a
        //! temporary value used during demand calculations
        DEFINE_VARIABLE( SIMPLE, "subregional-income", mCurrentSubregionalIncome, Value )

    )
                           
    void copy( const FoodDemandInput& aNodeInput );
    virtual bool XMLDerivedClassParse( const std::string& aNodeName, const xercesc::DOMNode* aNode ) = 0;
};

class StaplesFoodDemandInput : public FoodDemandInput {
public:
    StaplesFoodDemandInput();
    virtual ~StaplesFoodDemandInput();
    
    static const std::string& getXMLNameStatic();

    // FoodDemandInput specific methods
    virtual double getCrossPriceElasticity( const FoodDemandInput* aOther,
                                           const std::string& aRegionName,
                                           const int aPeriod ) const;

    virtual double getPriceScaler() const;

    virtual double calcIncomeExponent( double aAdjIncome ) const;
    
    virtual double calcIncomeExponentDerivative( double aAdjIncome ) const;

    /*virtual double calcSelfPriceExponent( double aAdjIncome,
                                          const std::string& aRegionName,
                                          const int aPeriod ) const;

    virtual double calcCrossPriceExponent( const FoodDemandInput* aOther,
                                           double aAdjIncome,
                                           const std::string& aRegionName,
                                           const int aPeriod ) const*/;
    
    // IInput methods
    virtual IInput* clone() const;
    
    virtual const std::string& getXMLReportingName() const;
    
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
    virtual bool XMLDerivedClassParse( const std::string& aNodeName, const xercesc::DOMNode* aNode );
};

class NonStaplesFoodDemandInput : public FoodDemandInput {
public:
    NonStaplesFoodDemandInput();
    virtual ~NonStaplesFoodDemandInput();
    
    static const std::string& getXMLNameStatic();

    // FoodDemandInput specific methods
    virtual double getCrossPriceElasticity( const FoodDemandInput* aOther,
                                           const std::string& aRegionName,
                                           const int aPeriod ) const;

    virtual double getPriceScaler() const;

    virtual double calcIncomeExponent( double aAdjIncome ) const;
    
    virtual double calcIncomeExponentDerivative( double aAdjIncome ) const;

    /*virtual double calcSelfPriceExponent( double aAdjIncome,
                                          const std::string& aRegionName,
                                          const int aPeriod ) const;

    virtual double calcCrossPriceExponent( const FoodDemandInput* aOther,
                                           double aAdjIncome,
                                           const std::string& aRegionName,
                                           const int aPeriod ) const*/;
    
    // IInput methods
    virtual IInput* clone() const;
    
    virtual const std::string& getXMLReportingName() const;
    
protected:
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        FoodDemandInput,

        //! Income elasticity (nu)
        DEFINE_VARIABLE( SIMPLE, "income-elasticity", mIncomeElasticity, Value )

    )
    
    void copy( const NonStaplesFoodDemandInput& aNodeInput );
    virtual bool XMLDerivedClassParse( const std::string& aNodeName, const xercesc::DOMNode* aNode );
};

#endif // _FOOD_DEMAND_INPUT_H_
