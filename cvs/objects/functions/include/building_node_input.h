#ifndef _BUILDING_NODE_INPUT_H_
#define _BUILDING_NODE_INPUT_H_
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
* \brief BuildingNodeInput class header file.
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
 * \brief A node class which drives building services and floorspace demands.
 *
 * \details Contains leaves which will represent building services as well as
 *          characteristics for this type of building such as floorspace, shell
 *          conductance and floor to surface area ratio.  This class is also
 *          responsible for setting up a trial market for internal gains.
 *
 *          <b>XML specification for BuildingNodeInput</b>
 *          - XML name: \c BuildingNodeInput::getXMLNameStatic()
 *          - Contained by: NodeInput
 *          - Parsing inherited from class: None
 *          - Attributes: \c name BuildingNodeInput::mName
 *          - Elements:
 *              - \c BuildingServiceInput::getXMLNameStatic() BuildingNodeInput::mNestedInputs
 *                   A building service input which will be added as a child to this node.
 *              - \c ThermalBuildingServiceInput::getXMLNameStatic() BuildingNodeInput::mNestedInputs
 *                   A heating or cooling building service input which will be added as a
 *                   child to this node.
 *              - \c prodDmdFnType BuildingNodeInput::mFunctionType
 *                   The name of the function that will calculate demand for the child inputs.
 *                   This will most likely be a building-service-function.
 *              - \c base-building-size BuildingNodeInput::mBuildingSize
 *                   The base year size in terms of floorspace which can be utilized to back out
 *                   coefficients.
 *              - \c is-building-size-fixed BuildingNodeInput::mIsFixedBuildingSize
 *                   A flag to indicate the user wants to fix the building size to the parsed value
 *              - \c price-exponent BuildingNodeInput::mPriceExponent
 *                   Price exponent by period to be used in calculating a demand for this
 *                   building type.
 *              - \c shell-conductance BuildingNodeInput::mShellConductance
 *                   The building shell conductance by period which encapsulates technical change
 *                   characterisitics which affect heating and cooling service demand.
 *              - \c floor-to-surface-ratio BuildingNodeInput::mFloorToSurfaceRatio
 *                   The building floor to surface area by period which building structual change
 *                   characterisitics which affect heating and cooling service demand.
 *              - \c internal-gains-market-name BuildingNodeInput::mInternalGainsMarketname
 *                   A name to use when setting up a trial market for the internal gains of this
 *                   building type.
 *              - \c internal-gains-unit BuildingNodeInput::mInternalGainsUnit
 *                   A string which describes the units for internal gains.
 *              - \c internal-gains-trial-supply BuildingNodeInput::mInternalGainsTrialSupply
 *                   Initial trial supplies to set into the market for internal gains.  These
 *                   values are not necessary and only used to start the solver with a reasonable
 *                   trial supply.
 *              - \c SatiationDemandFunction::getXMLNameStatic() BuildingNodeInput::mSatiationDemandFunction
 *                   The self contained satiation demand function which will parse it's own
 *                   parameters.

  *              - \c GompertzDemandFunction::getXMLNameStatic() BuildingNodeInput::GompertzDemandFunction
 *                   The self contained gompertz demand function which will parse it's own
 *                   parameters.
 *
 * \author Pralit Patel
 * \author Jiyong Eom
 */
class BuildingNodeInput : public INestedInput
{
friend class GompertzDemandFunction;
public:
    BuildingNodeInput();
    ~BuildingNodeInput();

    static const std::string& getXMLNameStatic();

    // Building NodeInput specific methods
    Value getSubregionalPopulation() const;

    Value getSubregionalIncome() const;

    Value getShellConductance( const int aPeriod ) const;

    Value getFloorToSurfaceRatio( const int aPeriod ) const;

    double getInternalGains( const std::string& aRegionName, const int aPeriod ) const;

    SatiationDemandFunction* getSatiationDemandFunction() const;

    // INestedInput methods
    virtual void removeEmptyInputs();

    virtual void initialize();

    virtual void calcCoefficient( const std::string& aRegionName, const std::string& aSectorName,
        const int aTechPeriod );

    virtual void changeElasticity( const std::string& aRegionName, const int aPeriod,
        const double aAlphaZero );

    virtual void calcLevelizedCost( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const double aAlphaZero );

    virtual double calcInputDemand( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const double aPhysicalOutput, const double aUtilityParameterA,
        const double aAlphaZero );

    virtual const IFunction* getFunction() const;
    
    virtual double getLevelizedCost( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod ) const;

    // INestedInput methods that will not be implemented
    virtual void changeSigma( const std::string& aRegionName, const int aPeriod,
        const double aAlphaZero )
    {
        // Change sigma refers to adjusting from a new vintage technology to an old vintage
        // technology.  So for instance changing from a high elasticity of substitutio to a low.
        // This concept makes no sense for buildings and so it is not implemented.
    }

    virtual double calcCapitalOutputRatio( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const double aAlphaZero )
    {
        return 0;
    }

    virtual void calcVariableLevelizedCost( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const double aAlphaZero )
    {
    }

    virtual void applyTechnicalChange( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const TechChange& aTechChange )
    {
    }

    virtual void resetCalcLevelizedCostFlag() {}

    // IInput methods
    virtual IInput* clone() const;
    
    virtual void copyParam( const IInput* aInput,
                            const int aPeriod );

    virtual bool isSameType( const std::string& aType ) const;
    
    virtual const std::string& getName() const;

    virtual const std::string& getMarketName( const std::string& aRegionName ) const { return aRegionName; }

    virtual const std::string& getXMLReportingName() const;
    
    virtual const std::string& getXMLName() const;

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

    virtual double getPriceElasticity( const int aPeriod ) const;

    // Methods that NodeInput will not implement
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

        //! Vector of child inputs
        DEFINE_VARIABLE( CONTAINER, "nodeInput", mNestedInputs, std::vector<INestedInput*> ),
        
        //! The name of this input
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),

        //! Type of function used
        DEFINE_VARIABLE( SIMPLE, "prodDmdFnType", mFunctionType, std::string ),

        //! Building size by period.
        DEFINE_VARIABLE( ARRAY | STATE, "base-building-size", mBuildingSize, objects::PeriodVector<Value> ),

        //! A flag to indicate the user wants to fix the building size to the parsed value.
        DEFINE_VARIABLE( ARRAY | NOT_PARSABLE, "is-building-size-fixed", mIsFixedBuildingSize, objects::PeriodVector<bool>),

        //! Price exponent by period.
        DEFINE_VARIABLE( ARRAY, "price-exponent", mPriceExponent, objects::PeriodVector<Value> ),

        //! Satiation demand function.
        DEFINE_VARIABLE( CONTAINER, "satiation-demand-function", mSatiationDemandFunction, SatiationDemandFunction* ),

        //! Shell conductance by period.
        DEFINE_VARIABLE( ARRAY, "shell-conductance", mShellConductance, objects::PeriodVector<Value> ),

        //! Floor to surface ratio by period.
        DEFINE_VARIABLE( ARRAY, "floor-to-surface-ratio", mFloorToSurfaceRatio, objects::PeriodVector<Value> ),

        //! Internal gains market name
        DEFINE_VARIABLE( SIMPLE, "internal-gains-market-name", mInternalGainsMarketname, std::string ),

        //! Internal gains output unit used to create the market
        DEFINE_VARIABLE( SIMPLE, "internal-gains-unit", mInternalGainsUnit, std::string ),

        //! Current Subregional population.  Note that this is just a
        //! temporary value used during demand calculations
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "subregional-population", mCurrentSubregionalPopulation, Value ),

        //! Current Subregional income.  Note that this is just a
        //! temporary value used during demand calculations
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "subregional-income", mCurrentSubregionalIncome, Value ),

        //! The sum product of energy service price necessary to drive demands.
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "price", mPrice, objects::PeriodVector<Value> ),

        //! The unadjusted satiation level to use during calcDemand. Parsed from XML
        DEFINE_VARIABLE(SIMPLE, "unadjust-satiation", mUnadjustSatiation, Value),

        //! The habitable land to use during calcDemand. Parsed from XML
        DEFINE_VARIABLE(SIMPLE, "habitable-land", mHabitableLand, Value),

        //! The base pcFlsp to use during calcDemand. Parsed from XML
        DEFINE_VARIABLE(SIMPLE, "base-pcFlsp", mBasepcFlsp, Value),

        //! The land density parameter to use during calcDemand. Parsed from XML
        DEFINE_VARIABLE(SIMPLE, "land-density-param", mLandDensityParam, Value),

        //! The base floorspace parameter to use during calcDemand. Parsed from XML
        DEFINE_VARIABLE(SIMPLE, "b-param", mbParam, Value),

        //! The income parameter to use during calcDemand. Parsed from XML
        DEFINE_VARIABLE(SIMPLE, "income-param", mIncomeParam, Value),

        //! The bias correction parameter to use during calcDemand. Parsed from XML
        DEFINE_VARIABLE(SIMPLE, "bias-adjust-param", mBiasAdjustParam, Value)

    )
                           
    //! Pointer to function this class will use
    const IFunction* mFunction;

    //! Cache the vector of children as IInput* which is needed for the mFunction
    std::vector<IInput*> mChildInputsCache;
                       
    typedef std::vector<INestedInput*>::iterator NestedInputIterator;
    typedef std::vector<INestedInput*>::const_iterator CNestedInputIterator;
    
    void copy( const BuildingNodeInput& aNodeInput );
};

#endif // _BUILDING_NODE_INPUT_H_
