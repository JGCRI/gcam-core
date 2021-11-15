#ifndef _BUILDING_SERVICE_INPUT_H_
#define _BUILDING_SERVICE_INPUT_H_
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
* \file building_service_input.h
* \ingroup Objects
* \brief BuildingServiceInput class header file.
* \author Pralit Patel
* \author Jiyong Eom
*/

#include "util/base/include/definitions.h"

#include "functions/include/inested_input.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

class IFunction;
class BuildingNodeInput;
class SatiationDemandFunction;

/*! 
 * \ingroup Objects
 * \brief An input class which demands generic building services.
 * \details Building services will operate with a building service function which
 *          will gradually satiate demands as affordability of the service increases.
 *
 *          <b>XML specification for BuildingServiceInput</b>
 *          - XML name: \c BuildingServiceInput::getXMLNameStatic()
 *          - Contained by: BuildingNodeInput
 *          - Parsing inherited from class: None
 *          - Attributes: \c name BuildingServiceInput::mName
 *          - Elements:
 *              - \c base-service BuildingNodeInput::mServiceDemand
 *                   The base year service which can be utilized to back out
 *                   coefficients.
 *              - \c SatiationDemandFunction::getXMLNameStatic() BuildingNodeInput::mSatiationDemandFunction
 *                   The self contained satiation demand function which will parse it's own
 *                   parameters.
 *
 * \author Pralit Patel
 * \author Jiyong Eom
 */
class BuildingServiceInput : public INestedInput
{
    friend class XMLDBOutputter;
public:
    BuildingServiceInput();
    virtual ~BuildingServiceInput();

    static const std::string& getXMLNameStatic();

    // Building service specific methods
    void setServiceDensity( const double aServiceDensity, const int aPeriod );
    
    SatiationDemandFunction* getSatiationDemandFunction() const;
    
    virtual double calcThermalLoad( const BuildingNodeInput* aBuildingInput,
                                    const double aInternalGainsPerSqMeter,
                                    const int aPeriod ) const;

    // INestedInput methods
    // define them to do nothing since a BuildingServiceInput is a leaf in the nesting structure
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

    virtual double getCoefficient( const int aPeriod ) const;

    virtual void setCoefficient( const double aCoefficient,
                                 const int aPeriod );

    // input methods which will not be implemented
    virtual double getCurrencyDemand( const int aPeriod ) const
    {
        return 0;
    }

    virtual void setCurrencyDemand( const double aCurrencyDemand,
                                    const std::string& aRegionName, 
                                    const int aPeriod )
    {
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

    virtual double getCarbonContent( const int aPeriod ) const
    {
        return 0;
    }

    virtual double getTechChange( const int aPeriod ) const
    {
        return 0;
    }

    virtual double getPriceAdjustment() const
    {
        return 0;
    }

    virtual double getCalibrationQuantity( const int aPeriod ) const
    {
        return -1;
    }

    virtual void tabulateFixedQuantity( const std::string& aRegionName,
                                        const double aFixedOutput,
                                        const bool aIsInvestmentPeriod,
                                        const int aPeriod ) {}

    virtual void scaleCalibrationQuantity( const double aScaleFactor ) {}

    virtual double getPriceElasticity( const int aPeriod ) const {return 0;}

    virtual double getIncomeElasticity( const int aPeriod ) const {return 0;}
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

        //! The name of this input.
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),

        //! Building service demand by period.
        DEFINE_VARIABLE( ARRAY | STATE, "base-service", mServiceDemand, objects::PeriodVector<Value> ),

        //! Energy service density for reporting.
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "service-density", mServiceDensity, objects::PeriodVector<Value> ),

        //! Satiation demand function.
        DEFINE_VARIABLE( CONTAINER, "satiation-demand-function", mSatiationDemandFunction, SatiationDemandFunction* )
    )
    
    void copy( const BuildingServiceInput& aInput );
};

#endif // _BUILDING_SERVICE_INPUT_H_
