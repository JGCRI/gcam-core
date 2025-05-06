#ifndef _ENERGY_INPUT_H_
#define _ENERGY_INPUT_H_
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
 * \file energy_input.h
 * \ingroup Objects
 * \brief EnergyInput class header file.
 * \author Josh Lurz
 */

#include <string>
#include <memory>

#include "functions/include/minicam_input.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

class Tabs;
class ICoefficient;
class CachedMarket;

/*! 
 * \ingroup Objects
 * \brief Defines a single energy input to a MiniCAM production function.
 * \details Energy inputs to production functions represent fuels or other
 *          energy carriers, such as electricity, consumed to produce output.
 *          For instance, in the electric sector energy inputs consist of coal,
 *          oil, gas and uranium. The cost of an energy input is determined by
 *          the market price of the input.
 *
 *          <b>XML specification for EnergyInput</b>
 *          - XML name: \c minicam-energy-input
 *          - Contained by: Technology
 *          - Parsing inherited from class: MiniCAMInput
 *          - Attributes:
 *              - \c name MiniCAMInput::mName
 *          - Elements:
 *              - \c intensity Intensity of use of the input(Only intensity or
 *                             efficiency can be input).
 *              - \c efficiency Efficiency of use of the input(Only intensity or
 *                              efficiency can be input).
 *              - \c calibrated-value EnergyInput::mCalibrationInput
 *              - \c income-elasticity EnergyInput::mIncomeElasticity
 *              - \c tech-change EnergyInput::mTechChange
 *
 * \author Josh Lurz
 */
class EnergyInput: public MiniCAMInput
{
public:

    EnergyInput();

    virtual ~EnergyInput();

    virtual EnergyInput* clone() const;

    static const std::string& getXMLNameStatic();

    virtual const std::string& getXMLName() const;
    
    virtual const std::string& getXMLReportingName() const;

    virtual bool XMLParse( rapidxml::xml_node<char>* & aNode );

    virtual bool isSameType( const std::string& aType ) const;

    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;

    virtual void copyParam( const IInput* aInput,
                            const int aPeriod );

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

    virtual void initializeTypeFlags();

    virtual const std::string& getMarketName( const std::string& aRegionName ) const;

    virtual  double getCO2EmissionsCoefficient( const std::string& aGHGName,
                                             const int aPeriod ) const;
    
    virtual double getPhysicalDemand( const int aPeriod ) const;
    
    virtual double getCarbonContent( const int aPeriod ) const;
    
    virtual double getPrice( const std::string& aRegionName,
                             const int aPeriod ) const;

    virtual void setPrice( const std::string& aRegionName,
                           const double aPrice,
                           const int aPeriod );

    virtual void setPhysicalDemand( const double aPhysicalDemand,
                                    const std::string& aRegionName,
                                    const int aPeriod );

    virtual double getCoefficient( const int aPeriod ) const;

    virtual void setCoefficient( const double aCoefficient,
                                 const int aPeriod );

    virtual double getCalibrationQuantity( const int aPeriod ) const;
    
    virtual double getIncomeElasticity( const int aPeriod ) const;

    virtual double getPriceElasticity( const int aPeriod ) const;

    virtual double getTechChange( const int aPeriod ) const;

    virtual void copyParamsInto( EnergyInput& aInput,
                                 const int aPeriod ) const;
    
    virtual void doInterpolations( const int aYear, const int aPreviousYear,
                                   const int aNextYear, const IInput* aPreviousInput,
                                   const IInput* aNextInput );

protected:
    EnergyInput( const EnergyInput& aOther );

    void initializeCachedCoefficients( const std::string& aRegionName );
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        MiniCAMInput,

        //! Coefficient for production or demand function.
        DEFINE_VARIABLE( CONTAINER, "coefficient", mCoefficient, ICoefficient* ),
        
        //! Cached CO2 coefficient.
        DEFINE_VARIABLE( SIMPLE, "co2-coef", mCO2Coefficient, Value ),

        //! Read-in calibration value.
        DEFINE_VARIABLE( SIMPLE, "calibrated-value", mCalibrationInput, Value ),

        //! Income elasticity.
        DEFINE_VARIABLE( SIMPLE, "income-elasticity", mIncomeElasticity, Value ),
        
        //! Input specific technical change.
        DEFINE_VARIABLE( SIMPLE, "tech-change", mTechChange, Value ),

        //! The market name from which to demand.  It will default to the region
        //! in which this input is contained.
        DEFINE_VARIABLE( SIMPLE, "market-name", mMarketName, std::string ),

        //! Conversion factor to change the market price units to working units
        DEFINE_VARIABLE( SIMPLE, "price-unit-conversion", mPriceUnitConversionFactor, Value ),

        //! Physical Demand.
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "physical-demand", mPhysicalDemand, objects::TechVintageVector<Value> ),

        //! Current coefficient after adjustments have been made by the technology's
        //! capture component.
        DEFINE_VARIABLE( ARRAY | STATE, "current-coef", mAdjustedCoefficients, objects::TechVintageVector<Value> )
    )
    
    //! A pre-located market which has been cahced from the marketplace to get
    //! the price and add demands to.
    std::unique_ptr<CachedMarket> mCachedMarket;

private:
    const static std::string XML_REPORTING_NAME; //!< tag name for reporting xml db 
};

#endif // _ENERGY_INPUT_H_
