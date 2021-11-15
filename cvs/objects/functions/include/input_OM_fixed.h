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

#ifndef _INPUT_OM_FIXED_H_
#define _INPUT_OM_FIXED_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file input_om_fixed.h
 * \ingroup Objects
 * \brief InputOMFixed class header file.
 * \author Josh Lurz
 */

#include <string>

#include "functions/include/minicam_input.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

class Tabs;

/*! 
 * \ingroup Objects
 * \brief Defines a single capital input to a MiniCAM production function.
 * \details A capital input represents a constant marginal cost input in the
 *          production function, such as capital or operating and maintenance.
 *          The input does not have an associated good in the model, and so the
 *          quantity is not tracked.
 *
 *          <b>XML specification for InputOMFixed</b>
 *          - XML name: \c input-OM-fixed
 *          - Contained by: Technology
 *          - Parsing inherited from class: MiniCAMInput
 *          - Attributes:
 *              - \c name MiniCAMInput::mName
 *          - Elements:
 *              - \c OM-fixed InputOMFixed::mOMFixed
 *              - \c capacity-factor InputOMFixed::mCapacityFactor
 *              - \c levelized-OM-fixed InputOMFixed::mLevelizedOMFixedCost
 *              - \c tech-change InputOMFixed::mTechChange
 *
 * \author Josh Lurz
 */
class InputOMFixed: public MiniCAMInput
{
public:
    InputOMFixed();

    static const std::string& getXMLNameStatic();

    const std::string& getXMLReportingName() const;
    
    const std::string& getXMLName() const;

    InputOMFixed* clone() const;

    virtual bool isSameType( const std::string& aType ) const;

    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;

    virtual void copyParam( const IInput* aInput,
                            const int aPeriod );

    virtual void copyParamsInto( InputOMFixed& aInput,
                                const int aPeriod ) const;

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

    double getPhysicalDemand( const int aPeriod ) const;

    void setPhysicalDemand( const double aPhysicalDemand,
                            const std::string& aRegionName,
                            const int aPeriod );

    double getPrice( const std::string& aRegionName,
                     const int aPeriod ) const;
    
    virtual void setPrice( const std::string& aRegionName,
                           const double aPrice,
                           const int aPeriod );

    double getCO2EmissionsCoefficient( const std::string& aGHGName,
                                    const int aPeriod ) const;

    double getCoefficient( const int aPeriod ) const;

    void setCoefficient( const double aCoefficient,
                         const int aPeriod );

    void tabulateFixedQuantity( const std::string& aRegionName,
                                const double aFixedOutput,
                                const bool aIsInvestmentPeriod,
                                const int aPeriod );

    virtual void scaleCalibrationQuantity( const double aScaleFactor );

    virtual double getCalibrationQuantity( const int aPeriod ) const;

    bool hasTypeFlag( const int aTypeFlag ) const;

    virtual double getIncomeElasticity( const int aPeriod ) const;

    virtual double getPriceElasticity( const int aPeriod ) const;

    virtual double getTechChange( const int aPeriod ) const;
    
    virtual void doInterpolations( const int aYear, const int aPreviousYear,
                                   const int aNextYear, const IInput* aPreviousInput,
                                   const IInput* aNextInput );

protected:

    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        MiniCAMInput,
        
        //! Cost of the non-energy input adjusted for the additional costs of the
        //! capture component.
        DEFINE_VARIABLE( ARRAY | NOT_PARSABLE, "adjusted-cost", mAdjustedCosts, objects::TechVintageVector<Value> ),
        
        //! Coefficient for production or demand function. Coefficients are not
        // read in and are initialized to 1, but can increase over time with
        // technical change.
        DEFINE_VARIABLE( ARRAY | NOT_PARSABLE, "adjusted-coef", mAdjustedCoefficients, objects::TechVintageVector<Value> ),
        
        //! Input specific technical change.
        DEFINE_VARIABLE( SIMPLE, "tech-change", mTechChange, Value ),
        
        //! Variable O&M cost.
        DEFINE_VARIABLE( SIMPLE, "OM-fixed", mOMFixed, Value ),
        
        //! Calculated value for the levelized cost of capital.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "levelized-OM-fixed", mLevelizedOMFixedCost, Value ),
        
        //! Technology capacity factor.
        // TODO: create one in technology and use that instead.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "capacity-factor", mCapacityFactor, double )
    )
    
    void copy( const InputOMFixed& aOther );

private:
    const static std::string XML_REPORTING_NAME; //!< tag name for reporting xml db 

    // Function to calculate the levelized fixed O&M cost.
    double calcLevelizedOMFixedCost( void ) const;

};

#endif // _INPUT_OM_FIXED_H_
