#ifndef _CTAX_INPUT_H_
#define _CTAX_INPUT_H_
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
 * \file ctax_input.h
 * \ingroup Objects
 * \brief CTaxInput class header file.
 * \author Pralit Patel
 */

#include <string>
#include "functions/include/minicam_input.h"
#include "util/base/include/value.h"
#include <memory>

class Tabs;

/*! 
 * \ingroup Objects
 * \brief Defines a tax input which is applied relative to a carbon price.
 * \details This class only applies some price of a policy that is related
 *          to the carbon price.  It does not add to the supply or demand of
 *          any constraint.  The intended use would be from something such as
 *          a constraint on net negative emisssions.
 *
 *          <b>XML specification for CTaxInput</b>
 *          - XML name: \c ctax-input
 *          - Contained by: Technology
 *          - Parsing inherited from class: MiniCAMInput
 *          - Attributes:
 *              - \c name MiniCAMInput::mName
 *          - Elements:
 *              - \c mFuelName String indicating the name of the fuel to look up the
 *                             carbon coefficient of.  Generally the same as the fuel
 *                             name of the primary input to the containing technology.
 *
 * \author Pralit Patel
 */
class CTaxInput: public MiniCAMInput
{
    friend class InputFactory;
public:

    CTaxInput();

    virtual ~CTaxInput();

    virtual CTaxInput* clone() const;

    static const std::string& getXMLNameStatic();

    virtual const std::string& getXMLReportingName() const;
    
    virtual const std::string& getXMLName() const;

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

    virtual double getCO2EmissionsCoefficient( const std::string& aGHGName,
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

    virtual bool hasTypeFlag( const int aTypeFlag ) const;
    
    virtual double getIncomeElasticity( const int aPeriod ) const;

    virtual double getPriceElasticity( const int aPeriod ) const;

    virtual double getTechChange( const int aPeriod ) const;

protected:
    CTaxInput( const CTaxInput& aOther );

    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        MiniCAMInput,

        //! The name of the fuel to use to look up the C coef
        //! typically the name of the primary input into the
        //! containing technology
        DEFINE_VARIABLE( SIMPLE, "fuel-name", mFuelName, std::string ),

        //! The C coef associated with mFuelName
        DEFINE_VARIABLE( SIMPLE, "fuel-C-coef", mCachedCCoef, double ),
        
        //! State value for adding gross negative emissions value to market
        DEFINE_VARIABLE( SIMPLE | STATE | NOT_PARSABLE, "gross-negative-value", mNetTransferAdjust, Value )
    )
};

#endif // _CTAX_INPUT_H_
