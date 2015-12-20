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

#ifndef _INPUT_OM_VAR_H_
#define _INPUT_OM_VAR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file input_om_fixed.h
 * \ingroup Objects
 * \brief InputOMVar class header file.
 * \author Josh Lurz
 */

#include <string>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "functions/include/minicam_input.h"
#include "util/base/include/value.h"

class Tabs;

/*! 
 * \ingroup Objects
 * \brief Defines a single variable O&M input to a MiniCAM production function.
 * \details A variable O&M input adds to the marginal cost input in the
 *          production function.  The input does not have an associated good in 
 *          the model, and so the quantity is not tracked.
 *
 *          <b>XML specification for InputOMVar</b>
 *          - XML name: \c input-OM-fixed
 *          - Contained by: Technology
 *          - Parsing inherited from class: MiniCAMInput
 *          - Attributes:
 *              - \c name MiniCAMInput::mName
 *          - Elements:
 *              - \c OM-var InputOMVar::mOMVar
 *              - \c tech-change InputOMVar::mTechChange
 *
 * \author Josh Lurz
 */
class InputOMVar: public MiniCAMInput
{
    friend class InputFactory;
public:

    static const std::string& getXMLNameStatic();

    const std::string& getXMLReportingName() const;    

    InputOMVar* clone() const;

    virtual void XMLParse( const xercesc::DOMNode* aNode );

    virtual bool isSameType( const std::string& aType ) const;
    

    virtual void toInputXML( std::ostream& aOut,
                             Tabs* aTabs ) const;

    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;

    virtual void copyParam( const IInput* aInput,
                            const int aPeriod );

    virtual void copyParamsInto( InputOMVar& aInput,
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

    InputOMVar();

    //! Cost of the capital input adjusted for the additional costs of the
    //! capture component.
    std::vector<Value> mAdjustedCosts;

    //! Coefficient for production or demand function. Coefficients are not
    // read in and are initialized to 1, but can increase over time with
    // technical change.
    std::vector<Value> mAdjustedCoefficients;

    //! Input specific technical change.
    Value mTechChange;

private:
    //! Variable O&M cost.
    Value mOMVar;

    // Convert to 1975$/GJ for now.
    double calcOMVarCost( void ) const;

    const static std::string XML_REPORTING_NAME; //!< tag name for reporting xml db 

};

#endif // _INPUT_OM_VAR_H_
