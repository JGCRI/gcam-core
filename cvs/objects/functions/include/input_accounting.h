#ifndef _INPUT_ACCOUNTING_H_
#define _INPUT_ACCOUNTING_H_
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
 * \file input_accounting.h
 * \ingroup Objects
 * \brief InputAccounting class header file.
 * \author Pralit Patel
 */

#include <string>
#include <memory>

#include "functions/include/energy_input.h"


/*! 
 * \ingroup Objects
 * \brief Defines a single energy input which will also add it's demand to
 *        a secondary market for the purposes of tracking.
 * \details Such an input could be useful to track final energy demands for
 *          macro economic calculations.
 *
 *          <b>XML specification for InputAccounting</b>
 *          - XML name: \c input-accounting
 *          - Contained by: Technology
 *          - Parsing inherited from class: EnergyInput
 *          - Attributes:
 *              - \c name MiniCAMInput::mName
 *          - Elements:
 *              - \c tracking-market The seconary market to add demand to.
 *
 * \author Pralit Patel
 */
class InputAccounting : public EnergyInput
{
public:

    InputAccounting();

    virtual ~InputAccounting();

    virtual InputAccounting* clone() const;

    static const std::string& getXMLNameStatic();
    
    virtual const std::string& getXMLName() const;

    virtual const std::string& getXMLReportingName() const;

    virtual void setPhysicalDemand( const double aPhysicalDemand,
                                    const std::string& aRegionName,
                                    const int aPeriod );

protected:
    InputAccounting( const InputAccounting& aOther );
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        EnergyInput,

        //! Coefficient for production or demand function.
        DEFINE_VARIABLE( SIMPLE, "tracking-market", mTrackingMarket, std::string ),
                            
        //! The tracking market name to which to add mCurrencyOutput
        DEFINE_VARIABLE( SIMPLE, "currency-tracking-market", mCurrencyTrackingMarket, std::string ),
                            
        //! A value to be able to add the demand in currency amounts to market
        DEFINE_VARIABLE( SIMPLE | STATE | NOT_PARSABLE, "currency-output", mCurrencyOutput, Value )
    )
    
};

#endif // _INPUT_ACCOUNTING_H_
