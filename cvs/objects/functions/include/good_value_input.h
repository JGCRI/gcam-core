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


#ifndef _GOOD_VALUE_INPUT_H_
#define _GOOD_VALUE_INPUT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file good_value_input.h
 * \ingroup Objects
 * \brief GoodValueInput class header file.
 * \author Pralit Patel
 */

#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "functions/include/energy_input.h"

class Tabs;

/*! 
 * \ingroup Objects
 * \brief An input which adds the value of it's good to a market.
 * \details TODO
 *
 *          <b>XML specification for GoodValueInput</b>
 *          - XML name: \c renewable-input
 *          - Contained by: Technology
 *          - Parsing inherited from class: EnergyInput
 *          - Attributes:
 *              - \c name MiniCAMInput::mName
 *          - Elements: None.
 *
 * \author Pralit Patel
 */
class GoodValueInput: public EnergyInput
{
    friend class InputFactory;
    friend class UnmanagedLandTechnology;
public:
    static const std::string& getXMLNameStatic();

    const std::string& getXMLReportingName() const;
    
    virtual GoodValueInput* clone() const;

    virtual bool isSameType( const std::string& aType ) const;
    

    virtual void setPhysicalDemand( const double aPhysicalDemand,
                                    const std::string& aRegionName,
                                    const int aPeriod );
protected:
    GoodValueInput();

    // Constuctor to allow internal creation of this object
    GoodValueInput( const std::string& aName );

};

#endif // _GOOD_VALUE_INPUT_H_
