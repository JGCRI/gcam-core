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
 * \file input_accounting.cpp
 * \ingroup Objects
 * \brief The InputAccounting class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"

#include "functions/include/input_accounting.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/market_dependency_finder.h"
#include "functions/include/function_utils.h"

using namespace std;

extern Scenario* scenario;

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& InputAccounting::getXMLNameStatic() {
    const static string XML_NAME = "input-accounting";
    return XML_NAME;
}

const string& InputAccounting::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML name for reporting to XML file.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for reporting outputs and can be easily
* changed.
* \author Sonny Kim
* \return The constant XML_NAME.
*/
const string& InputAccounting::getXMLReportingName() const{
    return getXMLNameStatic();
}

//! Constructor
InputAccounting::InputAccounting()
{
}

/*!
 * \brief Destructor.
 * \note An explicit constructor must be defined to avoid the compiler inlining
 *       it in the header file before the header file for the type contained in
 *       the auto_ptr is included.
 */
InputAccounting::~InputAccounting() {
}

/*!
 * \brief Copy constructor.
 * \note This class requires a copy constructor because it has dynamically
 *          allocated memory.
 * \param aOther Energy input from which to copy.
 */
InputAccounting::InputAccounting( const InputAccounting& aOther ):EnergyInput( aOther ),
mTrackingMarket( aOther.mTrackingMarket ),
mCurrencyTrackingMarket( aOther.mCurrencyTrackingMarket )
{
}

InputAccounting* InputAccounting::clone() const {
    return new InputAccounting( *this );
}

void InputAccounting::setPhysicalDemand( double aPhysicalDemand,
                                     const string& aRegionName,
                                     const int aPeriod )
{
    EnergyInput::setPhysicalDemand( aPhysicalDemand, aRegionName, aPeriod );
    
    if(!mTrackingMarket.empty()) {
        scenario->getMarketplace()->addToDemand( mTrackingMarket, aRegionName, mPhysicalDemand[aPeriod], aPeriod, false );
    }

    // we are looking to ultimately land us in 1975 billion dollars:
    // input (EJ), price (75$/GJ), output*price (EJ*75$/GJ=75Bil$)
    // the macro calculations will then be responsible for converting to 1990 million dollars
    mCurrencyOutput = mPhysicalDemand[ aPeriod ]
                    * scenario->getMarketplace()->getPrice( getName(), mMarketName, aPeriod );

    if(!mCurrencyTrackingMarket.empty()) {
        scenario->getMarketplace()->addToDemand( mCurrencyTrackingMarket, aRegionName, mCurrencyOutput,
                                                     aPeriod, false );
    }
}
