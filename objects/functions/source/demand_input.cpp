/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Laboratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file demand_input.cpp
* \ingroup Objects
* \brief The Demand Input source file.
*
*  Detailed description.
*
* \author Pralit Patel
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include "functions/include/demand_input.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"

using namespace std;

//! Default Constructor
DemandInput::DemandInput() {
}

/*! \brief Creates a clone of this class
 *
 * \author Pralit Patel
 * \warning Does not copy everything, only non calculated values to pass on to the next period
 * \return Pointer to the new class created
 */
DemandInput* DemandInput::clone() const {
    return new DemandInput( *this );
}

void DemandInput::copyParam( const Input* aInput ) {
    Input::copyParam( aInput );
    aInput->copyParamsInto( *this );
}

void DemandInput::copyParamsInto( DemandInput& aDemandInput ) const {
    aDemandInput.mPriceElasticity.init( mPriceElasticity );
    aDemandInput.mIncomeElasticity.init( mIncomeElasticity );
}

//! Get Price Elasticity
double DemandInput::getPriceElasticity() const {
    return mPriceElasticity;
}

//! Get Income Elasticity
double DemandInput::getIncomeElasticity() const {
    return mIncomeElasticity;
}

//! XML parsing for derived class
bool DemandInput::XMLDerivedClassParse( const string& nodeName, const xercesc::DOMNode* curr ) {
    if ( nodeName == "incomeElasticity" ) {
        mIncomeElasticity.init( XMLHelper<double>::getValue( curr ) );
    }
    else if ( nodeName == "priceElasticity" ) {
        mPriceElasticity.init( XMLHelper<double>::getValue( curr ) );
    }
    else {
        return false;
    }
    return true;
}

//! Output XML for derived class
void DemandInput::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    XMLWriteElement( mIncomeElasticity, "incomeElasticity", out, tabs );
    XMLWriteElement( mPriceElasticity, "priceElasticity", out, tabs );
}

//! Output debug info to XML for derived class
void DemandInput::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteElement( mIncomeElasticity, "incomeElasticity", out, tabs );
    XMLWriteElement( mPriceElasticity, "priceElasticity", out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& DemandInput::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& DemandInput::getXMLNameStatic() {
    const static string XML_NAME = "demandInput";
    return XML_NAME;
}

void DemandInput::accept( IVisitor* aVisitor, const int aPeriod ) const {
    Input::accept( aVisitor, aPeriod );
	aVisitor->updateDemandInput( this );
}
