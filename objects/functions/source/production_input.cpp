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
* \file production_input.cpp
* \ingroup Objects
* \brief The Production Input class source file.
*
*  Detailed description.
*
* \author Pralit Patel
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include "functions/include/production_input.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"

using namespace std;
using namespace xercesc;

//! Default Constructor
ProductionInput::ProductionInput() {
}

/*! \brief Creates a clone of this class
 *
 * \author Pralit Patel
 * \warning Does not copy everything, only non calculated values to pass on to the next period
 * \return Pointer to the new class created
 */
ProductionInput* ProductionInput::clone() const {
    return new ProductionInput( *this );
}

void ProductionInput::copyParam( const Input* aInput ) {
    Input::copyParam( aInput );
    aInput->copyParamsInto( *this );
}

void ProductionInput::copyParamsInto( ProductionInput& aProductionInput ) const {
}

//! XML parsing for derived class
bool ProductionInput::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    return false;
}

//! Output XML for derived class
void ProductionInput::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
}

//! Output debug info to XML for derived class
void ProductionInput::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
}

//! Get Price Elasticity
double ProductionInput::getPriceElasticity() const {
    return 0; // makes no sense here
}

//! Get Income Elasticity
double ProductionInput::getIncomeElasticity() const {
    return 0; // makes no sense here
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& ProductionInput::getXMLName() const {
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
const string& ProductionInput::getXMLNameStatic() {
    const static string XML_NAME = "productionInput";
    return XML_NAME;
}

void ProductionInput::accept( IVisitor* aVisitor, const int aPeriod ) const {
    Input::accept( aVisitor, aPeriod );
	aVisitor->updateProductionInput( this );
}
