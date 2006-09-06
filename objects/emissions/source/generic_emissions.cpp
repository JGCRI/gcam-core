/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

/*! 
 * \file generic_emissions.h
 * \ingroup Objects
 * \brief GenericEmissions class header file.
 * \author Jim Naslund
 */

#include "util/base/include/definitions.h"

#include "emissions/include/generic_emissions.h"
#include "emissions/include/aemissions_driver.h"
#include "util/base/include/xml_helper.h"
#include "emissions/include/input_driver.h"

using namespace std;
using namespace xercesc;

//! Default Destructor.
GenericEmissions::~GenericEmissions(){
}

//! Clone operator.
GenericEmissions* GenericEmissions::clone() const {
    return new GenericEmissions( *this );
}

const string& GenericEmissions::getXMLNameStatic(){
    static const string XML_NAME = "generic-ghg";
    return XML_NAME;
}

const string& GenericEmissions::getName() const {
    return mName;
}

void GenericEmissions::initCalc( const string& aRegionName,
                                 const string& aFuelName,
                                 const IInfo* aLocalInfo,
                                 const int aPeriod )
{
    AComplexEmissions::initCalc( aRegionName, aFuelName, aLocalInfo, aPeriod );
    if( !mEmissionsDriver.get() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "No driver set for generic-ghg: " << getName()
                << "defaulting to an input driver." << endl;
        mEmissionsDriver.reset( new InputDriver );
    }
}

const string& GenericEmissions::getXMLName() const {
    return getXMLNameStatic();
}

// Assign mName to passed in name.
void GenericEmissions::parseName( const string& aNameAttr ){
    mName = aNameAttr;
}

void GenericEmissions::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    AComplexEmissions::toInputXMLDerived( out, tabs );
    // There is no value because the name of the driver is what is signifigant
    XMLWriteElement( "", mEmissionsDriver->getXMLName(),
                     out, tabs );
}

void GenericEmissions::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    AComplexEmissions::toDebugXMLDerived( period, out, tabs );
    // There is no value because the name of the driver is what is signifigant
    XMLWriteElement( "", mEmissionsDriver->getXMLName(),
                     out, tabs );
}
