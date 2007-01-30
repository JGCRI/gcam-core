/*
* This software, which is provided in confidence, was prepared by employees of
* Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
* Battelle has certain unperfected rights in the software which should not be
* copied or otherwise disseminated outside your organization without the express
* written authorization from Battelle. All rights to the software are reserved
* by Battelle. Battelle makes no warranty, express or implied, and assumes no
* liability or responsibility for the use of this software.
*/

/*! 
* \file input_driver.cpp
* \ingroup Objects
* \brief InputDriver source file.
* \author Jim Naslund
*/

#include "util/base/include/definitions.h"

#include "emissions/include/input_driver.h"

using namespace std;

double InputDriver::calcEmissionsDriver( const double aInputIn,
                                         const double aOutputIn ) const {
    return aInputIn;
}

InputDriver* InputDriver::clone() const {
    return new InputDriver( *this );
}

const string& InputDriver::getXMLName() const {
    return getXMLNameStatic();
}

const string& InputDriver::getXMLNameStatic(){
    static const string XML_NAME = "input-driver";
    return XML_NAME;
}
