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
* \file output_driver.cpp
* \ingroup Objects
* \brief OutputDriver source file.
* \author Jim Naslund
*/

#include "util/base/include/definitions.h"

#include "emissions/include/output_driver.h"

using namespace std;

double OutputDriver::calcEmissionsDriver( const double aInputIn,
                                          const double aOutputIn ) const {
    return aOutputIn;
}

OutputDriver* OutputDriver::clone() const {
    return new OutputDriver( *this );
}

const string& OutputDriver::getXMLName() const {
    return getXMLNameStatic();
}

const string& OutputDriver::getXMLNameStatic(){
    static const string XML_NAME = "output-driver";
    return XML_NAME;
}
