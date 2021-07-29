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
 * \file linear_interpolation_function.cpp
 * \ingroup Objects
 * \brief LinearInterpolationFunction class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include <string>

#include "util/base/include/linear_interpolation_function.h"
#include "util/curves/include/data_point.h"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"

class Tabs;

using namespace std;

//! Default Constructor
LinearInterpolationFunction::LinearInterpolationFunction() {
}

//! Destructor
LinearInterpolationFunction::~LinearInterpolationFunction() {
}

IInterpolationFunction* LinearInterpolationFunction::clone() const {
    // no parameters to copy
    return new LinearInterpolationFunction();
}

/*!
 * \brief The value for the xml name attribute which identifies this
 *        interpolation function.
 * \details The approach for using the name attribute value rather than
 *          the element name was taking to make generating the tags
 *          easier.
 * \return The string which identifies this function.
 * \see InterpolationFunctionFactory
 */
const string& LinearInterpolationFunction::getXMLNameStatic() {
    const static string XML_NAME = "linear";
    return XML_NAME;
}

void LinearInterpolationFunction::toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const {
    XMLWriteElement("", IInterpolationFunction::getXMLNameStatic(), aOut, aTabs, 0, getXMLNameStatic() );
}

double LinearInterpolationFunction::interpolate( const DataPoint* aLeftPoint, const DataPoint* aRightPoint,
                                                 const double aXValue ) const
{
    // TODO: error checking
    return ( aXValue - aLeftPoint->getX() )
        * ( aRightPoint->getY() - aLeftPoint->getY() ) / ( aRightPoint->getX() - aLeftPoint->getX() )
        + aLeftPoint->getY();
}
