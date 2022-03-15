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
 * \file efficiency.cpp
 * \ingroup Objects
 * \brief The Efficiency class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include "functions/include/efficiency.h"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"

using namespace std;

Efficiency::Efficiency():mReadInEfficiency(0) {
}

Efficiency::~Efficiency() {
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
* \details This public function accesses the private constant string, XML_NAME.
*          This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& Efficiency::getXMLNameStatic() {
    const static string XML_NAME = "efficiency";
    return XML_NAME;
}

const string& Efficiency::getXMLName() const {
    return getXMLNameStatic();
}

/*!
 * \brief Constructor
 * \param aEfficiency An efficiency.
 */
Efficiency::Efficiency( const double aEfficiency )
{
    mReadInEfficiency = aEfficiency;
}

Efficiency* Efficiency::clone() const {
    Efficiency* clone = new Efficiency( mReadInEfficiency );
    return clone;
}

bool Efficiency::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

const string& Efficiency::getName() const {
    // Coefficients do not have unique names.
    return getXMLNameStatic();
}

void Efficiency::toDebugXML( const int period,
                             ostream& aOut,
                             Tabs* aTabs ) const
{
    XMLWriteElement( mReadInEfficiency, getXMLNameStatic(), aOut, aTabs );
}

void Efficiency::completeInit() {
    // Error check the efficiency.
    if( mReadInEfficiency < util::getSmallNumber() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Resetting invalid efficiency to 1." << endl;
        mReadInEfficiency = 1;
    }
}

double Efficiency::getCoefficient() const {
    return 1 / mReadInEfficiency;
}
