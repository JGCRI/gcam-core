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
 * \file readin_control.cpp
 * \ingroup Objects
 * \brief ReadInControl class source file.
 * \author Kate Calvin
 */

#include "util/base/include/definitions.h"

#include "emissions/include/readin_control.h"
#include "containers/include/scenario.h"
#include "containers/include/gdp.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_parse_helper.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/model_time.h"
#include "containers/include/iinfo.h"

using namespace std;

extern Scenario* scenario;

//! Default constructor.
ReadInControl::ReadInControl():
AEmissionsControl(),
mFutureEmissionsFactors(new objects::PeriodVector<double>( 0 ) ),
mTechBuildPeriod( scenario->getModeltime()->getFinalCalibrationPeriod() )
{
}

//! Default destructor.
ReadInControl::~ReadInControl(){
}

//! Copy constructor.
ReadInControl::ReadInControl( const ReadInControl& aOther )
: AEmissionsControl( aOther )
{
    copy( aOther );
}

//! Clone operator.
ReadInControl* ReadInControl::clone() const {
    return new ReadInControl( *this );
}

//! Assignment operator.
ReadInControl& ReadInControl::operator=( const ReadInControl& aOther ){
    if( this != &aOther ){
        // If there was a destructor it would need to be called here.
        AEmissionsControl::operator=( aOther );
        copy( aOther );
    }
    return *this;
}

//! Copy helper function.
void ReadInControl::copy( const ReadInControl& aOther ){
    mFutureEmissionsFactors = aOther.mFutureEmissionsFactors;
}

/*!
 * \brief Get the XML node name for output to XML.
 * \details This public function accesses the private constant string, XML_NAME.
 *          This way the tag is always consistent for both read-in and output and can be easily changed.
 *          This function may be virtual to be overridden by derived class pointers.
 * \return The constant XML_NAME.
 */
const string& ReadInControl::getXMLName() const {
    return getXMLNameStatic();
}

const string& ReadInControl::getXMLNameStatic(){
    static const string XML_NAME = "readin-control";
    return XML_NAME;
}

bool ReadInControl::XMLParse(rapidxml::xml_node<char>* & aNode) {
    string nodeName = XMLParseHelper::getNodeName(aNode);
    if(nodeName == "future-emiss-factor") {
        Data<objects::PeriodVector<double>, ARRAY> futureEmissData(*mFutureEmissionsFactors, "");
        XMLParseHelper::parseData(aNode, futureEmissData);
        return true;
    }
    else {
        return false;
    }
}

void ReadInControl::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    const Modeltime* modeltime = scenario->getModeltime();
	XMLWriteVector( *mFutureEmissionsFactors, "future-emiss-factor", aOut, aTabs, modeltime, 0.0 );

}

void ReadInControl::completeInit( const string& aRegionName, const string& aSectorName,
                               const IInfo* aTechInfo )
{

}

void ReadInControl::initCalc( const string& aRegionName,
                              const IInfo* aTechInfo,
                              const NonCO2Emissions* aParentGHG,
                              const int aPeriod )
{

    if ( aTechInfo->getBoolean( "new-vintage-tech", true ) ) {
        mTechBuildPeriod = aPeriod;
    }
}

void ReadInControl::calcEmissionsReduction( const std::string& aRegionName, const int aPeriod, const GDP* aGDP ) {
    double reduction = 0.0;
    
    if ( (*mFutureEmissionsFactors)[ mTechBuildPeriod ] != 0.0 ) {
        reduction = ( (*mFutureEmissionsFactors)[ mTechBuildPeriod ] - (*mFutureEmissionsFactors)[ aPeriod ] ) / (*mFutureEmissionsFactors)[ mTechBuildPeriod ];
    }
    
    setEmissionsReduction( reduction );
}
