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
* \file aemissions_control.cpp
* \ingroup Objects
* \brief AEmissionsControl class source file.
* \author Kate Calvin
*/

#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "emissions/include/aemissions_control.h"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace xercesc;

//! Default constructor.
AEmissionsControl::AEmissionsControl():
mReduction( 0 )
{
}

//! Destructor
AEmissionsControl::~AEmissionsControl(){
}

//! Copy constructor.
AEmissionsControl::AEmissionsControl( const AEmissionsControl& aOther ){
    copy( aOther );
}

//! Assignment operator.
AEmissionsControl& AEmissionsControl::operator=( const AEmissionsControl& aOther ){
    if( this != &aOther ){
        // If there was a destructor it would need to be called here.
        copy( aOther );
    }
    return *this;
}

//! Copy helper function.
void AEmissionsControl::copy( const AEmissionsControl& aOther ){
    mName = aOther.mName;
    //mReduction = aOther.mReduction;
}

//! \brief initialize emissions control object with xml data
void AEmissionsControl::XMLParse(const DOMNode* aNode) {
    /*! \pre Assume we are passed a valid node. */
    assert( aNode );

    DOMNodeList* nodeList = aNode->getChildNodes();

    // Parse the name attribute.
    mName = XMLHelper<string>::getAttr( aNode, "name" );

    for( unsigned int i = 0; i < nodeList->getLength(); ++i ) {
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );      

        if( nodeName == "#text" ){
            continue;
        }
        else if( XMLDerivedClassParse( nodeName, curr ) ){
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing AEmissionsControl." << endl;
        }
    }
}

//! Writes datamembers to datastream in XML format.
void AEmissionsControl::toInputXML( ostream& aOut, Tabs* aTabs ) const {

    XMLWriteOpeningTag( getXMLName(), aOut, aTabs, getName() );
    
    // write xml for data members
    toInputXMLDerived( aOut, aTabs );
    // done writing xml for data members.

    XMLWriteClosingTag( getXMLName(), aOut, aTabs );

}
//! Writes datamembers to debugging datastream in XML format.
void AEmissionsControl::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {

    XMLWriteOpeningTag( getXMLName(), aOut, aTabs, getName() );

    // write xml for data members
    XMLWriteElement( mReduction, "reduction", aOut, aTabs );

    toDebugXMLDerived( aPeriod, aOut, aTabs );
    // done writing xml for data members.

    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

/*!
 * \brief Get the name fo the emissions reduction
 * \return The name of the reduction.
 */
const string& AEmissionsControl::getName() const {
    return mName;
}

double AEmissionsControl::getEmissionsReduction( const std::string& aRegionName, const int aPeriod, const GDP* aGDP ){
    calcEmissionsReduction( aRegionName, aPeriod, aGDP );
    return mReduction;
}

void AEmissionsControl::setEmissionsReduction( double aReduction ){
    mReduction = aReduction;
}
