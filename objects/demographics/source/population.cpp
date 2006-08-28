/*! 
* \file population.cpp
* \ingroup Objects
* \brief Population class source file.
* \author Sonny Kim, Katherine Chung, Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <string>
#include <map>
#include <cassert>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "containers/include/scenario.h"
#include "demographics/include/population.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/ivisitor.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default constructor.
Population::Population(){
    mYear = -1;
    mTotalPop = -1;
    mWorkingAgeMin = WORKING_AGE_MIN_DEFAULT;
    mWorkingAgeMax = WORKING_AGE_MAX_DEFAULT;
}

//! Population destructor. 
Population::~Population(){
}

//! Returns total population for this year
double Population::getTotal() const {
    assert( mTotalPop != -1 );
    return mTotalPop;
}

//! Returns year of population
int Population::getYear() const {
    assert( mYear != -1 );
    return mYear;
}

//! Returns name (year as a string)
const std::string Population::getName() const {
    return util::toString( mYear );
}

//! parses Population xml object
void Population::XMLParse( const xercesc::DOMNode* node ){
    /*! \pre make sure we were passed a valid node. */
    assert( node );

    DOMNodeList* nodeList = node->getChildNodes();

    // get the year attribute
    mYear = XMLHelper<int>::getAttr( node, "year" ); 
    for( unsigned int i = 0; i < nodeList->getLength(); ++i ){
        DOMNode* curr = nodeList->item( i );
        // get the name of the node.
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == "min-working-age" ){
            mWorkingAgeMin = XMLHelper<int>::getValue( curr );
        }
        else if( nodeName == "max-working-age" ){
            mWorkingAgeMax = XMLHelper<int>::getValue( curr );
        }
        else if( XMLDerivedClassParse( nodeName, curr ) ){
            // do nothing but dont warn.
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing " << getXMLName() << endl;
        }
    }
}

//! Write out datamembers to XML output stream.
void Population::toInputXML( std::ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag ( getXMLName(), out, tabs , "", mYear);

    XMLWriteElementCheckDefault( mTotalPop, "totalPop", out, tabs );
    XMLWriteElementCheckDefault( mWorkingAgeMin, "min-working-age", out, tabs, WORKING_AGE_MIN_DEFAULT );
    XMLWriteElementCheckDefault( mWorkingAgeMax, "max-working-age", out, tabs, WORKING_AGE_MAX_DEFAULT );
    // write out variables for derived classes
    toInputXMLDerived( out, tabs );

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Write out XML for debugging purposes.
void Population::toDebugXML( std::ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag ( getXMLName(), out, tabs , "", mYear);

    XMLWriteElement( mTotalPop, "totalPop", out, tabs );
    XMLWriteElement( mWorkingAgeMin, "min-working-age", out, tabs );
    XMLWriteElement( mWorkingAgeMax, "max-working-age", out, tabs );
    // write out variables for derived classes
    toDebugXMLDerived( out, tabs );

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

void Population::csvSGMOutputFile( ostream& aFile, const int period ) const {
    aFile << "Population Data Total" << endl;
    aFile << "Year" << ',' << "Total" << endl;
    aFile << mYear << ',' << mTotalPop << endl << endl;
}

/*! \brief Update a Visitor with information about a Population.
* \param aVisitor Visitor to update.
* \param aPeriod Period for which to update.
*/
void Population::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitPopulation( this, aPeriod );
    aVisitor->endVisitPopulation( this, aPeriod );
}
