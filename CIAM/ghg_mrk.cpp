/*! 
* \file ghg_mrk.cpp
* \ingroup CIAM
* \brief ghg_mrk class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
* \todo Fix the naming issue between the two ghg markets. 
*/

#include "Definitions.h"
#include <cassert>
#include <iostream>
#include <string>

#include <xercesc/dom/DOM.hpp>
#include "xmlHelper.h"

#include "scenario.h"
#include "modeltime.h"
#include "ghg_mrk.h"
#include "Marketplace.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default construtor.
ghg_mrk::ghg_mrk(){
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    constraint.resize( maxper );
    emission.resize( maxper ); // emissions (tgC or MTC)
}

//! Clear member variables.
void ghg_mrk::clear(){
    name = "";
    unit = "";
    market = "";
    constraint.clear();
    emission.clear();
}

//! Create GHG markets
void ghg_mrk::setMarket( const string& regionName ) {

    Marketplace* marketplace = scenario->getMarketplace();

    // name is GHG name
    marketplace->createMarket( regionName, market, name, Marketplace::GHG );
    marketplace->setMarketToSolve ( name, regionName );
    /* no need to use market.setPriceVector here unless GHG markets need
    initial prices read-in for the base year */  
}


//! Initializes data members from XML.
void ghg_mrk::XMLParse( const DOMNode* node ){

    const Modeltime* modeltime = scenario->getModeltime();
    DOMNodeList* nodeList;
    DOMNode* curr = 0;
    string nodeName;

    // PRECONDITION
    /*! \pre assume we are passed a valid node.*/
    assert( node );

    // get the name attribute.
    name = XMLHelper<string>::getAttrString( node, "name" );

#if( _DEBUG )
    cout << "\t Greenhouse gas market name set as " << name << endl;
#endif

    // get all child nodes.
    nodeList = node->getChildNodes();

    // loop through the child nodes.
    for( int i = 0; i < static_cast<int>( nodeList->getLength() ); i++ ){
        curr = nodeList->item( i );
        nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }

        else if( nodeName == "market" ){
            market = XMLHelper<string>::getValueString( curr ); // should be only one market
        }

        else if( nodeName == "constraint" ){
            XMLHelper<double>::insertValueIntoVector( curr, constraint, modeltime );
        }
        else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing ghgmarket." << endl;
        }
    }
}

//! Writes datamembers to datastream in XML format.
void ghg_mrk::toXML( ostream& out ) const {

    // write the beginning tag.
    Tabs::writeTabs( out );
    out << "<ghgmarket name=\"" << name << "\">" << endl;

    // increase the indent.
    Tabs::increaseIndent();

    // write the xml for the class members.
    XMLWriteElement( unit, "unit", out );
    // write out the market string.
    XMLWriteElement( market, "market", out );

    for( vector<double>::const_iterator i = constraint.begin(); i != constraint.end(); i++ ){
        Tabs::increaseIndent();
        Tabs::writeTabs( out );

        out << "<period>" << endl;

        Tabs::increaseIndent();
        XMLWriteElement( *i, "constraint", out );
        Tabs::decreaseIndent();

        Tabs::writeTabs( out );
        out << "</period>" << endl;	
        Tabs::decreaseIndent();
    }

    // finished writing xml for the class members.

    // decrease the indent.
    Tabs::decreaseIndent();

    // write the closing tag.
    Tabs::writeTabs( out );
    out << "</ghgmarket>" << endl;
}

//! Writes datamembers to datastream in XML format.
void ghg_mrk::toDebugXML( const int period, ostream& out ) const {

    // write the beginning tag.
    Tabs::writeTabs( out );
    out << "<ghgmarket name=\"" << name << "\">" << endl;

    // increase the indent.
    Tabs::increaseIndent();

    // Write the xml for the class members.
    XMLWriteElement( unit, "unit", out );

    // write out the market string.
    XMLWriteElement( market, "market", out );

    // Write the constraint for the current year
    XMLWriteElement( constraint[ period ], "constraint", out );

    XMLWriteElement( emission[ period ], "emission", out );
    // finished writing xml for the class members.

    // decrease the indent.
    Tabs::decreaseIndent();

    // write the closing tag.
    Tabs::writeTabs( out );
    out << "</ghgmarket>" << endl;
}

//! Set emissions.
void ghg_mrk::setEmission( const double amount, const int per )
{
    emission[ per ] = amount; // emissions (tgC or MTC)
}

//! Show emission name.
string ghg_mrk::getName() const
{
    return name;
}

//! Return emissions target.
double ghg_mrk::getConstraint( const int per ) const
{
    return constraint[ per ]; // emissions constraint (tgC or MTC)
}

//! Return emissions.
double ghg_mrk::getEmission( const int per ) const
{
    return emission[ per ]; // emissions (tgC or MTC)

}

