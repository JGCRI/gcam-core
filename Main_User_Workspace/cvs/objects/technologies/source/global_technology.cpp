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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
* \file global_technology.cpp
* \ingroup Objects
* \brief GlobalTechnology source file.
* \author Pralit Patel
*/
// Standard Library headers
#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "technologies/include/global_technology.h"
#include "util/base/include/xml_helper.h"

using namespace std;
using namespace xercesc;

GlobalTechnology::GlobalTechnology()
: mBaseEfficiency( 1 ),
mBaseNonEnergyCost( 0 ),
fMultiplier( 1 ), 
fuelPrefElasticity( 0 ) {
}

/*! Should not be called because GlobalTechnologies can not be cloned.
 * \warning will assert(false)
 * \return Nothing.
 */
ITechnologyInfo* GlobalTechnology::clone() {
    assert(false);
    return 0;
}

void GlobalTechnology::completeInit() {
    // Check for nonsensical efficiency.
    if( mBaseEfficiency <= 0 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Resetting invalid efficiency for Technology " << name << endl;
        mBaseEfficiency =  1;
    }
}

/*! Parses an identifier which would uniquely identifies a GlobalTechnology.
 *  Needs to be unique so that we can correctly identify the same technology
 *  during parsing.
 * \param tempNode The node to look for identifier attributes.
 * \return A unique identifier.
 */
const string GlobalTechnology::parseIdentifier( const DOMNode *tempnode ) {
    return XMLHelper<string>::getAttr( tempnode, XMLHelper<string>::name() ) +  
        XMLHelper<string>::getAttr( tempnode, "year" );
}

void GlobalTechnology::XMLParse( const DOMNode *tempnode ) {
    // assume we are passed a valid node.
    assert( tempnode );

    name = XMLHelper<string>::getAttr( tempnode, XMLHelper<string>::name() );
    year = XMLHelper<int>::getAttr( tempnode, "year" );

    // get all the children.
    DOMNodeList* nodeList = tempnode->getChildNodes();

    for( unsigned int i = 0;  i < nodeList->getLength(); ++i ){
        DOMNode* curr = nodeList->item( i );
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == "fuelname" ){
            fuelname = XMLHelper<string>::getValue( curr );
        }
        else if( nodeName == "fuelprefElasticity" ){
            fuelPrefElasticity = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "efficiency" ){
            mBaseEfficiency = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "nonenergycost" ){
            mBaseNonEnergyCost = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "fMultiplier" ){
            fMultiplier = XMLHelper<double>::getValue( curr );
        } else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Unknown element " << nodeName << " encountered while parsing " << getXMLNameStatic() << endl;
        }
    }
}

//! write object to xml output stream
void GlobalTechnology::toInputXML( ostream &out, Tabs *tabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), out, tabs, name, year );

    XMLWriteElement( fuelname, "fuelname", out, tabs );
    XMLWriteElementCheckDefault( mBaseEfficiency, "efficiency", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( mBaseNonEnergyCost, "nonenergycost", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( fuelPrefElasticity, "fuelprefElasticity", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( fMultiplier, "fMultiplier", out, tabs, 1.0 );

    XMLWriteClosingTag( getXMLNameStatic(), out, tabs );
}

//! write object to xml debugging output stream
void GlobalTechnology::toDebugXML( int period, ostream &out, Tabs *tabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), out, tabs, name, year );

    XMLWriteElement( fuelname, "fuelname", out, tabs );
    XMLWriteElementCheckDefault( mBaseEfficiency, "efficiency", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( mBaseNonEnergyCost, "nonenergycost", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( fuelPrefElasticity, "fuelprefElasticity", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( fMultiplier, "fMultiplier", out, tabs, 1.0 );

    XMLWriteClosingTag( getXMLNameStatic(), out, tabs );
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
const std::string& GlobalTechnology::getXMLNameStatic() {
    static const string XML_NAME = "global-technology";
    return XML_NAME;
}

/*! Get the year of this GlobalTechnology
 *
 * \return The year.
 */
const int GlobalTechnology::getYear() const {
    return year;
}

const string& GlobalTechnology::getName() const {
    return name;
}

const string& GlobalTechnology::getFuelName() const {
    return fuelname;
}

const double GlobalTechnology::getEfficiency() const {
    return mBaseEfficiency;
}

const double GlobalTechnology::getNonEnergyCost() const {
    return mBaseNonEnergyCost;
}

const double GlobalTechnology::getFMultiplier() const {
    return fMultiplier;
}

const double GlobalTechnology::getFuelPrefElasticity() const {
    return fuelPrefElasticity;
}

void GlobalTechnology::setFuelName( const string& aFuelName ) {
    assert( false );
}

void GlobalTechnology::setEfficiency( const double aEfficiency ) {
    assert( false );
}

void GlobalTechnology::setNonEnergyCost( const double aNonEnergyCost ) {
    assert( false );
}

void GlobalTechnology::setFMultiplier( const double aFMultiplier ) {
    assert( false );
}

void GlobalTechnology::setFuelPrefElasticity( const double aFuelPrefElasticity ) {
    assert( false );
}
