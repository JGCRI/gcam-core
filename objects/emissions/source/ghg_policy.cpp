/*! 
* \file ghg_policy.cpp
* \ingroup CIAM
* \brief GHGPolicy class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <iostream>
#include <string>

#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "emissions/include/ghg_policy.h"
#include "marketplace/include/marketplace.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default construtor.
GHGPolicy::GHGPolicy( const string nameIn, const string unitIn, const string marketIn, const bool isFixedTaxIn )
: name( nameIn ), unit( unitIn ), market( marketIn ), isFixedTax( isFixedTaxIn ) {
    const int maxper = scenario->getModeltime()->getmaxper();
    constraint.resize( maxper );
    fixedTaxes.resize( maxper );
    emissions.resize( maxper );
}

//! Clear member variables.
void GHGPolicy::clear(){
    name = "";
    unit = "";
    market = "";
    isFixedTax = false;
    constraint.clear();
    fixedTaxes.clear();
    emissions.clear();
}

//! Get the ghg policy name. 
string GHGPolicy::getName() const {
    return name;
}

//! Set GHG emissions. Do we need this here?
void GHGPolicy::setEmission( const double emission, const int period ) {
    emissions[ period ] = emission;
}

/*! \brief Create a market for this GHG policy.
* \details This function initializes a ghg market for the policy.
* GHG markets are created for both constraint and fixed tax policies.
* In the fixed tax policy, market prices are set to the fixed taxes, but
* the markets are not solved.  Also for the fixed tax policy, if the market name
* is the same for all regions, the fixed tax vector of the last region overrides
* the market prices.
* \author Sonny Kim and Josh Lurz
* \param regionName The name of the region the policy controls. 
*/
void GHGPolicy::setMarket( const string& regionName ) {
    Marketplace* marketplace = scenario->getMarketplace();
    bool marketCreated = marketplace->createMarket( regionName, market, name, Marketplace::GHG );
    
    // Put the taxes in the market as the market prices if it is a fixed tax policy.
    // And the market has not previously been initialized.
    // This allows the taxes to be set in a single region and used in all.
    if( isFixedTax ){
        if( marketCreated ){
            marketplace->setPriceVector( name, regionName, fixedTaxes );
        }
    } 
    // Otherwise solve the market, given the read-in constraint.
    else {
        marketplace->setMarketToSolve ( name, regionName );
    }
}

/*! \brief Add the allowed amount of the GHG to the market created for it.
* \details This function will add the amount specified in the read in constraint
* to the supply for the specific greenhouse gas within the market. If this policy
* is a fixed tax, the market will not be solved the amount add here has no effect, 
* although it should be zero. 
*
* \author Josh Lurz
* \param regionName The region name
* \param period Period in which to add the supply.
*/
void GHGPolicy::addGHGSupply( const string& regionName, const int period ) const {
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->addToSupply( name, regionName, constraint[ period ], period );	
}

/*! \brief Convert a policy from a constraint based policy to a fixed tax policy.
* \details This function resets a market related to a ghg policy to a fixed tax 
* policy market. This means the market is no longer solved, and taxes must be passed
* into it. The tax is initialized to zero for all periods. 
* This function is primarily used to create a carbon mitigation cost curve. 
* \author Josh Lurz
* \param regionName The name of the region for which the market must be modified.
*/
void GHGPolicy::changePolicyToFixedTax( const string& regionName ) {
    
    // First remove the constraints. This is not strictly neccessary but is clearer.
    const int maxPeriod = scenario->getModeltime()->getmaxdataper();
    constraint.clear();
    constraint.resize( maxPeriod, 0 );
    
    // Now set the tax to 0 for all periods.
    fixedTaxes.clear();
    fixedTaxes.resize( maxPeriod, 0 );
    
    // Set the internal variable noting that this is a fixed tax policy.
    isFixedTax = true;
    
    // Set the market not to solve.
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->unsetMarketToSolve( name, regionName );
    
    // Set the fixed tax to zero for all periods. 
    marketplace->setPriceVector( name, regionName, fixedTaxes );
}

/*! \brief Set the tax for a fixed tax policy.
* \details This function resets the fixed taxes for all periods to a given value.
* \warning This will only work if the market has been set not to solve.
* \author Josh Lurz
* \param regionName The region name this policy applies to.
* \param taxes A vector of taxes to set for each period.
*/
void GHGPolicy::setFixedTaxes( const string& regionName, const vector<double>& taxes ){
    
    /*! \pre This is a fixed tax policy. */
    assert( isFixedTax );
    
    // Set the policy tax to the new tax value for all periods.
    const int maxPeriod = scenario->getModeltime()->getmaxdataper();
    fixedTaxes.clear();
    fixedTaxes.resize( maxPeriod );
    
    // Set the value. If the fixedTaxes vector is larger than the taxes vector,
    // use the last input tax found. 
    double lastInputTax = 0;
    for( int i = 0; i < static_cast<int>( fixedTaxes.size() ); i++ ){
        if( static_cast<int>( taxes.size() ) > i ){
            lastInputTax = taxes[ i ];
        }
        fixedTaxes[ i ] = lastInputTax;
    }

    // Add the new tax into the marketplace as the tax. 
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->setPriceVector( name, regionName, fixedTaxes );
}

//! Initializes data members from XML.
void GHGPolicy::XMLParse( const DOMNode* node ){

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
        else if( nodeName == "isFixedTax" ) {
            isFixedTax = XMLHelper<bool>::getValue( curr );
        }
        else if( nodeName == "constraint" ){
            XMLHelper<double>::insertValueIntoVector( curr, constraint, modeltime );
        }
        else if( nodeName == "fixedTax" ){
            XMLHelper<double>::insertValueIntoVector( curr, fixedTaxes, modeltime );
        }
        else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing ghgmarket." << endl;
        }
    }
}

//! Writes datamembers to datastream in XML format.
void GHGPolicy::toXML( ostream& out, Tabs* tabs ) const {

	int m = 0;
    const Modeltime* modeltime = scenario->getModeltime();

    // write the beginning tag.
    tabs->writeTabs( out );
    out << "<ghgmarket name=\"" << name << "\">" << endl;

    // increase the indent.
    tabs->increaseIndent();

    // write the xml for the class members.
    XMLWriteElement( unit, "unit", out, tabs );
    // write out the market string.
    XMLWriteElement( market, "market", out, tabs );
    XMLWriteElement( isFixedTax, "isFixedTax", out, tabs );
    
    for( int i = 0; i < scenario->getModeltime()->getmaxper(); i++ ){
        tabs->increaseIndent();
        tabs->writeTabs( out );

        out << "<period>" << endl;

        tabs->increaseIndent();
        XMLWriteElementCheckDefault( constraint[ i ], "constraint", out, tabs, 0.0 );
        XMLWriteElementCheckDefault( fixedTaxes[ i ], "fixedTax", out, tabs, 0.0 );
        XMLWriteElementCheckDefault( emissions[ i ], "emissions", out, tabs, 0.0 );
        tabs->decreaseIndent();
        
        tabs->writeTabs( out );
        out << "</period>" << endl;	
        tabs->decreaseIndent();
    }

    // finished writing xml for the class members.

    // decrease the indent.
    tabs->decreaseIndent();

    // write the closing tag.
    tabs->writeTabs( out );
    out << "</ghgmarket>" << endl;
}

//! Writes datamembers to datastream in XML format.
void GHGPolicy::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    // write the beginning tag.
    tabs->writeTabs( out );
    out << "<ghgmarket name=\"" << name << "\">" << endl;

    // increase the indent.
    tabs->increaseIndent();

    // Write the xml for the class members.
    XMLWriteElement( unit, "unit", out, tabs );

    // write out the market string.
    XMLWriteElement( market, "market", out, tabs );
    
    // Write whether we are a fixed tax policy.
    XMLWriteElement( isFixedTax, "isFixedTax", out, tabs );
    
    // Write the constraint for the current year
    XMLWriteElement( constraint[ period ], "constraint", out, tabs );
    
    // Write out the fixed tax for the current year.
    XMLWriteElement( fixedTaxes[ period ], "fixedTax", out, tabs );
    
    // Write out the emissions for the periods.
    XMLWriteElement( emissions[ period ], "emissions", out, tabs );
    
    // finished writing xml for the class members.

    // decrease the indent.
    tabs->decreaseIndent();

    // write the closing tag.
    tabs->writeTabs( out );
    out << "</ghgmarket>" << endl;
}
