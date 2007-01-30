/*! 
* \file ghg_policy.cpp
* \ingroup Objects
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
#include "containers/include/iinfo.h"
#include "util/base/include/model_time.h"
#include "emissions/include/ghg_policy.h"
#include "marketplace/include/marketplace.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string GHGPolicy::XML_NAME = "ghgpolicy";

/*! \brief Default constructor. */
GHGPolicy::GHGPolicy()
: isFixedTax( false ),
constraint( scenario->getModeltime()->getmaxper(), -1 ),
fixedTaxes( scenario->getModeltime()->getmaxper(), -1 )
{
}

/*!
* \brief Constructor which initializes a GHG policy without setting a tax or
*        constraint.
*/
GHGPolicy::GHGPolicy( const string aName,
                      const string aMarket )
: mName( aName ), 
mMarket( aMarket ),
isFixedTax( false ),
constraint( scenario->getModeltime()->getmaxper(), -1 ),
fixedTaxes( scenario->getModeltime()->getmaxper(), -1 )
{
}

/*! \brief Constructor used when explicitly constructing a fixed tax.
*/
GHGPolicy::GHGPolicy( const string aName,
                      const string aMarket,
                      const vector<double>& aTaxes )
: mName( aName ), 
mMarket( aMarket ),
fixedTaxes( aTaxes ),
isFixedTax( true ),
constraint( scenario->getModeltime()->getmaxper(), -1 ){
    // Ensure that the taxes vector passed in is the right size.
    assert( aTaxes.size() == constraint.size() );
}

/*! \brief Create a copy of the GHG policy.
* \return An exact copy of the policy.
*/
GHGPolicy* GHGPolicy::clone() const {
    return new GHGPolicy( *this );
}

//! Get the ghg policy name. 
const string& GHGPolicy::getName() const {
    return mName;
}

/*! \brief Complete the initialization of the GHG policy.
* \details This function initializes a ghg market for the policy.
* GHG markets are created for both constraint and fixed tax policies.
* In the fixed tax policy, market prices are set to the fixed taxes, but
* the markets are not solved.  Also for the fixed tax policy, if the market name
* is the same for all regions, the fixed tax vector of the last region overrides
* the market prices.
* \author Sonny Kim and Josh Lurz
* \param regionName The name of the region the policy controls. 
*/
void GHGPolicy::completeInit( const string& aRegionName ) {
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->createMarket( aRegionName, mMarket, mName, IMarketType::GHG );

    // Set price and output units for period 0 market info
    IInfo* marketInfo = marketplace->getMarketInfo( mName, aRegionName, 0, true );
    //TODO: read-in as data the units of tax and emissions
    marketInfo->setString( "price-unit", "1990$/tC" );
    marketInfo->setString( "output-unit", "MTC" );
    
    // Put the taxes in the market as the market prices if it is a fixed tax policy.
    if( isFixedTax ){
        // Set any taxes that are not unset.
        for( unsigned int i = 0; i < fixedTaxes.size(); ++i ){
            // Make sure that the market is not solved. It could have been set
            // to solve by an earlier run.
            marketplace->unsetMarketToSolve( mName, aRegionName, i );
            if( fixedTaxes[ i ] != -1 ){
                marketplace->setPrice( mName, aRegionName, fixedTaxes[ i ], i );
            }
        }
    }       
    // Otherwise solve the market, given the read-in constraint.
    else {
        const Modeltime* modeltime = scenario->getModeltime();
        for( int per = 1; per < modeltime->getmaxper(); ++per ){
            if( constraint[ per ] != -1 ){
                marketplace->setMarketToSolve( mName, aRegionName, per );
                // Adding the difference between the constraint for this period
                // and the current supply because addToSupply adds to the current
                // supply.  Passing false to suppress a warning the first time through.
                marketplace->addToSupply( mName, aRegionName, constraint[ per ] - 
                    marketplace->getSupply( mName, aRegionName, per ), per, false );
            }
        }
    }
}

/*! \brief Determine if the tax is applicable for a given region.
* \param aRegion Region name.
* \return Whether the tax is applicable.
* \todo This is not entirely correct for multiple regions within a market.
*/
bool GHGPolicy::isApplicable( const string& aRegion ) const {
    return mMarket == "global" || mMarket == aRegion;
}

/*!
* \brief Set the constraint to the vector passed in.
* \param aConstraint new constraint vector
*/
void GHGPolicy::setConstraint( const vector<double>& aConstraint ){
    isFixedTax = false;
    constraint = aConstraint;
}

//! Initializes data members from XML.
void GHGPolicy::XMLParse( const DOMNode* node ){

    /*! \pre assume we are passed a valid node.*/
    assert( node );

    // get the name attribute.
    mName = XMLHelper<string>::getAttr( node, "name" );

    // get all child nodes.
    DOMNodeList* nodeList = node->getChildNodes();
    const Modeltime* modeltime = scenario->getModeltime();
    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == "market" ){
            mMarket = XMLHelper<string>::getValue( curr ); // should be only one market
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

//! Writes data members to data stream in XML format.
void GHGPolicy::toInputXML( ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag( getXMLName(), out, tabs, mName );
    XMLWriteElement( mMarket, "market", out, tabs );
    XMLWriteElement( isFixedTax, "isFixedTax", out, tabs );
    
    const Modeltime* modeltime = scenario->getModeltime();    
    XMLWriteVector( constraint, "constraint", out, tabs, modeltime, -1.0 );
    XMLWriteVector( fixedTaxes, "fixedTax", out, tabs, modeltime, 0.0 );

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Writes data members to data stream in XML format.
void GHGPolicy::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag( getXMLName(), out, tabs, mName );

    // write out the market string.
    XMLWriteElement( mMarket, "market", out, tabs );
    
    // Write whether we are a fixed tax policy.
    XMLWriteElement( isFixedTax, "isFixedTax", out, tabs );
    
    // Write the constraint for the current year
    XMLWriteElement( constraint[ period ], "constraint", out, tabs );
    
    // Write out the fixed tax for the current year.
    XMLWriteElement( fixedTaxes[ period ], "fixedTax", out, tabs );
    
    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& GHGPolicy::getXMLName() const {
    return XML_NAME;
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
const string& GHGPolicy::getXMLNameStatic() {
    return XML_NAME;
}
