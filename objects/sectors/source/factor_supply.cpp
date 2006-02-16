/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Laboratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file factor_supply.cpp
* \ingroup Objects
* \brief The FactorSupply class source file.
* \author Pralit Patel
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <fstream>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "sectors/include/factor_supply.h"
#include "sectors/include/more_sector_info.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "marketplace/include/imarket_type.h"
#include "util/base/include/ivisitor.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/iinfo.h"
#include "functions/include/function_utils.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default Constructor
FactorSupply::FactorSupply() {
    // resize vectors
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    mBasePrice = 0;
    mBaseSupply = 0;
    // instantiate moreSectorInfo object here,
    // override with data read in
    moreSectorInfo.reset( new MoreSectorInfo() );
}

//! Get the name of the factor supply
const string& FactorSupply::getName() const{
    return name;
}
    
//! Parses SOME xml object
bool FactorSupply::XMLParse( const xercesc::DOMNode* node) {
    /*! \pre make sure we were passed a valid node. */
    assert( node );

    // get the name attribute.
    name = XMLHelper<string>::getAttr( node, "name" );

    // get all child nodes.
    DOMNodeList* nodeList = node->getChildNodes();

    const Modeltime* modeltime = scenario->getModeltime();
    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == "supply" ) {
            mBaseSupply = XMLHelper<double>::getValue( curr );
        }
        else if (nodeName == "price" ) {
            mBasePrice = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == MoreSectorInfo::getXMLNameStatic() ) {
            moreSectorInfo->XMLParse( curr );
        }
        else {
            return false;
            // Should print an error here.
        }
    }
    return true;
}

//! Write to XML
void FactorSupply::toInputXML( ostream &out, Tabs* tabs ) const {
    const Modeltime* modeltime = scenario->getModeltime();

    // write the beginning tag.
    XMLWriteOpeningTag ( getXMLName(), out, tabs, name );
    
    // Write the data.
    XMLWriteElement( mBaseSupply, "supply", out, tabs );
    XMLWriteElement( mBasePrice, "price", out, tabs );

    // write the closing tag.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Debug info written to XML
void FactorSupply::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    // write the beginning tag.
    XMLWriteOpeningTag ( getXMLName(), out, tabs, name );

    XMLWriteElement( mBaseSupply, "supply", out, tabs );
    XMLWriteElement( mBasePrice, "price", out, tabs );

    // write the closing tag.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

double FactorSupply::getSupply( const string& aRegionName, const int period ) const{
    Marketplace* marketplace = scenario->getMarketplace();
    return marketplace->getSupply( name, aRegionName, period );
}

const string& FactorSupply::getXMLName() const {
    return getXMLNameStatic();
}

const string& FactorSupply::getXMLNameStatic() {
    const static string XML_NAME = "factorSupply";
    return XML_NAME;
}

void FactorSupply::completeInit( const string& aRegionName ) {
    setMarket( aRegionName );
}
/*! \brief Perform any initializations needed for each period.
*
* Any initializations or calcuations that only need to be done once per period (instead of every iteration) should be placed in this function.
*
* \author Sonny Kim
* \param aRegionName region name
* \param aPeriod Model period
*/
void FactorSupply::initCalc( const string& aRegionName, const int aPeriod ) {
    Marketplace* marketplace = scenario->getMarketplace();
    if( aPeriod == 0 ) {
        if( name != "Capital" ) {
            assert( mBaseSupply != 0 );
            mBasePrice = marketplace->getDemand( name, aRegionName, aPeriod ) / mBaseSupply;
        }
        if( mBasePrice == 0 ){
            mBasePrice = 1;
        }
        marketplace->setPrice( name, aRegionName, mBasePrice, aPeriod );
    }
    // set market prices before calling calcPricePaid
    calcPricePaid( aRegionName, aPeriod );
}

void FactorSupply::setMarket( const string& aRegionName ) {
    Marketplace* marketplace = scenario->getMarketplace();

    // Set the market name to the region name if it is not read in.
    // Currently this is never read in, not sure if this should be fixed.
    if( marketName.empty() ){
        marketName = aRegionName;
    }

    // name is factor supply name (name of primary factors)
    // market is the name of the regional market from the input file (i.e., global, region, regional group, etc.)
    if( marketplace->createMarket( aRegionName, marketName, name, IMarketType::NORMAL ) ) {
        for( int per = 1; per < scenario->getModeltime()->getmaxper(); ++per ){
            marketplace->setMarketToSolve( name, aRegionName, per );
        }
    }
}

/*! \brief For calculating the price paid for primary factors of input
 * \author Sonny Kim
 * Price paid for factor inputs should be calculated in production and demand
 * sectors.  These price paid values are place holders for the final demand
 * sectors as they do not have demands for factor inputs.
 * \param period 
*/
void FactorSupply::calcPricePaid( const string& aRegionName, const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();
    // set price received in market info
    double pricePaid = ( marketplace->getPrice(name, aRegionName, period) + 
        ( moreSectorInfo->getValue(MoreSectorInfo::TRANSPORTATION_COST)
        * moreSectorInfo->getValue(MoreSectorInfo::TRAN_COST_MULT) )
        * moreSectorInfo->getValue(MoreSectorInfo::PROPORTIONAL_TAX_RATE)
        + moreSectorInfo->getValue(MoreSectorInfo::ADDITIVE_TAX) ) // add carbon taxes
        * 1 ;
        //* moreSectorInfo->getValue(MoreSectorInfo::PRICE_ADJUST_MULT);
        
    // set price paid in market info.
    FunctionUtils::setPricePaid( aRegionName, name, period, pricePaid );
}

/*! \brief For outputing SGM data to a flat csv File
 * 
 * \author Pralit Patel, Sonny Kim
 * \param period The period which we are outputing for
 */
void FactorSupply::csvSGMOutputFile( ostream& aFile, const int period ) const {
    // Write factor supply output
    // aFile << "Factor Supply Results" << endl << endl;
}

void FactorSupply::accept( IVisitor* aVisitor, const int period ) const{
	aVisitor->updateFactorSupply( this, period );
}
