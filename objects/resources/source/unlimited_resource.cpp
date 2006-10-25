/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
 * \file unlimited_resource.cpp
 * \ingroup Objects
 * \brief UnlimitedResource class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"

#include <string>
#include <vector>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "resources/include/unlimited_resource.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "marketplace/include/imarket_type.h"
#include "containers/include/iinfo.h"
#include "util/base/include/ivisitor.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*!
 * \brief Get the XML name of the class.
 * \return The XML name of the class.
 */
const string& UnlimitedResource::getXMLNameStatic(){
    static const string XML_NAME = "unlimited-resource";
    return XML_NAME;
}

//! Constructor.
UnlimitedResource::UnlimitedResource()
: mFixedPrices( scenario->getModeltime()->getmaxper() ){
}

//! Destructor.
UnlimitedResource::~UnlimitedResource() {
}

void UnlimitedResource::XMLParse( const DOMNode* node ){

    // make sure we were passed a valid node.
    assert( node );

    // get the name attribute.
    mName = XMLHelper<string>::getAttr( node, "name" );

    // get all child nodes.
    const DOMNodeList* nodeList = node->getChildNodes();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        const DOMNode* curr = nodeList->item( i );
        const string nodeName =
            XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }

        else if( nodeName == "market" ){
            mMarket = XMLHelper<string>::getValue( curr );
        }
        else if( nodeName == "price" ){
            XMLHelper<double>::insertValueIntoVector( curr, mFixedPrices,
                                                      scenario->getModeltime() );
        }
        else if( nodeName == "capacity-factor" ){
            mCapacityFactor = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "variance" ){
            mVariance = XMLHelper<double>::getValue( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName
                    << " found while parsing " << getXMLNameStatic() << "."
                    << endl;
        }
    }
}

const string& UnlimitedResource::getXMLName() const {
    return getXMLNameStatic();
}

void UnlimitedResource::completeInit( const string& aRegionName,
                                      const IInfo* aRegionInfo )
{
    // Setup markets for this resource.
    setMarket( aRegionName );
}

void UnlimitedResource::initCalc( const string& aRegionName,
                                  const int aPeriod )
{
    // Set the capacity factor and variance.
    IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( mName,
                                                                   aRegionName,
                                                                   aPeriod,
                                                                   true );
    assert( marketInfo );

    if( mCapacityFactor.isInited() ){
        marketInfo->setDouble( "resourceCapacityFactor", mCapacityFactor );
    }
    if( mVariance.isInited() ){
        marketInfo->setDouble( "resourceVariance", mVariance );
    }
}

void UnlimitedResource::postCalc( const string& aRegionName,
                                  const int aPeriod )
{
}

void UnlimitedResource::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, mName );

    // write the xml for the class members.
    XMLWriteElement( mMarket, "market", aOut, aTabs );
    
    XMLWriteElementCheckDefault( mCapacityFactor, "capacity-factor", aOut,
                                 aTabs, Value( 0 ) );

    XMLWriteElementCheckDefault( mVariance, "variance", aOut,
                                 aTabs, Value( 0 ) );
    
    const Modeltime* modeltime = scenario->getModeltime();
    XMLWriteVector( mFixedPrices, "price", aOut, aTabs, modeltime, 0.0 );

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void UnlimitedResource::toDebugXML( const int aPeriod,
                                    ostream& aOut,
                                    Tabs* aTabs ) const
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, mName );

    // Write the xml for the class members.
    // Write out the market string.
    XMLWriteElement( mMarket, "market", aOut, aTabs );
    XMLWriteElement( mCapacityFactor, "capacity-factor", aOut, aTabs );
    XMLWriteElement( mVariance, "variance", aOut, aTabs );

    // Write out resource prices for debugging period.
    XMLWriteElement( mFixedPrices[ aPeriod ], "price", aOut, aTabs );

    // finished writing xml for the class members.

    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

const string& UnlimitedResource::getName() const {
    return mName;
}

void UnlimitedResource::calcSupply( const string& aRegionName,
                                    const GDP* aGDP,
                                    const int aPeriod )
{
    Marketplace* marketplace = scenario->getMarketplace();

    // Get the current demand and add the difference between current supply and
    // demand to the market.
    double currDemand = marketplace->getDemand( mName, aRegionName, aPeriod );
    double currSupply = marketplace->getSupply( mName, aRegionName, aPeriod );
    assert( currDemand >= currSupply );
    marketplace->addToSupply( mName, aRegionName, currDemand - currSupply,
                              aPeriod );
}

double UnlimitedResource::getAnnualProd( const string& aRegionName,
                                         const int aPeriod ) const
{
    // Return the market supply.
    return scenario->getMarketplace()->getSupply( mName, aRegionName, aPeriod );
}

void UnlimitedResource::csvOutputFile( const string& aRegionName )
{
    // function protocol
    void fileoutput3( string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    const int maxper = scenario->getModeltime()->getmaxper();
    vector<double> temp( maxper );
    for( int i = 0; i < maxper; ++i ){
        temp[ i ] = getAnnualProd( aRegionName, i );
    }
    fileoutput3( aRegionName , mName," "," ","production","EJ", temp );
}

void UnlimitedResource::dbOutput( const string& aRegionName ) {
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    for( int i = 0; i < maxper; ++i ){
        temp[ i ] = getAnnualProd( aRegionName, i );
    }
    dboutput4(aRegionName,"Pri Energy","Production by Sector", mName,"EJ", temp );
}

void UnlimitedResource::setCalibratedSupplyInfo( const int aPeriod,
                                                 const string& aRegionName )
{
    const double MKT_NOT_ALL_FIXED = -1;
    Marketplace* marketplace = scenario->getMarketplace();

    IInfo* resourceInfo = marketplace->getMarketInfo( mName, aRegionName,
                                                      aPeriod, true );
    assert( resourceInfo );

    resourceInfo->setDouble( "calSupply", MKT_NOT_ALL_FIXED );
}

/*
* \brief Create the resource market.
* \details The unlimited resource creates a single unsolved market for the
*          resource. The object will ensure that supply is always equal to
*          demand.
* \param aRegionName Region name.
*/
void UnlimitedResource::setMarket( const string& aRegionName ) {

    // Setup the market for the resource. This market will not be solved. Note
    // that in a standard Resource setMarketToSolve would be called here.
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->createMarket( aRegionName, mMarket, mName,
                               IMarketType::NORMAL );

    // Set the read-in fixed prices for each period.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        if( mFixedPrices[ i ] > util::getSmallNumber() ){
            marketplace->setPrice( mName, aRegionName, mFixedPrices[ i ], i,
                                   true );
        }
    }
}

void UnlimitedResource::accept( IVisitor* aVisitor,
                                const int aPeriod ) const
{
    aVisitor->startVisitResource( this, aPeriod );
    aVisitor->endVisitResource( this, aPeriod );
}
