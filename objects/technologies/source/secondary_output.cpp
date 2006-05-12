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
* \file secondary_output.cpp
* \ingroup Objects
* \brief SecondaryOutput class source file.
* \author Josh Lurz
*/
#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
#include "technologies/include/secondary_output.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "containers/include/iinfo.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/dependency_finder.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

const string& SecondaryOutput::getXMLNameStatic(){
    const static string XML_NAME = "secondary-output";
    return XML_NAME;
}

SecondaryOutput::SecondaryOutput():
mPhysicalOutputs( scenario->getModeltime()->getmaxper() ){
}

SecondaryOutput* SecondaryOutput::clone() const {
    return new SecondaryOutput( *this );
}

bool SecondaryOutput::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

const string& SecondaryOutput::getName() const {
    // Make sure the name is initialized.
    assert( !mName.empty() );

    return mName;
}

bool SecondaryOutput::XMLParse( const DOMNode* aNode ){
	// assume we are passed a valid node.
	assert( aNode );

	// get all the children.
	DOMNodeList* nodeList = aNode->getChildNodes();
    
    // get the name attribute.
    mName = XMLHelper<string>::getAttr( aNode, "name" );

	for( unsigned int i = 0;  i <  nodeList->getLength(); ++i ){
		const DOMNode* curr = nodeList->item( i );
		const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

		if( nodeName == "#text" ) {
			continue;
		}
        else if( nodeName == "output-ratio" ) {
            mOutputRatio.set( XMLHelper<double>::getValue( curr ) );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
	        mainLog << "Unrecognized text string: " << nodeName << " found while parsing "
                    << getXMLNameStatic() << "." << endl;
		}
	}

    // TODO: Improve error handling.
    return true;
}

void SecondaryOutput::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, mName );
    XMLWriteElement( mOutputRatio, "output-ratio", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void SecondaryOutput::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, mName );
    XMLWriteElement( mOutputRatio, "output-ratio", aOut, aTabs );
    XMLWriteElement( mPhysicalOutputs[ aPeriod ], "output", aOut, aTabs );
    XMLWriteElement( mCachedCO2Coef, "cached-co2-coef", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void SecondaryOutput::completeInit( const string& aSectorName,
                                    DependencyFinder* aDependencyFinder,
                                    const bool aIsTechOperating )
{
    // Secondary output is removed from demand, so add a dependency.
    if( aIsTechOperating ){
        aDependencyFinder->addDependency( aSectorName, mName );
    }
}

void SecondaryOutput::initCalc( const string& aRegionName,
                                const int aPeriod )
{
    // Initialize the cached CO2 coefficient.
    // TODO: Once demand sectors are split there will always be a market.
    const Marketplace* marketplace = scenario->getMarketplace();
    const IInfo* productInfo = marketplace->getMarketInfo( mName, aRegionName, aPeriod, false );

    // Output ratio is determined by the CO2 coefficient and the ratio of output to the primary good.
    double CO2Coef = productInfo ? productInfo->getDouble( "CO2Coef", false ) : 0;
    mCachedCO2Coef.set( CO2Coef * mOutputRatio );
}

double SecondaryOutput::calcPhysicalOutput( const double aPrimaryOutput,
                                          const string& aRegionName,
                                          const int aPeriod ) const
{
    // Secondary output is the primary output multiplied by the output ratio.
    return aPrimaryOutput * mOutputRatio;
}

void SecondaryOutput::setPhysicalOutput( const double aPrimaryOutput,
                                          const string& aRegionName,
                                          const int aPeriod )
{
    mPhysicalOutputs[ aPeriod ].set( calcPhysicalOutput( aPrimaryOutput,
                                                         aRegionName,
                                                         aPeriod ) );

    // Remove the secondary output from demand instead of adding to supply
    // because the sector which has this output as a primary will attempt to
    // fill all of demand. If this technology also added to supply, supply would
    // not equal demand.
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->addToDemand( mName, aRegionName, -1 * mPhysicalOutputs[ aPeriod ], aPeriod, true );
}

double SecondaryOutput::getPhysicalOutput( const int aPeriod ) const {
    assert( mPhysicalOutputs[ aPeriod ].isInited() );
    return mPhysicalOutputs[ aPeriod ];
}

double SecondaryOutput::getValue( const string& aRegionName,
                                  const int aPeriod ) const
{

    double price = scenario->getMarketplace()->getPrice( mName, aRegionName, aPeriod, true );

    // Market price should exist or there is not a sector with this good as the
    // primary output. This can be caused by incorrect input files.
    assert( price != Marketplace::NO_MARKET_PRICE );
    if( price == Marketplace::NO_MARKET_PRICE ){
        return 0;
    }

    // The value of the secondary output is the market price multiplied by the
    // output ratio.
    return price * mOutputRatio;
}

double SecondaryOutput::getEmissionsPerOutput( const string& aGHGName,
                                               const int aPeriod ) const
{
    // Currently other GHGs do not use output emissions coefficients.
    assert( aGHGName == "CO2" );
    assert( mCachedCO2Coef.isInited() );
    return mCachedCO2Coef;
}

void SecondaryOutput::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitOutput( this, aPeriod );
    aVisitor->endVisitOutput( this, aPeriod );
}
