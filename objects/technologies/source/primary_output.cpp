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
* \file primary_output.cpp
* \ingroup Objects
* \brief PrimaryOutput class source file.
* \author Josh Lurz
*/
#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
#include "technologies/include/primary_output.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "containers/include/iinfo.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/ivisitor.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

PrimaryOutput::PrimaryOutput( const string& aSectorName ):
mPhysicalOutputs( scenario->getModeltime()->getmaxper() ),
// Name of the primary output is the sector name.
mName( aSectorName ){
}

PrimaryOutput* PrimaryOutput::clone() const {
    return new PrimaryOutput( *this );
}

bool PrimaryOutput::isSameType( const string& aType ) const {
    return aType == "primary-output";
}

const string& PrimaryOutput::getName() const {
    // Make sure the name is initialized.
    assert( !mName.empty() );

    return mName;
}

bool PrimaryOutput::XMLParse( const DOMNode* aNode ){
    // Primary outputs are created and cannot be parsed.
    assert( false );
    return false;
}

void PrimaryOutput::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    // Primary outputs are not parsed and so do not write themselves to XML.
}

void PrimaryOutput::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( "primary-output", aOut, aTabs, mName );
    XMLWriteElement( mPhysicalOutputs[ aPeriod ], "output", aOut, aTabs );
    XMLWriteElement( mCachedCO2Coef, "cached-co2-coef", aOut, aTabs );
    XMLWriteClosingTag( "primary-output", aOut, aTabs );
}

void PrimaryOutput::completeInit( const string& aSectorName,
                                  DependencyFinder* aDependencyFinder,
                                  const bool aIsTechOperating )
{
    // Primary outputs do not have any additional dependencies.
}

void PrimaryOutput::initCalc( const string& aRegionName,
                              const int aPeriod )
{
    // Make sure the primary output has a name.
    assert( !mName.empty() );

    // Initialize the cached CO2 coefficient.
    // TODO: Once demand sectors are split there will always be a market.
    const Marketplace* marketplace = scenario->getMarketplace();
    const IInfo* productInfo = marketplace->getMarketInfo( mName, aRegionName, aPeriod, false );
    mCachedCO2Coef.set( productInfo ? productInfo->getDouble( "CO2Coef", false ) : 0 );
}

double PrimaryOutput::calcPhysicalOutput( const double aPrimaryOutput,
                                          const string& aRegionName,
                                          const int aPeriod ) const
{
    // Output of the primary output is given.
    return aPrimaryOutput;
}

void PrimaryOutput::setPhysicalOutput( const double aPrimaryOutput,
                                       const string& aRegionName,
                                       const int aPeriod )
{
    // Primary output is given by the technology.
    mPhysicalOutputs[ aPeriod ].set( aPrimaryOutput );

    // Add the primary output to the marketplace.
    // TODO: Once demand sectors are seperated this should be changed.
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->addToSupply( mName, aRegionName, aPrimaryOutput, aPeriod, false );
}

double PrimaryOutput::getPhysicalOutput( const int aPeriod ) const {
    assert( mPhysicalOutputs[ aPeriod ].isInited() );
    return mPhysicalOutputs[ aPeriod ];
}

double PrimaryOutput::getValue( const string& aRegionName,
                               const int aPeriod ) const
{
    // The value of the primary output is fully accounted by the technology.
    return 0;
}

double PrimaryOutput::getEmissionsPerOutput( const string& aGHGName,
                                           const int aPeriod ) const
{
    // Currently other GHGs do not use output emissions coefficients.
    assert( aGHGName == "CO2" );
    assert( mCachedCO2Coef.isInited() );
    return mCachedCO2Coef;
}

void PrimaryOutput::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitOutput( this, aPeriod );
    aVisitor->endVisitOutput( this, aPeriod );
}

