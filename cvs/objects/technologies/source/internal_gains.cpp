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
* \file internal_gains.cpp
* \ingroup Objects
* \brief InternalGains class source file.
* \author Josh Lurz
*/
#include "util/base/include/definitions.h"
#include <string>
#include "util/base/include/xml_helper.h"
#include "technologies/include/internal_gains.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/ivisitor.h"
#include "sectors/include/sector_utils.h"
#include "containers/include/market_dependency_finder.h"
#include "containers/include/iinfo.h"

using namespace std;

extern Scenario* scenario;

const string& InternalGains::getXMLNameStatic()
{
    const static string XML_NAME = "internal-gains";
    return XML_NAME;
}

InternalGains::InternalGains()
{
}

InternalGains::~InternalGains() {
}

InternalGains* InternalGains::clone() const
{
    InternalGains* clone = new InternalGains();
    clone->copy( *this );
    return clone;
}

void InternalGains::copy( const InternalGains& aOther ) {
    mName = aOther.mName;
    mOutputRatio = aOther.mOutputRatio;
    mTrialMarketName = aOther.mTrialMarketName;
    
    // note results are not copied.
}

bool InternalGains::isSameType( const string& aType ) const
{
    return aType == getXMLNameStatic();
}

const string& InternalGains::getName() const
{
    return mName;
}

/*! \brief Get the XML name for reporting to XML file.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for reporting outputs and can be easily
* changed.
* \author Sonny Kim
* \return The constant XML_NAME.
*/
const string& InternalGains::getXMLReportingName() const{
    static const string XML_REPORTING_NAME = "output-internal-gains";
    return XML_REPORTING_NAME;
}

const string& InternalGains::getXMLName() const{
    return getXMLNameStatic();
}

void InternalGains::toDebugXML( const int aPeriod,
                                ostream& aOut,
                                Tabs* aTabs ) const
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mOutputRatio, "output-ratio", aOut, aTabs );
    XMLWriteElement( mTrialMarketName, "internal-gains-market-name", aOut, aTabs );
    XMLWriteElement( mPhysicalOutputs[ aPeriod ], "output", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void InternalGains::completeInit( const string& aSectorName,
                                  const string& aRegionName,
                                  const IInfo* aTechInfo,
                                  const bool aIsTechOperating )
{
    // Internal gains create a simultaneity so we use a trial value for it.  Create
    // a link to that market.
    if( aIsTechOperating ) {
        MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();
        depFinder->addDependency( aSectorName, aRegionName,
                                  SectorUtils::getTrialMarketName( mTrialMarketName ),
                                  aRegionName );
    }
}

void InternalGains::initCalc( const string& aRegionName,
                              const string& aSectorName,
                              const int aPeriod )
{
    SectorUtils::setSupplyBehaviorBounds(SectorUtils::getTrialMarketName( mTrialMarketName ), aRegionName, 0.0, util::getLargeNumber(), aPeriod);
}

void InternalGains::postCalc( const string& aRegionName,
                              const int aPeriod )
{
}

void InternalGains::scaleCoefficient( const double aScaler ){
    // InternalGains gains do not support scaling.
}

IOutput::OutputList InternalGains::calcPhysicalOutput( const double aPrimaryOutput,
                                                       const string& aRegionName,
                                                       const ICaptureComponent* aCaptureComponent,
                                                       const int aPeriod ) const
{
    // Calculate total internal gains.
    double internalGains = mOutputRatio * aPrimaryOutput;

    OutputList outputList;
    
    // Add the heating internal gain.
    outputList.push_back( make_pair( mTrialMarketName, internalGains ) );

    return outputList;
}

void InternalGains::setPhysicalOutput( const double aPrimaryOutput,
                                       const string& aRegionName,
                                       ICaptureComponent* aCaptureComponent,
                                       const int aPeriod )
{
    // Calculate total internal gains.
    double internalGains = mOutputRatio * aPrimaryOutput;

    mPhysicalOutputs[ aPeriod ] = internalGains;

    // Add to the actual internal gains in the trials market
    SectorUtils::addToTrialDemand( aRegionName, mTrialMarketName, mPhysicalOutputs[ aPeriod ], aPeriod );
}

double InternalGains::getPhysicalOutput( const int aPeriod ) const
{
    assert( mPhysicalOutputs[ aPeriod ].isInited() );
    return mPhysicalOutputs[ aPeriod ];
}

double InternalGains::getValue( const string& aRegionName,
                                const ICaptureComponent* aCaptureComponent,
                                const int aPeriod ) const
{
    // There is no direct value for internal gains, only indirectly by changing
    // thermal load requirements.
    return 0;
}

string InternalGains::getOutputUnits( const string& aRegionName ) const {
    return scenario->getMarketplace()->getMarketInfo( SectorUtils::getTrialMarketName( mTrialMarketName ),
        aRegionName, 0, true )->getString( "output-unit", false );
}

double InternalGains::getEmissionsPerOutput( const string& aGHGName,
                                             const int aPeriod ) const
{
    // Internal gains do not contain any emissions
    return 0;
}

void InternalGains::accept( IVisitor* aVisitor,
                            const int aPeriod ) const
{
    aVisitor->startVisitOutput( this, aPeriod );
    aVisitor->endVisitOutput( this, aPeriod );
}

void InternalGains::doInterpolations( const int aYear, const int aPreviousYear,
                                      const int aNextYear, const IOutput* aPreviousOutput,
                                      const IOutput* aNextOutput )
{
    const InternalGains* prevInternalGains = static_cast<const InternalGains*>( aPreviousOutput );
    const InternalGains* nextInternalGains = static_cast<const InternalGains*>( aNextOutput );
    
    /*!
     * \pre We are given a valid InternalGains for the previous output.
     */
    assert( prevInternalGains );
    
    /*!
     * \pre We are given a valid InternalGains for the next output.
     */
    assert( nextInternalGains );
    
    // interpolate the output ratio
    mOutputRatio.set( util::linearInterpolateY( aYear, aPreviousYear, aNextYear,
                                                prevInternalGains->mOutputRatio,
                                                nextInternalGains->mOutputRatio ) );
}
