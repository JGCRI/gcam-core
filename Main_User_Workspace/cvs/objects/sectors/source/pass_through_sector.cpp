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
 * \file pass_through_sector.cpp
 * \ingroup Objects
 * \brief PassThroughSector class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>

// xml headers
#include "sectors/include/pass_through_sector.h"
#include "sectors/include/subsector.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/market_dependency_finder.h"
#include "containers/include/iinfo.h"
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/* \brief Constructor
 * \param aRegionName The name of the region.
 */
PassThroughSector::PassThroughSector( const string& aRegionName ):
SupplySector( aRegionName )
{
}

const string& PassThroughSector::getXMLNameStatic() {
    const static string XML_NAME = "pass-through-sector";
    return XML_NAME;
}

bool PassThroughSector::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aNode ) {
    bool didParse = true;
    if( aNodeName == "marginal-revenue-sector" ) {
        mMarginalRevenueSector = XMLHelper<string>::getValue( aNode );
    }
    else if( aNodeName == "marginal-revenue-market" ) {
        mMarginalRevenueMarket = XMLHelper<string>::getValue( aNode );
    }
    else {
        didParse = false;
    }
    return didParse;
}

void PassThroughSector::toInputXMLDerived( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteElement( mMarginalRevenueSector, "marginal-revenue-sector", aOut, aTabs );
    XMLWriteElementCheckDefault( mMarginalRevenueMarket, "marginal-revenue-market", aOut, aTabs, regionName );
}

void PassThroughSector::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    toInputXMLDerived( aOut, aTabs );
}

void PassThroughSector::completeInit( const IInfo* aRegionInfo,
                                      ILandAllocator* aLandAllocator )
{
    SupplySector::completeInit( aRegionInfo, aLandAllocator );

    if( mMarginalRevenueSector.empty() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "No marginal revenue sector set for pass-through sector " << name << " in " << regionName << "." << endl;
        // TODO: allow this or just quit?
        mMarginalRevenueSector = name;
    }

    // The default market for the marginal revenue sector is just the same region
    // as this sector.
    if( mMarginalRevenueMarket.empty() ) {
        mMarginalRevenueMarket = regionName;
    }

    // Add dependencies for a calc item to gather up the fixed demands from this
    // pass through sector and make that available for the downstream sector
    const string fixedDemandActivityName = name + "-fixed-output";
    MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();

    // Ensure we gather the fixed demands after we calculate prices / before we
    // set supplies
    depFinder->addDependency( fixedDemandActivityName, regionName, name, regionName );
    depFinder->addDependency( fixedDemandActivityName, regionName, mMarginalRevenueSector, mMarginalRevenueMarket );

    // Set the activity to create the call back.
    // Note the market dependency finder will manage the newly allocated memory.
    depFinder->resolveActivityToDependency( regionName, fixedDemandActivityName,
                                            new CalcFixedOutputActivity( this ) );
}

void PassThroughSector::initCalc( NationalAccount* aNationalAccount,
                                  const Demographic* aDemographics,
                                  const int aPeriod )
{
    SupplySector::initCalc( aNationalAccount, aDemographics, aPeriod );
}

double PassThroughSector::getFixedOutput( const int aPeriod ) const {
    const double marginalRevenue = scenario->getMarketplace()->getPrice( mMarginalRevenueSector,
        mMarginalRevenueMarket, aPeriod );
    double totalfixedOutput = 0;
    for( CSubsectorIterator subSecIter = subsec.begin(); subSecIter != subsec.end(); subSecIter++ ) {
        totalfixedOutput += (*subSecIter)->getFixedOutput( aPeriod, marginalRevenue );
    }
    return totalfixedOutput;
}

/*!
 * \brief Calculate the total fixed output in this sector (including vintaged output)
 *        and add it to the market info where it can be retrieved downstream.
 * \param aPeriod The current model period.
 */
void PassThroughSector::setFixedDemandsToMarket( const int aPeriod ) const {
    double fixedOutput = getFixedOutput( aPeriod );

    Marketplace* marketplace = scenario->getMarketplace();
    IInfo* marketInfo = marketplace->getMarketInfo( name, regionName, aPeriod, true );
    marketInfo->setDouble( "fixed-output", fixedOutput );
}

CalcFixedOutputActivity::CalcFixedOutputActivity( const PassThroughSector* aSector ):
mSector( aSector )
{
}

CalcFixedOutputActivity::~CalcFixedOutputActivity() {
    // Note that this object does not own any memory
}

void CalcFixedOutputActivity::calc( const int aPeriod ) {
    mSector->setFixedDemandsToMarket( aPeriod );
}

void CalcFixedOutputActivity::setStale() {
    // nothing to do
}

string CalcFixedOutputActivity::getDescription() const {
    return mSector->regionName + " " + mSector->getName() + "-fixed-output";
}

