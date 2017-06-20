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
 * \file consumer_final_demand.cpp
 * \ingroup Objects
 * \brief ConsumerFinalDemand source file.
 * \author Robert Link
 */

#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "sectors/include/consumer_final_demand.hpp"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "demographics/include/demographic.h"

// Scenario is supplied as a global
extern Scenario *scenario;
namespace {
    // Names of elements in the XML input
    const std::string supply_component_tag = "supply-component";
    const std::string demand_sys_tag = "demand-system";
    const std::string demand_out_tag = "demand";
    const std::string demand_component_attr = "component";
    const std::string base_service_tag = "base-service";
}


void ConsumerFinalDemand::setFinalDemand( const std::string &aRegionName,
                     const Demographic *aDemographics,
                     const GDP *aGDP,
                     const int aPeriod )
{
    Marketplace *mp = scenario->getMarketplace();
    if( aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod() ) {
        // These values are totals (even if the underlying demand model is per
        // capita) and are kept in the units of the upstream sector
        for(unsigned i=0; i<mSupplySectors.size(); ++i) {
            // add these values to the upstreadm sectors
            mLastDemand[i] = mp->addToDemand( mSupplySectors[i], aRegionName,
                                              mBaseServices[i][aPeriod],
                                              mLastDemand[i], aPeriod );
            // Store these for later reporting.  If the demand system is per
            // capita then we convert to per capita values and store those.
            mDemand[aPeriod][i] = mBaseServices[i][aPeriod];
            if( mDemandSys->isPerCapita() )
                mDemand[aPeriod][i] /= aDemographics->getTotal( aPeriod );
        }
    }
    else {
        std::vector<double> prices(mSupplySectors.size());
        for(unsigned i=0; i<mSupplySectors.size(); ++i)
            prices[i] =
                mp->getPrice( mSupplySectors[i], aRegionName, aPeriod, true );
        mDemandSys->calcDemand( aRegionName, *aDemographics, *aGDP,
                                prices, aPeriod, mDemand[aPeriod] );
        for(unsigned i=0; i<mSupplySectors.size(); ++i) {
            double value = mDemand[aPeriod][i];
            if( mDemandSys->isPerCapita() ) {
                // If the demand is calculated per capita, then add
                // the total demand to the market.  We still record
                // per capita demand.
                value *= aDemographics->getTotal( aPeriod );
            }
            mLastDemand[i] = mp->addToDemand( mSupplySectors[i], aRegionName,
                                              mDemand[aPeriod][i],
                                              mLastDemand[i], aPeriod
                                            );
        }

    }
}


void ConsumerFinalDemand::getDemand( std::vector<double> &aOutDemand, int aPeriod ) const
{
    int ncomp = mDemandSys->ngoods();
    if( aOutDemand.size() != ncomp ) {
        aOutDemand.resize( ncomp );
    }
    aOutDemand = mDemand[aPeriod]; // get stored demand
    mDemandSys->reportDemand( aOutDemand ); // convert to reporting units
}


void ConsumerFinalDemand::getReportingUnits( std::vector<std::string> &aOutUnits ) const
{
    int ncomp = mDemandSys->ngoods();
    if( aOutUnits.size() != ncomp ) {
        aOutUnits.resize( ncomp );
    }
    mDemandSys->reportUnits( aOutUnits );
}


void ConsumerFinalDemand::getComponentNames( std::vector<std::string> &aOutComponents ) const
{
    aOutComponents = mSupplySectors;
}


void ConsumerFinalDemand::completeInit( const std::string &aRegionName,
                                        const IInfo *aRegionInfo )
{
    // We hve two things to do here:  set up the storage for our demand results,
    // and call the complete init for the demand system
    int ncomp = mSupplySectors.size();
    const Modeltime *modeltime = scenario->getModeltime();
    int nper = modeltime->getmaxper();

    mLastDemand.resize(ncomp);
    mDemand.resize(nper);
    for(int i=0; i<nper; ++i)
        mDemand[i].resize(ncomp);

    mDemandSys->completeInit( aRegionName, getName() ); 
}

void ConsumerFinalDemand::initCalc( const std::string &aRegionName,
                                    const GDP *aGDP,
                                    const Demographic *aDemographics,
                                    const int aPeriod )
{
    // currently nothing to do at start of period
}


const std::string &ConsumerFinalDemand::getXMLNameStatic( void )
{
    static const std::string XML_NAME = "consumer-final-demand";
    return XML_NAME;
}

bool ConsumerFinalDemand::XMLParse( const xercesc::DOMNode *aNode )
{
    using namespace xercesc;
    const Modeltime* modeltime = scenario->getModeltime();
    
    ILogger &mainlog = ILogger::getLogger( "main_log" );

    // get all child nodes
    DOMNodeList *childnodes = aNode->getChildNodes();

    // loop over child nodes
    for(unsigned i=0; i < childnodes->getLength(); ++i) {
        DOMNode *child = childnodes->item(i);

        const std::string nodename =
            XMLHelper<std::string>::safeTranscode( child->getNodeName() );

        if( nodename == "#text" ) {
            continue;
        }
        else if( nodename == supply_component_tag ) {
            mSupplySectors.push_back(
                XMLHelper<std::string>::getValue( child ) );
        }
        else if( nodename == base_service_tag ) {
            // A base service child node should have "component" and "year" attributes.  We'll grab
            // the component here, and the year will be used in XMLHelper::insertValueIntoVector().
            std::string compname = XMLHelper<std::string>::getAttr( aNode,
                                                                    "component" );
            std::vector<std::string>::iterator citer = std::find(mSupplySectors.begin(), mSupplySectors.end(), compname );
            if( citer == mSupplySectors.end() ) {
                mainlog.setLevel( ILogger::SEVERE );
                mainlog << "Error parsing Consumer Final Demand Sector. Component " 
                        << compname << " has not been defined." << std::endl;
                abort();
            }
            int compindex = std::distance(mSupplySectors.begin(), citer );
            if( compindex >= mBaseServices.size() ) {
                mBaseServices.resize( compindex+1 );
            }
            XMLHelper<double>::insertValueIntoVector( child, mBaseServices[compindex],
                                                     modeltime );
        }
        else if( nodename == demand_sys_tag ) {
            std::string dstype = XMLHelper<std::string>::getAttr( child, "type" );
            if( IDemandSystem::isSubtype( dstype ) ) {
                std::auto_ptr<IDemandSystem> ptmp;
                // parseSingleNode expects an auto_ptr, but we're storing our stuff
                // in a unique_ptr, so we need to make a temporary.
                parseSingleNode( child, ptmp, IDemandSystem::create( dstype ),
                                 "type" );

                // take ownership of the pointer.
                mDemandSys.reset( ptmp.release() );
            }
            else {
                mainlog.setLevel( ILogger::SEVERE );
                mainlog << "ConsumerFinalDemand::XMLParse:  unknown demand system type: "
                        << dstype << std::endl;
                abort();
            }
        }
        else {
            mainlog.setLevel( ILogger::ERROR );
            mainlog << "ConsumerFinalDemand::XMLParse: unknown element tag: "
                    << nodename << std::endl;
        }
    }

    // check for consistent configuration
    // any further output from here will be the result of a severe error.
    mainlog.setLevel( ILogger::SEVERE ); 
    if( mDemandSys.get() == 0 ) {
        mainlog << "ConsumerFinalDemand::XMLParse: Demand system not supplied."
                << std::endl;
        abort();
    }

    if( mDemandSys->ngoods() != mSupplySectors.size() ) {
        mainlog << "ConsumerFinalDemand::XMLParse: number of supply sectors doesn't match demand system. "
                << "Found " << mSupplySectors.size()
                << " supply sectors. Expecting "
                << mDemandSys->ngoods() << "." << std::endl;
        abort();
    }
    return true; 
}

void ConsumerFinalDemand::toInputXML( std::ostream &aOut, Tabs *aTabs ) const
{
    XMLWriteOpeningTag( getXMLName(), aOut, aTabs, mName );
    aTabs->increaseIndent();

    // Write the component supply sectors
    for(unsigned i=0; i < mSupplySectors.size(); ++i) {
        XMLWriteElement(mSupplySectors[i], supply_component_tag, aOut, aTabs);
    }

    // Write the demand system parameters
    mDemandSys->toInputXML( aOut, aTabs );

    aTabs->decreaseIndent();
    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

void ConsumerFinalDemand::toDebugXML( const int aPeriod, std::ostream &aOut,
                                      Tabs *aTabs) const
{
    XMLWriteOpeningTag( getXMLName(), aOut, aTabs, mName );
    aTabs->increaseIndent();
    std::map<std::string, std::string> attr;
    for(unsigned comp=0; comp < mSupplySectors.size(); ++comp) {
        attr[demand_component_attr] = mSupplySectors[comp];
        XMLWriteElementWithAttributes( mDemand[aPeriod][comp], demand_out_tag,
                                       aOut, aTabs, attr );

    }

    aTabs->decreaseIndent();
    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

void ConsumerFinalDemand::csvOutputFile( const std::string
                                         &aRegionName ) const
{
    // do nothing.  Nobody uses this output anymore
}

void ConsumerFinalDemand::accept( IVisitor *aVisitor, int aPeriod )
    const
{
    aVisitor->startVisitFinalDemand( this, aPeriod );
    acceptDerived( aVisitor, aPeriod );
    aVisitor->endVisitFinalDemand( this, aPeriod );
}

void ConsumerFinalDemand::acceptDerived( IVisitor *aVisitor, int
                                         aPeriod ) const
{
    aVisitor->startVisitConsumerFinalDemand( this, aPeriod );
    aVisitor->endVisitConsumerFinalDemand( this, aPeriod );
}


void ConsumerFinalDemand::dbOutput( const std::string &aRegionName )
    const
{
// old-style database output.  We don't use this anymore. 
}
