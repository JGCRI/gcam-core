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
#include "containers/include/market_dependency_finder.h"

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


// ConsumerFinalDemand constructor
ConsumerFinalDemand::ConsumerFinalDemand(void):
mDemandSys(0)
{
  // nothing to do.
}

ConsumerFinalDemand::~ConsumerFinalDemand() {
    delete mDemandSys;
    for(auto demandComp : mDemandComponents) {
        delete demandComp;
    }
}

const std::string& ConsumerFinalDemand::DemandComponentHelper::getName() const {
    return mSupplySectors;
}

void ConsumerFinalDemand::setFinalDemand( const std::string &aRegionName,
                     const Demographic *aDemographics,
                     const GDP *aGDP,
                     const int aPeriod )
{
    Marketplace *marketplace = scenario->getMarketplace();
    if( aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod() ) {
        // These values are totals (even if the underlying demand model is per
        // capita) and are kept in the units of the upstream sector
        for(unsigned i=0; i<mDemandComponents.size(); ++i) {
            // Store these for later reporting.  If the demand system is per
            // capita then we convert to per capita values and store those.
            mDemandComponents[i]->mDemand[aPeriod] = mDemandComponents[i]->mBaseServices[aPeriod];
            // add these values to the upstreadm sectors
            marketplace->addToDemand( mDemandComponents[i]->mSupplySectors, aRegionName,
                             mDemandComponents[i]->mDemand[aPeriod], aPeriod );
        }
    }
    else {
        std::vector<double> prices(mDemandComponents.size());
        std::vector<double> demands(mDemandComponents.size());
        for(unsigned i=0; i<mDemandComponents.size(); ++i) {
            prices[i] = marketplace->getPrice( mDemandComponents[i]->mSupplySectors, aRegionName, aPeriod, true );
        }
        mDemandSys->calcDemand( aRegionName, *aDemographics, *aGDP,
                                prices, aPeriod, demands );
        for(unsigned i=0; i<mDemandComponents.size(); ++i) {
            double value = demands[i];
            if( mDemandSys->isPerCapita() ) {
                // If the demand is calculated per capita, then add
                // the total demand to the market.  We still record
                // per capita demand.
                value *= 1000.0 * aDemographics->getTotal( aPeriod );
            }
            mDemandComponents[i]->mDemand[aPeriod] = value;
            marketplace->addToDemand( mDemandComponents[i]->mSupplySectors, aRegionName,
                                              mDemandComponents[i]->mDemand[aPeriod], aPeriod);
        }

    }
}


void ConsumerFinalDemand::getDemand( std::vector<double> &aOutDemand, int aPeriod ) const
{
    int numComponents = mDemandSys->getNumGoods();
    if( aOutDemand.size() != numComponents ) {
        aOutDemand.resize( numComponents );
    }
    for(unsigned i = 0; i < mDemandComponents.size(); ++i) {
        aOutDemand[i] = mDemandComponents[i]->mDemand[ aPeriod ];
    }
    mDemandSys->reportDemand( aOutDemand ); // convert to reporting units
}


void ConsumerFinalDemand::getReportingUnits( std::vector<std::string> &aOutUnits ) const
{
    int numComponents = mDemandSys->getNumGoods();
    if( aOutUnits.size() != numComponents ) {
        aOutUnits.resize( numComponents );
    }
    mDemandSys->reportUnits( aOutUnits );
}


void ConsumerFinalDemand::getComponentNames( std::vector<std::string> &aOutComponents ) const
{
    size_t numComponents = mDemandComponents.size();
    if( aOutComponents.size() != numComponents ) {
        aOutComponents.resize( numComponents );
    }
    for(unsigned i=0; i<mDemandComponents.size(); ++i) {
        aOutComponents[i] = mDemandComponents[i]->mSupplySectors;
    }
}


void ConsumerFinalDemand::completeInit( const std::string &aRegionName,
                                        const IInfo *aRegionInfo )
{
    MarketDependencyFinder *df = scenario->getMarketplace()->getDependencyFinder();
    for(unsigned i=0; i<mDemandComponents.size(); ++i) {
            df->addDependency( mName, aRegionName, mDemandComponents[i]->mSupplySectors,
                           aRegionName );
    }
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

        const std::string nodename = XMLHelper<std::string>::safeTranscode( child->getNodeName() );

        if( nodename == "#text" ) {
            continue;
        }
        else if( nodename == supply_component_tag ) {
            std::string componentName = XMLHelper<std::string>::getValue( child );
            DemandComponentHelper* newComponent = new DemandComponentHelper;
            newComponent->mSupplySectors = componentName;
            mDemandComponents.push_back( newComponent );
            mainlog.setLevel( ILogger::DEBUG );
            mainlog << "Found supply component: >" << componentName << "< ." << std::endl;
        }
        else if( nodename == base_service_tag ) {
            // A base service child node should have "component" and "year" attributes.  We'll grab
            // the component here, and the year will be used in XMLHelper::insertValueIntoVector().
            std::string componentName = XMLHelper<std::string>::getAttr( child, "component" );
            auto citer = objects::searchForValue(mDemandComponents, componentName );
            if( citer == mDemandComponents.end() ) {
                mainlog.setLevel( ILogger::SEVERE );
                mainlog << "Error parsing Consumer Final Demand Sector. Component " 
                        << componentName << " has not been defined." << std::endl;
                abort();
            }
            int componentIndex = std::distance(mDemandComponents.begin(), citer );
            if( componentIndex >= mDemandComponents.size() ) {
                // TODO: wouldn't this have been caught by the above abort check already?
                mDemandComponents.push_back(new DemandComponentHelper);
            }
            XMLHelper<double>::insertValueIntoVector( child, mDemandComponents[componentIndex]->mBaseServices, modeltime );
        }
        else if( nodename == demand_sys_tag ) {
            std::string demandSystemType = XMLHelper<std::string>::getAttr( child, "type" );
            if( IDemandSystem::isSubtype( demandSystemType ) ) {
                std::auto_ptr<IDemandSystem> tempPointer;
                // parseSingleNode expects an auto_ptr, but we're storing our stuff
                // in a unique_ptr, so we need to make a temporary.
                parseSingleNode( child, tempPointer, IDemandSystem::create( demandSystemType ), "type" );

                // take ownership of the pointer.
                delete mDemandSys;
                mDemandSys = tempPointer.release();
            }
            else {
                mainlog.setLevel( ILogger::SEVERE );
                mainlog << "ConsumerFinalDemand::XMLParse:  unknown demand system type: "
                        << demandSystemType << std::endl;
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
    if( !mDemandSys ) {
        mainlog << "ConsumerFinalDemand::XMLParse: Demand system not supplied."
                << std::endl;
        abort();
    }

    if( mDemandSys->getNumGoods() != mDemandComponents.size() ) {
        mainlog << "ConsumerFinalDemand::XMLParse: number of supply sectors doesn't match demand system. "
                << "Found " << mDemandComponents.size()
                << " supply sectors. Expecting "
                << mDemandSys->getNumGoods() << "." << std::endl;
        abort();
    }
    return true; 
}

void ConsumerFinalDemand::toDebugXML( const int aPeriod, std::ostream &aOut,
                                      Tabs *aTabs) const
{
    XMLWriteOpeningTag( getXMLName(), aOut, aTabs, mName );
    aTabs->increaseIndent();
    std::map<std::string, std::string> attr;
    for(unsigned comp=0; comp < mDemandComponents.size(); ++comp) {
        attr[demand_component_attr] = mDemandComponents[comp]->getName();
        XMLWriteElementWithAttributes( mDemandComponents[comp]->mDemand[aPeriod], demand_out_tag,
                                       aOut, aTabs, attr );

    }

    aTabs->decreaseIndent();
    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
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
