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

// Scenario is supplied as a global
extern Scenario *scenario;
namespace {
    // Names of elements in the XML input
    const std::string supply_component_tag = "supply-component";
    const std::string demand_sys_tag = "demand-system";
    const std::string demand_out_tag = "demand";
    const std::string demand_component_attr = "component";
}


void ConsumerFinalDemand::setFinalDemand( const std::string &aRegionName,
                     const Demographic *aDemographics,
                     const GDP *aGDP,
                     const int aPeriod )
{
    if( aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod() ) {
        // XXX In calibration periods, return the calibration data instead of
        // running the model.
    }
    else {
        Marketplace *mp = scenario->getMarketplace();
        // XXX maybe make this a member so it persists?
        std::vector<double> prices(mSupplySectors.size());
        for(unsigned i=0; i<mSupplySectors.size(); ++i)
            prices[i] =
                mp->getPrice( mSupplySectors[i], aRegionName, aPeriod, true );
        // XXX Not sure this will work, since mDemand holds "Values" instead of
        // doubles. 
        mDemand[aPeriod] = mDemandSys->calcDemand( aRegionName, *aDemographics, *aGDP,
                                                   prices, aPeriod );
        for(unsigned i=0; i<mSupplySectors.size(); ++i)
            mLastDemand[i] = mp->addToDemand( mSupplySectors[i], aRegionName,
                                              mDemand[aPeriod][i],
                                              mLastDemand[i], aPeriod );

    }
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
    mDemand.resize(ncomp);
    for(int i=0; i<ncomp; ++i)
        mDemand[i].resize(nper);

    mDemandSys->completeInit( aRegionName, getName() ); 
}

void ConsumerFinalDemand::initCalc( const std::string &aRegionName,
                                    const GDP *aGDP,
                                    const Demographic *aDemographics,
                                    const int aPeriod )
{
    // currently nothing to do at startup
}


const std::string &ConsumerFinalDemand::getXMLNameStatic( void )
{
    static const std::string XML_NAME = "consumer-final-demand";
    return XML_NAME;
}

bool ConsumerFinalDemand::XMLParse( const xercesc::DOMNode *aNode )
{
    using namespace xercesc;
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
        XMLWriteElementWithAttributes( mDemand[comp][aPeriod], demand_out_tag,
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
