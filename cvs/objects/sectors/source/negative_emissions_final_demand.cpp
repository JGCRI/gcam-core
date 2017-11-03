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
 * \file negative_emissions_final_demand.cpp
 * \ingroup Objects
 * \brief NegativeEmissionsFinalDemand class source file.
 * \author Pralit Patel
 */

#include <string>
#include <algorithm>

#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "util/base/include/definitions.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"
#include "util/base/include/model_time.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/market_dependency_finder.h"
#include "sectors/include/negative_emissions_final_demand.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Constructor.
*/
NegativeEmissionsFinalDemand::NegativeEmissionsFinalDemand()
{
}

/*! \brief Destructor.
*/
NegativeEmissionsFinalDemand::~NegativeEmissionsFinalDemand(){
}

const string& NegativeEmissionsFinalDemand::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& NegativeEmissionsFinalDemand::getXMLNameStatic() {
    const static string XML_NAME = "negative-emissions-final-demand";
    return XML_NAME;
}

const string& NegativeEmissionsFinalDemand::getName() const {
    return mName;
}

bool NegativeEmissionsFinalDemand::XMLParse( const DOMNode* aNode ) {

    assert( aNode );

    // get the name attribute.
    mName = XMLHelper<string>::getAttr( aNode, "name" );

    // get all child nodes.
    DOMNodeList* nodeList = aNode->getChildNodes();
    const Modeltime* modeltime = scenario->getModeltime();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); ++i ){
        DOMNode* curr = nodeList->item( i );
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        if( nodeName == "policy-name" ){
            mPolicyName = XMLHelper<string>::getValue( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unknown element " << nodeName
                    << " encountered while parsing " << getXMLName() << endl;
        }
    }
    return true;
}

void NegativeEmissionsFinalDemand::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLName(), aOut, aTabs, mName );
    const Modeltime* modeltime = scenario->getModeltime();
   
    // write the xml for the class members.
    XMLWriteElement( mPolicyName, "policy-name", aOut, aTabs );

    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}   

void NegativeEmissionsFinalDemand::toDebugXML( const int aPeriod,
                                    ostream& aOut,
                                    Tabs* aTabs ) const
{
    toInputXML( aOut, aTabs );
}

void NegativeEmissionsFinalDemand::completeInit( const string& aRegionName,
                                      const IInfo* aRegionInfo )
{
    if( mPolicyName.empty() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "No policy-name set for " << getXMLName() << ": " << mName
                << " in region " << aRegionName << endl;
        abort();
    }
    const static bool isTargetFinding = Configuration::getInstance()->getBool(
        "find-path", false, false );
    const bool hasTaxMarket = scenario->getMarketplace()->getPrice( mName, aRegionName, 0, false ) !=
        Marketplace::NO_MARKET_PRICE;
    if( isTargetFinding && !hasTaxMarket ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Using " << getXMLNameStatic() << " wth target-finder without explicitly creating a market for "
                << mName << " may hinder solution performance." << endl;
        mainLog << "Please read in a policy file with a zero tax." << endl;
    }

    MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();
    depFinder->addDependency( mPolicyName, aRegionName, mName, aRegionName );
}

void NegativeEmissionsFinalDemand::initCalc( const string& aRegionName,
                                  const GDP* aGDP,
                                  const Demographic* aDemographics,
                                  const int aPeriod )
{
}

/*! \brief Set the final demand for service into the marketplace after 
* calling the aggregate demand function.
*
* \detail Adding the demand for final services into the marketplace
*  is separted from the actual calculation of final service so that services can
*  be calculated and used without being adding to marketplace.
* \author Sonny Kim, Josh Lurz
* \param string& aRegionName region name.
* \param GDP* aGDP object.
* \param Demographic* aDemographicss.
* \param aPeriod Model aPeriod
*/
void NegativeEmissionsFinalDemand::setFinalDemand( const string& aRegionName,
                                        const Demographic* aDemographics,
                                        const GDP* aGDP,
                                        const int aPeriod )
{
    Marketplace* marketplace = scenario->getMarketplace();
    double co2Price = marketplace->getPrice( mName, aRegionName, aPeriod, false );
    if( co2Price == Marketplace::NO_MARKET_PRICE ) {
        // no CO2 policy so the negative emissions policy is inactive too
        // TODO: warn?
        return;
    }
    double regionalCO2Emiss = marketplace->getDemand( mName, aRegionName, aPeriod );
    double regionalCO2EmissValue = -1.0 * regionalCO2Emiss * co2Price;
    if(regionalCO2EmissValue > 0.0) {
        double policyPrice = std::min( marketplace->getPrice( mPolicyName, aRegionName, aPeriod ), 1.0 );
        double policyAdj = ( 1.0 - policyPrice );
        regionalCO2EmissValue *= policyAdj;
    }
    mCurrNegEmissValue = regionalCO2EmissValue;
    marketplace->addToDemand( mPolicyName, aRegionName, mCurrNegEmissValue, aPeriod );
}

double NegativeEmissionsFinalDemand::getWeightedEnergyPrice( const string& aRegionName,
                                                  const int aPeriod ) const
{
    return 0;
}

// Documentation is inherited.
void NegativeEmissionsFinalDemand::accept( IVisitor* aVisitor,
                                const int aPeriod ) const
{
    aVisitor->startVisitFinalDemand( this, aPeriod );
    aVisitor->endVisitFinalDemand( this, aPeriod );
}

//! Write sector output to database.
void NegativeEmissionsFinalDemand::csvOutputFile( const string& aRegionName ) const {
}

//! Write MiniCAM style demand sector output to database.
void NegativeEmissionsFinalDemand::dbOutput( const string& aRegionName ) const  {
}

