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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
* \file building_service_input.cpp
* \ingroup Objects
* \brief The BuildingServiceInput class source file.
* \author Pralit Patel
* \author Jiyong Eom
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <cmath>

#include "functions/include/building_service_input.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"
#include "functions/include/satiation_demand_function.h"
#include "containers/include/market_dependency_finder.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default Constructor
BuildingServiceInput::BuildingServiceInput()
{
}

//! Destructor
BuildingServiceInput::~BuildingServiceInput() {}

/*! \brief Get the XML name for reporting to XML file.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for reporting outputs and can be easily
* changed.
* \author Sonny Kim
* \return The constant XML_NAME.
*/
const string& BuildingServiceInput::getXMLNameStatic() {
    static const string XML_REPORTING_NAME = "building-service-input";
    return XML_REPORTING_NAME;
}

const string& BuildingServiceInput::getXMLReportingName() const {
    return getXMLNameStatic();
}

void BuildingServiceInput::XMLParse( const DOMNode* aNode ) {
    /*! \pre make sure we were passed a valid node. */
    assert( aNode );

    // get the name attribute.
    mName = XMLHelper<string>::getAttr( aNode, "name" );

    // get all child nodes.
    const DOMNodeList* nodeList = aNode->getChildNodes();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        const DOMNode* curr = nodeList->item( i );
        if( curr->getNodeType() == DOMNode::TEXT_NODE ){
            continue;
        }
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if ( nodeName == "base-service" ) {
            XMLHelper<Value>::insertValueIntoVector( curr, mServiceDemand, scenario->getModeltime() );
        }
        else if( nodeName == SatiationDemandFunction::getXMLNameStatic() ) {
            parseSingleNode( curr, mSatiationDemandFunction, new SatiationDemandFunction );
        }
        else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing " << getXMLNameStatic() << "." << endl;
        }
    }
}

void BuildingServiceInput::completeInit( const string& aRegionName,
                             const string& aSectorName,
                             const string& aSubsectorName,
                             const string& aTechName,
                             const IInfo* aTechInfo)
{
    // Indicate that this sector depends on the service this input represents.
    // Note tech name is the name of the consumer which in GCAM is called directly
    // and so should be the name used in dependency tracking.
    scenario->getMarketplace()->getDependencyFinder()->addDependency( aTechName,
                                                                      aRegionName,
                                                                      mName,
                                                                      aRegionName );
}

void BuildingServiceInput::initCalc( const string& aRegionName,
                         const string& aSectorName,
                         const bool aIsNewInvestmentPeriod,
                         const bool aIsTrade,
                         const IInfo* aTechInfo,
                         const int aPeriod )
{
    /*! \pre There must be a valid region name. */
    assert( !aRegionName.empty() );
}

void BuildingServiceInput::copyParam( const IInput* aInput,
                          const int aPeriod )
{
    /*!
     * \warning The ability to copyParams has been left unimplemented for GCAM consumers.
     */
    assert( false );
}

IInput* BuildingServiceInput::clone() const {
    BuildingServiceInput* retNodeInput = new BuildingServiceInput;
    retNodeInput->copy( *this );
    return retNodeInput;
}

void BuildingServiceInput::copy( const BuildingServiceInput& aInput ) {
    mName = aInput.mName;
    mServiceDemand = aInput.mServiceDemand;

    mSatiationDemandFunction.reset( new SatiationDemandFunction( *aInput.mSatiationDemandFunction.get() ) );
}

bool BuildingServiceInput::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

//! Output to XML data
void BuildingServiceInput::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    // write the beginning tag.
    XMLWriteOpeningTag ( getXMLNameStatic(), aOut, aTabs, mName );

    const Modeltime* modeltime = scenario->getModeltime();
    // only write base year values
    for( int period = 0; period <= modeltime->getFinalCalibrationPeriod(); period++ ) {
        const int year = modeltime->getper_to_yr( period );
        XMLWriteElement( mServiceDemand[ period ], "base-service", aOut, aTabs, year );
    }
    mSatiationDemandFunction->toInputXML( aOut, aTabs );

    // write the closing tag.
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

//! Output debug info to XML
void BuildingServiceInput::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    // write the beginning tag.
    XMLWriteOpeningTag ( getXMLNameStatic(), aOut, aTabs, mName );

    XMLWriteElement( mServiceDemand[ aPeriod ], "service", aOut, aTabs );
    XMLWriteElement( mServiceDensity[ aPeriod ], "service-density", aOut, aTabs );

    // write the closing tag.
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

double BuildingServiceInput::calcThermalLoad( const BuildingNodeInput* aBuildingInput,
                                              const double aInternalGainsPerSqMeter,
                                              const int aPeriod ) const
{
    // Generic building services do not adjust demands based on thermal load.
    return 1;
}

/*!
 * \brief Set the calculated service density for reporting.
 * \param aServiceDensity The calculated service density.
 * \param aPeriod The model period in which the serivce density was calculated.
 */
void BuildingServiceInput::setServiceDensity( const double aServiceDensity, const int aPeriod ) {
    mServiceDensity[ aPeriod ].set( aServiceDensity );
}

/*!
 * \brief Get the satiation demand function to be used in demand calculations.
 * \return The satiation demand function.
 */
SatiationDemandFunction* BuildingServiceInput::getSatiationDemandFunction() const {
    return mSatiationDemandFunction.get();
}

//! Get the name of the input
const string& BuildingServiceInput::getName() const {
    return mName;
}

/*! 
 * \brief Get the Physical Demand.
 * \param aPeriod Model period.
 * \return Physical demand.
 */
double BuildingServiceInput::getPhysicalDemand( const int aPeriod ) const {
    assert( mServiceDemand[ aPeriod ].isInited() );
    
    return mServiceDemand[ aPeriod ];
}

//! Set Physical Demand.
void BuildingServiceInput::setPhysicalDemand( double aPhysicalDemand, const string& aRegionName, const int aPeriod )
{
    mServiceDemand[ aPeriod ].set( aPhysicalDemand );
    
    mLastCalcValue = scenario->getMarketplace()->addToDemand( mName, aRegionName,
        aPhysicalDemand, mLastCalcValue, aPeriod );
}

/*!
 * \brief Get the building service coefficient.
 * \param aPeriod Model period.
 * \return The coefficient.
*/
double BuildingServiceInput::getCoefficient( const int aPeriod ) const {
    // Generic building services do not have coefficients.
    return 1;
}

/*! \brief Set the building service coefficient.
 * \param aCoefficient new coefficient value
 * \param aPeriod Model period.
 */
void BuildingServiceInput::setCoefficient( const double aCoefficient, const int aPeriod ) {
    // Generic building services do not have coefficients.
}

/*!
 * \brief Return the market price, or unadjusted price, for the building service.
 * \param aRegionName Region containing the input.
 * \param aPeriod Period to find the price in.
 * \return The market or unadjusted price.
 */
double BuildingServiceInput::getPrice( const string& aRegionName, const int aPeriod ) const {
    return scenario->getMarketplace()->getPrice( mName, aRegionName, aPeriod );
}

void BuildingServiceInput::setPrice( const string& aRegionName,
                         const double aPrice,
                         const int aPeriod )
{
    // The service price is set by the supply sector and so can not be set here.
}

/*! \brief Returns the price paid for each BuildingServiceInput.
* \param aRegionName Name of the containing region.
* \param aPeriod Model period.
*/
double BuildingServiceInput::getPricePaid( const string& aRegionName, const int aPeriod ) const{
    return getPrice( aRegionName, aPeriod );
}

/*! \brief Set the price paid for each BuildingServiceInput.
*
* \param aPricePaid new price paid value
* \param aPeriod Model period.
*/
void BuildingServiceInput::setPricePaid( double aPricePaid, const int aPeriod ) {
    // The service price is set by the supply sector and so can not be set here.
}

bool BuildingServiceInput::hasTypeFlag( const int aTypeFlag ) const {
    return false;
}

void BuildingServiceInput::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitBuildingServiceInput( this, aPeriod );
    aVisitor->endVisitBuildingServiceInput( this, aPeriod );
}
