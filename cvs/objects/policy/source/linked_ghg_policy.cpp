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
 * \file linked_ghg_policy.cpp
 * \ingroup Objects
 * \brief LinkedGHGPolicy class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include <cassert>

#include "policy/include/linked_ghg_policy.h"
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "containers/include/iinfo.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/market_dependency_finder.h"
#include "containers/include/iactivity.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Default constructor. */
LinkedGHGPolicy::LinkedGHGPolicy():
mStartYear( -1 )
{
}

/*! \brief Create a copy of the linked GHG policy.
 * \return An exact copy of the policy.
 */
GHGPolicy* LinkedGHGPolicy::clone() const {
    LinkedGHGPolicy* clone = new LinkedGHGPolicy();
    clone->copy( *this );
    return clone;
}

void LinkedGHGPolicy::copy( const LinkedGHGPolicy& aOther ) {
    GHGPolicy::copy( aOther );
    mLinkedPolicyName = aOther.mLinkedPolicyName;
    mPriceUnits = aOther.mPriceUnits;
    mOutputUnits = aOther.mOutputUnits;
    std::copy( aOther.mPriceAdjust.begin(), aOther.mPriceAdjust.end(), mPriceAdjust.begin() );
    std::copy( aOther.mDemandAdjust.begin(), aOther.mDemandAdjust.end(), mDemandAdjust.begin() );
}

/*! \brief Get the XML node name for output to XML.
 *
 * This public function accesses the private constant string, XML_NAME.
 * This way the tag is always consistent for both read-in and output and can be easily changed.
 * This function may be virtual to be overridden by derived class pointers.
 * \return The constant XML_NAME.
 */
const string& LinkedGHGPolicy::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
 *
 * This public function accesses the private constant string, XML_NAME.
 * This way the tag is always consistent for both read-in and output and can be easily changed.
 * The "==" operator that is used when parsing, required this second function to return static.
 * \note A function cannot be static and virtual.
 * \return The constant XML_NAME as a static.
 */
const string& LinkedGHGPolicy::getXMLNameStatic() {
    const static string XML_NAME = "linked-ghg-policy";
    return XML_NAME;
}

//! Get the linked ghg policy name. 
const string& LinkedGHGPolicy::getName() const {
    return mName;
}

//! Initializes data members from XML.
void LinkedGHGPolicy::XMLParse( const DOMNode* node ){
    
    /*! \pre assume we are passed a valid node.*/
    assert( node );
    
    // get the name attribute.
    mName = XMLHelper<string>::getAttr( node, "name" );
    
    // get all child nodes.
    DOMNodeList* nodeList = node->getChildNodes();
    const Modeltime* modeltime = scenario->getModeltime();
    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        
        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == "policy-name" ) {
            mPolicyName = XMLHelper<string>::getValue( curr );
        }
        else if( nodeName == "market" ){
            mMarket = XMLHelper<string>::getValue( curr );
        }
        else if( nodeName == "linked-policy" ){
            mLinkedPolicyName = XMLHelper<string>::getValue( curr );
        }
        else if( nodeName == "price-unit" ){
            mPriceUnits = XMLHelper<string>::getValue( curr );
        }
        else if( nodeName == "output-unit" ){
            mOutputUnits = XMLHelper<string>::getValue( curr );
        }
        else if( nodeName == "price-adjust" ){
            XMLHelper<Value>::insertValueIntoVector( curr, mPriceAdjust, modeltime );
        }
        else if( nodeName == "demand-adjust" ){
            XMLHelper<Value>::insertValueIntoVector( curr, mDemandAdjust, modeltime );
        }
        else if( nodeName == "start-year" ){
            mStartYear = XMLHelper<int>::getValue( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing " << getXMLName() << "." << endl;
        }
    }
}

//! Writes data members to data stream in XML format.
void LinkedGHGPolicy::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag( getXMLName(), out, tabs, mName );
    XMLWriteElementCheckDefault( mPolicyName, "policy-name", out, tabs, mName );
    XMLWriteElement( mMarket, "market", out, tabs );
    XMLWriteElement( mLinkedPolicyName, "linked-policy", out, tabs );
    XMLWriteElementCheckDefault( mStartYear, "start-year", out, tabs, -1 );
    XMLWriteElementCheckDefault( mPriceUnits, "price-unit", out, tabs );
    XMLWriteElementCheckDefault( mOutputUnits, "output-unit", out, tabs );
    
    const Modeltime* modeltime = scenario->getModeltime();
    for( int period = 0; period < modeltime->getmaxper(); ++period ) {
        const int year = modeltime->getper_to_yr( period );
        XMLWriteElementCheckDefault( mPriceAdjust[ period ], "price-adjust", out, tabs, Value(), year );
        XMLWriteElementCheckDefault( mDemandAdjust[ period ], "demand-adjust", out, tabs, Value(), year );
    }
    
    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Complete the initialization of the GHG policy.
 * \details This function initializes a ghg market for the policy.
 * GHG markets are created for both mConstraint and fixed tax policies.
 * In the fixed tax policy, market prices are set to the fixed taxes, but
 * the markets are not solved.  Also for the fixed tax policy, if the market name
 * is the same for all regions, the fixed tax vector of the last region overrides
 * the market prices.
 * \param aRegionName The name of the region the policy controls. 
 */
void LinkedGHGPolicy::completeInit( const string& aRegionName ) {
    if( mPolicyName.empty() ) {
        mPolicyName = mName;
    }
    const Modeltime* modeltime = scenario->getModeltime();
    Marketplace* marketplace = scenario->getMarketplace();
    // Forward the -1 flag to indicate we do not intend to change market links over time.
    int startPeriodForCreate = mStartYear == -1 ? -1 : modeltime->getyr_to_per( mStartYear );
    int startPeriod = max( startPeriodForCreate, 0 );
    marketplace->createLinkedMarket( aRegionName, mMarket, mPolicyName, mLinkedPolicyName, startPeriodForCreate );
    
    // Set price and output units for period 0 market info
    IInfo* marketInfo = marketplace->getMarketInfo( mPolicyName, aRegionName, startPeriod, true );

    // Set the units of tax and emissions for reporting.
    if( !mPriceUnits.empty() ) {
        marketInfo->setString( "price-unit", mPriceUnits );
    }
    if( !mOutputUnits.empty() ) {
        marketInfo->setString( "output-unit", mOutputUnits );
    }

    // Add a dependency between this mand the linked market and include a dummy
    // activity to ensure the dependecy finder has an activity to traverse
    MarketDependencyFinder* depFinder = marketplace->getDependencyFinder();
    depFinder->addDependency( mPolicyName, aRegionName, mLinkedPolicyName, aRegionName, false );
    depFinder->resolveActivityToDependency( aRegionName, mPolicyName,
            new DummyActivity(), new DummyActivity() );
    
    // Interpolations for price and demand adjustments.  Note that we do not use
    // the utilty method for interpolating here to avoid extrapolating the policy.
    for( int i = 1; i < modeltime->getmaxper(); ++i ) {
        if( !mPriceAdjust[ i ].isInited() && mPriceAdjust[ i - 1 ].isInited() ) {
            int j;
            for( j = i + 1; j < modeltime->getmaxper() && !mPriceAdjust[ j ].isInited(); ++j ) {
            }
            if( j < modeltime->getmaxper() ) {
                mPriceAdjust[ i ] = util::linearInterpolateY( modeltime->getper_to_yr( i ),
                                                              modeltime->getper_to_yr( i - 1 ),
                                                              modeltime->getper_to_yr( j ),
                                                              mPriceAdjust[ i - 1 ],
                                                              mPriceAdjust[ j ] );
            }
        }
        if( !mDemandAdjust[ i ].isInited() && mDemandAdjust[ i - 1 ].isInited() ) {
            int j;
            for( j = i + 1; j < modeltime->getmaxper() && !mDemandAdjust[ j ].isInited(); ++j ) {
            }
            if( j < modeltime->getmaxper() ) {
                mDemandAdjust[ i ] = util::linearInterpolateY( modeltime->getper_to_yr( i ),
                                                               modeltime->getper_to_yr( i - 1 ),
                                                               modeltime->getper_to_yr( j ),
                                                               mDemandAdjust[ i - 1 ],
                                                               mDemandAdjust[ j ] );
            }
        }
    }

    // Set the price/demand adjustments into the linked market info object for
    // retrieval by that market object.
    for( int per = startPeriod; per < modeltime->getmaxper(); ++per ){
        IInfo* currMarketInfo = marketplace->getMarketInfo( mPolicyName, aRegionName, per, true );
        if( mPriceAdjust[ per ].isInited() ) {
            currMarketInfo->setDouble( "price-adjust", mPriceAdjust[ per ] );
        }
        if( mDemandAdjust[ per ].isInited() ) {
            currMarketInfo->setDouble( "demand-adjust", mDemandAdjust[ per ] );
        }
    }
}

/*! \brief Determine if the tax is applicable for a given region.
 * \param aRegion Region name.
 * \return Whether the tax is applicable.
 */
bool LinkedGHGPolicy::isApplicable( const string& aRegion ) const {
    return false;
}

/*!
 * \brief Set the mConstraint to the vector passed in.
 * \param aConstraint new mConstraint vector
 */
void LinkedGHGPolicy::setConstraint( const vector<double>& aConstraint ){
    assert( false );
}
