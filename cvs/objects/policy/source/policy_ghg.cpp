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
* \file policy_ghg.cpp
* \ingroup Objects
* \brief GHGPolicy class source file.
* \author Sonny Kim
* \date $Date: 2007/01/11 00:12:30 $
* \version $Revision: 1.12.2.4 $
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <iostream>
#include <string>
#include <algorithm>

#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "containers/include/iinfo.h"
#include "util/base/include/model_time.h"
#include "policy/include/policy_ghg.h"
#include "marketplace/include/marketplace.h"
#include "sectors/include/sector_utils.h"

using namespace std;

extern Scenario* scenario;

/*! \brief Default constructor. */
GHGPolicy::GHGPolicy()
{
}

/*!
* \brief Constructor which initializes a GHG policy without setting a tax or
*        constraint.
*/
GHGPolicy::GHGPolicy( const string aName, const string aMarket ):
mName(aName),
mMarket(aMarket)
{
}

/*! \brief Constructor used when explicitly constructing a fixed tax.
*/
GHGPolicy::GHGPolicy( const string aName, const string aMarket,
                      const vector<double>& aTaxes ):
mName(aName),
mMarket(aMarket)
{
    // Ensure that the taxes vector passed in is the right size.
    assert( aTaxes.size() == mConstraint.size() );
    
    std::copy( aTaxes.begin(), aTaxes.end(), mFixedTax.begin() );
}

/*! \brief Create a copy of the GHG policy.
* \return An exact copy of the policy.
*/
GHGPolicy* GHGPolicy::clone() const {
    GHGPolicy* clone = new GHGPolicy();
    clone->copy( *this );
    return clone;
}

void GHGPolicy::copy( const GHGPolicy& aOther ) {
    mName = aOther.mName;
    mMarket = aOther.mMarket;
    std::copy( aOther.mConstraint.begin(), aOther.mConstraint.end(), mConstraint.begin() );
    std::copy( aOther.mFixedTax.begin(), aOther.mFixedTax.end(), mFixedTax.begin() );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& GHGPolicy::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& GHGPolicy::getXMLNameStatic() {
    const static string XML_NAME = "ghgpolicy";
    return XML_NAME;
}

//! Get the ghg policy name. 
const string& GHGPolicy::getName() const {
    return mName;
}

//! Writes data members to data stream in XML format.
void GHGPolicy::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag( getXMLName(), out, tabs, mName );

    // write out the market string.
    XMLWriteElement( mMarket, "market", out, tabs );

    // Write the mConstraint for the current year
    XMLWriteElement( mConstraint[ period ], "constraint", out, tabs );
    
    // Write out the fixed tax for the current year.
    XMLWriteElement( mFixedTax[ period ], "fixedTax", out, tabs );
    
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
* \author Sonny Kim and Josh Lurz
* \param regionName The name of the region the policy controls. 
*/
void GHGPolicy::completeInit( const string& aRegionName ) {
    const Modeltime* modeltime = scenario->getModeltime();
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->createMarket( aRegionName, mMarket, mName, IMarketType::TAX );

    // Set price and output units for period 0 market info
    IInfo* marketInfo = marketplace->getMarketInfo( mName, aRegionName, 0, true );
    //TODO: read-in as data the units of tax and emissions
    marketInfo->setString( "price-unit", "1990$/tC" );
    marketInfo->setString( "output-unit", "MTC" );

    // check for missing periods in which case interpolate
    for( int i = 1; i < modeltime->getmaxper(); ++i ) {
        if( !mFixedTax[ i ].isInited() && mFixedTax[ i - 1 ].isInited() ) {
            int j;
            for( j = i + 1; j < modeltime->getmaxper() && !mFixedTax[ j ].isInited(); ++j ) {
            }
            if( j < modeltime->getmaxper() ) {
                mFixedTax[ i ] = util::linearInterpolateY( modeltime->getper_to_yr( i ),
                                                           modeltime->getper_to_yr( i - 1 ),
                                                           modeltime->getper_to_yr( j ),
                                                           mFixedTax[ i - 1 ],
                                                           mFixedTax[ j ] );
            }
        }
        if( !mConstraint[ i ].isInited() && mConstraint[ i - 1 ].isInited() ) {
            int j;
            for( j = i + 1; j < modeltime->getmaxper() && !mConstraint[ j ].isInited(); ++j ) {
            }
            if( j < modeltime->getmaxper() ) {
                mConstraint[ i ] = util::linearInterpolateY( modeltime->getper_to_yr( i ),
                                                             modeltime->getper_to_yr( i - 1 ),
                                                             modeltime->getper_to_yr( j ),
                                                             mConstraint[ i - 1 ],
                                                             mConstraint[ j ] );
            }
        }
    }

    // Loop through each period
    // If it is a fixed tax, set the tax level and set the market not to solve
    // If it is a constraint, add the constraint to the market and set the 
    // market to solve.
    for( unsigned int i = 0; i < modeltime->getmaxper(); ++i ){
        if( mFixedTax[ i ].isInited() ){
            marketplace->unsetMarketToSolve( mName, aRegionName, i );
            marketplace->setPrice( mName, aRegionName, mFixedTax[ i ], i );
        }
        // if both a fixed tax and constraint are provided the constraint takes
        // precedence and the fixed tax is used as initial guess
        if( mConstraint[ i ].isInited() ){
            marketplace->setMarketToSolve( mName, aRegionName, i );
            // Adding the difference between the constraint for this period
            // and the current supply because addToSupply adds to the current
            // supply.  Passing false to suppress a warning the first time through.
            marketplace->addToSupply( mName, aRegionName, Value( mConstraint[ i ] -
                marketplace->getSupply( mName, aRegionName, i ) ), i, false );
        }

        // GHG policies must have a price >= 0.  It may be the case that the constraint is
        // non-binding at a zero price in which case the solver can use this information to
        // make a supply correction to still ensure equality.
        SectorUtils::setSupplyBehaviorBounds( mName, aRegionName, 0, util::getLargeNumber(), i );
    }
}

/*! \brief Determine if the tax is applicable for a given region.
* \param aRegion Region name.
* \return Whether the tax is applicable.
* \todo This is not entirely correct for multiple regions within a market.
*/
bool GHGPolicy::isApplicable( const string& aRegion ) const {
    return mMarket == "global" || mMarket == aRegion;
}

/*!
* \brief Set the mConstraint to the vector passed in.
* \param aConstraint new mConstraint vector
*/
void GHGPolicy::setConstraint( const vector<double>& aConstraint ){
    std::copy( aConstraint.begin(), aConstraint.end(), mConstraint.begin() );
}
