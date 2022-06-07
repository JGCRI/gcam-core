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
* \file policy_portfolio_standard.cpp
* \ingroup Objects
* \brief PolicyPortfolioStandard class source file.
* \author Sonny Kim
* \date $Date: 2007/11/21 00:12:30 $
* \version $Revision: 1.12.2.4 $
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <iostream>
#include <string>

#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "containers/include/iinfo.h"
#include "util/base/include/model_time.h"
#include "policy/include/policy_portfolio_standard.h"
#include "marketplace/include/marketplace.h"
#include "util/logger/include/ilogger.h"
#include "sectors/include/sector_utils.h"

using namespace std;

extern Scenario* scenario;

/*! \brief Default constructor. */
PolicyPortfolioStandard::PolicyPortfolioStandard():
mMinPrice( 0.0 ),
mMaxPrice( util::getLargeNumber() ),
mPriceUnits( "1975$/GJ" ),
mOutputUnits( "EJ_or_Share" )
{
    mIsShareBased = false;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Sonny Kim
* \return The constant XML_NAME.
*/
const string& PolicyPortfolioStandard::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Sonny Kim
* \return The constant XML_NAME as a static.
*/
const string& PolicyPortfolioStandard::getXMLNameStatic() {
    const static string XML_NAME = "policy-portfolio-standard";
    return XML_NAME;
}

//! Get the ghg policy name. 
const string& PolicyPortfolioStandard::getName() const {
    return mName;
}

//! Writes data members to data stream in XML format.
void PolicyPortfolioStandard::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag( getXMLName(), out, tabs, mName );

    // write out the market string.
    XMLWriteElement( mMarket, "market", out, tabs );
    
    // write whether we have a tax or subsidy
    XMLWriteElement( mPolicyType, "policyType", out, tabs );
    
    // Write whether we are a fixed tax policy.
    XMLWriteElement( mIsShareBased, "isShareBased", out, tabs );
    
    // Write the constraint for the current year
    XMLWriteElement( mConstraint[ period ], "constraint", out, tabs );
    
    // Write out the fixed tax for the current year.
    XMLWriteElement( mFixedTax[ period ], "fixedTax", out, tabs );
    
    // Write out the share for the current year.
    XMLWriteElement( mShareOfSectorOutput[ period ], "share-of-sector-output", out, tabs );

    XMLWriteElement( mMinPrice[ period ], "min-price", out, tabs );
    XMLWriteElement( mMaxPrice[ period ], "max-price", out, tabs );
    
    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Complete the initialization of the portfolio standard policy.
* \details This function initializes a market for the policy.
* Policy markets are created for both constraint and share policies.
* Both markets are solved.
* \author Sonny Kim
* \param regionName The name of the region the policy controls. 
*/
void PolicyPortfolioStandard::completeInit( const string& aRegionName ) {
    const Modeltime* modeltime = scenario->getModeltime();
    Marketplace* marketplace = scenario->getMarketplace();
    // Create the policy market, a solved market of GHG type which
    // sets the supply side as the constraint and the demand side
    // as the calculated value.

	if ( mPolicyType == "tax") {
        marketplace->createMarket( aRegionName, mMarket, mName, IMarketType::TAX );
    }
	else if ( mPolicyType == "RES") {
        marketplace->createMarket( aRegionName, mMarket, mName, IMarketType::RES );	
	} 
    else if( mPolicyType == "subsidy" ) {
        marketplace->createMarket( aRegionName, mMarket, mName, IMarketType::SUBSIDY );
    }
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Unrecognized policy type: " << mPolicyType << endl;
        mainLog << "Valid policy type strings include: tax, RES, subsidy" << endl;
        abort();
    }

    // Set price and output units for period 0 market info.
    IInfo* marketInfo = marketplace->getMarketInfo( mName, aRegionName, 0, true );
    marketInfo->setString( "price-unit", mPriceUnits );
    marketInfo->setString( "output-unit", mOutputUnits );
    marketInfo->setString( "policy-type", mPolicyType );

    // Put the taxes in the market as the market prices if it is a fixed tax policy.
    for( unsigned int i = 0; i < mFixedTax.size(); ++i ){
        // Make sure that the market is not solved. It could have been set
        // to solve by an earlier run.
        marketplace->unsetMarketToSolve( mName, aRegionName, i );
        if( mFixedTax[ i ].isInited() ){
            marketplace->setPrice( mName, aRegionName, mFixedTax[ i ], i );
        }
    }

    objects::PeriodVector<Value> tempConstraint = mConstraint;

    // Override tempConstraint with shares if shared based.
    // Note: the share is based on the total output of the sector that the
    // technology is in.
    if( mIsShareBased ){
        tempConstraint = mShareOfSectorOutput;
        marketInfo->setBoolean( "isShareBased", true );
    }
    // Set either of the constraints, quantity or share, into the
    // DEMAND side of the market for a subsidy, so that increasing subsidy 
    // (market price) increases supply, and into the SUPPLY side of the 
    // market for a tax, so increasing tax decreases demand.
    // USING SUPPLY SIDE AS THE DEMAND FOR SUBSIDY AND DEMAND SIDE AS THE
    // CONSTRAINT. TAXES USE DEMAND SIDE FOR DEMAND AND SUPPLY SIDE FOR 
    // CONSTRAINT. DEFAULT IS SUBSIDY
    for( int per = 1; per < modeltime->getmaxper(); ++per ){
        // Subtracting the current demand for this period to set the constraint
        // because addToDemand adds to any existing demand in the market.
        // Passing false to suppress a warning the first time through.
        if( tempConstraint[ per ].isInited() ){
            if ( mPolicyType == "tax" ){
                marketplace->setMarketToSolve( mName, aRegionName, per );
                marketplace->addToSupply( mName, aRegionName, Value( tempConstraint[ per ] -
                    marketplace->getSupply( mName, aRegionName, per ) ), per, false );
            }
            else if ( mPolicyType == "RES" ){  // maw doesn't understand this
                marketplace->setMarketToSolve( mName, aRegionName, per );
            //	maw doesn't understand this.  But it doesn;t work otherwise
                marketplace->addToSupply( mName, aRegionName, Value( tempConstraint[ per ] -
                    marketplace->getSupply( mName, aRegionName, per ) ), per, false );
            }
            else {
                marketplace->setMarketToSolve( mName, aRegionName, per );
                marketplace->addToDemand( mName, aRegionName, Value( tempConstraint[ per ] -
                    marketplace->getDemand( mName, aRegionName, per ) ), per, false );
            }
            // Constraint policies must have a price >= mMinPrice.  It may be the case that the constraint is
            // non-binding at the minimum price in which case the solver can use this information to
            // make a supply currection to still ensure equality.
            // Also set a maximum price in which case this value is only used as a hint to the solver to keep
            // the solver on track.
            SectorUtils::setSupplyBehaviorBounds( mName, aRegionName, mMinPrice[ per ], mMaxPrice[ per ], per );
        }
    }
}

