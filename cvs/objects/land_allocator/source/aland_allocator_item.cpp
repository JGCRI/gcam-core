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
 * \file aland_allocator_item.cpp
 * \ingroup Objects
 * \brief ALandAllocatorItem class source file.
 * \author James Blackwood, Kate Calvin
 */

#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
#include "land_allocator/include/aland_allocator_item.h"
#include "containers/include/scenario.h"
#include "functions/include/idiscrete_choice.hpp"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*!
 * \brief Constructor.
 * \param aParent Pointer to this item's parent.
 * \param aType Enum representing this nodes type.
 * \author James Blackwood, Kate Calvin
 */
ALandAllocatorItem::ALandAllocatorItem( const ALandAllocatorItem* aParent,
                                        const LandAllocatorItemType aType )
    : mParent( aParent ), 
      mLandAllocation( 0.0 ),
      mShare( -1.0 ), // this is so initialization can be checked.
      mProfitRate( 0.0 ), 
      mType( aType ),
      mIsLandExpansionCost( false )
{
}

//! Destructor
ALandAllocatorItem::~ALandAllocatorItem() {
}

void ALandAllocatorItem::setShare( const double aShare,
                                   const int aPeriod )
{
    assert( aShare >= 0 && aShare <= 1 );
    mShare[ aPeriod ] = aShare;
}

const string& ALandAllocatorItem::getName() const {
    return mName;
}

/*!
 * \brief Returns the parent of the item.
 * \return ALandAllocatorItem pointer to the parent of this item.
 */
const ALandAllocatorItem* ALandAllocatorItem::getParent() const {
    return mParent;
}

/*!
 * \brief Returns the profit rate for the specified period.
 * \param aPeriod The period to get the rate for.
 * \return double representing the profit rate of this item for the specified
 *         period.
 */
double ALandAllocatorItem::getProfitRate( const int aPeriod ) const {
    return mProfitRate[ aPeriod ];
}

/*!
 * \brief Returns the share for the specified period.
 * \param aPeriod The period to get the rate for.
 * \return double representing the share of this item for the specified period.
 */
double ALandAllocatorItem::getShare( const int aPeriod ) const {
    return mShare[ aPeriod ];
}

/*!
 * \brief Returns an enum representing the type of node (node/leaf).
 * \return Enum representing the type of this item.
 */
LandAllocatorItemType ALandAllocatorItem::getType() const {
    return mType;
}

void ALandAllocatorItem::calculateShareWeights( const string& aRegionName, 
                                                IDiscreteChoice* aChoiceFnAbove,
                                                const int aPeriod )
{

    mShareWeight[ aPeriod ] = aChoiceFnAbove->calcShareWeight( mShare[ aPeriod ], mProfitRate[ aPeriod ], aPeriod );

    // if we are in the final calibration year and we have "ghost" share-weights to calculate
    // we do that now with the current profit rate in the final calibration period.
    const Modeltime* modeltime = scenario->getModeltime();
    if( aPeriod == modeltime->getFinalCalibrationPeriod() ) {
        for( int futurePer = aPeriod + 1; futurePer < modeltime->getmaxper(); ++futurePer ) {
            if( mGhostUnormalizedShare[ futurePer ].isInited() ) {
                double profitRateForCal = mProfitRate[ aPeriod ];
                mShareWeight[ futurePer ] = aChoiceFnAbove->calcShareWeight( mGhostUnormalizedShare[ futurePer ],
                                                                             profitRateForCal,
                                                                             futurePer );
            }
        }
    }
}

void ALandAllocatorItem::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag ( getXMLName(), aOut, aTabs, mName );

    // write out basic data members
    XMLWriteElement( mLandAllocation[ aPeriod ], "land-allocation", aOut, aTabs );
    XMLWriteElement( mProfitRate[ aPeriod ], "profit-rate", aOut, aTabs );
    XMLWriteElement( mShare[ aPeriod ], "share", aOut, aTabs );
    XMLWriteElement( mShareWeight[ aPeriod ], "share-weight", aOut, aTabs );
    XMLWriteElement( mGhostUnormalizedShare[ aPeriod ], "ghost-unnormalized-share", aOut, aTabs );

    // if we are in the final calibration year and we have "ghost" share-weights to calculate it
    // would be useful to see what was calculated.
    const Modeltime* modeltime = scenario->getModeltime();
    if( aPeriod == modeltime->getFinalCalibrationPeriod() ) {
        for( int futurePer = aPeriod + 1; futurePer < modeltime->getmaxper(); ++futurePer ) {
            if( mGhostUnormalizedShare[ futurePer ].isInited() ) {
                const int futureYear = modeltime->getper_to_yr( futurePer );
                XMLWriteElement( mShareWeight[ futurePer ], "share-weight", aOut, aTabs, futureYear );
                XMLWriteElement( mGhostUnormalizedShare[ futurePer ], "ghost-unnormalized-share", aOut, aTabs, futureYear );
            }
        }
    }

    // Call derived class method
    toDebugXMLDerived( aPeriod, aOut, aTabs );

    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

