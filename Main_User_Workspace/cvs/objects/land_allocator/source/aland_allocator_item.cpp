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
 * \file aland_allocator_item.cpp
 * \ingroup Objects
 * \brief ALandAllocatorItem class source file.
 * \author James Blackwood
 */

#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
#include "land_allocator/include/aland_allocator_item.h"
#include "containers/include/scenario.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*!
 * \brief Constructor.
 * \param aParent Pointer to this item's parent.
 * \param aType Enum representing this nodes type.
 * \author James Blackwood
 */
ALandAllocatorItem::ALandAllocatorItem( const ALandAllocatorItem* aParent,
                                        const TreeItemType aType )
: mParent( aParent ),
  mType( aType )
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
 * \brief Returns the intrinsic rate for the specified period.
 * \param aPeriod The period to get the rate for.
 * \return double representing the intrinsic rate of this item for the specified
 *         period.
 */
double ALandAllocatorItem::getInstrinsicRate( const int aPeriod ) const {
    assert( mIntrinsicRate[ aPeriod ].isInited() );
    return mIntrinsicRate[ aPeriod ];
}

/*!
 * \brief Returns the share for the specified period.
 * \param aPeriod The period to get the rate for.
 * \return double representing the share of this item for the specified period.
 */
double ALandAllocatorItem::getShare( const int aPeriod ) const {
    assert( mShare[ aPeriod ].isInited() &&
            mShare[ aPeriod ] >= 0 &&
            mShare[ aPeriod ] <= 1 );
    return mShare[ aPeriod ];
}

/*!
 * \brief Returns an enum representing the type of node (node/leaf).
 * \return Enum representing the type of this item.
 */
TreeItemType ALandAllocatorItem::getType() const {
    return mType;
}

void ALandAllocatorItem::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    
    XMLWriteOpeningTag ( getXMLName(), aOut, aTabs, mName );

    // write out basic data members
    XMLWriteElement( mIntrinsicRate[ aPeriod ], "IntrinsicRate", aOut, aTabs );
    XMLWriteElement( mShare[ aPeriod ], "share", aOut, aTabs );

    toDebugXMLDerived( aPeriod, aOut, aTabs );
    // Finished writing xml for the class members.

    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

/*! \brief Write output to csv output file. 
*
* Put the variables here that will be output for each node and leaf
*
* \author Steve Smith
*/
void ALandAllocatorItem::csvOutput( const string& aRegionName ) const {
    // function protocol
    void fileoutput3(string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    // write land allocations for region
    vector<double> temp( scenario->getModeltime()->getmaxper() );
    for( unsigned int i = 0; i < temp.size(); ++i ){
        temp[ i ] = getTotalLandAllocation( eAnyLand, i );
    }
    fileoutput3(aRegionName, mName," "," ","Land Use","000Ha", temp );

}

void ALandAllocatorItem::dbOutput( const string& aRegionName ) const {
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    // write land allocations for region
    vector<double> temp( scenario->getModeltime()->getmaxper() );
    for( unsigned int i = 0; i < temp.size(); ++i ){
        temp[ i ] = getTotalLandAllocation( eAnyLand, i );
    }
    dboutput4(aRegionName, "Land Allocation", mName,"Land Use","000Ha", temp );
}

/*! \brief Return default share for use in calculating intrinsic rates. 
*
* This routine provides the default numerical value for the share needed to
* calculate the intrinsic rates. Returning an arbirary value is a hack,
* need to find a better way to do this. Using the intrinsic rate from a
* comparable crop that is grown in the calibration period might be a better
* solution (or scaled to a comparable crop).
*
* This routine provides this value for both nodes and leaves that have no
* land share in the calibration period.
*
* \author Steve Smith
*/
double ALandAllocatorItem::getDefaultShare( ) const {
    return 0.25;
}

