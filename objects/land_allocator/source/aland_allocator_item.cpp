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
 * \return double reprenting the intrinsic rate of this item for the specified
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

    // write out basic datamembers
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
