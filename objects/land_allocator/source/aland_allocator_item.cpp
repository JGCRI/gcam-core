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

/*! \brief Constructor.
* \author James Blackwood
*/
ALandAllocatorItem::ALandAllocatorItem()
{
}

//! Default destructor
ALandAllocatorItem::~ALandAllocatorItem() {
}

/*! \brief This method is called from the node version of calcLandShares and it
*          normalizes each share.
* \param aSum The sum of all the children's shares before normalization.
* \author James Blackwood
*/
void ALandAllocatorItem::normalizeLandAllocation( const double aSum,
                                                  const int aPeriod )
{
    if ( aSum > util::getTinyNumber() ) {
        mShare[ aPeriod ] /= aSum;
    }
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Land Allocator item " << mName << " had invalid share total of " << aSum << "." << endl;
        mShare[ aPeriod ] = 0;
    }
    assert( util::isValidNumber( mShare[ aPeriod ] ) );
}

/*! \brief Returns the share of this land type at a given period.
* \param period the time period.
* \author James Blackwood
*/
double ALandAllocatorItem::getShare( const int aPeriod ) const {
    return mShare[ aPeriod ];
}

/*! \brief Returns the name.
* \author James Blackwood
* \return the name of this ALandAllocatorItem
*/
const string& ALandAllocatorItem::getName() const {
    return mName;
}

/*! \brief Write datamembers to datastream in XML format for debugging purposes.  
* Calls XMLWriteElement function from the XMLHelper class for the actual
* writing. Calls debug functions in other contained objects. 
*
* \param aPeriod Model time period
* \param aOut Output file for debugging purposes in XML format
* \param aTabs Tabs object used to track the number of tabs to print.
*/
void ALandAllocatorItem::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    
    XMLWriteOpeningTag ( getXMLName(), aOut, aTabs, mName );

    // write out basic datamembers
    XMLWriteElement( mIntrinsicRate[ aPeriod ], "IntrinsicRate", aOut, aTabs );
    XMLWriteElement( mLandAllocation[ aPeriod ], "landAllocation", aOut, aTabs );

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
    fileoutput3(aRegionName, mName," "," ","Land Use","000Ha", mLandAllocation.convertToVector() );

}

void ALandAllocatorItem::dbOutput( const string& aRegionName ) const {
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    // write land allocations for region
    dboutput4(aRegionName, "Land Allocation", mName,"Land Use","000Ha", mLandAllocation.convertToVector() );
}
