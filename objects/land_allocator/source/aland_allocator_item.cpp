/*! 
* \file aland_allocator_item.cpp
* \ingroup Objects
* \brief ALandAllocatorItem class source file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
#include "land_allocator/include/aland_allocator_item.h"
#include "util/base/include/summary.h"
#include "containers/include/scenario.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Constructor.
* \author James Blackwood
*/
ALandAllocatorItem::ALandAllocatorItem(){
    const Modeltime* modeltime = scenario->getModeltime();
    int maxper = modeltime->getmaxper();
    share.resize( maxper );
    landAllocation.resize( maxper );
    intrinsicRate.resize( maxper );
    summary.resize( maxper );
}

//! Default destructor
ALandAllocatorItem::~ALandAllocatorItem( ) {
}

/*! \brief Set data members from XML input
*
* \author James Blackwood
* \param node pointer to the current node in the XML input tree
*/
void ALandAllocatorItem::XMLParse( const DOMNode* node ){

    // assume we are passed a valid node.
    assert( node );
    
    // Set the node name.
    name = XMLHelper<string>::getAttrString( node, "name" );

    // get all the children.
    DOMNodeList* nodeList = node->getChildNodes();
    
    for( unsigned int i = 0;  i < nodeList->getLength(); ++i ){
        const DOMNode* curr = nodeList->item( i );
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if ( !XMLDerivedClassParse( nodeName, curr ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing "
                    << getXMLName() << "." << endl;
        }
    }
}

/*! \brief Sets the name of this ALandAllocatorItem.
* \author James Blackwood
*/
void ALandAllocatorItem::setName( const string& nameIn ) {
    name = nameIn;
}

/*! \brief This method is called from the node version of calcLandShares and it
*          normalizes each share.
* \param sum The sum of all the children's shares before normalization.
* \author James Blackwood
*/
void ALandAllocatorItem::normalizeLandAllocation( double sum, int period ) {
    share[ period ] /= sum;
}

/*! \brief Returns the share of this land type at a given period.
* \param period the time period.
* \author James Blackwood
*/
double ALandAllocatorItem::getShare ( int period ) {
    return share[ period ];
}

/*! \brief Returns the name.
* \author James Blackwood
* \return the name of this ALandAllocatorItem
*/
const string& ALandAllocatorItem::getName() const {
    return name;
}

/*! 
* \brief Write datamembers to datastream in XML format. Calls XMLWriteElement
*        function from the XMLHelper class for the actual writing.
* \param out Output file in XML format.
* \param tabs Tabs object used to track the number of tabs to print.
* \ref faqitem1 
*/
void ALandAllocatorItem::toInputXML( ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag ( getXMLName(), out, tabs, name );

    toInputXMLDerived( out, tabs );

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Write datamembers to datastream in XML format for debugging purposes.  
* Calls XMLWriteElement function from the XMLHelper class for the actual
* writing. Calls debug functions in other contained objects. 
*
* \param period Model time period
* \param out Output file for debugging purposes in XML format
* \param tabs Tabs object used to track the number of tabs to print.
*/
void ALandAllocatorItem::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    
    XMLWriteOpeningTag ( getXMLName(), out, tabs, name );

    // write out basic datamembers
    XMLWriteElement( intrinsicRate[ period ], "IntrinsicRate", out, tabs );
    XMLWriteElement( landAllocation[ period ], "landAllocation", out, tabs );

    toDebugXMLDerived( period, out, tabs );
    // Finished writing xml for the class members.

    XMLWriteClosingTag( getXMLName(), out, tabs );
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
    fileoutput3(aRegionName,name," "," ","Land Use","000Ha",landAllocation);

}

void ALandAllocatorItem::dbOutput( const string& aRegionName ) const {
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    // write land allocations for region
    dboutput4(aRegionName,name," ","Land Use","000Ha",landAllocation);
}
