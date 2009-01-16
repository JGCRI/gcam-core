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
* \file resource.cpp
* \ingroup Objects
* \brief Resource class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"

#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <xercesc/dom/DOMNamedNodeMap.hpp>

// class headers
#include "util/base/include/xml_helper.h"
#include "resources/include/resource.h"
#include "resources/include/subresource.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "marketplace/include/imarket_type.h"
#include "resources/include/resource.h"
#include "resources/include/renewable_subresource.h"
#include "resources/include/smooth_renewable_subresource.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/info_factory.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string DepletableResource::XML_NAME = "depresource";
const string FixedResource::XML_NAME = "fixedresource";
const string RenewableResource::XML_NAME = "renewresource";

//! Default constructor.
Resource::Resource()
: mObjectMetaInfo()
{
    nosubrsrc = 0;
    // resize vectors not read in
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    available.resize(maxper); // total resource available
    annualprod.resize(maxper); // annual production rate of resource
    cummprod.resize(maxper); // cumulative production of resource
    rscprc.resize( maxper );
}

//! Destructor.
Resource::~Resource() {
    for ( vector<SubResource*>::iterator iter = subResource.begin(); iter != subResource.end(); iter++ ) {
        delete *iter;
    }
}

//! Set data members from XML input.
void Resource::XMLParse( const DOMNode* node ){
    const Modeltime* modeltime = scenario->getModeltime();
    string nodeName;
    DOMNodeList* nodeList = 0;
    DOMNode* curr = 0;

    // make sure we were passed a valid node.
    assert( node );

    // get the name attribute.
    mName = XMLHelper<string>::getAttr( node, "name" );

    // get all child nodes.
    nodeList = node->getChildNodes();

    // loop through the child nodes.
    for( int i = 0; i < static_cast<int>( nodeList->getLength() ); i++ ){
        curr = nodeList->item( i );
        nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == "output-unit" ){
            mOutputUnit = XMLHelper<string>::getValue( curr );
        }
        else if( nodeName == "price-unit" ){
            mPriceUnit = XMLHelper<string>::getValue( curr );
        }
        else if( nodeName == "market" ){
            mMarket = XMLHelper<string>::getValue( curr ); // only one market element.
        }
        else if( nodeName == "price" ){
            XMLHelper<double>::insertValueIntoVector( curr, rscprc, modeltime );
        }
        else if( nodeName == "keyword" ){
            DOMNamedNodeMap* keywordAttributes = curr->getAttributes();
            for( unsigned int attrNum = 0; attrNum < keywordAttributes->getLength(); ++attrNum ) {
                DOMNode* attrTemp = keywordAttributes->item( attrNum );
                mKeywordMap[ XMLHelper<string>::safeTranscode( attrTemp->getNodeName() ) ] = 
                    XMLHelper<string>::safeTranscode( attrTemp->getNodeValue() );
            }
        }
        else if ( nodeName == object_meta_info_type::getXMLNameStatic() ){
            object_meta_info_type metaInfo;
            if ( metaInfo.XMLParse( curr ) ){
                // Add to collection
                mObjectMetaInfo.push_back( metaInfo );
            }
        }
        else if( XMLDerivedClassParse( nodeName, curr ) ){
            // no-op
        }
        else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing Resource." << endl;
        }
    }
}

//! Write data members to data stream in XML format for replicating input file.
void Resource::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLName(), aOut, aTabs, mName );

    // write the xml for the class members.
    XMLWriteElement( mOutputUnit, "output-unit", aOut, aTabs );
    XMLWriteElement( mPriceUnit, "price-unit", aOut, aTabs );
    XMLWriteElement( mMarket, "market", aOut, aTabs );
    if( !mKeywordMap.empty() ) {
        XMLWriteElementWithAttributes( "", "keyword", aOut, aTabs, mKeywordMap );
    }

    if ( mObjectMetaInfo.size() ) {
        for ( object_meta_info_vector_type::const_iterator metaInfoIterItem = mObjectMetaInfo.begin();
            metaInfoIterItem != mObjectMetaInfo.end(); 
            ++metaInfoIterItem ) {
                metaInfoIterItem->toInputXML( aOut, aTabs );
            }
    }

    // write out resource prices for base period only
    XMLWriteVector( rscprc, "price", aOut, aTabs, scenario->getModeltime() );
    // write out the subresource objects.
    for( vector<SubResource*>::const_iterator i = subResource.begin(); i != subResource.end(); i++ ){
        ( *i )->toInputXML( aOut, aTabs );
    }

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

//! Write data members to data stream in XML format for debugging.
void Resource::toDebugXML( const int period, ostream& aOut, Tabs* aTabs ) const {

    XMLWriteOpeningTag( getXMLName(), aOut, aTabs, mName );

    // Write the xml for the class members.
    XMLWriteElement( mOutputUnit, "output-unit", aOut, aTabs );
    XMLWriteElement( mPriceUnit, "price-unit", aOut, aTabs );
    // Write out the market string.
    XMLWriteElement( mMarket, "market", aOut, aTabs );

    if ( mObjectMetaInfo.size() ) {
        for ( object_meta_info_vector_type::const_iterator metaInfoIterItem = mObjectMetaInfo.begin();
            metaInfoIterItem != mObjectMetaInfo.end(); 
            ++metaInfoIterItem ) {
                metaInfoIterItem->toInputXML( aOut, aTabs );
            }
    }

    // Write out resource prices for debugging period.
    XMLWriteElement( rscprc[ period ], "rscprc", aOut, aTabs );

    // Write out available resources for debugging period.
    XMLWriteElement( available[ period ], "available", aOut, aTabs );

    // Write out annualprod for debugging period.
    XMLWriteElement( annualprod[ period ], "annualprod", aOut, aTabs );

    // Write out cumulative prod for debugging period.
    XMLWriteElement( cummprod[ period ], "cummprod", aOut, aTabs );

    // Write out the number of sub-resources.
    XMLWriteElement( nosubrsrc, "nosubrsrc", aOut, aTabs );

    // Write out the subresource objects.
    for( vector<SubResource*>::const_iterator i = subResource.begin(); i != subResource.end(); i++ ){
        ( *i )->toDebugXML( period, aOut, aTabs );
    }

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

/*! \brief Complete the initialization
*
* This routine is only called once per model run
*
* \author Josh Lurz
* \warning markets are not necessarily set when completeInit is called
*/

void Resource::completeInit( const string& aRegionName, const IInfo* aRegionInfo ) {
    nosubrsrc = static_cast<int>( subResource.size() );

    // default unit to EJ
    if ( mOutputUnit.empty() ) {
        mOutputUnit = "EJ"; 
    }
    // default unit to $/GJ
    if ( mPriceUnit.empty() ) {
        mPriceUnit = "1975$/GJ"; 
    }
    // Allocate the resource info.
    mResourceInfo.reset( InfoFactory::constructInfo( aRegionInfo, aRegionName + "-" + mName ) );
    // Set output and price unit of resource into the resource info.
    mResourceInfo->setString( "output-unit", mOutputUnit );
    mResourceInfo->setString( "price-unit", mPriceUnit );

    if ( mObjectMetaInfo.size() ) {
        // Put values in mSectorInfo
        for ( object_meta_info_vector_type::const_iterator metaInfoIterItem = mObjectMetaInfo.begin(); 
            metaInfoIterItem != mObjectMetaInfo.end();
            ++metaInfoIterItem ) {
                mResourceInfo->setDouble( (*metaInfoIterItem).getName(), (*metaInfoIterItem).getValue() );
            }
    }

    for( vector<SubResource*>::iterator subResIter = subResource.begin(); subResIter != subResource.end(); subResIter++ ) {
        ( *subResIter )->completeInit( mResourceInfo.get() );
    }

    // Set markets for this sector
    setMarket( aRegionName );
}

/*! \brief Perform any initializations needed for each period.
* \details Any initializations or calculations that only need to be done once per
*          period(instead of every iteration) should be placed in this function.
* \author Sonny Kim
* \param aRegionName Region name.
* \param aPeriod Model period
*/
void Resource::initCalc( const string& aRegionName, const int aPeriod ) {
    // call subResource initializations
    for ( unsigned int i = 0; i < subResource.size(); i++ ){
        subResource[i]->initCalc( aRegionName, mName, aPeriod );
    }
}

/*! \brief Perform any calculations needed for each period after solution is
*          found.
* \details Any calculations that only need to be done once per period after
*          solution is found(instead of every iteration) should be placed in
*          this function.
* \author Sonny Kim
* \param aRegionName Region name.
* \param aPeriod Model period
*/
void Resource::postCalc( const string& aRegionName, const int aPeriod ) {
    // Call subResource post calculations
    for ( unsigned int i = 0; i < subResource.size(); i++ ) {
        subResource[i]->postCalc( aRegionName, mName, aPeriod);
    }
    // Reset initial resource prices to solved prices
    rscprc[ aPeriod ] = scenario->getMarketplace()->getPrice( mName, aRegionName, aPeriod, true );
}

//! Create markets
void Resource::setMarket( const string& aRegionName ) {

    Marketplace* pMarketplace   = scenario->getMarketplace();
    const Modeltime* pModeltime = scenario->getModeltime();
    // name is resource name
    if ( pMarketplace->createMarket( aRegionName, mMarket, mName, IMarketType::NORMAL ) ) {
        // Set price and output units for period 0 market info
        IInfo* pMarketInfo = pMarketplace->getMarketInfo( mName, aRegionName, 0, true );
        pMarketInfo->setString( "price-unit", mPriceUnit );
        pMarketInfo->setString( "output-unit", mOutputUnit );

        pMarketplace->setPriceVector( mName, aRegionName, rscprc );
        for( int period = 0; period < pModeltime->getmaxper(); ++period ){
        
            // TODO: Remove or improve this. Intermittent technologies need to know during initCalc 
            // which good has a variance. This will get set again later, which is bad.
            IInfo* marketInfo = pMarketplace->getMarketInfo( mName, aRegionName, period, true );
            marketInfo->setDouble( "resourceVariance", 0 );
            if( period >= 1 ){
                pMarketplace->setMarketToSolve( mName, aRegionName, period );
            }
  
            // Put region name in market info for error checking in renewable sub-resource
            marketInfo->setString( "Market-Region", mMarket );
      }
    }
    // Put values in market.
    if ( mObjectMetaInfo.size() ) {
        // Put values in market
        for ( int period = 0; period < pModeltime->getmaxper(); ++period ) {
            IInfo* pMarketInfo = pMarketplace->getMarketInfo( mName, aRegionName, period, true );
            if ( pMarketInfo ) {
                for ( object_meta_info_vector_type::const_iterator metaInfoIterItem = mObjectMetaInfo.begin();
                    metaInfoIterItem != mObjectMetaInfo.end(); 
                    ++metaInfoIterItem ) {
                        pMarketInfo->setDouble( (*metaInfoIterItem).getName(), (*metaInfoIterItem).getValue() );
                }
            }
        }
    }
}

//! Return resource name.
const string& Resource::getName() const {
    return mName;
}

//! Calculate total resource supply for a period.
void Resource::calcSupply( const string& aRegionName, const GDP* aGDP, const int aPeriod ){
    // This code is moved down from Region
    Marketplace* marketplace = scenario->getMarketplace();

    double price = marketplace->getPrice( mName, aRegionName, aPeriod );
    double lastPeriodPrice;

    if ( aPeriod == 0 ) {
        lastPeriodPrice = price;
    }
    else {
        lastPeriodPrice = marketplace->getPrice( mName, aRegionName, aPeriod - 1 );
    }

    // calculate annual supply
    annualsupply( aRegionName, aPeriod, aGDP, price, lastPeriodPrice ); 
    // set market supply of resource
    marketplace->addToSupply( mName, aRegionName, annualprod[ aPeriod ], aPeriod );
}

void Resource::cumulsupply(double prc,int per)
{   
    int i=0;
    cummprod[per]=0.0;

    rscprc[per] = prc;
    // sum cumulative production of each subsector
    for (i=0;i<nosubrsrc;i++) {
        subResource[i]->cumulsupply(prc,per);
        cummprod[per] += subResource[i]->getCumulProd(per);
    }
}

//! Calculate annual production
void Resource::annualsupply( const string& regionName, int per, const GDP* gdp, double price, double prev_price )
{   
    int i=0;
    annualprod[per]=0.0;
    available[per]=0.0;

    // calculate cumulative production
    cumulsupply(price,per);

    // sum annual production of each subsector
    for (i=0;i<nosubrsrc;i++) {
        subResource[i]->annualsupply( per, gdp, price, prev_price );
        annualprod[per] += subResource[i]->getAnnualProd(per);
        available[per] += subResource[i]->getAvailable(per);
    }
}


//! Return annual production of resources.
double Resource::getAnnualProd( const string& aRegionName, const int per ) const {
    return annualprod[per];
}

//! Write resource output to file.
void Resource::csvOutputFile( const string& regname )
{
    // function protocol
    void fileoutput3( string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    // function arguments are variable name, double array, db name, table name
    // the function writes all years
    // total sector output
    fileoutput3( regname,mName," "," ","production",mOutputUnit,annualprod);

    // do for all subsectors in the sector
    for (int i=0;i<nosubrsrc;i++) {
        subResource[i]->csvOutputFile(regname ,mName);
    }
}

//! Write resource output to database.
void Resource::dbOutput( const string& regname ) {
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    // resource price
    dboutput4(regname,"Price","by Sector",mName,mPriceUnit,rscprc);
    // do for all subsectors in the sector
    temp.assign( temp.size(), 0.0 );
    for (int m=0;m<maxper;m++) {
        for (int i=0;i<nosubrsrc;i++) {
            temp[m] += subResource[i]->getCumulProd(m);
        }
    }
    dboutput4(regname,"Resource","CummProd "+mName,"zTotal",mOutputUnit,temp);

    temp.assign( temp.size(), 0.0 );
    for (int m=0;m<maxper;m++) {
        for (int i=0;i<nosubrsrc;i++) {
            temp[m] += subResource[i]->getAnnualProd(m);
        }
    }
    dboutput4(regname,"Resource","annual-production", mName, mOutputUnit, temp);

    temp.assign( temp.size(), 0.0 );
    // do for all subsectors in the sector
    for (int m=0;m<maxper;m++) {
        for (int i=0;i<nosubrsrc;i++) {
            temp[m] += subResource[i]->getAvailable(m);
        }
    }
    dboutput4(regname,"Resource","Available "+mName,"zTotal",mOutputUnit,temp);
    
    // do for all subsectors in the sector
    for (int i=0;i<nosubrsrc;i++) {
        subResource[i]->dbOutput(regname,mName);
    }
}

/*! \brief Update an output container for a Resource.
* \param aVisitor Output container to update.
* \param aPeriod Period to update.
*/
void Resource::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitResource( this, aPeriod );

    // Update the output container for the subresources.
    for( unsigned int i = 0; i < subResource.size(); ++i ){
        subResource[ i ]->accept( aVisitor, aPeriod );
    }

    aVisitor->endVisitResource( this, aPeriod );
}

// ************************************************************
// Definitions for the derived classes below.
// Since these are very small changes, keep in same file for simplicity
// ************************************************************

// *******************************************************************
// DepletableResource Class
// *******************************************************************
/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& DepletableResource::getXMLName() const {
    return XML_NAME;
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
const std::string& DepletableResource::getXMLNameStatic() {
    return XML_NAME;
}

//! 
/*! In this case, this read-in just substantiates the appropriate type of subResource */
/*! \brief Performs XML read-in that is specific to this derived class
*
*  this case, this read-in just instantiates the appropriate type of subResource
*
* \author Steve Smith
* \param aNodeName Name of the current node.
* \param aNode Pointer to the current node in the XML input tree
* \return Whether an element was parsed.
* \todo In the input file, *SubResources should be read in as such, not silently converted here.
*/
bool DepletableResource::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aNode ) {
    // TODO: Fix this.
    if( aNodeName == SubResource::getXMLNameStatic() || aNodeName == SubDepletableResource::getXMLNameStatic() ){
        parseContainerNode( aNode, subResource, subResourceNameMap, new SubDepletableResource() );
        return true;
    }
    return false;
}

// *******************************************************************
// FixedResource Class
// *******************************************************************
/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& FixedResource::getXMLName() const {
    return XML_NAME;
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
const std::string& FixedResource::getXMLNameStatic() {
    return XML_NAME;
}

//! 
/*! In this case, this read-in just substantiates the appropriate type of subResource */
/*! \brief Performs XML read-in that is specific to this derived class
*
*  this case, this read-in just instantiates the appropriate type of subResource
*
* \author Steve Smith
* \param node pointer to the current node in the XML input tree
* \param nodeName name of the current node 
* \return Whether an element was parsed.
*/
bool FixedResource::XMLDerivedClassParse( const string& nodeName, const DOMNode* node ) {
    if( nodeName == SubResource::getXMLNameStatic() || nodeName == SubFixedResource::getXMLNameStatic() ){
        parseContainerNode( node, subResource, subResourceNameMap, new SubFixedResource() );
        return true;
    }
    return false;
}
// *******************************************************************
// RenewableResource Class
// *******************************************************************

//! \brief set the size for the resourceVariance member
RenewableResource::RenewableResource()
{
    resourceVariance.resize( scenario->getModeltime()->getmaxper() );
    resourceCapacityFactor.resize( scenario->getModeltime()->getmaxper() );
}


/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& RenewableResource::getXMLName() const {
    return XML_NAME;
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
const std::string& RenewableResource::getXMLNameStatic() {
    return XML_NAME;
}

//! 
/*! In this case, this read-in just substantiates the appropriate type of subResource */
/*! \brief Performs XML read-in that is specific to this derived class
*
*  this case, this read-in just instantiates the appropriate type of subResource
*
* \author Steve Smith
* \param node pointer to the current node in the XML input tree
* \param nodeName name of the current node 
* \return Whether an element was parsed.
*/
bool RenewableResource::XMLDerivedClassParse( const string& nodeName, const DOMNode* node )
{
    if( nodeName == SubResource::getXMLNameStatic() ||
        nodeName == SubRenewableResource::getXMLNameStatic() ) {
            parseContainerNode( node, subResource, subResourceNameMap, new SubRenewableResource() );
            return true;
        }
    else if ( nodeName == SmoothRenewableSubresource::getXMLNameStatic() ) {
        parseContainerNode( node, subResource, subResourceNameMap, new SmoothRenewableSubresource() );
        return true;
    }
    return false;
}

/*! \brief Complete the initialization
*
* This routine is only called once per model run.
*
* \author Sonny Kim
* \warning markets are not necessarily set when completeInit is called
*/

void RenewableResource::completeInit( const string& aRegionName, const IInfo* aRegionInfo ) {

    // Complete resource initialization.
    // Base class completeInit must be called first to create the market.
    Resource::completeInit( aRegionName, aRegionInfo );

    Marketplace* pMarketplace = scenario->getMarketplace();
    const Modeltime* pModeltime = scenario->getModeltime();
    // Initialize resource capacity factor for renewable resources to 0.35
    // to avoid sequence issues and warning messages to screen.
    // This is updated in annualsupply().
    for( int period = 0; period < pModeltime->getmaxper(); ++period ){
        IInfo* marketInfo = pMarketplace->getMarketInfo( mName, aRegionName, period, true );
        marketInfo->setDouble( "resourceCapacityFactor", 0.35 );
    }
}    

//! Calculate annual production
/*! \brief Adds to the base Resource::annualsupply by computing a weighted-average
*  variance of the resource based on the variance of the subresources.
*
* \author Steve Smith.  Mod for intermittent by Marshall Wise
*/
void RenewableResource::annualsupply( const string& regionName, int per, const GDP* gdp, double price, double prev_price )
{

    // calculate cumulative production
    cumulsupply(price,per);

    // clear out sums for this iteration
    annualprod[per]=0.0;
    available[per]=0.0;
    resourceVariance[per]=0.0;
    resourceCapacityFactor[per] = 0.0;
    
    // If annual production is extremely small or zero, these will not be calculated properly.
    // In this case, set production to some minimum number.
    const double MIN_RESOURCE_PROD = util::getVerySmallNumber();
    double adjustedProduction = 0;
    
    // sum annual production of each subsector
    for (int i=0;i<nosubrsrc;i++) {
        subResource[i]->annualsupply( per, gdp, price, prev_price );
        annualprod[per] += subResource[i]->getAnnualProd(per);
        double adjustedSubResourceProd = max( subResource[i]->getAnnualProd(per), MIN_RESOURCE_PROD );
        adjustedProduction += adjustedSubResourceProd;
        available[per] += subResource[i]->getAvailable(per);
        
        // and compute weighted average variance
        resourceVariance[per] += adjustedSubResourceProd * subResource[i]->getVariance();
        // and compute weighted average capacity factor
        resourceCapacityFactor[per] += adjustedSubResourceProd * subResource[i]->getAverageCapacityFactor();
    }

    // TODO: This is currently wrong because the resource calculation is done last
    // so if there were multiple subresources with different variances this would
    // calculate a stale value and result in solution problems. It would also
    // be wrong in that case for global markets.

    // This may be a global market and the resource may only exist to add the
    // region into the market. In this case the resource will not have any
    // subresources, and should not adjust the market info values.
    if( !subResource.empty() ){
        if ( adjustedProduction >= MIN_RESOURCE_PROD ) {
            resourceVariance[per] /= adjustedProduction;
            resourceCapacityFactor[per] /= adjustedProduction;
        }

        // add variance to marketinfo
        IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( mName, regionName, per, true );
        marketInfo->setDouble( "resourceVariance", resourceVariance[ per ] );

        // add capacity factor to marketinfo
        marketInfo->setDouble( "resourceCapacityFactor", resourceCapacityFactor[ per ] );
    }
}
