/*! 
* \file resource.cpp
* \ingroup CIAM
* \brief Resource class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"

#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include <cassert>
#include <xercesc/dom/DOM.hpp>
#include "util/base/include/xml_helper.h"

// class headers
#include "resources/include/resource.h"
#include "resources/include/subresource.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "resources/include/resource.h"
#include "resources/include/renewable_subresource.h"

using namespace std;
using namespace xercesc;

extern ofstream bugoutfile,outfile;	
extern Scenario* scenario;

//! Default constructor.
Resource::Resource(){
   nosubrsrc = 0;
   // resize vectors not read in
   const Modeltime* modeltime = scenario->getModeltime();
   const int maxper = modeltime->getmaxper();
   available.resize(maxper); // total resource availabl
   annualprod.resize(maxper); // annual production rate of resource
   cummprod.resize(maxper); // cummulative production of resource
   rscprc.resize( maxper ); 
}

//! Destructor.
Resource::~Resource() {
   for ( vector<SubResource*>::iterator iter = subResource.begin(); iter != subResource.end(); iter++ ) {
      delete *iter;
   }
}

//! Clear data members.
void Resource::clear(){
   name = "";
   market = "";
   nosubrsrc = 0;
   rscprc.clear();
   subResource.clear();
   available.clear();
   annualprod.clear();
   cummprod.clear();
   rscprc.clear();
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
   name = XMLHelper<string>::getAttrString( node, "name" );
   
   // get all child nodes.
   nodeList = node->getChildNodes();
   
   // loop through the child nodes.
   for( int i = 0; i < static_cast<int>( nodeList->getLength() ); i++ ){
      curr = nodeList->item( i );
      nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
      
      if( nodeName == "#text" ) {
         continue;
      }
      
      else if( nodeName == "market" ){
         market = XMLHelper<string>::getValueString( curr ); // only one market element.
      }
      else if( nodeName == "price" ){
         XMLHelper<double>::insertValueIntoVector( curr, rscprc, modeltime );
      }
      else {
         XMLDerivedClassParse( nodeName, curr );
      }
   }
}

//! Complete the initialization.
void Resource::completeInit() {
   nosubrsrc = static_cast<int>( subResource.size() );
   
   for( vector<SubResource*>::iterator subResIter = subResource.begin(); subResIter != subResource.end(); subResIter++ ) {
      ( *subResIter )->completeInit();
   }
}

//! Write datamembers to datastream in XML format for replicating input file.
void Resource::toXML( ostream& out ) const {
   const Modeltime* modeltime = scenario->getModeltime();
   // write the beginning tag.
   Tabs::writeTabs( out );
   out << "<" << getXMLType() << " name=\"" << name << "\">"<< endl;
   
   // increase the indent.
   Tabs::increaseIndent();
   
   // write the xml for the class members.
   // write out the market string.
   XMLWriteElement( market, "market", out );
   
   // write out resource prices for base period only
   int m = 0;
   XMLWriteElement( rscprc[m], "price", out, modeltime->getper_to_yr(m));
   
   // write out the depresource objects.
   for( vector<SubResource*>::const_iterator i = subResource.begin(); i != subResource.end(); i++ ){
      ( *i )->toXML( out );
   }
   
   // finished writing xml for the class members.
   
   // decrease the indent.
   Tabs::decreaseIndent();
   
   // write the closing tag.
   Tabs::writeTabs( out );
   out << "</" << getXMLType() << ">" << endl;
   
}

//! Write datamembers to datastream in XML format for outputting results.
void Resource::toOutputXML( ostream& out ) const {
   const Modeltime* modeltime = scenario->getModeltime();
   // write the beginning tag.
   Tabs::writeTabs( out );
   out << "<" << getXMLType() << " name=\"" << name << "\">"<< endl;
   
   // increase the indent.
   Tabs::increaseIndent();
   
   // write the xml for the class members.
   // write out the market string.
   XMLWriteElement( market, "market", out );
   
   // write out resource prices for all periods
   for(int m = 0; m < static_cast<int>(rscprc.size()); m++ ) {
      XMLWriteElement( rscprc[m], "price", out, modeltime->getper_to_yr(m));
   }
   
   // write out the depresource objects.
   for( vector<SubResource*>::const_iterator i = subResource.begin(); i != subResource.end(); i++ ){
      ( *i )->toXML( out );
   }
   
   // finished writing xml for the class members.
   
   // decrease the indent.
   Tabs::decreaseIndent();
   
   // write the closing tag.
   Tabs::writeTabs( out );
   out << "</" << getXMLType() << ">" << endl;
   
}

//! Write datamembers to datastream in XML format for debugging.
void Resource::toDebugXML( const int period, ostream& out ) const {
   
   // write the beginning tag.
   Tabs::writeTabs( out );
   out << "<" << getXMLType() << " name=\"" << name << "\">"<< endl;
   
   // increase the indent.
   Tabs::increaseIndent();
   
   // Write the xml for the class members.
   
   // Write out the market string.
   XMLWriteElement( market, "market", out );
   
   // Write out resource prices for debugging period.
   XMLWriteElement( rscprc[ period ], "rscprc", out );
   
   // Write out available resources for debugging period.
   XMLWriteElement( available[ period ], "available", out );
   
   // Write out annualprod for debugging period.
   XMLWriteElement( annualprod[ period ], "annualprod", out );
   
   // Write out cumulative prod for debugging period.
   XMLWriteElement( cummprod[ period ], "cummprod", out );
   
   // Write out the number of sub-resources.
   XMLWriteElement( nosubrsrc, "nosubrsrc", out );
   
   // Write out the depresource objects.
   for( vector<SubResource*>::const_iterator i = subResource.begin(); i != subResource.end(); i++ ){
      ( *i )->toDebugXML( period, out );
   }
   
   // finished writing xml for the class members.
   
   // decrease the indent.
   Tabs::decreaseIndent();
   
   // write the closing tag.
   Tabs::writeTabs( out );
   out << "</" << getXMLType() << ">" << endl;
}

//! Create markets
void Resource::setMarket( const string& regionName ) {
   
   Marketplace* marketplace = scenario->getMarketplace();
   // name is resource name
   if ( marketplace->createMarket( regionName, market, name, Marketplace::NORMAL ) ) {
      marketplace->setPriceVector( name, regionName, rscprc );
      marketplace->setMarketToSolve (name, regionName);
   }
}

//! Return resource name.
string Resource::getName() const {
   return name;
}

//! Return resource price.
double Resource::getPrice(int per)
{
   return rscprc[per] ;
}

//! Returns total number of subsectors.
int Resource::getNoSubrsrc() 
{
   return nosubrsrc;
}

void Resource::cumulsupply(double prc,int per)
{	
   int i=0;
   cummprod[per]=0.0;
   
   rscprc[per] = prc;
   // sum cummulative production of each subsector
   for (i=0;i<nosubrsrc;i++) {
      subResource[i]->cumulsupply(prc,per);
      cummprod[per] += subResource[i]->getCumulProd(per);
   }
}

double Resource::getCummProd(int per)
{
   return cummprod[per];
}


//! Calculate annual production
void Resource::annualsupply(int per,double gnp,double prev_gnp,double price,double prev_price)
{	
   int i=0;
   annualprod[per]=0.0;
   available[per]=0.0;
   
   // calculate cummulative production
   cumulsupply(price,per);
   
   // sum annual production of each subsector
   for (i=0;i<nosubrsrc;i++) {
      subResource[i]->annualsupply(per,gnp,prev_gnp,price,prev_price);
      annualprod[per] += subResource[i]->getAnnualProd(per);
      available[per] += subResource[i]->getAvailable(per);
   }
}


//! Return annual production of resources.
double Resource::getAnnualProd(int per)
{
   return annualprod[per];
}

//! Return resource available from all subsectors.
double Resource::getAvailable(int per)
{
   return available[ per ];
}

//! Return resource available from each subsector.
double Resource::getSubAvail( const string& subResourceName, const int per ) {
   for (int i=0;i<nosubrsrc;i++) {
      if (subResource[i]->getName() == subResourceName )
         return subResource[i]->getAvailable(per);
   }
   return 0;
}

void Resource::show()
{
   int i=0;
   //write to file or database later
   cout << name << endl;
   cout << "Number of Subsectors: " << nosubrsrc <<"\n";
   for (i=0;i<nosubrsrc;i++) {
      cout<<subResource[i]->getName()<<"\n";
   }
}

//! Write resource output to file.
void Resource::outputfile( const string& regname )
{
   // function protocol
   void fileoutput3( string var1name,string var2name,string var3name,
				  string var4name,string var5name,string uname,vector<double> dout);
   
   // function arguments are variable name, double array, db name, table name
   // the function writes all years
   // total sector output
   fileoutput3( regname,name," "," ","production","EJ",annualprod);
   
   // do for all subsectors in the sector
   for (int i=0;i<nosubrsrc;i++) {
      subResource[i]->outputfile(regname ,name);
   }
}

//! Write resource output to database.
void Resource::MCoutput( const string& regname ) {
   const Modeltime* modeltime = scenario->getModeltime();
   const int maxper = modeltime->getmaxper();
   vector<double> temp(maxper);
   // function protocol
   void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);
   
   // function arguments are variable name, double array, db name, table name
   // the function writes all years
   // total sector output
   dboutput4(regname,"Pri Energy","Production by Sector",name,"EJ",annualprod);
   // resource price
   dboutput4(regname,"Price","by Sector",name,"$/GJ",rscprc);
   
   // do for all subsectors in the sector
   for (int i=0;i<nosubrsrc;i++) {
      subResource[i]->MCoutput(regname,name);
   }
}

// ************************************************************
// Definitions for the derived classes below.
// Since these are very small changes, keep in same file for simplicity
// ************************************************************

// *******************************************************************
// DepletableResource Class
// *******************************************************************
//! Returns the type of the Resource.
const string DepletableResource::getType() const {
   return "Depletable";
}

//! Returns the XML type of the Resource.
const string DepletableResource::getXMLType() const {
   return "depresource";
}

//! Performs XML read-in that is specific to this derived class
/*! In this case, this read-in just substantiates the appropriate type of subResource */
void DepletableResource::XMLDerivedClassParse( const string nodeName, const DOMNode* node ) {   
   SubResource* tempSubResource = 0;
   
   if( nodeName == "subresource" ){
      map<string,int>::const_iterator subResourceMapIter = subResourceNameMap.find( XMLHelper<string>::getAttrString( node, "name" ) );
      if( subResourceMapIter != subResourceNameMap.end() ) {
         // subResource already exists.
         subResource[ subResourceMapIter->second ]->XMLParse( node );
      }
      else {
         tempSubResource = new SubDepletableResource();
         tempSubResource->XMLParse( node );
         subResource.push_back( tempSubResource );
         subResourceNameMap[ tempSubResource->getName() ] = static_cast<int>( subResource.size() ) - 1;
      }
   }
}
// *******************************************************************
// FixedResource Class
// *******************************************************************
//! Returns the type of the Resource.
const string FixedResource::getType() const {
   return "Fixed";
}

//! Returns the XML type of the Resource.
const string FixedResource::getXMLType() const {
   return "fixedresource";
}

//! Performs XML read-in that is specific to this derived class
/*! In this case, this read-in just substantiates the appropriate type of subResource */
void FixedResource::XMLDerivedClassParse( const string nodeName, const DOMNode* node ) {
   SubResource* tempSubResource = 0;
   if( nodeName == "subresource" ){
      map<string,int>::const_iterator subResourceMapIter = subResourceNameMap.find( XMLHelper<string>::getAttrString( node, "name" ) );
      if( subResourceMapIter != subResourceNameMap.end() ) {
         // subResource already exists.
         subResource[ subResourceMapIter->second ]->XMLParse( node );
      }
      else {
         tempSubResource = new SubFixedResource();
         tempSubResource->XMLParse( node );
         subResource.push_back( tempSubResource );
         subResourceNameMap[ tempSubResource->getName() ] = static_cast<int>( subResource.size() ) - 1;
      }
   }
}

// *******************************************************************
// RenewableResource Class
// *******************************************************************
//! Returns the type of the Resource.
const string RenewableResource::getType() const {
   return "Renewable";
}

//! Returns the XML type of the Resource.
const string RenewableResource::getXMLType() const {
   return "renewresource";
}

//! Performs XML read-in that is specific to this derived class
/*! In this case, this read-in just substantiates the appropriate type of subResource */
void RenewableResource::XMLDerivedClassParse( const string nodeName, const DOMNode* node ) {
   SubResource* tempSubResource = 0;
   if( nodeName == "subresource" ){
      map<string,int>::const_iterator subResourceMapIter = subResourceNameMap.find( XMLHelper<string>::getAttrString( node, "name" ) );
      if( subResourceMapIter != subResourceNameMap.end() ) {
         // subResource already exists.
         subResource[ subResourceMapIter->second ]->XMLParse( node );
      }
      else {
         tempSubResource = new SubRenewableResource();
         tempSubResource->XMLParse( node );
         subResource.push_back( tempSubResource );
         subResourceNameMap[ tempSubResource->getName() ] = static_cast<int>( subResource.size() ) - 1;
      }
   }
}

