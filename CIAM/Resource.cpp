/* resource.cpp												*
 * Method definition for resource class						*
 * Coded by Sonny Kim 9/13/00								*/

#include "Definitions.h"

#include <string>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <ctime> // to use clock and time functions
#include <vector>
#include <cassert>

// xml headers
#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>

// class headers
#include "xmlHelper.h"
#include "modeltime.h"
#include "resource.h"
#include "market.h"
#include "Marketplace.h"

using namespace std; // enables elimination of std:

extern ofstream bugoutfile,outfile;	
extern Modeltime modeltime;
extern Marketplace marketplace;

//! Default constructor.
Resource::Resource(){
	nosubrsrc = 0;
}

//! Destructor.
Resource::~Resource() {

	for ( vector<subrsrc*>::iterator iter = depsubrsrc.begin(); iter != depsubrsrc.end(); iter++ ) {
		delete *iter;
	}
}

//! Clear data members.
void Resource::clear(){
	name = "";
	market = "";
	nosubrsrc = 0;
	rscprc.clear();
	depsubrsrc.clear();
	available.clear();
	annualprod.clear();
	cummprod.clear();
}

//! Set data members from XML input.
void Resource::XMLParse( const DOMNode* node ){
	
	string nodeName;
	DOMNodeList* nodeList = 0;
	DOMNode* curr = 0;
	subrsrc* tempSubResource = 0;

	// make sure we were passed a valid node.
	assert( node );
	
	// get the name attribute.
	name = XMLHelper<string>::getAttrString( node, "name" );
	
	#if( _DEBUG )
		cout << "\tResource name set as " << name << endl;
	#endif

	// get all child nodes.
	nodeList = node->getChildNodes();

	// loop through the child nodes.
	for( int i = 0; i < nodeList->getLength(); i++ ){
		curr = nodeList->item( i );
		nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

		if( nodeName == "market" ){
			market = XMLHelper<string>::getValueString( curr ); // only one market element.
		}
		else if( nodeName == "price" ){
			rscprc.push_back( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "subresource" ){
			tempSubResource = new subrsrc();
			tempSubResource->XMLParse( curr );
			depsubrsrc.push_back( tempSubResource );
		}
	}

	nosubrsrc = depsubrsrc.size();

	// resize vectors not read in
	int maxper = modeltime.getmaxper();
	available.resize(maxper); // total resource availabl
	annualprod.resize(maxper); // annual production rate of resource
	cummprod.resize(maxper); // cummulative production of resource
}

//! Write datamembers to datastream in XML format.
void Resource::toXML( ostream& out ) const {
	
	// write the beginning tag.
	Tabs::writeTabs( out );
	out << "<" << getType() << " name=\"" << name << "\">"<< endl;
	
	// increase the indent.
	Tabs::increaseIndent();

	// write the xml for the class members.
	// write out the market string.
	XMLWriteElement( market, "market", out );

	// write out resource prices for all periods
	for(int m = 0; m < static_cast<int>(rscprc.size()); m++ ) {
		XMLWriteElement( rscprc[m], "price", out, modeltime.getper_to_yr(m));
	}
		
	// write out the depresource objects.
	for( vector<subrsrc*>::const_iterator i = depsubrsrc.begin(); i != depsubrsrc.end(); i++ ){
		( *i )->toXML( out );
	}

	// finished writing xml for the class members.
	
	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</" << getType() << ">" << endl;

}

//! Write datamembers to datastream in XML format for debugging.
void Resource::toDebugXML( const int period, ostream& out ) const {
	
	// write the beginning tag.
	Tabs::writeTabs( out );
		out << "<" << getType() << " name=\"" << name << "\">"<< endl;
	
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
	
	// Write out the number of depletable resources.
	XMLWriteElement( nosubrsrc, "nosubrsrc", out );

	// Write out the depresource objects.
	for( vector<subrsrc*>::const_iterator i = depsubrsrc.begin(); i != depsubrsrc.end(); i++ ){
		( *i )->toDebugXML( period, out );
	}

	// finished writing xml for the class members.
	
	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</" << getType() << ">" << endl;
}

//! Create markets
void Resource::setMarket( const string& regionName )
{
	// marketplace is a global object
	// name is resource name
	if ( marketplace.setMarket( regionName, market, name, Market::NORMAL ) ) {
		marketplace.setPriceVector( name, regionName, rscprc );
                marketplace.setMarketToSolve (name, regionName);
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

void Resource::cummsupply(double prc,int per)
{	
	int i=0;
	cummprod[per]=0.0;

	rscprc[per] = prc;
	// sum cummulative production of each subsector
	for (i=0;i<nosubrsrc;i++) {
		depsubrsrc[i]->cummsupply(prc,per);
		cummprod[per] += depsubrsrc[i]->getCummProd(per);
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
       
	// sum annual production of each subsector
	for (i=0;i<nosubrsrc;i++) {
		depsubrsrc[i]->annualsupply(per,gnp,prev_gnp,price,prev_price);
		annualprod[per] += depsubrsrc[i]->getAnnualProd(per);
		available[per] += depsubrsrc[i]->getAvailable(per);
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

//! Return resource available from each subsectors.
double Resource::getSubAvail( const string& subResourceName, const int per ) {
	for (int i=0;i<nosubrsrc;i++) {
		if (depsubrsrc[i]->getName() == subResourceName )
			return depsubrsrc[i]->getAvailable(per);
	}
	return 0;
}

void Resource::show()
{
	int i=0;
	//write to file or database later
	cout << name << endl;
	cout << "Number of Subsectors: " << nosubrsrc <<"\n";
	for (i=0;i<nosubrsrc;i++)
		cout<<depsubrsrc[i]->getName()<<"\n";
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
	for (int i=0;i<nosubrsrc;i++)
		depsubrsrc[i]->outputfile(regname ,name);
}

//! Write resource output to database.
void Resource::MCoutput( const string& regname ) {
	int maxper = modeltime.getmaxper();
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
	for (int i=0;i<nosubrsrc;i++)
		depsubrsrc[i]->MCoutput(regname,name);
}

//! Returns the type of the Resource.
string DepletableResource::getType() const {
	return "Depletable";
}

//! Returns the type of the Resource.
string FixedResource::getType() const {
	return "Fixed";
}

//! Returns the type of the Resource.
string RenewableResource::getType() const {
	return "Renewable";
}

