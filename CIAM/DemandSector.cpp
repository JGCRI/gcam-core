/*! 
* \file DemandSector.cpp
* \ingroup CIAM
* \brief demsector class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <string>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cmath>
#include <cassert>

// xml headers
#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>
#include "xmlHelper.h"

#include "DemandSector.h"
#include "modeltime.h"
#include "Marketplace.h"
#include "summary.h"
#include "subsector.h"
#include "scenario.h"

using namespace std;

extern Scenario* scenario;
extern ofstream outfile, bugoutfile;

//! Default constructor
demsector::demsector() {
	perCapitaBased = 0;
	pElasticityBase = 0;
	priceRatio = 1;

	// resize vectors
   const Modeltime* modeltime = scenario->getModeltime();
	const int maxper = modeltime->getmaxper();
	sectorfuelprice.resize( maxper ); // total end-use sector service .
   fe_cons.resize( maxper ); // end-use sector final energy consumption.
	service.resize( maxper ); // total end-use sector service 
   iElasticity.resize( maxper );
   pElasticity.resize( maxper ); // price elasticity for each period
   aeei.resize( maxper );
   techChangeCumm.resize( maxper ); // cummulative technical change
}

//! Clear member variables.
void demsector::clear(){
	
	// call super clear
	sector::clear();
	
	// now clear own data.
	perCapitaBased = 0;
	pElasticityBase = 0;
	priceRatio = 1;
	fe_cons.clear();
	service.clear();
	iElasticity.clear();
	pElasticity.clear();
	techChangeCumm.clear();
}

//! Set data members from XML input.
void demsector::XMLParse( const DOMNode* node ){
	const Modeltime* modeltime = scenario->getModeltime();
	DOMNode* curr = 0;
	DOMNodeList* nodeList = 0;
	string nodeName;
	subsector* tempSubSector = 0;
	
	//! \pre Make sure we were passed a valid node.
	assert( node );
	
	// get the name attribute.
	name = XMLHelper<string>::getAttrString( node, "name" );

	// get the perCapitaBased attribute.
	perCapitaBased = XMLHelper<bool>::getAttr( node, "perCapitaBased" );

	#if( _DEBUG )
		cout << "\tSector name set as " << name << endl;
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
		else if( nodeName == "priceelasticity" ) {
         XMLHelper<double>::insertValueIntoVector( curr, pElasticity, modeltime );
		}
		else if( nodeName == "price" ){
         XMLHelper<double>::insertValueIntoVector( curr, sectorprice, modeltime );
		}
		else if( nodeName == "serviceoutput" ){
         XMLHelper<double>::insertValueIntoVector( curr, service, modeltime );
		}
		else if( nodeName == "energyconsumption" ){
         XMLHelper<double>::insertValueIntoVector( curr, fe_cons, modeltime );
		}
		else if( nodeName == "incomeelasticity" ){
         XMLHelper<double>::insertValueIntoVector( curr, iElasticity, modeltime );
		}
		else if( nodeName == "output" ) {
         XMLHelper<double>::insertValueIntoVector( curr, output, modeltime );
		}
		else if( nodeName == "aeei" ) {
         XMLHelper<double>::insertValueIntoVector( curr, aeei, modeltime );
		}
		else if( nodeName == "subsector" ){
			tempSubSector = new subsector();
			tempSubSector->XMLParse( curr );
			subsec.push_back( tempSubSector );
			
		}
	
	}
	
	nosubsec = subsec.size();
	pElasticityBase = pElasticity[ 0 ];
}

//! Write object to xml output stream.
void demsector::toXML( ostream& out ) const {
	const Modeltime* modeltime = scenario->getModeltime();
	int i = 0;

	// write the beginning tag.
	Tabs::writeTabs( out );
	out << "<demandsector name=\"" << name << "\">" << endl;
	
	// increase the indent.
	Tabs::increaseIndent();

	// write the xml for the class members.
	// write out the market string.
	XMLWriteElement( market, "market", out );
	XMLWriteElement( unit, "unit", out );
	XMLWriteElement( pElasticityBase, "pElasticityBase", out );
	
	for( i = 0; i < static_cast<int>( sectorprice.size() ); i++ ){
		XMLWriteElement( sectorprice[ i ], "price", out, modeltime->getper_to_yr( i ) );
	}
	
	for( i = 0; i < static_cast<int>( output.size() ); i++ ){
		XMLWriteElement( output[ i ], "output", out, modeltime->getper_to_yr( i ) );
	}

	for( i = 0; i < static_cast<int>( service.size() ); i++ ){
		XMLWriteElement( service[ i ], "serviceoutput", out, modeltime->getper_to_yr( i ) );
	}

	for( i = 0; i < static_cast<int>( fe_cons.size() ); i++ ){
		XMLWriteElement( fe_cons[ i ], "energyconsumption", out, modeltime->getper_to_yr( i ) );
	}
	
	for( i = 0; i < static_cast<int>( iElasticity.size() ); i++ ){
		XMLWriteElement( iElasticity[ i ], "incomeelasticity", out, modeltime->getper_to_yr( i ) );
	}

	for( i = 0; i < static_cast<int>( pElasticity.size() ); i++ ){
		XMLWriteElement( pElasticity[ i ], "priceelasticity", out, modeltime->getper_to_yr( i ) );
	}
	
	for( i = 0; i < static_cast<int>( aeei.size() ); i++ ){
		XMLWriteElement( aeei[ i ], "aeei", out, modeltime->getper_to_yr( i ) );
	}

	// does aeei need to be written?
	

	// write out the subsector objects.
	for( vector<subsector*>::const_iterator j = subsec.begin(); j != subsec.end(); j++ ){
		( *j )->toXML( out );
	}

	// finished writing xml for the class members.
	
	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</demandsector>" << endl;
}

//! Write object to debugging xml output stream.
void demsector::toDebugXML( const int period, ostream& out ) const {
	
	// write the beginning tag.
	Tabs::writeTabs( out );
	out << "<demandsector name=\"" << name << "\" perCapitaBased=\""
		<< perCapitaBased << "\">" << endl;
	
	// increase the indent.
	Tabs::increaseIndent();

	// write the xml for the class members.
	// write out the market string.
	XMLWriteElement( market, "market", out );
	XMLWriteElement( unit, "unit", out );
	XMLWriteElement( pElasticityBase, "pElasticityBase", out );
	XMLWriteElement( priceRatio, "priceRatio", out );

	// Write out the data in the vectors for the current period.
	// First write out inherited members.
	XMLWriteElement( sectorprice[ period ], "sectorprice", out );
	XMLWriteElement( pe_cons[ period ], "pe_cons", out );
	XMLWriteElement( input[ period ], "input", out );
	XMLWriteElement( output[ period ], "output", out );
	XMLWriteElement( carbontaxpaid[ period ], "carbontaxpaid", out );

	XMLWriteElement( sectorfuelprice[ period ], "sectorfuelprice", out );
	XMLWriteElement( techChangeCumm[ period ], "techChangeCumm", out );
	

	// Now write out own members.
	if ( period < fe_cons.size() ){
		XMLWriteElement( fe_cons[ period ], "fe_cons", out );
	}
	else {
		XMLWriteElement( 0, "fe_cons", out );
	}

	if ( period < service.size() ) {
		XMLWriteElement( service[ period ], "service", out );
	}
	else {
		XMLWriteElement( 0, "service", out );
	}

	if ( period < iElasticity.size() ) {
		XMLWriteElement( iElasticity[ period ], "iElasticity", out );
	}
	else {
		XMLWriteElement( 0, "iElasticity", out );
	}

	if ( period < pElasticity.size() ) {
		XMLWriteElement( pElasticity[ period ], "pElasticity", out );
	}
	else {
		XMLWriteElement( 0, "pElasticity", out );
	}
	if ( period < aeei.size() ) {
		XMLWriteElement( aeei[ period ], "aeei", out );
	}
	else {
		XMLWriteElement( 0, "aeei", out );
	}
	
	// Write out the summary
	// summary[ period ].toDebugXML( period, out );

	// write out the subsector objects.
	for( vector<subsector*>::const_iterator j = subsec.begin(); j != subsec.end(); j++ ){
		( *j )->toDebugXML( period, out );
	}

	// finished writing xml for the class members.
	
	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</demandsector>" << endl;
}

//! Create a market for the sector.
void demsector::setMarket( const string& regionName ) {
	Marketplace* marketplace = scenario->getMarketplace();

	if( marketplace->setMarket( regionName, market, name, Marketplace::NORMAL ) ) {
		marketplace->setPriceVector( name, regionName, sectorprice );
                /* The above initilaizes prices with any values that are read-in. 
                   This only affects the base period, which is not currently solved.
                   Any prices not initialized by read-in, are set by initXMLPrices(). */
  	}

	/* The above is not quite right becuase there could be many sectors within the same market, this would result 
	in the prices being reset each time.
	*/

	// The above problem has been fixed.
}

//! Calculate subsector shares.
void demsector::calc_share( const string regionName, const int per, const double gnp_cap )
{
	int i=0;
	double sum = 0.0;
	for (i=0;i<nosubsec;i++) {
		// determine subsector shares based on technology shares
		subsec[i]->calcShare( regionName, per, gnp_cap );
		sum += subsec[i]->getShare(per);
	}
	// normalize subsector shares to total 100 %
	for (i=0;i<nosubsec;i++)
		subsec[i]->normShare(sum, per);	
}

//! Calculate weighted average price of subsectors.
void demsector::price(int per)
{
	sectorprice[per]=0.0;
	for (int i=0;i<nosubsec;i++) {	
		if (per == 0) {
			// uses read in base year shares
			sectorprice[per] += subsec[i]->getShare(per) * subsec[i]->getPrice(per);
		}
		else {
			// uses previous period's actual shares
			sectorprice[per] += subsec[i]->getShare(per-1) * subsec[i]->getPrice(per);
		}
	}
}

//! Calculate end-use service price elasticity
void demsector::calc_pElasticity(int per)
{
	pElasticity[per]=0.0;
	sectorfuelprice[per] = 0;
	double tmpPriceRatio = 0; // ratio of total price to fuel price
	for (int i=0;i<nosubsec;i++) {
		sectorfuelprice[per] += subsec[i]->getwtfuelprice(per);
	}
	tmpPriceRatio = sectorprice[per]/sectorfuelprice[per];
	pElasticity[per] = pElasticityBase*tmpPriceRatio;
}



//! Aggrgate sector energy service demand function.
void demsector::aggdemand( const string& regionName, const double gnp_cap, const double gnp, const int per) {
	const Modeltime* modeltime = scenario->getModeltime();
	double ser_dmd, base, ser_dmd_adj;
	// double pelasticity = -0.9;
	double pelasticity = pElasticity[per];
	
	base = getoutput(0);

	// normalize prices to 1990 base
	int normPeriod = modeltime->getyr_to_per(1990);
	priceRatio = sectorprice[per]/sectorprice[normPeriod];
	//priceRatio = 1;

	// demand for service
	if (per == 0) {
		ser_dmd = base; // base output is initialized by data
		techChangeCumm[per] = 1; // base year technical change
	}
	else {
		// perCapitaBased is true or false
		if (perCapitaBased) { // demand based on per capita GNP
			//ser_dmd = base*pow(priceRatio,pElasticity[per])*pow(gnp_cap,iElasticity[per]);
			ser_dmd = base*pow(priceRatio,pelasticity)*pow(gnp_cap,iElasticity[per]);
			// need to multiply above by population ratio (current population/base year
			// population).  The gnp ratio provides the population ratio.
			ser_dmd *= gnp/gnp_cap;
		}
		else { // demand based on scale of GNP
			//ser_dmd = base*pow(priceRatio,pElasticity[per])*pow(gnp,iElasticity[per]);
			ser_dmd = base*pow(priceRatio,pelasticity)*pow(gnp,iElasticity[per]);
		}

		// calculate cummulative technical change using AEEI, autonomous end-use energy intensity
		techChangeCumm[per] = techChangeCumm[per-1]*pow(1+aeei[per],modeltime->gettimestep(per));
	}

	// adjust demand using cummulative technical change
	ser_dmd_adj = ser_dmd/techChangeCumm[per];
	// demand sector output is total end-use sector demand for service
	//service[per] = ser_dmd_adj; 
	service[per] = ser_dmd; 

	set_ser_dmd(ser_dmd_adj,per); // sets the output
	// sets subsector outputs, technology outputs, and market demands
	sector::setoutput(regionName,ser_dmd_adj,per);
	sector::sumoutput(per);
}

//! Write sector output to database.
void demsector::outputfile(const string& regionName ) {
	const Modeltime* modeltime = scenario->getModeltime();
	const int maxper = modeltime->getmaxper();
	int m=0;
	vector<double> temp(maxper);
	// function protocol
	void fileoutput3( string var1name,string var2name,string var3name,
				  string var4name,string var5name,string uname,vector<double> dout);
	
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total sector output
	for (m=0;m<maxper;m++)
		temp[m] = sector::getoutput(m);
	fileoutput3(regionName,getName()," "," ","prodution","SerUnit",temp);
	// total sector eneryg input
	for (m=0;m<maxper;m++)
		temp[m] = sector::showinput(m);
	fileoutput3(regionName,getName()," "," ","consumption","EJ",temp);
	// sector price
	for (m=0;m<maxper;m++)
		temp[m] = sector::showprice(m);
	fileoutput3(regionName,getName()," "," ","price","$/Service",temp);
	// sector carbon taxes paid
	for (m=0;m<maxper;m++)
		temp[m] = sector::showcarbontaxpaid(m);
	fileoutput3(regionName,getName()," "," ","C tax paid","Mil90$",temp);
	
}

//! Write MiniCAM style demand sector output to database.
void demsector::MCoutput( const string& regionName ) {
	const Modeltime* modeltime = scenario->getModeltime();
    int m;
	const int maxper = modeltime->getmaxper();
	vector<double> temp(maxper);
        
	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);
	
	const string secname = sector::getName();
	string str; // temporary string

	// total sector output
	dboutput4(regionName,"End-Use Service","by Sector",secname,"Ser Unit",service);
	dboutput4(regionName,"End-Use Service",secname,"zTotal","Ser Unit",temp);

	// End-use service price elasticity
	str = secname + "_price";
	dboutput4(regionName,"End-Use Service","Elasticity",str," ",pElasticity);
	str = secname + "_income";
	// End-use service income elasticity
	dboutput4(regionName,"End-Use Service","Elasticity",str," ",iElasticity);

	// sector fuel consumption by fuel type
	typedef map<string,double>:: const_iterator CI;
	map<string,double> tfuelmap = sector::getfuelcons(m=0);
	CI fmap; // define fmap
	// *** Either write out for each fuel here which is contained in the map
	// or write out in subsector
/*	for (fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
		for (m=0;m<maxper;m++) {
			temp[m] = sector::getfuelcons_second(m,fmap->first);
		}
		string strtemp = fmap->first;
		dboutput4(regionName,"Fuel Consumption",secname,fmap->first,"EJ",temp);

		// if last element which is zTotal
		// note: end() returns one past the last so use --
		// Total fuel consumption by end-use sector
		if (fmap == --tfuelmap.end()) {
			dboutput4(regionName,"Fuel Consumption","by End-Use Sector",secname,"EJ",temp);
		}
	}
*/
	// Write out total (zTotal) fuel consumption for each sector only
	fmap = --tfuelmap.end();
	for (m=0;m<maxper;m++) {
		temp[m] = sector::getfuelcons_second(m,fmap->first);
	}
	dboutput4(regionName,"Fuel Consumption",secname,fmap->first,"EJ",temp);
	dboutput4(regionName,"Fuel Consumption","by End-Use Sector",secname,"EJ",temp);
	// output for zTotal get written for each demand sector and dataviewer sums it up
	dboutput4(regionName,"Fuel Consumption","by End-Use Sector","zTotal","EJ",temp);


	// sector emissions for all greenhouse gases
	map<string,double> temissmap = summary[0].getemission(); // get gases for per 0
	for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
		for (int m=0;m<maxper;m++) {
			temp[m] = summary[m].get_emissmap_second(gmap->first);
		}
		str = "Sec: "; // sector heading
		str+= secname; // sector name
		dboutput4(regionName,"Emissions",str,gmap->first,"MTC",temp);
	}

	// CO2 emissions by sector
	for (m=0;m<maxper;m++) {
		temp[m] = summary[m].get_emissmap_second("CO2");
	}
	dboutput4(regionName,"CO2 Emiss","by Sector",secname,"MTC",temp);
	dboutput4(regionName,"CO2 Emiss",secname,"zTotal","MTC",temp);

	// CO2 indirect emissions by sector
	for (m=0;m<maxper;m++) {
		temp[m] = summary[m].get_emindmap_second("CO2");
	}
	dboutput4(regionName,"CO2 Emiss(ind)",secname,"zTotal","MTC",temp);

	// sector price (not normalized)
	for (m=0;m<maxper;m++) {
		temp[m] = sector::showprice(m);
	}
	dboutput4(regionName,"Price",secname,"zSectorAvg","75$/Ser",temp);
	
	// sector price normalized to base price
	for (m=0;m<maxper;m++) {
		temp[m] = sector::showprice(m)/sector::showprice(0);
	}
	dboutput4(regionName,"Price","by End-Use Sector",secname,"Norm75",temp);
	
	// sector carbon taxes paid
	for (m=0;m<maxper;m++) {
		temp[m] = sector::showcarbontaxpaid(m);
	}
	dboutput4(regionName,"General","CarbonTaxPaid",secname,"$",temp);
	
	// do for all subsectors in the sector
	MCoutput_subsec( regionName );
}

//! Return demand sector service
double demsector::getService(const int per) {
	return service[per];
}