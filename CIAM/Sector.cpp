/* sector.cpp												*
 * Method definition for sector class						*
 * Coded by Sonny Kim 7/13/00								*/

#include "Definitions.h"
#include <string>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cmath>
#include <cassert>

#include "market.h"
#include "modeltime.h"
#include "sector.h"
#include "Marketplace.h"

// xml headers
#include "xmlHelper.h"
#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>

using namespace std; // enables elimination of std::

extern Marketplace marketplace;
extern Modeltime modeltime;

extern ofstream outfile, bugoutfile;

//! Default constructor
sector::sector() {
	initElementalMembers();
}

//! Default destructor.
sector::~sector() {
	for( vector<subsector*>::iterator subSecIter = subsec.begin(); subSecIter != subsec.end(); subSecIter++ ) {
		delete *subSecIter;
	}
}

//! Clear member variables
void sector::clear(){
	initElementalMembers();
	name = "";
	unit = "";
	market = "";
	subsec.clear();
	sectorprice.clear();
	price_norm.clear();
	pe_cons.clear();
	input.clear();
	output.clear();
	carbontaxpaid.clear();
	summary.clear();
}

//! Initialize elemental data members.
void sector::initElementalMembers(){
	nosubsec = 0;
	tax = 0;
}

//! Return sector name.
string sector::getName()
{
	return name;
}

//! Set data members from XML input.
void sector::XMLParse( const DOMNode* node ){
	
	DOMNode* curr = 0;
	DOMNodeList* nodeList = 0;
	string nodeName;
	subsector* tempSubSector = 0;

	//! \pre make sure we were passed a valid node.
	assert( node );
	
	// get the name attribute.
	name = XMLHelper<string>::getAttrString( node, "name" );
	
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
		else if( nodeName == "price" ){
			sectorprice.push_back( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "output" ) {
			output.push_back( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "subsector" ){
			tempSubSector = new subsector(); // memory leak
			tempSubSector->XMLParse( curr );
			subsec.push_back( tempSubSector );
		}	
	}
	nosubsec = subsec.size();
	// resize vectors not read in
	int maxper = modeltime.getmaxper();
	price_norm.resize( maxper ); // sector price normalized to base year
	pe_cons.resize( maxper ); // sectoral primary energy consumption
	input.resize( maxper ); // sector total energy consumption
	// output.resize( maxper ); // total amount of final output from sector
	carbontaxpaid.resize( maxper ); // total sector carbon taxes paid
	summary.resize( maxper ); // object containing summaries // memory leak
}

//! Write object to xml output stream.
void sector::toXML( ostream& out ) const {
	
	// write the beginning tag.
	Tabs::writeTabs( out );
	out << "<supplysector name=\"" << name << "\">"<< endl;
	
	// increase the indent.
	Tabs::increaseIndent();

	// write the xml for the class members.
	// write out the market string.
	XMLWriteElement( market, "market", out );
	XMLWriteElement( unit, "unit", out );
	
	for( int i = 0; i < static_cast<int>( sectorprice.size() ); i++ ){
		XMLWriteElement( sectorprice[ i ], "price", out, modeltime.getper_to_yr( i ) );
	}
	
	for( int j = 0; j < static_cast<int>( output.size() ); j++ ){
		XMLWriteElement( output[ j ], "output", out, modeltime.getper_to_yr( j ) );
	}

	// write out the subsector objects.
	for( vector<subsector*>::const_iterator k = subsec.begin(); k != subsec.end(); k++ ){
		( *k )->toXML( out );
	}

	// finished writing xml for the class members.
	
	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</supplysector>" << endl;
}

//! Write object to xml output stream.
void sector::toDebugXML( const int period, ostream& out ) const {
	
	// write the beginning tag.
	Tabs::writeTabs( out );
	out << "<supplysector name=\"" << name << "\">"<< endl;
	
	// increase the indent.
	Tabs::increaseIndent();

	// write the xml for the class members.
	// write out the market string.
	XMLWriteElement( market, "market", out );
	XMLWriteElement( unit, "unit", out );
	
	// Write out the data in the vectors for the current period.
	XMLWriteElement( sectorprice[ period ], "sectorprice", out );
	XMLWriteElement( pe_cons[ period ], "pe_cons", out );
	XMLWriteElement( input[ period ], "input", out );
	XMLWriteElement( output[ period ], "output", out );
	XMLWriteElement( carbontaxpaid[ period ], "carbontaxpaid", out );
	
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
	out << "</supplysector>" << endl;
}

//! Create a market for the sector.
void sector::setMarket( const string& regionName )
{
	// marketplace is a global object
	// name is resource name
        // market is the name of the regional market from the input file (i.e., global, region, regional group, etc.)
	if( marketplace.setMarket( regionName, market, name, Market::NORMAL ) ) {
		marketplace.setPriceVector( name, regionName, sectorprice );
	}
	/* The above is not quite right becuase there could be many sectors within the 
           same market, this would result in the prices being reset each time. But little
           practical effect -- see notes for demsector::setMarket.
	*/
}

//! Pass along carbon taxes.
void sector::applycarbontax(double tax, int per)
{
	int i=0;
	for (i=0;i<nosubsec;i++) {
		subsec[i]->applycarbontax(tax,per);
	}
}

//! Set ghg tax to technologies.
void sector::addghgtax( const string ghgname, const string regionName, const int per)
{
	for (int i=0;i<nosubsec;i++) {
		subsec[i]->addghgtax(ghgname,regionName,per);
	}
}

//! Calculat subsector shares.
void sector::calc_share( const string regionName, const int per, const double gnp_cap )
{
	int i=0;
	double sum = 0.0;
	for (i=0;i<nosubsec;i++) {
		// determine subsector shares based on technology shares
		subsec[i]->calc_share( regionName, per );
		sum += subsec[i]->showshare(per);
	}
	// normalize subsector shares to total 100 %
	for (i=0;i<nosubsec;i++)
		subsec[i]->norm_share(sum, per);	
}

//! Calculate weighted average price of subsectors.
void sector::price(int per)
{
	sectorprice[per]=0.0;
	for (int i=0;i<nosubsec;i++) {	
		sectorprice[per] += subsec[i]->showshare(per) * subsec[i]->getprice(per);
	}
}

//! return sector price
double sector::showprice(int per)
{
	return sectorprice[per];
}

//! Set output for sector (ONLY USED FOR energy service demand at present).
/*! Demand from the "dmd" parameter (could be energy or energy service) is passed to subsectors.
    This is then shared out at the technology level.
        In the case of demand, what is passed here is the energy service demand. 
        The technologies convert this to an energy demand.
    The demand is then summed at the subsector level (subsec[i].sumoutput) then
    later at the sector level (in region via supplysector[j].sumoutput(per))
    to equal the total sector output.
*/
void sector::setoutput( const string& regionName,double dmd, int per)
{
    int i;
	carbontaxpaid[per] = 0; // initialize carbon taxes paid
 
	for (i=0;i<nosubsec;i++) {
		// set subsector output from sector demand
		subsec[i]->setoutput( regionName,dmd,per);
		subsec[i]->sumoutput(per);
		carbontaxpaid[per] += subsec[i]->showcarbontaxpaid(per);
	}
}

//! Sum subsector outputs.
void sector::sumoutput(int per)
{
	output[per] = 0;
	for (int i=0;i<nosubsec;i++) {
		output[per] += subsec[i]->getoutput(per);
	}
}

//! Set supply sector output.
/*! This routine takes the market demand and propagates that through the supply sub-sectors
    where it is shared out (and subsequently passed to the technology level within each sub-sector
     to be shared out) */
void sector::supply( const string regionName, const int per)
{
	double mrkprice, mrkdmd;
    int i;
    bool bug = true;
	double totalFixedSupply = 0;
    double fixedSupply = 0;
    double varShareTot = 0; // sum of shares without fixed supply   
    double Sharetotal = 0; // sum of shares without fixed supply   
           
	carbontaxpaid[per] = 0; // initialize carbon taxes paid
	
	mrkprice = marketplace.showprice( name, regionName, per ); // price for the good produced by this sector
	mrkdmd = marketplace.showdemand( name, regionName, per ); // demand for the good produced by this sector

    if (mrkdmd < 0) {
        cerr << "ERROR: Demand value < 0 for good " << name << " in region " << regionName << endl;
    }
            
	// calculate output from technologies that have fixed outputs such as hydro electric
    // Determine total fixed production and total var shares
    // Need to change the exog_supply function once new, general fixed supply method is available
	for (i=0;i<nosubsec;i++) {
        fixedSupply = subsec[i]->exog_supply(per);
        if (fixedSupply == 0) { 
			varShareTot += subsec[i]->showshare(per);
		}
        totalFixedSupply += fixedSupply;
        Sharetotal += subsec[i]->showshare(per);
	}

     // Adjust shares for any fixed output
	if (totalFixedSupply > 0) {
		for (i=0;i<nosubsec;i++) {
			subsec[i]->adjShares( mrkdmd, varShareTot, totalFixedSupply, per ); 
		}
	}
                
	for (i=0;i<nosubsec;i++) {
		// set subsector output from sector demand
		subsec[i]->setoutput( regionName, mrkdmd, per ); // CHANGED JPL
		subsec[i]->sumoutput( per );
		carbontaxpaid[per] += subsec[i]->showcarbontaxpaid( per );
	}
    
    if (bug) {
        sumoutput(per); // Sum output just so its available below
		double mrksupply = getoutput(per);
		if (per > 0 && abs(mrksupply - mrkdmd) > 0.01) {
			mrksupply = mrksupply * 1.0000001;
			cout << "Market supply and demand are not equal";
		}
	}
}

void sector::show()
{
	int i=0;
	int m=0; // per = 0
	//write to file or database later
	cout << "Sector: " << name<< endl;
	cout << "Number of Subsectors: " << nosubsec << endl;
	for (i=0;i<nosubsec;i++)
		cout<<"Share["<<i<<"] "<<subsec[i]->showshare(m)<< endl;
	cout <<"Total Sector Output: " << output[m] << endl;

}

//! Prints to outputfile all technologies in each subsector.
void sector::showsubsec(int per, const char *ofile)
{
	int i=0;

	for (i=0;i<nosubsec;i++) {
		// shows subsector label (name and index)
		subsec[i]->showlabel(ofile);
		// write technology info in each subsector to file
		subsec[i]->showtechs(per, ofile);
	}
}

//! Prints to outputfile sector label (name and index).
void sector::showlabel(const char* ofile)
{
	ofstream outfile;

	outfile.open(ofile, ios::app);

		if (!outfile) {
			//open failed
			cerr<<"Cannot open file for output\n";
			exit(-1);
		}

	outfile << "Sector: " << name << endl;
	outfile << "Number of Subsectors: " << nosubsec << endl;
	outfile.close();
}

int sector::shownosubsec(void)
{
	return nosubsec;
}

//! returns sector output
double sector::getoutput(int per)
{
	return output[per]; 
}


//! Calculate GHG emissions for each sector from subsectors.
void sector::emission(int per)
{
	summary[per].clearemiss(); // clear emissions map
	summary[per].clearemfuelmap(); // clear emissions fuel map
	for (int i=0;i<nosubsec;i++) {
		subsec[i]->emission(per,name);
		summary[per].updateemiss(subsec[i]->getemission(per));
		summary[per].updateemfuelmap(subsec[i]->getemfuelmap(per));
	}
}

//! Calculate indirect GHG emissions for each sector from subsectors.
void sector::indemission(int per)
{
	summary[per].clearemindmap(); // clear emissions map
	for (int i=0;i<nosubsec;i++) {
		subsec[i]->indemission(per);
		summary[per].updateemindmap(subsec[i]->getemindmap(per));
	}
}

//! Return sectoral primary energy consumption.
double sector::showpe_cons(int per)
{
	pe_cons[per] = 0;
	for (int i=0;i<nosubsec;i++) {
		pe_cons[per] += subsec[i]->showpe_cons(per);
	}
	return pe_cons[per];
}

//! Sums subsector primary and final energy consumption.
void sector::suminput(int per)
{
	input[per] = 0;
	for (int i=0;i<nosubsec;i++)
		input[per] += subsec[i]->showinput(per);
}

//! Returns sectoral energy consumption.
double sector::showinput(int per)
{
	return input[per];
}

//! Write sector output to database.
void sector::outputfile( const string& regname)
{
	// function protocol
	void fileoutput3(string var1name,string var2name,string var3name,
				  string var4name,string var5name,string uname,vector<double> dout);
	
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total sector output
	fileoutput3(regname,name," "," ","production","EJ",output);
	// total sector eneryg input
	fileoutput3( regname,name," "," ","consumption","EJ",input);
	// sector price
	fileoutput3( regname,name," "," ","price","$/GJ",sectorprice);
	// sector carbon taxes paid
	fileoutput3( regname,name," "," ","C tax paid","Mil90$",carbontaxpaid);
}

//! Write out subsector results from demand sector.
void sector::MCoutput_subsec( const string& regname )	
{	// do for all subsectors in the sector
	for (int i=0;i<nosubsec;i++) {
		// output or demand for each technology
		subsec[i]->MCoutputB(regname,name);
		subsec[i]->MCoutputC(regname,name);
	}
}

//! Write MiniCAM style sector output to database.
void sector::MCoutput(const string& regname )
{
	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);
	int m;
        
	// total sector output
	dboutput4(regname,"Secondary Energy Prod","by Sector",name,"EJ",output);
	dboutput4(regname,"Secondary Energy Prod",name,"zTotal","EJ",output);
	
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	string str; // temporary string

	// sector fuel consumption by fuel type
	typedef map<string,double>:: const_iterator CI;
	map<string,double> tfuelmap = summary[0].getfuelcons();
	for (CI fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
		for (int m=0;m<maxper;m++) {
			temp[m] = summary[m].get_fmap_second(fmap->first);
		}
		dboutput4(regname,"Fuel Consumption",name,fmap->first,"EJ",temp);
	}

	// sector emissions for all greenhouse gases
	map<string,double> temissmap = summary[0].getemission(); // get gases for per 0
	for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
		for (int m=0;m<maxper;m++) {
			temp[m] = summary[m].get_emissmap_second(gmap->first);
		}
		str = "Sec: "; // sector heading
		str+= name; // sector name
		dboutput4(regname,"Emissions",str,gmap->first,"MTC",temp);
	}
	// CO2 emissions by sector
	for (m=0;m<maxper;m++) {
		temp[m] = summary[m].get_emissmap_second("CO2");
	}
	dboutput4(regname,"CO2 Emiss","by Sector",name,"MTC",temp);
	dboutput4(regname,"CO2 Emiss",name,"zTotal","MTC",temp);

	// CO2 indirect emissions by sector
	for (m=0;m<maxper;m++) {
		temp[m] = summary[m].get_emindmap_second("CO2");
	}
	dboutput4(regname,"CO2 Emiss(ind)",name,"zTotal","MTC",temp);

	// sector price
	dboutput4(regname,"Price",name,"zSectorAvg","$/GJ",sectorprice);
	// sector price
	dboutput4(regname,"Price","by Sector",name,"$/GJ",sectorprice);
	// sector carbon taxes paid
	dboutput4(regname,"General","CarbonTaxPaid",name,"$",carbontaxpaid);
	// do for all subsectors in the sector
	for (int i=0;i<nosubsec;i++) {
		// output or demand for each technology
		subsec[i]->MCoutputA(regname,name);
		subsec[i]->MCoutputC(regname,name);
	}
}
	
//! Write subsector output to database.
void sector::subsec_outfile( const string& regname )
{
	// do for all subsectors in the sector
	for (int i=0;i<nosubsec;i++) {
		// output or demand for each technology
		subsec[i]->outputfile(regname,name);
	}
}

void sector::set_ser_dmd(double dmd, int per)
{
	output[per] = dmd;
}

//! Return total sector carbon taxes paid.
double sector::showcarbontaxpaid(int per)
{
	return carbontaxpaid[per];
}

//! Get the fuel consumption map in summary object.
map<string, double> sector::getfuelcons(int per) 
{
	return summary[per].getfuelcons();
}

//!  Get the second fuel consumption map in summary object.
double sector::getfuelcons_second(int per,string key) 
{
	return summary[per].get_fmap_second(key);
}

//! Clear fuel consumption map in summary object.
void sector::clearfuelcons(int per) 
{
	summary[per].clearfuelcons();
}

//!  Get the ghg emissions map in summary object.
map<string, double> sector::getemission(int per) 
{
	return summary[per].getemission();
}

//! Get ghg emissions map in summary object.
map<string, double> sector::getemfuelmap(int per) 
{
	return summary[per].getemfuelmap();
}

//! update summaries for reporting
void sector::updateSummary( const int per )
{
	int i = 0;
	// clears sector fuel consumption map
	summary[per].clearfuelcons();

	for (i=0;i<nosubsec;i++) {
		// clears subsector fuel consumption map
		subsec[i]->clearfuelcons(per);
		// call update summary for subsector
		subsec[i]->updateSummary(per);
		// sum subsector fuel consumption for sector fuel consumption
		summary[per].updatefuelcons(subsec[i]->getfuelcons(per)); 
	}

}


//**********************************
// demand sector method definitions
//**********************************

//! Default constructor
demsector::demsector() {
	perCapitaBased = 0;
	pElasticityBase = 0;
}

//! Clear member variables.
void demsector::clear(){
	
	// call super clear
	sector::clear();
	
	// now clear own data.
	perCapitaBased = 0;
	pElasticityBase = 0;
	fe_cons.clear();
	service.clear();
	iElasticity.clear();
	pElasticity.clear();
	techChangeCumm.clear();
}

//! Set data members from XML input.
void demsector::XMLParse( const DOMNode* node ){
	
	DOMNode* curr = 0;
	DOMNodeList* nodeList = 0;
	string nodeName;
	subsector* tempSubSector = 0;
	
	// resize vectors not read in
	int maxper = modeltime.getmaxper();
	pe_cons.resize( maxper ); // sectoral primary energy consumption
	input.resize( maxper ); // sector total energy consumption
	// output.resize( maxper ); // total amount of final output from sector
	pElasticity.resize( maxper ); // price elasticity for each period
	carbontaxpaid.resize( maxper ); // total sector carbon taxes paid
	summary.resize( maxper ); // object containing summaries // memory leak
	fe_cons.resize(maxper); // end-use sector final energy consumption
	service.resize(maxper); // total end-use sector service 
	sectorfuelprice.resize(maxper); // total end-use sector service 
	techChangeCumm.resize(maxper); // cummulative technical change
		
	
	//! \pre Make sure we were passed a valid node.
	assert( node );
	
	// get the name attribute.
	name = XMLHelper<string>::getAttrString( node, "name" );

	// get the perCapitaBased attribute.
	perCapitaBased = XMLHelper<int>::getAttr( node, "perCapitaBased" );

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
			int year = XMLHelper<int>::getAttr( curr, "year" );
			if (year == modeltime.getstartyr()) {
				pElasticityBase = XMLHelper<double>::getValue( curr );
			}
			int period = modeltime.getyr_to_per(year);
			pElasticity[period] =  XMLHelper<double>::getValue( curr );
			//pElasticity.push_back( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "price" ){
			sectorprice.push_back( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "serviceoutput" ){
			service.push_back( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "energyconsumption" ){
			fe_cons.push_back( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "incomeelasticity" ){
			iElasticity.push_back( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "output" ) {
			output.push_back( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "aeei" ) {
			aeei.push_back( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "subsector" ){
			tempSubSector = new subsector(); // memory leak.
			tempSubSector->XMLParse( curr );
			subsec.push_back( tempSubSector );
			
		}
	
	}
	
	nosubsec = subsec.size();
	
}

//! Write object to xml output stream.
void demsector::toXML( ostream& out ) const {
	
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
		XMLWriteElement( sectorprice[ i ], "price", out, modeltime.getper_to_yr( i ) );
	}
	
	for( i = 0; i < static_cast<int>( output.size() ); i++ ){
		XMLWriteElement( output[ i ], "output", out, modeltime.getper_to_yr( i ) );
	}

	for( i = 0; i < static_cast<int>( service.size() ); i++ ){
		XMLWriteElement( service[ i ], "serviceoutput", out, modeltime.getper_to_yr( i ) );
	}

	for( i = 0; i < static_cast<int>( fe_cons.size() ); i++ ){
		XMLWriteElement( fe_cons[ i ], "energyconsumption", out, modeltime.getper_to_yr( i ) );
	}
	
	for( i = 0; i < static_cast<int>( iElasticity.size() ); i++ ){
		XMLWriteElement( iElasticity[ i ], "incomeelasticity", out, modeltime.getper_to_yr( i ) );
	}

	for( i = 0; i < static_cast<int>( pElasticity.size() ); i++ ){
		XMLWriteElement( pElasticity[ i ], "priceelasticity", out, modeltime.getper_to_yr( i ) );
	}
	
	for( i = 0; i < static_cast<int>( aeei.size() ); i++ ){
		XMLWriteElement( aeei[ i ], "aeei", out, modeltime.getper_to_yr( i ) );
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
void demsector::setMarket( const string& regionName )
{
	// marketplace is a global object
	// name is resource name
	if( marketplace.setMarket( regionName, market, name, Market::NORMAL
         ) ) {
		marketplace.setPriceVector( name, regionName, sectorprice );
                /* The above initilaizes prices with any values that are read-in. 
                   This only affects the base period, which is not currently solved.
                   Any prices not initialized by read-in, are set by initXMLPrices(). */
  	}

	/* The above is not quite right becuase there could be many sectors within the same market, this would result 
	in the prices being reset each time.
	*/
}

//! Calculate subsector shares.
void demsector::calc_share( const string regionName, const int per, const double gnp_cap )
{
	int i=0;
	double sum = 0.0;
	for (i=0;i<nosubsec;i++) {
		// determine subsector shares based on technology shares
		subsec[i]->calc_share( regionName, per, gnp_cap );
		sum += subsec[i]->showshare(per);
	}
	// normalize subsector shares to total 100 %
	for (i=0;i<nosubsec;i++)
		subsec[i]->norm_share(sum, per);	
}

//! Calculate end-use service price elasticity
void demsector::calc_pElasticity(int per)
{
	pElasticity[per]=0.0;
	//double sectorfuelprice = 0; // using basesharewts, for p elasticity only
	sectorfuelprice[per] = 0;
	double priceRatio = 0; // ratio of total price to fuel price
	for (int i=0;i<nosubsec;i++) {
		sectorfuelprice[per] += subsec[i]->getwtfuelprice(per);
	}
	priceRatio = sectorprice[per]/sectorfuelprice[per];
	pElasticity[per] = pElasticityBase*priceRatio;
}



//! Aggrgate sector energy service demand function.
void demsector::aggdemand( const string& regionName, const double gnp_cap, const double gnp, const int per)
{
	double ser_dmd, base, ser_dmd_adj;
	// double pelasticity = -0.9;
	double pelasticity = pElasticity[per];
	
	base = getoutput(0);

	// normalize prices to 1990 base
	int normPeriod = modeltime.getyr_to_per(1990);
	//price_norm[per] = sectorprice[per]/sectorprice[normPeriod];
	//double priceRatio = price_norm[per];
	double priceRatio = sectorprice[per]/sectorprice[normPeriod];

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
		}
		else { // demand based on scale of GNP
			//ser_dmd = base*pow(priceRatio,pElasticity[per])*pow(gnp,iElasticity[per]);
			ser_dmd = base*pow(priceRatio,pelasticity)*pow(gnp,iElasticity[per]);
		}
		// calculate cummulative technical change using AEEI, autonomous end-use energy intensity
		techChangeCumm[per] = techChangeCumm[per-1]*pow(1+aeei[per],modeltime.gettimestep(per));
	}

	// adjust demand using cummulative technical change
	ser_dmd_adj = ser_dmd/techChangeCumm[per];
	// demand sector output is total end-use sector demand for service
	service[per] = ser_dmd_adj; 

	set_ser_dmd(ser_dmd_adj,per); // sets the output
	// sets subsector outputs, technology outputs, and market demands
	setoutput( regionName,ser_dmd_adj,per);
	sumoutput(per);
}

//! Write sector output to database.
void demsector::outputfile(const string& regionName )
{
	int maxper = modeltime.getmaxper();
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
void demsector::MCoutput( const string& regionName )
{
    int m;
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
        
	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);
	
	string secname = sector::getName();
	string str; // temporary string

	// total sector output
	for (m=0;m<maxper;m++) {
		temp[m] = sector::getoutput(m);
	}
	dboutput4(regionName,"End-Use Service","by Sector",secname,"Ser Unit",temp);
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
	for (CI fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
		for (m=0;m<maxper;m++) {
			temp[m] = sector::getfuelcons_second(m,fmap->first);
		}
		dboutput4(regionName,"Fuel Consumption",secname,fmap->first,"EJ",temp);
	}

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

	// sector price
	for (m=0;m<maxper;m++) {
		temp[m] = sector::showprice(m);
	}
	dboutput4(regionName,"Price",secname,"zSectorAvg","$/Ser",temp);
	dboutput4(regionName,"Price","by End-Use Sector",secname,"$/Ser",temp);
	// sector carbon taxes paid
	for (m=0;m<maxper;m++) {
		temp[m] = sector::showcarbontaxpaid(m);
	}
	dboutput4(regionName,"General","CarbonTaxPaid",secname,"$",temp);
	// do for all subsectors in the sector
	MCoutput_subsec( regionName );
}
	
