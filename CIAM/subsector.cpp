/*! 
* \file subsector.cpp
* \ingroup CIAM
* \brief subsector class source file.
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
#include <vector>
#include "scenario.h"
#include "market.h"
#include "modeltime.h"
#include "subsector.h"
#include "xmlHelper.h"
#include "marketplace.h"

using namespace std;

extern ofstream outfile;	
extern Scenario scenario;
	
 //! Default constructor
subsector::subsector(){
	notech = 0;
	tax = 0;
	basesharewt = 0;
}

//! Destructor.
subsector::~subsector() {

	for ( vector< vector< technology* > >::iterator outerIter = techs.begin(); outerIter != techs.end(); outerIter++ ) {
		for( vector< technology* >::iterator innerIter = outerIter->begin(); innerIter != outerIter->end(); innerIter++ ) {
			delete *innerIter;
		}
	}
}

//! Clear the subsector member variables.
void subsector::clear(){
	notech = 0;
	tax = 0;
	basesharewt = 0;
	name = "";
	unit = "";
	fueltype = "";

	// clear the vectors.
	techs.clear();
	hydro.clear();
	shrwts.clear();
	lexp.clear();
	fuelPrefElasticity.clear();
	share.clear();
	input.clear();
	pe_cons.clear();
	subsectorprice.clear();
	output.clear();
	carbontaxpaid.clear();
	summary.clear();
}

//! Return the sector name.
const string subsector::getName() const {
	return name;
}


//! Initialize subsector with xml data
void subsector::XMLParse( const DOMNode* node ) {	
	DOMNodeList* nodeList = 0;
	DOMNodeList* childNodeList = 0;
	DOMNode* curr = 0;
	DOMNode* currChild = 0;
	string nodeName;
	string childNodeName;
	vector<technology*> techVec;
	technology* tempTech = 0;
        
	// resize vectors not read in, therefore not sized by XML input
	const Modeltime* modeltime = scenario.getModeltime();
	const int maxper = modeltime->getmaxper();
	share.resize(maxper); // subsector shares
	input.resize(maxper); // subsector energy input
	pe_cons.resize(maxper); // subsector primary energy consumption
	subsectorprice.resize(maxper); // subsector price for all periods
	fuelprice.resize(maxper); // subsector fuel price for all periods
	output.resize(maxper); // total amount of final output from subsector
	carbontaxpaid.resize(maxper); // total subsector carbon taxes paid
	summary.resize(maxper); // object containing summaries

	//! \pre Make sure we were passed a valid node.
	assert( node );
	
	// get the name attribute.
	name = XMLHelper<string>::getAttrString( node, "name" );

	#if( _DEBUG )
		// cout << "\t\tSubSector name set as " << name << endl;
	#endif

	// get all child nodes.
	nodeList = node->getChildNodes();

	// loop through the child nodes.
	for( int i = 0; i < nodeList->getLength(); i++ ){
		curr = nodeList->item( i );
		nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
		
		if( nodeName == "capacitylimit" ){
			capLimit.push_back( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "sharewt" ){
			if(name=="windxxx") { // Change this to "wind" to turn wind shares down
				shrwts.push_back(0.001);
			}
			else {
				shrwts.push_back( XMLHelper<double>::getValue( curr ) );
			}
		}

		else if( nodeName == "logitexp" ){
			lexp.push_back( XMLHelper<double>::getValue( curr ) );
		}

		else if( nodeName == "fuelprefElasticity" ){
			fuelPrefElasticity.push_back( XMLHelper<double>::getValue( curr ) );
		}

		// basesharewt is not a vector but a single value
		else if( nodeName == "basesharewt" ){
			basesharewt = XMLHelper<double>::getValue( curr );
			share[0] = basesharewt;
		}

		else if( nodeName == "technology" ){
			childNodeList = curr->getChildNodes();
			
			// loop through technologies children.
			for( int j = 0; j < childNodeList->getLength(); j++ ){
				
				currChild = childNodeList->item( j );
				childNodeName = XMLHelper<string>::safeTranscode( currChild->getNodeName() );
				
				if( childNodeName == "period" ){
					tempTech = new technology();
					tempTech->XMLParse( currChild );
					techVec.push_back( tempTech );
				}
			}
			techs.push_back( techVec );
			techVec.clear();
		}
	}
	// completed parsing.

    // Initialzie any arrays that have non-zero default value
    capLimit.resize(maxper,1.0); 
	
	notech = techs.size();

}

//! Output the subsector member variables in XML format.
void subsector::toXML( ostream& out ) const {
	const Modeltime* modeltime = scenario.getModeltime();
	int i;

	// write the beginning tag.
	Tabs::writeTabs( out );
	out << "<subsector name=\"" << name << "\">"<< endl;
	
	// increase the indent.
	Tabs::increaseIndent();

	// write the xml for the class members.
	for( i = 0; i < static_cast<int>( capLimit.size() ); i++ ){
		XMLWriteElement( capLimit[ i ], "capacitylimit", out, modeltime->getper_to_yr( i ) );
	}
	
	for( i = 0; i < static_cast<int>( shrwts.size() ); i++ ){
		XMLWriteElement( shrwts[ i ], "sharewt", out, modeltime->getper_to_yr( i ) );
	}
	
	for( i = 0; i < static_cast<int>( lexp.size() ); i++ ){
		XMLWriteElement( lexp[ i ], "logitexp", out, modeltime->getper_to_yr( i ) );
	}

	for( i = 0; i < static_cast<int>( fuelPrefElasticity.size() ); i++ ){
		XMLWriteElement( fuelPrefElasticity[ i ], "fuelPrefElasticity", out, modeltime->getper_to_yr( i ) );
	}

	XMLWriteElement( basesharewt, "basesharewt", out, modeltime->getstartyr( ) );

	// write out the technology objects.
	for( vector< vector< technology* > >::const_iterator j = techs.begin(); j != techs.end(); j++ ){
		Tabs::writeTabs( out );
		out << "<technology>" << endl;
		
		Tabs::increaseIndent();

		for( vector<technology*>::const_iterator k = j->begin(); k != j->end(); k++ ){
			( *k )->toXML( out );
		}

		Tabs::decreaseIndent();

		Tabs::writeTabs( out );
		out << "</technology>" << endl;
	}

	// finished writing xml for the class members.
	
	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</subsector>" << endl;
}

//! Write out the state of all member variables to the debug xml output stream.
void subsector::toDebugXML( const int period, ostream& out ) const {
	
	// write the beginning tag.
	Tabs::writeTabs( out );
	out << "<subsector name=\"" << name << "\">"<< endl;
	
	// increase the indent.
	Tabs::increaseIndent();
	
	// Write the xml for the class members.
	XMLWriteElement( unit, "unit", out );
	XMLWriteElement( fueltype, "fueltype", out );
	XMLWriteElement( notech, "notech", out );
	XMLWriteElement( tax, "tax", out );
	
	// Write the data for the current period within the vector.
	XMLWriteElement( capLimit[ period ], "capLimit", out );
	XMLWriteElement( shrwts[ period ], "sharewts", out );
	XMLWriteElement( lexp[ period ], "lexp", out );
	if ( ! fuelPrefElasticity.empty() ) {
		XMLWriteElement( fuelPrefElasticity[ period ], "fuelprefElasticity", out );
	}
	XMLWriteElement( share[ period ], "share", out );
	XMLWriteElement( basesharewt, "basesharewt", out );
	XMLWriteElement( input[ period ], "input", out );
	XMLWriteElement( pe_cons[ period ], "pe_cons", out );
	XMLWriteElement( subsectorprice[ period ], "subsectorprice", out );
	XMLWriteElement( output[ period ], "output", out );
	XMLWriteElement( carbontaxpaid[ period ], "carbontaxpaid", out );
	
	// Write out the summary object.
	// summary[ period ].toDebugXML( period, out );
	// write out the technology objects.

	for( int j = 0; j < static_cast<int>( techs.size() ); j++ ){
		techs[ j ][ period ]->toDebugXML( period, out );
	}
	
	// write out the hydrotech. Not yet implemented
	// hydro[ period ].toDebugXML( period, out );

	// finished writing xml for the class members.
	
	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</subsector>" << endl;
}

//! Set the share weight and logit exponentional for the current period to the values from the last period.
void subsector::copytolast( const int period ) {
	shrwts[ period ] = shrwts[ period - 1 ];
	lexp[ period ] = lexp[ period - 1 ];
}

//! Computes weighted cost of all technologies in subsector
/*! price function called below in calc_share after technology shares are determined.
  price function separated to allow different weighting for subsector price
	changed to void return maw */
void subsector::calc_price( const string regionName, const int per ) {
	int i=0;
	subsectorprice[per] = 0; // initialize to 0 for summing
	fuelprice[per] = 0; // initialize to 0 for summing

	for (i=0;i<notech;i++) {
		// calculate weighted average price for subsector
		subsectorprice[per] += techs[i][per]->getShare()*
			techs[i][per]->gettechcost();
		// calculate weighted average price of fuel only
		// technology shares are based on total cost
		fuelprice[per] += techs[i][per]->getShare()*
			techs[i][per]->getfuelcost();
	}
}

//! returns subsector price 
double subsector::getprice( const int period ) const {
	return subsectorprice[ period ];
}

//! returns subsector capacity limit 
double subsector::getCapacityLimit( const int period ) const {
	return capLimit[ period ];
}

//! returns subsector fuel price 
double subsector::getfuelprice(int per) const
{
	return fuelprice[per];
}

//! returns subsector base share weighted fuel price 
double subsector::getwtfuelprice(int per) const
{
    if (per == 0) {
        //return basesharewt*fuelprice[per];
        return share[per]*fuelprice[per]; // base year share initialized to basesharewt
    }
    else {
        return share[per-1]*fuelprice[per];
    }
}

//! passes carbon tax to technology
void subsector::applycarbontax( const double tax, const int period ) {
	for (int i=0;i<notech;i++) {
		techs[i][ period ]->applycarbontax(tax);
	}
}

//! sets ghg tax to technologies
void subsector::addghgtax( const string& ghgname, const string& regionName, const int per ) {
	for (int i=0;i<notech;i++) {
		techs[i][per]->addghgtax( ghgname, regionName, per );
	}
}


// maw  calculate technology shares within subsector
void subsector::calcTechShares( const string& regionName, const int per ) {
	int i=0;
	double sum = 0;

	for (i=0;i<notech;i++) {
		// calculate technology cost
		techs[i][per]->cost( regionName, per );
		// determine shares based on technology costs
		techs[i][per]->calc_share( regionName,per );
		sum += techs[i][per]->getShare();
	}
	// normalize technology shares to total 100 %
	for (i=0;i<notech;i++) {
		techs[i][per]->norm_share(sum);
		// Logit exponential should not be zero or positive when more than
		// one technology
		if(notech>1 && techs[i][per]->getlexp()>=0) cerr << "Tech Logit Exponential is invalid." << endl;
	}
}	
	

//! calculate subsector share numerator 
void subsector::calcShare( const string& regionName, const int per, const double gnp_cap )
{
	// call function to compute technology shares
	subsector::calcTechShares(regionName, per);

	// calculate and return subsector share; uses above price function
	// calc_price() uses normalized technology shares calculated above
	// Logit exponential should not be zero

	// compute subsector weighted average price of technologies
	subsector::calc_price( regionName,per);
	
	// subsector logit exponential
	if(lexp[per]==0) cerr << "SubSec Logit Exponential is 0." << endl;

	if(subsectorprice[per]==0) {
		share[per] = 0;
	}
	else {
		if (fuelPrefElasticity.empty()) { // supply subsector
			share[per] = shrwts[per]*pow(subsectorprice[per],lexp[per]);
		}
		else { // demand subsector
			share[per] = shrwts[per]*pow(subsectorprice[per],lexp[per])*pow(gnp_cap,fuelPrefElasticity[per]);
		}

	}
}

//! normalizes shares to 100%
void subsector::normShare( const double sum, const int per) {
	if ( sum==0 ) {
		share[per]=0;
	}
	else {
		share[per] /= sum;
	}
}

/*!
 * \brief normalizes shares to 100% subject to capacity limit.
 *
 * Used by sector::calc_share() to re-normalize shares, adjusting for capacity limits.
 *
 * Note that a multiplier is passed, not a divisor. The appropriate multiplier must be calculated by the calling routine.
 *
 * Sub-sectors that are not subject to a capacity limit get multiplied by mult.
 * Capacity limited subsectors are set to their capacity limit.
 *
 * \author Steve Smith
 * \warning The routine assumes that shares are already normalized.
 * \param mult Multiplier by which to scale shares of all non-capacity limited sub-sectors
 * \param per Model period
 */
void subsector::limitShares( const double multiplier, const int per) {
    if ( multiplier == 0 ) {
	share[per] = 0;
	}
    else {		
        if ( share[per] >= capLimit[per]) {
                share[per] = capLimit[per];
        } else {
                share[per] *= multiplier;
        }
    }
}

//! call technology production, only exogenously driven technology gives an output
/*! Since the calls below set output, this call must be done before
    calls to technology production with non-zero demand . g*/
double subsector::exogSupply( const int per ) {
	double fixedOutput = 0;
	for (int i=0;i<notech;i++) {
		techs[i][per]->calcFixedSupply(per);
		fixedOutput += techs[i][per]->getFixedSupply();
	}
	return fixedOutput;
}

//! Scale down fixed supply if the total fixed production is greater than the actual demand 
void subsector::scaleFixedSupply( const double scaleRatio, const int per ) {
	// scale fixed technology output down
	for (int i=0;i<notech;i++) {
		techs[i][per]->scaleFixedSupply(scaleRatio);
	}
}

//! Adjusts shares to be consistant with any fixed production 
// total demand for all sectors
// sum of sector shares that have no fixed production
// total fixed supply from all sectors
// model period
void subsector::adjShares( const double dmd, double shareRatio, 
                          const double totalFixedSupply, const int per) {
	double sumFixedSupply = 0; // total subsector fixed supply
	double fixedSupply = 0; // fixed supply for each technology
	double varShareTot = 0; // sum of shares without fixed supply
	double subsecdmd; // subsector demand adjusted with new shares
	// add up the fixed supply and share of non-fixed supply
	for (int i=0;i<notech;i++) {
		fixedSupply = techs[i][per]->getFixedSupply();
		sumFixedSupply += fixedSupply;
		if (fixedSupply == 0) varShareTot += techs[i][per]->getShare();
	}
	
	// Adjust the share for this subsector
	// This makes the implicit assumption that the subsector is either all
	// fixed production or all variable. Would need to amend the logic below
	// to take care of other cases.
	
    // totalFixedSupply is the sector total
	if(totalFixedSupply > 0) {
		if (sumFixedSupply > 0) {	// This subsector has a fixed supply
			if (dmd > 0) {
				share[per] = sumFixedSupply/dmd; 
			}
            else { // no fixed share if no demand
				share[per] = 0; 
			}
		}
		else {	// This subsector does not have fixed supply
			if (dmd > 0) {
				share[per] = share[per] * shareRatio; 
			}
			else {
				share[per] = 0; // Maybe not correct
			}  
		} 
	}
	
	// then adjust technology shares to be consistent
	subsecdmd = share[per]*dmd; // share is subsector level
	for (int j=0;j<notech;j++) {
		// adjust tech shares 
		techs[j][per]->adjShares(subsecdmd, fixedSupply, varShareTot, per);
	}
}

//! sets demand to output and output
/* Demand from the "dmd" parameter (could be energy or energy service) is passed to technologies.
    This is then shared out at the technology level.
    See explanation for sector::setoutput. */
void subsector::setoutput( const string& regionName, const string& prodName, const double dmd, const int per) {
	int i=0;
	input[per] = 0; // initialize subsector total fuel input 
	carbontaxpaid[per] = 0; // initialize subsector total carbon taxes paid 

	// output is in service unit when called from demand sectors
	double subsecdmd = share[per]*dmd; // share is subsector level
	for (i=0;i<notech;i++) {
		// calculate technology output and fuel input from subsector output
		techs[i][per]->production(regionName,prodName,subsecdmd,per);

		// total energy input into subsector, must call after tech production
		input[per] += techs[i][per]->showinput();
		// sum total carbon tax paid for subsector
		carbontaxpaid[per] += techs[i][per]->showcarbontaxpaid();
	}
}
  
//! calculates fuel input and subsector output
void subsector::sumoutput( const int per ) {
	output[per] = 0;
	for (int i=0;i<notech;i++) {
		output[per] += techs[i][per]->showoutput();
	}
}

/*
//! set supply subsector total output and technology output
double subsector::supply( const string& regionName, const int per ) {	
	double dmd=0;

	dmd = marketplace->showdemand( name, regionName, per );

	// this demand does not account for simultaneity

	// this function pertains to the same subsector
	setoutput( regionName, name, dmd, per);
	sumoutput(per);

	return dmd;
}
*/
//! write subsector info to screen
void subsector::show_subsec() const {
	int i=0;
	int m=0; // temp period
	//write to file or database later
	cout << name << endl;
	cout << "Number of Technologies: " << notech << endl;
	for (i=0;i<notech;i++)
		cout<<"Share["<<i<<"] "<<techs[i][m]->getShare()<< endl;
	cout <<"Total subsector Output: " << output[m] << endl;
}
//! returns share for each subsector
double subsector::getShare( const int per ) const {
	return share[per];
}

//! prints to outputfile all technologies in subsector by period
void subsector::showtechs( const int per, const string ofile ) const {
	int i=0;

	for (i=0;i<notech;i++) {
		// use technology class method show_tech
		techs[i][per]->printTech(ofile);
	}
}

//! prints to outputfile all subsector labels (name and index)
void subsector::showlabel( const string& ofile ) const {
	ofstream outfile;

	outfile.open(ofile.c_str(), ios::app);

		if (!outfile) {
			//open failed
			cerr<<"Cannot open file for output\n";
			exit(-1);
		}

	outfile << "Subsector: " << name <<  endl;
	outfile << "Number of Technologies: " << notech <<  endl;
	outfile.close();
}

//! write subsector output to database
void subsector::outputfile( const string& regname, const string& secname ) const {
	// function protocol
	void fileoutput3( string var1name,string var2name,string var3name,
				  string var4name,string var5name,string uname,vector<double> dout);

	int i=0, m=0;
	int mm=0; // temp period
	const Modeltime* modeltime = scenario.getModeltime();
	const int maxper = modeltime->getmaxper();
	vector<double> temp(maxper);
	
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total subsector output
	fileoutput3( regname,secname,name," ","production","EJ",output);
	// subsector price
	fileoutput3( regname,secname,name," ","price","$/GJ(ser)",subsectorprice);
	for (m=0;m<maxper;m++)
		temp[m] = summary[m].get_emissmap_second("CO2");
	fileoutput3( regname,secname,name," ","CO2 emiss","MTC",temp);
	// subsector carbon taxes paid
	fileoutput3( regname,secname,name," ","C tax paid","Mil90$",carbontaxpaid);
	
	// do for all technologies in the subsector
	for (i=0;i<notech;i++) {
		// output or demand for each technology
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m]->showoutput();
		fileoutput3( regname,secname,name,techs[i][mm]->showname(),"production","EJ",temp);
		// technology share
		if(notech>1) {
			for (m=0;m<maxper;m++)
				temp[m] = techs[i][m]->getShare();
			fileoutput3( regname,secname,name,techs[i][mm]->showname(),"tech share","%",temp);
		}
		// technology cost
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m]->gettechcost();
		fileoutput3( regname,secname,name,techs[i][mm]->showname(),"price","$/GJ",temp);
		// ghg tax applied to technology
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m]->showcarbontax();
		fileoutput3( regname,secname,name,techs[i][mm]->showname(),"C tax","$/TC",temp);
		// ghg tax paid
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m]->showcarbontaxpaid();
		fileoutput3( regname,secname,name,techs[i][mm]->showname(),"C tax paid","90Mil$",temp);
		// technology fuel input
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m]->showinput();
		fileoutput3( regname,secname,name,techs[i][mm]->showname(),"fuel consump","EJ",temp);
		// technology efficiency
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m]->showeff();
		fileoutput3( regname,secname,name,techs[i][mm]->showname(),"efficiency","%",temp);
		// technology non-energy cost
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m]->getnecost();
		fileoutput3( regname,secname,name,techs[i][mm]->showname(),"non-energy cost","$/GJ",temp);
		// technology CO2 emission
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m]->get_emissmap_second("CO2");
		fileoutput3( regname,secname,name,techs[i][mm]->showname(),"CO2 emiss","MTC",temp);
		// technology indirect CO2 emission
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m]->get_emissmap_second("CO2ind");
		fileoutput3( regname,secname,name,techs[i][mm]->showname(),"CO2 emiss(ind)","MTC",temp);
	}
}

//! write MiniCAM style subsector output to database
/*! Part A for supply sector, titles and units are different for Part B */
void subsector::MCoutputA( const string& regname, const string& secname ) const {
	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);

	int i=0, m=0;
	int mm=0; // temp period
	const Modeltime* modeltime = scenario.getModeltime();
	const int maxper = modeltime->getmaxper();
	const double cvrt90 = 2.212; //  convert '75 price to '90 price
	vector<double> temp(maxper);
	
	// total subsector output
	dboutput4(regname,"Secondary Energy Prod",secname,name,"EJ",output);
	// subsector price
	dboutput4(regname,"Price",secname,name,"75$/GJ",subsectorprice);

	string tssname = "tech_"; // tempory subsector name
	string str1, str2; // tempory string
	// do for all technologies in the subsector
	for (i=0;i<notech;i++) {
		str1 = secname;
		str1 += "_tech";
		str2 = techs[i][mm]->showname();
		// technology non-energy cost
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m]->getnecost();
		dboutput4(regname,"Price NE Cost",secname,str2,"75$/GJ",temp);
		// secondary energy and price output by tech
		// output or demand for each technology
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m]->showoutput();
		dboutput4(regname,"Secondary Energy Prod",str1,str2,"EJ",temp);
		// technology cost
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m]->gettechcost()*cvrt90;
		dboutput4(regname,"Price",str1,str2,"90$/GJ",temp);
	}
}

//! write MiniCAM style subsector output to database
/*! Part B for demand sector, titles and units are different from Part A */
void subsector::MCoutputB( const string& regname, const string& secname ) const {
	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);
	const Modeltime* modeltime = scenario.getModeltime();
	int i=0, m=0;
	int mm=0; // temp period
	const int maxper = modeltime->getmaxper();
	vector<double> temp(maxper);
	
	// total subsector output
	dboutput4(regname,"End-Use Service",secname,name,"Ser Unit",output);
	// subsector price
	dboutput4(regname,"Price",secname,name,"75$/Ser",subsectorprice);

	string tssname = "tech_"; // tempory subsector name
	string str; // tempory string
	// do for all technologies in the subsector
	for (i=0;i<notech;i++) {
		//str = tssname + techs[i][mm].showname();
		str = techs[i][mm]->showname();
		if(notech>1) {  // write out if more than one technology
			// output or demand for each technology
			for (m=0;m<maxper;m++)
				temp[m] = techs[i][m]->showoutput();
			dboutput4(regname,"End-Use Service",secname,str,"Ser Unit",temp);
			// technology cost
			for (m=0;m<maxper;m++)
				temp[m] = techs[i][m]->gettechcost();
			dboutput4(regname,"Price",secname,str,"75$/Ser",temp);
		}
		// technology fuel cost
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m]->getfuelcost();
		dboutput4(regname,"Price",secname+" Fuel Cost",str,"75$/Ser",temp);
		// technology non-energy cost
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m]->getnecost();
		dboutput4(regname,"Price",secname+" NE Cost",str,"75$/Ser",temp);
	}
}


//! write MiniCAM style subsector output to database
void subsector::MCoutputC( const string& regname, const string& secname ) const {
	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);

	int i=0, m=0;
	const Modeltime* modeltime = scenario.getModeltime();
	const int maxper = modeltime->getmaxper();
	vector<double> temp(maxper);
	string str; // tempory string
	
	//for (m=0;m<maxper;m++)
	//	temp[m] = summary[m].get_emissmap_second("CO2");
	//dboutput4(regname,"CO2 Emiss",secname,name,"MTC",temp);
	// subsector carbon taxes paid
	dboutput4(regname,"General","CarbonTaxPaid",name,"$",carbontaxpaid);
	// subsector share 
	dboutput4(regname,"Subsec Share",secname,name,"100%",share);
	// subsector emissions for all greenhouse gases
	typedef map<string,double>:: const_iterator CI;
	map<string,double> temissmap = summary[0].getemission(); // get gases for per 0
	for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
		for (m=0;m<maxper;m++) {
			temp[m] = summary[m].get_emissmap_second(gmap->first);
		}
		str = "Subsec: "; // subsector heading
		str+= secname; // sector name
		str+= "_";
		str+= name; // subsector name
		dboutput4(regname,"Emissions",str,gmap->first,"MTC",temp);
	}

	//string tssname = name; // tempory subsector name
	string tssname = "tech_"; // tempory subsector name
	int mm=0; // temp period to get base period
	// do for all technologies in the subsector
	for (i=0;i<notech;i++) {
		//str = tssname + techs[i][mm].showname();
		str = techs[i][mm]->showname();
//		if(notech>1) {  // write out if more than one technology
		if(notech>0) {  // write out if more than one technology
			// technology CO2 emission
			for (m=0;m<maxper;m++) {
				// this gives subsector total CO2 emissions
				//temp[m] = summary[m].get_emissmap_second("CO2");
				// get CO2 emissions for each technology
				temp[m] = techs[i][m]->get_emissmap_second("CO2");
			}
			dboutput4(regname,"CO2 Emiss",secname,str,"MTC",temp);
			// technology indirect CO2 emission
			for (m=0;m<maxper;m++)
				temp[m] = summary[m].get_emindmap_second("CO2");
			dboutput4(regname,"CO2 Emiss(ind)",secname,str,"MTC",temp);
			// technology ghg emissions, get gases for per 
			// all gases not just CO2
			map<string,double> temissmap = techs[i][0]->getemissmap();
			for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
				for (m=0;m<maxper;m++) {
					temp[m] = techs[i][m]->get_emissmap_second(gmap->first);
				}
				str = "Tech: "; // subsector heading
				str += secname; // sector name
				str += "_";
				str += name; // subsector name
				str += "_";
				str += techs[i][mm]->showname(); // technology name
				dboutput4(regname,"Emissions",str,gmap->first,"MTC",temp);
			}
			// technology share
			for (m=0;m<maxper;m++)
				temp[m] = techs[i][m]->getShare();
			dboutput4(regname,"Tech Share",secname,str,"%",temp);
			// ghg tax applied to technology
			for (m=0;m<maxper;m++)
				temp[m] = techs[i][m]->showcarbontax();
			dboutput4(regname,"C Tax",secname,str,"$/TC",temp);
			// ghg tax paid
			for (m=0;m<maxper;m++)
				temp[m] = techs[i][m]->showcarbontaxpaid();
			dboutput4(regname,"C Tax Paid",secname,str,"90Mil$",temp);
			// technology fuel input
			for (m=0;m<maxper;m++)
				temp[m] = techs[i][m]->showinput();
			dboutput4(regname,"Fuel Consumption",secname,techs[i][0]->getfname(),"EJ",temp);
		}

		
		
		
/*	CI fmap; // define fmap
	map<string,double> tfuelmap = summary[0].getfuelcons();
	for (fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
		for (m=0;m<maxper;m++) {
			temp[m] = summary[m].get_fmap_second(fmap->first);
		}
		dboutput4(regname,"Fuel Consumption",secname,fmap->first,"EJ",temp);
	}
*/		
		
		
		// for 1 or more technologies
		// technology efficiency
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m]->showeff();
		dboutput4(regname,"Tech Efficiency",secname,str,"%",temp);
	}
}

int subsector::shownotech() const {
	return notech;
}

//! calculate GHG emissions from annual production of subresource
void subsector::emission( const int per, const string& prodname ){
	//! \pre per is less than or equal to max period.
	assert( per <= scenario.getModeltime()->getmaxper() );
	summary[per].clearemiss(); // clear emissions map
	summary[per].clearemfuelmap(); // clear emissions map
	for (int i=0;i<notech;i++) {
		techs[i][per]->emission(prodname);
		summary[per].updateemiss(techs[i][per]->getemissmap());
		summary[per].updateemfuelmap(techs[i][per]->getemfuelmap());
	}
}

//! calculate indirect GHG emissions from annual production of subresource
void subsector::indemission(const int per) {
	//! \pre per is less than or equal to max period.
	assert( per <= scenario.getModeltime()->getmaxper() );
	summary[per].clearemindmap(); // clear emissions map
	for (int i=0;i<notech;i++) {
		techs[i][per]->indemission();
		summary[per].updateemindmap(techs[i][per]->getemindmap());
	}
}

//! returns subsector primary energy consumption
double subsector::showpe_cons( const int per ) {
	//! \pre per is less than or equal to max period.
	assert( per <= scenario.getModeltime()->getmaxper() );
	pe_cons[per] = 0;
	for (int i=0;i<notech;i++) {
		// depleatable resource indeces are less than 5
		// how should this condition be made generic?
		// shk 7/23/01
		if (techs[i][per]->showfuelno() < 5) {
			// int num = techs[i][per].showfuelno() ;
			pe_cons[per] += techs[i][per]->showinput();
			//double temp = techs[i][per]->showinput();
			int stop = 1;
		}
	}
	return pe_cons[per];
}

//! returns primary or final energy input
double subsector::showinput( const int per ) const {
	//! \pre per is less than or equal to max period.
	assert( per <= scenario.getModeltime()->getmaxper() );

	return input[per];
}

//! returns subsector output
double subsector::getoutput( const int per ) const {
	//! \pre per is less than or equal to max period.
	assert( per <= scenario.getModeltime()->getmaxper() );
	
	return output[per];
}

//! returns total subsector carbon taxes paid
double subsector::showcarbontaxpaid( const int per ) const {
	//! \pre per is less than or equal to max period.
	assert( per <= scenario.getModeltime()->getmaxper() );
	
	return carbontaxpaid[per];
}

//!  gets fuel consumption map in summary object.
map<string, double> subsector::getfuelcons(const int per) const {
	//! \pre per is less than or equal to max period.
	assert( per <= scenario.getModeltime()->getmaxper() );

	return summary[per].getfuelcons();
}

//! clears fuel consumption map in summary object
void subsector::clearfuelcons(const int per) {
	summary[per].clearfuelcons();
}

//!  get ghg emissions map in summary object
map<string, double> subsector::getemission(const int per) const {
	return summary[per].getemission();
}

//!  get ghg emissions map in summary object
map<string, double> subsector::getemfuelmap( const int per ) const {
	return summary[per].getemfuelmap();
}

//!  get ghg emissions map in summary object
map<string, double> subsector::getemindmap(const int per) const {
	return summary[per].getemindmap();
}

//! update summaries for reporting
void subsector::updateSummary(const int per) {
	int i = 0;
	string goodName;
	
	// clears subsector fuel consumption map
	summary[per].clearfuelcons();

	for (i=0;i<notech;i++) {
		goodName = techs[i][0]->getfname();
		summary[per].initfuelcons(goodName,techs[i][per]->showinput());
	}
}


