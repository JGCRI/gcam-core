/* subrsrc.cpp												*
* Method definition for subrsrc class						*
* Coded by Sonny Kim 9/13/00								*/

#include "Definitions.h"
#include <vector>
#include <string>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cmath>
#include <ctime> 
#include <cassert>
#include "modeltime.h"
#include "subrsrc.h"
#include "xmlHelper.h"

using namespace std;

extern ofstream bugoutfile, outfile;	
extern Modeltime modeltime;

//! Default constructor.
subrsrc::subrsrc() {
	nograde = 0;
	minShortTermSLimit = 0;
	prc_elas = 1;	// default value if not read in
}

//! Destructor.
subrsrc::~subrsrc() {
	
	for ( vector< vector< grade* > >::iterator outerIter = depgrade.begin(); outerIter != depgrade.end(); outerIter++ ) {
		for( vector< grade* >::iterator innerIter = outerIter->begin(); innerIter != outerIter->end(); innerIter++ ) {
			delete *innerIter;
		}
	}
}

//! Clear member variables
void subrsrc::clear(){
	name = ""; // MSVC is missing String::Clear();
	nograde = 0;
	minShortTermSLimit = 0;
	prc_elas = 1;
	depgrade.clear();
	rscprc.clear();
	available.clear();
	annualprod.clear();
	cummprod.clear();
	gdpExpans.clear();
}

//! return subrsrc name
string subrsrc::getName() const {
	return name;
}

//! Initialize member variables from xml data
void subrsrc::XMLParse( const DOMNode* node )
{	
	DOMNodeList* nodeList = 0;
	DOMNodeList* childNodeList = 0;
	DOMNode* curr = 0;
	DOMNode* currChild = 0;
	string nodeName;
	string childNodeName;
	vector<grade*> tempGradesVec;
	grade* tempGrade = 0;
	
	// make sure we were passed a valid node.
	assert( node );
	
	// get the name attribute.
	name = XMLHelper<string>::getAttrString( node, "name" );
	
#if ( _DEBUG )
	cout << "\t\tSubResource name set as " << name << endl;
#endif
	
	// get all child nodes.
	nodeList = node->getChildNodes();
	
	// loop through the child nodes.
	for( int i = 0; i < nodeList->getLength(); i++ ){
		curr = nodeList->item( i );
		nodeName = XMLString::transcode( curr->getNodeName() );
		
		if( nodeName == "grade" ){
			childNodeList = curr->getChildNodes();
			
			// loop through grades children.
			for( int j = 0; j < childNodeList->getLength(); j++ ){
				
				currChild = childNodeList->item( j );
				childNodeName = XMLString::transcode( currChild->getNodeName() );
				
				if( childNodeName == "period" ){
					tempGrade = new grade();
					tempGrade->XMLParse( currChild );
					tempGradesVec.push_back( tempGrade );
				}
			}
			depgrade.push_back( tempGradesVec );
			tempGradesVec.clear(); // clears vector, size is 0
		}
		else if( nodeName == "annualprod" ){
			annualprod.push_back( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "minShortTermSLimit" ){
			minShortTermSLimit = XMLHelper<double>::getValue( curr );
		}
		else if( nodeName == "prc_elas" ){
			prc_elas = XMLHelper<double>::getValue( curr );
		}
		
	}
	// completed parsing.
	
	nograde = depgrade.size(); // number of grades for each subresource
	// resize vectors not read in
	int maxper = modeltime.getmaxper();
	rscprc.resize( maxper ); // subresource price
	available.resize( maxper ); // total available resource
	cummprod.resize( maxper ); // cummulative production of subrsrc
	gdpExpans.resize( maxper ); // cummulative production of subrsrc
	updateAvailable( 0 ); 
	
	//Temporary code until this variable is read in (might, for some reason, want to make this var zero)
	for( int k = 0; k < static_cast<int>( gdpExpans.size() ); k++ ){
		if (gdpExpans[ k ] == 0) { gdpExpans[ k ] = 1; }
	}
	
}

void subrsrc::toXML( ostream& out ) const {
	int m = 0;
	// write the beginning tag.
	Tabs::writeTabs( out );
	out << "<subresource name=\"" << name << "\">"<< endl;
	
	// increase the indent.
	Tabs::increaseIndent();
	
	// write the xml for the class members.
	
	// write out the grade objects.
	for( vector< vector<grade*> >::const_iterator i = depgrade.begin(); i != depgrade.end(); i++ ){	
		Tabs::writeTabs( out );
		out << "<grade>" << endl;
		Tabs::increaseIndent();
		for( vector<grade*>::const_iterator j = i->begin(); j != i->end(); j++ ){
			( *j )->toXML( out );
		}
		Tabs::decreaseIndent();
		Tabs::writeTabs( out );
		out << "</grade>" << endl;
	}
	
	for(m = 0; m < static_cast<int>(annualprod.size() ); m++ ) {
		XMLWriteElement(annualprod[m],"annualprod",out,modeltime.getper_to_yr(m));
	}
	
	for(m = 0; m < static_cast<int>(gdpExpans.size() ); m++ ) {
		XMLWriteElement(gdpExpans[m],"gdpExpans",out,modeltime.getper_to_yr(m));
	}
	
	XMLWriteElement(minShortTermSLimit,"minShortTermSLimit",out);
	XMLWriteElement(prc_elas,"prc_elas",out);
	
	// finished writing xml for the class members.
	
	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</subresource>" << endl;
}

void subrsrc::toDebugXML( const int period, ostream& out ) const {
	
	// write the beginning tag.
	Tabs::writeTabs( out );
	out << "<subresource name=\"" << name << "\">"<< endl;
	
	// increase the indent.
	Tabs::increaseIndent();
	
	// write the xml for the class members.
	XMLWriteElement( nograde, "nograde", out );
	XMLWriteElement( minShortTermSLimit, "minShortTermSLimit", out );
	XMLWriteElement( prc_elas, "prc_elas", out );
	
	// Write out data for the period we are in from the vectors.
	XMLWriteElement( rscprc[ period ], "rscprc", out );
	XMLWriteElement( available[ period ], "available", out );
	XMLWriteElement( annualprod[ period ], "annualprod", out );
	XMLWriteElement( cummprod[ period ], "cummprod", out );
	XMLWriteElement( gdpExpans[ period ], "gdpExpans", out );
	
	// write out the grade objects.
	for( int i = 0; i < static_cast<int>( depgrade.size() ); i++ ){	
		depgrade[ i ][ period ]->toDebugXML( period, out );
	}
	
	// finished writing xml for the class members.
	
	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</subresource>" << endl;
}

double subrsrc::price(int per)
{
	rscprc[per] = 0.0;
	
	return rscprc[per] ;
}

int subrsrc::maxgrade() // returns total number of grades
{
	return nograde;
}

void subrsrc::cummsupply(double prc,int per)
{	
	int i=0,maxgrd;
	double slope=0;
	cummprod[per]=0.0;
	
	rscprc[per] = prc;
	// the index of the last grade is number of grades minus one
	// don't forget 0 as first
	maxgrd = nograde-1;
	
	// calculate total extraction cost for each grade
	for (int gr=0; gr<nograde; gr++) {
		depgrade[gr][per]->cost(per);
	}
	
	if (per == 0) cummprod[per] = 0.0;
	else {
		// Case 1
		// if market price is less than cost of first grade, then zero cummulative 
		// production
		if (prc <= depgrade[0][per]->getCost()) 
			cummprod[per] = cummprod[per-1];
		
		// Case 2
		// if market price is in between cost of first and last grade, then calculate 
		// cummulative production in between those grades
		if (prc > depgrade[0][per]->getCost() && prc <= depgrade[maxgrd][per]->getCost()) {
			int iL=0,iU=0;
			while (depgrade[i][per]->getCost() < prc) {
				iL=i; i++; iU=i;
			}
			// add subrsrcs up to the lower grade
			for (i=0;i<=iL;i++) cummprod[per]+=depgrade[i][0]->getAvail();
			// price must reach upper grade cost to produce all of lower grade
			slope = depgrade[iL][0]->getAvail()
				/ (depgrade[iU][per]->getCost() - depgrade[iL][per]->getCost());
			cummprod[per] -= slope * (depgrade[iU][per]->getCost() - prc);
		}
		
		// Case 3
		// if market price greater than the cost of the last grade, then
		// cummulative production is the amount in all grades
		if (prc > depgrade[maxgrd][per]->getCost())
			for (i=0;i<nograde;i++) cummprod[per]+=depgrade[i][0]->getAvail();
	}
	//available[per]=available[0]-cummprod[per];
}

double subrsrc::showcummprod(int per)
{
	return cummprod[per];
}

void subrsrc::updateAvailable( const int period ){
	available[ period ] = 0;
	for ( int i = 0; i < nograde; i++ ) {
		available[ period ] += depgrade[ i ][ period ]->getAvail();
	}
}

//! return annual supply
/*! Takes into account short-term capacity limits.
Note that cummsupply() must be called before calling this function. */
void subrsrc::annualsupply(int per,double gnp,double prev_gnp,double price,double prev_price)
{	
	// for per = 0 use initial annual supply
	// cummulative production is 0 for per = 0
	if (per >= 1) {
		// 2 is for the average of the annual productions
		annualprod[per] = 2.0 * (cummprod[per] - cummprod[per-1])/modeltime.gettimestep(per)
			- annualprod[per-1];
		if(annualprod[per] <= 0) {
			cummprod[per] = cummprod[per-1];
			annualprod[per] = 0.0;
		} 
		
		// incorporate short-term capacity limits after 1990
		// This represents a limit to how fast production capacity can expand
		if (per >= 2) {
			// minShortTermSLimit is defined in object and read in from database
			// minShortTermSLimit is the minimun short-term capacity limit
			double cur_annualprod = 0;
			
			// check to see if base short-term capacity (supply) limit is smaller than the minimum
			double max_annualprod = annualprod[per-1]*pow(gnp/prev_gnp,gdpExpans[per]);
			if(minShortTermSLimit < max_annualprod) { 
				cur_annualprod = max_annualprod; }
			else { 
				cur_annualprod = minShortTermSLimit; }
			
			// adjust short-term capacity limit for price effects
			cur_annualprod *= pow((price/prev_price),prc_elas);
			
			// Adjust current production and cummulative production to date
			// if greater than the short-term capacity limit
			if(cur_annualprod < annualprod[per]) {
				cummprod[per] = cummprod[per-1]+(cur_annualprod+annualprod[per-1])
					*modeltime.gettimestep(per)/2.0;
				annualprod[per] = cur_annualprod;
			}
		}
		// available is the total resource (stock)
		//available[per]=available[per-1]-(annualprod[per]*modeltime.gettimestep(per)/2);
		available[per]=available[per-1]-((annualprod[per]+annualprod[per-1])
			/2*modeltime.gettimestep(per));
		if (available[per]<=0) available[per] = 0;
	}
}


//! return annual production for period
double subrsrc::showannualprod(int per)
{
	return annualprod[per];
}

//! return available resource for period
double subrsrc::showavailable(int per)
{
	return available[per];
}

//! write subrsrc output to database
void subrsrc::MCoutput( const string &regname, const string& secname )
{
	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
		string uname,vector<double> dout);
	
	int i=0, m=0;
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total subsector output
	dboutput4(regname,"Pri Energy Production",secname,name,"EJ",annualprod);
	dboutput4(regname,"Resource",secname,name,"EJ",available);
	dboutput4(regname,"Price",secname,name,"$/GJ",rscprc);
	
	string tssname = name; // tempory subsector name
	string str; // tempory string
	
	// do for all grades in the sector
	for (i=0;i<nograde;i++) {
		str = tssname + "_" + depgrade[i][0]->getName();
		// grade cost
		for (m=0;m<maxper;m++)
			temp[m] = depgrade[i][m]->getCost();
		dboutput4(regname,"Price",secname,str,"$/GJ",temp);
		// grade extraction cost
		for (m=0;m<maxper;m++)
			temp[m] = depgrade[i][m]->getExtCost();
		dboutput4(regname,"Price ExtCost",secname,str,"$/GJ",temp);
		// grade environmental cost
		for (m=0;m<maxper;m++)
			temp[m] = depgrade[i][m]->getEnvCost();
		dboutput4(regname,"Price EnvCost",secname,str,"$/GJ",temp);
	}
}

//! write subrsrc output to file
void subrsrc::outputfile( const string &regname, const string& sname)
{
	// function protocol
	void fileoutput3( string var1name,string var2name,string var3name,
		string var4name,string var5name,string uname,vector<double> dout);
	
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total subsector output
	fileoutput3( regname,sname,name," ","production","EJ",annualprod);
	fileoutput3( regname,sname,name," ","resource","EJ",available);
	
	/*	// do for all grades in the sector
	for (i=0;i<nograde;i++) {
	// output or demand for each grade
	for (m=0;m<maxper;m++)
	temp[m] = depgrade[i][m].showavail();
	fileoutput2(reg,regname,sname,name,"supply",depgrade[i][0].getname(),temp,"EJ");
	// grade cost
	for (m=0;m<maxper;m++)
	temp[m] = depgrade[i][m].getcost();
	fileoutput2(reg,regname,sname,name,"cost",depgrade[i][0].getname(),temp,"$/GJ");
	// grade efficiency
	for (m=0;m<maxper;m++)
	temp[m] = depgrade[i][m].getextcost();
	fileoutput2(reg,regname,sname,name,"ext cost",depgrade[i][0].getname(),temp,"$/GJ");
	// grade environmental cost
	for (m=0;m<maxper;m++)
	temp[m] = depgrade[i][m].getenvcost();
	fileoutput2(reg,regname,sname,name,"env cost",depgrade[i][0].getname(),temp,"$/GJ");
	}
	*/
}

