/* technology.cpp									*
* Method definition for the technology class.		*
* This technology class is based on the MiniCAM	*
* description of technology.						*
* SHK  6/12/00										*/

// Standard Library headers
#include "Definitions.h"
#include <cstdlib>
#include <string>
#include <fstream>
#include <iostream>
#include <algorithm>
#include <functional>
#include <cmath>
#include <cassert>

// User headers
#include "xmlHelper.h"
#include "market.h"
#include "technology.h"
#include "modeltime.h"
#include "Marketplace.h"

extern Marketplace marketplace;
extern Modeltime modeltime;

// Technology class method definition

//! Default constructor.
technology::technology() {
	initElementalMembers();
}

// ! Destructor
technology::~technology() {
	for( vector<Ghg*>::iterator iter = ghg.begin(); iter != ghg.end(); iter++ ) {
		delete *iter;
	}
}

//! Clear member variables.
void technology::clear(){
	initElementalMembers();
	name = "";
	unit = "";
	fuelname = "";
	ghg.clear();
	emissmap.clear();
	emfuelmap.clear();
	emindmap.clear();
}

//! Initialize elemental data members.
void technology::initElementalMembers(){
	fueltype = 0;
	year = 0;
	shrwts = 0;
	eff = 0;
	necost = 0;
	techcost = 0;
	tax = 0;
	carbontax = 0;
	carbontaxgj = 0;
	carbontaxpaid = 0;
	lexp = 0;
	share = 0;
	input = 0;
	output = 0;
	techchange = 0;
	A = 0;
	B = 0;
	resource = 0;
    fixedOutputVal = 0;
}

//! initialize technology with xml data
void technology::XMLParse( const DOMNode* node )
{	
	Ghg* tempGhg = 0;
	DOMNode* curr = 0;
	string nodeName;
	DOMNodeList* nodeList;
	
	//! \pre Assume we are passed a valid node.
	assert( node );
	
	nodeList = node->getChildNodes();
	
	for( int i = 0; i < nodeList->getLength(); i++ ) {
		curr = nodeList->item( i );
		nodeName = XMLString::transcode( curr->getNodeName() );		
		
		if( nodeName == "name" ) {
			name = XMLHelper<string>::getValueString( curr );
			
#if( _DEBUG )
			// cout << "\t\t\tTechnology name set as " << name << endl;
#endif
		} 
		else if ( nodeName == "fueltype" ){
			fueltype = XMLHelper<int>::getValue( curr );
		}
		else if( nodeName == "year" ){
			year = XMLHelper<int>::getValue( curr );
		}
		else if( nodeName == "fuelname" ){
			fuelname = XMLHelper<string>::getValueString( curr );
		}
		else if( nodeName == "sharewt" ){
			shrwts = XMLHelper<double>::getValue( curr );
		}
		else if( nodeName == "efficiency" ){
			eff = XMLHelper<double>::getValue( curr );
		}
		else if( nodeName == "nonenergycost" ){
			necost = XMLHelper<double>::getValue( curr );
		}
		else if( nodeName == "tax" ){
			tax = XMLHelper<double>::getValue( curr );
		}
		else if( nodeName == "logitexp" ){
			lexp = XMLHelper<double>::getValue( curr );
		}
		else if( nodeName == "techchange" ){
			techchange = XMLHelper<double>::getValue( curr );
		}
		else if( nodeName == "resource" ){
			resource = XMLHelper<double>::getValue( curr );
		}
		else if( nodeName == "A" ){
			A = XMLHelper<double>::getValue( curr );
		}
		else if( nodeName == "B" ){
			B = XMLHelper<double>::getValue( curr );
		}
		else if( nodeName == "GHG" ){
			tempGhg = new Ghg();
			tempGhg->XMLParse( curr );
			ghg.push_back( tempGhg );
		}
	}
	
	if( ghg.empty() ) {
		Ghg* CO2 = new Ghg( "CO2", "MTC", 0, 0, 1 ); // at least CO2 must be present
		ghg.push_back( CO2 );
	}
}

//! write object to xml output stream
void technology::toXML( ostream& out ) const {
	
	Tabs::writeTabs( out );
	out << "<period>" << endl;
	
	Tabs::increaseIndent();
	
	// write the xml for the class members.
	
	XMLWriteElement( name, "name", out );
	XMLWriteElement( year, "year", out );
	XMLWriteElement( fueltype, "fueltype", out );
	XMLWriteElement( shrwts, "sharewt", out );
	XMLWriteElement( fuelname, "fuelname", out );
	XMLWriteElement( eff, "efficiency", out );
	XMLWriteElement( necost, "nonenergycost", out );
	XMLWriteElement( tax, "tax", out );
	XMLWriteElement( lexp, "logitexp", out );
	XMLWriteElement( techchange, "techchange", out );
	XMLWriteElement( resource, "resource", out );
	XMLWriteElement( A, "A", out );
	XMLWriteElement( B, "B", out );
	
	for( vector<Ghg*>::const_iterator ghgIter = ghg.begin(); ghgIter != ghg.end(); ghgIter++ ){
		( *ghgIter )->toXML( out );
	}
	
	// finished writing xml for the class members.
	
	Tabs::decreaseIndent();
	
	Tabs::writeTabs( out );
	out << "</period>" << endl;
}

//! write object to xml debugging output stream
void technology::toDebugXML( const int period, ostream& out ) const {
	
	Tabs::writeTabs( out );
	out << "<technology name=\"" << name << "\" year=\"" << year << "\">" << endl;
	
	Tabs::increaseIndent();
	// write the xml for the class members.
	
	XMLWriteElement( unit, "unit", out );
	XMLWriteElement( fueltype, "fueltype", out );
	XMLWriteElement( fuelname, "fuelname", out );
	XMLWriteElement( shrwts, "sharewt", out );
	XMLWriteElement( fuelname, "fuelname", out );
	XMLWriteElement( eff, "efficiency", out );
	XMLWriteElement( necost, "nonenergycost", out );
	XMLWriteElement( tax, "tax", out );
	XMLWriteElement( carbontax, "carbontax", out );
	XMLWriteElement( carbontaxgj, "carbontaxgj", out );
	XMLWriteElement( carbontaxpaid, "carbontaxpaid", out );
	XMLWriteElement( lexp, "logitexp", out );
	XMLWriteElement( share, "share", out );
	XMLWriteElement( input, "output", out );
	XMLWriteElement( techchange, "techchange", out );
	XMLWriteElement( resource, "resource", out );
	XMLWriteElement( A, "A", out );
	XMLWriteElement( B, "B", out );
	// write our ghg object, vector is of number of gases
	for( vector<Ghg*>::const_iterator i = ghg.begin(); i != ghg.end(); i++ ){
		( *i )->toDebugXML( period, out );
	}
	
	// finished writing xml for the class members.
	
	Tabs::decreaseIndent();
	
	Tabs::writeTabs( out );
	out << "</technology>" << endl;
}

//! apply carbon tax to appropriate technology
void technology::applycarbontax(double tax)
{
	// convert tax from $/carbon unit to $/energy unit
	// if fuel does not contain carbon, emissions coefficient
	// is zero and so is the carbon tax
	// units: tax (90$/TC), CO2coef (MTC/EJ), carbontax (75$/GJ)
	carbontaxgj = 0; // initialize
	carbontax = tax;
	
	// returns emissions coefficient only if fuels are primary fuels
	// crude oil, natural gas and coal
	// add to previous ghg tax if more than one ghg
	for(int i=0;i<ghg.size();i++) {
		carbontaxgj += carbontax*ghg[i]->taxcnvrt(fuelname)*1e-3;
	}
}

//! sets ghg tax to technologies
/*! does not get called if there are no markets for ghgs */
void technology::addghgtax( const string ghgname, const string regionName, const int per )
{
	// returns coef for primary fuels only
	// carbontax has value for primary fuels only
	carbontaxgj = 0; // initialize
	carbontax = marketplace.showprice(ghgname,regionName,per);
	// add to previous ghg tax if more than one ghg
	for(int i=0;i<ghg.size();i++) {
		carbontaxgj += carbontax*ghg[i]->taxcnvrt(fuelname)*1e-3;
	}
	// need to add taxes from all ghgs
}

//! define technology fuel cost and total cost
void technology::cost( const string regionName, const int per ) 
{
	double fuelprice = 0;
	
	fuelprice = marketplace.showprice(fuelname,regionName,per);
	
	//techcost = fprice/eff/pow(1+techchange,modeltime.gettimestep(per)) + necost;
	fuelcost = (fuelprice+carbontaxgj)/eff;
	techcost = fuelcost + necost;
}

//! calculate technology shares
void technology::calc_share( const string regionName, const int per)
{
	// use pow(x,y) == x**y in fortran, x and y are double, need math.h
	share = shrwts * pow(techcost,lexp);
}

//! normalize technology shares
void technology::norm_share(double sum) 
{
	if (sum==0)
		share = 0;
	else
		share /= sum;
}

//! calculates fuel input and technology output
/*! This function sets the value of fixed supply. At present this does so only for Hydro 
acccording to the MiniCAM formula. In the future, this will be superseeded by read-in values. */
double technology::getFixedSupply(int per)
{
	string hydro = "hydro";
	// dmd is total subsector demand
	if(name != hydro) {
		return 0; 
	}
	else { // do for hydroelectricity
		int T = per*modeltime.gettimestep(per);
		// resource and logit function 
		double fact = exp(A+B*T);
		output = resource*fact/(1+fact);
		return output;
	}
}

//! Adjusts shares to be consistant with any fixed production 
/*! This version will probalby not work if more than one technology within each sector 
has a fixed supply */
void technology::adjShares(double subsecdmd, double totalFixedSupply, double varShareTot, int per)
{
    double remainingDemand;
    double fixedSupply;
    
    if(totalFixedSupply != 0) {
        remainingDemand = subsecdmd - totalFixedSupply;
        if (remainingDemand < 0) {
			remainingDemand = 0;
		}
        fixedSupply = getFixedSupply(per); 
        
        if ( fixedSupply != 0 ) {	// This tech has a fixed supply
            if (subsecdmd != 0) {
                share = fixedSupply/subsecdmd;
                // Set value of fixed supply
                if (fixedSupply > subsecdmd) {
                    fixedOutputVal = totalFixedSupply; }  // downgrade output if > fixedsupply
                else {
                    fixedOutputVal = fixedSupply; }
            }
            else {
                share = 0;
			}
        }
        else {	// This tech does not have fixed supply
            if (subsecdmd != 0 && varShareTot != 0) {
                share = share * (remainingDemand/subsecdmd)/varShareTot;
			}
            else {
				// Maybe not correct, but if other params are zero then something else is wrong
                share = 0; 
			}
        }
    }    
}

//! calculates fuel input and technology output
void technology::production(double dmd,int per) 
{
	string hydro = "hydro";
	// dmd is total subsector demand
	if(name != hydro) {
		output = share * dmd; // use share to get output for each technology
	}
	else { // do for hydroelectricity
		output = fixedOutputVal;
	}
	
	if (fueltype == 0) // fueltye=0 reserved for renewables
		input = output/eff;
	else {// demand for fossil, uranium and secondary energy
		//input = output/eff/pow(1+techchange,timestep);
		input = output/eff;
	}
	
	if (input < 0) {
		cerr << "ERROR: Output value < 0 for technology " << name << endl;
	}
	
	// total carbon taxes paid
	// carbontax is null for technologies that do not consume fossil fuels
	// input(EJ), carbontax(90$/GJ), carbontaxpaid(90$Mil)
	carbontaxpaid = input*carbontaxgj*1e+3;
}

//! calculate GHG emissions from technology use
void technology::emission( const string prodname ) {
	// alternative ghg emissions calculation
	//for_each(ghg.begin(),ghg.end(),ghg.calc_emiss());
	emissmap.clear(); // clear emissions map
	emfuelmap.clear(); // clear emissions map
	for (int i=0; i<ghg.size(); i++) {
		ghg[i]->calc_emiss(fuelname,input,prodname,output);
		emissmap[ghg[i]->getname()] = ghg[i]->getemission();
		emfuelmap[ghg[i]->getname()] = ghg[i]->getemiss_fuel();
	}
}

//! calculate indirect GHG emissions from technology use
void technology::indemission()
{
	emindmap.clear(); // clear emissions map
	for (int i=0; i<ghg.size(); i++) {
		ghg[i]->calc_emiss_ind(input,fuelname);
		emindmap[ghg[i]->getname()] = ghg[i]->getemiss_ind();
	}
}

//! show technology info
void technology::printTech( const string& outFile ) const {
	bool toScreen = false;
	
	ofstream outStream;
	if( outFile == "" ){
		toScreen = true; ;
	}
	else {
		outStream.open( outFile.c_str(), ios::app );
		if ( !outStream ) { //open failed
			cerr << "Cannot open file for output" << endl;
			exit( -1 );
		}
	}
	
	if( toScreen ) {
		cout << "Technology: " << name << endl;
	}
	else {
		outStream << "Technology: " << name << endl;
	}
	if( toScreen ) {
		cout << "Year: "<< year << endl;
	}
	else {
		outStream << "Year: "<< year << endl;
	}
	if( toScreen ) {
		cout << "Fuel Type: "<< fueltype << endl;
	}
	else {
		outStream << "Fuel Type: "<< fueltype << endl;
	}
	if( toScreen ) {
		cout << "Share Weights: "<< shrwts << endl;
	}
	else {
		outStream << "Share Weights: "<< shrwts << endl;
	}
	if( toScreen ) {
		cout << "Energy Efficiency: "<< eff << endl;
	}
	else {
		outStream << "Energy Efficiency: "<< eff << endl;
	}
	if ( toScreen ) {
		cout << "Non-Energy Cost: "<< necost << endl;
	}
	else {
		outStream << "Non-Energy Cost: "<< necost << endl;
	}
	if ( toScreen ) {
		cout << "Tax: "<< tax << endl;
	}
	else {
		outStream << "Tax: "<< tax << endl;
	}
	if ( toScreen ) {
		cout << "Logit Exponential: "<< lexp << endl;
	}
	else {
		outStream << "Logit Exponential: "<< lexp << endl;
	}
	if ( toScreen ) {
		cout << "Technical Change: "<< techchange << endl;
	}
	else {
		outStream << "Technical Change: "<< techchange << endl;
	}
	
	if( outStream != cout ) {
		outStream.close();
	}
}

//! return technology name
string technology::showname() const {
	return name;
}

//! return fuel name
string technology::getfname() const {
	return fuelname;
}

//! return fuel type 
int technology::showfuelno() const {
	return fueltype;
}

//! return fuel efficiency
double technology::showeff() const {
	return eff;
}

//! return technology share
double technology::showshare() const {
	return share;
}

//! return fuel input for technology
double technology::showinput() const {
	return input;
}

//! return output of technology
double technology::showoutput() const {
	return output;
}

//! return technology fuel cost only
double technology::getfuelcost( ) const {
	return fuelcost;
}


//! return the cost of technology
double technology::gettechcost() const {
	return techcost;
}

//! return the non-energy cost of the technology
double technology::getnecost() const {
	return necost;
}

//! return carbon taxes applied to technology
double technology::showcarbontax() const {
	// ($/TC)
	return carbontax;
}

//! return carbon taxes applied to technology
double technology::showcarbontaxgj() const {
	// ($/GJ)
	return carbontaxgj;
}

//! return carbon taxes paid by technology
double technology::showcarbontaxpaid() const {
	return carbontaxpaid;
}

//! returns actual CO2 emissions from technology, alternate
double technology::getCO2()  const {
	return ghg[0]->getemission(); // index 0 is for CO2
}

//! return map of all ghg emissions
map<string,double> technology::getemissmap() const {
	return emissmap;
}

//! return map of all ghg emissions
map<string,double> technology::getemfuelmap() const {
	return emfuelmap;
}

//! return map of all ghg emissions
map<string,double> technology::getemindmap() const {
	return emindmap;
}

//! return value for ghg
double technology::get_emissmap_second( const string& str) const {
	return ( emissmap.find( str ) )->second;
}

//! returns technology logit exponential
double technology::getlexp()  const {
	return lexp;
}

//  ******* method definition for hydro_tech

//! Default constructor
hydro_tech::hydro_tech(){
	resource = 0;
	A = 0;
	B = 0;
}

//! Clear all member variables.
void hydro_tech::clear(){
	technology::clear();
	resource = 0;
	A = 0;
	B = 0;
}

//! calculates hydroelectricity output based on logit function
void hydro_tech::production(double dmd,int per) 
{
	int T = per*modeltime.gettimestep(per);
	double tempOutput;
	// resource and logit function 
	double fact = exp(A+B*T);
	tempOutput = resource*fact/(1+fact);
	tempOutput = min(tempOutput,fixedOutputVal);
	output = tempOutput;
	input = 0; // no fuel input for hydro
}
