/*! 
* \file GHG.CPP
* \ingroup CIAM
* \brief Ghg class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <string>
#include <iostream>
#include <map>
#include <vector>
#include <cmath>
#include <cassert>
#include "ghg.h"
#include "Emcoef_ind.h" // indirect greenhouse gas emissions coefficient class
#include "xmlHelper.h"

using namespace std; // enables elimination of std::

// map of CO2 emissions coefficient for primary fuel only
extern map<string, double> co2coefpri;
// map of CO2 emissions coefficient for all fossil fuels
extern map<string, double> co2coefall;
// indirect CO2 emissions coefficient
// coefficients for secondary energy only
extern map<string, double> co2coefind; 
extern vector<Emcoef_ind> emcoef_ind;

Ghg::Ghg( const string& nameIn, const string& unitIn, const double rmfracIn, const double gwpIn, const double emissionIn ){
	name = nameIn;
	unit = unitIn;
	rmfrac = rmfracIn;
	gwp = gwpIn;
	emission = emissionIn;
	seq_emiss = 0;
	emiss_gwp = 0;
	emiss_coef = 0;
	emiss_fuel = 0;
	emiss_ind = 0;
}

//! Clear member variables.
void Ghg::clear(){

	// clear elemental data.
	rmfrac = 0;
	gwp = 0;
	emission = 0;
	seq_emiss = 0;
	emiss_gwp = 0;
	emiss_coef = 0;
	emiss_fuel = 0;
	emiss_ind = 0;
	name = "";
	unit = "";
}

//! Set emissions coefficient from data
void Ghg::setcoef( const double em_coef ) {
	emiss_coef = em_coef; // set attribute
}

//! initialize Ghg object with xml data
void Ghg::XMLParse(const DOMNode* node)
{	
	DOMNode* curr = 0;
	DOMNodeList* nodeList;
	string nodeName;

	//! \pre Assume we are passed a valid node.
	assert( node );
	
	// get the name attribute.
	// name of the GHG
	name = XMLHelper<string>::getAttrString( node, "name" );
	
	#if ( _DEBUG )
		//cout << "\t\t\t\tGHG name set as " << name << endl;
	#endif

	nodeList = node->getChildNodes();
	
	for( int i = 0; i < nodeList->getLength(); i++ ) {
		curr = nodeList->item( i );
		nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );		
		
		if( nodeName == "unit"){
			unit = XMLHelper<string>::getValueString( curr );
		}

		else if( nodeName == "emisscoef" ){
			emiss_coef = XMLHelper<double>::getValue( curr );
		}

		else if( nodeName == "removefrac" ){
			rmfrac = XMLHelper<double>::getValue( curr );
		}

		else if( nodeName == "GWP" ){
			gwp = XMLHelper<double>::getValue( curr );
		}
	}
}

//! Writes datamembers to datastream in XML format.
void Ghg::toXML( ostream& out ) const {
	
	Tabs::writeTabs( out );
	out << "<GHG name=\"" << name << "\">" << endl;
	
	Tabs::increaseIndent();
	
	// write xml for data members
	XMLWriteElement( unit, "unit", out );
	XMLWriteElement( emiss_coef, "emisscoef", out );
	XMLWriteElement( rmfrac, "removefrac", out );
	XMLWriteElement( gwp, "GWP", out );
	// done writing xml for data members.

	Tabs::decreaseIndent();

	Tabs::writeTabs( out );
	out << "</GHG>" << endl;
}

//! Writes datamembers to debugging datastream in XML format.
void Ghg::toDebugXML( const int period, ostream& out ) const {
	
	Tabs::writeTabs( out );
	out << "<GHG name=\"" << name << "\">" << endl;
	
	Tabs::increaseIndent();
	
	// write xml for data members
	XMLWriteElement( unit, "unit", out );
	XMLWriteElement( rmfrac, "removefrac", out );
	XMLWriteElement( gwp, "GWP", out );
	XMLWriteElement( emission, "emission", out );
	XMLWriteElement( seq_emiss, "seq_emiss", out );
	XMLWriteElement( emiss_gwp, "emiss_gwp", out );
	XMLWriteElement( emiss_coef, "emisscoef", out );
	XMLWriteElement( emiss_fuel, "emiss_fuel", out );
	XMLWriteElement( emiss_ind, "emiss_ind", out );
	// done writing xml for data members.

	Tabs::decreaseIndent();

	Tabs::writeTabs( out );
	out << "</GHG>" << endl;
}

//! Set remove fraction from data.

void Ghg::setrmfrac( const double trmfrac ) {
	rmfrac = trmfrac;	
}

//! Convert carbon tax to energy tax.
double Ghg::taxcnvrt( const string& fuelname ) const {
	double conversion; 
	if (name == "CO2") {
		// use map object for coefficient
		conversion = (1.0 - rmfrac) * gwp * co2coefpri[fuelname];
	}
	// for all other gases used read-in emissions coefficient
	else {
		// apply carbon equivalent to emiss coefficienr
		//conversion = (1.0 - rmfrac) * gwp * emiss_coef;
		conversion = 0;
	}
	return conversion;
}

//! Calculate Ghg emissions.
void Ghg::calc_emiss( const string& fuelname, const double input, const string& prodname, const double output ) {
	// for CO2 use default emissions coefficient by fuel
	// remove fraction only applicable for CO2
	if (name == "CO2") {
		// use map object for coefficient
		// 100% efficiency and same coefficient, no emissions
		if (input==output && co2coefall[fuelname]==co2coefall[prodname]) {
			emission = 0;
			emiss_gwp = 0;
			seq_emiss = 0;
			emiss_fuel = (1.0-rmfrac)*input*co2coefall[fuelname];
		}
		else {
			// sequestered emissions
			seq_emiss = rmfrac * ((input*co2coefall[fuelname])
			                       - (output*co2coefall[prodname]));
			emission = (1.0 - rmfrac) * ((input*co2coefall[fuelname])
			                       - (output*co2coefall[prodname]));
			emiss_gwp = (1.0 - rmfrac) * gwp * ((input*co2coefall[fuelname])
			                              - (output*co2coefall[prodname]));
			emiss_fuel = (1.0-rmfrac)*input*co2coefall[fuelname];
		}
	}
	// for all other gases used read-in emissions coefficient
	else {
		seq_emiss = rmfrac * (input-output) * emiss_coef;
		emission = (1.0 - rmfrac) * (input-output) * emiss_coef;
		emiss_gwp = (1.0 - rmfrac) * gwp * (input-output) * emiss_coef;
		emiss_fuel = (1.0 - rmfrac) * input * emiss_coef;
	}
}

//! Calculate emissions alternative.
void Ghg::calc_emiss2( const double activity, const double gamma, const double* cntl ) {
	emission = pow((emiss_coef*activity),gamma) * (1 - cntl[1]) * (1 - cntl[2]) * (1 - cntl[3]);
}
	
//! calculates emissions associated with the use of secondary energy
/*! get indirect emissions coefficient from map object */
void Ghg::calc_emiss_ind( const double input, const string& fuelname ) {
	emiss_ind = 0; // to initialize
	for (int i=0;i<emcoef_ind.size();i++) {
		if (emcoef_ind[i].getName() == fuelname) { // sector name
			double temp = emcoef_ind[i].getemcoef(name);
			emiss_ind = emcoef_ind[i].getemcoef(name) * input;
		}
	}
}

//! Return name of Ghg.
string Ghg::getname() const {
	return name;
}

//! Return unit for Ghg.
string Ghg::getunit() const {
	return unit;
}

//! Return Ghg emissions.
double Ghg::getemission() const {
	return emission;
}

//! Return sequestered ghg emissions.
double Ghg::getseq_emiss() const {
	return seq_emiss;
}

//! Return ghg emissions inplicit in fuel.
double Ghg::getemiss_fuel() const {
	return emiss_fuel;
}

//! Return indirect ghg emissions.
double Ghg::getemiss_ind() const {
	return emiss_ind;
}

//! Return ghg emissions coefficient.
double Ghg::getemiss_coef() const{
	return emiss_coef;
}