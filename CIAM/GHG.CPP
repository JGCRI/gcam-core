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
#include <cassert>
#include "ghg.h"
#include "Emcoef_ind.h"
#include "xmlHelper.h"
#include "Scenario.h"
#include "World.h"
#include "Marketplace.h"

extern Scenario* scenario;

using namespace std;
using namespace xercesc;

Ghg::Ghg( const string& nameIn, const string& unitIn, const double rmfracIn, const double gwpIn, const double emissionIn ){
    name = nameIn;
    unit = unitIn;
    rmfrac = rmfracIn;
    gwp = gwpIn;
    emission = emissionIn;
    storageCost = 0;
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
    storageCost = 0;
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
    
    /*! \pre Assume we are passed a valid node. */
    assert( node );
    
    // get the name attribute.
    // name of the GHG
    name = XMLHelper<string>::getAttrString( node, "name" );
    
#if ( _DEBUG )
    //cout << "\t\t\t\tGHG name set as " << name << endl;
#endif
    
    nodeList = node->getChildNodes();
    
    for( int i = 0; i < static_cast<int>( nodeList->getLength() ); i++ ) {
        curr = nodeList->item( i );
        nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );		
        
        if( nodeName == "#text" ){
            continue;
        }
        
        else if( nodeName == "unit"){
            unit = XMLHelper<string>::getValueString( curr );
        }
        
        else if( nodeName == "emisscoef" ){
            emiss_coef = XMLHelper<double>::getValue( curr );
        }
        
        else if( nodeName == "removefrac" ){
            rmfrac = XMLHelper<double>::getValue( curr );
        }
        
        else if( nodeName == "storageCost" ){
            storageCost = XMLHelper<double>::getValue( curr );
        }
        
        else if( nodeName == "GWP" ){
            gwp = XMLHelper<double>::getValue( curr );
        }
        else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing GHG." << endl;
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
    XMLWriteElementCheckDefault( emiss_coef, "emisscoef", out, 0 );
    XMLWriteElementCheckDefault( rmfrac, "removefrac", out, 0 );
    XMLWriteElementCheckDefault( gwp, "GWP", out, 0 );
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
double Ghg::taxcnvrt( const string& regionName, const string& fuelname ) const {
    
    const World* world = scenario->getWorld();
    
    double conversion; 
    if (name == "CO2") {
        // use map object for coefficient
        conversion = (1.0 - rmfrac) * gwp * world->getCarbonTaxCoef( regionName, fuelname );
    }
    // for all other gases used read-in emissions coefficient
    else {
        // apply carbon equivalent to emiss coefficienr
        //conversion = (1.0 - rmfrac) * gwp * emiss_coef;
        conversion = 0;
    }
    return conversion;
}

/*! Convert GHG tax and any storage costs into energy units using GHG coefficients
*   and return the value or cost of the tax and storage for the GHG
*  \param regionName Name of the region for GHG
*  \param fuelName Name of the fuel
*  \return Generalized cost or value of the GHG
*/
double Ghg::getGHGValue( const string& regionName, const string& fuelName, const int period) const {
    
    const World* world = scenario->getWorld();
    const Marketplace* marketplace = scenario->getMarketplace();
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    // returns coef for primary fuels only
    // carbontax has value for primary fuels only
    // name is GHG name
    double GHGTax = marketplace->getPrice(name,regionName,period);
    
    double generalizedCost = 0; 
    if (name == "CO2") {
        // storageCost is a fixed cost for storage that is read in (unit 90$/tC)
        // in the future, get storageCost from the market like GHGTax
        // 1e-3 is to get teragrams of carbon per EJ to metric tons of carbon per GJ
        // if remove fraction is greater than zero and storage is required
        if (rmfrac > 0) {
            generalizedCost = (GHGTax*(1.0 - rmfrac)*gwp + storageCost/CVRT90)
            * world->getCarbonTaxCoef( regionName, fuelName ) * 1e-3;
        }
        // no storage required
        else {
            generalizedCost = GHGTax*(1.0 - rmfrac)*gwp
            * world->getCarbonTaxCoef( regionName, fuelName ) * 1e-3;
        }

    }
    // for all other gases used read-in emissions coefficient
    else {
        // apply carbon equivalent to emiss coefficienr
        // if remove fraction is greater than zero and storage is required
        if (rmfrac > 0) {
            generalizedCost = (GHGTax*(1.0 - rmfrac)*gwp + storageCost/CVRT90) * emiss_coef;
        }
        // no storage required
        else {
            generalizedCost = GHGTax*(1.0 - rmfrac)*gwp * emiss_coef;
        }
    }
    return generalizedCost;
}

//! Calculate Ghg emissions.
void Ghg::calc_emiss( const string& regionName, const string& fuelname, const double input, const string& prodname, const double output ) {
    
    const World* world = scenario->getWorld();
    
    // for CO2 use default emissions coefficient by fuel
    // remove fraction only applicable for CO2
    if (name == "CO2") {
        const double coefFuel = world->getPrimaryFuelCO2Coef( regionName, fuelname );
        const double coefProduct = world->getPrimaryFuelCO2Coef( regionName, prodname );
        
        // 100% efficiency and same coefficient, no emissions
        if (input==output && coefFuel == coefProduct ) {
            emission = 0;
            emiss_gwp = 0;
            seq_emiss = 0;
            emiss_fuel = (1.0-rmfrac)*input* coefFuel;
            // Note: The primary fuel emissions will not be correct if sequestered emissions occur down the line.
        }
        else {
            // sequestered emissions
            seq_emiss = rmfrac * ( (input * coefFuel ) - ( output * coefProduct ) );
            // Note that negative emissions can occur here since biomass has a coef of 0. 
            emission = ( 1.0 - rmfrac ) * ( ( input* coefFuel ) - ( output* coefProduct ) );
            emiss_gwp = ( 1.0 - rmfrac ) * gwp * ( ( input * coefFuel ) - ( output * coefProduct ) );
            emiss_fuel = (1.0-rmfrac) * input* coefFuel;
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

//! calculates emissions associated with the use of secondary energy
/*! get indirect emissions coefficient from map object */
void Ghg::calc_emiss_ind( const double input, const string& fuelname, const vector<Emcoef_ind>& emcoef_ind ) {
    emiss_ind = 0; // to initialize
    for (int i=0;i< static_cast<int>( emcoef_ind.size() );i++) {
        if (emcoef_ind[i].getName() == fuelname) { // sector name
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

