/*! 
* \file ghg.cpp
* \ingroup CIAM
* \brief Ghg class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <map>
#include <vector>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "emissions/include/ghg.h"
#include "emissions/include/indirect_emiss_coef.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "containers/include/world.h"
#include "marketplace/include/marketplace.h"

extern Scenario* scenario;

using namespace std;
using namespace xercesc;

Ghg::Ghg( const string& nameIn, const string& unitIn, const double rmfracIn, const double gwpIn, const double emissCoefIn ){
    name = nameIn;
    unit = unitIn;
    rmfrac = rmfracIn;
    gwp = gwpIn;
    emissCoef = emissCoefIn;
    emission = 0;
    isGeologicSequestration = true;
    storageCost = util::getLargeNumber(); // default to a large cost to turn off CCS
    sequestAmountGeologic = 0;
    sequestAmountNonEngy = 0;
    emissGwp = 0;
    emissFuel = 0;
    emissInd = 0;
    emissCoefPrev = 0;
    inputEmissions = 0;
    emissionsWereInput = false;
}

//! Destructor
Ghg::~Ghg(){
}

//! Clear member variables.
void Ghg::clear(){

    // clear elemental data.
    rmfrac = 0;
    storageCost = util::getLargeNumber(); // default to a large cost to turn off CCS
    gwp = 0;
    emission = 0;
	isGeologicSequestration = false;
    sequestAmountGeologic = 0;
    sequestAmountNonEngy = 0;
    emissGwp = 0;
    emissCoef = 0;
    emissFuel = 0;
    emissInd = 0;
    name = "";
    unit = "";
    storageName = "";
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
        else if( nodeName == "inputEmissions" ){
            inputEmissions = XMLHelper<double>::getValue( curr );
				emissionsWereInput = true;
        }
        else if( nodeName == "emisscoef" ){
            emissCoef = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "removefrac" ){
            rmfrac = XMLHelper<double>::getValue( curr );
        }
		// is geologic sequestration, true or false
        else if( nodeName == "isGeologicSequestration" ){
            isGeologicSequestration = XMLHelper<bool>::getValue( curr );
        }
		// fixed storage cost read in from data
        else if( nodeName == "storageCost" ){
            storageName = XMLHelper<string>::getAttrString( curr, "name" );
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
void Ghg::toXML( ostream& out, Tabs* tabs ) const {

    tabs->writeTabs( out );
    out << "<GHG name=\"" << name << "\">" << endl;

    tabs->increaseIndent();

    // write xml for data members
    XMLWriteElement( unit, "unit", out, tabs );
    if( emissionsWereInput ) {
	    XMLWriteElement( inputEmissions, "inputEmissions", out, tabs );
	 } else {
		 XMLWriteElementCheckDefault( emissCoef, "emisscoef", out, tabs, 0.0 );
	 }
    XMLWriteElementCheckDefault( rmfrac, "removefrac", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( isGeologicSequestration, "isGeologicSequestration", out, tabs, true );
    XMLWriteElementCheckDefault( storageCost, "storageCost", out, tabs, util::getLargeNumber() );
    XMLWriteElementCheckDefault( gwp, "GWP", out, tabs, 0.0 );
    // done writing xml for data members.

    tabs->decreaseIndent();

    tabs->writeTabs( out );
    out << "</GHG>" << endl;
}

//! Writes datamembers to debugging datastream in XML format.
void Ghg::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    tabs->writeTabs( out );
    out << "<GHG name=\"" << name << "\">" << endl;

    tabs->increaseIndent();

    // write xml for data members
    XMLWriteElement( unit, "unit", out, tabs );
    XMLWriteElement( rmfrac, "removefrac", out, tabs );
    XMLWriteElement( gwp, "GWP", out, tabs );
    XMLWriteElement( emission, "emission", out, tabs );
    XMLWriteElement( isGeologicSequestration, "isGeologicSequestration", out, tabs );
    XMLWriteElement( storageCost, "storageCost", out, tabs );
    XMLWriteElement( sequestAmountGeologic, "sequestAmountGeologic", out, tabs );
    XMLWriteElement( sequestAmountNonEngy, "sequestAmountNonEngy", out, tabs );
    XMLWriteElement( emissGwp, "emissGwp", out, tabs );
    XMLWriteElement( emissCoef, "emisscoef", out, tabs );
    XMLWriteElement( emissFuel, "emissFuel", out, tabs );
    XMLWriteElement( emissInd, "emissInd", out, tabs );
    // done writing xml for data members.

    tabs->decreaseIndent();

    tabs->writeTabs( out );
    out << "</GHG>" << endl;
}

/*! Second Method: Convert GHG tax and any storage costs into energy units using GHG coefficients
*   and return the value or cost of the tax and storage for the GHG.
*   Apply taxes only if emissions occur.  Emissions occur if there is a difference in the emissions
*   coefficients.
*  \param regionName Name of the region for GHG
*  \param fuelName Name of the fuel
*  \param prodName The name of the output product.
*  \param efficiency The efficience of the technology this ghg emitted by.
*  \param period The period in which this calculation is occurring. 
*  \return Generalized cost or value of the GHG
*/
double Ghg::getGHGValue( const string& regionName, const string& fuelName, const string& prodName, const double efficiency, const int period ) const {

    const World* world = scenario->getWorld();
    const Marketplace* marketplace = scenario->getMarketplace();
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    const double SMALL_NUM = util::getSmallNumber();
    const double CVRT_tg_MT = 1e-3; // to get teragrams of carbon per EJ to metric tons of carbon per GJ
    // name is GHG name
    double GHGTax = marketplace->getPrice(name,regionName,period);
	// get carbon storage cost from the market
	double marketStorageCost = 0;
	if ( marketplace->doesMarketExist( storageName, regionName, period ) ) {
		// market exists, use market storage cost
        marketStorageCost = marketplace->getPrice(storageName,regionName,period);
	}
	else {
		// market does not exist, use default or read in storage cost
		marketStorageCost = storageCost;
	}

	// if tax is 0 or small, turn off sequestration technology by increasing storage cost
	if (GHGTax < SMALL_NUM) {
		marketStorageCost = util::getLargeNumber();
	}
    
	// units for generalized cost is in 75$/gj
    double generalizedCost = 0; 
    const double coefFuel = world->getPrimaryFuelCO2Coef( regionName, fuelName );
    const double coefProduct = world->getPrimaryFuelCO2Coef( regionName, prodName );

	if (name == "CO2") {
        // if remove fraction is greater than zero and storage cost is required
        if (rmfrac > 0) {
			// add geologic sequestration cost
			if (isGeologicSequestration) {
				// gwp applied only on the amount emitted
				// account for conversion losses through efficiency
		        generalizedCost = ((1.0 - rmfrac)*GHGTax*gwp + rmfrac*marketStorageCost)
			        * (coefFuel/efficiency - coefProduct) / CVRT90 * CVRT_tg_MT;
			}
			// no sequestration or storage cost added for non-energy use of fossil fuels
			else {
				generalizedCost = ((1.0 - rmfrac)*GHGTax*gwp)
			        * (coefFuel/efficiency - coefProduct) / CVRT90 * CVRT_tg_MT;
			}
        }
        // no storage required
        else {
            generalizedCost = GHGTax * gwp * (coefFuel/efficiency - coefProduct) / CVRT90 * CVRT_tg_MT;
        }
		//******* override generalizedCost if coefFuel is 0 *******
		// need to fix this
		if (coefFuel < SMALL_NUM) {
			generalizedCost = 0;
		}
    }
    // for all other gases used read-in emissions coefficient
    else {
        // apply carbon equivalent to emiss coefficienr
        // if remove fraction is greater than zero and storage is required
        if (rmfrac > 0) {
			// add geologic sequestration cost
			if (isGeologicSequestration) {
	            generalizedCost = ((1.0 - rmfrac)*GHGTax*gwp + rmfrac*storageCost) * emissCoef / CVRT90;
			}
			// no storage cost added
			else {
	            generalizedCost = ((1.0 - rmfrac)*GHGTax*gwp) * emissCoef / CVRT90;
			}
        }
        // no storage required
        else {
            generalizedCost = GHGTax * gwp * emissCoef / CVRT90;
        }
    }
	// for debugging
	if (generalizedCost < 0) {
		cout<<"generalized cost " << generalizedCost << endl;
		cout<<"GHGTax "<<GHGTax<<"  coefFuel  "<<coefFuel<<"  coefProduct"<<coefProduct<<endl;
		exit(-1);
	}
    return generalizedCost;
}

//! Calculate Ghg emissions.
void Ghg::calcEmission( const string& regionName, const string& fuelname, const double input, const string& prodname, const double output ) {

    const World* world = scenario->getWorld();

    // for CO2 use default emissions coefficient by fuel
    // remove fraction only applicable for CO2
    if (name == "CO2") {
        const double coefFuel = world->getPrimaryFuelCO2Coef( regionName, fuelname );
        const double coefProduct = world->getPrimaryFuelCO2Coef( regionName, prodname );

        // 100% efficiency and same coefficient, no emissions
        if (input==output && coefFuel == coefProduct ) {
            emission = 0;
            emissGwp = 0;
            sequestAmountGeologic = 0;
            sequestAmountNonEngy = 0;
            emissFuel = (1.0-rmfrac)*input* coefFuel;
            // Note: The primary fuel emissions will not be correct if sequestered emissions occur down the line.
        }
        else {
            // sequestered emissions
            if (rmfrac > 0) {
				// geologic sequestration
				if(isGeologicSequestration) {
					sequestAmountGeologic = rmfrac * ( (input * coefFuel ) - ( output * coefProduct ) );
				}
				// non-energy use of fuel, ie petrochemicals
				else {
					sequestAmountNonEngy = rmfrac * ( (input * coefFuel ) - ( output * coefProduct ) );
				}
            }
            // Note that negative emissions can occur here since biomass has a coef of 0. 
            emission = ( 1.0 - rmfrac ) * ( ( input* coefFuel ) - ( output* coefProduct ) );
            emissGwp = ( 1.0 - rmfrac ) * gwp * ( ( input * coefFuel ) - ( output * coefProduct ) );
            emissFuel = ( 1.0 - rmfrac ) * input* coefFuel;
        }
	}
    // for all other gases used read-in emissions coefficient
    else {
        // sequestered emissions
        if (rmfrac > 0) {
			// geologic sequestration
			if(isGeologicSequestration) {
	            sequestAmountGeologic = rmfrac * (input-output) * emissCoef;
			}
			// non-energy use of fuel, ie petrochemicals
			else {
				sequestAmountNonEngy = rmfrac * (input-output) * emissCoef;
			}
        }
	
		if ( emissionsWereInput ) {
			emission = inputEmissions;
			emissFuel = inputEmissions;
			if ( input != output ) {
				emissCoef = inputEmissions / ( (1.0 - rmfrac) * (input-output) );
			} else {
				emissCoef = 0;
			}
		} else {
			emission = ( input - output ) * emissCoef;
			emissFuel =  emissCoef * emissCoef;
		}
		emissGwp = gwp * emission;
    }
}

//! calculates emissions associated with the use of secondary energy
/*! get indirect emissions coefficient from map object */
void Ghg::calcIndirectEmission( const double input, const string& fuelname, const vector<Emcoef_ind>& emcoef_ind ) {
    emissInd = 0; // to initialize
    for (int i=0;i< static_cast<int>( emcoef_ind.size() );i++) {
        if (emcoef_ind[i].getName() == fuelname) { // sector name
            emissInd = emcoef_ind[i].getemcoef(name) * input;
        }
    }
}

//! Return name of Ghg.
string Ghg::getName() const {
    return name;
}

//! Return unit for Ghg.
string Ghg::getUnit() const {
    return unit;
}

//! Return Ghg emissions.
double Ghg::getEmission() const {
    return emission;
}

//! Return geologic sequestered ghg emissions.
double Ghg::getSequestAmountGeologic() const {
    return sequestAmountGeologic;
}

//! Return non-energy sequestered ghg emissions.
double Ghg::getSequestAmountNonEngy() const {
    return sequestAmountNonEngy;
}

//! Return ghg emissions inplicit in fuel.
double Ghg::getEmissFuel() const {
    return emissFuel;
}

//! Return indirect ghg emissions.
double Ghg::getEmissInd() const {
    return emissInd;
}

//! Return ghg emissions coefficient.
double Ghg::getEmissCoef() const{
    return emissCoef;
}

//! Return ghg emissions coefficient.
void Ghg::setEmissCoef( const double emissCoefIn ) {
	emissCoef = emissCoefIn;
}

//! Return flag that indicates if emissions were input for this technology
bool Ghg::getEmissionsInputStatus() const {
	return emissionsWereInput;
}
//! Set the flag that indicates that emissions were input for this technology
void Ghg::setEmissionsInputStatus() {
	emissionsWereInput = true;
}
