/*! 
* \file Region.cpp
* \ingroup CIAM
* \brief The Region class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cassert>
#include <xercesc/dom/DOM.hpp>
#include <algorithm>

#include "region.h"
#include "summary.h"
#include "Sector.h"
#include "DemandSector.h"
#include "tranSector.h"
#include "Resource.h"
#include "AgSector.h"
#include "demographic.h"
#include "ghg_mrk.h"
#include "xmlHelper.h"
#include "scenario.h"
#include "Emcoef_ind.h"
#include "World.h"
#include "modeltime.h" 
#include "Marketplace.h"
#include "Configuration.h"
#include "Util.h"
#include "Logger.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
extern ofstream outfile, sdfile;	

//! Default constructor
Region::Region() {
    agSector = 0; // null pointer
    population = 0; // null pointer
    initElementalMembers(); //

    // Resize all vectors to maximum period
    const int maxper = scenario->getModeltime()->getmaxper();
    gnp.resize( maxper ); // regional gross national product normalized
    gnpAdj.resize( maxper ); // regional gross national product adjusted for energy and normalized
    gnpCap.resize( maxper ); // regional gross national product per capita and normalized
    gnpDol.resize( maxper ); // regional gross national product in dollar value
    input.resize( maxper ); // total fuel and energy consumption
    TFEcalb.resize( maxper ); // Total Final Energy calibration value
    priceSer.resize( maxper ); // aggregate price for demand services
    carbonTaxPaid.resize( maxper ); // total regional carbon taxes paid
    summary.resize( maxper ); // summary object for reporting
    carbonTax.resize( maxper ); // fixed carbon tax
    calibrationGNPs.resize( maxper ); // GNPs for calibration
    iElasticity.resize( maxper ); // income elasticity for region
}

//! Default destructor destroys sector, demsector, Resource, agSector, and population objects.
Region::~Region() {
    clear();
}

//! Clear member variables and initialize elemental members.
void Region::clear(){
    
    // Free memory.
    for ( vector<Sector*>::iterator secIter = supplySector.begin(); secIter != supplySector.end(); secIter++ ) {
        delete *secIter;
    }

    for ( vector<DemandSector*>::iterator demIter = demandSector.begin(); demIter != demandSector.end(); demIter++ ) {
        delete *demIter;
    }

    for ( vector<Resource*>::iterator rescIter = resources.begin(); rescIter != resources.end(); rescIter++ ) {
        delete *rescIter;
    }

    if ( agSector != 0 ) {
        delete agSector;	
    }

    delete population;
}

//! Initialize elemental data members.
void Region::initElementalMembers(){
    noGhg = 0;
    numResources = 0;
    noSSec = 0;
    noDSec = 0;
    noRegMrks = 0;
    EnergyGNPElas = 0;
}

/*! Return the region name.
* \return The string name of the region is returned.
*/
string Region::getName() const {
    return name;
}

/*! 
* \brief Sets the data members from the XML input.  This function parses all XML data from Region down to the lowest set of objects.
*  As the XML data is parsed, new objects are continually added to the object container using the push_back routine.
*
* \param node XML DOM node of the region
*/
void Region::XMLParse( const DOMNode* node ){

    int i;
    int j;
    string nodeName;
    string nodeNameChild;
    DOMNode* curr = 0;
    DOMNode* currChild = 0;
    DOMNodeList* nodeListChild = 0;
    Resource* tempResource = 0;
    Sector* tempSupSector = 0;
    DemandSector* tempDemSector = 0;
    tranSector* tempTranSector = 0;
    GHGPolicy* tempGhgMrk = 0;
    map<string,int>::const_iterator resourceIter;

    const Modeltime* modeltime = scenario->getModeltime();

    // make sure we were passed a valid node.
    assert( node );

    // get the name attribute.
    name = XMLHelper<string>::getAttrString( node, "name" );

#if ( _DEBUG )
    cout << "Region name set as " << name << endl;
#endif

    // get all child nodes.
    DOMNodeList* nodeList = node->getChildNodes();

    // loop through the child nodes.
    for( i = 0; i < static_cast<int>( nodeList->getLength() ); i++ ){
        curr = nodeList->item( i );
        nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }

        else if( nodeName == "e_GNP_elas" ){
            EnergyGNPElas = XMLHelper<double>::getValue( curr ); 
        }

        else if( nodeName == "PrimaryFuelCO2Coef" ) {
            primaryFuelCO2Coef[ XMLHelper<string>::getAttrString( curr, "name" ) ] = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "CarbonTaxFuelCoef" ) {
            carbonTaxFuelCoef[ XMLHelper<string>::getAttrString( curr, "name" ) ] = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "demographics" ){
            if( population == 0 ) {
                population = new demographic();
            }
            population->XMLParse( curr ); // only one demographics object.
        }
        else if( nodeName == "depresource" ){
            resourceIter = resourceNameMap.find( XMLHelper<string>::getAttrString( curr, "name" ) );
            if( resourceIter != resourceNameMap.end() ){
                // The resource already exists.
                resources[ resourceIter->second ]->XMLParse( curr );
            }
            else {
                tempResource = new DepletableResource();
                tempResource->XMLParse( curr );
                resources.push_back( tempResource );
                resourceNameMap[ tempResource->getName() ] = static_cast<int>( resources.size() ) - 1;
            }
        }
        else if( nodeName == "fixedresource" ){
            map<string,int>::const_iterator resourceIter = resourceNameMap.find( XMLHelper<string>::getAttrString( curr, "name" ) );
            resourceIter = resourceNameMap.find( XMLHelper<string>::getAttrString( node, "name" ) );
            if( resourceIter != resourceNameMap.end() ){
                // The resource already exists.
                resources[ resourceIter->second ]->XMLParse( curr );
            }
            else {
                tempResource = new FixedResource();
                tempResource->XMLParse( curr );
                resources.push_back( tempResource );
                resourceNameMap[ tempResource->getName() ] = static_cast<int>( resources.size() ) - 1;
            }
        }
        else if( nodeName == "renewresource" ){
            resourceIter = resourceNameMap.find( XMLHelper<string>::getAttrString( curr, "name" ) );
            if( resourceIter != resourceNameMap.end() ){
                // The resource already exists.
                resources[ resourceIter->second ]->XMLParse( curr );
            }
            else {
                tempResource = new RenewableResource();
                tempResource->XMLParse( curr );
                resources.push_back( tempResource );
                resourceNameMap[ tempResource->getName() ] = static_cast<int>( resources.size() ) - 1;
            }
        }
        else if( nodeName == "supplysector" ){
            map<string,int>::const_iterator supplySectorIter = supplySectorNameMap.find( XMLHelper<string>::getAttrString( curr, "name" ) );
            if( supplySectorIter != supplySectorNameMap.end() ) {
                // The supply sector already exists.
                supplySector[ supplySectorIter->second ]->XMLParse( curr );
            }
            else {
                tempSupSector = new Sector( name );
                tempSupSector->XMLParse( curr );
                supplySector.push_back( tempSupSector );
                supplySectorNameMap[ tempSupSector->getName() ] = static_cast<int>( supplySector.size() ) - 1;
            }
        }
        else if( nodeName == "demandsector" ){
            map<string,int>::const_iterator demandSectorIter = demandSectorNameMap.find( XMLHelper<string>::getAttrString( curr, "name" ) );
            if( demandSectorIter != demandSectorNameMap.end() ) {
                // The demand sector already exists.
                demandSector[ demandSectorIter->second ]->XMLParse( curr );
            }
            else {
                tempDemSector = new DemandSector( name );
                tempDemSector->XMLParse( curr );
                demandSector.push_back( tempDemSector );
                demandSectorNameMap[ tempDemSector->getName() ] = static_cast<int>( demandSector.size() ) - 1;
            }
        }
        // transportation sector is contained in demandSector
        else if( nodeName == "tranSector" ){
            map<string,int>::const_iterator demandSectorIter = demandSectorNameMap.find( XMLHelper<string>::getAttrString( node, "name" ) );
            if( demandSectorIter != demandSectorNameMap.end() ) {
                // The demand sector already exists.
                demandSector[ demandSectorIter->second ]->XMLParse( curr );
            }
            else {
                tempTranSector = new tranSector( name );
                tempTranSector->XMLParse( curr );
                demandSector.push_back( tempTranSector );
                demandSectorNameMap[ tempTranSector->getName() ] = static_cast<int>( demandSector.size() ) - 1;
            }
        } 
        else if( nodeName == "agsector" ) {
            if( Configuration::getInstance()->getBool( "agSectorActive" ) ){
                if( agSector == 0 ) {
                    agSector = new AgSector();
                    agSector->XMLParse( curr );
                }
            }
        }
        else if( nodeName == "ghgpolicy" ){
            map<string,int>::const_iterator ghgMarketIter = ghgMarketNameMap.find( XMLHelper<string>::getAttrString( curr, "name" ) );
            if( ghgMarketIter != ghgMarketNameMap.end() ) {
                // ghg market already exists.
                ghgMarket[ ghgMarketIter->second ]->XMLParse( curr );
            }
            else {
                tempGhgMrk = new GHGPolicy();
                tempGhgMrk->XMLParse( curr );
                ghgMarket.push_back( tempGhgMrk );
                ghgMarketNameMap[ tempGhgMrk->getName() ] = static_cast<int>( ghgMarket.size() ) - 1;
            }
        }
        // regional taxes
        else if( nodeName == "taxes" ){
            // get all child nodes.
            nodeListChild = curr->getChildNodes();
            // loop through the child nodes.
            for(j = 0; j < static_cast<int>( nodeListChild->getLength() ); j++ ){
                currChild= nodeListChild->item( j );
                nodeNameChild = XMLHelper<string>::safeTranscode( currChild->getNodeName() );

                if( nodeNameChild == "#text" ) {
                    continue;
                }
                else if( nodeNameChild == "carbonTax" ) {
                    XMLHelper<double>::insertValueIntoVector( currChild, carbonTax, modeltime );
                }
                else {
                    cout << "Unrecognized text string: " << nodeNameChild << " found while parsing region->taxes." << endl;
                }
            }
        }
        // regional economic data
        else if( nodeName == "economicdata" ){
            // get all child nodes.
            nodeListChild = curr->getChildNodes();
            // loop through the child nodes.
            for(j = 0; j < static_cast<int>( nodeListChild->getLength() ); j++ ){
                currChild = nodeListChild->item( j );
                nodeNameChild = XMLHelper<string>::safeTranscode( currChild->getNodeName() );

                if( nodeNameChild == "#text" ) {
                    continue;
                }
                else if( nodeNameChild == "GNP" ) {
                    XMLHelper<double>::insertValueIntoVector( currChild, calibrationGNPs, modeltime );
                }
                else if(nodeNameChild == "incomeelasticity") {
                    XMLHelper<double>::insertValueIntoVector( currChild, iElasticity, modeltime );
                }

                else if(nodeNameChild == "TFEcalb") {
                    XMLHelper<double>::insertValueIntoVector( currChild, TFEcalb, modeltime );
                }
                else {
                    cout << "Unrecognized text string: " << nodeNameChild << " found while parsing region->economicdata." << endl;
                }

            }
        }

        else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing region." << endl;
        }
    }
}

/*! Complete the initialization.  Get the size of vectors, initialize AGLU, create all markets, call complete initialization 
*  functions for nested objects, update the fuel map, and find simultaneities.
* \todo I think since there is one indirect ghg object for each sector, it might be better in sector. This may require deriving supply sector.
*/
void Region::completeInit() {

    int i = 0;

    Configuration* conf = Configuration::getInstance();

    gnpDol[ 0 ] = calibrationGNPs[ 0 ];

    numResources = static_cast<int>( resources.size() );
    noSSec = static_cast<int>( supplySector.size() );
    noDSec = static_cast<int>( demandSector.size() );
    noGhg = static_cast<int>( ghgMarket.size() );
    
    // Need to perform the resize by iteratively adding each one so we can set the sector name. 
    for( vector<Sector*>::const_iterator sectorIter = supplySector.begin(); sectorIter != supplySector.end(); ++sectorIter ){
        Emcoef_ind temp( ( *sectorIter )->getName() );
        emcoefInd.push_back( temp );
    }
    
    // emcoefInd.resize( noSSec ); // indirect GHG coef object for every supply sector
    
    // Finish initializing agLu
    if( conf->getBool( "agSectorActive" ) ){
        agSector->setGNP( calcFutureGNP() );
        agSector->setPop( population->getTotalPopVec() );
    }
    // create markets and set market indeces
    // Resource markets, pass region name
    for( i = 0; i < numResources; i++ ){
        resources[ i ]->setMarket( name );
    }

    // ghg markets, pass region name
    for( i = 0; i < noGhg; i++ ){
        ghgMarket[i]->setMarket( name );
    }

    // supply sector markets, pass region name
    for( i = 0;i < noSSec; i++ ){
        supplySector[ i ]->setMarket();
    }

    // Create AgLU markets
    if( conf->getBool( "agSectorActive" ) ){
        agSector->setMarket( name );
    }

    // Complete the initializations.
    for( vector<Resource*>::iterator resourceIter = resources.begin(); resourceIter != resources.end(); resourceIter++ ) {
        ( *resourceIter )->completeInit();
    }

    for( vector<Sector*>::iterator supplySectorIter = supplySector.begin(); supplySectorIter != supplySector.end(); supplySectorIter++ ) {
        ( *supplySectorIter )->completeInit();

    }

    for( vector<DemandSector*>::iterator demandSectorIter = demandSector.begin(); demandSectorIter != demandSector.end(); demandSectorIter++ ) {
        ( *demandSectorIter )->completeInit();
    }

    // Find simuls.
    updateSummary( 0 );	// Dummy call to final supply to setup fuel map
    findSimul( 0 );

    // Setup each sector for sorting. 
    for( vector<Sector*>::iterator supplySectorSortIter = supplySector.begin(); supplySectorSortIter != supplySector.end(); supplySectorSortIter++ ){
        ( *supplySectorSortIter )->setupForSort( this );
    }

    // Now sort the sectors by dependency.
    std::sort( supplySector.begin(), supplySector.end(), Sector::DependencyOrdering() );
}

/*! 
* \brief Write datamembers to datastream in XML format. Calls XMLWriteElement function from the XMLHelper class for the actual writing.
* \param out Output file in XML format.
* \note 
* \ref faqitem1 
*/
void Region::toXML( ostream& out ) const {

    const Modeltime* modeltime = scenario->getModeltime();

    int m;

    // write the beginning tag.
    Tabs::writeTabs( out );
    out << "<region name=\"" << name << "\">"<< endl;

    // increase the indent.
    Tabs::increaseIndent();

    // Write out gnp energy elasticity.
    XMLWriteElementCheckDefault( EnergyGNPElas, "e_GNP_elas", out, 0 );

    // Write out the Co2 Coefficients. 
    for( map<string,double>::const_iterator coefAllIter = primaryFuelCO2Coef.begin(); coefAllIter != primaryFuelCO2Coef.end(); coefAllIter++ ) {
        XMLWriteElement( coefAllIter->second, "PrimaryFuelCO2Coef", out, 0, coefAllIter->first );
    }

    for( map<string,double>::const_iterator coefPriIter = carbonTaxFuelCoef.begin(); coefPriIter != carbonTaxFuelCoef.end(); coefPriIter++ ) {
        XMLWriteElement( coefPriIter->second, "CarbonTaxFuelCoef", out, 0, coefPriIter->first );
    }
    // write the xml for the class members.
    // write out the single population object.
    population->toXML( out );

    // write out the resources objects.
    for( vector<Resource*>::const_iterator i = resources.begin(); i != resources.end(); i++ ){
        ( *i )->toXML( out );
    }

    // write out supply sector objects.
    for( vector<Sector*>::const_iterator j = supplySector.begin(); j != supplySector.end(); j++ ){
        ( *j )->toXML( out );
    }

    // write out demand sector objects.
    for( vector<DemandSector*>::const_iterator k = demandSector.begin(); k != demandSector.end(); k++ ){
        ( *k )->toXML( out );
    }

    Tabs::writeTabs( out );
    out << "<agsector/>" << endl;

    if( agSector != 0 ){
        agSector->toXML( out );
    }
    // write out ghgMarket objects.
    for( vector<GHGPolicy*>::const_iterator l = ghgMarket.begin(); l != ghgMarket.end(); l++ ){
        ( *l )->toXML( out );
    }

    // Write out regional economic data
    Tabs::writeTabs( out );
    out << "<economicdata>" << endl;

    Tabs::increaseIndent();

    // write out GNP
    for( m = 0; m < static_cast<int>( gnpDol.size() ); m++ ){
        XMLWriteElementCheckDefault( gnpDol[ m ], "GNP", out, 0, modeltime->getper_to_yr( m ) );
    }

    // write out income elasticity
    for( m = 0; m < static_cast<int>( iElasticity.size() ); m++ ) {
        XMLWriteElementCheckDefault( iElasticity[ m ],"incomeelasticity", out, 0, modeltime->getper_to_yr( m ) );
    }

    // write out TFE calibration values
    for( m = 0; m < static_cast<int>( TFEcalb.size() ); m++ ) {
        if ( TFEcalb[ m ] != 0 ) {
            XMLWriteElementCheckDefault( TFEcalb[ m ],"TFEcalb", out, 0, modeltime->getper_to_yr( m ) );
        }
    }

    Tabs::decreaseIndent();
    Tabs::writeTabs( out );
    out << "</economicdata>" << endl;
    // End write out regional economic data

    // Write out regional taxes
    Tabs::writeTabs( out );
    out << "<taxes>"<< endl;
    Tabs::increaseIndent();

    // Write out regional fixed taxes
    for( m = 0; m < static_cast<int>( carbonTax.size() ); m++ ){
        XMLWriteElementCheckDefault( carbonTax[ m ],"carbonTax", out, 0, modeltime->getper_to_yr( m ) );			
    }

    Tabs::decreaseIndent();
    Tabs::writeTabs( out );
    out << "</taxes>"<< endl;
    // End write out regional taxes

    // finished writing xml for the class members.

    // decrease the indent.
    Tabs::decreaseIndent();

    // write the closing tag.
    Tabs::writeTabs( out );
    out << "</region>" << endl;
}

/*! \brief Write datamembers to datastream in XML format for debugging purposes.  
* Calls XMLWriteElement function from the XMLHelper class for the actual writing.
*  Calls debug functions in other contained objects. 
*
* \param period Model time period
* \param out Output file for debugging purposes in XML format
*
*/
void Region::toDebugXML( const int period, ostream& out ) const {

    // write the beginning tag.
    Tabs::writeTabs( out );
    out << "<region name=\"" << name << "\">"<< endl;

    // increase the indent.
    Tabs::increaseIndent();

    // write out basic datamembers
    XMLWriteElement( noGhg, "noGhg", out );
    XMLWriteElement( numResources, "numResources", out );
    XMLWriteElement( noSSec, "noSSec", out );
    XMLWriteElement( noDSec, "noDSec", out );
    XMLWriteElement( noRegMrks, "noRegMrks", out );
    XMLWriteElement( gnp[ period ], "gnp", out );
    XMLWriteElement( gnpCap[ period ], "gnpPerCapita", out );
    XMLWriteElement( gnpDol[ period ], "gnpDollarValue", out);
    XMLWriteElement( calibrationGNPs[ period ], "calibrationGNPs", out );
    XMLWriteElement( gnpAdj[ period ], "gnpAdj", out );
    XMLWriteElement( input[ period ], "input", out );
    XMLWriteElement( priceSer[ period ], "priceSer", out );
    XMLWriteElement( carbonTaxPaid[ period ], "carbonTaxPaid", out );
    // Write out gnp energy elasticity.
    XMLWriteElement( EnergyGNPElas, "e_GNP_elas", out );

    // Write out the Co2 Coefficients. 
    for( map<string,double>::const_iterator coefAllIter = primaryFuelCO2Coef.begin(); coefAllIter != primaryFuelCO2Coef.end(); coefAllIter++ ) {
        XMLWriteElement( coefAllIter->second, "PrimaryFuelCO2Coef", out, 0, coefAllIter->first );
    }

    for( map<string,double>::const_iterator coefPriIter = carbonTaxFuelCoef.begin(); coefPriIter != carbonTaxFuelCoef.end(); coefPriIter++ ) {
        XMLWriteElement( coefPriIter->second, "CarbonTaxFuelCoef", out, 0, coefPriIter->first );
    }
    // write the xml for the class members.
    // write out the single population object.
    population->toDebugXML( period, out );

    // write out the resources objects.
    for( vector<Resource*>::const_iterator i = resources.begin(); i != resources.end(); i++ ){
        ( *i )->toDebugXML( period, out );
    }

    // write out supply sector objects.
    for( vector<Sector*>::const_iterator j = supplySector.begin(); j != supplySector.end(); j++ ){
        ( *j )->toDebugXML( period, out );
    }

    // write out demand sector objects.
    for( vector<DemandSector*>::const_iterator k = demandSector.begin(); k != demandSector.end(); k++ ){
        ( *k )->toDebugXML( period, out );
    }

    // Write out the single agSector object.
    // agSector->toDebugXML( period, out );

    // write out ghgMarket objects.
    for( vector<GHGPolicy*>::const_iterator l = ghgMarket.begin(); l != ghgMarket.end(); l++ ){
        ( *l )->toDebugXML( period, out );
    }

    // Write out summary object.
    //summary[ period ].toDebugXML( period, out ); // is this vector by period?

    // Write out regional economic data.
    Tabs::writeTabs( out );
    out << "<economicdata>" << endl;
    Tabs::increaseIndent();

    // Write out GNP.
    XMLWriteElement( gnpDol[ period ], "gnpDol", out );

    // Write out income elasticity.
    XMLWriteElement( iElasticity[ period ],"iElasticity", out );

    Tabs::decreaseIndent();
    Tabs::writeTabs( out );
    out << "</economicdata>"<< endl;
    // End write out regional economic data

    // Write out regional taxes.
    Tabs::writeTabs( out );
    out << "<taxes>"<< endl;
    Tabs::increaseIndent();

    // Write out regional fixed taxes.
    XMLWriteElement( carbonTax[ period ], "carbonTax", out );			

    Tabs::decreaseIndent();
    Tabs::writeTabs( out );
    out << "</taxes>"<< endl;
    // End write out regional taxes

    // Finished writing xml for the class members.

    // decrease the indent.
    Tabs::decreaseIndent();

    // write the closing tag.
    Tabs::writeTabs( out );
    out << "</region>" << endl;
}

//! Initialize calibration markets.
/* \todo Calibration shouldn't be in the population object.
*/
void Region::setupCalibrationMarkets() {
    population->setupCalibrationMarkets( name );
}

/*! Run the agLu Model and determine CO2 emitted.
* \param period Model time period
*/
void Region::calcAgSector( const int period ) {
    agSector->runModel( period, name );
    agSector->carbLand( period, name );
}

/*! \brief Set regional ghg constraint from input data to market supply.
*
* \param period Model time period
*/
void Region::setGhgSupply( const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();

    string ghgName;
    double ghgtarget;

    for (int i=0;i<noGhg;i++) {
        ghgName = ghgMarket[i]->getName();
        ghgtarget = ghgMarket[i]->getConstraint(period);
        marketplace->addToSupply(ghgName,name,ghgtarget,period);		
    }
}

/*! Set regional ghg tax to individual technologies.
*
* \param period Model time period
*/
void Region::addGhgTax( const int period ) {
    string ghgname;
    int i,j,k;

    for (i=0;i<noGhg;i++) {
        ghgname = ghgMarket[i]->getName();
        for (j=0;j<noSSec;j++) {
            supplySector[j]->addGhgTax( ghgname, period );
        }
        for (k=0;k<noDSec;k++) {
            demandSector[k]->addGhgTax( ghgname, period );
        }
    }
}


/*! Calculates annual supply of primay resources.
*
* \param period Model time period
*/
void Region::rscSupply( const int period )  {
    Marketplace* marketplace = scenario->getMarketplace();
    string goodName;
    string regionName = name; // name is Region attribute
    double prev_price = 0;
    double prev_gdp = 0;
    double price = 0;
    //for (int i=0;i<numResources-1;i++) {
    for (int i=0;i<numResources;i++) {
        goodName = resources[i]->getName();
        price = marketplace->getPrice(goodName,regionName,period); // get market price
        if (period==0) {
            prev_price = price;
            prev_gdp = gnp[period];
        }
        else {
            prev_price = marketplace->getPrice(goodName,regionName,period-1); // get market price
            prev_gdp = gnp[period-1];
        }

        // calculate annual supply
        resources[i]->annualsupply(period,gnp[period],prev_gdp,price,prev_price);

        // set market supply of resources used for solution mechanism
        marketplace->addToSupply(goodName,regionName,resources[i]->getAnnualProd(period),period);

    }
}

/*! Calculate prices of refined fuels and electricity.
*
* \param period Model time period
*/
void Region::finalSupplyPrc( const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();
    string goodName;
    double goodPrice;

    for (int i=0;i<noSSec;i++) {
        goodName = supplySector[i]->getName();
       
        // name is region or country name
        supplySector[i]->calcShare( period );
        goodPrice = supplySector[ i ]->getPrice( period );
        // set market price of intermediate goods
        // name is region or country name
        marketplace->setPrice( goodName, name, goodPrice, period );
    }
}

/*! Calculates supply of final energy and other goods.
*
* \param period Model time period
*/
void Region::finalSupply( const int period ) {

    Marketplace* marketplace = scenario->getMarketplace();
    string goodName;
    int i = 0;
    int j = 0;
    double mrksupply;


    // loop through all sectors once to get total output
    for ( vector<Sector*>::reverse_iterator ri = supplySector.rbegin(); ri != supplySector.rend(); ri++ ) {

        goodName = ( *ri )->getName();		

        // name is country/region name
        ( *ri )->supply( period );
        double sectorOutput = ( *ri )->getOutput(period);

        carbonTaxPaid[period] += ( *ri )->getTotalCarbonTaxPaid(period);
    }

    // loop through supply sectors and assign supplies to marketplace and update fuel consumption map
    // the supplies in the market sector are, at present, not used except to double check 
    // that the output of the supply sectors does equal supply

    for (i=0;i<noSSec;i++) {
        // name is country/region name
        //supplySector[j].supply(name,no,period);
        // supply and demand for intermediate and final good are set equal
        goodName = supplySector[i]->getName();
        mrksupply = supplySector[i]->getOutput(period);

        // set market supply of intermediate goods
        marketplace->addToSupply(goodName,name,mrksupply,period);

        // update sector input
        // supplySector[ i ]->sumInput( period );
    }
}

/*! Calculate regional gnp.
*
* \param period Model time period
*/
void Region::calcGnp( const int period ) {

    const Modeltime* modeltime = scenario->getModeltime();
    const int baseYear = modeltime->getstartyr();
    const int basePer = modeltime->getyr_to_per(baseYear);

    if ( period == modeltime->getyr_to_per( baseYear ) ) {
        gnp[ period ] = 1.0; // normalize to 1975
    }
    else {
        double currentLF = population->getlaborforce( period );
        double lastLF = population->getlaborforce( period - 1 );
        double tlab = population->getTotalLaborProductivity( period );
        gnp[ period ] = gnp[ period - 1 ] * tlab * ( currentLF / lastLF );
        if (gnp[period] == 0) {
            cerr << "error with GNP calculation:  currentLF: " << currentLF
                << "  lastLF: " << lastLF << "  lab: " << tlab << "\n";
        }
    }


    // gnp period capita normalized
    // correct using energy adjusted gnp*****
    gnpCap[ period ] = gnp[ period ] * population->total( basePer ) / population->total( period );
}

//! Calculate a forward looking gnp.
const vector<double> Region::calcFutureGNP() const {
    const Modeltime* modeltime = scenario->getModeltime();
    vector<double> gnps;
    double laborProd = 0;
    double currentLaborForce = 0;
    double lastLaborForce = 0;
    double tlab = 0;

    assert( calibrationGNPs.size() > 1 );

    const double baseYearConversion = calibrationGNPs[ 1 ] / calibrationGNPs[ 0 ];

    gnps.resize( modeltime->getmaxper() );

    for ( int period = 0; period < modeltime->getmaxper(); period++ ) {
        if ( period == 0 ) { // Normalize to the base year.
            gnps[ 0 ] = 1.0;
        }

        else if ( static_cast<int>( calibrationGNPs.size() ) > period && calibrationGNPs[ period ] > 0 ){
            gnps[ period ] = calibrationGNPs[ period ] / baseYearConversion;
        }
        else {
            laborProd = 1 + population->labor( period );
            currentLaborForce = population->getlaborforce( period );
            lastLaborForce = population->getlaborforce( period - 1 );
            tlab = pow( laborProd, modeltime->gettimestep( period ) );
            gnps[ period ] = gnps[ period - 1 ] * tlab * ( currentLaborForce / lastLaborForce );
            assert( gnps[ period ] != 0 );
        }
    }

    for ( vector<double>::iterator iter = gnps.begin(); iter != gnps.end(); iter++ ){
        *iter *= baseYearConversion / 1000000;
    }
    return gnps;
}

/*! Calculate regional GNP using laborforce participation and labor productivity.
*
* \param period Model time period
*/
void Region::calcGNPlfp( const int period ) {
    const Modeltime* modeltime = scenario->getModeltime();
    double labprd=0;
    const int baseYear = modeltime->getstartyr();
    const int basePer = modeltime->getyr_to_per(baseYear);
    if (period==modeltime->getyr_to_per(baseYear)) {
        gnp[period] = 1.0;
    }
    else {
        // 1 + labor productivity growth rate
        // population->labor returns labor productivity growth rate
        labprd = 1 + population->labor(period);
        double tlabprd = pow(labprd,modeltime->gettimestep(period));
        gnp[period] = gnp[period-1] * tlabprd * ( population->getlaborforce(period)
            / population->getlaborforce(period-1) );
        if (gnp[period] == 0) {
            cerr << "error with GNP calculation:  labor force(period): " 
                << population->getlaborforce(period)
                << "  labor force(period-1): " << population->getlaborforce(period-1) 
                << "  labor productivity: " << tlabprd << "\n";
        }
    }
    // gnp period capita normalized
    // correct using energy adjusted gnp*****
    gnpCap[period] = gnp[period]*population->total(basePer)/population->total(period);

}

/*! Calculate demand sector aggregate price.
*
* \param period Model time period
*/
void Region::calcEndUsePrice( const int period ) {

    priceSer[ period ] = 0;

    for ( int i = 0; i < noDSec; i++ ) {
        demandSector[ i ]->calcShare( period, gnpCap[ period ] );		

        // calculate service price for each demand sector
        // demandsector[ i ]->price( period ); Protected and moved to getPrice function

        // calculate aggregate service price for region
        priceSer[ period ] += demandSector[ i ]->getOutput( 0 ) * demandSector[ i ]->getPrice( period );

        // calculate service price elasticity for each demand sector
        // or use read in value, temporary code
        bool useReadinData = true;
        // do nothing if false
        if (!useReadinData) {
            demandSector[ i ]->calc_pElasticity( period );
        } 
    }
}

/*! Adjust regional gnp for energy.
*
* \param period Model time period
*/
void Region::adjustGnp( const int period ) {
    const Modeltime* modeltime = scenario->getModeltime();

    const int baseYear = modeltime->getstartyr();
    double tempratio;
    if (period<=modeltime->getyr_to_per(1990)) {
        gnpAdj[period] = gnp[period];
    }
    else {
        // adjust gnp using energy cost changes and 
        // energy to gnp feedback elasticity
        tempratio = priceSer[period]/priceSer[period-1];
        if (tempratio != tempratio) {
            cerr << " Error: priceSer[period] = " << priceSer[period];
            cerr << " and, priceSer[period-1] = " << priceSer[period-1] << endl;
        }
        try {
            gnpAdj[period] = gnp[period]*pow(tempratio,EnergyGNPElas);
        } catch(...) {
            cerr << "Error calculating gnpAdj in region.adjust_gnp()\n";
        }
    }

    // calculate dollar value gnp using base year dollar value GNP
    if ( period > modeltime->getyr_to_per( baseYear ) ){ 
        gnpDol[ period ] = gnpAdj[ period ] * gnpDol[ modeltime->getyr_to_per( baseYear ) ];
    }
}

/*! Write back the calibrated values from the marketplace into the member variables. 
*
* \param period Model time period
*/
void Region::writeBackCalibratedValues( const int period ) {
    population->writeBackCalibratedValues( name, period );
}

//! Do regional calibration
/*! Must be done after demands are calculated. 
Two levels of calibration are possible. 
First at the sector or technology level (via. calibrateSector method),
or, at the level of total final energy demand (via calibrateTFE)
*
* \param doCalibrations Boolean for running or not running calibration routine
* \param period Model time period
*/
void Region::calibrateRegion( const bool doCalibrations, const int period ) {
    int i;

    // Do subsector and technology level energy calibration
    // can only turn off calibrations that do not involve markets
    if ( doCalibrations ) {
        // Calibrate demand sectors
        for ( i=0;i<noDSec;i++) {
            demandSector[ i ]->calibrateSector( period );
        }

        // Calibrate supply sectors
        for ( i=0;i<noSSec;i++) {
            supplySector[ i ]->calibrateSector( period );
        }
    }

    // Calibrate Regional TFE
    if ( doCalibrations ) {
        if ( !isDemandAllCalibrated( period ) ) {
            calibrateTFE( period );
        } else {
            // do nothing now. Need to make a variant of the total cal outputs function
            // so that can compare TFE with cal value 
        }
    }

    // Set up the GDP calibration. Need to do it each time b/c of nullsup call in marketplace.
    // Insert the newly calculated values into the calibration markets. 
    if( static_cast<int>( calibrationGNPs.size() ) > period && calibrationGNPs[ period ] > 0 ){ 
        const string goodName = "GDP";
        Marketplace* marketplace = scenario->getMarketplace();
        marketplace->addToDemand( goodName, name, calibrationGNPs[ period ], period );
        marketplace->addToSupply( goodName, name, gnpDol[ period ], period );
        marketplace->setMarketToSolve( goodName, name );
    }
}

/*! Returns true if all demand sectors are calibrated (or fixed)
*
* \param period Model time period
*/
bool Region::isDemandAllCalibrated( const int period ) const {
    bool allCalibrated = true;

    for ( int i = 0; i < noDSec; i++ && allCalibrated ) {
        if ( !demandSector[ i ]->isAllCalibrated( period ) ) {
            allCalibrated = false;
        }
    }

    return allCalibrated;
}

//! Calibrate total final energy Demand for this region.
/*! Adjusts AEEI in each demand sector until TFE is equal to the calibration value.
*/
void Region::calibrateTFE( const int period ) {
    int i;

    // Calculate total final energy demand for all demand sectors
    double totalFinalEnergy = 0;
    for ( i=0;i<noDSec;i++) {
        totalFinalEnergy += demandSector[ i ]->getInput( period );;
    }

    // Don't calibrate unless non zero value of TFE
    if ( TFEcalb[ period ]  > 0 ) {
        // Ratio of TFE in sector to cal value
        double TFEtemp = TFEcalb[ period ];
        double scaleFactor = TFEcalb[ period ] / totalFinalEnergy;

        if ( totalFinalEnergy == 0 ) {
            cout << "ERROR: totalFinalEnergy = 0 in region " << name << endl;
        }

        //   cout << name << ":  TFE Calib: " << TFEcalb[ period ] << "; TFE: " << totalFinalEnergy << endl;

        // Scale each sector's output to appraoch calibration value
        for ( i=0;i<noDSec;i++) {
            if ( !demandSector[ i ]->isAllCalibrated( period ) ) {
                demandSector[ i ]->scaleOutput( period , scaleFactor );
            }
        }
    }
}

//! Call any initializations that are only done once per period
// \todo put somewhere (maybe not here) a check for prev period to see how well calibrations worked
void Region::initCalc( const int period ) 
{
    int i;
    for ( i=0;i<noDSec;i++) {
        demandSector[ i ]->initCalc( period ); 
    }

    for ( i=0;i<noSSec;i++) {
        supplySector[ i ]->initCalc( period ); 
    }
}

//! Calculate regional demand for energy and other goods for all sectors.
void Region::enduseDemand( const int period ) {
    carbonTaxPaid[period] = 0; // initialize total regional carbon taxes paid

    // gnpCap using energy adjusted gnp
    gnpCap[period] = gnpAdj[period]*population->total(0)/population->total(period);

    // This is an early point in the calcuation and, thus, a good point for a NaN error check.
    if ( gnpCap[period] != gnpCap[period] ) {
        cerr << "Error in Region: " << name << ", bad value. " ;
        cerr << " gnpCap[period] = " << gnpCap[period];
        cout << " gnpAdj[period] = " << gnpAdj[period] << endl;
    }

    for (int i=0;i<noDSec;i++) {
        // calculate aggregate demand for end-use sector services
        // set fuel demand from aggregate demand for services
        // name is region or country name
        demandSector[ i ]->aggdemand( gnpCap[period], gnpAdj[period], period ); 
        carbonTaxPaid[ period ] += demandSector[ i ]->getTotalCarbonTaxPaid( period );

        // update sector input
        // sjs -- moved to getInput ( but that may never be called! Don't think input var is ever used.)
        // demandSector[ i ]->sumInput( period );
    }
}

//! Apply carbon taxes to appropriate sectors.
void Region::applycarbontax( const int period )
{
    int i=0;
    // apply carbon taxes by period to primary fossil fuel user only
    for (i=0;i<noSSec;i++)
        supplySector[i]->applycarbontax( carbonTax[period],period);
    for (i=0;i<noDSec;i++)
        demandSector[i]->applycarbontax( carbonTax[period],period);
}

//! Calculate regional emissions from resources.
void Region::emission( const int period )
{
    int i=0;

    summary[period].clearemiss(); // clear emissions map

    // need to call emissions function but sum is not needed
    for (i=0;i<noSSec;i++) {
        supplySector[i]->emission(period);
        summary[period].updateemiss(supplySector[i]->getemission(period));
        emcoefInd[i].setemcoef(supplySector[i]->getemfuelmap(period), 
            supplySector[i]->getOutput(period));
    }
    for (i=0;i<noDSec;i++) {
        demandSector[i]->emission(period);
        summary[period].updateemiss(demandSector[i]->getemission(period));
    }
}

//! Calculate regional emissions by fuel for reporting
void Region::calcEmissFuel( const int period )
{
    map<string, double> fuelemiss; // tempory emissions by fuel
    const vector<string> primaryFuelList = scenario->getWorld()->getPrimaryFuelList();

    for( vector<string>::const_iterator fuelIter = primaryFuelList.begin(); fuelIter != primaryFuelList.end(); fuelIter++ ) {
        fuelemiss[ *fuelIter ] = summary[period].get_pemap_second( *fuelIter ) * primaryFuelCO2Coef[ *fuelIter ];
    }

    summary[period].updateemiss(fuelemiss); // add CO2 emissions by fuel
}

//! Calculate regional indirect emissions from intermediate and final demand sectors.
void Region::emissionInd( const int period )
{
    int i;
    // calculate indirect GHG emissions
    for (i=0;i<noSSec;i++)
        supplySector[i]->indemission( period, emcoefInd );
    for (i=0;i<noDSec;i++) 
        demandSector[i]->indemission( period, emcoefInd );
}

//! Set regional GHG emissions as market demand.
void Region::setGhgDemand( const int period )
{
    double ghgemiss;
    string ghgname;

    for (int i=0;i<noGhg;i++) {
        ghgname = ghgMarket[i]->getName();
        if(ghgname == "CO2") {
            ghgemiss = summary[period].get_emissmap_second("CO2");
            ghgMarket[i]->setEmission(ghgemiss,period);
        }
        else if(ghgname == "CH4") {
            ghgemiss = summary[period].get_emissmap_second("CH4");
            ghgMarket[i]->setEmission(ghgemiss,period);
        }
    }
}	

//! Write all outputs to file.
void Region::outputFile() const {
    const Modeltime* modeltime = scenario->getModeltime();
    int i=0;
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    // function protocol
    void fileoutput3(string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    // write population results to database
    population->outputfile( name );
    // write gnp and adjusted gnp for region
    fileoutput3(name," "," "," ","GNP","Bil90US$",gnpDol);
    fileoutput3(name," "," "," ","GNP","norm",gnp);
    fileoutput3(name," "," "," ","GNP","energy adj",gnpAdj);
    // regional carbon taxes
    fileoutput3(name," "," "," ","C tax (fixed)","90$/TC",carbonTax);
    // regional total carbon taxes paid
    fileoutput3(name," "," "," ","C tax revenue","Mil90$",carbonTaxPaid);

    // write total emissions for region
    for (int m=0;m<maxper;m++)
        temp[m] = summary[m].get_emissmap_second("CO2");
    fileoutput3(name," "," "," ","CO2 emiss","MTC",temp);
    // write depletable resource results to file
    for (i=0;i<numResources;i++) 
        resources[i]->outputfile( name );
    // write supply sector results to file
    for (i=0;i<noSSec;i++) {
        supplySector[i]->outputfile();
        supplySector[i]->subsec_outfile();
    }
    // write end-use sector demand results to file
    for (i=0;i<noDSec;i++) {
        demandSector[i]->outputfile();	
        demandSector[i]->subsec_outfile();	
    }

}

//! Write MiniCAM style outputs to file.
void Region::MCoutput() const {
    const Modeltime* modeltime = scenario->getModeltime();
    int i=0, m=0;
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper),temptot(maxper);
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    // write population results to database
    population->MCoutput( name.c_str() );
    // write gnp and adjusted gnp for region
    dboutput4(name,"General","GDP 90$","GDP(90mer)","90US$",gnpDol);
    dboutput4(name,"General","GDP","norm","unitless",gnp);
    dboutput4(name,"General","GDP","energy adj","unitless",gnpAdj);
    dboutput4(name,"General","GDP","per cap","unitless",gnpCap);
    // regional carbon taxes
    dboutput4(name,"General","CarbonTax","Fos Fuel","90US$",carbonTax);
    // regional total carbon taxes paid
    dboutput4(name,"General","CarbonTax","revenue","90US$",carbonTaxPaid);


    // CO2 emissions by fuel
    const vector<string> primaryFuelList = scenario->getWorld()->getPrimaryFuelList();

    for( vector<string>::const_iterator fuelIter = primaryFuelList.begin(); fuelIter != primaryFuelList.end(); fuelIter++ ) {
        for (m=0;m<maxper;m++) {
            temp[m] = summary[m].get_emissfuelmap_second( *fuelIter );
            temptot[m] += temp[m];
        }
        dboutput4(name,"CO2 Emiss","by Fuel",*fuelIter,"MTC",temp);
    }
    // add total sequested amount to emissions by fuel
    for (m=0;m<maxper;m++) {
        // note the negative value for sequestered amount
        temp[m] = - summary[m].get_emissmap_second( "CO2sequestered" );
        temptot[m] += temp[m];
    }
    dboutput4(name,"CO2 Emiss","by Fuel","sequestered","MTC",temp);

    // total emissions by sector for region
    for (m=0;m<maxper;m++) {
        temp[m] = summary[m].get_emissmap_second("CO2");
    }
    // CO2 emissions by fuel and sector totals use same value
    dboutput4(name,"CO2 Emiss","by Fuel","zTotal","MTC",temptot);
    dboutput4(name,"CO2 Emiss","by Sector","zTotal","MTC",temp);

    // regional emissions for all greenhouse gases
    typedef map<string,double>:: const_iterator CI;
    map<string,double> temissmap = summary[0].getemission(); // get gases for period 0
    for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
        for (int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_emissmap_second(gmap->first);
        }
        dboutput4(name,"Emissions","by gas",gmap->first,"MTC",temp);
    }

    // regional total end-use service demand for all demand sectors
    for (m=0;m<maxper;m++) {
        temp[m] = 0; // initialize temp to 0 for each period
        for (i=0;i<noDSec;i++) { // sum for all period and demand sectors
            temp[m] += demandSector[i]->getService( m );
        }
    }
    dboutput4(name,"End-Use Service","by Sector","zTotal","Ser Unit",temp);

    // regional total end-use service demand without Tech Change for all demand sectors
    for (m=0;m<maxper;m++) {
        temp[m] = 0; // initialize temp to 0 for each period
        for (i=0;i<noDSec;i++) { // sum for all period and demand sectors
            temp[m] += demandSector[i]->getServiceWoTC( m );
        }
    }
    dboutput4(name,"End-Use Service","by Sector w/o TC","zTotal","Ser Unit",temp);

    // regional fuel consumption (primary and secondary) by fuel type
    map<string,double> tfuelmap = summary[0].getfuelcons();
    for (CI fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
        for (int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_fmap_second(fmap->first);
        }
        dboutput4(name,"Fuel Consumption","by fuel",fmap->first,"EJ",temp);
    }

    /*	summary does not contain fuel consumption by sector	
    // regional total fuel consumption for all demand sectors
    for (m=0;m<maxper;m++) {
    temp[m] = 0; // initialize temp to 0 for each period
    for (i=0;i<noDSec;i++) { // sum for all period and demand sectors
    temp[m] += summary[m].get_fmap_second(demandSector[i]->getName());
    }
    }
    dboutput4(name,"Fuel Consumption","by End-Use Sector","zTotal","EJ",temp);
    */	
    // region primary energy consumption by fuel type
    map<string,double> tpemap = summary[0].getpecons();
    CI pmap;
    for (pmap=tpemap.begin(); pmap!=tpemap.end(); ++pmap) {
        for (int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_pemap_second(pmap->first);
        }
        dboutput4(name,"Pri Energy","Consumption by fuel",pmap->first,"EJ",temp);
    }

    // region primary energy trade by fuel type
    tpemap = summary[0].getpetrade();
    for (pmap=tpemap.begin(); pmap!=tpemap.end(); ++pmap) {
        for (int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_petrmap_second(pmap->first);
        }
        dboutput4(name,"Pri Energy","Trade by fuel",pmap->first,"EJ",temp);
    }

    // regional Pri Energy Production Total
    for (m=0;m<maxper;m++) {
        temp[m] = summary[m].get_peprodmap_second("zTotal");
    }
    dboutput4(name,"Pri Energy","Production by Sector","zTotal","EJ",temp);

    // write depletable resource results to database
    for (i=0;i<numResources;i++) {
        resources[i]->MCoutput( name );
    }
    // write supply sector results to database
    for (i=0;i<noSSec;i++) {
        supplySector[i]->MCoutput();
    }
    // write end-use sector demand results to database
    for (i=0;i<noDSec;i++) {
        demandSector[i]->MCoutput();
    }
}

//! Find out which markets have simultaneities 
/* Want to loop through each sector, then loop through each fuels that sector uses.
Then loop through each other sector that is also a fuel.
Then loop through the fuels in that sector to see if that sector uses
the first as a fuel.  */
void Region::findSimul(const int period) {
    Marketplace* marketplace = scenario->getMarketplace();
    int isec;
    int	jsec;
    string OuterSectorName;
    string InnerSectorName;
    string InnerFuelName;    
    map<string, double> fuelcons;  
    map<string, double> Innerfuelcons;  
    typedef map<string,double>:: const_iterator CI;
    CI fuelIterOne;
    CI fuelIterTwo;
    const bool WriteOut = false;

    // Loop through all supply sectors
    if (WriteOut) { cout << "Region: " << name << endl; }
    for ( isec=0; isec<noSSec; isec++ ) {				
        OuterSectorName = supplySector[isec]->getName();
        if (WriteOut) { cout << "Checking Sector: " << OuterSectorName << endl; }
        fuelcons = supplySector[isec]->getfuelcons(period);	// Get fuel consumption map for outer sector
        // Inner loop through all supply sectors
        for ( jsec=0; jsec<noSSec; jsec++ ) {
            InnerSectorName = supplySector[jsec]->getName();
            fuelIterOne=fuelcons.find(InnerSectorName);	// Search in outer sector for name of inner sector 
            // Check if the inner sector is a fuel used by the outer sector (and not same sector!)
            if ( ( jsec != isec ) && ( fuelIterOne!=fuelcons.end() ) ) {	
                Innerfuelcons = supplySector[jsec]->getfuelcons(period);	// Get map of fuels used in inner sector
                if (WriteOut) {  cout << " Against Sector: " << InnerSectorName << endl; }

                // Now loop through inner sector, checking to see if it uses the outer sector as an input
                for ( fuelIterTwo=Innerfuelcons.begin(); fuelIterTwo!=Innerfuelcons.end(); fuelIterTwo++ ) {
                    InnerFuelName = fuelIterTwo->first;
                    if(InnerFuelName == OuterSectorName) {
                        // Have found a simultaneity
                        supplySector[ isec ]->addSimul( supplySector[ jsec ]->getName() );
                        supplySector[ jsec ]->addSimul( supplySector[ isec ]->getName() );

                        marketplace->resetToPriceMarket( InnerFuelName, name );
                        marketplace->resetToPriceMarket(InnerSectorName, name);
                        if (WriteOut) { 
                            cout << "  ***Sector " << InnerSectorName << " uses " << InnerFuelName << endl; 
                        }
                    }
                    else {
                        if (WriteOut) { 
                            cout << "     Sector " << InnerSectorName << " also uses " << InnerFuelName << endl; 
                        }
                    }
                }
            }
        }
    }
}

//! Initialize the market prices for the agricultural products.
void Region::initializeAgMarketPrices( const vector<double>& pricesIn ) { 
    agSector->initMarketPrices( name, pricesIn );
}


//! update regional summaries for reporting
void Region::updateSummary( const int period ) { 

    int i = 0;

    summary[period].clearpeprod();
    summary[period].clearfuelcons();

    for (i=0;i<numResources;i++) {
        summary[period].initpeprod(resources[i]->getName(),resources[i]->getAnnualProd(period));
    }
    for (i=0;i<noDSec;i++) {
        // call update for demand sector
        demandSector[i]->updateSummary( period );
        // update regional fuel consumption (primary and secondary) for demand sector
        summary[ period ].updatefuelcons( demandSector[ i ]->getfuelcons( period ) ); 
        summary[ period ].updateemfuelmap( demandSector[ i ]->getemfuelmap( period ) );
    }
    for (i=0;i<noSSec;i++) {
        // call update for supply sector
        supplySector[i]->updateSummary( period );
        // update regional fuel consumption (primary and secondary) for supply sector
        summary[period].updatefuelcons(supplySector[i]->getfuelcons(period)); 
        summary[ period ].updateemfuelmap( supplySector[ i ]->getemfuelmap( period ) );
    }
    // update primary energy trade from consumption and production amounts
    summary[period].updatepetrade(); 
}

/*! A function which print dependency graphs showing fuel usage by sector.
*
* This function prints the opening tag for the graph, calls Sector::addToDependencyGraph
* on all supply and demand sectors, and then prints the closing tag.
*
* \param outStream An output stream to write to which was previously created.
* \param period The period to print graphs for.
*/
void Region::printGraphs( ostream& outStream, const int period ) const {

    // Make sure the outputstream is open.
    assert( outStream );

    // Remove spaces from the region name.
    string tempName = name;
    util::replaceSpaces( tempName );

    // Print the graph header.
    outStream << "digraph " << tempName << " {" << endl;

    // Now iterate through sectors.

    // Loop through all supply sectors
    for ( int supplyIter = 0; supplyIter < noSSec; supplyIter++ ) {				
        supplySector[ supplyIter ]->addToDependencyGraph( outStream, period );
    }

    // Loop through all demand sectors.
    for ( int demandIter = 0; demandIter < noDSec; demandIter++ ) {				
        demandSector[ demandIter ]->addToDependencyGraph( outStream, period );
    }

    // Now close the graph
    outStream << "}" << endl << endl;
}

//! Return the primaryFuelCO2Coef for a specific  fuel.
double Region::getPrimaryFuelCO2Coef( const string& fuelName ) const {

    // Determine the correct fuel.
    double coef = 0;
    map<string,double>::const_iterator coefIter = primaryFuelCO2Coef.find( fuelName );
    if( coefIter != primaryFuelCO2Coef.end() ) {
        coef = coefIter->second;
    }

    return coef;
}

//! Return the carbonTaxCoef for a specific  fuel.
double Region::getCarbonTaxCoef( const string& fuelName ) const {

    // Determine the correct fuel.
    double coef = 0;
    map<string,double>::const_iterator coefIter = carbonTaxFuelCoef.find( fuelName );
    if( coefIter != carbonTaxFuelCoef.end() ) {
        coef = coefIter->second;
    }

    return coef;
}

//! Return the summary object for the given period.
/*! \todo This is a temporary fix to get the global CO2. This should be restructured.
* \param period Model period to return the summary for.
* \return The summary object.
*/
const Summary Region::getSummary( const int period ) const {
    return summary[ period ];
}

/*! \brief Return the dynamically determined input dependencies for a given sector.
*
* This function is a helper function to the recursive sector::getInputDependencies.
* It is required so that a sector can determine its full list of input dependencies,
* which in turn requires determining that for each of its input sectors.
*
* \author Josh Lurz
* \param sectorName Sector to find the full list of input dependencies for.
* \return The full list of input dependencies for the given sector
*/
vector<string> Region::getSectorDependencies( const string& sectorName ) const {

    // Setup the return vector.
    vector<string> retVector;

    // Find the correct sector.
    map<string,int>::const_iterator iter = supplySectorNameMap.find( sectorName );

    // If the sector exists returns the dependency list.
    if( iter != supplySectorNameMap.end() ) {
        retVector = supplySector[ iter->second ]->getInputDependencies( this );
    }

    // Return the resulting list.
    return retVector;
}

/*! \brief A function to print a csv file including all sectors and their dependencies
* 
* \author Josh Lurz
* \param logger The to which to print the dependencies. 
*/
void Region::printSectorDependencies( Logger* logger ) const {
    LOG( logger, Logger::DEBUG_LEVEL ) << name << ",Sector,Dependencies ->," << endl;
    for( vector<Sector*>::const_iterator sectorIter = supplySector.begin(); sectorIter != supplySector.end(); sectorIter++ ) {
        ( *sectorIter )->printSectorDependencies( logger );
    }
    LOG( logger, Logger::DEBUG_LEVEL ) << endl;
}


