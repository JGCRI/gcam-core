/*! 
* \file Region.cpp
* \ingroup CIAM
* \brief Region class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cmath>
#include <cassert>

// xml headers
#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>

#include "region.h"
#include "summary.h"
#include "Sector.h"
#include "DemandSector.h"
#include "Resource.h"
#include "AgSector.h"
#include "demographic.h"
#include "ghg_mrk.h"
#include "xmlHelper.h"
#include "scenario.h"
#include "Emcoef_ind.h"

#include "modeltime.h" 
#include "Marketplace.h"
#include "Configuration.h"

using namespace std;

extern Scenario* scenario;
extern ofstream outfile, sdfile;	

//! map of CO2 emissions coefficient for primary fuel only
map<string, double> co2coefpri;
//! map of CO2 emissions coefficient for all fossil fuels
map<string, double> co2coefall;
//! vector of objects containing indirect emissions coefficients
vector<Emcoef_ind> emcoef_ind;
//! indirect emissions coefficients for secondary energy
map<string, double> co2coefind;

//! Default constructor
Region::Region() {
   agSector = 0;
   population = 0;
   initElementalMembers();
   
   // Resize all vectors of period information.
   const int maxper = scenario->getModeltime()->getmaxper();
   gnp.resize( maxper ); // normalized regional gross national product
   gnp_adj.resize( maxper ); // regional gross national product adjusted for energy
   gnp_cap.resize( maxper ); // regional gross national product per capita
   gnp_dol.resize( maxper ); 
   input.resize( maxper ); // total fuel and energy consumption
   price_ser.resize( maxper ); // aggregate price for demand services
   carbontaxpaid.resize( maxper ); // total regional carbon taxes paid
   summary.resize( maxper ); // summary for reporting
   carbontax.resize( maxper );
   calibrationGNPs.resize( maxper );
   i_elas.resize( maxper );
}

Region::~Region() {
   
   for ( vector<sector*>::iterator secIter = supplysector.begin(); secIter != supplysector.end(); secIter++ ) {
      delete *secIter;
   }
   
   for ( vector<demsector*>::iterator demIter = demandsector.begin(); demIter != demandsector.end(); demIter++ ) {
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

//! Clear member variables.
void Region::clear(){
   
   // initialize elemental data members.
   initElementalMembers();
   
   // now clear strings and vectors.
   name = "";
   population->clear();
   agSector->clear();
   resources.clear();
   supplysector.clear();
   demandsector.clear();
   ghgmarket.clear();
   i_elas.clear();
   gnp_dol.clear();
   gnp.clear();
   gnp_adj.clear();
   gnp_cap.clear();
   calibrationGNPs.clear();
   input.clear();
   price_ser.clear();
   carbontax.clear();
   carbontaxpaid.clear();
   summary.clear();
}

//! Initialize elemental data members.
void Region::initElementalMembers(){
   noghg = 0;
   numResources = 0;
   nossec = 0;
   nodsec = 0;
   noregmrks = 0;
}

//! Return the region name.
string Region::getName() const {
   return name;
}

//! Sets the data members from the XML input.
void Region::XMLParse( const DOMNode* node ){
   
   int i;
   int j;
   string nodeName;
   string nodeNameChild;
   DOMNode* curr = 0;
   DOMNode* currChild = 0;
   DOMNodeList* nodeListChild = 0;
   Resource* tempResource = 0;
   sector* tempSupSector = 0;
   demsector* tempDemSector = 0;
   //	TransSector* tempTransSector = 0;  //maw
   ghg_mrk* tempGhgMrk = 0;
   Configuration* conf = Configuration::getInstance();
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
   for(i = 0; i < static_cast<int>( nodeList->getLength() ); i++ ){
      curr = nodeList->item( i );
      nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
      
      if( nodeName == "e_GNP_elas" ){
         EnergyGNPElas = XMLHelper<double>::getValue( curr ); 
      }
      else if( nodeName == "demographics" ){
         population = new demographic();
         population->XMLParse( curr ); // only one demographics object.
      }
      else if( nodeName == "depresource" ){
         tempResource = new DepletableResource();
         tempResource->XMLParse( curr );
         resources.push_back( tempResource );
      }
      else if( nodeName == "fixedresource" ){
         tempResource = new FixedResource();
         tempResource->XMLParse( curr );
         resources.push_back( tempResource );
      }
      else if( nodeName == "renewresource" ){
         tempResource = new RenewableResource();
         tempResource->XMLParse( curr );
         resources.push_back( tempResource );
      }
      else if( nodeName == "supplysector" ){
         tempSupSector = new sector();
         tempSupSector->XMLParse( curr );
         supplysector.push_back( tempSupSector );
      }
      else if( nodeName == "demandsector" ){
         tempDemSector = new demsector();
         tempDemSector->XMLParse( curr );
         demandsector.push_back( tempDemSector );
      }
      /*		// maw
      else if( nodeName == "transsector" ){
      tempTransSector = new TransSector();
      tempTransSector->XMLParse( curr );
      demandsector.push_back( tempTransSector );
   } */
      else if( nodeName == "agsector" ) {
         if( conf->getBool( "agSectorActive" ) ){
            agSector = new AgSector();
            agSector->XMLParse( curr );	
         }
      }
      else if( nodeName == "ghgmarket" ){
         tempGhgMrk = new ghg_mrk();
         tempGhgMrk->XMLParse( curr );
         ghgmarket.push_back( tempGhgMrk );
      }
      // regional taxes
      else if( nodeName == "taxes" ){
         // get all child nodes.
         nodeListChild = curr->getChildNodes();
         // loop through the child nodes.
         for(j = 0; j < static_cast<int>( nodeListChild->getLength() ); j++ ){
            currChild= nodeListChild->item( j );
            nodeNameChild = XMLHelper<string>::safeTranscode( currChild->getNodeName() );
            if( nodeNameChild == "carbontax" ) {
               XMLHelper<double>::insertValueIntoVector( currChild, carbontax, modeltime );
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
            
            if( nodeNameChild == "GNP" ) {
               XMLHelper<double>::insertValueIntoVector( currChild, calibrationGNPs, modeltime );
            }
            else if(nodeNameChild == "incomeelasticity") {
               XMLHelper<double>::insertValueIntoVector( currChild, i_elas, modeltime );
            }
         }
         
      }
   }
   
   gnp_dol[ 0 ] = calibrationGNPs[ 0 ];
   
   numResources = resources.size();
   nossec = supplysector.size();
   nodsec = demandsector.size();
   noghg = ghgmarket.size();
   
   emcoef_ind.resize( nossec ); // indirect GHG coef object for every supply sector
   
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
   
   // supply sector markets, pass region name
   for( i = 0;i < nossec; i++ ){
      supplysector[ i ]->setMarket( name );
   }
   
   // ghg markets, pass region name
   for(i=0;i<noghg;i++){
      ghgmarket[i]->setMarket(name);
   }
   
   // Create AgLU markets
   if( conf->getBool( "agSectorActive" ) ){
      agSector->setMarket( name );
   }
   
   // MarketSetup;
   updateSummary(0);	// Dummy call to final supply to setup fuel map
   findSimul(0);
   
   
}

//! Write datamembers to datastream in XML format.
void Region::toXML( ostream& out ) const {
   
   const Modeltime* modeltime = scenario->getModeltime();
   
   int m;
   
   // write the beginning tag.
   Tabs::writeTabs( out );
   out << "<region name=\"" << name << "\">"<< endl;
   
   // increase the indent.
   Tabs::increaseIndent();
   
   // write the xml for the class members.
   // write out the single population object.
   population->toXML( out );
   
   // write out the resources objects.
   for( vector<Resource*>::const_iterator i = resources.begin(); i != resources.end(); i++ ){
      ( *i )->toXML( out );
   }
   
   // write out supply sector objects.
   for( vector<sector*>::const_iterator j = supplysector.begin(); j != supplysector.end(); j++ ){
      ( *j )->toXML( out );
   }
   
   // write out demand sector objects.
   for( vector<demsector*>::const_iterator k = demandsector.begin(); k != demandsector.end(); k++ ){
      ( *k )->toXML( out );
   }
   
   // agSector->toXML( out );
   
   // write out ghgmarket objects.
   for( vector<ghg_mrk*>::const_iterator l = ghgmarket.begin(); l != ghgmarket.end(); l++ ){
      ( *l )->toXML( out );
   }
   
   // Write out regional economic data
   Tabs::writeTabs( out );
   out << "<economicdata>" << endl;
   
   Tabs::increaseIndent();
   
   // write out GNP
   for( m = 0; m < static_cast<int>( gnp_dol.size() ); m++ ){
      XMLWriteElement( gnp_dol[ m ], "GNP", out, modeltime->getper_to_yr( m ) );
   }
   
   // write out income elasticity
   for( m = 0; m < static_cast<int>( i_elas.size() ); m++ ) {
      XMLWriteElement( i_elas[ m ],"incomeelasticity", out, modeltime->getper_to_yr( m ) );
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
   for( m = 0; m < static_cast<int>( carbontax.size() ); m++ ){
      XMLWriteElement( carbontax[ m ],"carbontax", out, modeltime->getper_to_yr( m ) );			
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

//! Write datamembers to datastream in XML format.
void Region::toDebugXML( const int period, ostream& out ) const {
   
   // write the beginning tag.
   Tabs::writeTabs( out );
   out << "<region name=\"" << name << "\">"<< endl;
   
   // increase the indent.
   Tabs::increaseIndent();
   
   // write out basic datamembers
   XMLWriteElement( noghg, "noghg", out );
   XMLWriteElement( numResources, "numResources", out );
   XMLWriteElement( nossec, "nossec", out );
   XMLWriteElement( nodsec, "nodsec", out );
   XMLWriteElement( noregmrks, "noregmrks", out );
   XMLWriteElement( gnp[ period ], "gnp", out );
   XMLWriteElement( gnp_cap[ period ], "gnpPerCapita", out );
   XMLWriteElement( gnp_dol[ period ], "gnpDollarValue", out);
   XMLWriteElement( calibrationGNPs[ period ], "calibrationGNPs", out );
   XMLWriteElement( gnp_adj[ period ], "gnp_adj", out );
   XMLWriteElement( input[ period ], "input", out );
   XMLWriteElement( price_ser[ period ], "price_ser", out );
   XMLWriteElement( carbontaxpaid[ period ], "carbontaxpaid", out );
   
   // write the xml for the class members.
   // write out the single population object.
   population->toDebugXML( period, out );
   
   // write out the resources objects.
   for( vector<Resource*>::const_iterator i = resources.begin(); i != resources.end(); i++ ){
      ( *i )->toDebugXML( period, out );
   }
   
   // write out supply sector objects.
   for( vector<sector*>::const_iterator j = supplysector.begin(); j != supplysector.end(); j++ ){
      ( *j )->toDebugXML( period, out );
   }
   
   // write out demand sector objects.
   for( vector<demsector*>::const_iterator k = demandsector.begin(); k != demandsector.end(); k++ ){
      ( *k )->toDebugXML( period, out );
   }
   
   // Write out the single agSector object.
   // agSector->toDebugXML( period, out );
   
   // write out ghgmarket objects.
   for( vector<ghg_mrk*>::const_iterator l = ghgmarket.begin(); l != ghgmarket.end(); l++ ){
      ( *l )->toDebugXML( period, out );
   }
   
   // Write out summary object.
   //summary[ period ].toDebugXML( period, out ); // is this vector by period?
   
   // Write out regional economic data.
   Tabs::writeTabs( out );
   out << "<economicdata>" << endl;
   Tabs::increaseIndent();
   
   // Write out GNP.
   XMLWriteElement( gnp_dol[ period ], "gnp_dol", out );
   
   // Write out income elasticity.
   XMLWriteElement( i_elas[ period ],"i_elas", out );
   
   Tabs::decreaseIndent();
   Tabs::writeTabs( out );
   out << "</economicdata>"<< endl;
   // End write out regional economic data
   
   // Write out regional taxes.
   Tabs::writeTabs( out );
   out << "<taxes>"<< endl;
   Tabs::increaseIndent();
   
   // Write out regional fixed taxes.
   XMLWriteElement( carbontax[ period ], "carbontax", out );			
   
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
void Region::setupCalibrationMarkets() {
   population->setupCalibrationMarkets( name );
}

//! Set default emissions coefficient for CO2.
//  Must find another way to set emissions coefficients rather than
//  hardcoding it here. shk
void Region::setCO2coef()
{
   // initialize map (tgC/EJ) or (MTC/EJ)
   // apply carbon taxes to primary fuels
   /*	co2coefpri["crude oil"] = 18.4; 
   co2coefpri["natural gas"] = 14.2;
   co2coefpri["coal"] = 27.3;
   */
   // setting emissions coefficients to these fuels
   // applies carbon taxes to secondary fuels
   co2coefpri["refined oil"] = 19.9691;
   co2coefpri["delivered gas"] = 14.2;
   co2coefpri["delivered coal"] = 27.3;
   
   // initialize map (tgC/EJ) or (MTC/EJ)
   co2coefall["crude oil"] = 19.9691; 
   co2coefall["crude oil regional"] = 19.9691; 
   co2coefall["refined oil"] = 19.9691;
   co2coefall["natural gas"] = 14.2;
   co2coefall["natural gas regional"] = 14.2;
   co2coefall["delivered gas"] = 14.2;
   co2coefall["coal"] = 27.3;
   co2coefall["coal regional"] = 27.3;
   co2coefall["delivered coal"] = 27.3;
}


//! Run the agLu Model and determine CO2 emitted.
void Region::calcAgSector( const int period ) {
   agSector->runModel( period, name );
   agSector->carbLand( period, name );
}

//! Set regional ghg constraint to market supply.
void Region::setghgsupply( int per ) {
   Marketplace* marketplace = scenario->getMarketplace();
   
   string ghgName;
   double ghgtarget;
   
   for (int i=0;i<noghg;i++) {
      ghgName = ghgmarket[i]->getName();
      ghgtarget = ghgmarket[i]->getConstraint(per);
      marketplace->setsupply(ghgName,name,ghgtarget,per);		
   }
}

//! Set regional ghg tax to individual technologies.
void Region::addghgtax( int per ) {
   string ghgname;
   int i,j,k;
   
   for (i=0;i<noghg;i++) {
      ghgname = ghgmarket[i]->getName();
      for (j=0;j<nossec;j++) {
         supplysector[j]->addghgtax(ghgname,name,per);
      }
      for (k=0;k<nodsec;k++) {
         demandsector[k]->addghgtax(ghgname,name,per);
      }
   }
}


//! Calculates annual supply of primay resources.
void Region::rscsupply(int per)  {
   Marketplace* marketplace = scenario->getMarketplace();
   string goodName;
   string regionName = name; // name is Region attribute
   double prev_price = 0;
   double prev_gdp = 0;
   double price = 0;
   //for (int i=0;i<numResources-1;i++) {
   for (int i=0;i<numResources;i++) {
      goodName = resources[i]->getName();
      price = marketplace->showprice(goodName,regionName,per); // get market price
      if (per==0) {
         prev_price = price;
         prev_gdp = gnp[per];
      }
      else {
         prev_price = marketplace->showprice(goodName,regionName,per-1); // get market price
         prev_gdp = gnp[per-1];
      }
      
      // calculate annual supply
      resources[i]->annualsupply(per,gnp[per],prev_gdp,price,prev_price);
      
      // set market supply of resources used for solution mechanism
      marketplace->setsupply(goodName,regionName,resources[i]->getAnnualProd(per),per);				
   }
}

//! Calculate prices of refined fuels and electricity.
void Region::finalsupplyprc(int per) {
   Marketplace* marketplace = scenario->getMarketplace();
   string goodName;
   double goodPrice;
   
   for (int i=0;i<nossec;i++) {
      goodName = supplysector[i]->getName();
      // name is region or country name
      supplysector[i]->calc_share( name, per );
      supplysector[i]->price( per );
      goodPrice = supplysector[ i ]->showprice( per );
      // set market price of intermediate goods
      // name is region or country name
      marketplace->setprice( goodName, name, goodPrice, per );
   }
}

//! Calculates supply of final energy and other goods.
void Region::finalsupply(int per) {

	Marketplace* marketplace = scenario->getMarketplace();
	string goodName;
	int i = 0;
	int j = 0;
	double mrksupply;


	// loop through all sectors once to get total output
	for (i=0;i<nossec;i++) {
	// start with last supply sector
	// need demand for all intermediate and final energy to
	// determine need for primary energy
		j = nossec - (i+1);
		
		goodName = supplysector[j]->getName();		
      
     if (name == "USA" && goodName == "electricity" && 1==2) {
          cout << ".";
      }

		// name is country/region name
		supplysector[j]->supply( name, per );
		supplysector[j]->sumoutput(per);
      double sectorOutput = supplysector[j]->getoutput(per);
      
     if (name == "USA" && goodName == "electricity"  && 1==2) {
          cout << "out: " << sectorOutput;
      }

		carbontaxpaid[per] += supplysector[j]->showcarbontaxpaid(per);
	}
	
	// loop through supply sectors and assign supplies to marketplace and update fuel consumption map
	// the supplies in the market sector are, at present, not used except to double check 
	// that the output of the supply sectors does equal supply

	for (i=0;i<nossec;i++) {
		// name is country/region name
		//supplysector[j].supply(name,no,per);
		// supply and demand for intermediate and final good are set equal
		goodName = supplysector[i]->getName();
		mrksupply = supplysector[i]->getoutput(per);
		
      if (name == "USA" && goodName == "electricity"   && 1==2) {
          cout << ".";
      }
		// set market supply of intermediate goods
		marketplace->setsupply(goodName,name,mrksupply,per);
	}
}

//! Calculate regional gnp.
void Region::calc_gnp( int per ) {
   
   const Modeltime* modeltime = scenario->getModeltime();
   const int baseYear = modeltime->getstartyr();
   const int basePer = modeltime->getyr_to_per(baseYear);
   
   if ( per == modeltime->getyr_to_per( baseYear ) ) {
      gnp[ per ] = 1.0; // normalize to 1975
   }
   else {
      double currentLF = population->getlaborforce( per );
      double lastLF = population->getlaborforce( per - 1 );
      double tlab = population->getTotalLaborProductivity( per );
      gnp[ per ] = gnp[ per - 1 ] * tlab * ( currentLF / lastLF );
      if (gnp[per] == 0) {
         cerr << "error with GNP calculation:  currentLF: " << currentLF
            << "  lastLF: " << lastLF << "  lab: " << tlab << "\n";
      }
   }
   
   
   // gnp per capita normalized
   // correct using energy adjusted gnp*****
   gnp_cap[ per ] = gnp[ per ] * population->total( basePer ) / population->total( per );
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

//! Calculate regional GNP using laborforce participation and labor productivity.
void Region::calcGNPlfp(int per) {
   const Modeltime* modeltime = scenario->getModeltime();
   double labprd=0;
   const int baseYear = modeltime->getstartyr();
   const int basePer = modeltime->getyr_to_per(baseYear);
   if (per==modeltime->getyr_to_per(baseYear)) {
      gnp[per] = 1.0;
   }
   else {
      // 1 + labor productivity growth rate
      // population->labor returns labor productivity growth rate
      labprd = 1 + population->labor(per);
      double tlabprd = pow(labprd,modeltime->gettimestep(per));
      gnp[per] = gnp[per-1] * tlabprd * ( population->getlaborforce(per)
         / population->getlaborforce(per-1) );
      if (gnp[per] == 0) {
         cerr << "error with GNP calculation:  labor force(per): " 
            << population->getlaborforce(per)
            << "  labor force(per-1): " << population->getlaborforce(per-1) 
            << "  labor productivity: " << tlabprd << "\n";
      }
   }
   // gnp per capita normalized
   // correct using energy adjusted gnp*****
   gnp_cap[per] = gnp[per]*population->total(basePer)/population->total(per);
   
}

//! Calculate demand sector aggregate price.
void Region::calcEndUsePrice( const int period ) {
   
   price_ser[ period ] = 0;
   
   for ( int i = 0; i < nodsec; i++ ) {
      demandsector[ i ]->calc_share( name, period, gnp_cap[period] );		
      
      // calculate service price for each demand sector
      demandsector[ i ]->price( period );
      
      // calculate aggregate service price for region
      price_ser[ period ] += demandsector[ i ]->getoutput( 0 ) * demandsector[ i ]->showprice( period );
      
      // calculate service price elasticity for each demand sector
      // or use read in value, temporary code
      bool useReadinData = true;
      // do nothing if false
      if (!useReadinData) {
         demandsector[ i ]->calc_pElasticity( period );
      }
      
      
   }
}

//! Adjust regional gnp for energy.
void Region::adjust_gnp(int per) {
   const Modeltime* modeltime = scenario->getModeltime();
   
   const int baseYear = modeltime->getstartyr();
   double tempratio;
   if (per<=modeltime->getyr_to_per(1990)) {
      gnp_adj[per] = gnp[per];
   }
   else {
      // adjust gnp using energy cost changes and 
      // energy to gnp feedback elasticity
      tempratio = price_ser[per]/price_ser[per-1];
      try {
         gnp_adj[per] = gnp[per]*pow(tempratio,EnergyGNPElas);
      } catch(...) {
         cerr << "Error calculating gnp_adj in region.adjust_gnp()\n";
      }
   }
   
   // calculate dollar value gnp using base year dollar value GNP
   if ( per > modeltime->getyr_to_per( baseYear ) ){ 
      gnp_dol[ per ] = gnp_adj[ per ] * gnp_dol[ modeltime->getyr_to_per( baseYear ) ];
   }
}

//! Write back the calibrated values from the marketplace into the member variables. 
void Region::writeBackCalibratedValues( const int period ) {
   population->writeBackCalibratedValues( name, period );
}

//! Do regional calibration
/*! Must be done after demands are calculated
*/
void Region::doCalibration( const bool doCalibrations, const int per ) {
   int i;

	// Do subsector and technology level energy calibration
   // can only turn off calibrations that do not involve markets
   if ( doCalibrations ) {
      // Calibrate demand sectors
	   for ( i=0;i<nodsec;i++) {
		   demandsector[ i ]->calibrateSector( name, per );
 	   }

      // Calibrate supply sectors
	   for ( i=0;i<nossec;i++) {
		   supplysector[ i ]->calibrateSector( name, per );
	   }
   }
   
	// Set up the GDP calibration. Need to do it each time b/c of nullsup call in marketplace.
   // Insert the newly calculated values into the calibration markets. 
	if( calibrationGNPs.size() > per && calibrationGNPs[ per ] > 0 ){ 
      const string goodName = "GDP";
      Marketplace* marketplace = scenario->getMarketplace();
		marketplace->setdemand( goodName, name, calibrationGNPs[ per ], per );
		marketplace->setsupply( goodName, name, gnp_dol[ per ], per );
		marketplace->setMarketToSolve( goodName, name );
	}
}

//! Call any initializations that are only done once per period
void Region::initCalc( const int per ) 
{
   int i;
	for ( i=0;i<nodsec;i++) {
		demandsector[ i ]->initCalc( name, per ); 
	}

	for ( i=0;i<nossec;i++) {
		supplysector[ i ]->initCalc( name, per ); 
	}
}

//! Calculate regional demand for energy and other goods for all sectors.
void Region::endusedemand(int per) 
{
   carbontaxpaid[per] = 0; // initialize total regional carbon taxes paid
   
   // gnp_cap using energy adjusted gnp
   gnp_cap[per] = gnp_adj[per]*population->total(0)/population->total(per);
   
   for (int i=0;i<nodsec;i++) {
      // calculate aggregate demand for end-use sector services
      // set fuel demand from aggregate demand for services
      // name is region or country name
      demandsector[ i ]->aggdemand( name, gnp_cap[per], gnp_adj[per], per ); 
      carbontaxpaid[ per ] += demandsector[ i ]->showcarbontaxpaid( per );
   }
}

//! Print all supply sector information.
void Region::showsupsector(int per, const char *ofile) 
{
   for (int i=0;i<nossec;i++) {
      supplysector[i]->showlabel(ofile);
      supplysector[i]->showsubsec(per,ofile);
   }
}

// Print all demand sector information.
void Region::showdemsector(int per, const char* ofile) 
{
   for (int i=0;i<nodsec;i++) {
      demandsector[i]->showlabel(ofile);
      demandsector[i]->showsubsec(per,ofile);
   }
}

//! Apply carbon taxes to appropriate sectors.
void Region::applycarbontax(int per)
{
   int i=0;
   // apply carbon taxes by period to primary fossil fuel user only
   for (i=0;i<nossec;i++)
      supplysector[i]->applycarbontax(carbontax[per],per);
   for (i=0;i<nodsec;i++)
      demandsector[i]->applycarbontax(carbontax[per],per);
}

//! Return regional population->
double Region::showpop(int per)
{
   return population->total(per);
}

//! Return regional available Resource.
double Region::showrsc( const string resourceName, const int per )
{
   for (int i=0;i<numResources;i++) {
      if (resources[i]->getName() == resourceName )
         return resources[i]->getAvailable(per);
   }
   return 0;
}

//! Return regional available subresource.
double Region::showsubrsc( const string resourceName, const string& subResourceName, const int per ) 
{
   for (int i=0;i<numResources;i++) {
      if (resources[i]->getName() == resourceName )
         return resources[i]->getSubAvail( subResourceName, per );
   }
   return 0;
}

//! Calculate regional emissions from resources.
void Region::emission(int per)
{
   int i=0;
   
   summary[per].clearemiss(); // clear emissions map
   
   // need to call emissions function but sum is not needed
   for (i=0;i<nossec;i++) {
      supplysector[i]->emission(per);
      summary[per].updateemiss(supplysector[i]->getemission(per));
      emcoef_ind[i].setemcoef(supplysector[i]->getemfuelmap(per), 
         supplysector[i]->getoutput(per));
   }
   for (i=0;i<nodsec;i++) {
      demandsector[i]->emission(per);
      summary[per].updateemiss(demandsector[i]->getemission(per));
   }
}

//! Calculate regional emissions by fuel for reporting
void Region::calcEmissFuel(int per)
{
   map<string, double> fuelemiss; // tempory emissions by fuel
   // add CO2OIL,CO2GAS,CO2COAL to the emissions map for emissions
   // by fuel
   fuelemiss["CO2OIL"] = summary[per].get_pemap_second("crude oil")
      * co2coefall["crude oil"];
   double temp = fuelemiss["CO2oil"];
   temp = summary[per].get_pemap_second("crude oil");
   temp = co2coefall["crude oil"];
   fuelemiss["CO2GAS"] = summary[per].get_pemap_second("natural gas")
      * co2coefall["natural gas"];
   fuelemiss["CO2COAL"] = summary[per].get_pemap_second("coal")
      * co2coefall["coal"];
   
   summary[per].updateemiss(fuelemiss); // add CO2 emissions by fuel
}

//! Calculate regional indirect emissions from intermediate and final demand sectors.
void Region::emiss_ind(int per)
{
   int i;
   // calculate indirect GHG emissions
   for (i=0;i<nossec;i++)
      supplysector[i]->indemission(per);
   for (i=0;i<nodsec;i++) 
      demandsector[i]->indemission(per);
}

//! Set regional GHG emissions as market demand.
void Region::setghgdemand(int per)
{
   double ghgemiss;
   string ghgname;
   
   for (int i=0;i<noghg;i++) {
      ghgname = ghgmarket[i]->getName();
      if(ghgname == "CO2") {
         ghgemiss = summary[per].get_emissmap_second("CO2");
         ghgmarket[i]->setEmission(ghgemiss,per);
      }
      else if(ghgname == "CH4") {
         ghgemiss = summary[per].get_emissmap_second("CH4");
         ghgmarket[i]->setEmission(ghgemiss,per);
      }
   }
}	

//! Write all outputs to file.
void Region::outputfile() {
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
   fileoutput3(name," "," "," ","GNP","Bil90US$",gnp_dol);
   fileoutput3(name," "," "," ","GNP","norm",gnp);
   fileoutput3(name," "," "," ","GNP","energy adj",gnp_adj);
   // regional carbon taxes
   fileoutput3(name," "," "," ","C tax (fixed)","90$/TC",carbontax);
   // regional total carbon taxes paid
   fileoutput3(name," "," "," ","C tax revenue","Mil90$",carbontaxpaid);
   
   // write total emissions for region
   for (int m=0;m<maxper;m++)
      temp[m] = summary[m].get_emissmap_second("CO2");
   fileoutput3(name," "," "," ","CO2 emiss","MTC",temp);
   // write depletable resource results to file
   for (i=0;i<numResources;i++) 
      resources[i]->outputfile( name );
   // write supply sector results to file
   for (i=0;i<nossec;i++) {
      supplysector[i]->outputfile( name );
      supplysector[i]->subsec_outfile( name );
   }
   // write end-use sector demand results to file
   for (i=0;i<nodsec;i++) {
      demandsector[i]->outputfile( name );	
      demandsector[i]->subsec_outfile( name );	
   }
   
}

//! Write MiniCAM style outputs to file.
void Region::MCoutput() {
   const Modeltime* modeltime = scenario->getModeltime();
   int i=0, m=0;
   const int maxper = modeltime->getmaxper();
   vector<double> temp(maxper);
   // function protocol
   void dboutput4(string var1name,string var2name,string var3name,string var4name,
      string uname,vector<double> dout);
   
   // write population results to database
   population->MCoutput( name.c_str() );
   // write gnp and adjusted gnp for region
   dboutput4(name,"General","GDP 90$","GDP(90mer)","90US$",gnp_dol);
   dboutput4(name,"General","GDP","norm","unitless",gnp);
   dboutput4(name,"General","GDP","energy adj","unitless",gnp_adj);
   dboutput4(name,"General","GDP","per cap","unitless",gnp_cap);
   // regional carbon taxes
   dboutput4(name,"General","CarbonTax","Fos Fuel","90US$",carbontax);
   // regional total carbon taxes paid
   dboutput4(name,"General","CarbonTax","revenue","90US$",carbontaxpaid);
   
   
   // emissions by fuel for region crude oil
   for (m=0;m<maxper;m++) {
      temp[m] = summary[m].get_emissmap_second("CO2OIL");
   }
   dboutput4(name,"CO2 Emiss","by Fuel","crude oil","MTC",temp);
   // emissions by fuel for region natural gas
   for (m=0;m<maxper;m++) {
      temp[m] = summary[m].get_emissmap_second("CO2GAS");
   }
   dboutput4(name,"CO2 Emiss","by Fuel","natural gas","MTC",temp);
   // emissions by fuel for region coal
   for (m=0;m<maxper;m++) {
      temp[m] = summary[m].get_emissmap_second("CO2COAL");
   }
   dboutput4(name,"CO2 Emiss","by Fuel","coal","MTC",temp);
   // total emission by fuel for region
   for (m=0;m<maxper;m++)
      temp[m] = summary[m].get_emissmap_second("CO2");
   dboutput4(name,"CO2 Emiss","by Fuel","zTotal","MTC",temp);
   dboutput4(name,"CO2 Emiss","by Sector","zTotal","MTC",temp);
   
   // regional emissions for all greenhouse gases
   typedef map<string,double>:: const_iterator CI;
   map<string,double> temissmap = summary[0].getemission(); // get gases for per 0
   for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
      for (int m=0;m<maxper;m++) {
         temp[m] = summary[m].get_emissmap_second(gmap->first);
      }
      dboutput4(name,"Emissions","by gas",gmap->first,"MTC",temp);
   }
   
   // regional total end-use service demand for all demand sectors
   for (m=0;m<maxper;m++) {
      temp[m] = 0; // initialize temp to 0 for each period
      for (i=0;i<nodsec;i++) { // sum for all period and demand sectors
         temp[m] += demandsector[i]->getService( m );
      }
   }
   dboutput4(name,"End-Use Service","by Sector","zTotal","Ser Unit",temp);
   
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
   for (i=0;i<nodsec;i++) { // sum for all period and demand sectors
			temp[m] += summary[m].get_fmap_second(demandsector[i]->getName());
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
   for (i=0;i<nossec;i++) {
      supplysector[i]->MCoutput( name );
   }
   // write end-use sector demand results to database
   for (i=0;i<nodsec;i++) {
      demandsector[i]->MCoutput( name );
   }
}

//! Find out which markets have simultaneities 
/* Want to loop through each sector, then loop through each fuels that sector uses.
Then loop through each other sector that is also a fuel.
Then loop through the fuels in that sector to see if that sector uses
the first as a fuel.  */
void Region::findSimul(const int per) const {
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
   for ( isec=0; isec<nossec; isec++ ) {				
      OuterSectorName = supplysector[isec]->getName();
      if (WriteOut) { cout << "Checking Sector: " << OuterSectorName << endl; }
      fuelcons = supplysector[isec]->getfuelcons(per);	// Get fuel consumption map for outer sector
      // Inner loop through all supply sectors
      for ( jsec=0; jsec<nossec; jsec++ ) {
         InnerSectorName = supplysector[jsec]->getName();
         fuelIterOne=fuelcons.find(InnerSectorName);	// Search in outer sector for name of inner sector 
         // Check if the inner sector is a fuel used by the outer sector (and not same sector!)
         if ( ( jsec != isec ) && ( fuelIterOne!=fuelcons.end() ) ) {	
            Innerfuelcons = supplysector[jsec]->getfuelcons(per);	// Get map of fuels used in inner sector
            if (WriteOut) {  cout << " Against Sector: " << InnerSectorName << endl; }
            
            // Now loop through inner sector, checking to see if it uses the outer sector as an input
            for ( fuelIterTwo=Innerfuelcons.begin(); fuelIterTwo!=Innerfuelcons.end(); fuelIterTwo++ ) {
               InnerFuelName = fuelIterTwo->first;
               if(InnerFuelName == OuterSectorName) {
                  // Have found a simultaneity
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
void Region::updateSummary( const int per ) { 
   
   int i = 0;
   
   summary[per].clearpeprod();
   summary[per].clearfuelcons();
   
   for (i=0;i<numResources;i++) {
      summary[per].initpeprod(resources[i]->getName(),resources[i]->getAnnualProd(per));
   }
   for (i=0;i<nodsec;i++) {
      // call update for demand sector
      demandsector[i]->updateSummary( per );
      // update regional fuel consumption (primary and secondary) for demand sector
      summary[ per ].updatefuelcons( demandsector[ i ]->getfuelcons( per ) ); 
   }
   for (i=0;i<nossec;i++) {
      // call update for supply sector
      supplysector[i]->updateSummary( per );
      // update regional fuel consumption (primary and secondary) for supply sector
      summary[per].updatefuelcons(supplysector[i]->getfuelcons(per)); 
   }
   // update primary energy trade from consumption and production amounts
   summary[per].updatepetrade(); 
}
