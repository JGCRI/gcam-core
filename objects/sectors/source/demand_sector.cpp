/*! 
* \file demand_sector.cpp
* \ingroup CIAM
* \brief DemandSector class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cmath>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/xml_helper.h"

#include "sectors/include/demand_sector.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/summary.h"
#include "sectors/include/subsector.h"
#include "containers/include/scenario.h"
#include "containers/include/gdp.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, and sets value of debug flag.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
DemandSector::DemandSector( const string regionName ): Sector( regionName ){
    perCapitaBased = 0;
    pElasticityBase = 0;
    priceRatio = 1;
    
    // resize vectors
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    sectorFuelCost.resize( maxper ); // total end-use sector service .
    finalEngyCons.resize( maxper ); // end-use sector final energy consumption.
    service.resize( maxper ); // total end-use sector service 
    iElasticity.resize( maxper );
    pElasticity.resize( maxper ); // price elasticity for each period
    aeei.resize( maxper );
    techChangeCumm.resize( maxper ); // cummulative technical change
    servicePreTechChange.resize( maxper );
}

//! Default destructor
DemandSector::~DemandSector() {
}

//! Clear member variables.
void DemandSector::clear(){
    
    // call super clear
    Sector::clear();
    
    // now clear own data.
    perCapitaBased = 0;
    pElasticityBase = 0;
    priceRatio = 1;
    finalEngyCons.clear();
    service.clear();
    iElasticity.clear();
    pElasticity.clear();
    techChangeCumm.clear();
    servicePreTechChange.clear();
}

/*! \brief Parses any child nodes specific to derived classes
*
* Method parses any input data from child nodes that are specific to the classes derived from this class. 
*
* \author Josh Lurz, Steve Smith, Sonny Kim
* \param node pointer to the current node in the XML input tree
*/
void DemandSector::XMLDerivedClassParseAttr( const DOMNode* node ) {
    // get the perCapitaBased attribute for the demand sector
    perCapitaBased = XMLHelper<bool>::getAttr( node, "perCapitaBased" );
}

/*! \brief Parses any attributes specific to derived classes
*
* Method parses any input data attributes (not child nodes, see XMLDerivedClassParse) that are specific to any classes derived from this class.
*
* \author Josh Lurz, Steve Smith
* \param nodeName The name of the curr node. 
* \param curr pointer to the current node in the XML input tree
*/
void DemandSector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    
    const Modeltime* modeltime = scenario->getModeltime();
    
    if( nodeName == "priceelasticity" ) {
        XMLHelper<double>::insertValueIntoVector( curr, pElasticity, modeltime );
    }
    else if( nodeName == "serviceoutput" ){
        XMLHelper<double>::insertValueIntoVector( curr, service, modeltime );
    }
    else if( nodeName == "energyconsumption" ){
        XMLHelper<double>::insertValueIntoVector( curr, finalEngyCons, modeltime );
    }
    else if( nodeName == "incomeelasticity" ){
        XMLHelper<double>::insertValueIntoVector( curr, iElasticity, modeltime );
    }
    else if( nodeName == "aeei" ) {
        XMLHelper<double>::insertValueIntoVector( curr, aeei, modeltime );
    }
}


//! Write object to xml output stream.
void DemandSector::toXML( ostream& out, Tabs* tabs ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    int i = 0;
    
    // write the beginning tag.
    tabs->writeTabs( out );
    out << "<demandsector name=\"" << name << "\" perCapitaBased=\"" << perCapitaBased << "\">" << endl;
    
    // increase the indent.
    tabs->increaseIndent();
    
    // write the xml for the class members.
    // write out the market string.
    XMLWriteElement( market, "market", out, tabs );
    XMLWriteElement( unit, "unit", out, tabs );
    XMLWriteElementCheckDefault( pElasticityBase, "pElasticityBase", out, tabs, 0 );
    
    for( i = 0; modeltime->getper_to_yr( i ) <= 1990; i++ ){
        XMLWriteElementCheckDefault( sectorprice[ i ], "price", out, tabs, 0, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( pElasticity.size() ); i++ ){
        XMLWriteElementCheckDefault( pElasticity[ i ], "priceelasticity", out, tabs, 0, modeltime->getper_to_yr( i ) );
    }

    for( i = 0; modeltime->getper_to_yr( i ) <= 1990; i++ ){
        XMLWriteElementCheckDefault( service[ i ], "serviceoutput", out, tabs, 0, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; modeltime->getper_to_yr( i ) <= 1990; i++ ){
        XMLWriteElement( output[ i ], "output", out, tabs, modeltime->getper_to_yr( i ) );
    }

    for( i = 0; i < static_cast<int>( finalEngyCons.size() ); i++ ){
        XMLWriteElementCheckDefault( finalEngyCons[ i ], "energyconsumption", out, tabs, 0, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( iElasticity.size() ); i++ ){
        XMLWriteElementCheckDefault( iElasticity[ i ], "incomeelasticity", out, tabs, 0, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( aeei.size() ); i++ ){
        XMLWriteElementCheckDefault( aeei[ i ], "aeei", out, tabs, 0, modeltime->getper_to_yr( i ) );
    }
    
    // write out the subsector objects.
    for( vector<Subsector*>::const_iterator j = subsec.begin(); j != subsec.end(); j++ ){
        ( *j )->toXML( out, tabs );
    }
    
    // finished writing xml for the class members.
    
    // decrease the indent.
    tabs->decreaseIndent();
    
    // write the closing tag.
    tabs->writeTabs( out );
    out << "</demandsector>" << endl;
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
*
* \author Steve Smith, Josh Lurz
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void DemandSector::toXMLDerivedClass( ostream& out, Tabs* tabs ) const {  
    
}	


//! XML output for viewing.
void DemandSector::toOutputXML( ostream& out, Tabs* tabs ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    int i = 0;
    
    // write the beginning tag.
    tabs->writeTabs( out );
    out << "<demandsector name=\"" << name << "\">" << endl;
    
    // increase the indent.
    tabs->increaseIndent();
    
    // write the xml for the class members.
    // write out the market string.
    XMLWriteElement( market, "market", out, tabs );
    XMLWriteElement( unit, "unit", out, tabs );
    XMLWriteElement( pElasticityBase, "pElasticityBase", out, tabs );
    
    for( i = 0; i < static_cast<int>( sectorprice.size() ); i++ ){
        XMLWriteElement( sectorprice[ i ], "price", out, tabs, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( output.size() ); i++ ){
        XMLWriteElement( output[ i ], "output", out, tabs, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( service.size() ); i++ ){
        XMLWriteElement( service[ i ], "serviceoutput", out, tabs, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( servicePreTechChange.size() ); i++ ){
        XMLWriteElement( servicePreTechChange[ i ], "servicePreTechChange", out, tabs, modeltime->getper_to_yr( i ) );
    }

    for( i = 0; i < static_cast<int>( finalEngyCons.size() ); i++ ){
        XMLWriteElement( finalEngyCons[ i ], "energyconsumption", out, tabs, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( iElasticity.size() ); i++ ){
        XMLWriteElement( iElasticity[ i ], "incomeelasticity", out, tabs, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( pElasticity.size() ); i++ ){
        XMLWriteElement( pElasticity[ i ], "priceelasticity", out, tabs, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( aeei.size() ); i++ ){
        XMLWriteElement( aeei[ i ], "aeei", out, tabs, modeltime->getper_to_yr( i ) );
    }
    
    // does aeei need to be written?
    
    
    // write out the subsector objects.
    for( vector<Subsector*>::const_iterator j = subsec.begin(); j != subsec.end(); j++ ){
        ( *j )->toXML( out, tabs );
    }
    
    // finished writing xml for the class members.
    
    // decrease the indent.
    tabs->decreaseIndent();
    
    // write the closing tag.
    tabs->writeTabs( out );
    out << "</demandsector>" << endl;
}

//! Write object to debugging xml output stream.
void DemandSector::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    
    // write the beginning tag.
    tabs->writeTabs( out );
    out << "<demandsector name=\"" << name << "\" perCapitaBased=\""
        << perCapitaBased << "\">" << endl;
    
    // increase the indent.
    tabs->increaseIndent();
    
    // write the xml for the class members.
    // write out the market string.
    XMLWriteElement( market, "market", out, tabs );
    XMLWriteElement( unit, "unit", out, tabs );
    XMLWriteElement( pElasticityBase, "pElasticityBase", out, tabs );
    XMLWriteElement( priceRatio, "priceRatio", out, tabs );
    
    // Write out the data in the vectors for the current period.
    // First write out inherited members.
    XMLWriteElement( sectorprice[ period ], "sectorprice", out, tabs );
    XMLWriteElement( pe_cons[ period ], "pe_cons", out, tabs );
    XMLWriteElement( input[ period ], "input", out, tabs );
    XMLWriteElement( output[ period ], "output", out, tabs );
    XMLWriteElement( carbonTaxPaid[ period ], "carbonTaxPaid", out, tabs );
    
    XMLWriteElement( sectorFuelCost[ period ], "sectorFuelCost", out, tabs );
    XMLWriteElement( techChangeCumm[ period ], "techChangeCumm", out, tabs );
    
    
    // Now write out own members.
    XMLWriteElement( finalEngyCons[ period ], "finalEngyCons", out, tabs );
    XMLWriteElement( service[ period ], "service", out, tabs );
    XMLWriteElement( servicePreTechChange[ period ], "servicePreTechChange", out, tabs );
    XMLWriteElement( iElasticity[ period ], "iElasticity", out, tabs );
    XMLWriteElement( pElasticity[ period ], "pElasticity", out, tabs );
    XMLWriteElement( aeei[ period ], "aeei", out, tabs );
    
    // Write out the summary
    // summary[ period ].toDebugXML( period, out );
    
    // write out the subsector objects.
    for( vector<Subsector*>::const_iterator j = subsec.begin(); j != subsec.end(); j++ ){
        ( *j )->toDebugXML( period, out, tabs );
    }
    
    // finished writing xml for the class members.
    
    // decrease the indent.
    tabs->decreaseIndent();
    
    // write the closing tag.
    tabs->writeTabs( out );
    out << "</demandsector>" << endl;
}

/*! \brief Create new market for this sector
*
* Sets up the appropriate market within the marketplace for this sector. Note that the type of market is NORMAL -- signifying that this market is a normal market that is solved (if necessary).
*
* \author Sonny Kim, Josh Lurz, Steve Smith
*/
void DemandSector::setMarket() {
    Marketplace* marketplace = scenario->getMarketplace();
    
    if( marketplace->createMarket( regionName, market, name, Marketplace::NORMAL ) ) {
        marketplace->setPriceVector( name, regionName, sectorprice );
        /* The above initilaizes prices with any values that are read-in. 
        This only affects the base period, which is not currently solved.
        Any prices not initialized by read-in, are set by initXMLPrices(). */
    }    
}

/*!
* \brief  Calculate subsector shares, adjusting for capacity limits.

* This routine calls subsector::calcShare for each subsector, which calculated an unnormalized share, and then calls normShare to normalize the shares for each subsector.
* \param period model period
* \param gdp pointer to the gdp object for this region

* \author Sonny Kim, Steve Smith, Josh Lurz
*/
void DemandSector::calcShare( const int period, const GDP* gdp ) {
    int i=0;
    double sum = 0.0;
    for (i=0;i<nosubsec;i++) {
        // determine subsector shares based on technology shares
        subsec[i]->calcShare( period, gdp );
        sum += subsec[i]->getShare(period);

        // initialize cap limit status as false
        // (will be changed in adjSharesCapLimit if necessary)
        subsec[ i ]->setCapLimitStatus( false , period );
    }
    // normalize subsector shares to total 100 %
    for (i=0;i<nosubsec;i++) {
        subsec[i]->normShare(sum, period);
   }

    // Now adjust for capacity limits, if any are present
    if ( capLimitsPresent[ period ] ) {
        adjSharesCapLimit( period );
    }
}

/*! \brief Calculate weighted average price of subsectors.
*
* weighted price is put into sectorprice variable
*
* Note that the demand sector price uses the previous period's share to calucate the price.
* Since this is only used for a feedback term its not a large error. But is there a way around this?
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \todo consider if there is a way to use the current sector share so that this is not different from the supply sector function.
*/
void DemandSector::calcPrice(int period)
{
    sectorprice[period]=0.0;
    for (int i=0;i<nosubsec;i++) {	
        if (period == 0) {
            // uses read in base year shares
            sectorprice[period] += subsec[i]->getShare(period) * subsec[i]->getPrice(period);
        }
        else {
            // uses previous period's actual shares
            sectorprice[period] += subsec[i]->getShare(period-1) * subsec[i]->getPrice(period);
        }
    }
}

//! Calibrate sector output
/* This performs supply sector technology and sub-sector output/input calibration. 
   Determines total amount of calibrated and fixed output and passes that down to the subsectors.
   
   The first part of this code is identical to that for the supply sectors. 
   The second portion is specific to demand sectors. 
   (could be made for supply sectors too but that is not needed at present and 
    would require a consistency check between demand and supply.)
   
   If all subsector demands are calibrated (or zero) then also adjusts AEEI in order to be 
   consistent with calibrated values.
* \author Steve Smith
* \param regionName region name
* \param period Model period
*/
void DemandSector::calibrateSector( const int period ) {
    double totalFixedSupply = 0; // no fixed supply for demand sectors
    double mrkdmd;
    double totalCalOutputs = getCalOutput( period );
    
    mrkdmd = getService( period ); // demand for the good produced by this sector
    
    for (int i=0; i<nosubsec; i++ ) {
        if ( subsec[i]->getCalibrationStatus( period ) ) {
            subsec[i]->adjustForCalibration( mrkdmd, totalFixedSupply, totalCalOutputs, period );
        }
    }

    if ( isAllCalibrated( period ) ) {
       scaleOutput( period , totalCalOutputs/service[ period ] );
       if ( service[ period ] == 0 ) {
          cout << "ERROR: service = 0 in " << regionName << ":" << name << endl;
         }
     //  cout << "scaled output for " << regionName << ":" << name <<" by " << totalCalOutputs/service[ period ] << endl;
    }
}

//! scale output by changing value of some scale parameters (used for calibration)
/*! The scaleFactor is the amount by which the output needs to change. 
    The routine then calculates the necessary change in the AEEI. 
    
    \warning For derived demand sectors, some version of this routine needs to be included
    in order for the output of that sector to be able to be calibrated.
    sjs
* \author Steve Smith
* \param scaleFactor amount by which to scale output from this sector
* \param period Model period
*/
void DemandSector::scaleOutput( int period, double scaleFactor ) {
   const Modeltime* modeltime = scenario->getModeltime();
    
    // The solution for the scaling factor for AEEI (Afact), is
    // SF = TC_0 * (1+AEII)^T / [ TC_0 * (1+Afact*AEII)^T ] = (1+AEII)^T /[(1+Afact*AEII)^T]
    // So Afact = [( (1+AEII)^T /SF )^(1/T)-1]/AEII
    // TC_0 = techChangeCumm[period-1] & SF = scaleFactor

   // If scale factor is significant then change AEEI
   if ( fabs( 1 - scaleFactor ) > 1e-6 ) {   
      double timeStep = modeltime->gettimestep(period);
      double aeeiScale; // amount to change AEEI
      double temp = pow( 1+aeei[period] , timeStep );
      aeeiScale = ( pow( temp/ scaleFactor ,1/timeStep ) - 1) / aeei[ period ]; 
 
      aeei[ period ]  = aeei[ period ] * aeeiScale;
   }
}

/*! \brief Calculate end-use service price elasticity
*
*
* \author Sonny Kim
* \param period Model period
* \todo Sonny to add more to this description
*/
void DemandSector::calc_pElasticity(int period) {
    pElasticityBase = pElasticity[ 0 ]; // base year read in value
    pElasticity[period]=0.0;
    sectorFuelCost[period] = 0;
    double tmpPriceRatio = 0; // ratio of total price to fuel price
    for (int i=0;i<nosubsec;i++) {
        sectorFuelCost[period] += subsec[i]->getwtfuelprice(period);
    }
    tmpPriceRatio = sectorprice[period]/sectorFuelCost[period];
    pElasticity[period] = pElasticityBase*tmpPriceRatio;
}

/*! \brief Aggrgate sector energy service demand function
*
* Function calculates the aggregate demand for energy services and passes that down to the sub-sectors. 
* Demand is proportional to either GDP (to a power) or GDP per capita (to a power) times population.
*
* \author Sonny Kim
* \param gdp GDP (relative or absolute?)
* \param scaledGDPperCap GDP per capita, relative to base year
* \param period Model period
* \todo Sonny to add more to this description if necessary
* \pre Sector price attribute must have been previously calculated and set (via calcPrice)
*/
void DemandSector::aggdemand( const GDP* gdp, const int period ) {
    const Modeltime* modeltime = scenario->getModeltime();
    double ser_dmd;
    // double pelasticity = -0.9;
    double pelasticity = pElasticity[period];
    const double base = getOutput(0);
     
    // demand for service
    if (period == 0) {
        priceRatio = 0;

        ser_dmd = base; // base output is initialized by data
        techChangeCumm[period] = 1; // base year technical change
    }
    else {
        const int normPeriod = modeltime->getyr_to_per(1990);
		  const int basePer = modeltime->getyr_to_per( modeltime->getstartyr() );
        priceRatio = getPrice( period ) / getPrice( normPeriod );
        // perCapitaBased is true or false
		  
		  double gdpRatio = gdp->getGDP( period ) / gdp->getGDP( basePer );
        if ( perCapitaBased ) { // demand based on per capita GDP
				double scaledGDPperCap = gdp->getScaledGDPperCap( period );
            ser_dmd = base*pow(priceRatio,pelasticity)*pow(scaledGDPperCap,iElasticity[period]);
            // need to multiply above by population ratio (current population/base year
            // population).  This ratio provides the population ratio.
            ser_dmd *= gdpRatio/scaledGDPperCap;
				
        }
        else { // demand based on scale of GDP    

            ser_dmd = base*pow( priceRatio, pelasticity )*pow( gdpRatio, iElasticity[period] );
        }
        
        // calculate cummulative technical change using AEEI, autonomous end-use energy intensity
        techChangeCumm[period] = techChangeCumm[period-1]*pow(1+aeei[period],modeltime->gettimestep(period));
    }
    
    // Save the service demand without technical change applied for comparison with miniCAM.
    servicePreTechChange[ period ] = ser_dmd;
    
    // demand sector output is total end-use sector demand for service
    // adjust demand using cummulative technical change
    service[ period ] = ser_dmd/techChangeCumm[period];
    
    setServiceDemand( service[ period ], period ); // sets the output

    // sets subsector outputs, technology outputs, and market demands
    Sector::setoutput( service[ period ], period );
    // sums output of technologies and subsectors
    Sector::sumOutput( period );
}

//! Write sector output to database.
void DemandSector::outputfile() const {
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
    for (m=0;m<maxper;m++) {
        temp[m] = output[ m ]; }
    fileoutput3(regionName,getName()," "," ","production","SerUnit",temp);
    // total sector eneryg input
    for (m=0;m<maxper;m++) {
        temp[m] = input[ m ]; }
    fileoutput3(regionName,getName()," "," ","consumption","EJ",temp);
    // sector price
    for (m=0;m<maxper;m++) {
        temp[m] = sectorprice[m]; 
    }
    fileoutput3(regionName,getName()," "," ","price","$/Service",temp);
    // sector carbon taxes paid
    for (m=0;m<maxper;m++) {
        temp[m] = carbonTaxPaid[ m ]; }
    fileoutput3(regionName,getName()," "," ","C tax paid","Mil90$",temp);
    
}

//! Write MiniCAM style demand sector output to database.
void DemandSector::MCoutput() const {
    const Modeltime* modeltime = scenario->getModeltime();
    int m;
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    
    const string secname = Sector::getName();
    string str; // temporary string
    
    // total sector output
    dboutput4(regionName,"End-Use Service","by Sector",secname,"Ser Unit",service);
    dboutput4(regionName,"End-Use Service",secname,"zTotal","Ser Unit",service);
    dboutput4(regionName,"End-Use Service",secname+"_bySubsec","zTotal","Ser Unit",service);

    dboutput4(regionName,"End-Use Service","by Sector w/o TC",secname,"Ser Unit",servicePreTechChange);
    //dboutput4(regionName,"End-Use Service",secname,"zTotal","Ser Unit",temp);

    // End-use service price elasticity
    str = secname + "_price";
    dboutput4(regionName,"End-Use Service","Elasticity",str," ",pElasticity);
    str = secname + "_income";
    // End-use service income elasticity
    dboutput4(regionName,"End-Use Service","Elasticity",str," ",iElasticity);
    
    // sector fuel consumption by fuel type
    typedef map<string,double>:: const_iterator CI;
    map<string,double> tfuelmap = Sector::getfuelcons(m=0);
    CI fmap; // define fmap
    // *** Either write out for each fuel here which is contained in the map
    // or write out in subsector
    /*	for (fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
    for (m=0;m<maxper;m++) {
    temp[m] = Sector::getConsByFuel(m,fmap->first);
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
        temp[m] = Sector::getConsByFuel(m,fmap->first);
    }
    dboutput4(regionName,"Fuel Consumption",secname,fmap->first,"EJ",temp);
    dboutput4(regionName,"Fuel Consumption","by End-Use Sector",secname,"EJ",temp);
    // output for zTotal get written for each demand sector and dataviewer sums it up
    dboutput4(regionName,"Fuel Consumption","by End-Use Sector","zTotal","EJ",temp);
    
    
    // sector emissions for all greenhouse gases
    map<string,double> temissmap = summary[0].getemission(); // get gases for period 0
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
        temp[m] = sectorprice[ m ];
    }
    dboutput4(regionName,"Price",secname,"zSectorAvg","75$/Ser",temp);
    
    // sector price normalized to base price
    for (m=0;m<maxper;m++) {
        temp[m] = sectorprice[ m ] / sectorprice[ 0 ];
    }
    dboutput4(regionName,"Price","by End-Use Sector",secname,"Norm75",temp);
    
    // sector carbon taxes paid
    for (m=0;m<maxper;m++) {
        temp[m] = Sector::getTotalCarbonTaxPaid(m);
    }
    dboutput4(regionName,"General","CarbonTaxPaid",secname,"$",temp);
    
    // do for all subsectors in the sector
    MCoutput_subsec();
}

/*! \brief returns the demand sector service supplied.
*
*
* \author Sonny Kim
* \param period Model period
* \return amount of energy service supplied by this sector
*/
double DemandSector::getService( const int period ) const {
    return service[period];
}

/*! \brief returns the demand sector service before tech change is applied.
*
* This is useful for debugging and output, but is not used by the model itself at this point
*
* \author Sonny Kim
* \param period Model period
* \return energy service demand before technological change is applied
*/
double DemandSector::getServiceWoTC( const int period ) const {
    return servicePreTechChange[ period ];
}

void DemandSector::printStyle( ostream& outStream ) const {

    // Make sure the output stream is open.
    assert( outStream );
    
    // Get the sector name.
   string sectorName = getName();
   util::replaceSpaces( sectorName );

   // output sector coloring here.
   outStream << "\t" << sectorName << " [style=filled, color=steelblue1 ];" << endl;
}

