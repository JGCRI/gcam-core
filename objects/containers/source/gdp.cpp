/*! 
* \file gdp.cpp
* \ingroup CIAM
* \brief The GDP class source file.
* \author Josh Lurz, Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <cmath>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "containers/include/gdp.h"
#include "demographics/include/population.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/xml_helper.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string GDP::XML_NAME = "GDP";
const int BASE_PPP_YEAR = 1990;   // Base year for PPP conversion. PPP values are not known before about this time.

//! Default Constructor
GDP::GDP() {
	// Resize all vectors to the max population period. 
	const int popMaxPeriod = scenario->getModeltime()->getmaxpopdata();
	laborProdGrowthRate.resize( popMaxPeriod );
	laborForceParticipationPercent.resize( popMaxPeriod );
	laborForce.resize( popMaxPeriod );

	// resize other vectors to normal periods
	const int maxper = scenario->getModeltime()->getmaxper();
	gdpValue.resize( maxper );
	gdpPerCapita.resize( maxper );
	gdpValueAdjusted.resize( maxper );
	gdpPerCapitaAdjusted.resize( maxper );
	gdpPerCapitaAdjustedPPP.resize( maxper );
	gdpAdjustedFlag.resize( maxper, false );
	calibrationGDPs.resize( maxper );

	EnergyGDPElas = 0;
	PPPConversionFact = 1;
   PPPDelta = 0;
	constRatio = false;
}

//! Destructor
GDP::~GDP(){
}

//! parses Population xml object
void GDP::XMLParse( const DOMNode* node ){

	const Modeltime* modeltime = scenario->getModeltime();

	DOMNode* curr = 0;
	DOMNodeList* nodeList;
	string nodeName;

	// make sure we were passed a valid node.
	assert( node );

	nodeList = node->getChildNodes();

	for( int i = 0; i < static_cast<int>( nodeList->getLength() ); i++ ){
		curr = nodeList->item( i );

		// get the name of the node.
		nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

		if( nodeName == "#text" ) {
			continue;
		}
		// GDP to PPP conversion factor
      // Note that variable conversion attribute defaults to true
		else if ( nodeName == "PPPConvert" ){
			PPPConversionFact = XMLHelper<double>::getValue( curr );
         constRatio = XMLHelper<bool>::getAttr( curr, "constRatio" );
		}
		// base-year GDP
		else if ( nodeName == "baseGDP" ){
			baseGDP = XMLHelper<double>::getValue( curr );
		}
		// labor force participation rate
		else if ( nodeName == "e_GDP_elas" ){
			EnergyGDPElas = XMLHelper<double>::getValue( curr );
		}
		// labor force participation rate
		else if ( nodeName == "laborproductivity" ){
			XMLHelper<double>::insertValueIntoVector( curr, laborProdGrowthRate, modeltime, true );
		}
		// labor force participation rate
		else if( nodeName == "laborforce" ){
			XMLHelper<double>::insertValueIntoVector( curr, laborForceParticipationPercent, modeltime, true );
		} else {
			cout << "Unrecognized text string: " << nodeName << " found while parsing GDP." << endl;
		}
	}
}

//! Writes datamembers to datastream in XML format.
void GDP::toInputXML( ostream& out, Tabs* tabs ) const {

	const Modeltime* modeltime = scenario->getModeltime();
	int iter;

	XMLWriteOpeningTag( getXMLName(), out, tabs );

	// GDP to PPP conversion factor
	XMLWriteElementAndAttribute( PPPConversionFact, "PPPConvert", out, tabs, constRatio, "constRatio" );

	// Write out base-year GDP
	XMLWriteElement( baseGDP, "baseGDP", out, tabs);

	// Write out gdp energy elasticity.
	XMLWriteElementCheckDefault( EnergyGDPElas, "e_GDP_elas", out, tabs, 0.0 );

	for( iter = 0; iter < static_cast<int>( laborProdGrowthRate.size() ); iter++ ){
		XMLWriteElement( laborProdGrowthRate[ iter ], "laborproductivity", out, tabs, modeltime->getPopPeriodToYear( iter ) );
	}

	for( iter = 0; iter < static_cast<int>( laborForceParticipationPercent.size() ); iter++ ){
		XMLWriteElement( laborForceParticipationPercent[ iter ], "laborforce", out, tabs, modeltime->getPopPeriodToYear( iter ) );
	}

	// Would want these in an xml-output file, but not in the input file.
	// write out MER-based GDP
	for( int m = 0; m < static_cast<int>( gdpValueAdjusted.size() ); m++ ){
		//       XMLWriteElementCheckDefault( gdpValueAdjusted[ m ], "GDP(MER)", out, tabs, 0, modeltime->getper_to_yr( m ) );
	}

	// write out PPP-based GDP
	for( int m = 0; m < static_cast<int>( gdpValueAdjusted.size() ); m++ ){
		//     XMLWriteElementCheckDefault( gdpValueAdjustedPPP[ m ], "GDP(PPP)", out, tabs, 0, modeltime->getper_to_yr( m ) );
	}

	XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Writes datamembers to debugging datastream in XML format.
void GDP::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

	const Modeltime* modeltime = scenario->getModeltime();
	int popPeriod = modeltime->getmod_to_pop( period );

	XMLWriteOpeningTag( getXMLName(), out, tabs );

	// GDP to PPP conversion factor
	XMLWriteElementCheckDefault( PPPConversionFact, "PPPConvert", out, tabs, 0.0 );

	// Write out base-year GDP
	XMLWriteElement( baseGDP, "baseGDP", out, tabs);

	// Write out gdp energy elasticity.
	XMLWriteElementCheckDefault( EnergyGDPElas, "e_GDP_elas", out, tabs, 0.0 );

	XMLWriteElement( laborProdGrowthRate[ popPeriod ], "laborprod", out, tabs );

	XMLWriteElement( laborForceParticipationPercent[ popPeriod ], "laborforce_p", out, tabs );

	XMLWriteElement( laborForce[ popPeriod ], "laborforce", out, tabs );
	// Done writing XML for the class members.

	// write out MER-based GDP
	for(int m = 0; m < static_cast<int>( gdpValueAdjusted.size() ); m++ ){
		XMLWriteElementCheckDefault( gdpValueAdjusted[ m ], "GDP(MER)", out, tabs, 0.0, modeltime->getper_to_yr( m ) );
	}

	XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& GDP::getXMLName() const {
	return XML_NAME;
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& GDP::getXMLNameStatic() {
	return XML_NAME;
}

//! Initialize the labor force.
void GDP::initData( const Population* regionalPop ){
	const Modeltime* modeltime = scenario->getModeltime();
	const int popmaxper = modeltime->getmaxpopdata();

	for ( int i = 0; i < popmaxper; i++ ) {
		laborForce[ i ] = regionalPop->getTotal(  i, true ) * laborForceParticipationPercent[ i ];
	}
}

//! Create calibration markets
void GDP::setupCalibrationMarkets( const string& regionName ) {

	const string goodName = "GDP";
	const Modeltime* modeltime = scenario->getModeltime();
	Marketplace* marketplace = scenario->getMarketplace();

	if ( marketplace->createMarket( regionName, regionName, goodName, Marketplace::CALIBRATION ) ) {
		vector<double> tempLFPs( modeltime->getmaxper() );
		for( int i = 0; i < modeltime->getmaxper(); i++ ){
			tempLFPs[ i ] = pow( 1 + laborProdGrowthRate[ modeltime->getmod_to_pop( i ) ], modeltime->gettimestep( i ) );
		}
		marketplace->setPriceVector( goodName, regionName, tempLFPs );
	}
}

//! Write back the calibrated values from the marketplace to the member variables.
void GDP::writeBackCalibratedValues( const string& regionName, const int period ) {

	const Marketplace* marketplace = scenario->getMarketplace();
	const Modeltime* modeltime = scenario->getModeltime();
	const string goodName = "GDP";

	// Only need to write back calibrated values for the current period.
	double totalLaborProd = marketplace->getPrice( goodName, regionName, period );

	int  popPeriod = modeltime->getmod_to_pop( period ); 
	laborProdGrowthRate[ popPeriod ] = pow( totalLaborProd, double( 1 ) / double( modeltime->gettimestep( period ) ) ) - 1;

	// sjs -- put in check for illegal growth rate so that NaN does not occur
	if ( laborProdGrowthRate[ popPeriod ] <= -1 ) {
		cout << "ERROR: laborProd Growth Rate reset from " << laborProdGrowthRate[ popPeriod ]  << endl;
		laborProdGrowthRate[ popPeriod ]  = -0.99;
	}
}

//! Write GDP info to text file
void GDP::csvOutputFile( const string& regionName ) const {
   const Modeltime* modeltime = scenario->getModeltime();
   const int maxPeriod = modeltime->getmaxper();
   vector<double> temp( maxPeriod );

   // function protocol
   void fileoutput3( string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

   // write gdp to temporary array since not all will be sent to output
   for ( int i = 0; i < maxPeriod; i++ ) {
        temp[ i ] = laborProdGrowthRate[ modeltime->getmod_to_pop( i ) ];
   }
   fileoutput3( regionName," "," "," ", "labor prod", "%/yr", temp );	
	
	// write gdp and adjusted gdp for region
	fileoutput3(regionName," "," "," ","GDP","Mil90US$",gdpValueAdjusted);
	fileoutput3(regionName," "," "," ","GDPperCap","thousand90US$",gdpPerCapitaAdjusted);
	fileoutput3(regionName," "," "," ","PPPperCap","thousand90US$",gdpPerCapitaAdjustedPPP);
}

//! MiniCAM output to file
void GDP::dbOutput( const string& regionName ) const {
	const Modeltime* modeltime = scenario->getModeltime();
	const int maxPeriod = modeltime->getmaxper();
	vector<double> temp( maxPeriod );

	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
		string uname,vector<double> dout);

	// labor productivity
	for( int i = 0; i < maxPeriod; i++ ){
		temp[ i ] = laborProdGrowthRate[ modeltime->getmod_to_pop( i ) ];
	}
	dboutput4( regionName, "General", "LaborProd", "GrowthRate", "perYr", temp );	

	// write gdp and adjusted gdp for region
	dboutput4(regionName,"General","GDP90$","GDP(90mer)","Mil90US$", gdpValueAdjusted );
	dboutput4(regionName,"General","GDP90$","GDPApprox(90mer)","Mil90US$", gdpValue );
	dboutput4(regionName,"General","GDP","perCap","thousand90US$", gdpPerCapitaAdjusted );
	dboutput4(regionName,"General","GDP90$","perCAP_PPP","thousand90US$", gdpPerCapitaAdjustedPPP );
}

/*! Calculate innitial regional gdps.
*
*  Routine calcuates GDPs without current period energy adjustment.
*  The gdpValue and gdpPerCapita variables have values that are approximations to the current GDP,
*  the "adjusted" values contain the values as adjusted for energy (and ultimate any other) feedbacks.
*
* \author Steve Smith, Sonny Kim, Josh Lurz(?),
* \param period Model time period
*/
void GDP::initialGDPcalc( const int period, const double population ) {

	const Modeltime* modeltime = scenario->getModeltime();
	const int baseYear = modeltime->getstartyr();
	const int basePer = modeltime->getyr_to_per(baseYear);

	// Set flag, current GDP values are not adjusted
	gdpAdjustedFlag[ period ] = false; 	 
	if ( period <= modeltime->getyr_to_per( 1990 ) ) {
		gdpAdjustedFlag[ period ] = true; // GDP is never adjusted for historial periods
	}

	if ( period == basePer ) {
		gdpValue[ period ] = baseGDP; 
		gdpValueAdjusted[ period ] = gdpValue[ period ];
	}
	else {
		double currentLF = getLaborForce( period );
		double lastLF = getLaborForce( period - 1 );
		double tlab = getTotalLaborProductivity( period );
		gdpValue[ period ] = gdpValueAdjusted[ period - 1 ] * tlab * ( currentLF / lastLF );
		gdpValueAdjusted[ period ] = gdpValue[ period ]; // Temporary value so that is never zero
		if ( gdpValue[period] == 0 ) {
			cerr << "error in GDP (GDP = 0) :  currentLF: " << currentLF
				<< "  lastLF: " << lastLF << "  lab: " << tlab << endl;
		}
	}

	// gdp period capita 
	// gdpValue is in millions, population in 1000's, so result is in 1000's of dollars per capita
	gdpPerCapita[ period ] = gdpValue[ period ] / population;   
   
   // Temporary values so that if requested a real value is returned (with error warning)
	gdpPerCapitaAdjusted[ period ] = gdpPerCapita[ period ]; 
   gdpPerCapitaAdjustedPPP[ period ] = gdpValue[ period ] / population;
}

/*! Adjust regional gdp for energy service price effect
*  Also calculates PPP-based GDP per capita
*  Note that GDP is only adjusted for periods after 1990. See notes for method initialGDPcalc().
* 
* \author Steve Smith, Sonny Kim, Josh Lurz(?),
* \param period Model time period
* \param priceratio Energy service price ratio
*/
void GDP::adjustGDP( const int period, const double priceRatio ) {
	const Modeltime* modeltime = scenario->getModeltime();

	if ( period > modeltime->getyr_to_per(1990) ) {
		// adjust gdp using energy cost changes and energy to gdp feedback elasticity
		try {
			gdpValueAdjusted[ period ] = gdpValue[ period ]*pow( priceRatio, EnergyGDPElas );
			gdpPerCapitaAdjusted[ period ] = gdpPerCapita[ period ] * gdpValueAdjusted[ period ] / gdpValue[ period ];
			gdpAdjustedFlag[ period ] = true;
		} catch(...) {
			cerr << "Error calculating gdpAdj in gdp.adjustGDP()"<<endl;
		}
   }
   calculatePPP( period, gdpPerCapitaAdjusted[ period ]);
}

/*! Calculate GDP on a PPP basis
* 
* This routine performs a logarithmic conversion between market exchange rate (MER) and PPP based GDP. 
* This conversion simulates the process of a developing economy transforming to a market economy where
* all sectors participate. This is, therefore, meant to account for the large MER/PPP differences between
* developing and market economies, not the fairly small differences between OECD market economies.
* In the base year (1990), values start at the PPP/MER ratio given as input data (taken from world bank or other
* studies). PPP and MER GDP values then converge exponentially until the crossover point is reached, 
* after which values are equal.  
* See Smith et al. (2004), "Future SO2 Emissions" paper for a full description of the conversion.
*
* \author Steve Smith
* \param period Model time period
* \param marketGDPperCapAdj Adjusted GDP per capita (market basis)
*/
void GDP::calculatePPP( const int period, const double marketGDPperCapAdj ) {
	const Modeltime* modeltime = scenario->getModeltime();
	const double CROSSOVER_POINT = 15.0; // Point at which PPP and Market values are equal ($15,000 per capita)
 
	// Don't do variable conversion if turned off for this region or if is before conversion data exist (before 1990)
   // Also don't do conversion is PPPConversionFact is < 1 since this is not defined!
	if ( constRatio || ( period < modeltime->getyr_to_per( BASE_PPP_YEAR ) || ( PPPConversionFact < 1 ) ) ) {
		gdpPerCapitaAdjustedPPP[ period ] = PPPConversionFact * marketGDPperCapAdj ;
	} else {
		// Only calculate this parameter in 1990 since won't change after that
		if  ( period == modeltime->getyr_to_per( BASE_PPP_YEAR ) )  {
			try { 
				PPPDelta = log( PPPConversionFact ) / log( marketGDPperCapAdj / CROSSOVER_POINT );
			} catch(...) {
				cerr << "Error calculating PPPDelta. "<< "PPPConversionFact = "<< PPPConversionFact;
				cerr << "; marketGDPperCapAdj = "<< marketGDPperCapAdj << endl;
			}
		}

		// Now do the conversion. 
		if ( marketGDPperCapAdj > CROSSOVER_POINT ) { // If GDP/cap is > crossover point then set equal.
			gdpPerCapitaAdjustedPPP[ period ] = marketGDPperCapAdj ;
		} else {
         try {
            double conversionFact =  pow( marketGDPperCapAdj / CROSSOVER_POINT , PPPDelta );
            gdpPerCapitaAdjustedPPP[ period ] = marketGDPperCapAdj * conversionFact;
         } catch(...) {
            cerr << "Error calculating PPP basis GDP in gdp.calculatePPP()" << endl;
         }
      }
	}
}

/*! Return approximate GDP per capita scaled to base year
* 
* This routine should be used only in the case where GDP per capita is needed before energy prices are available.
*
* \author Steve Smith
* \param period Model time period
*/
double GDP::getApproxScaledGDPperCap( const int period ) const {

	const Modeltime* modeltime = scenario->getModeltime();
	const int baseYear = modeltime->getstartyr();
	return gdpPerCapita[ period ] / gdpPerCapita[ modeltime->getyr_to_per( baseYear )  ];
}

/*! Return approximate GDP scaled to base year
* 
* This routine should be used only in the case where GDP is needed before energy prices are available.
*
* \author Sonny Kim
* \param period Model time period
*/
double GDP::getApproxScaledGDP( const int period ) const {

	const Modeltime* modeltime = scenario->getModeltime();
	const int baseYear = modeltime->getstartyr();
	return gdpValue[ period ] / gdpValue[ modeltime->getyr_to_per( baseYear )  ];
}

/*! Return approximate GDP per capita (1000's of dollars/cap)
* 
* This routine should be used only in the case where GDP per cap is needed before energy prices are available.
*
* \author Steve Smith
* \param period Model time period
*/
double GDP::getApproxGDPperCap( const int period ) const {
	return gdpPerCapita[ period ];
}

/*! Return adjusted GDP scaled to base year
* 
* This routine should be used in preference to getApproxScaledGDPperCap() above
*
* \author Steve Smith
* \param period Model time period
*/
double GDP::getScaledGDPperCap( const int period ) const {

	if ( !gdpAdjustedFlag[ period ] ) {
		cerr << "ERROR - Request for adjusted GDP -- not calculated yet" << endl;
	}
	const Modeltime* modeltime = scenario->getModeltime();
	const int baseYear = modeltime->getstartyr();
	return gdpPerCapitaAdjusted[ period ] / gdpPerCapitaAdjusted[ modeltime->getyr_to_per( baseYear )  ];
}

/*! Return GDP per capita (in $1000's of dollars)
*
* \author Steve Smith
* \param period Model time period
*/
double GDP::getGDPperCap( const int period ) const {

	if ( !gdpAdjustedFlag[ period ] ) {
		cerr << "ERROR - Request for adjusted GDP -- not calculated yet" << endl;
	}
	return gdpPerCapitaAdjusted[ period ];
}

/*! Return approximate GDP (in $1000's of dollars, before scaling)
*
* \author Steve Smith
* \param period Model time period
*/
double GDP::getApproxGDP( const int period ) const {

	return gdpValue[ period ];
}

/*! Return PPP-based GDP per capita (in $1000's of dollars)
* 
* \author Steve Smith
* \param period Model time period
*/
double GDP::getPPPGDPperCap( const int period ) const {

	if ( !gdpAdjustedFlag[ period ] ) {
		cerr << "ERROR - Request for adjusted GDP -- not calculated yet" << endl;
	}
	return gdpPerCapitaAdjustedPPP[ period ] ;
}

/*! Return MER-based GDP in constant dollars
* 
* \author Steve Smith
* \param period Model time period
*/
double GDP::getGDP( const int period ) const {

	if ( !gdpAdjustedFlag[ period ] ) {
		cerr << "ERROR - Request for adjusted GDP -- not calculated yet" << endl;
	}
	return gdpValueAdjusted[ period ] ;
}


/*! Return either approximate GDP or adjusted GDP scaled to base year
* 
* This routine is used where the model doesn't know if the scaled GDP has been calculated yet. 
* Should be used sparingly -- is intended to be used in subsector and technology share calculations 
* where it is not necessarilly known if the adjusted GDP is available.
*
* \author Steve Smith
* \param period Model time period
*/
double GDP::getBestScaledGDPperCap( const int period ) const {
	if ( !gdpAdjustedFlag[ period ] ) {
		return getApproxScaledGDPperCap( period );
	} 
	else {
		return getScaledGDPperCap( period );
	}
}

//! return labor productivity
double GDP::getLaborProdGR( const int per ) const {
	const Modeltime* modeltime = scenario->getModeltime();
	return laborProdGrowthRate[modeltime->getmod_to_pop(per)];
}

//! Return the  total labor force productivity. 
double GDP::getTotalLaborProductivity( const int period ) const {

	const Modeltime* modeltime = scenario->getModeltime();
	return pow( 1 + laborProdGrowthRate[ modeltime->getmod_to_pop( period ) ], modeltime->gettimestep( period ) );
}

//! return the labor force (actual working)
double GDP::getLaborForce( const int per ) const {
	const Modeltime* modeltime = scenario->getModeltime();
	return laborForce[ modeltime->getmod_to_pop( per ) ];
}

