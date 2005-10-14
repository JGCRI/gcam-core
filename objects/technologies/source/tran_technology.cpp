/*! 
* \file tran_technology.cpp
* \ingroup Objects
* \brief transporation technology class source file.
* \author Sonny Kim, Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <cmath>
#include "technologies/include/tran_technology.h"
#include "emissions/include/ghg.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/gdp.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

const string TranTechnology::XML_NAME = "tranTechnology";

//! Default constructor.
TranTechnology::TranTechnology() {
	intensity = 1;
    loadFactor = 1;
	mTechnicalChange = 0;
}

//! Clone function. Returns a deep copy of the current TranTechnology.
TranTechnology* TranTechnology::clone() const {
    return new TranTechnology( *this );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& TranTechnology::getXMLName1D() const {
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
const std::string& TranTechnology::getXMLNameStatic1D() {
	return XML_NAME;
}

//! initialize TranTechnology with xml data
bool TranTechnology::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {
    if( nodeName == "intensity" ){
        intensity = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "loadFactor" ){
        loadFactor = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "techchange" ){
        mTechnicalChange = XMLHelper<double>::getValue( curr );
    }
    else {
        return false;
    }
    return true;
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
*
* \author Josh Lurz
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void TranTechnology::toInputXMLDerived( ostream& out, Tabs* tabs ) const {  
    XMLWriteElementCheckDefault( intensity, "intensity", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( loadFactor, "loadFactor", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( mTechnicalChange, "techchange", out, tabs, 0.0 );
}	

//! XML output for viewing.
void TranTechnology::toOutputXMLDerived( ostream& out, Tabs* tabs ) const {
    XMLWriteElementCheckDefault( intensity, "intensity", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( loadFactor, "loadFactor", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( mTechnicalChange, "techchange", out, tabs, 0.0 );
}

//! Write object to debugging xml output stream.
void TranTechnology::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const { 
    XMLWriteElement( intensity, "intensity", out, tabs );
    XMLWriteElement( loadFactor, "loadFactor", out, tabs );
    XMLWriteElement( getCumulativeTechnicalChange( period ), "techChangeCumm", out, tabs );
    XMLWriteElement( output / loadFactor, "vehicleOutput", out, tabs );
    XMLWriteElement( mTechnicalChange, "techchange", out, tabs );
}	

//! Perform initializations that only need to be done once per period.
/*! Check to see if illegal values have been read in
* This avoids serious errors that can be hard to trace
*/
//! 
void TranTechnology::initCalc( const IInfo* aSubsectorInfo ) {    
    
    technology::initCalc( aSubsectorInfo );

    // Check if illegal values have been read in
    if ( loadFactor == 0 ) {
        loadFactor = 1;
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "LoadFactor was zero in technology: " << name << ". Reset to 1." << endl;
    }
    
    if ( intensity == 0 ) {
        intensity = 1;
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Intensity was zero in technology: " << name << ". Reset to 1." << endl;
    }
}

//! define technology fuel cost and total cost
void TranTechnology::calcCost( const string& regionName, const string& sectorName, const int per ) {
    // fMultiplier and pMultiplier are initialized to 1 for those not read in
    // 75$/GJ 
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    const double JPERBTU = 1055; // 1055 Joules per BTU
    calcTotalGHGCost( regionName, sectorName, per );

    Marketplace* marketplace = scenario->getMarketplace();
    double fuelprice = marketplace->getPrice(fuelname,regionName,per);
	
	/*! \invariant The market price of the fuel must be valid. */
	assert( fuelprice != Marketplace::NO_MARKET_PRICE );
    
	fuelcost = ( (fuelprice * fMultiplier) + totalGHGCost ) * intensity/ getCumulativeTechnicalChange( per )
             * JPERBTU/(1.0E9)*CVRT90;
    techcost = ( fuelcost + necost ) * pMultiplier;
    
    // Convert cost to cost per service instead of cost per vehicle.
    // For example,  convert $/vehicle-mi into $/pass-mi or $/ton-mi 

    techcost = techcost/loadFactor;
}

//! Calculates fuel input and TranTechnology output.
/*! Adds demands for fuels and ghg emissions to markets in the marketplace
* NOTE, the demand passed in is the service demand, 
* which is then converted to a per vehicle demand through the loadfactor
* \param regionName name of the region
* \param prodName name of the product for this sector
* \param gdp pointer to gdp object
* \param dmd total demand for this subsector
* \param per Model period
*/
void TranTechnology::production(const string& regionName,const string& prodName,
                                double dmd, const GDP* gdp, const int per ) {
    output = share * dmd;

    // Convert from service (pas-km) to vehicle demand (vehicle-km)
    double vehicleOutput = output/loadFactor;
        
    // for transportation technology use intensity instead of efficiency
    // convert from million Btu to EJ
    const double ECONV = 1.055e-9;

    //intensity /= pow(1+techchange,timestep*per);
    input = vehicleOutput*intensity*ECONV/ getCumulativeTechnicalChange( per );
    //input = vehicleOutput*intensity*ECONV;
   
    if (input < 0) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Input value < 0 for TranTechnology " << name << endl;
    }
    
    Marketplace* marketplace = scenario->getMarketplace();    
    // set demand for fuel in marketplace
    marketplace->addToDemand(fuelname,regionName,input,per);
    // Don't need to add to supply because this is a service good.
    // calculate emissions for each gas after setting input and output amounts
    for ( unsigned int i = 0; i < ghg.size(); ++i ) {
        ghg[i]->calcEmission(regionName, fuelname,input,prodName,output, gdp, per );
    }    
}

//! return technology calibration value
double TranTechnology::getCalibrationOutput( ) const {
    const double ECONV = 1.055e-9;
	const int period = scenario->getModeltime()->getyr_to_per( year );
    return calInputValue * getCumulativeTechnicalChange( period ) * loadFactor / (intensity*ECONV);
}

//! return fuel intensity
double TranTechnology::getIntensity(const int per) const {
    return intensity / getCumulativeTechnicalChange( per );
}

/*! \brief Calculate the cumulative technical change for a period.
* \param aPeriod Period for which to calculate the cumulative technical change.
* \return Cumulative technical change.
*/
double TranTechnology::getCumulativeTechnicalChange( const int aPeriod ) const {
	/*! \pre The period is valid. */
	assert( aPeriod >= 0 && aPeriod < scenario->getModeltime()->getmaxper() );

	// Default to 1 for period 0 and 1.
	double cumulativeTechChange = 1;
	if( aPeriod > 1 ){
		const Modeltime* modeltime = scenario->getModeltime();
		const int timestep = modeltime->gettimestep( aPeriod );

		// Calculate cumulative technical change from period 1.
		// TODO: Correct this for variable timesteps.
		cumulativeTechChange = pow( 1 + mTechnicalChange, timestep * ( aPeriod - 1 ) );
	}

	/*! \post Cumulative technical change is greater than or equal to one.*/
	assert( cumulativeTechChange >= 1 );
	return cumulativeTechChange;
}
