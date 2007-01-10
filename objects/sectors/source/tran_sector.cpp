/*! 
* \file tran_sector.cpp
* \ingroup Objects
* \brief Transporation demand sector class source file.
* \author Sonny Kim, Josh Lurz, Marshall Wise, Steve Smith
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include "marketplace/include/marketplace.h"
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"
#include "sectors/include/tran_sector.h"
#include "sectors/include/tran_subsector.h"
#include "containers/include/gdp.h"
#include "demographics/include/demographic.h"

// xml headers
#include "util/base/include/xml_helper.h"
#include <xercesc/dom/DOMNode.hpp>

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string TranSector::XML_NAME = "tranSector";

//! Default constructor
TranSector::TranSector( const string& aRegionName ): DemandSector( aRegionName ) {
	percentLicensed.resize( scenario->getModeltime()->getmaxper() );
}

/*! \brief Initialize the Tran.
* \details Currently only calls the base class initCalc.
* \param aNationalAccount National accounts container.
* \param aDemographics Regional demographics object.
* \param aPeriod Period for which to initialize the TranSector.
*/
void TranSector::initCalc( NationalAccount* aNationalAccount,
						  const Demographic* aDemographics,
						  const int aPeriod )
{
	DemandSector::initCalc( aNationalAccount, aDemographics, aPeriod );
    checkSectorCalData( aPeriod );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& TranSector::getXMLName() const {
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
const std::string& TranSector::getXMLNameStatic() {
	return XML_NAME;
}

//! Parses any input variables specific to derived classes
bool TranSector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
	// call the demand sector XML parse to fill demand sector attributes
	if( DemandSector::XMLDerivedClassParse( nodeName, curr ) ){
	}
	else if( nodeName == "percentLicensed" ) {
		XMLHelper<double>::insertValueIntoVector( curr, percentLicensed, scenario->getModeltime() );
	}
    else if( nodeName == "serviceoutput" ){
        XMLHelper<double>::insertValueIntoVector( curr, mService, scenario->getModeltime() );
    }
	else if( nodeName == TranSubsector::getXMLNameStatic() ){
		parseContainerNode( curr, subsec, subSectorNameMap, new TranSubsector( regionName, name ) );
	}
	else {
		return false;
	}
	return true;
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
* \author Josh Lurz
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void TranSector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
	// Write out parent class information.
	DemandSector::toInputXMLDerived( out, tabs );

	const Modeltime* modeltime = scenario->getModeltime();
    XMLWriteVector( percentLicensed, "percentLicensed", out, tabs, modeltime, 0.0 );
    XMLWriteVector( mService, "serviceoutput", out, tabs, modeltime, 0.0 );
}	

//! Write object to debugging xml output stream.
void TranSector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
	// Write out parent class information.
	DemandSector::toDebugXMLDerived( period, out, tabs );
	XMLWriteElement( percentLicensed[ period ], "percentLicensed", out, tabs );
}

/*! \brief Perform any sector level calibration data consistancy checks
*
* Check to make sure that total calibrated outputs are equal to sector demand in base period.
* \author Steve Smith
* \param period Model period
* \bug This does not check calibration status.
*/
void TranSector::checkSectorCalData( const int period ) {
	// For any periods where inputs are calibrated, must make sure that read-in calibration is equal to service demand. 
	// Adjust aggregate demand to match calibrated outputs of all inputs to this sector are calibrated
	if ( inputsAllFixed( period, "allInputs" ) ) {
		mService[0] = getCalOutput( period );
		double scaleFactor = getCalOutput( period ) / mService[0];
		ILogger& mainLog = ILogger::getLogger( "main_log" );
		mainLog.setLevel( ILogger::DEBUG );
		mainLog << "Calibrated Demand Scaled by " << scaleFactor << " in region " << regionName << " sector " << name << endl;
	}
}

/*! \brief Function to calculate sector service demand.
*
* \author Sonny Kim
* \param aGDP The regional GDP container.
* \param aPeriod The model period.
* \param aDemographic The regional demographic container.
*/
void TranSector::calcAggregateDemand( const GDP* aGDP, const Demographic* aDemographics, const int aPeriod ) { 

	// read-in values for mService for periods 0 and 1 are required
	// for all other periods
	if (aPeriod > 1 ) {
		// note normalized to previous year not base year
		// has implications for how technical change is applied
		double priceRatio = getPrice( aGDP, aPeriod ) / getPrice( aGDP, aPeriod - 1 );
		double priceRatioNotLic = priceRatio;
		if (mIsPerCapitaBased) {
			double perCapGDPRatio = aGDP->getGDPperCap( aPeriod ) / aGDP->getGDPperCap( aPeriod - 1 );
			double populationRatio = aDemographics->getTotal( aPeriod )
				/ aDemographics->getTotal( aPeriod - 1 );
			mService[ aPeriod ] = mService[ aPeriod - 1 ] * pow( priceRatio, mPriceElasticity[ aPeriod ] )
				* pow( perCapGDPRatio, mIncomeElasticity[ aPeriod ] )
				* populationRatio;
		}
		else {
			double gdpRatio = aGDP->getGDP( aPeriod ) / aGDP->getGDP( aPeriod - 1 );
			mService[ aPeriod ] = mService[ aPeriod - 1 ] * pow( priceRatio, mPriceElasticity[ aPeriod ] )
				* pow( gdpRatio, mIncomeElasticity[ aPeriod ] );
		}
	}

	// sets subsector outputs, technology outputs, and market demands
	setOutput( mService[ aPeriod ] / getTechnicalChange( aPeriod ), aGDP , aPeriod);
}
