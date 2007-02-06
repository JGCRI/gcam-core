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
    // If inputs are all fixed (or calibrated), then service must be set equal to calibrated inputs.
    if ( inputsAllFixed( aPeriod, "allInputs" ) ) {
        mService[ aPeriod ] = getCalOutput( aPeriod );
    }

    // Demand function won't work if mService[0] is 0
    // Demand sector assures that getOutput is non zero for period 0
    if ( mService[ 0 ] == 0 ) {
        mService[ 0 ] = getOutput( 0 );
    }

	DemandSector::initCalc( aNationalAccount, aDemographics, aPeriod );

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


/*! \brief Function to calculate sector service demand.
*
* \author Sonny Kim
* \param aGDP The regional GDP container.
* \param aPeriod The model period.
* \param aDemographic The regional demographic container.
*/
void TranSector::calcAggregateDemand( const GDP* aGDP, const Demographic* aDemographics, const int aPeriod ) { 

    // Start out with mBaseScaler (so that has valid value for per 0)
    if ( aPeriod == 0 ) {
        mService[ aPeriod ] = mBaseScaler[ aPeriod ];
    }
    // For period 1.
    // All the ratios are 1, so the equation falls out to the following.
    else if ( aPeriod == 1) {
        mService[ aPeriod ] = mBaseScaler[ aPeriod ] * mService[ aPeriod - 1 ];
    }
    // For all other periods.
    else {
        // note all ratios are normalized to previous year not base year
        // has implications for how technical change is applied
        double priceRatio = getPrice( aGDP, aPeriod ) / getPrice( aGDP, aPeriod - 1 );
        if ( mIsPerCapitaBased ) {
            double perCapGDPRatio = aGDP->getGDPperCap( aPeriod ) / aGDP->getGDPperCap( aPeriod - 1 );
            double populationRatio = aDemographics->getTotal( aPeriod ) / aDemographics->getTotal( aPeriod - 1 );
            mService[ aPeriod ] = mBaseScaler[ aPeriod ]
                                * mService[ aPeriod - 1 ] * pow( priceRatio, mPriceElasticity[ aPeriod ] )
                                * pow( perCapGDPRatio, mIncomeElasticity[ aPeriod ] )
                                * populationRatio;                
        }
        else {
            double gdpRatio = aGDP->getGDP( aPeriod ) / aGDP->getGDP( aPeriod - 1 );
            mService[ aPeriod ] = mBaseScaler[ aPeriod ]
                                * mService[ aPeriod - 1 ] * pow( priceRatio, mPriceElasticity[ aPeriod ] )
                                * pow( gdpRatio, mIncomeElasticity[ aPeriod ] );
        }
    }

	// sets subsector outputs, technology outputs, and market demands
	setOutput( mService[ aPeriod ] / getTechnicalChange( aPeriod ), aGDP , aPeriod);
}
