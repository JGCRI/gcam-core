/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*!
 * \file building_dmd_sector.cpp
 * \ingroup Objects
 * \brief BuildingDemandSector class source file.
 * \author Steve Smith, Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include <cmath>
#include <algorithm>

#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/building_dmd_sector.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/scenario.h"
#include "containers/include/gdp.h"
#include "util/base/include/model_time.h"
#include "demographics/include/demographic.h"
#include "sectors/include/building_dmd_subsector.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*!
 * \brief Constructor.
 * \author Steve Smith, Josh Lurz
 * \param aRegionName Name of the containing region.
 */
BuildingDemandSector::BuildingDemandSector( const string& aRegionName )
: DemandSector( aRegionName ),
mBaseService( scenario->getModeltime()->getmaxper(), -1.0 ),
mBaseScaler( scenario->getModeltime()->getmaxper() )
{
}

//! Destructor
BuildingDemandSector::~BuildingDemandSector() {
}

/*! \brief Parses any attributes specific to derived classes
* \author Josh Lurz, Steve Smith
* \param nodeName The name of the curr node. 
* \param curr pointer to the current node in the XML input tree
* \return returns true if the node was parsed
*/
bool BuildingDemandSector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    const Modeltime* modeltime = scenario->getModeltime();
            
    if ( DemandSector::XMLDerivedClassParse( nodeName, curr ) ) {
        // if false, node was not parsed so far so try to parse here
    }   
    else if( nodeName == BuildingDemandSubSector::getXMLNameStatic() ){
        parseContainerNode( curr, subsec, subSectorNameMap, new BuildingDemandSubSector( regionName, name ) );
    }
    else if( nodeName == "base-service" ){
        XMLHelper<double>::insertValueIntoVector( curr, mBaseService, modeltime );
    }
    else if( nodeName == "saturation-elasticity" ){
        mSaturationElasticity = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "saturation-point" ){
        mSaturationPoint = XMLHelper<double>::getValue( curr );
    }
    else {
        return false;
    }
    // If was true somewhere above then noce was parsed
    return true;
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
*
* \author Steve Smith, Josh Lurz
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void BuildingDemandSector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {  
    const Modeltime* modeltime = scenario->getModeltime();
   
    DemandSector::toInputXMLDerived( out, tabs );
    XMLWriteVector( mBaseService, "base-service", out, tabs, modeltime, 0.0 );
    XMLWriteElement( mSaturationElasticity, "saturation-elasticity", out, tabs );
    XMLWriteElement( mSaturationPoint, "saturation-point", out, tabs );
}   

//! Write object to debugging xml output stream.
void BuildingDemandSector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteElement( mBaseService[ period ], "base-service", out, tabs );
    XMLWriteElement( mBaseScaler[ period ], "base-scaler", out, tabs );
    XMLWriteElement( mSaturationElasticity, "saturation-elasticity", out, tabs );
    XMLWriteElement( mSaturationPoint, "saturation-point", out, tabs );
    DemandSector::toDebugXMLDerived( period, out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& BuildingDemandSector::getXMLName() const {
    return getXMLNameStatic();
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
const string& BuildingDemandSector::getXMLNameStatic() {
    static const string XML_NAME = "buildingdemandsector";
    return XML_NAME;
}

void BuildingDemandSector::completeInit( const IInfo* aRegionInfo,
                                         DependencyFinder* aDepFinder,
                                         ILandAllocator* aLandAllocator,
                                         const GlobalTechnologyDatabase* aGlobalTechDB )
{
    DemandSector::completeInit( aRegionInfo, aDepFinder, aLandAllocator, aGlobalTechDB );

    // TODO: Remove this when base class is fixed.
    if( !mIsPerCapitaBased ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Detailed buildings always using a per capita base driver." << endl;
        mIsPerCapitaBased = true;
    }

    // TODO: Remove this when base class is fixed.
    // Income and price elasticities must be constant.
    if( std::count( mIncomeElasticity.begin(), mIncomeElasticity.end(), mIncomeElasticity[ 0 ] ) != mIncomeElasticity.size() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Income elasticities should be constant." << endl;
    }
    if( std::count( mPriceElasticity.begin(), mPriceElasticity.end(), mPriceElasticity[ 0 ] ) != mPriceElasticity.size() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Price elasticities should be constant." << endl;
    }
}

void BuildingDemandSector::initCalc( NationalAccount* aNationalAccount,
                                     const Demographic* aDemographic,
                                     const int aPeriod )
{
    DemandSector::initCalc( aNationalAccount, aDemographic, aPeriod );
}

void BuildingDemandSector::calcAggregateDemand( const GDP* aGDP,
                                                const Demographic* aDemographics,
                                                const int aPeriod )
{
    // Prices are not calculated reliably until period 1 so do not use price
    // ratio until after this note normalized to previous year not base year
    // (this is also done in detailed transportation)
    double priceRatio = 1;
    if( aPeriod > 1 ){
        priceRatio = getPrice( aGDP, aPeriod ) / getPrice( aGDP, 1 );
    }

    // Calculate unscaled per capita demand.
    double income = aGDP->getApproxGDPperCap( aPeriod );

    // See formula in header file for full explanation.
    double unscaledDemand = pow( income, mIncomeElasticity[ aPeriod ] )
                            / ( 1 + pow( income / mSaturationPoint, mSaturationElasticity.get() ) )
                            * pow( priceRatio, mPriceElasticity[ aPeriod ] );

    // Calibrate the base scaler for the period if the service demand was read
    // in.
    double scaledDemand;
    if ( mBaseService[ aPeriod ] >= 0 ) {
        // Set the scaled demand to the read-in service.
        scaledDemand = mBaseService[ aPeriod ];

        // Calculate the base scaler.
        mBaseScaler[ aPeriod ] = scaledDemand / unscaledDemand;
        
        // Set all future basescalers to this value. This could go to postcalc, but is all changing in multi-inputs anyway.
        for ( int aTempPeriod = aPeriod + 1;  aTempPeriod < mBaseScaler.size(); ++aTempPeriod ) {
            mBaseScaler[ aTempPeriod ] = mBaseScaler[ aPeriod ];
        }
    }
    else {
        scaledDemand = getBaseScaler( aPeriod ) * unscaledDemand;
    }
    
    // Calculate the total service demand.
    mService[ aPeriod ] = scaledDemand * aDemographics->getTotal( aPeriod );

    // Set subsector outputs, technology outputs, and market demands
    setOutput( mService[ aPeriod ], aGDP, aPeriod );
}

/*!
 * \brief Get the base scaler to use for calculating demand in a given period.
 * \details Searches backwards through the previous time periods for the last
 *          base scaler which was calculated for calibrated data.
 * \param aPeriod Current period.
 * \return Base scaler to use in the period. The function reports an error and
 *         returns 1 if a base scaler is not found.
 */
double BuildingDemandSector::getBaseScaler( const int aPeriod ) const {
    // Find the base scaler in the closest past year.
    for( unsigned int i = static_cast<unsigned int>( aPeriod ); i >= 0; --i ){
        if( mBaseScaler[ i ].isInited() ){
            return mBaseScaler[ i ];
        }
    }

    // There was no base scaler found so report an error.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::ERROR );
    mainLog << "No base scaler was found for detailed building sector " << name << "." << endl;
    return 1;
}
