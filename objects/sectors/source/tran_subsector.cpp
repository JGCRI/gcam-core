/*! 
* \file tran_subsector.cpp
* \ingroup Objects
* \brief TranSubsector class source file.
* \author Marshall Wise, Sonny Kim, Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "sectors/include/tran_subsector.h"
#include "technologies/include/tran_technology.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/summary.h"
#include "containers/include/gdp.h"

using namespace std;
using namespace xercesc;
    
extern Scenario* scenario;
const string TranSubsector::XML_NAME = "tranSubsector";

/*  Begin TranSubsector Method Definitions */

//! Default constructor
TranSubsector::TranSubsector( const string regionName, const string sectorName ): Subsector( regionName, sectorName ) {
    // resize vectors
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    speed.resize( maxper ); // average speed of mode
    popDenseElasticity.resize( maxper );
    timeValue.resize( maxper );
    mServiceOutputs.resize( maxper );
    popDensity = 1; // initialize to 1 for now
    baseScaler = -1;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& TranSubsector::getXMLName() const {
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
const std::string& TranSubsector::getXMLNameStatic() {
    return XML_NAME;
}

//! Parses any input variables specific to derived classes
bool TranSubsector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {    
    // additional read in for transportation
    const Modeltime* modeltime = scenario->getModeltime();
    if( nodeName == "speed" ){
        XMLHelper<double>::insertValueIntoVector( curr, speed, modeltime );
    }
    else if( nodeName == "popDenseElasticity" ){
        XMLHelper<double>::insertValueIntoVector( curr, popDenseElasticity, modeltime );
    }
    // Is this going to conflict with parsing output? 
    else if( nodeName == "serviceoutput" ){
        XMLHelper<double>::insertValueIntoVector( curr, mServiceOutputs, modeltime );
    }
    else {
        return false;
    }
    return true;
}

/*! \brief Returns true if the nodename is a valid child for this class.
*
* Virtual function which specifies the XML name of the possible technology children of this class.
* This function allows all technologies to be properly parsed using the base subsector code.
* \author Steve Smith
* \pre Needs cooresponding createChild() function
* \return True if nodename is a valid child of this class.
*/
bool TranSubsector::isNameOfChild  ( const string& nodename ) const {
    return nodename == TranTechnology::getXMLNameStatic1D();
}

/*!
 * \brief Derived helper function to generate a child element or construct the
 *        appropriate technology.
 * \param aTechType The name of the XML node, which is the type of the
 *        technology.
 * \param aTechName The name of the new technology.
 * \param aYear The year of the new technology.
 * \pre isNameOfChild returned that the type could be created.
 * \author Steve Smith
 * \return A newly created technology of the specified type.
 */
ITechnology* TranSubsector::createChild( const string& aTechType,
                                        const string& aTechName,
                                        const int aTechYear ) const
{
    return new TranTechnology( aTechName, aTechYear );
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
* \author Josh Lurz
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void TranSubsector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    XMLWriteVector( speed, "speed", out, tabs, modeltime, 0.0 );
    XMLWriteVector( popDenseElasticity, "popDenseElasticity", out, tabs, modeltime, 0.0 );
    XMLWriteVector( mServiceOutputs, "serviceoutput", out, tabs, modeltime, 0.0 );
}

//! Write object to debugging xml output stream.
void TranSubsector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteElement( speed[ period ], "speed", out, tabs );
    XMLWriteElement( popDenseElasticity[ period ], "popDenseElasticity", out, tabs );
    XMLWriteElement( mServiceOutputs[ period ], "serviceoutput", out, tabs );
    XMLWriteElement( timeValue[ period ], "timeValue", out, tabs );
    XMLWriteElement( subsectorprice[period] + timeValue[period], "generalizedCost", out, tabs );
    XMLWriteElement( popDensity, "popDensity", out, tabs );
    XMLWriteElement( baseScaler, "baseScaler", out, tabs );
}

void TranSubsector::completeInit( const IInfo* aSectorInfo,
                                  DependencyFinder* aDependencyFinder,
                                  ILandAllocator* aLandAllocator,
                                  const GlobalTechnologyDatabase* aGlobalTechDB )
{
    // Only call base class completeInit.
    Subsector::completeInit( aSectorInfo, aDependencyFinder, aLandAllocator, aGlobalTechDB );
}

/*!
* \brief Perform any initializations needed for each period.
* \details Perform any initializations or calcuations that only need to be done
*          once per period (instead of every iteration) should be placed in this
*          function.
* \warning The ghg part of this routine assumes the existance of technologies in
*          the previous and future periods
* \author Steve Smith, Sonny Kim
* \param aNationalAccount National accounts container.
* \param aDemographics Regional demographics container.
* \param aMoreSectorInfo SGM sector info object.
* \param aPeriod Model period
*/
void TranSubsector::initCalc( NationalAccount* aNationalAccount,
                              const Demographic* aDemographics,
                              const MoreSectorInfo* aMoreSectorInfo,
                              const int aPeriod )
{
    // Check if illegal values have been read in
    if ( speed[ aPeriod ] <= 0 ) {
        speed[ aPeriod ] = 1;
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Speed was zero or negative in subsector: " << name << " in region " 
            << regionName << ". Reset to 1." << endl;
    }
    Subsector::initCalc( aNationalAccount, aDemographics, aMoreSectorInfo, aPeriod );
}

//! calculate subsector share numerator
void TranSubsector::calcShare( const int period, const GDP* gdp )
{
    const double scaledGdpPerCapita = gdp->getBestScaledGDPperCap(period);

    // Call function to compute technology shares
    calcTechShares( gdp, period );
    
    // Calculate and return subsector share; uses calcPrice function
    // calcPrice() uses normalized technology shares calculated above
    // Logit exponential should not be zero
    
    // Compute subsector weighted average price of technologies
    calcPrice( period );
    
    // Adjust price to consider time value 
    const double WEEKS_PER_YEAR = 50;
    const double HOURS_PER_WEEK = 40;
    
    /*! \pre Speed must be greater than 0. */
    assert( speed[ period ] > 0 );

    // Add cost of time spent on travel by converting gdp/cap into an hourly
    // wage and multipling by average speed calculate time value based on hours
    // worked per year. Convert per capita GDP into dollars (instead of 1000's
    // of $'s). GDP value at this point in the code does not include energy
    // feedback calculation for this year, so is, therefore, approximate.
    timeValue[period] = gdp->getApproxGDPperCap( period ) * 1000 
                       / ( HOURS_PER_WEEK * WEEKS_PER_YEAR ) / speed[ period ];
    
    /*! \invariant Time value must be valid and greater than or equal to zero. */
    assert( timeValue[ period ] >= 0 && util::isValidNumber( timeValue[ period ] ) );
    
    double generalizedCost = subsectorprice[period] + timeValue[period] ;
    
    /*! \invariant Generalized cost be valid and greater than or equal to zero. */
    assert( generalizedCost >= 0 && util::isValidNumber( generalizedCost ) );
    
    // Compute calibrating scaler if first period, otherwise use computed
    // scaler in subsequent periods.
    if( period == 0 ) {
        // Can only calculate a base period scaler if there is a non-zero share
        // weight.
        if( shrwts[ period ] > 0 ){
            baseScaler = mServiceOutputs[0] / shrwts[ period ] 
                         * pow( generalizedCost, -lexp[ period ] )
                         * pow( scaledGdpPerCapita, -fuelPrefElasticity[ period ] )
                         * pow( popDensity, -popDenseElasticity[ period ] );
        }
        else {
            baseScaler = 1;
        }
    }
    
    /*! \pre The base scaler must have already been calculated. */
    assert( baseScaler != -1 );

    // Compute the share of the subsector.
    share[period] = baseScaler * shrwts[period] * pow( generalizedCost, lexp[period])
                    * pow( scaledGdpPerCapita, fuelPrefElasticity[period] )
                    * pow( popDensity, popDenseElasticity[period] );
    
    /*! \post The share must be greater than or equal to zero and valid. */
    assert( share[ period ] >= 0 && util::isValidNumber( share[ period ] ) );
}

//! sets demand to output and output
/* Demand from the "dmd" parameter (could be energy or energy service) is passed to technologies.
*  This is then shared out at the technology level.
*  See explanation for sector::setoutput. 
*/
void TranSubsector::setOutput( const double aDemand,
                               const GDP* aGDP,
                               const int aPeriod )
{
    // output is in service unit when called from demand sectors
    double subsecdmd = share[ aPeriod ]* aDemand; // share is subsector level
    
    for( unsigned int i = 0; i < techs.size(); ++i ){
        // calculate technology output and fuel input from subsector output
        techs[i][ aPeriod ]->production( regionName, sectorName, subsecdmd, aGDP, aPeriod );
    }
}

/*! \brief Writes variables specific to transportation class to database.
*
* \author Steve Smith
*/
void TranSubsector::MCDerivedClassOutput() const {
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    // Subsector timeValue price
    dboutput4( regionName, "General", "TimeValue", sectorName + name, "$/pass(ton)-mi", timeValue );
    // Subsector speed
    dboutput4( regionName, "General", "Speed", sectorName + name, "Miles/hr", speed );
}

