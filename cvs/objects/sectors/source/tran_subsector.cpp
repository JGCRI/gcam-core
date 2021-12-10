/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*! 
* \file tran_subsector.cpp
* \ingroup Objects
* \brief transporation technology class source file.
* \author Sonny Kim, Josh Lurz, Steve Smith, Marshall Wise
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <vector>

#include "sectors/include/tran_subsector.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "containers/include/info_factory.h"
#include "containers/include/iinfo.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/gdp.h"
#include "demographics/include/demographic.h"
#include "technologies/include/itechnology_container.h"
#include "technologies/include/itechnology.h"
#include "sectors/include/sector_utils.h"
#include "util/base/include/ivisitor.h"

using namespace std;

extern Scenario* scenario;

/*  Begin TranSubsector Method Definitions */

/*! \brief Default constructor for TranSubsector.
*
* Default constructor takes region name, sector name and units as arguments
* and resizes and initializes vectors.
* \param aUnit The sector output unit.
* \author Josh Lurz, Sonny Kim
*/
TranSubsector::TranSubsector():
Subsector(),
mTimeValueMult( Value( 1.0 ) )
{
    mAddTimeValue = false; // initialize to false
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
const std::string& TranSubsector::getXMLNameStatic() {
    static const string XML_NAME = "tranSubsector";
    return XML_NAME;
}

/*! \brief XML output for debugging.
* Function writes output to debugging XML
* \author Josh Lurz, Sonny Kim
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void TranSubsector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteElement( mAddTimeValue, "addTimeValue", out, tabs );
    XMLWriteElement( mPopDenseElasticity[ period ], "popDenseElasticity", out, tabs );
    XMLWriteElement( mSpeed[ period ], "speed", out, tabs );
    XMLWriteElement( mTimeValueMult[ period ], "time-value-multiplier", out, tabs );
}

/*! \brief Perform any initializations needed for each period.
* \author Sonny Kim, Steve Smith, Josh Lurz
* \param aSectorInfo The sector info object for additional sector data.
* \param aLandAllocator The regional land allocator.
*/
void TranSubsector::completeInit( const IInfo* aSectorInfo,
                                  ILandAllocator* aLandAllocator )
{
    // Only call base class completeInit.
    Subsector::completeInit( aSectorInfo, aLandAllocator );

    SectorUtils::fillMissingPeriodVectorInterpolated( mSpeed );
    SectorUtils::fillMissingPeriodVectorInterpolated( mTimeValueMult );
}

/*!
* \brief Perform any initializations needed for each period.
* \details Perform any initializations or calculations that only need to be done
*          once per period (instead of every iteration) should be placed in this
*          function.
* \warning The ghg part of this routine assumes the existence of technologies in
*          the previous and future periods
* \author Steve Smith, Sonny Kim
* \param aNationalAccount National accounts container.
* \param aDemographics Regional demographics container.
* \param aPeriod Model period
*/
void TranSubsector::initCalc( NationalAccount* aNationalAccount,
							 const Demographic* aDemographics,
							 const int aPeriod )
{
    // Check if illegal values have been read in
    if ( mSpeed[ aPeriod ] <= 0 ) {
        mSpeed[ aPeriod ] = 1;
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Speed was zero or negative in subsector: " << mName << " in region "
            << mRegionName << ". Reset to 1." << endl;
    }
    // time in transit
    // initialize vector to hold population (thousands)
    // TODO: revise access to population to avoid statement below
    mPopulation[ aPeriod ] = aDemographics->getTotal( aPeriod );

    Subsector::initCalc( aNationalAccount, aDemographics, aPeriod );
}

/*! \brief returns the subsector price.
* \details Calculates and returns share-weighted total price (subsectorprice)
*          with or without value of time.
* \author Sonny Kim
* \param aGDP Regional GDP object.
* \param aPeriod Model period
* \return The subsector price with or without value of time. 
*/
double TranSubsector::getPrice( const GDP* aGDP, const int aPeriod ) const {
    // mAddTimeValue is a boolean that determines whether the service price includes
    // the value of time
    if (mAddTimeValue) {
        return getGeneralizedPrice( aGDP, aPeriod );
    }
    // normal share-weighted total technology cost only
    return Subsector::getPrice( aGDP, aPeriod );
}

/*! \brief Get the time value for the period.
* \param aGDP The regional GDP container.
* \param aPeriod The model period.
* \author Sonny Kim
* \return The time value.
*/
double TranSubsector::getTimeValue( const GDP* aGDP, const int aPeriod ) const {
    const double WEEKS_PER_YEAR = 50;
    const double HOURS_PER_WEEK = 40;
    // calculate time value based on hours worked per year Convert GDPperCap
    // into dollars (instead of 1000's of $'s) GDP value at this point in the
    // code does not include energy feedback calculation for this year, so is,
    // therefore, approximate
    return aGDP->getApproxGDPperCap( aPeriod ) * 1000 * mTimeValueMult[ aPeriod ] / ( HOURS_PER_WEEK * WEEKS_PER_YEAR ) / mSpeed[ aPeriod ];
}

/*! \brief Calculate the generalized service price for the mode that includes time value.
* \author Sonny Kim
* \param aGDP The regional GDP container.
* \param aPeriod The model period.
* \return The the generalized price.
*/
double TranSubsector::getGeneralizedPrice( const GDP* aGDP, const int aPeriod ) const {
    // add cost of time spent on travel by converting gdp/cap into an hourly
    // wage and multiplying by average speed.
    // The price unit is $ per service, e.g. $/pass-mi or $/ton-mi
    
    // Save time value so can print out
    // Maybe also write to XML DB?
    double timeValue =  getTimeValue( aGDP, aPeriod );
    return Subsector::getPrice( aGDP, aPeriod ) + timeValue;
}

/*! \brief Get the time in transit per day per person for the period.
*  Currently used for reporting only.
* \author Sonny Kim
* \param aPeriod The model period.
* \return The time in transit.
*/
double TranSubsector::getTimeInTransit( const int aPeriod ) const {
    const double DAYS_PER_YEAR = 365;
    const double POP_MILE_CONV = 1000;
    // calculate time in transit per day for each person using total population
    return getOutput( aPeriod ) / mPopulation[ aPeriod ] * POP_MILE_CONV  
        / mSpeed[ aPeriod ] / DAYS_PER_YEAR ;
}

/*! \brief Get service per day per capita for the period.
* \author Sonny Kim
* \param aPeriod The model period.
* \return The service per day per capita.
*/
double TranSubsector::getServicePerCapita( const int aPeriod ) const {
    const double DAYS_PER_YEAR = 365;
    const double POP_MILE_CONV = 1000;
    // units: million pass or ton mi / thousand persons
    return getOutput( aPeriod ) / mPopulation[ aPeriod ] * POP_MILE_CONV
        / DAYS_PER_YEAR ;
}

void TranSubsector::setOutput( const double aVariableSubsectorDemand,
                               const double aFixedOutputScaleFactor,
                               const GDP* aGDP,
                               const int aPeriod )
{
    Subsector::setOutput( aVariableSubsectorDemand,
                          aFixedOutputScaleFactor,
                          aGDP,
                          aPeriod );
}

void TranSubsector::accept( IVisitor* aVisitor, const int period ) const {
    aVisitor->startVisitSubsector( this, period );
    aVisitor->startVisitTranSubsector( this, period );

    for( CTechIterator techIter = mTechContainers.begin(); techIter != mTechContainers.end(); ++techIter ) {
        (*techIter)->accept( aVisitor, period );
    }
	
    aVisitor->endVisitTranSubsector( this,  period );
    aVisitor->endVisitSubsector( this, period );
}
