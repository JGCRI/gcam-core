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
 * \file fixed_final_demand.cpp
 * \ingroup Objects
 * \brief FixedFinalDemand class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "sectors/include/fixed_final_demand.h"
#include "sectors/include/sector_utils.h"
#include "util/logger/include/ilogger.h"

using namespace std;

extern Scenario* scenario;

/*! \brief Constructor.
*/
FixedFinalDemand::FixedFinalDemand()
{
}

/*! \brief Destructor.
*/
FixedFinalDemand::~FixedFinalDemand(){
}

const string& FixedFinalDemand::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \return The constant XML_NAME as a static.
*/
const string& FixedFinalDemand::getXMLNameStatic() {
    const static string XML_NAME = "fixed-final-demand";
    return XML_NAME;
}

const string& FixedFinalDemand::getName() const {
    return mName;
}

void FixedFinalDemand::toDebugXML( const int aPeriod,
                                    ostream& aOut,
                                    Tabs* aTabs ) const
{
    XMLWriteOpeningTag ( getXMLName(), aOut, aTabs, mName );

    XMLWriteElement( mServiceDemand[ aPeriod ], "service", aOut, aTabs );
    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

void FixedFinalDemand::completeInit( const string& aRegionName,
                                      const IInfo* aRegionInfo )
{
    SectorUtils::fillMissingPeriodVectorInterpolated( mServiceDemand );
}


void FixedFinalDemand::initCalc( const string& aRegionName,
                                  const Demographic* aDemographics,
                                  const int aPeriod )
{
    if(!mServiceDemand[aPeriod].isInited()) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "No service read in for " << aRegionName << ", " << mName << endl;
    }
}

/*! \brief Set the final demand for service into the marketplace.
 * \detail This is just a fixed value read in by the user.
 * \param string& aRegionName region name.
 * \param Demographic* aDemographicss.
 * \param aPeriod Model aPeriod
 */
void FixedFinalDemand::setFinalDemand( const string& aRegionName,
                                        const Demographic* aDemographics,
                                        const int aPeriod )
{
    // Set the service demand into the marketplace.
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->addToDemand( mName, aRegionName, mServiceDemand[ aPeriod ], aPeriod );
}

double FixedFinalDemand::getWeightedEnergyPrice( const string& aRegionName,
                                                 const int aPeriod ) const
{
    // TODO: this method is no longer used and should be dropped
    // when the macro module is introduced
    return 0.0;
}

void FixedFinalDemand::accept( IVisitor* aVisitor,
                                const int aPeriod ) const
{
    aVisitor->startVisitFinalDemand( this, aPeriod );
    aVisitor->endVisitFinalDemand( this, aPeriod );
}

