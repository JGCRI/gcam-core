/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Laboratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file final_demand_sector.cpp
* \ingroup Objects
* \brief The Final Demand Sector class source file.
*
*  Detailed Description.
*
* \author Pralit Patel
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>

#include "sectors/include/final_demand_sector.h"
#include "sectors/include/subsector.h"

using namespace std;

//! Default constructor
FinalDemandSector::FinalDemandSector( const string& aRegionName ):Sector( aRegionName ){
}

//! Destructor
FinalDemandSector::~FinalDemandSector() {}

//! Setup the markets for the FinalDemandSector. This currently does nothing.
void FinalDemandSector::setMarket(){
}

//! Parse xml file for data
bool FinalDemandSector::XMLDerivedClassParse( const string& nodeName, const xercesc::DOMNode* curr ) {
    return false;
}

//! For derived classes to output XML data
void FinalDemandSector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
	//do something maybe
}

//! Output debug info for derived class
void FinalDemandSector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
}

const string& FinalDemandSector::getXMLName() const {
	return getXMLNameStatic();
}

const string& FinalDemandSector::getXMLNameStatic() {
    const static string XML_NAME = "finalDemandSector";
	return XML_NAME;
}

/*!
 * \brief Get the FinalDemandSector price.
 * \param aGDP Regional GDP container(null for SGM).
 * \param aPeriod Model period.
 * \return Final demand sectors do not have a price, so return 0.
 */
double FinalDemandSector::getPrice( const GDP* aGDP,
                                    const int aPeriod ) const {
    return 0;
}

//! Operate the consumers.
void FinalDemandSector::operate( NationalAccount& aNationalAccount, const Demographic* aDemographic,
                                 const int aPeriod )
{
    for( SubsectorIterator currSub = subsec.begin(); currSub != subsec.end(); ++currSub ){
        // flag tells the subsector to operate all capital, old and new.
        (*currSub)->operate( aNationalAccount, aDemographic, moreSectorInfo.get(), true, aPeriod );
    }
}

/*! \brief Perform any initializations needed for each period.
*
* Any initializations or calculations that only need to be done once per period (instead of every iteration) should be placed in this function.
*
* \author Sonny Kim
* \param aNationalAccount NationalAccount object
* \param aDemographics Demographics object
* \param aPeriod Period to initialize
*/
void FinalDemandSector::initCalc( NationalAccount* aNationalAccount,
                                  const Demographic* aDemographics,
                                  const int aPeriod )
{
    // do any sub-Sector initializations
    for( SubsectorIterator currSub = subsec.begin(); currSub != subsec.end(); ++currSub ){
        (*currSub)->initCalc( aNationalAccount, aDemographics,
                              moreSectorInfo.get(), aPeriod );
    }
}

/*! \brief Complete the initialization of the final demand sector.
* \param aRegionInfo The regional information object.
* \param aDependencyFinder Region's dependency finder, should be null for CGE regions.
* \param aLandAllocator Region's land allocator, should be null for CGE regions.
* \param aGlobalTechDB Global technology database.
*/
void FinalDemandSector::completeInit( const IInfo* aRegionInfo,
                                      DependencyFinder* aDependencyFinder,
                                      ILandAllocator* aLandAllocator,
                                      const GlobalTechnologyDatabase* aGlobalTechDB )
{
    Sector::completeInit( aRegionInfo, aDependencyFinder, aLandAllocator, aGlobalTechDB );
}
