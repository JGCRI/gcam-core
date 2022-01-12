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
 * \file stub_technology_container.cpp
 * \ingroup Objects
 * \brief StubTechnologyContainer class source file.
 * \author Pralit Patel
 */
#include "util/base/include/definitions.h"
#include <string>
#include <cassert>

#include "technologies/include/stub_technology_container.h"
#include "technologies/include/global_technology_database.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_parse_helper.h"
#include "containers/include/scenario.h"
#include "containers/include/world.h"
#include "util/base/include/model_time.h"
#include "technologies/include/technology.h"

using namespace std;

extern Scenario* scenario;

//! Constructor
StubTechnologyContainer::StubTechnologyContainer()
:mTechnology( 0 )
{
}

//! Destructor
StubTechnologyContainer::~StubTechnologyContainer() {
    delete mTechnology;
    
    // Note the XML adjustments's memory will be managed by their document.
}


/*!
 * \brief Cloning of stubs are not allowed.
 * \return Null.
 */
ITechnologyContainer* StubTechnologyContainer::clone() const {
    return 0;
}

const string& StubTechnologyContainer::getXMLNameStatic() {
    const static string XML_NAME = "stub-technology";
    return XML_NAME;
}

// AParsable methods
bool StubTechnologyContainer::XMLParse( rapidxml::xml_node<char>* & aNode ) {
    mXMLAdjustments.push_back( XMLParseHelper::deepClone(aNode) );
    return true;
}

void StubTechnologyContainer::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    mTechnology->toDebugXML( aPeriod, aOut, aTabs );
}

const string& StubTechnologyContainer::getName() const {
    return mName;
}

void StubTechnologyContainer::completeInit( const string& aRegionName,
                                            const string& aSectorName,
                                            const string& aSubsectorName,
                                            const IInfo* aSubsecInfo,
                                            ILandAllocator* aLandAllocator )
{
    // get the technology from the global technology database
    const ITechnologyContainer* temp =
        scenario->getWorld()->getGlobalTechnologyDatabase()->getTechnology( aSectorName, aSubsectorName, mName );
    if( temp ) {
        mTechnology = temp->clone();
    }
    else {
        // If the global technology did not exist we can not go forward.  Note the
        // error message was printed by the global technology database.
        abort();
    }
     
    // Make the XML adjustments note that this may produce parsing errors.
    for(rapidxml::xml_node<char>* currNode : mXMLAdjustments) {
        mTechnology->XMLParse(currNode);
    }
    
    // now that the XML adjustments are parsed no need to keep them around any longer
    // Note the XML adjustments's memory is managed by rapidxml and will get cleaned up
    // when XMLParseHelper::cleanupParser is called.
    mXMLAdjustments.clear();
    
    // Now call complete init on the completed technology.  Note any other interpolations
    // which need to occur will happen here.
    mTechnology->completeInit( aRegionName, aSectorName, aSubsectorName,
                               aSubsecInfo, aLandAllocator );
}

void StubTechnologyContainer::initCalc( const string& aRegionName, const string& aSectorName,
                                        const IInfo* aSubsecInfo, const Demographic* aDemographic,
                                        const int aPeriod )
{
    mTechnology->initCalc( aRegionName, aSectorName, aSubsecInfo, aDemographic, aPeriod );
}

void StubTechnologyContainer::postCalc( const string& aRegionName, const int aPeriod ) {
    mTechnology->postCalc( aRegionName, aPeriod );
}

ITechnology* StubTechnologyContainer::getNewVintageTechnology( const int aPeriod ) {
    return mTechnology->getNewVintageTechnology( aPeriod );
}

const ITechnology* StubTechnologyContainer::getNewVintageTechnology( const int aPeriod ) const {
    return mTechnology->getNewVintageTechnology( aPeriod );
}

ITechnologyContainer::TechRangeIterator StubTechnologyContainer::getVintageBegin( const int aPeriod ) {
    return mTechnology->getVintageBegin( aPeriod );
}

ITechnologyContainer::CTechRangeIterator StubTechnologyContainer::getVintageBegin( const int aPeriod ) const {
    return mTechnology->getVintageBegin( aPeriod );}

ITechnologyContainer::TechRangeIterator StubTechnologyContainer::getVintageEnd( const int aPeriod ) {
    return mTechnology->getVintageEnd( aPeriod );
}

ITechnologyContainer::CTechRangeIterator StubTechnologyContainer::getVintageEnd( const int aPeriod ) const {
    return mTechnology->getVintageEnd( aPeriod );
}

void StubTechnologyContainer::accept( IVisitor* aVisitor, const int aPeriod ) const {
    mTechnology->accept( aVisitor, aPeriod );
}

void StubTechnologyContainer::doDataExpansion( ExpandDataVector<ParentClass::SubClassFamilyVector>& aVisitor ) {
    // if mTechnology has not yet been filled in, i.e. during XMLParse
    // the make available the Data from this subclass, otherwise it should
    // always just appear that mTechnology is the tech container that exists
    // here
    if(!mTechnology) {
        aVisitor.setSubClass( this );
    }
    else {
        mTechnology->doDataExpansion( aVisitor );
    }
}
