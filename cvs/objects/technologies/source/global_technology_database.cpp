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
* \file global_technology_datbase.cpp
* \ingroup Objects
* \brief GlobalTechnologyDatabase class source file.
* \author Pralit Patel
*/              

#include "util/base/include/definitions.h"
#include <cassert>

// User headers
#include "technologies/include/global_technology_database.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_parse_helper.h"
#include "technologies/include/itechnology_container.h"
#include "technologies/include/technology_container.h"
#include "util/base/include/util.h"

using namespace std;

//! Default Constructor
GlobalTechnologyDatabase::GlobalTechnologyDatabase() {
}

//! Destructor
GlobalTechnologyDatabase::~GlobalTechnologyDatabase() {
    for( CTechLocationIterator locIter = mTechnologyList.begin(); locIter != mTechnologyList.end(); ++locIter ) {
        const vector<ITechnologyContainer*>& tempTechs = ( *locIter ).second;
        for( CTechListIterator techIter = tempTechs.begin(); techIter != tempTechs.end(); ++techIter ) {
            delete *techIter;
        }
    }
    mTechnologyList.clear();
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
const std::string& GlobalTechnologyDatabase::getXMLNameStatic() {
    const static string XML_NAME = "global-technology-database";
    return XML_NAME;
}

//! parses GlobalTechnologyDatabase xml object
bool GlobalTechnologyDatabase::XMLParse( rapidxml::xml_node<char>* & aNode ) {
    string nodeName = XMLParseHelper::getNodeName(aNode);
    if( nodeName == "location-info" ) {
        map<string, string> attrs = XMLParseHelper::getAllAttrs(aNode);
        string sectorName = attrs["sector-name"];
        string subsectorName = attrs["subsector-name"];
        
        if( sectorName.empty() ) {
            // warn missing sector name
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Missing sector-name attribute while parsing location-info in "
                    << getXMLNameStatic();
        }
        else if( subsectorName.empty() ) {
            // warn missing subsector name
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Missing subsector-name attribute while parsing location-info in "
                    << getXMLNameStatic();
        }
        else {
            // try to find the contained technology
            for( rapidxml::xml_node<char>* currInner = aNode->first_node(); currInner; currInner = currInner->next_sibling() ) {
                if(currInner->type() == rapidxml::node_element) {
                    const string innerNodeName = XMLParseHelper::getNodeName(currInner);
                    
                    if( TechnologyContainer::hasTechnologyType( innerNodeName ) ) {
                        // note only technology containers are considered, no stubs
                        pair<string, string> locationInfo( sectorName, subsectorName );
                        
                        // Get the tech list by reference so that updates are reflected
                        // in mTechnologyList as well.
                        std::vector<ITechnologyContainer*>& tempTechList = mTechnologyList[ locationInfo ];
                        Data<std::vector<ITechnologyContainer*>, CONTAINER> tempTechListData(tempTechList, "");
                        XMLParseHelper::parseData(currInner, tempTechListData);
                        //parseContainerNode( currInner, tempTechList, new TechnologyContainer );
                    }
                    else {
                        ILogger& mainLog = ILogger::getLogger( "main_log" );
                        mainLog.setLevel( ILogger::ERROR );
                        mainLog << "Unknown element " << innerNodeName << " encountered while parsing location-info" << endl;
                    }
                }
            }
        }
        return true;
    }
    
    return false;
}

/*!
 * \brief Get the global technology identified by the given sector, subsector,
 *        and technology name.
 * \details A const pointer to the technology will be returned.  A user should clone
 *          the returned technology.  If the technology was not found null will be
 *          returned.
 * \param aSectorName The name of the sector this technology should be located under.
 * \param aSubsectorName The name of the Subsector this technology should be located under.
 * \param aTechnologyName The technology name to find.
 * \return A pointer to the global technology container or null if not found.
 */
const ITechnologyContainer* GlobalTechnologyDatabase::getTechnology( const string& aSectorName,
                                                                     const string& aSubsectorName,
                                                                     const string& aTechnologyName ) const
{
    const pair<string, string> locationToFind( aSectorName, aSubsectorName );
    // functor to find a technology in a vector by name
    util::NameEquals<INamed*> nameComparison( aTechnologyName );
    
    CTechLocationIterator techLocationIter = mTechnologyList.find( locationToFind );
    if( techLocationIter != mTechnologyList.end() ) {
        const vector<ITechnologyContainer*>& tempTechContainers = ( *techLocationIter ).second;
        CTechListIterator techIter = find_if( tempTechContainers.begin(), tempTechContainers.end(),
                                              nameComparison );
        if( techIter != tempTechContainers.end() ) {
            return *techIter;
        }
    }
    
    // otherwise the lookup was unsucessful
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::ERROR );
    mainLog << "Could not find global technology for sector: " << aSectorName << ", subsector: "
            << aSubsectorName << ", technology: " << aTechnologyName << endl;
    return 0;
}
