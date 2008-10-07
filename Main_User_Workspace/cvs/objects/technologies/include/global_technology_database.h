#ifndef _GLOBAL_TECHNOLOGY_DATABASE_H_
#define _GLOBAL_TECHNOLOGY_DATABASE_H_
#if defined(_MSC_VER)
#pragma once
#endif

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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
* \file global_technology_database.h
* \ingroup Objects
* \brief The GlobalTechnologyDatabase class header file.
* \author Pralit Patel
*/

#include <vector>
#include <map>
#include <boost/shared_ptr.hpp>
#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/global_technology.h"

// Forward declarations
class Tabs;

/*! 
* \ingroup Objects
* \brief GlobalTechnologyDatabase holds GlobalTechnologies.
* \details This class contains GlobalTechnologies and allows technologies
*          to retrieve them on request through the getTechnology method.
*          Global technologies are read in from XML and are expected in
*          the following specification.
*
*          <b>XML specification for GlobalTechnologyDatabase</b>
*          - XML name: -c globalTechnologyDatabase
*          - Contained by: World
*          - Parsing inherited from class: None
*          - Attributes: None
*          - Elements:
*              - \c GlobalTechnology GlobalTechnologyDatase::mTechnologyList
* \author Pralit Patel
*/


class GlobalTechnologyDatabase
{
public:
    GlobalTechnologyDatabase();
    void XMLParse( const xercesc::DOMNode* aNode );
    void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;
    void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;
    static const std::string& getXMLNameStatic();

    const boost::shared_ptr<GlobalTechnology>& getTechnology( const std::string& aTechnologyName, const int aYear ) const;
private:
    //! List of GlobalTechnologies
    std::vector<boost::shared_ptr<GlobalTechnology> > mTechnologyList;
};

#endif // _GLOBAL_TECHNOLOGY_DATABASE_H_

