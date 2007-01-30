#ifndef _GLOBAL_TECHNOLOGY_DATABASE_H_
#define _GLOBAL_TECHNOLOGY_DATABASE_H_
#if defined(_MSC_VER)
#pragma once
#endif

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

