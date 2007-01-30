#ifndef _BUILDING_SUPPLY_SUBSECTOR_H_
#define _BUILDING_SUPPLY_SUBSECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file building_supply_subsector.h
* \ingroup CIAM
* \brief The BuildingSupplySubSector class header file.
* \author Steve Smith
*/

#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/subsector.h"

/*! 
* \ingroup CIAM
* \brief A class which defines a building supply Subsector of the model.

* This subsector supplies building services. The main differences 
* to provide calibration and internal load values to the building demand subsectors and techs

* \author Steve Smith
*/

class BuildingSupplySubSector : public Subsector
{
public:
    BuildingSupplySubSector( const std::string& regionName, const std::string& sectorName );
    static const std::string& getXMLNameStatic();
protected:
    virtual const std::string& getXMLName() const;
    bool isNameOfChild  ( const std::string& nodename ) const;

    virtual ITechnology* createChild( const std::string& aTechType,
                                      const std::string& aTechName,
                                      const int aTechYear ) const;

    bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    double unitInternalLoads; //!< internal loads per unit service
private:
    static const std::string XML_NAME; //!< node name for toXML methods
};
#endif // _BUILDING_SUPPLY_SUBSECTOR_H_
