#ifndef BLDG_SUPPLY_SUBSECTOR_H_
#define BLDG_SUPPLY_SUBSECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file subsector.h
* \ingroup CIAM
* \brief The subsector class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <map>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/subsector.h"

// Forward declarations
class GDP;

/*! 
* \ingroup CIAM
* \brief A class which defines a building supply Subsector of the model.

* This subsector supplies building servicies. The main differences 
* to provide calibration and internal load values to the building demand subsectors and techs

* \author Steve Smith
*/

class BuildingSupplySubSector : public Subsector
{
private:
    static const std::string XML_NAME; //!< node name for toXML methods
protected:
public:
    BuildingSupplySubSector( const std::string regionName, const std::string sectorName );
    virtual ~BuildingSupplySubSector();
    virtual const std::string& getXMLName() const;
    static const std::string& getXMLNameStatic();
    void calcShare(const int period, const GDP* gdp );
};
#endif // BLDG_SUPPLY_SUBSECTOR_H_
