#ifndef BLDG_SER_SUPPLY_SECTOR_H_
#define BLDG_SER_SUPPLY_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file demand_sector.h
* \ingroup CIAM
* \brief The BuildingSerSupplySector class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/sector.h"

// Forward declarations
class GDP;

/*! 
* \ingroup CIAM
* \brief A class which defines the building service supplysector.
*
*  This sector is identical in operation to the regular supply sector.
*
*  The only addition is that additional information is supplied about the sector 
*  characteristics through the marketInfo mechanism.
*
* \author Steve Smith
*/

class BuildingSerSupplySector: public Sector
{
public:
    BuildingSerSupplySector( const std::string regionName );
    virtual ~BuildingSerSupplySector();
	static const std::string& getXMLNameStatic();
    void checkSectorCalData( const int period );
protected:
 	virtual const std::string& getXMLName() const; 
private:
	static const std::string XML_NAME; //!< node name for toXML methods
};

#endif // BLDG_SER_SUPPLY_SECTOR_H_

