#ifndef _BUILDLING_SECTOR_H_
#define _BUILDLING_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file demand_sector.h
* \ingroup CIAM
* \brief The DemandSector class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/demand_sector.h"

// Forward declarations
class GDP;

/*! 
* \ingroup CIAM
* \brief A class which defines a single building demand sector.
*
*  The building demand sector is derived from the demand sector class.  The building demand sector
*  calculates the demand for building servicies in terms of square feet. The actual building service is
*  supplied by a number of separate supply sectors. No energy is used directly by this sector or its technologies.
*
* \author Steve Smith
*/

class BuildingDemandSector: public DemandSector
{
public:
    BuildingDemandSector( const std::string regionName );
    virtual ~BuildingDemandSector();
	static const std::string& getXMLNameStatic();
    virtual void initCalc( const int period );
    void aggdemand( const GDP* gdp, const int period );
protected:
    double heatingDegreeDays; //!< demand equation based on per capita GNP, true or false.
    double coolingDegreeDays; //!< base year energy price elasticity
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    virtual void toOutputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
 	virtual const std::string& getXMLName() const; 
    std::vector<double> baseService; //!< base service level for a given year
    baseScaler; // scaler for determing demand for future years
    
private:
	static const std::string XML_NAME; //!< node name for toXML methods
};

#endif // _BUILDLING_SECTOR_H_

