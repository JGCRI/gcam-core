#ifndef BLDG_SUBSECTOR_H_
#define BLDG_SUBSECTOR_H_
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
* \brief A class which defines a building demand Subsector of the model.

* The subsector contains a group of building demand technology objects, 
* which act differently than normal technology objects in that they each generate a demand for a 
* specific building service (heating, cooling, lighting, etc.), which is then provided by 
* a supply sector. Therefore, this subsector does not share between technologies.
* This subsector also mediates information flow between the supply sectors and the building demand 
* technologies through marketInfo and other mechanisms.

* \author Steve Smith
*/

class BuildingSubSector : public Subsector
{
private:
    static const std::string XML_NAME; //!< node name for toXML methods
protected:
    std::vector<double> dayLighting; //!< amount of lighting need provided by daylighting
    std::vector<double> aveInsulation; //!< average insulation value (J/s-m^2) for this building type
    std::vector<double> floorToSurfaceArea; //!< conversion from floor space to surface area for this building type
    bool XMLDerivedClassParse( const std::string nodeName, const xercesc::DOMNode* curr );
public:
    BuildingSubSector( const std::string regionName, const std::string sectorName );
    virtual ~BuildingSubSector();
    virtual const std::string& getXMLName() const;
    static const std::string& getXMLNameStatic();
    void calcShare(const int period, const GDP* gdp );
    void initCalc( const int period, std::auto_ptr<MarketInfo> mSectorInfo );
    void adjustForCalibration( double sectorDemand, double totalfixedOutput, double totalCalOutputs, const bool allFixedOutput, const int period ); 
    const std::string& getChildXMLName() const;
    technology* createChild() const;
};
#endif // BLDG_SUBSECTOR_H_
