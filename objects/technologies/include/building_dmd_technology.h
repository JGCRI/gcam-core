#ifndef _BLDG_TECHNOLOGY_H_
#define _BLDG_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file technology.h
* \ingroup CIAM
* \brief The technology class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <map>
#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/technology.h"

// Forward declaration
class GDP;

/*! 
* \ingroup CIAM
* \brief This building technology class calculates demand for building energy services.
*
* Building demand technology objects, act differently than normal technology objects in that they 
* each generate a demand for a 
* specific building service (heating, cooling, lighting, etc.), which is then provided by 
* a supply sector.
* These technologies do not consume fuels or generate GHG emissions. These come from the supply sectors.
*
* \author Steve Smith
*/

class BuildingDmdTechnology : public technology
{
public:
    BuildingDmdTechnology(); // default construtor
    BuildingDmdTechnology& operator=( const BuildingDmdTechnology& techIn ); // assignment operator.
    virtual BuildingDmdTechnology* clone() const;
    virtual ~BuildingDmdTechnology();
    virtual bool XMLDerivedClassParse( const std::string nodeName, const xercesc::DOMNode* curr ); // for derived classes
    virtual void calcShare( const std::string& regionName, const GDP* gdp, const int per ); 
    virtual void production(const std::string& regionName,const std::string& prodName,double dmd, const GDP* gdp, const int per);
    virtual void adjustForCalibration( double subSectorDemand );
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toOutputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual void initCalc( std::auto_ptr<MarketInfo> mSubsectorInfo );
private:
    static const std::string XML_NAME1D; //!< tag name for toInputXML
    static const std::string XML_NAME2D; //!< tag name for toInputXML
protected:
    double saturation; //!< penetration level for this technology
    double unitDemand; //!< Demand per unit driver (usually floor space) 
};
#endif // _BLDG_TECHNOLOGY_H_

