#ifndef _DEMAND_SECTOR_H_
#define _DEMAND_SECTOR_H_
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
#include "sectors/include/sector.h"

// Forward declarations
class GDP;

/*! 
* \ingroup CIAM
* \brief A class which defines a single demand sector.
*
*  The demand sector is derived from the sector class.  The demand sector
*  is similar to the supply sector except that it represents a service sector
*  and incorporates a demand function that determines the total demand for the
*  service.
*  The demand sector is not a Final Demand sector, but combines a service sector with 
*  a final demand for the service.
*
*  In the future, the demand sector should be treated as a supply sector and a 
*  separate Final Demand Sector class should be created to drive the demand for
*  the service.  This is representative of the general equilibrium framework.
*
* \author Sonny Kim
*/

class DemandSector: public Sector
{
protected:
    bool perCapitaBased; //!< demand equation based on per capita GNP, true or false.
    double pElasticityBase; //!< base year energy price elasticity
    double priceRatio; //!< temp price ratio
    std::vector<double> sectorFuelCost; // !< fuel cost portion of the total cost
    std::vector<double> finalEngyCons; //!< end-use sector final energy consumption
    std::vector<double> service; //!< total end-use sector service
    std::vector<double> servicePreTechChange; //!< total end-use sector service before cummulative technical change is applied.
    std::vector<double> iElasticity; //!< income elasticity 
    std::vector<double> pElasticity; //!< price elasticity.
    std::vector<double> aeei; //!< autonomous end-use energy intensity parameter
    std::vector<double> techChangeCumm; //!< cummulative technical change on end-use service
    virtual void calcPrice( const int period );
    virtual void printStyle( std::ostream& outStream ) const;

public:
    DemandSector( const std::string regionName );
    virtual ~DemandSector();
    virtual void clear();
    virtual void XMLDerivedClassParseAttr( const xercesc::DOMNode* node ); 
    virtual void XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    virtual void toXML( std::ostream& out, Tabs* tabs ) const;
    virtual void toOutputXML( std::ostream& out, Tabs* tabs ) const;
    virtual void toXMLDerivedClass( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual void setMarket();
    virtual void calcShare( const int period, const GDP* gdp );
    virtual void calc_pElasticity( const int period );
    virtual void aggdemand( const GDP* gdp, const int period ); 
    virtual void outputfile() const;
    virtual void MCoutput() const;
    virtual void calibrateSector( const int period );
    double getService( const int period ) const;
    double getServiceWoTC( const int period ) const;
    void scaleOutput( const int period, double scaleFactor );
};

#endif // _DEMAND_SECTOR_H_

