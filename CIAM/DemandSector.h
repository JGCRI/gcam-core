#ifndef _DEMAND_SECTOR_H_
#define _DEMAND_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file DemandSector.h
* \ingroup CIAM
* \brief The demsector class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <xercesc/dom/DOM.hpp>
#include "sector.h"

// Forward declaration
class subsector;

/*! 
* \ingroup CIAM
* \brief A class which defines a single demand sector.
* \author Sonny Kim
*/

class demsector: public sector
{
protected:
    bool perCapitaBased; //!< demand equation based on per capita GNP, true or false.
    double pElasticityBase; //!< base year energy price elasticity
    double priceRatio; //!< temp price ratio
    std::vector<double> sectorfuelprice; // temp vec
    std::vector<double> fe_cons; //!< end-use sector final energy consumption
    std::vector<double> service; //!< total end-use sector service
    std::vector<double> servicePreTechChange; //!< total end-use sector service before cummulative technical change is applied.
    std::vector<double> iElasticity; //!< income elasticity 
    std::vector<double> pElasticity; //!< price elasticity.
    std::vector<double> aeei; //!< autonomous end-use energy intensity parameter
    std::vector<double> techChangeCumm; //!< cummulative technical change on end-use service
    
public:
    demsector();
    virtual ~demsector();
    virtual void clear();
    virtual void XMLDerivedClassParseAttr( const xercesc::DOMNode* node ); 
    virtual void XMLDerivedClassParse( const std::string nodeName, const xercesc::DOMNode* curr ); 
    virtual void toXML( std::ostream& out ) const;
    virtual void toOutputXML( std::ostream& out ) const;
    virtual void toXMLDerivedClass( std::ostream& out ) const;
    virtual void toDebugXML( const int period, std::ostream& out ) const;
    virtual void setMarket( const std::string& regname );
    virtual void calc_share( const std::string regionName, const int per, const double gnp_cap = 1 );
    virtual void price(int per);
    virtual void calc_pElasticity( const int per );
    virtual void aggdemand( const std::string& regionName, const double gnp_cap, const double gnp, const int per); 
    virtual void outputfile( const std::string& regionName );
    virtual void MCoutput( const std::string& regionName );
    virtual void calibrateSector( const std::string regionName, const int per );
    double getService(const int per);
    double getServiceWoTC(const int per);
    void scaleOutput( int per, double scaleFactor );
};

#endif // _DEMAND_SECTOR_H_

