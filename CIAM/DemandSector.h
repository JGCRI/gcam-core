#ifndef _DEMAND_SECTOR_H_
#define _DEMAND_SECTOR_H_
#pragma once

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

using namespace std;
using namespace xercesc;

// Forward declaration
class subsector;

/*! 
* \ingroup CIAM
* \brief A class which defines a single demand sector.
* \author Sonny Kim
*/

class demsector : public sector
{
protected:
    bool perCapitaBased; //!< demand equation based on per capita GNP, true or false.
    double pElasticityBase; //!< base year energy price elasticity
    double priceRatio; //!< temp price ratio
    vector<double> sectorfuelprice; // temp vec
    vector<double> fe_cons; //!< end-use sector final energy consumption
    vector<double> service; //!< total end-use sector service
    vector<double> servicePreTechChange; //!< total end-use sector service before cummulative technical change is applied.
    vector<double> iElasticity; //!< income elasticity 
    vector<double> pElasticity; //!< price elasticity.
    vector<double> aeei; //!< autonomous end-use energy intensity parameter
    vector<double> techChangeCumm; //!< cummulative technical change on end-use service
    
public:
    demsector();
    virtual ~demsector();
    virtual void clear();
    virtual void XMLDerivedClassParseAttr( const DOMNode* node ); 
    virtual void XMLDerivedClassParse( const string nodeName, const DOMNode* curr ); 
    virtual void toXML( ostream& out ) const;
    virtual void toOutputXML( ostream& out ) const;
    virtual void toXMLDerivedClass( ostream& out ) const;
    virtual void toDebugXML( const int period, ostream& out ) const;
    virtual void setMarket( const string& regname );
    virtual void calc_share( const string regionName, const int per, const double gnp_cap = 1 );
    virtual void price(int per); // calculates sector price
    virtual void calc_pElasticity( const int per );
    virtual void aggdemand( const string& regionName, const double gnp_cap, const double gnp, const int per); 
    virtual void outputfile( const string& regionName );
    virtual void MCoutput( const string& regionName );
    virtual void calibrateSector( const string regionName, const int per ); // sets demand to totoutput and output
    double getService(const int per); // sector service with tech change
    double getServiceWoTC(const int per); // service without tech change
};

#endif // _DEMAND_SECTOR_H_