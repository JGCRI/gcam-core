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
#include "subsector.h"

using namespace std;
using namespace xercesc;

/*! 
* \ingroup CIAM
* \brief A class which defines a single demand sector.
* \author Sonny Kim
*/

class demsector : public sector
{
protected:
	int perCapitaBased; //!< demand equation based on per capita GNP, true or false
	double pElasticityBase; //!< base year energy price elasticity
	double priceRatio; //!< temp price ratio
	vector<double> sectorfuelprice; // temp vec
	vector<double> fe_cons; //!< end-use sector final energy consumption
	vector<double> service; //!< total end-use sector service 
	vector<double> iElasticity; //!< income elasticity 
	vector<double> pElasticity; //!< price elasticity.
	vector<double> aeei; //!< autonomous end-use energy intensity parameter
	vector<double> techChangeCumm; //!< cummulative technical change on end-use service

public:
	demsector();
	virtual void clear();
	virtual void XMLParse(const DOMNode* node);
	virtual void toXML( ostream& out ) const;
	virtual void toDebugXML( const int period, ostream& out ) const;
	virtual void setMarket( const string& regname );
	virtual void calc_share( const string regionName, const int per, const double gnp_cap = 1 );
	virtual void calc_pElasticity( const int per );
	virtual void aggdemand( const string& regionName, const double gnp_cap, const double gnp, const int per); 
	virtual void outputfile( const string& regionName );
	virtual void MCoutput( const string& regionName );
};

#endif // _DEMAND_SECTOR_H_