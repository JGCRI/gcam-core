/* TransSector.h									            *
 * This header contains the Specialized Transportation Sector   *
 * MAW  3/11/03							                        *
 * Revised to work with latest code.                            *
 * SHK 6/30/03                                                  *
*/

#ifndef _TRANSECTOR_H_
#define _TRANSECTOR_H_
#pragma once

#include <vector>
#include <xercesc/dom/DOM.hpp>
#include "DemandSector.h"
// xerces xml headers
#include <xercesc/util/XMLString.hpp>

using namespace std; // enables elimination of std::
using namespace xercesc;

// transportation demand sector class derived from demsector class
class tranSector : public demsector
{
protected:
	vector<double> percentLicensed; // Percent of population licensed
	double baseScaler; // constant scaler to scale base output
    // for unlicensed percentage of population
	double baseScalerNotLic; // constant scaler to scale base unlicensed output
    double priceRatioNotLic;// price ratio for unlicensed population
public:
    tranSector();
    virtual ~tranSector();
	virtual void clear();
    virtual void XMLDerivedClassParse( const string nodeName, const DOMNode* curr ); 
	// aggregate demand for service
	virtual void aggdemand( const string& regionName, const double gnp_cap, const double gnp, const int per); 
};

#endif // _TRANSSECTOR_H_