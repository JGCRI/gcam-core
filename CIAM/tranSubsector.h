/* TransSubSector.h									*
 * This header contains the Specialized Transportation SubSector
 * MAW  3/11/03						
 * Revised to work with latest code.
 * SHK  6/30/03
*/

#ifndef _TRANSUBSECTOR_H_
#define _TRANSUBSECTOR_H_
#pragma once

#include <vector>
#include <xercesc/dom/DOM.hpp>
#include "subsector.h"
// xerces xml headers
#include <xercesc/util/XMLString.hpp>

using namespace std; // enables elimination of std::
using namespace xercesc;

// transportation demand subsector class derived from base subsector class
// Modes of transportation are implemented as subsectors
class tranSubsector : public subsector
{
protected:
	vector<double> techChange; // Energy Intensity decrease per year of mode
	vector<double> speed; // Speed of Mode in Miles/hour
	vector<double> popDenseElasticity; // Population Density Elasticity of mode
	vector<double> adjPrice; // subsector price adjusted for value of time, scaled by pd.
	vector<double> loadFactor; //Load factor, persons or tons per vehicle (pass./freight)
	double popDensity; // population density per land area

public:
    tranSubsector();
	virtual void clear();
    virtual void XMLDerivedClassParse( const string nodeName, const DOMNode* curr ); // for derived classes
    virtual void calcShare( const string& regionName, const int period, const double gnp_cap = 1 ); 
    virtual void setoutput( const string& regionName, const string& prodName, const double dmd, const int per);

};


#endif // _TRANSSUBSECTOR_H_