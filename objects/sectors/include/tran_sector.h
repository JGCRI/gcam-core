#ifndef _TRANSECTOR_H_
#define _TRANSECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/* TransSector.h									            *
* This header contains the Specialized Transportation Sector   *
* MAW  3/11/03							                        *
* Revised to work with latest code.                            *
* SHK 6/30/03                                                  *
*/

#include <vector>
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/demand_sector.h"

// Forward declarations
class GDP;

// transportation demand sector class derived from demsector class
class TranSector : public DemandSector
{
protected:
    std::vector<double> percentLicensed; //!< Percent of population licensed
    double baseScaler; //!< constant scaler to scale base output
    double baseScalerNotLic; //!< constant scaler to scale base unlicensed output
    double priceRatioNotLic;//!< price ratio for unlicensed population
public:
    TranSector( const std::string regionName );
    virtual ~TranSector();
    virtual void clear();
    virtual void XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    virtual void aggdemand(  const GDP* gdp, const int period  ); 
};

#endif // _TRANSSECTOR_H_

