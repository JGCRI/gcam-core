#ifndef _CPRICESUBSECTOR_H_
#define _CPRICESUBSECTOR_H_
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
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/Subsector.h"

// Forward declarations
class Summary;
class technology;
class hydro_tech;
class Emcoef_ind;
class Tabs;
class GDP;


/*! 
* \ingroup CIAM
* \brief A temporary class that uses the carbon value for a fuel in the sub-sector share calcuation

* This is to make gasohol, etc. sub-sector shares reflect the carbon price.
* Is a stopgap until emissions re-calcuation is recoded.

* The subsector contains a group of technology objects, which produce or consume commodities in the marketplace. Each sub-sector has attributes such as share, share weight, logit expoential, fixed capacity, and capacity limits. 

* \author Steve Smith
*/

class Cpricesubsector: public Subsector {

protected:
 
public:
     Cpricesubsector( const std::string regionName, const std::string sectorName );
     void calcShare( const int period, const GDP* gdp ); 
};
#endif // _CPRICESUBSECTOR_H_
