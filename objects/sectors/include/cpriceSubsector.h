#ifndef _CPRICESUBSECTOR_H_
#define _CPRICESUBSECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file cpriceSubsector.h
* \ingroup 
* \brief The Cpricesubsector class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>
#include "sectors/include/subsector.h"

/*! 
* \ingroup CIAM
* \brief A temporary class that uses the carbon value for a fuel in the sub-sector share calcuation

* This is to make gasohol, etc. sub-sector shares reflect the carbon price.
* Is a stopgap until emissions re-calcuation is recoded.

* The subsector contains a group of technology objects, which produce or consume commodities in the marketplace. Each sub-sector has attributes such as share, share weight, logit expoential, fixed capacity, and capacity limits. 

* \author Steve Smith
*/

class Cpricesubsector: public Subsector {
public:
     Cpricesubsector( const std::string regionName, const std::string sectorName );
     void calcShare( const int period, const GDP* gdp ); 
	 const std::string& getXMLName() const;
	 static const std::string& getXMLNameStatic();
private:
	static const std::string XML_NAME; //!< node name for toXML methods
};
#endif // _CPRICESUBSECTOR_H_
