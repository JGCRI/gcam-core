#ifndef _FOOD_DEMAND_TECHNOLOGY_H_
#define _FOOD_DEMAND_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file food_demand_technology.h
* \ingroup CIAM
* \brief The FoodDemandTechnology class header file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/technology.h"

/*! 
* \ingroup CIAM
* \brief This technology class is based on the MiniCAM description of technology.
*
* The Technology class where demand is calibrated.
*
* \author James Blackwood
*/

class FoodDemandTechnology : public technology {
public:
	FoodDemandTechnology();
	static const std::string& getXMLNameStatic1D();
	virtual void tabulateFixedDemands( const std::string regionName, const int period, const IInfo* aSubsectorIInfo );
protected:
	virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
	virtual const std::string& getXMLName1D() const;
private:

};

#endif // _FOOD_DEMAND_TECHNOLOGY_H_

