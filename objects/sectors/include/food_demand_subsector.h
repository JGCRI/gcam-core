#ifndef _FOOD_DEMAND_SUBSECTOR_H_
#define _FOOD_DEMAND_SUBSECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file food_demand_subsector.h
* \ingroup Objects
* \brief The FoodDemandSubsector class header file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/subsector.h"

/*! 
* \ingroup Objects
* \brief A class which defines a single FoodDemandSubsector of the model.
* \details This class exists solely to create FoodDemandTechnologies.
* \author James Blackwood
*/

class FoodDemandSubsector : public Subsector {
public:
	FoodDemandSubsector( const std::string regionName, const std::string sectorName );
	static const std::string& getXMLNameStatic();
protected:
	virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
	virtual const std::string& getXMLName() const;
    bool isNameOfChild( const std::string& nodename ) const;
    virtual technology* createChild( const std::string& nodename ) const;
};
#endif // _FOOD_DEMAND_SUBSECTOR_H_
