#ifndef _FOREST_SUPPLY_SECTOR_H_
#define _FOREST_SUPPLY_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file forest_supply_sector.h
* \ingroup Objects
* \brief The ForestSupplySector class header file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/food_supply_sector.h"

class ForestSupplySector : public FoodSupplySector {
public:
	ForestSupplySector( std::string& regionName );
	virtual ~ForestSupplySector();
	static const std::string& getXMLNameStatic();
protected:
	virtual void setMarket();
	virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
	virtual const std::string& getXMLName() const;
private:
	static const std::string prefix;

    //! Name of the market for the good.
    std::string mMarket;
};

#endif // _FOREST_SUPPLY_SECTOR_H_
