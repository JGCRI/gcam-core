#ifndef _FOOD_SUPPLY_SUBSECTOR_H_
#define _FOOD_SUPPLY_SUBSECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file food_supply_subsector.h
* \ingroup CIAM
* \brief The FoodSupplySubsector class header file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/subsector.h"

/*! 
* \ingroup Objects
* \brief A class which defines a single FoodSupplySubsector of the model.
* \details This subsector exists soley to create FoodProductionTechnologies.
* \author James Blackwood
*/

class FoodSupplySubsector : public Subsector {
public:
    FoodSupplySubsector( const std::string& regionName, const std::string& sectorName );
    virtual ~FoodSupplySubsector();
    static const std::string& getXMLNameStatic();
protected:
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual const std::string& getXMLName() const;
    bool isNameOfChild( const std::string& nodename ) const;

    virtual ITechnology* createChild( const std::string& aTechType,
                                     const std::string& aTechName,
                                     const int aTechYear ) const;

    virtual void MCDerivedClassOutput() const;
};
#endif // _FOOD_SUPPLY_SUBSECTOR_H_
