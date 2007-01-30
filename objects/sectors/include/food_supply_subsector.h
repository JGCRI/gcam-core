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
*/

#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/subsector.h"

/*! 
* \ingroup Objects
* \brief A class which defines a single FoodSupplySubsector of the model.
* \details This subsector exists solely to create FoodProductionTechnologies.
* \author James Blackwood
*/

class FoodSupplySubsector : public Subsector {
public:
    FoodSupplySubsector( const std::string& regionName, const std::string& sectorName );
    virtual ~FoodSupplySubsector();
    static const std::string& getXMLNameStatic();

    virtual double calcShare( const int aPeriod,
                              const GDP* aGDP ) const;

    virtual void adjustForCalibration( double aSubsectorVariableDemand,
                                       const GDP* aGDP,
                                       const int aPeriod );
protected:
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual const std::string& getXMLName() const;
    bool isNameOfChild( const std::string& nodename ) const;

    virtual ITechnology* createChild( const std::string& aTechType,
                                      const std::string& aTechName,
                                      const int aTechYear ) const;

    virtual void MCoutputAllSectors( const GDP* aGDP,
                                     const IndirectEmissionsCalculator* aIndirectEmissionsCalc,
                                     const std::vector<double> aSectorOutput ) const;
};
#endif // _FOOD_SUPPLY_SUBSECTOR_H_
