#ifndef _FOREST_SUPPLY_SUBSECTOR_H_
#define _FOREST_SUPPLY_SUBSECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file forest_supply_subsector.h
* \ingroup CIAM
* \brief The ForestSupplySubsector class header file.
* \author James Blackwood
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/subsector.h"

/*! 
* \ingroup Objects
* \brief A class which defines a single ForestSupplySubsector of the model.
* \details The forest supply subsector exists solely to create
*          ForestProductTechnologies.
* \author James Blackwood
*/

class ForestSupplySubsector : public Subsector {
public:
    ForestSupplySubsector( const std::string& aRegionName,
                           const std::string& aSectorName );

    virtual ~ForestSupplySubsector();
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
private:
};
#endif // _FOREST_SUPPLY_SUBSECTOR_H_
