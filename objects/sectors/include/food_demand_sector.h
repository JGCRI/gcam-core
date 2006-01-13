#ifndef _FOOD_DEMAND_SECTOR_H_
#define _FOOD_DEMAND_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file food_demand_sector.h
* \ingroup Objects
* \brief The FoodDemandSector class header file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/demand_sector.h"

// Forward declarations
class GDP;

class FoodDemandSector: public DemandSector {
public:
	FoodDemandSector( const std::string regionName );
	static const std::string& getXMLNameStatic();
	virtual double getEnergyInput( const int aPeriod ) const;
	virtual double getWeightedEnergyPrice ( const int aPeriod ) const;
	virtual void tabulateFixedDemands( const int period, const GDP* gdp );
	virtual void aggdemand( const GDP* gdp, const int period );
	virtual void scaleCalibratedValues( const int period, const std::string& goodName, const double scaleValue );

protected:
    // TODO: What period is this value in?
	double caloriesperCapDay; //!< calories per person per day.
	std::vector<double> baseConsumptionPerCap; //!< base consumption level for a given year

	virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
	virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
	virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
	virtual const std::string& getXMLName() const;

private:
	double calcServiceDemand( const GDP* gdp, const int period );
};

#endif // _FOOD_DEMAND_SECTOR_H_

