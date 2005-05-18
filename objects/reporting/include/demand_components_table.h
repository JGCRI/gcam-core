#ifndef _DEMAND_COMPONENTS_TABLE_H_
#define _DEMAND_COMPONENTS_TABLE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Labratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file demand_components_table.h
* \ingroup Objects
* \brief DemandComponentsTable class header file.
*
*  Detailed description.
*
* \author Katherine Chung
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <memory>
#include <iosfwd>
#include "reporting/include/output_container.h"

class RegionCGE;
class HouseholdConsumer;
class GovtConsumer;
class InvestConsumer;
class TradeConsumer;
class ProductionTechnology;
class Input;
class StorageTable;

class DemandComponentsTable : public OutputContainer {
public:
    DemandComponentsTable();
    void output( std::ostream& aFile, const int period ) const;
	void updateRegionCGE( const RegionCGE* regionCGE );
	void updateHouseholdConsumer( const HouseholdConsumer* householdConsumer, const int aPeriod );
	void updateGovtConsumer( const GovtConsumer* govtConsumer, const int aPeriod );
	void updateTradeConsumer( const TradeConsumer* tradeConsumer, const std::string& aRegionName, const int aPeriod );
	void updateInvestConsumer( const InvestConsumer* investConsumer, const int aPeriod );
	void updateProductionTechnology( const ProductionTechnology* prodTech, 
		const std::string& aRegionName, const std::string& aSectorName, const int aPeriod );
private:
    //! The type of the demand components category.
    enum CategoryType {
        TOTAL,
        INTERMED,
        CONSUMPTION,
        INVESTMENT,
        GOVERNMENT,
        TRADE
    };

    const std::string& getLabel( const CategoryType aType ) const;
    std::auto_ptr<StorageTable> mTable; //!< Internal storage table.
};

#endif // _DEMAND_COMPONENTS_TABLE_H_
