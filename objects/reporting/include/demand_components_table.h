#ifndef _DEMAND_COMPONENTS_TABLE_H_
#define _DEMAND_COMPONENTS_TABLE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Laboratory operated by Battelle Memorial
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
* \author Katherine Chung
*/

#include <string>
#include <memory>
#include <iosfwd>
#include "util/base/include/default_visitor.h"

class RegionCGE;
class HouseholdConsumer;
class GovtConsumer;
class InvestConsumer;
class TradeConsumer;
class ProductionTechnology;
class Input;
class StorageTable;

/*!
 * \brief A visitor which collects reporting information on demands for goods.
 */
class DemandComponentsTable : public DefaultVisitor {
public:
    DemandComponentsTable( std::ostream& aFile );
    void finish() const;
	void startVisitRegionCGE( const RegionCGE* regionCGE, const int aPeriod );
	void updateHouseholdConsumer( const HouseholdConsumer* householdConsumer, const int aPeriod );
	void updateGovtConsumer( const GovtConsumer* govtConsumer, const int aPeriod );
	void updateTradeConsumer( const TradeConsumer* tradeConsumer, const int aPeriod );
	void updateInvestConsumer( const InvestConsumer* investConsumer, const int aPeriod );
	void updateProductionTechnology( const ProductionTechnology* prodTech, const int aPeriod );
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

    //! The file to which to write.
    std::ostream& mFile;

    std::auto_ptr<StorageTable> mTable; //!< Internal storage table.
};

#endif // _DEMAND_COMPONENTS_TABLE_H_
