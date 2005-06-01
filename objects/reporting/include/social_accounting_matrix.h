#ifndef _SOCIAL_ACCOUNTING_MATRIX_H_
#define _SOCIAL_ACCOUNTING_MATRIX_H_
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
* \file social_accounting_matrix.h
* \ingroup Objects
* \brief SocialAccountingMatrix class header file.
* \author Pralit Patel
* \author Katherine Chung
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <memory>
#include <iosfwd>
#include "reporting/include/output_container.h"



class Region;
class Sector;
class Subsector;
class BaseTechnology;
class HouseholdConsumer;
class GovtConsumer;
class InvestConsumer;
class TradeConsumer;
class ProductionTechnology;
class FactorSupply;
class Input;
class StorageTable;

/*! 
* \ingroup Objects
* \brief An object which outputs a social accounting matrix.
* \details TODO
* \author Pralit Patel, Katherine Chung
*/
class SocialAccountingMatrix : public OutputContainer {
public:
    SocialAccountingMatrix( const std::string& aRegionName );
    void output( std::ostream& aFile, const int period ) const;
	void updateHouseholdConsumer( const HouseholdConsumer* householdConsumer, const int aPeriod );
	void updateGovtConsumer( const GovtConsumer* govtConsumer, const int aPeriod );
	void updateTradeConsumer( const TradeConsumer* tradeConsumer, const std::string& aRegionName, const int aPeriod );
	void updateInvestConsumer( const InvestConsumer* investConsumer, const int aPeriod );
	void updateProductionTechnology( const ProductionTechnology* prodTech, 
		const std::string& aRegionName, const std::string& aSectorName,  const int period );
	void updateFactorSupply( const FactorSupply* factorSupply, const int period );
private:
    enum CategoryType {
		ACTIVITIES,
		COMMODITIES,
		FACTORS_LAND,
		FACTORS_LABOR,
		FACTORS_CAPITAL,
		HOUSEHOLDS,
		ENTERPRISES,
		GOVERNMENT,
		CAPITAL_ACCOUNT,
		REST_OF_WORLD,
		TOTALS
    };
    void addToType( CategoryType aRowCat, CategoryType aColCat, double aValue );
    static const std::string& getString( const CategoryType aType );
    
    std::auto_ptr<StorageTable> mTable;
    const std::string mRegionName;
};

#endif // _SOCIAL_ACCOUNTING_MATRIX_H_
