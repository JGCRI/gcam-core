#ifndef _SGM_GEN_TABLE_H_
#define _SGM_GEN_TABLE_H_
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
* \file sgm_gen_table.h
* \ingroup CIAM
* \brief SGMGenTable class header file.
*
*  Detailed description.
*
* \author Katherine Chung, Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <map>
#include <iosfwd>
#include "reporting/include/output_container.h"

class Region;
class NationalAccount;
class Demographic;
class Sector;
class ProductionSector;
class Subsector;
class BaseTechnology;
class Consumer;
class HouseholdConsumer;
class GovtConsumer;
class InvestConsumer;
class TradeConsumer;
class ProductionTechnology;
class FactorSupply;
class Input;
class Modeltime;

class SGMGenTable : public OutputContainer {
public:
    SGMGenTable( const std::string& aName, const std::string& aHeader, const Modeltime* aModeltime );
	void output( std::ostream& aSGMGenFile, const int aPeriod ) const;
	void updateRegionCGE( const RegionCGE* regionCGE );
	void updateProductionSector( const ProductionSector* aProductionSector, const int aPeriod );
    void updateDemographic( const Demographic* aDemographic, const int aPeriod );
    void updateConsumer( const Consumer* aConsumer, const int aPeriod );
	void updateHouseholdConsumer( const HouseholdConsumer* householdConsumer, const int aPeriod );
	void updateGovtConsumer( const GovtConsumer* govtConsumer, const int aPeriod );
	void updateTradeConsumer( const TradeConsumer* tradeConsumer, const std::string& aRegionName, const int aPeriod );
	void updateInvestConsumer( const InvestConsumer* investConsumer, const int aPeriod );
	void updateProductionTechnology( const ProductionTechnology* prodTech, 
		const std::string& aRegionName, const std::string& aSectorName, const int aPeriod );
    void updateFactorSupply( const FactorSupply* aFactorSupply, const int aPeriod );
    void updateNationalAccount( const NationalAccount* aNationalAccount, const int aPeriod );
private:

    void addToType( const int aTypeRow, const std::string aTypeCol, const double value );
    void setType( const int aTypeRow, const std::string aTypeCol, const double value );
    double getValue( const int aTypeRow, const std::string aTypeCol) const;

    const std::string mName;
    const std::string mHeader;
    std::map<int, std::map< std::string, double> > mTable;
    const Modeltime* mModeltime;
};

#endif // _SGM_GEN_TABLE_H_

