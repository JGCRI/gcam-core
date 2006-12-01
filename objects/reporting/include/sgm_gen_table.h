#ifndef _SGM_GEN_TABLE_H_
#define _SGM_GEN_TABLE_H_
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
* \file sgm_gen_table.h
* \ingroup CIAM
* \brief SGMGenTable class header file.
*
*  Detailed description.
*
* \author Katherine Chung, Sonny Kim
*/

#include <string>
#include <map>
#include <iosfwd>
#include "util/base/include/default_visitor.h"

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

/*!
 * \brief A visitor which constructs the general SGM output tables.
 */
class SGMGenTable : public DefaultVisitor {
public:
    SGMGenTable( const std::string& aName, const std::string& aHeader,
                 const Modeltime* aModeltime );
    void finish() const;
    void startVisitRegionCGE( const RegionCGE* regionCGE, const int aPeriod );
    void endVisitRegionCGE( const RegionCGE* aRegionCGE, const int aPeriod );
    void startVisitSector( const Sector* aSector, const int aPeriod );
    void endVisitSector( const Sector* aSector, const int aPeriod );
    void startVisitProductionSector( const ProductionSector* aProductionSector, const int aPeriod );
    void startVisitDemographic( const Demographic* aDemographic, const int aPeriod );
    void startVisitConsumer( const Consumer* aConsumer, const int aPeriod );
    void startVisitHouseholdConsumer( const HouseholdConsumer* householdConsumer, const int aPeriod );
    void startVisitGovtConsumer( const GovtConsumer* govtConsumer, const int aPeriod );
    void startVisitTradeConsumer( const TradeConsumer* tradeConsumer, const int aPeriod );
    void startVisitInvestConsumer( const InvestConsumer* investConsumer, const int aPeriod );
    void startVisitProductionTechnology( const ProductionTechnology* prodTech, const int aPeriod );
    void startVisitFactorSupply( const FactorSupply* aFactorSupply, const int aPeriod );
    void startVisitNationalAccount( const NationalAccount* aNationalAccount, const int aPeriod );

    // Non-interface function.
    void setOutputFile( std::ostream& aOutputFile );
private:

    void addToType( const int aTypeRow, const std::string aTypeCol, const double value );
    void setType( const int aTypeRow, const std::string aTypeCol, const double value );
    double getValue( const int aTypeRow, const std::string aTypeCol) const;

    const std::string mName;
    const std::string mHeader;

    //! The current region name.
    std::string mCurrentRegionName;
    
    //! The current sector name.
    std::string mCurrentSectorName;
    
    //! The file to which to write.
    std::ostream* mFile;

    std::map<int, std::map< std::string, double> > mTable;
    const Modeltime* mModeltime;
};

#endif // _SGM_GEN_TABLE_H_

