#ifndef _OUTPUT_CONTAINER_H_
#define _OUTPUT_CONTAINER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file expenditure.h
* \ingroup Objects
* \brief OutputContainer class header file.
*
*  Detailed description.
*
* \author Pralit Patel, Katherine Chung
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <iosfwd>

class Region;
class RegionCGE;
class NationalAccount;
class Demographic;
class Sector;
class Subsector;
class BaseTechnology;
class Consumer;
class HouseholdConsumer;
class GovtConsumer;
class InvestConsumer;
class TradeConsumer;
class ProductionTechnology;
class DemandInput;
class ProductionInput;
class Input;
class FactorSupply;
class ProductionSector;

class OutputContainer {
public:
    OutputContainer(){};
    virtual ~OutputContainer(){};
    virtual void output( std::ostream& aFile, const int aPeriod ) const = 0;
    virtual void updateRegion( const Region* aRegion ){}
    virtual void updateRegionCGE( const RegionCGE* aRegionCGE ){}
    virtual void updateDemographic( const Demographic* aDemographic, const int aPeriod ){}
    virtual void updateSector( const Sector* aSector ){}
    virtual void updateProductionSector( const ProductionSector* aProdSector, const int aPeriod ){}
    virtual void updateSubsector( const Subsector* aSubsector ){}
    virtual void updateBaseTechnology( const BaseTechnology* aBaseTechnology ){}
    virtual void updateConsumer( const Consumer* aConsumer, const int aPeriod ){}
	virtual void updateHouseholdConsumer( const HouseholdConsumer* aHouseholdConsumer,
        const int aPeriod ){}
    virtual void updateGovtConsumer( const GovtConsumer* aGovtConsumer, const int aPeriod ){}
    virtual void updateInvestConsumer( const InvestConsumer* aInvestConsumer, const int aPeriod ){}
    virtual void updateTradeConsumer( const TradeConsumer* aTradeConsumer, const std::string& aRegionName, 
		const int aPeriod ){}
	virtual void updateProductionTechnology( const ProductionTechnology* aProductionTechnology,
        const std::string& aRegionName, const std::string& aSectorName, const int aPeriod ){}
    virtual void updateFactorSupply( const FactorSupply* aFactorSupply, const int aPeriod ){}
    virtual void updateNationalAccount( const NationalAccount* aNationalAccount, const int aPeriod ){}
    virtual void updateInput( const Input* aInput ){}
    virtual void updateProductionInput( const ProductionInput* aProdInput ){}
    virtual void updateDemandInput( const DemandInput* aDemandInput ){}
};

#endif // _OUTPUT_CONTAINER_H_
