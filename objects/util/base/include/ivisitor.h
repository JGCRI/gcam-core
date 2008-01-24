#ifndef _IVISITOR_H_
#define _IVISITOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* This software, which is provided in confidence, was prepared by employees of
* Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
* Battelle has certain unperfected rights in the software which should not be
* copied or otherwise disseminated outside your organization without the express
* written authorization from Battelle. All rights to the software are reserved
* by Battelle. Battelle makes no warranty, express or implied, and assumes no
* liability or responsibility for the use of this software.
*/

/*! 
* \file ivisitor.h
* \ingroup Objects
* \brief IVisitor class header file.
* \author Josh Lurz
*/

#include <string>

class World;
class Region;
class RegionCGE;
class RegionMiniCAM;
class NationalAccount;
class Demographic;
class Sector;
class Subsector;
class BuildingDemandSubSector;
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
class AResource;
class Technology;
class Expenditure;
class AResource;
class DemandSector;
class Scenario;
class AGHG;
class OutputMetaData;
class Marketplace;
class Market;
class SubResource;
class Grade;
class Population;
class PopulationMiniCAM;
class PopulationSGMFixed;
class PopulationSGMRate;
class AgeCohort;
class Gender;
class Male;
class Female;
class GDP;
class IOutput;
class LandLeaf;
class UnmanagedLandLeaf;
class LandNode;
class ICarbonCalc;
class AgSector;
class IClimateModel;
class CarbonBox;
class CarbonBox;
class NPP;
class ACarbonFlow;
class CarbonBoxModel;
class LandUseHistory;

/*!
 * \brief An interface to a class which visits every node in the tree and
 *        optionally performs an operation on each.
 * \details Any class which implements the IVisitor interface may be passed to
 *          the accept method of any class that implements the Visitable
 *          interface. Once the Visitable class accepts the visitor, it will
 *          call startVisit for itself, call accept on all its children with the
 *          visitor, and then call endVisit with itself. The visitor may perform
 *          any desired work in the start and end visit methods, or it may
 *          choose to do nothing. The visitor will always perform a depth first
 *          traversal, which means it will visit all children of an item before
 *          visiting the next sibling of an item.
 *
 */
class IVisitor {
public:
    inline virtual ~IVisitor();
    virtual void finish() const = 0;
    virtual void startVisitScenario( const Scenario* aScenario, const int aPeriod ) = 0;
    virtual void endVisitScenario( const Scenario* aScenario, const int aPeriod ) = 0;

    virtual void startVisitWorld( const World* aWorld, const int aPeriod ) = 0;
    virtual void endVisitWorld( const World* aWorld, const int aPeriod ) = 0;

    virtual void startVisitRegion( const Region* aRegion, const int aPeriod ) = 0;
    virtual void endVisitRegion( const Region* aRegion, const int aPeriod ) = 0;

    virtual void startVisitRegionMiniCAM( const RegionMiniCAM* aRegionMiniCAM, const int aPeriod ) = 0;
    virtual void endVisitRegionMiniCAM( const RegionMiniCAM* aRegionMiniCAM, const int aPeriod ) = 0;

    virtual void startVisitRegionCGE( const RegionCGE* aRegionCGE, const int aPeriod ) = 0;
    virtual void endVisitRegionCGE( const RegionCGE* aRegionCGE, const int aPeriod ) = 0;
    
    virtual void startVisitDemographic( const Demographic* aDemographic, const int aPeriod ) = 0;
    virtual void endVisitDemographic( const Demographic* aDemographic, const int aPeriod ) = 0;

    virtual void startVisitPopulation( const Population* aPopulation, const int aPeriod ) = 0;
    virtual void endVisitPopulation( const Population* aPopulation, const int aPeriod ) = 0;

    virtual void startVisitPopulationMiniCAM( const PopulationMiniCAM* aPopulation, const int aPeriod ) = 0;
    virtual void endVisitPopulationMiniCAM( const PopulationMiniCAM* aPopulation, const int aPeriod ) = 0;

    virtual void startVisitPopulationSGMFixed( const PopulationSGMFixed* aPopulation, const int aPeriod ) = 0;
    virtual void endVisitPopulationSGMFixed( const PopulationSGMFixed* aPopulation, const int aPeriod ) = 0;

    virtual void startVisitPopulationSGMRate( const PopulationSGMRate* aPopulation, const int aPeriod ) = 0;
    virtual void endVisitPopulationSGMRate( const PopulationSGMRate* aPopulation, const int aPeriod ) = 0;

    virtual void startVisitAgeCohort( const AgeCohort* aAgeCohort, const int aPeriod ) = 0;
    virtual void endVisitAgeCohort( const AgeCohort* aAgeCohort, const int aPeriod ) = 0;
    
    virtual void startVisitGender( const Gender* aGender, const int aPeriod ) = 0;
    virtual void endVisitGender( const Gender* aGender, const int aPeriod ) = 0;

    virtual void startVisitFemale( const Female* aFemale, const int aPeriod ) = 0;
    virtual void endVisitFemale( const Female* aFemale, const int aPeriod ) = 0;

    virtual void startVisitMale( const Male* aMale, const int aPeriod ) = 0;
    virtual void endVisitMale( const Male* aMale, const int aPeriod ) = 0;

    virtual void startVisitResource( const AResource* aResource, const int aPeriod ) = 0;
    virtual void endVisitResource( const AResource* aResource, const int aPeriod ) = 0;
    
    virtual void startVisitSubResource( const SubResource* aSubResource, const int aPeriod ) = 0;
    virtual void endVisitSubResource( const SubResource* aSubResource, const int aPeriod ) = 0;

    virtual void startVisitGrade( const Grade* aGrade, const int aPeriod ) = 0;
    virtual void endVisitGrade( const Grade* aGrade, const int aPeriod ) = 0;

    virtual void startVisitSector( const Sector* aSector, const int aPeriod ) = 0;
    virtual void endVisitSector( const Sector* aSector, const int aPeriod ) = 0;
    
    virtual void startVisitDemandSector( const DemandSector* aDemandSector, const int aPeriod ) = 0;
    virtual void endVisitDemandSector( const DemandSector* aDemandSector, const int aPeriod ) = 0;

    virtual void startVisitProductionSector( const ProductionSector* aProdSector, const int aPeriod ) = 0;
    virtual void endVisitProductionSector( const ProductionSector* aProdSector, const int aPeriod ) = 0;
    
    virtual void startVisitSubsector( const Subsector* aSubsector, const int aPeriod ) = 0;
    virtual void endVisitSubsector( const Subsector* aSubsector, const int aPeriod ) = 0;
    
    virtual void startVisitBuildingDemandSubsector( const BuildingDemandSubSector* aSubsector,
                                                    const int aPeriod ) = 0;
    virtual void endVisitBuildingDemandSubsector( const BuildingDemandSubSector* aSubsector,
                                                  const int aPeriod ) = 0;

    virtual void startVisitTechnology( const Technology* aTechnology, const int aPeriod ) = 0;
    virtual void endVisitTechnology( const Technology* aTechnology, const int aPeriod ) = 0;

    virtual void startVisitBaseTechnology( const BaseTechnology* aBaseTechnology, const int aPeriod ) = 0;
    virtual void endVisitBaseTechnology( const BaseTechnology* aBaseTechnology, const int aPeriod ) = 0;

    virtual void startVisitConsumer( const Consumer* aConsumer, const int aPeriod ) = 0;
    virtual void endVisitConsumer( const Consumer* aConsumer, const int aPeriod ) = 0;

    virtual void startVisitHouseholdConsumer( const HouseholdConsumer* aHouseholdConsumer, 
        const int aPeriod ) = 0;
    virtual void endVisitHouseholdConsumer( const HouseholdConsumer* aHouseholdConsumer, 
        const int aPeriod ) = 0;

    virtual void startVisitGovtConsumer( const GovtConsumer* aGovtConsumer, const int aPeriod ) = 0;
    virtual void endVisitGovtConsumer( const GovtConsumer* aGovtConsumer, const int aPeriod ) = 0;

    virtual void startVisitInvestConsumer( const InvestConsumer* aInvestConsumer, const int aPeriod ) = 0;
    virtual void endVisitInvestConsumer( const InvestConsumer* aInvestConsumer, const int aPeriod ) = 0;

    virtual void startVisitTradeConsumer( const TradeConsumer* aTradeConsumer, const int aPeriod ) = 0;
    virtual void endVisitTradeConsumer( const TradeConsumer* aTradeConsumer, const int aPeriod ) = 0;

    virtual void startVisitProductionTechnology( const ProductionTechnology* aProductionTechnology, 
        const int aPeriod ) = 0;
    virtual void endVisitProductionTechnology( const ProductionTechnology* aProductionTechnology, 
        const int aPeriod ) = 0;

    virtual void startVisitFactorSupply( const FactorSupply* aFactorSupply, const int aPeriod ) = 0;
    virtual void endVisitFactorSupply( const FactorSupply* aFactorSupply, const int aPeriod ) = 0;

    virtual void startVisitNationalAccount( const NationalAccount* aNationalAccount, const int aPeriod ) = 0;
    virtual void endVisitNationalAccount( const NationalAccount* aNationalAccount, const int aPeriod ) = 0;

    virtual void startVisitInput( const Input* aInput, const int aPeriod ) = 0;
    virtual void endVisitInput( const Input* aInput, const int aPeriod ) = 0;

    virtual void startVisitProductionInput( const ProductionInput* aProductionInput, const int aPeriod ) = 0;
    virtual void endVisitProductionInput( const ProductionInput* aProductionInput, const int aPeriod ) = 0;

    virtual void startVisitDemandInput( const DemandInput* aDemandInput, const int aPeriod ) = 0;
    virtual void endVisitDemandInput( const DemandInput* aDemandInput, const int aPeriod ) = 0;

    virtual void startVisitExpenditure( const Expenditure* aExpenditure, const int aPeriod ) = 0;
    virtual void endVisitExpenditure( const Expenditure* aExpenditure, const int aPeriod ) = 0;
    
    virtual void startVisitOutput( const IOutput* aOutput, const int aPeriod ) = 0;
    virtual void endVisitOutput( const IOutput* aOutput, const int aPeriod ) = 0;

    virtual void startVisitGHG( const AGHG* aGHG, const int aPeriod ) = 0;
    virtual void endVisitGHG( const AGHG* aGHG, const int aPeriod ) = 0;
    
    virtual void startVisitOutputMetaData( const OutputMetaData* aOutputMetaData, const int aPeriod ) = 0;
    virtual void endVisitOutputMetaData( const OutputMetaData* aOutputMetaData, const int aPeriod ) = 0;
    
    virtual void startVisitMarketplace( const Marketplace* aMarketplace, const int aPeriod ) = 0;
    virtual void endVisitMarketplace( const Marketplace* aMarketplace, const int aPeriod ) = 0;
    
    virtual void startVisitMarket( const Market* aMarket, const int aPeriod ) = 0;
    virtual void endVisitMarket( const Market* aMarket, const int aPeriod ) = 0;

    virtual void startVisitGDP( const GDP* aGDP, const int aPeriod ) = 0;
    virtual void endVisitGDP( const GDP* aGDP, const int aPeriod ) = 0;
    
    virtual void startVisitLandNode( const LandNode* aLandNode, const int aPeriod ) = 0;
    virtual void endVisitLandNode( const LandNode* aLandNode, const int aPeriod ) = 0;

    virtual void startVisitLandLeaf( const LandLeaf* aLandLeaf, const int aPeriod ) = 0;
    virtual void endVisitLandLeaf( const LandLeaf* aLandLeaf, const int aPeriod ) = 0;
	
	virtual void startVisitLandUseHistory( const LandUseHistory* aLandUseHistory, const int aPeriod ) = 0;
	virtual void endVisitLandUseHistory( const LandUseHistory* aLandUseHistory, const int aPeriod ) = 0;

    virtual void startVisitCarbonCalc( const ICarbonCalc* aCarbonCalc, const int aPeriod ) = 0;
    virtual void endVisitCarbonCalc( const ICarbonCalc* aCarbonCalc, const int aPeriod ) = 0;

    virtual void startVisitCarbonBox( const CarbonBox* aCarbonBox, const int aPeriod ) = 0;
    virtual void endVisitCarbonBox( const CarbonBox* aCarbonBox, const int aPeriod ) = 0;

    virtual void startVisitNPP( const NPP* aNPP, const int aPeriod ) = 0;
    virtual void endVisitNPP( const NPP* aNPP, const int aPeriod ) = 0;

    virtual void startVisitCarbonFlow( const ACarbonFlow* aCarbonFlow, const int aPeriod ) = 0;
    virtual void endVisitCarbonFlow( const ACarbonFlow* aCarbonFlow, const int aPeriod ) = 0;

    virtual void startVisitAgSector( const AgSector* aAgSector, const int aPeriod ) = 0;
    virtual void endVisitAgSector( const AgSector* aAgSector, const int aPeriod ) = 0;

    virtual void startVisitClimateModel( const IClimateModel* aClimateModel, const int aPeriod ) = 0;
    virtual void endVisitClimateModel( const IClimateModel* aClimateModel, const int aPeriod ) = 0;
};

IVisitor::~IVisitor(){
}
#endif // _IVISITOR_H_
