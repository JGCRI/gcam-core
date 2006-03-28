#ifndef _DEFAULT_VISITOR_H_
#define _DEFAULT_VISITOR_H_
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
* \file default_visitor.h
* \ingroup Objects
* \brief DefaultVisitor class header file.
* \author Pralit Patel, Katherine Chung, Josh Lurz
*/

#include "util/base/include/ivisitor.h"
#include <string>

/*! \brief DefaultVisitor is an implementation of IVisitor which defines all
*          methods as empty.
* \details This is a convenience class used so that derived classes do not have
*          to implement all the methods of the IVisitor interface. 
*/
class DefaultVisitor : public IVisitor {
public:
    virtual ~DefaultVisitor(){}
	virtual void finish() const {}
	virtual void startVisitScenario( const Scenario* aScenario, const int aPeriod ){}
	virtual void endVisitScenario( const Scenario* aScenario, const int aPeriod ){}
	virtual void startVisitWorld( const World* aWorld, const int aPeriod ){}
	virtual void endVisitWorld( const World* aWorld, const int aPeriod ){}
	virtual void startVisitRegion( const Region* aRegion, const int aPeriod ){}
	virtual void endVisitRegion( const Region* aRegion, const int aPeriod ){}
	virtual void startVisitRegionCGE( const RegionCGE* aRegionCGE, const int aPeriod ){}
	virtual void endVisitRegionCGE( const RegionCGE* aRegionCGE, const int aPeriod ){}
    
	virtual void startVisitDemographic( const Demographic* aDemographic, const int aPeriod ){}
	virtual void endVisitDemographic( const Demographic* aDemographic, const int aPeriod ){}

	virtual void startVisitPopulation( const Population* aPopulation, const int aPeriod ){}
	virtual void endVisitPopulation( const Population* aPopulation, const int aPeriod ){}

	virtual void startVisitPopulationMiniCAM( const PopulationMiniCAM* aPopulation, const int aPeriod ){}
	virtual void endVisitPopulationMiniCAM( const PopulationMiniCAM* aPopulation, const int aPeriod ){}

	virtual void startVisitPopulationSGMFixed( const PopulationSGMFixed* aPopulation, const int aPeriod ){}
	virtual void endVisitPopulationSGMFixed( const PopulationSGMFixed* aPopulation, const int aPeriod ){}

	virtual void startVisitPopulationSGMRate( const PopulationSGMRate* aPopulation, const int aPeriod ){}
	virtual void endVisitPopulationSGMRate( const PopulationSGMRate* aPopulation, const int aPeriod ){}

	virtual void startVisitAgeCohort( const AgeCohort* aAgeCohort, const int aPeriod ){}
	virtual void endVisitAgeCohort( const AgeCohort* aAgeCohort, const int aPeriod ){}
	
	virtual void startVisitGender( const Gender* aGender, const int aPeriod ){}
	virtual void endVisitGender( const Gender* aGender, const int aPeriod ){}

	virtual void startVisitFemale( const Female* aFemale, const int aPeriod ){}
	virtual void endVisitFemale( const Female* aFemale, const int aPeriod ){}

	virtual void startVisitMale( const Male* aMale, const int aPeriod ){}
	virtual void endVisitMale( const Male* aMale, const int aPeriod ){}

	virtual void startVisitResource( const Resource* aResource, const int aPeriod ){}
	virtual void endVisitResource( const Resource* aResource, const int aPeriod ){}
    
	virtual void startVisitSubResource( const SubResource* aSubResource, const int aPeriod ){}
	virtual void endVisitSubResource( const SubResource* aSubResource, const int aPeriod ){}

	virtual void startVisitGrade( const Grade* aGrade, const int aPeriod ){}
	virtual void endVisitGrade( const Grade* aGrade, const int aPeriod ){}

	virtual void startVisitSector( const Sector* aSector, const int aPeriod ){}
	virtual void endVisitSector( const Sector* aSector, const int aPeriod ){}
	virtual void startVisitDemandSector( const DemandSector* aDemandSector, const int aPeriod ){}
	virtual void endVisitDemandSector( const DemandSector* aDemandSector, const int aPeriod ){}
    virtual void updateProductionSector( const ProductionSector* aProdSector, const int aPeriod ){}
    virtual void startVisitSubsector( const Subsector* aSubsector, const int aPeriod ){}
    virtual void endVisitSubsector( const Subsector* aSubsector, const int aPeriod ){}

    virtual void startVisitBuildingDemandSubsector( const BuildingDemandSubSector* aSubsector,
                                                    const int aPeriod ){}
    virtual void endVisitBuildingDemandSubsector( const BuildingDemandSubSector* aSubsector,
                                                  const int aPeriod ){}

    virtual void updateBaseTechnology( const BaseTechnology* aBaseTechnology ){}
    virtual void updateConsumer( const Consumer* aConsumer, const int aPeriod ){}
	virtual void updateHouseholdConsumer( const HouseholdConsumer* aHouseholdConsumer,
        const int aPeriod ){}
    virtual void updateGovtConsumer( const GovtConsumer* aGovtConsumer, const int aPeriod ){}
    virtual void updateInvestConsumer( const InvestConsumer* aInvestConsumer, const int aPeriod ){}
    virtual void updateTradeConsumer( const TradeConsumer* aTradeConsumer, const int aPeriod ){}
	virtual void updateProductionTechnology( const ProductionTechnology* aProductionTechnology, const int aPeriod ){}
	virtual void startVisitTechnology( const technology* aTechnology, const int aPeriod ){}
	virtual void endVisitTechnology( const technology* aTechnology, const int aPeriod ){}
    virtual void updateFactorSupply( const FactorSupply* aFactorSupply, const int aPeriod ){}
    virtual void updateNationalAccount( const NationalAccount* aNationalAccount, const int aPeriod ){}
    virtual void updateInput( const Input* aInput ){}
    virtual void updateProductionInput( const ProductionInput* aProdInput ){}
    virtual void updateDemandInput( const DemandInput* aDemandInput ){}
	virtual void startVisitGHG( const Ghg* aGHG, const int aPeriod ){}
	virtual void endVisitGHG( const Ghg* aGHG, const int aPeriod ){}
	virtual void startVisitOutputMetaData( const OutputMetaData* aOutputMetaData, const int aPeriod ){}
	virtual void endVisitOutputMetaData( const OutputMetaData* aOutputMetaData, const int aPeriod ){}
	virtual void startVisitMarketplace( const Marketplace* aMarketplace, const int aPeriod ){}
	virtual void endVisitMarketplace( const Marketplace* aMarketplace, const int aPeriod ){}
	virtual void startVisitMarket( const Market* aMarket, const int aPeriod ){}
	virtual void endVisitMarket( const Market* aMarket, const int aPeriod ){}

	virtual void startVisitMagiccModel( const MagiccModel* aMagiccModel, const int aPeriod ){}
	virtual void endVisitMagiccModel( const MagiccModel* aMagiccModel, const int aPeriod ){}

    virtual void startVisitGDP( const GDP* aGDP, const int aPeriod ){}
    virtual void endVisitGDP( const GDP* aGDP, const int aPeriod ){}

    virtual void startVisitLandNode( const LandNode* aLandNode, const int aPeriod ){}
    virtual void endVisitLandNode( const LandNode* aLandNode, const int aPeriod ){}

	virtual void startVisitLandLeaf( const LandLeaf* aLandLeaf, const int aPeriod ){}
    virtual void endVisitLandLeaf( const LandLeaf* aLandLeaf, const int aPeriod ){}

    virtual void startVisitCarbonCalc( const ICarbonCalc* aCarbonCalc, const int aPeriod ){}
    virtual void endVisitCarbonCalc( const ICarbonCalc* aCarbonCalc, const int aPeriod ){}

    virtual void startVisitAgSector( const AgSector* aAgSector, const int aPeriod ){}
    virtual void endVisitAgSector( const AgSector* aAgSector, const int aPeriod ){}
};

#endif // _DEFAULT_VISITOR_H_
