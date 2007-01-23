#ifndef _XML_DB_OUTPUTTER_H_
#define _XML_DB_OUTPUTTER_H_
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
* \file xml_db_outputter.h
* \ingroup Objects
* \brief XMLDBOutputter class header file.
* \author Josh Lurz
*/

#if( __USE_XML_DB__ )
#include <iosfwd>
#include <sstream>
#include <memory>
#include "util/base/include/default_visitor.h"

#include "dbxml/DbXml.hpp"

class IndirectEmissionsCalculator;

/*! 
* \ingroup Objects
* \brief A visitor which writes model results to an XML database.
* \details
* \author Josh Lurz
*/

class XMLDBOutputter : public DefaultVisitor {
public:
    XMLDBOutputter();

    ~XMLDBOutputter();

    void finish() const;

	void startVisitScenario( const Scenario* aScenario, const int aPeriod );
	void endVisitScenario( const Scenario* aScenario, const int aPeriod );
	
	void startVisitOutputMetaData( const OutputMetaData* aOutputMetaData, const int aPeriod );
	void endVisitOutputMetaData( const OutputMetaData* aOutputMetaData, const int aPeriod );
	
	void startVisitWorld( const World* aWorld, const int aPeriod );
	void endVisitWorld( const World* aWorld, const int aPeriod );

	void startVisitRegion( const Region* aRegion, const int aPeriod );
	void endVisitRegion( const Region* aRegion, const int aPeriod );

    void startVisitRegionMiniCAM( const RegionMiniCAM* aRegionMiniCAM, const int aPeriod );
    void endVisitRegionMiniCAM( const RegionMiniCAM* aRegionMiniCAM, const int aPeriod );

    void startVisitRegionCGE( const RegionCGE* aRegionCGE, const int aPeriod );
    void endVisitRegionCGE( const RegionCGE* aRegionCGE, const int aPeriod );

    void startVisitResource( const AResource* aResource, const int aPeriod );
    void endVisitResource( const AResource* aResource, const int aPeriod );
    
	void startVisitSubResource( const SubResource* aSubResource, const int aPeriod );
	void endVisitSubResource( const SubResource* aSubResource, const int aPeriod );

    void startVisitGrade( const Grade* aGrade, const int aPeriod );
    void endVisitGrade( const Grade* aGrade, const int aPeriod );

    void startVisitSector( const Sector* aSector, const int aPeriod );
    void endVisitSector( const Sector* aSector, const int aPeriod );

    void startVisitSubsector( const Subsector* aSubsector, const int aPeriod );
    void endVisitSubsector( const Subsector* aSubsector, const int aPeriod );

    virtual void startVisitBuildingDemandSubsector( const BuildingDemandSubSector* aSubsector,
                                                    const int aPeriod );
    virtual void endVisitBuildingDemandSubsector( const BuildingDemandSubSector* aSubsector,
                                                  const int aPeriod );

	void startVisitTechnology( const Technology* aTechnology, const int aPeriod );
	void endVisitTechnology( const Technology* aTechnology, const int aPeriod );

    void startVisitGHG( const AGHG* aGHG, const int aPeriod );
    void endVisitGHG( const AGHG* aGHG, const int aPeriod );

    void startVisitMarketplace( const Marketplace* aMarketplace, const int aPeriod );
    void endVisitMarketplace( const Marketplace* aMarketplace, const int aPeriod );

    void startVisitMarket( const Market* aMarket, const int aPeriod );
    void endVisitMarket( const Market* aMarket, const int aPeriod );

    virtual void startVisitClimateModel( const IClimateModel* aClimateModel, const int aPeriod );
    virtual void endVisitClimateModel( const IClimateModel* aClimateModel, const int aPeriod );

    void startVisitDemographic( const Demographic* aDemographic, const int aPeriod );
    void endVisitDemographic( const Demographic* aDemographic, const int aPeriod );

    void startVisitPopulation( const Population* aPopulation, const int aPeriod );
    void endVisitPopulation( const Population* aPopulation, const int aPeriod );

    void startVisitPopulationMiniCAM( const PopulationMiniCAM* aPopulation, const int aPeriod );
    void endVisitPopulationMiniCAM( const PopulationMiniCAM* aPopulation, const int aPeriod );

    void startVisitPopulationSGMRate( const PopulationSGMRate* aPopulation, const int aPeriod );
    void endVisitPopulationSGMRate( const PopulationSGMRate* aPopulation, const int aPeriod );

    void startVisitPopulationSGMFixed( const PopulationSGMFixed* aPopulation, const int aPeriod );
    void endVisitPopulationSGMFixed( const PopulationSGMFixed* aPopulation, const int aPeriod );
    
    void startVisitAgeCohort( const AgeCohort* aAgeCohort, const int aPeriod );
    void endVisitAgeCohort( const AgeCohort* aAgeCohort, const int aPeriod );
    
    void startVisitGender( const Gender* aGender, const int aPeriod );
    void endVisitGender( const Gender* aGender, const int aPeriod );
    
    void startVisitGDP( const GDP* aGDP, const int aPeriod );
    void endVisitGDP( const GDP* aGDP, const int aPeriod );

    void startVisitLandNode( const LandNode* aLandNode, const int aPeriod );
    void endVisitLandNode( const LandNode* aLandNode, const int aPeriod );

    void startVisitLandLeaf( const LandLeaf* aLandLeaf, const int aPeriod );
    void endVisitLandLeaf( const LandLeaf* aLandLeaf, const int aPeriod );

    void startVisitCarbonCalc( const ICarbonCalc* aCarbon, const int aPeriod );
    void endVisitCarbonCalc( const ICarbonCalc* aCarbon, const int aPeriod );

    void startVisitBaseTechnology( const BaseTechnology* aBaseTech, const int aPeriod );
    void endVisitBaseTechnology( const BaseTechnology* aBaseTech, const int aPeriod );

    void startVisitExpenditure( const Expenditure* aExpenditure, const int aPeriod );
    void endVisitExpenditure( const Expenditure* aExpenditure, const int aPeriod );

    virtual void startVisitInput( const Input* aInput, const int aPeriod );
    virtual void endVisitInput( const Input* aInput, const int aPeriod );

    virtual void startVisitHouseholdConsumer( const HouseholdConsumer* aHouseholdConsumer, 
        const int aPeriod );
    virtual void endVisitHouseholdConsumer( const HouseholdConsumer* aHouseholdConsumer, 
        const int aPeriod );

    virtual void startVisitGovtConsumer( const GovtConsumer* aGovtConsumer, const int aPeriod );
    virtual void endVisitGovtConsumer( const GovtConsumer* aGovtConsumer, const int aPeriod );

    virtual void startVisitTradeConsumer( const TradeConsumer* aTradeConsumer, const int aPeriod );
    virtual void endVisitTradeConsumer( const TradeConsumer* aTradeConsumer, const int aPeriod );

    virtual void startVisitInvestConsumer( const InvestConsumer* aInvestConsumer, const int aPeriod );
    virtual void endVisitInvestConsumer( const InvestConsumer* aInvestConsumer, const int aPeriod );

    virtual void startVisitProductionTechnology( const ProductionTechnology* aProductionTechnology, 
        const int aPeriod );
    virtual void endVisitProductionTechnology( const ProductionTechnology* aProductionTechnology, 
        const int aPeriod );

    virtual void startVisitFactorSupply( const FactorSupply* aFactorySupply, const int aPeriod );
    virtual void endVisitFactorSupply( const FactorSupply* aFactorSupply, const int aPeriod );

    static bool appendData( const std::string& aData, const std::string& aLocation );
private:
    //! Stringstream containing all the information until it is printed.
    mutable std::stringstream mBuffer;
    
    //! Current region name.
    std::string mCurrentRegion;
    
    //! Current sector name.
    std::string mCurrentSector;

    //! Current price unit.
    std::string mCurrentPriceUnit;

    //! Current output unit.
    std::string mCurrentOutputUnit;

    //! Current market name.
    std::string mCurrentMarket;

    //! Current technology fuel stored from Technology so that GHG may access
    //! this. This is updated when the visitor reaches each Technology.
    std::string mCurrentFuel;
    
    //! Current indirect emissions for the Technology. These are more easily
    //! calculated at the Technology but logically belong in the GHG writeout.
    objects::PeriodVector<double> mCurrIndirectEmissions;

    //! Tabs object.
    std::auto_ptr<Tabs> mTabs;
   
    //! Weak pointer to the current region's GDP object.
    const GDP* mGDP;

    //! Indirect emissions calculator for the current region.
    std::auto_ptr<IndirectEmissionsCalculator> mIndirectEmissCalc;

    /*! \brief Contains all objects neccessary to operate on a container.
    * \details This struct defines the set of objects that must have the same
    *          lifetime so that the XML database outputter can operate on the
    *          container. The struct also ensures that the objects are deleted
    *          in the correct ordering to avoid accessing already deleted
    *          objects.
    * \note These objects must be in this order so destruction works correctly.
    */
    struct DBContainer {
        //! The database environment.
        DbEnv* mDBEnvironment;

        //! The database manager.
        std::auto_ptr<DbXml::XmlManager> mManager;
        
        //! Wrapper around the XML container so the memory can be dynamically
        //! allocated.
        struct XMLContainerWrapper {
            XMLContainerWrapper( DbXml::XmlContainer aContainer );
            //! The XML container.
            DbXml::XmlContainer mContainer;
        };
        
        //! The wrapper around the XML container.
        std::auto_ptr<XMLContainerWrapper> mContainerWrapper;
        DBContainer();
        ~DBContainer();
    };
    static std::auto_ptr<DBContainer> createContainer();
    static const std::string createContainerName( const std::string& aScenarioName );
    
    void writeItem( const std::string& aName,
                    const std::string& aUnit,
                    const double aValue,
                    const int aPeriod );
    
    void writeItemUsingYear( const std::string& aName,
                             const std::string& aUnit,
                             const double aValue,
                             const int aYear );
};
#endif //  __USE_XML_DB__
#endif // _XML_DB_OUTPUTTER_H_
