#ifndef _XML_DB_OUTPUTTER_H_
#define _XML_DB_OUTPUTTER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*! 
* \file xml_db_outputter.h
* \ingroup Objects
* \brief XMLDBOutputter class header file.
* \author Josh Lurz
*/

#include <stack>
#include <memory>
#include <iosfwd>
#include <boost/iostreams/filtering_stream.hpp>
#include "util/base/include/default_visitor.h"

#if( __HAVE_JAVA__ )
#include <jni.h>
#include <boost/iostreams/concepts.hpp>
#endif

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
    void finalizeAndClose();

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
    
    void startVisitSubRenewableResource( const SubRenewableResource* aSubResource, const int aPeriod );
    void endVisitSubRenewableResource( const SubRenewableResource* aSubResource, const int aPeriod );

    void startVisitGrade( const Grade* aGrade, const int aPeriod );
    void endVisitGrade( const Grade* aGrade, const int aPeriod );

    void startVisitSector( const Sector* aSector, const int aPeriod );
    void endVisitSector( const Sector* aSector, const int aPeriod );

    void startVisitSubsector( const Subsector* aSubsector, const int aPeriod );
    void endVisitSubsector( const Subsector* aSubsector, const int aPeriod );

    void startVisitEnergyFinalDemand( const EnergyFinalDemand* aEnergyFinalDemand, const int aPeriod );
    void endVisitEnergyFinalDemand( const EnergyFinalDemand* aEnergyFinalDemand, const int aPeriod );

    void startVisitBaseTechnology( const BaseTechnology* aBaseTech, const int aPeriod );
    void endVisitBaseTechnology( const BaseTechnology* aBaseTech, const int aPeriod );

    void startVisitTechnology( const Technology* aTechnology, const int aPeriod );
    void endVisitTechnology( const Technology* aTechnology, const int aPeriod );

    virtual void startVisitTranSubsector( const TranSubsector* aTranSubsector, const int aPeriod );
    virtual void endVisitTranSubsector( const TranSubsector* aTranSubsector, const int aPeriod );
	
    virtual void startVisitTranTechnology( const TranTechnology* aTranTechnology, const int aPeriod );
    virtual void endVisitTranTechnology( const TranTechnology* aTranTechnology, const int aPeriod );

    virtual void startVisitMiniCAMInput( const MiniCAMInput* aInput, const int aPeriod );
    virtual void endVisitMiniCAMInput( const MiniCAMInput* aInput, const int aPeriod );

    virtual void startVisitInput( const IInput* aInput, const int aPeriod );
    virtual void endVisitInput( const IInput* aInput, const int aPeriod );

    virtual void startVisitOutput( const IOutput* aOutput, const int aPeriod );
    virtual void endVisitOutput( const IOutput* aOutput, const int aPeriod );

    virtual void startVisitAgProductionTechnology( const AgProductionTechnology* AgProductionTechnology, const int aPeriod );
    virtual void endVisitAgProductionTechnology( const AgProductionTechnology* AgProductionTechnology, const int aPeriod );

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

    void startVisitCarbonCalc( const ICarbonCalc* aCarbonCalc, const int aPeriod );
    void endVisitCarbonCalc( const ICarbonCalc* aCarbonCalc, const int aPeriod );

    void startVisitLandNode( const LandNode* aLandNode, const int aPeriod );
    void endVisitLandNode( const LandNode* aLandNode, const int aPeriod );

    void startVisitLandLeaf( const LandLeaf* aLandLeaf, const int aPeriod );
    void endVisitLandLeaf( const LandLeaf* aLandLeaf, const int aPeriod );

    void startVisitExpenditure( const Expenditure* aExpenditure, const int aPeriod );
    void endVisitExpenditure( const Expenditure* aExpenditure, const int aPeriod );

    virtual void startVisitSGMInput( const SGMInput* aInput, const int aPeriod );
    virtual void endVisitSGMInput( const SGMInput* aInput, const int aPeriod );

    virtual void startVisitNodeInput( const NodeInput* aNodeInput, const int aPeriod );
    virtual void endVisitNodeInput( const NodeInput* aNodeInput, const int aPeriod );

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

    virtual void startVisitNationalAccount( const NationalAccount* aNationalAccount, const int aPeriod );
    virtual void endVisitNationalAccount( const NationalAccount* aNationalAccount, const int aPeriod );


    virtual void startVisitGCAMConsumer( const GCAMConsumer* aGCAMConsumer, const int aPeriod );
    virtual void endVisitGCAMConsumer( const GCAMConsumer* aGCAMConsumer, const int aPeriod );

    virtual void startVisitBuildingNodeInput( const BuildingNodeInput* aBuildingNodeInput, const int aPeriod );
    virtual void endVisitBuildingNodeInput( const BuildingNodeInput* aBuildingNodeInput, const int aPeriod );

    virtual void startVisitBuildingServiceInput( const BuildingServiceInput* aBuildingServiceInput, const int aPeriod );
    virtual void endVisitBuildingServiceInput( const BuildingServiceInput* aBuildingServiceInput, const int aPeriod );

    bool appendData( const std::string& aData, const std::string& aLocation );
private:
    //! A boost iostream which will send output to the DB as it is printed.
    mutable boost::iostreams::filtering_ostream mBuffer;

    //! Current region name.
    std::string mCurrentRegion;

    //! Current sector name.
    std::string mCurrentSector;

    //! Current price unit.
    std::string mCurrentPriceUnit;

    //! Current output unit.
    std::string mCurrentOutputUnit;

    //! Current Input unit.
    std::string mCurrentInputUnit;

    //! Current market name.
    std::string mCurrentMarket;

    //! Current indirect emissions for the Technology. These are more easily
    //! calculated at the Technology but logically belong in the GHG output.
    objects::PeriodVector<double> mCurrIndirectEmissions;

    //! Tabs object.
    std::auto_ptr<Tabs> mTabs;

    //! Weak pointer to the current region's GDP object.
    const GDP* mGDP;

    //! Weak pointer to the current technology object.
    const Technology* mCurrentTechnology;
    
    //! A stack used to keep track of what needs to be written to the
    //! database.
    std::stack<std::iostream*> mBufferStack;

    //! Indirect emissions calculator for the current region.
    std::auto_ptr<IndirectEmissionsCalculator> mIndirectEmissCalc;

#if( __HAVE_JAVA__ )
    /*!
     * \brief Contains all objects necessary to interact with Java.
     * \details Interacting with Java through the Java Native Interface (JNI) can
     *          be burdensome and tricky.  We will keep all various Java methods /
     *          environment / class pointers to help manage and organize it all.
     */
    struct JNIContainer {
        //! The Java runtime.  Note this is made static because it is apparently
        //! not possible to close and re-open a JVM.
        static JavaVM* mJavaVM;

        //! The Java runtime environment.
        JNIEnv* mJavaEnv;

        //! A "global" reference to the Java class that will write the XML to the db.
        jclass mWriteDBClass;

        //! A "global" reference to the actual instance of the mWriteDBClass.
        jobject mWriteDBInstance;

        JNIContainer();
        ~JNIContainer();
    };

    //! An auto pointer to all the JNI data that needs to be maintained through out
    //! the like of the XMLDBOutputter.
    const std::auto_ptr<JNIContainer> mJNIContainer;

    static std::auto_ptr<JNIContainer> createContainer();
#endif
    static const std::string createContainerName( const std::string& aScenarioName );

    void writeItemToBuffer( const double aValue,
        const std::string& aName,
        std::ostream& out,
        const Tabs* tabs,
        const int aPeriod,
        const std::string& aUnit );

    void writeItem( const std::string& aName,
        const std::string& aUnit,
        const double aValue,
        const int aPeriod );

    void writeItemUsingYear( const std::string& aName,
        const std::string& aUnit,
        const double aValue,
        const int aYear );

    bool isTechnologyOperating( const int aPeriod );
    
    std::iostream* popBufferStack();

#if( __HAVE_JAVA__ )
    /*!
     * \brief A boost IO "sink" which will transfer XML as it is written to mBuffer
     *        to the Java class responsible for writing it to the database.
     * \details This class implements the boost IO sink device concept so that it
     *          can get direct access to the data as it is being generated and
     *          send it to the database via Java without having to use large amounts
     *          of memory to keep the XML document in memory at any point.  Using
     *          the boost::iostreams interface to accomplish this is much easier
     *          and less error prone than trying to do it in the std::iostream.
     * \note If an error is raised while trying to write the data to the DB the
     *       error flag in this class will be set.  Since there is no way to stop
     *       visiting once it has starting the best we can do is ignore all data
     *       once the error flag is set.
     */
    class SendToJavaIOSink : public boost::iostreams::sink {
    public:
        SendToJavaIOSink( const JNIContainer* aJNIContainer );
        virtual ~SendToJavaIOSink();
        
        // boost::iostreams::sink methods
        virtual std::streamsize write( const char* aData, std::streamsize aLength );
    private:
        //! A weak pointer to the JNIContainer to communicate with Java
        const JNIContainer* mJNIContainer;

        //! A JNI method ID to the Java method that will receive the data.
        jmethodID mReceiveDataMID;

        const std::streamsize BUFFER_SIZE;

        //! A JNI buffer that can be data can be put in to send to Java.
        jbyteArray mJNIBuffer;

        //! An error flag which may be set if there is an error writing the data
        //! on the Java side.
        bool mErrorFlag;
    };
#endif

};
#endif // _XML_DB_OUTPUTTER_H_
