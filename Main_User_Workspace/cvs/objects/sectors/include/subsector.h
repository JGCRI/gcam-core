#ifndef _SUBSECTOR_H_
#define _SUBSECTOR_H_
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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */


/*! 
* \file subsector.h
* \ingroup Objects
* \brief The Subsector class header file.
* \author Sonny Kim
*/

#include <string>
#include <vector>
#include <map>
#include <list>
#include <xercesc/dom/DOMNode.hpp>
#include "investment/include/iinvestable.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/iround_trippable.h"

// Forward declarations
class Summary;
class ITechnology;
class GDP;
class IInfo;
class DependencyFinder;
class BaseTechnology;
class NationalAccount;
class Demographic;
class MoreSectorInfo;
class IExpectedProfitRateCalculator;
class TechnologyType;
class IDistributor;
class Tabs;
class ILandAllocator;
class Demographics;
class IndirectEmissionsCalculator;
class GlobalTechnologyDatabase;

/*! 
* \ingroup Objects
* \brief A class which defines a single Subsector of the model.
* \details The subsector contains a group of technology objects, which produce
*          and consume commodities in the marketplace. Each Subsector has
*          attributes such as share, share weight and a logit exponential.
* \author Sonny Kim, Steve Smith, Josh Lurz
*/

class Subsector: public IInvestable,
                 public IVisitable,
                 public IRoundTrippable
{
    friend class SocialAccountingMatrix;
    friend class DemandComponentsTable;
    friend class SectorReport;
    friend class SGMGenTable;
    friend class XMLDBOutputter;
private:
    static const std::string XML_NAME; //!< node name for toXML methods
    void clear();
protected:
    std::string name; //!< subsector name
    std::string regionName; //!< region name
    std::string sectorName; //!< sector name
    int scaleYear; //!< year to scale share weights to after calibration
    int techScaleYear; //!< year to scale technology share weights to after calibration
    std::auto_ptr<IInfo> mSubsectorInfo; //!< The subsector's information store.

    std::vector<std::vector<ITechnology*> > techs; //!< vector of technology by period

    std::vector<double> shrwts; //!< subsector logit share weights
    std::vector<double> lexp; //!< subsector logit exponential
    std::vector<double> fuelPrefElasticity; //!< Fuel preference elasticity

    std::vector<double> mInvestments; //!< Investment by period.
    std::vector<double> mFixedInvestments; //!< Input fixed subsector level investment by period.
    std::vector<Summary> summary; //!< summary for reporting
    std::vector<BaseTechnology*> baseTechs; // for the time being
    std::map<std::string, TechnologyType*> mTechTypes; //!< Mapping from technology name to group of technology vintages.

    virtual void interpolateShareWeights( const int period ); // Consistantly adjust share weights
    std::map<std::string,int> baseTechNameMap; //!< Map of base technology name to integer position in vector. 
    typedef std::vector<BaseTechnology*>::const_iterator CBaseTechIterator;
    typedef std::vector<BaseTechnology*>::iterator BaseTechIterator;
    void shareWeightLinearInterpFn( const int beginPeriod,  const int endPeriod );

    virtual bool getCalibrationStatus( const int aPeriod ) const;

    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual const std::string& getXMLName() const;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const {};
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {};
    void normalizeTechShareWeights( const int period );
    virtual void adjustTechnologyShareWeights( const int period );
    void techShareWeightLinearInterpFn( const int beginPeriod,  const int endPeriod );
    void parseBaseTechHelper( const xercesc::DOMNode* curr, BaseTechnology* aNewTech );
    virtual bool isNameOfChild  ( const std::string& nodename ) const;
    
    virtual ITechnology* createChild( const std::string& aTechType,
                                     const std::string& aTechName,
                                     const int aTechYear ) const;
    
    virtual const std::vector<double> calcTechShares ( const GDP* gdp, const int period ) const;
   
    static bool initializeTechVector( std::vector<ITechnology*>& aTechVector,
                                      const std::string& aRegionName,
                                      const std::string& aSectorName,
                                      const std::string& aSubsectorName,
                                      DependencyFinder* aDependencyFinder,
                                      const IInfo* aSubsecInfo,
                                      ILandAllocator* aLandAllocator,
                                      const GlobalTechnologyDatabase* aGlobalTechDB );

    static const std::string findTechName( const std::vector<ITechnology*>& aTechVector );

public:
    Subsector( const std::string& regionName, const std::string& sectorName );
    virtual ~Subsector();
    const std::string& getName() const;

    void XMLParse( const xercesc::DOMNode* aNode );

    virtual void completeInit( const IInfo* aSectorInfo,
                               DependencyFinder* aDependencyFinder,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB );
    
    virtual void initCalc( NationalAccount* aNationalAccount,
                           const Demographic* aDemographics,
                           const MoreSectorInfo* aMoreSectorInfo,
                           const int aPeriod );


    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    static const std::string& getXMLNameStatic();
    virtual double getPrice( const GDP* aGDP, const int aPeriod ) const;
    bool allOutputFixed( const int period ) const;
    virtual double getAverageFuelPrice( const GDP* aGDP, const int aPeriod ) const;

    virtual void calcCost( const int aPeriod );

    virtual double calcShare( const int period, const GDP* gdp ) const; 
    virtual double getShareWeight( const int period ) const;
    virtual void scaleShareWeight( const double scaleValue, const int period );

    virtual void setOutput( const double aVariableDemand,
                            const double aFixedOutputScaleFactor,
                            const GDP* aGDP,
                            const int aPeriod );

    bool isAllCalibrated( const int aPeriod, double aCalAccuracy, const bool aPrintWarnings ) const;
    double getFixedOutput( const int period ) const;

    virtual double getTotalCalOutputs( const int period ) const;

    double getCalAndFixedOutputs( const int period, const std::string& goodName ) const;
    bool inputsAllFixed( const int period, const std::string& goodName ) const;

    bool setImpliedFixedInput( const int period, const std::string& goodName, const double requiredOutput );
    void csvOutputFile( const GDP* aGDP,
                        const IndirectEmissionsCalculator* aIndirectEmissCalc ) const; 
    virtual void MCoutputSupplySector( const GDP* aGDP ) const; 
    void MCoutputDemandSector( const GDP* aGDP ) const; 
    virtual void MCoutputAllSectors( const GDP* aGDP, 
                                     const IndirectEmissionsCalculator* aIndirectEmissCalc,
                                     const std::vector<double> aSectorOutput ) const; 

    void emission( const int period );

    double getInput( const int period ) const;
    virtual double getOutput( const int period ) const;

    virtual double getEnergyInput( const int aPeriod ) const;

    double getAnnualInvestment( const int aPeriod ) const;

    double distributeInvestment( const IDistributor* aDistributor,
                                 NationalAccount& aNationalAccount,
                                 const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                 const std::string& aRegionName,
                                 const std::string& aSectorName,
                                 const double aNewInvestment,
                                 const int aPeriod );

    std::map<std::string, double> getfuelcons( const int period ) const; 
    std::map<std::string, double> getemission( const int period ) const;
    std::map<std::string, double> getemfuelmap( const int period ) const; 

    void updateSummary( const std::list<std::string>& aPrimaryFuelList, const int period );
    
    void adjustForCalibration( double aSubsectorVariableDemand,
                               const bool aIsOnlySubsector,
                               const GDP* aGDP,
                               const int aPeriod );
    
    void scaleCalibratedValues( const int period, const std::string& goodName, const double scaleValue );
    virtual void tabulateFixedDemands( const int period, const IInfo* aSectorInfo );
    
    double getExpectedProfitRate( const NationalAccount& aNationalAccount,
                                  const std::string& aRegionName,
                                  const std::string& aSectorName,
                                  const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                  const double aInvestmentLogitExp,
                                  const bool aIsShareCalc,
                                  const int aPeriod ) const;
    
    double getCapitalOutputRatio( const IDistributor* aDistributor,
                                  const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                  const NationalAccount& aNationalAccount,
                                  const std::string& aRegionName,
                                  const std::string& aSectorName, 
                                  const int aPeriod ) const;

    void operate( NationalAccount& aNationalAccount, const Demographic* aDemographic,
                  const MoreSectorInfo* aMoreSectorInfo, const bool isNewVintageMode, const int aPeriod );
    
    void updateMarketplace( const int period );
    void postCalc( const int aPeriod );
    void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    double getFixedInvestment( const int aPeriod ) const;
};
#endif // _SUBSECTOR_H_
