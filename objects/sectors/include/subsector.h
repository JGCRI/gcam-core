#ifndef _SUBSECTOR_H_
#define _SUBSECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file subsector.h
* \ingroup Objects
* \brief The subsector class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <map>
#include <list>
#include <xercesc/dom/DOMNode.hpp>
#include "investment/include/iinvestable.h"

// Forward declarations
class Summary;
class technology;
class Emcoef_ind;
class Tabs;
class GDP;
class IInfo;
class DependencyFinder;
class BaseTechnology;
class NationalAccount;
class Demographic;
class MoreSectorInfo;
class OutputContainer;
class IExpectedProfitRateCalculator;
class TechnologyType;
class IDistributor;

/*! 
* \ingroup Objects
* \brief A class which defines a single Subsector of the model.
* \details The subsector contains a group of technology objects, which produce
*          or consume commodities in the marketplace. Each sub-sector has
*          attributes such as share, share weight, logit expoential, fixed
*          capacity, and capacity limits. 
* \author Sonny Kim, Steve Smith, Josh Lurz
*/

class Subsector: public IInvestable
{
    friend class SocialAccountingMatrix;
    friend class DemandComponentsTable;
    friend class SectorReport;
    friend class SGMGenTable;
private:
    static const std::string XML_NAME; //!< node name for toXML methods
    void clear();
protected:
    std::string name; //!< subsector name
    std::string regionName; //!< region name
    std::string sectorName; //!< sector name
    int scaleYear; //!< year to scale share weights to after calibration
    int techScaleYear; //!< year to scale technology share weights to after calibration
    double basesharewt; //! subsector base year consumption share weight
    std::auto_ptr<IInfo> mSubsectorInfo; //!< The subsector's information store.
    std::vector<std::vector<technology*> > techs; //!< vector of technology by period

    std::vector<double> capLimit; //!< subsector capacity limit
    std::vector<double> fixedShare; //!< share of this sub-sector that is fixed capacity -- set in sector
    std::vector<double> shrwts; //!< subsector logit share weights
    std::vector<double> lexp; //!< subsector logit exponential
    std::vector<double> share; //!< subsector shares
    std::vector<double> subsectorprice; //!< subsector price for all periods
    std::vector<double> fuelprice; //! subsector fuel price only for all periods
    std::vector<double> fuelPrefElasticity; //!< Fuel preference elasticity
    std::vector<double> calOutputValue; //!< Calibration value
    std::vector<double> mInvestments; //!< Investment by period.
    std::vector<double> mFixedInvestments; //!< Input fixed subsector level investment by period.
    std::vector<bool> capLimited; //!< true if subsector has hit its capacity limit
    std::vector<bool> doCalibration; // Flag set if calibration value is read-in
    std::vector<bool> calibrationStatus; // Set true if sector or any tech is calibrated
    std::vector<Summary> summary; //!< summary for reporting
    std::vector<BaseTechnology*> baseTechs; // for the time being
    std::map<std::string, TechnologyType*> mTechTypes; //!< Mapping from technology name to group of technology vintages.
    std::map<std::string,int> techNameMap; //!< Map of technology name to integer position in vector. 
    virtual void interpolateShareWeights( const int period ); // Consistantly adjust share weights
    std::map<std::string,int> baseTechNameMap; //!< Map of base technology name to integer position in vector. 
    typedef std::vector<BaseTechnology*>::const_iterator CBaseTechIterator;
    typedef std::vector<BaseTechnology*>::iterator BaseTechIterator;
    void shareWeightScale( const int pmer ); // Consistantly adjust share weights
    void shareWeightLinearInterpFn( const int beginPeriod,  const int endPeriod );
    bool techHasInput( const technology* thisTech, const std::string& goodName ) const;
    virtual void MCDerivedClassOutput() const;
    virtual void csvDerivedClassOutput() const;
    virtual bool XMLDerivedClassParse( const std::string nodeName, const xercesc::DOMNode* curr );
    virtual const std::string& getXMLName() const;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const {};
    virtual void toOutputXMLDerived( std::ostream& out, Tabs* tabs ) const {};
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const{};
    void normalizeTechShareWeights( const int period );
    virtual void adjustTechnologyShareWeights( const int period );
    void techShareWeightLinearInterpFn( const int beginPeriod,  const int endPeriod );
    void parseBaseTechHelper( const xercesc::DOMNode* curr, BaseTechnology* newTech );
    virtual bool isNameOfChild  ( const std::string& nodename ) const;
    virtual technology* createChild( const std::string& nodename ) const;
public:
    Subsector( const std::string regionName, const std::string sectorName );
    virtual ~Subsector();
    static double capLimitTransform( double capLimit, double orgShare ); 
    const std::string getName() const;
    void XMLParse( const xercesc::DOMNode* tempNode );
	virtual void completeInit( const IInfo* aSectorInfo, DependencyFinder* aDependencyFinder );
    
    virtual void initCalc( NationalAccount& aNationalAccount,
                           const Demographic* aDemographics,
                           const MoreSectorInfo* aMoreSectorInfo,
                           const int aPeriod );

    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toOutputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    static const std::string& getXMLNameStatic();
    virtual void calcPrice( const int period );
    double getPrice( const int period ) const;
    bool getCalibrationStatus( const int period ) const;
    virtual void setCalibrationStatus( const int period );
    void scaleCalibrationInput( const int period, const double scaleFactor );
    bool allOutputFixed( const int period ) const;
    double getFixedShare( const int period ) const;
    void setFixedShare( const int period, const double share );
    void setShareToFixedValue( const int period );
    double getfuelprice( const int period ) const; 
    double getwtfuelprice( const int period ) const;
    double getCapacityLimit( const int period ) const;
    virtual void calcShare( const int period, const GDP* gdp ); 
    void setShare( const double shareVal, const int period );
    void normShare( const double sum, const int period );
    double getShare( const int period ) const;
    virtual double getShareWeight( const int period ) const;
    virtual void scaleShareWeight( const double scaleValue, const int period );
    void limitShares( const double sum, const int period );
    void setCapLimitStatus( const bool value, const int period );
    bool getCapLimitStatus( const int period ) const;
    virtual void calcTechShares ( const GDP* gdp, const int period );
    virtual void setoutput( const double demand, const int period, const GDP* gdp ); 
    bool inputsAllFixed( const int period, const std::string& goodName ) const;
    void scaleFixedOutput( const double scaleRatio, const int period );
    double getFixedOutput( const int period ) const;
    void resetFixedOutput( const int period );
    double getTotalCalOutputs( const int period ) const;
    double getCalAndFixedInputs( const int period, const std::string& goodName, const bool bothVals ) const;
    double getCalAndFixedOutputs( const int period, const std::string& goodName, const bool bothVals ) const;
    bool setImpliedFixedInput( const int period, const std::string& goodName, const double requiredOutput );
    void csvOutputFile() const; 
    virtual void MCoutputSupplySector() const; 
    void MCoutputDemandSector() const; 
    void MCoutputAllSectors() const; 
    void emission( const int period );
    void indemission( const int period, const std::vector<Emcoef_ind>& emcoef_ind );
    double getInput( const int period ) const;
    double getOutput( const int period ) const;
    double getAnnualInvestment( const int aPeriod ) const;

    double distributeInvestment( const IDistributor* aDistributor,
                                 NationalAccount& aNationalAccount,
                                 const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                 const std::string& aRegionName,
                                 const std::string& aSectorName,
                                 const double aNewInvestment,
                                 const int aPeriod );

    double getTotalCarbonTaxPaid( const int period ) const;
    std::map<std::string, double> getfuelcons( const int period ) const; 
    std::map<std::string, double> getemission( const int period ) const;
    std::map<std::string, double> getemfuelmap( const int period ) const; 
    std::map<std::string, double> getemindmap( const int period ) const;
    void adjShares( const double demand, const double shareRatio, const double totalfixedOutput, const int period );
    void updateSummary( const int period );
    virtual void adjustForCalibration( double sectorDemand, double totalfixedOutput, double totalCalOutputs, const bool allFixedOutput, const int period );
    void scaleCalibratedValues( const int period, const std::string& goodName, const double scaleValue );
    int getNumberAvailTechs( const int period ) const;
    void tabulateFixedDemands( const int period );
    void adjustForCalibration( double sectorDemand, double totalFixedSupply, double totalCalOutputs, const int period );
    
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
    void finalizePeriod( const int aPeriod );
    void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
	void updateOutputContainer( OutputContainer* outputContainer, const int period ) const;
    double getFixedInvestment( const int aPeriod ) const;
};
#endif // _SUBSECTOR_H_
