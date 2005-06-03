#ifndef _PRODUCTION_TECHNOLOGY_H_
#define _PRODUCTION_TECHNOLOGY_H_
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
* \file production_technology.h
* \ingroup Objects
* \brief ProductionTechnology class header file.
* \author Pralit Patel
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include <memory>

#include "technologies/include/base_technology.h"
#include "functions/include/ifunction.h" // could remove if mTechChange was a pointer, but need a copyctor.
class Tabs;
class Demographic;
class MoreSectorInfo;
class OutputContainer;
class IExpectedProfitRateCalculator;
class IShutdownDecider;
/*! 
* \ingroup Objects
* \brief CHANGE
* \details CHANGE
*
* \note CHANGE
* \author Pralit Patel, Sonny Kim
*/

class ProductionTechnology : public BaseTechnology
{
    friend class SocialAccountingMatrix;
    friend class DemandComponentsTable;
    friend class SectorReport;
    friend class SGMGenTable;
    friend class SectorResults;
    friend class GovtResults;
public:
	ProductionTechnology();
	ProductionTechnology* clone() const;
	~ProductionTechnology();
    void copyParam( const BaseTechnology* baseTech );
	void copyParamsInto( ProductionTechnology& prodTechIn ) const;

	virtual void completeInit( const std::string& regionName );
    
    virtual void initCalc( const MoreSectorInfo* aMoreSectorInfo, const std::string& aRegionName, 
        const std::string& aSectorName, NationalAccount& nationalAccount, 
        Demographic* aDemographics, const double aCapitalStock, const int aPeriod );
	
	static const std::string& getXMLNameStatic();

    virtual void operate( NationalAccount& aNationalAccount, const Demographic* aDemographic, 
        const MoreSectorInfo* aMoreSectorInfo, const std::string& aRegionName, const std::string& aSectorName, 
        const bool aIsNewVintageMode, const int aPeriod );

    virtual double setInvestment( const std::string& aRegionName, const double aAnnualInvestment,
                                  const double aTotalInvestment, const int aPeriod );

    virtual double getCapital() const;
    virtual double getAnnualInvestment( const int aPeriod ) const;
    double getExpectedProfitRate( const NationalAccount& aNationalAccount,
                                  const std::string& aRegionName,
                                  const std::string& aSectorName,
                                  const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                  const double aInvestmentLogitExp,
                                  const bool aIsShareCalc,
                                  const int aPeriod ) const;
    virtual double getCapitalOutputRatio( const std::string& aRegionName, const std::string& aSectorName,
                                          const int aPeriod ) const;
    double getFixedInvestment( const int aPeriod ) const;
    double distributeInvestment( const IDistributor* aDistributor, NationalAccount& aNationalAccount,
                                 const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                 const std::string& aRegionName, const std::string& aSectorName,
                                 const double aNewInvestment, const int aPeriod );
    void updateMarketplace( const std::string& sectorName, const std::string& regionName, 
        const int period );
    void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
    void updateOutputContainer( OutputContainer* outputContainer,
		const std::string& aRegionName, const std::string& aSectorName, const int aPeriod ) const;
    void setTypeHelper( TechnologyType* aTechType );
    void finalizePeriod( const std::string& aRegionName, const std::string& aSectorName, const int aPeriod );
protected:
    virtual bool isCoefBased() const { return true; }
    virtual void calcEmissions( const std::string& aGoodName, const std::string& aRegionName, const int aPeriod );
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string &nodeName, const xercesc::DOMNode* curr );
	virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
	virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
private:
     //! Lower limit of sigma for CES.
    const static double LEONTIEF_THRESHOLD;

    //! Costs stored by period for reporting.
    std::vector<double> mCostsReporting;

    //! Total profits stored by period.
    std::vector<double> mProfits;
    
    //! Structure containing types of technological change.
    TechChange mTechChange;
    
    //! Technology type containing this vintage. This may change.
    TechnologyType* mParentTechType;
    
    //! Expected profit rate stored for reporting.
    double mExpectedProfitRateReporting;
    
    
    /*! \brief An enum which describes the types of cached function return
    *          values for a specific year. 
    */
    enum CacheValues {
        //! The return value of the isAvailable function.
        AVAILABLE, 
        //! The return value of the isRetired function.
        RETIRED,
        //! The return value of the isNewInvestment function.
        NEW_INVESTMENT,
        //! A market for the last enum value, insert new enum values prior to this.
        END
    };
    //! A vector of cached results indexed by year for frequently called checks.
    std::vector<bool> mCachedValues;
    
	double capital; //!< capital cost
	double alphaZeroScaler; //!< scaler for production funtion
	double indBusTax; //!< indirect business tax
	double sigma1; //!< elasticity of substitution for new vintage
	double sigma2; //!< elasticity of substitution for old vintage
    double currSigma; //!< The sigma currently being used.
    double mBasePhysicalOutput; //!< base year physical output for calculating conversion factor
    double mConversionFactor; //!< conversion factor to get physical output from currency output
    double mAnnualInvestment; //!< Annual investment
    double mFixedInvestment; //!< Quantity of fixed investment in the initial year for the technology.
    
    // All times must be in years because some periods are < 0.
    
    //! The period for which all cached values stored in mCachedValues are valid for.
    int mValidCachePeriod;

    int lifeTime; //!< nameplate lifetime of the technology
	int	delayedInvestTime; //!< number of years between initial investment and first operation
	int	maxLifeTime; //!< maximum allowable lifetime
	int	retrofitLifeTime; //!< lifetime of the technology renovation
	int	periodIniInvest; //!< number of periods until initial investment
	int	periodInvestUnallowed; //!< period in which investment is no longer allowed

    
    void calcTaxes( NationalAccount& aNationalAccount, const MoreSectorInfo* aMoreSectorInfo, 
        const std::string& aRegionName, const std::string aSectorName, const int aPeriod );
    
    inline bool isNewInvestment( const int period ) const;
    bool calcIsNewInvestment( const int aPeriod ) const;
    
    inline bool isAvailable( const int aPeriod ) const;
    bool calcIsAvailable( const int aPeriod ) const;
    
    inline bool isRetired( const int aPeriod ) const;
    bool calcIsRetired( const int aPeriod ) const;

    double calcShutdownCoef( const std::string& aRegionName,
                             const std::string& aSectorName,
                             const int aPeriod ) const;
};

/*! \brief Return whether a technology is new investment for the current period.
* \param aPeriod The current period.
* \return Whether the technology is new investment in the period.
* \author Josh Lurz
*/
bool ProductionTechnology::isNewInvestment( const int aPeriod ) const {
    // Check if we have a valid cached result for the period.
    if( aPeriod == mValidCachePeriod ){
        return mCachedValues[ NEW_INVESTMENT ];
    }
    // There is no cached result available for this period, so calculate it
    // dynamically.
    return calcIsNewInvestment( aPeriod );
}

/*! \brief Return whether a technology is available to go online.
* \param aPeriod The current period.
* \return Whether the technology has gone online.
* \author Josh Lurz
*/
bool ProductionTechnology::isAvailable( const int aPeriod ) const {
    // Check if we have a valid cached result for the period.
    if( aPeriod == mValidCachePeriod ){
        return mCachedValues[ AVAILABLE ];
    }
    // There is no cached result available for this period, so calculate it
    // dynamically.
    return calcIsAvailable( aPeriod );
}

/*! \brief Return whether a technology has been retired yet.
* \param aPeriod The current period.
* \return Whether the technology has been retired.
* \author Josh Lurz
*/
bool ProductionTechnology::isRetired( const int aPeriod ) const {
    // Check if we have a valid cached result for the period.
    if( aPeriod == mValidCachePeriod ){
        return mCachedValues[ RETIRED ];
    }
    // There is no cached result available for this period, so calculate it
    // dynamically.
    return calcIsRetired( aPeriod );
}

#endif // _PRODUCTION_TECHNOLOGY_H_
