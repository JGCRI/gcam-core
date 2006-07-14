#ifndef _BASE_TECHNOLOGY_H_
#define _BASE_TECHNOLOGY_H_
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
* \file base_technology.h
* \ingroup Objects
* \brief Base Technology class header file.
* \author Pralit Patel
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include <cassert>
#include <map>

#include "technologies/include/expenditure.h"
#include "investment/include/iinvestable.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/iround_trippable.h"

class Input;
class IFunction;
class MoreSectorInfo;
class Demographic;
class NationalAccount;
class Consumer;
class HouseholdConsumer;
class GovtConsumer;
class TradeConsumer;
class InvestConsumer;
class ProductionTechnology;
class InvestConsumer;
class IVisitor;
class Ghg;
class IExpectedProfitRateCalculator;
class TechnologyType;

/*! 
* \ingroup Objects
* \brief The common base class of ProductionTechnology and Consumer.
* \details TODO
* \author Pralit Patel, Sonny Kim
*/

class BaseTechnology: public IInvestable, public IVisitable, IRoundTrippable
{
public:
    BaseTechnology();
    BaseTechnology( const BaseTechnology& baseTechIn );
    BaseTechnology& operator= (const BaseTechnology& baseTechIn );
    virtual BaseTechnology* clone() const = 0;
    virtual ~BaseTechnology();
    virtual void copyParam( const BaseTechnology* baseTechIn ) = 0;
    virtual void copyParamsInto( GovtConsumer& govtConsumerIn ) const { assert( false ); }
    virtual void copyParamsInto( TradeConsumer& tradeConsumerIn ) const { assert( false ); }
    virtual void copyParamsInto( InvestConsumer& investConsumerIn ) const { assert( false ); }
    virtual void copyParamsInto( ProductionTechnology& prodTechIn) const { assert( false ); }
    virtual void copyParamsInto( HouseholdConsumer& householdConsumerIn ) const { assert( false ); }
    virtual void copyParamsInto( Consumer& consumerIn ) const { assert( false ); }
    void XMLParse( const xercesc::DOMNode* node );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    
    virtual void completeInit( const std::string& aRegionName ) = 0;
    
    virtual void initCalc( const MoreSectorInfo* aMoreSectorInfo,
                           const std::string& aRegionName, 
                           const std::string& aSectorName,
                           NationalAccount& nationalAccount, 
                           const Demographic* aDemographics,
                           const double aCapitalStock,
                           const int aPeriod ) = 0;

    const std::string& getName() const;
    int getYear() const;
    void setYear( int newYear );

    virtual void operate( NationalAccount& aNationalAccount, const Demographic* aDemographics, 
        const MoreSectorInfo* aMoreSectorInfo, const std::string& aRegionName, 
        const std::string& aSectorName, const bool isNewVintageMode, const int aPeriod ) = 0;

    virtual double setInvestment( const std::string& aRegionName, const double aAnnualInvestment,
                                 const double aTotalInvestment, const int aPeriod ) = 0;

    double getOutput( const int aPeriod ) const;
    virtual double getCapital() const = 0;
    void calcPricePaid( const MoreSectorInfo* aMoreSectorInfo, const std::string& aRegionName,
        const std::string& aSectorName, const int aPeriod );

    virtual double getAnnualInvestment( const int aPeriod ) const = 0;
    
    virtual double getExpectedProfitRate( const NationalAccount& aNationalAccount,
                                          const std::string& aRegionName,
                                          const std::string& aSectorName,
                                          const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                          const double aInvestmentLogitExp,
                                          const bool aIsShareCalc,
                                          const int aPeriod ) const = 0;
    
    virtual double getCapitalOutputRatio( const IDistributor* aDistributor,
                                          const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                          const NationalAccount& aNationalAccount,
                                          const std::string& aRegionName,
                                          const std::string& aSectorName, 
                                          const int aPeriod ) const = 0;

    virtual double getFixedInvestment( const int aPeriod ) const = 0;
    virtual double distributeInvestment( const IDistributor* aDistributor,
                                         NationalAccount& aNationalAccount,
                                         const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                         const std::string& aRegionName,
                                         const std::string& aSectorName,
                                         const double aNewInvestment,
                                         const int aPeriod ) = 0;
    virtual void updateMarketplace( const std::string& sectorName, const std::string& regionName,
                                    const int period ) = 0;
	virtual void csvSGMOutputFile( std::ostream& aFile, const int period ) const = 0;
	virtual void accept( IVisitor* aVisitor, const int period ) const = 0;
    const std::string getIdentifier() const;
    static const std::string createIdentifier( const std::string& aName, int aYear );
    void removeEmptyInputs();
    virtual void setTypeHelper( TechnologyType* aTechType ) = 0;
    virtual void finalizePeriod( const std::string& aRegionName, const std::string& aSectorName,
                                 const int aPeriod ) = 0;
protected:
    virtual bool isCoefBased() const = 0;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ) = 0;
    virtual const std::string& getXMLName() const = 0;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const = 0;
    void initProdDmdFn();

    std::string name; //!< Name
    std::string categoryName; //!< Category name, used for reporting
    std::string prodDmdFnType; //<! Type of function used
    int year; //!< Year the technology was created in.
    std::map<std::string,int> inputNameToNo; //!< Mapping of input name to number.
    std::map<std::string,int> mGhgNameMap; //!< Mapping of ghg name to number.
    std::vector<Input*> input; //!< Inputs
    typedef std::vector<Input*>::iterator InputIterator;
    typedef std::vector<Input*>::const_iterator CInputIterator;
    std::vector<Ghg*> mGhgs; //!< Green-House gases.
    typedef std::vector<Ghg*>::iterator GHGIterator;
    typedef std::vector<Ghg*>::const_iterator CGHGIterator;
    
    std::vector<double> mOutputs; //!< Outputs
    const IFunction* prodDmdFn; //!< Pointer to function this class will use
    Expenditure expenditure; //!< Keep track of expenditures
private:
    void clear();
    void copy( const BaseTechnology& baseTechIn );
};

#endif // _BASE_TECHNOLOGY_H_

