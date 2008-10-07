#ifndef _BASE_TECHNOLOGY_H_
#define _BASE_TECHNOLOGY_H_
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
* \file base_technology.h
* \ingroup Objects
* \brief BaseTechnology class header file.
* \author Pralit Patel
* \author Sonny Kim
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

class IInput;
class IOutput;
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
class AGHG;
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
    friend class XMLDBOutputter;
public:
    BaseTechnology();
    BaseTechnology( const BaseTechnology& baseTechIn );
    BaseTechnology& operator= (const BaseTechnology& baseTechIn );
    virtual BaseTechnology* clone() const = 0;
    virtual ~BaseTechnology();

    virtual void copyParam( const BaseTechnology* baseTechIn,
                            const int aPeriod ) = 0;

    virtual void copyParamsInto( GovtConsumer& govtConsumerIn,
                                 const int aPeriod ) const { assert( false ); }

    virtual void copyParamsInto( TradeConsumer& tradeConsumerIn,
                                 const int aPeriod ) const { assert( false ); }

    virtual void copyParamsInto( InvestConsumer& investConsumerIn,
                                 const int aPeriod ) const { assert( false ); }

    virtual void copyParamsInto( ProductionTechnology& prodTechIn,
                                 const int aPeriod ) const { assert( false ); }

    virtual void copyParamsInto( HouseholdConsumer& householdConsumerIn,
                                 const int aPeriod ) const { assert( false ); }

    virtual void copyParamsInto( Consumer& consumerIn,
                                 const int aPeriod ) const { assert( false ); }

    void XMLParse( const xercesc::DOMNode* node );

    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const std::string& aSubsectorName ) = 0;
    
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
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const = 0;

    const std::string getIdentifier() const;
    static const std::string createIdentifier( const std::string& aName, int aYear );
    void removeEmptyInputs();
    virtual void setTypeHelper( TechnologyType* aTechType ) = 0;
    virtual void postCalc( const std::string& aRegionName, const std::string& aSectorName,
                           const int aPeriod ) = 0;
protected:
    virtual bool isCoefBased() const = 0;
    
    virtual bool isTrade() const {
        return false;
    };

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

    std::vector<IInput*> input; //!< Inputs
    std::vector<AGHG*> mGhgs; //!< Green-House gases.
    
    std::vector<IOutput*> mOutputs; //!< Outputs
    const IFunction* prodDmdFn; //!< Pointer to function this class will use
    std::vector<Expenditure> expenditures; //!< Keep track of expenditures
private:
    void clear();
    void copy( const BaseTechnology& baseTechIn );
};

#endif // _BASE_TECHNOLOGY_H_

