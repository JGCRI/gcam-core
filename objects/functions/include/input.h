#ifndef _INPUT_H_
#define _INPUT_H_
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
* \file input.h
* \ingroup Objects
* \brief Input class header file.
* \author Pralit Patel, Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/value.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/iround_trippable.h"

class IVisitor;
class DemandInput;
class ProductionInput;

/*! 
* \ingroup Objects
* \brief Defines a single input to a production or demand function.
* \details TODO
* \author Pralit Patel, Sonny Kim, Josh Lurz
*/
class Input: public IVisitable, public IRoundTrippable
{
    friend class SocialAccountingMatrix;
    friend class DemandComponentsTable;
    friend class SectorReport;
    friend class SGMGenTable;
public:
    Input();
    virtual ~Input();
    virtual Input* clone() const = 0;
    virtual void copyParam( const Input* aInput ) = 0;
    virtual void copyParamsInto( ProductionInput& aInput ) const = 0;
    virtual void copyParamsInto( DemandInput& aInput ) const = 0;
    void XMLParse( const xercesc::DOMNode* node );
    void completeInit();
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    const std::string& getName() const;
    double getConversionFactor( const std::string& aRegionName ) const;
    double getGHGCoefficient( const std::string& aGHGName, const std::string& aRegionName ) const;
    double getDemandPhysical( const std::string& aRegionName ) const;
    double getDemandCurrency() const;
    double getTechChange( double aEnergyTechChange, double aMaterialTechChange,
                          const std::string& aRegionName ) const;
    double getPrice( const std::string& aRegionName, const int aPeriod ) const;
    double getPricePaid() const;
    void setPricePaid( double aPricePaid );
    double getPriceReceived( const std::string& aRegionName, const int aPeriod ) const;
    void setDemandCurrency( const double aDemand, const std::string& aRegionName, 
        const std::string& aSectorName, int aPeriod );
    virtual void scaleCoefficient( double scaleValue );
    double getCoefficient() const;
    void setCoefficient( double coef );
    double getPriceAdjustment() const;
    virtual double getPriceElasticity() const = 0;
    virtual double getIncomeElasticity() const = 0;
    virtual void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    bool isFactorSupply() const;
    inline bool isCapital() const;
    inline bool isNumeraire() const;
    static bool isInputEnergyGood( const std::string& aInputName, const std::string& aRegionName );
    static bool isInputPrimaryEnergyGood( const std::string& aInputName, const std::string& aRegionName );
    static bool isInputSecondaryEnergyGood( const std::string& aInputName, const std::string& aRegionName );
    static double getMarketConversionFactor( const std::string& aInputName, const std::string& aRegionName );
protected:
    enum Type {
        ENERGY,
        MATERIAL,
        LAND,
        LABOR,
        CAPITAL,
        NOT_SET
    };
    std::string mName; //!< Name of Input
    Value mCoefficient; //!< Coefficient for production or demand function
    Value mDemandCurrency; //!< Currency Demand
    mutable Value mConversionFactor; //!< Conversion Factor
    //! Cached GHG coefficient, this will have to be fixed for multiple gases.
    mutable Value mGHGCoefficient;
    Value mPriceAdjustFactor; //!< Price adjustment factor
    Value mPricePaid; //!< price paid for input, adjusted from market price
    Value mTechnicalChange; //!< Technical Change
    bool mIsNumeraire; //!< Whether the input is the numeraire input.
    bool mIsCapital; //!< Whether the input is capital.

    virtual const std::string& getXMLName() const = 0;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ) = 0;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const = 0;
    Type getType( const std::string& aRegionName ) const;
private:
};

/*! \brief Return whether the input is Capital.
* \author Josh Lurz
* \return Whether the input is capital.
*/
bool Input::isCapital() const {
    return mIsCapital;
}

/*! \brief Return whether the input is the numeraire.
* \author Josh Lurz, Sonny Kim
* \return Whether the input is the numeraire.
*/
bool Input::isNumeraire() const {
    return mIsNumeraire;
}

#endif // _INPUT_H_
