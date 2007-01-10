#ifndef _CO2_EMISSIONS_H_
#define _CO2_EMISSIONS_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

/*! 
 * \file co2_emissions.h
 * \ingroup Objects
 * \brief CO2Emissions class header file.
 * \author Jim Naslund
 */

#include "emissions/include/aghg.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/value.h"

/*! 
 * \ingroup Objects
 * \brief A class that represents CO2 emissions.
 * \author Jim Naslund
 */
class CO2Emissions: public AGHG { 
public:
    virtual ~CO2Emissions();

    virtual CO2Emissions* clone() const;
    
    virtual void copyGHGParameters( const AGHG* aPrevGHG );
    
    static const std::string& getXMLNameStatic();

    virtual const std::string& getName() const;
    
    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aFuelName,
                           const IInfo* aLocalInfo,
                           const int aPeriod );

    virtual double getGHGValue( const std::string& aRegionName,
                                const std::string& aFuelName,
                                const std::vector<IOutput*>& aOutputs,
                                const double aEfficiency,
                                const ICaptureComponent* aSequestrationDevice,
                                const int aPeriod ) const;

    virtual void calcEmission( const std::string& aRegionName,
                               const std::string& aFuelname,
                               const double aInput,
                               const std::vector<IOutput*>& aOutputs,
                               const GDP* aGDP,
                               ICaptureComponent* aSequestrationDevice,
                               const int aPeriod );

protected:
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void parseName( const std::string& aNameAttr );
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;

    //! Fuel coefficient.
    Value mFuelCoefficient;
};

#endif // _CO2_EMISSIONS_H_

