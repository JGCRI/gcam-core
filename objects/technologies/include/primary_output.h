#ifndef _PRIMARY_OUTPUT_H_
#define _PRIMARY_OUTPUT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
 * \file primary_output.h
 * \ingroup Objects
 * \brief PrimaryOutput class header file.
 * \author Josh Lurz
 */

#include <string>
#include <xercesc/dom/DOMNode.hpp>

class Tabs;
class DependencyFinder;

#include "technologies/include/ioutput.h"
#include "util/base/include/value.h"

/*! 
 * \ingroup Objects
 * \brief An output representing the primary output of a Technology.
 * \details The primary output of a Technology is the same as the output of the
 *          sector containing a Technology. The primary output should always be
 *          chosen such that the revenue from the primary output always exceeds
 *          any revenue from secondary outputs. Primary outputs have expenses and
 *          revenues calculated directly by the Technology object.
 *          
 *          <b>XML specification for PrimaryOutput</b>
 *          - XML name: None. The primary output is generated automatically
                        and cannot be parsed.
 *
 * \author Josh Lurz
 */
class PrimaryOutput: public IOutput
{ 
public:
    /*!
     * \brief Constructor.
     * \param aSectorName Name of the sector and thus the primary output.
     */
    PrimaryOutput( const std::string& aSectorName );

    virtual PrimaryOutput* clone() const;

    virtual bool isSameType( const std::string& aType ) const;
    
    virtual const std::string& getName() const;

    virtual bool XMLParse( const xercesc::DOMNode* aNode );
    
    virtual void toInputXML( std::ostream& aOut,
                             Tabs* aTabs ) const;

    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;

    virtual void completeInit( const std::string& aSectorName,
                               DependencyFinder* aDependencyFinder,
                               const bool aIsTechOperating );

    virtual void initCalc( const std::string& aRegionName,
                           const int aPeriod );

    virtual void postCalc( const std::string& aRegionName,
                           const int aPeriod );

    virtual void scaleCoefficient( const double aScaler );

    virtual double calcPhysicalOutput( const double aPrimaryOutput,
                                       const std::string& aRegionName,
                                       const ICaptureComponent* aCaptureComponent,
                                       const int aPeriod ) const;

    virtual void setPhysicalOutput( const double aPrimaryOutput,
                                    const std::string& aRegionName,
                                    ICaptureComponent* aCaptureComponent,
                                    const int aPeriod );

    virtual double getPhysicalOutput( const int aPeriod ) const;

    virtual double getValue( const std::string& aRegionName,
                             const ICaptureComponent* aCaptureComponent,
                             const int aPeriod ) const;

    virtual double getEmissionsPerOutput( const std::string& aGHGName,
                                          const int aPeriod ) const;

    virtual void accept( IVisitor* aVisitor,
                        const int aPeriod ) const;

    // Documentation is inherited.
    virtual void setLandAllocator(
       ILandAllocator*    aLandAllocator,
       const std::string& aName,
       const std::string& aLandType ) {}

protected:
    //! Physical output by period.
    std::vector<Value> mPhysicalOutputs;

    //! Name of the primary output. This is the same as the sector name.
    const std::string mName;

    //! CO2 emissions coefficient cached from the marketplace.
    Value mCachedCO2Coef;
};

#endif // _PRIMARY_OUTPUT_H_
