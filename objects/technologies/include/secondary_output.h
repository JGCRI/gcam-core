#ifndef _SECONDARY_OUTPUT_H_
#define _SECONDARY_OUTPUT_H_
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
 * \file secondary_output.h
 * \ingroup Objects
 * \brief SecondaryOutput class header file.
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
 * \brief An output representing a secondary output of the Technology.
 * \details Secondary outputs represent a product produced in conjunction with a
 *          primary product which may have a positive value. Output of the
 *          secondary output is not optimized, it is always produced at a fixed
 *          ratio to the primary output, and may not be discarded. The revenue
 *          from the secondary output must not exceed the cost of the primary
 *          output. The secondary output must be the primary output of another
 *          sector, and the total secondary output of the good must not exceed
 *          demand.
 *          
 *          <b>XML specification for SecondaryOutput</b>
 *          - XML name: \c secondary-output
 *          - Contained by: Technology
 *          - Parsing inherited from class: None.
 *          - Attributes:
 *              - \c name SecondaryOutput::mName
 *          - Elements:
 *              - \c output-ratio SecondaryOutput::mOutputRatio
 *          
 * \author Josh Lurz
 */
class SecondaryOutput: public IOutput
{
    friend class OutputFactory;
public:
    /*!
     * \brief Get the XML name for the class.
     * \return The XML name for the class.
     */
    static const std::string& getXMLNameStatic();

    virtual SecondaryOutput* clone() const;

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
protected:
    /*!
     * \brief Protected constructor so the class can only be created by the
     *        OutputFactory.
     */
    SecondaryOutput();

    //! Physical output by period.
    std::vector<Value> mPhysicalOutputs;

    //! Name of the secondary output. Corresponds to a market for this good and
    //! a supply sector which supplies this good as its primary output.
    std::string mName;

    //! CO2 emissions coefficient cached from the marketplace.
    Value mCachedCO2Coef;
    
    //! Ratio of the secondary output to primary output production such that
    //! primary output multiplied by the ratio is equal to secondary output.
    Value mOutputRatio;
};

#endif // _SECONDARY_OUTPUT_H_
