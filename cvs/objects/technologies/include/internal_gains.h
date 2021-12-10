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


#ifndef _INTERNAL_GAINS_H_
#define _INTERNAL_GAINS_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file internal_gains.h
 * \ingroup Objects
 * \brief InternalGains class header file.
 * \author Josh Lurz
 */

#include <string>

class Tabs;

#include "technologies/include/ioutput.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

/*! 
 * \ingroup Objects
 * \brief An output representing a heat output produced as a secondary output of
 *        the Technology.
 * \details Internal gains represent a heat output produced in conjunction with
 *          a primary product which has a positive value in the heating market
 *          and a negative value in the cooling market. Output of the internal
 *          gain is not optimized, it is always produced at a fixed ratio to the
 *          primary output, and may not be discarded.
 *          
 *          <b>XML specification for InternalGains</b>
 *          - XML name: \c internal-gains
 *          - Contained by: Technology
 *          - Parsing inherited from class: None.
 *          - Attributes: None.
 *          - Elements:
 *              - \c output-ratio InternalGains::mOutputRatio
 *                   The ratio of primary product to internal gains in this
 *                   technology year.
 *              - \c internal-gains-market-name InternalGains::mTrialMarketName
 *                   The name of the market to add internal gains into.
 *          
 * \author Josh Lurz
 * \author Pralit Patel
 * \author Jiyong Eom
 */
class InternalGains : public IOutput {
public:
    InternalGains();

    virtual const std::string& getXMLReportingName() const;
    
    virtual const std::string& getXMLName() const;

    /*!
     * \brief Get the XML name for the class.
     * \return The XML name for the class.
     */
    static const std::string& getXMLNameStatic();
    
    virtual ~InternalGains();

    virtual InternalGains* clone() const;

    virtual bool isSameType( const std::string& aType ) const;

    virtual const std::string& getName() const;

    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;

    virtual void completeInit( const std::string& aSectorName,
                               const std::string& aRegionName,
                               const IInfo* aTechInfo,
                               const bool aIsTechOperating );

    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const int aPeriod );

    virtual void postCalc( const std::string& aRegionName,
                           const int aPeriod );

    virtual void scaleCoefficient( const double aScaler );

    virtual OutputList calcPhysicalOutput( const double aPrimaryOutput,
                                           const std::string& aRegionName,
                                           const ICaptureComponent* aCaptureComponent,
                                           const int aPeriod ) const;

    virtual void setPhysicalOutput( const double aPrimaryOutput,
                                    const std::string& aRegionName,
                                    ICaptureComponent* aCaptureComponent,
                                    const int aPeriod );

    virtual double getPhysicalOutput( const int aPeriod ) const;

    virtual void setCurrencyOutput( const std::string& aRegionName,
                                    const double aOutput,
                                    const int aPeriod )
    {
    }

    virtual double getCurrencyOutput( const int aPeriod ) const
    {
        return 0;
    }

    virtual double getValue( const std::string& aRegionName,
                             const ICaptureComponent* aCaptureComponent,
                             const int aPeriod ) const;
    
    virtual std::string getOutputUnits( const std::string& aRegionName ) const;

    virtual double getEmissionsPerOutput( const std::string& aGHGName,
                                          const int aPeriod ) const;

    virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const;

    // Documentation is inherited.
    virtual void sendLandAllocator(
       const ILandAllocator*    aLandAllocator,
       const std::string& aName) {}
    
    virtual void doInterpolations( const int aYear, const int aPreviousYear,
                                   const int aNextYear, const IOutput* aPreviousInput,
                                   const IOutput* aNextInput );
protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        IOutput,

        //! Physical output by period.
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "physical-output", mPhysicalOutputs, objects::TechVintageVector<Value> ),

        //! The name of the output
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),

        //! Internal Gains trial market name.
        DEFINE_VARIABLE( SIMPLE, "internal-gains-market-name", mTrialMarketName, std::string ),

        //! Ratio of the internal gains to primary output production such that
        //! primary output multiplied by the ratio is equal to internal gains.
        DEFINE_VARIABLE( SIMPLE, "output-ratio", mOutputRatio, Value )
    )
    
    void copy( const InternalGains& aOther );
};

#endif // _INTERNAL_GAINS_H_
