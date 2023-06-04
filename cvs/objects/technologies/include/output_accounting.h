#ifndef _OUTPUT_ACCOUNTING_H_
#define _OUTPUT_ACCOUNTING_H_
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
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*! 
 * \file output_accounting.h
 * \ingroup Objects
 * \brief OutputAccounting class header file.
 * \author Pralit Patel
 */

#include <string>

class Tabs;

#include "technologies/include/ioutput.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

/*! 
 * \ingroup Objects
 * \brief An output representing a secondary output of the Technology used for financial accounting.
 * \details This output will be used to keep track of net expenditures of certain goods
 *          for use in macro economic calculations.  Note this output will not have
 *          any direct effect on technology costs, instead it is here for accounting
 *          purposes only.
 *          
 *          <b>XML specification for OutputAccounting</b>
 *          - XML name: \c output-accounting
 *          - Contained by: Technology
 *          - Parsing inherited from class: None.
 *          - Attributes:
 *              - \c name OutputAccounting::mName
 *          - Elements:
 *              - \c output-ratio OutputAccounting::mOutputRatio
 *          
 * \author Pralit Patel
 */
class OutputAccounting: public IOutput
{
public:
    /*!
     * \brief Get the XML name for the class.
     * \return The XML name for the class.
     */
    static const std::string& getXMLNameStatic();
    
    virtual const std::string& getXMLName() const;
    
    OutputAccounting();

    virtual ~OutputAccounting();
    
    virtual OutputAccounting* clone() const;

    virtual bool isSameType( const std::string& aType ) const;

    virtual const std::string& getName() const;

    virtual void setName( const std::string& aName );

    virtual const std::string& getXMLReportingName() const;

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

    // not needed in this output
    virtual void scaleCoefficient( const double aScaler ) {}

    virtual OutputList calcPhysicalOutput( const double aPrimaryOutput,
                                           const std::string& aRegionName,
                                           const ICaptureComponent* aCaptureComponent,
                                           const int aPeriod ) const;

    // not needed in this output
    virtual void setPhysicalOutput( const double aPrimaryOutput,
                                    const std::string& aRegionName,
                                    ICaptureComponent* aCaptureComponent,
                                    const int aPeriod ) {}

    // not needed in this output
    virtual double getPhysicalOutput( const int aPeriod ) const { return 0.0; }

    virtual void setCurrencyOutput( const double aPysicalOutput,
                                    const double aCurrencyConversionPrice,
                                    const std::string& aRegionName,
                                    const int aPeriod );

    virtual double getCurrencyOutput( const int aPeriod ) const;

    // not needed in this output
    virtual double getValue( const std::string& aRegionName,
                             const ICaptureComponent* aCaptureComponent,
                             const int aPeriod ) const { return 0.0; }
    
    virtual std::string getOutputUnits( const std::string& aRegionName ) const;

    // not needed in this output
    virtual double getEmissionsPerOutput( const std::string& aGHGName,
                                          const int aPeriod ) const { return 0.0; }

    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;

    // Documentation is inherited.
    virtual void sendLandAllocator( const ILandAllocator* aLandAllocator,
                                   const std::string& aName ) {}
    
    virtual void doInterpolations( const int aYear, const int aPreviousYear,
                                   const int aNextYear, const IOutput* aPreviousInput,
                                   const IOutput* aNextInput );

protected:

    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        IOutput,

        //! Currency output by period.
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "currency-output", mCurrencyOutputs, objects::TechVintageVector<Value> ),

        //! Name of the secondary output. Corresponds to a market which will collect
        //! this accounting information.
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),
                            
        //! The market name in which this output is adjusting the value.  If empty
        //! the current region is assumed.
        DEFINE_VARIABLE( SIMPLE, "market-name", mMarketName, std::string ),

        //! Ratio of the secondary output to primary output production such that
        //! primary output multiplied by the ratio is equal to secondary output.
        DEFINE_VARIABLE( SIMPLE, "output-ratio", mOutputRatio, Value ),
        
        //! The base-price to use if mUseBasePrice is set
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "base-price", mBasePrice, Value ),
        
        //! A flag to indicate if this output should use the "base-price" to convert to currency instead of the
        //! current price
        DEFINE_VARIABLE( SIMPLE, "use-base-price", mUseBasePrice, bool )
    )
    
    void copy( const OutputAccounting& aOther );
};

#endif // _OUTPUT_ACCOUNTING_H_
