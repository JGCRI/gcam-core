#ifndef _FRACTIONAL_SECONDARY_OUTPUT_H_
#define _FRACTIONAL_SECONDARY_OUTPUT_H_
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
 * \file fractional_secondary_output.h
 * \ingroup Objects
 * \brief FractionalSecondaryOutput class header file.
 * \author Pralit Patel
 */

#include <string>

class Tabs;

#include "technologies/include/ioutput.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

class PointSetCurve;

/*! 
 * \ingroup Objects
 * \brief A secondary output that adds to the supply of the secondary good based
 *        on a fixed output ratio as well as a fractional adjustment made based
 *        on the price of the secondary good.
 * \details This secondary output is meant to add to the supply of a market which is
 *          independently solved for.  The fractional-production adjustment allows us
 *          to set the price scale for the market and allow smooth supply behavior
 *          down to zero should demand for this secondary good fall off.
 *          
 *          <b>XML specification for FractionalSecondaryOutput</b>
 *          - XML name: \c fractional-secondary-output
 *          - Contained by: Technology
 *          - Parsing inherited from class: None.
 *          - Attributes:
 *              - \c name FractionalSecondaryOutput::mName
 *          - Elements:
 *              - \c output-ratio FractionalSecondaryOutput::mOutputRatio
 *              - \c market-name FractionalSecondaryOutput::mMarketName
 *              - \c fraction-produced FractionalSecondaryOutput::mCostCurve
 *          
 * \author Pralit Patel
 */
class FractionalSecondaryOutput: public IOutput
{
public:
    FractionalSecondaryOutput();
    
    virtual ~FractionalSecondaryOutput();
    
    /*!
     * \brief Get the XML name for the class.
     * \return The XML name for the class.
     */
    static const std::string& getXMLNameStatic();

    virtual FractionalSecondaryOutput* clone() const;

    virtual bool isSameType( const std::string& aType ) const;

    virtual const std::string& getName() const;

    virtual void setName( const std::string& aName );

    virtual const std::string& getXMLReportingName() const;
    
    virtual const std::string& getXMLName() const;

    virtual bool XMLParse( rapidxml::xml_node<char>* & aNode );

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
        // TODO: This could work by converting from physical to currency with
        // the market price.
    }

    virtual double getCurrencyOutput( const int aPeriod ) const
    {
        // TODO: This could work by converting from physical to currency with
        // the market price.
        return 0;
    }

    virtual double getValue( const std::string& aRegionName,
                             const ICaptureComponent* aCaptureComponent,
                             const int aPeriod ) const;
    
    virtual std::string getOutputUnits( const std::string& aRegionName ) const;

    virtual double getEmissionsPerOutput( const std::string& aGHGName,
                                          const int aPeriod ) const;

    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;

    // Documentation is inherited.
    virtual void sendLandAllocator( const ILandAllocator* aLandAllocator,
                                   const std::string& aName ) {}
    
    virtual void doInterpolations( const int aYear, const int aPreviousYear,
                                   const int aNextYear, const IOutput* aPreviousInput,
                                   const IOutput* aNextInput );

protected:

    double getMarketPrice( const std::string& aRegionName, const int aPeriod ) const;

    double calcPhysicalOutputInternal( const std::string& aRegionName, const double aPrimaryOutput,
                                       const int aPeriod ) const;
    
    void copy( const FractionalSecondaryOutput& aOther );
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        IOutput,

        //! Physical output by period.
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "physical-output", mPhysicalOutputs, objects::TechVintageVector<Value> ),

        //! Name of the secondary output. Corresponds to a market for this good 
        //! which must be explicitly solved for.
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),

        //! Ratio of the secondary output to primary output production such that
        //! primary output multiplied by the ratio is equal to secondary output.
        DEFINE_VARIABLE( SIMPLE, "output-ratio", mOutputRatio, Value ),
                            
        //! A fixed price to use for calibration if set.
        //! Note if we are calibrating then we assume a output-ratio of 1 and disable solving.
        DEFINE_VARIABLE( SIMPLE, "calPrice", mCalPrice, Value ),
        
        //! Piece-wise linear cost curve that contains price driven fraction adjustments
        //! to mOutputRatio.
        DEFINE_VARIABLE( CONTAINER | NOT_PARSABLE, "fraction-produced", mCostCurve, PointSetCurve* ),
                                
        //! The market name in which this output is adjusting the value.  If empty
        //! the current region is assumed.
        DEFINE_VARIABLE( SIMPLE, "market-name", mMarketName, std::string )
    )
};

#endif // _FRACTIONAL_SECONDARY_OUTPUT_H_
