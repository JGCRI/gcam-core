#ifndef _AGHG_H_
#define _AGHG_H_
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
* \file aghg.h
* \ingroup Objects
* \brief The AGHG class header file.
* \author Sonny Kim
* \author Jim Naslund
*/

#include <xercesc/dom/DOMNode.hpp>
#include <vector>
#include <memory>
#include <string>

#include "util/base/include/ivisitable.h"
#include "util/base/include/iround_trippable.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/data_definition_util.h"

// Forward declarations
class GDP;
class IInfo;
class IOutput;
class AEmissionsDriver;
class AEmissionsControl;
class ICaptureComponent;
class IInput;
class CachedMarket;

// Need to forward declare the subclasses as well.
class CO2Emissions;
class NonCO2Emissions;

/*! 
 * \ingroup Objects
 * \brief The AGHG class describes a single gas.
 * \details The AGHG class describes a single gas with attributes of gas name,
 *          unit, emissions coefficients, and the calculated emissions.
 *
 *          Note that for non-CO2 GHGs, there are two methods of setting
 *          emissions. Through an emissions coefficient or a read-in input
 *          emissions for a base year (or years). These are mutually exclusive.
 *          The last one of these read in determines the method used.
 * \author Sonny Kim, Marshall Wise, Steve Smith, Nick Fernandez, Jim Naslund
 */
class AGHG: public IVisitable, public IRoundTrippable
{ 
    friend class XMLDBOutputter;

public:
    //! Virtual Destructor.
    virtual ~AGHG();
    
    //! Clone operator.
    virtual AGHG* clone() const = 0;
    
    virtual void copyGHGParameters( const AGHG* aPrevGHG ) = 0;

    // IRoundTrippable methods
    void XMLParse( const xercesc::DOMNode* aNode );

    void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;

    void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;

    virtual const std::string& getName() const;

    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const IInfo* aTechIInfo );

    virtual void initCalc( const std::string& aRegionName,
                           const IInfo* aLocalInfo,
                           const int aPeriod );

    /*!
     * \note This is only used by SGM inputs.
     */
    double getGHGValue( const IInput* aInput, const std::string& aRegionName, const std::string& aProdName,
                        const ICaptureComponent* aSequestrationDevice, const int aPeriod ) const;
    
    /*!
     * \note This is only used by SGM outputs.
     */
    double getGHGValue( const IOutput* aOutput, const std::string& aRegionName, const std::string& aProdName,
                        const ICaptureComponent* aSequestrationDevice, const int aPeriod ) const;
    /*! 
     * \brief Convert GHG tax and any storage costs into energy units using GHG
     *        coefficients and return the value or cost of the tax and storage
     *        for the GHG.
     * \details Applies taxes only if emissions occur. Emissions occur if there
     *          is a difference in the emissions coefficients.
     * \author Sonny Kim
     * \param aRegionName Name of the region for GHG
     * \param aInputs Vector of Technology inputs.
     * \param aOutputs Vector of Technology outputs.
     * \param aSequestrationDevice The device responsible for capturing emissions.
     * \param aPeriod The period in which this calculation is occurring.
     * \return Generalized cost or value of the GHG
     */
    virtual double getGHGValue( const std::string& aRegionName,
                                const std::vector<IInput*>& aInputs,
                                const std::vector<IOutput*>& aOutputs,
                                const ICaptureComponent* aSequestrationDevice,
                                const int aPeriod ) const = 0;
    /*!
     * \brief Calculates emissions of GHG's
     * \details Emissions of these gases are equal to the emissions driver
     *          multiplied by the emissions coefficient (how much of the
     *          chemical forming the GHG is emitted per unit driver) multiplied
     *          by the control function (the extent to which regions are
     *          expected to put controls on end-of-pipe emissions- based on
     *          their pppGdp) multiplied by the result of the Marginal Abatement
     *          curve, and finally by an external read-in emissions Adjustment
     *          factor(if any). The function also sets the emissions coefficient
     *          if emissions are read in.  
     * \author Nick Fernandez, Steve Smith
     * \param aRegionName Name of the region for GHG
     * \param aInputs Vector of Technology inputs.
     * \param aOutputs Vector of Technology outputs.
     * \param aGDP Regional GDP.
     * \param aSequestrationDevice The object potentially capturing emissions.
     * \param aPeriod The period in which this calculation is occurring.
     */
    virtual void calcEmission( const std::string& aRegionName, 
                               const std::vector<IInput*>& aInputs,
                               const std::vector<IOutput*>& aOutputs,
                               const GDP* aGDP,
                               ICaptureComponent* aSequestrationDevice,
                               const int aPeriod ) = 0;
    
    double getEmission( const int aPeriod ) const;
    
    double getEmissionsSequestered( const int aPeriod ) const;

    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    
    virtual void doInterpolations( const int aYear, const int aPreviousYear,
                                   const int aNextYear, const AGHG* aPreviousGHG,
                                   const AGHG* aNextGHG );

protected:

    AGHG();
    AGHG( const AGHG& aOther );
    AGHG& operator=( const AGHG& aOther );

    /*!
     * \brief Get the XML node name for output to XML.
     * \details This public function accesses the private constant string,
     *          XML_NAME. This way the tag is always consistent for both read-in
     *          and output and can be easily changed. This function may be
     *          virtual to be overridden by derived class pointers.
     * \author Jim Naslund
     * \return The constant XML_NAME.
     */
    virtual const std::string& getXMLName() const = 0;
    
    DEFINE_DATA(
        /* Declare all subclasses of AGHG to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( AGHG, CO2Emissions, NonCO2Emissions ),
        
        //! GHG name
        CREATE_SIMPLE_VARIABLE( mName, std::string, "name" ),

        //! Unit of emissions
        CREATE_SIMPLE_VARIABLE( mEmissionsUnit, std::string, "emissions-unit" ),

        //! Emissions (calculated)
        //! TODO: These are sized to store emissions for all periods however only
        //!       a fraction of that will actually be used (depending on the technology
        //!       vintage and lifetime.
        CREATE_ARRAY_VARIABLE( mEmissions, objects::PeriodVector<double>, "emissions" ),

        //! Emissions sequestered by a ICaptureComponent
        //! TODO: These are sized to store emissions for all periods however only
        //!       a fraction of that will actually be used (depending on the technology
        //!       vintage and lifetime.
        CREATE_ARRAY_VARIABLE( mEmissionsSequestered, objects::PeriodVector<double>, "emissions-sequestered" )
    )
    
    //! Pre-located market which has been cached from the marketplace to get the price
    //! of this ghg and add demands to the market.
    std::auto_ptr<CachedMarket> mCachedMarket;

    /*!
     * \brief Parses any child nodes specific to derived classes
     * \details Method parses any input data from child nodes that are specific
     *          to the classes derived from this class.
     * \author Josh Lurz, Steve Smith
     * \param aNodeName name of current node
     * \param aCurrNode pointer to the current node in the XML input tree
     * \return Whether any node was parsed.
     */
    virtual bool XMLDerivedClassParse( const std::string& aNodeName, const xercesc::DOMNode* aCurrNode ) = 0;

    /*!
     * \brief XML output stream for derived classes
     * \details Function writes output due to any variables specific to derived
     *          classes to XML
     * \author Jim Naslund
     * \param aOut reference to the output stream
     * \param aTabs A tabs object responsible for printing the correct number of
     *        tabs. 
     */
    virtual void toInputXMLDerived( std::ostream& aOut, Tabs* aTabs ) const = 0;
    
    /*!
     * \brief XML debug output stream for derived classes
     * \details Function writes output due to any variables specific to derived
     *          classes to XML
     * \author Jim Naslund
     * \param aPeriod The model period.
     * \param aOut reference to the output stream
     * \param aTabs A tabs object responsible for printing the correct number of
     *        tabs. 
     */
    virtual void toDebugXMLDerived( const int period, std::ostream& aOut, Tabs* aTabs ) const = 0;

    void addEmissionsToMarket( const std::string& aRegionName, const int aPeriod );
    
private:
    void copy( const AGHG& other );
};

#endif // _AGHG_H_
