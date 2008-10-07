#ifndef _ACARBON_FLOW_H_
#define _ACARBON_FLOW_H_
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
 * \file acarbon_flow.h
 * \ingroup Objects
 * \brief ACarbonFlow class header file.
 * \author Jim Naslund
 */

#include <xercesc/dom/DOMNode.hpp>

#include "util/base/include/ivisitable.h"
#include "util/base/include/iparsable.h"
#include "util/base/include/iround_trippable.h"
#include "ccarbon_model/include/carbon_box_model.h"

class EnvironmentalInfo;
class CarbonBox;
class ICarbonContainer;

/*!
 * \brief An abstract class which represents a carbon flow from one carbon
 *        container to another.
 * \details This class represents a one-way flow from one carbon container
 *          (the source) to another (the target).  The flow is owned by the
 *          source of the flow and contains a pointer to the target.
 */
class ACarbonFlow : public IParsable,
                    public IVisitable,
                    public IRoundTrippable {

friend class ComplexCarbonDotPrinter;
friend class ComplexCarbonPrinter;
friend class XMLDBOutputter;

public:
    //! default constructor
    ACarbonFlow();
    //! constructor 1: take in a FlowType as parameter
    ACarbonFlow( FlowType aType );
    //! copy constructor for carbon flow
    ACarbonFlow( const ACarbonFlow& aCarbonFlow );    
    //! virtual copy constructor
    virtual ACarbonFlow* clone() const = 0;    
    //! default destructor
    virtual ~ACarbonFlow();

    // IParsable Interface
    virtual bool XMLParse( const xercesc::DOMNode* aNode );
    // IRoundTripable Interface
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut,
                             Tabs* aTabs ) const;
    // IVisitable Interface
    virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const;
    // Derived class XML functions
    virtual bool XMLDerivedClassParse( const xercesc::DOMNode* aNode ) = 0;
    virtual void toInputXMLDerived( std::ostream& aOut, Tabs* aTabs ) const = 0;
    virtual void toDebugXMLDerived( std::ostream& aOut, Tabs* aTabs ) const = 0;
    virtual const std::string& getXMLName() const = 0;
    static const std::string& getXMLNameStatic();
    
    //! Initialization function
    virtual void completeInit( CarbonBoxModel& aParentBoxModel );

     bool matches( FlowType aFlowType ) const;

    /*!
     * \brief Transfers carbon from this box to another.
     * \details Transfers the passed in value to this flow's target.
     * \param aValue the amount to transfer.
     * \param aEnvInfo environmental info object.
     * \param aYear the year.
     * \param aBoxType the type of box.
     */
    virtual void transfer( const double aValue, const EnvironmentalInfo* aEnvInfo,
                           const int aYear, const BoxType aBoxType ) = 0;

    virtual void transferIfOfType( const double aValue, const EnvironmentalInfo* aEnvInfo,
                                   const int aYear, const BoxType aBoxType,
                                   const FlowType aFlowType );

    const std::string& getTargetName() const;

    void setTargetName(const std::string aTargetName);
    void setTargetFlowType( );
    BoxType getTargetFlowType( ) const;

    double getFraction() const;
    void setFraction( const double aFraction );

    FlowType getFlowType() const;

    virtual void copyBoxType( BoxType aCarbonBoxType );

    virtual void setDefaultLUCFLowOut( const std::string aTargetName );
    virtual void initialization();
protected:
    //! The carbon container this flow transfers to
    ICarbonContainer* mTarget;
    //! Fraction of this container's carbon store that will flow to the target box.
    double mFraction;

private:
    //! Name of target, set during parsing.
    std::string mTargetName;
    //! Enum for the type of this flow.
    FlowType mType;
    //! Enum for the type of box assosiate with this flow
    BoxType mBoxType;
    //! Enum for the type of this flow of this box's target.
    BoxType mTargetType;
};

#endif // _ACARBON_FLOW_H_
