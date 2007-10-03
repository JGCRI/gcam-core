#ifndef _ACARBON_FLOW_H_
#define _ACARBON_FLOW_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
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
    virtual void completeInit( CarbonBoxModel& aParent );

    double getNumForErrorChecking( FlowType aFlowType ) const;
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

    double getFraction() const;

	FlowType getFlowType() const;

	virtual void copyBoxType( BoxType aCarbonBoxType );

	virtual void setDefaultLUCFLowOut( const std::string aTargetName );

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
};

#endif // _ACARBON_FLOW_H_
