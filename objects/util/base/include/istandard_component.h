#ifndef _ISTANDARD_COMPONENT_H_
#define _ISTANDARD_COMPONENT_H_
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
* \file istandard_component.h
* \ingroup Objects
* \brief IStandardComponent interface header file.
* \author Josh Lurz
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/iparsable.h"
#include "util/base/include/iround_trippable.h"

class Tabs;

/*! 
 * \ingroup Objects
 * \brief Defines the interface to a standard component.
 * \details The Objects model contains a series of components to represent the
 *          parts of the economic model. These components all share several
 *          common function necessary for the operation of the model. This
 *          interface unifies these methods so that they are always defined in
 *          the exact same way.
 * \author Josh Lurz
 */
class ISimpleComponent { 
public:
	/*!
     * \brief Constructor.
	 * \details Inlined constructor to avoid compiler problems with abstract base
     *          classes. 
     */
    ISimpleComponent();

	/*!
     * \brief Virtual destructor so objects can be deleted through an interface
     *          pointer.
	 * \details Inlined destructor to avoid compiler problems with abstract base
     *          classes. 
     */
	virtual ~ISimpleComponent();

	/*!
     * \brief Creates an exact copy of the component.
	 * \return An exact copy of the component. 
     */
	virtual ISimpleComponent* clone() const = 0;

	/*!
     * \brief Returns whether the type of the object is the same as the passed
     *          in type.
	 * \param aType Type to check the object's type against.
	 * \return Whether the type of the object is the same as the passed in type.
     */
	virtual bool isSameType( const std::string& aType ) const = 0;
    
	/*!
     * \brief Get the name of the component.
	 * \return The name of the component.
	 */
	virtual const std::string& getName() const = 0;

	/*!
     * \brief Write data from this object in an XML format for debugging.
	 * \param aPeriod Period for which to write data.
	 * \param aOut Filestream to which to write.
	 * \param aTabs Object responsible for writing the correct number of tabs. 
     */
	virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const = 0;
};

// Inline function definitions.
inline ISimpleComponent::ISimpleComponent(){
}

inline ISimpleComponent::~ISimpleComponent(){
}

/*! 
 * \ingroup Objects
 * \brief Defines the interface to a standard component which is serialized to XML.
 * \details TODO
 * \todo Is there a better name? DataComponent, XMLComponent, etc?
 * \author Josh Lurz
 */
class IParsedComponent: public ISimpleComponent,
                        public IParsable,
                        public IRoundTrippable 
{ 
public:
	/*!
     * \brief Constructor.
	 * \details Inlined constructor to avoid compiler problems with abstract base
     *          classes. 
     */
    IParsedComponent();
};

// Inline function definitions.
inline IParsedComponent::IParsedComponent(){
}

#endif // _ISTANDARD_COMPONENT_H_
