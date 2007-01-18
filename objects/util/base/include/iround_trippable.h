#ifndef _IROUND_TRIPPABLE_H_
#define _IROUND_TRIPPABLE_H_
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
* \file iround_trippable.h
* \ingroup Objects
* \brief IRoundTrippable class header file.
* \author Josh Lurz
*/

class Tabs;
#include <iosfwd>

/*! \brief The IRoundTrippable interface allows the data necessary to create an object 
*          to be written to an XML file so that it can be read back in by a later scenario.
* \details Interface which specifies that the object must implement an interface
*          which writes out the data necessary to create this object so it can be read back 
*          in and used in a subsequent scenario. The interface specifies a single method, 
*          toInputXML which must output the object to the given stream. 
*/

class IRoundTrippable {
public:
	//! Virtual destructor so that instances of the interface may be deleted
    //! correctly through a pointer to the interface.
    inline virtual ~IRoundTrippable();

	/*! \brief Serialize the object to an output stream in an XML format.
	* \details Function which writes out all data members of an object which are
    *          necessary to duplicate a model run. This should not include
    *          internal state variables, only variables that were read-in or
    *          changed by calibration.
	* \param aOut Stream into which to write.
	* \param aTabs Object which controls formatting of the file.
    */
	virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const = 0;
};

IRoundTrippable::~IRoundTrippable(){
}

#endif // _IROUND_TRIPPABLE_H_
