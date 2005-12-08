#ifndef _IPARSABLE_H_
#define _IPARSABLE_H_
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
* \file iparsable.h  
* \ingroup Objects
* \brief Header file for the IParsable interface.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include <xercesc/dom/DOMNode.hpp>
/*!
* \ingroup Objects
* \brief An interface to a class which can be parsed by the XMLParser.
* \details This interface represents a contract by a class that it implements
*          the XMLParse function. Classes that implement this interface are
*          classes that initiate an XML parse. Since they implement this
*          interface, these classes can be passed to the XMLHelper as a pointer
*          to the IParsable interface. This allows the XMLParser to then call
*          their parsing routine at the appropriate time, which still
*          controlling XML parsing. This class has no data members and so
*          classes that already inherit from another class may implement this
*          interface without multiple inheritance problems. 
* \author Josh Lurz
*/
class IParsable {
public:
	//! Virtual destructor so that instances of the interface may be deleted
    //! correctly through a pointer to the interface.
    inline virtual ~IParsable();
    
	/*! \brief A function which initializes a top level XML parse of a DOM tree
    *          given the root of the DOM tree.
    * \param aRoot The root of a DOM tree.
    * \return Whether the parse completed successfully.
    */
    virtual bool XMLParse( const xercesc::DOMNode* aRoot ) = 0;
};

// Inline function definitions.
IParsable::~IParsable(){
}
#endif // _IPARSABLE_H_
