#ifndef _IVISITABLE_H_
#define _IVISITABLE_H_
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
* \file ivisitiable.h
* \ingroup Objects
* \brief IVisitable class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
class IVisitor;

/*! \brief The IVisitable interface allows an object to be visited by an
*          IVisitor.
* \details The interface defines an ability to accept a IVisitor to the object.
*          An object implementing this interface must define a single function
*          accept which takes a visitor and a period as a parameters. The object
*          must call the correct visit methods on the IVisitor with itself and
*          the period as the parameters. This allows double-dispatch to occur,
*          or more simply the function called is based on the type of both the
*          object and the output container. The object must then pass the
*          IVisitor object onto any of its children which can be visited, using
*          the accept method.
*/

class IVisitable {
public:
	//! Virtual destructor so that instances of the interface may be deleted
    //! correctly through a pointer to the interface.
    inline virtual ~IVisitable();
	
	/*! \brief Accept a visitor to the object.
	* \details The accept method causes the object to inform the IVisitor that
    *          it is visiting the current object for the given period. The
    *          object must then pass the IVisitor onto any children which can be
    *          visited.
	* \param aIVisitor Visitor to accept.
	* \param aPeriod Period
    */
	virtual void accept( IVisitor* aVisitor, const int aPeriod ) const = 0;
};

// Inline methods
IVisitable::~IVisitable(){
}

#endif // _IVISITABLE_H_
