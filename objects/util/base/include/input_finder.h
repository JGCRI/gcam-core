#ifndef _INPUT_FINDER_H_
#define _INPUT_FINDER_H_
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
* liability or responisbility for the use of this software.
*/

/*! 
* \file input_finder.h
* \ingroup Objects
* \brief InputFinder class header file.
* \author Josh Lurz
*/

#include "util/base/include/default_visitor.h"
#include <list>
class Technology;

/*! 
* \ingroup Objects
* \brief A class which determines the list of inputs used for a portion of the
*        model tree.
* \details The input finder is a visitor which when passed through a portion of
*          the model tree will determine a unique list of inputs used. The input
*          finder offers a simple interface to access this list. The elements of
*          the list are guaranteed to be unique. No other contracts, such as
*          order, are specified for the list. There is also no information
*          stored about which technology used which input, or the quantity it
*          used. This allows the visitor to be used before demands are
*          calculated.
* \author Josh Lurz
*/

class InputFinder : public DefaultVisitor {
public:
    InputFinder();

    virtual void startVisitTechnology( const Technology* aTechnology,
                                       const int aPeriod );

    virtual void finish() const;

    // Non-IVisitor interface method.
    const std::list<std::string>& getInputs() const;
private:
    //! List of inputs
    std::list<std::string> mInputs;
};

#endif // _INPUT_FINDER_H_
