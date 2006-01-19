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
* \file input_finder.cpp
* \ingroup Objects
* \brief The InputFinder class source file.
*
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <algorithm>
#include "util/base/include/input_finder.h"
#include "technologies/include/technology.h"

using namespace std;

/*! \brief Constructor
*/
InputFinder::InputFinder(){
}

/*!
* \brief Add an input used by the technology to the list of inputs.
* \details Adds the technology's input to the list of inputs if it is unique.
* \param aTechnology Technology from which to update inputs.
* \param aPeriod Period in which to update.
*/
void InputFinder::startVisitTechnology( const technology* aTechnology, const int aPeriod ){
    // Check if the input is already known.
    const string input = aTechnology->getFuelName();
    if( find( mInputs.begin(), mInputs.end(), input ) == mInputs.end() ){
        mInputs.push_back( input );
    }
}

/*! \brief Get the list of inputs used.
* \pre The InputFinder has already visited technologies.
* \return The list of inputs used.
*/
const list<string>& InputFinder::getInputs() const {
    return mInputs;
}

void InputFinder::finish() const {
}
