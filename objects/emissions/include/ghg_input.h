#ifndef _GHGoutput_H_
#define _GHGoutput_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file ghg_output.h
* \ingroup CIAM
* \brief The Ghg class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include <vector>
#include "Ghg.h"

// Forward declaration
class Emcoef_ind;
class Tabs;

/*! 
* \ingroup CIAM
* \brief The Ghg class describes a single gas with
* attributes of gas name, unit, emissions coefficients,
* and the calculated emissions.
*
* Emissions emitted indirectly through use of technology are also calculated.
* \author Sonny Kim and Marshall Wise
*/

class GhgInput: public Ghg {
public:
    virtual void calcEmission( const std::string& regionName, const std::string& fuelname, const double input, const std::string& prodname, const double output );
protected:
};

#endif // _GHGoutput_H_

