#ifndef _GHG_INPUT_H_
#define _GHG_INPUT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file ghg_input.h
* \ingroup CIAM
* \brief The GhgInput class header file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/
#include "emissions/include/ghg.h"

/*! 
* \ingroup CIAM
* \brief A type of GHG which drives emissions the input value.
* \author Steve Smith
*/

class GhgInput: public Ghg {
public:
    virtual void calcEmission( const std::string& regionName, const std::string& fuelname, const double input, const std::string& prodname, const double output );
};

#endif // _GHG_INPUT_H_

