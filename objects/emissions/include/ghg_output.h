#ifndef _GHG_OUTPUT_H_
#define _GHG_OUTPUT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file ghg_output.h
* \ingroup CIAM
* \brief The GhgOutput class header file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/
#include "Ghg.h"

/*! 
* \ingroup CIAM
* \brief A type of GHG which drives emissions from the output value. 
* \author Steve Smith
*/

class GhgOutput: public Ghg {
public:
    virtual void calcEmission( const std::string& regionName, const std::string& fuelname, const double input, const std::string& prodname, const double output );
protected:
};

#endif // _GHG_OUTPUT_H_

