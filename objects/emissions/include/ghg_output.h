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

#include <string>
#include "emissions/include/ghg.h"

/*! 
* \ingroup CIAM
* \brief A type of GHG which drives emissions from the output value. 
* \author Steve Smith
*/

class GhgOutput: public Ghg {
public:
    GhgOutput* clone() const;
    const std::string& getXMLName() const;
    static const std::string& getXMLNameStatic();
protected:
    double emissionsDriver( const double inputIn, const double outputIn ) const;
private:
    static const std::string XML_NAME; //!< node name for toXML methods
};

#endif // _GHG_OUTPUT_H_

