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
    GhgInput* clone() const;
    const std::string& getXMLName() const;
    static const std::string& getXMLNameStatic();
protected:
    double emissionsDriver( const double inputIn, const double outputIn ) const;
private:
    static const std::string XML_NAME; //!< node name for toXML methods
};

#endif // _GHG_INPUT_H_

