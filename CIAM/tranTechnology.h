#ifndef _TRANTECHNOLOGY_H_
#define _TRANTECHNOLOGY_H_
#pragma once

/*! 
* \file tranTechnology.h
* \ingroup CIAM
* \brief The transportation technology class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/


// Standard Library headers.
#include <vector>
#include <map>
#include <string>

// xerces xml headers
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

#include "technology.h"


using namespace std;
using namespace xercesc;

/*! 
* \ingroup CIAM
* \brief This transportation technology class is based on the MiniCAM description of technology.
* \author Sonny Kim
*/

class tranTechnology : public technology
{
protected:
    double intensity; //!< energy intensity
    
public:
    tranTechnology(); // default construtor
    virtual void clear();
    virtual void XMLDerivedClassParse( const string nodeName, const DOMNode* curr ); // for derived classes
    // calculates fuel input and technology output
    virtual void production(const string& regionName,const string& prodName,double dmd,const int per);    
};

#endif // _TRANTECHNOLOGY_H_