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
    double techChangeCumm; //!< cummulative technical change
    double loadFactor; //!< vechile load factor
    double vehicleOutput; //!< service per vehicle
    double serviceOutput; //!< service by technology
	double baseScaler; // constant scaler to scale base output
    
public:
    tranTechnology(); // default construtor
    virtual void clear();
    virtual void XMLDerivedClassParse( const string nodeName, const DOMNode* curr ); // for derived classes
    virtual void calcCost( const string regionName, const int per); 
    virtual void calcShare( const string regionName, const int per); 
    // calculates fuel input and technology output
    virtual void production(const string& regionName,const string& prodName,double dmd,const int per);    
    virtual double getIntensity(const int per) const; // return fuel intensity
};

#endif // _TRANTECHNOLOGY_H_