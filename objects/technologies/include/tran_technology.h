#ifndef _TRAN_TECHNOLOGY_H_
#define _TRAN_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file tran_technology.h
* \ingroup CIAM
* \brief The transportation technology class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/technology.h"

class GDP;

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
    tranTechnology();
    virtual tranTechnology* clone() const;
    virtual void clear();
    virtual void XMLDerivedClassParse( const std::string nodeName, const xercesc::DOMNode* curr ); // for derived classes
    virtual void calcCost( const std::string regionName, const int per); 
    virtual void calcShare( const std::string regionName, const int per); 
    // calculates fuel input and technology output
    virtual void production( const std::string& regionName, const std::string& prodName,double dmd, const GDP* gdp, const int per);    
    virtual double getIntensity(const int per) const; // return fuel intensity
};

#endif // _TRAN_TECHNOLOGY_H_

