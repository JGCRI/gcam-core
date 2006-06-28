#ifndef _TRAN_TECHNOLOGY_H_
#define _TRAN_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file tran_technology.h
* \ingroup Objects
* \brief The transportation technology class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/technology.h"

class GDP;
class IInfo;

/*! 
* \ingroup Objects
* \brief This transportation technology class is based on the MiniCAM description of technology.
* \author Sonny Kim
*/

class TranTechnology : public technology
{
public:
    TranTechnology( const std::string& aName, const int aYear );
    TranTechnology* clone() const;
    const std::string& getXMLName1D() const;
    static const std::string& getXMLNameStatic1D();
    void calcCost( const std::string& regionName, const std::string& sectorName, const int per );
    
    virtual void production( const std::string& aRegionName,
                             const std::string& aSectorName,
                             const double aDemand,
                             const GDP* aGDP,
                             const int aPeriod );

    double getIntensity(const int per) const;

    virtual double getCalibrationOutput( const int aPeriod ) const;
    
    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const IInfo* aSubsectorIInfo,
                           const Demographic* aDemographics,
                           const int aPeriod );

protected:
	//! Vehicle load factor.
    double loadFactor;

	//! Annual technical change rate.
	double mTechnicalChange;
	
    //! Vehicle intensity.
    double mIntensity;

	double getCumulativeTechnicalChange( const int aPeriod ) const;
    bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
private:
    static const std::string XML_NAME; //!< The XML name of this object.
};

#endif // _TRAN_TECHNOLOGY_H_

