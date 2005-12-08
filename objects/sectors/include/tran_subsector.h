#ifndef _TRAN_SUBSECTOR_H_
#define _TRAN_SUBSECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file tran_subsector.h
* \ingroup Objects
* \brief The Transportation Subsector header file. 
* \author Marshall Wise, Sonny Kim, Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/subsector.h"

// Forward declarations
class GDP;
class MoreSectorInfo;
class Demographic;
class IInfo;

/*! 
* \ingroup Objects
* \brief A derived subsector representing a mode of transportation.
* \author Marshall Wise, Sonny Kim, Josh Lurz
*/
class TranSubsector: public Subsector

{
public:
    TranSubsector( std::string regionName, std::string sectorName );
    void calcShare( const int period, const GDP* gdp ); 
    void setoutput( const double demand, const int period, const GDP* gdp );
    static const std::string& getXMLNameStatic();
    
    virtual void initCalc( NationalAccount& aNationalAccount,
                           const Demographic* aDemographics,
                           const MoreSectorInfo* aMoreSectorInfo,
                           const int aPeriod );

protected:
    std::vector<double> speed; // Speed of Mode in Miles/hour
    std::vector<double> popDenseElasticity; // Population Density Elasticity of mode
    std::vector<double> timeValue; // time value of average modal speed
    std::vector<double> mServiceOutputs; //!< Service output by period.
    
	//! Population density per land area. This is currently always 1 and not
    //! read in.
	double popDensity;
	
	//! Constant calculated scaler to scale base output
    double baseScaler;

    void MCDerivedClassOutput() const;
    bool XMLDerivedClassParse( const std::string nodeName, const xercesc::DOMNode* curr );
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    const std::string& getXMLName() const;
    bool isNameOfChild  ( const std::string& nodename ) const;
    technology* createChild( const std::string& nodename ) const;
private:
    static const std::string XML_NAME; //!< XML name of this object.
};


#endif // _TRAN_SUBSECTOR_H_
