#ifndef _GDP_H_
#define _GDP_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file gdp.h
* \ingroup CIAM
* \brief The GDP class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include <vector>
#include <xercesc/dom/DOMNode.hpp>
class Population;
class Tabs;

/*! 
* \ingroup CIAM
* \brief This class defines an object which contains the GDP information for a single region
* along with function which can be used to access the GDP in various ways.
* \details This class all controls the read-in and initialization of this data along with calibration
* of GDP.
* \note This class is constructed of code that was formerly in several classes throughout the model. 
* \author Josh Lurz, Sonny Kim
*/

class GDP
{
private:
    std::vector<double> laborProdGrowthRate; //!< labor productivity growth rate
    std::vector<double> laborForceParticipationPercent; //!< labor force participation percent
    std::vector<double> laborForce; //!< actual labor force
public:
    GDP();
    ~GDP();
    void XMLParse( const xercesc::DOMNode* node );
    void toXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    void initData( const Population* regionalPop );
    void setupCalibrationMarkets( const std::string& regionName );
    void writeBackCalibratedValues( const std::string& regionName, const int period );
    double getTotalLaborProductivity( const int period ) const;
    double getLaborForce( const int per ) const;    
    double getLaborProdGR( const int per ) const;
    void outputfile( const std::string& regionName ) const;
    void MCoutput( const std::string& regionName ) const;
};

#endif // _GDP_H_

