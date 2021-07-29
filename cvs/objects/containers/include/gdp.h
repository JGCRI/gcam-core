#ifndef _GDP_H_
#define _GDP_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/



/*! 
* \file gdp.h
* \ingroup Objects
* \brief The GDP class header file.
* \author Josh Lurz
*/
#include <vector>
#include <boost/core/noncopyable.hpp>

#include "util/base/include/ivisitable.h"
#include "util/base/include/value.h"
#include "util/base/include/data_definition_util.h"

class Demographic;
class Tabs;

/*! 
* \ingroup Objects
* \brief This class defines an object which contains the GDP information and calculations for a single region
* along with function which can be used to access the GDP in various ways.
* \details This class all controls the read-in and initialization of this data along with calibration
* of GDP. The class contains code to check if adjusted GDP values exist when they are requested and 
* prints an error if this is not the case.
*
* \note This class is constructed of code that was formerly in several classes throughout the model. 
* \author Josh Lurz, Sonny Kim, Steve Smith
*/

class GDP: public IVisitable, private boost::noncopyable
{
    friend class XMLDBOutputter;
public:
    GDP();
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    void initData( const Demographic* regionalPop );
    void initialGDPcalc( const int period, const double population);
    static const std::string& getXMLNameStatic();
    void setupCalibrationMarkets( const std::string& regionName, const std::vector<double> aCalibrationGDPs  );
    void writeBackCalibratedValues( const std::string& regionName, const int period );
    void adjustGDP( const int period, const double priceratio );
    double getApproxGDPperCap( const int period ) const;
    double getApproxScaledGDPperCap( const int period ) const;
    double getApproxScaledGDP( const int period ) const;
    double getScaledGDPperCap( const int period ) const;
    double getGDPperCap( const int period ) const;
    double getPPPGDPperCap( const int period ) const;
    double getGDP( const int period ) const;
    double getApproxGDP( const int period ) const;
    double getBestScaledGDPperCap( const int period ) const;
    double getGDPNotAdjusted( const int period ) const;
    double getGDPPerCapitaNotAdjusted( const int period ) const;
    double getApproxPPPperCap( const int period ) const;
    void accept( IVisitor* aVisitor, const int aPeriod ) const;
    
protected:
    
    DEFINE_DATA(
        /*! \brief GDP is the only member of this container hierarchy. */
        DEFINE_SUBCLASS_FAMILY( GDP ),
        
        /*! \brief labor productivity growth rate. */
        DEFINE_VARIABLE( ARRAY, "laborproductivity", laborProdGrowthRate, std::vector<Value> ),
                
        /*! \brief labor force participation percent. */
        DEFINE_VARIABLE( ARRAY, "laborforce", laborForceParticipationPercent, std::vector<Value> ),
        
        /*! \brief actual labor force. */
        DEFINE_VARIABLE( ARRAY, "total-laborForce", laborForce, std::vector<double> ),
        
        /*! \brief approximate regional gross domestic product in constant dollars, not adjusted for energy price for this period. */
        DEFINE_VARIABLE( ARRAY, "gdpValue", gdpValue, std::vector<double> ),
        
        /*! \brief regional gross domestic product per capita in constant dollars ($). */
        DEFINE_VARIABLE( ARRAY, "gdpPerCapita", gdpPerCapita, std::vector<double> ),
        
        /*! \brief regional gross domestic product adjusted for energy price feedback. */
        DEFINE_VARIABLE( ARRAY, "gdpValueAdjusted", gdpValueAdjusted, std::vector<double> ),
        
        /*! \brief regional gross domestic product without any adjustments for energy price feedback in any period. */
        DEFINE_VARIABLE( ARRAY, "gdpValueNotAdjusted", gdpValueNotAdjusted, std::vector<double> ),
        
        /*! \brief regional GDP per cap without any adjustments for energy price feedback in any period. */
        DEFINE_VARIABLE( ARRAY, "gdpPerCapitaNotAdjusted", gdpPerCapitaNotAdjusted, std::vector<double> ),
        
        /*! \brief regional adjusted GDP in PPP terms. */
        DEFINE_VARIABLE( ARRAY, "gdpValueAdjustedPPP", gdpValueAdjustedPPP, std::vector<double> ),
        
        /*! \brief regional gross domestic product per capita in constant dollars ($). */
        DEFINE_VARIABLE( ARRAY, "gdpPerCapitaAdjusted", gdpPerCapitaAdjusted, std::vector<double> ),
        
        /*! \brief regional gross domestic product per capita in constant dollars ($). */
        DEFINE_VARIABLE( ARRAY, "gdpPerCapitaAdjustedPPP", gdpPerCapitaAdjustedPPP, std::vector<double> ),
        
        /*! \brief approximate regional GDP per capita PPP terms (before energy price adjustment). */
        DEFINE_VARIABLE( ARRAY, "gdpPerCapitaApproxPPP", gdpPerCapitaApproxPPP, std::vector<double> ),
                
        /*! \brief flag to tell if GDPs have been adjusted yet. */
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "gdpAdjustedFlag", gdpAdjustedFlag, std::vector<bool> ),
        
        /*! \brief Calibration values for GDP (constant dollars). */
        DEFINE_VARIABLE( ARRAY, "calibrationGDPs", calibrationGDPs, std::vector<double> ),
                
        /*! \brief Unit for GDP. */
        DEFINE_VARIABLE( SIMPLE, "GDP-unit", mGDPUnit, std::string ),
                
        /*! \brief Base-year value (constant dollars) for regional GDP. */
        DEFINE_VARIABLE( SIMPLE, "baseGDP", baseGDP, double ),
                
        /*! \brief Energy service price feedback elasticity for GDP. */
        DEFINE_VARIABLE( SIMPLE, "e_GDP_elas", mEnergyGDPElasticity, double ),
        
        /*! \brief 1990 Ratio of PPP to Market GDP. */
        DEFINE_VARIABLE( SIMPLE, "PPPConvert", PPPConversionFact, double ),
        
        /*! \brief Internal exponent variable for PPP conversion. */
        DEFINE_VARIABLE( SIMPLE, "PPPDelta", PPPDelta, double ),
                
        /*! \brief Flag to turn on dynamic ratio of PPP to Market GDP. */
        DEFINE_VARIABLE( SIMPLE, "PPPConvertFlag", constRatio, bool )
    )

    double calculatePPPPerCap( const int period,const double marketGDPperCap ); // function to calculate PPP values
    double getPPPMERRatio( const int period, const double marketGDPperCap ); // function to calculate PPP/MER ratio
    double getTotalLaborProductivity( const int period ) const;
    double getLaborForce( const int per ) const;
    int findNextPeriodWithValue( const int aStartPeriod, const std::vector<Value>& aValueVector ) const;
 };

#endif // _GDP_H_

