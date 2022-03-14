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
 * \file no_climate_model.cpp
 * \ingroup Objects
 * \brief Implementation for the NoClimateModel class.
 * \author Pralit Patel
 */

#include <memory>
#include <limits>
#include <fstream>

#include "climate/include/no_climate_model.h"

#include "util/logger/include/ilogger.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"

using namespace std;

NoClimateModel::NoClimateModel()
{
}

NoClimateModel::~NoClimateModel()
{
}

/*!
 * \brief Get the XML name for this class.
 * \return The static XML node name identifying this class.
 */
const string& NoClimateModel::getXMLNameStatic() {
    static string XMLNAME( "no-climate-model" );
    return XMLNAME;
}

const string& NoClimateModel::getXMLName() const{
    return getXMLNameStatic();
}

void NoClimateModel::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLName(), aOut, aTabs );
    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
} 

void NoClimateModel::completeInit( const string& aScenarioName ) {
}

bool NoClimateModel::setEmissions( const string& aGasName, const int aPeriod,
                                   double aEmissions )
{
    return true;
}

bool NoClimateModel::setLUCEmissions( const string& aGasName,
                                      const int aYear, double aEmissions )
{
    return true;
}

double NoClimateModel::getEmissions( const string& aGasName, const int aYear ) const {
    return 0.0;
}

IClimateModel::runModelStatus NoClimateModel::runModel( const int aYear ) {
    return SUCCESS;
}

IClimateModel::runModelStatus NoClimateModel::runModel() {
    return SUCCESS;
}

double NoClimateModel::getConcentration( const string& aGasName, const int aYear) const {
    return 0.0;
}

double NoClimateModel::getTemperature( const int aYear ) const {
    return 0.0;
}

double NoClimateModel::getTotalForcing( const int aYear ) const {
    return 0.0;
}

double NoClimateModel::getForcing( const string& aGas, int aYear ) const {
    return 0.0;
}

double NoClimateModel::getNetTerrestrialUptake( const int aYear ) const {
    return 0.0;
}

double NoClimateModel::getNetOceanUptake(const int aYear ) const {
    return 0.0;
}

void NoClimateModel::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitClimateModel( this, aPeriod );
    aVisitor->endVisitClimateModel( this, aPeriod );
}
