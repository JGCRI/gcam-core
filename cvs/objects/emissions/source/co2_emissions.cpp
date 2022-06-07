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
 * \file co2_emissions.h
 * \ingroup Objects
 * \brief CO2Emissions class header file.
 * \author Jim Naslund
 */

#include "util/base/include/definitions.h"

#include "emissions/include/co2_emissions.h"
#include "containers/include/iinfo.h"
#include "functions/include/iinput.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/icapture_component.h"
#include "marketplace/include/cached_market.h"
#include "marketplace/include/marketplace.h"

using namespace std;

//! Default Constructor with default emissions unit and name.
CO2Emissions::CO2Emissions() {
    mName = "CO2";
    mEmissionsUnit = "MTC";
}

//! Default Destructor.
CO2Emissions::~CO2Emissions()
{
}

//! Clone operator.
CO2Emissions* CO2Emissions::clone() const {
    CO2Emissions* clone = new CO2Emissions();
    clone->copy( *this );
    return clone;
}

void CO2Emissions::copyGHGParameters( const AGHG* aPrevGHG ){
    // Nothing needs to be copied.
}

/*!
 * \brief Get the XML node name for output to XML.
 * \details This public function accesses the private constant string, XML_NAME.
 *          This way the tag is always consistent for both read-in and output and can be easily changed.
 *          This function may be virtual to be overridden by derived class pointers.
 * \author Jim Naslund
 * \return The constant XML_NAME.
 */
const string& CO2Emissions::getXMLName() const {
    return getXMLNameStatic();
}

const string& CO2Emissions::getXMLNameStatic(){
    static const string XML_NAME = "CO2";
    return XML_NAME;
}


void CO2Emissions::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
}

/*!
 * \brief Calculate the aggregate output emissions coefficient for the gas.
 * \details The output coefficient is the sum of all output coefficients of all
 *          the outputs.
 * \param aOutputs Vector of Technology outputs.
 * \param aPeriod Period.
 * \return Aggregate output coefficient.
 */
double CO2Emissions::calcOutputCoef( const vector<IOutput*>& aOutputs, const int aPeriod ) const {
    // The output coefficient is the sum of the output coefficients of all outputs.
    double outputCoef = 0;
    for( unsigned int i = 0; i < aOutputs.size(); ++i ){
        outputCoef += aOutputs[ i ]->getEmissionsPerOutput( getName(), aPeriod );
    }
    return outputCoef;
}

/*!
 * \brief Calculate the aggregate input emissions coefficient for the gas.
 * \details The input coefficient is the weighted sum of all input coefficients
 *          of all the inputs.
 * \param aOutputs Vector of Technology inputs.
 * \param aPeriod Period.
 * \return Aggregate input coefficient.
 */
double CO2Emissions::calcInputCoef( const vector<IInput*>& aInputs, const int aPeriod ) const {
    // Calculate an aggregate coefficient.
    double coefFuel = 0;
    for( unsigned int i = 0; i < aInputs.size(); ++i ){
        // Input coefficients must be greater than zero if they contribute
        // to the aggregate emissions.
        if( aInputs[ i ]->getCoefficient( aPeriod ) > 0 ){
            coefFuel += aInputs[ i ]->getCO2EmissionsCoefficient( getName(), aPeriod )
                * aInputs[ i ]->getCoefficient( aPeriod );
        }
    }
    return coefFuel;
}

double CO2Emissions::getGHGValue( const std::string& aRegionName,
                                  const std::vector<IInput*>& aInputs,
                                  const std::vector<IOutput*>& aOutputs,
                                  const ICaptureComponent* aSequestrationDevice,
                                  const int aPeriod ) const
{
    // Constants
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    // Conversion from teragrams of carbon per EJ to metric tons of carbon per GJ
    const double CVRT_TG_MT = 1e-3;

    // Get carbon storage cost from the sequestrion device if there is one.
    double storageCost = aSequestrationDevice ?
        aSequestrationDevice->getStorageCost( aRegionName, getName(), aPeriod ) : 0;

    // Get the remove fraction from the sequestration device. The remove
    // fraction is zero if there is no sequestration device.
    double removeFraction = aSequestrationDevice ? aSequestrationDevice->getRemoveFraction( getName() ) : 0;

    // Get the greenhouse gas tax from the marketplace.
    double GHGTax = mCachedMarket->getPrice( getName(), aRegionName, aPeriod, false );

    if( GHGTax == Marketplace::NO_MARKET_PRICE ){
        GHGTax = 0;
    }
    
    // get the summation of emissions coefficients from all outputs
    // SHK 3/15/07: is this correct?
    double coefProduct = calcOutputCoef( aOutputs, aPeriod );
    double coefInput = calcInputCoef( aInputs, aPeriod );
	
	// Prevent fuels with a zero input coefficient and positive output coefficient from having
	// a large negative generalizedCost. This prevents CCS in the reference case.
	if ((coefInput-coefProduct) < 0 && GHGTax == 0){
		storageCost *= -1;
	}

    // Calculate the generalized emissions cost per unit.
    // units for generalized cost is in 1975$/GJ
    double generalizedCost = ( ( 1 - removeFraction ) * GHGTax + removeFraction * storageCost )
            * ( coefInput - coefProduct) / CVRT90 * CVRT_TG_MT;

    return generalizedCost;
}

/*!
 * \brief Calculate the sum of all emissions contained in all outputs.
 * \details Determines the emissions in each output by multiplying the output's
 *          coefficient by its physical output. These emissions are then summed.
 * \param aOutputs Vector of technology outputs.
 * \param aPeriod Period.
 * \return Sum of emissions in all outputs
 */
double CO2Emissions::calcOutputEmissions( const vector<IOutput*>& aOutputs,
                                          const int aPeriod ) const
{
    double emissions = 0;
    assert(aOutputs.size() > 0);
    const double primaryOutput = aOutputs[ 0 ]->getPhysicalOutput( aPeriod );
    for( unsigned int i = 0; i < aOutputs.size(); ++i ){
        emissions += aOutputs[ i ]->getEmissionsPerOutput( getName(), aPeriod)
            * primaryOutput;
    }
    return emissions;
}

/*! \brief Calculate the input CO2 emissions for a good.
 * \details Calculates the sum of all emissions contained in the inputs to the production of a good. This is calculated
 * by looping over all the inputs and for each input, determining its carbon by multiplying its coefficient and its
 * physical demand. This amount of carbon is then added to the total, which is returned by the function. This carbon
 * may not all be emitted, as a portion may remain in the output good. This function may or may not work for non-CO2
 * gases, depending on when it is called as their emissions coefficients are not available
 * \param aInputs Vector of inputs to determine the amount of carbon in.
 * \param aRegionName Name of the region in which the emission is occurring.
 * \param aPeriod Period in which the emission is occurring.
 */
double CO2Emissions::calcInputCO2Emissions( const vector<IInput*>& aInputs, const string& aRegionName, const int aPeriod ) const {
    double totalEmissions = 0;

    // Loop over the inputs calculating the amount of carbon in each.
    typedef vector<IInput*>::const_iterator CInputIterator;
    for( CInputIterator input = aInputs.begin(); input != aInputs.end(); ++input ){
        // Add on the physical amount of the input multplied by the amount of
        // emissions per unit of physical output.
        totalEmissions += (*input)->getPhysicalDemand( aPeriod )
            * (*input)->getCO2EmissionsCoefficient( getName(), aPeriod );
    }
 
    return totalEmissions;
}

void CO2Emissions::calcEmission( const std::string& aRegionName,
                                 const std::vector<IInput*>& aInputs,
                                 const std::vector<IOutput*>& aOutputs,
                                 const GDP* aGDP,
                                 ICaptureComponent* aSequestrationDevice,
                                 const int aPeriod )
{
    // Calculate the aggregate emissions of all inputs.
    double inputEmissions = calcInputCO2Emissions( aInputs, aRegionName, aPeriod );
    double outputEmissions = calcOutputEmissions( aOutputs, aPeriod );

    /* Total emissions are the difference in carbon of the input fuel and the output
     * fuel, if any. For conversion technologies like liquefaction or gasification, the
     * total emissions is the carbon involved in the conversion process, and the carbon
     * in the produced fuel is passed through. For electricity and hydrogen production,
     * this same logic works as the carbon in the output is zero */

    double totalEmissions = inputEmissions - outputEmissions;

    // Calculate sequestered emissions if there is a sequestration device
    // and subtract from total emissions.
    if( aSequestrationDevice ){
        double emissionSequestered = aSequestrationDevice->calcSequesteredAmount(
                                               aRegionName, getName(), totalEmissions, aPeriod );

        totalEmissions -= emissionSequestered;
    }


    // Store the total emissions.
    mEmissions[ aPeriod ] = totalEmissions;

    addEmissionsToMarket( aRegionName, aPeriod );
}
