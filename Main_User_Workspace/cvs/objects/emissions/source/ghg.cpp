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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
* \file ghg.cpp
* \ingroup Objects
* \brief Ghg class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <map>
#include <vector>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "emissions/include/ghg.h"
#include "emissions/include/indirect_emiss_coef.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/gdp.h"
#include "emissions/include/ghg_mac.h"
#include "functions/include/iinput.h"
#include "technologies/include/icapture_component.h"
#include "functions/include/function_utils.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/iinfo.h"
#include "util/logger/include/ilogger.h"
#include "technologies/include/ioutput.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string Ghg::XML_NAME = "GHG";

Ghg::Ghg(){
    gwp = 1;
    emissCoef = 0;
    emissInd = 0;
    inputEmissions = -1;
    valueWasInputAtSomePoint = false;
    emAdjust = 0;
    gdpCap = 0;
    maxCntrl = -1000;
    techDiff = 0;
    gdpcap0 = 0;
    finalEmissCoef = -1;
    tau = 0;
    // this is inefficient as it is greater than the lifetime
    // but much simpler than converting period to liftime period 
    // TODO: Fix this so it has one spot per active period.
    mEmissions.resize( scenario->getModeltime()->getmaxper() );
    mEmissionsByFuel.resize( scenario->getModeltime()->getmaxper() );
    adjMaxCntrl = 1;
    multMaxCntrl = 1;
}

//! Destructor
Ghg::~Ghg(){
}

//! Copy constructor.
Ghg::Ghg( const Ghg& other ){
    copy( other );
}

//! Assignment operator.
Ghg& Ghg::operator=( const Ghg& other ){
    if( this != &other ){
        // If there was a destructor it would need to be called here.
        copy( other );
    }
    return *this;
}

//! Copy helper function.
void Ghg::copy( const Ghg& other ){
    name = other.name;
    gwp = other.gwp;
    emissCoef = other.emissCoef;
    emissInd = other.emissInd;
    inputEmissions = other.inputEmissions;
    valueWasInputAtSomePoint = other.valueWasInputAtSomePoint;
    emAdjust = other.emAdjust;
    maxCntrl = other.maxCntrl;
    techDiff = other.techDiff;
    gdpcap0 = other.gdpcap0;
    adjMaxCntrl = other.adjMaxCntrl;
    multMaxCntrl = other.multMaxCntrl;
    finalEmissCoef = other.finalEmissCoef;
    tau = other.tau;
    mEmissions.resize( scenario->getModeltime()->getmaxper() );
    mEmissionsByFuel.resize( scenario->getModeltime()->getmaxper() );
    // Perform a deep copy on the GhgMac.
    if( ghgMac.get() ){
        ghgMac.reset( other.ghgMac->clone() );
    }
}

//! Clone function which returns a deep copy of the Ghg.
Ghg* Ghg::clone() const {
    return new Ghg( *this );
}

/*!
 * \brief Get the name of the GHG.
 * \return Name of the GHG.
 */
const string& Ghg::getName() const {
    return name;
}

/*! \brief initialize Ghg object with xml data
*
*/
void Ghg::XMLParse(const DOMNode* node) {   
    /*! \pre Assume we are passed a valid node. */
    assert( node );

    // get the name attribute.
    name = XMLHelper<string>::getAttr( node, "name" );
    DOMNodeList* nodeList = node->getChildNodes();

    for( unsigned int i = 0; i < nodeList->getLength(); ++i ) {
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );      

        if( nodeName == "#text" ){
            continue;
        }
        else if( nodeName == "inputEmissions" ){
            inputEmissions = XMLHelper<double>::getValue( curr );
            emissCoef = 0;
        }
        else if( nodeName == "emAdjust" ){
            emAdjust = XMLHelper<double>::getValue( curr );
        }
        else if( ( nodeName == "maxCntrl" ) ){
            maxCntrl = XMLHelper<double>::getValue( curr );
         }
        else if( ( nodeName == "gdpcap0" ) ){
            gdpcap0 = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "tau" ){
            tau = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "finalEmissCoef" ){
            finalEmissCoef = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "techDiff" ){
            techDiff = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "emisscoef" ){
            emissCoef = XMLHelper<double>::getValue( curr );
            inputEmissions = -1; // Reset inputEmissions to default value
        }
        // Adjust max Control level, leaving current emissions constant
        else if( nodeName == "adjMaxCntrl" ){
            adjMaxCntrl = XMLHelper<double>::getValue( curr );
        }
        // Multiply maximum control level, changing current emissions
        else if( nodeName == "multMaxCntrl" ){
            multMaxCntrl = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "GWP" ){
            gwp = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == GhgMAC::getXMLNameStatic() ){
            // Delete the MAC if requested.
            if( XMLHelper<int>::getAttr( curr, "delete" ) ){
                // Check if the curve exists.
                if( ghgMac.get() ){
                    ILogger& mainLog = ILogger::getLogger( "main_log" );
                    mainLog.setLevel( ILogger::DEBUG);
                    mainLog << "Deleting GHG MAC from GHG " << name << endl;
                    ghgMac.reset( 0 );
                }
            }
            // Create and parse the MAC.
            else {
                if( !ghgMac.get() ){
                    ghgMac.reset( new GhgMAC() );
                }
                ghgMac->XMLParse( curr );
            }
        }
        else if( XMLDerivedClassParse( nodeName, curr ) ){
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing GHG." << endl;
        }
    }
}
/*! \brief Parses any child nodes specific to derived classes
*
* Method parses any input data from child nodes that are specific to the classes derived from this class. Since Sector is the generic base class, there are no values here.
*
* \author Josh Lurz, Steve Smith
* \param nodeName name of current node
* \param curr pointer to the current node in the XML input tree
* \return Whether any node was parsed.
*/
bool Ghg::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    return false;
}

//! Writes datamembers to datastream in XML format.
void Ghg::toInputXML( ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag( getXMLName(), out, tabs, name );

    // write xml for data members
    XMLWriteElementCheckDefault( inputEmissions, "inputEmissions", out, tabs, -1.0 );
    XMLWriteElementCheckDefault( emissCoef, "emisscoef", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( gwp, "GWP", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( emAdjust, "emAdjust", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( maxCntrl, "maxCntrl", out, tabs, -1000.0 );
    XMLWriteElementCheckDefault( gdpcap0, "gdpcap0", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( tau, "tau", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( finalEmissCoef, "finalEmissCoef", out, tabs, -1.0 );
    XMLWriteElementCheckDefault( adjMaxCntrl, "adjMaxCntrl", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( multMaxCntrl, "multMaxCntrl", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( techDiff, "techDiff", out, tabs, 0.0 );

    // Write out the GHGMAC
    if( ghgMac.get() ){
        ghgMac->toInputXML( out, tabs );
    }
    toInputXMLDerived( out, tabs );
    // done writing xml for data members.

    XMLWriteClosingTag( getXMLName(), out, tabs );
}


//! Write out any inherited class specific datamembers. Since GHG is not an ABC, it must define this as a noop.
void Ghg::toInputXMLDerived( std::ostream& out, Tabs* tabs ) const {
}

//! Writes datamembers to debugging datastream in XML format.
void Ghg::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag( getXMLName(), out, tabs, name );

    // write xml for data members
    XMLWriteElement( gwp, "GWP", out, tabs );
    XMLWriteElement( mEmissions[ period ], "emission", out, tabs );
    XMLWriteElement( emissCoef, "emisscoef", out, tabs );
    XMLWriteElement( inputEmissions, "inputEmissions", out, tabs );
    XMLWriteElement( mEmissionsByFuel[ period ], "emissFuel", out, tabs );
    XMLWriteElement( emissInd, "emissInd", out, tabs );
    XMLWriteElement( emAdjust, "emAdjust", out, tabs );
    XMLWriteElement( maxCntrl, "maxCntrl", out, tabs );
    XMLWriteElement( gdpcap0, "gdpcap0", out, tabs );
    XMLWriteElement( tau, "tau", out, tabs );
    XMLWriteElement( finalEmissCoef, "finalEmissCoef", out, tabs );
    XMLWriteElement( adjMaxCntrl, "adjMaxCntrl", out, tabs );
    XMLWriteElement( techDiff, "techDiff", out, tabs );

     // Write out the GHGMAC
    if( ghgMac.get() ){
        ghgMac->toDebugXML( period, out, tabs );
    }
    toDebugXMLDerived( period, out, tabs );
    // done writing xml for data members.

    XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Write out any inherited class specific datamembers. Since GHG is not an ABC, it must define this as a noop.
void Ghg::toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {
}

/*! \brief Get the XML node name for output to XML.
*
* This protected function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& Ghg::getXMLName() const {
    return XML_NAME;
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& Ghg::getXMLNameStatic() {
    return XML_NAME;
}

/*! \brief Copies parameters such as Tau, GDP0, and MAC curve that should only be specified once
* \detailed Certain parameters for GHG emissions should only be specified once so that they are
* consistent for all years (and also to simplify input). Given that GHG objects are embedded in 
* technology objects this means that these parameters need to be copied from object to object.
* This method copies any needed parameters from the previous year's GHG object.
* Also included in this function is code for the variable adjMaxCntrl.  The code for this varible
* needs to be run only once, with the values at the end of the period, so it is useful to have it here
* where those values are defined.  adjMaxCntrl has a default of 1, so if it is not input, maxCntrl will simply
* be multiplied by 1, and the function for adjusting gdpcap0 will simplify to gdpcap0 = gdpcap0, thus keeping it
* at the same value.  If adjMaxCntrl != 1, it will adjust gdpcap0 up or down so that the base year emissions
* remain unchanged.  adjMaxCntrl should be input once, in the base year. 
*
* \author Steve Smith and Nick Fernandez
* \param prevGHG pointer to previous period's GHG object
*/
void Ghg::copyGHGParameters( const Ghg* prevGHG ) {

    assert( prevGHG ); // Make sure valid pointer was passed
   
   // Copy values that always need to be the same for all periods.
    // Note that finalEmissCoef is not copied, since maxCntrl has already been set appropriately
    maxCntrl = prevGHG->maxCntrl;
    gdpcap0 = prevGHG->gdpcap0;
    tau = prevGHG->tau;
    
    adjMaxCntrl = prevGHG->adjMaxCntrl;

    // Adjust for maximum control level once GDP per capita is determined
    // This could better be put into a post-calculation processing function if we implemented that in general
    adjustMaxCntrl( prevGHG->gdpCap );
    

    // Copy values that could change, so only copy if these are still zero (and, thus, were never read-in)
   if ( !techDiff ) { 
        techDiff = prevGHG->techDiff; // only copy if techDiff has not changed
    }
 
   if ( !gwp ) { 
        gwp = prevGHG->gwp; // only copy if GWP has not changed
    }
    
    // Default value for emissCoef is zero, so only copy if a new value was read in
    if ( !emissCoef ) {
        emissCoef = prevGHG->emissCoef;
    }

    // If an emissions value was input last period, and none was input this period, then copy emissions coefficient
    // This overwrites anything that was read in
    if (  !valueWasInputAtSomePoint && prevGHG->valueWasInputAtSomePoint ) {
        emissCoef = prevGHG->emissCoef;
    }

    // If Mac curve was input then copy it, as long as one was not read in for this period
    if ( !ghgMac.get() && prevGHG->ghgMac.get() ) {
        ghgMac.reset( prevGHG->ghgMac->clone() );
    }
}

//! Perform initializations that only need to be done once per period
void Ghg::initCalc( const IInfo* aLocalInfo,
                    const int aPeriod )
{

    maxCntrl *= multMaxCntrl;
    // Make sure control percentage never goes above 100% so there are no negative emissions!
    maxCntrl = min( maxCntrl, 100.0 );
    
    // Set flag that an emission input value was set
    if( inputEmissions >= 0 ) {
        valueWasInputAtSomePoint = true;
    }

     // Perform any MAC initializations
    if( ghgMac.get() ){
        ghgMac->initCalc( name );
    }
}

/*! \brief Second Method: Convert GHG tax and any storage costs into energy
*          units using GHG coefficients and return the value or cost of the tax
*          and storage for the GHG.
* \details Apply taxes only if emissions occur. Emissions occur if there is a
*            difference in the emissions coefficients.
* \author Sonny Kim
* \param aInput Vector of inputs to the technology.
* \param aRegionName Name of the region for GHG
* \param aOutputs Vector of Technology outputs.
* \param aEfficiency The efficience of the technology this ghg emitted by.

* \param aPeriod The period in which this calculation is occurring. 
* \return Generalized cost or value of the GHG
*/
double Ghg::getGHGValue( const string& aRegionName,
                         const vector<IInput*>& aInputs,
                         const vector<IOutput*>& aOutputs,
						 const ICaptureComponent* aSequestrationDevice,
                         const int aPeriod ) const
{
    // Constants
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    // Conversion from teragrams of carbon per EJ to metric tons of carbon per GJ
    const double CVRT_TG_MT = 1e-3; 

    // Get carbon storage cost from the sequestrion device if there is one.
    double storageCost = aSequestrationDevice ? aSequestrationDevice->getStorageCost( aRegionName,
                                                                                      name,
                                                                                      aPeriod )
                                              : 0;

    // Get the remove fraction from the sequestration device. The remove
    // fraction is zero if there is no sequestration device.
    double removeFraction = aSequestrationDevice ? aSequestrationDevice->getRemoveFraction( name ) : 0;

    // Get the greenhouse gas tax from the marketplace.
    const Marketplace* marketplace = scenario->getMarketplace();
    double GHGTax = marketplace->getPrice( name, aRegionName, aPeriod, false );
    if( GHGTax == Marketplace::NO_MARKET_PRICE ){
        GHGTax = 0;
    }

    // Calculate the generalized emissions cost per unit.
    double generalizedCost = 0;
    if ( name == "CO2" ) {
        double coefProduct = calcOutputCoef( aOutputs, aPeriod );
        double coefInput = calcInputCoef( aInputs, aPeriod );

		// units for generalized cost is in 75$/gj
		generalizedCost = ( ( 1 - removeFraction ) * GHGTax * gwp + removeFraction * storageCost )
                              * ( coefInput - coefProduct) / CVRT90 * CVRT_TG_MT;
	}
    // for all other gases used read-in emissions coefficient
    else {
        generalizedCost = ( ( 1 - removeFraction ) * GHGTax * gwp + removeFraction * storageCost) 
            * emissCoef / CVRT90;
    }

    // The generalized cost returned by the GHG may be negative if
    // emissions crediting is occuring.
    return generalizedCost;
}

/*! \brief finds an appropriate value for maxCntrl and adjusts gdpcap0 as necessary
* The control function is needed in the calcEmission function and takes 4 parameters, maxCntrl, tau, gdpcap0, and gdpCap.
* tau and gdpcap0 are necessary inputs to the control function, maxCntrl can either be inputed directly, or
* can be computed in this function using the input "finalEmissCoef."
* if either tau or gdpcap0 are not input, or are 0, or if the emissions coefficient is 0, the function will set
* fControl to 0, and hence, there will be no emissions adjustment due to controls. In the case that both maxCntrl and
* finalEmissCoef are input (which does not make sense)  only finalEmissCoef will be used.
* The function additionally calls calcTechChange which reduces gdpcap0 over time to account for technological change/diffusion.
* \author Nick Fernandez & Steve Smith
* \param gdpCap - The gdp per capita for the current period 
* \param emissDrive The amount of fuel that emissions are proportional to
* \param period the current period where calculations are occurring
* \return adjustedGdpCap0
* \todo let initCalc know the period so that calcTechChange calculation can be moved there and will only have to be done once per period
*/
double Ghg::adjustControlParameters( const double gdpCap, const double emissDrive, const double macReduction, const int period ){
    double adjustedGdpCap0 = gdpcap0; //! gdpCap0 used by the control function- adjusted for techDiffusion

    if (techDiff !=0){
        adjustedGdpCap0 = gdpcap0 / calcTechChange(period);
    }
    if ( finalEmissCoef > 0 ){
        if ( inputEmissions >= 0 ){
            const double multiplier = emissDrive * ( 1 - emAdjust ) * ( 1 - macReduction );

            const double B = (1/controlFunction(100,tau,adjustedGdpCap0,gdpCap));
            if ( multiplier != 0){
                //cannot divide by zero- when no driver is present
                maxCntrl = 100 * (1 - (finalEmissCoef * ((B - 1)) / (((B * inputEmissions) / multiplier ) - finalEmissCoef)));
            }
            else{
                maxCntrl = 0;
            }
            // method for calculating an maxCntrl when using emissions coefficients that require maxCntrl in their
            // calculation.  The formula is derived by setting up equations where maxCntrl can be eliminated through 
            // substitution, the emissions coefficient can be solved for, and that expression can be substituted back in 
            // into the expression for maxCntrl. See formula for emission in calcEmission()
        }
        else{
            if (emissCoef != 0){
                // cannot divide by 0 

                maxCntrl = 100 * (1 - ( finalEmissCoef / (emissCoef)));

            }
            else {
                maxCntrl = 0;
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::WARNING );
                mainLog << " emissCoef = 0, control function set to 0"<< endl;

            }
        }
        // Control values should never be larger than 100%.
        maxCntrl = min( maxCntrl, 100.0 );
    }
    return adjustedGdpCap0;
}

/*! \brief Returns the value by which to adjust gdpcap0, based on technological diffusion
* The Variable TechCh represents the percent reduction in gdpcap0 per year, due to technological change and diffusion.
* The overall reduction in gdpcap0 is 1 + the techCh percentage raised to the power of the number of years after 
* the base year.  When applied to the control function, this will allow emissions controls to approach maxCntrl sooner.
*\ Author Nick Fernandez
* \param period the current period where calculations occur
* \returns amount to reduce the parameter gdpCap0 by
*/
double Ghg::calcTechChange( const int period ){
    const Modeltime* modeltime = scenario->getModeltime();
    int year = modeltime->getper_to_yr( period ); 
    year -=  modeltime->getper_to_yr(1); // subtracts off base year to find number of years after base year
    return pow(1 + (techDiff / 100), year );
}

/*!
 * \brief Calculate the aggregate output emissions coefficient for the gas.
 * \details The output coefficient is the sum of all output coefficients of all
 *          the outputs.
 * \param aOutputs Vector of Technology outputs.
 * \param aPeriod Period.
 * \return Aggregate output coefficient.
 */
double Ghg::calcOutputCoef( const vector<IOutput*>& aOutputs, const int aPeriod ) const {
    // The output coefficient is the sum of the output coefficients of all outputs.
    double outputCoef = 0;
    for( unsigned int i = 0; i < aOutputs.size(); ++i ){
        outputCoef += aOutputs[ i ]->getEmissionsPerOutput( name, aPeriod );
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
double Ghg::calcInputCoef( const vector<IInput*>& aInputs, const int aPeriod ) const {
    // Calculate an aggregate coefficient.
    double coefFuel = 0;
    for( unsigned int i = 0; i < aInputs.size(); ++i ){
        // Input coefficients must be greater than zero if they contribute
        // to the aggregate emissions.
        if( aInputs[ i ]->getCoefficient( aPeriod ) > 0 ){
            coefFuel += aInputs[ i ]->getEmissionsCoefficient( name, aPeriod )
                       * aInputs[ i ]->getCoefficient( aPeriod );
        }
    }
    return coefFuel;
}

/*!
 * \brief Calculate the sum of all emissions contained in all outputs.
 * \details Determines the emissions in each output by multiplying the output's
 *          coefficient by its physical output. These emissions are then summed.
 * \param aOutputs Vector of technology outputs.
 * \param aPeriod Period.
 * \return Sum of emissions in all outputs
 */
double Ghg::calcOutputEmissions( const vector<IOutput*>& aOutputs,
                                 const int aPeriod ) const
{
    double emissions = 0;
    for( unsigned int i = 0; i < aOutputs.size(); ++i ){
        emissions += aOutputs[ i ]->getEmissionsPerOutput( name, aPeriod )
                     * aOutputs[ i ]->getPhysicalOutput( aPeriod );
    }
    return emissions;
}

/*! Second Method: Convert GHG tax and any storage costs into energy units using
*   GHG coefficients and return the value or cost of the tax and storage for the
*   GHG. Apply taxes only if emissions occur. Emissions occur if there is a
*   difference in the emissions coefficients.
*  \param aInput Input for which to calculate the carbon tax.
*  \param aRegionName The name of the current region.
*  \param aGoodName The name of the output product.
* \param aSequestrationDevice The technologies optional sequestration device.
*  \param aPeriod The period in which this calculation is occurring. 
*  \return Generalized cost or value of the GHG
*  \todo Sequestration and collapsing two methods.
*/
double Ghg::getGHGValue( const IInput* aInput, const string& aRegionName, const string& aGoodName,
						 const ICaptureComponent* aSequestrationDevice, const int aPeriod ) const
{
    // Determine if there is a tax.
    const Marketplace* marketplace = scenario->getMarketplace();
    double ghgTax = marketplace->getPrice( name, aRegionName, aPeriod, false );
    if( ghgTax == Marketplace::NO_MARKET_PRICE ){
        ghgTax = 0;
    }
     // Get carbon storage cost from the sequestrion device if there is one.
	double storageCost = aSequestrationDevice ? aSequestrationDevice->getStorageCost( name, aRegionName, aPeriod ) : 0;
	// Get the remove fraction from the sequestration device. The remove
	// fraction is zero if there is no sequestration device.
	double removeFraction = aSequestrationDevice ? aSequestrationDevice->getRemoveFraction( name ) : 0;

    // Return the rate.
    return ( ( 1 - removeFraction ) * ghgTax * gwp + removeFraction * storageCost ) 
		    * aInput->getEmissionsCoefficient( aRegionName, aPeriod ) * aInput->getConversionFactor( aPeriod );
}

/*! \brief Calculate the input emissions for a good.
* \details Calculates the sum of all emissions contained in the inputs to the production of a good. This is calculated
* by looping over all the inputs and for each input, determining its carbon by multiplying its coefficient and its
* physical demand. This amount of carbon is then added to the total, which is returned by the function. This carbon
* may not all be emitted, as a portion may remain in the output good.
* \param aInputs Vector of inputs to determine the amount of carbon in.
* \param aRegionName Name of the region in which the emission is occurring.
* \param aPeriod Period in which the emission is occurring. 
*/
double Ghg::calcInputEmissions( const vector<IInput*>& aInputs, const string& aRegionName, const int aPeriod ) const {
    double totalEmissions = 0;
    // Loop over the inputs calculating the amount of carbon in each.
    for( vector<IInput*>::const_iterator input = aInputs.begin(); input != aInputs.end(); ++input ){
        // Add on the physical amount of the input multiplied by the amount of
        // emissions per unit of physical output.
        totalEmissions += (*input)->getPhysicalDemand( aPeriod )
                           * (*input)->getEmissionsCoefficient( name, aPeriod );
        
    }
    return totalEmissions;
}

/*! \brief Calculates emissions of GHG's that use input-output as the emissions
*          Driver
* \details Emissions of these gases are equal to the emissions driver multiplied
*          by the emissions coefficient (how much of the chemical forming the
*          GHG is present in the fuel) multiplied by the control function (the
*          extent to which regions are expected to put controls on end-of-pipe
*          emissions- based on their pppGdp) multiplied by the result of the
*          Marginal Abatement curve, and finally by an external read-in
*          emissions Adjustment factor(if any). The function also sets the
*          emissions coefficient if emissions are read in.
* \details Performs an activity based calculation of the emissions produced by
*          the technology. The calculation is performed by summing the total
*          carbon contained in the inputs to the good and the substracting the
*          carbon contained in the physical output. The carbon contained in the
*          output is not removed for primary fuel sectors, as their inputs do
*          not account for the carbon extracted in the fuel from the ground.
*          This function also stores the emissions of the primary fuel sectors
*          seperatelyso they can be reported later for emissions by fuel. The
*          emission is then converted to a global-warming-potential based
*          emission and added to the constraint market.
* \author Nick Fernandez, Steve Smith, Josh Lurz
* \param aInputs Vector of inputs to the technology.
* \param aRegionName Name of the region where the emission will occur.
* \param aGoodName Name of the sector creating the emission.
* \param aOutput Physical quantity of output.
* \param aPeriod Period in which the emissions is occurring.
* \note aOutput is in physical units, not currency units.
* \todo Emissions by fuel are horked.
* \todo PRIORITY - separate out CO2 from non-CO2 GHGs since CO2 is much simpler.
* \todo Emissions calc will not work properly with vintaging (base-year emissions will not work, and some thought needs to be given to how emissions controls should work)
*/
void Ghg::calcEmission( const string& aRegionName,
                        const vector<IInput*>& aInputs,
                        const vector<IOutput*>& aOutputs,
                        const GDP* aGDP,
                        ICaptureComponent* aSequestrationDevice,
                        const int aPeriod )
{
	// for CO2 use default emissions coefficient by fuel
	// remove fraction only applicable for CO2
    Marketplace* marketplace = scenario->getMarketplace();
	if( name == "CO2" ) {
		// Calculate the aggregate emissions of all inputs.
		double tempEmission = calcInputEmissions( aInputs, aRegionName, aPeriod );

		// calculate the output emissions.
		const double outputEmissions = calcOutputEmissions( aOutputs, aPeriod );

    	// Calculate sequestered emissions if there is a sequestration device.
		if( aSequestrationDevice ){
			tempEmission -= aSequestrationDevice->calcSequesteredAmount( aRegionName, name,
                                                                         tempEmission, aPeriod );
		}

		// If the good is a primary fuel, don't subtract output emissions as this is
		// extraction of the resource, not sequestration, and store the output
		// emissions as emissions by primary fuel. This should probably be stored by
		// the GHG.
		// This is wrong if capture occurs down the line.
        // TODO: Store this property in the output object.
        const IInfo* marketInfo = marketplace->getMarketInfo( aOutputs[ 0 ]->getName(),
                                                                   aRegionName, aPeriod, false );
        if( marketInfo && marketInfo->getBoolean( "IsPrimaryEnergyGood", false ) ){
			mEmissionsByFuel[ aPeriod ] = outputEmissions;
		}
		else {
			// Remove emissions contained in the output from the total technology emissions.
			tempEmission -= outputEmissions;
		}

		// Calculate emissions for the constraint market based on the global warming potential of the gas.
		// CO2 is 1.
		double emissGwp = gwp * tempEmission;

		// Store the total emissions.
		mEmissions[ aPeriod ] = emissGwp;
	}
  // for all other gases used read-in emissions coefficient or base-year emissions
    else {
        double macReduction = 0;
        gdpCap = aGDP->getPPPGDPperCap( aPeriod );
        double primaryOutput = aOutputs[ 0 ]->getPhysicalOutput( aPeriod );
        const double emissDriver = emissionsDriver( calcInputEmissions( aInputs, aRegionName, aPeriod ), primaryOutput );
        if ( ghgMac.get() ){
            macReduction = ghgMac->findReduction( aRegionName, aPeriod);
        }

        double fControl = 0;    
        double adjustedGdpCap0 = adjustControlParameters( gdpCap, emissDriver, macReduction, aPeriod );
        if ( ( finalEmissCoef > 0 ) || ( maxCntrl > -999 ) ){
            fControl = controlFunction( maxCntrl, tau, adjustedGdpCap0, gdpCap );
        }

        if ( inputEmissions >= 0 ) {
            mEmissions[ aPeriod ] = inputEmissions;
            mEmissionsByFuel[ aPeriod ] = inputEmissions;
            if ( (emissDriver != 0) && (emAdjust != 1) && (fControl != 1) && (macReduction != 1)) {
                emissCoef = inputEmissions / (emissDriver * (1 - emAdjust) * (1 - fControl)* ( 1 - macReduction ) );
            } else {
                emissCoef = 0;
            }
        } else {
            mEmissions[ aPeriod ] = emissDriver * emissCoef * ( 1 - emAdjust )* ( 1 - fControl ) * ( 1 - macReduction ) ;
            mEmissionsByFuel[ aPeriod ] =  mEmissions[ aPeriod ];
        }
    }

    // set emissions as demand side of gas market
    // Optimize special case of no-emission ghg.
    if( mEmissions[ aPeriod ] != 0 ){
        marketplace->addToDemand( name, aRegionName, mEmissions[ aPeriod ], aPeriod, false );
    }
}

//! Return Ghg emissions.
double Ghg::getEmission( const int aPeriod ) const {
    assert( aPeriod < static_cast<int>( mEmissions.size() ) );
    return mEmissions[ aPeriod ];
}

//! Return ghg emissions inplicit in fuel.
double Ghg::getEmissFuel( const int aPeriod ) const {
    return mEmissionsByFuel[ aPeriod ];
}

//! Return ghg emissions coefficient.
double Ghg::getEmissCoef() const {
    return emissCoef;
}

/*! \brief Returns the control level for this gas in the current period
* \detailed The control function is a logistic exponential function.  It approaches 0 for values of gdpCap much less
* than gdpcap0, and approaches maxCntrl for values of gdpCap much greater than gdpcap0. CLOGIT is a constant equal
* to 2 times the natural log of 9, such that fControl is equal to 1/2 maxCntrl at gdpcap0.  
* the function returns the value of 0 in the case that either gdpcap0 or tau are not input, or are equal to 0.
* \author Nick Fernandez
* \param maxCntrlIn maximum emissions reduction fraction due to controls
* \param tauIn the range over which the control percentage goes from 10% maxCntrl to 90% maxCntrl
* \param gdpcap0In the midpoint gdpCap value of the control curve
* \param gdpCapIn the gdp per capita in PPP terms for the current period
*/
double Ghg::controlFunction( const double maxCntrlIn, const double tauIn, const double gdpcap0In, const double gdpCapIn ){
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    if( tauIn != 0 && gdpcap0In != 0 ){
        const double CLOGIT = 4.394; // See above for documentation.
        return (maxCntrlIn/100) / (1 + exp( -CLOGIT * (gdpCapIn - gdpcap0In) / tauIn ));
    }
    else {
        if ( tauIn == 0 ){
            mainLog.setLevel( ILogger::WARNING );
            mainLog << " control function requires an input of tau." << endl;
        }
        if ( gdpcap0In == 0 ){
            mainLog.setLevel( ILogger::WARNING );
            mainLog << " control function requires an input of gdpcap0." << endl;
        }
        return 0;
    }
}

//! returns the emissions Driver value. emissions are proportional to input minus output.
double Ghg::emissionsDriver( const double inputIn, const double outputIn ) const {
    return inputIn - outputIn;
}

/*! \brief Get the carbon tax paid for the ghg.
* \details Calculate and return the total amount of carbon tax paid, or credit received
* for a single greenhouse gas. The tax or credit is calculated as emissions multiplied by
* the tax from the marketplace for the gas and the global warming potential.
* \warning This function calculates this value dynamically which requires a call to the marketplace,
* so it is slow. It should be avoided except for reporting purposes.
* \param aRegionName The name of the region containing the ghg.
* \param aPeriod The name of the period for which to calculate carbon tax paid.
* \author Josh Lurz
* \return The total carbon tax paid.
*/
double Ghg::getCarbonTaxPaid( const string& aRegionName, const int aPeriod ) const {
    const Marketplace* marketplace = scenario->getMarketplace();
    double GHGTax = marketplace->getPrice( name, aRegionName, aPeriod, false );
    if( GHGTax == Marketplace::NO_MARKET_PRICE ){
        GHGTax = 0;
    }
    // The carbon tax paid is the amount of the emission multiplied by the tax and the global
    // warming emission. This may be a negative in the case of a credit.
    return GHGTax * mEmissions[ aPeriod ] * gwp;
}

/*! \brief adjusts maxCntrl (and then gdpcap0 to recalibrate emissions) based on the read in multiplier adjMaxCntrl
*\ detailed adjMaxCntrl is a read in variable that represents a multiplier to maxCntrl.
* This is used to adjust the maximum emissions control level while leaving current emissions constant.
* the function multiplies maxCntrl by this value, and chacks to make sure maxCntrl has not been
* given a multiplier that makes it greater that 100.  If this happens, it sets maxCntrl to 100
* and resets adjMaxCntrl to the value necessary to make maxCntrl 100.
* It then solves the control function for gdpcap0, keeping the base year emissions the same,
* so that changing maxCntrl does not mess with the base year calibrations.
* Note also that adjustMaxCntrl is run only once, in the base year when and if adjMaxCntrl != 1
* \author Nick Fernandez
* \param GDPcap the previous periods GDP per capita in PPP terms for this region
*/
void Ghg::adjustMaxCntrl( const double GDPcap ){
    if ( tau != 0 && gdpcap0 != 0 && adjMaxCntrl != 1 ) {
        // Note that maxCntrl is in percentage units
        maxCntrl *= adjMaxCntrl;
        if ( maxCntrl > 100 ) {
            adjMaxCntrl *= ( 100 / maxCntrl );
            maxCntrl = 100;
        }

        const double CLOGIT = 4.394;
        double factor1 =  1 + exp( -CLOGIT * ( GDPcap - gdpcap0 ) / tau );
        
        if ( adjMaxCntrl != 1 ){
            gdpcap0 = ( tau / CLOGIT ) * log( adjMaxCntrl * factor1 - 1 ) + GDPcap;
        }
        // After finished adjustments, adjMaxCntrl should be set to one so is not used anymore
        adjMaxCntrl = 1;
    }
}

/*! \brief Set the name of the GHG.
* \details Set the name of a GHG. This should only be used when a GHG is created
*          directly instead of initialized from XML.
* \param aName New name of the GHG.
*/
void Ghg::setName( const string& aName ){
    name = aName;
}

/*! \brief Update a visitor with information from a GHG for a given period.
* \param aVisitor The visitor to update.
* \param aPeriod The period for which to update.
*/
void Ghg::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitGHG( this, aPeriod );
    aVisitor->endVisitGHG( this, aPeriod );
}
