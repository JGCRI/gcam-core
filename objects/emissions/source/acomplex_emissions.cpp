/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

/*! 
 * \file acomplex_emissions.cpp
 * \ingroup Objects
 * \brief AComplexEmissions class header file.
 * \author Jim Naslund
 */

#include "util/base/include/definitions.h"

#include <cmath>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "emissions/include/acomplex_emissions.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"
#include "emissions/include/ghg_mac.h"
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "technologies/include/ioutput.h"
#include "containers/include/gdp.h"
#include "emissions/include/aemissions_coef.h"
#include "emissions/include/input_emissions_coef.h"
#include "emissions/include/read_emissions_coef.h"
#include "emissions/include/aggr_emissions_coef.h"

using namespace std;
using namespace xercesc;


extern Scenario* scenario;

//! Default constructor.
AComplexEmissions::AComplexEmissions():
AGHG(),
emAdjust( 0 ),
gdpCap( 0 ),
maxCntrl( -1000 ),
techDiff( 0 ),
gdpcap0( 0 ),
finalEmissCoef( -1 ),
tau( 0 ),
adjMaxCntrl( 1 ),
multMaxCntrl( 1 ),
gwp( 1 )
{
    // default unit for emissions
    mEmissionsUnit = "tg";
}

//! Default destructor.
AComplexEmissions::~AComplexEmissions(){
}

//! Copy constructor.
AComplexEmissions::AComplexEmissions( const AComplexEmissions& other )
: AGHG( other ){
    copy( other );
}

//! Assignment operator.
AComplexEmissions& AComplexEmissions::operator=( const AComplexEmissions& other ){
    if( this != &other ){
        // If there was a destructor it would need to be called here.
        AGHG::operator=( other );
        copy( other );
    }
    return *this;
}

//! Copy helper function.
void AComplexEmissions::copy( const AComplexEmissions& other ){
    gwp = other.gwp;
    maxCntrl = other.maxCntrl;
    gdpcap0 = other.gdpcap0;
    tau = other.tau;
    gdpCap = other.gdpCap;
    techDiff = other.techDiff;
    adjMaxCntrl = other.adjMaxCntrl;
    multMaxCntrl = other.multMaxCntrl;
    emAdjust = other.emAdjust;
    finalEmissCoef = other.finalEmissCoef;
    gwp = other.gwp;
    mEmissionsUnit = other.mEmissionsUnit;

    // Deep copy the auto_ptrs
    if( other.ghgMac.get() ){
        ghgMac.reset( other.ghgMac->clone() );
    }
    if( other.mEmissionsCoef.get() ){
        mEmissionsCoef.reset( other.mEmissionsCoef->clone() );
    }
}

void AComplexEmissions::copyGHGParameters( const AGHG* prevGHG ){
    assert( prevGHG ); // Make sure valid pointer was passed

    // Copy values that always need to be the same for all periods.
    // Note that finalEmissCoef is not copied, since maxCntrl has already been set appropriately

    // Ensure that prevGHG can be cast to AComplexEmissions* otherwise return early
    // TODO: Fix this, maybe pass around a struct.
    const AComplexEmissions* prevComplexGHG = dynamic_cast<const AComplexEmissions*>( prevGHG );
    if( !prevComplexGHG ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Bad dynamic cast occurred in copyGHGParameters." << endl;
        return;
    }

    if ( !gwp ) { 
        gwp = prevComplexGHG->gwp; // only copy if GWP has not changed
    }
    
    maxCntrl = prevComplexGHG->maxCntrl;
    gdpcap0 = prevComplexGHG->gdpcap0;
    tau = prevComplexGHG->tau;

    adjMaxCntrl = prevComplexGHG->adjMaxCntrl;

    // Adjust for maximum control level once GDP per capita is determined
    // This could better be put into a post-calculation processing function if we implimented that in general
    adjustMaxCntrl( prevComplexGHG->gdpCap );

    // Copy values that could change, so only copy if these are still zero (and, thus, were never read-in)
    if ( !techDiff ) { 
        techDiff = prevComplexGHG->techDiff; // only copy if techDiff has not changed
    }

    // Default value for emissCoef is zero, so only copy if a new value was not read in
    // OR
    // If an emissions value was input in a previous period and none was input this period
    // the copy from the previous period
    if( !mEmissionsCoef->getCoef() 
        || prevComplexGHG->mEmissionsCoef->getOverride() ){
        // Note that the overrideCoef method does nothing in the AEmissionsCoef class.
        // A subclass would have to override it if they want to allow it to be overridden.
        mEmissionsCoef->overrideCoef( prevComplexGHG->mEmissionsCoef->getCoef() );
    }

    // If Mac curve was input then copy it, as long as one was not read in for this period
    if ( !ghgMac.get() && prevComplexGHG->ghgMac.get() ) {
        ghgMac.reset( prevComplexGHG->ghgMac->clone() );
    }

}

double AComplexEmissions::getGHGValue( const string& regionName, const string& fuelName,
                                       const vector<IOutput*>& aOutputs,
                                       const double efficiency,
                                       const ICaptureComponent* aCaptureComponent,
                                       const int period ) const
{
    const Marketplace* marketplace = scenario->getMarketplace();
    
    // Constants
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    const double CVRT_tg_MT = 1e-3; // to get teragrams of carbon per EJ to metric tons of carbon per GJ
    
    double GHGTax = marketplace->getPrice( getName(), regionName, period, false );
    if( GHGTax == Marketplace::NO_MARKET_PRICE ){
        GHGTax = 0;
    }

    // Fuel market may not exist.
    const IInfo* fuelInfo = marketplace->getMarketInfo( fuelName, regionName, period, false );
    const double coefFuel = fuelInfo ? fuelInfo->getDouble( "CO2Coef", false ) : 0;

    // apply carbon equivalent to emiss coefficient
    double generalizedCost = GHGTax * gwp * mEmissionsCoef->getCoef() / CVRT90;

    // The generalized cost returned by the GHG may be negative if
    // emissions crediting is occuring.
    return generalizedCost;
}

void AComplexEmissions::calcEmission( const string& regionName,
                                      const string& fuelname,
                                      const double input,
                                      const vector<IOutput*>& aOutputs,
                                      const GDP* aGDP,
                                      ICaptureComponent* aCaptureComponent,
                                      const int aPeriod )
{
    // Primary output is always stored at position zero and used to drive
    // emissions.
    assert( aOutputs[ 0 ] );
    double primaryOutput = aOutputs[ 0 ]->getPhysicalOutput( aPeriod );

    double macReduction = 0;
    gdpCap = aGDP->getPPPGDPperCap( aPeriod );

    const double emissDriver = emissionsDriver(input, primaryOutput);
    if ( ghgMac.get() ){
        macReduction = ghgMac->findReduction(regionName, aPeriod);
    }

    double fControl = 0;    
    double adjustedGdpCap0 = adjustControlParameters( gdpCap, emissDriver, macReduction, aPeriod );
    if ( ( finalEmissCoef > 0 ) || ( maxCntrl > -999 ) ){
        fControl = controlFunction( maxCntrl, tau, adjustedGdpCap0, gdpCap );
    }

    double emissionsOutput = emissDriver * ( 1 - emAdjust ) * ( 1 - fControl ) * ( 1 - macReduction );
    mEmissionsCoef->updateCoef( emissionsOutput );
 
    // This will dynamically get the emissions value.
    mEmissions[ aPeriod ] = mEmissionsCoef->getEmissions( emissionsOutput );
    mEmissionsByFuel[ aPeriod ] = mEmissions[ aPeriod ];

    addEmissionsToMarket( regionName, aPeriod );
}

bool AComplexEmissions::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ){
    if( nodeName == "GWP" ){
        gwp = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "inputEmissions" ){
        mEmissionsCoef.reset( new InputEmissionsCoef( XMLHelper<double>::getValue( curr ) ) );
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
        mEmissionsCoef.reset( new ReadEmissionsCoef( XMLHelper<double>::getValue( curr ) ) );
    }
    // Adjust max Control level, leaving current emissions constant
    else if( nodeName == "adjMaxCntrl" ){
        adjMaxCntrl = XMLHelper<double>::getValue( curr );
    }
    // Multiply maximum control level, changing current emissions
    else if( nodeName == "multMaxCntrl" ){
        multMaxCntrl = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == GhgMAC::getXMLNameStatic() ){
        parseSingleNode( curr, ghgMac, new GhgMAC );
    }
    else if( nodeName == "aggregate" ){
        mEmissionsCoef.reset( new AggrEmissionsCoef );
    }
    else if( nodeName == "GWP" ){
        gwp = XMLHelper<double>::getValue( curr );
    }
    else{
        return false;
    }
    return true;
}

void AComplexEmissions::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    // Write out the EmissionsCoef
    mEmissionsCoef->toInputXML( out, tabs );
    XMLWriteElementCheckDefault( gwp, "GWP", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( emAdjust, "emAdjust", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( maxCntrl, "maxCntrl", out, tabs, -1000.0 );
    XMLWriteElementCheckDefault( gdpcap0, "gdpcap0", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( tau, "tau", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( finalEmissCoef, "finalEmissCoef", out, tabs, -1.0 );
    XMLWriteElementCheckDefault( adjMaxCntrl, "adjMaxCntrl", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( multMaxCntrl, "multMaxCntrl", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( techDiff, "techDiff", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( gwp, "GWP", out, tabs, 1.0 );
    XMLWriteElement( mEmissionsUnit, "emissionsUnit", out, tabs );

    // Write out the GHGMAC
    if( ghgMac.get() ){
        ghgMac->toInputXML( out, tabs );
    }
}

void AComplexEmissions::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    // Write out the EmissionsCoef
    mEmissionsCoef->toDebugXML( out, tabs );
    XMLWriteElement( gwp, "GWP", out, tabs );
    XMLWriteElement( emAdjust, "emAdjust", out, tabs );
    XMLWriteElement( maxCntrl, "maxCntrl", out, tabs );
    XMLWriteElement( gdpcap0, "gdpcap0", out, tabs );
    XMLWriteElement( tau, "tau", out, tabs );
    XMLWriteElement( finalEmissCoef, "finalEmissCoef", out, tabs );
    XMLWriteElement( adjMaxCntrl, "adjMaxCntrl", out, tabs );
    XMLWriteElement( techDiff, "techDiff", out, tabs );
    XMLWriteElement( gwp, "GWP", out, tabs );
    XMLWriteElement( mEmissionsUnit, "emissionsUnit", out, tabs );

     // Write out the GHGMAC
    if( ghgMac.get() ){
        ghgMac->toDebugXML( period, out, tabs );
    }
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
double AComplexEmissions::adjustControlParameters( const double gdpCap, const double emissDrive, const double macReduction, const int period ){
    double adjustedGdpCap0 = gdpcap0; //! gdpCap0 used by the control function- adjusted for techDiffusion

    if (techDiff !=0){
        adjustedGdpCap0 = gdpcap0 / calcTechChange(period);
    }
    if ( finalEmissCoef > 0 ){
        double B = 0;
        double multiplier = 0;
        // TODO: This is kind of a hack to avoid this computation.  There must be a better way.
        //       What about creating a struct to pass some of the needed variables?
        if( mEmissionsCoef->needsCalcForAdjustment() ){
            B = (1/controlFunction(100,tau,adjustedGdpCap0,gdpCap));
            multiplier = emissDrive * ( 1 - emAdjust ) * ( 1 - macReduction );
        }
        maxCntrl = mEmissionsCoef->calcMaxCntrl( finalEmissCoef, B, multiplier );
        // Control values should never be larger than 100%.
        maxCntrl = min( maxCntrl, 100.0 );
    }
    return adjustedGdpCap0;
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
double AComplexEmissions::controlFunction( const double maxCntrlIn, const double tauIn, const double gdpcap0In, const double gdpCapIn ){
    if( tauIn != 0 && gdpcap0In != 0 ){
        const double CLOGIT = 4.394; // See above for documentation.
        return (maxCntrlIn/100) / (1 + exp( -CLOGIT * (gdpCapIn - gdpcap0In) / tauIn ));
    }
    return 0; 
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
void AComplexEmissions::adjustMaxCntrl( const double GDPcap ){
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

/*! \brief Returns the value by which to adjust gdpcap0, based on technological diffusion
* The Variable TechCh represents the percent reduction in gdpcap0 per year, due to technological change and diffusion.
* The overall reduction in gdpcap0 is 1 + the techCh percentage raised to the power of the number of years after 
* the base year.  When applied to the control function, this will allow emissions controls to approach maxCntrl sooner.
*\ Author Nick Fernandez
* \param period the current period where calculations occur
* \returns amount to reduce the parameter gdpCap0 by
*/
double AComplexEmissions::calcTechChange( const int period ){
    const Modeltime* modeltime = scenario->getModeltime();
    int year = modeltime->getper_to_yr( period ); 
    year -=  modeltime->getper_to_yr(1); // subtracts off base year to find number of years after base year
    return pow(1 + (techDiff / 100), year );
}

void AComplexEmissions::initCalc( const string& aRegionName,
                                  const string& aFuelName,
                                  const IInfo* aLocalInfo,
                                  const int aPeriod )
{
    maxCntrl *= multMaxCntrl;
    // Make sure control percentage never goes above 100% so there are no negative emissions!
    maxCntrl = min( maxCntrl, 100.0 );

     // Perform any MAC initializations
    if( ghgMac.get() ){
        ghgMac->initCalc( getName() );
    }

    // Ensure the user set an emissions coefficient in the input
    if( !mEmissionsCoef.get() ){
        mEmissionsCoef.reset( new ReadEmissionsCoef( 0 ) );
    }

    mEmissionsCoef->initCalc( aLocalInfo, getName() );

    // If a finalEmissCoef or maxCntrl were read in, a tau and gdpcap0
    // must also have been read in.
    if( ( finalEmissCoef > 0 ) || ( maxCntrl > -999 ) ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        if ( tau == 0 ){
        mainLog << "Control function for " << getName() 
                << " requires an input of tau because either maxCntrl or "
                << " finalEmissCoef were read in." << endl;
        }
        if ( gdpcap0 == 0 ){  
            mainLog << "Control function for " << getName()
                    << " requires an input of gdpcap0 because either maxCntrl "
                    << " or finalEmissCoef were read in." << endl;
        }
    }

}




