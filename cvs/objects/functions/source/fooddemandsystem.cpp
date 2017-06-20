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
 * \file fooddemandsystem.cpp
 * \ingroup Objects
 * \brief FoodDemandSystem source file
 * \author Robert Link
 */

#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <math.h>

#include "functions/include/fooddemandsystem.hpp"
#include "functions/include/function_utils.h"
#include "containers/include/gdp.h"
#include "marketplace/include/marketplace.h"
#include "demographics/include/demographic.h"
#include "containers/include/info.h"
#include "sectors/include/sector_utils.h"

// canonical parameter names.  The input file is not required to supply
// parameter names, but if it does, they will be validated against this list.
// When generating input XML we will also use this list to provide names for the
// parameters in the generated XML.
namespace {
    const std::vector<std::string> pnames = {
        "A-staple",
        "A-nonstaple",
        "g-ss",
        "g-nn",
        "g-cross",
        "nu",
        "lambda",
        "kappa",
        "pm",
        "rgn-bias-staple",
        "rgn-bias-nonstaple"
    };
    const std::string param_tag = "param"; //!< XML tag for parameters
    // The two scale factors below give us a little extra control over the shape of
    // the demand functions.  Unlike the other model parameters, we didn't fit
    // these; we fixed them ahead of time and then fit the other model
    // parameters subject to those assumed values.  As such, they shouldn't be
    // changed without re-fitting the model.  However, you shouldn't do that.
    // If we ever want to consider changing these, we should add a scale
    // parameter to the nonstaple demand and fit the model with that as an
    // additional parameter.  Note that although there are two numbers here, only
    // their ratio is significant.  Differences in the absolute levels will be
    // absorbed into Pm during the fitting process.
    const double psscl = 100.0;            //!< Scale factor for staple prices
    const double pnscl = 20.0;             //!< Scale factor for nonstaple prices
    const std::string food_demand_unit_str = "KCal/(person-day)"; 
}


// Price conversion factor.  Input prices are in 1975$ per Mcal-year.
// Convert to 2005$ per Mcal-day (these are thermodynamic calories, so
// 1 Mcal is 1000 dietary Calories.)
const double FoodDemandSystem::mprice_conversion_fac =
    1.0 / 365.0 / FunctionUtils::DEFLATOR_1975_PER_DEFLATOR_2005();

// Quantity conversion factor.  Output quantities are in Mcal/day (per
// capita).  We need to convert to Pcal/year (also per capita).
const double FoodDemandSystem::mqty_conversion_fac = 365.0 * 1e-9;

void FoodDemandSystem::calcDemand(
    const std::string &aRegionName,
    const Demographic &aDemographics,
    const GDP &aGDP,
    const std::vector<double> &aprices,
    int aPeriod,
    std::vector<double> &aDemandOutput ) const
{
    std::vector<double> prices(aprices);
    for(unsigned i=0; i<aprices.size(); ++i) {
        prices[i] *= mprice_conversion_fac;
    }
    
    // get parameters into more convenient form
    double as = mParams[0] * mParams[9]; // As * (staple-bias-fac)
    double an = mParams[1] * mParams[10]; // An * (nonstaple-bias-fac)
    double gss = mParams[2];
    double gnn = mParams[3];
    double gns = mParams[4];
    double nu = mParams[5];
    double lam = mParams[6];
    double kap = mParams[7];
    double pm = mParams[8];

    double x = aGDP.getPPPGDPperCap(aPeriod) / pm;
    double ws = prices[0] / pm * psscl;
    double wn = prices[1] / pm * pnscl;

    // Get the trial budget fractions.  These will be used to calculate
    // price exponents in the demand equations.
    double alphas = getTrialBudgetFrac(aRegionName, aPeriod, 0);
    double alphan = getTrialBudgetFrac(aRegionName, aPeriod, 1);

    double amin = 0.1;          // For stability, we limit how small the alphas
                                // can be when calculating the condition for the
                                // cross-elasticity 
    double gsn = std::max(alphan, amin) / std::max(alphas, amin) * gns;

    // The income functions return both the relevant quantity factor and the
    // partial derivative.  
    double qis, etas;
    double qin, etan; 
    stapleIncomeFunc(lam, kap, x, qis, etas);
    nonStapleIncomeFunc(nu, x, qin, etan);

    // Calculate the exponents
    double ess = gss - alphas*etas;
    double ens = gns - alphas*etan;
    double esn = gsn - alphan*etas;
    double enn = gnn - alphan*etan;

    double Qs = as * std::pow(ws, ess) * std::pow(wn, esn) * qis;
    double Qn = an * std::pow(ws, ens) * std::pow(wn, enn) * qin;

    // Calculate actual budget fractions
    double alphas_actual = ws*Qs/x / psscl;
    double alphan_actual = wn*Qn/x / pnscl;

    // Check budget constraint.  If we're spending more than the total income on
    // food, then reduce nonstaples first, followed by staples.  We'll use the
    // actuals for this calculation.
    double alphat = alphas_actual + alphan_actual;
    double budget = 1.0;        // Fraction of income available for food. We
                                // didn't expermient with changing this in the
                                // original model development.
    
    if(alphat > budget) {
        if(alphas_actual < budget) {
            alphan_actual = budget - alphas_actual;
        }
        else {
            alphan_actual = 0.0;
            alphas_actual = budget;
        }
        // Recalculate quantities based on the new budget fractions
        Qs = alphas_actual * x/ws * psscl;
        Qn = alphan_actual * x/wn * pnscl;
    }

    setActualBudgetFrac( aRegionName, aPeriod, 0, alphas_actual);
    setActualBudgetFrac( aRegionName, aPeriod, 1, alphan_actual);

    // Convert demand output units from thousands of dietary calories per
    // day to Pcal (1e15 thermodynamic calories) per year.
    double fac = 1e3 / 1e15 * 365;
    Qs *= fac;
    Qn *= fac;

    // Set the primary outputs of the demand system
    aDemandOutput[0] = Qs*mqty_conversion_fac;
    aDemandOutput[1] = Qn*mqty_conversion_fac;
}

void FoodDemandSystem::reportDemand(std::vector<double> &aDemand) const
{
    // Convert from Pcal/yr back to KCal/day (both values are per
    // capita)
    for(unsigned i=0; i<aDemand.size(); ++i)
        aDemand[i] /= mqty_conversion_fac;
}


void FoodDemandSystem::reportUnits(std::vector<std::string> &aUnits)
    const
{
    for(unsigned i=0; i < aUnits.size(); ++i)
        aUnits[i] = food_demand_unit_str;
}

void FoodDemandSystem::completeInit( const std::string &aRegionName, const
                                     std::string &aSectorName )
{
    // Create trial value markets for the two budget fractions. 
    // We have to create unique names for the trial value markets.
    mTrialValueMktNames.resize(2);
    mTrialValueMktNames[0] = aSectorName + "-staple-budget-fraction";
    mTrialValueMktNames[1] = aSectorName + "-nonstaple-budget-fraction";
    mLastValues.resize(2);
    mLastValues[0] = mLastValues[1] = 0.0;

    bool snew = SectorUtils::createTrialSupplyMarket( aRegionName,
                                                      mTrialValueMktNames[0],
                                                      "unitless", "" );
    bool nnew = SectorUtils::createTrialSupplyMarket( aRegionName,
                                                      mTrialValueMktNames[1],
                                                      "unitless", "" );
    // Check to see that the trial value markets were new.
    if( ! (snew && nnew) ) {
        ILogger & mainlog = ILogger::getLogger( "main_log" );
        mainlog.setLevel(ILogger::ERROR);
        mainlog << "FoodDemandSystem::completeInit:  created duplicate trial value market in region= " 
                << aRegionName << "  sector= " << aSectorName << std::endl;
    }
}

const std::string &FoodDemandSystem::getXMLNameStatic(void)
{
    static const std::string XML_NAME = "food-demand-system";
    return XML_NAME;
}

bool FoodDemandSystem::XMLParse( const xercesc::DOMNode *aNode )
{
    using namespace xercesc;
    // counter for the number of parameters parsed
    int nparsed=0;

    ILogger &mainlog = ILogger::getLogger( "main_log" );
        
    // get all child nodes.
    DOMNodeList* nodeList = aNode->getChildNodes();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); ++i ){
        DOMNode* cnode = nodeList->item( i );
        const std::string nodeName =
            XMLHelper<std::string>::safeTranscode( cnode->getNodeName() ); 
        
        if( nodeName == "#text" ) {
            continue;
        } 
        else if( nodeName == param_tag ) {
            // Parse parameter value
            double pval = XMLHelper<double>::getValue( cnode );
            mParams.push_back(pval);

            // Get the name attribute, if any.  If we find one, compare it to
            // the names in the list.
            std::string pname = XMLHelper<std::string>::getAttr( cnode, "name" );
            if(pname != std::string() && pname != pnames[nparsed]) {
                // there was a name, but it doesn't match the list
                mainlog.setLevel( ILogger::SEVERE );
                mainlog << "While parsing FoodDemandSystem: bad parameter name."
                        << " Expected " << pnames[nparsed] << " got " << pname
                        << std::endl;
                abort();
            }
            // either the name matched, or it wasn't provided.  Increment the
            // count either way.
            nparsed++;
        }
        else {
            // unrecognized tag.  Log as error, but not fatal.
            mainlog.setLevel(ILogger::ERROR);
            mainlog << "While parsing FoodDemandSystem:  unknown XML element "
                    << nodeName << std::endl;
        }
    }

    // Check to see that the total numbeer of parameters parsed matches
    if( mParams.size() != pnames.size() ) {
        mainlog.setLevel(ILogger::SEVERE);
        mainlog << "FoodDemandSystem parsed " << mParams.size()
                << "parameters.  Expected " << pnames.size();
        abort();
    }

    return true;
}

void FoodDemandSystem::toInputXML( std::ostream &aOut, Tabs *aTabs ) const
{
    XMLWriteOpeningTag( getXMLName(), aOut, aTabs, mName );
    aTabs->increaseIndent();
    
    // Write the parameter values
    std::map<std::string, std::string> attr;
    for(unsigned i=0; i < pnames.size(); ++i) {
        attr["name"] = pnames[i];
        XMLWriteElementWithAttributes(mParams[i], param_tag, aOut, aTabs, attr);
    }

    aTabs->decreaseIndent();
    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

double FoodDemandSystem::getTrialBudgetFrac( const std::string &aRegion, int aPeriod,
                                             int acomp ) const
{
    double param = SectorUtils::getTrialSupply( aRegion, mTrialValueMktNames[acomp],
                                                aPeriod );

    // transform the value from the trial value market (-infinity, infinity) to
    // (0,1). 
    return 0.5 * ( 1 + tanh(param) );
}


void FoodDemandSystem::setActualBudgetFrac( const std::string &aRegion, int aPeriod,
                                            int acomp, double alpha ) const
{
    // transform budget fraction from (0,1) to (-infinity, infinity)
    double param = atanh( 2.0*alpha - 1 );

    // set value in trial value market
    SectorUtils::addToTrialDemand( aRegion, mTrialValueMktNames[acomp], param,
                                   mLastValues[acomp], aPeriod );
}


void FoodDemandSystem::stapleIncomeFunc( double lam, double kap, double x,
                                         double &qis, double &etas )
{
    assert(lam > 0 && kap > 0 && x > 0);
    
    double k = exp(kap);        // k-value from the R version of the model
    // The limit as x-> 0 of the logarithmic derivative of this function is not
    // well behaved.  However, the quantity is very small for k*x < ~1e-3, so
    // we can replace this segment with a linear ramp and get essentially the same
    // behavior.  The parameters below facilitate that.
    double x1 = 1.0e-3 / k;
    double scale = pow(k, -lam); // Normalization factor so that Qi(1) == 1
    
    if(x > x1) {
        qis = scale * pow(k*x, lam/x);
        etas = lam*(1-log(k*x)) / x;
    }
    else {
        qis = scale * pow(k*x1, lam/x1) / x1 * x;
        etas = 1.0;
    }
}


void FoodDemandSystem::nonStapleIncomeFunc( double nu, double x, double &qin,
                                            double &etan )
{
    double enu = exp(-nu);
    double delta = 1.0-x;
    double delta2 = delta*delta;
    double scale = 1.0 / enu;   // normalization factor

    if(fabs(delta) > 1.0e-3/nu) {
        qin = scale * pow(x, nu / delta);
        // lim_x->0 etan = 1
        etan = x < 1.0e-4 ? 1 : 1/delta + x*log(x)/delta2;
    }
    else {
        // Represent q and eta near x==1 as a Taylor series
        double delta3 = delta2*delta;
        qin = scale * (enu -
                       0.5 * nu * enu * delta +
                       1.0/24.0 * enu * nu*(3.0*nu-8.0) * delta2);
        etan = nu * (0.5 +
                     1.0/6.0 * delta +
                     1.0/12.0 * delta2 +
                     1.0/20.0 * delta3);
    }
}
