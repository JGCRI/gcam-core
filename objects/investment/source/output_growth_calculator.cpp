/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Labratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responisbility for the 
	use of this software.
*/

/*! 
* \file output_growth_calculator.cpp
* \ingroup Objects
* \brief OutputGrowthCalculator class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "sectors/include/output_growth_calculator.h"
#include "util/base/include/xml_helper.h"
#include "sectors/include/investment_utils.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "sectors/include/iinvestable.h"
#include "sectors/include/simple_expected_profit_calculator.h"

using namespace std;

extern Scenario* scenario;

OutputGrowthCalculator::OutputGrowthCalculator ():
mAggregateInvestmentFraction( 0.01 ),
mOutputGrowthRate( scenario->getModeltime()->getmaxper() ),
mTrialCapital( scenario->getModeltime()->getmaxper() )
{
}

//! Return the XML name of this object statically.
const string& OutputGrowthCalculator::getXMLNameStatic(){
    const static string XML_NAME = "output-growth-calculator";
    return XML_NAME;
}

/*! \brief Parses all data associated with the class
*
*
* \author Josh Lurz
* \param aNode pointer to the current node in the XML input tree
*/
void OutputGrowthCalculator::XMLParse( const xercesc::DOMNode* aNode ) {
    /*! \pre make sure we were passed a valid node. */
    assert( aNode );

    // get all child nodes.
    const xercesc::DOMNodeList* nodeList = aNode->getChildNodes();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        const xercesc::DOMNode* curr = nodeList->item( i );
        if( curr->getNodeType() == xercesc::DOMNode::TEXT_NODE ){
            continue;
        }

        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        if( nodeName == "aggregate-investment-fraction" ){
            mAggregateInvestmentFraction = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "output-growth-rate" ){
            XMLHelper<double>::insertValueIntoVector( curr, mOutputGrowthRate, scenario->getModeltime() );
        }
        else {
            cout << "Warning unknown node " << nodeName << " found while parsing " << getXMLNameStatic() << endl;
        }
    }
}

//! Write out debugging information.
void OutputGrowthCalculator::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mAggregateInvestmentFraction, "aggregate-investment-fraction", aOut, aTabs );
    XMLWriteElement( mOutputGrowthRate[ aPeriod ], "output-growth-rate", aOut, aTabs );
    XMLWriteElement( mTrialCapital[ aPeriod ], "trial-capital", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

//! Write out input XML information.
void OutputGrowthCalculator::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElementCheckDefault( mAggregateInvestmentFraction, "aggregate-investment-fraction", aOut, aTabs );
    
    const Modeltime* modeltime = scenario->getModeltime();
    // 0 isn't really the default.
    for( int per = 0; per < modeltime->getmaxper(); ++per ){
        XMLWriteElementCheckDefault( mOutputGrowthRate[ per ], "output-growth-rate", aOut, aTabs, 0.0,
                                     modeltime->getper_to_yr( per ) );
    }
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

/*!\ brief Calculate an overall scalar used to grow investment from the previous aPeriod.
* \param aSubsecs The subsectors contained in the Sector currently calculating investment.
* \param aDemographic The Demographics object needed for calculating the increase in working age population.
* \param aRegionName The name of the region containing the sector being invested in.
* \param aPrevInvestment Total sector investment for the previous period.
* \param aProfitElasExp The expected profit rate exponent.
* \param aPeriod The aPeriod in which to calculate the scalar.
* \return An overall scalar used to grow investment from the previous aPeriod.
* \author Josh Lurz
*/
double OutputGrowthCalculator::calcInvestmentDependencyScalar( const vector<IInvestable*>& aInvestables,
                                                                const Demographic* aDemographic,
                                                                const NationalAccount& aNationalAccount,
                                                                const string& aGoodName,
                                                                const string& aRegionName,
                                                                const double aPrevInvestment,
                                                                const double aInvestmentLogitExp,
                                                                const int aPeriod ) 
{
    // We should store the entire scalar for the period, not just TESTK like fortran does.
    // Calculate the starting level of capital.
    const double baseCapital = InvestmentUtils::calcBaseCapital( aRegionName, aPrevInvestment,
                                                                 mAggregateInvestmentFraction, 
                                                                 aPeriod );
    // Calculate trial capital from the subsectors.
    const double trialCapital = calcTrialCapital( aInvestables, aNationalAccount, aGoodName, aRegionName,
                                                  aInvestmentLogitExp, aPeriod );
    
    // Calculate the trial investment. Performs a capital interpolation.
    const double trialInvestment = ( trialCapital - ( 2 * baseCapital ) ) / 3;

    // Calculate the scalar.
    const double invDepScalar = max( trialInvestment, trialCapital / 10 );

    /*! \post The investment scalar is positive */
    assert( invDepScalar > 0 );
    return invDepScalar;
}

/*! \brief Calculate the trial amount of capital(TESTK)
* \details TODO
* \param aInvestables The investable objects
* \param aPeriod The period in which to calculate trial capital.
* \return The amount of trial capital.
*/
double OutputGrowthCalculator::calcTrialCapital( const vector<IInvestable*>& aInvestables, 
                                                  const NationalAccount& aNationalAccount, const string& aGoodName,
                                                  const string& aRegionName, const double aInvestmentLogitExp,
                                                  const int aPeriod )
{
    // Fortran code special cases period 0, but you shouldn't calculate investment in period 0.
    assert( aPeriod > 0 );
    
    // If the trial capital for this period has already been calculated, return that.
    // This ensures this calculation is only performed once per period.
    if( mTrialCapital[ aPeriod ] > 0 ){
        return mTrialCapital[ aPeriod ];
    }
    
    const double additionalOutput = calcOutputGap( aInvestables, aGoodName, aRegionName, aInvestmentLogitExp, aPeriod );
    const double capitalOutputRatio = calcSectorCapitalOutputRatio( aInvestables, aNationalAccount, aGoodName,
                                                                    aRegionName, aInvestmentLogitExp, aPeriod );

    mTrialCapital[ aPeriod ] = capitalOutputRatio * additionalOutput +
                               InvestmentUtils::sumInvestment( aInvestables, aPeriod - 1 ) * 2 +
                               InvestmentUtils::calcFixedInvestment( aInvestables, aPeriod ) * 3;
    
    assert( mTrialCapital[ aPeriod ] > 0 );
    return mTrialCapital[ aPeriod ];
}

/*! \brief Calculate gap between what is produced by old vintages and will be produced by baseline investment
* and what is desired based on an output growth rate and the previous period's demand.
* \details 
* \param aSubsecs The subsectors of the current sector.
* \param aPeriod The period in which to calculate the output gap.
* \return The output gap.
*/
double OutputGrowthCalculator::calcOutputGap( const vector<IInvestable*>& aInvestables,
                                              const string& aGoodName,
                                              const string& aRegionName,
                                              const double aInvestmentLogitExp,
                                              const int aPeriod ) const 
{
    const Marketplace* marketplace = scenario->getMarketplace();
    
    // Get the sales of the good and accelerate the growth.
    const double projSales = marketplace->getSupply( aGoodName, aRegionName, aPeriod - 1 ) 
                             * mOutputGrowthRate[ aPeriod ];
    
    // Get the supply of the good. This is called after only operating old vintages(QOLD)
    const double oldVintageSupply = marketplace->getSupply( aGoodName, aRegionName, aPeriod );
    
    // Calculate the quantity the new vintages will need to produce(QNEW)
    const double desiredNewVintageOutput = projSales - oldVintageSupply;

    // Calculate the amount of investment that would be done if the accelerator was one.
    // This uses the capital interpolation formula to determine a total.
    double totalFixedInvestment = 0;
    double totalPrevAnnualInvestment = 0;
    for( unsigned int i = 0; i < aInvestables.size(); ++i ){
        // Determine the capital to output ratio for the subsector. 
        const double capOutputRatio = aInvestables[ i ]->getCapitalOutputRatio( aRegionName,
                                                                                aGoodName,
                                                                                aPeriod );
        // Invert the ratio to find output per unit of capital.
        const double capQuotient = ( capOutputRatio > 0 ) ? 1 / capOutputRatio : 0;

        // Add the previous periods investment.
        // I think these are only right for a 5 year timestep.
        totalPrevAnnualInvestment += aInvestables[ i ]->getAnnualInvestment( aPeriod - 1 ) *
                                     capQuotient * 2;
        // Add any fixed investment.
        totalFixedInvestment += aInvestables[ i ]->getFixedInvestment( aPeriod ) *
                                capQuotient * 3;
    }

    // Determine the gap between desired and actual output(RGAP). Don't let the
    // output gap drop below zero.
    return  max( desiredNewVintageOutput - totalFixedInvestment - totalPrevAnnualInvestment, 0.0 );
}

double OutputGrowthCalculator::calcSectorCapitalOutputRatio( const vector<IInvestable*>& aInvestables,
                                                              const NationalAccount& aNationalAccount,
                                                              const string& aGoodName,
                                                              const string& aRegionName,
                                                              const double aInvestmentLogitExp,
                                                              const int aPeriod ) const 
{
    // There is a lot of code copied from InvestmentAccelerator currently. This
    // needs to be fixed. This exactly duplicates the Fortran logic, which is 
    // different than in the investment accelerator in dealing with fixed
    // investment. I think the behavior will be the same though. 
    
    // Calculate total expected profit rates.
    double expProfitRateTotal = 0;

    // Create a simple expected profit rate calculator.
    auto_ptr<IExpectedProfitRateCalculator> expProfitRateCalc( new SimpleExpectedProfitCalculator );

    for( InvestmentUtils::CInvestableIterator currInv = aInvestables.begin();
         currInv != aInvestables.end(); ++currInv )
    {
        double currExpProfitRate = (*currInv)->getExpectedProfitRate( aNationalAccount,
                                                                      aRegionName,
                                                                      aGoodName,
                                                                      expProfitRateCalc.get(),
                                                                      aInvestmentLogitExp,
                                                                      false,
                                                                      aPeriod );
        if( currExpProfitRate > 0 ){
            expProfitRateTotal += pow( currExpProfitRate, aInvestmentLogitExp );
        }
    }
    // Create a vector of investment shares, one per subsector.
    vector<double> shares( aInvestables.size() );
    
    // If there is no expected profit then the shares will stay at zero.
    if( expProfitRateTotal > 0 ){
        // Loop through subsectors first to calculate shares.
        for( unsigned int i = 0; i < aInvestables.size(); ++i ){
            double currExpProfitRate = aInvestables[ i ]->getExpectedProfitRate( aNationalAccount,
                                                                                 aRegionName,
                                                                                 aGoodName,
                                                                                 expProfitRateCalc.get(),
                                                                                 aInvestmentLogitExp,
                                                                                 false,
                                                                                 aPeriod );

            // Store the subsector investment share. This will be zero for negative profit subsectors.
            shares[ i ] = pow( currExpProfitRate, aInvestmentLogitExp ) / expProfitRateTotal;
        }
    }
    
    double outputRatio = 0;
    // Now calculate the sector level average capital output ratio.
    for( unsigned int i = 0; i < aInvestables.size(); ++i ){
        const double capOutputRatio = aInvestables[ i ]->getCapitalOutputRatio( aRegionName,
                                                                                aGoodName,
                                                                                aPeriod );
        if( capOutputRatio > 0 ){
            outputRatio += shares[ i ] * 1 / capOutputRatio;
        }
    }
    // If the output ratio is positive, return the inverse. Otherwise return 0.
    return ( outputRatio > 0 ) ? 1 / outputRatio : 0;
}
