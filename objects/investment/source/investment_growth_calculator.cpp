/*! 
* \file investment_growth_calculator.cpp
* \ingroup Objects
* \brief InvestmentGrowthCalculator class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "sectors/include/investment_growth_calculator.h"
#include "util/base/include/xml_helper.h"
#include "demographics/include/demographic.h"
#include "sectors/include/investment_utils.h"

using namespace std;

InvestmentGrowthCalculator::InvestmentGrowthCalculator ():
mAggregateInvestmentFraction( 0.01 ),
mInvestmentAcceleratorScalar( 1.2 ),
mEconomicGrowthExp( 1 ),
mMarginalValueDollar( 1 )
{
    // TEMP
    mTempInvScalar = 0;
    mTempEconScalar = 0;
}

//! Return the XML name of this object statically.
const string& InvestmentGrowthCalculator::getXMLNameStatic(){
    const static string XML_NAME = "investment-growth-calculator";
    return XML_NAME;
}

/*! \brief Parses all data associated with the class
*
*
* \author Josh Lurz
* \param aNode pointer to the current node in the XML input tree
*/
void InvestmentGrowthCalculator::XMLParse( const xercesc::DOMNode* aNode ) {
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
        if( nodeName == "AggregateInvestmentFraction" ){
            mAggregateInvestmentFraction = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "InvestmentAcceleratorScalar" ){
            mInvestmentAcceleratorScalar = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "EconomicGrowthExp" ){
            mEconomicGrowthExp = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "MarginalValueDollar" ){
            mMarginalValueDollar = XMLHelper<double>::getValue( curr );
        }
        else {
            cout << "Warning unknown node " << nodeName << " found while parsing " << getXMLNameStatic() << endl;
        }
    }
}

//! Write aOut debugging information.
void InvestmentGrowthCalculator::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mAggregateInvestmentFraction, "AggregateInvestmentFraction", aOut, aTabs );
    XMLWriteElement( mInvestmentAcceleratorScalar, "InvestmentAcceleratorScalar", aOut, aTabs );
    XMLWriteElement( mEconomicGrowthExp, "EconomicGrowthExp", aOut, aTabs );
    XMLWriteElement( mMarginalValueDollar, "MarginalValueDollar", aOut, aTabs );
    // TEMP
    XMLWriteElement( mTempEconScalar, "econ-growth-scalar", aOut, aTabs );
    XMLWriteElement( mTempInvScalar, "investment-scalar", aOut, aTabs );

    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

//! Write aOut input XML information.
void InvestmentGrowthCalculator::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElementCheckDefault( mAggregateInvestmentFraction, "AggregateInvestmentFraction", aOut, aTabs );
    XMLWriteElementCheckDefault( mInvestmentAcceleratorScalar, "InvestmentAcceleratorScalar", aOut, aTabs );
    XMLWriteElementCheckDefault( mEconomicGrowthExp, "EconomicGrowthExp", aOut, aTabs, 1.0 );
    XMLWriteElementCheckDefault( mMarginalValueDollar, "MarginalValueDollar", aOut, aTabs, 1.0 );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

/*!\ brief Calculate an overall scalar used to grow investment from the previous aPeriod.
* \param aSubsecs The subsectors contained in the Sector currently calculating investment.
* \param aDemographic The Demographics object needed for calculating the increase in working age population.
* \param aRegionName The name of the region containing the sector being invested in.
* \param aPrevInvestment Total sector investment for the previous period.
* \param aPeriod The aPeriod in which to calculate the scalar.
* \return An overall scalar used to grow investment from the previous aPeriod.
* \author Josh Lurz
*/
double InvestmentGrowthCalculator::calcInvestmentDependencyScalar( const vector<IInvestable*>& aInvestables,
                                                                 const Demographic* aDemographic,
                                                                 const NationalAccount& aNationalAccount,
                                                                 const string& aGoodName,
                                                                 const string& aRegionName,
                                                                 const double aPrevInvestment,
                                                                 const double aInvestmentLogitExp,
                                                                 const int aPeriod ) 
{   
    // Calculate the starting level of investment based on the previous period.
    const double baseCapital = InvestmentUtils::calcBaseCapital( aRegionName, aPrevInvestment,
                                                                 mAggregateInvestmentFraction, aPeriod );

    // Calculate the scalar based on economic growth. 
    const double economicGrowthScalar = calcEconomicGrowthScalar( aDemographic, aPeriod );
    assert( economicGrowthScalar > 0 );
    // TEMP
    mTempEconScalar = economicGrowthScalar;

    // Calculate the scalar for the investment.
    double invDepScalar = baseCapital * mInvestmentAcceleratorScalar * economicGrowthScalar *
                          pow( mMarginalValueDollar, -1 * aInvestmentLogitExp );
    // TEMP
    mTempInvScalar = invDepScalar;
    assert( invDepScalar > 0 );
    return invDepScalar;
}

/*! \brief Calculate a growth scalar based on the increase in economic activity in the region.
* \param aDemographic The Demographic object used to calculate the change in working age population.
* \param The aPeriod in which to calculate the economic growth scalar.
* \return The economic growth scalar.
* \author Josh Lurz
*/
double InvestmentGrowthCalculator::calcEconomicGrowthScalar( const Demographic* aDemographic,
                                                           const int aPeriod ) const
{
    /*! \pre aPeriod is greater than the base aPeriod. */
    assert( aPeriod > 0 );
    
    // Calculate the change in the working age population.
    double workingAgeRateChange = aDemographic->getWorkingAgePopulation( aPeriod ) 
                                  / aDemographic->getWorkingAgePopulation( aPeriod - 1 );
    assert( workingAgeRateChange > 0 );
    
    // Calculate the economic growth scalar. 
    double econGrowthScalar = pow( workingAgeRateChange, mEconomicGrowthExp );
    
    /*! \post Economic Growth Scalar is greater than zero. */
    assert( econGrowthScalar > 0 );
    
    return econGrowthScalar;
}
