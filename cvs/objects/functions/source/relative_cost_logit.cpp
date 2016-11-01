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
* \file relative_cost_logit.cpp
* \ingroup objects
* \brief RelativeCostLogit class source file
* \author Robert Link
*/

#include "util/base/include/definitions.h"
#include <stdlib.h>
#include <math.h>
#include <cassert>
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "functions/include/relative_cost_logit.hpp"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace xercesc;

//! default constructor:  arg value <= 0 will get filled in with the default
RelativeCostLogit::RelativeCostLogit():
mLogitExponent( 1.0 ),
mOutputCost( 0.0 )
{
}

//! destructor: nothing to clean up
RelativeCostLogit::~RelativeCostLogit() {
}

const string& RelativeCostLogit::getXMLNameStatic() {
    const static string XML_NAME = "relative-cost-logit";
    return XML_NAME;
}

bool RelativeCostLogit::XMLParse( const DOMNode *aNode ) {
    /*! \pre Make sure we were passed a valid node. */
    assert( aNode );

    const Modeltime* modeltime = scenario->getModeltime();
    
    // get the children of the node
    DOMNodeList* nodeList = aNode->getChildNodes();

    bool parsingSuccessful = true;

    // loop over the child nodes
    for( unsigned int i = 0; i < nodeList->getLength(); ++i ) {
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == XMLHelper<void>::text() ) {
            continue;
        }
        else if( nodeName == "logit-exponent" ) {
            /*
            double value = XMLHelper<double>::getValue( curr );
            if( value > 0 ) {
                ILogger& mainlog = ILogger::getLogger( "main_log" );
                mainlog.setLevel( ILogger::WARNING );
                mainlog << "Skipping invalid value for logit exponent: " << value
                        << " while parsing " << getXMLNameStatic() << "."
                        << endl;
                parsingSuccessful = false;
            }
            else {
            */
                XMLHelper<double>::insertValueIntoVector( curr, mLogitExponent, modeltime );
            //}
        }
        else {
            ILogger& mainlog = ILogger::getLogger( "main_log" );
            mainlog.setLevel( ILogger::WARNING );
            mainlog << "Unknown text string: " << nodeName
                    << " found while parsing " << getXMLNameStatic() << "."
                    << endl;
            parsingSuccessful = false;
        }
    }

    return parsingSuccessful;
}

void RelativeCostLogit::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteVector( mLogitExponent, "logit-exponent", aOut, aTabs, modeltime );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}


void RelativeCostLogit::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mLogitExponent[ aPeriod ], "logit-exponent", aOut, aTabs );
    XMLWriteElement( mOutputCost, "output-cost", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}


/*!
 * \brief Relative cost logit discrete choice function.
 * \details Calculate the log of the numerator of the discrete choice (i.e., the unnormalized version) 
 *          function being used to calculate subsector shares in this sector.  The normalization 
 *          factor will be calculated later.
 * \param aShareWeight share weight for the choice for which the share is being calculated.
 * \param aCost cost for the choice for which the share is being calculated.
 * \param aPeriod model time period for the calculation.
 * \return log of the unnormalized share.
 * \warning Negative costs can not be used in this logit formulation.  Instead the cost
 *          the cost is capped at RelativeCostLogit::getMinCostThreshold.  This implies
 *          no behavior once costs have crossed this threshold.
 */
double RelativeCostLogit::calcUnnormalizedShare( const double aShareWeight, const double aCost,
                                                 const int aPeriod ) const
{
    /*!
     * \pre A valid logit exponent has been set.
     */
    assert( mLogitExponent[ aPeriod ] <= 0 );
    
    // Zero share weight implies no share which is signaled by negative infinity.
    const double minInf = -std::numeric_limits<double>::infinity();
    double logShareWeight = aShareWeight > 0.0 ? log( aShareWeight ) : minInf;

    // Negative costs are not allowed so they are instead capped at getMinCostThreshold()
    double cappedCost = std::max( aCost, getMinCostThreshold() );
    
    return logShareWeight + mLogitExponent[ aPeriod ] * log( cappedCost );
    // This log is the difference between the relative cost    --^
    // logit and the absolute cost logit.
}

double RelativeCostLogit::calcAverageCost( const double aUnnormalizedShareSum,
                                            const int aPeriod ) const
{
    const double minInf = -std::numeric_limits<double>::infinity();
    double ret;
    if( mLogitExponent[ aPeriod ] == 0.0 ) {
        // TODO: what to do with zero logit?
        ret = 1.0;
    }
    else if( ( aUnnormalizedShareSum == minInf || aUnnormalizedShareSum == 0 ) && mLogitExponent[ aPeriod ] < 0 ) {
        // No Valid options and negative logit so return a large cost so a nested
        // logit would not want to choose this nest.
        ret = util::getLargeNumber();
    }
    else if( ( aUnnormalizedShareSum == minInf || aUnnormalizedShareSum == 0 ) && mLogitExponent[ aPeriod ] > 0 ) {
        // No Valid options and positive logit so return a large negative cost
        // so a nested logit would not want to choose this nest.
        ret = -util::getLargeNumber();
    }
    else {
        ret = pow( aUnnormalizedShareSum, 1.0 / mLogitExponent[ aPeriod ] );
    }

    return ret;
}

/*!
 * \brief Share weight calculation for the relative cost logit.
 * \details  Given an an "anchor" choice with observed share and price and another choice
 *           also with observed share and price, compute the inverse of the discrete choice function
 *           to produce a share weight.
 * \param aShare observed share for the current choice.
 * \param aCost observed cost for the current choice.
 * \param aAnchorShare observed share for the anchor choice.
 * \param aAnchorCost observed cost for the anchor choice.
 * \param aPeriod model time period for the calculation.
 * \return share weight for the current choice.
 */
double RelativeCostLogit::calcShareWeight( const double aShare, const double aCost, const double aAnchorShare,
                                           const double aAnchorCost, const int aPeriod ) const
{
    // Negative costs are not allowed so they are instead capped at getMinCostThreshold()
    double cappedCost = std::max( aCost, getMinCostThreshold() );
    double cappedAnchorCost = std::max( aAnchorCost, getMinCostThreshold() );
    
    return ( aShare / aAnchorShare ) * pow( cappedAnchorCost / cappedCost, mLogitExponent[ aPeriod ] );
}

double RelativeCostLogit::calcShareWeight( const double aShare, const double aCost, const int aPeriod ) const {
    // Negative costs are not allowed so they are instead capped at getMinCostThreshold()
    double cappedCost = std::max( aCost, getMinCostThreshold() );
    return ( aShare ) * pow( mOutputCost / cappedCost, mLogitExponent[ aPeriod ] );
}

double RelativeCostLogit::calcImpliedCost( const double aShare, const double aCost, const int aPeriod ) const {
    // Negative costs are not allowed so they are instead capped at getMinCostThreshold()
    double cappedCost = std::max( aCost, getMinCostThreshold() );
    return mLogitExponent[ aPeriod ] == 0.0 ? cappedCost
        : ( cappedCost ) * pow( aShare, 1.0 / mLogitExponent[ aPeriod ] );
}

void RelativeCostLogit::setOutputCost( const double aCost ) {
    // Negative costs are not allowed so they are instead capped at getMinCostThreshold()
    mOutputCost = std::max( aCost, getMinCostThreshold() );
}

void RelativeCostLogit::setBaseCost( const double aBaseCost ) {
    // the relative cost logit does not utilize this parameter.
}

/*!
 * \brief Get the minimum cost threshold value that may be used in this logit share
 *        equation.
 * \return The threshold value.
 */
double RelativeCostLogit::getMinCostThreshold() {
    const double MIN_COST = 0.001;
    return MIN_COST;
}
