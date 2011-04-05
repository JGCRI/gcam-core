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
 * \file summer_carbon_stock.cpp
 * \ingroup Objects
 * \brief SummerCarbonStock class source file.
 * \author Jim Naslund and Ming Chang
 */

#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "ccarbon_model/include/luc_carbon_summer_stock.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "ccarbon_model/include/acarbon_flow.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_helper.h"
#include "ccarbon_model/include/environmental_info.h"

using namespace std;
using namespace xercesc;

/*! 
 * \brief Default Constructor
 */
SummerCarbonStock::SummerCarbonStock(){
}

SummerCarbonStock::SummerCarbonStock( const SummerCarbonStock& aSummerCarbonStock ){
}

SummerCarbonStock::~SummerCarbonStock(){
}

bool SummerCarbonStock::XMLParse( const xercesc::DOMNode* aNode ){
    // SummerCarbonStocks are not parsed.
    assert( false );
    return false;
}

double SummerCarbonStock::transfer( const EnvironmentalInfo* aEnvInfo,
                                    FlowType aFlowType, const int aYear ){
    // This is a flow from the luc summer to a bunch of carbon models.  We
    // want to deplete the entire stock.
   if( aFlowType == eLUCFlowIn ){
        double carbonLost = mCarbonStock[ aYear ];
        mCarbonStock[ aYear ] = 0;
        return carbonLost;
    }
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Summer Carbon Stock encountered unknown flow type." << endl;
        return 0;
    }
}

SummerCarbonStock* SummerCarbonStock::clone() const{
    return new SummerCarbonStock( *this );
}
