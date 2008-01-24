/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
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