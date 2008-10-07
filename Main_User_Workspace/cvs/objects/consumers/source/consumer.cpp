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
* \file consumer.cpp
* \ingroup Objects-SGM
* \brief The Consumer class source file.
*
*  Detailed Description.
*
* \author Pralit Patel
* \author Sonny Kim
*/
#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNode.hpp>

#include "consumers/include/consumer.h"
#include "util/base/include/xml_helper.h"
#include "functions/include/iinput.h"
#include "functions/include/ifunction.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "functions/include/function_manager.h"
#include "util/base/include/ivisitor.h"
#include "technologies/include/primary_output.h"
#include "emissions/include/aghg.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

typedef vector<AGHG*>::const_iterator CGHGIterator;
typedef vector<AGHG*>::iterator GHGIterator;

//!< Default Constructor
Consumer::Consumer() {
}

void Consumer::initCalc( const MoreSectorInfo* aMoreSectorInfo,
                         const string& aRegionName, 
                         const string& aSectorName,
                         NationalAccount& nationalAccount,
                         const Demographic* aDemographics,
                         const double aCapitalStock,
                         const int aPeriod )
{
    BaseTechnology::initCalc( aMoreSectorInfo, aRegionName, aSectorName,
                              nationalAccount, aDemographics, aCapitalStock,
                              aPeriod );
}

//! Calculate Demand
void Consumer::calcInputDemand( double aConsumption, const string& aRegionName, 
                                const string& aSectorName, int aPeriod ) 
{
    // Consumer production is never scaled.
    const double CONSUMER_SCALE_FACTOR = 1;
    double output = prodDmdFn ? prodDmdFn->calcDemand( input, aConsumption, aRegionName,
                                                       aSectorName,  CONSUMER_SCALE_FACTOR,
                                                       aPeriod ) : 0;
    mOutputs[ 0 ]->setCurrencyOutput( aRegionName, output, aPeriod );

}

void Consumer::updateMarketplace( const string& aSectorName, const string& aRegionName, const int aPeriod ) {
    Marketplace* marketplace = scenario->getMarketplace();
    double totalDemand = 0;
    for( unsigned int i = 0; i < input.size(); i++ ) {
        // don't add govement deficit to marketplace demand
        // TODO: it would be better to check the type but that will not be set until
        //  initCalc
        if( input[i]->getName() != "Capital" ) {
            double tempDemand = input[ i ]->getCurrencyDemand( aPeriod );
            assert( util::isValidNumber( tempDemand ) );
            if( tempDemand < 0 ){
                cout << "Error: Trying to add a negative demand currency to the marketplace for " << input[ i ]->getName() << endl;
            }

            marketplace->addToDemand( input[ i ]->getName(), aRegionName, tempDemand, aPeriod );
            totalDemand += tempDemand;
        }
    }
    marketplace->addToSupply( aSectorName, aRegionName, totalDemand, aPeriod, false );
}

void Consumer::calcEmissions( const string& aGoodName, const string& aRegionName, const int aPeriod ) {
    // Loop over GHGs and calculate emissions.
    for( GHGIterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
        assert( *ghg );
        // Consumers have no physical output so pass in zero.
        (*ghg)->calcEmission( aRegionName, input, mOutputs, 0, 0, aPeriod );
    }
}

void Consumer::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitConsumer( this, aPeriod );
    BaseTechnology::accept( aVisitor, aPeriod );
    aVisitor->endVisitConsumer( this, aPeriod );
}
