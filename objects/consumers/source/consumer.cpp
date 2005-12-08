/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Laboratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
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
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNode.hpp>

#include "consumers/include/consumer.h"
#include "util/base/include/xml_helper.h"
#include "functions/include/input.h"
#include "functions/include/ifunction.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "functions/include/function_manager.h"
#include "util/base/include/ivisitor.h"
#include "emissions/include/ghg.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//!< Default Constructor
Consumer::Consumer() {
}

//! Calculate Demand
void Consumer::calcInputDemand( double aConsumption, const string& aRegionName, 
							    const string& aSectorName, int aPeriod ) 
{
    // Consumer production is never scaled.
    const double CONSUMER_SCALE_FACTOR = 1;
    mOutputs[ aPeriod ] = prodDmdFn ? prodDmdFn->calcDemand( input, aConsumption, aRegionName,
                                                             aSectorName,  CONSUMER_SCALE_FACTOR,
                                                             aPeriod ) : 0;
}

void Consumer::updateMarketplace( const string& aSectorName, const string& aRegionName, const int aPeriod ) {
	Marketplace* marketplace = scenario->getMarketplace();
	double totalDemand = 0;
	for( unsigned int i = 0; i < input.size(); i++ ) {
		// don't add govement deficit to marketplace demand
		if( !input[i]->isCapital() ) {
			double tempDemand = input[ i ]->getDemandCurrency();
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
        (*ghg)->calcEmission( input, aRegionName, aGoodName, 0, aPeriod );
    }
}

void Consumer::accept( IVisitor* aVisitor, const int aPeriod ) const {
    BaseTechnology::accept( aVisitor, aPeriod );
    aVisitor->updateConsumer( this, aPeriod );
}
