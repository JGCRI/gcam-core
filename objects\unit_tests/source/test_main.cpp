#include <boost/test/unit_test.hpp>
#include <iostream>
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <ctime>

using namespace std;
using boost::unit_test_framework::test_suite;

namespace ExplicitPointSetTestsNS {
   test_suite* init_unit_test_suite();
}
namespace GHGTestsNS {
    test_suite* init_unit_test_suite();
}
namespace MarketInfoTestsNS {
    test_suite* init_unit_test_suite();
}
namespace MarketTestsNS {
    test_suite* init_unit_test_suite();
}
namespace MarketplaceTestsNS {
    test_suite* init_unit_test_suite();
}
namespace XYDataPointTestsNS {
    test_suite* init_unit_test_suite();
}
/* \todo Finish removing globals-JPL */
ofstream bugoutfile, outfile, logfile;	

Scenario* scenario = 0; // model scenario info
time_t ltime;
int Tabs::numTabs = 0;
xercesc::XercesDOMParser* XMLHelper<void>::parser = 0;
xercesc::ErrorHandler* XMLHelper<void>::errHandler = 0;

test_suite* init_unit_test_suite( int argc, char* argv[] ) {
    test_suite* test= BOOST_TEST_SUITE( "Master test suite" );
    test->add( ExplicitPointSetTestsNS::init_unit_test_suite() );
    test->add( GHGTestsNS::init_unit_test_suite() );
    test->add( MarketInfoTestsNS::init_unit_test_suite() );
    test->add( MarketTestsNS::init_unit_test_suite() );
    test->add( MarketplaceTestsNS::init_unit_test_suite() );
    test->add( XYDataPointTestsNS::init_unit_test_suite() );
    return test;
}


