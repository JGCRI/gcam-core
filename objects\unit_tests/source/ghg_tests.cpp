/*! 
* \file ghg_tests.cpp
* \ingroup Unit_Tests
* \brief Unit tests for the Ghg class
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>
#include "emissions/include/ghg.h"

using namespace boost::unit_test_framework;
using namespace std;

namespace GHGTestsNS {
    void constructorTest(){
        const string ghgName = "testGHG";
        const string ghgUnit = "ghgUnit";
        const double rmFrac = 0.5;
        const double gwp = 0.8;
        const double coef = 0.5;

        Ghg* ghg = new Ghg( ghgName, ghgUnit, rmFrac, gwp, coef );
        BOOST_CHECK_EQUAL( ghgName, ghg->getName() );
        BOOST_CHECK_EQUAL( ghgUnit, ghg->getUnit() );
        BOOST_CHECK_EQUAL( coef, ghg->getemiss_coef() );
    }

    test_suite* init_unit_test_suite() {
        test_suite* test = BOOST_TEST_SUITE( "ghg tests" );

        test->add( BOOST_TEST_CASE( &constructorTest ) );

        return test;
    }

}