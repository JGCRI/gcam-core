/*! 
* \file marketplace_tests.cpp
* \ingroup Unit_Tests
* \brief Unit tests for the Marketplace class
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>
#include "marketplace/include/marketplace.h"

using namespace std;
using namespace boost::unit_test_framework;

namespace MarketplaceTestsNS {
    void testConstructor(){
    }

    void testGetMarketInfo(){
    }

    void testSetMarketInfo(){
    }

    test_suite* init_unit_test_suite() {
        test_suite* test = BOOST_TEST_SUITE( "marketplace tests" );

        test->add( BOOST_TEST_CASE( &testConstructor ) );
        test->add( BOOST_TEST_CASE( &testGetMarketInfo ) );
        test->add( BOOST_TEST_CASE( &testSetMarketInfo ) );
        return test;
    }
}