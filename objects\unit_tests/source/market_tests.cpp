/*! 
* \file market_tests.cpp
* \ingroup Unit_Tests
* \brief Unit tests for the Market class
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>
#include "marketplace/include/market.h"
#include "marketplace/include/normal_market.h"

#include <iostream>

using namespace boost::unit_test_framework;
using namespace std;

namespace MarketTestsNS {
    void testConstructor(){
    }

    void testSetAndGetMarketInfo() {
        Market* market = new NormalMarket( "goodName", "regionName", 1 );
        const string ITEM_ONE_NAME = "ITEM1";
        const string ITEM_TWO_NAME = "ITEM2";
        const double VALUE1 = 2.3;
        const double VALUE2 = 4.1;
        const double VALUE3 = 9.1;

        // Test simple addItem and getItem
        market->setMarketInfo( ITEM_ONE_NAME, VALUE1 );
        BOOST_CHECK_EQUAL( market->getMarketInfo( ITEM_ONE_NAME ), VALUE1 );

        // Now add the same thing and make sure the value does not change.
        market->setMarketInfo( ITEM_ONE_NAME, VALUE1 );
        BOOST_CHECK_EQUAL( market->getMarketInfo( ITEM_ONE_NAME ), VALUE1 );

        // Add another item and make sure they both are correct.
        market->setMarketInfo( ITEM_TWO_NAME, VALUE2 );
        BOOST_CHECK_EQUAL( market->getMarketInfo( ITEM_ONE_NAME ), VALUE1 );
        BOOST_CHECK_EQUAL( market->getMarketInfo( ITEM_TWO_NAME ), VALUE2 );

        // Update an item and make sure the update is performed correctly.
        market->setMarketInfo( ITEM_ONE_NAME, VALUE3 );
        BOOST_CHECK_EQUAL( market->getMarketInfo( ITEM_ONE_NAME ), VALUE3 );
        BOOST_CHECK_EQUAL( market->getMarketInfo( ITEM_TWO_NAME ), VALUE2 );

        // Fetch a non-existant value.
        cout << "There should be an error message next: " << endl;
        BOOST_CHECK_EQUAL( market->getMarketInfo( "junk" ), 0.0 );

        delete market;
    }

    test_suite* init_unit_test_suite() {
        test_suite* test = BOOST_TEST_SUITE( "market tests" );

        test->add( BOOST_TEST_CASE( &testConstructor ) );
        test->add( BOOST_TEST_CASE( &testSetAndGetMarketInfo ) );
        return test;
    }
}