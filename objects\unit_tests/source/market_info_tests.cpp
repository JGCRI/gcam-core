/*! 
* \file market_info_tests.cpp
* \ingroup Unit_Tests
* \brief Unit tests for the MarketInfo class
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp> 
#include <string>
#include <iostream>
#include "marketplace/include/market_info.h"

using namespace boost::unit_test_framework;
using namespace std;

namespace MarketInfoTestsNS {

    void testConstructor(){
        MarketInfo* marketInfo = new MarketInfo();
        MarketInfo* marketInfo2 = new MarketInfo();

        // BOOST_CHECK_EQUAL( *marketInfo, *marketInfo2 );

        delete marketInfo;
        delete marketInfo2;
    }

    void testAddAndGetItem() {
        MarketInfo* marketInfo = new MarketInfo();
        const string ITEM_ONE_NAME = "ITEM1";
        const string ITEM_TWO_NAME = "ITEM2";
        const double VALUE1 = 2.3;
        const double VALUE2 = 4.1;
        const double VALUE3 = 9.1;

        // Test simple addItem and getItem
        marketInfo->addItem( ITEM_ONE_NAME, VALUE1 );
        BOOST_CHECK_EQUAL( marketInfo->getItemValue( ITEM_ONE_NAME ), VALUE1 );

        // Now add the same thing and make sure the value does not change.
        marketInfo->addItem( ITEM_ONE_NAME, VALUE1 );
        BOOST_CHECK_EQUAL( marketInfo->getItemValue( ITEM_ONE_NAME ), VALUE1 );

        // Add another item and make sure they both are correct.
        marketInfo->addItem( ITEM_TWO_NAME, VALUE2 );
        BOOST_CHECK_EQUAL( marketInfo->getItemValue( ITEM_ONE_NAME ), VALUE1 );
        BOOST_CHECK_EQUAL( marketInfo->getItemValue( ITEM_TWO_NAME ), VALUE2 );

        // Update an item and make sure the update is performed correctly.
        marketInfo->addItem( ITEM_ONE_NAME, VALUE3 );
        BOOST_CHECK_EQUAL( marketInfo->getItemValue( ITEM_ONE_NAME ), VALUE3 );
        BOOST_CHECK_EQUAL( marketInfo->getItemValue( ITEM_TWO_NAME ), VALUE2 );

        // Fetch a non-existant value.
        cout << "There should be an error message next: " << endl;
        BOOST_CHECK_EQUAL( marketInfo->getItemValue( "junk" ), 0.0 );

        delete marketInfo;
    }

    test_suite* init_unit_test_suite() {
        test_suite* test = BOOST_TEST_SUITE( "market info tests" );

        test->add( BOOST_TEST_CASE( &testConstructor ) );
        test->add( BOOST_TEST_CASE( &testAddAndGetItem ) );
        return test;
    }
}