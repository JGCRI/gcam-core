#ifndef _MARKET_TESTS_H_
#define _MARKET_TESTS_H_

#include <cppunit/extensions/HelperMacros.h>

class MarketTests : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE( MarketTests );
  CPPUNIT_TEST( testConstructor );
  CPPUNIT_TEST( testSetAndGetMarketInfo );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testConstructor();
  void testSetAndGetMarketInfo();
};

#endif  // _MARKET_TESTS_H_
