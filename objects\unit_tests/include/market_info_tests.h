#ifndef _MARKET_INFO_TESTS_H_
#define _MARKET_INFO_TESTS_H_

#include <cppunit/extensions/HelperMacros.h>

class MarketInfoTests : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE( MarketInfoTests );
  CPPUNIT_TEST( testConstructor );
  CPPUNIT_TEST( testAddAndGetItem );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testConstructor();
  void testAddAndGetItem();
};

#endif  // _GHG_TESTS_H_
