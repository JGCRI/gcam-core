#ifndef _MARKETPLACE_TESTS_H_
#define _MARKETPLACE_TESTS_H_

#include <cppunit/extensions/HelperMacros.h>

class MarketplaceTests : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE( MarketplaceTests );
  CPPUNIT_TEST( testConstructor );
  CPPUNIT_TEST( testSetMarketInfo );
  CPPUNIT_TEST( testGetMarketInfo );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testConstructor();
  void testSetMarketInfo();
  void testGetMarketInfo();
};

#endif  // _MARKETPLACE_TESTS_H_
