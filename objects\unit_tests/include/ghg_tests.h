#ifndef _GHG_TESTS_H_
#define _GHG_TESTS_H_

#include <cppunit/extensions/HelperMacros.h>

class GHGTests : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE( GHGTests );
  CPPUNIT_TEST( testConstructor );
  CPPUNIT_TEST( testIndirectEmissions );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testConstructor();
  void testIndirectEmissions();
};

#endif  // _GHG_TESTS_H_
