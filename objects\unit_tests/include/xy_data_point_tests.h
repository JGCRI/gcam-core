#ifndef _XY_DATA_POINT_TESTS_H_
#define _XY_DATA_POINT_TESTS_H_

#include <cppunit/extensions/HelperMacros.h>

class XYDataPointTests : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE( XYDataPointTests );
  CPPUNIT_TEST( testConstructor );
  CPPUNIT_TEST( testClone );
  CPPUNIT_TEST( testSetX );
  CPPUNIT_TEST( testSetY );
  CPPUNIT_TEST( testEquals );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testConstructor();
  void testClone();
  void testSetX();
  void testSetY();
  void testEquals();
};

#endif  // _GHG_TESTS_H_
