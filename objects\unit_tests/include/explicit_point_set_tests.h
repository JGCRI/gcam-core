#ifndef _EXPLICIT_POINT_SET_TESTS_H_
#define _EXPLICIT_POINT_SET_TESTS_H_

/*! 
* \file explicit_point_set_tests.h
* \ingroup Unit_Tests
* \brief Contains the header for the ExplicitPointSetTests class.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <cppunit/extensions/HelperMacros.h>
#include <vector>
#include "util/curves/include/explicit_point_set.h"

class ExplicitPointSetTests : public CppUnit::TestFixture
{
    CPPUNIT_TEST_SUITE( ExplicitPointSetTests );
    CPPUNIT_TEST( testConstructor );
    CPPUNIT_TEST( testCopy );
    CPPUNIT_TEST( testAssignment );
    CPPUNIT_TEST( testDestructor );
    CPPUNIT_TEST( testEquality );
    CPPUNIT_TEST( testInequality );
    CPPUNIT_TEST( testAddPoint );
    CPPUNIT_TEST( testGetY );
    CPPUNIT_TEST( testGetX );
    CPPUNIT_TEST( testSetY );
    CPPUNIT_TEST( testSetX );
    // CPPUNIT_TEST( testRemovePointFindX );
    // CPPUNIT_TEST( testRemovePointFindY );
    CPPUNIT_TEST( testGetXCoords );
    CPPUNIT_TEST( testGetYCoords );
    CPPUNIT_TEST( testContainsX );
    CPPUNIT_TEST( testContainsY );
    // CPPUNIT_TEST( testGetNearestXBelow );
    // CPPUNIT_TEST( testGetNearestXAbove );
    // CPPUNIT_TEST( testGetNearestYBelow );
    // CPPUNIT_TEST( testGetNearestYAbove );
    CPPUNIT_TEST_SUITE_END();

public:
    void setUp();
    void tearDown();
    void testConstructor();
    void testCopy();
    void testAssignment();
    void testDestructor();
    void testEquality();
    void testInequality();
    void testAddPoint();
    void testGetY();
    void testGetX();
    void testSetY();
    void testSetX();
    void testRemovePointFindX();
    void testRemovePointFindY();
    void testGetXCoords();
    void testGetYCoords();
    void testContainsX();
    void testContainsY();
    void testGetNearestXBelow();
    void testGetNearestXAbove();
    void testGetNearestYBelow();
    void testGetNearestYAbove();
private:
    std::vector<ExplicitPointSet::DataPoint*> p; //!< A container for a common set of test datapoints. 
};

#endif  // _EXPLICIT_POINT_SET_TESTS_H_
