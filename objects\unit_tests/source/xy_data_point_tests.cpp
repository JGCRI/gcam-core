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

#include "util/curves/include/xy_data_point.h"

using namespace boost::unit_test_framework;
using namespace std;

namespace XYDataPointTestsNS {
    //! Test the constructor. 
    void testConstructor(){
        const double Y = 0.2;
        const double X = -1;

        // First test default constructor.
        XYDataPoint* p = new XYDataPoint();
        BOOST_CHECK_EQUAL( p->getX(), 0.0 );
        BOOST_CHECK_EQUAL( p->getY(), 0.0 );
        delete p;

        // Test 2 arg constructor
        p = new XYDataPoint( X, Y );
        BOOST_CHECK_EQUAL( p->getX(), X );
        BOOST_CHECK_EQUAL( p->getY(), Y );
        delete p;
    }

    //! Test the clone function.
    void testClone() {
        const double Y = 1.9;
        const double X = 2.5;
        XYDataPoint* p = new XYDataPoint( X, Y );
        ExplicitPointSet::DataPoint* q = p->clone();
        BOOST_CHECK_EQUAL( *( dynamic_cast<XYDataPoint*>( q ) ), *p );
        delete p;
        delete q;
    }

    //! Test the setX function.
    void testSetX(){
        const double X = 1.4;

        XYDataPoint* p = new XYDataPoint();
        p->setX( X );
        BOOST_CHECK_EQUAL( p->getX(), X );
        delete p;
    }

    //! Test the setY function.
    void testSetY(){
        const double Y = 1.4;

        XYDataPoint* p = new XYDataPoint();
        p->setY( Y );
        BOOST_CHECK_EQUAL( p->getY(), Y );
        delete p;
    }

    //! Test the equals operator.
    void testEquals(){
        const double X1 = 1.2;
        const double Y1 = 5;
        const double X2 = 63.3;
        const double Y2 = 12.2;

        ExplicitPointSet::DataPoint* dp1 = new XYDataPoint( X1, Y1 );
        ExplicitPointSet::DataPoint* dp2 = new XYDataPoint( X2, Y2 );
        ExplicitPointSet::DataPoint* dp3 = new XYDataPoint( X2, Y2 );

        BOOST_CHECK( *dp1 != *dp2 );
        BOOST_CHECK_EQUAL( *dp2, *dp3 );

        delete dp1;
        delete dp2;
        delete dp3;
    }

    test_suite* init_unit_test_suite() {
        test_suite* test = BOOST_TEST_SUITE( "XYDataPoint tests" );

        test->add( BOOST_TEST_CASE( &testConstructor ) );
        test->add( BOOST_TEST_CASE( &testClone ) );
        test->add( BOOST_TEST_CASE( &testSetX ) );
        test->add( BOOST_TEST_CASE( &testSetY ) );
        test->add( BOOST_TEST_CASE( &testEquals ) );
        return test;
    }
}