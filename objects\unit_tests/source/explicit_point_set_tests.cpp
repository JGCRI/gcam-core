/*! 
* \file explicit_point_set_tests.cpp
* \ingroup Unit_Tests
* \brief Unit tests for the explicit point set.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>
#include <vector>
#include <iostream>

#include "util/curves/include/explicit_point_set.h"
#include "util/curves/include/xy_data_point.h"

using namespace boost::unit_test_framework;
using namespace std;

namespace ExplicitPointSetTestsNS {
    //! Test the ExplicitPointSet constructor.
    void testConstructor(){
        ExplicitPointSet* pointSet = new ExplicitPointSet();
        BOOST_CHECK( pointSet != 0 ); 
        delete pointSet;
    }

    //! Test the copy constructor.
    void testCopy(){
        vector<ExplicitPointSet::DataPoint*> p; 
        p.resize( 20 );
        for( int i = 0; i < 20; i++ ){
            p[ i ] = new XYDataPoint( i, i + 20 );
        }

        ExplicitPointSet* pointSet1 = new ExplicitPointSet();
        pointSet1->addPoint( new XYDataPoint( p[ 1 ]->getX(), p[ 1 ]->getY() ) );
        pointSet1->addPoint( new XYDataPoint( p[ 2 ]->getX(), p[ 2 ]->getY() ) );
        pointSet1->addPoint( new XYDataPoint( p[ 3 ]->getX(), p[ 3 ]->getY() ) );
        ExplicitPointSet* pointSet2 = new ExplicitPointSet( *pointSet1 );
        BOOST_CHECK_EQUAL( *pointSet1, *pointSet2 );
        delete pointSet1;
        delete pointSet2;

        for( vector<ExplicitPointSet::DataPoint*>::iterator i = p.begin(); i != p.end(); i++ ){
            delete * i;
        }
    }

    //! Test the assignment operator. 
    void testAssignment(){
        vector<ExplicitPointSet::DataPoint*> p; 
        p.resize( 20 );
        for( int i = 0; i < 20; i++ ){
            p[ i ] = new XYDataPoint( i, i + 20 );
        }
        ExplicitPointSet* pointSet1 = new ExplicitPointSet();
        ExplicitPointSet* pointSet2 = new ExplicitPointSet();

        pointSet1->addPoint( p[ 1 ]->clone() );
        pointSet1->addPoint( p[ 2 ]->clone() );
        pointSet1->addPoint( p[ 3 ]->clone() );


        pointSet2->addPoint( p[ 4 ]->clone() );
        pointSet2->addPoint( p[ 5 ]->clone() );

        *pointSet1 = *pointSet2;
        BOOST_CHECK_EQUAL( *pointSet2, *pointSet1 );

        delete pointSet1;
        delete pointSet2;

        for( vector<ExplicitPointSet::DataPoint*>::iterator i = p.begin(); i != p.end(); i++ ){
            delete * i;
        }
    }

    //! Test the destructor.
    void testDestructor(){
        ExplicitPointSet* pointSet1 = new ExplicitPointSet();
        pointSet1->addPoint( new XYDataPoint( 1, 3 ) );
        pointSet1->addPoint( new XYDataPoint( 2, 4 ) );
        delete pointSet1;
    }

    //! Test the equality operator.
    void testEquality(){
        vector<ExplicitPointSet::DataPoint*> p; 
        p.resize( 20 );
        for( int i = 0; i < 20; i++ ){
            p[ i ] = new XYDataPoint( i, i + 20 );
        }
        ExplicitPointSet* ps1 = new ExplicitPointSet();
        ExplicitPointSet* ps2 = new ExplicitPointSet();

        ps1->addPoint( p[ 1 ]->clone() );
        ps1->addPoint( p[ 2 ]->clone() );
        ps1->addPoint( p[ 3 ]->clone() );

        ps2->addPoint( p[ 1 ]->clone() );
        ps2->addPoint( p[ 2 ]->clone() );
        ps2->addPoint( p[ 3 ]->clone() );

        BOOST_CHECK_EQUAL( *ps1, *ps2 );

        ps1->addPoint( p[ 4 ]->clone() );

        BOOST_CHECK( !( *ps1 == *ps2 ) );

        delete ps1;
        delete ps2;

        for( vector<ExplicitPointSet::DataPoint*>::iterator i = p.begin(); i != p.end(); i++ ){
            delete * i;
        }
    }

    //! Test the inequality operator.
    void testInequality(){
        vector<ExplicitPointSet::DataPoint*> p; 
        p.resize( 20 );
        for( int i = 0; i < 20; i++ ){
            p[ i ] = new XYDataPoint( i, i + 20 );
        }
        ExplicitPointSet* ps1 = new ExplicitPointSet();
        ExplicitPointSet* ps2 = new ExplicitPointSet();

        ps1->addPoint( p[ 1 ]->clone() );
        ps1->addPoint( p[ 4 ]->clone() );
        ps1->addPoint( p[ 1 ]->clone() );

        ps2->addPoint( p[ 1 ]->clone() );
        ps2->addPoint( p[ 4 ]->clone() );
        ps2->addPoint( p[ 1 ]->clone() );

        BOOST_CHECK( !( *ps1 != *ps2 ) );

        ps1->addPoint( p[ 5 ]->clone() );
        BOOST_CHECK( *ps1 != *ps2 );

        delete ps1;
        delete ps2;
        for( vector<ExplicitPointSet::DataPoint*>::iterator i = p.begin(); i != p.end(); i++ ){
            delete * i;
        }
    }

    //! Test the addPoint function which adds a new datapoint to the vector. 
    void testAddPoint(){

        // Set up some vectors of points.
        vector<double> x;
        vector<double> y;

        x.push_back( -4.2 );
        x.push_back( 3 );
        x.push_back( 3 );
        x.push_back( 3 );

        y.push_back( -12.3 );
        y.push_back( 7 );
        y.push_back( 7 );
        y.push_back( 12.3 );

        // Add them to the vector.
        ExplicitPointSet* ps1 = new ExplicitPointSet();
        XYDataPoint* dp1 = new XYDataPoint( x[ 0 ], y[ 0 ] );
        XYDataPoint* dp2 = new XYDataPoint( x[ 1 ], y[ 1 ] );
        XYDataPoint* dp3 = new XYDataPoint( x[ 2 ], y[ 2 ] );
        XYDataPoint* dp4 = new XYDataPoint( x[ 3 ], y[ 3 ] );

        BOOST_CHECK( ps1->addPoint( dp1 ) ); 
        BOOST_CHECK( ps1->addPoint( dp2 ) );

        // This is a duplicate point and should not be added.
        BOOST_CHECK( !ps1->addPoint( dp3 ) );
        // Need to deallocate since the set is not responsible for the memory.
        delete dp3;

        BOOST_CHECK( ps1->addPoint( dp4 ) );

        // Remove the duplicate points.
        vector<double>::iterator removeX = x.begin() + 2;
        x.erase( removeX );

        vector<double>::iterator removeY = y.begin() + 2;
        y.erase( removeY );

        // Check if things were added correctly.
        BOOST_CHECK( ps1->getXCoords() == x );
        BOOST_CHECK( ps1->getYCoords() == y );

        delete ps1;
    }

    //! Test getting a Y coordinate
    void testGetY(){
        vector<ExplicitPointSet::DataPoint*> p; 
        p.resize( 20 );
        for( int i = 0; i < 20; i++ ){
            p[ i ] = new XYDataPoint( i, i + 20 );
        }
        // Set up a test point set.
        ExplicitPointSet* ps1 = new ExplicitPointSet();
        ps1->addPoint( p[ 0 ]->clone() );
        ps1->addPoint( p[ 1 ]->clone() );
        ps1->addPoint( p[ 2 ]->clone() );

        // Test getting a point.
        BOOST_CHECK_EQUAL( ps1->getY( p[ 0 ]->getX() ), p[ 0 ]->getY() );

        // Test another
        BOOST_CHECK_EQUAL( ps1->getY( p[ 1 ]->getX() ), p[ 1 ]->getY() );

        // Test a non-existant point.
        BOOST_CHECK_EQUAL( ps1->getY( 1000.0 ), DBL_MAX );

        delete ps1;

        for( vector<ExplicitPointSet::DataPoint*>::iterator i = p.begin(); i != p.end(); i++ ){
            delete * i;
        }
    }

    //! Test getting an X coordinate.
    void testGetX(){
        vector<ExplicitPointSet::DataPoint*> p; 
        p.resize( 20 );
        for( int i = 0; i < 20; i++ ){
            p[ i ] = new XYDataPoint( i, i + 20 );
        }
        // Set up a test point set.
        ExplicitPointSet* ps1 = new ExplicitPointSet();
        ps1->addPoint( p[ 0 ]->clone() );
        ps1->addPoint( p[ 1 ]->clone() );
        ps1->addPoint( p[ 2 ]->clone() );

        // Test getting a point.
        BOOST_CHECK_EQUAL( ps1->getX( p[ 0 ]->getY() ), p[ 0 ]->getX() );

        // Test another
        BOOST_CHECK_EQUAL( ps1->getX( p[ 1 ]->getY() ), p[ 1 ]->getX() );

        // Test a non-existant point.
        BOOST_CHECK_EQUAL( ps1->getX( 1000.0 ), DBL_MAX );

        delete ps1;

        for( vector<ExplicitPointSet::DataPoint*>::iterator i = p.begin(); i != p.end(); i++ ){
            delete * i;
        }
    }

    //! Test setting an X coordinate.
    void testSetY(){
        // Create vectors of x and y points.
        vector<double> x( 3 );
        vector<double> y( 3 );
        x[ 0 ] = 2.4;
        x[ 1 ] = 3.2;
        x[ 2 ] = -2.9;
        y[ 0 ] = 4.2;
        y[ 1 ] = 2.0;
        y[ 2 ] = 1502.2;

        ExplicitPointSet* ps1 = new ExplicitPointSet();
        ps1->addPoint( new XYDataPoint( x[ 0 ], y[ 0 ] ) );
        ps1->addPoint( new XYDataPoint( x[ 1 ], y[ 1 ] ) );
        ps1->addPoint( new XYDataPoint( x[ 2 ], y[ 2 ] ) );

        // Make sure they were added correctly before we run the test.
        BOOST_CHECK( ps1->getXCoords() == x );
        BOOST_CHECK( ps1->getYCoords() == y );

        // Make a change to the y value.
        y[ 1 ] = 99.3;

        // Set the point.
        ps1->setY( x[ 1 ], y[ 1 ] );

        // Check if the new value is correct.
        BOOST_CHECK( ps1->getXCoords() ==  x );
        BOOST_CHECK( ps1->getYCoords() == y );

        delete ps1;
    }
    //! Test setting an X coordinate.
    void testSetX(){
        // Create vectors of x and y points.
        vector<double> x( 3 );
        vector<double> y( 3 );
        x[ 0 ] = 2.4;
        x[ 1 ] = 3.2;
        x[ 2 ] = -2.9;
        y[ 0 ] = 4.2;
        y[ 1 ] = 2.0;
        y[ 2 ] = 1502.2;

        ExplicitPointSet* ps1 = new ExplicitPointSet();
        ps1->addPoint( new XYDataPoint( x[ 0 ], y[ 0 ] ) );
        ps1->addPoint( new XYDataPoint( x[ 1 ], y[ 1 ] ) );
        ps1->addPoint( new XYDataPoint( x[ 2 ], y[ 2 ] ) );

        // Make sure they were added correctly before we run the test.
        BOOST_CHECK( ps1->getXCoords() == x );
        BOOST_CHECK( ps1->getYCoords() == y );

        // Make a change to the x value.
        x[ 1 ] = 99.3;

        // Set the point.
        ps1->setX( y[ 1 ], x[ 1 ] );

        // Check if the new value is correct.
        BOOST_CHECK( ps1->getXCoords() == x );
        BOOST_CHECK( ps1->getYCoords() == y );

        delete ps1;
    }

    //! Test removing a point based on an X value.
    void testRemovePointFindX(){
        // Create vectors of x and y points.
        vector<double> x;
        vector<double> y;
        x.push_back( 2.4 );
        x.push_back( 3.2 );
        x.push_back( 2.9 );
        y.push_back( 4.2 );
        y.push_back( 2.0 );
        y.push_back( 1502.2 );


        ExplicitPointSet* ps1 = new ExplicitPointSet(); 
        ps1->addPoint( new XYDataPoint( x[ 0 ], y[ 0 ] ) );
        ps1->addPoint( new XYDataPoint( x[ 1 ], y[ 1 ] ) );
        ps1->addPoint( new XYDataPoint( x[ 2 ], y[ 2 ] ) );

        // Make sure they were added correctly before we run the test.
        BOOST_CHECK( ps1->getXCoords() == x );
        BOOST_CHECK( ps1->getYCoords() == y );

        // Remove a point.
        BOOST_CHECK( ps1->removePointFindX( x[ 1 ] ) );

        vector<double> xRem( 2 );
        xRem[ 0 ] = x[ 0 ];
        xRem[ 1 ] = x[ 2 ];

        vector<double> yRem( 2 );
        yRem[ 0 ] = y[ 0 ];
        yRem[ 1 ] = y[ 2 ];

        // Check if the new points are corrected
        BOOST_CHECK( ps1->getXCoords() ==  xRem );
        BOOST_CHECK( ps1->getYCoords() == yRem );

        // Try removing a non existant point.
        BOOST_CHECK( !ps1->removePointFindX( 100000 ) );

        // Check if the new points are corrected
        BOOST_CHECK( ps1->getXCoords() ==  xRem );
        BOOST_CHECK( ps1->getYCoords() == yRem );

        delete ps1;
    }

    //! Test removing a datapoint based on a Y value.
    void testRemovePointFindY(){
        // Create vectors of x and y points.
        vector<double> x;
        vector<double> y;
        x.push_back( 2.4 );
        x.push_back( 3.2 );
        x.push_back( 2.9 );
        y.push_back( 4.2 );
        y.push_back( 2.0 );
        y.push_back( 1502.2 );


        ExplicitPointSet* ps1 = new ExplicitPointSet(); 
        ps1->addPoint( new XYDataPoint( x[ 0 ], y[ 0 ] ) );
        ps1->addPoint( new XYDataPoint( x[ 1 ], y[ 1 ] ) );
        ps1->addPoint( new XYDataPoint( x[ 2 ], y[ 2 ] ) );

        // Make sure they were added correctly before we run the test.
        BOOST_CHECK( ps1->getXCoords() == x );
        BOOST_CHECK( ps1->getYCoords() == y );

        // Remove a point.
        BOOST_CHECK( ps1->removePointFindY( y[ 1 ] ) );

        vector<double> xRem( 2 );
        xRem[ 0 ] = x[ 0 ];
        xRem[ 1 ] = x[ 2 ];

        vector<double> yRem( 2 );
        yRem[ 0 ] = y[ 0 ];
        yRem[ 1 ] = y[ 2 ];

        // Check if the new points are corrected
        BOOST_CHECK( ps1->getXCoords() ==  xRem );
        BOOST_CHECK( ps1->getYCoords() == yRem );

        // Try removing a non existant point.
        BOOST_CHECK( !ps1->removePointFindY( 100000 ) );

        // Check if the new points are corrected
        BOOST_CHECK( ps1->getXCoords() ==  xRem );
        BOOST_CHECK( ps1->getYCoords() == yRem );

        delete ps1;
    }

    //! Test getXCoords function. 
    void testGetXCoords(){
        ExplicitPointSet* ps1 = new ExplicitPointSet();
        vector<double> x;
        const double ZERO = 0.0;

        x.push_back( 1.0 );
        x.push_back( 3.25 );
        x.push_back( 3.85 );

        BOOST_CHECK( ps1->addPoint( new XYDataPoint( x[ 0 ], ZERO ) ) );
        BOOST_CHECK( ps1->addPoint( new XYDataPoint( x[ 1 ], ZERO ) ) );
        BOOST_CHECK( ps1->addPoint( new XYDataPoint( x[ 2 ], ZERO ) ) );

        const vector<double> retX = ps1->getXCoords();
        BOOST_CHECK_EQUAL( retX.size(), x.size() );

        for( int i = 0; i < static_cast<int>( x.size() ); i++ ){
            BOOST_CHECK_EQUAL( retX[ i ], x[ i ] );
        }

        delete ps1;
    }

    //! Test getYCoords function.
    void testGetYCoords(){
        ExplicitPointSet* ps1 = new ExplicitPointSet(); 


        vector<double> y;
        const double ZERO = 0;

        y.push_back( 12.5 );
        y.push_back( 2.5 );
        y.push_back( 18.4 );

        BOOST_CHECK( ps1->addPoint( new XYDataPoint( ZERO, y[ 0 ] ) ) ); 
        BOOST_CHECK( ps1->addPoint( new XYDataPoint( ZERO, y[ 1 ]) ) );
        BOOST_CHECK( ps1->addPoint( new XYDataPoint( ZERO, y[ 2 ] ) ) );

        const vector<double> retY = ps1->getYCoords();
        BOOST_CHECK_EQUAL( retY.size(), y.size() );

        for( int i = 0; i < static_cast<int>( y.size() ); i++ ){
            BOOST_CHECK_EQUAL( retY[ i ], y[ i ] );
        }
        delete ps1;
    }

    //! Test containsX function
    void testContainsX() {
        vector<ExplicitPointSet::DataPoint*> p; 
        p.resize( 20 );
        for( int i = 0; i < 20; i++ ){
            p[ i ] = new XYDataPoint( i, i + 20 );
        }
        ExplicitPointSet* ps1 = new ExplicitPointSet();

        ps1->addPoint( p[ 0 ]->clone() );
        ps1->addPoint( p[ 1 ]->clone() );
        ps1->addPoint( p[ 2 ]->clone() );

        BOOST_CHECK( ps1->containsX( p[ 0 ]->getX() ) );
        BOOST_CHECK( ps1->containsX( p[ 1 ]->getX() ) );
        BOOST_CHECK( !ps1->containsX( p[ 0 ]->getY() ) );
        BOOST_CHECK( !ps1->containsX( 50000 ) );

        delete ps1;
        for( vector<ExplicitPointSet::DataPoint*>::iterator i = p.begin(); i != p.end(); i++ ){
            delete * i;
        }
    }

    //! Test containsY function
    void testContainsY() {
        vector<ExplicitPointSet::DataPoint*> p; 
        p.resize( 20 );
        for( int i = 0; i < 20; i++ ){
            p[ i ] = new XYDataPoint( i, i + 20 );
        }
        ExplicitPointSet* ps1 = new ExplicitPointSet();

        ps1->addPoint( p[ 0 ]->clone() );
        ps1->addPoint( p[ 1 ]->clone() );
        ps1->addPoint( p[ 2 ]->clone() );

        BOOST_CHECK( ps1->containsY( p[ 0 ]->getY() ) );
        BOOST_CHECK( ps1->containsY( p[ 1 ]->getY() ) );
        BOOST_CHECK( !ps1->containsY( p[ 0 ]->getX() ) );
        BOOST_CHECK( !ps1->containsY( 50000 ) );

        delete ps1;
        for( vector<ExplicitPointSet::DataPoint*>::iterator i = p.begin(); i != p.end(); i++ ){
            delete * i;
        }
    }

    //! Test get nearest X below function.
    void testGetNearestXBelow() {
        ExplicitPointSet* ps1 = new ExplicitPointSet();

    }

    //! Test get nearest X below function.
    void testGetNearestXAbove() {
    }

    //! Test get nearest Y below function.
    void testGetNearestYBelow() {
    }

    //! Test get nearest Y above function.
    void testGetNearestYAbove() {
    }

    test_suite* init_unit_test_suite() {
        test_suite* test = BOOST_TEST_SUITE( "ExplicitPointSet tests" );

        test->add( BOOST_TEST_CASE( &testConstructor ) );
        test->add( BOOST_TEST_CASE( &testCopy ) );
        test->add( BOOST_TEST_CASE( &testAssignment ) );
        test->add( BOOST_TEST_CASE( &testDestructor ) );
        test->add( BOOST_TEST_CASE( &testEquality ) );
        test->add( BOOST_TEST_CASE( &testInequality ) );
        test->add( BOOST_TEST_CASE( &testAddPoint ) );
        test->add( BOOST_TEST_CASE( &testGetY ) );
        test->add( BOOST_TEST_CASE( &testGetX ) );
        test->add( BOOST_TEST_CASE( &testSetY ) );
        test->add( BOOST_TEST_CASE( &testSetX ) );
        test->add( BOOST_TEST_CASE( &testRemovePointFindX ) );
        test->add( BOOST_TEST_CASE( &testRemovePointFindY ) );
        test->add( BOOST_TEST_CASE( &testGetXCoords ) );
        test->add( BOOST_TEST_CASE( &testGetYCoords ) );
        test->add( BOOST_TEST_CASE( &testContainsX ) );
        test->add( BOOST_TEST_CASE( &testContainsY ) );
        test->add( BOOST_TEST_CASE( &testContainsX ) );
        test->add( BOOST_TEST_CASE( &testContainsX ) );
        return test;
    }

}