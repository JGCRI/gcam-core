/*! 
* \file explicit_point_set.cpp
* \ingroup Util
* \brief ExplicitPointSet class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include "util/curves/include/explicit_point_set.h"
#include "util/base/include/util.h"
#include <vector>
#include <algorithm>
#include <limits>
#include <cassert>
#include "util/base/include/xml_helper.h"

using namespace std;

//! Constructor
ExplicitPointSet::ExplicitPointSet() {
}

//! Copy constructor. This is needed so we don't have a double-free problem.
ExplicitPointSet::ExplicitPointSet( const ExplicitPointSet& rhs ){
    copy( rhs );
}

//! Destructor
ExplicitPointSet::~ExplicitPointSet(){
    clear();
}

//! Assignment operator
ExplicitPointSet& ExplicitPointSet::operator=( const ExplicitPointSet& rhs ){
    if( this != &rhs ){
        clear();
        copy( rhs );
    }
    return *this;
}

//! Equals operator
bool ExplicitPointSet::operator ==( const ExplicitPointSet &rhs ) const {
    return( ( getXCoords() == rhs.getXCoords() ) && ( getYCoords() == rhs.getYCoords() ) );
}

//! Inequality operator
bool ExplicitPointSet::operator!=( const ExplicitPointSet& rhs ) const {
    return !( *this == rhs ); // Invokes ExplicitPointSet::operator==
}

//! Helper function which frees memory.
void ExplicitPointSet::clear() {
    for( vector<DataPoint*>::iterator delIter = points.begin(); delIter != points.end(); delIter++ ){
        delete *delIter;
    }
    points.clear();
}

//! Helper function which copies into a new object.
void ExplicitPointSet::copy( const ExplicitPointSet& rhs ){
     // First resize the vector.
    points.resize( rhs.points.size() );
    
    // Now copy all the points over, creating new objects.
    for( int i = 0; i < static_cast<int>( rhs.points.size() ); i++ ){
        points[ i ] = rhs.points[ i ]->clone();
    }
}

//! Add a new data point to the point set. Returns true if it was added successfully, it is a unique point.
bool ExplicitPointSet::addPoint( ExplicitPointSet::DataPoint* pointIn ) {
    bool retValue;

    // First check if the point already exists
    bool foundPoint = false;
    for( vector<ExplicitPointSet::DataPoint*>::const_iterator iter = points.begin(); iter != points.end(); iter++ ){
        if( **iter == *pointIn ){
            foundPoint = true;
            break;
        }
    }
    
    if( foundPoint ){
        retValue = false;
    } 
    else {
        points.push_back( pointIn );
        retValue = true;
    }
    
    return retValue;
}

//! Return the y coordinate associated with this xValue, DBL_MAX if the point is not found.
double ExplicitPointSet::getY( const double xValue ) const {
    double retValue = DBL_MAX;
    const DataPoint* point = findX( xValue );
    if( point ){
        retValue = point->getY();
    }
    return retValue;
}

//! Return the x coordinate associated with this yValue, DBL_MAX if the point is not found.
double ExplicitPointSet::getX( const double yValue ) const {
    double retValue = DBL_MAX;
    const DataPoint* point = findY( yValue );

    // If the point was found.
    if( point ){
        retValue = point->getX();
    }
    return retValue;
}

//! Set the y value for the point associated with the xValue. Return true if successful
bool ExplicitPointSet::setY( const double xValue, const double yValue ){
    bool retValue = false;
    DataPoint* point = findX( xValue );
    
    // If the point was found.
    if( point ){
        point->setY( yValue );
        retValue = true;
    }
    return retValue;
}

//! Set the x value for the point associated with the yValue. Return true if successful
bool ExplicitPointSet::setX( const double yValue, const double xValue ){
    bool retValue = false;
    DataPoint* point = findY( yValue );
    
    // If the point was found.
    if( point ){
        point->setX( xValue );
        retValue = true;
    }
    return retValue;
}

//! Remove a datapoint from the point set based on an x value.
bool ExplicitPointSet::removePointFindX( const double xValue ){
    DataPoint* point = findX( xValue );
    bool retValue = false;

    if( point != 0 ){
        vector<DataPoint*>::iterator delIter = find( points.begin(), points.end(), point );
        if( delIter == points.end() ){
            assert( false );
        } 
        else {
            delete point;
            points.erase( delIter );
            retValue = true;
        }
    }

    return retValue;
}

//! Remove a datapoint from the point set based on a y value.
bool ExplicitPointSet::removePointFindY( const double yValue ){
    DataPoint* point = findY( yValue );
    bool retValue = false;

    if( point != 0 ){
        vector<DataPoint*>::iterator delIter = find( points.begin(), points.end(), point );
        if( delIter == points.end() ){
            assert( false );
        } else {
            delete point;
            points.erase( delIter );
            retValue = true;
        }
    }

    return retValue;
}

//! Return a vector containing all x coordinates in the dataset between lowDomain and highDomain.
/*! \todo Handle minPoints. */
const vector<double> ExplicitPointSet::getXCoords( const double lowDomain, const double highDomain, const int minPoints ) const {
    vector<double> returnValues;
    
    for( vector<DataPoint*>::const_iterator iter = points.begin(); iter != points.end(); iter++ ){
        if( ( (*iter)->getX() >= lowDomain ) && ( (*iter)->getX() <= highDomain ) ){
            returnValues.push_back( (*iter)->getX() );
        }
    }
    return returnValues;
}

//! Return a vector containing all y coordinates in the dataset between lowRange and highRange.
const vector<double> ExplicitPointSet::getYCoords( const double lowRange, const double highRange, const int minPoints ) const {
    vector<double> returnValues;
    
    for( vector<DataPoint*>::const_iterator iter = points.begin(); iter != points.end(); iter++ ){
        if( ( (*iter)->getY() >= lowRange ) && ( (*iter)->getY() <= highRange ) ){
            returnValues.push_back( (*iter)->getY() );
        }
    }
    return returnValues;
}

//! Return a vector of pairs of x y coordinates sorted in increasing x order.
vector<pair<double,double> > ExplicitPointSet::getSortedPairs( const double lowDomain, const double highDomain, const int minPoints ) const {
    // Create a copy of the points as this is a const function.
    vector<DataPoint*> pointsCopy = points;
    sort( pointsCopy.begin(), pointsCopy.end() );

    // Now create a vector of std::pairs to return. This is due to the superclass being unaware of the underlying representation.
    vector<pair<double,double> > sortedPoints;
    for( vector<DataPoint*>::const_iterator iter = pointsCopy.begin(); iter != pointsCopy.end(); iter++ ){
        // Check if it is within the requested domain. 
        if( ( (*iter)->getX() >= lowDomain ) && ( (*iter)->getX() <= highDomain ) ){
            // add the point.
            sortedPoints.push_back( pair<double,double>( (*iter)->getX(), (*iter)->getY() ) );
        }
    }
    return sortedPoints;
}

//! Returns whether the point set contains a point with the given x value.
bool ExplicitPointSet::containsX( const double x ) const {
    bool retValue = false;
    
    if( findX( x ) ){
        retValue = true;
    }
    return retValue;
}

//! Returns whether the point set contains a point with the given y value.
bool ExplicitPointSet::containsY( const double y ) const {
    bool retValue = false;
    
    if( findY( y ) ){
        retValue = true;
    }
    return retValue;
}
 
//! Determines the x coordinate of the nearest point below x.
double ExplicitPointSet::getNearestXBelow( const double x ) const {
    double closestX = -DBL_MAX;
    for( vector<DataPoint*>::const_iterator pointsIter = points.begin(); pointsIter != points.end(); pointsIter++ ){
        double currX = ( *pointsIter )->getX();
        if( ( currX < x ) && ( fabs( x - currX ) < fabs( x - closestX ) ) ){
            closestX = currX;
        }
    }
    return closestX;
}

//! Determines the x coordinate of the nearest point above x.
double ExplicitPointSet::getNearestXAbove( const double x ) const {
    double closestX = DBL_MAX;
    for( vector<DataPoint*>::const_iterator pointsIter = points.begin(); pointsIter != points.end(); pointsIter++ ){
        double currX = ( *pointsIter )->getX();
        if( ( currX > x ) && ( fabs( currX - x ) < fabs( closestX - x ) ) ){
            closestX = currX;
        }
    }
    return closestX;
}

//! Determines the y coordinate of the nearest point below y.
double ExplicitPointSet::getNearestYBelow( const double y ) const {
    double closestY = -DBL_MAX;
    for( vector<DataPoint*>::const_iterator pointsIter = points.begin(); pointsIter != points.end(); pointsIter++ ){
        double currY = ( *pointsIter )->getY();
        if( ( currY < y ) && ( fabs( y - currY ) < fabs( y - closestY ) ) ){
            closestY = currY;
        }
    }
    return closestY;
}

//! Determines the x coordinate of the nearest point above y.
double ExplicitPointSet::getNearestYAbove( const double y ) const {
    double closestY = DBL_MAX;
    for( vector<DataPoint*>::const_iterator pointsIter = points.begin(); pointsIter != points.end(); pointsIter++ ){
        double currY = ( *pointsIter )->getY();
        if( ( currY > y ) && ( fabs( currY - y ) < fabs( closestY - y ) ) ){
            closestY = currY;
        }
    }
    return closestY;
}

//! Print out the PointSet to an XML file.
void ExplicitPointSet::toXML( ostream& out, Tabs* tabs ) const {
    tabs->writeTabs( out );
    out << "<ExplicitPointSet>" << endl;
    tabs->increaseIndent();
    for( vector<DataPoint*>::const_iterator point = points.begin(); point != points.end(); point++ ){
        ( *point )->toXML( out, tabs );
    }
    tabs->decreaseIndent();
    tabs->writeTabs( out );
    out << "</ExplicitPointSet>" << endl;
}

//! Const helper function which returns the point with a given x value.
const ExplicitPointSet::DataPoint* ExplicitPointSet::findX( const double xValue ) const {
    DataPoint* retValue = 0;
    for( vector<DataPoint*>::const_iterator pointsIter = points.begin(); pointsIter != points.end(); pointsIter++ ){
        if( util::isEqual( xValue, ( *pointsIter )->getX() ) ){
            retValue = *pointsIter;
            break;
        }
    }
    return retValue;
}

//! Non-Const helper function which returns the point with a given x value.
ExplicitPointSet::DataPoint* ExplicitPointSet::findX( const double xValue ) {
    DataPoint* retValue = 0;
    for( vector<DataPoint*>::iterator pointsIter = points.begin(); pointsIter != points.end(); pointsIter++ ){
        if( util::isEqual( xValue, ( *pointsIter )->getX() ) ){
            retValue = *pointsIter;
            break;
        }
    }
    return retValue;
}

//! Const helper function which returns the point with a given y value.
const ExplicitPointSet::DataPoint* ExplicitPointSet::findY( const double yValue ) const {
    DataPoint* retValue = 0;
    for( vector<DataPoint*>::const_iterator pointsIter = points.begin(); pointsIter != points.end(); pointsIter++ ){
        if( util::isEqual( yValue, ( *pointsIter )->getY() ) ){
            retValue = *pointsIter;
            break;
        }
    }
    return retValue;
}

//! Non-Const helper function which returns the point with a given y value.
ExplicitPointSet::DataPoint* ExplicitPointSet::findY( const double yValue ) {
    DataPoint* retValue = 0;    
    for( vector<DataPoint*>::iterator pointsIter = points.begin(); pointsIter != points.end(); pointsIter++ ){
        if( util::isEqual( yValue, ( *pointsIter )->getY() ) ){
            retValue = *pointsIter;
            break;
        }
    }
    return retValue;
}

/*! \brief Print function to print the PointSet in a csv format.
* \param out Stream to write to.
* \param lowDomain The lowest x value to write out.
* \param highDomain The highest x value to write out.
* \param lowRange The lowest y value to write out.
* \param highRange The highest y value to write out. 
*/
void ExplicitPointSet::print( ostream& out, const double lowDomain, const double highDomain,
                             const double lowRange, const double highRange, const int minPoints ) const {
    vector<DataPoint*> pointsCopy = points;
    sort( pointsCopy.begin(), pointsCopy.end(), ExplicitPointSet::DataPoint::LesserX() );
	
    out << "x,y" << endl;
    for( vector<DataPoint*>::const_iterator pointIter = pointsCopy.begin(); pointIter != pointsCopy.end(); pointIter++ ){
        // Check if the point meets the printing conditions.
	
        if( ( ( *pointIter )->getX() <= highDomain ) && ( ( *pointIter )->getX() >= lowDomain ) &&
            ( ( *pointIter )->getY() <= highRange ) && ( ( *pointIter )->getY() >= lowRange ) ){
                out << ( *pointIter )->getX() << "," << ( *pointIter )->getY() << endl;
            }
    }
    out << endl;
}