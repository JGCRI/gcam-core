/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

#ifndef _ARESOURCE_H_
#define _ARESOURCE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file aresource.h
 * \ingroup Objects
 * \brief AResource header file.
 * \author Josh Lurz
 */
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/ivisitable.h"

class Tabs;
class GDP;
class IInfo;

/*! 
* \ingroup Objects
* \brief An abstract class which defines a single resource.
* \todo This class needs much more documentation.
* \author Josh Lurz
*/
class AResource: public IVisitable {
    friend class XMLDBOutputter;
public:
    virtual ~AResource();

    virtual void XMLParse( const xercesc::DOMNode* aNode ) = 0;

    virtual void toInputXML( std::ostream& aOut,
                             Tabs* aTabs ) const = 0;

    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const = 0;

    virtual const std::string& getName() const = 0;

    virtual void completeInit( const std::string& aRegionName,
                               const IInfo* aRegionInfo ) = 0;
    
    virtual void initCalc( const std::string& aRegionName,
                           const int aPeriod ) = 0;

    virtual void postCalc( const std::string& aRegionName,
                           const int aPeriod ) = 0;
    
    virtual void calcSupply( const std::string& aRegionName,
                             const GDP* aGDP,
                             const int period ) = 0;

    virtual double getAnnualProd( const std::string& aRegionName,
                                  const int aPeriod ) const = 0;

    virtual void dbOutput( const std::string& aRegionName ) = 0;

    virtual void csvOutputFile( const std::string& aRegionName ) = 0;

    virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const = 0;
protected:
    // For the database output
    virtual const std::string& getXMLName() const = 0;
};

// Inline function definitions
inline AResource::~AResource(){
}

#endif // _ARESOURCE_H_
