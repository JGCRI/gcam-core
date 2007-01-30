#ifndef _HISTORICAL_LAND_USE_H_
#define _HISTORICAL_LAND_USE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*!
 * \file land_use_history.h
 * \ingroup Objects
 * \brief The LandUseHistory class header file.
 * \author Josh Lurz
 */
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/ivisitable.h"
#include <map>

class Tabs;
/*!
 * \brief Container of historical land allocations for a single land type.
 * \details A container mapping years to historical land use for a single land
 *          type. Each land type may have a single LandUseHistory, multiple are
 *          not allowed.  LandUseHistory objects can be contained in either
 *          LandNodes or UmanagedLandLeafs.
 *
 *          <b>XML specification for LandUseHistory</b>
 *          - XML name: \c land-use-history
 *          - Contained by: LandNode, UnmanagedLandLeaf
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements:
 *              - \c allocation LandUseHistory::mHistoricalLand (value is a double)
 *                  -Attributes
 *                      - \c year the year of the land allocation
 */
class LandUseHistory : public IVisitable,
                       public IParsable,
                       public IRoundTrippable
{
public:
    static const std::string& getXMLNameStatic();

    LandUseHistory();
    
    // IParsable Methods.
    virtual bool XMLParse( const xercesc::DOMNode* aNode );
    
    const std::string& getName() const;

    void toDebugXML( const int aPeriod,
                     std::ostream& aOut,
                     Tabs* aTabs ) const;
    
    // IRoundTrippable
    virtual void toInputXML( std::ostream& aOut,
                             Tabs* aTabs ) const;
    
    // IVisitableMethod
	virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const;

    unsigned int getMinYear() const;

    unsigned int getMaxYear() const;

    double getAllocation( const unsigned int aYear ) const;
protected:
    //! Map type for land allocations by year.
    typedef std::map<unsigned int, double> LandMapType;

    //! Sparse mapping of year to land allocation.
    LandMapType mHistoricalLand;
};

#endif // _HISTORICAL_LAND_USE_H_

