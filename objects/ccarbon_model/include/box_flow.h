#ifndef _BOX_FLOW_H_
#define _BOX_FLOW_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

/*! 
 * \file box_flow.h
 * \ingroup Objects
 * \brief BoxFlow class header file.
 * \author Jim Naslund
 */

#include "ccarbon_model/include/acarbon_flow.h"

/*!
 * \brief A class which represents a transfer from one box to another.
 * \details A flow from one carbon box in a box model to another carbon
 *          box in the same box model.  This flow is not driven by land
 *          use change.
 */
class BoxFlow : public ACarbonFlow {

public:
    BoxFlow();
    BoxFlow( const BoxFlow& aBoxFlow );
    virtual ~BoxFlow();
    virtual BoxFlow* clone() const;

    virtual bool XMLDerivedClassParse( const xercesc::DOMNode* aNode );
    virtual void toInputXMLDerived( std::ostream& aOut, Tabs* aTabs ) const;
    virtual void toDebugXMLDerived( std::ostream& aOut, Tabs* aTabs ) const;
    static const std::string& getXMLNameStatic();
    const std::string& getXMLName() const;

    virtual void transfer( const double aValue, const EnvironmentalInfo* aEnvInfo,
                           const int aYear, const BoxType aBoxType );

};

#endif // _BOX_FLOW_H_
