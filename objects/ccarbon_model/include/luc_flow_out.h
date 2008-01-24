#ifndef _LUC_FLOW_OUT_H_
#define _LUC_FLOW_OUT_H_
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
 * \file luc_flow_out.h
 * \ingroup Objects
 * \brief LUCFlowOut class header file.
 * \author Jim Naslund and Ming Chang
 */

#include "ccarbon_model/include/acarbon_flow.h"

/*!
 * \brief A class which represents a flow out of a container due to a reduction in land area.
 * \details This class represents a flow of carbon due to a negative land
 *          use change.  This flow should be from a carbon box in a
 *          carbon box model to a carbon box container in the singleton summer.
 */
class LUCFlowOut : public ACarbonFlow {

public:
    LUCFlowOut();
    LUCFlowOut( const LUCFlowOut& aLUCFlowOut );
    virtual ~LUCFlowOut();
    virtual LUCFlowOut* clone() const;

    virtual bool XMLDerivedClassParse( const xercesc::DOMNode* aNode );
    virtual void toInputXMLDerived( std::ostream& aOut, Tabs* aTabs ) const;
    virtual void toDebugXMLDerived( std::ostream& aOut, Tabs* aTabs ) const;
    static const std::string& getXMLNameStatic();
    const std::string& getXMLName() const;

    virtual void transfer( const double aValue, const EnvironmentalInfo* aEnvInfo,
                           const int aYear, const BoxType aBoxType );

};

#endif // _LUC_FLOW_OUT_H_

