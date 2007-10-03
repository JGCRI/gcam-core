#ifndef _LUC_FLOW_IN_H_
#define _LUC_FLOW_IN_H_
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
 * \file luc_flow_in.h
 * \ingroup Objects
 * \brief LUCFlowIn class header file.
 * \author Jim Naslund
 */

#include "ccarbon_model/include/acarbon_flow.h"

/*!
 * \brief A class which represents a flow into a container due to positive land use change.
 * \details This class represents a flow of carbon due to positive land
 *          use change.  This flow should be from a container in the singleton summer
 *          to a carbon box model.
 */
class LUCFlowIn : public ACarbonFlow {

public:
    LUCFlowIn( ICarbonContainer* aTarget, const int aFraction );
    LUCFlowIn( ICarbonContainer* aTarget );
    virtual ~LUCFlowIn();

    virtual bool XMLDerivedClassParse( const xercesc::DOMNode* aNode );
    virtual void toInputXMLDerived( std::ostream& aOut, Tabs* aTabs ) const;
    virtual void toDebugXMLDerived( std::ostream& aOut, Tabs* aTabs ) const;
    static const std::string& getXMLNameStatic();
    virtual void completeInit( const std::map< const std::string, ICarbonContainer* > aNamesToBoxes,
                               int aKey  );
    const std::string& getXMLName() const;

    virtual void transfer( const double aValue, const EnvironmentalInfo* aEnvInfo,
                           const int aYear, const BoxType aBoxType );

};

#endif // _LUC_FLOW_IN_H_

