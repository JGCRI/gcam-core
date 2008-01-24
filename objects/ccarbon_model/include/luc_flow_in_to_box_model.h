#ifndef _luc_flow_in_to_box_model_H_
#define _luc_flow_in_to_box_model_H_
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
 * \file luc_flow_in_to_box_model.h
 * \ingroup Objects
 * \brief LUCFlowInToBoxModel class header file.
 * \author Jim Naslund and Ming Chang
 */

#include "ccarbon_model/include/acarbon_flow.h"

/*!
 * \brief A class which represents a flow from the Summer to a CarbonBoxModel.
 * \details This class represents a flow of carbon due to positive land
 *          use change.  This flow should be from the summer to a CarbonBoxModel.
 *          The box model will then distribute the carbon via LUCFlowInToBox flows
 *          to carbon boxes.
 *
 *          A total carbon value is passed into this flow and a carbon value per unit
 *          land is then passed forward. 
 */
class LUCFlowInToBoxModel : public ACarbonFlow {

public:
    LUCFlowInToBoxModel( ICarbonContainer* aTarget, const int aFraction );
    LUCFlowInToBoxModel( ICarbonContainer* aTarget );
    LUCFlowInToBoxModel( const LUCFlowInToBoxModel& aLUCFlowInToBoxModel );
    virtual ~LUCFlowInToBoxModel();
    virtual LUCFlowInToBoxModel* clone() const;

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

#endif // _luc_flow_in_to_box_model_H_

